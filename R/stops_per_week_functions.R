#' Count the number of week days between two dates
#'
#'
#' @param cal GTFS calendar
#'
#' @return a GTFS calendar data frame with additional columms e.g. "runs_monday"
#'
#' @noRd
count_weekday_runs <- function(cal){

  # Data.table fix Internal error: storage mode of IDate is somehow no longer integer
  if(inherits(cal$start_date,"IDate")){
    cal$start_date = as.Date(cal$start_date)
    cal$end_date = as.Date(cal$end_date)
  }


  cal$TMP_d <- as.integer(cal$end_date - cal$start_date) + 1
  cal$TMP_d[is.na(cal$TMP_d)] <- 0

  dow = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

  res <- purrr::map2(cal$start_date,cal$TMP_d, function(startdate, d){
    dys <- weekdays(seq(startdate, length.out=d, by=1))
    dys <- as.data.frame.matrix(t(table(dys)))
    if(ncol(dys) < 7){
      dysmiss <- dow[!dow %in% names(dys)]
      dysmiss2 <- rep(0, length(dysmiss))
      names(dysmiss2) <- dysmiss
      dysmiss2 <- data.frame(as.list(dysmiss2))
      dys <- cbind(dys, dysmiss2)
    }
    dys <- dys[,dow]
  })

  res <- dplyr::bind_rows(res)
  names(res) <- paste0("n_",dow)
  cal <- cbind(cal, res)

  cal$runs_monday <- cal$monday * cal$n_Monday
  cal$runs_tuesday <- cal$tuesday * cal$n_Tuesday
  cal$runs_wednesday <- cal$wednesday * cal$n_Wednesday
  cal$runs_thursday <- cal$thursday * cal$n_Thursday
  cal$runs_friday <- cal$friday * cal$n_Friday
  cal$runs_saturday <- cal$saturday * cal$n_Saturday
  cal$runs_sunday <- cal$sunday * cal$n_Sunday

  cal <- dplyr::mutate(cal, runs_weekdays = runs_monday + runs_tuesday + runs_wednesday + runs_thursday + runs_friday)

  cal <- cal[,c("service_id",
                "monday","tuesday","wednesday","thursday","friday",
                "saturday","sunday","start_date","end_date",
                "runs_monday","runs_tuesday","runs_wednesday", "runs_thursday",
                "runs_friday","runs_saturday","runs_sunday", "runs_weekdays")]
  return(cal)

}



#' Convert GTFS times to seconds since midnight
#'
#' Handles the different classes GTFS time columns arrive in: lubridate
#' Period (stop_times from gtfs_read), ITime/difftime (fread auto-detection),
#' or "HH:MM:SS" character. Times past 24:00:00 are preserved.
#'
#' @param x a vector of times
#' @return numeric seconds since midnight
#'
#' @noRd
gtfs_time_to_seconds <- function(x){
  if(inherits(x, "Period")){
    return(lubridate::period_to_seconds(x))
  }
  if(inherits(x, "difftime")){
    return(as.numeric(x, units = "secs"))
  }
  if(inherits(x, "ITime")){
    return(as.numeric(unclass(x)))
  }
  if(is.character(x)){
    return(vapply(strsplit(x, ":", fixed = TRUE), function(p){
      sum(as.numeric(p) * c(3600, 60, 1)[seq_along(p)])
    }, numeric(1)))
  }
  as.numeric(x)
}

#' Count the departures represented by each frequencies.txt row
#'
#' Trips depart every headway_secs from start_time up to but not including
#' end_time, so a row spans ceiling((end - start) / headway) departures.
#'
#' @param frequencies GTFS frequencies table
#' @return integer vector of departures per row
#'
#' @noRd
frequency_departures <- function(frequencies){
  start_secs <- gtfs_time_to_seconds(frequencies$start_time)
  end_secs <- gtfs_time_to_seconds(frequencies$end_time)
  headway <- as.numeric(frequencies$headway_secs)
  pmax(ceiling((end_secs - start_secs) / headway), 1)
}

#' Total departures per day for each frequency-based trip
#'
#' @param frequencies GTFS frequencies table
#' @return data frame of trip_id and freq_runs (departures per day)
#'
#' @noRd
frequency_runs_per_trip <- function(frequencies){
  frequencies <- as.data.frame(frequencies)
  frequencies$trip_id <- as.character(frequencies$trip_id)
  frequencies$n_departures <- frequency_departures(frequencies)
  freq_runs <- dplyr::group_by(frequencies, trip_id)
  freq_runs <- dplyr::summarise(freq_runs, freq_runs = sum(n_departures))
  freq_runs
}

#' Expand frequency-based trips into one pseudo-trip per departure
#'
#' Each departure implied by frequencies.txt becomes its own trip in
#' gtfs$trips and gtfs$stop_times, with stop times shifted so that departure
#' hours (and so time bands) are correct. The stop_times of a frequency-based
#' trip define relative travel times from its first stop.
#'
#' @param gtfs GTFS object with a frequencies table
#' @return the GTFS object with trips and stop_times expanded
#'
#' @noRd
expand_frequency_trips <- function(gtfs){
  freq <- as.data.frame(gtfs$frequencies)
  freq$trip_id <- as.character(freq$trip_id)
  freq <- freq[freq$trip_id %in% gtfs$trips$trip_id, ]
  if(nrow(freq) == 0){
    return(gtfs)
  }

  start_secs <- gtfs_time_to_seconds(freq$start_time)
  headway <- as.numeric(freq$headway_secs)
  n_dep <- frequency_departures(freq)

  departures <- data.frame(
    trip_id = rep(freq$trip_id, n_dep),
    dep_secs = unlist(mapply(function(s, h, n){s + (seq_len(n) - 1) * h},
                             start_secs, headway, n_dep, SIMPLIFY = FALSE))
  )
  departures$trip_id_new <- paste0(departures$trip_id, "_freq",
                                   seq_len(nrow(departures)))

  trips <- as.data.frame(gtfs$trips)
  is_freq_trip <- trips$trip_id %in% departures$trip_id
  trips_freq <- trips[is_freq_trip, , drop = FALSE]
  trips_freq <- dplyr::left_join(trips_freq,
                                 departures[, c("trip_id", "trip_id_new")],
                                 by = "trip_id", relationship = "many-to-many")
  trips_freq$trip_id <- trips_freq$trip_id_new
  trips_freq$trip_id_new <- NULL
  gtfs$trips <- rbind(trips[!is_freq_trip, , drop = FALSE], trips_freq)

  secs_to_hms <- function(secs){
    lubridate::hms(ifelse(is.na(secs), NA_character_,
                          sprintf("%02d:%02d:%02d",
                                  as.integer(secs %/% 3600),
                                  as.integer((secs %% 3600) %/% 60),
                                  as.integer(secs %% 60))),
                   quiet = TRUE)
  }

  stop_times <- as.data.frame(gtfs$stop_times)
  # Expanded rows get Period times, so all rows must use them
  if(!inherits(stop_times$departure_time, "Period")){
    stop_times$departure_time <- secs_to_hms(
      gtfs_time_to_seconds(stop_times$departure_time))
  }
  if("arrival_time" %in% names(stop_times) &&
     !inherits(stop_times$arrival_time, "Period")){
    stop_times$arrival_time <- secs_to_hms(
      gtfs_time_to_seconds(stop_times$arrival_time))
  }
  is_freq_st <- stop_times$trip_id %in% departures$trip_id
  st_freq <- stop_times[is_freq_st, , drop = FALSE]

  st_freq$TMP_dep <- gtfs_time_to_seconds(st_freq$departure_time)
  first_dep <- dplyr::group_by(st_freq, trip_id)
  first_dep <- dplyr::summarise(first_dep, TMP_first = min(TMP_dep, na.rm = TRUE))
  st_freq <- dplyr::left_join(st_freq, first_dep, by = "trip_id")
  st_freq <- dplyr::left_join(st_freq, departures, by = "trip_id",
                              relationship = "many-to-many")

  shift <- st_freq$dep_secs - st_freq$TMP_first
  st_freq$departure_time <- secs_to_hms(st_freq$TMP_dep + shift)
  if("arrival_time" %in% names(st_freq)){
    st_freq$arrival_time <- secs_to_hms(
      gtfs_time_to_seconds(st_freq$arrival_time) + shift)
  }
  st_freq$trip_id <- st_freq$trip_id_new
  st_freq <- st_freq[, names(stop_times), drop = FALSE]
  gtfs$stop_times <- rbind(stop_times[!is_freq_st, , drop = FALSE], st_freq)

  return(gtfs)
}


#' Count the number of trips stopping at each stop between two dates
#'
#' @param gtfs GTFS object from gtfs_read()
#' @param startdate Start date
#' @param enddate End date
#' @return the stops table with total stop counts and stops per week added
#' @details For frequency-based services (frequencies.txt), each trip is
#'   counted once per departure implied by its frequency windows.
#'
#' @export
gtfs_stop_frequency <- function(gtfs,
                        startdate = lubridate::ymd("2020-03-01"),
                        enddate = lubridate::ymd("2020-04-30")){
  message("Only using stops between ",startdate," and ",enddate)
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_days <- gtfs$calendar_dates

  # New gtfs_read loads in data.table IDate format
  if(inherits(calendar$start_date,"IDate")){
    startdate <- data.table::as.IDate(startdate)
    enddate <- data.table::as.IDate(enddate)
  }

  calendar <- calendar[calendar$start_date <= enddate,]
  calendar <- calendar[calendar$end_date >= startdate,]

  if(nrow(calendar) == 0){
    stop("No services between dates, check your start and end dates")
  }

  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)

  #summary(calendar$end_date >= calendar$start_date)

  calendar_days <- calendar_days[calendar_days$service_id %in% calendar$service_id,]
  calendar_days <- calendar_days[calendar_days$date >= startdate,]
  calendar_days <- calendar_days[calendar_days$date <= enddate,]

  calendar_days <- dplyr::left_join(calendar_days,
                             calendar[,c("service_id", "start_date", "end_date")],
                             by = "service_id")

  calendar_days <- calendar_days[calendar_days$date >= calendar_days$start_date, ]
  calendar_days <- calendar_days[calendar_days$date <= calendar_days$end_date, ]

  # A date can only be added or cancelled once per service
  calendar_days <- calendar_days[!duplicated(
    paste(calendar_days$service_id, calendar_days$date, calendar_days$exception_type)), ]

  # GTFS semantics: a cancellation only removes a trip on a day the calendar
  # operates, and an extra only adds a trip on a day it does not already
  # operate. Feeds contain cancellations on non-operating days (no-ops), which
  # would otherwise make counts go negative.
  dow_cols <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
  calendar_days <- dplyr::left_join(calendar_days,
                                    calendar[, c("service_id", dow_cols)],
                                    by = "service_id")
  calendar_days$day_flag <- as.matrix(calendar_days[, dow_cols])[
    cbind(seq_len(nrow(calendar_days)),
          lubridate::wday(calendar_days$date, week_start = 1))]

  calendar_days <- dplyr::group_by(calendar_days, service_id)
  calendar_days <- dplyr::summarise(calendar_days,
                     runs_extra = sum(exception_type == 1 & day_flag == 0),
                     runs_canceled = sum(exception_type == 2 & day_flag == 1))

  trips <- trips[trips$service_id %in% calendar$service_id, ]
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  message("Counting trips on each day")
  calendar <- count_weekday_runs(calendar)

  # work out how many times the trip in run
  trips <- dplyr::left_join(trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_days, by = "service_id")

  trips$runs_canceled[is.na(trips$runs_canceled)] <- 0
  trips$runs_extra[is.na(trips$runs_extra)] <- 0



  message("Summarising results")
  trips$runs_days <- trips$runs_monday + trips$runs_tuesday +
    trips$runs_wednesday + trips$runs_thursday + trips$runs_friday +
    trips$runs_saturday + trips$runs_sunday

  trips$runs_total <-  trips$runs_days + trips$runs_extra - trips$runs_canceled

  # Frequency-based trips represent multiple departures per day
  if(!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0){
    message("Scaling frequency-based trips by departures per day")
    freq_runs <- frequency_runs_per_trip(gtfs$frequencies)
    trips <- dplyr::left_join(trips, freq_runs, by = "trip_id")
    trips$freq_runs[is.na(trips$freq_runs)] <- 1
    trips$runs_total <- trips$runs_total * trips$freq_runs
  }

  trips <- trips[,c("trip_id","start_date","end_date","runs_total")]
  stop_times <- dplyr::left_join(stop_times, trips, by = "trip_id")
  stop_times_summary <- dplyr::group_by(stop_times, stop_id)
  stop_times_summary <- dplyr::summarise(stop_times_summary, stops_total = sum(runs_total))

  stop_times_summary$stops_per_week <- stop_times_summary$stops_total / ((as.numeric(enddate - startdate) + 1)/7)

  stops <- dplyr::left_join(gtfs$stops, stop_times_summary, by = "stop_id")
  return(stops)
}


#' Trim a GTFS file between two dates
#'
#' @param gtfs GTFS object from gtfs_read()
#' @param startdate Start date
#' @param enddate End date
#' @return a gtfs object trimmed to services running between the two dates
#'
#' @export
gtfs_trim_dates <- function(gtfs,
                            startdate = lubridate::ymd("2020-03-01"),
                            enddate = lubridate::ymd("2020-04-30")) {

  if(enddate < startdate){
    stop("enddate is before start date")
  }

  message("Trimming GTFS between ",startdate," and ",enddate)
  stop_times <- gtfs$stop_times
  trips <- gtfs$trips
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # New gtfs_read loads in data.table IDate format
  if(inherits(calendar$start_date,"IDate")){
    calendar$start_date <- as.Date(calendar$start_date)
    calendar$end_date <- as.Date(calendar$end_date)
  }

  if(inherits(calendar_dates$date,"IDate")){
    calendar_dates$date <- as.Date(calendar_dates$date)
  }


  calendar <- calendar[calendar$start_date <= enddate,]
  calendar <- calendar[calendar$end_date >= startdate,]

  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)
  if(!is.null(calendar_dates)){
    calendar_dates <- calendar_dates[calendar_dates$service_id %in% calendar$service_id,]
    calendar_dates <- calendar_dates[calendar_dates$date >= startdate,]
    calendar_dates <- calendar_dates[calendar_dates$date <= enddate,]

    calendar_dates <- dplyr::left_join(calendar_dates,
                                       calendar[,c("service_id", "start_date", "end_date")],
                                       by = "service_id")

    calendar_dates <- calendar_dates[calendar_dates$date >= calendar_dates$start_date, ]
    calendar_dates <- calendar_dates[calendar_dates$date <= calendar_dates$end_date, ]

    calendar_dates$start_date <- NULL
    calendar_dates$end_date <- NULL
  }

  trips <- trips[trips$service_id %in% calendar$service_id, ]
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  if(!is.null(gtfs$frequencies)){
    gtfs$frequencies <- gtfs$frequencies[gtfs$frequencies$trip_id %in% trips$trip_id, ]
  }

  gtfs$stop_times <- stop_times
  gtfs$trips <- trips
  gtfs$calendar <- calendar
  gtfs$calendar_dates <- calendar_dates
  return(gtfs)
}


#' Trim a GTFS file between two dates
#'
#' @param gtfs GTFS object from gtfs_read()
#' @param zone SF data frame of polygons
#' @param startdate Start date
#' @param enddate End date
#' @param zone_id Which column in `zone` is the ID column
#' @param by_mode logical, disaggregate by mode?
#' @param ncores numeric, how many cores to use in parallel processing
#' @param time_bands list with two named vectors breaks and labels. Used to
#'   define the time breakdown. Length of breaks must be one greater than length
#'   of labels.
#' @return a data frame of trips per zone, day of week, and time band
#' @details For frequency-based services (frequencies.txt), each departure
#'   implied by a frequency window is counted as a separate trip in the time
#'   band of its departure time.
#'
#' @export
gtfs_trips_per_zone <- function(gtfs,
                                zone,
                                startdate = min(gtfs$calendar$start_date),
                                enddate = min(gtfs$calendar$start_date) + 31,
                                zone_id = 1,
                                by_mode = TRUE,
                                ncores = 1,
                                time_bands = list(breaks = c(-1, 6, 10, 15, 18, 22, Inf),
                                                  labels = c("Night", "Morning Peak", "Midday","Afternoon Peak","Evening","Night"))){

  if(!sf::st_is_longlat(zone)){
    message("Transforming zones to 4326")
    zone <- sf::st_transform(zone, 4326)
  }

  zone <- zone[,zone_id]
  names(zone)[1] <- "zone_id"

  # Join Zone id onto stop
  stops_zids <- gtfs$stops
  stops_zids <- stops_zids[!is.na(stops_zids$stop_lon),]

  stops_zids <- sf::st_as_sf(stops_zids,
                             coords = c("stop_lon","stop_lat"),
                             crs = 4326)
  stops_zids <- sf::st_join(stops_zids, zone) # Some stops in multiple Zones
  if(anyNA(stops_zids$zone_id)){
    foo = stops_zids[is.na(stops_zids$zone_id),]
    warning(nrow(foo)," stops outside all zones")
  }

  stops_zids <- stops_zids[,c("stop_id","zone_id")]

  # Trim GTFS to study period
  gtfs <- gtfs_trim_dates(gtfs, startdate = startdate, enddate = enddate)

  # Expand frequency-based trips into one pseudo-trip per departure so each
  # departure is counted in its own time band
  if(!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0){
    message("Expanding frequency-based trips into individual departures")
    gtfs <- expand_frequency_trips(gtfs)
  }

  # Get the summaries for calendar
  calendar_dates_summary <- gtfs$calendar_dates
  # A date can only be added or cancelled once per service; duplicate rows
  # (e.g. from merged feeds) would otherwise be double-counted
  calendar_dates_summary <- calendar_dates_summary[!duplicated(
    paste(calendar_dates_summary$service_id,
          calendar_dates_summary$date,
          calendar_dates_summary$exception_type)), ]
  calendar_dates_summary$weekday = as.character(lubridate::wday(calendar_dates_summary$date, label = TRUE))
  calendar_dates_summary <- dplyr::group_by(calendar_dates_summary, service_id, weekday)
  calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                             extra = sum(exception_type == 1),
                                             canceled = sum(exception_type == 2))

  calendar_dates_summary_missing = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  calendar_dates_summary_missing = calendar_dates_summary_missing[!calendar_dates_summary_missing %in% unique(calendar_dates_summary$weekday)]
  if(length(calendar_dates_summary_missing) > 0){
    calendar_dates_summary_missing = data.frame(service_id = NA,
                                                weekday = calendar_dates_summary_missing,
                                                extra = NA,
                                                canceled = NA)
    calendar_dates_summary = rbind(calendar_dates_summary, calendar_dates_summary_missing)
  }

  calendar_dates_summary <- tidyr::pivot_wider(calendar_dates_summary,
                                               names_from = "weekday",
                                               values_from = c("extra","canceled"),
                                               values_fill = 0)
  calendar_dates_summary <- calendar_dates_summary[!is.na(calendar_dates_summary$service_id),]
  calendar <- count_weekday_runs(gtfs$calendar)
  calendar <- calendar[,c("service_id","runs_monday","runs_tuesday",
                          "runs_wednesday","runs_thursday",
                          "runs_friday","runs_saturday","runs_sunday")]
  names(calendar) <- c("service_id","runs_Mon","runs_Tue",
                       "runs_Wed","runs_Thu",
                       "runs_Fri","runs_Sat","runs_Sun")

  # Add Modes
  if(by_mode){
    routes <- gtfs$routes[,c("route_id","route_type")]
    gtfs$trips <- dplyr::left_join(gtfs$trips, routes, by = "route_id")
    rm(routes)
  }


  #Join to Trips
  trips <- dplyr::left_join(gtfs$trips, calendar, by = "service_id")
  trips <- dplyr::left_join(trips, calendar_dates_summary, by = "service_id")
  rm(calendar, calendar_dates_summary, calendar_dates_summary_missing)

  #TODO: Fix this as ncols may be different beween sources
  trips = as.data.frame(trips)
  nms_match = grep("(runs_)|(extra_)|(canceled_)",names(trips))
  trips[nms_match] <- lapply(trips[nms_match], function(x){
    ifelse(is.na(x),0,x)
  })

  # Apply calendar_dates exceptions with GTFS semantics: a cancellation
  # (exception_type 2) only removes a trip on a day the calendar operates, and
  # an extra (exception_type 1) only adds a trip on a day it does not already
  # operate. TransXChange feeds routinely cancel special days for every service
  # of a route regardless of its day pattern; subtracting those no-op
  # cancellations produced negative run counts.
  for(d in c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")){
    runs <- trips[[paste0("runs_", d)]]
    trips[[paste0("runs_", d)]] <- ifelse(runs > 0,
                                          pmax(runs - trips[[paste0("canceled_", d)]], 0),
                                          trips[[paste0("extra_", d)]])
  }

  # trim out unneeded data
  if(by_mode){
    trips <- trips[,c("trip_id","route_id","service_id","route_type",
                      "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                      "runs_Fri","runs_Sat","runs_Sun")]
  } else {
    trips <- trips[,c("trip_id","route_id","service_id",
                      "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                      "runs_Fri","runs_Sat","runs_Sun")]
  }


  # Join on trip info to stop times
  stop_times <- dplyr::left_join(gtfs$stop_times, trips, by = "trip_id")
  rm(gtfs, trips)

  # -1 so that time between 00:00 and 00:59 are not NA
  # +35 for any service in GTFS that runs past midnight (note that some may arrive following morning but a counted as evening)
  message("Stops that run past midnight are recorded in Night regardless of the time")
  stop_times$time_bands <- cut(lubridate::hour(stop_times$departure_time),
                               breaks = time_bands$breaks,
                               labels = time_bands$labels)
  gc()
  if(by_mode){
    stop_times <- stop_times[,c(c("trip_id","route_id","stop_id","time_bands","route_type",
                                  "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                                  "runs_Fri","runs_Sat","runs_Sun"))]
  } else {
    stop_times <- stop_times[,c(c("trip_id","route_id","stop_id","time_bands",
                                  "runs_Mon","runs_Tue","runs_Wed","runs_Thu",
                                  "runs_Fri","runs_Sat","runs_Sun"))]
  }

  stop_times = stop_times[!is.na(stop_times$time_bands),]

  stop_times <- dplyr::left_join(stop_times, sf::st_drop_geometry(stops_zids), by = "stop_id", relationship = "many-to-many")
  rm(stops_zids)
  #stop_times <- sf::st_drop_geometry(stop_times)
  stop_times$geometry <- NULL

  # Count number of days in study period
  days_tot <- seq(startdate, enddate, by = 1)
  days_tot <- as.character(lubridate::wday(days_tot, label = TRUE))
  days_tot <- as.data.frame(table(days_tot))

  gc()
  message("Processing timetable")

  res <- dplyr::group_by(stop_times, zone_id)
  res <- dplyr::group_split(res)
  future::plan(future::multisession, workers = ncores)
  #res <- future.apply::future_lapply(res, internal_trips_per_zone, by_mode, days_tot)
  res <- furrr::future_map(.x = res,
                           .f = internal_trips_per_zone,
                           by_mode = by_mode,
                           days_tot = days_tot,
                           time_bands = time_bands,
                           .progress = TRUE)
  future::plan(future::sequential)


  res <- dplyr::bind_rows(res)
  res$`.id` <- NULL
  res[2:ncol(res)] <- lapply(res[2:ncol(res)],function(x){ifelse(is.na(x),0,x)})


  return(res)
}

#' Internal helper function
#' @noRd
internal_trips_per_zone <- function(x, by_mode = TRUE, days_tot, time_bands){
  x <- x[!duplicated(x$trip_id),]
  #zone_id = x$zone_id[1]
  #x <- x[,c("time_bands","runs_Mon","runs_Tue","runs_Wed","runs_Thu","runs_Fri","runs_Sat","runs_Sun")]

  x$tot_Mon = days_tot$Freq[days_tot$days_tot == "Mon"]
  x$tot_Tue = days_tot$Freq[days_tot$days_tot == "Tue"]
  x$tot_Wed = days_tot$Freq[days_tot$days_tot == "Wed"]
  x$tot_Thu = days_tot$Freq[days_tot$days_tot == "Thu"]
  x$tot_Fri = days_tot$Freq[days_tot$days_tot == "Fri"]
  x$tot_Sat = days_tot$Freq[days_tot$days_tot == "Sat"]
  x$tot_Sun = days_tot$Freq[days_tot$days_tot == "Sun"]

  # timebands <- data.frame(time_bands =  c("Night", "Morning Peak", "Midday","Afternoon Peak","Evening"),
  #                         band_hours = c(8, 4, 5,3,4))
  timebands = data.frame(time_bands = time_bands$labels)
  band_hours = time_bands$breaks
  band_hours[band_hours < 0] = 0
  band_hours[band_hours > 24] = 24
  timebands$band_hours = diff(band_hours)
  timebands = dplyr::group_by(timebands, time_bands)
  timebands = dplyr::summarise(timebands, band_hours = sum(band_hours))

  x = dplyr::left_join(x, timebands, "time_bands")




  if(by_mode){
    x <- dplyr::group_by(x,zone_id, time_bands, route_type)
  } else {
    x <- dplyr::group_by(x,zone_id, time_bands)
  }


  suppressMessages({
    x <- dplyr::summarise(x,
                          runs_Mon = sum(runs_Mon),
                          runs_Tue = sum(runs_Tue),
                          runs_Wed = sum(runs_Wed),
                          runs_Thu = sum(runs_Thu),
                          runs_Fri = sum(runs_Fri),
                          runs_Sat = sum(runs_Sat),
                          runs_Sun = sum(runs_Sun),
                          tph_Mon = sum(runs_Mon)/ max(tot_Mon * band_hours),
                          tph_Tue = sum(runs_Tue)/ max(tot_Tue * band_hours),
                          tph_Wed = sum(runs_Wed)/ max(tot_Wed * band_hours),
                          tph_Thu = sum(runs_Thu)/ max(tot_Thu * band_hours),
                          tph_Fri = sum(runs_Fri)/ max(tot_Fri * band_hours),
                          tph_Sat = sum(runs_Sat)/ max(tot_Sat * band_hours),
                          tph_Sun = sum(runs_Sun)/ max(tot_Sun * band_hours),
                          routes = length(unique(route_id))
                          )
  })

  if(by_mode){
    x <- tidyr::pivot_wider(x,
                            id_cols = c("zone_id","route_type"),
                            values_from = c(runs_Mon:routes),
                            names_from = c(time_bands)
    )
  } else {
    x <- tidyr::pivot_wider(x,
                            id_cols = "zone_id",
                            values_from = c(runs_Mon:runs_Sun),
                            names_from = c(time_bands)
    )
  }


  return(x)
}






