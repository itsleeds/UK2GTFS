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
  # A calendar row whose end_date precedes its start_date describes no service
  # at all, so it contributes no days of any weekday. Without this it reaches
  # seq(length.out = negative) below and the whole count dies with
  # "'length.out' must be a non-negative number", naming a purrr index and no
  # service_id - which is a long way from the one impossible row that caused
  # it. Feeds do carry such rows: an operator published NW_05_VISB_66_2.xml in
  # the October 2026 TNDS archive with an OperatingPeriod of 2026-10-25 to
  # 2026-10-24, and transxchange2gtfs can synthesise one when it intersects an
  # operating period with the days a service actually runs. Counting zero days
  # for them is right either way, and the zero path is already exercised by
  # the is.na() line above.
  cal$TMP_d[cal$TMP_d < 0] <- 0

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
#'   Service days follow the GTFS rules exactly, date by date. A service runs
#'   on a date when its `calendar.txt` row covers the date and flags its
#'   weekday; an `exception_type = 1` row in `calendar_dates.txt` adds a date
#'   it would not otherwise run on, and an `exception_type = 2` row removes a
#'   date it would. A cancellation of a date the service does not run on
#'   anyway is ignored, as is an addition of a date it already runs on.
#'   Services defined in `calendar_dates.txt` alone, with no `calendar.txt`
#'   row, are counted on the dates they add.
#'
#'   Until version 0.4.0 this function kept only trips whose `service_id` was
#'   in `calendar.txt`, and only additions falling inside that row's own date
#'   range. Both lost real service, and unevenly: the DfT's Bus Open Data
#'   Service GTFS publishes its school-holiday timetables as
#'   `calendar_dates.txt`-only services (about 5% of its trips), so the stop
#'   counts of that one source came out low.
#'
#' @export
gtfs_stop_frequency <- function(gtfs,
                        startdate = lubridate::ymd("2020-03-01"),
                        enddate = lubridate::ymd("2020-04-30")){
  message("Only using stops between ",startdate," and ",enddate)
  startdate <- as.Date(startdate)
  enddate <- as.Date(enddate)
  if(enddate < startdate){
    stop("enddate is before start date")
  }

  # Deliberately not gtfs_trim_dates(): it clips each exception to its own
  # calendar row's date range and drops a service whose row misses the window
  # even when the service adds dates inside it, which are the two cases this
  # function used to get wrong. The untrimmed calendar is evaluated directly.
  runs <- trip_runs_between(gtfs, startdate, enddate)
  if(nrow(runs) == 0 || sum(runs$runs_total) == 0){
    stop("No services between dates, check your start and end dates")
  }

  message("Summarising results")
  # Only the two key columns are taken from stop_times: its time columns are
  # lubridate Periods, which row subsets and joins can silently corrupt, and
  # they are not needed to count.
  st <- data.table::data.table(trip_id = as.character(gtfs$stop_times$trip_id),
                               stop_id = as.character(gtfs$stop_times$stop_id))
  st <- runs[st, on = "trip_id"]
  st$runs_total[is.na(st$runs_total)] <- 0
  stop_times_summary <- st[, list(stops_total = sum(runs_total)),
                           by = "stop_id"]

  stop_times_summary$stops_per_week <- stop_times_summary$stops_total /
    ((as.numeric(enddate - startdate) + 1)/7)

  stops <- gtfs$stops
  stops$stop_id <- as.character(stops$stop_id)
  stops <- dplyr::left_join(stops, as.data.frame(stop_times_summary),
                            by = "stop_id")
  return(stops)
}


#' The dates each service runs between two dates, by the GTFS rules
#'
#' @param gtfs GTFS object, trimmed or not
#' @param startdate,enddate the dates, as Date
#' @return data.table of service_id and date, one row per date a service runs
#' @details Evaluated date by date, so an exception is judged against whether
#'   the service runs on that particular date - its weekday flag *and* its
#'   calendar date range - rather than against its weekday alone. Shared by
#'   gtfs_stop_frequency() and gtfs_trips_per_zone() so that the two count
#'   service days identically.
#' @noRd
service_days_between <- function(gtfs, startdate, enddate){
  startdate <- as.Date(startdate)
  enddate <- as.Date(enddate)
  dow <- c("monday","tuesday","wednesday","thursday","friday","saturday","sunday")
  dates <- seq(startdate, enddate, by = 1)
  dates_dow <- lubridate::wday(dates, week_start = 1)

  cal <- data.table::as.data.table(as.data.frame(gtfs$calendar))
  if(nrow(cal) > 0){
    cal$service_id <- as.character(cal$service_id)
    cal$start_date <- as.Date(cal$start_date)
    cal$end_date <- as.Date(cal$end_date)
    # One row per service and date it runs on in calendar.txt terms
    base <- data.table::rbindlist(lapply(seq_along(dates), function(i){
      d <- dates[i]
      on_day <- cal[[dow[dates_dow[i]]]] == 1L &
        !is.na(cal$start_date) & !is.na(cal$end_date) &
        cal$start_date <= d & cal$end_date >= d
      on_day[is.na(on_day)] <- FALSE
      if(!any(on_day)) return(NULL)
      data.table::data.table(service_id = cal$service_id[on_day], date = d)
    }))
  } else {
    base <- NULL
  }
  if(is.null(base) || nrow(base) == 0){
    base <- data.table::data.table(service_id = character(0),
                                   date = as.Date(character(0)))
  }
  base <- unique(base)

  cd <- gtfs$calendar_dates
  if(!is.null(cd) && nrow(cd) > 0){
    cd <- data.table::data.table(service_id = as.character(cd$service_id),
                                 date = as.Date(cd$date),
                                 exception_type = as.integer(cd$exception_type))
    cd <- cd[cd$date >= startdate & cd$date <= enddate, ]
    cd <- unique(cd)
    rem <- cd[cd$exception_type == 2L, c("service_id", "date")]
    add <- cd[cd$exception_type == 1L, c("service_id", "date")]
    # A removal of a date the service does not run on matches nothing, and an
    # addition of a date it already runs on is absorbed by unique()
    if(nrow(rem) > 0) base <- base[!rem, on = c("service_id", "date")]
    if(nrow(add) > 0) base <- unique(data.table::rbindlist(list(base, add)))
  }
  base
}


#' Times each trip runs between two dates, by the GTFS rules
#'
#' @param gtfs GTFS object, trimmed or not
#' @param startdate,enddate the dates, as Date
#' @return data.table of trip_id and runs_total
#' @noRd
trip_runs_between <- function(gtfs, startdate, enddate){
  base <- service_days_between(gtfs, startdate, enddate)

  svc <- base[, list(runs_total = .N), by = "service_id"]
  trips <- data.table::data.table(trip_id = as.character(gtfs$trips$trip_id),
                                  service_id = as.character(gtfs$trips$service_id))
  trips <- svc[trips, on = "service_id"]
  trips$runs_total[is.na(trips$runs_total)] <- 0

  # Frequency-based trips represent multiple departures per day
  if(!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0){
    message("Scaling frequency-based trips by departures per day")
    freq_runs <- data.table::as.data.table(frequency_runs_per_trip(gtfs$frequencies))
    trips <- freq_runs[trips, on = "trip_id"]
    trips$freq_runs[is.na(trips$freq_runs)] <- 1
    trips$runs_total <- trips$runs_total * trips$freq_runs
  }
  trips[, c("trip_id", "runs_total")]
}


#' Trim a GTFS file between two dates
#'
#' @param gtfs GTFS object from gtfs_read()
#' @param startdate Start date
#' @param enddate End date
#' @return a gtfs object trimmed to services running between the two dates
#' @details The GTFS specification allows a `service_id` to be defined in
#'   `calendar_dates.txt` alone, with no row in `calendar.txt`; the service
#'   then runs on exactly the dates listed with `exception_type = 1`. Such
#'   services are kept if any of those dates falls inside the window. The
#'   DfT's Bus Open Data Service GTFS uses them heavily (for example for
#'   school-holiday timetables), so dropping them silently removes real
#'   service.
#'
#'   Exceptions follow the GTFS rules rather than the calendar row's date
#'   range. An addition (`exception_type = 1`) inside the window is kept even
#'   when it falls outside its service's own `start_date`-`end_date`: GTFS adds
#'   the date regardless. A service whose `calendar.txt` row does not reach
#'   the window at all, but which adds dates inside it, is kept too. Its row is
#'   dropped, so it comes out as a `calendar_dates.txt`-only service running
#'   on exactly those dates. A removal (`exception_type = 2`) is still clipped
#'   to the row's range, because outside it the service does not run and the
#'   removal does nothing.
#'
#'   Until version 0.4.0 both kinds of addition were discarded.
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

  # service_ids that exist only in calendar_dates.txt, which the date range in
  # calendar.txt therefore cannot be used to trim
  dates_only <- character()
  if(!is.null(calendar_dates)){
    dates_only <- setdiff(unique(as.character(calendar_dates$service_id)),
                          unique(as.character(calendar$service_id)))
  }

  all_cal <- unique(as.character(calendar$service_id))
  calendar <- calendar[calendar$start_date <= enddate,]
  calendar <- calendar[calendar$end_date >= startdate,]

  calendar$start_date <- dplyr::if_else(calendar$start_date < startdate,
                                        startdate,
                                        calendar$start_date)
  calendar$end_date <- dplyr::if_else(calendar$end_date > enddate,
                                      enddate,
                                      calendar$end_date)

  # Services whose calendar.txt row does not reach the window. Their row says
  # nothing about the window, but an addition inside it still runs them, so
  # they are treated like calendar_dates-only services from here on.
  row_outside <- setdiff(all_cal, as.character(calendar$service_id))

  keep_dates_only <- character()
  if(!is.null(calendar_dates)){
    calendar_dates <- calendar_dates[calendar_dates$date >= startdate,]
    calendar_dates <- calendar_dates[calendar_dates$date <= enddate,]
    cd_sid <- as.character(calendar_dates$service_id)

    # A service with no row in the window runs on the dates it adds; if none
    # of them is in the window it is outside the window and goes.
    no_row <- c(dates_only, row_outside)
    keep_dates_only <- unique(cd_sid[cd_sid %in% no_row &
                                       calendar_dates$exception_type == 1])

    # Additions are kept for every service kept, wherever they fall in the
    # window. Removals only matter on dates the row runs, so they are kept
    # only for services with a row, and only inside that row's (clipped)
    # range: a removal outside it is a no-op in GTFS, but counting that works
    # by weekday, such as gtfs_trips_per_zone(), would subtract it.
    is_add <- calendar_dates$exception_type == 1 &
      cd_sid %in% c(as.character(calendar$service_id), keep_dates_only)
    adds <- calendar_dates[is_add, ]

    rems <- calendar_dates[calendar_dates$exception_type == 2 &
                             cd_sid %in% as.character(calendar$service_id), ]
    rems <- dplyr::left_join(rems,
                             calendar[,c("service_id", "start_date", "end_date")],
                             by = "service_id")
    rems <- rems[rems$date >= rems$start_date & rems$date <= rems$end_date, ]
    rems$start_date <- NULL
    rems$end_date <- NULL

    # Any other exception_type is invalid GTFS and is dropped, as before.
    # The join appended start_date and end_date and they are gone again, so
    # the two have the same columns in the same order. (Not rems[, names(adds)]:
    # gtfs_read() returns data.tables, where that is an expression yielding the
    # names themselves, not a column selection.)
    calendar_dates <- rbind(adds, rems)
  }

  trips <- trips[trips$service_id %in% c(calendar$service_id, keep_dates_only), ]
  stop_times <- stop_times[stop_times$trip_id %in% trips$trip_id,]

  gtfs$stop_times <- stop_times
  gtfs$trips <- trips
  gtfs$calendar <- calendar
  gtfs$calendar_dates <- calendar_dates

  # keep frequencies, shapes and the other optional tables consistent with
  # the trips that survived the date trim
  gtfs <- gtfs_prune_orphans(gtfs)

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

  # Runs of each service on each weekday, counted date by date. This used to
  # take the weekday counts from calendar.txt and then apply calendar_dates
  # by weekday: a cancellation counted where the row ran that weekday, an
  # addition only where it did not. That is right only when the row covers the
  # whole window. An addition that falls after the row's end_date, on a
  # weekday the row flags, was discarded as a no-op though the service does
  # not otherwise run that day. service_days_between() asks the question date
  # by date and so has no such case.
  wd_lab <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
  sdays <- service_days_between(gtfs, startdate, enddate)
  sdays$weekday <- wd_lab[lubridate::wday(sdays$date, week_start = 1)]
  wruns <- sdays[, list(n = .N), by = c("service_id", "weekday")]
  wruns <- if(nrow(wruns) > 0){
    data.table::dcast(wruns, service_id ~ weekday, value.var = "n", fill = 0)
  } else {
    data.table::data.table(service_id = character(0))
  }
  for(d in wd_lab){
    if(!d %in% names(wruns)) wruns[[d]] <- 0
  }
  wruns <- as.data.frame(wruns)[, c("service_id", wd_lab)]
  names(wruns) <- c("service_id", paste0("runs_", wd_lab))

  # Add Modes
  if(by_mode){
    routes <- gtfs$routes[,c("route_id","route_type")]
    gtfs$trips <- dplyr::left_join(gtfs$trips, routes, by = "route_id")
    rm(routes)
  }

  #Join to Trips
  gtfs$trips$service_id <- as.character(gtfs$trips$service_id)
  trips <- dplyr::left_join(gtfs$trips, wruns, by = "service_id")
  rm(wruns, sdays)
  trips = as.data.frame(trips)
  for(d in wd_lab){
    v <- trips[[paste0("runs_", d)]]
    v[is.na(v)] <- 0
    trips[[paste0("runs_", d)]] <- v
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






