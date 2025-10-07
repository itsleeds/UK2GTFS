#' Export ATOC stations as GTFS stops.txt
#'
#' @details
#' Export ATOC stations as GTFS stops.txt
#'
#' @param station station SF data frame from the importMSN function
#' @param TI TI object from the importMCA function
#' @family atoc
#' @export
#'
station2stops <- function(station, TI) {

  # Discard Unneded Columns
  TI <- TI[, c("TIPLOC code", "NALCO", "TPS Description", "CRS Code")]
  station <- station[, c(
    "Station Name", "CATE Interchange status", "TIPLOC Code",
    "CRS Code", "geometry"
  )]

  jnd <- dplyr::left_join(TI, station, by = c("TIPLOC code" = "TIPLOC Code"))
  station.extra <- station[!station$`TIPLOC Code` %in% jnd$`TIPLOC code`, ]
  station.extra$`TIPLOC code` <- station.extra$`TIPLOC Code`
  station.extra$NALCO <- NA
  station.extra$`CRS Code.y` <- station.extra$`CRS Code`
  station.extra$`TPS Description` <- NA
  station.extra$`CRS Code.x` <- NA
  station.extra <- station.extra[, names(jnd)]
  jnd <- suppressWarnings(dplyr::bind_rows(jnd, station.extra))

  jnd$geometry <- sf::st_sfc(jnd$geometry)
  jnd <- sf::st_sf(jnd)
  sf::st_crs(jnd) <- 4326

  jnd$CRS <- ifelse(is.na(jnd$`CRS Code.y`), jnd$`CRS Code.x`,
    jnd$`CRS Code.y`
  )
  jnd$name <- ifelse(is.na(jnd$`TPS Description`), jnd$`Station Name`,
    jnd$`TPS Description`
  )

  stops <- jnd[, c("CRS", "TIPLOC code", "name")]
  stops <- stops[!sf::st_is_empty(stops), ]

  stops.final <- stops

  stops.final <- as.data.frame(stops.final)
  stops.final$geometry <- sf::st_sfc(stops.final$geometry)
  stops.final <- sf::st_sf(stops.final)
  sf::st_crs(stops.final) <- 4326
  stops.final <- stops.final[, c("TIPLOC code", "CRS", "name", "geometry")]

  # recorder the match the GTFS stops.txt
  names(stops.final) <- c("stop_id", "stop_code", "stop_name", "geometry")
  coords <- sf::st_coordinates(stops.final)
  stops.final$stop_lat <- coords[, 2]
  stops.final$stop_lon <- coords[, 1]
  # sub metre precison is sufficent
  stops.final$stop_lat <- round(stops.final$stop_lat, 5)
  stops.final$stop_lon <- round(stops.final$stop_lon, 5)
  stops.final <- as.data.frame(stops.final)
  stops.final$geometry <- NULL

  # Built tiploc to CRS lookup
  lookup <- as.data.frame(jnd)
  lookup <- lookup[, c("TIPLOC code", "CRS")]
  lookup$match <- ifelse(is.na(lookup$CRS), lookup$`TIPLOC code`, lookup$CRS)
  lookup <- lookup[, c("TIPLOC code", "match")]
  names(lookup) <- c("TIPLOC", "match")



  results <- list(stops.final, lookup)
  names(results) <- c("stops", "lookup")
  return(results)
}


#' Export ATOC stations and FLF file as transfers.txt
#'
#' @details
#' Export ATOC FLF file as transfers.txt
#'
#' @param station station SF data frame from the importMSN function
#' @param flf imported flf file from importFLF
#' @noRd
#'
station2transfers <- function(station, flf) {

  ### SECTION 4: ############################################################
  # make make the transfers.txt
  # transfer between stations are in the FLF file
  transfers1 <- flf[, c("from", "to", "time")]
  transfers1$time <- transfers1$time * 60
  transfers1$transfer_type <- 2

  # transfer within stations are in the stations file
  transfers2 <- station[, c("TIPLOC Code", "CRS Code", "Minimum Change Time")]
  transfers2 <- as.data.frame(transfers2)
  transfers2$geometry <- NULL

  transfers3 <- transfers2[, c("TIPLOC Code", "CRS Code")]
  names(transfers3) <- c("from_stop_id", "CRS Code")
  transfers1 <- dplyr::left_join(transfers1, transfers3,
    by = c("from" = "CRS Code"),
    relationship = "many-to-many"
  )
  names(transfers3) <- c("to_stop_id", "CRS Code")
  transfers1 <- dplyr::left_join(transfers1, transfers3,
    by = c("to" = "CRS Code"),
    relationship = "many-to-many"
  )
  transfers1 <- transfers1[, c(
    "from_stop_id", "to_stop_id",
    "transfer_type", "time"
  )]
  names(transfers1) <- c(
    "from_stop_id", "to_stop_id",
    "transfer_type", "min_transfer_time"
  )

  transfers2$min_transfer_time <- as.integer(transfers2$`Minimum Change Time`) * 60
  transfers2$to_stop_id <- transfers2$`TIPLOC Code`
  transfers2$transfer_type <- 2
  names(transfers2) <- c(
    "from_stop_id", "CRS Code", "Minimum Change Time",
    "min_transfer_time", "to_stop_id", "transfer_type"
  )
  transfers2 <- transfers2[, c(
    "from_stop_id", "to_stop_id", "transfer_type",
    "min_transfer_time"
  )]

  transfers <- rbind(transfers1, transfers2)
  return(transfers)
}




#' internal function for constructing longnames of routes
#'
#' @details
#' creates the long name of a route from appropriate variables
#'
#' @param routes routes data.frame
#' @param stop_times stop_times data.frame
#' @param stops stops data.frame
#' @noRd
#'
longnames <- function(routes, stop_times, stops) {

  stop_times_sub <- dplyr::group_by(stop_times, trip_id)
  stop_times_sub <- dplyr::summarise(stop_times_sub,
    schedule = unique(schedule),
    stop_id_a = stop_id[stop_sequence == 1],
    # seq = min(stop_sequence),
    stop_id_b = stop_id[stop_sequence == max(stop_sequence)]  )

  # Add names for `stop_id_[a|b]` as `stop_name_[a|b]`
  stop_times_sub <- dplyr::left_join(
    stop_times_sub,
    dplyr::rename(stops[, c("stop_id", "stop_name")], stop_name_a = stop_name),
    by = c("stop_id_a" = "stop_id"))

  stop_times_sub <- dplyr::left_join(
    stop_times_sub,
    dplyr::rename(stops[, c("stop_id", "stop_name")], stop_name_b = stop_name),
    by = c("stop_id_b" = "stop_id"))

  stop_times_sub$route_long_name <- paste0("from ",
                    ifelse( is.na(stop_times_sub$stop_name_a), stop_times_sub$stop_id_a, stop_times_sub$stop_name_a),
                                           " to ",
                    ifelse( is.na(stop_times_sub$stop_name_b), stop_times_sub$stop_id_b, stop_times_sub$stop_name_b) )

  stop_times_sub$route_long_name <- gsub(" Rail Station", "" , stop_times_sub$route_long_name)

  stop_times_sub <- stop_times_sub[!duplicated(stop_times_sub$schedule), ]
  stop_times_sub <- stop_times_sub[, c("schedule", "route_long_name")]

  routes <- dplyr::left_join(routes, stop_times_sub,
                             by = c("rowID" = "schedule"))

  #you'd expect to only have to look at category to tell if it's a ship, but in practice the category for
  #ships is NA, so we have to look at 'Train Status' too.
  routes["SS" ==`Train Category` | "S"==`Train Status` | "4"==`Train Status`,
                            route_long_name := paste("Ship",route_long_name)]
  routes[`Train Category` %in% c("BS", "BR"),
                            route_long_name := paste("Bus",route_long_name)]

  #Tyne & Wear metro is "OL" in data OL="London Underground/Metro Service"
  routes[`Train Category` %in% c("EL", "OL"),
                            route_long_name := paste("Metro",route_long_name)]
  routes[!(`Train Category` %in% c("SS", "BS", "BR", "EL", "OL") | "S"==`Train Status` | "4"==`Train Status`),
                            route_long_name := paste("Train",route_long_name)]
  return(routes)
}



#' make calendar
#'
#' @details
#' split overlapping start and end dates
#'
#' @param schedule schedule data.frame
#' @param ncores number of processes for parallel processing (default = 1)
#' @noRd
#'
makeCalendar <- function(schedule, ncores = 1) {

  #treatDatesAsInt = getOption("UK2GTFS_opt_treatDatesAsInt", default=TRUE)
  #set_TREAT_DATES_AS_INT( treatDatesAsInt )

  #tryCatch({

    # prep the inputs
    calendar <- schedule[, c("Train UID", "Date Runs From", "Date Runs To", "Days Run", "STP indicator", "rowID" )]
    names(calendar) <- c("UID", "start_date", "end_date", "Days", "STP", "rowID" )

    if(FALSE) #treatDatesAsInt ) # Disabling as causing failures
    {
      setupDatesCache( calendar )
      #treating date as int: seem to be about twice as fast on the critical line when selecting base timetable
      calendar$start_date = as.integer( calendar$start_date )
      calendar$end_date = as.integer( calendar$end_date )
    }


    okCalendarDates = validateCalendarDates( calendar )
    if ( !all( okCalendarDates ) )
    {
      message(Sys.time(), " Some calendar dates had incorrect start or end dates that did not align with operating day bitmask.\n Services=",
              paste( unique( calendar$UID[ !okCalendarDates ] ), collapse = "," ) )
    }

    #we're going to be splitting and replicating calendar entries - stash the original UID so we can join back on it later
    calendar$originalUID <- calendar$UID
    calendar$STP <- as.character(calendar$STP)
    calendar$duration <- calendar$end_date - calendar$start_date + 1


    #brutal, but makes code later on simpler, make all cancellations one day cancellations by splitting
    #TODO don't split up into one day cancellations if all the operating day patterns on a service are identical
    cancellations <- makeAllOneDay( calendar[calendar$STP == "C", ] )
    calendar <- calendar[calendar$STP != "C", ]


    #debugging option
    set_STOP_PROCESSING_UID( getOption("UK2GTFS_opt_stopProcessingAtUid") )

    message(paste0(Sys.time(), " Constructing calendar and calendar_dates"))
    calendar$`__TEMP__` <- calendar$UID
    calendar_split <- calendar[, .(list(.SD)), by = `__TEMP__`][,V1]

    if (ncores > 1) {
      future::plan(future::multisession, workers = ncores)
      res <- furrr::future_map(calendar_split, .f = makeCalendarInner, .progress = TRUE)
      future::plan(future::sequential)

    } else {
      res <- purrr::map(calendar_split, .f = makeCalendarInner, .progress = TRUE)
    }


    res.calendar <- lapply(res, `[[`, 1)
    res.calendar <- data.table::rbindlist(res.calendar, use.names=FALSE) #performance, takes 10 minutes to execute bind_rows on full GB daily timetable

    res.cancellation_dates <- lapply(res, `[[`, 2)
    res.cancellation_dates <- res.cancellation_dates[!is.na(res.cancellation_dates)]
    res.cancellation_dates <- data.table::rbindlist(res.cancellation_dates, use.names=FALSE)
    stopifnot( 0==nrow(res.cancellation_dates) )
    rm(res.cancellation_dates)
    #since we didn't pass in any cancellations we should no longer get any back

    res.calendar = splitAndRebindBitmask( res.calendar )
    cancellations = splitAndRebindBitmask( cancellations )

    #associate the split up cancellations with the (new) calendar they are associated with
    #(only works because cancellations are all one day duration)
    cancellations = allocateCancellationsAcrossCalendars( res.calendar, cancellations )

    #no longer need the field that was used to associate the original and replicated calendars together
    cancellations$originalUID <- NULL
    res.calendar$originalUID <- NULL

    #error checking
    dups = duplicated( res.calendar$UID )
    if( any(TRUE==dups) )
    {
      dups = unique( res.calendar$UID[ dups ] )

      warning(paste(Sys.time(), "Duplicate UIDs were created by the makeCalendar() process, this is likely to cause downstream proceessing errors. ",
                    "Please capture the data and raise a bug / create a test case. ", dups))
    }


    #}, finally = {
    #set_TREAT_DATES_AS_INT( FALSE )

    #revert treating date as int
  #   if( TRUE==treatDatesAsInt )
  #   {
  #     if (exists("res.calendar")){ res.calendar = makeDateFieldsDateType( res.calendar ) }
  #     if (exists("cancellations")){ cancellations = makeDateFieldsDateType( cancellations ) }
  #   }
  # })

  return(list(res.calendar, cancellations))
}


makeDateFieldsDateType<- function( cal )
{
  cal$start_date = as.Date( cal$start_date, origin = DATE_EPOC )
  cal$end_date = as.Date( cal$end_date, origin = DATE_EPOC )
  cal$duration = cal$end_date - cal$start_date + 1

  return (cal)
}


#' duplicateItem
#'
#' @details
#' Function that duplicates a data.table, adding a "index" column to all rows in the output indicating which
#' instance of the duplication the row is associated with
#'
#' @param dt data.table
#' @param reps number of duplicates to be created
#' @param indexStart starting number for the "index" value added to the item
#' @noRd
#'
duplicateItem <- function( dt, reps, indexStart=1 )
{
  if ( is.na(reps) | reps<1 ) return (NULL)

  #replicate all the rows in dt       times=reps
  duplicates <- dt[rep(seq(1, nrow(dt)), reps), ]

  #create and apply indexes to the created rows- each group of replicated rows gets the same index number.
  index <- rep(seq( indexStart, indexStart-1+reps ), nrow(dt))

  duplicates$index <- index[order(index)]

  return(duplicates)
}


#' fix times for journeys that run past midnight
#'
#' @details
#' When train runs over midnight GTFS requires the stop times to be in
#'    24h+ e.g. 26:30:00
#'
#' @param stop_times stop_times data.frame
#' @param safe logical (default = TRUE) should the check for trains
#'    running more than 24h be performed?
#'
#' @details
#' Not running the 24 check is faster, if the check is run a warning
#'    is returned, but the error is not fixed. As the longest train
#'    journey in the UK is 13 hours (Aberdeen to Penzance) this is
#'    unlikely to be a problem.
#' @noRd
#'
afterMidnight <- function(stop_times, safe = TRUE) {

  stop_times$arv <- as.integer(stop_times$arrival_time)
  stop_times$dept <- as.integer(stop_times$departure_time)

  stop_times.summary <- dplyr::group_by(stop_times, trip_id)
  stop_times.summary <- dplyr::summarise(stop_times.summary,
    dept_first = dept[stop_sequence == min(stop_sequence)]
  )

  stop_times <- dplyr::left_join(stop_times, stop_times.summary, by = "trip_id")
  stop_times$arvfinal <- ifelse(stop_times$arv < stop_times$dept_first, stop_times$arv + 240000, stop_times$arv)
  stop_times$depfinal <- ifelse(stop_times$dept < stop_times$dept_first, stop_times$dept + 240000, stop_times$dept)


  if (safe) {
    # check if any train more than 24 hours
    stop_times.summary2 <- dplyr::group_by(stop_times, trip_id)
    stop_times.summary2 <- dplyr::summarise(stop_times.summary2,
      arv_last = arvfinal[stop_sequence == max(stop_sequence)],
      arv_max = max(arvfinal, na.rm = TRUE)
    )

    check <- stop_times.summary2$arv_last < stop_times.summary2$arv_max
    if (any(check)) {
      warning("24 hour clock correction will return false results for any trip where total travel time exceeds 24 hours")
    }
  }

  numb2time2 <- function(dt, colNameDest, colNameSource){
    #performance, substr is relatively expensive
    data.table::set(dt, j=colNameDest, value= sprintf("%02d:%02d:%02d",
              dt[[colNameSource]] %/% 10000, (dt[[colNameSource]] %/% 100) %% 100, dt[[colNameSource]] %% 100) )
  }

  numb2time2(stop_times, "arrival_time", "arvfinal")
  numb2time2(stop_times, "departure_time", "depfinal")

  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time",
                               "stop_id", "stop_sequence", "pickup_type",
                               "drop_off_type")]
  return(stop_times)
}


#' Clean Activities
#' @param x character activities
#' @details
#' Change Activities code to pickup and drop_off
#' https://wiki.openraildata.com//index.php?title=Activity_codes
#'
#' @noRd
#'
clean_activities2 <- function(x, public_only = TRUE) {

  x <- data.frame(activity = x, stringsAsFactors = FALSE)

  if (public_only)
  {
    utils::data(activity_codes)

    x <- dplyr::left_join(x, activity_codes, by = c("activity"))
    if (anyNA(x$pickup_type)) {
      mss <- unique(x$activity[is.na(x$pickup_type)])
      warning("Unknown Activity codes '", paste(unique(mss), collapse = "' '"), "' please report these codes as a GitHub Issue")
      x$pickup_type[is.na(x$pickup_type)] <- 0
      x$drop_off_type[is.na(x$drop_off_type)] <- 0
    }
  }
  else #set all of the stops on a route to be valid for passenger boarding / alighting from a GTFS perspective
       # (unless they are 'pass' which have no 'activity' as they woosh past - just like deadlines.)
  {
    x$pickup_type <- ifelse( is.na(x$activity), 1, 0 )
    x$drop_off_type <- ifelse( is.na(x$activity), 1, 0 )
  }

  x <- x[, c("pickup_type", "drop_off_type")]

  return(x)
}
