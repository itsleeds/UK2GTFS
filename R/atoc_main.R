#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param stop_times stop-times
#' @param stops stops data.frame
#' @param schedule list of dataframes
#' @param silent logical
#' @param ncores number of cores to use
#' @param public_only filters to services / calls that are public pickup/set down only
#' @noRd
#'
schedule2routes <- function(stop_times, stops, schedule, silent = TRUE, ncores = 1, public_only=TRUE) {


  ### SECTION 1: ###############################################################################
  # make stop_times.txt
  if (!silent) {
    message(paste0(Sys.time(), " Building stop_times"))
  }

  stop_times <- fixStopTimeData( stop_times )

  # Convert Activity to pickup_type and drop_off_type
  upoffs <- clean_activities2(stop_times$Activity, public_only=public_only)
  stop_times <- cbind(stop_times, upoffs)

  stop_times <- stop_times[, c("arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "rowID", "schedule")]

  if (public_only)
  {
    #remove calls that don't allow the public to board or alight (type==1)
    stop_times <- stop_times[!(stop_times$pickup_type == 1 & stop_times$drop_off_type == 1), ]
  }


  ### SECTION 2: ###############################################################################
  # make make the calendar.txt and calendar_dates.txt file from the schedule
  if (!silent) {
    message(paste0(Sys.time(), " Building calendar and calendar_dates"))
  }

  # build the calendar file
  res <- makeCalendar(schedule = schedule, ncores = ncores)
  calendar <- res[[1]]
  cancellation_dates <- res[[2]]
  rm(res)

  #remove columns we don't need any more
  schedule <- schedule[, c(
    "Train UID", "Train Status", "Train Category",
    "Headcode", "rowID", "ATOC Code", "Retail Train ID", "Power Type", "Train Identity"
  )]

  #making the calendar will duplicate rows where the base timetable has been layered over with cancellations etc.
  #join back to the original to get the extra columns not returned from makeCalendar
  calendar <- dplyr::left_join(calendar, schedule, by = c("rowID"))

  gc()

  # In CIF land, one service ID can have multiple operating patterns, expressed as multiple BS records (and their associated LO,LI,LT records) having the same UID
  # that's the opposite of the GTFS concept where a single calendar (operating pattern) can be used by multiple trips, but a single trip can only have one operating pattern.
  # The CIF concept of a single UID maps more closely onto GTFS routes.
  #
  # so we maintain a 1:1 relationship between Trips and Calendars, duplicating CIF service IDs as required to represent the multiplicity of possible operating patterns.
  #
  # see merging code where there is functionality to de-duplicate calendar patterns down to a unique set. (condenseServicePatterns)


  # clean calendars
  names(calendar)[names(calendar) == "UID"] <- "service_id"
  calendar <- formatAttributesToGtfsSchema( calendar )

  cancellation_dates <- cancellation_dates[, c("UID", "start_date")]
  names(cancellation_dates) <- c("service_id", "date")
  cancellation_dates <- formatAttributesToGtfsSchema( cancellation_dates )
  cancellation_dates$exception_type <- 2 # all events passed to cancellation_dates are single day cancellations



  ### SECTION 3: ###############################################################################

  calendar$trip_id <- 1:nrow(calendar)
  # calendar$service_id = 1:nrow(calendar)



  # When splitting the calendar rowIDs are duplicated
  # so create new system of trip_ids and duplicate the relevant stop_times
  if (!silent) {
    message(paste0(Sys.time(), " Duplicating necessary stop times"))
  }

  #TODO if ncores > 1 this takes forever - the data being joined must somehow trigger massive memory copying ?
  #also sets the trip_id on stop_times
  stop_times <- duplicate_stop_times(calendar = calendar, stop_times = stop_times, ncores = 1)


  ### SECTION 5: ###############################################################################
  # make make the trips.txt  file by matching the calendar to the stop_times

  trips <- calendar[, c("service_id", "trip_id", "rowID", "ATOC Code", "Train Status", "Train Category", "Power Type", "Train Identity")]
  trips <- longnames(routes = trips, stop_times = stop_times, stops=stops)


  # Fix Times (and remove some fields)
  stop_times <- afterMidnight(stop_times)


  ### SECTION 4: ###############################################################################
  # make the routes.txt
  # a route is all the trips with a common start and end
  # i.e. schedules original UID
  if (!silent) {
    message(paste0(Sys.time(), " Building routes.txt"))
  }

  #do the conversion to route_type before grouping because several statuses map to the same route_type and we get 'duplicate' routes that look the same.
  train_status <- data.table::data.table(
    train_status = c("B", "F", "P", "S", "T", "1", "2", "3", "4", "5"),
    route_type = c(   3,   NA,  2,   4,   NA,  2,   NA,  NA,  4,   3),
    stringsAsFactors = FALSE
  )

  trips$`Train Status` <- as.character(trips$`Train Status`)
  trips <- dplyr::left_join(trips, train_status, by = c("Train Status" = "train_status"))
  rm(train_status)

  trips$route_type[trips$`Train Category` %in% c("EL", "OL") & trips$route_type == 2 ] <- 1
  # London Underground is Metro (unless already identified as a bus/ship etc)
  # "OL" is also used for Tyne & Wear metro

  routes <- trips

  routes <- dplyr::group_by(routes, `ATOC Code`, route_long_name, route_short_name, `Train Category`, route_type )
  routes <- dplyr::summarise(routes)
  routes$route_id <- 1:nrow(routes)

  #join route_id back into trip table
  trips <- dplyr::left_join(trips, routes, by = c("ATOC Code", "route_long_name", "route_short_name", "Train Category", "route_type"))

  routes <- routes[, c("route_id", "route_type", "ATOC Code", "route_short_name", "route_long_name", "Train Category" )]
  names(routes) <- c("route_id", "route_type", "agency_id", "route_short_name", "route_long_name", "train_category" )



  ### Section 6: #######################################################
  # Final Checks



  #gtfs systems tend to be tolerant of additional fields, so expose the train_category and power_type so that the consumer can do analysis on them.
  #e.g. filter out ECS moves

  #https://developers.google.com/transit/gtfs/reference#tripstxt 'trip_short_name'
  #"Public facing text used to identify the trip to riders, e.g.to identify train numbers for commuter rail trips.
  #If provided, should uniquely identify a trip within a service day; it should not be used for destination names or limited/express designations."
  #best mapping from alpha headcode onto standard GTFS field is 'trip_short_name' since it's unique within a single day.

  # Ditch unnecessary columns
  routes <- routes[, c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type", "train_category")]
  trips <- trips[, c("trip_id", "route_id", "service_id", "Train Identity", "Power Type")]
  names(trips) <- c("trip_id", "route_id", "service_id", "trip_short_name", "power_type")
  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type")]
  calendar <- calendar[, c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]


  # end of function
  timetables <- list(calendar, cancellation_dates, routes, stop_times, trips)
  names(timetables) <- c("calendar", "calendar_dates", "routes", "stop_times", "trips")

  return(timetables)
}



