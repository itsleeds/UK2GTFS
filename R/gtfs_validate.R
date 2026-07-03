#' Validate a GTFS object (in R)
#'
#' Does some basic checks on the validity of the GTFS object
#' @param gtfs a gtfs object
#' @return Invisibly returns NULL, called for its printed messages
#' @export

gtfs_validate_internal <- function(gtfs) {
  # Basic checks
  # Rows
  if (nrow(gtfs$agency) < 1) {
    message("No rows in agency")
  }
  if (nrow(gtfs$stops) < 1) {
    message("No rows in stops")
  }
  if (nrow(gtfs$routes) < 1) {
    message("No rows in routes")
  }
  if (nrow(gtfs$trips) < 1) {
    message("No rows in trips")
  }
  if (nrow(gtfs$stop_times) < 1) {
    message("No rows in stop_times")
  }
  if (nrow(gtfs$calendar) < 1) {
    message("No rows in calendar")
  }

  # Columns
  # Agency
  nm_agency <- names(gtfs$agency)
  nm_agency_req <- c("agency_id","agency_name","agency_url",
                     "agency_timezone")
  nm_agency_op <- c("agency_lang","agency_phone","agency_fare_url",
                     "agency_email")
  if (!all(nm_agency_req %in% nm_agency)) {
    message(paste(nm_agency_req[!nm_agency_req %in% nm_agency],
                  collapse = ", ") ,"are required columns missing from agency.txt")
  }

  if (!all(nm_agency %in% c(nm_agency_req, nm_agency_op))) {
    message(paste(nm_agency[!nm_agency %in% c(nm_agency_req, nm_agency_op)],
                  collapse = ", ") ,"are invalid columns in agency.txt")
  }

  # Stops
  nm_stops <- names(gtfs$stops)
  nm_stops_req <- c("stop_id","stop_name","stop_lat",
                     "stop_lon")
  nm_stops_op <- c("stop_code","stop_desc","stop_url",
                    "stops_email","zone_id","location_type",
                   "parent_station","stop_timezone","wheelchair_boarding",
                   "level_id","platform_code")
  if (!all(nm_stops_req %in% nm_stops)) {
    message(paste(nm_stops_req[!nm_stops_req %in% nm_stops],
                  collapse = ", ") ," are required columns missing from stops.txt")
  }

  if (!all(nm_stops %in% c(nm_stops_req, nm_stops_op))) {
    message(paste(nm_stops[!nm_stops %in% c(nm_stops_req, nm_stops_op)],
                  collapse = ", ") ," are invalid columns in stops.txt")
  }

  # Routes
  nm_routes <- names(gtfs$routes)
  nm_routes_req <- c("route_id","agency_id","route_short_name",
                     "route_long_name","route_type")
  nm_routes_op <- c("route_desc","route_url","route_color",
                    "route_sort_order","continuous_pickup",
                    "continuous_drop_off")
  if (!all(nm_routes_req %in% nm_routes)) {
    message(paste(nm_routes_req[!nm_routes_req %in% nm_routes],
                  collapse = ", ") ," are required columns missing from routes.txt")
  }

  if (!all(nm_routes %in% c(nm_routes_req, nm_routes_op))) {
    message(paste(nm_routes[!nm_routes %in% c(nm_routes_req, nm_routes_op)],
                  collapse = ", ") ," are invalid columns in routes.txt")
  }

  # Trips
  nm_trips <- names(gtfs$trips)
  nm_trips_req <- c("route_id","service_id","trip_id")
  nm_trips_op <- c("trip_headsign","trip_short_name","direction_id",
                   "block_id","shape_id","wheelchair_accessible",
                   "bikes_allowed")
  if (!all(nm_trips_req %in% nm_trips)) {
    message(paste(nm_trips_req[!nm_trips_req %in% nm_trips],
                  collapse = ", ") ," are required columns missing from trips.txt")
  }

  if (!all(nm_trips %in% c(nm_trips_req, nm_trips_op))) {
    message(paste(nm_trips[!nm_trips %in% c(nm_trips_req, nm_trips_op)],
                  collapse = ", ") ," are invalid columns in trips.txt")
  }

  if (any(duplicated(gtfs$trips$trip_id))) {
    message("Duplicated trip_id in trips:")
    message(gtfs$trips$trip_id[duplicated(gtfs$trips$trip_id)])
  }

  # stop_times
  nm_stop_times <- names(gtfs$stop_times)
  nm_stop_times_req <- c("trip_id","arrival_time","departure_time",
                         "stop_id","stop_sequence")
  nm_stop_times_op <- c("stop_headsign","pickup_type","drop_off_type",
                        "continuous_pickup","continuous_drop_off",
                        "shape_dist_traveled","timepoint")
  if (!all(nm_stop_times_req %in% nm_stop_times)) {
    message(paste(nm_stop_times_req[!nm_stop_times_req %in% nm_stop_times],
                  collapse = ", ") ," are required columns missing from stop_times.txt")
  }

  if (!all(nm_stop_times %in% c(nm_stop_times_req, nm_stop_times_op))) {
    message(paste(nm_stop_times[!nm_stop_times %in% c(nm_stop_times_req, nm_stop_times_op)],
                  collapse = ", ") ," are invalid columns in stop_times.txt")
  }

  # calendar
  nm_calendar <- names(gtfs$calendar)
  nm_calendar_req <- c("service_id","monday","tuesday","wednesday","thursday",
                       "friday","saturday","sunday","start_date","end_date")
  if (!all(nm_calendar_req %in% nm_calendar)) {
    message(paste(nm_calendar_req[!nm_calendar_req %in% nm_calendar],
                  collapse = ", ") ," are required columns missing from calendar.txt")
  }

  if (!all(nm_calendar %in% c(nm_calendar_req))) {
    message(paste(nm_calendar[!nm_calendar %in% c(nm_calendar_req)],
                  collapse = ", ") ," are invalid columns in calendar.txt")
  }

  # calendar_dates
  if(!is.null(gtfs$calendar_dates)){
    nm_calendar_dates <- names(gtfs$calendar_dates)
    nm_calendar_dates_req <- c("service_id","date","exception_type")
    if (!all(nm_calendar_dates_req %in% nm_calendar_dates)) {
      message(paste(nm_calendar_dates_req[!nm_calendar_dates_req %in% nm_calendar_dates],
                    collapse = ", ") ," are required columns missing from calendar_dates.txt")
    }

    if (!all(nm_calendar_dates %in% c(nm_calendar_dates_req))) {
      message(paste(nm_calendar_dates[!nm_calendar_dates %in% c(nm_calendar_dates_req)],
                    collapse = ", ") ," are invalid columns in calendar_dates.txt")
    }
  } else {
    message("No calendar_dates.txt")
  }

  # check for NAs
  if(anyNA(gtfs$agency)){
    message("NA values in agency")
  }

  if(anyNA(gtfs$stops)){
    message("NA values in stops")
  }

  if(anyNA(gtfs$routes)){
    message("NA values in routes")
  }

  if(anyNA(gtfs$trips)){
    message("NA values in trips")
  }

  if(anyNA(gtfs$stop_times)){
    message("NA values in stop_times")
  }

  if(anyNA(gtfs$calendar)){
    message("NA values in calendar")
  }

  if(anyNA(gtfs$calendar_dates)){
    message("NA values in calendar_dates")
  }

  # Check for missing values
  if (!all(gtfs$routes$agency_id %in% gtfs$agency$agency_id)) {
    unknown = unique(gtfs$routes$agency_id[!(gtfs$routes$agency_id %in% gtfs$agency$agency_id)])
    message("Unknown agency_id in routes: (", length(unknown), ") ", paste(unknown, collapse=" ") )
  }

  if (!all(gtfs$stop_times$trip_id %in% gtfs$trips$trip_id)) {
    unknown = unique(gtfs$stop_times$trip_id[!(gtfs$stop_times$trip_id %in% gtfs$trips$trip_id)])
    message("Unknown trip_id in stop_times: (", length(unknown), ") values:")
    message( paste(unknown, collapse=" ") )
  }

  if (!all(gtfs$stop_times$stop_id %in% gtfs$stops$stop_id)) {
    unknown = unique(gtfs$stop_times$stop_id[!(gtfs$stop_times$stop_id %in% gtfs$stops$stop_id)])
    message("Unknown stop_id in stop_times: (", length(unknown), ") values: (TIPLOC data may need refreshing)")
    message( paste(unknown, collapse=" ") )
  }

  # Duplicated IDs
  if (any(duplicated(gtfs$agency$agency_id))) {
    unknown = unique(gtfs$agency$agency_id[duplicated(gtfs$agency$agency_id)])
    message("Duplicated agency_id in agency: (", length(unknown), ") ", paste(unknown, collapse=" ") )
  }

  if (any(duplicated(gtfs$stops$stop_id))) {
    unknown = unique(gtfs$stops$stop_id[duplicated(gtfs$stops$stop_id)])
    message("Duplicated stop_id in stops: (", length(unknown), ") values:")
    message( paste(unknown, collapse=" ") )
  }

  if (any(duplicated(gtfs$trips$trip_id))) {
    unknown = unique(gtfs$trips$trip_id[duplicated(gtfs$trips$trip_id)])
    message("Duplicated trip_id in trips: (", length(unknown), ") values:")
    message( paste(unknown, collapse=" "))
  }

  if (any(duplicated(gtfs$routes$route_id))) {
    unknown = unique(gtfs$routes$route_id[duplicated(gtfs$routes$route_id)])
    message("Duplicated route_id in routes: (", length(unknown), ") values:")
    message( paste(unknown, collapse=" "))
  }


}

#' Validate GTFS with Google Validator
#' @param path_gtfs path to gtfs.zip file
#' @param path_validator path to google validator
#' @noRd
gtfs_validate_external <- function(path_gtfs, path_validator) {

}


#' Force a GTFS to be valid by removing problems
#' @param gtfs gtfs object
#' @details
#' Actions performed
#' 1. Remove stops with missing location
#' 2. Remove routes that don't exist in agency
#' 3. Remove trips that don't exist in routes
#' 4. Remove stop_times(calls) that don't exist in trips
#' 5. Remove stop_times(calls) that don't exist in stops
#' 6. Remove Calendar that have service_id that doesn't exist in trips
#' 7. Remove Calendar_dates that have service_id that doesn't exist in trips
#'
#' @return a gtfs object
#' @export
gtfs_force_valid <- function(gtfs) {
  message("This function does not fix problems it just removes them")

  # 1. Stops with missing lat/lon
  gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon) & !is.na(gtfs$stops$stop_lat),]

  # 2. Routes that have agency_id that doesn't exist in agency
  gtfs$routes <- gtfs$routes[gtfs$routes$agency_id %in%  gtfs$agency$agency_id,]

  # 3. Trips that have route_id that doesn't exist in route
  gtfs$trips <- gtfs$trips[gtfs$trips$route_id %in%  gtfs$routes$route_id,]

  # 4. Stop Times that have trip_id that doesn't exist in trips
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$trip_id %in% gtfs$trips$trip_id,]

  # 5. Stop Times that have stops_id that doesn't exist in stops
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in%  gtfs$stops$stop_id,]

  # 6. Calendar that have service_id that doesn't exist in trip
  gtfs$calendar <- gtfs$calendar[gtfs$calendar$service_id %in%  gtfs$trips$service_id,]

  # 7. Calendar_dates that have service_id that doesn't exist in trip
  gtfs$calendar_dates <- gtfs$calendar_dates[gtfs$calendar_dates$service_id %in%  gtfs$trips$service_id,]

  return(gtfs)
}
