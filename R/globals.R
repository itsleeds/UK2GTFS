# Fix for no variable bindings when using dplyr
utils::globalVariables(c(
  "trip_id", "dept", "stop_sequence",
  "arvfinal", "tiplocs", "atoc_agency", "calendar_dates",
  "activity_codes", "rowID", "trips", "minute",
  "second", "naptan_missing", "n", "arrival_time", "nstops",
  "stop_id", "exception_type", "start_date", "end_date",
  "monday", "tuesday", "wednesday", "thursday", "friday",
  "saturday", "sunday", "pattern", "schedule", "ATOC Code",
  "route_long_name", "Train Status", "i", "DaysOfWeek",
  'speed','agency_id','agency_name', 'agency_url','agency_timezone',
  'agency_lang','agency_id', 'Freq', 'operator_code','route_id',
  'UID','hash','vehicle_type','running_board','service_number',
  'speed_after','distance','school_terms','distance_after','historic_bank_holidays',
  'runs_monday','runs_tuesday','runs_wednesday','runs_thursday','runs_friday',
  'runs_saturday','runs_sunday', 'total_sunday',
  'runs_Mon','runs_Tue','runs_Wed','runs_Thu','runs_Fri',
  'runs_Sat','runs_Sun',
  'tot_Mon','tot_Tue','tot_Wed','tot_Thu','tot_Fri',
  'tot_Sat','tot_Sun',
  'zone_id','time_bands',
  '%>%', '.', 'Activity', 'Arrival Time', 'Departure Time', 'N', 'Public Arrival Time',
  'Public Departure Time','STP', 'Scheduled Arrival Time', 'Scheduled Departure Time',
  'Train Category', 'V1', '_TEMP_', '__TEMP__', 'duration',
  'i.friday', 'i.monday', 'i.saturday', 'i.sunday', 'i.thursday', 'i.tuesday', 'i.wednesday', 'originalUID',
  'route_id_new', 'route_type', 'service_id', 'service_id_new', 'stop_name', 'trip_id_new',
  'runs_total','weekday','band_hours','routes',
  # rail fares (atoc_fares_import.R / atoc_fares_gtfs.R)
  'update_marker', 'ticket_code', 'cluster_id', 'cluster_nlc', 'nlc', 'crs',
  'group_nlc', 'member_crs', 'group_uic', 'composite_indicator',
  'status_code', 'discount_category', 'railcard_code', 'route_code',
  'ticket_class', 'ticket_type', 'ticket_group', 'package_mkr',
  'max_passengers', 'flow_id', 'fare', 'child_fare', 'adult_fare',
  'direction', 'origin', 'destination', 'origin_crs', 'destination_crs',
  'fare_id', 'fare_product_id', 'max_flat', 'lower_min', 'higher_min',
  'code', 'endpoint_name',
  'cf_mkr', 'out_ret', 'matches', 'active', 'active_hd', 'sequence_no',
  'time_from', 'time_to', 'minutes_from', 'minutes_to', 'min_fare_flag',
  'arr_dep_via', 'location', 'row_id', 'date_from', 'date_to', 'days',
  'restriction_code', 'description', 'rider_category_id',
  'is_default_fare_category',
  # NeTEx fares (netex_fares_match.R / netex_fares_report)
  'agency_key', 'op_key', 'idx', 'line_public_code', 'route_short_name',
  'operator_noc', 'matched', 'i.route_id', 'x.route_id',
  # stops_per_week_functions.R
  'day_flag', 'TMP_dep', 'n_departures'
))



#' UK2GTFS option stopProcessingAtUid
#' @description sets/gets a UID value at which processing will stop - used for debugging
#' @param value option value to be set (char)
#' @details If no value passed in will return the current setting of the option. (Usually NULL)
#'   If value passed in, timetable build processing will stop in atoc_overlay.makeCalendarInner()
#'   when an exact match for that value is encountered.
#'
#'   THIS ONLY WORKS WITH ncores==1
#'
#' @return the current option value when called with no arguments, otherwise
#'   the result of setting the option
#' @export
UK2GTFS_option_stopProcessingAtUid <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_stopProcessingAtUid", default=NULL) )
  }
  else
  {
    if ( !is.null(value) && !inherits(value, "character") ){ value = as.character( value ) }

    if ( !is.null(value) && 0==nchar(value) ){ value=NULL }

    # setting this option is the documented purpose of the function; the
    # previous value is returned invisibly (as options() does) so the caller
    # can restore it
    return( invisible(options(UK2GTFS_opt_stopProcessingAtUid = value )) )
  }
}




#' UK2GTFS option treatDatesAsInt
#' @description sets/gets a logical value which determines how dates are processed while building calendar - used for debugging
#' @param value option value to be set (logical)
#' @details In the critical part of timetable building, handling dates as dates is about half the speed of handling as int
#'   so we treat them as integers. However that's a complete pain for debugging, so make it configurable.
#'   if errors are encountered during the timetable build phase, try setting this value to FALSE
#'
#' @return the current option value when called with no arguments, otherwise
#'   the result of setting the option
#' @export
UK2GTFS_option_treatDatesAsInt <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_treatDatesAsInt", default=TRUE) )
  }
  else
  {
    return( invisible(options(UK2GTFS_opt_treatDatesAsInt = as.logical(value) )) )
  }
}



#' UK2GTFS option updateCachedDataOnLibaryLoad
#' @description sets/gets a logical value which determines if the data cached in the library is checked for update when loaded
#' @param value option value to be set (logical)
#' @details when child processes are initialised we want to suppress this check, so it is also used for that purpose
#'
#' @return the current option value when called with no arguments, otherwise
#'   the result of setting the option
#' @export
UK2GTFS_option_updateCachedDataOnLibaryLoad <- function(value)
{
  if (missing(value))
  {
    return( getOption("UK2GTFS_opt_updateCachedDataOnLibaryLoad", default=TRUE) )
  }
  else
  {
    return( invisible(options(UK2GTFS_opt_updateCachedDataOnLibaryLoad = as.logical(value) )) )
  }
}



