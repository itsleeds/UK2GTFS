#' Reduce file size of a GTFS object
#'
#' @param gtfs a gtfs object
#' @return a gtfs object
#' @details by default UK2GTFS tries to preserve id numbers during the conversion
#'   process to allow back comparisons to the original files, e.g.
#'   `transxchange2gtfs()` retains stop ids from the NAPTAN. However this means
#'   files sizes are increased. This function replaces ids with integers and
#'   thus reduces the size of the gtfs file.
#'
#'   All tables that reference the rewritten ids are updated together:
#'   `stop_id` in stop_times, transfers, pathways and stop_areas; `trip_id`
#'   in stop_times and frequencies; `route_id` in trips, fare_rules and
#'   route_networks; and `shape_id` in trips and shapes. Rows referencing an
#'   id that does not exist in the parent table are dropped.
#'
#' @export
gtfs_compress <- function(gtfs) {
  agency <- gtfs$agency
  stops <- gtfs$stops
  routes <- gtfs$routes
  trips <- gtfs$trips
  stop_times <- gtfs$stop_times
  calendar <- gtfs$calendar
  calendar_dates <- gtfs$calendar_dates

  # Simplify stop_ids
  stop_id <- unique(stops$stop_id)
  stops$stop_id <- as.integer(factor(stops$stop_id, levels = stop_id))
  stop_times$stop_id <- as.integer(factor(stop_times$stop_id, levels = stop_id))

  # transfers.txt references stops by stop_id, so it must be remapped with the
  # same factor levels, otherwise it would point at stop_ids that no longer
  # exist and the feed would fail to load (e.g. in OpenTripPlanner).
  if (!is.null(gtfs$transfers)) {
    transfers <- gtfs$transfers
    transfers$from_stop_id <- as.integer(factor(transfers$from_stop_id, levels = stop_id))
    transfers$to_stop_id <- as.integer(factor(transfers$to_stop_id, levels = stop_id))
    # Drop any transfer whose endpoints are not in the stop table
    transfers <- transfers[!is.na(transfers$from_stop_id) & !is.na(transfers$to_stop_id), ]
    gtfs$transfers <- transfers
  }

  # pathways.txt also references stops by stop_id
  if (!is.null(gtfs$pathways)) {
    pathways <- gtfs$pathways
    pathways$from_stop_id <- as.integer(factor(pathways$from_stop_id, levels = stop_id))
    pathways$to_stop_id <- as.integer(factor(pathways$to_stop_id, levels = stop_id))
    pathways <- pathways[!is.na(pathways$from_stop_id) & !is.na(pathways$to_stop_id), ]
    gtfs$pathways <- pathways
  }

  # stop_areas.txt (GTFS Fares v2) references stops by stop_id
  if (!is.null(gtfs$stop_areas)) {
    stop_areas <- gtfs$stop_areas
    stop_areas$stop_id <- as.integer(factor(stop_areas$stop_id, levels = stop_id))
    stop_areas <- stop_areas[!is.na(stop_areas$stop_id), ]
    gtfs$stop_areas <- stop_areas
  }

  # Simplify trip_ids
  trip_id <- unique(trips$trip_id)
  trips$trip_id <- as.integer(factor(trips$trip_id, levels = trip_id))
  stop_times$trip_id <- as.integer(factor(stop_times$trip_id, levels = trip_id))

  # frequencies.txt references trips by trip_id
  if (!is.null(gtfs$frequencies)) {
    frequencies <- gtfs$frequencies
    frequencies$trip_id <- as.integer(factor(frequencies$trip_id, levels = trip_id))
    frequencies <- frequencies[!is.na(frequencies$trip_id), ]
    gtfs$frequencies <- frequencies
  }

  # Simplify route_ids
  route_id <- unique(routes$route_id)
  routes$route_id <- as.integer(factor(routes$route_id, levels = route_id))
  trips$route_id <- as.integer(factor(trips$route_id, levels = route_id))

  # fare_rules.txt (GTFS v1 fares) may reference routes by route_id. An
  # NA/blank route_id is valid (fare applies to all routes) and is kept, but
  # rows pointing at a route that does not exist are dropped rather than
  # silently becoming all-route fares.
  if (!is.null(gtfs$fare_rules) && "route_id" %in% names(gtfs$fare_rules)) {
    fare_rules <- gtfs$fare_rules
    had_route <- !is.na(fare_rules$route_id) & fare_rules$route_id != ""
    fare_rules$route_id <- as.integer(factor(fare_rules$route_id, levels = route_id))
    fare_rules <- fare_rules[!(had_route & is.na(fare_rules$route_id)), ]
    gtfs$fare_rules <- fare_rules
  }

  # route_networks.txt (GTFS Fares v2) references routes by route_id
  if (!is.null(gtfs$route_networks)) {
    route_networks <- gtfs$route_networks
    route_networks$route_id <- as.integer(factor(route_networks$route_id, levels = route_id))
    route_networks <- route_networks[!is.na(route_networks$route_id), ]
    gtfs$route_networks <- route_networks
  }

  # Simplify shape_ids
  if (!is.null(gtfs$shapes) && "shape_id" %in% names(trips)) {
    shapes <- gtfs$shapes
    shape_id <- unique(shapes$shape_id)
    shapes$shape_id <- as.integer(factor(shapes$shape_id, levels = shape_id))
    trips$shape_id <- as.integer(factor(trips$shape_id, levels = shape_id))
    gtfs$shapes <- shapes
  }

  gtfs$stops <- stops
  gtfs$routes <- routes
  gtfs$trips <- trips
  gtfs$stop_times <- stop_times

  return(gtfs)
}
