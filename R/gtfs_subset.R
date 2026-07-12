#' Clip a GTFS object to a geographical area
#'
#' Clips the GTFS file to only include stops within the bounds object, trips
#' that cross the boundary of the the object are truncated. Any trips that stop
#' only once in the bounds are removed completely. The optional tables
#' (shapes, frequencies, transfers, pathways and the GTFS v1/Fares v2 fare
#' tables) are pruned so they only reference the stops, routes and trips that
#' remain.
#'
#' @param gtfs a gtfs object
#' @param bounds an sf data frame of polygons or multi-polygons with CRS 4326
#' @return a gtfs object clipped to the bounds
#' @export
gtfs_clip <- function(gtfs, bounds) {


  if(!sf::st_is_longlat(bounds)){
    stop("The CRS of bounds is not EPSG:4326, please reproject with sf::st_transform(bounds, 4326)")
  }

  if (nrow(bounds) > 1) {
    message("Multiple geometrys offered, using total area of all geometries")
    bounds <- sf::st_combine(bounds)
    suppressWarnings(bounds <- sf::st_buffer(bounds, 0))
  }

  stops <- gtfs$stops
  stop_times <- gtfs$stop_times

  # convert to numeric first as some sources store coordinates as characters
  # (including the literal string "NA"), then drop stops with no location
  stops_inc <- stops
  suppressWarnings({
    stops_inc$stop_lon <- as.numeric(stops_inc$stop_lon)
    stops_inc$stop_lat <- as.numeric(stops_inc$stop_lat)
  })
  stops_inc <- stops_inc[!is.na(stops_inc$stop_lon) & !is.na(stops_inc$stop_lat), ]

  stops_inc <- sf::st_as_sf(stops_inc, coords = c("stop_lon", "stop_lat"), crs = 4326)
  suppressWarnings(stops_inc <- stops_inc[bounds, ])
  stops_inc <- unique(stops_inc$stop_id)

  gtfs$stops <- gtfs$stops[gtfs$stops$stop_id %in% stops_inc, ]
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in% stops_inc, ]
  # Check for single stop trips
  n_stops <- table(gtfs$stop_times$trip_id)
  single_stops <- names(n_stops[n_stops == 1])
  gtfs$stop_times <- gtfs$stop_times[!gtfs$stop_times$trip_id %in% single_stops, ]

  # Check for any unused stops
  gtfs$stops <- gtfs$stops[gtfs$stops$stop_id %in% unique(gtfs$stop_times$stop_id), ]

  gtfs$trips <- gtfs$trips[gtfs$trips$trip_id %in% unique(gtfs$stop_times$trip_id), ]
  gtfs$routes <- gtfs$routes[gtfs$routes$route_id %in% unique(gtfs$trips$route_id), ]
  gtfs$calendar <- gtfs$calendar[gtfs$calendar$service_id %in% unique(gtfs$trips$service_id), ]
  gtfs$calendar_dates <- gtfs$calendar_dates[gtfs$calendar_dates$service_id %in% unique(gtfs$trips$service_id), ]
  gtfs$agency <- gtfs$agency[gtfs$agency$agency_id %in% unique(gtfs$routes$agency_id), ]

  # Keep the optional tables (shapes, frequencies, transfers, fares etc.)
  # consistent with what remains of the core tables
  gtfs <- gtfs_prune_orphans(gtfs)

  return(gtfs)
}


#' Remove rows in optional GTFS tables that reference deleted core rows
#'
#' After the core tables (stops, routes, trips) of a gtfs object have been
#' subset, the optional tables can be left referencing rows that no longer
#' exist, which makes the feed invalid. This prunes the optional tables -
#' shapes, frequencies, transfers, pathways, the GTFS v1 fare tables
#' (fare_attributes/fare_rules) and the GTFS Fares v2 tables (areas,
#' stop_areas, networks, route_networks, fare_leg_rules, fare_transfer_rules,
#' fare_products, rider_categories, fare_media) - so that every reference
#' points at a surviving row. Tables that are absent are left absent.
#'
#' @param gtfs a gtfs object whose core tables have already been subset
#' @return a gtfs object
#' @noRd
gtfs_prune_orphans <- function(gtfs) {
  has <- function(nm) !is.null(gtfs[[nm]])

  # ---- trip references ----
  if (has("trips")) {
    valid_trips <- unique(gtfs$trips$trip_id)

    if (has("frequencies")) {
      gtfs$frequencies <- gtfs$frequencies[gtfs$frequencies$trip_id %in% valid_trips, ]
    }

    if (has("shapes")) {
      valid_shapes <- if ("shape_id" %in% names(gtfs$trips)) {
        unique(gtfs$trips$shape_id)
      } else {
        character()
      }
      gtfs$shapes <- gtfs$shapes[gtfs$shapes$shape_id %in% valid_shapes, ]
    }
  }

  # ---- stop references ----
  if (has("stops")) {
    valid_stops <- unique(gtfs$stops$stop_id)

    if (has("transfers")) {
      gtfs$transfers <- gtfs$transfers[gtfs$transfers$from_stop_id %in% valid_stops &
                                         gtfs$transfers$to_stop_id %in% valid_stops, ]
    }

    if (has("pathways")) {
      gtfs$pathways <- gtfs$pathways[gtfs$pathways$from_stop_id %in% valid_stops &
                                       gtfs$pathways$to_stop_id %in% valid_stops, ]
    }

    if (has("stop_areas")) {
      gtfs$stop_areas <- gtfs$stop_areas[gtfs$stop_areas$stop_id %in% valid_stops, ]
    }
  }

  # empty/NA foreign keys mean "applies to all" in the fare tables, keep them
  blank_or <- function(x, valid) is.na(x) | x == "" | x %in% valid

  # ---- GTFS v1 fares ----
  if (has("fare_rules")) {
    fare_rules_orig <- gtfs$fare_rules
    keep <- rep(TRUE, nrow(fare_rules_orig))
    if (has("routes") && "route_id" %in% names(fare_rules_orig)) {
      keep <- keep & blank_or(fare_rules_orig$route_id, unique(gtfs$routes$route_id))
    }
    if (has("stops") && "zone_id" %in% names(gtfs$stops)) {
      valid_zones <- unique(gtfs$stops$zone_id)
      for (col in c("origin_id", "destination_id", "contains_id")) {
        if (col %in% names(fare_rules_orig)) {
          keep <- keep & blank_or(fare_rules_orig[[col]], valid_zones)
        }
      }
    }
    gtfs$fare_rules <- fare_rules_orig[keep, ]

    # only drop fare_attributes that were attached to (now removed) rules;
    # a fare_id with no rules at all legitimately applies to the whole feed
    if (has("fare_attributes")) {
      had_rules <- gtfs$fare_attributes$fare_id %in% fare_rules_orig$fare_id
      still_used <- gtfs$fare_attributes$fare_id %in% gtfs$fare_rules$fare_id
      gtfs$fare_attributes <- gtfs$fare_attributes[!had_rules | still_used, ]
    }
  }

  # ---- GTFS Fares v2 ----
  if (has("areas") && has("stop_areas")) {
    gtfs$areas <- gtfs$areas[gtfs$areas$area_id %in% unique(gtfs$stop_areas$area_id), ]
  }

  if (has("route_networks") && has("routes")) {
    gtfs$route_networks <- gtfs$route_networks[
      gtfs$route_networks$route_id %in% unique(gtfs$routes$route_id), ]
  }

  if (has("networks")) {
    valid_networks <- character()
    if (has("route_networks")) {
      valid_networks <- c(valid_networks, unique(gtfs$route_networks$network_id))
    }
    if (has("routes") && "network_id" %in% names(gtfs$routes)) {
      valid_networks <- c(valid_networks, unique(gtfs$routes$network_id))
    }
    gtfs$networks <- gtfs$networks[gtfs$networks$network_id %in% valid_networks, ]
  }

  if (has("fare_leg_rules")) {
    flr <- gtfs$fare_leg_rules
    keep <- rep(TRUE, nrow(flr))
    if (has("areas")) {
      valid_areas <- unique(gtfs$areas$area_id)
      for (col in c("from_area_id", "to_area_id")) {
        if (col %in% names(flr)) {
          keep <- keep & blank_or(flr[[col]], valid_areas)
        }
      }
    }
    if (has("networks") && "network_id" %in% names(flr)) {
      keep <- keep & blank_or(flr$network_id, unique(gtfs$networks$network_id))
    }
    gtfs$fare_leg_rules <- flr[keep, ]
  }

  if (has("fare_transfer_rules") && has("fare_leg_rules") &&
      "leg_group_id" %in% names(gtfs$fare_leg_rules)) {
    ftr <- gtfs$fare_transfer_rules
    valid_groups <- unique(gtfs$fare_leg_rules$leg_group_id)
    keep <- rep(TRUE, nrow(ftr))
    for (col in c("from_leg_group_id", "to_leg_group_id")) {
      if (col %in% names(ftr)) {
        keep <- keep & blank_or(ftr[[col]], valid_groups)
      }
    }
    gtfs$fare_transfer_rules <- ftr[keep, ]
  }

  if (has("fare_products") && has("fare_leg_rules") &&
      "fare_product_id" %in% names(gtfs$fare_leg_rules)) {
    used_products <- unique(c(
      gtfs$fare_leg_rules$fare_product_id,
      if (has("fare_transfer_rules") &&
          "fare_product_id" %in% names(gtfs$fare_transfer_rules)) {
        gtfs$fare_transfer_rules$fare_product_id
      }
    ))
    gtfs$fare_products <- gtfs$fare_products[
      gtfs$fare_products$fare_product_id %in% used_products, ]
  }

  if (has("rider_categories") && has("fare_products") &&
      "rider_category_id" %in% names(gtfs$fare_products)) {
    gtfs$rider_categories <- gtfs$rider_categories[
      gtfs$rider_categories$rider_category_id %in% gtfs$fare_products$rider_category_id, ]
    # Fares v2 requires exactly one default category, reinstate if it was pruned
    if (nrow(gtfs$rider_categories) > 0 &&
        "is_default_fare_category" %in% names(gtfs$rider_categories) &&
        !any(gtfs$rider_categories$is_default_fare_category == 1L, na.rm = TRUE)) {
      gtfs$rider_categories$is_default_fare_category[1] <- 1L
    }
  }

  if (has("fare_media") && has("fare_products") &&
      "fare_media_id" %in% names(gtfs$fare_products)) {
    gtfs$fare_media <- gtfs$fare_media[
      gtfs$fare_media$fare_media_id %in% gtfs$fare_products$fare_media_id, ]
  }

  return(gtfs)
}
