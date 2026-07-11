# Convert NeTEx fares into GTFS fares (v1 and v2)
#
# Takes a GTFS object (from transxchange2gtfs()) plus parsed NeTEx fare files
# (from netex_read_fares_multiple()) and adds fare information, either in the
# original GTFS fares specification ("v1": fare_attributes + fare_rules) or the
# GTFS-Fares-v2 specification (areas, fare_products, fare_leg_rules, ...).
#
# The NeTEx fares are a zone-to-zone "fare triangle": every origin-zone /
# destination-zone pair on a line has a price. This maps naturally onto GTFS:
#
#   GTFS v1                               GTFS v2
#   -------                               -------
#   stops.zone_id      <- fare zone       areas + stop_areas   <- fare zone
#   fare_attributes    <- price band      fare_products        <- price band
#   fare_rules         <- O/D + route     fare_leg_rules       <- O/D + network
#                                         rider_categories     <- user type
#                                         fare_media           <- ticket type
#
# GTFS v1 has no concept of passenger category, so it can only represent a
# single fare product (by default the adult single) per route. GTFS v2 can
# represent several products (adult/child, single/return) at once.
#
# See netex_fares_read.R (reading) and netex_fares_match.R (choosing a product
# and matching lines to routes).


#' Make a safe GTFS identifier
#' @param x character vector
#' @noRd
netex_sanitize_id <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}


#' Indices of NeTEx files matched to a GTFS route
#'
#' Returns the `netex_list` positions that matched a route, and reports once how
#' many were skipped (nationally most files belong to lines not in a given GTFS,
#' so a per-file warning would flood the log).
#'
#' @param match_tbl output of [netex_match_routes()].
#' @noRd
netex_matched_indices <- function(match_tbl) {
  n_unmatched <- sum(!match_tbl$matched)
  if (n_unmatched > 0) {
    message(n_unmatched, " NeTEx fare file(s) had no matching GTFS route and ",
            "were skipped (", sum(match_tbl$matched), " matched).")
  }
  match_tbl$idx[match_tbl$matched]
}


#' Normalise a NeTEx zones table
#'
#' Guarantees a zones table with the expected columns. Some NeTEx files declare
#' a zonal fare but list no zone members, leaving an empty, column-less table;
#' this returns a correctly typed empty table so downstream column access is
#' safe (such a file simply yields no fare rules).
#'
#' @param zones the `zones` element from [netex_read_fares()].
#' @noRd
netex_zones_norm <- function(zones) {
  cols <- c("zone_id", "zone_name", "stop_id")
  if (is.null(zones) || nrow(zones) == 0 || !all(cols %in% names(zones))) {
    return(data.table::data.table(zone_id = character(), zone_name = character(),
                                  stop_id = character()))
  }
  zones
}


#' Add NeTEx fares to a GTFS object straight from a BODS archive
#'
#' End-to-end convenience wrapper for the national work flow: unpack a BODS
#' NeTEx fare archive, read every fare file (in parallel), report what was
#' found and how many lines match the GTFS, then add the chosen fares to the
#' GTFS object. Equivalent to calling [netex_unzip()],
#' [netex_read_fares_multiple()], [netex_fares_report()] and
#' [gtfs_add_fares()] in turn.
#'
#' @param archive path to a BODS NeTEx fare `.zip` archive (or a folder of
#'   NeTEx files).
#' @param gtfs a GTFS object (named list) to which fares will be added, for
#'   example the national feed produced by [transxchange2gtfs()].
#' @param fares_version numeric, `1` or `2` (see [gtfs_add_fares()]).
#' @param ncores integer, cores for the parallel read step (default `1`).
#' @param pattern optional regex to restrict which archive entries are
#'   extracted (e.g. an operator name), passed to [netex_unzip()].
#' @param exdir directory to extract into (default a temp folder).
#' @param report logical, print a [netex_fares_report()] summary (default
#'   `TRUE`).
#' @param trip_type,user_type,product_name,direction optional fare-type filters
#'   passed to [gtfs_add_fares()].
#' @return the GTFS object with fare tables added. The parsed NeTEx list and the
#'   report are attached as attributes `"netex"` and `"report"` for inspection.
#' @export
netex_fares_from_archive <- function(archive,
                                     gtfs,
                                     fares_version = 1,
                                     ncores = 1,
                                     pattern = NULL,
                                     exdir = tempfile("netex_"),
                                     report = TRUE,
                                     trip_type = NULL,
                                     user_type = NULL,
                                     product_name = NULL,
                                     direction = NULL) {
  xmls <- netex_unzip(archive, exdir = exdir, pattern = pattern)
  message("Reading ", length(xmls), " NeTEx fare files on ", ncores, " core(s)")
  netex <- netex_read_fares_multiple(xmls, ncores = ncores)
  rep <- if (report) netex_fares_report(netex, gtfs) else NULL
  gtfs <- gtfs_add_fares(gtfs, netex, fares_version = fares_version,
                         trip_type = trip_type, user_type = user_type,
                         product_name = product_name, direction = direction)
  attr(gtfs, "netex") <- netex
  attr(gtfs, "report") <- rep
  gtfs
}


#' Add fares to a GTFS object from NeTEx fare files
#'
#' High level wrapper that filters the supplied NeTEx fare files to the desired
#' product(s), matches each line to a GTFS route, and adds the corresponding
#' GTFS fare tables. Use `fares_version` to choose between the original GTFS
#' fares specification and GTFS-Fares-v2.
#'
#' @param gtfs a GTFS object (named list) as produced by
#'   [transxchange2gtfs()].
#' @param netex_list list of parsed NeTEx fare objects, from
#'   [netex_read_fares_multiple()].
#' @param fares_version numeric, `1` for the original GTFS fares
#'   (fare_attributes / fare_rules) or `2` for GTFS-Fares-v2. Default `1`.
#' @param trip_type,user_type,product_name,direction optional filters passed to
#'   [netex_filter_fares()] to choose which fare product(s) to convert. For
#'   `fares_version = 1` you should select a single product type (e.g.
#'   `trip_type = "single", user_type = "adult"`); if several products remain
#'   they will all be emitted but GTFS v1 cannot distinguish passenger types.
#' @return the GTFS object with fare tables added.
#' @export
gtfs_add_fares <- function(gtfs,
                           netex_list,
                           fares_version = 1,
                           trip_type = NULL,
                           user_type = NULL,
                           product_name = NULL,
                           direction = NULL) {
  netex_list <- netex_filter_fares(netex_list,
                                   trip_type = trip_type,
                                   user_type = user_type,
                                   product_name = product_name,
                                   direction = direction)
  if (length(netex_list) == 0) {
    warning("No NeTEx fare files matched the requested filters; GTFS unchanged.")
    return(gtfs)
  }

  if (fares_version == 1) {
    gtfs_add_fares_v1(gtfs, netex_list)
  } else if (fares_version == 2) {
    gtfs_add_fares_v2(gtfs, netex_list)
  } else {
    stop("fares_version must be 1 or 2")
  }
}


#' Add GTFS v1 fares (fare_attributes + fare_rules) from NeTEx
#'
#' Builds the original-specification GTFS fare tables from parsed NeTEx fare
#' files and attaches them to a GTFS object. A `zone_id` column is added to
#' `stops` giving each stop its fare zone, and `fare_rules` maps each
#' origin-zone / destination-zone pair on a route to a fare.
#'
#' Because GTFS v1 stores a single `zone_id` per stop, this works cleanly for a
#' single route; when several routes share a stop but place it in different
#' fare zones only the first assignment is kept (with a warning).
#'
#' @param gtfs a GTFS object (named list).
#' @param netex_list list of parsed NeTEx fare objects (already filtered to the
#'   product(s) you want, see [netex_filter_fares()]).
#' @param payment_method integer, GTFS `payment_method` (0 = paid on board,
#'   1 = paid before boarding). Default 0, matching on-board bus ticket sales.
#' @return the GTFS object with `fare_attributes`, `fare_rules` and a
#'   `stops$zone_id` column added.
#' @export
gtfs_add_fares_v1 <- function(gtfs, netex_list, payment_method = 0L) {
  match_tbl <- netex_match_routes(netex_list, gtfs)
  gtfs_stops <- unique(as.character(gtfs$stops$stop_id))
  use_idx <- netex_matched_indices(match_tbl)
  route_by_idx <- match_tbl$route_id[order(match_tbl$idx)]
  agency_by_route <- stats::setNames(gtfs$routes$agency_id, gtfs$routes$route_id)

  fare_attributes <- list()
  fare_rules <- list()
  zone_map <- list()   # stop_id -> area_id

  for (i in use_idx) {
    nx <- netex_list[[i]]
    m <- nx$meta
    route_id <- route_by_idx[i]
    agency_id <- agency_by_route[[route_id]]

    # Zone ids (fs@18 ...) are reused between directions for different stop
    # sets, so namespace areas by route AND direction to keep them distinct.
    dtag <- netex_sanitize_id(ifelse(is.na(m$direction), "d", m$direction))
    area <- function(z) paste0(route_id, "_", dtag, "_", netex_sanitize_id(z))
    ptag <- netex_sanitize_id(paste(m$user_type, m$trip_type, sep = "_"))

    # One fare per distinct price. Key the fare by route + product + amount, so
    # that the same amount is one fare and a fare_id always has one price. Drop
    # fares with no usable amount.
    fares_i <- data.table::copy(nx$fares)
    fares_i <- fares_i[!is.na(fares_i$amount), ]
    if (nrow(fares_i) == 0) next
    is_flat <- isTRUE(m$fare_kind == "flat") || all(is.na(fares_i$from_zone))
    fares_i$fare_id <- paste0(route_id, "_", ptag, "_",
                              netex_sanitize_id(as.character(fares_i$amount)))
    bands <- unique(fares_i[, c("fare_id", "amount")])

    fare_attributes[[length(fare_attributes) + 1]] <- data.table::data.table(
      fare_id = bands$fare_id,
      price = bands$amount,
      currency_type = m$currency,
      payment_method = payment_method,
      transfers = if (isTRUE(tolower(m$trip_type) == "single")) 0L else NA_integer_,
      agency_id = agency_id
    )

    if (is_flat) {
      # Flat fare applies to the whole route; no origin/destination zones.
      fare_rules[[length(fare_rules) + 1]] <- data.table::data.table(
        fare_id = unique(fares_i$fare_id),
        route_id = route_id,
        origin_id = NA_character_,
        destination_id = NA_character_
      )
    } else {
      # Restrict zones to stops that actually exist in this GTFS route, so we
      # do not emit fare rules referencing zones with no stop in the feed.
      zm <- unique(netex_zones_norm(nx$zones)[, c("stop_id", "zone_id")])
      zm <- zm[zm$stop_id %in% gtfs_stops, ]
      zm$area_id <- area(zm$zone_id)
      zone_map[[length(zone_map) + 1]] <- zm[, c("stop_id", "area_id")]
      valid_areas <- unique(zm$area_id)

      fares_i$origin_id <- area(fares_i$from_zone)
      fares_i$destination_id <- area(fares_i$to_zone)
      fr <- fares_i[fares_i$origin_id %in% valid_areas &
                      fares_i$destination_id %in% valid_areas, ]
      if (nrow(fr) > 0) {
        fare_rules[[length(fare_rules) + 1]] <- data.table::data.table(
          fare_id = fr$fare_id,
          route_id = route_id,
          origin_id = fr$origin_id,
          destination_id = fr$destination_id
        )
      }
    }
  }

  if (length(fare_attributes) == 0) {
    warning("No NeTEx fares could be matched to routes; GTFS unchanged.")
    return(gtfs)
  }

  fare_attributes <- unique(data.table::rbindlist(fare_attributes, fill = TRUE))
  fare_attributes <- fare_attributes[!duplicated(fare_attributes$fare_id), ]
  fare_rules <- unique(data.table::rbindlist(fare_rules, fill = TRUE))

  # assign zone_id to stops (first assignment wins per stop)
  stops <- data.table::as.data.table(gtfs$stops)
  if (length(zone_map) > 0) {
    zone_map <- data.table::rbindlist(zone_map, fill = TRUE)
    n_conflict <- sum(duplicated(zone_map$stop_id))
    if (n_conflict > 0) {
      warning(n_conflict, " stop(s) belong to different fare zones on different ",
              "products/directions; GTFS v1 allows one zone per stop, keeping the ",
              "first. Fare rules for the dropped zones are removed. Consider ",
              "fares_version = 2, or filter to a single direction.")
    }
    zone_map <- zone_map[!duplicated(zone_map$stop_id), ]
    stops$zone_id <- zone_map$area_id[match(stops$stop_id, zone_map$stop_id)]
  } else {
    stops$zone_id <- NA_character_
  }

  # Keep zonal rules only where both zones are actually realised as a stop
  # zone_id (a stop assigned to another zone can orphan a zone). Flat fares
  # (no origin/destination) are always kept. Then drop bands left with no rule.
  realised <- unique(stats::na.omit(stops$zone_id))
  is_flat_rule <- is.na(fare_rules$origin_id) & is.na(fare_rules$destination_id)
  fare_rules <- fare_rules[is_flat_rule |
                             (fare_rules$origin_id %in% realised &
                                fare_rules$destination_id %in% realised), ]
  fare_attributes <- fare_attributes[fare_attributes$fare_id %in% fare_rules$fare_id, ]

  gtfs$stops <- stops
  gtfs$fare_attributes <- rbind_fares(gtfs$fare_attributes, fare_attributes)
  gtfs$fare_rules <- rbind_fares(gtfs$fare_rules, fare_rules)
  gtfs
}


#' Add GTFS-Fares-v2 tables from NeTEx
#'
#' Builds GTFS-Fares-v2 tables (areas, stop_areas, networks, route_networks,
#' rider_categories, fare_media, fare_products and fare_leg_rules) from parsed
#' NeTEx fare files and attaches them to a GTFS object.
#'
#' Unlike v1, this can represent several fare products (e.g. adult and child,
#' single and return) at the same time, because passenger type is carried by
#' `rider_categories` / `fare_products` rather than by the stop.
#'
#' @param gtfs a GTFS object (named list).
#' @param netex_list list of parsed NeTEx fare objects.
#' @param fare_media_name,fare_media_type the ticket medium recorded in
#'   `fare_media`. Defaults describe a cash on-board paper ticket
#'   (`fare_media_type = 0`).
#' @return the GTFS object with GTFS-Fares-v2 tables added.
#' @export
gtfs_add_fares_v2 <- function(gtfs, netex_list,
                              fare_media_name = "cash",
                              fare_media_type = 0L) {
  match_tbl <- netex_match_routes(netex_list, gtfs)
  gtfs_stops <- unique(as.character(gtfs$stops$stop_id))
  use_idx <- netex_matched_indices(match_tbl)
  route_by_idx <- match_tbl$route_id[order(match_tbl$idx)]

  areas <- list()
  stop_areas <- list()
  networks <- list()
  route_networks <- list()
  rider_categories <- list()
  fare_products <- list()
  fare_leg_rules <- list()

  media_id <- netex_sanitize_id(fare_media_name)
  fare_media <- data.table::data.table(
    fare_media_id = media_id,
    fare_media_name = fare_media_name,
    fare_media_type = fare_media_type
  )

  for (i in use_idx) {
    nx <- netex_list[[i]]
    m <- nx$meta
    route_id <- route_by_idx[i]

    # Zone ids are reused between directions for different stop sets, so
    # namespace areas by route AND direction to keep them distinct.
    dtag <- netex_sanitize_id(ifelse(is.na(m$direction), "d", m$direction))
    area <- function(z) paste0(route_id, "_", dtag, "_", netex_sanitize_id(z))
    network_id <- paste0("net_", route_id)
    rc_id <- netex_sanitize_id(ifelse(is.na(m$user_type), "any", m$user_type))
    ptag <- netex_sanitize_id(paste(m$user_type, m$trip_type, sep = "_"))

    # network for this route
    networks[[length(networks) + 1]] <- data.table::data.table(
      network_id = network_id,
      network_name = m$line_name)
    route_networks[[length(route_networks) + 1]] <- data.table::data.table(
      network_id = network_id, route_id = route_id)

    fares_i <- data.table::copy(nx$fares)
    fares_i <- fares_i[!is.na(fares_i$amount), ]
    if (nrow(fares_i) == 0) next
    is_flat <- isTRUE(m$fare_kind == "flat") || all(is.na(fares_i$from_zone))

    # rider category
    rider_categories[[length(rider_categories) + 1]] <- data.table::data.table(
      rider_category_id = rc_id,
      rider_category_name = ifelse(is.na(m$user_name), m$user_type, m$user_name))

    # One product per (rider category, amount) - a v2 fare product is defined by
    # its price and who can use it, so key it that way to keep ids globally
    # consistent (the same GBP 2 adult single is one product across operators).
    fares_i$fare_product_id <- paste0(ptag, "_",
                                      netex_sanitize_id(as.character(fares_i$amount)))
    bands <- unique(fares_i[, c("fare_product_id", "amount")])
    fare_products[[length(fare_products) + 1]] <- data.table::data.table(
      fare_product_id = bands$fare_product_id,
      fare_product_name = paste0(m$product_name, " ", bands$amount),
      rider_category_id = rc_id,
      fare_media_id = media_id,
      amount = bands$amount,
      currency = m$currency)

    if (is_flat) {
      # Flat fare: one leg rule for the whole network, no areas.
      fare_leg_rules[[length(fare_leg_rules) + 1]] <- data.table::data.table(
        leg_group_id = paste0(network_id, "_", ptag),
        network_id = network_id,
        from_area_id = NA_character_,
        to_area_id = NA_character_,
        fare_product_id = unique(fares_i$fare_product_id))
    } else {
      # areas + stop areas, restricted to stops present in this GTFS feed
      zn <- unique(netex_zones_norm(nx$zones)[, c("zone_id", "zone_name", "stop_id")])
      zn <- zn[zn$stop_id %in% gtfs_stops, ]
      areas[[length(areas) + 1]] <- unique(data.table::data.table(
        area_id = area(zn$zone_id), area_name = zn$zone_name))
      stop_areas[[length(stop_areas) + 1]] <- unique(data.table::data.table(
        area_id = area(zn$zone_id), stop_id = zn$stop_id))
      valid_areas <- unique(area(zn$zone_id))

      fares_i$from_area_id <- area(fares_i$from_zone)
      fares_i$to_area_id <- area(fares_i$to_zone)
      fr <- fares_i[fares_i$from_area_id %in% valid_areas &
                      fares_i$to_area_id %in% valid_areas, ]
      if (nrow(fr) > 0) {
        fare_leg_rules[[length(fare_leg_rules) + 1]] <- data.table::data.table(
          leg_group_id = paste0(network_id, "_", ptag),
          network_id = network_id,
          from_area_id = fr$from_area_id,
          to_area_id = fr$to_area_id,
          fare_product_id = fr$fare_product_id)
      }
    }
  }

  if (length(fare_products) == 0) {
    warning("No NeTEx fares could be matched to routes; GTFS unchanged.")
    return(gtfs)
  }

  areas <- unique(data.table::rbindlist(areas, fill = TRUE))
  stop_areas <- unique(data.table::rbindlist(stop_areas, fill = TRUE))
  fare_products <- unique(data.table::rbindlist(fare_products, fill = TRUE))
  fare_leg_rules <- unique(data.table::rbindlist(fare_leg_rules, fill = TRUE))
  # area_id / fare_product_id must be unique keys, but the same id can arrive
  # with slightly different names (zone/product names vary between files); keep
  # the first occurrence of each id.
  areas <- areas[!duplicated(areas$area_id), ]
  fare_products <- fare_products[!duplicated(fare_products$fare_product_id), ]
  # drop products left with no leg rule (all their O/D areas were absent)
  fare_products <- fare_products[fare_products$fare_product_id %in% fare_leg_rules$fare_product_id, ]

  # rider_category_id must be unique (names for the same user type vary between
  # files, so unique() alone can leave duplicate ids), and GTFS Fares v2
  # requires exactly one category to be flagged as the default
  rider_categories <- unique(data.table::rbindlist(rider_categories, fill = TRUE))
  rider_categories <- rider_categories[!duplicated(rider_categories$rider_category_id), ]
  rider_categories$is_default_fare_category <- 0L
  default_idx <- match("adult", rider_categories$rider_category_id)
  if (is.na(default_idx)) default_idx <- 1L
  rider_categories$is_default_fare_category[default_idx] <- 1L

  gtfs$areas <- rbind_fares(gtfs$areas, areas)
  gtfs$stop_areas <- rbind_fares(gtfs$stop_areas, stop_areas)
  gtfs$networks <- rbind_fares(gtfs$networks, unique(data.table::rbindlist(networks, fill = TRUE)))
  gtfs$route_networks <- rbind_fares(gtfs$route_networks, unique(data.table::rbindlist(route_networks, fill = TRUE)))
  gtfs$rider_categories <- rbind_fares(gtfs$rider_categories, rider_categories)
  gtfs$fare_media <- rbind_fares(gtfs$fare_media, fare_media)
  gtfs$fare_products <- rbind_fares(gtfs$fare_products, fare_products)
  gtfs$fare_leg_rules <- rbind_fares(gtfs$fare_leg_rules, fare_leg_rules)
  gtfs
}


#' Append fare rows to an existing (possibly NULL) GTFS table
#' @param existing existing table or NULL
#' @param new new table
#' @noRd
rbind_fares <- function(existing, new) {
  if (is.null(existing) || nrow(existing) == 0) return(unique(new))
  unique(data.table::rbindlist(list(existing, new), fill = TRUE))
}
