# Where stop locations come from in transxchange2gtfs()
#
# A TransXchange file names every stop it calls at, and may also say where the
# stop is (see import_stop_locations()). NaPTAN is the national register of the
# same stops. Until 0.4.1 the conversion used NaPTAN only: a stop missing from
# the NaPTAN table the caller passed - one created after that copy was
# downloaded, typically - came out with no coordinates, and gtfs_clean() then
# deleted the stop and every call at it without a word, even though the file
# said exactly where it was. Transport for London defines every stop in full,
# so a new London stop was always recoverable.

utils::globalVariables(c("easting", "northing", "stop_name", "lon", "lat",
                         "txc_lon", "txc_lat", "mlon", "mlat"))


#' The TransXchange stop locations of a set of imported files, combined
#'
#' @param res_all list of transxchange_import() objects
#' @return data.frame stop_id, txc_name, txc_lon, txc_lat, txc_n (number of
#'   definitions with a location), txc_spread_m (furthest definition from the
#'   combined position, metres), one row per stop with at least a name or a
#'   location
#' @noRd
txc_collect_stop_locations <- function(res_all) {
  locs <- lapply(res_all, function(x) {
    if (is.list(x)) x[["StopLocations"]] else NULL
  })
  locs <- data.table::rbindlist(locs[!vapply(locs, is.null, logical(1))],
                                fill = TRUE)
  txc_resolve_stop_locations(locs)
}


#' Convert, check and combine TransXchange stop definitions
#'
#' Grid references are converted from British National Grid (EPSG:27700) to
#' WGS84, in one pass. Where a definition gives both, its own longitude and
#' latitude are used. Positions outside a box around Great Britain and
#' Ireland, and 0/0, are treated as absent. A stop defined by several files
#' takes the median of their positions; txc_spread_m records how far apart
#' they were.
#'
#' @param locs data.frame as import_stop_locations(), stacked
#' @noRd
txc_resolve_stop_locations <- function(locs) {
  out <- data.frame(stop_id = character(0), txc_name = character(0),
                    txc_lon = numeric(0), txc_lat = numeric(0),
                    txc_n = integer(0), txc_spread_m = numeric(0),
                    stringsAsFactors = FALSE)
  if (is.null(locs) || nrow(locs) == 0) {
    return(out)
  }
  locs <- data.table::as.data.table(locs)
  grid <- is.na(locs$lon) & !is.na(locs$easting) & !is.na(locs$northing) &
    locs$easting > 0 & locs$northing > 0
  if (any(grid)) {
    p <- sf::st_as_sf(locs[grid, list(easting, northing)],
                      coords = c("easting", "northing"), crs = 27700)
    p <- sf::st_coordinates(sf::st_transform(p, 4326))
    locs$lon[grid] <- p[, 1]
    locs$lat[grid] <- p[, 2]
  }
  ok <- !is.na(locs$lon) & !is.na(locs$lat) &
    locs$lon > -11 & locs$lon < 3 & locs$lat > 49 & locs$lat < 61.5
  locs$lon[!ok] <- NA
  locs$lat[!ok] <- NA

  res <- locs[, list(
    txc_name = { n <- stop_name[!is.na(stop_name) & stop_name != ""]
                 if (length(n)) names(sort(table(n), decreasing = TRUE))[1]
                 else NA_character_ },
    txc_lon = stats::median(lon, na.rm = TRUE),
    txc_lat = stats::median(lat, na.rm = TRUE),
    txc_n = sum(!is.na(lon))
  ), by = "stop_id"]
  res$txc_lon[res$txc_n == 0] <- NA
  res$txc_lat[res$txc_n == 0] <- NA

  spread <- merge(locs[!is.na(lon), list(stop_id, lon, lat)],
                  res[, list(stop_id, mlon = txc_lon, mlat = txc_lat)],
                  by = "stop_id")
  spread <- spread[, list(txc_spread_m = max(
    txc_distance_m(lon, lat, mlon, mlat))), by = "stop_id"]
  res <- merge(res, spread, by = "stop_id", all.x = TRUE)
  as.data.frame(res)
}


#' Distance in metres between WGS84 points (equirectangular; fine at the
#' scale of a bus stop)
#' @noRd
txc_distance_m <- function(lon1, lat1, lon2, lat2) {
  rad <- pi / 180
  x <- (lon2 - lon1) * rad * cos((lat1 + lat2) / 2 * rad)
  y <- (lat2 - lat1) * rad
  6371000 * sqrt(x * x + y * y)
}


#' Settle each stop's location from NaPTAN and the TransXchange files
#'
#' @param stops the merged feed's stops, already joined to NaPTAN (stop_id,
#'   stop_code, stop_name, stop_lon, stop_lat; NA where NaPTAN lacks the stop)
#' @param txc txc_collect_stop_locations() output
#' @param stop_locations one of "naptan_fill", "naptan", "txc", "txc_fill"
#' @param stop_times the merged stop_times, to count the calls a stop with no
#'   location takes with it
#' @return list(stops, report). report has one row per stop: naptan_lon,
#'   naptan_lat, txc_lon, txc_lat, txc_n, txc_spread_m, dist_m (NaPTAN to
#'   TransXchange), source ("naptan", "txc" or "none") and calls
#' @noRd
txc_apply_stop_locations <- function(stops, txc, stop_locations, stop_times) {
  stops <- as.data.frame(stops)
  m <- match(stops$stop_id, txc$stop_id)
  rep <- data.frame(
    stop_id = stops$stop_id,
    naptan_lon = stops$stop_lon, naptan_lat = stops$stop_lat,
    txc_lon = txc$txc_lon[m], txc_lat = txc$txc_lat[m],
    txc_n = ifelse(is.na(m), 0L, txc$txc_n[m]),
    txc_spread_m = txc$txc_spread_m[m],
    stringsAsFactors = FALSE)
  rep$dist_m <- txc_distance_m(rep$naptan_lon, rep$naptan_lat,
                               rep$txc_lon, rep$txc_lat)
  has_n <- !is.na(rep$naptan_lon) & !is.na(rep$naptan_lat)
  has_t <- !is.na(rep$txc_lon) & !is.na(rep$txc_lat)
  use_t <- switch(stop_locations,
                  naptan = rep(FALSE, nrow(rep)),
                  naptan_fill = !has_n & has_t,
                  txc = has_t,
                  txc_fill = has_t)
  use_n <- switch(stop_locations,
                  naptan = has_n,
                  naptan_fill = has_n,
                  txc = rep(FALSE, nrow(rep)),
                  txc_fill = !has_t & has_n)
  stops$stop_lon[use_t] <- rep$txc_lon[use_t]
  stops$stop_lat[use_t] <- rep$txc_lat[use_t]
  stops$stop_lon[!use_t & !use_n] <- NA
  stops$stop_lat[!use_t & !use_n] <- NA
  # A stop NaPTAN does not know has no name either; the file's own name keeps
  # apply_standard_modes() and anything reading stop_name working.
  no_name <- is.na(stops$stop_name) | stops$stop_name == ""
  stops$stop_name[no_name] <- txc$txc_name[m][no_name]

  rep$source <- ifelse(use_t, "txc", ifelse(use_n, "naptan", "none"))
  calls <- table(as.character(stop_times$stop_id))
  rep$calls <- as.integer(calls[rep$stop_id])
  rep$calls[is.na(rep$calls)] <- 0L
  list(stops = stops, report = rep)
}
