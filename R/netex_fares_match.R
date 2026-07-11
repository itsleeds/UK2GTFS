# Matching NeTEx fares to GTFS routes and choosing a fare type
#
# A BODS NeTEx fare archive contains many files, each describing the fares for
# one line / direction / product (e.g. "Adult Single"). Before converting to
# GTFS we need to
#   1. see what fare products are available   -> netex_fare_types()
#   2. pick the product(s) we want            -> netex_filter_fares()
#   3. join each NeTEx line to a GTFS route    -> netex_match_routes()
#
# These helpers operate on a *list* of parsed NeTEx fare objects, i.e. the
# output of netex_read_fares_multiple().


#' List the fare products available in a set of NeTEx files
#'
#' Summarises a list of parsed NeTEx fare objects so the caller can see which
#' fare types (single / return, adult / child, per line and direction) are
#' present and therefore what can be selected for conversion.
#'
#' @param netex_list list of parsed NeTEx fare objects, from
#'   [netex_read_fares_multiple()].
#' @return a data.table, one row per NeTEx file, with the operator, line,
#'   direction and product attributes plus the source list index (`idx`) so a
#'   selection can be mapped back to the original list.
#' @export
netex_fare_types <- function(netex_list) {
  metas <- lapply(seq_along(netex_list), function(i) {
    m <- data.table::copy(netex_list[[i]]$meta)
    m$idx <- i
    m$n_zones <- length(unique(netex_list[[i]]$zones$zone_id))
    m$n_prices <- nrow(netex_list[[i]]$fares)
    m
  })
  out <- data.table::rbindlist(metas, fill = TRUE)
  data.table::setcolorder(out, c("idx", "operator_noc", "line_public_code",
                                 "direction", "product_name", "trip_type",
                                 "user_type"))
  out[]
}


#' Select NeTEx fare files by product attributes
#'
#' Filters a list of parsed NeTEx fare objects, keeping only those whose
#' metadata match ALL of the supplied criteria. Any criterion left `NULL`
#' is not applied. Matching is case-insensitive.
#'
#' This is how the caller chooses which kind of fare to convert (the task
#' requires being able to pick, e.g., single tickets only). All valid choices
#' are supported: pass any combination of the attributes below, and inspect
#' [netex_fare_types()] to discover the values present in your data.
#'
#' @param netex_list list of parsed NeTEx fare objects.
#' @param trip_type character, e.g. "single" or "return" (matches
#'   `meta$trip_type`).
#' @param user_type character, e.g. "adult", "child", "senior" (matches
#'   `meta$user_type`).
#' @param product_name character, matched as a regular expression against
#'   `meta$product_name` (e.g. "Adult Single").
#' @param direction character, "inbound" or "outbound".
#' @param line_public_code character, the public line number (e.g. "91").
#' @param operator_noc character, the operator National Operator Code.
#' @return a filtered list of NeTEx fare objects (a subset of `netex_list`).
#' @export
netex_filter_fares <- function(netex_list,
                               trip_type = NULL,
                               user_type = NULL,
                               product_name = NULL,
                               direction = NULL,
                               line_public_code = NULL,
                               operator_noc = NULL) {
  keep <- vapply(netex_list, function(x) {
    m <- x$meta
    ok <- TRUE
    eq <- function(val, target) {
      if (is.null(target)) return(TRUE)
      !is.na(val) && tolower(val) == tolower(target)
    }
    ok <- ok && eq(m$trip_type, trip_type)
    ok <- ok && eq(m$user_type, user_type)
    ok <- ok && eq(m$direction, direction)
    ok <- ok && eq(m$line_public_code, line_public_code)
    ok <- ok && eq(m$operator_noc, operator_noc)
    if (!is.null(product_name)) {
      ok <- ok && !is.na(m$product_name) &&
        grepl(product_name, m$product_name, ignore.case = TRUE)
    }
    ok
  }, logical(1))
  netex_list[keep]
}


#' Summarise a set of NeTEx fare files (and optionally match to GTFS)
#'
#' Produces a high level report of what is in a collection of NeTEx fare files:
#' how many files, operators and lines are covered, the mix of products, trip
#' types and passenger types, and how many files describe a usable zonal fare
#' triangle. If a GTFS object is supplied it also reports how many lines could
#' be matched to a GTFS route (the main scaling bottleneck nationally).
#'
#' @param netex_list list of parsed NeTEx fare objects, from
#'   [netex_read_fares_multiple()].
#' @param gtfs optional GTFS object; if supplied, line-to-route match rates are
#'   included.
#' @return a list with elements `overview` (named numeric summary),
#'   `by_product`, `by_trip_type`, `by_user_type` (data.tables of counts) and,
#'   when `gtfs` is given, `match` (the [netex_match_routes()] table) and
#'   `unmatched` (distinct operator/line pairs with no GTFS route). The list is
#'   returned invisibly after printing a short summary.
#' @export
netex_fares_report <- function(netex_list, gtfs = NULL) {
  types <- netex_fare_types(netex_list)
  fails <- netex_read_failures(netex_list)

  count_by <- function(col) {
    d <- types[, .N, by = col]
    data.table::setorder(d, -N)
    d
  }

  fare_kind <- if ("fare_kind" %in% names(types)) types$fare_kind else rep(NA_character_, nrow(types))
  overview <- c(
    files = nrow(types),
    failed_to_parse = nrow(fails),
    operators = length(unique(types$operator_noc)),
    lines = nrow(unique(types[!is.na(line_public_code), c("operator_noc", "line_public_code")])),
    zonal_fares = sum(fare_kind == "zonal", na.rm = TRUE),
    flat_fares = sum(fare_kind == "flat", na.rm = TRUE),
    no_fares = sum(types$n_prices == 0 | is.na(types$n_prices))
  )

  out <- list(
    overview = overview,
    by_product = count_by("product_name"),
    by_trip_type = count_by("trip_type"),
    by_user_type = count_by("user_type")
  )

  if (!is.null(gtfs)) {
    m <- netex_match_routes(netex_list, gtfs)
    out$match <- m
    lines <- unique(m[, c("operator_noc", "line_public_code", "matched")])
    out$unmatched <- unique(m[!m$matched, c("operator_noc", "line_public_code")])
    overview["lines_matched"] <- sum(lines$matched)
    overview["lines_unmatched"] <- sum(!lines$matched)
    out$overview <- overview
  }

  message("NeTEx fares report")
  message("  files:            ", overview["files"],
          "  (failed to parse: ", overview["failed_to_parse"], ")")
  message("  operators:        ", overview["operators"])
  message("  lines:            ", overview["lines"])
  message("  zonal fare files: ", overview["zonal_fares"])
  message("  flat  fare files: ", overview["flat_fares"])
  if (!is.null(gtfs)) {
    message("  lines matched to GTFS route: ", overview["lines_matched"],
            " / ", overview["lines_matched"] + overview["lines_unmatched"])
  }
  invisible(out)
}


#' Match NeTEx fare lines to GTFS routes
#'
#' Joins each parsed NeTEx fare object to a route in a GTFS object. The join
#' key is the operator National Operator Code (NeTEx `operator_noc` == GTFS
#' `agency_id`) together with the public line number (NeTEx `line_public_code`
#' == GTFS `route_short_name`). The GTFS `route_id` is generated from the
#' TransXChange ServiceCode and cannot be matched to the NeTEx line id
#' directly, which is why the public codes are used.
#'
#' @param netex_list list of parsed NeTEx fare objects.
#' @param gtfs a GTFS object (named list) containing at least `routes` (with
#'   `route_id`, `route_short_name`, `agency_id`).
#' @return a data.table with columns `idx` (index into `netex_list`),
#'   `operator_noc`, `line_public_code`, `route_id` and `matched` (logical).
#'   Unmatched NeTEx files have `route_id = NA`.
#' @export
netex_match_routes <- function(netex_list, gtfs) {
  routes <- data.table::as.data.table(gtfs$routes)
  routes <- data.table::copy(routes)
  routes[, route_id := as.character(route_id)]
  routes[, route_short_name := as.character(route_short_name)]
  if (!"agency_id" %in% names(routes)) routes[, agency_id := NA_character_]
  routes[, agency_key := toupper(as.character(agency_id))]

  types <- netex_fare_types(netex_list)
  types[, op_key := toupper(operator_noc)]

  # Vectorised two-stage update join (nationally there can be tens of thousands
  # of routes, so per-file loops are too slow):
  #  1. exact match on line number AND operator
  #  2. fall back to line number alone (first route) for still-unmatched lines
  r_op <- routes[, .(route_id = route_id[1]), by = c("route_short_name", "agency_key")]
  r_ln <- routes[, .(route_id = route_id[1]), by = "route_short_name"]

  m <- data.table::copy(types)
  m[, route_id := NA_character_]
  m[r_op, on = c(line_public_code = "route_short_name", op_key = "agency_key"),
    route_id := i.route_id]
  m[is.na(route_id),
    route_id := r_ln[.SD, on = c(route_short_name = "line_public_code"), x.route_id]]

  m[, matched := !is.na(route_id)]
  data.table::setorder(m, idx)
  m[, c("idx", "operator_noc", "line_public_code", "direction",
        "product_name", "trip_type", "user_type", "route_id", "matched"),
    with = FALSE]
}
