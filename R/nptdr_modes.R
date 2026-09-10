# Correcting the mode of NPTDR light rail services.
#
# The ATCO-CIF vehicle type in NPTDR is a coarse instrument. "METRO" is a
# catch-all for everything that is neither a bus, a train nor a ferry, and the
# archives use it for the London Underground, the Glasgow Subway, every British
# tramway, the airport people movers and a long tail of heritage railways
# alike. Worse, the same system is filed inconsistently: Nottingham Express
# Transit is a tram in 2006-2008 and a metro in 2009-2011, and the London
# Underground is a *bus* in 2004.
#
# The operator code cannot fix it, because it is not stable. Manchester
# Metrolink appears as 1973, 1976, 2001, 2016 and 2024 in successive archives,
# Sheffield Supertram as both STM and SST, and several of those codes also
# carry ordinary bus routes.
#
# The stop names are stable, because they come from NaPTAN, and they name the
# system: "(Manchester Metrolink)", "(Sheffield Supertram)", "Tramlink Stop".
# That is what this keys on. NPTDR is a closed archive - the last one was
# published in 2011 and none of it will change - so a hardcoded table of the
# systems in it is a complete answer rather than a stopgap.


#' Light rail systems in the NPTDR archives, and the mode each really is
#'
#' @description The National Public Transport Data Repository files every
#'   tramway and every metro under one vehicle type, and not even consistently
#'   from one year to the next. This table says which is which, identified by
#'   the NaPTAN names of the stops each system serves.
#'
#' @details Metro (`route_type` 1) is a grade-separated urban railway: the
#'   London Underground, the Docklands Light Railway, the Tyne and Wear Metro
#'   and the Glasgow Subway. Tram (`route_type` 0) is street or segregated
#'   light rail: Manchester Metrolink, Midland Metro, Sheffield Supertram,
#'   Nottingham Express Transit, Croydon Tramlink and the Blackpool Tramway.
#'   Two of the tramways are called "Metro" by their owners, which is how they
#'   came to be filed as one.
#'
#'   The airport people movers and the heritage railways NPTDR also files as
#'   metro are deliberately absent: neither is a tram or a metro, and this
#'   table only claims to sort out the two it names.
#'
#' @return a data frame of `system`, `stop_pattern`, `route_type` and `note`
#' @export
#' @examples
#' nptdr_mode_overrides()
nptdr_mode_overrides <- function() {
  data.frame(
    system = c("London Underground", "Docklands Light Railway",
               "Tyne and Wear Metro", "Glasgow Subway",
               "Manchester Metrolink", "Midland Metro",
               "Sheffield Supertram", "Nottingham Express Transit",
               "Croydon Tramlink", "Blackpool Tramway"),
    stop_pattern = c("Underground Station",
                     "DLR Station",
                     "Tyne and Wear Metro|Metro Station",
                     "SPT Subway",
                     "Manchester Metrolink",
                     "Midland Metro",
                     "Sheffield Supertram",
                     "Tram Stop",
                     "Tramlink",
                     "Blackpool Tramway"),
    route_type = c(1, 1, 1, 1, 0, 0, 0, 0, 0, 0),
    note = c("filed as bus in the 2004 archive",
             "", "", "",
             "operator code changes every year",
             "called Metro, runs on the street",
             "filed as bus in most years",
             "tram in 2006-2008, metro in 2009-2011",
             "", ""),
    stringsAsFactors = FALSE
  )
}


#' Correct NPTDR light rail modes from the stops each route serves
#'
#' A route is reassigned only when at least `threshold` of the stops it calls
#' at belong to one system. A bus that passes a tram stop or two is nowhere
#' near that; a tramway published as a bus is at 100%. The London Underground
#' is additionally matched on its operator code, because `LUL` runs nothing
#' else and its stop names are not all marked ("Wembley Park" carries no
#' suffix).
#'
#' @param gtfs a gtfs object with `routes`, `trips`, `stop_times` and `stops`
#' @param overrides a table shaped like [nptdr_mode_overrides()]
#' @param threshold proportion of a route's stops that must belong to one
#'   system before the route is reassigned
#' @param quiet suppress the summary message
#' @return the gtfs object, with `routes$route_type` corrected
#' @noRd
apply_nptdr_modes <- function(gtfs, overrides = nptdr_mode_overrides(),
                              threshold = 0.8, quiet = TRUE) {
  if (is.null(overrides) || nrow(overrides) == 0) return(gtfs)
  needed <- c("routes", "trips", "stop_times", "stops")
  if (!all(needed %in% names(gtfs))) return(gtfs)
  if (any(vapply(gtfs[needed], function(x) is.null(x) || nrow(x) == 0, logical(1)))) {
    return(gtfs)
  }

  st <- data.table::data.table(
    trip_id = as.character(gtfs$stop_times$trip_id),
    stop_id = as.character(gtfs$stop_times$stop_id))
  tr <- data.table::data.table(
    trip_id = as.character(gtfs$trips$trip_id),
    route_id = as.character(gtfs$trips$route_id))
  sp <- data.table::data.table(
    stop_id = as.character(gtfs$stops$stop_id),
    stop_name = as.character(gtfs$stops$stop_name))

  # one row per route per stop it calls at, names attached
  rs <- unique(merge(st, tr, by = "trip_id")[, list(route_id, stop_id)])
  rs <- merge(rs, sp, by = "stop_id")
  if (nrow(rs) == 0) return(gtfs)

  n_stops <- rs[, list(n = .N), by = "route_id"]
  best <- data.table::data.table(route_id = character(0), route_type = numeric(0),
                                 share = numeric(0))
  for (i in seq_len(nrow(overrides))) {
    o <- overrides[i, ]
    # useBytes because NPTDR stop names carry Latin-1 bytes ("Square Deal
    # Cafe") that R cannot translate, and every one of them would otherwise
    # raise a warning. The patterns are ASCII, so byte matching is exact.
    hit <- rs[grepl(o$stop_pattern, stop_name, ignore.case = TRUE,
                    useBytes = TRUE),
              list(hits = .N), by = "route_id"]
    if (nrow(hit) == 0) next
    hit <- merge(hit, n_stops, by = "route_id")
    hit <- hit[hits / n >= threshold]
    if (nrow(hit) == 0) next
    best <- rbind(best, data.table::data.table(
      route_id = hit$route_id, route_type = o$route_type,
      share = hit$hits / hit$n))
  }

  route_type <- gtfs$routes$route_type
  route_id <- as.character(gtfs$routes$route_id)
  changed <- 0L

  if (nrow(best) > 0) {
    # where two systems both clear the threshold, the closer match wins
    data.table::setorderv(best, c("route_id", "share"), c(1L, -1L))
    best <- best[!duplicated(best$route_id)]
    pos <- match(best$route_id, route_id)
    ok <- !is.na(pos)
    changed <- changed + sum(route_type[pos[ok]] != best$route_type[ok])
    route_type[pos[ok]] <- best$route_type[ok]
  }

  # LUL runs the Underground and nothing else, whatever the archive says the
  # vehicle is - in 2004 it says bus
  if ("agency_id" %in% names(gtfs$routes)) {
    lul <- !is.na(gtfs$routes$agency_id) &
      toupper(trimws(as.character(gtfs$routes$agency_id))) == "LUL"
    changed <- changed + sum(lul & route_type != 1)
    route_type[lul] <- 1
  }

  if (!quiet) {
    message("apply_nptdr_modes: corrected the mode of ", changed, " routes")
  }
  gtfs$routes$route_type <- route_type
  gtfs
}
