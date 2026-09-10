# One set of mode rules for every source.
#
# The British sources this package converts do not agree with each other about
# what a light railway is, and none of them is consistent with itself. The
# Docklands Light Railway is metro in NPTDR and heavy rail in TNDS. The Glasgow
# Subway is metro in NPTDR and a tram in TNDS. The Bluebell Railway is a tram
# in the 2018 TNDS snapshot, heavy rail in 2021 and a bus in 2025. NPTDR files
# the London Underground as a bus in 2004 and Sheffield Supertram as a bus in
# most years.
#
# None of that is recoverable from the declared vehicle type, so this file
# settles it once, the same way for every source:
#
#   * heritage and minor railways are rail
#   * the Underground, the Tyne and Wear Metro, the Glasgow Subway and the
#     Docklands Light Railway are metro
#   * the airport people movers are trams
#   * the street and segregated tramways are trams
#
# The key is the NaPTAN stop name, because it is the one thing that is stable.
# Operator codes are not: Manchester Metrolink appears in NPTDR as 1973, 1976,
# 2001, 2016 and 2024 in successive archives, Sheffield Supertram as both STM
# and SST, and several of those codes carry ordinary bus routes as well. Stop
# names name the system - "(Manchester Metrolink)", "(Bluebell Railway)",
# "Buchanan Street SPT Subway Station" - and NaPTAN spells them the same way in
# every source and every year.


#' Systems whose declared mode is not to be trusted, and what they really are
#'
#' @description British timetable sources disagree with each other, and with
#'   themselves between years, about which light railways are trams, which are
#'   metros and which are heavy rail. This table settles it, so that a mode
#'   means the same thing in every source and a series built across them is
#'   comparable.
#'
#' @details Each row names a system, a regular expression matching the NaPTAN
#'   names of the stops it serves, and the `route_type` it should carry.
#'   `operator` is filled in only where the stops cannot identify the system on
#'   their own: the London Underground because not all of its stop names are
#'   marked ("Wembley Park" carries no suffix), the Birmingham Air-Rail Link
#'   because it has two stops one of which is a mainline station, and the
#'   Weardale Railway because its stops are named as ordinary rail stations.
#'
#'   The rules are: heritage and minor railways are rail (2); the London
#'   Underground, the Tyne and Wear Metro, the Glasgow Subway and the Docklands
#'   Light Railway are metro (1); the airport people movers are trams (0); and
#'   the street and segregated tramways are trams (0).
#'
#' @return a data frame of `system`, `operator`, `stop_pattern`, `route_type`
#'   and `note`
#' @export
#' @examples
#' standard_mode_overrides()
standard_mode_overrides <- function() {
  data.frame(
    system = c(
      # metro
      "London Underground", "Tyne and Wear Metro", "Glasgow Subway",
      "Docklands Light Railway",
      # street and segregated tramways
      "Manchester Metrolink", "Midland Metro", "Sheffield Supertram",
      "Nottingham Express Transit", "Croydon Tramlink", "Blackpool Tramway",
      "Edinburgh Trams",
      # airport people movers, which are trams
      "Birmingham Air-Rail Link", "Gatwick Airport shuttle",
      # heritage and minor railways, which are rail
      "Heritage and minor railways", "Weardale Railway"),
    operator = c("LUL", NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA,
                 "BHX", NA,
                 NA, "WRLY"),
    stop_pattern = c(
      "Underground Station",
      "Tyne and Wear Metro|Metro Station",
      "SPT Subway",
      "DLR Station",
      "Manchester Metrolink",
      "Midland Metro|West Midlands Metro",
      "Sheffield Supertram",
      "Tram Stop",
      "Tramlink",
      "Blackpool Tramway",
      "Edinburgh Tram",
      "Air.?Rail Link|Skytrain",
      "Terminal Shuttle",
      "Railway[)]|Rly[)]|[(]RHDR[)]|Mull Rail",
      NA),
    route_type = c(1, 1, 1, 1,
                   0, 0, 0, 0, 0, 0, 0,
                   0, 0,
                   2, 2),
    note = c(
      "NPTDR files it as a bus in 2004; not all stop names are marked",
      "",
      "TNDS files it as a tram",
      "TNDS files it as heavy rail",
      "NPTDR operator code changes every year",
      "called Metro, runs on the street",
      "NPTDR files it as a bus in most years",
      "NPTDR: tram in 2006-2008, metro in 2009-2011",
      "", "", "",
      "two stops, one a mainline station, so matched on the operator",
      "TNDS files it as metro in five of eight snapshots",
      "TNDS files the Bluebell as tram, rail and bus in different years",
      "stops are named as ordinary rail stations"),
    stringsAsFactors = FALSE
  )
}


#' Apply the standard mode rules to a converted feed
#'
#' A route is reassigned only when at least `threshold` of the stops it calls
#' at belong to one system. A bus that passes a tram stop or two is nowhere
#' near that; a tramway published as a bus is at 100%. Operators named in the
#' table are matched on their code as well, for the systems whose stops cannot
#' identify them.
#'
#' Only `routes$route_type` is altered. Nothing is added, removed or renamed.
#'
#' @param gtfs a gtfs object with `routes`, `trips`, `stop_times` and `stops`
#' @param overrides a table shaped like [standard_mode_overrides()]
#' @param threshold proportion of a route's stops that must belong to one
#'   system before the route is reassigned
#' @param quiet suppress the summary message
#' @return the gtfs object, with `routes$route_type` corrected
#' @noRd
apply_standard_modes <- function(gtfs, overrides = standard_mode_overrides(),
                                 threshold = 0.8, quiet = TRUE) {
  if (is.null(overrides) || nrow(overrides) == 0) {
    return(gtfs)
  }
  needed <- c("routes", "trips", "stop_times", "stops")
  if (!all(needed %in% names(gtfs))) {
    return(gtfs)
  }
  if (any(vapply(gtfs[needed], function(x) is.null(x) || nrow(x) == 0,
                 logical(1)))) {
    return(gtfs)
  }

  route_type <- gtfs$routes$route_type
  route_id <- as.character(gtfs$routes$route_id)
  changed <- 0L

  pats <- overrides[!is.na(overrides$stop_pattern), , drop = FALSE]
  if (nrow(pats) > 0) {
    sp <- data.table::data.table(
      stop_id = as.character(gtfs$stops$stop_id),
      stop_name = as.character(gtfs$stops$stop_name))
    # Which system, if any, each stop belongs to. Done on the stops table
    # rather than on stop_times, which on a national feed is tens of millions
    # of rows against a few hundred thousand stops. useBytes because NaPTAN
    # names carry Latin-1 bytes R cannot translate, and the patterns are ASCII.
    sp[, TMP_sys := NA_integer_]
    for (i in seq_len(nrow(pats))) {
      hit <- is.na(sp$TMP_sys) &
        grepl(pats$stop_pattern[i], sp$stop_name, ignore.case = TRUE,
              useBytes = TRUE)
      sp[hit, TMP_sys := i]
    }
    marked <- sp[!is.na(TMP_sys), list(stop_id, TMP_sys)]
    rm(sp)

    if (nrow(marked) > 0) {
      st <- data.table::data.table(
        trip_id = as.character(gtfs$stop_times$trip_id),
        stop_id = as.character(gtfs$stop_times$stop_id))
      tr_all <- data.table::data.table(
        trip_id = as.character(gtfs$trips$trip_id),
        route_id = as.character(gtfs$trips$route_id))

      # Only a route that touches a marked stop can be reassigned, so the
      # expensive join is restricted to those routes before the stops of each
      # are counted. Every trip of a candidate route is needed, not just the
      # ones that touched a marked stop, or the denominator would be wrong.
      cand_trips <- unique(st[stop_id %in% marked$stop_id]$trip_id)
      cand_routes <- unique(tr_all[trip_id %in% cand_trips]$route_id)
      tr <- tr_all[route_id %in% cand_routes]
      rm(tr_all)
      rs <- unique(merge(st[trip_id %in% tr$trip_id], tr,
                         by = "trip_id")[, list(route_id, stop_id)])
      rm(st)

      rs <- merge(rs, marked, by = "stop_id", all.x = TRUE)
      n_stops <- rs[, list(n = .N), by = "route_id"]
      # the counted column must not be called `hits` too: inside [.data.table
      # the bare name would resolve to the column, not the table
      hits <- rs[!is.na(TMP_sys),
                 list(n_hit = .N), by = c("route_id", "TMP_sys")]
      hits <- merge(hits, n_stops, by = "route_id")
      hits <- hits[n_hit / n >= threshold]

      if (nrow(hits) > 0) {
        # where two systems both clear the threshold, the closer match wins
        hits[, share := n_hit / n]
        data.table::setorderv(hits, c("route_id", "share"), c(1L, -1L))
        hits <- hits[!duplicated(hits$route_id)]
        pos <- match(hits$route_id, route_id)
        ok <- !is.na(pos)
        want <- pats$route_type[hits$TMP_sys[ok]]
        changed <- changed + sum(route_type[pos[ok]] != want)
        route_type[pos[ok]] <- want
      }
    }
  }

  # the systems whose stops cannot identify them
  if ("operator" %in% names(overrides) && "agency_id" %in% names(gtfs$routes)) {
    ag <- toupper(trimws(as.character(gtfs$routes$agency_id)))
    for (i in which(!is.na(overrides$operator))) {
      hit <- !is.na(ag) & ag == toupper(trimws(overrides$operator[i]))
      changed <- changed + sum(hit & route_type != overrides$route_type[i])
      route_type[hit] <- overrides$route_type[i]
    }
  }

  if (!quiet) {
    message("apply_standard_modes: corrected the mode of ", changed, " routes")
  }
  gtfs$routes$route_type <- route_type
  gtfs
}
