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
#   * the one aerial cable car is a gondola, which is its own GTFS mode
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
#'   `sources` limits a row to the converters whose data can contain that
#'   system, and exists because an operator code is not a stable identifier.
#'   NPTDR reuses codes freely between archives, so a rule keyed on one can
#'   fire on an unrelated operator in an earlier year: `CAB` is the London
#'   cable car in TransXChange from 2023, but in the 2010 and 2011 NPTDR
#'   archives it is somebody else entirely, and without this column those
#'   services were relabelled as an aerial lift two years before the cable car
#'   was built. Rows matched on stop names need no such limit - a stop called
#'   "Buchanan Street SPT Subway Station" is the Glasgow Subway in any year -
#'   so `"any"` is the normal value and only the operator-matched rows for
#'   systems that did not yet exist are restricted.
#'
#'   The rules are: heritage and minor railways are rail (2); the London
#'   Underground, the Tyne and Wear Metro, the Glasgow Subway and the Docklands
#'   Light Railway are metro (1); the airport people movers are trams (0); and
#'   the street and segregated tramways are trams (0). The London cable car is
#'   the one system none of those describe, and it takes the GTFS mode that
#'   does, aerial lift (6).
#'
#' @return a data frame of `system`, `operator`, `stop_pattern`, `route_type`,
#'   `sources` and `note`
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
      "Birmingham Air-Rail Link", "Gatwick Airport shuttle", "Luton DART",
      # heritage and minor railways, which are rail
      "Heritage and minor railways", "Weardale Railway",
      # the aerial cable car, under both the operator codes it has carried
      "London Cable Car", "London Cable Car"),
    operator = c("LUL", NA, NA, NA,
                 NA, NA, NA, NA, NA, NA, NA,
                 "BHX", NA, "DART",
                 NA, "WRLY",
                 "CAB", "EAL"),
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
      NA,
      "Railway[)]|Rly[)]|[(]RHDR[)]|Mull Rail",
      NA,
      NA,
      NA),
    route_type = c(1, 1, 1, 1,
                   0, 0, 0, 0, 0, 0, 0,
                   0, 0, 0,
                   2, 2,
                   6, 6),
    # "any" unless the system postdates a source: the Luton DART opened in
    # 2023 and the cable car in 2012, so neither can appear in NPTDR, which
    # ends in 2011. LUL, BHX and WRLY are matched on operator too but all
    # three do appear in NPTDR and are correct there.
    sources = c(
      "any", "any", "any", "any",
      "any", "any", "any", "any", "any", "any", "any",
      "any", "any", "txc",
      "any", "any",
      "txc", "txc"),
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
      "TNDS files it as heavy rail; two stops, one a mainline station",
      "TNDS files the Bluebell as tram, rail and bus in different years",
      "stops are named as ordinary rail stations",
      "TNDS files it as heavy rail; stop names identify nothing",
      "the same cable car before the sponsor changed"),
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
#' @param source which converter is calling: `"txc"`, `"nptdr"` or `"atoc"`.
#'   Rows whose `sources` column does not name it are skipped, which is what
#'   keeps an operator-code rule for a system built in 2023 from firing on a
#'   2010 archive that happens to reuse the code. `NULL` applies every row.
#' @param quiet suppress the summary message
#' @return the gtfs object, with `routes$route_type` corrected
#' @noRd
apply_standard_modes <- function(gtfs, overrides = standard_mode_overrides(),
                                 threshold = 0.8, source = NULL,
                                 quiet = TRUE) {
  if (is.null(overrides) || nrow(overrides) == 0) {
    return(gtfs)
  }
  if (!is.null(source) && "sources" %in% names(overrides)) {
    keep <- vapply(overrides$sources, function(x) {
      if (is.na(x) || !nzchar(x)) return(TRUE)
      parts <- trimws(strsplit(x, ",", fixed = TRUE)[[1]])
      "any" %in% parts || source %in% parts
    }, logical(1), USE.NAMES = FALSE)
    overrides <- overrides[keep, , drop = FALSE]
    if (nrow(overrides) == 0) return(gtfs)
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
