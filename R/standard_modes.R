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
  base <- data.frame(
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
      # NPTDR 2004 and 2005 suffix the stops "(Metrolink)" and the later
      # archives "(Manchester Metrolink)", so a pattern taken from either one
      # alone misses the other. Matching the shorter form as well is what
      # moves the Manchester trams out of the metro totals in those two
      # years; see test_modes_early_nptdr.R.
      "Manchester Metrolink|[(]Metrolink[)]",
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
    # `years` and `from_route_type` are only used by the NPTDR block below
    years = NA_character_,
    from_route_type = NA_integer_,
    stringsAsFactors = FALSE
  )

  # ---- the NPTDR archives, 2004-2006 --------------------------------------
  #
  # These five rows are deliberately hardcoded to the archives we hold. NPTDR
  # ended in 2011 and will not be republished, so a rule that is right about
  # these specific files is right permanently; the usual objection to keying on
  # an operator code - that the code may mean someone else next year - cannot
  # apply to a dataset with no next year.
  #
  # They exist because the stop-name patterns above were written from the 2006
  # and later archives. 2004 and 2005 name the same stops differently, and for
  # these four systems they carry no marker at all - the Supertram calls at
  # "Meadowhall Interchange" and "Carbrook", the Blackpool tramway at
  # "FLEETWOOD Rossall Lane" - so no pattern can find them and they kept
  # whatever mode the converter guessed, which was metro. Measured: Sheffield
  # 27 routes in 2004, 15+23 in 2005 and 43 in 2006; Nottingham 29+20 in 2004;
  # the Midland Metro 13 in 2004; Blackpool 20 in 2005 and 20 in 2006.
  #
  # Note the 2005 Sheffield and 2004 Nottingham splits. Part of each system is
  # filed as metro and part as bus in the same archive, and both parts call at
  # the same tram stops, so the rule has to take the operator whole rather than
  # correct one mode and leave the other. Hence no `from_route_type` on those.
  #
  # `STM`, `NET` and `TMM` need no year gate: each means the same system in
  # every NPTDR archive, and in 2007-2011 all three are already trams, so the
  # rule is a no-op there. They appear under no other converter.
  #
  # The four numeric codes are the opposite case and are gated hard, on the
  # archive year AND on the mode they start from. NPTDR renumbers operators
  # every year and these codes carry a bus fleet as well as the tramway:
  # `1129` is Blackpool's 20 tram routes plus 192 bus routes in 2005, and 132
  # bus routes and no tramway at all in 2004; `1001` is the same arrangement in
  # 2006; `1973` is Manchester Metrolink in 2004 and an ordinary bus operator
  # from 2006. Ungated, any of them would relabel several hundred bus routes.
  #
  # Manchester needs the operator code as well as the "(Metrolink)" stop
  # pattern above, because the pattern reaches only part of the network. The
  # Eccles line, opened in 2000, was never given the suffix - its stops are
  # "EXCHANGE QUAY", "SALFORD QUAYS", "HARBOUR CITY", "ECCLES" - so routes over
  # it sit at 0.15-0.25 marked stops, and the city-centre routes that touch
  # Piccadilly or Victoria sit at 0.67-0.79, just under the 0.8 threshold. The
  # pattern alone moved 8 of 18 routes in 2004 and 7 of 34 in 2005. All of them
  # are Metrolink: every route under the code is filed as metro, and their
  # short names are MET1, MET2, MET3.
  nptdr <- data.frame(
    system = c("Sheffield Supertram", "Nottingham Express Transit",
               "Midland Metro", "Blackpool Tramway", "Blackpool Tramway",
               "Manchester Metrolink", "Manchester Metrolink"),
    operator = c("STM", "NET", "TMM", "1129", "1001", "1973", "1976"),
    stop_pattern = NA_character_,
    route_type = 0,
    sources = "nptdr",
    note = c(
      "stops carry no marker before 2007; metro in 2004 and 2006, split between metro and bus in 2005",
      "stops carry no marker in 2004, where the system is split between metro and bus",
      "13 routes filed as metro in 2004, and their stop ids resolve to nothing",
      "20 tram routes under a code that also carries 192 bus routes",
      "the same arrangement one year later under a renumbered code",
      "18 routes; the Eccles line carries no suffix so the stop pattern reaches only 8",
      "34 routes under a renumbered code; the stop pattern reaches only 7"),
    years = c(NA, NA, NA, "2005", "2006", "2004", "2005"),
    from_route_type = c(NA, NA, NA, 1L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )

  rbind(base, nptdr[, names(base), drop = FALSE])
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
                                 year = NULL, quiet = TRUE) {
  if (is.null(overrides) || nrow(overrides) == 0) {
    return(gtfs)
  }

  # A rule naming particular years applies to those only. Two of the NPTDR
  # rules key on a numeric operator code, which that dataset reassigns every
  # year, so the gate is what keeps them off the wrong archive. A rule that
  # names years is dropped when the caller has not said which year this is,
  # rather than applied blind: no year means no evidence that it is the right
  # one, and these rules exist precisely because the code is ambiguous.
  if ("years" %in% names(overrides)) {
    named <- !is.na(overrides$years) & nzchar(overrides$years)
    if (any(named)) {
      ok <- rep(TRUE, nrow(overrides))
      if (is.null(year)) {
        ok[named] <- FALSE
      } else {
        yr <- as.character(year)
        ok[named] <- vapply(overrides$years[named], function(x) {
          yr %in% trimws(strsplit(x, ",", fixed = TRUE)[[1]])
        }, logical(1), USE.NAMES = FALSE)
      }
      overrides <- overrides[ok, , drop = FALSE]
      if (nrow(overrides) == 0) return(gtfs)
    }
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

  # Which routes touch a stop belonging to each stop-pattern system, kept
  # whatever the threshold decided. The operator block below uses it to check
  # that a code means in this feed what the rule says it means; see the
  # comment there. Row numbers index `pats`, so `pats_row` maps an
  # `overrides` row onto its `pats` row.
  sys_routes <- NULL
  pats_row <- rep(NA_integer_, nrow(overrides))
  pats_row[which(!is.na(overrides$stop_pattern))] <- seq_len(nrow(pats))

  if (nrow(pats) > 0) {
    sp <- data.table::data.table(
      stop_id = as.character(gtfs$stops$stop_id),
      stop_name = as.character(gtfs$stops$stop_name))

    # Most rules recognise a system by its stops' NAPTAN names, so a feed
    # whose stops are not named yet cannot match any of them. That is a
    # caller error - the names are attached by a join that has to happen
    # first - and it is invisible from the result, because no route matching
    # looks exactly like no route needing correction. transxchange2gtfs()
    # once ran this before txc_join_naptan() and quietly produced a whole
    # series of feeds with the Docklands Light Railway filed as heavy rail.
    if (!any(!is.na(sp$stop_name) & nzchar(sp$stop_name))) {
      warning("apply_standard_modes: no stop carries a name, so the ",
              nrow(pats), " stop-pattern rules cannot match. Attach the ",
              "stop names before calling this.", call. = FALSE)
    }

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
      sys_routes <- unique(hits[, list(route_id, TMP_sys)])
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

      # Some codes cover a tramway and a bus fleet at once. Where the rule says
      # which mode it is correcting, only routes already on that mode move, so
      # the operator's buses are left as buses. Where it does not, the operator
      # is taken whole - which is what the 2005 Sheffield and 2004 Nottingham
      # archives need, both filing one tram system partly as metro and partly
      # as bus, with both parts calling at the same tram stops.
      if ("from_route_type" %in% names(overrides) &&
            !is.na(overrides$from_route_type[i])) {
        hit <- hit & !is.na(route_type) &
          route_type == overrides$from_route_type[i]
      }
      if (!any(hit)) next

      # An operator code is not a unique identifier, and `sources` only keeps
      # a rule away from a converter that cannot contain the system at all.
      # Within one converter the same code still means different operators in
      # different archives: `LUL` is London Underground in TNDS and in NPTDR,
      # but in the 2016 and 2017 Bus Archive it is Lancashire United Ltd, and
      # firing the rule there moved 66 Lancashire bus routes into the metro
      # totals. Both are TransXChange, so no `sources` value separates them.
      #
      # Where a rule carries a stop pattern as well, the feed can settle it:
      # require at least one of the operator's own routes to call at a stop
      # this system's pattern matches. Lancashire United calls at no stop
      # named "Underground Station" and the rule stands down; every real
      # Underground feed has such stops on 93.6-100% of the routes, so
      # nothing that should fire stops firing. Rules with no stop pattern -
      # the Weardale Railway, the Luton DART, the cable car - have only the
      # code to go on and keep the old behaviour, guarded by `sources`.
      if (!is.na(pats_row[i])) {
        ok <- !is.null(sys_routes) &&
          any(sys_routes$TMP_sys == pats_row[i] &
                sys_routes$route_id %in% route_id[hit])
        if (!ok) {
          if (!quiet) {
            message("apply_standard_modes: operator ",
                    overrides$operator[i], " matches ", sum(hit),
                    " routes but none call at a stop named like ",
                    overrides$system[i], "; leaving them alone")
          }
          next
        }
      }

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
