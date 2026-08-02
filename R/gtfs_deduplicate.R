# Detection and removal of duplicated vehicle journeys
#
# Feeds assembled from several publishers (or from several revisions of the
# same publisher's data) routinely describe one bus, train or ferry twice. The
# duplicate is a statement about the feed rather than about the road: it
# inflates every count made from the feed, but no extra vehicle runs.
#
# Removing them is only safe if two things are true of a pair of trips: they
# really are the same journey, and the copy being removed runs on no date that
# the copy being kept does not. This file tests both, and removes nothing
# unless both hold.


#' Parse a GTFS date column
#'
#' GTFS dates arrive as `Date`, `IDate`, an eight digit integer (`fread` on a
#' raw feed) or "YYYYMMDD"/"YYYY-MM-DD" text, depending on how the feed was
#' read. `as.Date()` alone silently fails or errors on some of these.
#'
#' @param x a vector of dates
#' @return a Date vector
#'
#' @noRd
gtfs_as_date <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    return(as.Date(as.character(x), format = "%Y%m%d"))
  }
  if (is.character(x)) {
    nonmiss <- x[!is.na(x) & nzchar(x)]
    if (length(nonmiss) > 0 && all(grepl("^[0-9]{8}$", nonmiss))) {
      return(as.Date(x, format = "%Y%m%d"))
    }
    return(as.Date(x, format = "%Y-%m-%d"))
  }
  as.Date(x)
}


#' The dates each service actually operates on
#'
#' Expands `calendar.txt` and `calendar_dates.txt` into one row per service per
#' operating date, using GTFS semantics: the day-of-week flags apply inside
#' `start_date` to `end_date`, `exception_type = 2` removes a date and
#' `exception_type = 1` adds one (including dates outside the calendar range,
#' and for services that have no `calendar.txt` row at all).
#'
#' Dates are returned as integers (days since 1970-01-01) because the callers
#' only ever test them for equality and the tables involved can be very large.
#'
#' @param gtfs a gtfs object
#' @param service_ids optional character vector, expand only these services
#' @return a data.table of `service_id` and `date`
#'
#' @noRd
service_operating_dates <- function(gtfs, service_ids = NULL) {
  dow <- c("monday", "tuesday", "wednesday", "thursday", "friday",
           "saturday", "sunday")

  base <- data.table::data.table(service_id = character(0), date = integer(0))

  cal <- gtfs$calendar
  if (!is.null(cal) && nrow(cal) > 0 && all(dow %in% names(cal))) {
    cal <- as.data.frame(cal)
    cal$service_id <- as.character(cal$service_id)
    if (!is.null(service_ids)) {
      cal <- cal[cal$service_id %in% service_ids, , drop = FALSE]
    }
  } else {
    cal <- NULL
  }

  if (!is.null(cal) && nrow(cal) > 0) {
    start_int <- as.integer(gtfs_as_date(cal$start_date))
    end_int <- as.integer(gtfs_as_date(cal$end_date))
    len <- end_int - start_int + 1L
    ok <- !is.na(start_int) & !is.na(end_int) & !is.na(len) & len > 0L

    if (any(ok)) {
      rows <- which(ok)
      idx <- rep(rows, len[ok])
      dates <- rep(start_int[ok], len[ok]) + (sequence(len[ok]) - 1L)
      # 1970-01-01 was a Thursday, so day 0 must map to weekday 4
      wday <- ((dates + 3L) %% 7L) + 1L

      flags <- matrix(unlist(lapply(dow, function(d) {
        v <- cal[[d]]
        if (is.logical(v)) as.integer(v) else suppressWarnings(as.integer(as.character(v)))
      }), use.names = FALSE), ncol = 7L)

      runs <- flags[cbind(idx, wday)]
      runs <- !is.na(runs) & runs == 1L
      base <- data.table::data.table(service_id = cal$service_id[idx][runs],
                                     date = dates[runs])
    }
  }

  cd <- gtfs$calendar_dates
  if (!is.null(cd) && nrow(cd) > 0) {
    cd <- data.table::data.table(
      service_id = as.character(cd$service_id),
      date = as.integer(gtfs_as_date(cd$date)),
      exception_type = suppressWarnings(as.integer(cd$exception_type)))
    if (!is.null(service_ids)) {
      cd <- cd[service_id %in% service_ids]
    }
    cd <- cd[!is.na(date) & !is.na(exception_type)]
    # a date can only be added or removed once per service
    cd <- unique(cd, by = c("service_id", "date", "exception_type"))

    rem <- cd[exception_type == 2L, list(service_id, date)]
    add <- cd[exception_type == 1L, list(service_id, date)]
    if (nrow(rem) > 0) {
      base <- base[!rem, on = c("service_id", "date")]
    }
    if (nrow(add) > 0) {
      base <- unique(data.table::rbindlist(list(base, add)))
    }
  }

  base[]
}


#' Convert a GTFS time column to seconds, treating blanks as missing
#'
#' @param x a stop_times time column
#' @return numeric seconds since midnight, NA where no time is given
#'
#' @noRd
gtfs_time_secs_strict <- function(x) {
  secs <- suppressWarnings(gtfs_time_to_seconds(x))
  if (is.character(x)) {
    secs[!nzchar(x)] <- NA_real_
  }
  secs[is.na(x)] <- NA_real_
  secs
}


#' A grouping key for the operator of a route
#'
#' `agency_id` identifies an agency *record*, not an operator, and one operator
#' is regularly filed under several records in the same feed. Grouping on the
#' record therefore splits a service published twice by one operator into two
#' groups, where it can never be recognised as duplicated. Comparing the
#' operator's name instead re-joins them.
#'
#' The name is normalised to lower case with runs of non alphanumeric
#' characters collapsed to a single space, so "Go-Ahead London" and "Go Ahead
#' London" are one operator. An agency with no usable name falls back to its
#' `agency_id`, which leaves it grouped exactly as it was before.
#'
#' @param gtfs a gtfs object
#' @param agency_id character, the agency id of each route, already aligned to
#'   the trips being grouped
#' @param match_operator "name" or "agency_id"
#' @return a character vector the same length as `agency_id`
#' @noRd
operator_key <- function(gtfs, agency_id, match_operator = "name") {
  if (!identical(match_operator, "name")) {
    return(agency_id)
  }
  ag <- gtfs$agency
  if (is.null(ag) || nrow(ag) == 0 || !"agency_name" %in% names(ag)) {
    return(agency_id)
  }
  ag <- as.data.frame(ag)
  nm <- if ("agency_id" %in% names(ag)) {
    as.character(ag$agency_name)[match(agency_id, as.character(ag$agency_id))]
  } else if (nrow(ag) == 1L) {
    # a single agency needs no id: every route belongs to it
    rep(as.character(ag$agency_name)[1], length(agency_id))
  } else {
    return(agency_id)
  }

  nm <- tolower(gsub("[^[:alnum:]]+", " ", nm))
  nm <- trimws(nm)
  # an agency with no usable name keeps its id, so it groups as it did before
  bad <- is.na(nm) | !nzchar(nm)
  nm[bad] <- paste0("\ragency_id\r", agency_id[bad])
  nm
}


#' Remove duplicated trips from a GTFS object
#'
#' @description Finds trips that describe the same vehicle journey twice and
#'   removes the redundant copies, leaving the rest of the feed untouched. A
#'   duplicate is a property of the feed rather than of the road: two identical
#'   journeys on the same day is one vehicle described twice, and it inflates
#'   every count made from the feed.
#'
#' @param gtfs a gtfs object
#' @param match_route how much the routes of two trips must agree before they
#'   can be called duplicates. `"short_name"` (the default) requires the same
#'   operator, `route_type` and `route_short_name`, so the same service
#'   published twice under different `route_id`s is caught; `"route_id"`
#'   requires the identical `route_id`, which is stricter; `"none"` compares
#'   itineraries alone, which is the least cautious.
#' @param match_operator how two routes must agree on their operator when
#'   `match_route = "short_name"`. `"name"` (the default) compares the
#'   operator's name from `agency.txt`, ignoring case and punctuation;
#'   `"agency_id"` requires the identical `agency_id`, which is stricter. One
#'   operator is quite often filed under two `agency_id`s in the same feed -
#'   Arriva London North appears in the DfT's GTFS as both `OP401` (NOC `ARVA`)
#'   and `OP16197` (NOC `ALNO`) - and with `"agency_id"` its duplicate journeys
#'   land in different groups and survive.
#' @param match_block logical, whether `block_id` must agree before two trips
#'   can be called duplicates. `FALSE` by default, because feeds routinely fill
#'   `block_id` with a value generated per dataset revision rather than a
#'   stable reference to a vehicle's day: in the DfT's GTFS it is a 40
#'   character hash, so two copies of one journey never agree on it and
#'   requiring agreement would let every such duplicate through. Set `TRUE` for
#'   a feed whose `block_id` really does identify a vehicle block.
#' @param quiet logical, suppress the summary message
#' @return the gtfs object with duplicate trips, and their `stop_times` and
#'   `frequencies` rows, removed
#'
#' @details
#' Two trips are treated as the same journey only when all of the following
#' hold.
#'
#' 1. **Identical itineraries.** The whole sequence of (`stop_id`,
#'    `arrival_time`, `departure_time`) must match, in order, at every stop -
#'    together with `pickup_type` and `drop_off_type` where the feed supplies
#'    them. Times are compared as seconds since midnight, so times past
#'    24:00:00 and the different classes a time column can arrive in (lubridate
#'    Period, `ITime`, text) all compare correctly. Trips with fewer than two
#'    stops, or with fewer than two stops that carry a time, are never removed,
#'    because their signature is too weak to be sure.
#' 2. **The same route**, to the degree set by `match_route` and
#'    `match_operator`.
#' 3. **The same trip attributes.** Where the feed has them, `direction_id`,
#'    `wheelchair_accessible` and `bikes_allowed` must agree - and `block_id`
#'    too when `match_block = TRUE`. Trips differing in any of these carry
#'    information that removal would lose: an accessible journey is not
#'    interchangeable with one not marked accessible.
#' 4. **Redundant operating dates.** `calendar.txt` and `calendar_dates.txt`
#'    are expanded to the actual dates each service runs, and a copy is removed
#'    only when every date it runs is also run by a copy that is kept. Nothing
#'    that would leave a date with less service than it started with is
#'    touched.
#'
#' The defaults of `match_operator` and `match_block` are deliberately the
#' looser of the two settings each offers, because the stricter reading turned
#' out to key identity on fields that carry no information about the road. Both
#' were measured against published timetables on the July 2026 DfT GTFS: with
#' `match_block = TRUE` and `match_operator = "agency_id"` the feed's copy of
#' First Bristol's 21 stayed at 5,816 journeys over a four week window against
#' the 3,296 its operator prints, and with the defaults it lands on 3,296
#' exactly - as does First Bristol's A1, at 6,916 against 6,916. Nationally the
#' defaults raise the trips removed from that feed from 3.2% to 5.2%. Restore
#' either setting for a feed that fills the field meaningfully.
#'
#' The fourth test is the one that matters most. GTFS models a school-term
#' journey and its school-holiday twin as two trips with identical times and
#' complementary calendars, which is correct modelling and not duplication; a
#' test that ignored dates would remove one of them and delete real service.
#' Where two copies overlap only partly, both are kept - trimming a calendar to
#' resolve the overlap would change service the caller did not ask to change.
#'
#' Trips listed in `frequencies.txt` are never removed. There the stop times
#' are a template rather than a journey, so two identical templates with
#' different headways are not duplicates.
#'
#' Only `trips`, `stop_times` and `frequencies` are altered, because leaving
#' the rows of a removed trip behind would make the feed invalid. Routes,
#' calendars, shapes and stops are left exactly as they were, even where a
#' removal leaves one unused - that is valid GTFS, and [gtfs_clean()] will tidy
#' it if the caller wants it tidied. With the default `match_route`, a service
#' published twice under two `route_id`s loses the trips of one of them, so
#' that `route_id` is left in `routes.txt` with no trips against it.
#'
#' Expanding the calendars is the expensive step, so it is done only for the
#' services of trips that have already matched on every other test.
#'
#' @examples
#' \dontrun{
#' gtfs <- gtfs_read("feed.zip")
#' gtfs <- gtfs_deduplicate(gtfs)
#' }
#' @export
gtfs_deduplicate <- function(gtfs,
                             match_route = c("short_name", "route_id", "none"),
                             match_operator = c("name", "agency_id"),
                             match_block = FALSE,
                             quiet = FALSE) {
  match_route <- match.arg(match_route)
  match_operator <- match.arg(match_operator)

  if (is.null(gtfs$trips) || nrow(gtfs$trips) == 0 ||
      is.null(gtfs$stop_times) || nrow(gtfs$stop_times) == 0) {
    return(gtfs)
  }

  trip_id_all <- as.character(gtfs$trips$trip_id)
  if (anyDuplicated(trip_id_all) > 0) {
    warning("trips$trip_id is not unique, cannot deduplicate safely")
    return(gtfs)
  }

  st_names <- names(gtfs$stop_times)
  time_cols <- intersect(c("arrival_time", "departure_time"), st_names)
  if (length(time_cols) == 0) {
    warning("stop_times has no arrival_time or departure_time, cannot deduplicate safely")
    return(gtfs)
  }

  # ---- journey signatures ------------------------------------------------
  # Pull only the columns needed; stop_times can run to tens of millions of
  # rows and copying the rest of it is pure cost.
  st <- data.table::data.table(
    trip_id = as.character(gtfs$stop_times$trip_id),
    stop_id = as.character(gtfs$stop_times$stop_id))

  data.table::set(st, j = "TMP_seq",
                  value = if ("stop_sequence" %in% st_names) {
                    suppressWarnings(as.numeric(gtfs$stop_times$stop_sequence))
                  } else {
                    # no stop_sequence: fall back to the order of the rows
                    seq_len(nrow(st))
                  })

  arr <- if ("arrival_time" %in% time_cols) {
    gtfs_time_secs_strict(gtfs$stop_times$arrival_time)
  } else {
    rep(NA_real_, nrow(st))
  }
  dep <- if ("departure_time" %in% time_cols) {
    gtfs_time_secs_strict(gtfs$stop_times$departure_time)
  } else {
    rep(NA_real_, nrow(st))
  }
  data.table::set(st, j = "TMP_timed", value = !is.na(arr) | !is.na(dep))

  # NA and 0 mean the same thing in pickup_type/drop_off_type, so normalise
  # rather than let a blank block a removal that is otherwise certain
  boarding <- function(nm) {
    if (!nm %in% st_names) {
      return(NULL)
    }
    v <- as.character(gtfs$stop_times[[nm]])
    v[is.na(v) | !nzchar(v)] <- "0"
    v
  }

  data.table::set(st, j = "TMP_arr", value = arr)
  data.table::set(st, j = "TMP_dep", value = dep)
  pair_cols <- c("stop_id", "TMP_arr", "TMP_dep")
  for (nm in c("pickup_type", "drop_off_type")) {
    v <- boarding(nm)
    if (!is.null(v)) {
      data.table::set(st, j = paste0("TMP_", nm), value = v)
      pair_cols <- c(pair_cols, paste0("TMP_", nm))
    }
  }
  # Number each distinct (stop, times) pair and compare the numbers rather
  # than the text: identity is all that matters here, and interning a string
  # of ATCO code and timestamps for every one of tens of millions of calls
  # costs far more memory than the answer is worth.
  st[, TMP_pid := .GRP, by = pair_cols]
  st[, (setdiff(pair_cols, "stop_id")) := NULL]
  st[, stop_id := NULL]
  rm(arr, dep)

  data.table::setorderv(st, c("trip_id", "TMP_seq"))
  sig <- st[, list(sig = paste(TMP_pid, collapse = ","),
                   n_stops = .N,
                   n_timed = sum(TMP_timed)), by = "trip_id"]
  rm(st)

  # A one stop trip, or a trip with no times to compare, cannot be matched
  # with any confidence
  sig <- sig[n_stops >= 2L & n_timed >= 2L]
  if (nrow(sig) < 2L) {
    if (!quiet) message("gtfs_deduplicate: removed 0 duplicate trips")
    return(gtfs)
  }
  sig[, sig_id := match(sig, unique(sig))]

  # ---- what else has to match before two trips are the same journey ------
  trips <- as.data.frame(gtfs$trips)
  trips$trip_id <- trip_id_all
  cand <- data.table::data.table(
    trip_id = trips$trip_id,
    service_id = as.character(trips$service_id),
    route_id = as.character(trips$route_id))

  data.table::set(cand, j = "TMP_rkey", value = switch(
    match_route,
    "route_id" = cand$route_id,
    "none" = rep("", nrow(cand)),
    "short_name" = {
      routes <- as.data.frame(gtfs$routes)
      pos <- match(cand$route_id, as.character(routes$route_id))
      pick <- function(nm) {
        if (nm %in% names(routes)) as.character(routes[[nm]])[pos] else rep("", nrow(cand))
      }
      short <- pick("route_short_name")
      # an unnamed route cannot be grouped by name, so it stands alone
      short[is.na(short) | !nzchar(short)] <-
        paste0("\rroute_id\r", cand$route_id[is.na(short) | !nzchar(short)])
      paste(operator_key(gtfs, pick("agency_id"), match_operator),
            pick("route_type"), short, sep = "\r")
    }))

  attr_wanted <- c("direction_id", "block_id", "wheelchair_accessible",
                   "bikes_allowed")
  if (!isTRUE(match_block)) attr_wanted <- setdiff(attr_wanted, "block_id")
  attr_cols <- intersect(attr_wanted, names(trips))
  data.table::set(cand, j = "TMP_tkey", value = if (length(attr_cols) > 0) {
    do.call(paste, c(lapply(attr_cols, function(nm) {
      v <- as.character(trips[[nm]])
      v[is.na(v)] <- ""
      v
    }), list(sep = "\r")))
  } else {
    ""
  })

  cand <- merge(cand, sig[, list(trip_id, sig_id)], by = "trip_id")
  rm(sig)

  # frequency based trips describe a pattern, not a journey, so two identical
  # ones are not two of the same bus
  if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0) {
    cand <- cand[!trip_id %in% as.character(gtfs$frequencies$trip_id)]
  }
  if (nrow(cand) < 2L) {
    if (!quiet) message("gtfs_deduplicate: removed 0 duplicate trips")
    return(gtfs)
  }

  grp_key <- paste(cand$TMP_rkey, cand$TMP_tkey, cand$sig_id, sep = "\r")
  cand[, grp := match(grp_key, unique(grp_key))]
  cand[, n_grp := .N, by = "grp"]
  cand <- cand[n_grp > 1L]
  if (nrow(cand) == 0L) {
    if (!quiet) message("gtfs_deduplicate: removed 0 duplicate trips")
    return(gtfs)
  }

  # ---- do the copies run on the same days? -------------------------------
  sdates <- service_operating_dates(gtfs, unique(cand$service_id))
  n_dates <- sdates[, list(n_dates = .N), by = "service_id"]

  # a service with no operating dates at all is unknown rather than duplicated
  cand <- merge(cand, n_dates, by = "service_id")
  cand[, n_grp := .N, by = "grp"]
  cand <- cand[n_grp > 1L]
  if (nrow(cand) == 0L) {
    if (!quiet) message("gtfs_deduplicate: removed 0 duplicate trips")
    return(gtfs)
  }

  # Rank the copies of a journey, the most used calendar first. A copy can be
  # removed only if every date it runs is also run by a lower ranked copy;
  # rank 1 is therefore never removed, and any date it does not cover keeps
  # whichever copy first supplied it.
  data.table::setorderv(cand, c("grp", "n_dates", "trip_id"), c(1L, -1L, 1L))
  cand[, rnk := seq_len(.N), by = "grp"]

  # The join below multiplies the candidates by their operating days, so carry
  # integer keys through it rather than service and trip ids
  svc <- unique(cand$service_id)
  cand[, TMP_svc := match(service_id, svc)]
  runs <- merge(cand[, list(TMP_svc, TMP_trip = .I, grp, rnk)],
                sdates[, list(TMP_svc = match(service_id, svc), date)],
                by = "TMP_svc", allow.cartesian = TRUE)
  rm(sdates)
  runs[, min_rnk := min(rnk), by = c("grp", "date")]
  covered <- runs[, list(redundant = all(min_rnk < rnk)), by = "TMP_trip"]
  drop_ids <- cand$trip_id[covered$TMP_trip[covered$redundant]]

  if (!quiet) {
    message("gtfs_deduplicate: removed ", length(drop_ids),
            " duplicate trips of ", length(trip_id_all),
            " (", round(100 * length(drop_ids) / length(trip_id_all), 2), "%)")
  }
  if (length(drop_ids) == 0L) {
    return(gtfs)
  }

  # ---- remove them, and nothing else -------------------------------------
  gtfs$trips <- gtfs$trips[!as.character(gtfs$trips$trip_id) %in% drop_ids, ]
  gtfs$stop_times <-
    gtfs$stop_times[!as.character(gtfs$stop_times$trip_id) %in% drop_ids, ]
  if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0) {
    gtfs$frequencies <-
      gtfs$frequencies[!as.character(gtfs$frequencies$trip_id) %in% drop_ids, ]
  }

  return(gtfs)
}
