#' Remove copies of one service published in two files
#'
#' @description [gtfs_deduplicate()] recognises a journey only when two trips
#'   carry it with identical times at every stop, under one operator name.
#'   That misses a whole class of duplicates in merged TransXChange feeds: one
#'   timetable written out twice by two publishers. Each publisher
#'   interpolates the times at the stops between timing points its own way,
#'   and files the operator under its own name, so no two copies agree call by
#'   call even though every journey leaves the same first stop at the same
#'   minute.
#'
#'   This function compares *services* instead of trips. Where two services of
#'   the same line and mode, from different source files, start most of their
#'   journeys at the same stop at the same time, one is a copy of the other:
#'   the lower ranked one loses its journeys on every date the higher ranked
#'   one also runs them.
#'
#' @param gtfs a gtfs object
#' @param min_share numeric between 0 and 1, the share of one service's
#'   distinct journey starts (first stop and departure time) that must also be
#'   starts of the other before the two are called copies. Default 0.8.
#' @param min_shared integer, the fewest shared starts that can make two
#'   services copies. Default 2, so that one coincident journey never
#'   condemns a service.
#' @param match `"start"` (the default) matches journeys on their first stop
#'   and departure time; `"ends"` also requires the same last stop and arrival
#'   time, which is stricter and misses a copy that was re-routed or re-timed
#'   towards its end.
#' @param keep character, the order in which the criteria decide which copy
#'   is kept, best first. `"newer"` prefers the service whose calendar starts
#'   later - the more recently published version - and only discriminates on
#'   a feed whose calendars have not been trimmed to a common window;
#'   `"longer"` prefers the service whose journeys call at more stops (finer
#'   detail, or a whole route rather than one region's share of it);
#'   `"more_trips"` prefers the service with more journeys. Remaining ties go
#'   to the lower `route_id`.
#' @param scope `"trips"` (the default) removes only the lower ranked copy's
#'   journeys that match a journey of the kept copy, on the dates both run.
#'   `"route"` also removes its unmatched journeys on dates the kept copy
#'   runs, treating it as a superseded version of the whole timetable. Even
#'   then a service is only removed wholesale if its own share of matched
#'   starts reaches `min_share`; a larger service that merely contains a
#'   smaller one loses only the matched journeys.
#' @param fixed_track integer vector of `route_type`s left out. Duplicates on
#'   rail, metro and tram are already handled by [gtfs_deduplicate()]'s
#'   termini test. Default `c(0, 1, 2)`.
#' @param quiet logical, suppress the summary message
#' @return the gtfs object with the copies' journeys removed or their dates
#'   trimmed. A data.frame describing every pair of copies found, and what was
#'   done about it, is attached as attribute `"copies_report"`.
#'
#' @details
#' **Which routes are one service.** A route is identified with its source
#' service: the `[ServiceCode: ...]` that [transxchange2gtfs()] writes into
#' `route_desc`, together with `agency_id`. Routes of one service are variants
#' of one file and are never compared with each other - that is
#' [gtfs_deduplicate()]'s job. A feed without ServiceCodes falls back to
#' `route_id`.
#'
#' **When two services are copies.** They must share `route_short_name` and
#' `route_type`, and at least `min_share` of the distinct journey starts of one
#' of them (and at least `min_shared` starts) must be starts of the other. The
#' operator is deliberately not compared: in the cases this exists for, the
#' two copies name the operator differently ("Arriva" against "Arriva
#' Midlands", both NOC AMID) or not at all the same operator (Go-Ahead's
#' superseded export of London route 439 against Transport for London's
#' current file naming Metrobus).
#'
#' A line number alone could not make two services copies: two towns' route 1
#' do not leave the same stop. Two operators running a joint timetable do not
#' either, because they share the departures out rather than repeat them.
#'
#' **What is removed.** Dates are respected throughout. A journey of the copy
#' is removed on the dates the kept service runs a journey with the same start
#' (scope `"trips"`) or runs at all (scope `"route"`). If that is every date
#' the journey runs, the trip is deleted; if it is some of them, the trip is
#' moved to a new service that no longer runs on those dates; if none, it is
#' left alone. A superseded open-ended file therefore loses its journeys only
#' from the date its successor begins.
#'
#' Groups of three or more copies are ranked together, and each loses its
#' journeys to every higher ranked service it is a copy of.
#'
#' @examples
#' \dontrun{
#' gtfs <- gtfs_merge(list(gtfs_a, gtfs_b), force = TRUE)
#' gtfs <- gtfs_deduplicate_copies(gtfs)
#' attr(gtfs, "copies_report")
#' }
#' @export
gtfs_deduplicate_copies <- function(gtfs,
                                    min_share = 0.8,
                                    min_shared = 2L,
                                    match = c("start", "ends"),
                                    keep = c("newer", "longer", "more_trips"),
                                    scope = c("trips", "route"),
                                    fixed_track = c(0L, 1L, 2L),
                                    quiet = FALSE) {
  match <- match.arg(match)
  scope <- match.arg(scope)
  keep <- match.arg(keep, c("newer", "longer", "more_trips"), several.ok = TRUE)
  checkmate::assert_number(min_share, lower = 0, upper = 1)
  fixed_track <- suppressWarnings(as.integer(fixed_track))

  empty <- function(g) {
    attr(g, "copies_report") <- copies_report_empty()
    g <- conversion_report_step(g, "gtfs_deduplicate_copies",
                                "pairs of copies", 0)
    if (!quiet) message("gtfs_deduplicate_copies: no copies found")
    g
  }
  if (is.null(gtfs$routes) || nrow(gtfs$routes) == 0 ||
      is.null(gtfs$trips) || nrow(gtfs$trips) == 0 ||
      is.null(gtfs$stop_times) || nrow(gtfs$stop_times) == 0) {
    return(empty(gtfs))
  }

  # ---- services and lines ------------------------------------------------
  rt <- as.data.frame(gtfs$routes)
  col <- function(d, nm) {
    if (nm %in% names(d)) as.character(d[[nm]]) else rep(NA_character_, nrow(d))
  }
  desc <- col(rt, "route_desc")
  has_code <- !is.na(desc) & grepl("\\[ServiceCode: [^]]*\\]", desc)
  svc_code <- rep(NA_character_, nrow(rt))
  svc_code[has_code] <- sub("^.*\\[ServiceCode: ([^]]*)\\].*$", "\\1",
                            desc[has_code])
  routes <- data.table::data.table(
    route_id = as.character(rt$route_id),
    agency_id = col(rt, "agency_id"),
    short = col(rt, "route_short_name"),
    type = suppressWarnings(as.integer(col(rt, "route_type"))),
    code = svc_code)
  routes[, svc := ifelse(is.na(code), paste0("\rroute\r", route_id),
                         paste(agency_id, code, sep = "\r"))]
  routes <- routes[!is.na(short) & nzchar(short) & !is.na(type) &
                     !type %in% fixed_track]
  routes[, line := paste(short, type, sep = "\r")]
  routes[, n_svc := data.table::uniqueN(svc), by = "line"]
  routes <- routes[n_svc > 1L]
  if (nrow(routes) == 0) return(empty(gtfs))

  trips <- data.table::data.table(
    trip_id = as.character(gtfs$trips$trip_id),
    route_id = as.character(gtfs$trips$route_id),
    service_id = as.character(gtfs$trips$service_id))
  if (anyDuplicated(trips$trip_id)) {
    warning("trips$trip_id is not unique, cannot remove copies safely")
    return(empty(gtfs))
  }
  if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0) {
    trips <- trips[!trip_id %in% as.character(gtfs$frequencies$trip_id)]
  }
  trips <- merge(trips, routes[, list(route_id, svc, line)], by = "route_id")
  if (nrow(trips) == 0) return(empty(gtfs))

  # ---- journey starts (and ends) ----------------------------------------
  # Times are converted on the whole column before any subsetting: stop_times
  # time columns can be lubridate Periods, which a row subset corrupts.
  st_trip <- as.character(gtfs$stop_times$trip_id)
  idx <- which(st_trip %in% trips$trip_id)
  arr <- gtfs_time_secs_strict(gtfs$stop_times$arrival_time)[idx]
  dep <- gtfs_time_secs_strict(gtfs$stop_times$departure_time)[idx]
  seq <- if ("stop_sequence" %in% names(gtfs$stop_times)) {
    suppressWarnings(as.numeric(gtfs$stop_times$stop_sequence))[idx]
  } else {
    as.numeric(idx)
  }
  st <- data.table::data.table(trip_id = st_trip[idx],
                               stop_id = as.character(gtfs$stop_times$stop_id)[idx],
                               seq = seq, arr = arr,
                               dep = data.table::fifelse(is.na(dep), arr, dep))
  st[is.na(arr), arr := dep]
  rm(arr, dep, seq, idx)
  data.table::setorderv(st, c("trip_id", "seq"))
  jt <- st[, list(first_stop = stop_id[1L], first_dep = dep[1L],
                  last_stop = stop_id[.N], last_arr = arr[.N],
                  n_stops = .N), by = "trip_id"]
  rm(st)
  jt <- jt[n_stops >= 2L & !is.na(first_dep)]
  jt[, key := if (match == "start") {
    paste(first_stop, first_dep, sep = "\r")
  } else {
    paste(first_stop, first_dep, last_stop, last_arr, sep = "\r")
  }]
  trips <- merge(trips, jt[, list(trip_id, key, n_stops)], by = "trip_id")
  if (nrow(trips) == 0) return(empty(gtfs))

  # ---- pairs of services sharing starts ----------------------------------
  sk <- unique(trips[, list(line, svc, key)])
  sk[, n_keys := .N, by = "svc"]
  pr <- merge(sk, sk, by = c("line", "key"), suffixes = c("_a", "_b"),
              allow.cartesian = TRUE)
  pr <- pr[svc_a < svc_b]
  if (nrow(pr) == 0) return(empty(gtfs))
  pr <- pr[, list(shared = .N, n_keys_a = n_keys_a[1L], n_keys_b = n_keys_b[1L]),
           by = c("line", "svc_a", "svc_b")]
  pr[, share_a := shared / n_keys_a]
  pr[, share_b := shared / n_keys_b]
  pr <- pr[shared >= min_shared & pmax(share_a, share_b) >= min_share]
  if (nrow(pr) == 0) return(empty(gtfs))

  # ---- rank the services -------------------------------------------------
  involved <- unique(c(pr$svc_a, pr$svc_b))
  sv <- trips[svc %in% involved,
              list(n_trips = .N, stops = as.numeric(stats::median(n_stops)),
                   services = list(unique(service_id)),
                   route_ids = paste(sort(unique(route_id)), collapse = " "),
                   first_route = min(route_id)),
              by = "svc"]
  all_svc_ids <- unique(unlist(sv$services))
  sdates <- service_operating_dates(gtfs, all_svc_ids)
  first_date <- sdates[, list(start = min(date)), by = "service_id"]
  sv[, start := vapply(services, function(s) {
    v <- first_date$start[first_date$service_id %in% s]
    if (length(v)) as.numeric(min(v)) else NA_real_
  }, numeric(1))]
  # higher is better on every criterion
  crit <- list(newer = sv$start, longer = sv$stops, more_trips = sv$n_trips)
  ord_args <- c(lapply(keep, function(k) {
    v <- crit[[k]]
    v[is.na(v)] <- -Inf
    -v
  }), list(sv$first_route))
  sv[, rank := order(do.call(order, ord_args))]
  rk <- stats::setNames(sv$rank, sv$svc)

  # In each pair: K is kept, D loses journeys, and why
  pr[, keep_a := rk[svc_a] < rk[svc_b]]
  pr[, `:=`(svc_k = ifelse(keep_a, svc_a, svc_b),
            svc_d = ifelse(keep_a, svc_b, svc_a),
            share_d = ifelse(keep_a, share_b, share_a),
            share_k = ifelse(keep_a, share_a, share_b))]
  pr[, decided_by := vapply(seq_len(.N), function(i) {
    k <- match(svc_k[i], sv$svc); d <- match(svc_d[i], sv$svc)
    for (cr in keep) {
      a <- crit[[cr]][k]; b <- crit[[cr]][d]
      if (!isTRUE(all.equal(a, b)) && !(is.na(a) && is.na(b))) return(cr)
    }
    "route_id"
  }, character(1))]

  # ---- which journeys of D, on which dates -------------------------------
  tr <- trips[svc %in% involved]
  tr[, TMP_svc := match(service_id, all_svc_ids)]
  sdates[, TMP_svc := match(service_id, all_svc_ids)]
  runs <- merge(tr[, list(trip_id, svc, key, TMP_svc)],
                sdates[, list(TMP_svc, date)], by = "TMP_svc",
                allow.cartesian = TRUE)

  hits <- list()
  for (i in seq_len(nrow(pr))) {
    k_runs <- runs[svc == pr$svc_k[i]]
    d_runs <- runs[svc == pr$svc_d[i]]
    whole <- scope == "route" && pr$share_d[i] >= min_share
    cover <- if (whole) {
      unique(k_runs[, list(date)])[, hit := TRUE]
    } else {
      unique(k_runs[, list(key, date)])[, hit := TRUE]
    }
    on <- if (whole) "date" else c("key", "date")
    h <- cover[d_runs, on = on, nomatch = NULL]
    if (nrow(h)) hits[[length(hits) + 1L]] <- h[, list(trip_id, date, pair = i)]
  }
  if (!length(hits)) return(empty(gtfs))
  hits <- unique(data.table::rbindlist(hits), by = c("trip_id", "date"))

  n_run <- runs[trip_id %in% hits$trip_id, list(n_dates = .N), by = "trip_id"]
  n_hit <- hits[, list(n_hit = .N, pair = min(pair)), by = "trip_id"]
  fate <- merge(n_run, n_hit, by = "trip_id")
  drop_ids <- fate$trip_id[fate$n_hit >= fate$n_dates]
  trim <- fate[n_hit < n_dates]

  # ---- report ------------------------------------------------------------
  ag <- as.data.frame(gtfs$agency)
  ag_name <- stats::setNames(col(ag, "agency_name"), col(ag, "agency_id"))
  part <- function(s, what) {
    r <- routes[match(s, routes$svc)]
    switch(what,
           agency = r$agency_id,
           name = unname(ag_name[r$agency_id]),
           code = r$code)
  }
  cnt <- fate[, list(trips_removed = sum(n_hit >= n_dates),
                     trips_trimmed = sum(n_hit < n_dates),
                     trip_days_removed = sum(n_hit)), by = "pair"]
  pr[, pair := .I]
  pr <- merge(pr, cnt, by = "pair", all.x = TRUE)
  for (v in c("trips_removed", "trips_trimmed", "trip_days_removed")) {
    data.table::set(pr, which(is.na(pr[[v]])), v, 0L)
  }
  day <- function(x) as.Date(x, origin = "1970-01-01")
  report <- data.frame(
    line = sub("\r.*$", "", pr$line),
    route_type = as.integer(sub("^.*\r", "", pr$line)),
    kept_routes = sv$route_ids[match(pr$svc_k, sv$svc)],
    kept_agency = part(pr$svc_k, "agency"),
    kept_name = part(pr$svc_k, "name"),
    kept_code = part(pr$svc_k, "code"),
    kept_start = day(sv$start[match(pr$svc_k, sv$svc)]),
    kept_trips = sv$n_trips[match(pr$svc_k, sv$svc)],
    dropped_routes = sv$route_ids[match(pr$svc_d, sv$svc)],
    dropped_agency = part(pr$svc_d, "agency"),
    dropped_name = part(pr$svc_d, "name"),
    dropped_code = part(pr$svc_d, "code"),
    dropped_start = day(sv$start[match(pr$svc_d, sv$svc)]),
    dropped_trips = sv$n_trips[match(pr$svc_d, sv$svc)],
    shared_starts = pr$shared,
    share_dropped = round(pr$share_d, 3),
    share_kept = round(pr$share_k, 3),
    decided_by = pr$decided_by,
    scope = ifelse(scope == "route" & pr$share_d >= min_share, "route", "trips"),
    trips_removed = pr$trips_removed,
    trips_trimmed = pr$trips_trimmed,
    trip_days_removed = pr$trip_days_removed,
    stringsAsFactors = FALSE)
  report <- report[order(-report$trip_days_removed), ]
  rownames(report) <- NULL

  # ---- apply -------------------------------------------------------------
  if (nrow(trim) > 0) {
    gtfs <- copies_trim_services(gtfs, trim$trip_id, hits, trips)
  }
  if (length(drop_ids) > 0) {
    gtfs$trips <- gtfs$trips[!as.character(gtfs$trips$trip_id) %in% drop_ids, ]
    gtfs$stop_times <-
      gtfs$stop_times[!as.character(gtfs$stop_times$trip_id) %in% drop_ids, ]
    if (!is.null(gtfs$frequencies) && nrow(gtfs$frequencies) > 0) {
      gtfs$frequencies <-
        gtfs$frequencies[!as.character(gtfs$frequencies$trip_id) %in% drop_ids, ]
    }
  }
  if (!quiet) {
    message("gtfs_deduplicate_copies: ", nrow(report), " pairs of copies; ",
            length(drop_ids), " trips removed, ", nrow(trim),
            " trips moved to calendars without the duplicated dates")
  }
  attr(gtfs, "copies_report") <- report
  gtfs <- conversion_report_step(
    gtfs, "gtfs_deduplicate_copies",
    c("pairs of copies", "trips removed", "trips moved to trimmed calendars"),
    c(nrow(report), length(drop_ids), nrow(trim)))
  r <- attr(gtfs, "conversion_report")
  if (!is.null(r)) {
    r$copies <- report
    attr(gtfs, "conversion_report") <- r
  }
  gtfs
}


#' Move trips onto copies of their services that skip the given dates
#'
#' @param gtfs a gtfs object
#' @param trip_ids trips to move
#' @param hits data.table of trip_id and date (integer days) to remove
#' @param trips data.table with trip_id and service_id
#' @noRd
copies_trim_services <- function(gtfs, trip_ids, hits, trips) {
  h <- hits[trip_id %in% trip_ids]
  h <- merge(h, trips[, list(trip_id, service_id)], by = "trip_id")
  # one new service per original service and set of dates removed
  sig <- h[order(date), list(dates = paste(date, collapse = ","),
                             service_id = service_id[1L]), by = "trip_id"]
  sig[, new_id := paste0(service_id, "_xcopy",
                         match(paste(service_id, dates),
                               unique(paste(service_id, dates))))]
  new <- unique(sig[, list(service_id, new_id, dates)])

  date_like <- function(template, d) {
    d <- as.Date(d, origin = "1970-01-01")
    if (inherits(template, "Date")) return(d)
    if (is.numeric(template)) return(as.integer(format(d, "%Y%m%d")))
    if (is.character(template)) {
      nonmiss <- template[!is.na(template) & nzchar(template)]
      if (length(nonmiss) && all(grepl("^[0-9]{4}-", nonmiss))) {
        return(format(d, "%Y-%m-%d"))
      }
      return(format(d, "%Y%m%d"))
    }
    d
  }

  cal <- gtfs$calendar
  cd <- gtfs$calendar_dates
  cal_dt <- data.table::is.data.table(cal)
  cd_dt <- data.table::is.data.table(cd)
  if (!is.null(cal)) cal <- as.data.frame(cal)
  if (is.null(cd) || ncol(cd) == 0) {
    cd <- data.frame(service_id = character(), date = as.Date(character()),
                     exception_type = integer(), stringsAsFactors = FALSE)
  }
  cd <- as.data.frame(cd)
  cd_sid <- as.character(cd$service_id)
  cd_int <- as.integer(gtfs_as_date(cd$date))
  cd_ex <- suppressWarnings(as.integer(as.character(cd$exception_type)))
  ex2 <- if (is.character(cd$exception_type)) "2" else 2L
  new_cal <- list()
  new_cd <- list()
  for (i in seq_len(nrow(new))) {
    old <- new$service_id[i]
    rm_dates <- as.integer(strsplit(new$dates[i], ",", fixed = TRUE)[[1]])
    if (!is.null(cal) && nrow(cal) > 0) {
      cr <- cal[as.character(cal$service_id) == old, , drop = FALSE]
      if (nrow(cr) > 0) {
        cr$service_id <- new$new_id[i]
        new_cal[[length(new_cal) + 1L]] <- cr
      }
    }
    mine <- which(cd_sid == old)
    added <- cd_int[mine][cd_ex[mine] %in% 1L]
    # additions on the removed dates are simply not copied; dates the
    # calendar itself supplies need an explicit removal
    oc <- cd[mine[!cd_int[mine] %in% rm_dates], , drop = FALSE]
    by_cal <- setdiff(rm_dates, added)
    if (length(by_cal)) {
      extra <- cd[rep(NA_integer_, length(by_cal)), , drop = FALSE]
      extra$date <- date_like(cd$date, by_cal)
      extra$exception_type <- rep(ex2, length(by_cal))
      oc <- rbind(oc, extra)
    }
    oc$service_id <- rep(new$new_id[i], nrow(oc))
    new_cd[[length(new_cd) + 1L]] <- oc
  }
  if (length(new_cal)) cal <- rbind(cal, do.call(rbind, new_cal))
  cd <- rbind(cd, do.call(rbind, new_cd))
  rownames(cd) <- NULL
  gtfs$calendar <- if (cal_dt) data.table::as.data.table(cal) else cal
  gtfs$calendar_dates <- if (cd_dt) data.table::as.data.table(cd) else cd

  pos <- match(as.character(gtfs$trips$trip_id), sig$trip_id)
  moved <- !is.na(pos)
  sid <- as.character(gtfs$trips$service_id)
  sid[moved] <- sig$new_id[pos[moved]]
  gtfs$trips$service_id <- sid
  gtfs
}


copies_report_empty <- function() {
  data.frame(line = character(), route_type = integer(),
             kept_routes = character(), kept_agency = character(),
             kept_name = character(), kept_code = character(),
             kept_start = as.Date(character()), kept_trips = integer(),
             dropped_routes = character(), dropped_agency = character(),
             dropped_name = character(), dropped_code = character(),
             dropped_start = as.Date(character()), dropped_trips = integer(),
             shared_starts = integer(), share_dropped = numeric(),
             share_kept = numeric(), decided_by = character(),
             scope = character(), trips_removed = integer(),
             trips_trimmed = integer(), trip_days_removed = integer(),
             stringsAsFactors = FALSE)
}

utils::globalVariables(c(
  "short", "svc", "line", "n_svc", "first_stop", "first_dep", "last_stop",
  "last_arr", "n_stops", "key", "n_keys", "svc_a", "svc_b", "n_keys_a",
  "n_keys_b", "shared", "share_a", "share_b", "services", "keep_a", "svc_k",
  "svc_d", "share_d", "share_k", "decided_by", "TMP_svc", "hit", "pair",
  "n_hit", "n_dates", "dates", "new_id", "seq", "dep", "arr", "code", "type",
  "route_ids", "first_route", "service_id", "rank"))
