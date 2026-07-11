# Convert National Rail fares (RSPS5045) to GTFS fare tables.
#
# Rail fares are set per "flow" (an origin/destination pair, possibly a
# station cluster or group station), per fare route, per ticket type. This
# maps naturally onto zone-based GTFS fares:
#   v1: one zone per station (CRS code), fare_rules per origin/destination
#   v2: one area per flow endpoint (station, cluster or group station),
#       fare_leg_rules per flow, fare_products per ticket type, with
#       rider_categories for child and railcard fares.


#' Select ticket types for conversion
#'
#' @param fares fares list from [atoc_fares_read()]
#' @param ticket_codes explicit ticket codes or NULL
#' @param ticket_class "standard"/"first"
#' @param ticket_type "single"/"return"/"season"
#' @param walkup_only keep only Anytime/Off-Peak/Super Off-Peak tickets
#' @return data.table of selected ticket type records
#' @noRd
railfares_tickets <- function(fares, ticket_codes, ticket_class, ticket_type,
                              walkup_only = TRUE) {
  tt <- fares$ticket_type
  if (!is.null(ticket_codes)) {
    sel <- tt[ticket_code %in% ticket_codes]
    missing <- setdiff(ticket_codes, sel$ticket_code)
    if (length(missing) > 0) {
      warning("Ticket code(s) not found in fares feed: ",
              paste(missing, collapse = ", "))
    }
    return(sel)
  }

  class_map <- c(standard = "2", first = "1")
  type_map <- c(single = "S", return = "R", season = "N")
  want_class <- class_map[match.arg(tolower(ticket_class), names(class_map),
                                    several.ok = TRUE)]
  want_type <- type_map[match.arg(tolower(ticket_type), names(type_map),
                                  several.ok = TRUE)]

  sel <- tt[ticket_class %in% want_class &
              ticket_type %in% want_type &
              ticket_group %in% c("S", "F") &  # exclude promotions and Euro tickets
              package_mkr == "N" &             # exclude packages
              max_passengers == 1L]            # exclude group/family tickets
  if (walkup_only) {
    # Walk-up (turn-up-and-go) fares are the Anytime / Off-Peak /
    # Super Off-Peak families. Everything else in the flow file is trade,
    # advance-purchase tier, carnet or otherwise restricted ticketing whose
    # prices are misleading in a GTFS feed (e.g. GBP 0.05 ITX tickets).
    sel <- sel[grepl("ANYTIME|OFF[ -]?PEAK|OFFPK", description,
                     ignore.case = TRUE)]
  }
  sel
}


#' Resolve flow endpoint codes to stations
#'
#' A flow endpoint is a 4-character NLC code which may be a station, a
#' station cluster (expanded via the FSC file) or a group station such as
#' 1072 "LONDON TERMINALS" (expanded via the LOC 'M' records). Cluster
#' members may themselves be group stations. Returns the member CRS codes of
#' every endpoint, plus a human-readable name.
#'
#' @param fares fares list from [atoc_fares_read()]
#' @param codes character vector of endpoint codes appearing in the flows
#' @return list with `members` (data.table code -> crs) and `names`
#'   (data.table code -> endpoint_name)
#' @noRd
railfares_endpoints <- function(fares, codes) {
  codes <- unique(codes)

  # 1. cluster expansion: a code is either a cluster (-> member NLCs) or
  #    already an NLC
  cl <- fares$cluster[cluster_id %in% codes]
  ep <- data.table::rbindlist(list(
    data.table::data.table(code = cl$cluster_id, nlc = cl$cluster_nlc),
    data.table::data.table(code = setdiff(codes, cl$cluster_id),
                           nlc = setdiff(codes, cl$cluster_id))
  ))

  # 2. NLC -> CRS: group stations expand to their members, ordinary
  #    stations map via the location file
  grp <- fares$group_member[, c("group_nlc", "member_crs")]
  data.table::setnames(grp, c("nlc", "crs"))
  stn <- fares$location[crs != "" & !nlc %in% grp$nlc, c("nlc", "crs")]
  nlc2crs <- unique(data.table::rbindlist(list(grp, stn)))

  members <- merge(ep, nlc2crs, by = "nlc", allow.cartesian = TRUE)
  members <- unique(members[, c("code", "crs")])

  # endpoint names for GTFS areas
  loc_names <- stats::setNames(fares$location$description, fares$location$nlc)
  grp_names <- stats::setNames(fares$group$description, fares$group$group_nlc)
  nm <- ifelse(!is.na(grp_names[codes]), grp_names[codes], loc_names[codes])
  is_cluster <- codes %in% fares$cluster$cluster_id
  nm[is_cluster] <- paste("Station cluster", codes[is_cluster])
  nm[is.na(nm)] <- codes[is.na(nm)]
  names <- data.table::data.table(code = codes, endpoint_name = unname(nm))

  list(members = members, names = names)
}


#' Build the adult fares table from flows and non-derivable overrides
#'
#' Joins the fare records to their flows, restricts to the selected tickets,
#' and overlays the non-derivable fare overrides (which take precedence over
#' the flow file for the same origin/destination/route/ticket).
#'
#' @param fares fares list from [atoc_fares_read()]
#' @param tickets data.table from railfares_tickets()
#' @param ndf logical, include non-derivable fare overrides
#' @return data.table with columns origin, destination, route_code,
#'   ticket_code, direction, fare, child_fare (NA except for NDF rows)
#' @noRd
railfares_base <- function(fares, tickets, ndf = TRUE) {
  flow <- fares$flow[status_code == "000"]
  ff <- merge(
    fares$fare[ticket_code %in% tickets$ticket_code],
    flow[, c("flow_id", "origin", "destination", "route_code", "direction")],
    by = "flow_id"
  )
  ff <- ff[, c("origin", "destination", "route_code", "ticket_code",
               "direction", "fare", "restriction_code")]
  ff[, child_fare := NA_integer_]

  if (ndf && nrow(fares$ndf) > 0) {
    nd <- fares$ndf[railcard_code == "" & ticket_code %in% tickets$ticket_code]
    if (nrow(nd) > 0) {
      nd <- nd[, c("origin", "destination", "route_code", "ticket_code",
                   "adult_fare", "child_fare", "restriction_code")]
      data.table::setnames(nd, "adult_fare", "fare")
      # non-derivable fares are directional and override flow fares
      nd[, direction := "S"]
      key <- c("origin", "destination", "route_code", "ticket_code")
      ff <- ff[!nd, on = key]
      ff <- data.table::rbindlist(list(ff, nd), use.names = TRUE)
    }
  }
  # keep child-only non-derivable fares (adult 99999999 = not available)
  ff[!is.na(fare) | !is.na(child_fare)]
}


#' Coerce a GTFS calendar date column to Date
#'
#' Calendar dates can be Date, IDate, integer yyyymmdd or character
#' depending on how the GTFS was built/read.
#'
#' @param x vector of dates in any of the above forms
#' @noRd
railfares_as_date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (is.numeric(x)) {
    if (all(x > 10000000, na.rm = TRUE)) {
      return(as.Date(as.character(x), format = "%Y%m%d"))
    }
    return(as.Date(x, origin = "1970-01-01"))
  }
  x <- as.character(x)
  out <- as.Date(x, format = "%Y%m%d")
  bad <- is.na(out) & !is.na(x)
  out[bad] <- as.Date(x[bad])
  out
}


#' Validate and normalise the scenario arguments
#'
#' Shared by [gtfs_add_railfares()] and [atoc2gtfs()] (which calls it before
#' the timetable build, so impossible scenarios fail fast).
#'
#' @param travel_date,travel_time,booking_date as documented in
#'   [gtfs_add_railfares()]
#' @return list with `travel_date` (Date or NULL), `travel_time` (integer
#'   minutes or NULL) and `booking_date` (Date or NULL)
#' @noRd
railfares_check_scenario <- function(travel_date, travel_time, booking_date) {
  if (!is.null(travel_date)) travel_date <- as.Date(travel_date)
  if (!is.null(booking_date)) booking_date <- as.Date(booking_date)
  if (is.null(travel_date) && (!is.null(travel_time) || !is.null(booking_date))) {
    stop("travel_time and booking_date require a travel_date")
  }
  if (!is.null(travel_time)) {
    if (is.character(travel_time)) {
      hm <- suppressWarnings(
        as.integer(strsplit(travel_time, ":", fixed = TRUE)[[1]]))
      if (length(hm) != 2 || anyNA(hm)) {
        stop("travel_time must be \"HH:MM\" (e.g. \"08:00\") or minutes ",
             "after midnight")
      }
      travel_time <- hm[1] * 60L + hm[2]
    }
    checkmate::assert_integerish(travel_time, lower = 0, upper = 1439)
    travel_time <- as.integer(travel_time)
  }
  if (!is.null(booking_date)) {
    if (booking_date > travel_date) {
      stop("booking_date (", booking_date, ") is after travel_date (",
           travel_date, ")")
    }
    ahead <- as.numeric(travel_date - booking_date)
    if (ahead > 365) {
      stop("booking_date is ", round(ahead), " days before travel_date; ",
           "rail bookings open roughly 12 weeks ahead, so no fare in the ",
           "feed can describe this scenario")
    }
    if (ahead > 12 * 7) {
      warning("booking_date is ", round(ahead), " days before travel_date; ",
              "rail bookings usually open about 12 weeks ahead, so ",
              "Advance tickets for the travel date would probably not have ",
              "been on sale yet")
    }
  }
  list(travel_date = travel_date, travel_time = travel_time,
       booking_date = booking_date)
}


#' Does a MMDD date band + day-of-week marker match a date?
#'
#' @param date_from,date_to character MMDD band (from > to wraps year end)
#' @param days 7-character Y/N marker string, Monday first
#' @param travel_date Date
#' @return logical vector
#' @noRd
railfares_band_matches <- function(date_from, date_to, days, travel_date) {
  mmdd <- as.integer(format(travel_date, "%m%d"))
  from <- suppressWarnings(as.integer(date_from))
  to <- suppressWarnings(as.integer(date_to))
  in_band <- data.table::fifelse(
    is.na(from) | is.na(to), TRUE,
    data.table::fifelse(from <= to, mmdd >= from & mmdd <= to,
                        mmdd >= from | mmdd <= to))
  wday <- as.integer(format(travel_date, "%u"))  # Monday = 1
  in_band & substr(days, wday, wday) == "Y"
}


#' Time restrictions in force on a travel date
#'
#' Resolves the Current/Future marker from the RD records, then returns the
#' outward ('O') time restriction records whose date bands (TD, falling back
#' to the header HD bands, falling back to always) match the travel date.
#'
#' @param fares fares list from [atoc_fares_read()]
#' @param travel_date Date
#' @return data.table of active time restriction rows (restriction_code,
#'   time_from/time_to in minutes since midnight, arr_dep_via, location,
#'   min_fare_flag)
#' @noRd
railfares_active_restrictions <- function(fares, travel_date) {
  dateint <- as.integer(format(travel_date, "%Y%m%d"))
  rd <- fares$restriction_dates
  rd <- rd[!is.na(start_date) & !is.na(end_date) &
             start_date <= dateint & end_date >= dateint]
  marker <- if ("C" %in% rd$cf_mkr) "C" else rd$cf_mkr[1]
  if (is.na(marker) || length(marker) == 0) {
    warning("The travel date ", travel_date, " is outside the date range of ",
            "the restrictions data; time restrictions were not applied.")
    return(fares$time_restriction[0])
  }

  tr <- fares$time_restriction[cf_mkr == marker & out_ret == "O"]
  if (nrow(tr) == 0) return(tr)
  td <- fares$time_restriction_date_band[cf_mkr == marker & out_ret == "O"]
  hd <- fares$restriction_date_band[cf_mkr == marker]

  # a time restriction is active if any of its own date bands match; with no
  # bands of its own, the header date bands decide; with neither it always
  # applies
  td[, matches := railfares_band_matches(date_from, date_to, days, travel_date)]
  td_any <- td[, list(active = any(matches)),
               by = c("restriction_code", "sequence_no")]
  hd[, matches := railfares_band_matches(date_from, date_to, days, travel_date)]
  hd_any <- hd[, list(active_hd = any(matches)), by = "restriction_code"]

  tr <- merge(tr, td_any, by = c("restriction_code", "sequence_no"),
              all.x = TRUE)
  tr <- merge(tr, hd_any, by = "restriction_code", all.x = TRUE)
  tr[, active := data.table::fifelse(!is.na(active), active,
                                     data.table::fifelse(!is.na(active_hd),
                                                         active_hd, TRUE))]
  tr <- tr[active == TRUE]
  tr[, c("active", "active_hd") := NULL]
  tr[, minutes_from := suppressWarnings(
    as.integer(substr(time_from, 1, 2)) * 60L +
      as.integer(substr(time_from, 3, 4)))]
  tr[, minutes_to := suppressWarnings(
    as.integer(substr(time_to, 1, 2)) * 60L +
      as.integer(substr(time_to, 3, 4)))]
  tr[!is.na(minutes_from) & !is.na(minutes_to)]
}


#' Drop fares invalid for a travel date/time scenario
#'
#' Evaluates the RST time restrictions against the scenario: a fare is
#' dropped when its restriction code has an active outward time restriction
#' with `min_fare_flag = 'N'` (fare not valid) whose window covers the
#' departure time, and which applies network-wide (blank location) or to
#' departures from the fare's origin stations. Arrival-based, via-based and
#' train-specific restrictions cannot be checked against a departure time
#' and are treated as not applying, and windows with `min_fare_flag = 'Y'`
#' keep the fare (the minimum-fare rule is not modelled) - both make the
#' filter err on the side of keeping a fare.
#'
#' When no departure time is given only all-day prohibitions (windows
#' covering 00:00-23:59, e.g. Christmas bans) are applied.
#'
#' @param base data.table from railfares_base()
#' @param fares fares list
#' @param travel_date Date
#' @param travel_time minutes since midnight, or NULL
#' @param silent logical
#' @return `base` without the rows invalid in the scenario
#' @noRd
railfares_filter_restrictions <- function(base, fares, travel_date,
                                          travel_time = NULL, silent = TRUE) {
  act <- railfares_active_restrictions(fares, travel_date)
  act <- act[min_fare_flag == "N" & arr_dep_via == "D"]
  act <- act[restriction_code %in% base$restriction_code]
  if (nrow(act) == 0) return(base)

  if (is.null(travel_time)) {
    # date-only scenario: apply only all-day prohibitions
    act <- act[minutes_from == 0L & minutes_to >= 1439L]
    if (nrow(act) == 0) return(base)
    in_window <- function(from, to) rep(TRUE, length(from))
  } else {
    in_window <- function(from, to) {
      data.table::fifelse(from <= to,
                          travel_time >= from & travel_time <= to,
                          travel_time >= from | travel_time <= to)
    }
  }
  act <- act[in_window(minutes_from, minutes_to)]
  if (nrow(act) == 0) return(base)

  # origin CRS members, for restrictions on departures from a named station
  ep <- railfares_endpoints(fares, unique(base$origin))
  base[, row_id := .I]
  hits <- merge(base[, c("row_id", "origin", "restriction_code")],
                act[, c("restriction_code", "location")],
                by = "restriction_code", allow.cartesian = TRUE)
  loc_free <- hits[location == ""]
  loc_bound <- hits[location != ""]
  if (nrow(loc_bound) > 0) {
    om <- ep$members
    data.table::setnames(om, c("origin", "crs"))
    loc_bound <- merge(loc_bound, om,
                       by.x = c("origin", "location"),
                       by.y = c("origin", "crs"))
  }
  drop_rows <- unique(c(loc_free$row_id, loc_bound$row_id))

  if (!silent && length(drop_rows) > 0) {
    message(length(drop_rows), " of ", nrow(base),
            " fares are not valid at the requested travel date/time ",
            "and were dropped")
  }
  out <- base[!row_id %in% drop_rows]
  out[, row_id := NULL]
  base[, row_id := NULL]
  out
}


#' Advance tickets bookable for a given scenario
#'
#' Uses the TAP file to find the advance-purchase tickets whose booking
#' horizon is satisfied: booking on `booking_date` for travel on
#' `travel_date` (at `travel_time` where the horizon is expressed in hours).
#' Booking is assumed to happen by the end of `booking_date`.
#'
#' @param fares fares list from [atoc_fares_read()]
#' @param booking_date,travel_date Dates
#' @param travel_time minutes since midnight or NULL (midnight assumed)
#' @return character vector of ticket codes
#' @noRd
railfares_advance_tickets <- function(fares, booking_date, travel_date,
                                      travel_time = NULL) {
  adv <- fares$advance
  if (nrow(adv) == 0) return(character())
  travel_minutes <- if (is.null(travel_time)) 0L else travel_time
  # hours between the end of the booking day and departure
  hours_ahead <- as.numeric(difftime(travel_date, booking_date + 1,
                                     units = "hours")) + travel_minutes / 60
  days_ahead <- as.numeric(travel_date - booking_date)
  bookint <- as.integer(format(booking_date, "%Y%m%d"))

  ap_num <- suppressWarnings(as.numeric(adv$ap_data))
  ap_date <- fares_dateint(adv$ap_data)
  ok <- (adv$check_type == "0" & !is.na(ap_date) & bookint <= ap_date) |
    (adv$check_type == "1" & !is.na(ap_num) & hours_ahead >= ap_num) |
    (adv$check_type == "2" & !is.na(ap_num) & days_ahead >= ap_num)
  unique(adv$ticket_code[ok])
}


#' Apply a status discount to adult fares
#'
#' Implements the discount calculation of RSPS5045 section 4.17: find the
#' discount percentage for the status/discount-category pair and apply the
#' discount indicator rules (percentage, flat fare, maximum and minimum
#' caps). Rounding rules (FRR) are not applied, so prices can differ from
#' retail prices by a few pence.
#'
#' @param base data.table from railfares_base() (adult fares)
#' @param tickets data.table from railfares_tickets()
#' @param fares fares list
#' @param status 3-character status code (e.g. child status "001")
#' @return copy of `base` with `fare` discounted; rows where no discounted
#'   fare exists are dropped
#' @noRd
railfares_discount <- function(base, tickets, fares, status) {
  st <- fares$status[status_code == status]
  ds <- fares$status_discount[status_code == status]
  if (nrow(st) == 0 || nrow(ds) == 0) {
    warning("No status discount records for status '", status,
            "'; skipping")
    return(base[0])
  }

  x <- data.table::copy(base)
  x <- merge(x, tickets[, c("ticket_code", "ticket_class", "ticket_type",
                            "discount_category")], by = "ticket_code")
  x <- merge(x, ds[, c("discount_category", "discount_indicator",
                       "discount_percentage")],
             by = "discount_category", all.x = TRUE)

  # caps depend on ticket class and single/return
  first <- x$ticket_class == "1"
  rtn <- x$ticket_type == "R"
  x[, max_flat := ifelse(first,
                         ifelse(rtn, st$first_return_max_flat, st$first_single_max_flat),
                         ifelse(rtn, st$std_return_max_flat, st$std_single_max_flat))]
  x[, lower_min := ifelse(first, st$first_lower_min, st$std_lower_min)]
  x[, higher_min := ifelse(first, st$first_higher_min, st$std_higher_min)]

  pct <- x$discount_percentage / 1000  # field is percentage to 1 dp
  disc <- as.integer(round(x$fare * (1 - pct)))
  ind <- x$discount_indicator
  # in the caps, 0 means "no cap applies" and 99999999 (NA after import)
  # means no discounted fare exists for this class/type
  max_flat <- data.table::fifelse(!is.na(x$max_flat) & x$max_flat == 0L,
                                  disc, x$max_flat)
  higher_min <- data.table::fifelse(is.na(x$higher_min), 0L, x$higher_min)
  lower_min <- data.table::fifelse(is.na(x$lower_min), 0L, x$lower_min)
  new_fare <- data.table::fifelse(
    ind == "0", disc,
    data.table::fifelse(
      ind == "F", max_flat,
      data.table::fifelse(
        ind == "M", pmin(disc, max_flat),
        data.table::fifelse(
          ind == "H", pmax(disc, higher_min),
          data.table::fifelse(ind == "L", pmax(disc, lower_min),
                              NA_integer_)))))
  # a fare capped/flat-rated at 99999999 (NA after import) cannot be issued
  x[, fare := new_fare]
  x <- x[!is.na(fare)]
  x[, c("discount_category", "ticket_class", "ticket_type",
        "discount_indicator", "discount_percentage", "max_flat",
        "lower_min", "higher_min") := NULL]
  x
}


#' Add National Rail fares to a rail GTFS object
#'
#' Converts a National Rail fares feed (see [atoc_fares_read()]) to GTFS
#' fare tables and attaches them to a GTFS object produced by [atoc2gtfs()].
#' Both the original GTFS fares specification (`fares_version = 1`) and
#' GTFS Fares v2 (`fares_version = 2`) are supported.
#'
#' @details
#' **How rail fares map to GTFS**
#'
#' Rail fares are set per *flow*: an origin/destination pair which may be a
#' single station, a station cluster or a group station (e.g. "LONDON
#' TERMINALS"), per fare route and per ticket type.
#'
#' With `fares_version = 1` every station is a fare zone (`zone_id` =
#' CRS code) and each flow is expanded to all its member station pairs in
#' `fare_rules`. Because GTFS v1 has no concept of passenger types or ticket
#' choice, the *cheapest* fare among the selected ticket types is emitted
#' for each station pair, and `rider_categories`/`railcards` are ignored.
#' Choose e.g. `ticket_type = "single"` to control what "the fare" means.
#'
#' With `fares_version = 2` each flow endpoint becomes an area in
#' `areas`/`stop_areas` (clusters and group stations map directly, without
#' expansion), each ticket type/price becomes a `fare_products` row, and
#' each flow becomes rows in `fare_leg_rules`. Passenger types are
#' distinguished via `rider_categories`: adult fares always, child fares
#' and railcard-discounted fares on request. Where several fare routes link
#' the same pair of areas, multiple products apply and a journey planner may
#' choose among them.
#'
#' **Prices**
#'
#' Adult prices come directly from the flow file (plus the non-derivable
#' overrides file, which contains fares that cannot be derived, e.g. most
#' London fares - disable with `ndf = FALSE`). Child and railcard prices are
#' calculated from the status discount file: percentage discounts with
#' flat-fare/minimum-fare caps, as specified in RSPS5045. The industry
#' rounding rules file (FRR) is *not* applied, so calculated discount prices
#' can differ from retail prices by a few pence.
#'
#' **Restrictions and scenario conversion**
#'
#' GTFS has no general way to say *when* a ticket is valid, so by default
#' time restrictions are ignored: an Off-Peak product appears alongside the
#' Anytime product with no indication that it cannot be used at 08:00, and
#' Advance tickets are excluded entirely (their tier prices carry no
#' availability information).
#'
#' The `travel_date`, `travel_time` and `booking_date` arguments offer an
#' alternative: a **scenario snapshot**. Give a date (and optionally a
#' departure time and a booking date) and the restriction data is evaluated
#' at conversion time, so the output contains exactly the fares available
#' for that scenario - e.g. "travelling at 08:00 on Monday 3 August, booked
#' on 1 July" drops the Off-Peak products (invalid before 09:30 on most
#' flows) and includes the Advance tiers bookable a month ahead. The output
#' is still plain GTFS; the selection has simply been made for you.
#'
#' Caveats: Advance *prices* are the tier prices from the flow file - which
#' tier is actually on sale for a particular train is quota-controlled by
#' the reservation system and is not in any public feed, so treat Advance
#' fares as "best case". Restriction evaluation covers date bands and
#' departure-time bands (network-wide or origin-specific); arrival-based,
#' via-based and train-specific restrictions, easements and minimum-fare
#' windows are treated as "fare remains valid", so the filter errs towards
#' keeping a fare.
#'
#' Scenarios are validated against the coverage of the data: a
#' `travel_date` outside the GTFS calendar is an error (the timetable
#' contains no trips then), a date outside the feed's fares rounds warns
#' (see [atoc_fares_read()]), booking more than a year before travel is an
#' error and more than ~12 weeks (when bookings usually open) warns, and a
#' fares list read for a different date than `travel_date` warns.
#'
#' @param gtfs a GTFS object (named list of data frames) from [atoc2gtfs()].
#' @param fares a fares list from [atoc_fares_read()], or a path to a fares
#'   zip/folder which will be read for you.
#' @param fares_version numeric, `1` for the original GTFS fares tables
#'   (`fare_attributes`/`fare_rules`) or `2` for GTFS Fares v2. Default `1`.
#' @param ticket_codes optional character vector of ticket codes (e.g.
#'   `c("SDS","SDR")`) to convert, overriding
#'   `ticket_class`/`ticket_type`. See the `ticket_type` table of the fares
#'   list for available codes.
#' @param ticket_class character, `"standard"` and/or `"first"`. Default
#'   `"standard"`.
#' @param ticket_type character, any of `"single"`, `"return"`, `"season"`.
#'   Default `"single"` for `fares_version = 1` and
#'   `c("single","return")` for `fares_version = 2`. Note GTFS has no
#'   native concept of a return or season ticket: returns are emitted as a
#'   product priced for the round trip, seasons (weekly price) are excluded
#'   by default.
#' @param walkup_only logical, keep only walk-up (turn-up-and-go) tickets:
#'   the Anytime, Off-Peak and Super Off-Peak families (default TRUE).
#'   When FALSE every ticket type passing the class/type filters is
#'   converted, which includes trade, carnet and advance-purchase tier
#'   tickets whose headline prices are misleading (some are as low as
#'   GBP 0.05). Ignored when `ticket_codes` is given.
#' @param rider_categories character, any of `"adult"`, `"child"`. Which
#'   passenger types to emit (GTFS Fares v2 only). Default
#'   `c("adult","child")`.
#' @param railcards optional character vector of railcard codes (e.g.
#'   `"YNG"` = 16-25, `"SRN"` = Senior, `"DIS"` = Disabled; see the
#'   `railcard` table of the fares list). Each becomes an additional GTFS
#'   Fares v2 rider category with discounted prices. Default `NULL`.
#' @param ndf logical, include the non-derivable fare overrides file
#'   (default TRUE).
#' @param travel_date optional Date: convert fares for a journey on this
#'   date. The date/time restriction data (RST file) is evaluated and fares
#'   not valid on that date are dropped (e.g. tickets barred on certain
#'   dates). If `fares` is a path, the feed is also read as of this date.
#' @param travel_time optional departure time on `travel_date`, as
#'   `"HH:MM"` (or minutes after midnight). Fares whose time restriction
#'   makes them invalid at this departure time - e.g. Off-Peak tickets
#'   during the morning peak - are dropped. Requires `travel_date`.
#' @param booking_date optional Date: the day the ticket is bought. When
#'   given (with `travel_date`), Advance tickets whose booking horizon (TAP
#'   file) is satisfied are *included* in addition to the walk-up tickets,
#'   at their tier prices. Booking is assumed to happen by the end of this
#'   day. See Details for the quota caveat.
#' @param silent logical, suppress progress messages (default TRUE).
#' @return the GTFS object with fare tables added.
#' @family rail fares
#' @examples
#' \dontrun{
#' gtfs <- atoc2gtfs("ttis123.zip", ncores = 4)
#' fares <- atoc_fares_read("RJFAF756.zip")
#' # GTFS v1: cheapest standard single per station pair
#' gtfs_v1 <- gtfs_add_railfares(gtfs, fares, fares_version = 1)
#' # GTFS v2: adult/child/16-25 railcard singles and returns
#' gtfs_v2 <- gtfs_add_railfares(gtfs, fares, fares_version = 2,
#'                               railcards = "YNG")
#' # Scenario: departing 08:00 on 3 August, booked 1 July - peak-time
#' # walk-up fares plus the Advance tiers bookable a month ahead
#' gtfs_s <- gtfs_add_railfares(gtfs, fares, fares_version = 2,
#'                              travel_date = as.Date("2026-08-03"),
#'                              travel_time = "08:00",
#'                              booking_date = as.Date("2026-07-01"))
#' }
#' @md
#' @export
gtfs_add_railfares <- function(gtfs,
                               fares,
                               fares_version = 1,
                               ticket_codes = NULL,
                               ticket_class = "standard",
                               ticket_type = NULL,
                               walkup_only = TRUE,
                               rider_categories = c("adult", "child"),
                               railcards = NULL,
                               ndf = TRUE,
                               travel_date = NULL,
                               travel_time = NULL,
                               booking_date = NULL,
                               silent = TRUE) {
  checkmate::assert_choice(fares_version, c(1, 2))
  scenario <- railfares_check_scenario(travel_date, travel_time, booking_date)
  travel_date <- scenario$travel_date
  travel_time <- scenario$travel_time
  booking_date <- scenario$booking_date

  # the timetable cannot serve trips outside its calendar, so a scenario
  # beyond it describes journeys this GTFS does not contain
  if (!is.null(travel_date) && !is.null(gtfs$calendar) &&
      nrow(gtfs$calendar) > 0) {
    tt_range <- suppressWarnings(range(c(
      railfares_as_date(gtfs$calendar$start_date),
      railfares_as_date(gtfs$calendar$end_date)), na.rm = TRUE))
    if (!anyNA(tt_range) &&
        (travel_date < tt_range[1] || travel_date > tt_range[2])) {
      stop("travel_date (", travel_date, ") is outside the timetable ",
           "coverage of this GTFS (", tt_range[1], " to ", tt_range[2], ")")
    }
  }

  if (is.character(fares)) {
    read_date <- if (is.null(travel_date)) Sys.Date() else travel_date
    fares <- atoc_fares_read(fares, date = read_date, silent = silent)
  }
  checkmate::assert_list(fares)
  checkmate::assert_names(names(fares),
                          must.include = c("flow", "fare", "cluster",
                                           "location", "ticket_type"))

  # a pre-read fares list is already filtered to records valid on its read
  # date; converting for a different travel date risks using the wrong
  # fares round
  valid_on <- attr(fares, "valid_on")
  if (!is.null(travel_date) && !is.null(valid_on) &&
      as.Date(valid_on) != travel_date) {
    warning("The fares data was read as valid on ", valid_on,
            " but travel_date is ", travel_date,
            "; re-read it with atoc_fares_read(date = travel_date) ",
            "(or pass the fares zip path) to be sure of using the right ",
            "fares round")
  }
  if (is.null(ticket_type)) {
    ticket_type <- if (fares_version == 1) "single" else c("single", "return")
  }

  tickets <- railfares_tickets(fares, ticket_codes, ticket_class, ticket_type,
                               walkup_only = walkup_only)

  # bring in Advance tickets bookable at the requested horizon
  if (!is.null(booking_date) && is.null(ticket_codes)) {
    adv_codes <- railfares_advance_tickets(fares, booking_date, travel_date,
                                           travel_time)
    adv <- railfares_tickets(fares, NULL, ticket_class, ticket_type,
                             walkup_only = FALSE)
    # the TAP file also covers trade/test tickets; keep genuine Advance
    # products only (ITX = inclusive tour trade tickets)
    adv <- adv[ticket_code %in% adv_codes &
                 !ticket_code %in% tickets$ticket_code &
                 grepl("ADVANCE|\\bADV\\b", description, ignore.case = TRUE) &
                 !grepl("ITX", description, ignore.case = TRUE)]
    if (!silent) {
      message(Sys.time(), " Including ", nrow(adv), " Advance ticket types ",
              "bookable on ", booking_date, " for travel on ", travel_date,
              " (tier prices; actual availability is quota controlled)")
    }
    tickets <- rbind(tickets, adv)
  }

  if (nrow(tickets) == 0) {
    warning("No ticket types matched the selection; GTFS unchanged.")
    return(gtfs)
  }
  if (!silent) {
    message(Sys.time(), " Converting fares for ", nrow(tickets),
            " ticket types: ",
            paste(utils::head(tickets$ticket_code, 20), collapse = " "),
            if (nrow(tickets) > 20) " ...")
  }

  base <- railfares_base(fares, tickets, ndf = ndf)

  # scenario filtering: drop fares invalid on the travel date / at the
  # departure time
  if (!is.null(travel_date)) {
    base <- railfares_filter_restrictions(base, fares, travel_date,
                                          travel_time, silent = silent)
  }

  if (nrow(base) == 0) {
    warning("No fares found for the selected ticket types; GTFS unchanged.")
    return(gtfs)
  }

  if (fares_version == 1) {
    if (!is.null(railcards)) {
      warning("railcards are only supported with fares_version = 2, ignoring")
    }
    gtfs_add_railfares_v1(gtfs, fares, base, silent = silent)
  } else {
    gtfs_add_railfares_v2(gtfs, fares, base, tickets,
                          rider_categories = rider_categories,
                          railcards = railcards, silent = silent)
  }
}


#' Expand flows to station (CRS) pairs
#'
#' @param base fares table from railfares_base()
#' @param members endpoint members table from railfares_endpoints()
#' @return data.table with origin_crs/destination_crs per fare row;
#'   reversible flows are emitted in both directions
#' @noRd
railfares_expand_crs <- function(base, members) {
  o <- data.table::copy(members)
  data.table::setnames(o, c("origin", "origin_crs"))
  d <- data.table::copy(members)
  data.table::setnames(d, c("destination", "destination_crs"))
  x <- merge(base, o, by = "origin", allow.cartesian = TRUE)
  x <- merge(x, d, by = "destination", allow.cartesian = TRUE)
  rev <- x[direction == "R"]
  if (nrow(rev) > 0) {
    data.table::setnames(rev, c("origin_crs", "destination_crs"),
                         c("destination_crs", "origin_crs"))
    x <- data.table::rbindlist(list(x, rev), use.names = TRUE)
  }
  x[origin_crs != destination_crs]
}


#' Add GTFS v1 fares (fare_attributes + fare_rules) from a rail fares feed
#'
#' Internal worker for [gtfs_add_railfares()]. Every station becomes a fare
#' zone (its CRS code) and the cheapest selected fare per station pair is
#' emitted.
#'
#' @param gtfs GTFS object
#' @param fares fares list
#' @param base adult fares table from railfares_base()
#' @param silent logical
#' @return GTFS object with fare_attributes, fare_rules and stops$zone_id
#' @noRd
gtfs_add_railfares_v1 <- function(gtfs, fares, base, silent = TRUE) {
  base <- base[!is.na(fare)]  # v1 has no child fares
  stops <- gtfs$stops
  # zone_id = CRS code; stops without a CRS code get their own zone
  zone <- ifelse(is.na(stops$stop_code) | stops$stop_code == "",
                 stops$stop_id, stops$stop_code)
  gtfs$stops$zone_id <- zone

  ep <- railfares_endpoints(fares, c(base$origin, base$destination))
  members <- ep$members[crs %in% zone]

  codes <- unique(c(base$origin, base$destination))
  unresolved <- setdiff(codes, members$code)
  if (length(unresolved) > 0 && !silent) {
    message(length(unresolved), " of ", length(codes),
            " flow endpoints do not match any stop in this GTFS ",
            "(non-station codes or stations outside the timetable)")
  }

  x <- railfares_expand_crs(base, members)
  # v1 cannot express ticket choice: keep the cheapest fare per station pair
  x <- x[, list(fare = min(fare)),
         by = c("origin_crs", "destination_crs")]

  x[, fare_id := paste0(origin_crs, "_", destination_crs)]
  fare_attributes <- data.table::data.table(
    fare_id = x$fare_id,
    price = round(x$fare / 100, 2),
    currency_type = "GBP",
    payment_method = 1L,
    transfers = NA_integer_,   # empty = unlimited transfers on one ticket
    transfer_duration = NA_integer_
  )
  fare_rules <- data.table::data.table(
    fare_id = x$fare_id,
    route_id = NA_character_,
    origin_id = x$origin_crs,
    destination_id = x$destination_crs
  )

  if (!silent) {
    message(Sys.time(), " Built ", nrow(fare_attributes),
            " fares between ", length(unique(c(x$origin_crs, x$destination_crs))),
            " fare zones")
  }

  gtfs$fare_attributes <- rbind_fares(gtfs$fare_attributes, fare_attributes)
  gtfs$fare_rules <- rbind_fares(gtfs$fare_rules, fare_rules)
  gtfs
}


#' Add GTFS Fares v2 tables from a rail fares feed
#'
#' Internal worker for [gtfs_add_railfares()]. Flow endpoints become areas,
#' ticket types become fare products (per rider category) and flows become
#' fare leg rules on the "rail" network.
#'
#' @param gtfs GTFS object
#' @param fares fares list
#' @param base adult fares table from railfares_base()
#' @param tickets data.table from railfares_tickets()
#' @param rider_categories character, "adult" and/or "child"
#' @param railcards character vector of railcard codes or NULL
#' @param silent logical
#' @return GTFS object with areas, stop_areas, networks, route_networks,
#'   rider_categories, fare_media, fare_products and fare_leg_rules
#' @noRd
gtfs_add_railfares_v2 <- function(gtfs, fares, base, tickets,
                                  rider_categories = c("adult", "child"),
                                  railcards = NULL, silent = TRUE) {
  rider_categories <- match.arg(tolower(rider_categories),
                                c("adult", "child"), several.ok = TRUE)

  # ---- rider categories: adult, child, railcards -------------------------
  no_railcard <- fares$railcard[railcard_code == ""]
  fare_tables <- list()
  rider_tbl <- list()

  if ("adult" %in% rider_categories) {
    fare_tables[["adult"]] <- base[!is.na(fare)]
    rider_tbl[["adult"]] <- data.table::data.table(
      rider_category_id = "adult", rider_category_name = "Adult",
      is_default_fare_category = 1L)
  }
  if ("child" %in% rider_categories) {
    child_status <- if (nrow(no_railcard) > 0) no_railcard$child_status else "001"
    child <- railfares_discount(base[is.na(child_fare)], tickets, fares,
                                child_status)
    # non-derivable records carry an explicit child fare
    nd_child <- base[!is.na(child_fare)]
    if (nrow(nd_child) > 0) {
      nd_child[, fare := child_fare]
      child <- data.table::rbindlist(list(child, nd_child), use.names = TRUE)
    }
    fare_tables[["child"]] <- child
    rider_tbl[["child"]] <- data.table::data.table(
      rider_category_id = "child", rider_category_name = "Child (age 5-15)",
      is_default_fare_category = 0L)
  }
  for (rc in railcards) {
    rl <- fares$railcard[railcard_code == rc]
    if (nrow(rl) == 0) {
      warning("Railcard code '", rc, "' not found in fares feed, skipping")
      next
    }
    rc_id <- paste0("railcard_", tolower(rc))
    disc <- railfares_discount(base, tickets, fares, rl$adult_status)
    # non-derivable fares for this railcard override the calculated price
    nd_rc <- fares$ndf[railcard_code == rc &
                         ticket_code %in% tickets$ticket_code &
                         !is.na(adult_fare)]
    if (nrow(nd_rc) > 0) {
      nd_rc <- nd_rc[, c("origin", "destination", "route_code",
                         "ticket_code", "adult_fare", "restriction_code")]
      data.table::setnames(nd_rc, "adult_fare", "fare")
      nd_rc[, direction := "S"]
      nd_rc[, child_fare := NA_integer_]
      key <- c("origin", "destination", "route_code", "ticket_code")
      disc <- disc[!nd_rc, on = key]
      disc <- data.table::rbindlist(list(disc, nd_rc), use.names = TRUE)
    }
    fare_tables[[rc_id]] <- disc
    rc_name <- rl$description
    if (!grepl("railcard", rc_name, ignore.case = TRUE)) {
      rc_name <- paste(rc_name, "railcard")
    }
    rider_tbl[[rc_id]] <- data.table::data.table(
      rider_category_id = rc_id,
      rider_category_name = rc_name,
      is_default_fare_category = 0L)
  }
  fare_tables <- fare_tables[vapply(fare_tables, nrow, 1L) > 0]
  if (length(fare_tables) == 0) {
    warning("No fares survived rider category selection; GTFS unchanged.")
    return(gtfs)
  }

  # ---- areas and stop areas ----------------------------------------------
  all_codes <- unique(unlist(lapply(fare_tables, function(x)
    c(x$origin, x$destination))))
  ep <- railfares_endpoints(fares, all_codes)
  crs2stop <- data.table::data.table(
    crs = as.character(gtfs$stops$stop_code),
    stop_id = as.character(gtfs$stops$stop_id))
  crs2stop <- crs2stop[!is.na(crs) & crs != ""]
  members <- merge(ep$members, crs2stop, by = "crs", allow.cartesian = TRUE)

  unresolved <- setdiff(all_codes, members$code)
  if (length(unresolved) > 0 && !silent) {
    message(length(unresolved), " of ", length(all_codes),
            " flow endpoints do not match any stop in this GTFS ",
            "(non-station codes or stations outside the timetable)")
  }

  area_id <- function(code) paste0("rail_", code)
  areas <- ep$names[code %in% members$code]
  areas <- data.table::data.table(
    area_id = area_id(areas$code),
    area_name = areas$endpoint_name)
  stop_areas <- unique(data.table::data.table(
    area_id = area_id(members$code),
    stop_id = members$stop_id))

  # ---- network -----------------------------------------------------------
  networks <- data.table::data.table(network_id = "rail",
                                     network_name = "National Rail")
  rail_routes <- gtfs$routes$route_id[gtfs$routes$route_type == 2]
  route_networks <- data.table::data.table(
    network_id = "rail", route_id = rail_routes)

  fare_media <- data.table::data.table(
    fare_media_id = "ticket",
    fare_media_name = "Rail ticket",
    fare_media_type = 1L)  # physical paper ticket

  # ---- products and leg rules --------------------------------------------
  ticket_names <- stats::setNames(tickets$description, tickets$ticket_code)
  rider_names <- stats::setNames(
    vapply(rider_tbl, function(x) x$rider_category_name, ""),
    vapply(rider_tbl, function(x) x$rider_category_id, ""))
  valid_codes <- unique(members$code)

  fare_products <- list()
  fare_leg_rules <- list()
  for (rc_id in names(fare_tables)) {
    x <- fare_tables[[rc_id]][origin %in% valid_codes &
                                destination %in% valid_codes]
    if (nrow(x) == 0) next
    # a product is a ticket at a price for a rider category
    x[, fare_product_id := paste0(tolower(ticket_code), "_", rc_id, "_", fare)]
    prods <- unique(x[, c("fare_product_id", "ticket_code", "fare")])
    fare_products[[rc_id]] <- data.table::data.table(
      fare_product_id = prods$fare_product_id,
      fare_product_name = paste0(ticket_names[prods$ticket_code], " (",
                                 rider_names[rc_id], ")"),
      rider_category_id = rc_id,
      fare_media_id = "ticket",
      amount = round(prods$fare / 100, 2),
      currency = "GBP")

    lr <- data.table::data.table(
      leg_group_id = paste0("rail_", tolower(x$ticket_code), "_", rc_id),
      network_id = "rail",
      from_area_id = area_id(x$origin),
      to_area_id = area_id(x$destination),
      fare_product_id = x$fare_product_id,
      direction = x$direction)
    rev <- lr[direction == "R"]
    if (nrow(rev) > 0) {
      data.table::setnames(rev, c("from_area_id", "to_area_id"),
                           c("to_area_id", "from_area_id"))
      lr <- data.table::rbindlist(list(lr, rev), use.names = TRUE)
    }
    lr[, direction := NULL]
    fare_leg_rules[[rc_id]] <- unique(lr)
  }
  if (length(fare_leg_rules) == 0) {
    warning("No fares could be matched to the stops in this GTFS; unchanged.")
    return(gtfs)
  }
  fare_products <- unique(data.table::rbindlist(fare_products))
  fare_products <- fare_products[!duplicated(fare_product_id)]
  fare_leg_rules <- unique(data.table::rbindlist(fare_leg_rules))
  rider_categories_tbl <- data.table::rbindlist(rider_tbl)
  # keep only categories that ended up with a fare product, and make sure
  # exactly one is flagged default (required by GTFS Fares v2; "adult" may
  # not have been selected)
  rider_categories_tbl <- rider_categories_tbl[
    rider_category_id %in% fare_products$rider_category_id]
  if (!any(rider_categories_tbl$is_default_fare_category == 1L)) {
    rider_categories_tbl[1, is_default_fare_category := 1L]
  }

  if (!silent) {
    message(Sys.time(), " Built ", nrow(areas), " areas, ",
            nrow(fare_products), " fare products and ",
            nrow(fare_leg_rules), " fare leg rules for ",
            nrow(rider_categories_tbl), " rider categories")
  }

  gtfs$areas <- rbind_fares(gtfs$areas, areas)
  gtfs$stop_areas <- rbind_fares(gtfs$stop_areas, stop_areas)
  gtfs$networks <- rbind_fares(gtfs$networks, networks)
  gtfs$route_networks <- rbind_fares(gtfs$route_networks, route_networks)
  gtfs$rider_categories <- rbind_fares(gtfs$rider_categories, rider_categories_tbl)
  gtfs$fare_media <- rbind_fares(gtfs$fare_media, fare_media)
  gtfs$fare_products <- rbind_fares(gtfs$fare_products, fare_products)
  gtfs$fare_leg_rules <- rbind_fares(gtfs$fare_leg_rules, fare_leg_rules)
  gtfs
}
