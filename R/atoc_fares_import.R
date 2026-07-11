# Readers for the RSP / National Rail "DTD" fares feed (RSPS5045).
#
# The fares feed is a set of fixed-width plain-text files (one per record
# type family) distributed as e.g. RJFAF756.FFL, RJFAF756.LOC, ... inside a
# single zip. Field positions below come from RSPS5045 issue 02-00.


#' Read a fixed-width DTD fares file into a character vector
#'
#' Strips the `/!!` header comment lines that start every DTD file.
#'
#' @param file path to a single fares file
#' @return character vector of data records
#' @noRd
fares_read_lines <- function(file) {
  x <- data.table::fread(
    file,
    sep = NULL,
    header = FALSE,
    quote = "",
    strip.white = FALSE,
    colClasses = "character",
    showProgress = FALSE
  )[[1]]
  x[!startsWith(x, "/")]
}


#' Extract a fixed-width field
#'
#' @param x character vector of records
#' @param from,to 1-based start/end positions (as printed in RSPS5045)
#' @param trim trim surrounding white space (default TRUE)
#' @noRd
fares_field <- function(x, from, to, trim = TRUE) {
  out <- substr(x, from, to)
  if (trim) out <- trimws(out)
  out
}


#' Parse a ddmmyyyy date field to an integer yyyymmdd
#'
#' Comparing integer yyyymmdd values is much faster than converting millions
#' of records to Date. Blank fields become NA.
#'
#' @param x character vector of ddmmyyyy values
#' @noRd
fares_dateint <- function(x) {
  suppressWarnings(as.integer(paste0(
    substr(x, 5, 8), substr(x, 3, 4), substr(x, 1, 2)
  )))
}


#' Parse an 8-digit pence field, treating 99999999 (no fare) as NA
#' @param x character vector
#' @noRd
fares_pence <- function(x) {
  out <- suppressWarnings(as.integer(x))
  out[!is.na(out) & out == 99999999L] <- NA_integer_
  out
}


#' Keep records valid on a given date and drop delete records
#'
#' @param dt data.table with `start_date`/`end_date` integer yyyymmdd columns
#'   (either may be absent) and optionally an `update_marker` column.
#' @param dateint integer yyyymmdd
#' @noRd
fares_filter_date <- function(dt, dateint) {
  if ("update_marker" %in% names(dt)) {
    dt <- dt[is.na(update_marker) | update_marker != "D"]
    dt[, update_marker := NULL]
  }
  if ("start_date" %in% names(dt)) {
    dt <- dt[is.na(start_date) | start_date <= dateint]
  }
  if ("end_date" %in% names(dt)) {
    dt <- dt[is.na(end_date) | end_date >= dateint]
  }
  dt
}


#' Import the Flow (FFL) file
#'
#' Reads the flow file of the fares feed, which contains 'F' (flow) records
#' describing an origin/destination pair and 'T' (fare) records giving the
#' price of each ticket type on that flow.
#'
#' @param file path to the .FFL file
#' @param dateint integer yyyymmdd, keep flows valid on this date
#' @param silent logical, suppress progress messages
#' @return list with data.tables `flow` and `fare`
#' @noRd
importFFL <- function(file, dateint, silent = TRUE) {
  if (!silent) message(Sys.time(), " Reading fares flow file (this is the big one)")
  x <- fares_read_lines(file)
  types <- substr(x, 2, 2)

  f <- x[types == "F"]
  coverage_start <- suppressWarnings(
    min(fares_dateint(substr(f, 29, 36)), na.rm = TRUE))
  coverage_end <- suppressWarnings(
    max(fares_dateint(substr(f, 21, 28)), na.rm = TRUE))
  flow <- data.table::data.table(
    update_marker = substr(f, 1, 1),
    origin = fares_field(f, 3, 6),
    destination = fares_field(f, 7, 10),
    route_code = fares_field(f, 11, 15),
    status_code = fares_field(f, 16, 18),
    usage_code = substr(f, 19, 19),
    direction = substr(f, 20, 20),
    end_date = fares_dateint(substr(f, 21, 28)),
    start_date = fares_dateint(substr(f, 29, 36)),
    toc = fares_field(f, 37, 39),
    cross_london = substr(f, 40, 40),
    ns_disc_ind = substr(f, 41, 41),
    flow_id = substr(f, 43, 49)
  )
  flow <- fares_filter_date(flow, dateint)

  t <- x[types == "T"]
  rm(x, types)
  t <- t[substr(t, 1, 1) != "D"]  # drop deletes in changes-only files
  fare <- data.table::data.table(
    flow_id = substr(t, 3, 9),
    ticket_code = substr(t, 10, 12),
    fare = fares_pence(substr(t, 13, 20)),
    restriction_code = fares_field(t, 21, 22)
  )
  fare <- fare[!is.na(fare)]
  # keep only fares whose flow survived the date filter
  fare <- fare[flow_id %in% flow$flow_id]

  list(flow = flow, fare = fare,
       coverage = c(start = coverage_start, end = coverage_end))
}


#' Import the Station Clusters (FSC) file
#' @inheritParams importFFL
#' @return data.table with `cluster_id` and `cluster_nlc`
#' @noRd
importFSC <- function(file, dateint) {
  x <- fares_read_lines(file)
  cluster <- data.table::data.table(
    update_marker = substr(x, 1, 1),
    cluster_id = fares_field(x, 2, 5),
    cluster_nlc = fares_field(x, 6, 9),
    end_date = fares_dateint(substr(x, 10, 17)),
    start_date = fares_dateint(substr(x, 18, 25))
  )
  cluster <- fares_filter_date(cluster, dateint)
  cluster[, c("start_date", "end_date") := NULL]
  unique(cluster)
}


#' Import the Locations (LOC) file
#'
#' Parses the 'L' (location), 'G' (group location) and 'M' (group member)
#' record types. Group locations (e.g. 1072 "LONDON TERMINALS" or the London
#' travelcard zones) are pseudo-locations used as flow endpoints; their NLC is
#' characters 3-6 of the UIC code.
#'
#' @inheritParams importFFL
#' @return list with data.tables `location`, `group` and `group_member`
#' @noRd
importLOC <- function(file, dateint) {
  x <- fares_read_lines(file)
  types <- substr(x, 2, 2)

  l <- x[types == "L"]
  location <- data.table::data.table(
    update_marker = substr(l, 1, 1),
    uic = fares_field(l, 3, 9),
    end_date = fares_dateint(substr(l, 10, 17)),
    start_date = fares_dateint(substr(l, 18, 25)),
    nlc = fares_field(l, 37, 40),
    description = fares_field(l, 41, 56),
    crs = fares_field(l, 57, 59),
    fare_group = fares_field(l, 70, 75)
  )
  location <- fares_filter_date(location, dateint)
  location <- location[nlc != ""]
  location <- location[!duplicated(nlc)]

  g <- x[types == "G"]
  group <- data.table::data.table(
    update_marker = substr(g, 1, 1),
    group_uic = fares_field(g, 3, 9),
    end_date = fares_dateint(substr(g, 10, 17)),
    start_date = fares_dateint(substr(g, 18, 25)),
    description = fares_field(g, 34, 49)
  )
  group <- fares_filter_date(group, dateint)
  group[, group_nlc := substr(group_uic, 3, 6)]
  group <- group[!duplicated(group_nlc)]

  m <- x[types == "M"]
  group_member <- data.table::data.table(
    update_marker = substr(m, 1, 1),
    group_uic = fares_field(m, 3, 9),
    end_date = fares_dateint(substr(m, 10, 17)),
    member_crs = fares_field(m, 25, 27)
  )
  group_member <- fares_filter_date(group_member, dateint)
  group_member[, group_nlc := substr(group_uic, 3, 6)]
  group_member <- unique(group_member[member_crs != "",
                                      c("group_nlc", "member_crs")])

  list(location = location, group = group, group_member = group_member)
}


#' Import the Ticket Types (TTY) file
#' @inheritParams importFFL
#' @return data.table of ticket types
#' @noRd
importTTY <- function(file, dateint) {
  x <- fares_read_lines(file)
  tt <- data.table::data.table(
    update_marker = substr(x, 1, 1),
    ticket_code = fares_field(x, 2, 4),
    end_date = fares_dateint(substr(x, 5, 12)),
    start_date = fares_dateint(substr(x, 13, 20)),
    description = fares_field(x, 29, 43),
    ticket_class = substr(x, 44, 44),
    ticket_type = substr(x, 45, 45),
    ticket_group = substr(x, 46, 46),
    max_passengers = suppressWarnings(as.integer(substr(x, 55, 57))),
    min_passengers = suppressWarnings(as.integer(substr(x, 58, 60))),
    max_adults = suppressWarnings(as.integer(substr(x, 61, 63))),
    max_children = suppressWarnings(as.integer(substr(x, 67, 69))),
    package_mkr = substr(x, 108, 108),
    discount_category = fares_field(x, 112, 113)
  )
  tt <- fares_filter_date(tt, dateint)
  tt[!duplicated(ticket_code)]
}


#' Import the Non-Derivable Fare Overrides (NFO) file
#'
#' Contains point-to-point adult and child fares which cannot be derived from
#' the flow file (for example most London travelcard-zone fares), keyed by
#' origin/destination/route/railcard/ticket.
#'
#' @inheritParams importFFL
#' @return data.table of non-derivable fares
#' @noRd
importNFO <- function(file, dateint) {
  x <- fares_read_lines(file)
  ndf <- data.table::data.table(
    update_marker = substr(x, 1, 1),
    origin = fares_field(x, 2, 5),
    destination = fares_field(x, 6, 9),
    route_code = fares_field(x, 10, 14),
    railcard_code = fares_field(x, 15, 17),
    ticket_code = fares_field(x, 18, 20),
    end_date = fares_dateint(substr(x, 22, 29)),
    start_date = fares_dateint(substr(x, 30, 37)),
    adult_fare = fares_pence(substr(x, 47, 54)),
    child_fare = fares_pence(substr(x, 55, 62)),
    restriction_code = fares_field(x, 63, 64),
    composite_indicator = substr(x, 65, 65)
  )
  ndf <- fares_filter_date(ndf, dateint)
  # 'N' composites duplicate fares already present in the flow file
  ndf <- ndf[composite_indicator == "Y"]
  ndf[, composite_indicator := NULL]
  # where several records survive for one key keep the latest start date
  data.table::setorder(ndf, -start_date, na.last = TRUE)
  ndf <- ndf[!duplicated(ndf[, c("origin", "destination", "route_code",
                                 "railcard_code", "ticket_code")])]
  ndf
}


#' Import the Status Discounts (DIS) file
#'
#' 'S' records describe a passenger status (adult = '000', child = '001',
#' plus one per railcard status) with flat-fare and minimum-fare caps;
#' 'D' records give the discount percentage per discount category.
#'
#' @inheritParams importFFL
#' @return list with data.tables `status` and `status_discount`
#' @noRd
importDIS <- function(file, dateint) {
  x <- fares_read_lines(file)
  types <- substr(x, 1, 1)

  s <- x[types == "S"]
  status <- data.table::data.table(
    status_code = fares_field(s, 2, 4),
    end_date = fares_dateint(substr(s, 5, 12)),
    start_date = fares_dateint(substr(s, 13, 20)),
    first_single_max_flat = fares_pence(substr(s, 32, 39)),
    first_return_max_flat = fares_pence(substr(s, 40, 47)),
    std_single_max_flat = fares_pence(substr(s, 48, 55)),
    std_return_max_flat = fares_pence(substr(s, 56, 63)),
    first_lower_min = fares_pence(substr(s, 64, 71)),
    first_higher_min = fares_pence(substr(s, 72, 79)),
    std_lower_min = fares_pence(substr(s, 80, 87)),
    std_higher_min = fares_pence(substr(s, 88, 95))
  )
  status <- fares_filter_date(status, dateint)
  status <- status[!duplicated(status_code)]

  d <- x[types == "D"]
  status_discount <- data.table::data.table(
    status_code = fares_field(d, 2, 4),
    end_date = fares_dateint(substr(d, 5, 12)),
    discount_category = fares_field(d, 13, 14),
    discount_indicator = substr(d, 15, 15),
    discount_percentage = suppressWarnings(as.integer(substr(d, 16, 18)))
  )
  status_discount <- fares_filter_date(status_discount, dateint)
  status_discount <- status_discount[
    !duplicated(status_discount[, c("status_code", "discount_category")])]

  list(status = status, status_discount = status_discount)
}


#' Import the Railcards (RLC) file
#'
#' The record with a blank railcard code carries the status codes used to
#' derive undiscounted child fares.
#'
#' @inheritParams importFFL
#' @return data.table of railcards
#' @noRd
importRLC <- function(file, dateint) {
  x <- fares_read_lines(file)
  rlc <- data.table::data.table(
    railcard_code = fares_field(x, 1, 3),
    end_date = fares_dateint(substr(x, 4, 11)),
    start_date = fares_dateint(substr(x, 12, 19)),
    holder_type = substr(x, 28, 28),
    description = fares_field(x, 29, 48),
    adult_status = fares_field(x, 119, 121),
    child_status = fares_field(x, 122, 124)
  )
  rlc <- fares_filter_date(rlc, dateint)
  rlc[!duplicated(railcard_code)]
}


#' Import the Routes (RTE) file ('R' records only)
#' @inheritParams importFFL
#' @return data.table of fare route descriptions
#' @noRd
importRTE <- function(file, dateint) {
  x <- fares_read_lines(file)
  r <- x[substr(x, 2, 2) == "R"]
  rte <- data.table::data.table(
    update_marker = substr(r, 1, 1),
    route_code = fares_field(r, 3, 7),
    end_date = fares_dateint(substr(r, 8, 15)),
    start_date = fares_dateint(substr(r, 16, 23)),
    route_description = fares_field(r, 32, 47)
  )
  rte <- fares_filter_date(rte, dateint)
  rte[!duplicated(route_code)]
}


#' Import the Restrictions (RST) file
#'
#' Parses the record types needed to evaluate whether a fare is valid on a
#' given date and time of travel: 'RD' (the date ranges of the Current and
#' Future restriction data), 'RH' (restriction headers), 'HD' (header date
#' bands), 'TR' (time restrictions) and 'TD' (time restriction date bands).
#' The other record types (train-specific restrictions, easements, railcard
#' restrictions...) are not parsed.
#'
#' Unlike the other fares files, restriction records are not filtered by
#' date here: they carry a Current/Future marker instead, which is resolved
#' against the travel date at conversion time.
#'
#' @param file path to the .RST file
#' @return list of data.tables: `restriction_dates`, `restriction`,
#'   `restriction_date_band`, `time_restriction`,
#'   `time_restriction_date_band`
#' @noRd
importRST <- function(file) {
  x <- fares_read_lines(file)
  x <- x[substr(x, 1, 1) != "D"]  # drop deletes in changes-only files
  types <- substr(x, 2, 3)

  rd <- x[types == "RD"]
  restriction_dates <- data.table::data.table(
    cf_mkr = substr(rd, 4, 4),
    start_date = fares_dateint(substr(rd, 5, 12)),
    end_date = fares_dateint(substr(rd, 13, 20))
  )

  rh <- x[types == "RH"]
  restriction <- data.table::data.table(
    cf_mkr = substr(rh, 4, 4),
    restriction_code = fares_field(rh, 5, 6),
    description = fares_field(rh, 7, 36)
  )

  hd <- x[types == "HD"]
  restriction_date_band <- data.table::data.table(
    cf_mkr = substr(hd, 4, 4),
    restriction_code = fares_field(hd, 5, 6),
    date_from = substr(hd, 7, 10),   # MMDD
    date_to = substr(hd, 11, 14),    # MMDD
    days = substr(hd, 15, 21)        # YN markers, Monday first
  )

  tr <- x[types == "TR"]
  time_restriction <- data.table::data.table(
    cf_mkr = substr(tr, 4, 4),
    restriction_code = fares_field(tr, 5, 6),
    sequence_no = substr(tr, 7, 10),
    out_ret = substr(tr, 11, 11),
    time_from = substr(tr, 12, 15),  # HHMM
    time_to = substr(tr, 16, 19),    # HHMM
    arr_dep_via = substr(tr, 20, 20),
    location = fares_field(tr, 21, 23),
    min_fare_flag = substr(tr, 26, 26)
  )

  td <- x[types == "TD"]
  time_restriction_date_band <- data.table::data.table(
    cf_mkr = substr(td, 4, 4),
    restriction_code = fares_field(td, 5, 6),
    sequence_no = substr(td, 7, 10),
    out_ret = substr(td, 11, 11),
    date_from = substr(td, 12, 15),  # MMDD
    date_to = substr(td, 16, 19),    # MMDD
    days = substr(td, 20, 26)        # YN markers, Monday first
  )

  list(
    restriction_dates = restriction_dates,
    restriction = restriction,
    restriction_date_band = restriction_date_band,
    time_restriction = time_restriction,
    time_restriction_date_band = time_restriction_date_band
  )
}


#' Import the Advance Purchase Tickets (TAP) file
#'
#' Gives the booking horizon of each advance-purchase ticket: either a
#' book-by date, a minimum number of hours before departure, or a minimum
#' number of days before travel.
#'
#' @inheritParams importFFL
#' @return data.table of advance purchase booking horizons
#' @noRd
importTAP <- function(file, dateint) {
  x <- fares_read_lines(file)
  adv <- data.table::data.table(
    ticket_code = fares_field(x, 1, 3),
    restriction_code = fares_field(x, 4, 5),
    restriction_flag = substr(x, 6, 6),
    toc_id = fares_field(x, 7, 8),
    end_date = fares_dateint(substr(x, 9, 16)),
    start_date = fares_dateint(substr(x, 17, 24)),
    check_type = substr(x, 25, 25),
    ap_data = fares_field(x, 26, 33),
    booking_time = fares_field(x, 34, 37)
  )
  fares_filter_date(adv, dateint)
}


#' Import the TOCs (TOC) file
#' @param file path to the .TOC file
#' @return data.table mapping fare TOC ids to CIF TOC ids and names
#' @noRd
importTOC <- function(file) {
  x <- fares_read_lines(file)
  f <- x[substr(x, 1, 1) == "F"]
  data.table::data.table(
    fare_toc_id = fares_field(f, 2, 4),
    toc_id = fares_field(f, 5, 6),
    fare_toc_name = fares_field(f, 7, 36)
  )
}


#' Read a National Rail (DTD/RSPS5045) fares feed
#'
#' Reads the fixed-width fares files distributed by the Rail Delivery Group
#' via the \href{https://opendata.nationalrail.co.uk/}{National Rail Data
#' Portal} (the "Fares" download, e.g. \code{RJFAF756.zip}), as documented in
#' RSPS5045. These are the same files used by ticket issuing systems and
#' cover every advertised point-to-point fare in Great Britain.
#'
#' Only the files needed to build GTFS fares are read: FFL (flows and
#' prices), FSC (station clusters), LOC (locations and group stations), TTY
#' (ticket types), NFO (non-derivable fare overrides), DIS (status
#' discounts), RLC (railcards), RTE (fare routes), TOC (operator codes),
#' RST (date/time restrictions) and TAP (advance purchase booking horizons).
#'
#' Records are filtered to those valid on \code{date}: the feed contains
#' current and future fares rounds, so changing \code{date} selects the
#' fares round in force on that day. A date outside the fares rounds
#' present in the feed raises a warning (the feed is a snapshot - reading
#' it for a long-past or far-future date does not give the prices of that
#' day), and a date on which no records at all are valid is an error.
#'
#' @param path character, path to the fares zip file (e.g.
#'   \code{"RJFAF756.zip"}) or to a folder containing the extracted files.
#' @param date Date (or something coercible), keep records valid on this
#'   date. Default \code{Sys.Date()}.
#' @param silent logical, suppress progress messages (default TRUE).
#' @return A named list of data.tables: \code{flow}, \code{fare},
#'   \code{cluster}, \code{location}, \code{group}, \code{group_member},
#'   \code{ticket_type}, \code{ndf}, \code{status}, \code{status_discount},
#'   \code{railcard}, \code{route}, \code{toc}, \code{advance} and the
#'   restriction tables \code{restriction_dates}, \code{restriction},
#'   \code{restriction_date_band}, \code{time_restriction} and
#'   \code{time_restriction_date_band}.
#' @family rail fares
#' @seealso [gtfs_add_railfares()] to convert to GTFS fare tables,
#'   [atoc2gtfs()] which can do both steps in one go.
#' @md
#' @export
atoc_fares_read <- function(path, date = Sys.Date(), silent = TRUE) {
  checkmate::assert_character(path, len = 1)
  checkmate::assert_logical(silent)
  date <- as.Date(date)
  dateint <- as.integer(format(date, "%Y%m%d"))

  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    checkmate::assert_file_exists(path)
    exdir <- file.path(tempdir(), "uk2gtfs_fares")
    unlink(exdir, recursive = TRUE)
    dir.create(exdir)
    utils::unzip(path, exdir = exdir)
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
    files <- list.files(exdir, full.names = TRUE, recursive = TRUE)
  } else {
    checkmate::assert_directory_exists(path)
    files <- list.files(path, full.names = TRUE)
  }

  find_file <- function(ext, required = TRUE) {
    fl <- files[grepl(paste0("\\.", ext, "$"), files, ignore.case = TRUE)]
    if (length(fl) == 0) {
      if (required) stop("Fares feed is missing the .", ext, " file")
      return(NULL)
    }
    fl[1]
  }

  if (!silent) message(Sys.time(), " Reading fares feed, valid on ", date)

  ffl <- importFFL(find_file("FFL"), dateint, silent = silent)
  if (nrow(ffl$flow) == 0) {
    fmt_cov <- function(x) {
      if (is.infinite(x)) return("unknown")
      if (x == 29991231) return("open-ended")
      as.character(as.Date(as.character(x), format = "%Y%m%d"))
    }
    stop("No fare records in this feed are valid on ", date,
         "; the feed covers ", fmt_cov(ffl$coverage[["start"]]), " to ",
         fmt_cov(ffl$coverage[["end"]]),
         ". Choose a date inside that range (see the date/travel_date ",
         "arguments).")
  }
  loc <- importLOC(find_file("LOC"), dateint)
  dis <- importDIS(find_file("DIS"), dateint)

  fares <- list(
    flow = ffl$flow,
    fare = ffl$fare,
    cluster = importFSC(find_file("FSC"), dateint),
    location = loc$location,
    group = loc$group,
    group_member = loc$group_member,
    ticket_type = importTTY(find_file("TTY"), dateint),
    status = dis$status,
    status_discount = dis$status_discount,
    railcard = importRLC(find_file("RLC"), dateint),
    route = importRTE(find_file("RTE"), dateint),
    toc = importTOC(find_file("TOC"))
  )

  rst <- find_file("RST", required = FALSE)
  if (is.null(rst)) {
    fares$restriction_dates <- data.table::data.table(
      cf_mkr = character(), start_date = integer(), end_date = integer())
    fares$restriction <- data.table::data.table(
      cf_mkr = character(), restriction_code = character(),
      description = character())
    fares$restriction_date_band <- data.table::data.table(
      cf_mkr = character(), restriction_code = character(),
      date_from = character(), date_to = character(), days = character())
    fares$time_restriction <- data.table::data.table(
      cf_mkr = character(), restriction_code = character(),
      sequence_no = character(), out_ret = character(),
      time_from = character(), time_to = character(),
      arr_dep_via = character(), location = character(),
      min_fare_flag = character())
    fares$time_restriction_date_band <- data.table::data.table(
      cf_mkr = character(), restriction_code = character(),
      sequence_no = character(), out_ret = character(),
      date_from = character(), date_to = character(), days = character())
  } else {
    fares <- c(fares, importRST(rst))
  }

  tap <- find_file("TAP", required = FALSE)
  fares$advance <- if (is.null(tap)) {
    data.table::data.table(
      ticket_code = character(), restriction_code = character(),
      restriction_flag = character(), toc_id = character(),
      end_date = integer(), start_date = integer(),
      check_type = character(), ap_data = character(),
      booking_time = character())
  } else {
    importTAP(tap, dateint)
  }

  nfo <- find_file("NFO", required = FALSE)
  fares$ndf <- if (is.null(nfo)) {
    data.table::data.table(
      origin = character(), destination = character(),
      route_code = character(), railcard_code = character(),
      ticket_code = character(), end_date = integer(),
      start_date = integer(), adult_fare = integer(),
      child_fare = integer(), restriction_code = character()
    )
  } else {
    importNFO(nfo, dateint)
  }

  # the restriction dates records give the fares rounds this feed actually
  # describes; prices for dates outside them are not representative
  rd <- fares$restriction_dates
  rd <- rd[!is.na(rd$start_date) & !is.na(rd$end_date), ]
  if (nrow(rd) > 0 &&
      (dateint < min(rd$start_date) || dateint > max(rd$end_date))) {
    fmt <- function(x) {
      if (x == 29991231) "open-ended" else
        as.character(as.Date(as.character(x), format = "%Y%m%d"))
    }
    warning("The date ", date, " is outside the fares rounds in this feed (",
            fmt(min(rd$start_date)), " to ", fmt(max(rd$end_date)),
            "); the feed is a snapshot of current and future fares, so ",
            "prices read for this date may not be representative.")
  }

  if (!silent) {
    message(Sys.time(), " Read ", nrow(fares$flow), " flows, ",
            nrow(fares$fare), " fares, ", nrow(fares$ndf),
            " non-derivable fares")
  }

  # remember which date the records were filtered to, so the converter can
  # spot a mismatch with a later travel_date
  attr(fares, "valid_on") <- date
  attr(fares, "coverage") <- ffl$coverage
  fares
}
