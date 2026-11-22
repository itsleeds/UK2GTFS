#' Normalise a TransXChange Description for comparison
#'
#' A description is free text, retyped by the publisher with each
#' re-registration, and it does not come back the same twice: case moves,
#' punctuation moves, and the places are listed in a different order. Compared
#' raw it splits one service into several groups, which is the opposite of what
#' the key is for.
#'
#' Case is folded, every run of non alphanumeric characters becomes a single
#' space, and the words are sorted, so "Ealing Broadway/West Ruislip -
#' Liverpool Street - Epping/Hainault/Woodford" and "West Ruislip/Ealing
#' Broadway - Liverpool Street - Hainault/Woodford/Epping" compare equal.
#'
#' Sorting the words cannot repair a misspelling - "Broaddway" is still not
#' "Broadway" - which is why fixed track services are grouped without the
#' description at all. See [txc_filter_files()].
#'
#' @param x character vector of descriptions
#' @return a character vector, "" where there is no usable description
#' @noRd
normalise_description <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x <- tolower(gsub("[^[:alnum:]]+", " ", x))
  x <- trimws(x)
  vapply(strsplit(x, " ", fixed = TRUE), function(w) {
    w <- w[nzchar(w)]
    if (length(w) == 0) "" else paste(sort(w), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}


#' Expand TransXChange DaysOfWeek names to individual days
#'
#' The children of `<DaysOfWeek>` are not all single days: the schema also
#' allows ranges and negations (`MondayToSaturday`, `Weekend`, `NotSunday`).
#' Compared as the strings they are written in, `MondayToSaturday` and
#' `Saturday` look like different days when one contains the other, and a
#' publisher writing `MondayToFriday` in one file and five separate days in
#' the next looks like two different timetables.
#'
#' Expanding to the set of individual days settles both. Anything unrecognised
#' is kept as itself, so an unexpected value groups with its own kind rather
#' than silently matching everything.
#'
#' @param x character vector of DaysOfWeek child element names
#' @return a sorted character vector of day names, possibly empty
#' @noRd
expand_days_of_week <- function(x) {
  wk <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
  all7 <- c(wk, "Saturday", "Sunday")
  map <- list(
    MondayToFriday = wk,
    MondayToSaturday = c(wk, "Saturday"),
    MondayToSunday = all7,
    MondayToThursday = wk[1:4],
    Weekend = c("Saturday", "Sunday"),
    NotMonday = setdiff(all7, "Monday"),
    NotTuesday = setdiff(all7, "Tuesday"),
    NotWednesday = setdiff(all7, "Wednesday"),
    NotThursday = setdiff(all7, "Thursday"),
    NotFriday = setdiff(all7, "Friday"),
    NotSaturday = setdiff(all7, "Saturday"),
    NotSunday = setdiff(all7, "Sunday")
  )
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  out <- unlist(lapply(x, function(d) {
    if (!is.null(map[[d]])) map[[d]] else d
  }), use.names = FALSE)
  sort(unique(out))
}


#' Does this TransXChange Mode run on fixed track?
#'
#' Fixed track here means anything that is not a road vehicle: a named line on
#' rails or water is unique to its operator in a way a bus route number is not.
#' A missing Mode is bus, which is the TransXChange schema default.
#'
#' @param mode character vector of TransXChange Mode values
#' @return a logical vector
#' @noRd
fixed_track_mode <- function(mode) {
  rt <- vapply(mode, function(m) {
    suppressWarnings(as.numeric(clean_route_type(m, guess_bus = TRUE)))
  }, numeric(1), USE.NAMES = FALSE)
  !is.na(rt) & !rt %in% c(3, 11, 200)
}


#' Filter superseded TransXchange file versions
#'
#' Given a set of TransXchange XML files, returns the subset that represents
#' the operative timetable for each service, discarding superseded revisions
#' of the same service and closing the operating periods of registrations that
#' a later registration has replaced.
#'
#' @param files character vector of paths to TransXchange XML files
#' @param date Date, the reference date used to decide which file version is
#'   operative (default `Sys.Date()`). For historical analysis set this to a
#'   date within the period you are studying.
#' @param ncores numeric, number of cores used to read the file headers
#'   (default 1)
#' @param quiet logical, if FALSE a summary of removed files is printed
#' @param resolve_overlaps logical, if TRUE (default) registrations of the same
#'   service whose operating periods overlap are reconciled - see Details.
#' @param out_dir character, directory in which to write the rewritten copies
#'   of files whose operating period was truncated. Defaults to a new
#'   session-temporary directory. The originals are never modified.
#' @return a character vector, the subset of `files` to convert. Where an
#'   operating period was truncated the returned path points at a rewritten
#'   copy in `out_dir` rather than at the original file. Attribute
#'   `"filter_report"` holds one row per input file saying what was done with
#'   it: `decision` ("kept", "dropped", "truncated", or "added" for the
#'   remaining days of a split file), the `rule` (1, 2 or 4 below) and
#'   `reason`, the file it gave way to (`by`), and any new operating period.
#'   [transxchange2gtfs()] carries it into its [conversion_report()].
#'
#' @details
#' Archives of TransXchange data (such as the Bus Open Data Service change
#' archive) often contain several versions of the same registered service:
#' each time an operator updates a timetable a new file is uploaded for the
#' same `ServiceCode`, but the superseded files remain in the archive and
#' usually still declare an open-ended `OperatingPeriod`. If all versions are
#' converted, the same physical bus journey appears once per file version, so
#' counting trips on a given date over-estimates service levels.
#'
#' Every field used here is read from the contents of each file. Nothing is
#' inferred from file names: publishers prefix them inconsistently
#' (`tfl_`, `cen_`, `swe_`, `cambs_`) and in some regions the name does not
#' contain the `ServiceCode` at all.
#'
#' The function reads the header information of each file (`ServiceCode`,
#' `LineName`, `Description`, `NationalOperatorCode`, `OperatingPeriod` start
#' and end dates, `RevisionNumber`, `CreationDateTime` and
#' `ModificationDateTime`) and keeps, for each `ServiceCode` **published by one
#' operator**:
#'
#' A `ServiceCode` is unique within an operator, not nationally, so the
#' operator is part of the key. Five South East operators publish
#' `ServiceCode` \code{1}; keyed on the code alone they look like one service
#' with five competing revisions, and all but the most recently started are
#' discarded as superseded. The operator is taken as the whole set of
#' `NationalOperatorCode` values the file declares, sorted, so that a jointly
#' registered service is not split by the order its file happens to list them
#' in. Where no operator code can be read the code alone is used.
#'
#' \enumerate{
#'   \item For each distinct operating-period start date, **line** and **day
#'     type**, only the file with the highest `RevisionNumber` (ties broken by
#'     the most recent `ModificationDateTime`) - repeated uploads of the same
#'     timetable period are duplicates. A file is kept if it is the best
#'     available file for at least one of the lines it publishes. The line
#'     matters because a `ServiceCode` does not identify one timetable:
#'     operators such as Nottingham City Transport split a single registration
#'     into one file per line, all sharing the `ServiceCode` and operating
#'     period, and keying on the `ServiceCode` alone would discard every line
#'     but one. The day type matters for the same reason: Brighton & Hove
#'     files one document per day type, so a weekday, a Saturday and a Sunday
#'     file share the code, the line and the period and differ only in their
#'     `RegularDayType`. Day sets are read from `DaysOfWeek` and expanded, so
#'     that `MondayToFriday` and the five days listed separately compare
#'     equal.
#'   \item Of the start dates on or before `date`, only the most recent -
#'     this is the version operative on `date`; earlier versions have been
#'     superseded.
#'   \item All files whose operating period starts after `date` - these are
#'     future timetables that have not yet come into effect.
#' }
#'
#' @section Overlapping registrations:
#' Rules 1 to 3 key on the operator and `ServiceCode`, which catches a
#' re-upload of one registration but not a re-registration: some publishers,
#' Transport for London among them, mint a **new** `ServiceCode` every time a
#' service is re-registered. Each code then appears exactly once and nothing above
#' detects it, yet both files describe the same service over overlapping
#' dates and both convert into the feed.
#'
#' With `resolve_overlaps = TRUE` files are additionally grouped by
#' `NationalOperatorCode` + `Description` + the set of lines they publish -
#' the same registered service by any reading - and overlapping operating
#' periods within a group are reconciled.
#'
#' The `Description` is compared normalised rather than raw: case folded,
#' punctuation reduced to spaces and the words sorted, because publishers
#' retype it with each re-registration and it does not come back the same. On
#' fixed track - anything the `Mode` says is not a bus, coach or trolleybus -
#' it is left out of the key altogether, and the operator code and line name
#' identify the service on their own. Normalising cannot repair a misspelling,
#' and Transport for London's Central line files differ by exactly that: one
#' says "Ealing Broaddway" and another "West Ruilsip", which was enough to put
#' each in a group of its own where it had nothing to overlap with. A named
#' line on rails is unique to its operator in a way a bus route number is not,
#' so the description is not needed there; on the road it is kept, because one
#' operator really can run a route "1" in two towns.
#'
#' Two files in a group whose operating **days** do not intersect are left
#' alone, however their periods overlap: a weekday file and a Saturday file
#' cannot be alternative versions of one timetable. The test is disjointness,
#' not inequality, so a successor registration that merely drops Saturday
#' still shares the weekdays with the file it replaces and is reconciled as
#' usual. Without this, a publisher filing one document per day type had the
#' identical-period rule below keep the newest and call the rest duplicates.
#'
#' Within a group the overlaps are reconciled:
#'
#' \itemize{
#'   \item **Staggered starts.** Where a later registration runs to at least
#'     the end of an earlier one, the earlier one's `EndDate` is truncated to
#'     the day before the later one starts. This is the common case: the
#'     publisher issues the successor but leaves the predecessor open-ended.
#'   \item **Same start, different end.** The longer registration's
#'     `StartDate` is moved to the day after the shorter one ends, so the
#'     shorter, more specific period governs while it runs.
#'   \item **Identical periods.** Truncation cannot separate them, so the most
#'     recently created file is kept (`CreationDateTime`, falling back to
#'     `ModificationDateTime` then file mtime) and the others dropped.
#'   \item **One period wholly inside another.** Both are kept and reported.
#'     Closing the outer period would delete the service either side of the
#'     inner one, which a single `OperatingPeriod` cannot express.
#' }
#'
#' Truncation is preferred to deletion throughout: a journey on a date the
#' successor does not cover is never removed. A file whose period is emptied
#' by truncation is dropped. Because this works from declared identity and
#' declared validity rather than from journey times, it does not depend on two
#' timetables resembling each other, and cannot merge two services that merely
#' run at similar times.
#'
#' Resolving overlaps also removes the limitation that applied to rule 3 on
#' its own: a future timetable kept under that rule now truncates the
#' currently operative file at its start date, instead of both being counted
#' once the future timetable begins.
#'
#' Files whose `ServiceCode` cannot be read are always kept.
#'
#' @export
txc_filter_files <- function(files, date = Sys.Date(), ncores = 1, quiet = TRUE,
                             resolve_overlaps = TRUE, out_dir = NULL) {

  checkmate::assert_character(files, min.len = 1)
  checkmate::assert_logical(resolve_overlaps, len = 1)
  date <- as.Date(date)

  read_meta <- function(f) {
    meta <- try({
      xml <- xml2::read_xml(f)
      service <- xml2::xml_find_first(xml, "d1:Services/d1:Service")
      sc <- xml2::xml_text(xml2::xml_find_first(service, "d1:ServiceCode"))
      sd <- xml2::xml_text(xml2::xml_find_first(service, "d1:OperatingPeriod/d1:StartDate"))
      ed <- xml2::xml_text(xml2::xml_find_first(service, "d1:OperatingPeriod/d1:EndDate"))
      desc <- xml2::xml_text(xml2::xml_find_first(service, "d1:Description"))
      mode <- xml2::xml_text(xml2::xml_find_first(service, "d1:Mode"))
      nocs <- trimws(xml2::xml_text(xml2::xml_find_all(xml, "//d1:NationalOperatorCode")))
      nocs <- sort(unique(nocs[!is.na(nocs) & nzchar(nocs)]))
      noc <- if (length(nocs)) nocs[1] else NA_character_
      rev <- xml2::xml_attr(service, "RevisionNumber")
      if (is.na(rev)) rev <- xml2::xml_attr(xml, "RevisionNumber")
      mod <- xml2::xml_attr(service, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "CreationDateTime")
      cre <- xml2::xml_attr(xml, "CreationDateTime")
      if (is.na(cre)) cre <- mod
      # Which lines this file publishes. Needed because a ServiceCode does not
      # identify a timetable on its own - see the note on rule 1 below.
      # In upper case: Kempsford's July 2026 file for route K2 names the line
      # "k2" and its two later editions "K2", so the editions shared no line,
      # were never compared, and the July timetable ran beside the current
      # one (38 journeys a day for 19).
      lns <- xml2::xml_text(xml2::xml_find_all(xml, "//d1:LineName"))
      lns <- sort(unique(toupper(trimws(lns[!is.na(lns)]))))
      # Which days of the week this file's journeys run. Needed for the same
      # reason as the lines: a ServiceCode, a line and an operating period
      # still do not identify a timetable, because some publishers file one
      # document per day type and the documents differ only here.
      dys <- xml2::xml_name(xml2::xml_children(
        xml2::xml_find_all(xml, "//d1:RegularDayType/d1:DaysOfWeek")))
      dys <- expand_days_of_week(dys)
      # a holidays-only file runs on none of them: say so, rather than leave
      # its day set empty, which other rules read as unknown
      if (length(xml2::xml_find_all(xml, "//d1:RegularDayType/d1:HolidaysOnly"))) {
        dys <- sort(unique(c(dys, "HolidaysOnly")))
      }
      # Term and holiday versions of one timetable are told apart by their
      # ServicedOrganisationDayType, not by DaysOfWeek: Stirling's line UL is
      # one file running on the university's working days and one running on
      # every other day, sharing code, line, period and weekdays. Without
      # this rule 1 took them for a repeated upload and deleted the summer
      # timetable - 1,182 journeys in the July 2026 window. Recorded as which
      # side of the organisation's calendar the file runs on.
      #
      # And which calendar. Bee Network files route 143 from 30 August 2026
      # as one weekday file per combination of school and university terms
      # (organisations SchOnUniOn, SchOffUniOn, SchOnUniOff, SchOffUniOff),
      # every one on DaysOfOperation/WorkingDays. Recorded by side alone they
      # looked like four uploads of one file and three were deleted, leaving
      # the route running on one calendar's days (5,270 trips in the
      # November 2026 window down to 1,566). The calendar is identified by
      # its dates, not its OrganisationCode, which is local to the file: a
      # re-export that renumbers the code is still the same calendar.
      #
      # Only when the whole file is bound to the calendar, though. First
      # Norfolk & Suffolk publishes each edition of a route as one file: the
      # whole weekday and Saturday timetable on DaysOfWeek alone, plus a few
      # extra journeys bound to the school calendar. Route 64's edition from
      # 25 October 2026 had 39 such plain journeys and 4 on school holidays;
      # its successor from 1 November had the same 39 and some on school
      # days. Recorded from the extras, the first read as a holiday file and
      # the second as a term file, opposite sides of the calendar, so neither
      # replaced the other and both ran: every Ipswich journey of routes 63,
      # 64 and 65 twice. A file with any journey that runs without the
      # calendar is a whole timetable, not one side of one.
      #
      # Unless the journey is itself confined to a few dates. morebus files
      # Bournemouth University's U3 as a term file (journeys off on the
      # university's holidays) and a holiday file (journeys on them) from
      # 2 November 2026, same line, period and weekdays, different codes;
      # the holiday file also carries 112 Christmas journeys, each on
      # MondayToSunday with DaysOfNonOperation covering every date but one,
      # and the term file three that run only in a few weeks. Taken for
      # whole timetables they cost both files their calendar tokens, and
      # the identical-period tie-break deleted the holiday file (174
      # journeys). A journey excluded from most of the file's period is
      # dated extras, as much one side of the year as a bound journey.
      so <- xml2::xml_find_all(
        xml, "//d1:ServicedOrganisationDayType/*/*")
      if (length(so) && !txc_all_journeys_bound(xml, sd, ed)) so <- so[0]
      if (length(so)) {
        dys <- c(dys, unique(paste0("SO:", xml2::xml_name(xml2::xml_parent(so)),
                                    ":", xml2::xml_name(so), ":",
                                    so_calendar_id(xml, so))))
        dys <- sort(dys)
      }
      # The stops the file serves, for recognising the same route under a new
      # operator (see txc_overlap_plan()); read here because the file is
      # already parsed.
      stp <- xml2::xml_text(xml2::xml_find_all(
        xml, "//d1:StopPoints/d1:AnnotatedStopPointRef/d1:StopPointRef"))
      stp <- sort(unique(trimws(stp[!is.na(stp)])))
      # And the stops its journeys call at. The StopPoints list can be far
      # longer: Hodgsons' 2024 exports of routes 60 and 79 declare 28 and 160
      # stops, every stop the operator served, for routes calling at 8 and
      # 50, so two editions of one route looked like different routes.
      used <- xml2::xml_text(xml2::xml_find_all(
        xml, "//d1:JourneyPatternSection/d1:JourneyPatternTimingLink/*/d1:StopPointRef"))
      used <- sort(unique(trimws(used[!is.na(used)])))
      # The dates the whole service does not run, as "start/end" ranges. A
      # publisher can tell week variants of one timetable apart by these
      # alone: Metrobus files route 23 from 17 September 2026 as one weekday
      # file that runs only the week of 19 October and another that runs
      # every week but that one and half term, sharing code, line, period,
      # weekdays and nearly every journey (see txc_dates_shared()).
      nonop <- xml2::xml_find_all(service,
        "d1:OperatingProfile/d1:SpecialDaysOperation/d1:DaysOfNonOperation/d1:DateRange")
      nonop <- paste(xml2::xml_text(xml2::xml_find_first(nonop, "d1:StartDate")),
                     xml2::xml_text(xml2::xml_find_first(nonop, "d1:EndDate")), sep = "/")
      data.frame(file = f, ServiceCode = sc, StartDate = sd, EndDate = ed,
                 Description = desc, Mode = mode, NOC = noc,
                 NOCs = paste(nocs, collapse = "\r"),
                 RevisionNumber = rev, ModificationDateTime = mod,
                 CreationDateTime = cre,
                 Lines = paste(lns, collapse = "\r"),
                 Days = paste(dys, collapse = "\r"),
                 Stops = paste(stp, collapse = "\r"),
                 UsedStops = paste(used, collapse = "\r"),
                 NonOp = paste(nonop, collapse = "\r"),
                 Journeys = length(xml2::xml_find_all(
                   xml, "//d1:VehicleJourneys/d1:VehicleJourney")),
                 stringsAsFactors = FALSE)
    }, silent = TRUE)

    if (inherits(meta, "try-error")) {
      meta <- data.frame(file = f, ServiceCode = NA_character_,
                         StartDate = NA_character_,
                         EndDate = NA_character_,
                         Description = NA_character_,
                         Mode = NA_character_,
                         NOC = NA_character_,
                         NOCs = NA_character_,
                         RevisionNumber = NA_character_,
                         ModificationDateTime = NA_character_,
                         CreationDateTime = NA_character_,
                         Lines = NA_character_,
                         Days = NA_character_,
                         Stops = NA_character_,
                         UsedStops = NA_character_,
                         NonOp = NA_character_,
                         Journeys = NA_integer_,
                         stringsAsFactors = FALSE)
    }
    meta
  }

  if (ncores > 1) {
    oldplan <- future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(oldplan), add = TRUE)
    meta <- furrr::future_map(files, read_meta)
  } else {
    meta <- purrr::map(files, read_meta)
  }
  meta <- dplyr::bind_rows(meta)

  # clean up the metadata, filling in unusable values
  meta$StartDate <- as.Date(meta$StartDate, optional = TRUE)
  meta$StartDate[is.na(meta$StartDate)] <- as.Date("1900-01-01")
  # An absent EndDate is an open-ended registration, which is exactly what lets
  # a superseded file go on being counted. Treat it as far-future so that the
  # overlap rules see it, rather than as missing.
  meta$EndDate <- as.Date(meta$EndDate, optional = TRUE)
  meta$open_ended <- is.na(meta$EndDate)
  meta$EndDate[meta$open_ended] <- as.Date("2099-12-31")
  meta$RevisionNumber <- suppressWarnings(as.numeric(meta$RevisionNumber))
  meta$RevisionNumber[is.na(meta$RevisionNumber)] <- -1
  meta$ModificationDateTime <- suppressWarnings(
    lubridate::ymd_hms(meta$ModificationDateTime, quiet = TRUE))
  no_mod <- is.na(meta$ModificationDateTime)
  meta$ModificationDateTime[no_mod] <- file.mtime(meta$file[no_mod])
  meta$CreationDateTime <- suppressWarnings(
    lubridate::ymd_hms(meta$CreationDateTime, quiet = TRUE))
  no_cre <- is.na(meta$CreationDateTime)
  meta$CreationDateTime[no_cre] <- meta$ModificationDateTime[no_cre]

  # always keep files whose ServiceCode could not be read
  unknown <- meta$file[is.na(meta$ServiceCode)]
  meta <- meta[!is.na(meta$ServiceCode), ]
  # for the report: every file, and why each one that goes went
  meta0 <- meta
  r1 <- data.frame(file = character(0), by = character(0))
  r2 <- new.env()
  keep_r23 <- character(0)
  out_path <- character(0)
  plan <- NULL

  # A ServiceCode is unique within an operator, not nationally, so rules 1 to 3
  # key on the operator as well. Five South East operators publish ServiceCode
  # "1" (RRTR, GOCH, HAMS, HDBC, METR); keyed on the code alone they looked like
  # one service with five competing revisions, and the most recent start date
  # deleted the other four. Metrobus lost lines 1, 2, 10, 21 and 100 that way -
  # 876 of 2,892 journeys, five whole Crawley town routes. Measured over the
  # 2026-07-26 TNDS archive, keying on the operator recovers 6,958 journeys in
  # 195 files - 0.45% of the 1,532,854 kept nationally, 3.3% of the South East,
  # where the publisher uses bare route numbers as codes, and nothing at all in
  # the seven regions that publish codes like 33-74-S-y11-46. Nothing that the
  # ServiceCode key kept is dropped: a finer key can only split a group.
  #
  # The key is the whole set of operator codes, sorted, not the first one
  # listed: a jointly registered service names its operators in whatever order
  # the file happens to use, and a revision that reorders them is still the
  # same registration. Where a revision instead adds or drops an operator the
  # set does change and the two files group apart, which leaves a superseded
  # file in place rather than deleting a live one - the safe direction, and
  # rule 4 catches it on description and lines. Rule 4 deliberately keeps using
  # the first code alone, because there being lenient costs a surviving
  # duplicate while here it would cost real service.
  #
  # A file whose operator cannot be read keys on the ServiceCode alone, which
  # is the behaviour this replaces.
  meta$svc_key <- paste(ifelse(is.na(meta$NOCs), "", meta$NOCs),
                        meta$ServiceCode, sep = "\r")

  keep <- character()
  if (nrow(meta) > 0) {
    # rule 1: within each operator + ServiceCode + StartDate keep the highest
    # revision, breaking ties on the most recent ModificationDateTime (both
    # descending)
    meta <- meta[order(meta$svc_key, meta$StartDate,
                       -meta$RevisionNumber,
                       -as.numeric(meta$ModificationDateTime)), ]

    # Applied per line, not per file. A ServiceCode does not identify one
    # timetable: some operators split a single registration into one file per
    # line, all sharing the ServiceCode and the operating period. Nottingham
    # City Transport files ServiceCode NCT49 as four files, for lines 49, 49A,
    # 49B and 49X, and NCT40_41 as files for 40, 40A, 40B and 41. Deduplicating
    # on ServiceCode + StartDate alone kept one of them and silently deleted the
    # others - 114 live files in one TNDS region, including line 41 with 343
    # journeys and line 27 with 296.
    #
    # A file is kept if it is the best available file for at least one of its
    # lines. That still removes a repeated upload of the same period (identical
    # lines, so a later revision wins them all) and still removes a superseded
    # revision whose lines are all covered by a newer file, while keeping every
    # line that is only published once.
    #
    # The day type is part of the key for the same reason the line is. Some
    # publishers file one document per day type for a single registration, all
    # sharing the ServiceCode, the line and the operating period and differing
    # only in their RegularDayType. Brighton & Hove publishes ServiceCode
    # PK0001213:1 line 2 for the week of 2026-10-04 as three files - weekdays,
    # Saturday and Sunday - at the same RevisionNumber. Keyed without the day
    # type they collided and two of the three were deleted: across that
    # operator's files in a fortnight, 263 of the 285 rule 1 removed carried a
    # different day set from the file that displaced them, leaving 8 of 36
    # services with no Saturday or Sunday timetable at all.
    #
    # Day sets are compared for equality, not for overlap, so two files whose
    # days partly coincide (weekdays against Monday-to-Saturday) now both
    # survive and the shared days are published twice. That is the safe
    # direction - a surviving duplicate rather than deleted service, the same
    # trade made when the operator was added to svc_key - and what remains is
    # left to gtfs_deduplicate().
    lines_list <- strsplit(meta$Lines, "\r", fixed = TRUE)
    # a file naming no line at all still has to be groupable
    lines_list[lengths(lines_list) == 0] <- ""
    idx <- rep(seq_len(nrow(meta)), lengths(lines_list))
    ex <- data.frame(row = idx,
                     svc_key = meta$svc_key[idx],
                     StartDate = meta$StartDate[idx],
                     Days = ifelse(is.na(meta$Days[idx]), "",
                                   meta$Days[idx]),
                     Line = unlist(lines_list),
                     stringsAsFactors = FALSE)
    grp <- paste(ex$svc_key, ex$StartDate, ex$Days, ex$Line, sep = "\r")
    dup <- duplicated(grp)
    # Siblings, not repeated uploads. Some exports write one timetable as
    # several files that share every field the key uses: Stirling's University
    # line UL is two files, ServiceCode MBLB_UL, same line, period and days,
    # created 41 ms apart, carrying 247 and 434 different journeys. A repeated
    # upload repeats the journeys; a sibling does not. So a duplicate is only
    # dropped where it shares most of its journeys with the file it defers to.
    if (any(dup)) {
      jc <- list()
      jk <- function(f) {
        if (is.null(jc[[f]])) jc[[f]] <<- txc_journey_keys(f)
        jc[[f]]
      }
      winner <- ex$row[match(grp, grp)]
      for (k in which(dup & winner != ex$row)) {
        v <- txc_versions(jk(meta$file[ex$row[k]]), jk(meta$file[winner[k]]))
        if (isFALSE(v)) dup[k] <- FALSE
        # nor where they share few of their dates (Metrobus route 23)
        if (dup[k]) {
          sh <- txc_dates_shared(meta[c(ex$row[k], winner[k]), ])
          if (!is.na(sh) && sh < 0.5) dup[k] <- FALSE
        }
      }
    }
    win_row <- ex$row[match(grp, grp)]
    gone <- setdiff(seq_len(nrow(meta)), unique(ex$row[!dup]))
    if (length(gone)) {
      r1 <- data.frame(file = meta$file[gone],
                       by = meta$file[win_row[match(gone, ex$row)]])
    }
    ex <- ex[!dup, ]
    meta <- meta[sort(unique(ex$row)), ]

    # rules 2 and 3: keep the version operative on `date` plus future versions
    #
    # Per line and day set, like rule 1, not per ServiceCode. Arriva Cymru
    # registers lines 12 and X12 under one ServiceCode "12/X12" in two files
    # whose periods start a day apart (7 and 8 June 2026); keyed on the code
    # alone the X12 file, 16 journeys, was "the latest version" and the line
    # 12 file, 453 journeys, was deleted as superseded. In the July 2026 TNDS
    # archive that removed 24 files, 10 of them whole lines with nothing in
    # their place. A file is kept where it is the operative version of at
    # least one of its lines.
    lines_list <- strsplit(meta$Lines, "\r", fixed = TRUE)
    lines_list[lengths(lines_list) == 0] <- ""
    idx <- rep(seq_len(nrow(meta)), lengths(lines_list))
    # Keyed on the weekdays only. With the ServicedOrganisation side in the
    # key too, a successor that adds term-time journeys could never supersede
    # the file before it: First Eastern Counties' X2/X21/X22 (PF0000323:202)
    # from 25 October 2026, no term days, and its successor from 1 November,
    # with them, both stayed and every journey ran twice. Term and holiday
    # siblings are told apart below instead, as rule 4 does it: both files
    # name an organisation calendar, on opposite sides.
    split_days <- function(d) {
      d <- strsplit(ifelse(is.na(d), "", d), "\r", fixed = TRUE)
      list(wk = vapply(d, function(x) paste(x[!grepl("^SO:", x)], collapse = "\r"), ""),
           so = lapply(d, function(x) grep("^SO:", x, value = TRUE)))
    }
    sd <- split_days(meta$Days)
    so_of <- stats::setNames(sd$so, meta$file)
    ex2 <- data.frame(file = meta$file[idx],
                      StartDate = meta$StartDate[idx],
                      EndDate = meta$EndDate[idx],
                      key = paste(meta$svc_key[idx], sd$wk[idx],
                                  unlist(lines_list), sep = "\r"),
                      stringsAsFactors = FALSE)
    so_sibling <- function(f, o) {
      a <- so_side(so_of[[f]]); b <- so_side(so_of[[o]])
      length(a) > 0 && length(b) > 0 && !length(intersect(a, b))
    }
    #
    # And an earlier version is only superseded if the operative one repeats
    # its journeys. Arriva Cymru files route 35 as one file per depot (Rhyl,
    # 68 journeys from 25 January; Wrexham, 120 from 4 December), one
    # ServiceCode, line and day set, both open into 2027 and written in the
    # same export; "the latest start" kept Rhyl and deleted Wrexham's 120,
    # which the Rhyl file does not contain (share 0). A successor repeats most
    # of what it replaces, a sibling none of it - the same test rules 1 and 4
    # apply. A file whose journeys cannot be read is superseded as before.
    jc2 <- list()
    jk2 <- function(f) {
      if (is.null(jc2[[f]])) jc2[[f]] <<- txc_journey_keys(f)
      jc2[[f]]
    }
    #
    # And the operative version is one in force on `date`. trentbarton files
    # each route as an open-ended timetable (74: 1,555 journeys from 30
    # August 2026) and, under the same ServiceCode, a half-term week file
    # for 25-31 October with a subset of its journeys. On 11 November the
    # half-term file was "the latest version", the open-ended one was
    # deleted as superseded, and the network had no service: 157 lines in
    # the November 2026 archive. A file that has ended by `date` cannot be
    # what runs on it; only when nothing is in force does the latest past
    # file stand (harmless: the converter runs none of it).
    keep <- lapply(split(ex2, ex2$key), function(x) {
      past <- x$StartDate <= date
      operative <- character()
      if (any(past)) {
        live <- past & x$EndDate >= date
        cand <- if (any(live)) live else past
        newest <- x$file[cand & x$StartDate == max(x$StartDate[cand])]
        operative <- newest
        for (f in unique(x$file[past & !x$file %in% newest])) {
          v <- vapply(newest, function(o) {
            if (so_sibling(f, o)) return(FALSE)
            txc_versions(jk2(f), jk2(o))
          }, logical(1))
          if (all(!is.na(v) & !v)) operative <- c(operative, f)
        }
        for (f in setdiff(unique(x$file[past]), operative)) {
          assign(f, newest[1], envir = r2)
        }
      }
      c(operative, x$file[!past])
    })
    keep <- unique(unlist(keep, use.names = FALSE))
    keep_r23 <- keep
  }

  n_superseded <- length(files) - length(keep) - length(unknown)

  # rule 4: reconcile registrations of the same service whose periods overlap
  rewritten <- character()
  n_dropped_overlap <- 0L
  n_nested <- 0L
  n_handover <- 0L
  n_split <- 0L
  n_stale <- 0L
  if (resolve_overlaps && length(keep) > 0) {
    # The last day a later, newer edition of the same registration ran, where
    # that edition has ended by `date`. National Express West Midlands
    # published route 96 as an open-ended file from 30 August (revision 21)
    # and a last edition for 25-31 October (revision 22) before West
    # Midlands Bus took the route over on 1 November. Rules 2 and 3 rightly
    # set the ended edition aside (trentbarton's half-term weeks have the
    # same shape and the open-ended timetable resumes after them), but it is
    # the only sign that the outgoing operator stopped; the handover rule in
    # txc_overlap_plan() uses it where the new operator retimed the route.
    meta$ClosedEnd <- as.Date(NA)
    # set aside or not: NX's last Outer Circle edition runs every day, the
    # files it follows weekdays or weekends, so it is kept, alone in its key
    gone <- which(meta$EndDate < date)
    if (length(gone)) {
      ln <- strsplit(ifelse(is.na(meta$Lines), "", meta$Lines), "\r", fixed = TRUE)
      for (k in which(meta$file %in% keep)) {
        e <- gone[meta$svc_key[gone] == meta$svc_key[k] &
                    meta$StartDate[gone] > meta$StartDate[k] &
                    (meta$ModificationDateTime[gone] > meta$ModificationDateTime[k] |
                       meta$RevisionNumber[gone] > meta$RevisionNumber[k])]
        e <- e[vapply(e, function(x) length(intersect(ln[[x]], ln[[k]])) > 0, logical(1))]
        if (length(e)) meta$ClosedEnd[k] <- max(meta$EndDate[e])
      }
    }
    plan <- txc_overlap_plan(meta[meta$file %in% keep, ], date = date)
    n_nested <- attr(plan, "nested")
    n_handover <- attr(plan, "handover")
    if (is.null(n_handover)) n_handover <- 0L
    n_stale <- attr(plan, "stale")
    if (is.null(n_stale)) n_stale <- 0L
    base <- !plan$split
    drop <- plan$file[base & plan$drop]
    n_dropped_overlap <- length(drop)
    keep <- setdiff(keep, drop)

    changed <- plan[!plan$drop & plan$changed, ]
    if (nrow(changed) > 0) {
      if (is.null(out_dir)) out_dir <- tempfile("txc_truncated")
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      for (i in seq_len(nrow(changed))) {
        new_path <- txc_write_period(changed$file[i], changed$new_start[i],
                                     changed$new_end[i], out_dir,
                                     exclude = changed$exclude[i])
        if (changed$split[i]) {
          # the days a shorter-week successor leaves to the earlier file: an
          # extra file beside the truncated original
          if (!is.na(new_path)) {
            keep <- c(keep, new_path)
            n_split <- n_split + 1L
          }
        } else if (!is.na(new_path)) {
          out_path[changed$file[i]] <- new_path
          keep[keep == changed$file[i]] <- new_path
          rewritten <- c(rewritten, new_path)
        }
      }
    }
  }

  keep <- c(keep, unknown)
  # `keep` may now name rewritten copies, so it is the result in its own right
  result <- unique(keep)
  attr(result, "filter_report") <- txc_filter_report(
    meta0, unknown, r1, r2, keep_r23, plan, out_path, date)

  if (!quiet) {
    message(Sys.time(), " txc_filter_files: kept ", length(result), " of ",
            length(files), " files (", n_superseded,
            " superseded or duplicate versions removed",
            if (resolve_overlaps) paste0(", ", n_dropped_overlap,
                                         " duplicate registrations removed, ",
                                         length(rewritten),
                                         " operating periods truncated") else "",
            ")")
    if (resolve_overlaps && n_handover > 0) {
      message(Sys.time(), " txc_filter_files: ", n_handover,
              " file(s) closed where another operator took the route over")
    }
    if (resolve_overlaps && n_stale > 0) {
      message(Sys.time(), " txc_filter_files: ", n_stale,
              " open-ended file(s) more than a year old retired by a later version")
    }
    if (resolve_overlaps && n_split > 0) {
      message(Sys.time(), " txc_filter_files: ", n_split,
              " file(s) kept running on the days a successor does not cover")
    }
    if (resolve_overlaps && n_nested > 0) {
      message(Sys.time(), " txc_filter_files: ", n_nested,
              " overlapping registration(s) left in place - one period lies ",
              "wholly inside another and truncation cannot separate them")
    }
  }

  return(result)
}



#' Build txc_filter_files()'s per-file report
#'
#' Internal helper. One row per input file saying what the filter did with it
#' and why: kept, dropped (and by which rule, in favour of which file),
#' truncated (to which period), or added as the remaining days of a split.
#' @noRd
txc_filter_report <- function(meta0, unknown, r1, r2, keep_r23, plan,
                              out_path, date) {
  fmt_lines <- function(x) gsub("\r", " ", ifelse(is.na(x), "", x))
  rep <- data.frame(
    file = basename(meta0$file),
    service_code = meta0$ServiceCode,
    operator = fmt_lines(meta0$NOCs),
    lines = fmt_lines(meta0$Lines),
    start = meta0$StartDate,
    end = ifelse(meta0$open_ended, NA, as.character(meta0$EndDate)),
    journeys = if (is.null(meta0$Journeys)) NA_integer_ else meta0$Journeys,
    decision = "kept",
    rule = "",
    reason = "",
    by = "",
    new_start = as.Date(NA),
    new_end = as.Date(NA),
    path = meta0$file,
    stringsAsFactors = FALSE)
  rep$end <- as.Date(rep$end)

  set_row <- function(f, decision, rule, reason, by = "") {
    k <- match(f, rep$path)
    ok <- !is.na(k)
    rep$decision[k[ok]] <<- decision
    rep$rule[k[ok]] <<- rule
    rep$reason[k[ok]] <<- if (length(reason) > 1) reason[ok] else reason
    rep$by[k[ok]] <<- if (length(by) > 1) basename(by[ok]) else basename(by)
  }

  # rule 1: a repeated upload of the same period, line and day type
  if (nrow(r1)) {
    set_row(r1$file, "dropped", "1",
            "repeated upload: a higher revision of the same period, line and days",
            r1$by)
  }
  # rules 2 and 3: an earlier version than the one operative on `date`
  r1_files <- r1$file
  sup <- setdiff(meta0$file, c(keep_r23, r1_files))
  if (length(sup)) {
    by <- vapply(sup, function(f) {
      if (exists(f, envir = r2, inherits = FALSE)) get(f, envir = r2) else ""
    }, character(1))
    set_row(sup, "dropped", "2",
            paste0("superseded: an earlier version than the one operative on ",
                   format(as.Date(date))), by)
  }
  # rule 4: overlapping registrations
  if (!is.null(plan) && nrow(plan)) {
    base <- plan[!plan$split, , drop = FALSE]
    d <- base[base$drop, , drop = FALSE]
    if (nrow(d)) {
      why <- ifelse(is.na(d$reason), "overlap: emptied by truncation", d$reason)
      set_row(d$file, "dropped", "4", why, ifelse(is.na(d$by), "", d$by))
    }
    ch <- base[!base$drop & base$changed, , drop = FALSE]
    if (nrow(ch)) {
      set_row(ch$file, "truncated", "4",
              ifelse(is.na(ch$reason), "overlap: period shortened", ch$reason),
              ifelse(is.na(ch$by), "", ch$by))
      k <- match(ch$file, rep$path)
      rep$new_start[k] <- ch$new_start
      rep$new_end[k] <- ch$new_end
      hit <- ch$file %in% names(out_path)
      rep$path[k[hit]] <- out_path[ch$file[hit]]
    }
    sp <- plan[plan$split & !plan$drop, , drop = FALSE]
    if (nrow(sp)) {
      extra <- rep[match(sp$file, meta0$file), , drop = FALSE]
      extra$decision <- "added"
      extra$rule <- "4"
      extra$reason <- ifelse(is.na(sp$reason), "split", sp$reason)
      extra$by <- ifelse(is.na(sp$by), "", sp$by)
      extra$new_start <- sp$new_start
      extra$new_end <- sp$new_end
      rep <- rbind(rep, extra)
    }
  }
  if (length(unknown)) {
    rep <- rbind(rep, data.frame(
      file = basename(unknown), service_code = NA_character_, operator = "",
      lines = "", start = as.Date(NA), end = as.Date(NA),
      journeys = NA_integer_, decision = "kept", rule = "",
      reason = "ServiceCode unreadable: kept without checks", by = "",
      new_start = as.Date(NA), new_end = as.Date(NA), path = unknown,
      stringsAsFactors = FALSE))
  }
  rownames(rep) <- NULL
  rep
}

#' Whether two files are versions of one timetable rather than siblings
#'
#' Internal helper for [txc_filter_files()]. A successor registration repeats
#' most of the journeys of the file it replaces; a sibling - another part of
#' the same timetable, another depot's share of the route, another day type -
#' repeats few or none. Measured in TNDS: Arriva Cymru's depot files, First
#' Essex's 371 and Peter's Travel's weekday and Saturday 343 share none.
#'
#' Half the journeys is not quite enough on a frequent route, because a
#' re-timed successor drifts out of the three-minute match through the day:
#' Transport for London's 195, re-timed from every 21 to every 22 minutes,
#' shares 47%. What marks it as a version is that it keeps its number of
#' journeys (433 and 418). Siblings that overlap that much do not - Metrolink's
#' replacement-bus files share 42% with 439 journeys against 531, Compass's
#' two files for route 100 28% with 53 against 43 - so between a quarter and a
#' half of the journeys, files are versions only if their journey counts are
#' within a tenth of each other.
#'
#' @param a,b data.frames from txc_journey_keys()
#' @return TRUE, FALSE, or NA when either file's journeys cannot be read
#' @noRd
txc_versions <- function(a, b) {
  share <- txc_journey_share(a, b)
  if (is.na(share)) return(NA)
  if (share >= 0.5) return(TRUE)
  if (share < 0.25) return(FALSE)
  abs(nrow(a) - nrow(b)) <= 0.1 * max(nrow(a), nrow(b))
}

#' English weekday names whatever the locale
#' @noRd
#' Bit mask of English weekday names, Monday = 1 ... Sunday = 64
#' @noRd
day_mask <- function(d) {
  wk <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
          "Saturday", "Sunday")
  as.integer(sum(2^(match(unique(d[d %in% wk]), wk) - 1)))
}

weekdays_en <- function(d) {
  c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
    "Sunday")[as.integer(format(d, "%u"))]
}


#' Decide how overlapping registrations of one service should be reconciled
#'
#' Internal helper for [txc_filter_files()]. Takes the cleaned metadata frame
#' and returns one row per file with the operating period it should end up
#' with, and whether it should be dropped entirely.
#'
#' @param meta data.frame of file metadata
#' @param date the date the files are filtered for; NA skips the stale
#'   open-ended rule
#' @return data.frame with columns file, new_start, new_end, drop, changed and
#'   an attribute "nested" counting the pairs left unresolved
#' @noRd
txc_overlap_plan <- function(meta, date = NA) {

  # Days is read by txc_filter_files(); default it so this stays callable with
  # metadata built by hand, as the tests do.
  if (is.null(meta$Days)) meta$Days <- NA_character_

  out <- data.frame(file = meta$file,
                    new_start = meta$StartDate,
                    new_end = meta$EndDate,
                    drop = FALSE,
                    changed = FALSE,
                    # days of the week (mask, Monday = 1) whose journeys are
                    # removed from the file: non-zero only on the extra rows
                    # a day-split adds (see below), which name the file of
                    # the row they were split from
                    exclude = 0L,
                    split = FALSE,
                    # why the row was dropped or rewritten, and the file it
                    # gave way to, for txc_filter_files()'s report
                    reason = NA_character_,
                    by = NA_character_,
                    stringsAsFactors = FALSE)
  note <- function(k, reason, by) {
    out$reason[k] <<- reason
    out$by[k] <<- paste(basename(by), collapse = ";")
  }
  nested <- 0L
  split_pairs <- data.frame(split = character(0), by = character(0))

  # The same registered service by any reading: one operator, one description,
  # one set of lines. Description is what separates line 436 in London from
  # line 436 in Hereford; without it this key would be far too coarse.
  #
  # It is compared normalised, not raw. Publishers retype the description with
  # each re-registration and it does not come back the same: Transport for
  # London's Central line is "Ealing Broadway/West Ruislip - Liverpool Street -
  # Epping/Hainault/Woodford" in one file and lists the same places in another
  # order in the next.
  desc_key <- normalise_description(meta$Description)

  # Off the road the description is not needed at all, and normalising it is
  # not enough either - one transposed letter ("Broaddway", "Ruilsip") puts a
  # file in a group of its own where it has nothing to overlap with. An
  # operator running a named line on fixed track runs one of them: "Central"
  # under Transport for London's operator code is the Central line and nothing
  # else, however it is spelt that day. On the road the description is needed,
  # because one operator really can run a route "1" in two towns, so bus and
  # coach keep it.
  named_line <- fixed_track_mode(meta$Mode) & !is.na(meta$Lines) &
    nzchar(meta$Lines)
  desc_key[named_line] <- ""

  key <- paste(ifelse(is.na(meta$NOC), "", meta$NOC),
               desc_key,
               ifelse(is.na(meta$Lines), "", meta$Lines),
               sep = "\r")
  # A group needs something to identify it. Files that name neither a
  # description nor an operator are left alone rather than pooled together,
  # and a file grouped without its description needs the operator code to
  # stand in for it.
  usable <- ifelse(named_line,
                   !is.na(meta$NOC) & nzchar(meta$NOC),
                   (!is.na(meta$Description) & nzchar(meta$Description)) |
                     (!is.na(meta$NOC) & nzchar(meta$NOC)))
  groups <- split(seq_len(nrow(meta))[usable], key[usable])
  groups <- groups[lengths(groups) > 1]

  # One registration under every reading of it. The description and the line
  # set are retyped between editions and differ between the exports that
  # publish one service: London United's route 117 is "Hosptial - Hounslow -
  # Middlesex St..." from 2023 and "Ashford - Feltham - Hospital - Hounslow"
  # from December 2025; Dartline's own PR1 files name no description and the
  # second export of the same service does; First Berkshire's route 7 lists
  # lines 7 and 7X from 18 October 2026 and 7 alone from 1 November. Grouped
  # on description and lines, none of these pairs was ever compared, and each
  # ran twice. The operator and ServiceCode identify the registration, so
  # files sharing both are grouped as well (a file can sit in both kinds of
  # group). Per-day-type files of one registration are kept apart below by
  # the disjoint-days test, as before.
  sc_ok <- !is.na(meta$ServiceCode) & nzchar(meta$ServiceCode) &
    !is.na(meta$NOC) & nzchar(meta$NOC)
  code_key <- paste(meta$NOC, meta$ServiceCode, sep = "\r")
  code_groups <- split(seq_len(nrow(meta))[sc_ok], code_key[sc_ok])
  code_groups <- code_groups[lengths(code_groups) > 1]
  # only where they add a comparison the description groups do not make
  covered <- vapply(code_groups, function(cg) any(vapply(groups, function(g)
    all(cg %in% g), logical(1))), logical(1))
  groups <- c(groups, unname(code_groups[!covered]))
  # And files sharing operator, creation time and lines. TNDS numbers each
  # edition of a Stagecoach route as its own ServiceCode and retypes the
  # description: Stagecoach Merseyside's route 33 is NW_SC_SCMY_33_2
  # ("Maghull Station - Maghull Station", modified 11 August 2026) and
  # NW_SC_SCMY_33_1 ("Maghull - Maghull", 6 October), both open-ended, every
  # journey the same, and both ran (48 departures at the reference stop
  # against BODS's 24). The creation time is the registration's, kept
  # through every edition; it is shared by an operator's other routes, so
  # the lines go in the key too. Which pairs are siblings is decided below.
  cre_s <- if (is.null(meta$CreationDateTime)) rep(NA_character_, nrow(meta)) else
    as.character(meta$CreationDateTime)
  cre_ok <- !is.na(cre_s) & nzchar(cre_s) & !is.na(meta$NOC) & nzchar(meta$NOC) &
    !is.na(meta$Lines) & nzchar(meta$Lines)
  cre_key <- paste(meta$NOC, cre_s, meta$Lines, sep = "\r")
  cre_groups <- split(seq_len(nrow(meta))[cre_ok], cre_key[cre_ok])
  cre_groups <- cre_groups[lengths(cre_groups) > 1]
  covered <- vapply(cre_groups, function(cg) any(vapply(groups, function(g)
    all(cg %in% g), logical(1))), logical(1))
  groups <- c(groups, unname(cre_groups[!covered]))

  # Two files of one registration from different exports: a later
  # modification time or a different revision. Within one export, files
  # sharing a code are siblings (Stirling's UL, written 41 ms apart).
  mod_t <- if (inherits(meta$ModificationDateTime, "POSIXt")) meta$ModificationDateTime else
    suppressWarnings(as.POSIXct(sub("T", " ", substr(meta$ModificationDateTime, 1, 19)),
                                format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  # The modification time decides where both files have one: West Midlands
  # Bus numbers each file of one export separately (revisions 2 and 3 for the
  # "WA" and "WN" parts of route 4, written four minutes apart), and on the
  # revision test the two parts were taken for retimed versions and one was
  # dropped. The revision decides only where a time is missing or the two
  # times are identical (TfL's files share a placeholder, 2020-01-01).
  other_export <- function(i, j) {
    if (!is.na(mod_t[i]) && !is.na(mod_t[j]) && mod_t[i] != mod_t[j])
      return(abs(as.numeric(difftime(mod_t[i], mod_t[j], units = "hours"))) > 1)
    !is.na(meta$RevisionNumber[i]) && !is.na(meta$RevisionNumber[j]) &&
      meta$RevisionNumber[i] != meta$RevisionNumber[j]
  }
  same_code <- function(i, j) sc_ok[i] && sc_ok[j] && code_key[i] == code_key[j]
  jn_all <- if (is.null(meta$Journeys)) rep(NA_integer_, nrow(meta)) else meta$Journeys
  # A registration split into parts within each export. West Midlands Bus
  # publishes ServiceCode PD2093720:4 as two files per edition, "WA" (lines
  # 8 and 9, 147 journeys) and "WN" (line 9, 250), written minutes apart.
  # Across editions the 15 November WA passed the retimed test below against
  # the 1 November WN, and both WA files (the Inner Circle) were dropped. A
  # file with a sibling in its own export is a part, and a part is compared
  # as a retimed version only with a file publishing the same lines.
  has_sib <- logical(nrow(meta))
  for (cg in code_groups) for (k in cg) {
    has_sib[k] <- any(vapply(setdiff(cg, k), function(m) !other_export(k, m), logical(1)))
  }
  # Do two files run over the same stops (half of their union)? The stops
  # their journeys call at where known, else the stops they declare. Unknown
  # stops count as the same, so files without stop lists behave as before.
  stops_src <- if (!is.null(meta$UsedStops)) {
    ifelse(is.na(meta$UsedStops) | !nzchar(meta$UsedStops), meta$Stops, meta$UsedStops)
  } else meta$Stops
  stops_all <- if (is.null(stops_src)) vector("list", nrow(meta)) else
    strsplit(ifelse(is.na(stops_src), "", stops_src), "\r", fixed = TRUE)
  # Does one file start later and also come from a later export (a later
  # modification time, else a higher revision)? Then it is the next edition.
  st_d <- as.Date(meta$StartDate, optional = TRUE)
  st_d[!is.na(st_d) & st_d <= as.Date("1900-01-01")] <- NA
  rev_n <- suppressWarnings(as.numeric(meta$RevisionNumber))
  successive <- function(i, j) {
    if (is.na(st_d[i]) || is.na(st_d[j]) || st_d[i] == st_d[j]) return(FALSE)
    if (st_d[i] < st_d[j]) { a <- i; b <- j } else { a <- j; b <- i }
    if (!is.na(mod_t[a]) && !is.na(mod_t[b]) && mod_t[a] != mod_t[b])
      return(mod_t[b] > mod_t[a])
    !is.na(rev_n[a]) && !is.na(rev_n[b]) && rev_n[b] > rev_n[a]
  }
  # Two documents kept up side by side. First Essex's route 3 (Chelmsford -
  # Southend, registration PF0000599:376) is worked by two depots, each
  # publishing its own document for the whole route: Chelmsford's (created 7
  # September 2021, revisions 697 to 726, 20 journeys) and Hadleigh's (29
  # September 2021, revisions 413 to 420, 12), with interleaved times and
  # the same stops. Each November file passed the retimed test against the
  # other and the older document's was dropped - half the route. A
  # document that a new one replaces is not edited again once the new one
  # appears; a depot's document is kept up beside the other depot's. Two
  # files are parallel documents when each one's document (its creation
  # time, within the registration) has at least two editions in the group
  # and the two documents' editions were written over overlapping spans.
  cre_c <- as.character(meta$CreationDateTime)
  parallel_docs <- function(i, j) {
    if (is.na(cre_c[i]) || is.na(cre_c[j]) || cre_c[i] == cre_c[j]) return(FALSE)
    peers <- which(sc_ok & code_key == code_key[i] & !is.na(mod_t) & !is.na(cre_c))
    mi <- mod_t[peers[cre_c[peers] == cre_c[i]]]
    mj <- mod_t[peers[cre_c[peers] == cre_c[j]]]
    length(mi) >= 2 && length(mj) >= 2 && max(mi) > min(mj) && max(mj) > min(mi)
  }
  same_stops <- function(i, j) {
    a <- stops_all[[i]]; b <- stops_all[[j]]
    if (!length(a) || !length(b)) return(TRUE)
    length(intersect(a, b)) >= 0.5 * length(union(a, b))
  }

  # Stale open-ended versions. An open-ended registration used to be given a
  # year from its start at import, which quietly retired versions nobody had
  # withdrawn; the horizon is now longer (UK2GTFS_opt_openEndedDays) so that
  # a service published once, years ago, still runs. But the BODS archive
  # keeps every open-ended version: London United's route 117 is four files,
  # from 19 August 2023, 25 November 2023, 4 January 2025 and 13 December
  # 2025, none with an EndDate. Retimed between them, they share too few
  # journeys for the versions test, so nothing below closes the old ones,
  # and on the longer horizon the 2023 timetables ran beside the current
  # one - routes 117, 290 and 411 at twice their real frequency in the
  # November 2026 archive. An open-ended file that started more than a year
  # before `date` is retired where a later version of the same service, on
  # a shared weekday and the same side of any organisation calendar, exists:
  # what the one-year horizon did, kept for the files that have a successor.
  # The successor must also run over the same stops (half of their union,
  # as for an operator handover): Centrebus files a Lincolnshire route 24
  # (open-ended from September 2024) and a Leicestershire 24 (from February
  # 2026) under one NOC and line with not a stop in common, and the second
  # is no version of the first.
  #
  # The successor must also be in force on `date`: Lynx files its route 42
  # as one open-ended timetable from April 2025 and, for the Christmas
  # week, a file for 29-31 December 2026, and a file for three days in
  # December is not what runs in November. And it must be of comparable
  # size, at least half the journeys of the file it retires: Hodgsons' 87
  # is a 26-journey weekday file from 18 November 2024 and a 6-journey file
  # from two days later, a supplement that runs beside the first, not a
  # version of it (so for Red Eagle's W20, 15 journeys against 2).
  n_stale <- 0L
  if (!is.na(date) && !is.null(meta$open_ended)) {
    dsets <- strsplit(ifelse(is.na(meta$Days), "", meta$Days), "\r", fixed = TRUE)
    wk_of <- lapply(dsets, function(d) d[nzchar(d) & !grepl("^SO:", d)])
    sos_of <- lapply(dsets, function(d) so_side(grep("^SO:", d, value = TRUE)))
    stp_of <- if (is.null(meta$Stops)) vector("list", nrow(meta)) else
      strsplit(ifelse(is.na(meta$Stops), "", meta$Stops), "\r", fixed = TRUE)
    jn <- if (is.null(meta$Journeys)) rep(NA_integer_, nrow(meta)) else meta$Journeys
    for (g in groups) {
      for (i in g) {
        if (!isTRUE(meta$open_ended[i]) || out$drop[i] || is.na(jn[i]) ||
            meta$StartDate[i] >= as.Date(date) - 365) next
        later <- g[meta$StartDate[g] > meta$StartDate[i] & !out$drop[g] &
                     meta$StartDate[g] <= as.Date(date) &
                     meta$EndDate[g] >= as.Date(date) &
                     !is.na(jn[g]) & jn[g] >= 0.5 * jn[i]]
        later <- later[vapply(later, function(j) {
          # from a later export: Hodgsons writes route 87's Saturday morning
          # file and its evening file in one export, and the evening file
          # is no successor to the morning one
          other_export(i, j) &&
          length(intersect(strsplit(meta$Lines[i], "\r", fixed = TRUE)[[1]],
                           strsplit(meta$Lines[j], "\r", fixed = TRUE)[[1]])) > 0 &&
          (!length(wk_of[[i]]) || !length(wk_of[[j]]) ||
             length(intersect(wk_of[[i]], wk_of[[j]])) > 0) &&
            !(length(sos_of[[i]]) && length(sos_of[[j]]) &&
                !length(intersect(sos_of[[i]], sos_of[[j]]))) &&
            length(stp_of[[i]]) > 0 && length(stp_of[[j]]) > 0 &&
            length(intersect(stp_of[[i]], stp_of[[j]])) >=
              0.5 * length(union(stp_of[[i]], stp_of[[j]]))
        }, logical(1))]
        if (length(later)) {
          out$drop[i] <- TRUE
          note(i, "stale: open-ended file over a year old, retired by a later version",
               meta$file[later[1]])
          n_stale <- n_stale + 1L
        }
      }
    }
  }

  # Journeys are read only for files in an overlapping pair, once each.
  journey_cache <- list()
  journeys <- function(k) {
    f <- meta$file[k]
    if (is.null(journey_cache[[f]])) journey_cache[[f]] <<- txc_journey_keys(f)
    journey_cache[[f]]
  }

  for (g in groups) {
    # Work in start-date order so that "the later registration" is always the
    # one further down the group.
    g <- g[order(out$new_start[g], out$new_end[g])]
    repeat {
      live <- g[!out$drop[g]]
      if (length(live) < 2) break
      acted <- FALSE
      for (a in seq_len(length(live) - 1)) {
        for (b in seq(a + 1, length(live))) {
          i <- live[a]; j <- live[b]
          if (out$drop[i] || out$drop[j]) next
          si <- out$new_start[i]; ei <- out$new_end[i]
          sj <- out$new_start[j]; ej <- out$new_end[j]
          if (max(si, sj) > min(ei, ej)) next          # no overlap
          # A ServiceCode can cover more than one route: Arriva Cymru's
          # "12/X12" is line 12 in one file and line X12 in another. Files
          # grouped by code must publish a line in common.
          if (!is.na(meta$Lines[i]) && !is.na(meta$Lines[j]) &&
              !length(intersect(strsplit(meta$Lines[i], "\r", fixed = TRUE)[[1]],
                                strsplit(meta$Lines[j], "\r", fixed = TRUE)[[1]]))) next

          # Siblings, not registrations. Some publishers split one timetable
          # across several files with their own ServiceCodes, all written in
          # the same export: First Essex publishes route X30 as
          # SE_FG_FESX_X30_1 and X30_2_A to X30_4_A, one operator, one
          # description, one line, one period and one CreationDateTime. They
          # carry different journeys, and the rules below kept only one of
          # them (126 of the 301 vehicle journeys). A successor registration
          # is written later than the file it replaces, so equal creation
          # times with different codes are left alone.
          #
          # But the creation time is the registration's, not the export's.
          # TNDS gives each of Stagecoach Midlands' editions of route 85 its
          # own code (WM_SC_SCNH_85_1, _4_A, _3_A), all created 8 December
          # 2021 and modified on 1 September, 12 October and 4 November 2026:
          # three open-ended editions of one timetable, every journey shared,
          # and all three ran (63 departures at Coventry against BODS's 21).
          # Equal creation times mean siblings only within one export, or
          # where the files do not share their journeys.
          if (!is.na(meta$CreationDateTime[i]) &&
              !is.na(meta$CreationDateTime[j]) &&
              meta$CreationDateTime[i] == meta$CreationDateTime[j] &&
              meta$ServiceCode[i] != meta$ServiceCode[j] &&
              (!other_export(i, j) || !isTRUE(txc_versions(journeys(i), journeys(j))))) next

          # Siblings written in different exports. The same publisher splits
          # route 371 into SE_FG_FESX_371_1 (one journey a day) and
          # SE_FG_FESX_371_4 (the other 47), exported a week apart, so the
          # test above does not see them; "same start, different end" then
          # moved the main file's start past the one-journey file's end and
          # deleted a week of the route. A successor registration repeats most
          # of the journeys of the file it replaces, a sibling repeats almost
          # none, so the journeys decide. That holds for a shared ServiceCode
          # too: two files with one code reach this point only because rules
          # 1 and 2 kept both, and rule 1 keeps exactly the files whose
          # journeys differ. Only a mismatch stops the reconciliation: a file
          # without readable journeys is treated as before.
          #
          # Except a retimed edition of one registration. Kempsford's K2 is
          # revision 2 (12 August 2026) and revision 3 (17 September), both
          # from 27 July, 19 journeys each, every time changed; London
          # United's 117 shares 3% of its journeys with the edition it
          # replaced. Same operator and ServiceCode, a different export and
          # a comparable number of journeys (the smaller at least half the
          # larger, so a supplement - Hodgsons' 6-journey 87 beside its
          # 26-journey file - is still not taken for a version) make them
          # versions whatever their times.
          #
          # Over the same stops, though. Yorkshire Coastliner's 31X is two
          # files, Easingwold - Kirkbymoorside (22 journeys, modified 14
          # October 2025) and Easingwold - York (15, the next day), with one
          # stop of 88 in common; a day apart, each looked like the other's
          # retimed edition and Kirkbymoorside was deleted. A new edition of
          # a timetable still serves its route.
          #
          # And a file starting later that is also the newer is from another
          # export, however soon after it was written. First Somerset files
          # route 4 as weekly editions of one registration: Christmas week
          # from 20 December (20 journeys, revision 859) and New Year from
          # 27 December (33, revision 860) were written five minutes apart,
          # taken for parts of one export, and both ran from 27 December.
          # Parts of one export start together (West Midlands Bus's WA and
          # WN), and the parts rule still applies.
          if (isFALSE(txc_versions(journeys(i), journeys(j)))) {
            retimed <- same_code(i, j) && (other_export(i, j) || successive(i, j)) &&
              !is.na(jn_all[i]) && !is.na(jn_all[j]) && max(jn_all[i], jn_all[j]) > 0 &&
              min(jn_all[i], jn_all[j]) >= 0.5 * max(jn_all[i], jn_all[j]) &&
              (!(has_sib[i] || has_sib[j]) || identical(meta$Lines[i], meta$Lines[j])) &&
              same_stops(i, j) && !parallel_docs(i, j)
            if (!retimed) next
          }

          # Different days, not competing registrations. A publisher filing one
          # document per day type gives this group a weekday file, a Saturday
          # file and a Sunday file sharing an operating period exactly, and the
          # identical-period branch below would keep the newest and call the
          # other two duplicates. Two files whose operating days do not
          # intersect cannot be alternative versions of one timetable, whatever
          # their periods say.
          #
          # Disjointness, not inequality: a successor registration that drops
          # Saturday still shares the weekdays with the file it replaces, so it
          # overlaps here and is reconciled as before. Only genuinely
          # complementary files are left alone.
          if (!is.na(meta$Days[i]) && !is.na(meta$Days[j]) &&
              nzchar(meta$Days[i]) && nzchar(meta$Days[j])) {
            di <- strsplit(meta$Days[i], "\r", fixed = TRUE)[[1]]
            dj <- strsplit(meta$Days[j], "\r", fixed = TRUE)[[1]]
            if (!length(intersect(di, dj))) next
            # Term and outside-term versions share their weekdays but run on
            # opposite sides of a ServicedOrganisation calendar: disjoint
            # dates, so not competing registrations either.
            # By side only: a successor with next year's term dates is on
            # the same side of a different calendar, and still a successor.
            si_so <- so_side(grep("^SO:", di, value = TRUE))
            sj_so <- so_side(grep("^SO:", dj, value = TRUE))
            if (length(si_so) && length(sj_so) &&
                !length(intersect(si_so, sj_so))) next
            # Week variants told apart by excluded dates (Metrobus route 23).
            # Metrobus's 3-journey school file has its journeys in the
            # one-week file and runs on every school day; the one-week file
            # runs on five of them. A file that runs on a minority of the
            # other's dates is a variant beside it, not a version of it.
            sh <- txc_dates_shared(meta[c(i, j), ])
            if (!is.na(sh) && sh < 0.5) next
          }

          if (si == sj && ei == ej) {
            # Identical periods: nothing to truncate, so the most recently
            # created file wins and the other is a duplicate registration -
            # unless only one of the two is bound to a ServicedOrganisation
            # calendar. Metrobus files routes 100 and 200 from 5 September
            # 2026 as two weekday files with the same journeys, period,
            # revision and creation time: the holiday version runs on days
            # that are not school WorkingDays, the term version names no
            # organisation and removes the half terms as special days. The
            # SO test above needs a calendar on both sides, so the pair came
            # here and the tie-break kept the holiday file - no weekday
            # Metrobus in term time. A file restricted to one side of a
            # calendar is not a repeated upload of one that is not, nor of
            # one restricted to a different calendar (Bee Network 143's four
            # term combinations, see so_calendar_id()). But the guard must
            # not shield every pair whose calendar tokens differ: TfL's H13
            # files 61368 and 61369 (July 2026) hold the same 280 journeys,
            # one encoded with both sides of the school calendar and the
            # other with term days only, and 364's 61731 is a term-time
            # re-encoding of the plain 61732 - both are versions, and the
            # journey-share test below tells them apart from siblings. So
            # the pair is left alone only when one file is a holidays-only
            # supplement, or both sit on the same side of different
            # calendars; otherwise the journeys decide.
            so_i <- grep("^SO:", strsplit(ifelse(is.na(meta$Days[i]), "", meta$Days[i]),
                                          "\r", fixed = TRUE)[[1]], value = TRUE)
            so_j <- grep("^SO:", strsplit(ifelse(is.na(meta$Days[j]), "", meta$Days[j]),
                                          "\r", fixed = TRUE)[[1]], value = TRUE)
            if (!setequal(so_i, so_j)) {
              ci <- so_class(so_i); cj <- so_class(so_j)
              if (ci == "holiday" || cj == "holiday") next
              if (ci == cj && ci != "mixed") next
            }
            older <- if (meta$CreationDateTime[i] == meta$CreationDateTime[j]) {
              # numerically: revision "10" follows "9"
              ri <- suppressWarnings(as.numeric(meta$RevisionNumber[i]))
              rj <- suppressWarnings(as.numeric(meta$RevisionNumber[j]))
              if (!is.na(ri) && !is.na(rj)) { if (ri < rj) i else j } else
                if (meta$RevisionNumber[i] < meta$RevisionNumber[j]) i else j
            } else if (meta$CreationDateTime[i] < meta$CreationDateTime[j]) {
              i
            } else {
              j
            }
            # A part, not a re-upload. West Midlands Bus files school route
            # 705/788 as two files of one export with one period, "WA" (five
            # journeys, lines 705 and 788) and "WB" (two, 705, written nine
            # minutes later); WB's 07:54 passes the versions test against
            # WA's 07:55, and the tie-break deleted WA and line 788 with it.
            # A repeated upload carries the whole timetable, so within one
            # export a file under half the size of the other does not
            # replace it (as for the small-successor rule below).
            newer <- if (older == i) j else i
            if (!other_export(i, j) && !is.na(jn_all[older]) && !is.na(jn_all[newer]) &&
                jn_all[newer] < 0.5 * jn_all[older]) next
            out$drop[older] <- TRUE
            note(older, "identical period: the older upload of the same registration",
                 meta$file[newer])
            acted <- TRUE
          } else if (si == sj) {
            # Same start, different end: let the shorter, more specific period
            # govern while it runs and move the longer one's start past it -
            # but only if it runs on every day the longer one does. Transport
            # for London files night route N97 as an every-night file to 23
            # December and a Saturday-only file to 19 December; standing the
            # whole-week file down until 20 December left the route with
            # Saturday nights only (264 of 308 journeys lost in the July 2026
            # window). A shorter file running on fewer days cannot stand in
            # for the longer one, so both stay and the shared days are left to
            # gtfs_deduplicate().
            short <- if (ei < ej) i else j
            long  <- if (ei < ej) j else i
            d_short <- strsplit(meta$Days[short], "\r", fixed = TRUE)[[1]]
            d_long <- strsplit(meta$Days[long], "\r", fixed = TRUE)[[1]]
            d_short <- d_short[nzchar(d_short) & !grepl("^SO:", d_short)]
            d_long <- d_long[nzchar(d_long) & !grepl("^SO:", d_long)]
            # Only the weekdays inside the shorter period count: Transport for
            # London lays one-weekend engineering-works files over the
            # District line's whole-week file, and a Saturday-and-Sunday file
            # for one weekend does run on every date of its own period.
            span <- seq(out$new_start[short],
                        min(out$new_end[short], out$new_start[short] + 6),
                        by = "day")
            in_span <- weekdays_en(span)
            d_long <- intersect(d_long, in_span)
            if (length(d_short) && length(d_long) &&
                !all(d_long %in% d_short)) next
            out$new_start[long] <- out$new_end[short] + 1
            out$changed[long] <- TRUE
            note(long, "same start: postponed until the shorter period ends",
                 meta$file[short])
            acted <- TRUE
          } else {
            early <- if (si < sj) i else j
            late  <- if (si < sj) j else i
            # A successor is of comparable size. Hodgsons' 87N is the six
            # evening journeys of route 87 (17:56 to 20:25, Monday to
            # Saturday) from 20 November 2024, beside the 26-journey weekday
            # file from two days before that carries the same evening times:
            # the versions test passes on the smaller file's journeys, and
            # closing the weekday file deleted its 20 daytime journeys. As
            # for the retimed and stale rules, a file with under half the
            # journeys of the one before it does not replace it - when both
            # come from one export, as 87 and 87N do (revision 45, one
            # modification time). A later export is the next edition however
            # short: First Somerset's Christmas week for route 4 (20
            # journeys, revision 859) follows the 13 December edition (41,
            # revision 858), and the guard ran both over Christmas.
            small_successor <- !is.na(jn_all[early]) && !is.na(jn_all[late]) &&
              jn_all[late] < 0.5 * jn_all[early] && !successive(early, late)
            if (!small_successor && out$new_end[early] <= out$new_end[late]) {
              # A successor that runs on fewer weekdays replaces the earlier
              # file on those days only. West Midlands Bus publishes routes
              # X21/X22 from 15 November 2026 as a term-time weekday file
              # beside its whole-week file from 1 November; closing the
              # whole-week file left the routes with no Saturday or Sunday
              # service, though the timetable in force runs both. The earlier
              # file carries on from the successor's start with only its
              # journeys on the days the successor does not cover: an extra
              # plan row, which later files can supersede in turn.
              #
              # Only that pattern, though: a successor running Monday to
              # Friday on school working days alone, beside a file with
              # weekend journeys. A term-time weekday file cannot be the
              # whole timetable of a route that runs at weekends. Shorter
              # weeks in general are not enough - First Bus publishes its
              # Christmas weeks as dated files running on fewer days (the
              # week of 20 December 2026 Monday to Thursday), and splitting
              # those ran the old Friday journeys on Christmas Day; 367 of
              # the 398 splits a general rule made in the November 2026 BODS
              # archive were of that kind.
              d_e <- strsplit(meta$Days[early], "\r", fixed = TRUE)[[1]]
              d_l <- strsplit(meta$Days[late], "\r", fixed = TRUE)[[1]]
              term_l <- any(startsWith(d_l, "SO:DaysOfOperation:WorkingDays"))
              d_e <- d_e[!is.na(d_e) & nzchar(d_e) & !grepl("^SO:", d_e)]
              d_l <- d_l[!is.na(d_l) & nzchar(d_l) & !grepl("^SO:", d_l)]
              wk5 <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
              if (term_l && setequal(d_l, wk5) &&
                  any(c("Saturday", "Sunday") %in% d_e)) {
                k <- nrow(out) + 1L
                meta[k, ] <- meta[early, ]
                # weekdays only, so that the disjoint-days test above keeps
                # the split row and the successor apart
                meta$Days[k] <- paste(setdiff(d_e, d_l), collapse = "\r")
                out[k, ] <- out[early, ]
                out$new_start[k] <- out$new_start[late]
                out$exclude[k] <- bitwOr(out$exclude[early], day_mask(d_l))
                out$split[k] <- TRUE
                out$changed[k] <- TRUE
                note(k, "split: kept on the days a term-time successor does not run",
                     meta$file[late])
                split_pairs[nrow(split_pairs) + 1, ] <- c(meta$file[early],
                                                          meta$file[late])
                g <- c(g, k)
                g <- g[order(out$new_start[g], out$new_end[g])]
              }
              # The successor runs at least as far as the file it replaces:
              # close the predecessor the day before the successor starts.
              out$new_end[early] <- out$new_start[late] - 1
              out$changed[early] <- TRUE
              note(early, "staggered: closed the day before its successor starts",
                   meta$file[late])
              acted <- TRUE
            }
            # The remaining case is a later period sitting wholly inside an
            # earlier one. Closing the outer period would delete the service
            # either side of the inner one, and one OperatingPeriod cannot
            # express a gap, so nothing is done here; what is left unresolved
            # is counted once the group has settled.
          }
          if (out$new_start[i] > out$new_end[i]) out$drop[i] <- TRUE
          if (out$new_start[j] > out$new_end[j]) out$drop[j] <- TRUE
          if (acted) break
        }
        if (acted) break
      }
      if (!acted) break
    }
    # Count what the group could not resolve, once it has settled, rather than
    # once per retry.
    live <- g[!out$drop[g]]
    if (length(live) > 1) {
      for (a in seq_len(length(live) - 1)) {
        for (b in seq(a + 1, length(live))) {
          i <- live[a]; j <- live[b]
          if (max(out$new_start[i], out$new_start[j]) <=
              min(out$new_end[i], out$new_end[j])) nested <- nested + 1L
        }
      }
    }
  }

  # Operator handover. When a route passes to another operator, the new
  # operator publishes the same timetable under its own operator code and
  # ServiceCode, and the outgoing operator's open-ended files often stay
  # published. From 1 November 2026 National Express West Midlands' routes
  # are run by West Midlands Bus Limited, and BODS holds both, so every
  # journey was counted twice (route 74: 896 a weekday for 448 run). The
  # groups above cannot see it, because they are keyed on the operator. A
  # file is closed the day before another operator's file starts where the
  # two publish the same lines, serve largely the same stops, run on the
  # same days, and carry the same timetable by the versions test; the stop
  # test comes first so that journeys are read only for real candidates.
  n_handover <- 0L
  handover_pairs <- data.frame(closed = character(0), by = character(0))
  if (!is.null(meta$Stops)) {
    ok <- !out$drop & !is.na(meta$Lines) & nzchar(meta$Lines) &
      !is.na(meta$NOC) & nzchar(meta$NOC) & !fixed_track_mode(meta$Mode) &
      !is.na(meta$Stops) & nzchar(meta$Stops)
    idx <- which(ok)
    by_line <- split(idx, meta$Lines[idx])
    by_line <- by_line[lengths(by_line) > 1]
    stops_of <- strsplit(meta$Stops, "\r", fixed = TRUE)
    days_of <- lapply(strsplit(ifelse(is.na(meta$Days), "", meta$Days), "\r",
                               fixed = TRUE),
                      function(d) d[nzchar(d) & !grepl("^SO:", d)])
    for (g in by_line) {
      if (length(unique(meta$NOC[g])) < 2) next
      for (a in seq_len(length(g) - 1)) {
        for (b in seq(a + 1, length(g))) {
          i <- g[a]; j <- g[b]
          if (out$drop[i] || out$drop[j] || meta$NOC[i] == meta$NOC[j]) next
          if (out$new_start[i] == out$new_start[j]) next
          early <- if (out$new_start[i] < out$new_start[j]) i else j
          late <- if (early == i) j else i
          if (out$new_end[early] < out$new_start[late]) next
          si <- stops_of[[early]]; sj <- stops_of[[late]]
          if (length(intersect(si, sj)) < 0.5 * length(union(si, sj))) next
          if (length(days_of[[early]]) && length(days_of[[late]]) &&
              !all(days_of[[early]] %in% days_of[[late]])) next
          # The outgoing operator's journeys must reappear under the new
          # one, not merely the other way round: First Potteries' 35-journey
          # route 9 sits inside D&G's 130-journey route 9 (97% of First's
          # journeys are D&G's times), and taking that for a takeover closed
          # D&G's file. The versions test alone is symmetric.
          je <- journeys(early); jl <- journeys(late)
          if (!isTRUE(txc_versions(je, jl))) next
          sh <- txc_journey_share(je, jl, of_first = TRUE)
          if (is.na(sh) || sh < 0.5) next
          out$new_end[early] <- out$new_start[late] - 1
          out$changed[early] <- TRUE
          note(early, "handover: another operator took the route over",
               meta$file[late])
          n_handover <- n_handover + 1L
          handover_pairs[nrow(handover_pairs) + 1, ] <- c(meta$file[early],
                                                          meta$file[late])
        }
      }
    }
  }

  # A handover the outgoing operator announced. Where a file's own
  # registration has a later, newer edition that ended by `date` (ClosedEnd,
  # see txc_filter_files()), and files of other operators starting the next
  # day, each running largely over its route, serve at least half of its
  # stops on all of its days, the file ends with that edition. National Express West Midlands
  # published route 96 open-ended from 30 August and a last edition for
  # 25-31 October; West Midlands Bus took the route over on 1 November,
  # retimed (61 of 100 departure times kept), so the test above, which needs
  # the same timetable, kept both. The same happened to the Outer Circle,
  # NX's 11A/11C in one file against West Midlands Bus's 11A/11S and 11C,
  # which that test, keyed on identical line sets, never compared. The ended
  # edition is the outgoing operator's word that it stopped; the successor
  # files need only cover the route - unless they are dated before the
  # takeover, when they must carry the outgoing timetable (see below).
  if (!is.null(meta$ClosedEnd) && !is.null(meta$Stops)) {
    lines_of <- strsplit(ifelse(is.na(meta$Lines), "", meta$Lines), "\r", fixed = TRUE)
    stops_of2 <- strsplit(ifelse(is.na(meta$Stops), "", meta$Stops), "\r", fixed = TRUE)
    days_of2 <- lapply(strsplit(ifelse(is.na(meta$Days), "", meta$Days), "\r",
                                fixed = TRUE),
                       function(d) d[nzchar(d) & !grepl("^SO:", d)])
    cand <- which(!out$drop & !is.na(meta$ClosedEnd) &
                    out$new_end > meta$ClosedEnd & !fixed_track_mode(meta$Mode) &
                    !is.na(meta$NOC) & nzchar(meta$NOC))
    for (k in cand) {
      ln <- lines_of[[k]][nzchar(lines_of[[k]])]
      if (!length(ln) || !length(stops_of2[[k]])) next
      ce1 <- meta$ClosedEnd[k] + 1
      succ <- which(!out$drop & !is.na(meta$NOC) & meta$NOC != meta$NOC[k] &
                      out$new_start <= ce1 & out$new_end >= ce1 &
                      !fixed_track_mode(meta$Mode))
      # each one on this route: a shared line number is not enough (First
      # Worcestershire's 20/20A/20B, from the same day, is not Coventry's)
      succ <- succ[vapply(succ, function(j) {
        length(intersect(lines_of[[j]], ln)) > 0 &&
          length(intersect(stops_of2[[j]], stops_of2[[k]])) >=
          0.5 * min(length(stops_of2[[j]]), length(stops_of2[[k]]))
      }, logical(1))]
      # A successor dated earlier than the takeover is a copy of the outgoing
      # timetable, or it is not a successor. West Midlands Bus published its
      # copies of NX's routes 9, 11/11A, 52 and others on 21 October with
      # NX's own start, 30 August, and the same journeys (448, 374, 342), so
      # both operators ran every journey from 30 August. Such a file must
      # carry the outgoing operator's journeys (the versions test, and at
      # least half of the outgoing file's journeys in it, as above); it then
      # starts at the takeover. Another operator already running on the
      # route - a joint service - carries its own journeys and is left alone.
      early_s <- succ[out$new_start[succ] < ce1]
      copies <- early_s[vapply(early_s, function(j) {
        jk <- journeys(k); jj <- journeys(j)
        if (!isTRUE(txc_versions(jk, jj))) return(FALSE)
        sh <- txc_journey_share(jk, jj, of_first = TRUE)
        !is.na(sh) && sh >= 0.5
      }, logical(1))]
      succ <- c(setdiff(succ, early_s), copies)
      if (!length(succ)) next
      if (length(intersect(stops_of2[[k]], unlist(stops_of2[succ]))) <
          0.5 * length(stops_of2[[k]])) next
      dk <- days_of2[[k]]
      if (length(dk) && !all(dk %in% unlist(days_of2[succ]))) next
      out$new_end[k] <- meta$ClosedEnd[k]
      out$changed[k] <- TRUE
      note(k, "handover: ended with its operator's last edition, another operator follows",
           meta$file[succ])
      if (length(copies)) {
        out$new_start[copies] <- ce1
        out$changed[copies] <- TRUE
        for (cc in copies) {
          note(cc, "handover: copy of the outgoing timetable, starts at the takeover",
               meta$file[k])
        }
      }
      n_handover <- n_handover + 1L
      handover_pairs[nrow(handover_pairs) + 1, ] <- c(meta$file[k],
                                                      paste(meta$file[succ], collapse = ";"))
    }
  }

  # a period truncated out of existence is a drop, not a rewrite
  out$drop[out$new_start > out$new_end] <- TRUE
  out$changed <- out$changed & !out$drop &
    (out$new_start != meta$StartDate | out$new_end != meta$EndDate | out$split)
  attr(out, "nested") <- nested
  attr(out, "handover") <- n_handover
  attr(out, "stale") <- n_stale
  attr(out, "handover_pairs") <- handover_pairs
  attr(out, "split_pairs") <- split_pairs
  out
}


#' First stop, departure minute and days of every vehicle journey in a file
#'
#' Internal helper for [txc_overlap_plan()]. Enough to tell whether two files
#' carry the same journeys: a successor registration repeats them, a file that
#' merely shares the route publishes others.
#'
#' The days matter as well as the times. Peter's Travel files route 343 as a
#' weekday file and a file that is mostly Saturday journeys, which leave at
#' the weekday times; compared on times alone the weekday file "repeated" 95%
#' of the other and was dropped as a duplicate registration. Each journey's
#' days are taken from its own OperatingProfile, else its journey pattern's,
#' else the service's, as TransXChange defines them, and held as a bit mask
#' (Monday = 1 ... Sunday = 64); a journey with no readable days runs on all
#' seven.
#'
#' @param file path to a TransXchange file
#' @return data.frame of stop, minute and days (integer mask), zero rows if
#'   none can be read
#' @noRd
txc_journey_keys <- function(file) {
  none <- data.frame(stop = character(0), minute = numeric(0),
                     days = integer(0))
  x <- try(xml2::read_xml(file), silent = TRUE)
  if (inherits(x, "try-error")) return(none)
  vj <- xml2::xml_find_all(x, "//d1:VehicleJourneys/d1:VehicleJourney")
  if (!length(vj)) return(none)
  dep <- xml2::xml_text(xml2::xml_find_first(vj, "d1:DepartureTime"))
  jpref <- xml2::xml_text(xml2::xml_find_first(vj, "d1:JourneyPatternRef"))
  jp <- xml2::xml_find_all(x, "//d1:JourneyPattern")
  jp_id <- xml2::xml_attr(jp, "id")
  jp_sec <- stats::setNames(
    xml2::xml_text(xml2::xml_find_first(jp, "d1:JourneyPatternSectionRefs")),
    jp_id)
  sec <- xml2::xml_find_all(x, "//d1:JourneyPatternSection")
  sec_stop <- stats::setNames(
    xml2::xml_text(xml2::xml_find_first(
      sec, "d1:JourneyPatternTimingLink/d1:From/d1:StopPointRef")),
    xml2::xml_attr(sec, "id"))
  stop <- unname(sec_stop[jp_sec[jpref]])
  minute <- suppressWarnings(as.numeric(substr(dep, 1, 2)) * 60 +
                               as.numeric(substr(dep, 4, 5)))
  days <- txc_vj_days(x, vj, jpref)
  ok <- !is.na(stop) & !is.na(minute)
  data.frame(stop = stop[ok], minute = minute[ok], days = as.integer(days[ok]))
}


#' Days of the week each vehicle journey runs, as a bit mask
#'
#' Internal helper for [txc_journey_keys()] and [txc_write_period()]. Taken
#' from the journey's own OperatingProfile, else its journey pattern's, else
#' the service's, as TransXChange defines them (Monday = 1 ... Sunday = 64);
#' a journey with no readable days runs on all seven.
#'
#' @param x parsed TransXchange document
#' @param vj nodeset of its VehicleJourney elements
#' @param jpref their JourneyPatternRef values
#' @return integer vector, one mask per journey
#' @noRd
txc_vj_days <- function(x, vj, jpref) {
  wk <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
          "Saturday", "Sunday")
  mask_of <- function(nodes) {
    out <- rep(NA_integer_, length(nodes))
    if (!length(nodes)) return(out)
    dw <- xml2::xml_find_first(nodes, "d1:RegularDayType/d1:DaysOfWeek")
    for (k in which(!is.na(xml2::xml_name(dw)))) {
      d <- expand_days_of_week(xml2::xml_name(xml2::xml_children(dw[k])))
      d <- d[d %in% wk]
      if (length(d)) out[k] <- as.integer(sum(2^(match(d, wk) - 1)))
    }
    # HolidaysOnly: no regular day at all, so it can match nothing. Taken
    # for "every day" it made Arriva North East's bank-holiday file for the
    # X22 a duplicate of the Sunday file with the same journeys, and the
    # Sunday file was the one dropped.
    ho <- xml2::xml_find_first(nodes, "d1:RegularDayType/d1:HolidaysOnly")
    out[!is.na(xml2::xml_name(ho))] <- 0L
    out
  }
  jp <- xml2::xml_find_all(x, "//d1:JourneyPattern")
  days <- mask_of(xml2::xml_find_first(vj, "d1:OperatingProfile"))
  jp_days <- stats::setNames(
    mask_of(xml2::xml_find_first(jp, "d1:OperatingProfile")),
    xml2::xml_attr(jp, "id"))
  svc_days <- mask_of(xml2::xml_find_all(
    x, "//d1:Services/d1:Service[1]/d1:OperatingProfile"))
  if (!length(svc_days)) svc_days <- NA_integer_
  days <- ifelse(is.na(days), unname(jp_days[jpref]), days)
  days <- ifelse(is.na(days), svc_days[1], days)
  days[is.na(days)] <- 127L
  as.integer(days)
}


#' Share of the smaller file's journeys that the other file also runs
#'
#' Internal helper for [txc_overlap_plan()]. A journey matches when the other
#' file has one from the same first stop within `tol` minutes on at least one
#' of the same days of the week - the times of a re-registered timetable move
#' by a minute or two, its stops do not.
#'
#' @param a,b data.frames from txc_journey_keys()
#' @param tol minutes
#' @param of_first share of `a`'s journeys found in `b`, rather than of the
#'   smaller file's
#' @return a number between 0 and 1, or NA when either file has no journeys
#' @noRd
txc_journey_share <- function(a, b, tol = 3, of_first = FALSE) {
  if (!nrow(a) || !nrow(b)) return(NA_real_)
  if (!of_first && nrow(a) > nrow(b)) { t <- a; a <- b; b <- t }
  if (is.null(a$days)) a$days <- rep(127L, nrow(a))
  if (is.null(b$days)) b$days <- rep(127L, nrow(b))
  by_stop <- split(seq_len(nrow(b)), b$stop)
  hit <- vapply(seq_len(nrow(a)), function(k) {
    m <- by_stop[[a$stop[k]]]
    !is.null(m) && any(abs(b$minute[m] - a$minute[k]) <= tol &
                         bitwAnd(b$days[m], a$days[k]) > 0L)
  }, logical(1))
  mean(hit)
}


#' Write a copy of a TransXchange file with a different operating period
#'
#' Internal helper for [txc_filter_files()]. Edits the text rather than the
#' parsed tree: re-serialising with xml2 would have to re-create the
#' `EndDate` element in the right namespace, and rewriting the raw text keeps
#' every other byte of the file exactly as published.
#'
#' @param file path to the source file
#' @param start Date, the new operating-period start
#' @param end Date, the new operating-period end
#' @param out_dir directory to write into
#' @param exclude bit mask of weekdays (Monday = 1) whose journeys are removed:
#'   a vehicle journey is kept only if it runs on some other day. Used for the
#'   part of a file left running beside a successor that covers fewer days.
#' @return path to the new file, or NA if the period could not be located or
#'   no journey is left
#' @noRd
txc_write_period <- function(file, start, end, out_dir, exclude = 0L) {

  txt <- try(paste(readLines(file, warn = FALSE), collapse = "\n"),
             silent = TRUE)
  if (inherits(txt, "try-error")) return(NA_character_)

  # the Service's own OperatingPeriod, not one belonging to a
  # ServicedOrganisation or a VehicleJourney
  svc <- regexpr("(?s)<([A-Za-z0-9_.-]+:)?Services>.*?</([A-Za-z0-9_.-]+:)?Services>",
                 txt, perl = TRUE)
  if (svc == -1) return(NA_character_)
  svc_txt <- substr(txt, svc, svc + attr(svc, "match.length") - 1)

  op <- regexpr("(?s)<([A-Za-z0-9_.-]+:)?OperatingPeriod>.*?</([A-Za-z0-9_.-]+:)?OperatingPeriod>",
                svc_txt, perl = TRUE)
  if (op == -1) return(NA_character_)
  op_txt <- substr(svc_txt, op, op + attr(op, "match.length") - 1)

  sd <- regexpr("<([A-Za-z0-9_.-]+:)?StartDate>[^<]*</([A-Za-z0-9_.-]+:)?StartDate>",
                op_txt, perl = TRUE)
  if (sd == -1) return(NA_character_)
  sd_txt <- substr(op_txt, sd, sd + attr(sd, "match.length") - 1)
  # reuse whatever namespace prefix the file itself uses
  prefix <- sub("^<([A-Za-z0-9_.-]+:)?StartDate>.*$", "\\1", sd_txt)

  new_op <- sub("<([A-Za-z0-9_.-]+:)?StartDate>[^<]*</([A-Za-z0-9_.-]+:)?StartDate>",
                sprintf("<%1$sStartDate>%2$s</%1$sStartDate>", prefix,
                        format(start, "%Y-%m-%d")),
                op_txt, perl = TRUE)

  end_txt <- sprintf("<%1$sEndDate>%2$s</%1$sEndDate>", prefix,
                     format(end, "%Y-%m-%d"))
  if (grepl("<([A-Za-z0-9_.-]+:)?EndDate>", new_op, perl = TRUE)) {
    new_op <- sub("<([A-Za-z0-9_.-]+:)?EndDate>[^<]*</([A-Za-z0-9_.-]+:)?EndDate>",
                  end_txt, new_op, perl = TRUE)
  } else {
    # an open-ended registration: the EndDate has to be created, and the
    # schema puts it directly after StartDate
    new_op <- sub("(<([A-Za-z0-9_.-]+:)?StartDate>[^<]*</([A-Za-z0-9_.-]+:)?StartDate>)",
                  paste0("\\1", end_txt), new_op, perl = TRUE)
  }

  new_svc <- paste0(substr(svc_txt, 1, op - 1), new_op,
                    substr(svc_txt, op + attr(op, "match.length"),
                           nchar(svc_txt)))
  new_txt <- paste0(substr(txt, 1, svc - 1), new_svc,
                    substr(txt, svc + attr(svc, "match.length"), nchar(txt)))

  # keep the original name so any downstream message still identifies the
  # service, but guarantee uniqueness across regions
  new_path <- file.path(out_dir, paste0(
    tools::file_path_sans_ext(basename(file)), "_",
    substr(digest::digest(file, algo = "md5"), 1, 8),
    if (exclude > 0L) paste0("_x", exclude) else "", ".xml"))
  if (exclude > 0L) {
    x <- try(xml2::read_xml(new_txt), silent = TRUE)
    if (inherits(x, "try-error")) return(NA_character_)
    vj <- xml2::xml_find_all(x, "//d1:VehicleJourneys/d1:VehicleJourney")
    jpref <- xml2::xml_text(xml2::xml_find_first(vj, "d1:JourneyPatternRef"))
    days <- txc_vj_days(x, vj, jpref)
    # holidays-only journeys (no regular day) are not the successor's to take
    gone <- days != 0L & bitwAnd(days, bitwAnd(bitwNot(exclude), 127L)) == 0L
    if (all(gone)) return(NA_character_)
    xml2::xml_remove(vj[gone])
    xml2::write_xml(x, new_path)
    return(new_path)
  }
  writeLines(new_txt, new_path, useBytes = TRUE)
  new_path
}


#' Identify the ServicedOrganisation calendar a day type refers to
#'
#' Internal helper for the day sets [txc_filter_files()] compares. Each node
#' is a `WorkingDays` or `Holidays` element of a ServicedOrganisationDayType,
#' holding one or more `ServicedOrganisationRef`s. The organisation's own
#' `WorkingDays` (or `Holidays`) date ranges are read from the file's
#' `ServicedOrganisations` and reduced to a short digest, so two files naming
#' the same dates under different codes give the same id and two calendars
#' with different dates never do. A reference the file does not define keeps
#' its code.
#'
#' @param xml a parsed TransXChange document (namespace prefix `d1`)
#' @param so nodeset of `WorkingDays` / `Holidays` elements
#' @return character vector, one id per node
#' @noRd
so_calendar_id <- function(xml, so) {
  orgs <- xml2::xml_find_all(xml, "//d1:ServicedOrganisations/d1:ServicedOrganisation")
  codes <- trimws(xml2::xml_text(xml2::xml_find_first(orgs, "d1:OrganisationCode")))
  vapply(so, function(node) {
    kind <- xml2::xml_name(node)
    refs <- sort(unique(trimws(xml2::xml_text(
      xml2::xml_find_all(node, "d1:ServicedOrganisationRef")))))
    if (!length(refs)) return("")
    parts <- vapply(refs, function(r) {
      o <- orgs[!is.na(codes) & codes == r]
      if (!length(o)) return(paste0("ref=", r))
      dr <- xml2::xml_find_all(o[[1]], paste0("d1:", kind, "/d1:DateRange"))
      if (!length(dr)) return(paste0("ref=", r))
      s <- trimws(xml2::xml_text(xml2::xml_find_first(dr, "d1:StartDate")))
      e <- trimws(xml2::xml_text(xml2::xml_find_first(dr, "d1:EndDate")))
      paste(sort(paste(s, e)), collapse = ",")
    }, character(1))
    substr(digest::digest(paste(parts, collapse = ";"), algo = "md5"), 1, 8)
  }, character(1), USE.NAMES = FALSE)
}


#' Is every journey of a file bound to a ServicedOrganisation calendar?
#'
#' Internal helper for the day sets [txc_filter_files()] compares. A journey
#' runs on the operating profile it carries, or failing that its journey
#' pattern's, or failing that the service's; it is bound when that profile
#' has a ServicedOrganisationDayType, or when its DaysOfNonOperation date
#' ranges cover at least half the file's operating period (a year at most):
#' dated extras, not a whole timetable. A file with no journeys counts as
#' bound, so files built by hand without journeys keep their calendar tokens.
#'
#' @param xml a parsed TransXChange document (namespace prefix `d1`)
#' @param start,end the file's operating period ("YYYY-MM-DD"; `end` may be
#'   NA); without a start only calendar-bound journeys count
#' @return logical(1)
#' @noRd
txc_all_journeys_bound <- function(xml, start = NA, end = NA) {
  vjs <- xml2::xml_find_all(xml, "//d1:VehicleJourneys/d1:VehicleJourney")
  if (!length(vjs)) return(TRUE)
  ps <- as.Date(start, optional = TRUE)
  pe <- as.Date(end, optional = TRUE)
  if (length(ps) != 1) ps <- NA
  if (length(pe) != 1 || is.na(pe) || (!is.na(ps) && pe > ps + 365)) pe <- ps + 365
  confined <- function(p) {
    if (is.na(ps) || is.na(pe) || pe < ps) return(FALSE)
    r <- xml2::xml_find_all(p, "d1:SpecialDaysOperation/d1:DaysOfNonOperation/d1:DateRange")
    if (!length(r)) return(FALSE)
    a <- as.Date(xml2::xml_text(xml2::xml_find_first(r, "d1:StartDate")), optional = TRUE)
    b <- as.Date(xml2::xml_text(xml2::xml_find_first(r, "d1:EndDate")), optional = TRUE)
    b[is.na(b)] <- a[is.na(b)]
    span <- seq(ps, pe, by = "day")
    off <- logical(length(span))
    for (k in seq_along(a)) if (!is.na(a[k])) off[span >= a[k] & span <= b[k]] <- TRUE
    mean(off) >= 0.5
  }
  bound <- function(p) !inherits(p, "xml_missing") &&
    (!inherits(xml2::xml_find_first(p, "d1:ServicedOrganisationDayType"), "xml_missing") ||
       confined(p))
  svc_p <- xml2::xml_find_first(xml, "//d1:Services/d1:Service/d1:OperatingProfile")
  jp_p <- xml2::xml_find_all(xml, "//d1:Services/d1:Service/d1:StandardService/d1:JourneyPattern")
  jp_ids <- xml2::xml_attr(jp_p, "id")
  for (v in vjs) {
    p <- xml2::xml_find_first(v, "d1:OperatingProfile")
    if (inherits(p, "xml_missing")) {
      ref <- xml2::xml_text(xml2::xml_find_first(v, "d1:JourneyPatternRef"))
      k <- match(ref, jp_ids)
      if (!is.na(k)) p <- xml2::xml_find_first(jp_p[[k]], "d1:OperatingProfile")
    }
    if (inherits(p, "xml_missing")) p <- svc_p
    if (!bound(p)) return(FALSE)
  }
  TRUE
}


#' The side of a ServicedOrganisation calendar a day-set token names
#'
#' Strips the calendar id [so_calendar_id()] appends, leaving e.g.
#' "SO:DaysOfOperation:WorkingDays". Used where a file's successor may carry
#' a different calendar (next year's terms) and must still supersede it.
#' @noRd
#' How many of their dates do two files share?
#'
#' From their operating periods, weekdays and the dates on which the whole
#' service does not run (`NonOp`), within their common period (a year at
#' most): the dates both run on as a share of the dates the busier one runs
#' on, 0 where they never meet. NA where neither excludes any dates, where
#' either is holidays-only or its days are unknown, or where the periods do
#' not meet, so files without excluded dates are compared as before. A
#' ServicedOrganisation calendar is not applied, so a term-only file counts
#' every weekday.
#'
#' @param m two rows of the filter's metadata
#' @noRd
txc_dates_shared <- function(m) {
  if (nrow(m) != 2 || is.null(m$NonOp)) return(NA_real_)
  nonop <- ifelse(is.na(m$NonOp), "", m$NonOp)
  if (!any(nzchar(nonop))) return(NA_real_)
  wk <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  days <- lapply(strsplit(ifelse(is.na(m$Days), "", m$Days), "\r", fixed = TRUE),
                 function(d) intersect(d, wk))
  if (any(lengths(days) == 0)) return(NA_real_)
  st <- max(as.Date(m$StartDate)); en <- min(as.Date(m$EndDate), st + 365)
  if (is.na(st) || is.na(en) || st > en) return(NA_real_)
  span <- seq(st, en, by = "day")
  wd <- wk[as.integer(format(span, "%u"))]
  on <- lapply(1:2, function(k) {
    keep <- wd %in% days[[k]]
    for (r in strsplit(strsplit(nonop[k], "\r", fixed = TRUE)[[1]], "/", fixed = TRUE)) {
      if (length(r) != 2) next
      a <- as.Date(r[1], optional = TRUE); b <- as.Date(r[2], optional = TRUE)
      if (is.na(a) || is.na(b)) next
      keep[span >= a & span <= b] <- FALSE
    }
    keep
  })
  n <- max(sum(on[[1]]), sum(on[[2]]))
  if (n == 0) return(NA_real_)
  sum(on[[1]] & on[[2]]) / n
}

so_side <- function(x) sub("^(SO:[^:]+:[^:]+):.*$", "\\1", x)


#' Which part of the school year a file's calendar tokens confine it to
#'
#' "holiday" when every token runs the file on organisation holidays only
#' (DaysOfOperation on Holidays, or DaysOfNonOperation on WorkingDays),
#' "term" when every token confines it to working days, "mixed" when it
#' carries both sides (a whole-year file with term and holiday journeys),
#' "none" when it names no calendar.
#' @noRd
so_class <- function(x) {
  if (!length(x)) return("none")
  s <- so_side(x)
  hol <- s %in% c("SO:DaysOfOperation:Holidays", "SO:DaysOfNonOperation:WorkingDays")
  term <- s %in% c("SO:DaysOfOperation:WorkingDays", "SO:DaysOfNonOperation:Holidays")
  if (all(hol)) "holiday" else if (all(term)) "term" else "mixed"
}
