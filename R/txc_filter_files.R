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
#'   copy in `out_dir` rather than at the original file.
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
#' `ModificationDateTime`) and keeps, for each `ServiceCode`:
#'
#' \enumerate{
#'   \item For each distinct operating-period start date **and line**, only the
#'     file with the highest `RevisionNumber` (ties broken by the most recent
#'     `ModificationDateTime`) - repeated uploads of the same timetable
#'     period are duplicates. A file is kept if it is the best available file
#'     for at least one of the lines it publishes. The line matters because a
#'     `ServiceCode` does not identify one timetable: operators such as
#'     Nottingham City Transport split a single registration into one file per
#'     line, all sharing the `ServiceCode` and operating period, and keying on
#'     the `ServiceCode` alone would discard every line but one.
#'   \item Of the start dates on or before `date`, only the most recent -
#'     this is the version operative on `date`; earlier versions have been
#'     superseded.
#'   \item All files whose operating period starts after `date` - these are
#'     future timetables that have not yet come into effect.
#' }
#'
#' @section Overlapping registrations:
#' Rules 1 to 3 key on the `ServiceCode`, which catches a re-upload of one
#' registration but not a re-registration: some publishers, Transport for
#' London among them, mint a **new** `ServiceCode` every time a service is
#' re-registered. Each code then appears exactly once and nothing above
#' detects it, yet both files describe the same service over overlapping
#' dates and both convert into the feed.
#'
#' With `resolve_overlaps = TRUE` files are additionally grouped by
#' `NationalOperatorCode` + `Description` + the set of lines they publish -
#' the same registered service by any reading - and overlapping operating
#' periods within a group are reconciled:
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
      noc <- xml2::xml_text(xml2::xml_find_first(xml, "//d1:NationalOperatorCode"))
      rev <- xml2::xml_attr(service, "RevisionNumber")
      if (is.na(rev)) rev <- xml2::xml_attr(xml, "RevisionNumber")
      mod <- xml2::xml_attr(service, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "CreationDateTime")
      cre <- xml2::xml_attr(xml, "CreationDateTime")
      if (is.na(cre)) cre <- mod
      # Which lines this file publishes. Needed because a ServiceCode does not
      # identify a timetable on its own - see the note on rule 1 below.
      lns <- xml2::xml_text(xml2::xml_find_all(xml, "//d1:LineName"))
      lns <- sort(unique(trimws(lns[!is.na(lns)])))
      data.frame(file = f, ServiceCode = sc, StartDate = sd, EndDate = ed,
                 Description = desc, NOC = noc,
                 RevisionNumber = rev, ModificationDateTime = mod,
                 CreationDateTime = cre,
                 Lines = paste(lns, collapse = "\r"),
                 stringsAsFactors = FALSE)
    }, silent = TRUE)

    if (inherits(meta, "try-error")) {
      meta <- data.frame(file = f, ServiceCode = NA_character_,
                         StartDate = NA_character_,
                         EndDate = NA_character_,
                         Description = NA_character_,
                         NOC = NA_character_,
                         RevisionNumber = NA_character_,
                         ModificationDateTime = NA_character_,
                         CreationDateTime = NA_character_,
                         Lines = NA_character_,
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

  keep <- character()
  if (nrow(meta) > 0) {
    # rule 1: within each ServiceCode + StartDate keep the highest revision,
    # breaking ties on the most recent ModificationDateTime (both descending)
    meta <- meta[order(meta$ServiceCode, meta$StartDate,
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
    lines_list <- strsplit(meta$Lines, "\r", fixed = TRUE)
    # a file naming no line at all still has to be groupable
    lines_list[lengths(lines_list) == 0] <- ""
    idx <- rep(seq_len(nrow(meta)), lengths(lines_list))
    ex <- data.frame(row = idx,
                     ServiceCode = meta$ServiceCode[idx],
                     StartDate = meta$StartDate[idx],
                     Line = unlist(lines_list),
                     stringsAsFactors = FALSE)
    ex <- ex[!duplicated(ex[, c("ServiceCode", "StartDate", "Line")]), ]
    meta <- meta[sort(unique(ex$row)), ]

    # rules 2 and 3: keep the version operative on `date` plus future versions
    meta_split <- split(meta, meta$ServiceCode)
    keep <- lapply(meta_split, function(x) {
      past <- x$StartDate <= date
      operative <- character()
      if (any(past)) {
        operative <- x$file[past & x$StartDate == max(x$StartDate[past])]
      }
      c(operative, x$file[!past])
    })
    keep <- unlist(keep, use.names = FALSE)
  }

  n_superseded <- length(files) - length(keep) - length(unknown)

  # rule 4: reconcile registrations of the same service whose periods overlap
  rewritten <- character()
  n_dropped_overlap <- 0L
  n_nested <- 0L
  if (resolve_overlaps && length(keep) > 0) {
    plan <- txc_overlap_plan(meta[meta$file %in% keep, ])
    n_nested <- attr(plan, "nested")
    drop <- plan$file[plan$drop]
    n_dropped_overlap <- length(drop)
    keep <- setdiff(keep, drop)

    changed <- plan[!plan$drop & plan$changed, ]
    if (nrow(changed) > 0) {
      if (is.null(out_dir)) out_dir <- tempfile("txc_truncated")
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      for (i in seq_len(nrow(changed))) {
        new_path <- txc_write_period(changed$file[i], changed$new_start[i],
                                     changed$new_end[i], out_dir)
        if (!is.na(new_path)) {
          keep[keep == changed$file[i]] <- new_path
          rewritten <- c(rewritten, new_path)
        }
      }
    }
  }

  keep <- c(keep, unknown)
  # `keep` may now name rewritten copies, so it is the result in its own right
  result <- unique(keep)

  if (!quiet) {
    message(Sys.time(), " txc_filter_files: kept ", length(result), " of ",
            length(files), " files (", n_superseded,
            " superseded or duplicate versions removed",
            if (resolve_overlaps) paste0(", ", n_dropped_overlap,
                                         " duplicate registrations removed, ",
                                         length(rewritten),
                                         " operating periods truncated") else "",
            ")")
    if (resolve_overlaps && n_nested > 0) {
      message(Sys.time(), " txc_filter_files: ", n_nested,
              " overlapping registration(s) left in place - one period lies ",
              "wholly inside another and truncation cannot separate them")
    }
  }

  return(result)
}


#' Decide how overlapping registrations of one service should be reconciled
#'
#' Internal helper for [txc_filter_files()]. Takes the cleaned metadata frame
#' and returns one row per file with the operating period it should end up
#' with, and whether it should be dropped entirely.
#'
#' @param meta data.frame of file metadata
#' @return data.frame with columns file, new_start, new_end, drop, changed and
#'   an attribute "nested" counting the pairs left unresolved
#' @noRd
txc_overlap_plan <- function(meta) {

  out <- data.frame(file = meta$file,
                    new_start = meta$StartDate,
                    new_end = meta$EndDate,
                    drop = FALSE,
                    changed = FALSE,
                    stringsAsFactors = FALSE)
  nested <- 0L

  # The same registered service by any reading: one operator, one description,
  # one set of lines. Description is what separates line 436 in London from
  # line 436 in Hereford; without it this key would be far too coarse.
  key <- paste(ifelse(is.na(meta$NOC), "", meta$NOC),
               ifelse(is.na(meta$Description), "", meta$Description),
               ifelse(is.na(meta$Lines), "", meta$Lines),
               sep = "\r")
  # A group needs something to identify it. Files that name neither a
  # description nor an operator are left alone rather than pooled together.
  usable <- (!is.na(meta$Description) & nzchar(meta$Description)) |
    (!is.na(meta$NOC) & nzchar(meta$NOC))
  groups <- split(seq_len(nrow(meta))[usable], key[usable])
  groups <- groups[lengths(groups) > 1]

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

          if (si == sj && ei == ej) {
            # Identical periods: nothing to truncate, so the most recently
            # created file wins and the other is a duplicate registration.
            older <- if (meta$CreationDateTime[i] == meta$CreationDateTime[j]) {
              if (meta$RevisionNumber[i] < meta$RevisionNumber[j]) i else j
            } else if (meta$CreationDateTime[i] < meta$CreationDateTime[j]) {
              i
            } else {
              j
            }
            out$drop[older] <- TRUE
            acted <- TRUE
          } else if (si == sj) {
            # Same start, different end: let the shorter, more specific period
            # govern while it runs and move the longer one's start past it.
            short <- if (ei < ej) i else j
            long  <- if (ei < ej) j else i
            out$new_start[long] <- out$new_end[short] + 1
            out$changed[long] <- TRUE
            acted <- TRUE
          } else {
            early <- if (si < sj) i else j
            late  <- if (si < sj) j else i
            if (out$new_end[early] <= out$new_end[late]) {
              # The successor runs at least as far as the file it replaces:
              # close the predecessor the day before the successor starts.
              out$new_end[early] <- out$new_start[late] - 1
              out$changed[early] <- TRUE
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

  # a period truncated out of existence is a drop, not a rewrite
  out$drop[out$new_start > out$new_end] <- TRUE
  out$changed <- out$changed & !out$drop &
    (out$new_start != meta$StartDate | out$new_end != meta$EndDate)
  attr(out, "nested") <- nested
  out
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
#' @return path to the new file, or NA if the period could not be located
#' @noRd
txc_write_period <- function(file, start, end, out_dir) {

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
    substr(digest::digest(file, algo = "md5"), 1, 8), ".xml"))
  writeLines(new_txt, new_path, useBytes = TRUE)
  new_path
}
