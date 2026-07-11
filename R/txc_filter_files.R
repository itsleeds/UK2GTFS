#' Filter superseded TransXchange file versions
#'
#' Given a set of TransXchange XML files, returns the subset that represents
#' the operative timetable for each service, discarding superseded revisions
#' of the same service.
#'
#' @param files character vector of paths to TransXchange XML files
#' @param date Date, the reference date used to decide which file version is
#'   operative (default `Sys.Date()`). For historical analysis set this to a
#'   date within the period you are studying.
#' @param ncores numeric, number of cores used to read the file headers
#'   (default 1)
#' @param quiet logical, if FALSE a summary of removed files is printed
#' @return a character vector, the subset of `files` to convert
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
#' This function reads only the header information of each file
#' (`ServiceCode`, `OperatingPeriod` start date, `RevisionNumber`, and
#' `ModificationDateTime`) and keeps, for each `ServiceCode`:
#'
#' \enumerate{
#'   \item For each distinct operating-period start date, only the file with
#'     the highest `RevisionNumber` (ties broken by the most recent
#'     `ModificationDateTime`) - repeated uploads of the same timetable
#'     period are duplicates.
#'   \item Of the start dates on or before `date`, only the most recent -
#'     this is the version operative on `date`; earlier versions have been
#'     superseded.
#'   \item All files whose operating period starts after `date` - these are
#'     future timetables that have not yet come into effect.
#' }
#'
#' Files whose `ServiceCode` cannot be read are always kept.
#'
#' Note one limitation: when a future timetable (kept under rule 3)
#' eventually starts, it supersedes the currently operative file, but both
#' are retained here because the operative file usually declares an
#' open-ended end date. Trip counts are therefore reliable around `date` but
#' may double-count dates after the next timetable change. For analysis of a
#' specific period, set `date` inside that period.
#'
#' This filtering is not needed when converting a normal single download of
#' current data (where each service appears once), only when converting
#' archives that accumulate every uploaded version.
#'
#' @export
txc_filter_files <- function(files, date = Sys.Date(), ncores = 1, quiet = TRUE) {

  checkmate::assert_character(files, min.len = 1)
  date <- as.Date(date)

  read_meta <- function(f) {
    meta <- try({
      xml <- xml2::read_xml(f)
      service <- xml2::xml_find_first(xml, "d1:Services/d1:Service")
      sc <- xml2::xml_text(xml2::xml_find_first(service, "d1:ServiceCode"))
      sd <- xml2::xml_text(xml2::xml_find_first(service, "d1:OperatingPeriod/d1:StartDate"))
      rev <- xml2::xml_attr(service, "RevisionNumber")
      if (is.na(rev)) rev <- xml2::xml_attr(xml, "RevisionNumber")
      mod <- xml2::xml_attr(service, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "ModificationDateTime")
      if (is.na(mod)) mod <- xml2::xml_attr(xml, "CreationDateTime")
      data.frame(file = f, ServiceCode = sc, StartDate = sd,
                 RevisionNumber = rev, ModificationDateTime = mod,
                 stringsAsFactors = FALSE)
    }, silent = TRUE)

    if (inherits(meta, "try-error")) {
      meta <- data.frame(file = f, ServiceCode = NA_character_,
                         StartDate = NA_character_,
                         RevisionNumber = NA_character_,
                         ModificationDateTime = NA_character_,
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
  meta$RevisionNumber <- suppressWarnings(as.numeric(meta$RevisionNumber))
  meta$RevisionNumber[is.na(meta$RevisionNumber)] <- -1
  meta$ModificationDateTime <- suppressWarnings(
    lubridate::ymd_hms(meta$ModificationDateTime, quiet = TRUE))
  no_mod <- is.na(meta$ModificationDateTime)
  meta$ModificationDateTime[no_mod] <- file.mtime(meta$file[no_mod])

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
    meta <- meta[!duplicated(meta[, c("ServiceCode", "StartDate")]), ]

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

  keep <- c(keep, unknown)
  result <- files[files %in% keep]

  if (!quiet) {
    message(Sys.time(), " txc_filter_files: kept ", length(result), " of ",
            length(files), " files (",
            length(files) - length(result),
            " superseded or duplicate versions removed)")
  }

  return(result)
}
