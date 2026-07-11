#' TransXchange to GTFS
#'
#' @details Convert transxchange files to GTFS
#'
#' @param path_in Either a character vector of paths to TransXchange xml
#'   files, or a single path to a zip archive of TransXchange files. The zip
#'   may be a simple flat archive of xml files or a nested archive containing
#'   sub-folders and further zip files (such as the Bus Open Data Service
#'   change archive); nested zip files are extracted automatically.
#' @param silent Logical, should progress be shown
#' @param ncores Numeric, When parallel processing how many cores to use
#' @param cal Calendar object from get_bank_holidays()
#' @param naptan Naptan stop locations from get_naptan()
#' @param scotland character, should Scottish bank holidays be used? Can be
#'   "auto" (default), "yes", "no". If "auto" and path_in ends with "S.zip"
#'   Scottish bank holidays will be used, otherwise England and Wales bank
#'   holidays are used.
#' @param try_mode Logical, if TRUE import and conversion are wrapped in try
#'   calls thus a failure on a single file will not cause the whole process to
#'   fail. Warning this could result in a GTFS file with missing routes.
#' @param force_merge Logical, passed to gtfs_merge(force), default FALSE
#' @param filter_duplicate_files Logical, if TRUE superseded versions of the
#'   same service are removed before conversion using [txc_filter_files()]
#'   (default FALSE). See Details.
#' @param filter_date Date, the reference date used by
#'   `filter_duplicate_files` to decide which version of each service is
#'   operative (default `Sys.Date()`). Only used when
#'   `filter_duplicate_files = TRUE`.
#' @return A GTFS named list
#' @details
#'
#' This is a meta function which aids TransXchange to GTFS conversion. It simple
#' runs transxchange_import(), transxchange_export(), gtfs_merge(), gtfs_write()
#'
#' Progress Bars
#'
#' To minimise overall processing when using multiple cores the function works
#' from largest to smallest file.This can mean the progress bar sits a 0% for
#' quite some time, before starting to move rapidly.
#'
#' Duplicate / superseded files
#'
#' Archives of TransXchange data (for example the Bus Open Data Service change
#' archive) often contain several versions of the same registered service:
#' each time an operator revises a timetable a new file is uploaded, but the
#' superseded files remain in the archive and usually still declare an
#' open-ended operating period. Converting all of them causes the same
#' physical journey to appear once per file version, so counting trips on a
#' given date over-estimates service levels (in one test archive the same bus
#' route appeared five times). A normal single download of current data does
#' not have this problem, as each service appears only once.
#'
#' Set `filter_duplicate_files = TRUE` to keep only the operative version of
#' each service before converting (see [txc_filter_files()] for the exact
#' rules and limitations). For historical analysis also set `filter_date` to a
#' date within the period you are studying.
#'
#'
#' @export


transxchange2gtfs <- function(path_in,
                              silent = TRUE,
                              ncores = 1,
                              cal = get_bank_holidays(),
                              naptan = get_naptan(),
                              scotland = "auto",
                              try_mode = TRUE,
                              force_merge = FALSE,
                              filter_duplicate_files = FALSE,
                              filter_date = Sys.Date()) {
  # Check inputs
  checkmate::assert_numeric(ncores)
  checkmate::assert_logical(silent)
  checkmate::assert_character(scotland)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(try_mode)
  checkmate::assert_logical(filter_duplicate_files)

  if (ncores == 1) {
    message(paste0(Sys.time(), " This will take some time, make sure you use 'ncores' to enable multi-core processing"))
  }

  # Check calendar and naptan
  if(!nrow(cal) > 0){
    stop("Calendar is missing")
  }

  if(!nrow(naptan) > 0){
    stop("Naptan is missing")
  }

  # Are we in Scotland?
  if (scotland == "yes") {
    scotland <- TRUE
  } else if (scotland == "no") {
    scotland <- FALSE
  } else if (scotland == "auto") {
    # Decide where we are
    if (length(path_in) == 1) {
      loc <- substr(path_in, nchar(path_in) - 5, nchar(path_in))
      if (loc %in% c("/S.zip", "\\S.zip")) {
        scotland <- TRUE
        message("Using Scottish Bank Holidays")
      } else {
        scotland <- FALSE
      }
    } else {
      scotland <- FALSE
    }
  } else {
    stop("Unknown value for scotland, can be 'yes' 'no' or 'auto'")
  }

  if (length(path_in) > 1) {
    if(!silent){message("Parsing provided xml files")}
    files <- path_in[substr(path_in, nchar(path_in) - 4 + 1, nchar(path_in)) == ".xml"]
  } else {
    txc_dir <- file.path(tempdir(), "txc")
    unlink(txc_dir, recursive = TRUE) # clear any data left over from a previous run
    dir.create(txc_dir, showWarnings = FALSE)
    if(!silent){ message(paste0(Sys.time(), " Unzipping data to temp folder"))}
    # Extract the archive, including any nested folders / zip files. The Bus
    # Open Data Service change archive is a zip of per-operator folders that
    # themselves contain a mix of loose xml files and further zip files.
    unzip_recursive(path_in, exdir = txc_dir, silent = silent)
    if(!silent){ message(paste0(Sys.time(), " Unzipping complete"))}

    files <- list.files(txc_dir,
                        pattern = "\\.xml$",
                        full.names = TRUE,
                        recursive = TRUE,
                        ignore.case = TRUE)

  }

  if(length(files) == 0){
    stop("No XML files found")
  } else {
    if(!silent){ message(length(files), " xml files have been found")}

  }

  # Remove superseded versions of the same service (see Details)
  if (filter_duplicate_files) {
    if(!silent){ message(paste0(Sys.time(), " Filtering duplicate / superseded files"))}
    n_before <- length(files)
    files <- txc_filter_files(files, date = filter_date, ncores = ncores,
                              quiet = silent)
    message(paste0(Sys.time(), " Removed ", n_before - length(files),
                   " superseded / duplicate files, ", length(files), " remain"))
    if (length(files) == 0) {
      stop("No XML files remain after filtering duplicates")
    }
  }


  files <- files[order(file.size(files), decreasing = TRUE)] # Large to small give optimum performance

  if (ncores == 1) {
    message(paste0(Sys.time(), " Importing TransXchange files, single core"))
    res_all <- purrr::map(files,
                           transxchange_import_try,
                           run_debug = TRUE,
                           full_import = FALSE,
                           try_mode = try_mode,
                           .progress = TRUE)
    res_all_message <- res_all[sapply(res_all, class) == "character"]
    res_all <- res_all[sapply(res_all, class) == "list"]
    if(length(res_all_message) > 0){
      message(" ")
      message("Failed to import files: ")
      res_all_message <- unlist(res_all_message)
      message(paste(res_all_message, collapse = ",  "))
    }
    message(paste0(Sys.time(), " Converting to GTFS, single core"))
    gtfs_all <- purrr::map(res_all,
                          transxchange_export_try,
                          run_debug = TRUE,
                          cal = cal,
                          naptan = naptan,
                          scotland = scotland,
                          try_mode = try_mode,
                          .progress = TRUE)
  } else {
    message(paste0(Sys.time(), " Importing TransXchange files, multicore"))

    future::plan(future::multisession, workers = ncores)
    res_all <- furrr::future_map(.x = files,
                             .f = transxchange_import_try,
                             run_debug = TRUE,
                             full_import = FALSE,
                             try_mode = try_mode,
                             .progress = TRUE)
    future::plan(future::sequential)


    # pb <- utils::txtProgressBar(max = length(files), style = 3)
    # progress <- function(n) utils::setTxtProgressBar(pb, n)
    # opts <- list(progress = progress, preschedule = FALSE)
    # cl <- parallel::makeCluster(ncores)
    # doSNOW::registerDoSNOW(cl)
    # boot <- foreach::foreach(i = seq_len(length(files)), .options.snow = opts)
    # res_all <- foreach::`%dopar%`(boot, {
    #     UK2GTFS:::transxchange_import_try(files[i],
    #                           try_mode = try_mode)
    # })
    # parallel::stopCluster(cl)
    # rm(cl, boot, opts, pb, progress)

    res_all_message <- res_all[sapply(res_all, class) == "character"]
    res_all <- res_all[sapply(res_all, class) == "list"]
    if(length(res_all_message) > 0){
      message(" ")
      message("Failed to import files: ")
      res_all_message <- unlist(res_all_message)
      message(paste(res_all_message, collapse = ",  "))
    } else {
      message(" ")
      message("All files imported")
    }

    # trim naptan, move less data to each worker
    sids <- purrr::map(res_all, function(x){
      s1 <- unique(x$JourneyPatternSections$From.StopPointRef)
      s2 <- unique(x$JourneyPatternSections$To.StopPointRef)
      s1 <- unique(c(s1,s2))
      s1
    })
    sids <- unique(unlist(sids, use.names = FALSE))
    naptan_trim <- naptan[naptan$stop_id %in% sids,]

    message(" ")
    message(paste0(Sys.time(), " Converting to GTFS, multicore"))

    future::plan(future::multisession, workers = ncores)
    gtfs_all <- furrr::future_map(.x = res_all,
                                 .f = transxchange_export_try,
                                 run_debug = TRUE,
                                 cal = cal,
                                 naptan = naptan_trim,
                                 scotland = scotland,
                                 try_mode = try_mode,
                                 .progress = TRUE)
    future::plan(future::sequential)


    # pb <- utils::txtProgressBar(min = 0, max = length(res_all), style = 3)
    # progress <- function(n) utils::setTxtProgressBar(pb, n)
    # opts <- list(progress = progress, preschedule = FALSE)
    # cl <- parallel::makeCluster(ncores)
    # doSNOW::registerDoSNOW(cl)
    # boot <- foreach::foreach(i = seq_len(length(res_all)), .options.snow = opts)
    # gtfs_all <- foreach::`%dopar%`(boot, {
    #     UK2GTFS:::transxchange_export_try(res_all[[i]],
    #                       cal = cal,
    #                       naptan = naptan_trim,
    #                       scotland = scotland,
    #                       try_mode = try_mode)
    #   # setTxtProgressBar(pb, i)
    # })
    #
    # parallel::stopCluster(cl)
    # rm(cl, boot, opts, pb, progress)
  }

  unlink(file.path(tempdir(), "txc"), recursive = TRUE)

  gtfs_all_message <- gtfs_all[sapply(gtfs_all, class) == "character"]
  gtfs_all <- gtfs_all[sapply(gtfs_all, class) == "list"]
  if(length(gtfs_all_message) > 0){
    message(" ")
    message("Failed to convert files: ")
    gtfs_all_message <- unlist(gtfs_all_message)
    message(paste(gtfs_all_message, collapse = ",  "))
  } else {
    message(" ")
    message("All files converted")
  }

  if(!silent){ message(paste0(Sys.time(), " Merging GTFS objects"))}

  gtfs_merged <- try(gtfs_merge(gtfs_all, force = force_merge, quiet = !silent))

  if (inherits(gtfs_merged, "try-error")) {
    message("Merging failed, returing unmerged GFTS object for analysis")
    return(gtfs_all)
  }
  return(gtfs_merged)
}


# Recursively extract a zip archive into `exdir`.
#
# Extracts `path_in` into `exdir` and then repeatedly extracts any zip files
# found inside it (at any depth), removing each zip once it has been extracted.
# This handles nested structures such as the Bus Open Data Service change
# archive, which is a zip containing per-operator folders that themselves hold
# a mix of loose xml files and further zip files.
#
# Each nested zip is extracted into a short, numbered folder ("z1", "z2", ...)
# at the root of `exdir` rather than next to the zip. BODS archives combine
# long operator-folder names with long file names, so keeping the extraction
# path short is necessary to stay within the Windows 260-character path limit
# (MAX_PATH), which would otherwise cause extraction to fail silently.
#
# `max_zips` is a safety cap against pathological / self-referential nesting.
# Returns `exdir` invisibly.
unzip_recursive <- function(path_in, exdir, silent = TRUE, max_zips = 100000L) {
  utils::unzip(path_in, exdir = exdir)

  processed <- character(0) # zip paths already handled (guards against loops)
  counter <- 0L
  repeat {
    nested <- list.files(exdir,
                         pattern = "\\.zip$",
                         full.names = TRUE,
                         recursive = TRUE,
                         ignore.case = TRUE)
    nested <- setdiff(nested, processed)
    if (length(nested) == 0) {
      break
    }
    if (!silent) {
      message(paste0(Sys.time(), " Extracting ", length(nested),
                     " nested zip file(s)"))
    }
    for (z in nested) {
      counter <- counter + 1L
      sub_exdir <- file.path(exdir, paste0("z", counter))
      dir.create(sub_exdir, showWarnings = FALSE, recursive = TRUE)
      res <- try(utils::unzip(z, exdir = sub_exdir), silent = TRUE)
      if (inherits(res, "try-error") || length(res) == 0) {
        warning("Failed to extract nested zip file: ", basename(z))
      }
      processed <- c(processed, z)
      unlink(z, force = TRUE) # remove so it is not re-listed on the next pass
    }
    if (counter > max_zips) {
      warning("Reached maximum number of nested zip files (", max_zips,
              "); some files may not have been extracted")
      break
    }
  }

  invisible(exdir)
}
