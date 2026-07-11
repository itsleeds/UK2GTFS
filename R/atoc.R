#' ATOC to GTFS
#'
#' Convert ATOC CIF files to GTFS
#'
#' @param path_in Character, path to ATOC file e.g."C:/input/ttis123.zip"
#' @param silent Logical, should progress messages be suppressed (default TRUE)
#' @param ncores Numeric, When parallel processing how many cores to use
#'   (default 1)
#' @param locations where to get tiploc locations (see details)
#' @param agency where to get agency.txt (see details)
#' @param shapes Logical, should shapes.txt be generated (default FALSE)
#' @param transfers Logical, should transfers.txt be generated (default TRUE)
#' @param missing_tiplocs Logical, if true will check for
#'   any missing tiplocs against the main file and add them.(default TRUE)
#' @param working_timetable Logical, should WTT times be used instead of public times (default FALSE)
#' @param public_only Logical, only return calls/services that are for public passenger pickup/set down (default TRUE)
#' @param fares Character, optional path to a National Rail fares feed (e.g.
#'   "RJFAF756.zip", see [atoc_fares_read()]). If provided, GTFS fare tables
#'   are built and added to the result (default NULL, no fares).
#' @param fares_version Numeric, `1` for the original GTFS fares tables or
#'   `2` for GTFS Fares v2 (default 1). See [gtfs_add_railfares()] for what
#'   each supports.
#' @param fares_ticket_codes Character, optional explicit ticket codes to
#'   convert, passed to [gtfs_add_railfares()] as `ticket_codes`.
#' @param fares_ticket_class Character, "standard" and/or "first"
#'   (default "standard").
#' @param fares_ticket_type Character, any of "single", "return", "season".
#'   Default: "single" for v1, singles and returns for v2.
#' @param fares_walkup_only Logical, keep only walk-up Anytime/Off-Peak/
#'   Super Off-Peak tickets (default TRUE), see [gtfs_add_railfares()].
#' @param fares_rider_categories Character, any of "adult", "child"; GTFS
#'   Fares v2 only (default both).
#' @param fares_railcards Character, optional railcard codes (e.g. "YNG");
#'   GTFS Fares v2 only (default NULL).
#' @param fares_ndf Logical, include non-derivable fare overrides
#'   (default TRUE).
#' @param fares_travel_date Date, optional: convert fares as a scenario
#'   snapshot for a journey on this date, applying the feed's date/time
#'   restriction data. See [gtfs_add_railfares()].
#' @param fares_travel_time Character "HH:MM", optional departure time for
#'   the scenario (drops e.g. Off-Peak tickets at peak times). Requires
#'   `fares_travel_date`.
#' @param fares_booking_date Date, optional: when the ticket is bought.
#'   Includes Advance tickets bookable at that horizon, at their tier
#'   prices. Requires `fares_travel_date`.
#' @return A gtfs object: a named list of data frames representing the tables
#'   of a GTFS file (agency, stops, routes, trips, stop_times, calendar,
#'   calendar_dates, and optionally transfers and fare tables)
#' @family main
#'
#' @details Locations
#'
#'   The .msn file contains the physical locations of stations and other TIPLOC
#'   codes (e.g. junctions). However, the quality of the locations is often poor
#'   only accurate to about 1km and occasionally very wrong. Therefore, the
#'   UK2GTFS package contains an internal dataset of the TIPLOC locations with
#'   better location accuracy, which are used by default.
#'
#'   However you can also specify `locations = "file"` to use the TIPLOC
#'   locations in the ATOC data or provide an SF data frame of your own.
#'
#'   Or you can provide your own sf data frame of points in the same format as
#'   `tiplocs` or a path to a csv file formatted like a GTFS stops.txt
#'
#'   Agency
#'
#'   The ATOC files do not contain the necessary information to build the
#'   agency.txt file. Therefore this data is provided with the package. You can
#'   also pass your own data frame of agency information.
#'
#'   Fares
#'
#'   The timetable feed contains no fares; these come in a separate fares
#'   feed available from the same National Rail Data Portal. Pass its path
#'   as `fares` to add GTFS fare tables to the output. See
#'   [gtfs_add_railfares()] for the fare model, the choices exposed by the
#'   `fares_*` arguments, and the limitations.
#' 
#'   Shapes
#' 
#'   The ATOC data does not contain any shape information. If `shapes = TRUE`,
#'   the function will attempt to build shapes.txt from an internal map of the
#'   rail network.
#'
#' @md
#' @export

atoc2gtfs <- function(path_in,
                      silent = TRUE,
                      ncores = 1,
                      locations = "tiplocs",
                      agency = "atoc_agency",
                      shapes = FALSE,
                      transfers = TRUE,
                      missing_tiplocs = TRUE,
                      working_timetable = FALSE,
                      public_only = TRUE,
                      fares = NULL,
                      fares_version = 1,
                      fares_ticket_codes = NULL,
                      fares_ticket_class = "standard",
                      fares_ticket_type = NULL,
                      fares_walkup_only = TRUE,
                      fares_rider_categories = c("adult", "child"),
                      fares_railcards = NULL,
                      fares_ndf = TRUE,
                      fares_travel_date = NULL,
                      fares_travel_time = NULL,
                      fares_booking_date = NULL) {
  # Checkmates
  checkmate::assert_character(path_in, len = 1)
  checkmate::assert_file_exists(path_in)
  checkmate::assert_logical(silent)
  checkmate::assert_numeric(ncores, lower = 1)
  checkmate::assert_logical(shapes)
  checkmate::assert_character(fares, len = 1, null.ok = TRUE)
  # validate the fares scenario now, before the (slow) timetable build
  railfares_check_scenario(fares_travel_date, fares_travel_time,
                           fares_booking_date)

  if (ncores == 1) {
    message(paste0(
      Sys.time(),
      " This will take some time, make sure you use 'ncores' to enable multi-core processing"
    ))
  }

  agency = getCachedAgencyData( agency )

  if ( !inherits(locations, "character") || "file"!=locations )
  {
    stops_sf = getCachedLocationData( locations )
    stops_sf = sf::st_drop_geometry(stops_sf)
    if("geometry" %in% names(stops_sf)){
      stops_sf$geometry = NULL
    }
  }

  # Is input a zip or a folder
  if (grepl("\\.zip$", path_in, ignore.case = TRUE)) {
    # Unzip to a temporary folder
    exdir <- file.path(tempdir(), "uk2gtfs_atoc")
    unlink(exdir, recursive = TRUE)
    dir.create(exdir)
    files <- utils::unzip(path_in, exdir = exdir)
    on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  } else {
    # folder
    files <- list.files(path_in, full.names = TRUE)
  }

  # Are all the files we would expect there?
  files.ext <- tolower(substr(files, nchar(files) - 3, nchar(files)))
  # ".alf", ".dat", ".set", ".ztr", ".tsi" Not used
  files.ext.need <- c(".flf", ".mca", ".msn")

  if (!all(files.ext.need %in% files.ext)) {
    # Missing Some files
    files.ext.missing <- files.ext.need[!files.ext.need %in% files.ext]
    stop(paste0(
      "Missing files with the extension(s) ",
      paste(files.ext.missing, collapse = " ")
    ))
  }

  # Read In each File
  # alf <- importALF(files[grepl(".alf", files)])
  # ztr = importMCA(files[grepl(".ztr",files)], silent = silent)

  if(transfers){
    flf <- importFLF(files[grepl(".flf", files, ignore.case = TRUE)])
  }

  mca <- importMCA(
      file = files[grepl(".mca", files, ignore.case = TRUE)],
      silent = silent,
      ncores = 1,
      full_import = TRUE,
      working_timetable = working_timetable,
      public_only = public_only
  )


  # Should the file be checked
  if ( TRUE==missing_tiplocs ||
       ( inherits(locations, "character") && "file"==locations ) )
  {
    msn <- importMSN(files[grepl(".msn", files, ignore.case = TRUE)], silent = silent)
    station <- msn[[1]]
    TI <- mca[["TI"]]
    stops.list <- station2stops(station = station, TI = TI)
    stops_file <- stops.list[["stops"]]
    rm(msn,TI,stops.list)

    if( FALSE==missing_tiplocs || !exists("stops_sf") )
    {
      stops <- stops_file
    }
    else
    {
      # Combine
      stops_missing <- stops_file[!stops_file$stop_id %in% stops_sf$stop_id,]
      if(nrow(stops_missing) > 0){
        message("Adding ",nrow(stops_missing)," missing tiplocs, these may have unreliable location data")
        stops <- rbind(stops_sf, stops_missing)
      } else {
        stops <- stops_sf
      }
    }
  } else {
    stops <- stops_sf
  }


  # Construct the GTFS
  stop_times <- mca[["stop_times"]]
  schedule <- mca[["schedule"]]
  rm(mca)
  gc()
  # rm(alf, flf, mca, msn)

  stop_times <- stop_times[, c(
    "Arrival Time",
    "Departure Time",
    "Location", "stop_sequence",
    "Activity", "rowID", "schedule"
  )]
  names(stop_times) <- c(
    "arrival_time", "departure_time", "stop_id",
    "stop_sequence", "Activity", "rowID", "schedule"
  )

  # remove any unused stops
  stops <- stops[stops$stop_id %in% stop_times$stop_id, ]

  if ( nrow(stops)<=0 )
  {
    stop("Could not match any stops in input data to stop database.")
  }


  # Main Timetable Build
  timetables <- schedule2routes(
    stop_times = stop_times,
    stops = stops,
    schedule = schedule,
    silent = silent,
    ncores = ncores,
    public_only = public_only
  )
  rm(schedule)
  gc()

  # TODO: check for stop_times that are not valid stops

  timetables$agency <- agency
  timetables$stops <- stops

  if (transfers) {
    if(!exists("station")){
      msn <- importMSN(files[grepl(".msn", files, ignore.case = TRUE)], silent = silent)
      station <- msn[[1]]
    }
    timetables$transfers <- station2transfers(station = station, flf = flf)
  }
 

  # Add Fares
  if (!is.null(fares)) {
    timetables <- gtfs_add_railfares(
      timetables,
      fares,
      fares_version = fares_version,
      ticket_codes = fares_ticket_codes,
      ticket_class = fares_ticket_class,
      ticket_type = fares_ticket_type,
      walkup_only = fares_walkup_only,
      rider_categories = fares_rider_categories,
      railcards = fares_railcards,
      ndf = fares_ndf,
      travel_date = fares_travel_date,
      travel_time = fares_travel_time,
      booking_date = fares_booking_date,
      silent = silent
    )
  }

  # Build Shapes
  if (shapes) {
    message("Building shapes.txt, this may take some time...")
    timetables = ATOC_shapes(timetables)
  }

  return(timetables)

}
