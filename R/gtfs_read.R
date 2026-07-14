#' Read GTFS
#'
#' Read in a GTFS zip  file
#'
#' @param path character, path to GTFS zip folder
#' @return a gtfs object: a named list of data frames, one per GTFS table
#' @details The core tables are read with explicit column types (ids as
#'   character, coordinates as numeric, etc.); any column type not listed is
#'   left to fread's detection. Times of day (stop_times arrival/departure,
#'   frequencies start/end) are returned as lubridate Periods so values past
#'   24:00:00 are preserved. Tables without an explicit specification are
#'   read with automatic types except that `*_id` columns are always
#'   character.
#' @export
#'

gtfs_read <- function(path){
  checkmate::assert_file_exists(path)

  tmp_folder <- file.path(tempdir(),"gtfsread")
  unlink(tmp_folder, recursive = TRUE) # remove any stale files from a previous read
  dir.create(tmp_folder)
  utils::unzip(path, exdir = tmp_folder)

  files <- list.files(tmp_folder, pattern = ".txt")

  # Read one GTFS table, applying the given column types to the columns that
  # are actually present (passing absent names to fread raises warnings)
  read_table <- function(file, col_classes, data_table = TRUE){
    fpath <- file.path(tmp_folder, file)
    header <- names(data.table::fread(fpath, nrows = 0, header = TRUE))
    col_classes <- col_classes[names(col_classes) %in% header]
    data.table::fread(
      fpath,
      colClasses = col_classes,
      showProgress = FALSE,
      sep = ',',
      header = TRUE,
      data.table = data_table
    )
  }

  gtfs <- list()

  if(checkmate::test_file_exists(file.path(tmp_folder,"agency.txt"))){
    gtfs$agency <- read_table("agency.txt",
      c(agency_id = "character",
        agency_noc = "character"))
  } else {
    warning("Unable to find required file: agency.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stops.txt"))){
    gtfs$stops <- read_table("stops.txt",
      c(stop_id = "character",
        stop_code = "character",
        stop_name = "character",
        stop_lat = "numeric",
        stop_lon = "numeric",
        wheelchair_boarding = "integer",
        location_type = "integer",
        parent_station = "character",
        platform_code = "character"))
  } else {
    warning("Unable to find required file: stops.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"routes.txt"))){
    gtfs$routes <- read_table("routes.txt",
      c(route_id = "character",
        agency_id = "character",
        route_short_name = "character",
        route_long_name = "character",
        route_type = "integer"))
  } else {
    warning("Unable to find required file: routes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"trips.txt"))){
    gtfs$trips <- read_table("trips.txt",
      c(trip_id = "character",
        route_id = "character",
        service_id = "character",
        block_id = "character",
        shape_id = "character",
        wheelchair_accessible = "integer"))
  } else {
    warning("Unable to find required file: trips.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stop_times.txt"))){
    gtfs$stop_times <- read_table("stop_times.txt",
      c(trip_id = "character",
        stop_id = "character",
        stop_sequence = "integer",
        departure_time = "character",
        arrival_time = "character",
        shape_dist_traveled = "numeric",
        timepoint = "integer",
        pickup_type = "integer",
        drop_off_type = "integer",
        stop_headsign = "character",
        stop_direction_name = "character"),
      data_table = FALSE # Data table causes problems with lubridate
    )

    gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
    gtfs$stop_times$departure_time <- lubridate::hms(gtfs$stop_times$departure_time)

  } else {
    warning("Unable to find required file: stop_times.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar.txt"))){
    gtfs$calendar <- read_table("calendar.txt",
      c(service_id = "character",
        monday = "integer",
        tuesday = "integer",
        wednesday = "integer",
        thursday = "integer",
        friday = "integer",
        saturday = "integer",
        sunday = "integer",
        start_date = "character",
        end_date = "character"))

    gtfs$calendar[, start_date := as.IDate(start_date, "%Y%m%d")]
    gtfs$calendar[, end_date := as.IDate(end_date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar_dates.txt"))){
    gtfs$calendar_dates <- read_table("calendar_dates.txt",
      c(service_id = "character",
        date = "character",
        exception_type = "integer"))
    gtfs$calendar_dates[, date := as.IDate(date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar_dates.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"frequencies.txt"))){
    gtfs$frequencies <- read_table("frequencies.txt",
      c(trip_id = "character",
        start_time = "character",
        end_time = "character",
        headway_secs = "integer",
        exact_times = "integer"),
      data_table = FALSE # consistent with stop_times: Period columns are S4
    )

    gtfs$frequencies$start_time <- lubridate::hms(gtfs$frequencies$start_time)
    gtfs$frequencies$end_time <- lubridate::hms(gtfs$frequencies$end_time)

  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"shapes.txt"))){
    gtfs$shapes <- read_table("shapes.txt",
      c(shape_id = "character",
        shape_pt_lat = "numeric",
        shape_pt_lon = "numeric",
        shape_pt_sequence = "integer",
        shape_dist_traveled = "numeric"))
  }


  #load any other tables in the .zip file
  filenamesOnly <- tools::file_path_sans_ext(basename(files))
  notLoadedFiles = setdiff(  filenamesOnly, names(gtfs) )

  for (fileName in notLoadedFiles)
  {
    table <- data.table::fread(
      file.path(tmp_folder, paste0(fileName, ".txt")),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

    # ID columns must be character: fread type-guesses numeric-looking ids
    # (e.g. an all-digit trip_id), which then fail to join against the
    # character ids of the core tables
    id_cols <- grep("_id$", names(table), value = TRUE)
    for (idc in id_cols) {
      if (!is.character(table[[idc]])) {
        data.table::set(table, j = idc, value = as.character(table[[idc]]))
      }
    }

    gtfs[[fileName]] <- table
  }

  #remove temp directory
  unlink(tmp_folder, recursive = TRUE)

  return(gtfs)
}
