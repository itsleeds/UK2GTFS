#' Read GTFS
#'
#' Read in a GTFS zip  file
#'
#' @param path character, path to GTFS zip folder
#' @return a gtfs object: a named list of data frames, one per GTFS table
#' @export
#'

gtfs_read <- function(path){
  checkmate::assert_file_exists(path)

  tmp_folder <- file.path(tempdir(),"gtfsread")
  unlink(tmp_folder, recursive = TRUE) # remove any stale files from a previous read
  dir.create(tmp_folder)
  utils::unzip(path, exdir = tmp_folder)

  files <- list.files(tmp_folder, pattern = ".txt")

  gtfs <- list()

  if(checkmate::test_file_exists(file.path(tmp_folder,"agency.txt"))){

    # Read the first few rows of the file to check for column names
    sample_data <- data.table::fread(file.path(tmp_folder, "agency.txt"), nrows = 1, header = TRUE)
    col_classes <- c(agency_id = "character")
    if ("agency_noc" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, agency_noc = "character")
    }

    gtfs$agency <- data.table::fread(
      file.path(tmp_folder, "agency.txt"),
      colClasses = col_classes,
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: agency.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stops.txt"))){

    # Read the first few rows of the file to check for column names
    sample_data <- data.table::fread(file.path(tmp_folder, "stops.txt"), nrows = 1, header = TRUE)
    col_classes <- c(stop_id = "character",
                     stop_code = "character",
                     stop_name = "character",
                     stop_lat = "numeric",
                     stop_lon = "numeric")
    if ("wheelchair_boarding" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, wheelchair_boarding = "integer")
    }
    if ("location_type" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, location_type = "integer")
    }
    if ("parent_station" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, parent_station = "character")
    }
    if ("platform_code" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, platform_code = "character")
    }


    gtfs$stops <- data.table::fread(
      file.path(tmp_folder, "stops.txt"),
      colClasses = col_classes,
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: stops.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"routes.txt"))){

    gtfs$routes <- data.table::fread(
      file.path(tmp_folder, "routes.txt"),
      colClasses = c(
        route_id = "character",
        agency_id = "character",
        route_short_name = "character",
        route_long_name = "character",
        route_type = "integer"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: routes.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"trips.txt"))){

    # Read the first few rows of the file to check for column names
    sample_data <- data.table::fread(file.path(tmp_folder, "trips.txt"), nrows = 1, header = TRUE)
    col_classes <- c( trip_id = "character",
                      route_id = "character",
                      service_id = "character")
    if ("block_id" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, block_id = "character")
    }
    if ("shape_id" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, shape_id = "character")
    }
    if ("wheelchair_accessible" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, wheelchair_accessible = "integer")
    }


    gtfs$trips <- data.table::fread(
      file.path(tmp_folder, "trips.txt"),
      colClasses = col_classes,
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

  } else {
    warning("Unable to find required file: trips.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"stop_times.txt"))){

    # Read the first few rows of the file to check for column names
    sample_data <- data.table::fread(file.path(tmp_folder, "stop_times.txt"), nrows = 1, header = TRUE)
    col_classes <- c( trip_id = "character",
                      stop_id = "character",
                      stop_sequence = "integer",
                      departure_time = "character",
                      arrival_time = "character")
    if ("shape_dist_traveled" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, shape_dist_traveled = "numeric")
    }
    if ("timepoint" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, timepoint = "integer")
    }
    if ("pickup_type" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, pickup_type = "integer")
    }
    if ("drop_off_type" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, drop_off_type = "integer")
    }
    if ("stop_headsign" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, stop_headsign = "character")
    }
    if ("stop_direction_name" %in% colnames(sample_data)) {
      col_classes <- c(col_classes, stop_direction_name = "character")
    }

    gtfs$stop_times <- data.table::fread(
      file.path(tmp_folder, "stop_times.txt"),
      colClasses = col_classes,
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = FALSE # Data table causes problems with lubridate
    )

    gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
    gtfs$stop_times$departure_time <- lubridate::hms(gtfs$stop_times$departure_time)

  } else {
    warning("Unable to find required file: stop_times.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar.txt"))){

    gtfs$calendar <- data.table::fread(
      file.path(tmp_folder, "calendar.txt"),
      colClasses = c(
        service_id = "character",
        monday = "integer",
        tuesday = "integer",
        wednesday = "integer",
        thursday = "integer",
        friday = "integer",
        saturday = "integer",
        sunday = "integer",
        start_date = "character",
        end_date = "character"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

    gtfs$calendar[, start_date := as.IDate(start_date, "%Y%m%d")]
    gtfs$calendar[, end_date := as.IDate(end_date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"calendar_dates.txt"))){

    gtfs$calendar_dates <- data.table::fread(
      file.path(tmp_folder, "calendar_dates.txt"),
      colClasses = c(
        service_id = "character",
        date = "character",
        exception_type = "integer"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )
    gtfs$calendar_dates[, date := as.IDate(date, "%Y%m%d")]

  } else {
    message("Unable to find conditionally required file: calendar_dates.txt")
  }

  if(checkmate::test_file_exists(file.path(tmp_folder,"shapes.txt"))){

    gtfs$shapes <- data.table::fread(
      file.path(tmp_folder, "shapes.txt"),
      colClasses = c(
        shape_id = "character",
        shape_pt_lat = "numeric",
        shape_pt_lon = "numeric",
        shape_pt_sequence = "integer",
        shape_dist_traveled = "numeric"
      ),
      showProgress = FALSE,
      sep=',',
      header=TRUE,
      data.table = TRUE
    )

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

    gtfs[[fileName]] <- table
  }

  #remove temp directory
  unlink(tmp_folder, recursive = TRUE)

  return(gtfs)
}

