# fls <- list.files("R", full.names = TRUE)
# for(fl in fls){source(fl)}
#' Import the .CIF file
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param path Path to zipped folder for NPTDR data
#' @param silent Logical, should messages be returned
#' @param n_files debug option numerical vector for files to be passed e.g. 1:10
#' @param enhance_stops Logical, if TRUE will download current NaPTAN to add in any missing stops
#' @param naptan Naptan Locations from get_naptan()
#' @return A gtfs object: a named list of data frames representing the tables
#'   of a GTFS file
#'
#' @export
nptdr2gtfs <- function(path = "D:/OneDrive - University of Leeds/Data/UK2GTFS/NPTDR/October-2006.zip",
                       silent = FALSE,
                       n_files = NULL,
                       enhance_stops = TRUE,
                       naptan = get_naptan()){

  checkmate::assert_file_exists(path, extension = "zip")
  dir.create(file.path(tempdir(),"nptdr_temp"))

  utils::unzip(path, exdir = file.path(tempdir(),"nptdr_temp"))

  # List the files

  fls <- list.files(file.path(tempdir(),"nptdr_temp"), recursive = TRUE, full.names = TRUE)
  fls <- fls[!grepl("MACOSX",fls)]

  fls_admin <- fls[grepl("Admin_Area",fls)]
  fls_naptan <- fls[grepl("naptan",fls, ignore.case = TRUE)]
  fls_ng <- fls[grepl("ng",fls, ignore.case = TRUE)]

  #2007 Fix
  fls_naptan <- fls_naptan[!grepl("Stops Data",fls_naptan)]
  fls_naptan <- fls_naptan[!grepl("xml",fls_naptan)]

  # Check all present
  if(length(fls_naptan) != 1){
    stop(length(fls_naptan)," NaPTAN files found")
  }

  if(length(fls_ng) != 1){
    warning(length(fls_ng)," NG files found")
  }

  if(!length(fls_admin) > 1){
    stop(length(fls_admin)," Admin Area files found")
  }

  # Unzip the Admin Area files
  dir.create(file.path(tempdir(),"nptdr_temp","areas"))

  for(i in seq_len(length(fls_admin))){
    utils::unzip(fls_admin[i], exdir = file.path(tempdir(),"nptdr_temp","areas"))
  }

  fls_cif <- list.files(file.path(tempdir(),"nptdr_temp","areas"),
                        recursive = TRUE, full.names = TRUE,
                        pattern = ".CIF")

  if(!length(fls_cif) >= 1){
    stop(length(fls_cif)," No CIF files found")
  } else {
    message(length(fls_cif)," CIF files found")
  }

  # Import the NaPTAN
  stops <- nptdr_naptan_import(fls_naptan)


  if(!silent){
    message(Sys.time()," Importing timetables")
  }

  # Import each CIF file
  if(is.null(n_files)){
    res <- purrr::map(fls_cif, importCIF, .progress = "Reading files ",
                      warn_missing_stops = !enhance_stops)
  } else {
    res <- purrr::map(fls_cif[n_files], importCIF, .progress = "Reading files ",
                      warn_missing_stops = !enhance_stops)
  }

  unlink(file.path(tempdir(),"nptdr_temp"), recursive = TRUE)


  if(!silent){
    message(Sys.time()," Processing results")
  }

  stop_times <- purrr::map(res, `[[`, "stop_times")
  location <- purrr::map(res, `[[`, "locations")
  schedule <- purrr::map(res, `[[`, "schedule")
  exceptions <- purrr::map(res, `[[`, "Exceptions")

  names(stop_times) <- as.character(seq_len(length(stop_times)))
  stop_times <- dplyr::bind_rows(stop_times, .id = "file_id")
  stop_times$schedule <- paste0(stop_times$file_id,"_",stop_times$schedule)
  stop_times$rowID <- paste0(stop_times$file_id,"_",stop_times$rowID)

  names(schedule) <- as.character(seq_len(length(schedule)))
  schedule <- dplyr::bind_rows(schedule, .id = "file_id")
  schedule$schedule <- paste0(schedule$file_id,"_",schedule$rowID)
  schedule$uid <- paste0(schedule$file_id,"_",schedule$uid)

  names(exceptions) <- as.character(seq_len(length(exceptions)))
  exceptions <- dplyr::bind_rows(exceptions, .id = "file_id")
  if (nrow(exceptions) > 0) {
    exceptions$schedule <- paste0(exceptions$file_id,"_",exceptions$schedule)
  }

  timetables <- nptdr_schedule2routes(
    stop_times = stop_times,
    schedule = schedule,
    exceptions = exceptions
  )

  stops <- stops[stops$stop_id %in% unique(timetables$stop_times$stop_id),]
  location <- dplyr::bind_rows(location, .id = "file_id")
  location <- location[,c("stop_id","stop_name","easting","northing")]
  location <- location[!duplicated(location$stop_id),]
  location$stop_code <- NA_character_

  # Add in any missing stops
  location_extra <- location[!location$stop_id %in% stops$stop_id,]








  if(nrow(location_extra) > 0){
    # Most location have 5/6 digit coordinates by some have 8
    location_extra <- location_extra[!is.na(location_extra$easting),]
    location_extra$easting <- trimws(location_extra$easting)
    location_extra$northing <- trimws(location_extra$northing)
    location_extra$nchar <- nchar(location_extra$easting)
    location_extra <- location_extra[location_extra$nchar > 4,]
    suppressWarnings(location_extra$easting <- as.numeric(location_extra$easting))
    suppressWarnings(location_extra$northing <- as.numeric(location_extra$northing))
    location_extra <- location_extra[!is.na(location_extra$northing) & !is.na(location_extra$easting), ]
    location_extra$easting <- dplyr::if_else(location_extra$nchar == 8, location_extra$easting/100, location_extra$easting)
    location_extra$northing <- dplyr::if_else(location_extra$nchar == 8, location_extra$northing/100, location_extra$northing)
  }


  if(nrow(location_extra) > 0){
    location_extra <- sf::st_as_sf(location_extra, coords = c("easting","northing"), crs = 27700)
    location_extra <- sf::st_transform(location_extra, 4326)
    location_extra <- cbind(sf::st_drop_geometry(location_extra), sf::st_coordinates(location_extra))
    names(location_extra) <- c("stop_id","stop_name","stop_code","stop_lon","stop_lat")

    # Filter out stops outside the UK
    ukbbox = c(-9,49,2,61)
    location_extra <- location_extra[
      location_extra$stop_lon > ukbbox[1] &
        location_extra$stop_lat > ukbbox[2] &
        location_extra$stop_lon < ukbbox[3] &
        location_extra$stop_lat < ukbbox[4]
      ,]

    location_extra <- location_extra[,names(stops)]
    location_extra <- location_extra[!is.na(location_extra$stop_lon),]
    stops <- rbind(stops, location_extra)
  }

  if(enhance_stops){
    stops_missing <- unique(timetables$stop_times$stop_id[!timetables$stop_times$stop_id %in% stops$stop_id])

    if(length(stops_missing) > 0){
      naptan <- naptan[naptan$stop_id %in% stops_missing,]

      if(inherits(naptan, "data.table")){
        naptan = naptan[, names(stops), with = FALSE]
      } else {
        naptan <- naptan[,names(stops)]
      }

      if(nrow(naptan) > 0){
        stops <- rbind(stops, naptan)
      }

    }
  }

  stops_missing <- unique(timetables$stop_times$stop_id[!timetables$stop_times$stop_id %in% stops$stop_id])
  if(length(stops_missing) > 0){
    warning(length(stops_missing)," stops (",round(length(stops_missing)/nrow(stops)*100,1),
            "%) in stop_times.txt are missing from stops.txt\n",
            "These are likley to be temporary or moveable stops\n",
            "Use gtfs_clean to remove them")
  }
  timetables$stops <- stops


  return(timetables)



}


#' Import CIF naptan
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param path_naptan Path to naptan file
#' @param ukbbox Bounding box for the UK
#' @noRd
nptdr_naptan_import <- function(path_naptan, ukbbox = c(-9,49,2,61)){

  dir.create(file.path(tempdir(),"nptdr_temp","naptan"))

  utils::unzip(path_naptan, exdir = file.path(tempdir(),"nptdr_temp","naptan"))

  flnap <- list.files(file.path(tempdir(),"nptdr_temp","naptan"), recursive = TRUE, full.names = TRUE)
  flnap <- flnap[grepl("stops.csv", flnap, ignore.case = TRUE)]

  naptan_stops <- utils::read.csv(flnap)
  naptan_stops <- naptan_stops[,c("ATCOCode","Lon","Lat","CommonName","SMSNumber")]
  names(naptan_stops) <- c("stop_id","stop_lon","stop_lat","stop_name","stop_code")

  # Filter out stops outside the UK
  naptan_stops <- naptan_stops[
                 naptan_stops$stop_lon > ukbbox[1] &
                 naptan_stops$stop_lat > ukbbox[2] &
                 naptan_stops$stop_lon < ukbbox[3] &
                 naptan_stops$stop_lat < ukbbox[4]
                 ,]

  unlink(file.path(tempdir(),"nptdr_temp","naptan"), recursive = TRUE)

  return(naptan_stops)

}


#' Convert parsed Latin-1 columns to UTF-8
#'
#' @details
#' ATCO-CIF is Latin-1 and its fixed-width layout is byte-based, so records
#' must be field-split as single-byte Latin-1 and only then converted to
#' UTF-8. Converting whole records first shifts every field after an
#' accented character (which becomes two bytes) and can even slice a
#' multi-byte character in half at a field boundary, creating invalid
#' UTF-8 that aborts later regex operations.
#'
#' @param df data.frame of raw parsed columns from iotools::dstrfw()
#' @noRd
latin1_cols_to_utf8 <- function(df) {
  chr <- vapply(df, is.character, logical(1))
  df[chr] <- lapply(df[chr], iconv, from = "latin1", to = "UTF-8")
  df
}

#' Pad truncated fixed-width records
#'
#' @details
#' Some NPTDR files contain records with trailing fields missing (e.g. the
#' 2006 archive has QL records without the final NatGazID field), which
#' makes iotools::dstrfw() error with "input line is too short". Pad such
#' records with spaces to the full record width so the missing fields
#' parse as blank.
#'
#' @param x character vector of records of one type
#' @param width full fixed-width record length (sum of dstrfw widths)
#' @noRd
pad_fixed_width <- function(x, width) {
  len <- nchar(x, type = "bytes")
  short <- !is.na(len) & len < width
  if (any(short)) {
    pad <- paste0(x[short], strrep(" ", width - len[short]))
    # paste0() re-encodes marked latin1 strings as UTF-8, which would break
    # the byte-based field alignment; convert those back to single-byte
    fix <- Encoding(pad) == "UTF-8"
    pad[fix] <- iconv(pad[fix], from = "UTF-8", to = "latin1")
    x[short] <- pad
  }
  x
}

#' Import the .CIF file
#'
#' @details
#' Imports the CIF file and returns data.frame
#'
#' @param file Path to .CIF file
#' @param warn_missing_stops logical, should warning be given for missing stops?
#' @noRd
importCIF <- function(file, warn_missing_stops = FALSE ) {

  # see https://slideplayer.com/slide/14931535/
  raw <- readLines(
    con = file,
    n = -1
  )
  # ATCO-CIF files are Latin-1 encoded and the fixed-width layout is
  # byte-based, so keep the single-byte Latin-1 records for field-splitting;
  # latin1_cols_to_utf8() converts the parsed columns afterwards
  Encoding(raw) <- "latin1"
  types <- substr(raw, 1, 2)

  # Sometime the file is empty
  if(length(raw) < 3){
    return(NULL)
  }

  # AT     QB     QI     QL     QO     QS     QT     VS
  # 1   4410 478789   4410  11692  11692  11692      1
  known_types <- c("QS","QO","QI","QT","QB","QL","AT","VS","QV","QE","QR")
  types_have <- unique(types)

  if(any(!types_have %in% known_types)){
    stop("Unknown types in types ",paste(types_have[!types_have %in% known_types], collapse = ", "))
  }

  # if(any(!known_types %in% types_have)){
  #   stop("Missing types in types ",paste(known_types[!known_types %in% types_have], collapse = ", "))
  # }

  # Mode information from file name
  file_mode = strsplit(file,"/", fixed = TRUE)[[1]]
  file_mode = file_mode[length(file_mode)]
  file_mode = strsplit(file,"_", fixed = TRUE)[[1]]
  file_mode = file_mode[length(file_mode)]
  file_mode = gsub(".CIF","",file_mode, fixed = TRUE)

  # QS - Service Header
  # QO - Origin
  # QT - Terminating
  # QI - Intermediate
  # QN - Note
  # QE - Running date (exceptions)
  # QR - Bus Repetitions
  # QP - Bus Operator
  # QD - Route Description
  # QV - Vehicle Type
  # QH - Bank Holiday
  # QL, QA, QB, QC, QG, QJ, QW - Location


  QS <- pad_fixed_width(raw[types == "QS"], 65) # Service
  QO <- pad_fixed_width(raw[types == "QO"], 25) # Origin
  QI <- pad_fixed_width(raw[types == "QI"], 29) # Intermediate
  QT <- pad_fixed_width(raw[types == "QT"], 25) # Terminate

  QB <- pad_fixed_width(raw[types == "QB"], 79) # Location
  QL <- pad_fixed_width(raw[types == "QL"], 73) # Location
  #QV <- raw[types == "QV"] # Vehicle Type
  QE <- pad_fixed_width(raw[types == "QE"], 19) # Running date (exceptions)
  QR <- pad_fixed_width(raw[types == "QR"], 38) # Bus Repetitions


  HD <- pad_fixed_width(raw[1], 74) # Some info not sure what
  tl <- raw[length(raw)]

  # Header
  HD <- iotools::dstrfw(
    x = HD,
    col_types = rep("character", 7),
    widths = c(
      8,2,2,32,16,8,6
    )
  )
  names(HD) <- c(
    "file_type","major_version","minor_version",
    "originator","source","date","time")
  HD <- latin1_cols_to_utf8(HD)


  # Journey Header, Service
  QS <- iotools::dstrfw(
    x = QS,
    col_types = rep("character", 14),
    widths = c(
      2,1,4,6,8,8,7,1,1,4,6,8,8,1
    )
  )
  names(QS) <- c(
    "recordID","transaction_type","operator_code", "uid", "start_date",
    "end_date", "days_operation", "school_term_time", "bank_holiday",
    "service_number", "running_board", "vehicle_type", "registation_number",
    "route_direction" )

  QS$recordID <- NULL
  QS <- latin1_cols_to_utf8(QS)
  QS <- strip_whitespace(QS)

  # clean data
  QS$start_date <- as.Date(QS$start_date, format = "%Y%m%d")
  QS$end_date <- as.Date(QS$end_date, format = "%Y%m%d")

  # Add the rowid
  QS$rowID <- seq(from = 1, to = length(types))[types == "QS"]

  # Add vehicle type if missing
  # vt <- substr(hd, 41, 46)
  # vt <- trimws(vt)
  # if(nchar(vt) == 0){
  #   vt <- file_mode
  # }
  good_types <- c("BUS","COACH","TRAIN","Bus","FERRY","METRO","Tram")

  QS$vehicle_type[!QS$vehicle_type %in% good_types] <- file_mode


  # Origin Stop
  QO <- iotools::dstrfw(
    x = QO,
    col_types = rep("character", 6),
    widths = c(2,12,4,3,2,2)
  )
  names(QO) <- c(
    "recordID", "stop_id", "departure_time", "bay_number",
    "timing_point", "fare_stage"
  )
  QO$recordID <- NULL
  QO <- latin1_cols_to_utf8(QO)
  QO <- strip_whitespace(QO)

  # Add the rowid
  QO$rowID <- seq(from = 1, to = length(types))[types == "QO"]

  # Terminating Stop
  QT <- iotools::dstrfw(
    x = QT,
    col_types = rep("character", 6),
    widths = c(2,12,4,3,2,2)
  )
  names(QT) <- c(
    "recordID", "stop_id", "arrival_time", "bay_number",
    "timing_point", "fare_stage"
  )
  QT$recordID <- NULL
  QT <- latin1_cols_to_utf8(QT)
  QT <- strip_whitespace(QT)

  # Add the rowid
  QT$rowID <- seq(from = 1, to = length(types))[types == "QT"]

  # Intermediate Stop
  QI <- iotools::dstrfw(
    x = QI,
    col_types = rep("character", 8),
    widths = c(2,12,4,4,1,3,2,1)
  )
  names(QI) <- c(
    "recordID", "stop_id", "arrival_time","departure_time",
    "activity","bay_number","timing_point", "fare_stage"
  )
  QI$recordID <- NULL

  QI <- latin1_cols_to_utf8(QI)
  QI <- strip_whitespace(QI)

  # Add the rowid
  QI$rowID <- seq(from = 1, to = length(types))[types == "QI"]

  # Location
  QL <- iotools::dstrfw(
    x = QL,
    col_types = rep("character", 7),
    widths = c(2,1,12,48,1,1,8)
  )
  names(QL) <- c(
    "recordID","transaction_type", "stop_id","stop_name","gazetteer_code",
    "point_type","NatGazID"
  )
  QL$recordID <- NULL
  QL <- latin1_cols_to_utf8(QL)
  QL <- strip_whitespace(QL)

  # Additional Location
  QB <- iotools::dstrfw(
    x = QB,
    col_types = rep("character", 7),
    widths = c(2,1,12,8,8,24,24)
  )
  names(QB) <- c(
    "recordID","transaction_type","stop_id","easting","northing","district","town"
  )
  QB$recordID <- NULL
  QB$unknown <- NULL
  QB <- latin1_cols_to_utf8(QB)
  QB <- strip_whitespace(QB)

  # Exceptions
  if(length(QE) > 0){
    QE <- iotools::dstrfw(
      x = QE,
      col_types = rep("character", 4),
      widths = c(2,8,8,1)
    )
    names(QE) <- c("recordID", "start_date", "end_date", "exception_type")
    QE$recordID <- NULL
    QE <- latin1_cols_to_utf8(QE)
    QE$start_date <- as.Date(QE$start_date, format = "%Y%m%d")
    QE$end_date <- as.Date(QE$end_date, format = "%Y%m%d")
    QE$rowID <- seq(from = 1, to = length(types))[types == "QE"]
    QE$schedule <- as.integer(as.character(cut(QE$rowID,
                                               c(QS$rowID, length(raw)),
                                               labels = QS$rowID
    )))
  } else {
    QE <- NULL
  }

  # Repetitions
  if(length(QR) > 0){
    QR <- iotools::dstrfw(
      x = QR,
      col_types = rep("character", 6),
      widths = c(2,12,4,6,6,8)
    )
    names(QR) <- c("recordID", "Location", "departure_time", "uid","running_board","vehicle_type")
    QR$recordID <- NULL
    QR <- latin1_cols_to_utf8(QR)
    QR$rowID <- seq(from = 1, to = length(types))[types == "QR"]
    QR$schedule <- as.integer(as.character(cut(QR$rowID,
                                               c(QS$rowID, length(raw)),
                                               labels = QS$rowID
    )))
    QR = strip_whitespace(QR)

  } else {
    QR <- NULL
  }


  stop_times <- dplyr::bind_rows(list(QO, QI, QT))
  stop_times <- stop_times[order(stop_times$rowID), ]
  stop_times$schedule <- as.integer(as.character(cut(stop_times$rowID,
                                                     c(QS$rowID, length(raw)),
                                                     labels = QS$rowID
  )))
  stop_times$arrival_time <- dplyr::if_else(is.na(stop_times$arrival_time),
                                            stop_times$departure_time,
                                            stop_times$arrival_time)

  stop_times$departure_time <- dplyr::if_else(is.na(stop_times$departure_time),
                                            stop_times$arrival_time,
                                            stop_times$departure_time)

  # Fix bug where times are 0000 when no pickup
  stop_times$arrival_time <- dplyr::if_else(stop_times$arrival_time == "0000" &
                                              stop_times$activity %in% "U",
                                            stop_times$departure_time,
                                            stop_times$arrival_time)


  #Occasional bugs with times e.g 00-1 @ $
  suppressWarnings(chk <- is.na(as.numeric(stop_times$departure_time)))
  if(any(chk)){
    warning("Invalid departure_time (e.g '@   ') replaced with 0000")
  }
  stop_times$departure_time[chk] <- "0000"

  suppressWarnings(chk <- is.na(as.numeric(stop_times$arrival_time)))
  if(any(chk)){
    warning("Invalid arrival_time (e.g '@   ') replaced with 0000")
  }
  stop_times$arrival_time[chk] <- "0000"

  stop_times <- stop_times[order(stop_times$schedule,as.numeric(stop_times$departure_time)), ]
  stop_times$stop_sequence <- sequence(rle(stop_times$schedule)$lengths)

  # Add in repetitions
  if(!is.null(QR)){
    QR$schedule <- as.integer(QR$schedule)
    st_res <- list()
    sch_res <- list()
    for(i in seq_len(nrow(QR))){
      qr_sub <- QR[i,]
      st_sub <- stop_times[stop_times$schedule == qr_sub$schedule,]
      qs_sub <- QS[QS$rowID == qr_sub$schedule,]
      qr_sub$departure_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", qr_sub$departure_time))
      st_sub$departure_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", st_sub$departure_time))
      st_sub$arrival_time <- lubridate::hm(sub("(\\d{2})(\\d{2})", "\\1:\\2", st_sub$arrival_time))

      offset <- (qr_sub$departure_time - st_sub$departure_time[1])
      st_sub$departure_time <- st_sub$departure_time + offset
      st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$departure_time))
      st_sub$arrival_time <- st_sub$arrival_time + offset
      st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(st_sub$arrival_time))

      st_sub$departure_time <- sprintf("%02d%02d", st_sub$departure_time@day *
                                       24 + st_sub$departure_time@hour,
                                     lubridate::minute(st_sub$departure_time))

      st_sub$arrival_time <- sprintf("%02d%02d", st_sub$arrival_time@day *
                                       24 + st_sub$arrival_time@hour,
                                     lubridate::minute(st_sub$arrival_time))
      #st_sub$schedule <- qr_sub$schedule
      st_sub$schedule <- as.integer(qr_sub$rowID)
      qs_sub$rowID <- as.integer(qr_sub$rowID)
      qs_sub$uid <- paste0(qr_sub$uid,"_",i)
      qs_sub$running_board  <- ifelse(is.na(qr_sub$running_board),qs_sub$running_board,qr_sub$running_board)
      qs_sub$vehicle_type   <- ifelse(is.na(qr_sub$vehicle_type),qs_sub$vehicle_type,qr_sub$vehicle_type)

      st_res[[i]] <- st_sub
      sch_res[[i]] <- qs_sub

    }

    st_res <- dplyr::bind_rows(st_res)
    sch_res <- dplyr::bind_rows(sch_res)

    stop_times <- rbind(stop_times, st_res)
    QS <- rbind(QS, sch_res)

  }

  QB <- QB[!duplicated(QB$stop_id),] # Handle occasional duplicates
  locs <- dplyr::left_join(QL, QB, by = "stop_id")

  # Check for missing stops
  if(warn_missing_stops){
    chk <- unique(stop_times$stop_id)
    chk <- chk[!chk %in% locs$stop_id]
    if(length(chk) > 0){
      warning("In '..",substr(file,nchar(file) - 17,nchar(file)),"' there ",length(chk)," are stops without locations")
    }
  }

  results <- list(stop_times, locs, QS, HD, tl, QE)
  names(results) <- c("stop_times", "locations","schedule","Header", "Footer", "Exceptions")

  return(results)
}

#' Export ATOC schedule as GTFS
#'
#' @details
#' Export ATOC schedule as GTFS
#'
#' @param stop_times stop-times
#' @param schedule list of dataframes
#' @param exceptions exceptions df
#' @param silent logical
#' @param ncores number of cores to use
#' @noRd
#'
nptdr_schedule2routes <- function(stop_times, schedule, exceptions, silent = TRUE, ncores = 1) {


  ### SECTION 1: ###############################################################################
  # make stop_times.txt
  if (!silent) {
    message(paste0(Sys.time(), " Building stop_times"))
  }

  # Convert Activity to pickup_type and drop_off_type
  stop_times$activity[is.na(stop_times$activity) & stop_times$stop_sequence == 1] <- "TB" # No activity specified at start

  utils::data("activity_codes")
  upoffs <- clean_activities2(stop_times$activity)
  stop_times <- cbind(stop_times, upoffs)

  stop_times$arrival_time[is.na(stop_times$arrival_time)] <- stop_times$departure_time[is.na(stop_times$arrival_time)]
  stop_times$departure_time[is.na(stop_times$departure_time)] <- stop_times$arrival_time[is.na(stop_times$departure_time)]
  stop_times <- stop_times[, c("arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type", "rowID", "schedule")]

  stop_times <- stop_times[!(stop_times$pickup_type == 1 & stop_times$drop_off_type == 1), ]

  ### SECTION 2: ###############################################################################
  # make make the calendar.txt and calendar_dates.txt file from the schedule
  if (!silent) {
    message(paste0(Sys.time(), " Building calendar and calendar_dates"))
  }

  # Fill in missing data
  schedule$end_date <- dplyr::if_else(is.na(schedule$end_date),
                                      lubridate::ymd("2011-12-31"),
                                      schedule$end_date)

  utils::data(historic_bank_holidays)

  # build the calendar file
  res <- nptdr_makeCalendar(schedule = schedule,
                            exceptions = exceptions,
                            historic_bank_holidays = historic_bank_holidays)
  calendar <- res[[1]]
  calendar_dates <- res[[2]]

  # clean calendars
  calendar_dates_hash <- calendar_dates
  calendar_dates_hash$hash <- as.numeric(calendar_dates_hash$date) + calendar_dates_hash$exception_type / 10
  calendar_dates_hash <- calendar_dates_hash[,c("UID","hash")]

  calendar_dates_hash <- dplyr::group_by(calendar_dates_hash, UID)
  calendar_dates_hash <- dplyr::summarise(calendar_dates_hash,
                                          hash = digest::digest(hash, algo = "xxhash32"))

  calendar <- dplyr::left_join(calendar, calendar_dates_hash, by = "UID")
  calendar$hash[is.na(calendar$hash)] <- "no_exceptions"


  calendar$start_date <- as.character(calendar$start_date)
  calendar$start_date <- gsub("-", "", calendar$start_date)
  calendar$end_date <- as.character(calendar$end_date)
  calendar$end_date <- gsub("-", "", calendar$end_date)


  #calendar$service_id <- seq_len(nrow(calendar))
  calendar <- calendar[,c("UID","start_date","end_date",
                          "monday","tuesday","wednesday","thursday","friday",
                          "saturday","sunday","schedule","route_direction","hash")]

  calendar_unique <- unique(calendar[,c("start_date","end_date",
                                 "monday","tuesday","wednesday",
                                 "thursday","friday",
                                 "saturday","sunday","hash")])
  calendar_unique$service_id <- seq(1, nrow(calendar_unique))

  calendar <- dplyr::left_join(calendar, calendar_unique, by = c("start_date","end_date",
                                                                 "monday","tuesday","wednesday",
                                                                 "thursday","friday",
                                                                 "saturday","sunday","hash"))
  calendar_dates <- dplyr::left_join(calendar_dates, calendar[,c("UID","service_id")], by = "UID")
  calendar_dates <- calendar_dates[,c("service_id","date","exception_type")]
  calendar_dates <- unique(calendar_dates)

  names(calendar)[names(calendar) == "UID"] <- "trip_id"

  trips <- calendar

  calendar <- calendar[,c("service_id","start_date","end_date",
                          "monday","tuesday","wednesday","thursday","friday",
                          "saturday","sunday")]
  calendar <- calendar[!duplicated(calendar$service_id),]

  # Remove stop times for completely excluded trips
  stop_times <- stop_times[stop_times$schedule %in% trips$schedule,]

  trips <- trips[,c("trip_id","service_id","schedule","route_direction")]
  trips$direction_id <- 0
  trips$direction_id[trips$route_direction == "O"] <- 1
  trips$route_direction <- NULL

  ### SECTION 4: ###############################################################################
  # make make the routes.txt
  # find stop_tims with common start and end

  routes <- dplyr::group_by(stop_times, schedule)
  routes <- dplyr::summarise(routes,
                             from = stop_id[stop_sequence == min(stop_sequence)],
                             to = stop_id[stop_sequence == max(stop_sequence)],
                             n_stops = dplyr::n())

  routes <- dplyr::left_join(routes, schedule, by = "schedule")

  routes <- routes[,c("uid","from","to","n_stops","operator_code","service_number","running_board","vehicle_type")]
  routes <- routes[order(routes$from, routes$to, routes$operator_code, routes$service_number, routes$running_board, routes$vehicle_type),]
  routes$is_dup <- duplicated(routes[,c("from","to","n_stops","operator_code","service_number","running_board","vehicle_type")])

  routes$route_id <- seq_len(nrow(routes))
  breaks <- routes$route_id[!routes$is_dup]
  breaks <- breaks -1
  routes$route_id <- as.integer(as.character(cut(routes$route_id,
                                                 c(breaks, nrow(routes)),
                                                 labels = breaks
  ))) + 1

  routes_join <- routes[,c("uid","route_id")]
  routes_join <- routes_join[!duplicated(routes_join$uid),]
  routes <- dplyr::group_by(routes, route_id)
  routes <- dplyr::summarise(routes,
                             agency_id = unique(operator_code),
                             route_short_name = unique(service_number),
                             route_long_name = unique(running_board),
                             route_type = unique(vehicle_type)
                             )


  routes$route_type[routes$route_type == ""] <- NA

  routes$route_type <- sapply(routes$route_type, clean_route_type, guess_bus = TRUE)


  trips <- dplyr::left_join(trips, routes_join, by = c("trip_id" = "uid"))

  stop_times <- dplyr::left_join(stop_times, trips[,c("trip_id","schedule")], by = "schedule")


  agency <- data.frame(agency_id = unique(routes$agency_id))
  agency$agency_name <- agency$agency_id
  agency$agency_url <- "http://www.unknownsite.com"
  agency$agency_timezone <- "Europe/London"

  # Ditch unneeded columns
  routes <- routes[, c("route_id", "agency_id", "route_short_name", "route_long_name", "route_type")]
  trips <- trips[, c("trip_id", "route_id", "service_id","direction_id")]
  stop_times <- stop_times[, c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence", "pickup_type", "drop_off_type")]
  calendar <- calendar[, c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")]

  # Fill in missing routes longname
  routes$route_long_name <- dplyr::if_else(is.na(routes$route_long_name),
                                           "",
                                           routes$route_long_name)


  # Fix Times
  # NPTDR times are HHMM but afterMidnight() expects HHMMSS, so append seconds
  stop_times$arrival_time <- ifelse(nchar(stop_times$arrival_time) == 4,
                                    paste0(stop_times$arrival_time, "00"),
                                    stop_times$arrival_time)
  stop_times$departure_time <- ifelse(nchar(stop_times$departure_time) == 4,
                                      paste0(stop_times$departure_time, "00"),
                                      stop_times$departure_time)
  stop_times <- afterMidnight(stop_times)


  # end of function
  timetables <- list(calendar, calendar_dates, routes, stop_times, trips, agency)
  names(timetables) <- c("calendar", "calendar_dates", "routes", "stop_times", "trips","agency")
  return(timetables)
}



