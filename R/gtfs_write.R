#' Write GTFS
#'
#' Takes a list of data frames representing the GTFS format and saves them as GTFS
#' Zip file.
#'
#' @param gtfs named list of data.frames
#' @param folder folder to save the gtfs file to
#' @param name the name of the zip file without the .zip extension, default "gtfs"
#' @param stripComma logical, should commas be stripped from text, default = FALSE
#' @param stripTab logical, should tabs be stripped from text, default = FALSE
#' @param stripNewline logical, should newlines be stripped from text, default = FALSE
#' @param quote logical, should strings be quoted, default = TRUE, passed to data.table::fwrite
#' @return Invisibly returns NULL, called for the side effect of writing a zip file
#' @export
#'
gtfs_write <- function(gtfs,
                       folder = getwd(),
                       name = "gtfs",
                       stripComma = FALSE,
                       stripTab = FALSE,
                       stripNewline = FALSE,
                       quote = TRUE) {

  if (stripComma) {
    for (i in seq_len(length(gtfs))) {
      gtfs[[i]] <- stripCommas(gtfs[[i]])
    }
  }

  if (stripTab) {
    for (i in seq_len(length(gtfs))) {
      gtfs[[i]] <- stripTabs(gtfs[[i]], stripNewline)
    }
  }


  if (FALSE)
  {
    #Format times
    if(inherits(gtfs$stop_times$arrival_time, "Period")){
      gtfs$stop_times$arrival_time <- period2gtfs(gtfs$stop_times$arrival_time)
    }

    if(inherits(gtfs$stop_times$departure_time, "Period")){
      gtfs$stop_times$departure_time <- period2gtfs(gtfs$stop_times$departure_time)
    }

    if("frequencies" %in% names(gtfs))
    {
      if("difftime" %in% class(gtfs$frequencies$start_time)){
        gtfs$frequencies$start_time <- format(gtfs$frequencies$start_time, format = "%H:%M:%S")
      }

      if("difftime" %in% class(gtfs$frequencies$end_time)){
        gtfs$frequencies$end_time <- format(gtfs$frequencies$end_time, format = "%H:%M:%S")
      }
    }
  }

  # remove any stale files from a previous write before creating the folder,
  # otherwise they would be included in the new zip
  unlink(paste0(tempdir(), "/gtfs_temp"), recursive = TRUE)
  dir.create(paste0(tempdir(), "/gtfs_temp"))

  for ( tableName in names(gtfs) )
  {
    table <- gtfs[[tableName]]

    table <- formatAttributesToGtfsSchema( table )

    if ( !is.null(table) & nrow(table) > 0 )
    {
      data.table::fwrite(table, file.path(tempdir(), "gtfs_temp", paste0(tableName, ".txt")), row.names = FALSE, quote = quote)
    }
  }

  zip::zipr(paste0(folder, "/", name, ".zip"), list.files(paste0(tempdir(), "/gtfs_temp"), full.names = TRUE), recurse = FALSE)

  unlink(paste0(tempdir(), "/gtfs_temp"), recursive = TRUE)
}


#' Strip Commas
#'
#' Remove commas from data frame
#'
#' @param df data frame
#' @noRd
#'
stripCommas <- function(df) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "character")) {
      if(!all(validUTF8(x))){
        Encoding(x) <- "latin1"
        x <- enc2utf8(x)
      }
      x <- gsub(",", " ", x, fixed = TRUE)
    }
    return(x)
  })
  return(df)
}

#' Strip tabs
#'
#' Remove tabs from data frame
#'
#' @param df data frame
#' @param stripNewline logical
#' @noRd
#'
stripTabs <- function(df, stripNewline) {
  df[] <- lapply(df, function(x) {
    if (inherits(x, "character")) {
      if(!all(validUTF8(x))){
        Encoding(x) <- "latin1"
        x <- enc2utf8(x)
      }
      if(stripNewline){
        x <- gsub("[\r\n]", " ", x, fixed = FALSE)
      }
      x <- gsub("\t", " ", x, fixed = TRUE)
    }
    return(x)
  })
  return(df)
}


#' Convert Period to GTFS timestamps
#' When writing a 400mb (zipped) file, we spend nearly 4 minutes in this fn(), about 10x longer than writing the files to the filesystem.
#' profiler reports this being mostly nchar(), so we optimise down to one sprintf which reduces the time to 1 minute
#' .format() is about 7x slower than sprintf() in this situation. Performance may be different on a data.table
#'
#' @param x periods
#' @noRd
#'
period2gtfs <- function(x) {

  # Check for days
  dys <- lubridate::day(x)
  if(sum(dys, na.rm = TRUE) > 0){
    stop("Days detected in period objects, incorectly formatted period object")
  }

  return( sprintf("%02d:%02d:%02d", lubridate::hour(x), lubridate::minute(x), lubridate::second(x)) )
}


formatAttributesToGtfsSchema <- function(dt)
{

  {
    periodColumnsToFormat <- names(dt)[ sapply(dt, function(x){ inherits(x, "Period") }) ]

    if (length(periodColumnsToFormat) > 0)
    {
      dt[, (periodColumnsToFormat) := lapply(.SD, period2gtfs), .SDcols = periodColumnsToFormat]
    }
  }

  {
    diffTimeColumnsToFormat <- names(dt)[ sapply(dt, function(x){ "difftime" %in% class( x ) }) ]

    if (length(diffTimeColumnsToFormat) > 0)
    {
      dt[, (diffTimeColumnsToFormat) := lapply(.SD, format, format = "%H:%M:%S"), .SDcols = diffTimeColumnsToFormat]
    }
  }

  {
    dateColumnsToFormat <- names(dt)[ sapply(dt, function(x){ inherits(x, "Date") }) ]

    if (length(dateColumnsToFormat) > 0)
    {
      dt[, (dateColumnsToFormat) := lapply(.SD,
                     function(d){ as.integer(sprintf("%04d%02d%02d", lubridate::year(d), lubridate::month(d), lubridate::mday(d))) } ),
         .SDcols = dateColumnsToFormat]
      #performance, sprintf runs 6x faster than as.character() (which calls format()), followed by gsub()
      #coercing back to int prevents value being quoted in output file
    }
  }

  {
    factorColumnsToFormat <- names(dt)[ sapply(dt, function(x){ is.factor( x ) }) ]

    if (length(factorColumnsToFormat) > 0)
    {
      dt[, (factorColumnsToFormat) := lapply(.SD, as.character), .SDcols = factorColumnsToFormat]
    }
  }


  {
    logicalColumnsToFormat <- names(dt)[ sapply(dt, function(x){ is.logical( x ) }) ]

    if (length(logicalColumnsToFormat) > 0)
    {
      dt[, (logicalColumnsToFormat) := lapply(.SD, as.integer), .SDcols = logicalColumnsToFormat]
    }
  }

  return (dt)
}








