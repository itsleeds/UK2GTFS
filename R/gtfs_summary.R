


#' Summarise a GTFS object
#'
#' Prints a short summary of a GTFS object: the number of rows in each table
#' and the range of dates covered by the calendar.
#'
#' @param gtfs a gtfs object
#' @return Invisibly returns NULL, called for its printed output
#' @export
gtfs_summary = function(gtfs){
  rows <- lapply(gtfs, nrow)
  rows <- unlist(rows)
  message("Tables and number of rows:")
  print(rows)

  message("Dates:")
  message("All From ",min(gtfs$calendar$start_date)," to ",max(gtfs$calendar$end_date))
  message("80% From ",as.Date(stats::quantile(as.numeric(gtfs$calendar$start_date), 0.1)),
          " to ",
          as.Date(stats::quantile(as.numeric(gtfs$calendar$end_date), 0.9)))
  invisible(NULL)
}
