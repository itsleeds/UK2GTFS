


gtfs_summary = function(gtfs){
  rows <- lapply(gtfs, nrow)
  rows <- unlist(rows)
  message("Tables and number of rows:")
  print(rows)

  message("Dates:")
  message("All From ",min(gtfs$calendar$start_date)," to ",max(gtfs$calendar$end_date))
  message("80% From ",as.Date(quantile(as.numeric(gtfs$calendar$start_date), 0.1)),
          " to ",
          as.Date(quantile(as.numeric(gtfs$calendar$end_date), 0.9)))
}
