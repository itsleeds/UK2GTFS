#' Interpolate stop times
#'
#' Sometimes bus timetables do not give unique stop times to every stop. Instead
#' several stops in an row are given the same stop time. This function
#' interpolates the stop times so that each bust stop is given a unique arrival
#' and departure time.
#'
#' Note this is not possible if the final arrival time is a duplicated time, in
#' which case the times are unmodified. Interpolation is based on arrival time
#' only. If after interpolation departure time is less than arrival time then
#' departure time is set to arrival time.
#'
#'
#'
#' @param gtfs named list of data.frames
#' @param ncores number of cores to use
#' @return a gtfs object with interpolated stop times
#' @export
#'

gtfs_interpolate_times <- function(gtfs, ncores = 1){
  stop_times <- gtfs$stop_times

  if(!inherits(stop_times$arrival_time, "Period")){
    stop_times$arrival_time <- lubridate::hms(stop_times$arrival_time, quiet = TRUE)
  }

  if(!inherits(stop_times$departure_time, "Period")){
    stop_times$departure_time <- lubridate::hms(stop_times$departure_time, quiet = TRUE)
  }

  if(inherits(stop_times$stop_sequence, "character")){
    stop_times$stop_sequence <- as.integer(stop_times$stop_sequence)
  }

  stop_times <- dplyr::group_by(stop_times, trip_id)
  stop_times <- dplyr::group_split(stop_times)

  if(ncores == 1){
    #stop_times <- pbapply::pblapply(stop_times, stops_interpolate)
    stop_times <- purrr::map(stop_times, stops_interpolate, .progress = TRUE)
  } else {
    # cl <- parallel::makeCluster(ncores)
    # parallel::clusterEvalQ(cl, {loadNamespace("UK2GTFS")})
    # stop_times <- pbapply::pblapply(stop_times,
    #                                 stops_interpolate,
    #                                 cl = cl
    # )
    # parallel::stopCluster(cl)
    # rm(cl)

    future::plan(future::multisession, workers = ncores)
    stop_times <- furrr::future_map(.x = stop_times,
                                    .f = stops_interpolate,
                                    .progress = TRUE)
    future::plan(future::sequential)

  }

  stop_times <- data.table::rbindlist(stop_times)
  # Period (S4) columns do not survive data.table row subsetting, so hand back
  # a plain data.frame (as gtfs_read does) before restoring the Period times
  data.table::setDF(stop_times)
  # quiet: NA times (untouched trips) legitimately fail to parse back
  stop_times$arrival_time <- lubridate::hms(stop_times$arrival_time, quiet = TRUE)
  stop_times$departure_time <- lubridate::hms(stop_times$departure_time, quiet = TRUE)

  gtfs$stop_times <- stop_times
  return(gtfs)

}


stops_interpolate <- function(x){
  # skip if NAs in times, as they cannot be handled. Times are still converted
  # to character (as on the main path below) so that rbindlist() can combine
  # trips from both paths.
  if(anyNA(x$arrival_time) || anyNA(x$departure_time)){
    x$arrival_time <- period2gtfs(x$arrival_time)
    x$departure_time <- period2gtfs(x$departure_time)
    return(x)
  }

  # Check for duplicates times
  if(any(duplicated(x$arrival_time))){
    # Check in correct order
    x <- x[order(x$stop_sequence),]
    # Identify Break points
    x$arr_char <- as.character(x$arrival_time)
    x$dup_arr <- duplicated(x$arr_char)
    x$batch <- cumsum(!x$dup_arr)
    btchs <- as.data.frame(table(x$batch))
    btchs$Var1 <- as.numeric(as.character(btchs$Var1))
    #x$arrival_time2 <- x$arrival_time
    for(i in 1:nrow(btchs)){
      frq <- btchs$Freq[i]
      if(frq != 1){
        if(i != nrow(btchs)){
          # Can't interpolate if last time is a duplicate, so skip
          btch <- btchs$Var1[i]
          tstart <- x$arrival_time[x$batch == btch]
          tstart <- tstart[1]

          tend <- x$arrival_time[x$batch == (btch + 1)]
          tend <- tend[1]
          interval <- (lubridate::seconds(tend - tstart) / (frq)) * c(0:(frq-1))
          interval <- round(interval)
          interval <- lubridate::period_to_seconds(interval)
          interval <- lubridate::as.duration(interval)
          newtimes <- lubridate::as.duration(tstart) + interval
          newtimes <- lubridate::as.period(newtimes)

          # Convert day:hours:min:sec to hours:min:sec
          newtimes <- period_days_to_hours(newtimes)

          x$arrival_time[x$batch == btch] <- newtimes
        }

      }
    }
    chk <- x$departure_time < x$arrival_time
    x$departure_time[chk] <- x$arrival_time[chk]
  }
  x$dup_arr <- NULL
  x$batch <- NULL
  x$arr_char <- NULL

  # Needed because rbindlist doesn't work with periods for some reason
  arrival_time <- try(period2gtfs(x$arrival_time), silent = TRUE)
  if(inherits(arrival_time, "try-error")){
    stop("conversion of times failed for tripID: ",unique(x$trip_id))
  }
  x$arrival_time <- arrival_time
  departure_time <- try(period2gtfs(x$departure_time), silent = TRUE)
  if(inherits(departure_time, "try-error")){
    stop("conversion of times failed for tripID: ",unique(x$trip_id))
  }
  x$departure_time <- departure_time
  return(x)
}


period_days_to_hours <- function(x){
  xday <- lubridate::day(x)
  xhour <- lubridate::hour(x)
  xmin <- lubridate::minute(x)
  xsec <- lubridate::second(x)

  y <- lubridate::period(hours = xhour + (xday * 24),
                         minutes = xmin,
                         seconds = xsec)
  return(y)
}
