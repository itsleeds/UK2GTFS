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
#' @param ncores unused, kept for backwards compatibility. The interpolation
#'   is fully vectorised and runs single-threaded; the old per-trip parallel
#'   implementation spent most of its time splitting millions of trips into
#'   individual data frames and doing lubridate Period (S4) arithmetic on
#'   each, which was slower on 10 cores than this version on one.
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

  # Only trips with duplicated arrival times (and no missing times) need
  # interpolating; identify them vectorised.
  flags <- data.table::data.table(
    trip_id = stop_times$trip_id,
    arr = lubridate::period_to_seconds(stop_times$arrival_time),
    dep = lubridate::period_to_seconds(stop_times$departure_time)
  )
  flags <- flags[, list(has_dup = anyDuplicated(arr) > 0L,
                        has_na = anyNA(arr) || anyNA(dep)),
                 by = "trip_id"]
  needs <- flags$trip_id[flags$has_dup & !flags$has_na]
  n_trips <- nrow(flags)
  rm(flags)
  message(length(needs), " of ", n_trips,
          " trips have duplicated stop times to interpolate")

  sel <- stop_times$trip_id %in% needs
  untouched <- stop_times[!sel, , drop = FALSE]
  # convert times to character, restored to Period at the end
  untouched$arrival_time <- period2gtfs(untouched$arrival_time)
  untouched$departure_time <- period2gtfs(untouched$departure_time)

  todo <- stop_times[sel, , drop = FALSE]
  rm(stop_times, sel)

  if (nrow(todo) > 0) {
    # Match the historic per-trip processing order (trips sorted by id in the
    # C locale as dplyr::group_split produced, rows by stop_sequence, stable)
    ord <- order(todo$trip_id, todo$stop_sequence, method = "radix")
    todo <- todo[ord, , drop = FALSE]

    arr <- lubridate::period_to_seconds(todo$arrival_time)
    dep <- lubridate::period_to_seconds(todo$departure_time)

    # A "batch" is opened by every arrival time not already seen earlier in
    # the trip; repeats (consecutive or not) join the batch of the previous
    # non-repeat row. Interpolation spreads each multi-row batch linearly
    # from its own first arrival towards the first arrival of the next
    # batch. The last batch of a trip is never interpolated (no end point).
    dt <- data.table::data.table(trip_id = todo$trip_id, arr = arr)
    dt[, batch := cumsum(!duplicated(arr)), by = "trip_id"]
    dt[, `:=`(k = seq_len(.N) - 1L, frq = .N, tstart = arr[1L]),
       by = c("trip_id", "batch")]
    bt <- dt[, list(tstart = arr[1L]), by = c("trip_id", "batch")]
    bt[, tend := data.table::shift(tstart, -1L), by = "trip_id"]
    dt[bt, tend := i.tend, on = c("trip_id", "batch")]

    interp <- dt$frq > 1L & !is.na(dt$tend)
    arr_new <- arr
    # same arithmetic as the old per-trip loop: round((step) * k) where
    # step = (tend - tstart) / frq, using base round()
    arr_new[interp] <- dt$tstart[interp] +
      round(((dt$tend[interp] - dt$tstart[interp]) / dt$frq[interp]) * dt$k[interp])
    rm(dt, bt)

    arr_chr <- period2gtfs(todo$arrival_time)
    arr_chr[interp] <- format_gtfs_seconds(arr_new[interp])
    dep_chr <- period2gtfs(todo$departure_time)
    fix <- dep < arr_new
    dep_chr[fix] <- arr_chr[fix]

    todo$arrival_time <- arr_chr
    todo$departure_time <- dep_chr

    stop_times <- data.table::rbindlist(
      list(data.table::as.data.table(todo),
           data.table::as.data.table(untouched)), use.names = TRUE)
  } else {
    stop_times <- data.table::as.data.table(untouched)
  }
  # Period (S4) columns do not survive data.table row subsetting, so hand back
  # a plain data.frame (as gtfs_read does) before restoring the Period times
  data.table::setDF(stop_times)
  # quiet: NA times (untouched trips) legitimately fail to parse back
  stop_times$arrival_time <- lubridate::hms(stop_times$arrival_time, quiet = TRUE)
  stop_times$departure_time <- lubridate::hms(stop_times$departure_time, quiet = TRUE)

  gtfs$stop_times <- stop_times
  return(gtfs)

}

#' Format seconds-since-midnight as a GTFS HH:MM:SS string
#'
#' Matches the string the old per-trip implementation produced via
#' seconds_to_period() + period_days_to_hours() + period2gtfs():
#' hours absorb whole days, so times past 24:00 stay as hours.
#' @param secs numeric whole seconds since midnight
#' @noRd
format_gtfs_seconds <- function(secs) {
  p <- lubridate::seconds_to_period(secs)
  sprintf("%02d:%02d:%02d",
          lubridate::hour(p) + lubridate::day(p) * 24,
          lubridate::minute(p),
          lubridate::second(p))
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
