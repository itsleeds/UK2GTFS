#' make calendar nptdr
#'
#' @details
#' split overlapping start and end dates
#'
#' @param schedule scheduel data.frame
#' @noRd
#'
nptdr_makeCalendar <- function(schedule, exceptions, historic_bank_holidays = historic_bank_holidays) {
  # prep the inputs
  message(paste0(Sys.time(), " Constructing calendar and calendar_dates"))

  calendar <- schedule[, c("uid", "start_date", "end_date",
                           "days_operation","school_term_time","bank_holiday", "rowID", "running_board",
                           "schedule","route_direction")]
  names(calendar) <- c("UID", "start_date", "end_date", "Days", "school_term_time","bank_holiday",
                       "rowID", "Headcode", "schedule","route_direction")
  calendar$duration <- calendar$end_date - calendar$start_date + 1

  # Trim Calendar
  # Sometime exclusion run out to 2900 etc
  calendar$end_date <- dplyr::if_else(calendar$end_date > lubridate::ymd("2012-12-31"),
                                      max(c(lubridate::ymd("2012-12-31"),calendar$start_date + 365 )),
                                      calendar$end_date)
  calendar$start_date <- dplyr::if_else(calendar$start_date < lubridate::ymd("2003-01-01"),
                                        min(c(lubridate::ymd("2003-01-01"),calendar$end_date - 365 )),
                                        calendar$start_date)

  # exception_type
  # In CIF    0 = exclude, 1 = include
  # In GTFS   2 = exclude, 1 = include
  if(nrow(exceptions) > 0){
    exceptions$exception_type[exceptions$exception_type == 0] <- 2
    # Process exclusions
    if(!all(exceptions$exception_type %in% c(1,2))){
      stop("Other types of exception")
    }

    # Split exclusions and inclusions
    cal_noexc <- calendar[!calendar$schedule %in% exceptions$schedule,]
    exceptions_exc <- exceptions[exceptions$exception_type == 2,]
    exceptions_inc <- exceptions[exceptions$exception_type == 1,]

    cal_exc <- calendar[calendar$schedule %in% exceptions_exc$schedule,]
    cal_exc <- dplyr::group_by(cal_exc, UID)
    cal_exc <- dplyr::group_split(cal_exc)
    cal_exc <- purrr::set_names(cal_exc, purrr::map_chr(cal_exc, ~.x$schedule[1]))

    # Trim Exceptions
    exceptions_exc <- exceptions_exc[!is.na(exceptions_exc$end_date),]
    exceptions_exc <- exceptions_exc[!is.na(exceptions_exc$start_date),]

    # Sometime exclusion run out to 2900 etc
    exceptions_exc$end_date <- dplyr::if_else(exceptions_exc$end_date > lubridate::ymd("2012-12-31"),
                                              max(c(lubridate::ymd("2012-12-31"),exceptions_exc$end_date + 365 )),
                                              exceptions_exc$end_date)
    exceptions_exc$start_date <- dplyr::if_else(exceptions_exc$start_date < lubridate::ymd("2003-01-01"),
                                                min(c(lubridate::ymd("2003-01-01"),exceptions_exc$end_date - 365 )),
                                                exceptions_exc$start_date)

    #trip_exc_sub <- trip_exc[trip_exc$schedule == trip_sub$schedule,]
    trip_exc <- exceptions_exc[,c("schedule","start_date","end_date")]
    trip_exc <- dplyr::group_by(trip_exc, schedule)
    trip_exc <- dplyr::group_split(trip_exc)
    trip_exc <- purrr::set_names(trip_exc, purrr::map_chr(trip_exc, ~.x$schedule[1]))

    trip_exc <- trip_exc[match(names(cal_exc), names(trip_exc))]

    cal_exc = purrr::map2(cal_exc, trip_exc,
                          .f = exclude_trips_nptdr2,
                          .progress = "Checking for Exclusions")

    cal_exc <- dplyr::bind_rows(cal_exc)

    cal_dates_exc <- data.frame(UID = rep(cal_exc$UID, times = lengths(cal_exc$exclude_days)),
                                Days = rep(cal_exc$Days, times = lengths(cal_exc$exclude_days)),
                                date = unlist(cal_exc$exclude_days))
    cal_dates_exc$date <- as.Date(cal_dates_exc$date, origin = "1970-01-01")
    cal_dates_exc$exception_type <- 2
    cal_exc$exclude_days <- NULL

    #Check for excluded data that don't run
    cal_dates_exc$day <- lubridate::wday(cal_dates_exc$date, week_start = 1, label = FALSE)
    cal_dates_exc$valid_day <- purrr::map2_lgl(cal_dates_exc$day, cal_dates_exc$Days, function(x,y){
      as.logical(as.integer(substr(y,x,x)))
    })
    cal_dates_exc <- cal_dates_exc[cal_dates_exc$valid_day,]
    cal_dates_exc <- cal_dates_exc[,c("UID","date","exception_type")]

    if(nrow(exceptions_inc) > 0){
      cal_dates_inc <- list_include_days_nptdr(exceptions_inc)
      cal_dates_inc <- dplyr::left_join(cal_dates_inc, calendar[,c("UID","schedule")], by = "schedule")
      cal_dates_inc <- cal_dates_inc[,c("UID","date")]
      cal_dates_inc$exception_type <- 1
    } else {
      cal_dates_inc <- NULL
    }

    calendar <- rbind(cal_noexc, cal_exc)
  }

  calendar_dates <- calendar[,c("UID","start_date","end_date","school_term_time","bank_holiday")]

  # TODO: bank_holiday = B as may need to exclude all other dates
  # I think it is fine to have a schedule only in calendar_dates
  calendar <- calendar[na2logical(calendar$bank_holiday != "B", TRUE),]

  calendar$bank_holiday <- NULL

  # TODO: Detect Scotland and NI
  message("Unique Scotland and NI bank holidays are not correctly handled")

  bh <- historic_bank_holidays[historic_bank_holidays$date >= min(calendar$start_date, na.rm = TRUE),]
  bh <- bh[bh$date <= max(calendar$end_date, na.rm = TRUE), ]
  bh <- bh[bh$England,]

  calendar_dates <- nptdr_parse_bank_holidays(calendar_dates, bh)
  if(exists("cal_dates_exc", inherits = FALSE)){
    calendar_dates <- rbind(calendar_dates, cal_dates_exc)
  }
  if(exists("cal_dates_inc", inherits = FALSE)){
    calendar_dates <- rbind(calendar_dates, cal_dates_inc)
  }


  # Historical term times
  utils::data("school_terms")
  message("School term dates are illustrative and not exact")

  calendar_noschool = calendar[is.na(calendar$school_term_time),]
  calendar_school = calendar[!is.na(calendar$school_term_time),]

  calendar_school_term = calendar_school[calendar_school$school_term_time == "S",]
  calendar_school_hol = calendar_school[calendar_school$school_term_time == "H",]

  if(nrow(calendar_school_term) > 0){
    calendar_school_term <- dplyr::group_by(calendar_school_term, schedule)
    calendar_school_term <- dplyr::group_split(calendar_school_term)

    # S means term time only so exclude holidays
    calendar_school_term = purrr::map(calendar_school_term,
                                      trip_exc_sub = school_terms[school_terms$type == "holiday",],
                                      .f = exclude_trips_nptdr2,
                                      exmode = FALSE,
                                      .progress = "School exclusions - term time only")
    calendar_school_term <- dplyr::bind_rows(calendar_school_term)
    cal_dates_school_term <- data.frame(UID = rep(calendar_school_term$UID, times = lengths(calendar_school_term$exclude_days)),
                                Days = rep(calendar_school_term$Days, times = lengths(calendar_school_term$exclude_days)),
                                date = unlist(calendar_school_term$exclude_days))
    cal_dates_school_term$date <- as.Date(cal_dates_school_term$date, origin = "1970-01-01")
    cal_dates_school_term$exception_type <- 2
    calendar_school_term$exclude_days <- NULL

    cal_dates_school_term$day <- lubridate::wday(cal_dates_school_term$date, week_start = 1, label = FALSE)
    cal_dates_school_term$valid_day <- purrr::map2_lgl(cal_dates_school_term$day, cal_dates_school_term$Days, function(x,y){
      as.logical(as.integer(substr(y,x,x)))
    })
    cal_dates_school_term <- cal_dates_school_term[cal_dates_school_term$valid_day,]
    cal_dates_school_term <- cal_dates_school_term[,c("UID","date","exception_type")]

  }

  if(nrow(calendar_school_hol) > 0){
    calendar_school_hol <- dplyr::group_by(calendar_school_hol, schedule)
    calendar_school_hol <- dplyr::group_split(calendar_school_hol)

    # H means holiday time only so exclude term
    calendar_school_hol = purrr::map(calendar_school_hol,
                                      trip_exc_sub = school_terms[school_terms$type == "term",],
                                      .f = exclude_trips_nptdr2,
                                      exmode = FALSE,
                                      .progress = "School exclusions - holiday time only")
    calendar_school_hol <- dplyr::bind_rows(calendar_school_hol)
    cal_dates_hol <- data.frame(UID = rep(calendar_school_hol$UID, times = lengths(calendar_school_hol$exclude_days)),
                                        Days = rep(calendar_school_hol$Days, times = lengths(calendar_school_hol$exclude_days)),
                                        date = unlist(calendar_school_hol$exclude_days))
    cal_dates_hol$date <- as.Date(cal_dates_hol$date, origin = "1970-01-01")
    cal_dates_hol$exception_type <- 2
    calendar_school_hol$exclude_days <- NULL

    cal_dates_hol$day <- lubridate::wday(cal_dates_hol$date, week_start = 1, label = FALSE)
    cal_dates_hol$valid_day <- purrr::map2_lgl(cal_dates_hol$day, cal_dates_hol$Days, function(x,y){
      as.logical(as.integer(substr(y,x,x)))
    })
    cal_dates_hol <- cal_dates_hol[cal_dates_hol$valid_day,]
    cal_dates_hol <- cal_dates_hol[,c("UID","date","exception_type")]

  }

  calendar <- rbind(calendar_noschool, calendar_school_term, calendar_school_hol)
  if(exists("cal_dates_school_term", inherits = FALSE)){
    calendar_dates <- rbind(calendar_dates, cal_dates_school_term)
  }
  if(exists("cal_dates_hol", inherits = FALSE)){
    calendar_dates <- rbind(calendar_dates, cal_dates_hol)
  }

  calendar$school_term_time <- NULL
  suppressWarnings(rm(calendar_noschool, calendar_school_term, calendar_school_hol,
                      cal_dates_school_term, cal_dates_hol))


  days <- lapply(calendar$Days, function(x) {
    as.integer(substring(x, 1:7, 1:7))
  })
  days <- matrix(unlist(days), ncol = 7, byrow = TRUE)
  days <- as.data.frame(days)
  names(days) <- c(
    "monday", "tuesday", "wednesday", "thursday",
    "friday", "saturday", "sunday"
  )

  calendar <- cbind(calendar, days)
  calendar$Days <- NULL

  return(list(calendar, calendar_dates))
}

na2logical <- function(x, logical = FALSE){
  x[is.na(x)] <- logical
  x
}


#' exclude trips
#' remove trips
#' @param trip_sub desc
#' @param trip_exc desc
#' @param exmode logical
#' @noRd
#'
exclude_trips_nptdr2 <- function(trip_sub, trip_exc_sub, exmode = TRUE) {

  if (!is.null(trip_exc_sub)) {

    if(exmode){
      if(any(unique(trip_exc_sub$schedule) != trip_sub$schedule)){
        stop("Error in matching schedules")
      }
    }

    # Classify Exclusions
    trip_exc_sub$type <- classify_exclusions(
      ExStartTime = trip_exc_sub$start_date,
      ExEndTime = trip_exc_sub$end_date,
      StartDate = trip_sub$start_date,
      EndDate = trip_sub$end_date
    )

    trip_exc_sub = trip_exc_sub[trip_exc_sub$type != "no overlap",]
    if ("total" %in% trip_exc_sub$type) {
      # Remove all
      return(NULL)
    } else {
      if ("start" %in% trip_exc_sub$type) {
        trip_sub$start_date <- max(trip_exc_sub$end_date[
          trip_exc_sub$type == "start"]) + 1
      }
      if ("end" %in% trip_exc_sub$type) {
        trip_sub$end_date <- min(trip_exc_sub$start_date[
          trip_exc_sub$type == "end"]) - 1
      }
      if ("middle" %in% trip_exc_sub$type) {
        exclude_days <- trip_exc_sub[trip_exc_sub$type == "middle", ]
        trip_sub$exclude_days <- list(list_exclude_days_nptdr(exclude_days))
      } else {
        trip_sub$exclude_days <- NA
      }
    }
  } else {
    # No Exclusions
    trip_sub$exclude_days <- NA
  }
  return(trip_sub)
}


#' list exclude days
#' ????
#' @param exclude_days desc
#' @noRd
list_include_days_nptdr <- function(include_days) {
  res <- mapply(
    function(ExStartTime, ExEndTime) {
      seq(ExStartTime, ExEndTime, by = "days")
    },
    include_days$start_date,
    include_days$end_date,
    SIMPLIFY = FALSE
  )
  res2 <- as.Date(unlist(res, use.names = FALSE), origin = "1970-01-01")
  res2 <- data.frame(date = res2, schedule = rep(include_days$schedule, lengths(res)))
  res2 <- unique(res2)
  res2 <- res2[,c("schedule","date")]
  return(res2)
}


#' list exclude days
#' SOmetime the dates are in the wrong order
#' ????
#' @param exclude_days desc
#' @noRd
list_exclude_days_nptdr <- function(exclude_days) {
  res <- mapply(
    function(ExStartTime, ExEndTime) {
      x <- try(seq(ExStartTime, ExEndTime, by = "days"), silent = TRUE)
      if(inherits(x, "try-error")){
        x <- seq(ExEndTime, ExStartTime, by = "days")
      }
      return(x)
    },
    exclude_days$start_date,
    exclude_days$end_date
  )
  res <- as.Date(unlist(res), origin = "1970-01-01")
  res <- unique(res)
  return(res)
}

#' Build calendar dates using historic bank holidays
#' ????
#' @param calendar_dates2 desc
#' @param bh desc
#' @noRd
nptdr_parse_bank_holidays <- function(calendar_dates2, bh){

  calendar_dates2$end_date[is.na(calendar_dates2$end_date)] <- max(bh$date, na.rm = TRUE) + 1

  calendar_dates2$school_term_time <- NULL

  # Blank = Operates days defined above
  # A = Operates additionally on bank holidays - GTFS 1
  # B = Operates on bank holidays only - GTFS 1
  # X = Operates except on bank holidays - GTFS 2
  calendar_dates2 <- calendar_dates2[!is.na(calendar_dates2$bank_holiday),]
  calendar_dates2$exception_type <- dplyr::if_else(calendar_dates2$bank_holiday == "X",
                                                   2,1)
  calendar_dates2$bank_holiday <- NULL

  n = nrow(calendar_dates2)
  calendar_dates2 <- calendar_dates2[rep(seq_len(n), each = nrow(bh)),]
  calendar_dates2$date <- rep(bh$date, times = n)
  calendar_dates2 <- calendar_dates2[calendar_dates2$date >= calendar_dates2$start_date,]
  calendar_dates2 <- calendar_dates2[calendar_dates2$date <= calendar_dates2$end_date,]

  calendar_dates2 <- calendar_dates2[,c("UID","date","exception_type")]
  return(calendar_dates2)
}
