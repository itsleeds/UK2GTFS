#' exclude trips
#' remove trips
#' @param trip_sub desc
#' @param trip_exc desc
#' @noRd
#'
exclude_trips <- function(trip_sub, trip_exc) {
  trip_exc_sub <- trip_exc[[trip_sub$trip_id[1]]]
  if (!is.null(trip_exc_sub)) {
    # Exclusions
    # Classify Exclusions
    trip_exc_sub$type <- classify_exclusions(
      ExStartTime = trip_exc_sub$StartDate,
      ExEndTime = trip_exc_sub$EndDate,
      StartDate = trip_sub$StartDate,
      EndDate = trip_sub$EndDate
    )
    trip_exc_sub = trip_exc_sub[trip_exc_sub$type != "no overlap",]
    if ("total" %in% trip_exc_sub$type) {
      # Remove all
      trip_sub$exclude_days <- NA
      trip_sub <- trip_sub[NULL, ]
    } else {
      if ("start" %in% trip_exc_sub$type) {
        trip_sub$StartDate <- max(trip_exc_sub$EndDate[
          trip_exc_sub$type == "start"]) + 1
      }
      if ("end" %in% trip_exc_sub$type) {
        trip_sub$EndDate <- min(trip_exc_sub$StartDate[
          trip_exc_sub$type == "end"]) - 1
      }
      if ("middle" %in% trip_exc_sub$type) {
        exclude_days <- trip_exc_sub[trip_exc_sub$type == "middle", ]
        trip_sub$exclude_days <- list(list_exclude_days(exclude_days))
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
list_exclude_days <- function(exclude_days) {
  res <- mapply(
    function(ExStartTime, ExEndTime) {
      seq(ExStartTime, ExEndTime, by = "days")
    },
    exclude_days$StartDate,
    exclude_days$EndDate
  )
  res <- as.Date(unlist(res), origin = "1970-01-01")
  res <- unique(res)
  return(res)
}

#' list exclude days
#' break up star and end include days into list of days
#' @param include_days desc
#' @noRd
list_include_days <- function(include_days) {
  res <- mapply(
    function(ExStartTime, ExEndTime) {
      if (ExEndTime >= ExStartTime) {
        seq.Date(ExStartTime, ExEndTime, by = "days")
      }
    },
    include_days$StartDate,
    include_days$EndDate
  )
  res <- as.Date(unlist(res), origin = "1970-01-01")
  res <- unique(res)
  return(res)
}


#' Classify Excusions
#' Takes start and end dates of exclusion to work out if they cover the
#'     start or end etc
#' @param ExStartTime desc
#' @param ExEndTime desc
#' @param StartDate desc
#' @param EndDate desc
#' @noRd
classify_exclusions <- function(ExStartTime, ExEndTime, StartDate, EndDate) {
  start_earlier <- ExStartTime <= StartDate
  finish_later <- ExEndTime >= EndDate
  no_overlap <- (ExStartTime > EndDate) | (ExEndTime < StartDate)

  ifelse(no_overlap, "no overlap",
         ifelse(start_earlier,
                ifelse(finish_later,"total","start"),
                ifelse(finish_later,"end","middle"))

  )

}

#' clean time
#' ????
#' @param x timepoints
#' @noRd
#'
clean_times <- function(x) {
  x <- as.character(x)
  x <- gsub("PT", "", x)

  help_times3 <- function(x_sub) {
    if (is.na(x_sub)) {
      return(0)
    }

    if (grepl("H", x_sub)) {
      hours <- gsub("H(.*)", "", x_sub)
      hours <- as.integer(hours)
    } else {
      hours <- 0
    }

    time <- gsub("(.*)H", "", x_sub)

    if (grepl("M", time)) {
      mins <- gsub("M(.*)", "", time)
      mins <- as.integer(mins)
    } else {
      mins <- 0
    }

    time <- gsub("(.*)M", "", time)

    if (grepl("S", time)) {
      secs <- gsub("S", "", time)
      secs <- as.integer(secs)
    } else {
      secs <- 0
    }

    return(secs + (mins * 60) + (hours * 3600))
  }


  times <- sapply(x, help_times3)
  return(times)
}

#' clean route type
#' Change rout types from charater to gtfs code
#'
#' Coaches are coded as 200 (GTFS extended route type "Coach Service"),
#' distinguishing them from local bus (3). This matches the DfT's BODS GTFS
#' feeds and lets analyses separate long-distance coach from local bus (the
#' TNDS NCSD archive and NPTDR COACH records are the main sources).
#'
#' @param rt character route type
#' @param guess_bus if true guess bus otherwise fail
#' @noRd
clean_route_type <- function(rt, guess_bus = FALSE) {
  if (is.na(rt)) {
    return(3)
  } else if (rt == "bus") {
    return(3)
  } else if (rt == "ferry") {
    return(4)
  } else if (rt == "coach") {
    return(200)
  } else if (rt == "rail") {
    return(2)
  } else if (rt == "underground") {
    return(1)
  } else if (rt == "tram") {
    return(0)
  } else if (rt == "metro") {
    return(1)
  } else if (rt == "TRAIN") {
    return(2)
  } else if (rt == "- B") {
    return(2)
  } else if (rt == "BUS") {
    return(3)
  } else if (rt == "COACH") {
    return(200)
  } else if (rt == "FERRY") {
    return(4)
  } else if (rt == "TRAM") {
    return(0)
  } else if (rt == "METRO") {
    return(1)
  } else if (rt == "air") {
    return(1100)
  } else if (rt == "AIR") {
    return(1100)
  }else {
    if(guess_bus){
      return(3)
    } else {
      stop(paste0("Unknown route_type ", rt))
    }

  }
}

#' Clean days
#' Change named days into GTFS fromat
#' @param days character of days
#' @noRd
clean_days <- function(days) {
  days_ul <- unlist(strsplit(days, " "))
  if (all(days_ul %in% c("Monday", "Tuesday", "Wednesday", "Thursday",
                         "Friday", "Saturday", "Sunday"))) {
    res <- c(0, 0, 0, 0, 0, 0, 0)
    if ("Monday" %in% days_ul) {
      res[1] <- 1
    }
    if ("Tuesday" %in% days_ul) {
      res[2] <- 1
    }
    if ("Wednesday" %in% days_ul) {
      res[3] <- 1
    }
    if ("Thursday" %in% days_ul) {
      res[4] <- 1
    }
    if ("Friday" %in% days_ul) {
      res[5] <- 1
    }
    if ("Saturday" %in% days_ul) {
      res[6] <- 1
    }
    if ("Sunday" %in% days_ul) {
      res[7] <- 1
    }
  } else if (all(days_ul %in% c("NotMonday", "NotTuesday", "NotWednesday", "NotThursday",
                                "NotFriday", "NotSaturday", "NotSunday"))){
    res <- c(1, 1, 1, 1, 1, 1, 1)
    if ("NotMonday" %in% days_ul) {
      res[1] <- 0
    }
    if ("NotTuesday" %in% days_ul) {
      res[2] <- 0
    }
    if ("NotWednesday" %in% days_ul) {
      res[3] <- 0
    }
    if ("NotThursday" %in% days_ul) {
      res[4] <- 0
    }
    if ("NotFriday" %in% days_ul) {
      res[5] <- 0
    }
    if ("NotSaturday" %in% days_ul) {
      res[6] <- 0
    }
    if ("NotSunday" %in% days_ul) {
      res[7] <- 0
    }
  } else if (is.na(days) | days == "NA") {
    res <- c(1, 1, 1, 1, 1, 1, 1)
  } else if (days == "MondayToFriday") {
    res <- c(1, 1, 1, 1, 1, 0, 0)
  } else if (days == "HolidaysOnly") {
    res <- c(0, 0, 0, 0, 0, 0, 0)
  } else if (days == "Weekend") {
    res <- c(0, 0, 0, 0, 0, 1, 1)
  } else if (days == "MondayToSaturday") {
    res <- c(1, 1, 1, 1, 1, 1, 0)
  } else if (days == "SaturdaySundayHolidaysOnly") {
    res <- c(0, 0, 0, 0, 0, 1, 1)
  } else if (days == "MondayToFriday Sunday") {
    res <- c(1, 1, 1, 1, 1, 0, 1)
  } else if (days %in% c("", "MondayToSunday",
                         "MondayToFridaySaturdaySundayHolidaysOnly")) {
    res <- c(1, 1, 1, 1, 1, 1, 1)
  } else {
    stop(paste0("Unknown day pattern: ", days))
  }
  names(res) <- NULL
  res
}


#' break up holidays2
#' Break up bank holiday data into GTFS style calendar_dates file
#' @param cal_data the bank_holidays object extracted from vehicle journeys
#' @param cl column name to use "BankHolidaysOperate" or "BankHolidaysNoOperate"
#' @param cal the bank holiday calendar
#' @noRd
break_up_holidays2 <- function(cal_dat, cl, cal) {
  cal_dat <- cal_dat[!is.na(cal_dat[[cl]]), ]
  cal_dat <- cal_dat[cal_dat[[cl]] != "", ]
  if (nrow(cal_dat) == 0) {
    return(NULL)
  } else {
    cal_dat_holidays <- lapply(strsplit(cal_dat[[cl]], ", "), function(x) {
      x[x != ""]
    })
    cal_dat <- cal_dat[rep(1:nrow(cal_dat),
                           times = lengths(cal_dat_holidays)), ]
    cal_dat$hols <- unlist(cal_dat_holidays)
    if (cl == "BankHolidaysOperate") {
      cal_dat$exception_type <- 1L
    } else {
      cal_dat$exception_type <- 2L
    }
    cal_dat <- cal_dat[, c("trip_id", "hols", "exception_type")]
    return(cal_dat)
  }
}


# to do, need to repeat stops times for each departure time
#' clean activities
#' ????
#' @param x desc
#' @param type desc
#' @noRd
clean_activity <- function(x, type) {
  if (type == "pickup") {
    if (x == "pickUp") {
      x <- 0L
    } else if (x == "pickUpAndSetDown") {
      x <- 0L
    } else if (x == "setDown") {
      x <- 1L
    } else {
      stop(paste0(x, " Invalid pickup type"))
    }
  }
  if (type == "drop_off") {
    if (x == "pickUp") {
      x <- 1L
    } else if (x == "pickUpAndSetDown") {
      x <- 0L
    } else if (x == "setDown") {
      x <- 0L
    } else {
      stop(paste0(x, " Invalid drop off type"))
    }
  }
  x
}



#' Expand stop_times2
#' ????
#' @param i desc
#' @param jps desc
#' @param trips desc
#' @noRd
#'
expand_stop_times2 <- function(i, jps, trips) {
  jps_sub <- jps[[i]]
  trips_sub <- trips[trips$JourneyPatternRef == jps_sub$JourneyPatternID[1], ]
  jps_sub$To.Activity[is.na(jps_sub$To.Activity)] <- "pickUpAndSetDown"

  if(length(unique(jps_sub$ss_order))!=1){
    jps_sub <- jps_sub[order(jps_sub$ss_order),]
  }
  # Check if in order, for not fix
  nrow_jps <- nrow(jps_sub)
  if(nrow_jps > 1){
    spfm = jps_sub$From.StopPointRef[2:(nrow(jps_sub))]
    spto = jps_sub$To.StopPointRef[1:(nrow(jps_sub)-1)]
    if(!all(spfm == spto)){
      jps_sub_new <- try(reorder_jps(jps_sub), silent = TRUE)
      if(inherits(jps_sub_new, "try-error")){
        jps_sub_new <- try(reorder_jps(jps_sub, func = max), silent = TRUE)
      }
      if(inherits(jps_sub_new, "try-error")){
        warning("Cannnot find correct order of stops, defaulting to file order")
      } else {
        jps_sub <- jps_sub_new
      }
    }
  }


  jps_sub$To.SequenceNumber <- seq(2, nrow(jps_sub) + 1)
  st_sub <- jps_sub[, c("To.StopPointRef", "To.Activity", "To.SequenceNumber",
                        "JourneyPatternID", "To.WaitTime", "To.TimingStatus",
                        "RunTime","From.WaitTime")]
  names(st_sub) <- c("stop_id", "To.Activity", "stop_sequence",
                     "JourneyPatternRef", "To.WaitTime", "timepoint", "RunTime",
                     "From.WaitTime")
  st_top <- data.frame(
    stop_id = jps_sub$From.StopPointRef[1],
    To.Activity = jps_sub$From.Activity[1],
    stop_sequence = "1",
    JourneyPatternRef = jps_sub$JourneyPatternID[1],
    To.WaitTime = 0,
    timepoint = jps_sub$From.TimingStatus[1],
    RunTime = 0,
    From.WaitTime = 0,
    stringsAsFactors = FALSE
  )
  if (is.na(st_top$To.Activity)) {
    st_top$To.Activity <- "pickUp"
  } else if (st_top$To.Activity == "pass") {
    st_top$To.Activity <- "pickUp"
  }

  st_sub <- rbind(st_top, st_sub)
  # st_sub$RunTime <- as.integer(st_sub$RunTime)
  st_sub$To.WaitTime <- as.integer(st_sub$To.WaitTime)

  st_sub$From.WaitTime <- as.integer(st_sub$From.WaitTime)
  st_sub$From.WaitTime <- c(st_sub$From.WaitTime[seq(2, length(st_sub$From.WaitTime))],0)
  st_sub$total_wait_time <- st_sub$To.WaitTime + st_sub$From.WaitTime
  st_sub$departure_time <- cumsum(st_sub$RunTime + st_sub$total_wait_time)
  st_sub$arrival_time <- st_sub$departure_time - st_sub$total_wait_time

  #st_sub$departure_time <- cumsum(st_sub$RunTime + st_sub$To.WaitTime)
  #st_sub$arrival_time <- st_sub$departure_time - st_sub$To.WaitTime
  st_sub$pickup_type <- sapply(st_sub$To.Activity, clean_activity,
                               type = "pickup")
  st_sub$drop_off_type <- sapply(st_sub$To.Activity, clean_activity,
                                 type = "drop_off")

  n_stops <- nrow(st_sub)
  n_trips <- nrow(trips_sub)
  st_sub <- st_sub[rep(1:n_stops, times = n_trips), ]
  st_sub$trip_id <- rep(trips_sub$trip_id, each = n_stops)
  st_sub$DepartureTime <- lubridate::hms(rep(trips_sub$DepartureTime,
                                             each = n_stops))

  st_sub$arrival_time <- lubridate::seconds_to_period(lubridate::as.duration(
    st_sub$arrival_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$arrival_time <- sprintf("%02d:%02d:%02d", st_sub$arrival_time@day *
                                   24 + st_sub$arrival_time@hour,
                                 lubridate::minute(st_sub$arrival_time),
                                 lubridate::second(st_sub$arrival_time))

  st_sub$departure_time <- lubridate::seconds_to_period(lubridate::as.duration(
    st_sub$departure_time) + lubridate::as.duration(st_sub$DepartureTime))
  st_sub$departure_time <- sprintf("%02d:%02d:%02d", st_sub$departure_time@day *
                                     24 + st_sub$departure_time@hour,
                                   lubridate::minute(st_sub$departure_time),
                                   lubridate::second(st_sub$departure_time))

  st_sub$timepoint <- sapply(st_sub$timepoint, clean_timepoints)

  st_sub <- st_sub[, c("trip_id", "arrival_time", "departure_time", "stop_id",
                       "stop_sequence", "timepoint")]
  #st_sub = dplyr::left_join(st_sub, stops, by = "stop_id")
  return(st_sub)
}

#' reorder_jps
#' Change the order of JPS_sub as the file order is wrong
#' @param jps_sub jps_sub from expand_stop_times2
#' @noRd
#'
reorder_jps <- function(jps_sub, func = min){

  res_order <- list()
  rwnumbs <- seq_len(nrow(jps_sub))
  loopflag <- NA_integer_
  for(j in rwnumbs){
    if(j == 1){
      if("pickUp" %in% jps_sub$From.Activity){
        fnmb1 <- rwnumbs[jps_sub$From.Activity == "pickUp"]
        if(length(fnmb1) == 1){
          res_order[[j]] <- fnmb1
        } else {
          res_order[[j]] <- min(fnmb1)
        }

      } else {
        res_order[[j]] <- 1
      }
    } else {
      fnmb <- rwnumbs[jps_sub$From.StopPointRef == jps_sub$To.StopPointRef[res_order[[j-1]]]]
      if(length(fnmb) == 1){
        res_order[[j]] <- fnmb
      } else {
        #message("Double trouble ",fnmb)
        diffs <- abs(fnmb - res_order[[j-1]])
        fsel <- fnmb[diffs == func(diffs)]
        if(length(fsel) == 1){
          res_order[[j]] <- fsel
        } else {
          for(k in seq_len(length(fsel))){
            #message("Loop trouble ")
            if(!func(fsel) %in% unlist(res_order)){
              res_order[[j]] <- func(fsel)
            } else {
              fsel <- fsel[fsel != func(fsel)]
            }
          }
          if(length(fsel) == 0){
            stop("Multiloop error:",paste(res_order[[j-1]], collapse = ","))
          }
        }
      }

    }
    #message(res_order[[j]])
  }
  res_order <- unlist(res_order)
  if(length(res_order) != nrow(jps_sub)){
    stop("Attemped to reorder stops and failed")
  }
  jps_sub <- jps_sub[res_order,]

}


#' clean_timepoints
#' ????
#' @param tp desc
#' @noRd
#'
clean_timepoints <- function(tp) {
  if (tp %in% c("OTH","otherPoint","timeInfoPoint")) {
    return(0L)
  } else if (tp %in% c("PTP", "TIP", "PPT",
                       "principleTimingPoint",
                       "principalTimingPoint")) {
    return(1L)
  } else {
    stop(paste0("Unknown timepoint type: ", tp))
  }
}

#' make stop times
#' ????
#' @param jps desc
#' @param trips desc
#' @param ss desc
#' @noRd
#'
make_stop_times <- function(jps, trips, ss) {
  jps <- jps[, c("JPS_id", "From.Activity", "From.StopPointRef", "From.WaitTime",
                 "From.TimingStatus", "To.WaitTime", "To.Activity",
                 "To.StopPointRef", "To.TimingStatus", "RunTime",
                 "From.SequenceNumber", "To.SequenceNumber")]
  jps[] <- lapply(jps, as.character)
  jps <- clean_pass(jps)
  # vj[] <- lapply(vj, as.character)
  # rts <- unique(vj$JourneyPatternRef)
  ss_join <- ss[, c("JourneyPatternSectionRefs", "JourneyPatternID")]
  ss_join[] <- lapply(ss_join, as.character)
  ss_join$ss_order <- seq_len(nrow(ss_join))
  jps$JPS_id <- as.character(jps$JPS_id)
  jps <- dplyr::left_join(jps, ss_join,
                          by = c("JPS_id" = "JourneyPatternSectionRefs"))
  jps <- split(jps, jps$JourneyPatternID)

  stop_times <- lapply(seq(1, length(jps)), expand_stop_times2, jps = jps, trips = trips)
  stop_times <- stop_times[sapply(stop_times, nrow) > 0] # edge case where jps has times but no trips are recorded
  stop_times <- dplyr::bind_rows(stop_times)
  return(stop_times)
}

#' Remove passes from journey patterns
#' JPS can have a "pass" type, remove and update runtimes
#' @param jps journeypatternsections
#' @noRd
clean_pass <- function(jps) {
  if ("pass" %in% jps$To.Activity) {
    is_pass <- jps$To.Activity %in% "pass"
    pass_post <- c(FALSE, is_pass[seq(1, length(is_pass) - 1)])
    runtime1 <- as.integer(jps$RunTime)
    runtime2 <- c(0, runtime1[seq(1, length(runtime1) - 1)])
    runtime3 <- ifelse(pass_post, runtime1 + runtime2, runtime1)
    jps$RunTime <- runtime3
    jps <- jps[jps$To.Activity != "pass", ]
  } else {
    jps$RunTime <- as.integer(jps$RunTime)
  }
  return(jps)
}

