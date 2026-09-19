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

#' include trips
#'
#' Restrict a trip to the dates of the ServicedOrganisations it operates on.
#'
#' A `ServicedOrganisationDayType/DaysOfOperation` reference means the journey
#' runs *only* on that organisation's dates (typically a school's holidays),
#' so it restricts the journey's calendar rather than adding to it. This is the
#' mirror of `exclude_trips()`: the trip is clipped to the span the ranges
#' cover, and any day inside that span but outside every range is added to the
#' trip's exclusions.
#'
#' Emitting `exception_type = 1` rows instead, while leaving the weekly
#' calendar untouched, produces a journey that runs every week: under GTFS
#' semantics an added date on a day the calendar already operates is a no-op.
#'
#' @param trip_sub one-row data frame for a single trip
#' @param trip_inc named list of inclusion date ranges, split by
#'   VehicleJourneyCode
#' @noRd
include_trips <- function(trip_sub, trip_inc) {
  trip_inc_sub <- trip_inc[[trip_sub$trip_id[1]]]
  if (is.null(trip_inc_sub)) {
    # No inclusions, the trip keeps its own calendar
    return(trip_sub)
  }

  # Clip the trip to the span the organisation's date ranges cover
  start <- max(min(trip_inc_sub$StartDate), trip_sub$StartDate[1])
  end <- min(max(trip_inc_sub$EndDate), trip_sub$EndDate[1])
  if (start > end) {
    # Trip and organisation dates do not overlap, so the trip never runs
    return(trip_sub[NULL, ])
  }
  trip_sub$StartDate <- start
  trip_sub$EndDate <- end

  # Days inside the span but outside every range are days the trip does not
  # run. Only days the weekly calendar would otherwise operate need excluding.
  operates <- list_include_days(trip_inc_sub)
  gaps <- seq(start, end, by = "days")
  gaps <- gaps[!gaps %in% operates]
  if (length(gaps) > 0) {
    days <- clean_days(as.character(trip_sub$DaysOfWeek[1]))
    gaps <- gaps[days[lubridate::wday(gaps, week_start = 1)] == 1]
  }

  if (length(gaps) > 0) {
    prev <- trip_sub$exclude_days[[1]]
    if (!inherits(prev, "Date")) {
      prev <- as.Date(character())
    } else {
      prev <- prev[!is.na(prev)]
    }
    trip_sub$exclude_days <- list(sort(unique(c(prev, gaps))))
  }

  return(trip_sub)
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
#'
#' Convert ISO 8601 durations ("PT1H30M", "PT45S") to seconds.
#'
#' Every distinct duration is parsed once and the answers are matched back.
#' TransXchange run times and wait times repeat heavily - the largest file in
#' the package's example archive has 11,009 of them and 16 distinct values -
#' and this is called three times per file on the whole JourneyPatternSections
#' table, so parsing per element was costing more than the rest of the export
#' put together.
#'
#' @param x timepoints
#' @noRd
#'
clean_times <- function(x) {
  x <- as.character(x)
  u <- unique(x)
  v <- gsub("PT", "", u, fixed = TRUE)

  # Take the number in front of `unit`, for the values that carry that unit at
  # all. Only those are coerced: running as.numeric() over the whole vector and
  # picking with ifelse() gives the same answer but warns about the values it
  # was never going to use, which on a normal conversion is pure noise.
  leading <- function(txt, unit) {
    out <- rep(0, length(txt))
    has <- grepl(unit, txt, fixed = TRUE)
    if (any(has)) {
      out[has] <- as.numeric(sub(paste0(unit, ".*$"), "", txt[has]))
    }
    out
  }

  # the same three peels the per-element parser did, vectorised: take the
  # hours off the front, then the minutes off what is left, then the seconds
  hours <- leading(v, "H")
  rest <- sub("^.*H", "", v)
  mins <- leading(rest, "M")
  rest <- sub("^.*M", "", rest)
  secs <- rep(0, length(rest))
  has_secs <- grepl("S", rest, fixed = TRUE)
  if (any(has_secs)) {
    secs[has_secs] <- as.numeric(sub("S", "", rest[has_secs], fixed = TRUE))
  }

  res <- secs + (mins * 60) + (hours * 3600)
  res[is.na(u)] <- 0 # a missing duration is no time at all, as before

  unname(res[match(x, u)])
}

#' clean route type
#' Change route types from character to gtfs code
#'
#' Coaches are coded as 200 (GTFS extended route type "Coach Service"),
#' distinguishing them from local bus (3). This matches the DfT's BODS GTFS
#' feeds and lets analyses separate long-distance coach from local bus (the
#' TNDS NCSD archive and NPTDR COACH records are the main sources). Air has no
#' core GTFS type either, so it takes the extended 1100. Everything else uses a
#' core type, including trolleybus (11).
#'
#' The comparison is case insensitive. TransXChange declares the enumeration in
#' lower camel case ("trolleyBus"), NPTDR CIF uses upper case ("BUS"), and real
#' files use title case too; matching each spelling separately meant an
#' unremarkable "Tram" either stopped the conversion or, with `guess_bus`,
#' silently became a bus.
#'
#' A missing Mode returns bus, which is the TransXChange schema default rather
#' than a guess.
#'
#' Tidy a TransXChange LineName into a GTFS route_short_name
#'
#' Applies the abbreviations that keep a published line name compact, and
#' removes spaces from a name longer than six characters so that a spaced
#' number ("X 1 2 3") comes out as one token.
#'
#' A name that is still long is kept. It used to be blanked, to satisfy a
#' validator notice that route_short_name should be short - but GTFS sets no
#' length limit and the notice is advisory, while blanking cost far more than
#' it saved. `gtfs_deduplicate()` groups routes by operator, mode and
#' `route_short_name`, and an unnamed route has to stand alone, so a blank name
#' is in effect an exemption from deduplication. Bus route numbers are short
#' and were rarely blanked, but line names are not: between 88% and 95% of
#' London Underground trips in every TNDS snapshot sat on a route blanked this
#' way, so a line published twice over the same dates could never be recognised
#' as one line. Every Underground name over six characters was lost - Central,
#' Piccadilly, Metropolitan, Bakerloo, District, Northern, Victoria, Jubilee -
#' sparing only Circle, which is exactly six.
#'
#' @param x character vector of LineName values
#' @return a character vector of route_short_name values
#' @noRd
clean_route_short_name <- function(x) {
  x <- gsub("Park & Ride", "P&R", x)
  x <- gsub("Road", "Rd", x)
  x <- gsub("Connecting Communities ", "", x)
  x <- gsub("the busway", "", x, ignore.case = TRUE)
  ifelse(nchar(x) > 6, gsub(" ", "", x), x)
}


#' @param rt character route type
#' @param guess_bus if true guess bus otherwise fail
#' @noRd
clean_route_type <- function(rt, guess_bus = FALSE) {
  if (is.na(rt)) {
    return(3)
  }
  key <- tolower(trimws(as.character(rt)))
  known <- c(
    bus = 3, "- b" = 2, coach = 200, ferry = 4, rail = 2, train = 2,
    underground = 1, metro = 1, tram = 0, trolleybus = 11, air = 1100
  )
  if (key %in% names(known)) {
    return(unname(known[[key]]))
  }
  if (guess_bus) {
    return(3)
  }
  stop(paste0("Unknown route_type ", rt))
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
#'
#' Map a TransXchange Activity to a GTFS pickup_type / drop_off_type.
#'
#' Vectorised: a character vector in, an integer vector out. Scalars still work
#' and still raise the same error, but expand_stop_times2() applies this to a
#' whole journey pattern at once rather than one stop at a time.
#'
#' @param x desc
#' @param type desc
#' @noRd
clean_activity <- function(x, type) {
  if (type == "pickup") {
    known <- c(pickUp = 0L, pickUpAndSetDown = 0L, setDown = 1L)
    label <- "Invalid pickup type"
  } else if (type == "drop_off") {
    known <- c(pickUp = 1L, pickUpAndSetDown = 0L, setDown = 0L)
    label <- "Invalid drop off type"
  } else {
    return(x)
  }

  res <- unname(known[match(x, names(known))])
  if (anyNA(res)) {
    stop(paste0(x[is.na(res)][1], " ", label))
  }
  res
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

  # Everything that depends only on the journey pattern is done here, while
  # st_sub is one row per stop. It used to be done after the rep() below, which
  # multiplied the work by the number of trips on the pattern: a file with 900
  # journeys over a 40-stop pattern converted 36,000 timepoints instead of 40.
  st_sub$pickup_type <- clean_activity(st_sub$To.Activity, type = "pickup")
  st_sub$drop_off_type <- clean_activity(st_sub$To.Activity, type = "drop_off")
  st_sub$timepoint <- clean_timepoints(st_sub$timepoint)

  n_stops <- nrow(st_sub)
  n_trips <- nrow(trips_sub)

  # Seconds past midnight for each trip's departure. Parsed once per distinct
  # departure time - a pattern's journeys share a handful of them - rather than
  # once per stop_time row, and added with plain arithmetic. Building a
  # lubridate Period for every row was the single most expensive thing the
  # export did; the S4 constructor and validity check alone were about a fifth
  # of its total run time.
  dep_chr <- as.character(trips_sub$DepartureTime)
  dep_unique <- unique(dep_chr)
  offset_unique <- vapply(strsplit(dep_unique, ":", fixed = TRUE), function(p) {
    p <- as.numeric(p)
    sum(p * c(3600, 60, 1)[seq_along(p)])
  }, numeric(1))
  offset <- offset_unique[match(dep_chr, dep_unique)]

  arrival <- rep(st_sub$arrival_time, times = n_trips) +
    rep(offset, each = n_stops)
  departure <- rep(st_sub$departure_time, times = n_trips) +
    rep(offset, each = n_stops)

  st_sub <- st_sub[rep(1:n_stops, times = n_trips), ]
  st_sub$trip_id <- rep(trips_sub$trip_id, each = n_stops)
  st_sub$arrival_time <- seconds_to_gtfs_time(arrival)
  st_sub$departure_time <- seconds_to_gtfs_time(departure)

  st_sub <- st_sub[, c("trip_id", "arrival_time", "departure_time", "stop_id",
                       "stop_sequence", "timepoint")]
  #st_sub = dplyr::left_join(st_sub, stops, by = "stop_id")
  return(st_sub)
}

#' Format seconds past midnight as a GTFS time string
#'
#' GTFS allows times past 24:00:00 for journeys that run into the next day, so
#' hours are not wrapped.
#'
#' @param secs numeric seconds past midnight
#' @noRd
seconds_to_gtfs_time <- function(secs) {
  secs <- as.integer(secs)
  sprintf("%02d:%02d:%02d", secs %/% 3600L, (secs %% 3600L) %/% 60L, secs %% 60L)
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
  # <TimingStatus> is minOccurs="0" with default="TIP" in the TransXChange
  # schema ("Default is Time Info Point (TIP)"), so an omitted element means TIP
  # rather than an error. Feeds that never publish it - the Bullocks 758 files,
  # for one - used to abort the whole conversion here.
  #
  # Vectorised: expand_stop_times2() has one of these per stop of a journey
  # pattern, and used to convert them one row at a time across every expanded
  # stop_time in the file.
  res <- rep(NA_integer_, length(tp))
  absent <- is.na(tp) | !nzchar(tp)
  res[absent] <- 1L
  res[!absent & tp %in% c("OTH", "otherPoint", "timeInfoPoint")] <- 0L
  res[!absent & tp %in% c("PTP", "TIP", "PPT",
                          "principleTimingPoint",
                          "principalTimingPoint")] <- 1L
  if (anyNA(res)) {
    stop(paste0("Unknown timepoint type: ", tp[is.na(res)][1]))
  }
  res
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

