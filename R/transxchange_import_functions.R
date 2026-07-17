
#' Clean NA from sequence
#' @param x sequency of numbers
#' @param y sequence of ids showing when to start a new sequency
#' @param displace if TRUE start at 2 rather than 2
#' @noRd

clean_sequence2 <- function(x, y, displace = FALSE) {
  if (anyNA(x)) {
    if (length(unique(y)) == 1) {
      # Only one Jounrey pattern
      res <- seq(1, length(y))
    } else {
      # Not changes in JPSid
      ly <- length(y)
      new_route <- y[seq(1, ly - 1)] != y[seq(2, ly)]
      new_route <- c(TRUE, new_route)
      start <- seq(1, ly)[new_route]
      end <- start - 1
      end <- end[seq(2, length(end))]
      end <- c(end, ly)
      diff <- end - start + 1
      res <- lapply(diff, function(z) {
        seq_len(z)
      })
      res <- unlist(res)
    }
    if (displace) {
      res <- res + 1
    }
    return(res)
  }
  return(x)
}


#' Import stoppoints
#' ????
#' @param StopPoints stoppoints
#' @param full_import logical
#' @noRd
import_stoppoints <- function(StopPoints, full_import = TRUE) {
  StopPointRef <- import_simple(StopPoints, ".//d1:StopPointRef")


  if (full_import) {
    CommonName <- import_simple(StopPoints, ".//d1:CommonName")
    LocalityName <- import_simple(StopPoints, ".//d1:LocalityName")
    LocalityQualifier <- import_simple(StopPoints, ".//d1:LocalityQualifier")
    Indicator <- import_withmissing(StopPoints, ".//d1:Indicator", 5)

    StopPoints <- data.frame(
      StopPointRef = StopPointRef,
      CommonName = CommonName,
      Indicator = Indicator,
      LocalityName = LocalityName,
      LocalityQualifier = LocalityQualifier
    )
  } else {
    StopPoints <- data.frame(StopPointRef = StopPointRef)
  }
  return(StopPoints)
}



#' Import journeypatternsections
#' ????
#' @param journeypatternsections journeypattern sections
#' @noRd
import_journeypatternsections <- function(journeypatternsections) {
  JourneyPatternTimingLink <- xml2::xml_find_all(journeypatternsections, ".//d1:JourneyPatternTimingLink")
  JPTL_ID <- import_simple(JourneyPatternTimingLink, "@id")
  # JPTL_ID               <- rep(JPTL_ID, times = xml2::xml_length(JourneyPatternTimingLink, only_elements = FALSE))


  RunTime <- import_simple(JourneyPatternTimingLink, "d1:RunTime")
  From <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:From")
  From.StopPointRef <- import_simple(From, "d1:StopPointRef")
  From.WaitTime <- import_simple_xml(From, "d1:WaitTime")
  From.Activity <- import_simple_xml(From, "d1:Activity")
  if (length(From.Activity) == 0) {
    From.Activity <- rep(NA, length(From.StopPointRef))
  }
  RouteLinkRef <- import_simple_xml(JourneyPatternTimingLink, "d1:RouteLinkRef")
  if (length(RouteLinkRef) == 0) {
    RouteLinkRef <- rep(NA, length(From.StopPointRef))
  }
  From.TimingStatus <- import_simple_xml(From, "d1:TimingStatus")
  # From.SequenceNumber <- import_FromTo(From, "@SequenceNumber")
  From.SequenceNumber <- xml2::xml_attr(From, "SequenceNumber")

  if (length(From.SequenceNumber) == 0) {
    From.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }
  To <- xml2::xml_find_all(JourneyPatternTimingLink, "d1:To")
  To.StopPointRef <- import_simple(To, "d1:StopPointRef")
  To.WaitTime <- xml2::xml_text(xml2::xml_find_first(To, "d1:WaitTime"))
  To.Activity <- import_simple_xml(To, "d1:Activity")
  if (length(To.Activity) == 0) {
    To.Activity <- rep(NA, length(To.StopPointRef))
  }
  To.TimingStatus <- import_simple(To, "d1:TimingStatus")
  # To.SequenceNumber <- import_FromTo(To, "@SequenceNumber")
  To.SequenceNumber <- xml2::xml_attr(To, "SequenceNumber")

  if (length(To.SequenceNumber) == 0) {
    To.SequenceNumber <- rep(NA, length(From.StopPointRef))
  }

  JPS <- xml2::xml_children(journeypatternsections)
  JPS_id <- import_simple(JPS, "@id")
  JPS_id <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))

  From.SequenceNumber <- clean_sequence2(From.SequenceNumber, JPS_id, FALSE)
  To.SequenceNumber <- clean_sequence2(To.SequenceNumber, JPS_id, TRUE)

  journeypatternsections <- data.frame(
    JPTL_ID = JPTL_ID,
    From.Activity = From.Activity,
    From.StopPointRef = From.StopPointRef,
    From.WaitTime = From.WaitTime,
    From.TimingStatus = From.TimingStatus,
    To.WaitTime = To.WaitTime,
    To.Activity = To.Activity,
    To.StopPointRef = To.StopPointRef,
    To.TimingStatus = To.TimingStatus,
    RouteLinkRef = RouteLinkRef,
    RunTime = RunTime,
    From.SequenceNumber = From.SequenceNumber,
    To.SequenceNumber = To.SequenceNumber,
    JPS_id = JPS_id
  )
  return(journeypatternsections)
}

#' import operators
#' slower so not used
#' @param operators operators object
#' @noRd
import_operators <- function(operators) {

  # Test for Issue 23
  operators_check <- xml2::xml_find_all(operators, "./d1:Operator")
  if(length(operators_check) == 0){
    # Don't do the find all
  } else {
    operators <- operators_check
  }

  NationalOperatorCode <- import_simple_xml(operators, ".//d1:NationalOperatorCode")
  OperatorCode <- import_simple_xml(operators, ".//d1:OperatorCode")
  OperatorShortName <- import_simple_xml(operators, ".//d1:OperatorShortName")
  OperatorNameOnLicence <- import_simple_xml(operators, ".//d1:OperatorNameOnLicence")
  TradingName <- import_simple_xml(operators, ".//d1:TradingName")
  # RegisteredOperatorRef in Services is an IDREF to this attribute
  OperatorID <- xml2::xml_attr(operators, "id")

  if (length(NationalOperatorCode) == 0) {
    NationalOperatorCode <- OperatorCode
  }

  if (length(OperatorNameOnLicence) == 0) {
    OperatorNameOnLicence <- rep(NA, length(NationalOperatorCode))
  }
  if (length(TradingName) == 0) {
    TradingName <- rep(NA, length(NationalOperatorCode))
  }
  if (length(OperatorID) == 0) {
    OperatorID <- rep(NA, length(NationalOperatorCode))
  }

  operators <- data.frame(
    NationalOperatorCode = NationalOperatorCode,
    OperatorCode = OperatorCode,
    OperatorShortName = OperatorShortName,
    OperatorNameOnLicence = OperatorNameOnLicence,
    TradingName = TradingName,
    OperatorID = OperatorID
  )
  return(operators)
}

#' import services
#' ????
#' @param service desc
#' @param full_import desc
#' @noRd

import_services <- function(service, full_import = TRUE) {
  ServiceCode <- import_simple(service, ".//d1:ServiceCode")
  Mode <- import_simple(service, ".//d1:Mode")
  if (length(Mode) == 0) {
    Mode <- NA
  }
  Description <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:Description"))
  RegisteredOperatorRef <- import_simple(service, ".//d1:RegisteredOperatorRef")
  if (length(RegisteredOperatorRef) == 0) {
    RegisteredOperatorRef <- rep(NA, length(ServiceCode))
  }
  StartDate <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:StartDate"))
  EndDate <- xml2::xml_text(xml2::xml_find_first(service, ".//d1:EndDate"))
  DaysOfWeek <- paste(xml2::xml_name(xml2::xml_children(xml2::xml_find_first(service, ".//d1:DaysOfWeek"))), collapse = " ")
  StopRequirements <- import_simple(service, ".//d1:StopRequirements")
  if (length(StopRequirements) == 0) {
    StopRequirements <- rep(NA, length(ServiceCode))
  }
  Origin <- import_simple_xml(service, ".//d1:Origin")
  Destination <- import_simple_xml(service, ".//d1:Destination")

  # A service may contain several Lines (e.g. "26" and "26a"); capture them
  # all so the export can build one GTFS route per line
  LineNodes <- xml2::xml_find_all(service, ".//d1:Line")
  Lines <- data.frame(
    LineID = xml2::xml_attr(LineNodes, "id"),
    LineName = xml2::xml_text(xml2::xml_find_first(LineNodes, "d1:LineName")),
    stringsAsFactors = FALSE
  )
  if (nrow(Lines) == 0) {
    LineName <- import_simple(service, ".//d1:LineName")
    if (length(LineName) == 0) LineName <- NA_character_
    Lines <- data.frame(LineID = NA_character_, LineName = LineName[1],
                        stringsAsFactors = FALSE)
  }
  LineName <- Lines$LineName[1]
  BankHolidayNonOperation <- import_simple(service, ".//d1:BankHolidayNonOperation")
  if (length(BankHolidayNonOperation) == 0) {
    BankHolidayNonOperation <- rep(NA, length(ServiceCode))
  }
  BankHolidayOperation <- import_simple(service, ".//d1:BankHolidayOperation")
  if (length(BankHolidayOperation) == 0) {
    BankHolidayOperation <- rep(NA, length(ServiceCode))
  }

  if (full_import) {
    PrivateCode <- import_simple(service, ".//d1:PrivateCode")
    if (length(PrivateCode) == 0) {
      PrivateCode <- rep(NA, length(ServiceCode))
    }
  }

  ss <- xml2::xml_find_all(service, ".//d1:JourneyPattern")
  Direction <- import_simple_xml(ss, ".//d1:Direction") # changeed from import_simple, to fix bug in Bus Archive
  VehicleType <- import_withmissing2(ss, ".//d1:Description", 3, "@id")
  # RouteRef <- import_simple(ss, ".//d1:RouteRef")
  RouteRef <- import_simple_xml(ss, ".//d1:RouteRef")

  if (length(RouteRef) == 0) {
    RouteRef <- rep(NA, length(Direction))
  }
  JourneyPatternSectionRefs <- import_simple(ss, ".//d1:JourneyPatternSectionRefs")
  JourneyPatternID <- import_simple(ss, "@id")

  if (length(JourneyPatternSectionRefs) != length(JourneyPatternID)) {
    # Some cases have muliple JourneyPatternSectionRefs
    lths <- list()
    for (i in seq(1, length(xml2::xml_length(ss)))) {
      lths[[i]] <- length(xml2::xml_find_all(ss[i], "d1:JourneyPatternSectionRefs"))
    }
    lths <- unlist(lths)
    Direction <- rep(Direction, times = lths)
    VehicleType <- rep(VehicleType, times = lths)
    RouteRef <- rep(RouteRef, times = lths)
    JourneyPatternID <- rep(JourneyPatternID, times = lths)
  }


  SpecialDaysOperation <- xml2::xml_find_all(service, ".//d1:SpecialDaysOperation")
  DaysOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfOperation")
  DaysNonOperation <- xml2::xml_find_all(SpecialDaysOperation, ".//d1:DaysOfNonOperation")
  # test fix ######
  DaysOperation <- xml2::xml_children(DaysOperation)
  DaysNonOperation <- xml2::xml_children(DaysNonOperation)
  # Seems to work now the if statements use any() on the length check.

  if (any(xml2::xml_length(DaysOperation) > 0)) {
    DaysOperation_StartDate <- import_simple(DaysOperation, ".//d1:StartDate")
    DaysOperation_EndDate <- import_simple(DaysOperation, ".//d1:EndDate")
    DaysOperation_Note <- import_simple_xml(DaysOperation, ".//d1:Note")
    DaysOperation <- data.frame(
      type = "DaysOperation",
      StartDate = DaysOperation_StartDate,
      EndDate = DaysOperation_EndDate,
      Note = DaysOperation_Note,
      stringsAsFactors = FALSE
    )
  } else {
    DaysOperation <- NULL
  }

  if (any(xml2::xml_length(DaysNonOperation) > 0)) {
    DaysNonOperation_StartDate <- import_simple(DaysNonOperation, ".//d1:StartDate")
    DaysNonOperation_EndDate <- import_simple(DaysNonOperation, ".//d1:EndDate")
    DaysNonOperation_Note <- import_simple_xml(DaysNonOperation, ".//d1:Note")
    if (length(DaysNonOperation_Note) == 0) {
      DaysNonOperation_Note <- rep(NA, length(DaysNonOperation_StartDate))
    }
    DaysNonOperation <- data.frame(
      type = "DaysNonOperation",
      StartDate = DaysNonOperation_StartDate,
      EndDate = DaysNonOperation_EndDate,
      Note = DaysNonOperation_Note,
      stringsAsFactors = FALSE
    )
  } else {
    DaysNonOperation <- NULL
  }

  if (!is.null(DaysOperation) & !is.null(DaysNonOperation)) {
    SpecialDaysOperation <- rbind(DaysOperation, DaysNonOperation)
  } else if (!is.null(DaysOperation)) {
    SpecialDaysOperation <- DaysOperation
  } else {
    SpecialDaysOperation <- DaysNonOperation
  }



  StandardService <- data.frame(
    Direction = Direction,
    VehicleType = VehicleType,
    RouteRef = RouteRef,
    JourneyPatternSectionRefs = JourneyPatternSectionRefs,
    JourneyPatternID = JourneyPatternID
  )

  Services_main <- data.frame(
    ServiceCode = ServiceCode,
    # PrivateCode = PrivateCode,
    Mode = Mode,
    Description = Description,
    RegisteredOperatorRef = RegisteredOperatorRef,
    StartDate = as.Date(StartDate),
    EndDate = as.Date(EndDate),
    DaysOfWeek = DaysOfWeek,
    StopRequirements = StopRequirements,
    Origin = Origin,
    Destination = Destination,
    LineName = LineName,
    BankHolidayNonOperation = BankHolidayNonOperation,
    BankHolidayOperation = BankHolidayOperation,
    stringsAsFactors = FALSE
  )

  if (full_import) {
    Services_main$PrivateCode <- PrivateCode
  }


  results <- list(StandardService, Services_main, SpecialDaysOperation, Lines)
  names(results) <- c("StandardService", "Services_main", "SpecialDaysOperation", "Lines")

  return(results)
}



#' Imports when Multiple Values
#' Returns a dataframe with appopiate lookup id
#' @param vehiclejourneys desc
#' @noRd
import_notes2 <- function(vehiclejourneys) {
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  result <- list()
  # materialise the child list once: xml_child(x, i) walks the sibling list
  # from the start on every call, which is O(n^2) over a large file
  children <- xml2::xml_children(vehiclejourneys)
  for (i in seq(1, xml2::xml_length(vehiclejourneys))) {
    # message(i)
    chld <- children[[i]]
    NoteCode <- import_simple(chld, ".//d1:NoteCode")
    NoteText <- import_simple(chld, ".//d1:NoteText")
    if (length(NoteCode) == 0) {
      NoteCode <- NA
    }
    if (length(NoteText) == 0) {
      NoteText <- NA
    }
    res <- data.frame(
      VehicleJourneyCode = VehicleJourneyCode[i],
      NoteCode = NoteCode,
      NoteText = NoteText,
      stringsAsFactors = FALSE
    )
    result[[i]] <- res
  }
  result <- dplyr::bind_rows(result)
  result <- result[!is.na(result$NoteCode), ]

  return(result)
}
