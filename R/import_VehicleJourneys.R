#' Import Vehicle Journeys
#' ????
#' @param vehiclejourneys desc
#' @noRd

import_vehiclejourneys2 <- function(vehiclejourneys) {

  # PrivateCode <- import_simple(vehiclejourneys, ".//d1:PrivateCode")
  VehicleJourneyCode <- import_simple(vehiclejourneys, ".//d1:VehicleJourneyCode")
  ServiceRef <- import_simple(vehiclejourneys, ".//d1:ServiceRef")
  LineRef <- import_simple(vehiclejourneys, ".//d1:LineRef")
  if (length(LineRef) != length(VehicleJourneyCode)) {
    # LineRef is mandatory per the TransXchange schema, but be robust to files
    # that omit it on some journeys: align per journey, NA where missing
    LineRef <- xml2::xml_text(xml2::xml_find_first(xml2::xml_children(vehiclejourneys), "d1:LineRef"))
  }
  # JourneyPatternRef <- import_simple(vehiclejourneys, ".//d1:JourneyPatternRef")
  DepartureTime <- import_simple(vehiclejourneys, ".//d1:DepartureTime")
  BankHolidaysOperate <- import_simple(vehiclejourneys, ".//d1:BankHolidaysOperate")
  Notes <- xml2::xml_find_all(vehiclejourneys, ".//d1:Note")

  JourneyPatternRef <- xml2::xml_text(xml2::xml_find_first(xml2::xml_children(vehiclejourneys), "d1:JourneyPatternRef"))
  VehicleJourneyRef <- xml2::xml_text(xml2::xml_find_first(xml2::xml_children(vehiclejourneys), "d1:VehicleJourneyRef"))

  if (any(xml2::xml_length(Notes) > 0)) {
    Notes <- import_notes2(vehiclejourneys)
  } else {
    Notes <- NA
  }

  if (length(BankHolidaysOperate) == 0) {
    BankHolidaysOperate <- rep(NA, length(VehicleJourneyCode))
  }
  BankHolidaysNoOperate <- xml2::xml_text(xml2::xml_find_all(vehiclejourneys, ".//d1:BankHolidaysNoOperate"))
  if (length(BankHolidaysNoOperate) == 0) {
    BankHolidaysNoOperate <- rep(NA, length(VehicleJourneyCode))
  }

  if (length(JourneyPatternRef) != length(VehicleJourneyCode)) {
    # JourneyPatternRef <- import_withmissing(vehiclejourneys, ".//d1:JourneyPatternRef", 8)

    # VehicleJourneyRef <- import_withmissing(vehiclejourneys, ".//d1:VehicleJourneyRef", 8)
    # JourneyPatternRef <- ifelse(is.na(JourneyPatternRef),
    #                             VehicleJourneyRef,
    #                             JourneyPatternRef)

    stop("JourneyPatternRef and VehicleJourneyCode not same length")
  }


  vj_simple <- data.frame( # PrivateCode = PrivateCode,
    VehicleJourneyCode = VehicleJourneyCode,
    ServiceRef = ServiceRef,
    LineRef = LineRef,
    JourneyPatternRef = JourneyPatternRef,
    VehicleJourneyRef = VehicleJourneyRef,
    DepartureTime = DepartureTime,
    # days = days,
    BankHolidaysOperate = BankHolidaysOperate,
    BankHolidaysNoOperate = BankHolidaysNoOperate,
    stringsAsFactors = FALSE
  )

  OperatingProfile <- xml2::xml_find_all(vehiclejourneys, ".//d1:OperatingProfile")
  if (length(OperatingProfile) > 0) {
    OperatingProfile <- import_OperatingProfile(OperatingProfile)
    SpecialDays <- OperatingProfile$SpecialDays
    OperatingProfile <- OperatingProfile$OperatingProfile
    if (nrow(SpecialDays) > 0) {
      DaysOfOperation <- SpecialDays[, c("VehicleJourneyCode", "OperateStart", "OperateEnd")]
      DaysOfNonOperation <- SpecialDays[, c("VehicleJourneyCode", "NoOperateStart", "NoOperateEnd")]
      names(DaysOfOperation) <- c("VehicleJourneyCode", "StartDate", "EndDate")
      names(DaysOfNonOperation) <- c("VehicleJourneyCode", "StartDate", "EndDate")
      DaysOfOperation <- DaysOfOperation[!is.na(DaysOfOperation$StartDate), ]
      DaysOfNonOperation <- DaysOfNonOperation[!is.na(DaysOfNonOperation$StartDate), ]
      if (nrow(DaysOfOperation) == 0) {
        DaysOfOperation <- NULL
      }
      if (nrow(DaysOfNonOperation) == 0) {
        DaysOfNonOperation <- NULL
      }
    } else {
      DaysOfOperation <- NULL
      DaysOfNonOperation <- NULL
    }

    vj_simple <- dplyr::left_join(vj_simple, OperatingProfile, by = "VehicleJourneyCode")
  } else {
    OperatingProfile <- NULL
    DaysOfOperation <- NULL
    DaysOfNonOperation <- NULL
    vj_simple$DaysOfWeek <- NA
    vj_simple$HolidaysOnly <- NA
    vj_simple$BHDaysOfOperation <- NA
    vj_simple$BHDaysOfNonOperation <- NA
    vj_simple$ServicedDaysOfOperation <- NA
    vj_simple$ServicedDaysOfOperationType <- NA
    vj_simple$ServicedDaysOfNonOperation <- NA
    vj_simple$ServicedDaysOfNonOperationType <- NA
  }





  result <- list(vj_simple, DaysOfOperation, DaysOfNonOperation, Notes)
  names(result) <- c("VehicleJourneys", "DaysOfOperation", "DaysOfNonOperation", "VJ_Notes")
  # JPS                   <- xml_children(journeypatternsections)
  # JPS_id                <- xml2::xml_text(xml2::xml_find_all(JPS, "@id"))
  # JPS_id                <- rep(JPS_id, times = xml2::xml_length(JPS, only_elements = FALSE))
  return(result)
}
