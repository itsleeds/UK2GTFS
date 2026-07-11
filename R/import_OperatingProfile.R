import_child <- function(children, nms, nm) {
  if (nm %in% nms) {
    return(children[match(nm, nms)])
  } else {
    return(NULL)
  }
}

import_name <- function(node) {
  if (!is.null(node)) {
    return(xml2::xml_name(xml2::xml_children(node)))
  } else {
    return(NA)
  }
}

import_OperatingProfile <- function(OperatingProfile) {
  result <- list()
  result_special <- list()
  # for(i in seq(1, 10)){
  for (i in seq(1, length(OperatingProfile))) {
    chld <- OperatingProfile[i]
    # VehicleJourneyCode <- xml2::xml_text(xml2::xml_child(xml2::xml_parent(chld), "d1:VehicleJourneyCode"))
    VehicleJourneyCode <- xml2::xml_children(xml2::xml_parent(chld))
    nms_VehicleJourneyCode <- xml2::xml_name(VehicleJourneyCode)
    VehicleJourneyCode <- import_child(VehicleJourneyCode, nms_VehicleJourneyCode, "VehicleJourneyCode")
    VehicleJourneyCode <- xml2::xml_text(VehicleJourneyCode)

    # Top Level Sections
    chld <- xml2::xml_children(chld)
    nms <- xml2::xml_name(chld)
    RegularDayType <- import_child(chld, nms, "RegularDayType")
    ServicedOrganisationDayType <- import_child(chld, nms, "ServicedOrganisationDayType")
    BankHolidayOperation <- import_child(chld, nms, "BankHolidayOperation")
    SpecialDaysOperation <- import_child(chld, nms, "SpecialDaysOperation")

    # Main Section #########################
    # RegularDayType
    if (!is.null(RegularDayType)) {
      RegularDayType <- xml2::xml_children(RegularDayType)
      nms_RegularDayType <- xml2::xml_name(RegularDayType)

      DaysOfWeek <- import_child(RegularDayType, nms_RegularDayType, "DaysOfWeek")
      DaysOfWeek <- import_name(DaysOfWeek)

      HolidaysOnly <- import_child(RegularDayType, nms_RegularDayType, "HolidaysOnly")
      if (!is.null(HolidaysOnly)) {
        HolidaysOnly <- xml2::xml_name(HolidaysOnly)
      } else {
        HolidaysOnly <- NA
      }
    } else {
      DaysOfWeek <- NA
      HolidaysOnly <- NA
    }

    # ServicedOrganisationDayType
    if (!is.null(ServicedOrganisationDayType)) {
      ServicedOrganisationDayType <- xml2::xml_children(ServicedOrganisationDayType)
      nms_ServicedOrganisationDayType <- xml2::xml_name(ServicedOrganisationDayType)

      ServicedDaysOfOperation <- import_child(ServicedOrganisationDayType, nms_ServicedOrganisationDayType, "DaysOfOperation")
      ServicedDaysOfNonOperation <- import_child(ServicedOrganisationDayType, nms_ServicedOrganisationDayType, "DaysOfNonOperation")

      # message(str(xml2::as_list(ServicedOrganisationDayType)))
      # ServicedDaysOfOperation <- xml2::xml_child(ServicedOrganisationDayType, "d1:DaysOfOperation")
      # ServicedDaysOfNonOperation <- xml2::xml_child(ServicedOrganisationDayType, "d1:DaysOfNonOperation")

      # Check for children
      sdo_check <- try(xml2::xml_child(ServicedDaysOfOperation), silent = TRUE)
      if(inherits(sdo_check, "try-error")){
        ServicedDaysOfOperation <- NULL
      }
      sndo_check <- try(xml2::xml_child(ServicedDaysOfNonOperation), silent = TRUE)
      if(inherits(sndo_check, "try-error")){
        ServicedDaysOfNonOperation <- NULL
      }

      if (!is.null(ServicedDaysOfOperation)) {
        ServicedDaysOfOperationType <- xml2::xml_name(xml2::xml_child(ServicedDaysOfOperation))
        ServicedDaysOfOperationRef <- xml2::xml_find_all(ServicedDaysOfOperation, ".//d1:ServicedOrganisationRef")
        ServicedDaysOfOperationRef <- xml2::xml_text(ServicedDaysOfOperationRef)
        ServicedDaysOfOperation <- data.frame(ServicedDaysOfOperation = ServicedDaysOfOperationRef,
                                              ServicedDaysOfOperationType = ServicedDaysOfOperationType,
                                              stringsAsFactors = FALSE)
      } else {
        ServicedDaysOfOperation <- NA
      }

      if (!is.null(ServicedDaysOfNonOperation)) {
        ServicedDaysOfNonOperationType <- xml2::xml_name(xml2::xml_child(ServicedDaysOfNonOperation))
        ServicedDaysOfNonOperationRef <- xml2::xml_find_all(ServicedDaysOfNonOperation, ".//d1:ServicedOrganisationRef")
        ServicedDaysOfNonOperationRef <- xml2::xml_text(ServicedDaysOfNonOperationRef)
        ServicedDaysOfNonOperation <- data.frame(ServicedDaysOfNonOperation = ServicedDaysOfNonOperationRef,
                                              ServicedDaysOfNonOperationType = ServicedDaysOfNonOperationType,
                                              stringsAsFactors = FALSE)
      } else {
        ServicedDaysOfNonOperation <- NA
      }
    } else {
      ServicedDaysOfOperation <- NA
      ServicedDaysOfNonOperation <- NA
    }

    # BankHolidayOperation
    if (!is.null(BankHolidayOperation)) {
      BankHolidayOperation <- xml2::xml_children(BankHolidayOperation)
      nms_BankHolidayOperation <- xml2::xml_name(BankHolidayOperation)

      BHDaysOfNonOperation <- import_child(BankHolidayOperation, nms_BankHolidayOperation, "DaysOfNonOperation")
      BHDaysOfNonOperation <- import_name(BHDaysOfNonOperation)

      BHDaysOfOperation <- import_child(BankHolidayOperation, nms_BankHolidayOperation, "DaysOfOperation")
      BHDaysOfOperation <- import_name(BHDaysOfOperation)

      # BHDaysOfNonOperation <- xml2::xml_child(BankHolidayOperation, "d1:DaysOfNonOperation")
      # BHDaysOfOperation    <- xml2::xml_child(BankHolidayOperation, "d1:DaysOfOperation")
      #
      # # Should be text based e.g. "AllBankHolidays"
      # BHDaysOfNonOperation <- xml2::xml_name(xml2::xml_children(BHDaysOfNonOperation))
      # BHDaysOfOperation <- xml2::xml_name(xml2::xml_children(BHDaysOfOperation))

      # # Clean NA
      # if(length(BHDaysOfNonOperation) == 0){
      #   BHDaysOfNonOperation <- NA
      # }
      #
      # if(length(BHDaysOfOperation) == 0){
      #   BHDaysOfOperation <- NA
      # }
    } else {
      BHDaysOfNonOperation <- NA
      BHDaysOfOperation <- NA
    }

    # SpecialDaysOperation
    if (!is.null(SpecialDaysOperation)) {
      SpecialDaysOperation <- xml2::xml_children(SpecialDaysOperation)
      nms_SpecialDaysOperation <- xml2::xml_name(SpecialDaysOperation)

      SDDaysOfNonOperation <- import_child(SpecialDaysOperation, nms_SpecialDaysOperation, "DaysOfNonOperation")
      SDDaysOfOperation <- import_child(SpecialDaysOperation, nms_SpecialDaysOperation, "DaysOfOperation")

      # SDDaysOfNonOperation <- xml2::xml_child(SpecialDaysOperation, "d1:DaysOfNonOperation")
      # SDDaysOfOperation    <- xml2::xml_child(SpecialDaysOperation, "d1:DaysOfOperation")

      if (!is.null(SDDaysOfNonOperation)) {
        SDDaysOfNonOperation_start <- xml2::xml_find_all(SDDaysOfNonOperation, ".//d1:StartDate")
        SDDaysOfNonOperation_start <- xml2::xml_text(SDDaysOfNonOperation_start)
        SDDaysOfNonOperation_end <- xml2::xml_find_all(SDDaysOfNonOperation, ".//d1:EndDate")
        SDDaysOfNonOperation_end <- xml2::xml_text(SDDaysOfNonOperation_end)
      } else {
        SDDaysOfNonOperation_start <- NA
        SDDaysOfNonOperation_end <- NA
      }


      if (!is.null(SDDaysOfOperation)) {
        SDDaysOfOperation_start <- xml2::xml_find_all(SDDaysOfOperation, ".//d1:StartDate")
        SDDaysOfOperation_start <- xml2::xml_text(SDDaysOfOperation_start)
        SDDaysOfOperation_end <- xml2::xml_find_all(SDDaysOfOperation, ".//d1:EndDate")
        SDDaysOfOperation_end <- xml2::xml_text(SDDaysOfOperation_end)
      } else {
        SDDaysOfOperation_start <- NA
        SDDaysOfOperation_end <- NA
      }

      # Check for when lenghts don't match
      lns <- length(SDDaysOfNonOperation_start)
      lne <- length(SDDaysOfNonOperation_end)
      los <- length(SDDaysOfOperation_start)
      loe <- length(SDDaysOfOperation_end)
      laa <- c(lns, lne, los, loe)


      if (length(unique(laa)) != 1) {
        if (lns != max(laa)) {
          SDDaysOfNonOperation_start <- c(SDDaysOfNonOperation_start, rep(NA, times = max(laa) - lns))
        }

        if (lne != max(laa)) {
          SDDaysOfNonOperation_end <- c(SDDaysOfNonOperation_end, rep(NA, times = max(laa) - lne))
        }

        if (los != max(laa)) {
          SDDaysOfOperation_start <- c(SDDaysOfOperation_start, rep(NA, times = max(laa) - los))
        }

        if (loe != max(laa)) {
          SDDaysOfOperation_end <- c(SDDaysOfOperation_end, rep(NA, times = max(laa) - loe))
        }
      }


      ssdf <- data.frame(
        VehicleJourneyCode = VehicleJourneyCode,
        OperateStart = as.Date(SDDaysOfOperation_start),
        OperateEnd = as.Date(SDDaysOfOperation_end),
        NoOperateStart = as.Date(SDDaysOfNonOperation_start),
        NoOperateEnd = as.Date(SDDaysOfNonOperation_end),
        stringsAsFactors = FALSE
      )

      result_special[[i]] <- ssdf
      rm(
        SDDaysOfOperation_start, SDDaysOfOperation_end,
        SDDaysOfNonOperation_start, SDDaysOfNonOperation_end,
        ssdf
      )
    } else {
      result_special[[i]] <- NULL
    }


    # Build Results #######################
    res <- data.frame(
      VehicleJourneyCode = VehicleJourneyCode,
      DaysOfWeek = paste(DaysOfWeek, collapse = " "),
      HolidaysOnly = paste(HolidaysOnly, collapse = " "),
      # comma-separated so multi-holiday lists survive break_up_holidays2()
      BHDaysOfOperation = paste(BHDaysOfOperation, collapse = ", "),
      BHDaysOfNonOperation = paste(BHDaysOfNonOperation, collapse = ", "),
      #ServicedDaysOfOperation = ServicedDaysOfOperation,
      #ServicedDaysOfNonOperation = ServicedDaysOfNonOperation,
      stringsAsFactors = FALSE
    )

    # merge(by = NULL) is a cross join: a journey can reference several
    # ServicedOrganisations, giving these frames more than one row each.
    # The resulting duplicate journey rows are collapsed again in
    # transxchange_export() once the organisation dates have been extracted.
    if(inherits(ServicedDaysOfOperation, "data.frame")){
      res <- merge(res, ServicedDaysOfOperation, by = NULL)
    } else {
      res$ServicedDaysOfOperation <- NA
      res$ServicedDaysOfOperationType <- NA
    }

    if(inherits(ServicedDaysOfNonOperation, "data.frame")){
      res <- merge(res, ServicedDaysOfNonOperation, by = NULL)
    } else {
      res$ServicedDaysOfNonOperation <- NA
      res$ServicedDaysOfNonOperationType <- NA
    }

    result[[i]] <- res
    rm(
      DaysOfWeek, HolidaysOnly,
      BHDaysOfOperation, BHDaysOfNonOperation
    )
  }
  result <- dplyr::bind_rows(result)
  result_special <- dplyr::bind_rows(result_special)

  # Check for HolidaysOnly services with NA Days of the week
  result$DaysOfWeek <- ifelse(result$DaysOfWeek == "NA",
                              NA_character_, result$DaysOfWeek)
  result$DaysOfWeek <- ifelse(is.na(result$DaysOfWeek) &
           result$HolidaysOnly == "HolidaysOnly",
         "HolidaysOnly", result$DaysOfWeek)

  result_final <- list(result, result_special)
  names(result_final) <- c("OperatingProfile", "SpecialDays")
  return(result_final)
}
