#' Import a TransXchange XML file
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param run_debug logical, if TRUE extra checks are performed, default TRUE
#' @param full_import logical, if false data no needed for GTFS is excluded
#'
#' @details This function imports the raw transXchange XML files and converts
#' them to a R readable format.
#' @return named list of data frames, one per TransXchange section (or NULL if
#'   the file contains no services)
#' @export

transxchange_import <- function(file, run_debug = TRUE, full_import = FALSE) {
  xml <- xml2::read_xml(file)

  ## StopPoints ##########################################
  StopPoints <- xml2::xml_child(xml, "d1:StopPoints")
  StopPoints <- import_stoppoints(StopPoints, full_import = full_import)

  ## RouteSections ##########################################
  if (full_import) {
    RouteSections <- xml2::xml_child(xml, "d1:RouteSections")
    RouteSections <- xml2::as_list(RouteSections)

    rs_clean <- function(rs) {
      rs_attr <- attributes(rs)$id
      rs <- rs[names(rs) == "RouteLink"]
      rs <- lapply(rs, function(x) {
        tmp <- x$Distance
        ids <- attributes(x)$id
        if (is.null(tmp)) {
          tmp <- NA
        }
        x$LinkID <- ids
        x$Distance <- tmp
        x <- x[c("From", "To", "Distance", "Direction", "LinkID")]
        return(x)
      })
      rs <- data.frame(matrix(unlist(rs), nrow = length(rs), byrow = TRUE), stringsAsFactors = FALSE)
      names(rs) <- c("From", "To", "Distance", "Direction", "LinkID")
      rs$SectionID <- rs_attr
      return(rs)
    }
    RouteSections <- lapply(RouteSections, rs_clean)
    RouteSections <- dplyr::bind_rows(RouteSections)
    RouteSections[] <- lapply(RouteSections, factor)
  } else {
    RouteSections <- NULL
  }


  ## Routes ##########################################
  # Routes <- xml2::xml_child(xml, "d1:Routes")
  # Routes <- import_routes(Routes)
  Routes <- NULL

  ## JourneyPatternSections ##########################################
  JourneyPatternSections <- xml2::xml_child(xml, "d1:JourneyPatternSections")
  if(length(JourneyPatternSections) > 0){
    JourneyPatternSections <- import_journeypatternsections(journeypatternsections = JourneyPatternSections)
  } else {
    JourneyPatternSections <- NULL
  }

  ## VehicleJourneysTimingLinks #############################

  VehicleJourneysTimingLinks <- NULL

  ## Services ##########################################
  Services <- xml2::xml_child(xml, "d1:Services")
  if (run_debug) {
    if (xml2::xml_length(Services) > 1) {
      stop("More than one service")
    }
  }
  if(length(Services) > 0){
    Services <- import_services(Services, full_import = full_import)
    StandardService <- Services$StandardService
    Services_main <- Services$Services_main
    SpecialDaysOperation <- Services$SpecialDaysOperation
    Lines <- Services$Lines
    rm(Services)
  } else {
    warning("No Services in ",file)
    return(NULL)
  }




  # Handle NA in service date
  # Sometimes end date is missing in which case assume service runs for one year

  CreationDate <- as.Date(lubridate::ymd_hms(xml2::xml_attr(xml, "CreationDateTime")))
  ModifiedDate <- as.Date(lubridate::ymd_hms(xml2::xml_attr(xml, "ModificationDateTime")))

  Services_main$EndDate <- dplyr::if_else(
    is.na(Services_main$EndDate),
    as.character(
      max(lubridate::ymd(Services_main$StartDate), CreationDate, ModifiedDate, na.rm=TRUE) +
       lubridate::days(365)),
    as.character(Services_main$EndDate)
  )



  ## Operators ##########################################
  Operators <- xml2::xml_child(xml, "d1:Operators")
  if(length(Operators) == 0){
    # Operators missing
    Operators <- NULL
  } else {
    Operators <- import_operators(operators = Operators)
    if (nrow(Operators) != 1) {
      Operators <- Operators[Operators$OperatorCode %in% Services_main$RegisteredOperatorRef |
                               Operators$OperatorID %in% Services_main$RegisteredOperatorRef, ]
      if (nrow(Operators) != 1) {
        message("Can't match operators to services, forcing link")
        if (nrow(Operators) == 0) {
          Operators <- xml2::xml_child(xml, "d1:Operators")
          Operators <- import_operators(Operators)
          Operators <- Operators[1, ]
          Services_main$RegisteredOperatorRef <- Operators$OperatorCode
        } else {
          stop("Can't force realtionship between Operators and Services")
        }
      }
    }
  }




  ## ServicedOrganisations ############################
  ServicedOrganisations <- xml2::xml_child(xml, "d1:ServicedOrganisations")
  if (xml2::xml_length(ServicedOrganisations) > 0) {
    ServicedOrganisations <- import_ServicedOrganisations(ServicedOrganisations)
  } else {
    ServicedOrganisations <- NULL
  }


  ## VehicleJourneys ##########################################
  VehicleJourneys <- xml2::xml_child(xml, "d1:VehicleJourneys")
  VehicleJourneys <- import_vehiclejourneys2(VehicleJourneys)

  DaysOfOperation <- VehicleJourneys$DaysOfOperation
  DaysOfNonOperation <- VehicleJourneys$DaysOfNonOperation
  VehicleJourneys_notes <- VehicleJourneys$VJ_Notes
  VehicleJourneys <- VehicleJourneys$VehicleJourneys





  ## Final Steps #########################################
  finalres <- list(
    JourneyPatternSections, Operators, Routes,
    RouteSections, Services_main, StandardService,
    SpecialDaysOperation, StopPoints, VehicleJourneys,
    DaysOfOperation, DaysOfNonOperation,
    VehicleJourneysTimingLinks, VehicleJourneys_notes,
    ServicedOrganisations, Lines,
    basename(file)
  )
  names(finalres) <- c(
    "JourneyPatternSections", "Operators", "Routes",
    "RouteSections", "Services_main", "StandardService",
    "SpecialDaysOperation", "StopPoints", "VehicleJourneys",
    "DaysOfOperation", "DaysOfNonOperation",
    "VehicleJourneysTimingLinks", "VehicleJourneys_notes",
    "ServicedOrganisations", "Lines",
    "filename"
  )

  return(finalres)
}
