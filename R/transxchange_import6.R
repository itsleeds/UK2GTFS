#' Import a TransXchange XML file
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param run_debug logical, if TRUE extra checks are performed, default TRUE
#' @param full_import logical, if false data no needed for GTFS is excluded
#'
#' @details This function imports the raw transXchange XML files and converts
#' them to a R readable format.
#'
#' A single TransXchange file may declare several `<Service>` elements (a
#' registration covering, say, the CB2 to CB6 school services as one document).
#' The export path is written around one service per object - it reads
#' `Services_main$...[1]` for the route id, name, agency and mode - so such a
#' file is split into one import result per service and the result carries
#' class `txc_multi`. [transxchange2gtfs()] flattens those back into the list
#' of objects it converts, so each service becomes its own GTFS route.
#'
#' @return For a single-service file, a named list of data frames, one per
#'   TransXchange section, or NULL if the file contains nothing to convert.
#'   For a multi-service file, a `txc_multi` list of such objects.
#' @export

transxchange_import <- function(file, run_debug = TRUE, full_import = FALSE) {
  xml <- xml2::read_xml(file)

  # Several services in one file cannot be represented by one import object,
  # so recurse over per-service copies of the document instead of failing.
  if (xml2::xml_length(xml2::xml_child(xml, "d1:Services")) > 1) {
    return(txc_import_multiservice(file, xml,
                                   run_debug = run_debug,
                                   full_import = full_import))
  }

  txc_import_single(xml, file, run_debug = run_debug,
                    full_import = full_import)
}


#' Import one service from a multi-service TransXchange file
#'
#' Re-reads `file` once per `<Service>`, prunes the copy down to that service
#' and its vehicle journeys, and imports it as though it had been published on
#' its own.
#'
#' @param file character, path to the XML file (used for re-reading and for the
#'   `filename` field of each result)
#' @param xml the already-parsed document, used only to count and name services
#' @param run_debug,full_import passed to the single-service importer
#' @noRd
txc_import_multiservice <- function(file, xml, run_debug = TRUE,
                                    full_import = FALSE) {
  services <- xml2::xml_children(xml2::xml_child(xml, "d1:Services"))
  codes <- xml2::xml_text(xml2::xml_find_first(services, "d1:ServiceCode"))

  # Splitting is only safe if every journey says which service it belongs to.
  # Without that the journeys cannot be divided up, and guessing would credit
  # one service with another's trips - so fail loudly, as this function's
  # predecessor did for every multi-service file.
  vjs <- xml2::xml_children(xml2::xml_child(xml, "d1:VehicleJourneys"))
  refs <- xml2::xml_text(xml2::xml_find_first(vjs, "d1:ServiceRef"))
  if (length(vjs) > 0 && (anyNA(refs) || !all(refs %in% codes))) {
    stop("More than one service, and VehicleJourneys cannot be attributed to ",
         "them: ", sum(is.na(refs)), " with no ServiceRef, ",
         sum(!is.na(refs) & !refs %in% codes), " referencing an absent service")
  }
  if (anyDuplicated(codes) > 0) {
    stop("More than one service, and ServiceCodes are not unique: ",
         paste(codes[duplicated(codes)], collapse = ", "))
  }

  res <- lapply(seq_along(codes), function(i) {
    doc <- xml2::read_xml(file)
    txc_prune_to_service(doc, i)
    txc_import_single(doc, file, run_debug = run_debug,
                      full_import = full_import)
  })

  # A service with no journeys of its own imports as NULL, exactly as a
  # single-service file with an empty <VehicleJourneys/> would.
  res <- res[!vapply(res, is.null, logical(1))]
  if (length(res) == 0) {
    return(NULL)
  }
  structure(res, class = "txc_multi")
}


#' Prune a TransXchange document to a single service, in place
#'
#' Drops every `<Service>` but the `i`th, and every `<VehicleJourney>` that
#' does not reference it. Journey patterns live inside `<Service>` and so go
#' with their service; the shared `<JourneyPatternSections>` and `<StopPoints>`
#' are left alone, as sections no journey pattern references are dropped when
#' stop times are built and unreferenced stops are removed on cleaning.
#'
#' @param xml a parsed TransXchange document, modified in place
#' @param i integer, index of the service to keep
#' @noRd
txc_prune_to_service <- function(xml, i) {
  services <- xml2::xml_children(xml2::xml_child(xml, "d1:Services"))
  keep <- xml2::xml_text(xml2::xml_find_first(services[[i]], "d1:ServiceCode"))
  xml2::xml_remove(services[-i])

  vjs <- xml2::xml_children(xml2::xml_child(xml, "d1:VehicleJourneys"))
  if (length(vjs) > 0) {
    refs <- xml2::xml_text(xml2::xml_find_first(vjs, "d1:ServiceRef"))
    drop <- is.na(refs) | refs != keep
    if (any(drop)) {
      xml2::xml_remove(vjs[drop])
    }
  }
  invisible(xml)
}


#' Import a single-service TransXchange document
#'
#' The body of [transxchange_import()] for the one-service case.
#'
#' @param xml a parsed TransXchange document declaring at most one service
#' @param file character, path the document was read from, recorded as
#'   `filename` in the result
#' @param run_debug,full_import as [transxchange_import()]
#' @noRd
txc_import_single <- function(xml, file, run_debug = TRUE,
                             full_import = FALSE) {

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
    # transxchange_import() splits multi-service documents before calling this,
    # so more than one service here means the split did not happen and the
    # export would silently collapse them into a single route.
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
  if (xml2::xml_length(VehicleJourneys) == 0) {
    # An empty <VehicleJourneys/> means there are no trips to convert
    warning("No VehicleJourneys in ", file)
    return(NULL)
  }
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
