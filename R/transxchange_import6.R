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

  # Journeys without a profile of their own take their journey pattern's or
  # their service's, whole; written into each journey so that everything
  # below reads exactly one profile per journey. See txc_inherit_profiles().
  txc_inherit_profiles(xml)

  ## StopPoints ##########################################
  StopPoints <- xml2::xml_child(xml, "d1:StopPoints")
  # The locations the file itself gives, kept apart from the stops table:
  # transxchange2gtfs() decides after merging whether to use them (see its
  # stop_locations argument).
  StopLocations <- import_stop_locations(StopPoints)
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
    # The service's (and journey patterns') special days now reach the
    # journeys through txc_inherit_profiles(), date ranges intact. Applying
    # them again here would add them to journeys with profiles of their own,
    # which TransXChange says replace the service's, and this import kept
    # only the first day of each range.
    SpecialDaysOperation <- NULL
    Lines <- Services$Lines
    rm(Services)
  } else {
    warning("No Services in ",file)
    return(NULL)
  }




  # An OperatingPeriod with no EndDate runs "until further notice" (UK PTI
  # profile). GTFS needs a date, so one is set this many days after the
  # latest of the start, creation and modification dates. It used to be 365,
  # which ended timetables that had simply not been re-published for a year:
  # 283 open-ended files in the November 2026 BODS archive that are the only
  # version of their line, and that TNDS also runs, stopped before the
  # window (Travel Express 1 and 11, Weardale 725, ...). Set the option
  # UK2GTFS_opt_openEndedDays to 365 for the old behaviour.
  open_days <- getOption("UK2GTFS_opt_openEndedDays", default = 3650)

  CreationDate <- as.Date(lubridate::ymd_hms(xml2::xml_attr(xml, "CreationDateTime")))
  ModifiedDate <- as.Date(lubridate::ymd_hms(xml2::xml_attr(xml, "ModificationDateTime")))

  Services_main$EndDate <- dplyr::if_else(
    is.na(Services_main$EndDate),
    as.character(
      max(lubridate::ymd(Services_main$StartDate), CreationDate, ModifiedDate, na.rm=TRUE) +
       lubridate::days(open_days)),
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
    ServicedOrganisations, Lines, StopLocations,
    basename(file)
  )
  names(finalres) <- c(
    "JourneyPatternSections", "Operators", "Routes",
    "RouteSections", "Services_main", "StandardService",
    "SpecialDaysOperation", "StopPoints", "VehicleJourneys",
    "DaysOfOperation", "DaysOfNonOperation",
    "VehicleJourneysTimingLinks", "VehicleJourneys_notes",
    "ServicedOrganisations", "Lines", "StopLocations",
    "filename"
  )

  return(finalres)
}


#' Give every vehicle journey the operating profile it inherits, in place
#'
#' In TransXChange a vehicle journey without an OperatingProfile of its own
#' runs to its journey pattern's profile, else its service's, and a profile
#' replaces the less specific one entirely rather than merging with it (UK
#' PTI profile 5.3.3.1). The import reads profiles per journey only, so a
#' journey with none used to take just the service's days of the week: its
#' bank holiday, school-term (ServicedOrganisation) and special-days rules
#' were lost, and a journey pattern's profile was ignored altogether. A copy
#' of the inherited profile is added to each such journey instead, so every
#' rule reaches it through the one code path.
#'
#' @param xml a parsed single-service TransXchange document, modified in place
#' @return number of journeys given a profile, invisibly
#' @noRd
txc_inherit_profiles <- function(xml) {
  vjs <- xml2::xml_find_all(xml, "d1:VehicleJourneys/d1:VehicleJourney")
  if (!length(vjs)) return(invisible(0L))
  own <- !is.na(xml2::xml_name(xml2::xml_find_first(vjs, "d1:OperatingProfile")))
  if (all(own)) return(invisible(0L))
  svc <- xml2::xml_find_first(xml, "d1:Services/d1:Service/d1:OperatingProfile")
  has_svc <- !is.na(xml2::xml_name(svc))
  jps <- xml2::xml_find_all(xml, "d1:Services/d1:Service//d1:JourneyPattern")
  jp_prof <- xml2::xml_find_first(jps, "d1:OperatingProfile")
  has_jp <- !is.na(xml2::xml_name(jp_prof))
  jp_ids <- xml2::xml_attr(jps, "id")
  jpref <- xml2::xml_text(xml2::xml_find_first(vjs, "d1:JourneyPatternRef"))
  n <- 0L
  for (k in which(!own)) {
    m <- match(jpref[k], jp_ids)
    prof <- if (!is.na(m) && has_jp[m]) jp_prof[[m]] else if (has_svc) svc else NULL
    if (is.null(prof)) next
    xml2::xml_add_child(vjs[[k]], prof)
    n <- n + 1L
  }
  invisible(n)
}
