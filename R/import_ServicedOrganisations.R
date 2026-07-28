
#' Import ServicedOrganisations
#' Imports ServicedOrganisations
#' @param ServicedOrganisations ServicedOrganisations object
#' @noRd
#'
import_ServicedOrganisations <- function(ServicedOrganisations, full_import = FALSE) {
  # Only WorkingDays and Holidays carry the dates this function extracts. The
  # schema allows plenty of descriptive siblings (PrivateCode, PostalAddress,
  # ServicedOrganisationClassification, NatureOfOrganisation, ...) that are
  # simply not needed, so rather than enumerate them, an unrecognised element
  # is tolerated unless it contains dates - which would mean operating dates
  # were being dropped silently.
  children <- xml2::xml_children(xml2::xml_children(ServicedOrganisations))
  unknown <- children[!xml2::xml_name(children) %in%
                        c("OrganisationCode", "Name", "WorkingDays", "Holidays",
                          "ParentServicedOrganisationRef")]
  if (length(unknown) > 0) {
    dates <- xml2::xml_find_num(
      unknown, "count(.//d1:DateRange | .//d1:StartDate | .//d1:EndDate)")
    if (any(dates > 0)) {
      stop("Unknown Structure in ServicedOrganisations")
    }
  }
  result <- list()
  for (i in seq(1, xml2::xml_length(ServicedOrganisations))) {
    sub <- xml2::xml_child(ServicedOrganisations, i)
    OrganisationCode <- import_simple(sub, ".//d1:OrganisationCode")

    WorkingDays <- xml2::xml_find_all(sub, ".//d1:WorkingDays")
    WorkingDays.StartDate <- import_simple(WorkingDays, ".//d1:StartDate")
    WorkingDays.EndDate <- import_simple(WorkingDays, ".//d1:EndDate")

    if (length(WorkingDays.StartDate) != length(WorkingDays.EndDate)) {
      WorkingDays.StartDate <- import_vialoop(WorkingDays, ".//d1:StartDate")
      WorkingDays.EndDate <- import_vialoop(WorkingDays, ".//d1:EndDate")
      WorkingDays.StartDate <- ifelse(is.na(WorkingDays.StartDate),
        WorkingDays.EndDate,
        WorkingDays.StartDate
      )
      WorkingDays.EndDate <- ifelse(is.na(WorkingDays.EndDate),
        WorkingDays.StartDate,
        WorkingDays.EndDate
      )
    }


    Holidays <- xml2::xml_find_all(sub, ".//d1:Holidays")
    Holidays.StartDate <- import_simple(Holidays, ".//d1:StartDate")
    Holidays.EndDate <- import_simple(Holidays, ".//d1:EndDate")
    Holidays.Description <- import_simple(Holidays, ".//d1:Description")

    rep_lengths_work <- sum(xml2::xml_length(WorkingDays))
    rep_lengths_holiday <- sum(xml2::xml_length(Holidays))

    if (length(Holidays.Description) == 0) {
      Holidays.Description <- rep(NA, times = rep_lengths_holiday)
    }

    if (rep_lengths_work > 0 & rep_lengths_holiday == 0) {
      rep_lengths <- rep_lengths_work
      Holidays.StartDate <- rep(NA, times = rep_lengths)
      Holidays.EndDate <- rep(NA, times = rep_lengths)
      Holidays.Description <- rep(NA, times = rep_lengths)
    } else if (rep_lengths_work == 0 & rep_lengths_holiday > 0) {
      rep_lengths <- rep_lengths_holiday
      WorkingDays.StartDate <- rep(NA, times = rep_lengths)
      WorkingDays.EndDate <- rep(NA, times = rep_lengths)
    } else {
      # stop("Lengths of Holidays and working days do not match in ServicedOrganisations")
      if (rep_lengths_work > rep_lengths_holiday) {
        rep_lengths <- rep_lengths_work
        rep_part <- rep_lengths_work - rep_lengths_holiday
        Holidays.StartDate <- c(Holidays.StartDate, rep(NA, times = rep_part))
        Holidays.EndDate <- c(Holidays.EndDate, rep(NA, times = rep_part))

        Holidays.Description <- c(Holidays.Description, rep(NA, times = rep_part))
      } else {
        rep_lengths <- rep_lengths_holiday
        rep_part <- rep_lengths_holiday - rep_lengths_work
        WorkingDays.StartDate <- c(WorkingDays.StartDate, rep(NA, times = rep_part))
        WorkingDays.EndDate <- c(WorkingDays.EndDate, rep(NA, times = rep_part))
      }
    }

    OrganisationCode <- rep(OrganisationCode, times = rep_lengths)


    res <- data.frame(
      OrganisationCode = OrganisationCode,
      WorkingDays.StartDate = as.Date(WorkingDays.StartDate),
      WorkingDays.EndDate = as.Date(WorkingDays.EndDate),
      Holidays.StartDate = as.Date(Holidays.StartDate),
      Holidays.EndDate = as.Date(Holidays.EndDate),
      Holidays.Description = Holidays.Description,
      stringsAsFactors = FALSE
    )

    if (full_import) {
      Name <- import_simple(ServicedOrganisations, ".//d1:Name")
      Name <- rep(Name, times = rep_lengths)
      res$Name <- Name
    }
    result[[i]] <- res
  }

  result <- result[vapply(result, function(x){nrow(x) > 0}, FUN.VALUE = TRUE)] #Remove any empty rows
  result <- dplyr::bind_rows(result)


  return(result)
}
