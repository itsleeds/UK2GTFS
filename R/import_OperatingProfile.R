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

#' Import the OperatingProfile of every VehicleJourney
#'
#' Fully vectorised: every field is pulled with one flat xml_find_all()/
#' xml_find_first() over the whole nodeset (C level) and regrouped per
#' profile arithmetically. The old implementation looped over every
#' OperatingProfile (one per vehicle journey) doing ~20 xml2 calls plus
#' data.frame construction and merge() per iteration; on TransXChange files
#' where every journey carries SpecialDaysOperation date ranges (e.g. school
#' services, 30k+ journeys per file) that took hours per file.
#'
#' Semantics preserved exactly, including: "NA" strings for absent
#' sections (the old paste(NA) result), "" for present-but-childless,
#' the merge(by = NULL) cross-join row expansion for journeys referencing
#' several ServicedOrganisations (operation ref cycling fastest), and the
#' NA-padding of SpecialDays start/end date vectors to equal length.
#'
#' @param OperatingProfile xml_nodeset of OperatingProfile elements
#' @noRd
import_OperatingProfile <- function(OperatingProfile) {

  n <- length(OperatingProfile)

  # Temporary index attribute so nodes found by flat searches can be mapped
  # back to their profile; removed again below
  xml2::xml_set_attr(OperatingProfile, "uk2gtfs_tmp_idx",
                     as.character(seq_len(n)))
  anc_idx <- function(nodes) {
    as.integer(xml2::xml_attr(
      xml2::xml_find_first(nodes, "ancestor::d1:OperatingProfile"),
      "uk2gtfs_tmp_idx"))
  }
  # profiles (by index) containing at least one node matching xpath
  present_at <- function(xpath) {
    tabulate(anc_idx(xml2::xml_find_all(OperatingProfile, xpath)), n) > 0
  }
  # Child-name summary per profile, matching the old import_name() + paste()
  # semantics: "NA" where the target node is absent, "" where present but
  # childless, otherwise the child element names pasted with `collapse`.
  paste_child_names <- function(xpath, collapse) {
    out <- rep("NA", n)
    out[present_at(xpath)] <- ""
    kids <- xml2::xml_find_all(OperatingProfile, paste0(xpath, "/*"))
    if (length(kids) > 0) {
      idx <- anc_idx(kids)
      pasted <- vapply(split(xml2::xml_name(kids), idx), paste,
                       character(1), collapse = collapse)
      out[as.integer(names(pasted))] <- pasted
    }
    out
  }
  # flat text + per-profile position for repeated descendants of xpath
  flat_field <- function(xpath) {
    nodes <- xml2::xml_find_all(OperatingProfile, xpath)
    idx <- anc_idx(nodes)
    k <- tabulate(idx, n)
    list(txt = xml2::xml_text(nodes), k = k, start = cumsum(k) - k)
  }

  parents <- xml2::xml_parent(OperatingProfile)
  VehicleJourneyCode <- xml2::xml_text(
    xml2::xml_find_first(parents, "d1:VehicleJourneyCode"))

  DaysOfWeek <- paste_child_names("d1:RegularDayType/d1:DaysOfWeek", " ")
  HolidaysOnly <- ifelse(present_at("d1:RegularDayType/d1:HolidaysOnly"),
                         "HolidaysOnly", "NA")
  # comma-separated so multi-holiday lists survive break_up_holidays2()
  BHDaysOfOperation <- paste_child_names(
    "d1:BankHolidayOperation/d1:DaysOfOperation", ", ")
  BHDaysOfNonOperation <- paste_child_names(
    "d1:BankHolidayOperation/d1:DaysOfNonOperation", ", ")

  ## ServicedOrganisationDayType ------------------------------------------
  # A journey may reference several ServicedOrganisations; the old code
  # cross-joined (merge by = NULL) the one-row profile frame with the k-row
  # refs frames, operation refs cycling fastest. r_* = rows contributed.
  so_do_active <- present_at("d1:ServicedOrganisationDayType/d1:DaysOfOperation")
  so_no_active <- present_at("d1:ServicedOrganisationDayType/d1:DaysOfNonOperation")
  so_do <- flat_field("d1:ServicedOrganisationDayType/d1:DaysOfOperation//d1:ServicedOrganisationRef")
  so_no <- flat_field("d1:ServicedOrganisationDayType/d1:DaysOfNonOperation//d1:ServicedOrganisationRef")

  type_of <- function(xpath) {
    nodes <- xml2::xml_find_all(OperatingProfile, xpath)
    out <- rep(NA_character_, n)
    out[anc_idx(nodes)] <- xml2::xml_name(nodes)
    out
  }
  so_do_type <- type_of("d1:ServicedOrganisationDayType/d1:DaysOfOperation/*[1]")
  so_no_type <- type_of("d1:ServicedOrganisationDayType/d1:DaysOfNonOperation/*[1]")

  r_do <- ifelse(so_do_active, so_do$k, 1L)
  r_no <- ifelse(so_no_active, so_no$k, 1L)
  m <- r_do * r_no
  prof <- rep.int(seq_len(n), m)
  j <- sequence(m) - 1L # 0-based position within each profile's row block

  SDO <- rep(NA_character_, length(prof))
  SDOT <- SDO
  SDNO <- SDO
  SDNOT <- SDO
  sel <- so_do_active[prof]
  SDO[sel] <- so_do$txt[so_do$start[prof[sel]] + (j[sel] %% r_do[prof[sel]]) + 1L]
  SDOT[sel] <- so_do_type[prof[sel]]
  sel <- so_no_active[prof]
  SDNO[sel] <- so_no$txt[so_no$start[prof[sel]] + (j[sel] %/% r_do[prof[sel]]) + 1L]
  SDNOT[sel] <- so_no_type[prof[sel]]

  result <- data.frame(
    VehicleJourneyCode = VehicleJourneyCode[prof],
    DaysOfWeek = DaysOfWeek[prof],
    HolidaysOnly = HolidaysOnly[prof],
    BHDaysOfOperation = BHDaysOfOperation[prof],
    BHDaysOfNonOperation = BHDaysOfNonOperation[prof],
    ServicedDaysOfOperation = SDO,
    ServicedDaysOfOperationType = SDOT,
    ServicedDaysOfNonOperation = SDNO,
    ServicedDaysOfNonOperationType = SDNOT,
    stringsAsFactors = FALSE
  )

  ## SpecialDaysOperation --------------------------------------------------
  # One SpecialDays row set per profile that has a SpecialDaysOperation
  # section: the four start/end vectors (a DaysOf(Non)Operation section that
  # is absent contributes a single NA) are NA-padded to a common length.
  sd_present <- present_at("d1:SpecialDaysOperation")
  sd_do_present <- present_at("d1:SpecialDaysOperation/d1:DaysOfOperation")
  sd_no_present <- present_at("d1:SpecialDaysOperation/d1:DaysOfNonOperation")
  os <- flat_field("d1:SpecialDaysOperation/d1:DaysOfOperation//d1:StartDate")
  oe <- flat_field("d1:SpecialDaysOperation/d1:DaysOfOperation//d1:EndDate")
  ns <- flat_field("d1:SpecialDaysOperation/d1:DaysOfNonOperation//d1:StartDate")
  ne <- flat_field("d1:SpecialDaysOperation/d1:DaysOfNonOperation//d1:EndDate")

  maxlen <- pmax(ifelse(sd_do_present, os$k, 1L),
                 ifelse(sd_do_present, oe$k, 1L),
                 ifelse(sd_no_present, ns$k, 1L),
                 ifelse(sd_no_present, ne$k, 1L))
  sdp <- which(sd_present)

  if (length(sdp) > 0) {
    mrow <- maxlen[sdp]
    prof_s <- rep.int(sdp, mrow)
    posn <- sequence(mrow) # 1-based row within each profile's SpecialDays set

    sd_col <- function(fld, fld_present) {
      val <- rep(NA_character_, length(prof_s))
      sel <- fld_present[prof_s] & posn <= fld$k[prof_s]
      val[sel] <- fld$txt[fld$start[prof_s[sel]] + posn[sel]]
      as.Date(val)
    }

    result_special <- data.frame(
      VehicleJourneyCode = VehicleJourneyCode[prof_s],
      OperateStart = sd_col(os, sd_do_present),
      OperateEnd = sd_col(oe, sd_do_present),
      NoOperateStart = sd_col(ns, sd_no_present),
      NoOperateEnd = sd_col(ne, sd_no_present),
      stringsAsFactors = FALSE
    )
  } else {
    result_special <- dplyr::bind_rows(list())
  }

  xml2::xml_set_attr(OperatingProfile, "uk2gtfs_tmp_idx", NULL)

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
