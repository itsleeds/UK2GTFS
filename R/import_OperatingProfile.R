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

#' Is each element of a nodeset a missing node?
#' @param nodes result of xml2::xml_find_first() on a nodeset
#' @noRd
node_is_missing <- function(nodes) {
  is.na(xml2::xml_text(nodes))
}

#' Child-name summary of a 1:1 (xml_find_first) container nodeset
#'
#' For each element: "NA" if the container itself is xml_missing (the field
#' was absent), "" if present but childless, otherwise its child element
#' names pasted with `collapse` -- matching the old paste(import_name(x),
#' collapse=) semantics. Vectorised via xml_length()+rep(), the same
#' grouping idiom import_journeypatternsections() uses (JPS_id <-
#' rep(JPS_id, times = xml_length(JPS))), so no document mutation and no
#' ancestor:: lookups are needed.
#' @param container result of xml2::xml_find_first(profiles, xpath)
#' @param collapse separator
#' @noRd
paste_child_names <- function(container, collapse) {
  present <- !node_is_missing(container)
  out <- rep("NA", length(container))
  out[present] <- ""
  if (any(present)) {
    cont <- container[present]
    counts <- xml2::xml_length(cont)
    kids <- xml2::xml_children(cont)
    if (length(kids) > 0) {
      grp <- rep(seq_along(cont), times = counts)
      pasted <- vapply(split(xml2::xml_name(kids), grp), paste,
                       character(1), collapse = collapse)
      out[which(present)[as.integer(names(pasted))]] <- pasted
    }
  }
  out
}

#' Per-profile count + flattened text of a repeated, arbitrary-depth,
#' descendant field reached via a 1:1 container (e.g. one
#' ServicedOrganisationRef, or several, under each profile's
#' ServicedOrganisationDayType/DaysOfOperation)
#'
#' Uses xml_find_num(container, "count(.//tag)"), a single vectorised XPath
#' evaluation (one count per input node, at the C level) -- the same
#' approach as xml_length() above but able to count arbitrarily nested
#' descendants, not just direct children. No ancestor:: lookups, no
#' document mutation.
#' @param container result of xml2::xml_find_first(profiles, xpath),
#'   already subset to the present (non-missing) elements
#' @param tag descendant element name, e.g. "d1:ServicedOrganisationRef"
#' @noRd
descendant_counts_and_text <- function(container, tag) {
  if (length(container) == 0) {
    return(list(k = integer(0), txt = character(0)))
  }
  k <- as.integer(xml2::xml_find_num(container, paste0("count(.//", tag, ")")))
  txt <- xml2::xml_text(xml2::xml_find_all(container, paste0(".//", tag)))
  list(k = k, txt = txt)
}

#' Import the OperatingProfile of every VehicleJourney
#'
#' Fully vectorised, read-only: every field is pulled with xml_find_first()
#' (1:1 per profile, using xml_missing placeholders) or a count()/flat
#' xml_find_all() pair for repeated fields, then regrouped arithmetically.
#' The old implementation looped over every OperatingProfile (one per
#' vehicle journey) doing ~20 xml2 calls plus data.frame construction and
#' merge() per iteration; on TransXChange files where every journey carries
#' SpecialDaysOperation date ranges (e.g. school services, 30k+ journeys per
#' file) that took hours per file.
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

  parents <- xml2::xml_parent(OperatingProfile)
  VehicleJourneyCode <- xml2::xml_text(
    xml2::xml_find_first(parents, "d1:VehicleJourneyCode"))

  DaysOfWeek <- paste_child_names(
    xml2::xml_find_first(OperatingProfile, "d1:RegularDayType/d1:DaysOfWeek"),
    " ")
  HolidaysOnly <- ifelse(
    node_is_missing(
      xml2::xml_find_first(OperatingProfile, "d1:RegularDayType/d1:HolidaysOnly")),
    "NA", "HolidaysOnly")

  # comma-separated so multi-holiday lists survive break_up_holidays2()
  BHDaysOfOperation <- paste_child_names(
    xml2::xml_find_first(OperatingProfile,
                         "d1:BankHolidayOperation/d1:DaysOfOperation"),
    ", ")
  BHDaysOfNonOperation <- paste_child_names(
    xml2::xml_find_first(OperatingProfile,
                         "d1:BankHolidayOperation/d1:DaysOfNonOperation"),
    ", ")

  ## ServicedOrganisationDayType --------------------------------------------
  # A journey may reference several ServicedOrganisations; the old code
  # cross-joined (merge by = NULL) the one-row profile frame with the k-row
  # refs frames, operation refs cycling fastest.
  so_do_container <- xml2::xml_find_first(OperatingProfile,
    "d1:ServicedOrganisationDayType/d1:DaysOfOperation")
  so_no_container <- xml2::xml_find_first(OperatingProfile,
    "d1:ServicedOrganisationDayType/d1:DaysOfNonOperation")
  so_do_active <- !node_is_missing(so_do_container)
  so_no_active <- !node_is_missing(so_no_container)

  so_do_type <- rep(NA_character_, n)
  so_do_type[so_do_active] <- xml2::xml_name(
    xml2::xml_find_first(so_do_container[so_do_active], "*[1]"))
  so_no_type <- rep(NA_character_, n)
  so_no_type[so_no_active] <- xml2::xml_name(
    xml2::xml_find_first(so_no_container[so_no_active], "*[1]"))

  so_do <- descendant_counts_and_text(so_do_container[so_do_active],
                                      "d1:ServicedOrganisationRef")
  so_no <- descendant_counts_and_text(so_no_container[so_no_active],
                                      "d1:ServicedOrganisationRef")

  r_do <- rep(1L, n); r_do[so_do_active] <- so_do$k
  r_no <- rep(1L, n); r_no[so_no_active] <- so_no$k
  k_do <- rep(0L, n); k_do[so_do_active] <- so_do$k
  k_no <- rep(0L, n); k_no[so_no_active] <- so_no$k
  start_do <- cumsum(k_do) - k_do
  start_no <- cumsum(k_no) - k_no

  m <- r_do * r_no
  prof <- rep.int(seq_len(n), m)
  j <- sequence(m) - 1L # 0-based position within each profile's row block

  SDO <- rep(NA_character_, length(prof))
  SDOT <- SDO
  SDNO <- SDO
  SDNOT <- SDO
  sel <- so_do_active[prof]
  SDO[sel] <- so_do$txt[start_do[prof[sel]] + (j[sel] %% r_do[prof[sel]]) + 1L]
  SDOT[sel] <- so_do_type[prof[sel]]
  sel <- so_no_active[prof]
  SDNO[sel] <- so_no$txt[start_no[prof[sel]] + (j[sel] %/% r_do[prof[sel]]) + 1L]
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
  sd_present <- !node_is_missing(
    xml2::xml_find_first(OperatingProfile, "d1:SpecialDaysOperation"))
  sd_do_container <- xml2::xml_find_first(OperatingProfile,
    "d1:SpecialDaysOperation/d1:DaysOfOperation")
  sd_no_container <- xml2::xml_find_first(OperatingProfile,
    "d1:SpecialDaysOperation/d1:DaysOfNonOperation")
  sd_do_present <- !node_is_missing(sd_do_container)
  sd_no_present <- !node_is_missing(sd_no_container)

  os <- descendant_counts_and_text(sd_do_container[sd_do_present], "d1:StartDate")
  oe <- descendant_counts_and_text(sd_do_container[sd_do_present], "d1:EndDate")
  ns <- descendant_counts_and_text(sd_no_container[sd_no_present], "d1:StartDate")
  ne <- descendant_counts_and_text(sd_no_container[sd_no_present], "d1:EndDate")

  os_k <- rep(0L, n); os_k[sd_do_present] <- os$k
  oe_k <- rep(0L, n); oe_k[sd_do_present] <- oe$k
  ns_k <- rep(0L, n); ns_k[sd_no_present] <- ns$k
  ne_k <- rep(0L, n); ne_k[sd_no_present] <- ne$k
  os_start <- cumsum(os_k) - os_k
  oe_start <- cumsum(oe_k) - oe_k
  ns_start <- cumsum(ns_k) - ns_k
  ne_start <- cumsum(ne_k) - ne_k

  maxlen <- pmax(ifelse(sd_do_present, os_k, 1L),
                 ifelse(sd_do_present, oe_k, 1L),
                 ifelse(sd_no_present, ns_k, 1L),
                 ifelse(sd_no_present, ne_k, 1L))
  sdp <- which(sd_present)

  if (length(sdp) > 0) {
    mrow <- maxlen[sdp]
    prof_s <- rep.int(sdp, mrow)
    posn <- sequence(mrow) # 1-based row within each profile's SpecialDays set

    sd_col <- function(fld_present, fld_start, fld_k, fld_txt) {
      val <- rep(NA_character_, length(prof_s))
      sel <- fld_present[prof_s] & posn <= fld_k[prof_s]
      val[sel] <- fld_txt[fld_start[prof_s[sel]] + posn[sel]]
      as.Date(val)
    }

    result_special <- data.frame(
      VehicleJourneyCode = VehicleJourneyCode[prof_s],
      OperateStart = sd_col(sd_do_present, os_start, os_k, os$txt),
      OperateEnd = sd_col(sd_do_present, oe_start, oe_k, oe$txt),
      NoOperateStart = sd_col(sd_no_present, ns_start, ns_k, ns$txt),
      NoOperateEnd = sd_col(sd_no_present, ne_start, ne_k, ne$txt),
      stringsAsFactors = FALSE
    )
  } else {
    result_special <- dplyr::bind_rows(list())
  }

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
