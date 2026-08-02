# Known operator mode misdeclarations.
#
# A TransXChange registration states its own mode, and UK2GTFS believes it.
# Occasionally an operator states the wrong one, and nothing in the file
# contradicts it, so no amount of care in the converter will catch it - the
# only fix is to know about the case. This is the same situation as
# `naptan_replace`, which patches stop coordinates that NaPTAN gets wrong.

#' Operators whose declared mode is known to be wrong
#'
#' @description A lookup of operators that declare the wrong `Mode` in their
#'   TransXChange registrations, used by [transxchange2gtfs()] to correct
#'   `route_type` after conversion.
#'
#' @details
#' Each row names an operator by National Operator Code and gives the mode its
#' services should have. `route_short_name` restricts the correction to one
#' line of that operator, for the common case of an operator running more than
#' one mode - Blackpool Transport runs a tramway and eighteen bus routes, and
#' only the tramway would ever need correcting. `NA` applies the correction to
#' every route of the operator.
#'
#' `mode` is any value [clean_route_type()] accepts: `"bus"`, `"coach"`,
#' `"ferry"`, `"rail"`, `"metro"`, `"underground"`, `"tram"`, `"trolleybus"`,
#' `"air"`.
#'
#' `route_short_name` is matched against the name **as it appears in the
#' converted feed**, after the shortening rules `transxchange2gtfs()` applies
#' to long line names, not against the raw `LineName`. Both it and `noc` are
#' matched case insensitively.
#'
#' The bar for adding a row is that the declaration is checkably wrong rather
#' than merely surprising: the operator runs a mode the file does not claim,
#' and some independent source says so. Corrections are deliberately narrow —
#' keyed to an operator, and usually to a single line — because a wrong entry
#' here silently re-labels real service.
#'
#' Current entries:
#'
#' * **NEXT / Nottingham Express Transit.** Its tramway is registered in TNDS
#'   as `<Mode>bus</Mode>` (`notts_NEXT_TRAM_NETTRAM.xml`, service `NETTRAM`,
#'   line `TRAM`, Clifton/Toton – Phoenix Park/Hucknall). Every other British
#'   tramway in TNDS is coded `tram`, and the DfT's BODS GTFS carries this one
#'   as a tram, so any analysis counting buses credits Nottingham with a tram
#'   network's worth of bus service — 144,488 vehicle journeys over four weeks
#'   in the July 2026 snapshot.
#'
#' @return a data frame of `noc`, `route_short_name`, `mode` and `note`
#' @export
#' @examples
#' operator_mode_overrides()
operator_mode_overrides <- function() {
  data.frame(
    noc = "NEXT",
    route_short_name = "TRAM",
    mode = "tram",
    note = paste("Nottingham Express Transit tramway registered as",
                 "Mode=bus in TNDS"),
    stringsAsFactors = FALSE
  )
}


#' Apply the operator mode overrides to a routes table
#'
#' @param route_type integer vector of GTFS route types, already cleaned
#' @param agency_id character vector of operator codes, one per route. These
#'   must be National Operator Codes for the lookup to hit; where a file has
#'   not been resolved to a NOC no override is applied, which is the safe
#'   outcome.
#' @param route_short_name character vector of line names, one per route
#' @param overrides a table shaped like [operator_mode_overrides()]
#' @return `route_type`, with corrected entries replaced
#' @noRd
apply_mode_overrides <- function(route_type, agency_id, route_short_name,
                                 overrides = operator_mode_overrides()) {
  if (is.null(overrides) || nrow(overrides) == 0 || length(route_type) == 0) {
    return(route_type)
  }
  noc <- toupper(trimws(as.character(agency_id)))
  line <- toupper(trimws(as.character(route_short_name)))

  for (i in seq_len(nrow(overrides))) {
    o <- overrides[i, ]
    hit <- !is.na(noc) & noc == toupper(trimws(o$noc))
    if (!is.na(o$route_short_name)) {
      hit <- hit & !is.na(line) & line == toupper(trimws(o$route_short_name))
    }
    if (any(hit)) {
      route_type[hit] <- clean_route_type(o$mode)
    }
  }
  route_type
}
