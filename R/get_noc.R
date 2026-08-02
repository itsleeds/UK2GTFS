# The National Operator Codes database.
#
# A GTFS agency record identifies a row in agency.txt, not a company. British
# feeds routinely file one company under several agency records - sometimes
# under several National Operator Codes, sometimes under a code and again under
# an unresolved local reference - and nothing inside the feed says they are the
# same operator. Traveline's NOC database does say so, and this file makes it
# available.

#' Get the National Operator Codes database
#'
#' @description Downloads Traveline's National Operator Codes register and
#'   returns one row per NOC, with the operator, management division and group
#'   it belongs to.
#'
#' @param url character, the NOC API endpoint
#' @param include_ceased logical, keep codes marked as ceased. `FALSE` by
#'   default; a ceased code can still appear in an old feed, but its names are
#'   the ones that were in use then and are more likely to collide with a
#'   current operator's.
#' @return a data frame of
#'   \describe{
#'     \item{noc}{National Operator Code, as written in TransXChange}
#'     \item{public_name}{the trading name the operator is known by}
#'     \item{licence_name}{the name on its PSV operator's licence}
#'     \item{operator_id}{Traveline's id for the legal operator. Two NOCs with
#'       the same `operator_id` are one company}
#'     \item{operator_name}{that operator's name}
#'     \item{division_id, division}{the management division, one level up}
#'     \item{group_id, group}{the corporate group, one level up again}
#'   }
#'
#' @details
#' The register is a four level hierarchy: code, operator, management division,
#' group. Only the operator level means "the same company" - `First Leeds` and
#' `First Bradford` share an operator, `Stagecoach London` and `Stagecoach
#' Yorkshire` share only a group, and treating a group as one operator would
#' merge companies that happen to have the same owner.
#'
#' The register is a national reference dataset like NaPTAN, and is used the
#' same way: download it once and pass it to the functions that need it, rather
#' than re-downloading per call.
#'
#' @examples
#' \dontrun{
#' noc <- get_noc()
#' # the codes one operator is filed under
#' noc[noc$operator_id == noc$operator_id[noc$noc == "ELBG"], ]
#' }
#' @export
get_noc <- function(url = "https://www.travelinedata.org.uk/noc/api/1.0/nocrecords.xml",
                    include_ceased = FALSE) {
  temp_folder <- file.path(tempdir(), "temp_noc")
  dir.create(temp_folder, showWarnings = FALSE)
  path <- file.path(temp_folder, "nocrecords.xml")

  doc <- tryCatch({
    utils::download.file(url = url, destfile = path, mode = "wb", quiet = TRUE)
    # The file declares UTF-8 and is not: operator names carry Windows-1252
    # smart quotes, which stop a strict parse dead.
    xml2::read_xml(path, encoding = "WINDOWS-1252")
  }, finally = {
    unlink(temp_folder, recursive = TRUE)
  })

  section <- function(name, fields) {
    node <- xml2::xml_find_first(doc, paste0("./", name))
    if (inherits(node, "xml_missing")) {
      stop("the NOC database has no ", name, " section")
    }
    rec <- xml2::xml_children(node)
    out <- lapply(fields, function(f) {
      trimws(xml2::xml_text(xml2::xml_find_first(rec, paste0("./", f))))
    })
    names(out) <- fields
    as.data.frame(out, stringsAsFactors = FALSE)
  }

  noc <- section("NOCTable", c("NOCCODE", "OperatorPublicName",
                               "VOSA_PSVLicenseName", "OpId", "DateCeased"))
  ops <- section("Operators", c("OpId", "OpNm", "ManDivId"))
  div <- section("ManagementDivisions", c("ManDivId", "ManDivNm", "GpId"))
  grp <- section("Groups", c("GpId", "GpNme"))

  if (!include_ceased) {
    noc <- noc[is.na(noc$DateCeased) | !nzchar(noc$DateCeased), , drop = FALSE]
  }

  out <- merge(noc, ops, by = "OpId", all.x = TRUE)
  out <- merge(out, div, by = "ManDivId", all.x = TRUE)
  out <- merge(out, grp, by = "GpId", all.x = TRUE)

  out <- data.frame(
    noc = out$NOCCODE,
    public_name = out$OperatorPublicName,
    licence_name = out$VOSA_PSVLicenseName,
    operator_id = out$OpId,
    operator_name = out$OpNm,
    division_id = out$ManDivId,
    division = out$ManDivNm,
    group_id = out$GpId,
    group = out$GpNme,
    stringsAsFactors = FALSE)
  out <- out[!is.na(out$noc) & nzchar(out$noc), , drop = FALSE]
  out[order(out$noc), , drop = FALSE]
}


#' Canonical form of an operator's name
#'
#' Case, punctuation, the ampersand and the legal suffix are written
#' differently by every publisher and mean nothing: "EAST LONDON BUS & COACH
#' COMPANY LIMITED" and "East London Bus & Coach Co Ltd" are one company.
#'
#' @param x character vector of names
#' @return character vector, "" where there was no usable name
#' @noRd
noc_normalise_name <- function(x) {
  y <- tolower(as.character(x))
  y <- gsub("&", " and ", y, fixed = TRUE)
  y <- gsub("[^a-z0-9]+", " ", y)
  y <- gsub("\\b(company|companies)\\b", "co", y)
  y <- gsub("\\b(limited|ltd|plc|llp)\\b", "ltd", y)
  y <- trimws(gsub("\\s+", " ", y))
  y[is.na(y)] <- ""
  y
}


#' Which operator does each agency record belong to?
#'
#' @description Resolves GTFS agency records to Traveline operator identities,
#'   so that the several records one operator is filed under can be recognised
#'   as one.
#'
#' @param agency_id character vector of `agency_id`s
#' @param agency_name character vector of `agency_name`s, the same length
#' @param noc the NOC database, as returned by [get_noc()]
#' @return a character vector of operator keys, the same length as `agency_id`.
#'   Records that resolve to the same operator get the same key; records that
#'   do not resolve keep a key derived from their own `agency_id`, so they
#'   group exactly as they did before.
#'
#' @details
#' Two ways in, tried in that order.
#'
#' 1. **By code.** `agency_id` is matched against the register's NOCs. This is
#'    the reliable route for a feed converted from TransXChange, where
#'    `agency_id` is the `NationalOperatorCode`.
#' 2. **By name.** `agency_name` is matched, after normalisation, against every
#'    name the register knows an operator by - trading name, PSV licence name
#'    and legal name. This is the only route for a feed whose `agency_id` is
#'    its own invention, as the DfT's GTFS is, with ids like `OP401`.
#'
#' A name that more than one operator answers to resolves nothing and is left
#' alone. That matters: six separate companies in the DfT's July 2026 GTFS are
#' all named `Bee Network`, and treating a shared brand as a shared operator
#' would merge journeys run by different companies.
#'
#' @examples
#' \dontrun{
#' noc <- get_noc()
#' # both of these are Stagecoach London, filed twice by TNDS - once under its
#' # National Operator Code and once under an unresolved local reference
#' noc_operator_key(c("ELBG", "IF"),
#'                  c("Stagecoach London",
#'                    "EAST LONDON BUS & COACH COMPANY LIMITED"), noc)
#' }
#' @export
noc_operator_key <- function(agency_id, agency_name, noc) {
  agency_id <- as.character(agency_id)
  agency_name <- as.character(agency_name)
  fallback <- paste0("\ragency_id\r", agency_id)
  if (is.null(noc) || nrow(noc) == 0) {
    return(fallback)
  }

  by_code <- noc$operator_id[match(toupper(trimws(agency_id)),
                                   toupper(trimws(noc$noc)))]

  alias <- data.frame(
    nm = noc_normalise_name(c(noc$public_name, noc$licence_name,
                              noc$operator_name)),
    op = rep(noc$operator_id, 3L),
    stringsAsFactors = FALSE)
  alias <- unique(alias[nzchar(alias$nm) & !is.na(alias$op), , drop = FALSE])
  # a name shared by two operators identifies neither
  n_op <- tapply(alias$op, alias$nm, function(z) length(unique(z)))
  alias <- alias[n_op[alias$nm] == 1L, , drop = FALSE]
  alias <- alias[!duplicated(alias$nm), , drop = FALSE]

  by_name <- alias$op[match(noc_normalise_name(agency_name), alias$nm)]

  op <- ifelse(!is.na(by_code), by_code, by_name)
  ifelse(is.na(op), fallback, paste0("\roperator\r", op))
}
