# NeTEx fares reader
#
# Reads UK Bus Open Data Service (BODS) NeTEx fare files that follow the
# DfT "fxc" (Fares & Ticketing) profile and turns them into a small set of
# tidy data.tables that can be converted to GTFS fares (v1 or v2).
#
# A single BODS NeTEx fare file describes the fares for ONE line, in ONE
# direction, for ONE fare product (e.g. "Adult Single"). The fares themselves
# are a zone-to-zone "fare triangle":
#
#   * fareZones          - each zone groups a set of scheduled stop points
#   * distanceMatrixElements - each origin-zone/destination-zone pair points
#                          at a price band (PriceGroupRef)
#   * priceGroups        - each price band has a monetary Amount
#
# These three pieces are enough to reconstruct "travelling from zone A to
# zone B on this line costs GBP X for this passenger type".
#
# The functions in this file only READ NeTEx. Conversion to GTFS lives in
# netex_to_gtfs_fares.R, and matching NeTEx files to TransXChange / GTFS
# services lives in netex_fares_match.R.


#' Extract NeTEx fare files from a BODS archive
#'
#' BODS fare archives are zips of zips: a top level archive contains one
#' folder per operator, and each operator folder contains either raw `.xml`
#' NeTEx files or further `.zip` files that themselves contain `.xml` files.
#' This function unpacks an archive (recursively, following nested zips) and
#' returns the paths of every extracted `.xml` file.
#'
#' Nested archives are extracted into short, sequentially-named sub-folders
#' (`z00001`, `z00002`, ...) directly under `exdir` rather than mirroring the
#' original deep folder structure. BODS NeTEx file names are very long (well
#' over 100 characters) and, combined with long operator folder names, easily
#' exceed the Windows 260-character path limit, which silently breaks
#' extraction. Flattening into short sub-folders keeps every path short while
#' still keeping each archive's files apart. For the best chance of success on
#' Windows, keep `exdir` itself short (the default temp folder is short).
#'
#' @param path path to a `.zip` archive, or a folder already containing NeTEx
#'   files / nested zips.
#' @param exdir directory to extract into. Defaults to a new temporary folder.
#'   Keep this short on Windows (see Details).
#' @param pattern optional regular expression; only entries whose name matches
#'   are extracted from the top-level archive, and only matching `.xml` paths
#'   are returned (e.g. `"Beestons"` to restrict to one operator).
#' @return a character vector of paths to extracted `.xml` files.
#' @export
netex_unzip <- function(path, exdir = tempfile("netex_"), pattern = NULL) {
  if (!dir.exists(exdir)) {
    dir.create(exdir, recursive = TRUE)
  }

  # A folder of already-extracted files is handled by treating any nested
  # zips the same way as zips pulled out of an archive.
  if (dir.exists(path)) {
    file.copy(list.files(path, full.names = TRUE), exdir, recursive = TRUE)
  } else {
    if (!is.null(pattern)) {
      # only extract the wanted entries from a large national archive
      nms <- utils::unzip(path, list = TRUE)$Name
      nms <- nms[grepl(pattern, nms)]
      utils::unzip(path, files = nms, exdir = exdir, junkpaths = FALSE)
    } else {
      utils::unzip(path, exdir = exdir, junkpaths = FALSE)
    }
  }

  # Recursively unpack any nested zips. Each is extracted into a short,
  # sequentially-named folder at the root of exdir to avoid MAX_PATH problems
  # (see Details); the source zip is then removed so the loop terminates.
  counter <- 0L
  n_fail <- 0L
  repeat {
    inner_zips <- list.files(exdir, pattern = "\\.zip$", recursive = TRUE,
                             full.names = TRUE, ignore.case = TRUE)
    if (length(inner_zips) == 0) {
      break
    }
    for (z in inner_zips) {
      counter <- counter + 1L
      subdir <- file.path(exdir, sprintf("z%05d", counter))
      dir.create(subdir, showWarnings = FALSE)
      ok <- tryCatch({
        utils::unzip(z, exdir = subdir, junkpaths = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (!ok) n_fail <- n_fail + 1L
      file.remove(z)
    }
  }
  if (n_fail > 0) {
    warning(n_fail, " nested archive(s) could not be extracted (possibly ",
            "corrupt or a path-length issue); try a shorter 'exdir'.")
  }

  # Note: `pattern` is applied when extracting from an archive (above). We do
  # not re-filter the final paths, because nested files are flattened into
  # short folders and no longer carry the operator name in their path.
  list.files(exdir, pattern = "\\.xml$", recursive = TRUE,
             full.names = TRUE, ignore.case = TRUE)
}


#' Read a single NeTEx fare file
#'
#' Parses one BODS NeTEx fare `.xml` file (DfT fxc profile) into a list of
#' tidy data.tables describing the line, its fare zones, the zone-to-zone
#' price matrix and the fare product.
#'
#' @param path path to a NeTEx `.xml` file.
#' @return a list with elements:
#'   \describe{
#'     \item{meta}{one-row data.table of file / operator / line / product
#'       attributes (operator NOC, line public code, direction, currency,
#'       product name & type, trip type, user type, validity dates).}
#'     \item{stops}{data.table of scheduled stop points (`stop_id` is the ATCO
#'       code with the `atco:` prefix removed), with name and locality.}
#'     \item{zones}{long data.table mapping each fare `zone_id` to its member
#'       `stop_id`s (plus zone name).}
#'     \item{prices}{data.table of price bands (`price_group`) and their
#'       `amount`.}
#'     \item{fares}{data.table of the fare triangle: `from_zone`, `to_zone`,
#'       `price_group`, `amount` (one row per origin/destination zone pair).}
#'   }
#' @export
netex_read_fares <- function(path) {
  xml <- xml2::read_xml(path)
  xml2::xml_ns_strip(xml)

  txt <- function(node, xpath) {
    r <- xml2::xml_text(xml2::xml_find_first(node, xpath))
    if (length(r) == 0) NA_character_ else r
  }
  attrx <- function(node, attr) {
    r <- xml2::xml_attr(node, attr)
    if (length(r) == 0) NA_character_ else r
  }
  strip_atco <- function(x) sub("^atco:", "", x)

  # --- line / operator (ServiceFrame) ------------------------------------
  line <- xml2::xml_find_first(xml, ".//ServiceFrame/lines/Line")
  operator <- xml2::xml_find_first(xml, ".//ResourceFrame//Operator")

  line_id <- attrx(line, "id")
  line_name <- txt(line, "./Name")
  # BODS encodes direction ("Inbound"/"Outbound") in the line Name; also
  # available in the file name. Pull it from the Name where present.
  direction <- NA_character_
  if (!is.na(line_name)) {
    if (grepl("Inbound", line_name, ignore.case = TRUE)) direction <- "inbound"
    if (grepl("Outbound", line_name, ignore.case = TRUE)) direction <- "outbound"
  }

  # --- tariff: product / user type / trip type ---------------------------
  tariff <- xml2::xml_find_first(xml, ".//tariffs/Tariff")
  product <- xml2::xml_find_first(xml, ".//fareProducts/PreassignedFareProduct")

  user_type <- txt(xml, ".//FareStructureElement[TypeOfFareStructureElementRef[contains(@ref,'eligibility')]]//UserProfile/UserType")
  user_name <- txt(xml, ".//FareStructureElement[TypeOfFareStructureElementRef[contains(@ref,'eligibility')]]//UserProfile/Name")
  trip_type <- txt(xml, ".//RoundTrip/TripType")

  currency <- txt(xml, ".//FrameDefaults/DefaultCurrency")
  if (is.na(currency)) currency <- "GBP"

  meta <- data.table::data.table(
    file = basename(path),
    operator_noc = sub("^noc:", "", attrx(operator, "id")),
    operator_name = txt(operator, "./Name"),
    line_id = line_id,
    line_public_code = txt(line, "./PublicCode"),
    line_private_code = txt(line, "./PrivateCode"),
    line_name = line_name,
    direction = direction,
    product_id = attrx(product, "id"),
    product_name = txt(product, "./Name"),
    product_type = txt(product, "./ProductType"),
    trip_type = trip_type,
    user_type = user_type,
    user_name = user_name,
    currency = currency,
    valid_from = txt(tariff, ".//ValidBetween/FromDate"),
    valid_to = txt(tariff, ".//ValidBetween/ToDate")
  )

  # --- scheduled stop points ---------------------------------------------
  ssp <- xml2::xml_find_all(xml, ".//ServiceFrame//scheduledStopPoints/ScheduledStopPoint")
  stops <- data.table::data.table(
    stop_id = strip_atco(xml2::xml_attr(ssp, "id")),
    stop_name = xml2::xml_text(xml2::xml_find_first(ssp, "./Name")),
    locality_ref = xml2::xml_attr(
      xml2::xml_find_first(ssp, "./TopographicPlaceView/TopographicPlaceRef"), "ref"),
    locality_name = xml2::xml_text(
      xml2::xml_find_first(ssp, "./TopographicPlaceView/Name"))
  )
  stops <- unique(stops)

  # --- fare zones -> stops (long) ----------------------------------------
  fz <- xml2::xml_find_all(xml, ".//fareZones/FareZone")
  zones_list <- lapply(fz, function(z) {
    members <- xml2::xml_find_all(z, "./members/ScheduledStopPointRef")
    if (length(members) == 0) return(NULL)
    data.table::data.table(
      zone_id = xml2::xml_attr(z, "id"),
      zone_name = xml2::xml_text(xml2::xml_find_first(z, "./Name")),
      stop_id = strip_atco(xml2::xml_attr(members, "ref"))
    )
  })
  zones <- data.table::rbindlist(zones_list, fill = TRUE)
  # Guarantee the expected columns even when no zone had any members.
  if (!all(c("zone_id", "zone_name", "stop_id") %in% names(zones))) {
    zones <- data.table::data.table(zone_id = character(),
                                    zone_name = character(), stop_id = character())
  }

  # --- price bands -------------------------------------------------------
  pg <- xml2::xml_find_all(xml, ".//priceGroups/PriceGroup")
  prices <- data.table::data.table(
    price_group = xml2::xml_attr(pg, "id"),
    amount = as.numeric(xml2::xml_text(
      xml2::xml_find_first(pg, ".//GeographicalIntervalPrice/Amount")))
  )
  prices <- unique(prices[!is.na(prices$price_group), ])

  # --- distance matrix (origin zone -> destination zone -> band) ---------
  dme <- xml2::xml_find_all(xml, ".//distanceMatrixElements/DistanceMatrixElement")
  od <- data.table::data.table(
    dme_id = xml2::xml_attr(dme, "id"),
    from_zone = xml2::xml_attr(
      xml2::xml_find_first(dme, "./StartTariffZoneRef"), "ref"),
    to_zone = xml2::xml_attr(
      xml2::xml_find_first(dme, "./EndTariffZoneRef"), "ref"),
    price_group = xml2::xml_attr(
      xml2::xml_find_first(dme, "./priceGroups/PriceGroupRef"), "ref")
  )
  # Zone-to-same-zone elements sometimes carry no price band; drop them.
  od <- od[!is.na(od$price_group), ]

  fares <- merge(od, prices, by = "price_group", all.x = TRUE)
  data.table::setcolorder(fares, c("from_zone", "to_zone", "price_group", "amount", "dme_id"))

  # Flat fares: some files price a whole line as a single amount with no zonal
  # fare triangle. The price is stored inline in a fare-table cell
  # (TimeIntervalPrice / GeographicalIntervalPrice) rather than in a price band.
  # Detect this when there is no zonal fare and fall back to the inline amount.
  fare_kind <- "zonal"
  if (nrow(fares) == 0) {
    amt <- suppressWarnings(as.numeric(
      xml2::xml_text(xml2::xml_find_all(xml, ".//fareTables//Amount"))))
    amt <- unique(amt[!is.na(amt)])
    if (length(amt) > 0) {
      fare_kind <- "flat"
      fares <- data.table::data.table(
        from_zone = NA_character_, to_zone = NA_character_,
        price_group = NA_character_, amount = amt, dme_id = NA_character_)
    }
  }
  meta$fare_kind <- fare_kind

  # drop any fare rows with no usable amount
  fares <- fares[!is.na(fares$amount), ]

  list(
    meta = meta,
    stops = stops,
    zones = zones,
    prices = prices,
    fares = fares
  )
}


#' Read many NeTEx fare files
#'
#' Convenience wrapper that reads a vector of NeTEx fare files and returns a
#' named list, one entry per file, each being the output of
#' [netex_read_fares()]. Reading is the slow, CPU-bound part of the fares
#' pipeline, so this function can spread the work over several cores.
#'
#' @param paths character vector of NeTEx `.xml` file paths.
#' @param ncores integer, number of cores to use for parallel reading (default
#'   `1`). Values above 1 use a `furrr`/`future` multisession backend. Reading
#'   the national fare archive benefits from a high core count.
#' @param quiet logical, suppress the summary message of how many files failed,
#'   default `FALSE`.
#' @return named list of parsed NeTEx fare objects (names are file basenames).
#'   Files that fail to parse are dropped, and (unless `quiet`) a summary of the
#'   failures is reported via [netex_read_failures()] attached as the attribute
#'   `"failures"`.
#' @export
netex_read_fares_multiple <- function(paths, ncores = 1, quiet = FALSE) {
  read_one <- function(p) {
    tryCatch(
      netex_read_fares(p),
      error = function(e) structure(list(error = conditionMessage(e), file = p),
                                    class = "netex_read_error")
    )
  }

  res <- netex_map(paths, read_one, ncores = ncores)
  names(res) <- basename(paths)

  # separate failures from successes
  is_err <- vapply(res, inherits, logical(1), what = "netex_read_error")
  if (any(is_err)) {
    fails <- data.table::rbindlist(lapply(res[is_err], function(e)
      data.table::data.table(file = basename(e$file), error = e$error)))
    if (!quiet) {
      message(sum(is_err), "/", length(paths),
              " NeTEx files failed to parse. See attr(x, 'failures').")
    }
  } else {
    fails <- data.table::data.table(file = character(0), error = character(0))
  }

  out <- res[!is_err]
  attr(out, "failures") <- fails
  out
}


#' Failures from the last multi-file NeTEx read
#'
#' Extracts the table of files that could not be parsed from an object returned
#' by [netex_read_fares_multiple()].
#'
#' @param netex_list output of [netex_read_fares_multiple()].
#' @return a data.table with columns `file` and `error`.
#' @export
netex_read_failures <- function(netex_list) {
  f <- attr(netex_list, "failures")
  if (is.null(f)) data.table::data.table(file = character(0), error = character(0)) else f
}


#' Map a function over a list, optionally in parallel with a progress bar
#'
#' Internal helper implementing the package's standard furrr/future parallel
#' idiom with a progress bar, falling back to a sequential purrr map when
#' `ncores == 1`.
#'
#' @param x a list or vector to iterate over.
#' @param fn function to apply to each element.
#' @param ncores integer number of cores.
#' @param ... further arguments passed to `fn`.
#' @noRd
netex_map <- function(x, fn, ncores = 1, ...) {
  if (ncores > 1 && length(x) > 1) {
    oldplan <- future::plan(future::multisession, workers = ncores)
    on.exit(future::plan(oldplan), add = TRUE)
    furrr::future_map(x, fn, ..., .progress = TRUE,
                      .options = furrr::furrr_options(seed = TRUE))
  } else {
    purrr::map(x, fn, ..., .progress = TRUE)
  }
}
