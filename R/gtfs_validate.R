#' Validate a GTFS object (in R)
#'
#' Checks a GTFS object against the GTFS specification and reports the
#' problems it finds. It does not change the data (see [gtfs_force_valid()]
#' for that).
#'
#' @param gtfs a gtfs object
#' @return Invisibly returns a data frame of the problems found (columns
#'   `severity`, `table`, `message`), with zero rows if no problems were
#'   found. Called mainly for its printed messages.
#' @details Checks performed include:
#'
#' * presence of the required tables and required columns of every table,
#'   including shapes, frequencies, transfers, pathways, levels, feed_info
#'   and the GTFS v1 (fare_attributes/fare_rules) and Fares v2 (areas,
#'   stop_areas, networks, route_networks, rider_categories, fare_media,
#'   fare_products, fare_leg_rules, fare_transfer_rules) fare tables
#' * non-standard columns and tables (reported as notes, the spec allows them)
#' * duplicated primary keys in every table
#' * referential integrity of every foreign key between tables
#' * missing values in required columns
#' * coordinate ranges in stops and shapes
#' * field values: enums (route_type, location_type, exception_type,
#'   pickup/drop_off types, transfer_type, payment_method etc.), colour
#'   formats, date and time formats, negative prices and headways
#' * time logic: departure before arrival, times that go backwards along a
#'   trip, frequency windows that end before they start
#' * calendar logic: start after end dates, services that can never run
#' * feed logic: trips with fewer than two stop_times, trips without a
#'   calendar, agencies with differing timezones, exactly one default rider
#'   category, unused stops/routes/services (notes)
#'
#' Problems are reported at three severities: `Error` (feed breaks the
#' spec), `Warning` (probably a mistake) and `Note` (worth knowing, not
#' necessarily wrong).
#' @export

gtfs_validate_internal <- function(gtfs) {

  issues <- list()
  note <- function(severity, table, ...) {
    msg <- paste0(...)
    issues[[length(issues) + 1L]] <<- data.frame(
      severity = severity, table = table, message = msg,
      stringsAsFactors = FALSE)
    message(severity, " [", table, "] ", msg)
  }

  has <- function(nm) !is.null(gtfs[[nm]])
  has_rows <- function(nm) has(nm) && nrow(gtfs[[nm]]) > 0
  has_col <- function(nm, col) has(nm) && all(col %in% names(gtfs[[nm]]))

  id_sample <- function(x, n = 10) {
    x <- unique(x)
    out <- paste(utils::head(x, n), collapse = " ")
    if (length(x) > n) {
      out <- paste0(out, " ... and ", length(x) - n, " more")
    }
    out
  }

  # ---- 1. Required tables --------------------------------------------------
  for (tb in c("agency", "stops", "routes", "trips", "stop_times")) {
    if (!has(tb)) {
      note("Error", tb, "required table is missing")
    } else if (nrow(gtfs[[tb]]) < 1) {
      note("Warning", tb, "table has no rows")
    }
  }
  if (!has("calendar") && !has("calendar_dates")) {
    note("Error", "calendar", "either calendar or calendar_dates is required")
  }
  if (has("calendar") && nrow(gtfs$calendar) < 1 && !has_rows("calendar_dates")) {
    note("Warning", "calendar", "calendar has no rows and there are no calendar_dates")
  }

  # ---- 2. Columns ------------------------------------------------------------
  # required and known-optional columns per table; anything else is reported
  # as non-standard (which the spec permits, so only a Note)
  spec <- list(
    agency = list(
      req = c("agency_id", "agency_name", "agency_url", "agency_timezone"),
      op = c("agency_lang", "agency_phone", "agency_fare_url", "agency_email",
             "agency_noc")),
    stops = list(
      req = c("stop_id", "stop_name", "stop_lat", "stop_lon"),
      op = c("stop_code", "tts_stop_name", "stop_desc", "zone_id", "stop_url",
             "location_type", "parent_station", "stop_timezone",
             "wheelchair_boarding", "level_id", "platform_code")),
    routes = list(
      req = c("route_id", "route_type"),
      op = c("agency_id", "route_short_name", "route_long_name", "route_desc",
             "route_url", "route_color", "route_text_color", "route_sort_order",
             "continuous_pickup", "continuous_drop_off", "network_id")),
    trips = list(
      req = c("route_id", "service_id", "trip_id"),
      op = c("trip_headsign", "trip_short_name", "direction_id", "block_id",
             "shape_id", "wheelchair_accessible", "bikes_allowed",
             "cars_allowed")),
    stop_times = list(
      req = c("trip_id", "arrival_time", "departure_time", "stop_id",
              "stop_sequence"),
      op = c("location_group_id", "location_id", "stop_headsign",
             "start_pickup_drop_off_window", "end_pickup_drop_off_window",
             "pickup_type", "drop_off_type", "continuous_pickup",
             "continuous_drop_off", "shape_dist_traveled", "timepoint",
             "pickup_booking_rule_id", "drop_off_booking_rule_id")),
    calendar = list(
      req = c("service_id", "monday", "tuesday", "wednesday", "thursday",
              "friday", "saturday", "sunday", "start_date", "end_date"),
      op = character()),
    calendar_dates = list(
      req = c("service_id", "date", "exception_type"),
      op = character()),
    shapes = list(
      req = c("shape_id", "shape_pt_lat", "shape_pt_lon", "shape_pt_sequence"),
      op = "shape_dist_traveled"),
    frequencies = list(
      req = c("trip_id", "start_time", "end_time", "headway_secs"),
      op = "exact_times"),
    transfers = list(
      req = c("from_stop_id", "to_stop_id", "transfer_type"),
      op = c("from_route_id", "to_route_id", "from_trip_id", "to_trip_id",
             "min_transfer_time")),
    pathways = list(
      req = c("pathway_id", "from_stop_id", "to_stop_id", "pathway_mode",
              "is_bidirectional"),
      op = c("length", "traversal_time", "stair_count", "max_slope",
             "min_width", "signposted_as", "reversed_signposted_as")),
    levels = list(
      req = c("level_id", "level_index"),
      op = "level_name"),
    feed_info = list(
      req = c("feed_publisher_name", "feed_publisher_url", "feed_lang"),
      op = c("default_lang", "feed_start_date", "feed_end_date",
             "feed_version", "feed_contact_email", "feed_contact_url")),
    fare_attributes = list(
      req = c("fare_id", "price", "currency_type", "payment_method",
              "transfers"),
      op = c("agency_id", "transfer_duration")),
    fare_rules = list(
      req = "fare_id",
      op = c("route_id", "origin_id", "destination_id", "contains_id")),
    areas = list(req = "area_id", op = "area_name"),
    stop_areas = list(req = c("area_id", "stop_id"), op = character()),
    networks = list(req = "network_id", op = "network_name"),
    route_networks = list(req = c("network_id", "route_id"), op = character()),
    rider_categories = list(
      req = c("rider_category_id", "rider_category_name",
              "is_default_fare_category"),
      op = "eligibility_url"),
    fare_media = list(
      req = c("fare_media_id", "fare_media_type"),
      op = "fare_media_name"),
    fare_products = list(
      req = c("fare_product_id", "amount", "currency"),
      op = c("fare_product_name", "rider_category_id", "fare_media_id")),
    fare_leg_rules = list(
      req = "fare_product_id",
      op = c("leg_group_id", "network_id", "from_area_id", "to_area_id",
             "from_timeframe_group_id", "to_timeframe_group_id",
             "rule_priority")),
    fare_transfer_rules = list(
      req = "fare_transfer_type",
      op = c("from_leg_group_id", "to_leg_group_id", "transfer_count",
             "duration_limit", "duration_limit_type", "fare_product_id")),
    timeframes = list(
      req = c("timeframe_group_id", "service_id"),
      op = c("start_time", "end_time")),
    attributions = list(
      req = "organization_name",
      op = c("attribution_id", "agency_id", "route_id", "trip_id",
             "is_producer", "is_operator", "is_authority", "attribution_url",
             "attribution_email", "attribution_phone")),
    translations = list(
      req = c("table_name", "field_name", "language", "translation"),
      op = c("record_id", "record_sub_id", "field_value"))
  )

  for (tb in names(spec)) {
    if (!has(tb)) next
    nm <- names(gtfs[[tb]])
    missing_req <- setdiff(spec[[tb]]$req, nm)
    if (length(missing_req) > 0) {
      note("Error", tb, "required column(s) missing: ",
           paste(missing_req, collapse = ", "))
    }
    extra <- setdiff(nm, c(spec[[tb]]$req, spec[[tb]]$op))
    if (length(extra) > 0) {
      note("Note", tb, "non-standard column(s): ",
           paste(extra, collapse = ", "))
    }
  }

  unknown_tables <- setdiff(names(gtfs), names(spec))
  for (tb in unknown_tables) {
    note("Note", tb, "not a standard GTFS table")
  }

  # routes must have a short or a long name
  if (has("routes")) {
    nm <- names(gtfs$routes)
    if (!any(c("route_short_name", "route_long_name") %in% nm)) {
      note("Error", "routes",
           "at least one of route_short_name or route_long_name is required")
    } else if (all(c("route_short_name", "route_long_name") %in% nm) &&
               nrow(gtfs$routes) > 0) {
      blank <- function(x) is.na(x) | x == ""
      both <- blank(gtfs$routes$route_short_name) &
        blank(gtfs$routes$route_long_name)
      if (any(both)) {
        note("Warning", "routes", sum(both),
             " route(s) have neither route_short_name nor route_long_name: ",
             id_sample(gtfs$routes$route_id[both]))
      }
    }
    if (!"agency_id" %in% nm && has("agency") && nrow(gtfs$agency) > 1) {
      note("Error", "routes",
           "agency_id is required when there is more than one agency")
    }
  }

  # ---- 3. Missing values in required columns -------------------------------
  for (tb in names(spec)) {
    if (!has_rows(tb)) next
    for (col in intersect(spec[[tb]]$req, names(gtfs[[tb]]))) {
      # NA times in stop_times are legal (untimed stops), checked separately
      if (tb == "stop_times" && col %in% c("arrival_time", "departure_time")) next
      # required columns whose values may legitimately be empty
      if (tb == "fare_attributes" && col == "transfers") next
      if (tb == "transfers" && col == "transfer_type") next
      x <- gtfs[[tb]][[col]]
      n_na <- sum(is.na(x))
      if (n_na > 0) {
        note("Error", tb, n_na, " missing value(s) in required column ", col)
      }
    }
  }

  # ---- 4. Duplicated primary keys -------------------------------------------
  keys <- list(
    agency = "agency_id",
    stops = "stop_id",
    routes = "route_id",
    trips = "trip_id",
    calendar = "service_id",
    calendar_dates = c("service_id", "date"),
    stop_times = c("trip_id", "stop_sequence"),
    shapes = c("shape_id", "shape_pt_sequence"),
    frequencies = c("trip_id", "start_time"),
    fare_attributes = "fare_id",
    areas = "area_id",
    stop_areas = c("area_id", "stop_id"),
    networks = "network_id",
    route_networks = "route_id",
    rider_categories = "rider_category_id",
    fare_media = "fare_media_id",
    fare_products = c("fare_product_id", "rider_category_id", "fare_media_id"),
    pathways = "pathway_id",
    levels = "level_id"
  )
  for (tb in names(keys)) {
    if (!has_rows(tb)) next
    kc <- intersect(keys[[tb]], names(gtfs[[tb]]))
    if (length(kc) == 0) next
    df <- as.data.frame(lapply(kc, function(cc) gtfs[[tb]][[cc]]),
                        col.names = kc, stringsAsFactors = FALSE)
    dup <- duplicated(df)
    if (any(dup)) {
      note("Error", tb, sum(dup), " duplicated ",
           paste(kc, collapse = "+"), " value(s): ",
           id_sample(do.call(paste, df[dup, , drop = FALSE])))
    }
  }

  # ---- 5. Referential integrity ---------------------------------------------
  check_fk <- function(child, col, parent, parent_col,
                       allow_blank = FALSE, severity = "Error", hint = "") {
    if (!has_rows(child) || !has(parent)) return(invisible(NULL))
    if (!col %in% names(gtfs[[child]])) return(invisible(NULL))
    if (!parent_col %in% names(gtfs[[parent]])) return(invisible(NULL))
    vals <- gtfs[[child]][[col]]
    ok <- vals %in% gtfs[[parent]][[parent_col]]
    if (allow_blank) {
      ok <- ok | is.na(vals) | vals == ""
    }
    if (!all(ok)) {
      unknown <- unique(vals[!ok])
      note(severity, child, length(unknown), " ", col,
           " value(s) not found in ", parent, "$", parent_col, hint, ": ",
           id_sample(unknown))
    }
    invisible(NULL)
  }

  check_fk("routes", "agency_id", "agency", "agency_id")
  check_fk("trips", "route_id", "routes", "route_id")
  check_fk("trips", "shape_id", "shapes", "shape_id", allow_blank = TRUE)
  check_fk("stop_times", "trip_id", "trips", "trip_id")
  check_fk("stop_times", "stop_id", "stops", "stop_id",
           hint = " (TIPLOC data may need refreshing)")
  check_fk("stops", "parent_station", "stops", "stop_id", allow_blank = TRUE)
  check_fk("stops", "level_id", "levels", "level_id", allow_blank = TRUE)
  check_fk("frequencies", "trip_id", "trips", "trip_id")
  check_fk("transfers", "from_stop_id", "stops", "stop_id")
  check_fk("transfers", "to_stop_id", "stops", "stop_id")
  check_fk("pathways", "from_stop_id", "stops", "stop_id")
  check_fk("pathways", "to_stop_id", "stops", "stop_id")
  check_fk("fare_attributes", "agency_id", "agency", "agency_id",
           allow_blank = TRUE)
  check_fk("fare_rules", "fare_id", "fare_attributes", "fare_id")
  check_fk("fare_rules", "route_id", "routes", "route_id", allow_blank = TRUE)
  check_fk("fare_rules", "origin_id", "stops", "zone_id", allow_blank = TRUE)
  check_fk("fare_rules", "destination_id", "stops", "zone_id",
           allow_blank = TRUE)
  check_fk("fare_rules", "contains_id", "stops", "zone_id", allow_blank = TRUE)
  check_fk("stop_areas", "area_id", "areas", "area_id")
  check_fk("stop_areas", "stop_id", "stops", "stop_id")
  check_fk("route_networks", "route_id", "routes", "route_id")
  check_fk("route_networks", "network_id", "networks", "network_id")
  check_fk("fare_leg_rules", "network_id", "networks", "network_id",
           allow_blank = TRUE)
  check_fk("fare_leg_rules", "from_area_id", "areas", "area_id",
           allow_blank = TRUE)
  check_fk("fare_leg_rules", "to_area_id", "areas", "area_id",
           allow_blank = TRUE)
  check_fk("fare_leg_rules", "fare_product_id", "fare_products",
           "fare_product_id")
  check_fk("fare_products", "rider_category_id", "rider_categories",
           "rider_category_id", allow_blank = TRUE)
  check_fk("fare_products", "fare_media_id", "fare_media", "fare_media_id",
           allow_blank = TRUE)
  check_fk("fare_transfer_rules", "from_leg_group_id", "fare_leg_rules",
           "leg_group_id", allow_blank = TRUE)
  check_fk("fare_transfer_rules", "to_leg_group_id", "fare_leg_rules",
           "leg_group_id", allow_blank = TRUE)
  check_fk("fare_transfer_rules", "fare_product_id", "fare_products",
           "fare_product_id", allow_blank = TRUE)
  check_fk("timeframes", "service_id", "calendar", "service_id")

  # a service must exist in calendar or calendar_dates
  if (has_rows("trips") && "service_id" %in% names(gtfs$trips) &&
      (has("calendar") || has("calendar_dates"))) {
    valid_sv <- c(
      if (has("calendar")) gtfs$calendar$service_id,
      if (has("calendar_dates")) gtfs$calendar_dates$service_id)
    bad <- !gtfs$trips$service_id %in% valid_sv
    if (any(bad)) {
      note("Error", "trips", length(unique(gtfs$trips$service_id[bad])),
           " service_id value(s) not found in calendar or calendar_dates: ",
           id_sample(gtfs$trips$service_id[bad]))
    }
  }

  # ---- 6. Field values -------------------------------------------------------
  check_enum <- function(tb, col, valid, allow_na = TRUE,
                         severity = "Error") {
    if (!has_col(tb, col) || !has_rows(tb)) return(invisible(NULL))
    x <- gtfs[[tb]][[col]]
    bad <- !x %in% valid
    if (allow_na) bad <- bad & !is.na(x)
    if (any(bad)) {
      note(severity, tb, sum(bad), " invalid ", col, " value(s): ",
           id_sample(x[bad]))
    }
    invisible(NULL)
  }

  check_latlon <- function(tb, lat_col, lon_col) {
    if (!has_col(tb, c(lat_col, lon_col)) || !has_rows(tb)) {
      return(invisible(NULL))
    }
    lat <- suppressWarnings(as.numeric(gtfs[[tb]][[lat_col]]))
    lon <- suppressWarnings(as.numeric(gtfs[[tb]][[lon_col]]))
    n_missing <- sum(is.na(lat) | is.na(lon))
    if (n_missing > 0) {
      note("Error", tb, n_missing, " row(s) with missing or non-numeric ",
           lat_col, "/", lon_col)
    }
    n_range <- sum(abs(lat) > 90 | abs(lon) > 180, na.rm = TRUE)
    if (n_range > 0) {
      note("Error", tb, n_range, " row(s) with ", lat_col, "/", lon_col,
           " outside valid ranges")
    }
    invisible(NULL)
  }

  check_color <- function(tb, col) {
    if (!has_col(tb, col) || !has_rows(tb)) return(invisible(NULL))
    x <- gtfs[[tb]][[col]]
    bad <- !(is.na(x) | x == "" | grepl("^[0-9A-Fa-f]{6}$", x))
    if (any(bad)) {
      note("Error", tb, sum(bad), " invalid ", col,
           " value(s), must be a 6 digit hex colour without #: ",
           id_sample(x[bad]))
    }
    invisible(NULL)
  }

  check_currency <- function(tb, col) {
    if (!has_col(tb, col) || !has_rows(tb)) return(invisible(NULL))
    x <- gtfs[[tb]][[col]]
    bad <- !grepl("^[A-Za-z]{3}$", x)
    if (any(bad)) {
      note("Error", tb, sum(bad), " invalid ", col,
           " value(s), must be a 3 letter ISO 4217 code: ", id_sample(x[bad]))
    }
    invisible(NULL)
  }

  # dates may be Date/IDate, integer yyyymmdd or character yyyymmdd
  date_int <- function(x) {
    if (inherits(x, "Date")) {
      return(suppressWarnings(as.integer(format(x, "%Y%m%d"))))
    }
    suppressWarnings(as.integer(as.character(x)))
  }
  check_dates <- function(tb, col) {
    if (!has_col(tb, col) || !has_rows(tb)) return(invisible(NULL))
    x <- gtfs[[tb]][[col]]
    if (inherits(x, "Date")) {
      bad <- is.na(x)
    } else {
      bad <- is.na(as.Date(as.character(x), format = "%Y%m%d"))
    }
    if (any(bad)) {
      note("Error", tb, sum(bad), " invalid ", col,
           " value(s), must be a YYYYMMDD date")
    }
    invisible(NULL)
  }

  # times may be lubridate Periods, difftimes, or HH:MM:SS characters
  time_secs <- function(x) {
    if (inherits(x, "Period")) {
      return(suppressWarnings(lubridate::period_to_seconds(x)))
    }
    if (inherits(x, "difftime")) return(as.numeric(x, units = "secs"))
    if (is.numeric(x)) return(as.numeric(x))
    x <- as.character(x)
    ok <- grepl("^\\d{1,3}:[0-5]\\d:[0-5]\\d$", x)
    n <- nchar(x)
    h <- suppressWarnings(as.integer(substr(x, 1, n - 6)))
    m <- suppressWarnings(as.integer(substr(x, n - 4, n - 3)))
    s <- suppressWarnings(as.integer(substr(x, n - 1, n)))
    out <- h * 3600 + m * 60 + s
    out[!ok] <- NA_real_
    out
  }
  check_time_format <- function(tb, col) {
    if (!has_col(tb, col) || !has_rows(tb)) return(invisible(NULL))
    x <- gtfs[[tb]][[col]]
    if (!is.character(x)) return(invisible(NULL))
    bad <- !(is.na(x) | x == "" | grepl("^\\d{1,3}:[0-5]\\d:[0-5]\\d$", x))
    if (any(bad)) {
      note("Error", tb, sum(bad), " invalid ", col,
           " value(s), must be HH:MM:SS: ", id_sample(x[bad]))
    }
    invisible(NULL)
  }

  # agency
  if (has_col("agency", "agency_timezone") && has_rows("agency")) {
    tz <- unique(gtfs$agency$agency_timezone)
    tz <- tz[!is.na(tz) & tz != ""]
    if (length(tz) > 1) {
      note("Error", "agency",
           "all agencies must share the same agency_timezone, found: ",
           paste(tz, collapse = ", "))
    }
  }
  if (has_col("agency", "agency_id") && has_rows("agency")) {
    n_blank <- sum(gtfs$agency$agency_id == "", na.rm = TRUE)
    if (n_blank > 0) {
      note("Warning", "agency", n_blank, " blank agency_id value(s)")
    }
  }

  # stops
  check_latlon("stops", "stop_lat", "stop_lon")
  check_enum("stops", "location_type", 0:4)
  check_enum("stops", "wheelchair_boarding", 0:2)
  if (has_col("stops", c("location_type", "parent_station")) &&
      has_rows("stops")) {
    lt <- gtfs$stops$location_type
    ps <- gtfs$stops$parent_station
    no_parent <- is.na(ps) | ps == ""
    bad <- !is.na(lt) & lt >= 2 & no_parent
    if (any(bad)) {
      note("Error", "stops", sum(bad),
           " entrance/node/boarding-area stop(s) (location_type >= 2)",
           " without a parent_station: ", id_sample(gtfs$stops$stop_id[bad]))
    }
    bad <- !is.na(lt) & lt == 1 & !no_parent
    if (any(bad)) {
      note("Error", "stops", sum(bad),
           " station(s) (location_type = 1) must not have a parent_station: ",
           id_sample(gtfs$stops$stop_id[bad]))
    }
    stations <- gtfs$stops$stop_id[!is.na(lt) & lt == 1]
    bad <- !no_parent & !ps %in% stations
    if (any(bad)) {
      note("Warning", "stops", sum(bad),
           " stop(s) whose parent_station is not a station",
           " (location_type = 1): ", id_sample(gtfs$stops$stop_id[bad]))
    }
  }

  # routes
  if (has_col("routes", "route_type") && has_rows("routes")) {
    rt <- gtfs$routes$route_type
    basic <- c(0:7, 11, 12)
    extended <- !is.na(rt) & rt >= 100 & rt <= 1799
    bad <- !is.na(rt) & !rt %in% basic & !extended
    if (any(bad)) {
      note("Error", "routes", sum(bad), " invalid route_type value(s): ",
           id_sample(rt[bad]))
    }
    if (any(is.na(rt))) {
      note("Error", "routes", sum(is.na(rt)), " missing route_type value(s)")
    }
    if (any(extended)) {
      note("Note", "routes", sum(extended),
           " route(s) use extended route types, not all consumers accept these")
    }
  }
  check_color("routes", "route_color")
  check_color("routes", "route_text_color")
  check_enum("routes", "continuous_pickup", 0:3)
  check_enum("routes", "continuous_drop_off", 0:3)

  # trips
  check_enum("trips", "direction_id", 0:1)
  check_enum("trips", "wheelchair_accessible", 0:2)
  check_enum("trips", "bikes_allowed", 0:2)

  # stop_times
  check_enum("stop_times", "pickup_type", 0:3)
  check_enum("stop_times", "drop_off_type", 0:3)
  check_enum("stop_times", "timepoint", 0:1)
  check_time_format("stop_times", "arrival_time")
  check_time_format("stop_times", "departure_time")
  if (has_col("stop_times", c("trip_id", "stop_sequence")) &&
      has_rows("stop_times")) {
    st_trip <- gtfs$stop_times$trip_id
    st_seq <- suppressWarnings(as.numeric(gtfs$stop_times$stop_sequence))

    if (any(st_seq < 0, na.rm = TRUE)) {
      note("Error", "stop_times", sum(st_seq < 0, na.rm = TRUE),
           " negative stop_sequence value(s)")
    }

    arr <- if ("arrival_time" %in% names(gtfs$stop_times)) {
      time_secs(gtfs$stop_times$arrival_time)
    }
    dep <- if ("departure_time" %in% names(gtfs$stop_times)) {
      time_secs(gtfs$stop_times$departure_time)
    }

    if (!is.null(arr) && !is.null(dep)) {
      bad <- !is.na(arr) & !is.na(dep) & dep < arr
      if (any(bad)) {
        note("Error", "stop_times", sum(bad),
             " row(s) where departure_time is before arrival_time,",
             " affecting trip(s): ", id_sample(st_trip[bad]))
      }

      ord <- order(st_trip, st_seq, method = "radix")
      trip_o <- st_trip[ord]
      arr_o <- arr[ord]
      dep_o <- dep[ord]
      n <- length(trip_o)
      if (n > 1) {
        same_trip <- trip_o[-1] == trip_o[-n]
        prev_dep <- dep_o[-n]
        cur_arr <- arr_o[-1]
        bad <- same_trip & !is.na(prev_dep) & !is.na(cur_arr) &
          cur_arr < prev_dep
        if (any(bad)) {
          note("Error", "stop_times",
               "times go backwards along ",
               length(unique(trip_o[-1][bad])), " trip(s): ",
               id_sample(trip_o[-1][bad]))
        }
      }

      # trips must have at least 2 stop_times and the first/last must be timed
      runs <- rle(trip_o)
      run_end <- cumsum(runs$lengths)
      run_start <- run_end - runs$lengths + 1L
      short <- runs$lengths < 2
      if (any(short)) {
        note("Warning", "stop_times", sum(short),
             " trip(s) with fewer than 2 stop_times: ",
             id_sample(runs$values[short]))
      }
      untimed <- (is.na(arr_o[run_start]) & is.na(dep_o[run_start])) |
        (is.na(arr_o[run_end]) & is.na(dep_o[run_end]))
      if (any(untimed)) {
        note("Error", "stop_times", sum(untimed),
             " trip(s) missing times at their first or last stop: ",
             id_sample(runs$values[untimed]))
      }

      # shape_dist_traveled must not decrease along a trip
      if ("shape_dist_traveled" %in% names(gtfs$stop_times) && n > 1) {
        sdt <- suppressWarnings(
          as.numeric(gtfs$stop_times$shape_dist_traveled))[ord]
        same_trip <- trip_o[-1] == trip_o[-n]
        prev_sdt <- sdt[-n]
        cur_sdt <- sdt[-1]
        bad <- same_trip & !is.na(prev_sdt) & !is.na(cur_sdt) &
          cur_sdt < prev_sdt
        if (any(bad)) {
          note("Warning", "stop_times",
               "shape_dist_traveled decreases along ",
               length(unique(trip_o[-1][bad])), " trip(s): ",
               id_sample(trip_o[-1][bad]))
        }
      }
    }

    # trips with no stop_times at all
    if (has_rows("trips")) {
      no_st <- !gtfs$trips$trip_id %in% st_trip
      if (any(no_st)) {
        note("Warning", "trips", sum(no_st), " trip(s) with no stop_times: ",
             id_sample(gtfs$trips$trip_id[no_st]))
      }
    }
  }

  # calendar
  if (has_rows("calendar")) {
    for (day in c("monday", "tuesday", "wednesday", "thursday", "friday",
                  "saturday", "sunday")) {
      check_enum("calendar", day, 0:1, allow_na = FALSE)
    }
    check_dates("calendar", "start_date")
    check_dates("calendar", "end_date")
    if (has_col("calendar", c("start_date", "end_date"))) {
      sd <- date_int(gtfs$calendar$start_date)
      ed <- date_int(gtfs$calendar$end_date)
      bad <- !is.na(sd) & !is.na(ed) & sd > ed
      if (any(bad)) {
        note("Error", "calendar", sum(bad),
             " service(s) with start_date after end_date: ",
             id_sample(gtfs$calendar$service_id[bad]))
      }
    }
    # services that can never run: no active days and no added dates
    day_cols <- intersect(c("monday", "tuesday", "wednesday", "thursday",
                            "friday", "saturday", "sunday"),
                          names(gtfs$calendar))
    if (length(day_cols) == 7) {
      active <- rowSums(as.data.frame(
        lapply(day_cols, function(cc) as.numeric(gtfs$calendar[[cc]])))) > 0
      added <- if (has_rows("calendar_dates") &&
                   all(c("service_id", "exception_type") %in%
                       names(gtfs$calendar_dates))) {
        gtfs$calendar_dates$service_id[
          gtfs$calendar_dates$exception_type == 1]
      } else {
        character()
      }
      never <- !active & !gtfs$calendar$service_id %in% added
      if (any(never)) {
        note("Warning", "calendar", sum(never),
             " service(s) with no operating days and no added dates,",
             " these never run: ", id_sample(gtfs$calendar$service_id[never]))
      }
    }
  }

  # calendar_dates
  check_enum("calendar_dates", "exception_type", 1:2, allow_na = FALSE)
  check_dates("calendar_dates", "date")

  # frequencies
  if (has_rows("frequencies")) {
    if (has_col("frequencies", "headway_secs")) {
      hw <- suppressWarnings(as.numeric(gtfs$frequencies$headway_secs))
      bad <- is.na(hw) | hw <= 0
      if (any(bad)) {
        note("Error", "frequencies", sum(bad),
             " row(s) with missing or non-positive headway_secs")
      }
    }
    check_time_format("frequencies", "start_time")
    check_time_format("frequencies", "end_time")
    if (has_col("frequencies", c("start_time", "end_time"))) {
      fs <- time_secs(gtfs$frequencies$start_time)
      fe <- time_secs(gtfs$frequencies$end_time)
      bad <- !is.na(fs) & !is.na(fe) & fe <= fs
      if (any(bad)) {
        note("Error", "frequencies", sum(bad),
             " row(s) where end_time is not after start_time")
      }
    }
    check_enum("frequencies", "exact_times", 0:1)
  }

  # transfers (an empty transfer_type means a recommended transfer point)
  check_enum("transfers", "transfer_type", 0:5)
  if (has_col("transfers", "min_transfer_time") && has_rows("transfers")) {
    mt <- suppressWarnings(as.numeric(gtfs$transfers$min_transfer_time))
    bad <- !is.na(mt) & mt < 0
    if (any(bad)) {
      note("Error", "transfers", sum(bad),
           " negative min_transfer_time value(s)")
    }
  }

  # shapes
  check_latlon("shapes", "shape_pt_lat", "shape_pt_lon")
  if (has_col("shapes", c("shape_id", "shape_pt_sequence",
                          "shape_dist_traveled")) && has_rows("shapes")) {
    ord <- order(gtfs$shapes$shape_id, gtfs$shapes$shape_pt_sequence,
                 method = "radix")
    sid <- gtfs$shapes$shape_id[ord]
    sdt <- suppressWarnings(
      as.numeric(gtfs$shapes$shape_dist_traveled))[ord]
    n <- length(sid)
    if (n > 1) {
      same <- sid[-1] == sid[-n]
      bad <- same & !is.na(sdt[-n]) & !is.na(sdt[-1]) & sdt[-1] < sdt[-n]
      if (any(bad)) {
        note("Warning", "shapes", "shape_dist_traveled decreases along ",
             length(unique(sid[-1][bad])), " shape(s): ",
             id_sample(sid[-1][bad]))
      }
    }
  }

  # feed_info
  if (has_col("feed_info", c("feed_start_date", "feed_end_date")) &&
      has_rows("feed_info")) {
    fs <- date_int(gtfs$feed_info$feed_start_date)
    fe <- date_int(gtfs$feed_info$feed_end_date)
    if (any(!is.na(fs) & !is.na(fe) & fs > fe)) {
      note("Error", "feed_info", "feed_start_date is after feed_end_date")
    }
  }

  # fares (GTFS v1)
  if (has_col("fare_attributes", "price") && has_rows("fare_attributes")) {
    pr <- suppressWarnings(as.numeric(gtfs$fare_attributes$price))
    bad <- is.na(pr) | pr < 0
    if (any(bad)) {
      note("Error", "fare_attributes", sum(bad),
           " row(s) with missing or negative price")
    }
  }
  check_currency("fare_attributes", "currency_type")
  check_enum("fare_attributes", "payment_method", 0:1, allow_na = FALSE)
  check_enum("fare_attributes", "transfers", 0:2)

  # fares (GTFS Fares v2)
  if (has_col("fare_products", "amount") && has_rows("fare_products")) {
    am <- suppressWarnings(as.numeric(gtfs$fare_products$amount))
    bad <- is.na(am) | am < 0
    if (any(bad)) {
      note("Error", "fare_products", sum(bad),
           " row(s) with missing or negative amount")
    }
  }
  check_currency("fare_products", "currency")
  check_enum("fare_media", "fare_media_type", c(0:2, 4), allow_na = FALSE)
  if (has_col("rider_categories", "is_default_fare_category") &&
      has_rows("rider_categories")) {
    n_default <- sum(
      gtfs$rider_categories$is_default_fare_category == 1, na.rm = TRUE)
    if (n_default != 1) {
      note("Error", "rider_categories", n_default,
           " default rider categories, exactly one must have",
           " is_default_fare_category = 1")
    }
  }

  # ---- 7. Unused rows (notes) -------------------------------------------------
  if (has_rows("routes") && has_rows("trips") &&
      has_col("routes", "route_id") && has_col("trips", "route_id")) {
    unused <- !gtfs$routes$route_id %in% gtfs$trips$route_id
    if (any(unused)) {
      note("Note", "routes", sum(unused), " route(s) with no trips: ",
           id_sample(gtfs$routes$route_id[unused]))
    }
  }
  if (has_rows("stops") && has_rows("stop_times") &&
      has_col("stops", "stop_id") && has_col("stop_times", "stop_id")) {
    used <- gtfs$stops$stop_id %in% gtfs$stop_times$stop_id
    # stations and parents of used stops are not directly referenced
    if ("parent_station" %in% names(gtfs$stops)) {
      used <- used | gtfs$stops$stop_id %in% gtfs$stops$parent_station
    }
    if ("location_type" %in% names(gtfs$stops)) {
      used <- used | (!is.na(gtfs$stops$location_type) &
                        gtfs$stops$location_type != 0)
    }
    if (any(!used)) {
      note("Note", "stops", sum(!used),
           " stop(s) not used by any stop_times: ",
           id_sample(gtfs$stops$stop_id[!used]))
    }
  }
  if (has_rows("calendar") && has_rows("trips") &&
      has_col("calendar", "service_id") && has_col("trips", "service_id")) {
    unused <- !gtfs$calendar$service_id %in% gtfs$trips$service_id
    if (any(unused)) {
      note("Note", "calendar", sum(unused),
           " service(s) not used by any trips: ",
           id_sample(gtfs$calendar$service_id[unused]))
    }
  }

  # ---- Summary ---------------------------------------------------------------
  out <- if (length(issues) > 0) {
    do.call(rbind, issues)
  } else {
    data.frame(severity = character(), table = character(),
               message = character(), stringsAsFactors = FALSE)
  }
  if (nrow(out) == 0) {
    message("No problems found")
  } else {
    message("Validation found ", sum(out$severity == "Error"), " errors, ",
            sum(out$severity == "Warning"), " warnings and ",
            sum(out$severity == "Note"), " notes")
  }
  invisible(out)
}


#' Force a GTFS to be valid by removing problems
#' @param gtfs gtfs object
#' @details
#' Actions performed
#' 1. Remove stops with missing location
#' 2. Remove routes that don't exist in agency
#' 3. Remove trips that don't exist in routes
#' 4. Remove stop_times(calls) that don't exist in trips
#' 5. Remove stop_times(calls) that don't exist in stops
#' 6. Remove Calendar that have service_id that doesn't exist in trips
#' 7. Remove Calendar_dates that have service_id that doesn't exist in trips
#' 8. Remove rows in the optional tables (shapes, frequencies, transfers,
#'    pathways and the GTFS v1/Fares v2 fare tables) that reference stops,
#'    routes or trips that don't exist
#'
#' @return a gtfs object
#' @export
gtfs_force_valid <- function(gtfs) {
  message("This function does not fix problems it just removes them")

  # 1. Stops with missing lat/lon
  gtfs$stops <- gtfs$stops[!is.na(gtfs$stops$stop_lon) & !is.na(gtfs$stops$stop_lat),]

  # 2. Routes that have agency_id that doesn't exist in agency
  gtfs$routes <- gtfs$routes[gtfs$routes$agency_id %in%  gtfs$agency$agency_id,]

  # 3. Trips that have route_id that doesn't exist in route
  gtfs$trips <- gtfs$trips[gtfs$trips$route_id %in%  gtfs$routes$route_id,]

  # 4. Stop Times that have trip_id that doesn't exist in trips
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$trip_id %in% gtfs$trips$trip_id,]

  # 5. Stop Times that have stops_id that doesn't exist in stops
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_id %in%  gtfs$stops$stop_id,]

  # 6. Calendar that have service_id that doesn't exist in trip
  gtfs$calendar <- gtfs$calendar[gtfs$calendar$service_id %in%  gtfs$trips$service_id,]

  # 7. Calendar_dates that have service_id that doesn't exist in trip
  gtfs$calendar_dates <- gtfs$calendar_dates[gtfs$calendar_dates$service_id %in%  gtfs$trips$service_id,]

  # 8. Optional tables (shapes, frequencies, transfers, pathways, fare tables)
  # that reference stops, routes or trips that don't exist. Dangling
  # references make the feed invalid and may be rejected by routing engines.
  gtfs <- gtfs_prune_orphans(gtfs)

  return(gtfs)
}
