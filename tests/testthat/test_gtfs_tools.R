# Tests for the GTFS toolkit functions: validation, compression, clipping
# and date-trimming keeping the full GTFS spec (fares, frequencies, shapes)
# consistent.

# A small but complete feed: two agencies, two trips, shapes, frequencies,
# transfers, GTFS v1 fares and GTFS Fares v2 tables
make_full_gtfs <- function() {
  list(
    agency = data.frame(
      agency_id = c("A1", "A2"), agency_name = c("One", "Two"),
      agency_url = "http://example.com", agency_timezone = "Europe/London",
      stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_name = c("s1", "s2", "s3", "s4"),
      stop_lat = c(51, 51.01, 52, 53),
      stop_lon = c(-1, -1.01, -2, -3),
      zone_id = c("Z1", "Z1", "Z2", "Z3"),
      stringsAsFactors = FALSE),
    routes = data.frame(
      route_id = c("R1", "R2"), agency_id = c("A1", "A2"),
      route_short_name = c("1", "2"), route_long_name = c("one", "two"),
      route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(
      route_id = c("R1", "R2"), service_id = c("SV1", "SV2"),
      trip_id = c("T1", "T2"), shape_id = c("SH1", "SH2"),
      stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T2", "T2"),
      arrival_time = c("10:00:00", "10:10:00", "11:00:00", "11:10:00"),
      departure_time = c("10:01:00", "10:11:00", "11:01:00", "11:11:00"),
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_sequence = c(1L, 2L, 1L, 2L),
      stringsAsFactors = FALSE),
    calendar = data.frame(
      service_id = c("SV1", "SV2"),
      monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L,
      friday = 1L, saturday = 0L, sunday = 0L,
      start_date = as.Date(c("2024-01-01", "2024-01-01")),
      end_date = as.Date(c("2024-12-31", "2024-01-31")),
      stringsAsFactors = FALSE),
    calendar_dates = data.frame(
      service_id = character(), date = as.Date(character()),
      exception_type = integer(), stringsAsFactors = FALSE),
    shapes = data.frame(
      shape_id = c("SH1", "SH1", "SH2", "SH2"),
      shape_pt_lat = c(51, 51.01, 52, 53),
      shape_pt_lon = c(-1, -1.01, -2, -3),
      shape_pt_sequence = c(1L, 2L, 1L, 2L),
      stringsAsFactors = FALSE),
    frequencies = data.frame(
      trip_id = c("T1", "T2"),
      start_time = "07:00:00", end_time = "09:00:00",
      headway_secs = 600L, stringsAsFactors = FALSE),
    transfers = data.frame(
      from_stop_id = "S1", to_stop_id = "S2", transfer_type = 2L,
      stringsAsFactors = FALSE),
    fare_attributes = data.frame(
      fare_id = c("F1", "F2"), price = c(1.5, 2),
      currency_type = "GBP", payment_method = 1L,
      transfers = NA_integer_, stringsAsFactors = FALSE),
    fare_rules = data.frame(
      fare_id = c("F1", "F2"), route_id = NA_character_,
      origin_id = c("Z1", "Z2"), destination_id = c("Z2", "Z3"),
      stringsAsFactors = FALSE),
    areas = data.frame(
      area_id = c("AR1", "AR2", "AR3"),
      area_name = c("a1", "a2", "a3"), stringsAsFactors = FALSE),
    stop_areas = data.frame(
      area_id = c("AR1", "AR2", "AR3"),
      stop_id = c("S1", "S3", "S4"), stringsAsFactors = FALSE),
    networks = data.frame(
      network_id = "net", network_name = "Network",
      stringsAsFactors = FALSE),
    route_networks = data.frame(
      network_id = "net", route_id = c("R1", "R2"),
      stringsAsFactors = FALSE),
    rider_categories = data.frame(
      rider_category_id = c("adult", "child"),
      rider_category_name = c("Adult", "Child"),
      is_default_fare_category = c(1L, 0L), stringsAsFactors = FALSE),
    fare_media = data.frame(
      fare_media_id = "ticket", fare_media_type = 1L,
      stringsAsFactors = FALSE),
    fare_products = data.frame(
      fare_product_id = c("P1", "P2"),
      fare_product_name = c("p1", "p2"),
      rider_category_id = c("adult", "child"),
      fare_media_id = "ticket",
      amount = c(1.5, 0.75), currency = "GBP", stringsAsFactors = FALSE),
    fare_leg_rules = data.frame(
      leg_group_id = c("L1", "L2"), network_id = "net",
      from_area_id = c("AR1", "AR2"), to_area_id = c("AR2", "AR3"),
      fare_product_id = c("P1", "P2"), stringsAsFactors = FALSE)
  )
}

# helper: no table may reference a row that does not exist
expect_consistent <- function(gtfs) {
  expect_true(all(gtfs$stop_times$trip_id %in% gtfs$trips$trip_id))
  expect_true(all(gtfs$stop_times$stop_id %in% gtfs$stops$stop_id))
  expect_true(all(gtfs$trips$route_id %in% gtfs$routes$route_id))
  if (!is.null(gtfs$frequencies)) {
    expect_true(all(gtfs$frequencies$trip_id %in% gtfs$trips$trip_id))
  }
  if (!is.null(gtfs$shapes)) {
    expect_true(all(gtfs$shapes$shape_id %in% gtfs$trips$shape_id))
  }
  if (!is.null(gtfs$transfers)) {
    expect_true(all(gtfs$transfers$from_stop_id %in% gtfs$stops$stop_id))
    expect_true(all(gtfs$transfers$to_stop_id %in% gtfs$stops$stop_id))
  }
  if (!is.null(gtfs$fare_rules)) {
    expect_true(all(gtfs$fare_rules$fare_id %in% gtfs$fare_attributes$fare_id))
    fr_zones <- c(gtfs$fare_rules$origin_id, gtfs$fare_rules$destination_id)
    fr_zones <- fr_zones[!is.na(fr_zones)]
    expect_true(all(fr_zones %in% gtfs$stops$zone_id))
  }
  if (!is.null(gtfs$stop_areas)) {
    expect_true(all(gtfs$stop_areas$stop_id %in% gtfs$stops$stop_id))
    expect_true(all(gtfs$stop_areas$area_id %in% gtfs$areas$area_id))
  }
  if (!is.null(gtfs$route_networks)) {
    expect_true(all(gtfs$route_networks$route_id %in% gtfs$routes$route_id))
  }
  if (!is.null(gtfs$fare_leg_rules)) {
    flr_areas <- c(gtfs$fare_leg_rules$from_area_id,
                   gtfs$fare_leg_rules$to_area_id)
    flr_areas <- flr_areas[!is.na(flr_areas)]
    expect_true(all(flr_areas %in% gtfs$areas$area_id))
    expect_true(all(gtfs$fare_leg_rules$fare_product_id %in%
                      gtfs$fare_products$fare_product_id))
  }
  if (!is.null(gtfs$fare_products) && nrow(gtfs$fare_products) > 0) {
    expect_true(all(gtfs$fare_products$rider_category_id %in%
                      gtfs$rider_categories$rider_category_id))
    expect_true(all(gtfs$fare_products$fare_media_id %in%
                      gtfs$fare_media$fare_media_id))
  }
  invisible(NULL)
}


test_that("gtfs_validate_internal passes a clean full-spec feed", {
  gtfs <- make_full_gtfs()
  res <- suppressMessages(gtfs_validate_internal(gtfs))
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 0)
})


test_that("gtfs_validate_internal finds seeded problems", {
  gtfs <- make_full_gtfs()
  # unknown stop in stop_times
  gtfs$stop_times$stop_id[1] <- "NOPE"
  # duplicate trip_id
  gtfs$trips <- rbind(gtfs$trips, gtfs$trips[1, ])
  # invalid route_type
  gtfs$routes$route_type[1] <- 99L
  # departure before arrival
  gtfs$stop_times$departure_time[3] <- "10:00:00"
  # trip with a service that exists nowhere
  gtfs$trips$service_id[2] <- "MISSING"
  # negative fare
  gtfs$fare_attributes$price[1] <- -1
  # two default rider categories
  gtfs$rider_categories$is_default_fare_category <- 1L

  res <- suppressMessages(gtfs_validate_internal(gtfs))

  found <- function(tb, pattern) {
    any(res$table == tb & grepl(pattern, res$message))
  }
  expect_true(found("stop_times", "stop_id"))
  expect_true(found("trips", "duplicated"))
  expect_true(found("routes", "route_type"))
  expect_true(found("stop_times", "departure_time is before"))
  expect_true(found("trips", "service_id"))
  expect_true(found("fare_attributes", "price"))
  expect_true(found("rider_categories", "default"))
  expect_true(all(c("severity", "table", "message") %in% names(res)))
})


test_that("gtfs_validate_internal handles Period times and IDate dates", {
  gtfs <- make_full_gtfs()
  gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
  gtfs$stop_times$departure_time <- lubridate::hms(gtfs$stop_times$departure_time)
  gtfs$calendar$start_date <- data.table::as.IDate(gtfs$calendar$start_date)
  gtfs$calendar$end_date <- data.table::as.IDate(gtfs$calendar$end_date)

  res <- suppressMessages(gtfs_validate_internal(gtfs))
  expect_equal(nrow(res), 0)
})


test_that("gtfs_clip prunes shapes, frequencies and fare tables", {
  gtfs <- make_full_gtfs()
  # bounds around S1/S2 only; T2 (S3, S4) is dropped entirely
  bounds <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_buffer(sf::st_point(c(-1.005, 51.005)), 0.1), crs = 4326))

  res <- suppressMessages(gtfs_clip(gtfs, bounds))

  expect_equal(res$trips$trip_id, "T1")
  expect_equal(res$frequencies$trip_id, "T1")
  expect_true(all(res$shapes$shape_id == "SH1"))
  # zones Z2/Z3 are gone so both v1 fares (Z1-Z2, Z2-Z3) must go
  expect_equal(nrow(res$fare_rules), 0)
  expect_equal(nrow(res$fare_attributes), 0)
  # areas AR2/AR3 lost their stops, so the leg rules and products go too
  expect_equal(res$areas$area_id, "AR1")
  expect_equal(nrow(res$fare_leg_rules), 0)
  expect_equal(nrow(res$fare_products), 0)
  expect_consistent(res)
})


test_that("gtfs_trim_dates prunes frequencies and shapes with the trips", {
  gtfs <- make_full_gtfs()
  # SV2 ends in January, so trimming to March removes T2
  res <- suppressMessages(gtfs_trim_dates(
    gtfs,
    startdate = as.Date("2024-03-01"),
    enddate = as.Date("2024-03-31")))

  expect_equal(res$trips$trip_id, "T1")
  expect_equal(res$frequencies$trip_id, "T1")
  expect_true(all(res$shapes$shape_id == "SH1"))
})


test_that("gtfs_compress remaps ids in every referencing table", {
  gtfs <- make_full_gtfs()
  res <- gtfs_compress(gtfs)

  # core remapping
  expect_true(all(res$stop_times$trip_id %in% res$trips$trip_id))
  expect_true(all(res$stop_times$stop_id %in% res$stops$stop_id))
  expect_true(all(res$trips$route_id %in% res$routes$route_id))
  # optional tables follow the new ids
  expect_true(all(res$frequencies$trip_id %in% res$trips$trip_id))
  expect_true(all(res$shapes$shape_id %in% res$trips$shape_id))
  expect_true(all(res$transfers$from_stop_id %in% res$stops$stop_id))
  expect_true(all(res$stop_areas$stop_id %in% res$stops$stop_id))
  expect_true(all(res$route_networks$route_id %in% res$routes$route_id))
  # NA route_id in fare_rules (fare applies to all routes) is preserved
  expect_equal(nrow(res$fare_rules), 2)
  expect_true(all(is.na(res$fare_rules$route_id)))
  # nothing lost
  expect_equal(nrow(res$frequencies), 2)
  expect_equal(nrow(res$shapes), 4)
  expect_equal(nrow(res$stop_areas), 3)
})


test_that("gtfs_clean and gtfs_force_valid keep fare tables consistent", {
  gtfs <- make_full_gtfs()
  # break S4: no coordinates, so gtfs_clean removes it and T2 (single stop)
  gtfs$stops$stop_lat[4] <- NA
  gtfs$stops$stop_lon[4] <- NA

  res <- suppressMessages(gtfs_clean(gtfs))
  expect_false("S4" %in% res$stops$stop_id)
  expect_consistent(res)

  res2 <- suppressMessages(gtfs_force_valid(gtfs))
  expect_false("S4" %in% res2$stops$stop_id)
  expect_true(all(res2$stop_areas$stop_id %in% res2$stops$stop_id))
})
