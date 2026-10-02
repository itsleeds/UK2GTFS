# Tests for gtfs_cleaning.R: splitting a feed by trip_id, finding
# implausibly fast trips and stops, and the train_category filtering in
# gtfs_clean(public_only = TRUE).
#
# The speed functions take the difference between consecutive times, and R
# chooses the units of a difftime from the largest difference in the vector.
# The fixture therefore keeps every consecutive gap under a minute, so the
# differences are in seconds and the speeds really are metres per second.

context("Cleaning problems out of a GTFS object")

# Three trips on two routes. T3 teleports 1 degree of latitude (about 111 km)
# in 30 seconds; T1 and T2 cover about 1.1 km in the same time.
speed_gtfs <- function() {
  list(
    agency = data.frame(agency_id = c("A1", "A2"),
                        agency_name = c("One", "Two"),
                        stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3", "S4", "S5"),
      stop_name = paste0("s", 1:5),
      stop_lat = c(53.80, 53.81, 53.90, 53.91, 54.80),
      stop_lon = c(-1.50, -1.50, -1.60, -1.60, -1.50),
      stringsAsFactors = FALSE),
    routes = data.frame(route_id = c("R1", "R2"),
                        agency_id = c("A1", "A2"),
                        route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(route_id = c("R1", "R1", "R2"),
                       service_id = c("SV1", "SV1", "SV2"),
                       trip_id = c("T1", "T3", "T2"),
                       stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T3", "T3", "T2", "T2"),
      arrival_time = c("10:00:00", "10:00:30",
                       "10:01:00", "10:01:30",
                       "10:02:00", "10:02:30"),
      departure_time = c("10:00:00", "10:00:30",
                         "10:01:00", "10:01:30",
                         "10:02:00", "10:02:30"),
      stop_id = c("S1", "S2", "S1", "S5", "S3", "S4"),
      stop_sequence = c(1L, 2L, 1L, 2L, 1L, 2L),
      stringsAsFactors = FALSE),
    calendar = data.frame(service_id = c("SV1", "SV2"),
                          monday = 1L, stringsAsFactors = FALSE),
    calendar_dates = data.frame(service_id = c("SV1", "SV2"),
                                date = as.Date("2024-01-01"),
                                exception_type = 2L, stringsAsFactors = FALSE)
  )
}


test_that("gtfs_split_ids splits every table into matching and other trips", {
  res <- gtfs_split_ids(speed_gtfs(), c("T1", "T3"))

  expect_named(res, c("true", "false"))
  expect_named(res$true, c("agency", "stops", "routes", "trips", "stop_times",
                           "calendar", "calendar_dates"))

  expect_setequal(res$true$trips$trip_id, c("T1", "T3"))
  expect_equal(res$false$trips$trip_id, "T2")

  # each side carries only the rows its trips need
  expect_equal(res$true$routes$route_id, "R1")
  expect_equal(res$false$routes$route_id, "R2")
  expect_equal(res$true$agency$agency_id, "A1")
  expect_equal(res$false$agency$agency_id, "A2")
  expect_setequal(res$true$stops$stop_id, c("S1", "S2", "S5"))
  expect_setequal(res$false$stops$stop_id, c("S3", "S4"))
  expect_equal(res$true$calendar$service_id, "SV1")
  expect_equal(res$false$calendar$service_id, "SV2")
  expect_equal(res$true$calendar_dates$service_id, "SV1")

  # no call is lost or duplicated
  expect_equal(nrow(res$true$stop_times) + nrow(res$false$stop_times),
               nrow(speed_gtfs()$stop_times))
})


test_that("gtfs_split_ids handles ids that match nothing", {
  res <- gtfs_split_ids(speed_gtfs(), "NOSUCHTRIP")

  expect_equal(nrow(res$true$trips), 0)
  expect_equal(nrow(res$true$stop_times), 0)
  expect_equal(nrow(res$true$stops), 0)
  expect_equal(nrow(res$false$trips), 3)
})


test_that("gtfs_fast_trips finds the trip that teleports", {
  gtfs <- speed_gtfs()

  # routes = FALSE looks at every trip
  expect_equal(gtfs_fast_trips(gtfs, routes = FALSE), "T3")

  # about 3700 m/s, so a threshold above it clears the feed
  expect_length(gtfs_fast_trips(gtfs, maxspeed = 5000, routes = FALSE), 0)

  # a threshold below the slow trips' 37 m/s flags everything
  expect_setequal(gtfs_fast_trips(gtfs, maxspeed = 10, routes = FALSE),
                  c("T1", "T2", "T3"))
})


test_that("gtfs_fast_trips with routes = TRUE checks one trip per route", {
  # T1 and T3 share R1 and only the first is kept, so the faster T3 is missed.
  # This is the documented trade off: faster, but may miss some trips.
  gtfs <- speed_gtfs()
  expect_length(gtfs_fast_trips(gtfs), 0)
  # T1 and T2 are the kept trips, and both exceed a 10 m/s threshold
  expect_setequal(gtfs_fast_trips(gtfs, maxspeed = 10), c("T1", "T2"))
})


test_that("gtfs_fast_trips accepts times as lubridate Periods", {
  # gtfs_read() returns stop_times with Period columns
  gtfs <- speed_gtfs()
  gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
  gtfs$stop_times$departure_time <-
    lubridate::hms(gtfs$stop_times$departure_time)

  expect_equal(gtfs_fast_trips(gtfs, routes = FALSE), "T3")
})


test_that("gtfs_fast_trips falls back to departure_time", {
  gtfs <- speed_gtfs()
  gtfs$stop_times$arrival_time <- NA_character_

  expect_equal(gtfs_fast_trips(gtfs, routes = FALSE), "T3")
})


test_that("gtfs_fast_stops returns the stops either side of a fast segment", {
  stops <- gtfs_fast_stops(speed_gtfs())

  expect_s3_class(stops, "sf")
  expect_setequal(stops$stop_id, c("S1", "S5"))
  expect_true(all(c("max_speed", "min_speed", "mean_speed", "max_distance",
                    "min_distance", "mean_distance", "trips") %in%
                    names(stops)))
  expect_equal(sf::st_crs(stops)$epsg, 4326L)

  # S1 is called at by the slow T1 and the fast T3
  s1 <- stops[stops$stop_id == "S1", ]
  expect_equal(s1$trips, 2L)
  expect_gt(s1$max_speed, 83)
  expect_lt(s1$min_speed, 83)

  # a threshold above every segment leaves nothing (sf warns when it takes the
  # bounding box of an empty table, which is not this function's problem)
  empty <- suppressWarnings(gtfs_fast_stops(speed_gtfs(), maxspeed = 5000))
  expect_equal(nrow(empty), 0)
})


test_that("gtfs_fast_stops accepts times as lubridate Periods", {
  gtfs <- speed_gtfs()
  gtfs$stop_times$arrival_time <- lubridate::hms(gtfs$stop_times$arrival_time)
  gtfs$stop_times$departure_time <-
    lubridate::hms(gtfs$stop_times$departure_time)

  expect_setequal(gtfs_fast_stops(gtfs)$stop_id, c("S1", "S5"))
})


# ---- gtfs_clean(public_only = TRUE) with rail train categories -------------

# A rail feed in data.table form, which is what the ATOC conversion produces.
# R2 carries empty coaching stock (train_category "EE"), which is not a public
# service and must not reach the output feed.
rail_gtfs <- function() {
  list(
    agency = data.table::data.table(agency_id = "A1", agency_name = "Train Co"),
    stops = data.table::data.table(
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_name = paste0("s", 1:4),
      stop_lat = c(53.80, 53.81, 53.90, 53.91),
      stop_lon = c(-1.50, -1.51, -1.60, -1.61)),
    routes = data.table::data.table(
      route_id = c("R1", "R2", "R3"), agency_id = "A1",
      route_type = c(2L, 2L, NA_integer_),
      train_category = c("OO", "EE", "OO")),
    trips = data.table::data.table(
      route_id = c("R1", "R2", "R3"), service_id = "SV1",
      trip_id = c("T1", "T2", "T3")),
    stop_times = data.table::data.table(
      trip_id = c("T1", "T1", "T2", "T2", "T3", "T3"),
      arrival_time = "10:00:00", departure_time = "10:00:00",
      stop_id = c("S1", "S2", "S3", "S4", "S1", "S3"),
      stop_sequence = c(1L, 2L, 1L, 2L, 1L, 2L)),
    calendar = data.table::data.table(service_id = "SV1", monday = 1L)
  )
}


test_that("gtfs_clean(public_only) keeps only public train categories", {
  out <- suppressMessages(gtfs_clean(rail_gtfs(), public_only = TRUE))

  # R2 is empty coaching stock, R3 has no route_type: both go
  expect_equal(out$routes$route_id, "R1")
  expect_equal(out$trips$trip_id, "T1")
  expect_equal(unique(out$stop_times$trip_id), "T1")
  # unused stops are pruned before the category filter runs, so S3 and S4 stay
  # in the feed even though nothing calls at them any more
  expect_setequal(out$stops$stop_id, c("S1", "S2", "S3", "S4"))
  # the columns of each table are unchanged by the filtering
  expect_true(all(c("trip_id", "arrival_time", "departure_time", "stop_id",
                    "stop_sequence") %in% names(out$stop_times)))
})


test_that("gtfs_clean keeps non-public train categories unless asked", {
  out <- suppressMessages(gtfs_clean(rail_gtfs()))

  # without public_only nothing is filtered on category, only the usual
  # consistency cleaning
  expect_setequal(out$routes$route_id, c("R1", "R2", "R3"))
  expect_setequal(out$trips$trip_id, c("T1", "T2", "T3"))
})


test_that("gtfs_clean(public_only) works on a feed with no train_category", {
  # TransXChange feeds have no train_category, so only route_type is used
  gtfs <- rail_gtfs()
  gtfs$routes$train_category <- NULL

  out <- suppressMessages(gtfs_clean(gtfs, public_only = TRUE))
  expect_setequal(out$routes$route_id, c("R1", "R2"))
  expect_setequal(out$trips$trip_id, c("T1", "T2"))
  expect_false("T3" %in% out$stop_times$trip_id)
})
