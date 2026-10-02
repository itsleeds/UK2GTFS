# Tests for gtfs_to_sf.R: turning a gtfs object into sf points and lines.
# These are pure geometry functions, so a four stop two trip feed exercises
# every branch without touching the network.

context("Converting GTFS tables to sf")

sf_gtfs <- function() {
  list(
    agency = data.frame(
      agency_id = c("A1", "A2"), agency_name = c("One", "Two"),
      stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_name = c("s1", "s2", "s3", "s4"),
      stop_lat = c(53.80, 53.81, 53.90, 53.91),
      stop_lon = c(-1.50, -1.51, -1.60, -1.61),
      stringsAsFactors = FALSE),
    routes = data.frame(
      route_id = c("R1", "R2"), agency_id = c("A1", "A2"),
      route_short_name = c("1", "2"), route_long_name = c("one", "two"),
      route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(
      route_id = c("R1", "R2"), service_id = "SV1",
      trip_id = c("T1", "T2"), stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T2", "T2"),
      arrival_time = c("10:00:00", "10:10:00", "11:00:00", "11:10:00"),
      departure_time = c("10:00:00", "10:10:00", "11:00:00", "11:10:00"),
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_sequence = c(1L, 2L, 1L, 2L),
      stringsAsFactors = FALSE)
  )
}


test_that("gtfs_stops_sf makes a point layer in WGS84", {
  stops <- gtfs_stops_sf(sf_gtfs())

  expect_s3_class(stops, "sf")
  expect_equal(nrow(stops), 4)
  expect_equal(as.character(unique(sf::st_geometry_type(stops))), "POINT")
  expect_equal(sf::st_crs(stops)$epsg, 4326L)
  # the coordinate columns become the geometry and are not left behind
  expect_false(any(c("stop_lon", "stop_lat") %in% names(stops)))
  expect_equal(sf::st_coordinates(stops)[1, ],
               c(X = -1.50, Y = 53.80))
})


test_that("gtfs_stops_sf converts character coordinates and says so", {
  # gtfs_read() can hand back coordinates as character; converting them
  # silently would hide a feed that needs fixing, so it reports
  g <- sf_gtfs()
  g$stops$stop_lat <- as.character(g$stops$stop_lat)
  g$stops$stop_lon <- as.character(g$stops$stop_lon)

  expect_message(gtfs_stops_sf(g), "Converting stop_lon to numeric")
  expect_message(gtfs_stops_sf(g), "Converting stop_lat to numeric")

  stops <- suppressMessages(gtfs_stops_sf(g))
  expect_equal(nrow(stops), 4)
  expect_equal(unname(sf::st_coordinates(stops)[1, ]), c(-1.50, 53.80))
})


test_that("gtfs_stops_sf drops stops with no coordinates", {
  g <- sf_gtfs()
  g$stops$stop_lon[3] <- NA_real_
  g$stops$stop_lat[4] <- NA_real_

  expect_message(gtfs_stops_sf(g), "Stops with missing lat/lng removed")
  stops <- suppressMessages(gtfs_stops_sf(g))
  expect_equal(stops$stop_id, c("S1", "S2"))
})


test_that("gtfs_trips_sf makes one line per trip", {
  trips <- suppressMessages(gtfs_trips_sf(sf_gtfs()))

  expect_s3_class(trips, "sf")
  expect_equal(nrow(trips), 2)
  expect_setequal(trips$trip_id, c("T1", "T2"))
  expect_equal(as.character(unique(sf::st_geometry_type(trips))), "LINESTRING")
  expect_equal(sf::st_crs(trips)$epsg, 4326L)
  # two stops per trip means two vertices, in stop_sequence order
  coords <- sf::st_coordinates(trips[trips$trip_id == "T1", ])
  expect_equal(nrow(coords), 2)
  expect_equal(unname(coords[, 1]), c(-1.50, -1.51))
})


test_that("gtfs_trips_sf reports stops it cannot place", {
  g <- sf_gtfs()
  # an unused stop with no coordinates: reported and dropped, and because
  # nothing calls at it the remaining trips are unaffected
  g$stops <- rbind(g$stops,
                   data.frame(stop_id = "S5", stop_name = "s5",
                              stop_lat = NA_real_, stop_lon = NA_real_,
                              stringsAsFactors = FALSE))

  expect_message(gtfs_trips_sf(g), "Stops with missing lat/lng removed")
  expect_equal(nrow(suppressMessages(gtfs_trips_sf(g))), 2)
})


test_that("gtfs_trips_sf accepts character coordinates", {
  g <- sf_gtfs()
  g$stops$stop_lat <- as.character(g$stops$stop_lat)
  g$stops$stop_lon <- as.character(g$stops$stop_lon)

  trips <- suppressMessages(gtfs_trips_sf(g))
  expect_equal(nrow(trips), 2)
  expect_false(anyNA(sf::st_coordinates(trips)))
})


test_that("gtfs_routes_sf keeps one trip per route and attaches route details", {
  g <- sf_gtfs()
  # a second trip on R1 that must not produce a second line
  g$trips <- rbind(g$trips,
                   data.frame(route_id = "R1", service_id = "SV1",
                              trip_id = "T3", stringsAsFactors = FALSE))
  g$stop_times <- rbind(
    g$stop_times,
    data.frame(trip_id = "T3",
               arrival_time = c("12:00:00", "12:10:00"),
               departure_time = c("12:00:00", "12:10:00"),
               stop_id = c("S1", "S2"), stop_sequence = c(1L, 2L),
               stringsAsFactors = FALSE))

  routes <- suppressMessages(gtfs_routes_sf(g))

  expect_s3_class(routes, "sf")
  expect_equal(nrow(routes), 2)
  expect_setequal(routes$route_id, c("R1", "R2"))
  expect_setequal(routes$agency_name, c("One", "Two"))
  expect_true(all(c("route_short_name", "route_long_name", "route_type") %in%
                    names(routes)))
  # trip_id is an implementation detail of the line building, not output
  expect_false("trip_id" %in% names(routes))
})


test_that("gtfs_routes_sf ignores calls at stops it has no coordinates for", {
  # S4 is referenced by stop_times but absent from stops, so R2 is drawn from
  # the calls that can be placed rather than being dropped
  g <- sf_gtfs()
  g$stops <- g$stops[g$stops$stop_id != "S4", ]

  routes <- suppressMessages(gtfs_routes_sf(g))
  expect_setequal(routes$route_id, c("R1", "R2"))
  expect_equal(nrow(sf::st_coordinates(routes[routes$route_id == "R1", ])), 2)
  expect_equal(nrow(sf::st_coordinates(routes[routes$route_id == "R2", ])), 1)
})


test_that("gtfs_routes_sf drops stops with missing coordinates first", {
  g <- sf_gtfs()
  g$stops$stop_lat[4] <- NA_real_

  expect_message(gtfs_routes_sf(g), "Stops with missing lat/lng removed")
  routes <- suppressMessages(gtfs_routes_sf(g))
  expect_setequal(routes$route_id, c("R1", "R2"))
})
