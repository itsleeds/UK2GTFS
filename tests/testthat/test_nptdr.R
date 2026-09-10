
context("Get the example nptdr files")
file_path <- file.path(tempdir(),"uk2gtfs_tests")
data_path <- file.path(tempdir(),"uk2gtfs_data")
if(!dir.exists(file_path)){
  dir.create(file_path)
}
if(!dir.exists(data_path)){
  dir.create(data_path)
}

test_that("test nptdr data is there", {
  expect_true(dl_example_file(data_path, "nptdr"))
  expect_true(file.exists(file.path(data_path, "nptdr.zip")))
})

context("Test the main nptdr function")

naptan = get_naptan()


test_that("test nptdr2gtfs singlecore", {
  gtfs <- nptdr2gtfs(path = file.path(data_path,"nptdr.zip"),
                    silent = FALSE,
                    naptan = naptan)

  expect_true(class(gtfs) == "list")

  stops_sf <- gtfs_stops_sf(gtfs)
  routes_sf <- gtfs_routes_sf(gtfs)

  zones <- sf::st_as_sf(data.frame(id = 1:2,
                                   x = c(-2.59330, -2.61088),
                                   y = c(51.46374, 51.44483)
                                   ),
                        coords = c("x","y"), crs = 4326)
  zones <- sf::st_buffer(zones, 500)

  gtfs$calendar$start_date = lubridate::ymd(gtfs$calendar$start_date)
  gtfs$calendar$end_date = lubridate::ymd(gtfs$calendar$end_date)

  gtfs$stop_times$departure_time = lubridate::hms(gtfs$stop_times$departure_time)
  gtfs$stop_times$arrival_time = lubridate::hms(gtfs$stop_times$arrival_time)

  zones_stats = gtfs_trips_per_zone(gtfs, zones,
                                    startdate = lubridate::ymd("2003-01-01"),
                                    enddate = lubridate::ymd("2003-02-01"))

  expect_true(inherits(zones_stats,"data.frame"))

})




unlink(file_path, recursive = TRUE)


# A minimal feed: one tramway, one metro, one bus that happens to call at a
# tram stop, all filed the way NPTDR files them.
nptdr_mode_gtfs <- function() {
  stops <- data.frame(
    stop_id = c("T1", "T2", "T3", "M1", "M2", "B1", "B2"),
    stop_name = c("Bilston Central (Midland Metro Stop)",
                  "Black Lake (Midland Metro Stop)",
                  "Snow Hill (Midland Metro Stop)",
                  "Bank Foot (Tyne and Wear Metro Station)",
                  "Byker (Tyne and Wear Metro Station)",
                  "High Street", "Market Square"),
    stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = c("R_TRAM", "R_METRO", "R_BUS", "R_LUL"),
    agency_id = c("TMM", "NXM", "BUSCO", "LUL"),
    route_short_name = c("MM1", "GRN", "42", "MET"),
    # NPTDR files the tramway and the metro under one type, and in 2004 it
    # files the Underground as a bus
    route_type = c(1L, 1L, 3L, 3L),
    stringsAsFactors = FALSE)
  trips <- data.frame(
    trip_id = c("t_tram", "t_metro", "t_bus", "t_lul"),
    route_id = c("R_TRAM", "R_METRO", "R_BUS", "R_LUL"),
    stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = c(rep("t_tram", 3), rep("t_metro", 2),
                # the bus calls at one tram stop out of four
                "t_bus", "t_bus", "t_bus", "t_bus",
                "t_lul", "t_lul"),
    stop_id = c("T1", "T2", "T3", "M1", "M2",
                "B1", "B2", "T1", "M1",
                "B1", "B2"),
    stringsAsFactors = FALSE)
  list(routes = routes, trips = trips, stops = stops, stop_times = stop_times)
}


test_that("apply_nptdr_modes sorts trams from metros", {
  g <- nptdr_mode_gtfs()
  out <- UK2GTFS:::apply_nptdr_modes(g)
  rt <- setNames(out$routes$route_type, out$routes$route_id)

  expect_equal(unname(rt["R_TRAM"]), 0)    # Midland Metro is a tramway
  expect_equal(unname(rt["R_METRO"]), 1)   # Tyne and Wear Metro is a metro
  expect_equal(unname(rt["R_LUL"]), 1)     # filed as a bus in the 2004 archive

  # A bus that passes a couple of tram stops is still a bus: only half its
  # calls belong to a light rail system, well under the threshold
  expect_equal(unname(rt["R_BUS"]), 3)
})


test_that("apply_nptdr_modes leaves a feed it cannot read alone", {
  g <- nptdr_mode_gtfs()
  g$stops <- NULL
  expect_equal(UK2GTFS:::apply_nptdr_modes(g)$routes$route_type,
               c(1L, 1L, 3L, 3L))
})


test_that("nptdr_mode_overrides names both modes", {
  o <- nptdr_mode_overrides()
  expect_true(all(c("system", "stop_pattern", "route_type") %in% names(o)))
  expect_setequal(unique(o$route_type), c(0, 1))
  expect_true("London Underground" %in% o$system)
})
