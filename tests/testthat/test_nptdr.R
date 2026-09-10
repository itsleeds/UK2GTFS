
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
