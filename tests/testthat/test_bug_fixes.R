context("Regression tests for audited bug fixes")

# These tests cover bugs found during the pre-CRAN code audit. Each test names
# the function it guards.


test_that("importMCA parses AA association dates as yymmdd (RSPS5046 5.5.8)", {
  mca_file <- file.path("tmp", "example.mca")
  skip_if_not(file.exists(mca_file), "example.mca not available")

  mca <- importMCA(mca_file, silent = TRUE, full_import = TRUE)
  aa <- mca$AA

  expect_true(nrow(aa) > 0)
  expect_s3_class(aa$`Assoc Start date`, "Date")
  expect_false(anyNA(aa$`Assoc Start date`))
  expect_false(anyNA(aa$`Assoc End date`))
  # The example file's first AA record runs 2019-12-15 to 2020-03-29
  expect_equal(aa$`Assoc Start date`[1], as.Date("2019-12-15"))
  expect_equal(aa$`Assoc End date`[1], as.Date("2020-03-29"))
  # start dates must not be after end dates when parsed with the right format
  expect_true(all(aa$`Assoc Start date` <= aa$`Assoc End date`))
})


test_that("gtfs_merge merges data.table inputs into single tables", {
  mk_gtfs <- function(pref) {
    list(
      agency = data.table::data.table(
        agency_id = paste0("A", pref), agency_name = paste0("Agency", pref),
        agency_url = "http://example.com", agency_timezone = "Europe/London",
        agency_lang = "en"),
      stops = data.table::data.table(
        stop_id = paste0("S", pref, 1:2), stop_name = c("a", "b"),
        stop_lat = c(51, 52), stop_lon = c(-1, -2)),
      routes = data.table::data.table(
        route_id = "R1", agency_id = paste0("A", pref), route_short_name = "1",
        route_long_name = "one", route_type = 3L),
      trips = data.table::data.table(
        route_id = "R1", service_id = "SV1", trip_id = "T1"),
      stop_times = data.table::data.table(
        trip_id = "T1", arrival_time = "10:00:00", departure_time = "10:00:00",
        stop_id = paste0("S", pref, 1:2), stop_sequence = 1:2),
      calendar = data.table::data.table(
        service_id = "SV1", monday = 1L, tuesday = 1L, wednesday = 1L,
        thursday = 1L, friday = 1L, saturday = 0L, sunday = 0L,
        start_date = "20230101", end_date = "20231231"),
      calendar_dates = data.table::data.table(
        service_id = "SV1", date = "20230704", exception_type = 2L)
    )
  }

  res <- gtfs_merge(list(mk_gtfs("x"), mk_gtfs("y")), force = FALSE, quiet = TRUE)

  # every output table must be a single data frame, not a list of tables
  for (tab in c("agency", "stops", "routes", "trips", "stop_times", "calendar")) {
    expect_true(is.data.frame(res[[tab]]), info = tab)
  }
  expect_equal(nrow(res$agency), 2)
  expect_equal(nrow(res$stops), 4)
  # the duplicated trip_ids must have been de-duplicated
  expect_false(any(duplicated(res$trips$trip_id)))
  expect_equal(nrow(res$stop_times), 4)
  # stop_times must reference the new trip ids
  expect_true(all(res$stop_times$trip_id %in% res$trips$trip_id))
})


test_that("gtfs_clean removes trips with fewer than two stops", {
  gtfs <- list(
    agency = data.frame(agency_id = "A1", agency_name = "Agency",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("S1", "S2", "S3"),
                       stop_lon = c(-1, -2, -3), stop_lat = c(51, 52, 53),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = c("R1", "R2"), agency_id = "A1",
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = c("R1", "R2"), service_id = "SV1",
                       trip_id = c("T1", "T2"), stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T2"),
      stop_id = c("S1", "S2", "S3"),
      stop_sequence = c(1L, 2L, 1L),
      stringsAsFactors = FALSE)
  )

  res <- gtfs_clean(gtfs)

  # T2 only has one stop so must be removed, T1 must be kept
  expect_equal(res$trips$trip_id, "T1")
  expect_true(all(res$stop_times$trip_id == "T1"))
})


test_that("gtfs_clip handles stops with missing or character coordinates", {
  gtfs <- list(
    agency = data.frame(agency_id = "A1", stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("S1", "S2", "S3"),
                       stop_lon = c("-2.59330", "-2.61088", NA),
                       stop_lat = c("51.46374", "51.44483", NA),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R1", agency_id = "A1",
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = "R1", service_id = "SV1", trip_id = "T1",
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = "T1", stop_id = c("S1", "S2", "S3"),
                            stop_sequence = 1:3, stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "SV1", stringsAsFactors = FALSE),
    calendar_dates = data.frame(service_id = character(),
                                stringsAsFactors = FALSE)
  )

  bounds <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_buffer(sf::st_point(c(-2.6, 51.455)), 0.1), crs = 4326))

  expect_silent(res <- gtfs_clip(gtfs, bounds))
  expect_true(all(res$stops$stop_id %in% c("S1", "S2")))
})


test_that("NPTDR-style HHMM times are converted to valid GTFS times", {
  # afterMidnight() expects HHMMSS; nptdr_schedule2routes() pads HHMM times
  stop_times <- data.frame(
    trip_id = c(1, 1, 1),
    arrival_time = c("0930", "0945", "2330"),
    departure_time = c("0930", "0946", "2331"),
    stop_id = c("S1", "S2", "S3"),
    stop_sequence = 1:3,
    pickup_type = 0,
    drop_off_type = 0,
    stringsAsFactors = FALSE
  )
  stop_times$arrival_time <- ifelse(nchar(stop_times$arrival_time) == 4,
                                    paste0(stop_times$arrival_time, "00"),
                                    stop_times$arrival_time)
  stop_times$departure_time <- ifelse(nchar(stop_times$departure_time) == 4,
                                      paste0(stop_times$departure_time, "00"),
                                      stop_times$departure_time)
  res <- afterMidnight(stop_times)

  expect_equal(res$arrival_time, c("09:30:00", "09:45:00", "23:30:00"))
  expect_equal(res$departure_time, c("09:30:00", "09:46:00", "23:31:00"))
})


test_that("afterMidnight applies 24h+ times for journeys crossing midnight", {
  stop_times <- data.frame(
    trip_id = c(1, 1, 1),
    arrival_time = c("233000", "235500", "001500"),
    departure_time = c("233000", "235600", "001600"),
    stop_id = c("S1", "S2", "S3"),
    stop_sequence = 1:3,
    pickup_type = 0,
    drop_off_type = 0,
    stringsAsFactors = FALSE
  )
  res <- afterMidnight(stop_times)
  expect_equal(res$arrival_time, c("23:30:00", "23:55:00", "24:15:00"))
  expect_equal(res$departure_time, c("23:30:00", "23:56:00", "24:16:00"))
})


test_that("clean_days handles standard TransXchange day patterns", {
  expect_equal(clean_days("Monday Tuesday"), c(1, 1, 0, 0, 0, 0, 0))
  expect_equal(clean_days("NotSaturday NotSunday"), c(1, 1, 1, 1, 1, 0, 0))
  expect_equal(clean_days("MondayToFriday"), c(1, 1, 1, 1, 1, 0, 0))
  expect_equal(clean_days("Weekend"), c(0, 0, 0, 0, 0, 1, 1))
  expect_equal(clean_days("MondayToSunday"), c(1, 1, 1, 1, 1, 1, 1))
  expect_error(clean_days("Fishday"))
})


test_that("classify_exclusions classifies date overlaps correctly", {
  s <- as.Date("2023-01-10")
  e <- as.Date("2023-01-20")
  expect_equal(classify_exclusions(as.Date("2023-01-01"), as.Date("2023-01-05"), s, e), "no overlap")
  expect_equal(classify_exclusions(as.Date("2023-01-01"), as.Date("2023-01-31"), s, e), "total")
  expect_equal(classify_exclusions(as.Date("2023-01-01"), as.Date("2023-01-12"), s, e), "start")
  expect_equal(classify_exclusions(as.Date("2023-01-15"), as.Date("2023-01-31"), s, e), "end")
  expect_equal(classify_exclusions(as.Date("2023-01-12"), as.Date("2023-01-15"), s, e), "middle")
})


test_that("clean_times parses ISO 8601 durations", {
  expect_equal(unname(clean_times("PT5M")), 300)
  expect_equal(unname(clean_times("PT1H2M3S")), 3723)
  expect_equal(unname(clean_times("PT30S")), 30)
  expect_equal(unname(clean_times(NA)), 0)
})
