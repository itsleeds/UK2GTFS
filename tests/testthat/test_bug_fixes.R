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


test_that("station2transfers drops unmatched CRS codes and de-duplicates", {
  # from/to are CRS codes; LDN has no matching station so those transfers
  # would otherwise gain NA from_stop_id/to_stop_id (invalid transfers.txt)
  flf <- data.frame(from = c("KGX", "LDN", "EUS"),
                    to   = c("EUS", "KGX", "KGX"),
                    time = c(5, 7, 6),
                    stringsAsFactors = FALSE)
  station <- data.frame(`TIPLOC Code` = c("KNGX", "EUSTON"),
                        `CRS Code` = c("KGX", "EUS"),
                        `Minimum Change Time` = c("10", "12"),
                        check.names = FALSE, stringsAsFactors = FALSE)

  tr <- station2transfers(station, flf)

  # required id fields must never be NA
  expect_false(anyNA(tr$from_stop_id))
  expect_false(anyNA(tr$to_stop_id))
  # only stops that exist in the station file survive
  expect_true(all(tr$from_stop_id %in% c("KNGX", "EUSTON")))
  expect_true(all(tr$to_stop_id %in% c("KNGX", "EUSTON")))
  # no duplicate (from, to) pairs
  expect_false(any(duplicated(tr[, c("from_stop_id", "to_stop_id")])))
  # integer time fields per the GTFS spec
  expect_true(is.integer(tr$min_transfer_time))
  expect_true(is.integer(tr$transfer_type))
})


test_that("gtfs_clean and gtfs_force_valid prune dangling transfers", {
  gtfs <- list(
    agency = data.frame(agency_id = "A1", agency_name = "Agency",
                        agency_url = "http://x", agency_timezone = "Europe/London",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("A", "B", "C"), stop_name = c("A", "B", "C"),
                       stop_lon = c(-1, -2, -3), stop_lat = c(51, 52, 53),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R1", agency_id = "A1", route_short_name = "1",
                        route_long_name = "one", route_type = 3L,
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = "R1", service_id = "SV1", trip_id = "T1",
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = "T1", arrival_time = c("10:00:00", "10:05:00"),
                            departure_time = c("10:00:00", "10:05:00"),
                            stop_id = c("A", "B"), stop_sequence = 1:2,
                            stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "SV1", monday = 1L, tuesday = 1L,
                          wednesday = 1L, thursday = 1L, friday = 1L,
                          saturday = 0L, sunday = 0L, start_date = "20200101",
                          end_date = "20201231", stringsAsFactors = FALSE),
    calendar_dates = data.frame(service_id = character(), date = character(),
                                exception_type = integer()),
    # "Z" does not exist in stops, so these two transfers are dangling
    transfers = data.frame(from_stop_id = c("A", "A", "Z"),
                           to_stop_id = c("B", "Z", "B"),
                           transfer_type = 2L, min_transfer_time = 120L,
                           stringsAsFactors = FALSE)
  )

  fv <- gtfs_force_valid(gtfs)
  expect_equal(nrow(fv$transfers), 1)
  expect_true(all(fv$transfers$from_stop_id %in% fv$stops$stop_id))
  expect_true(all(fv$transfers$to_stop_id %in% fv$stops$stop_id))

  cl <- gtfs_clean(gtfs)
  expect_equal(nrow(cl$transfers), 1)
  expect_true(all(cl$transfers$from_stop_id %in% cl$stops$stop_id))
  expect_true(all(cl$transfers$to_stop_id %in% cl$stops$stop_id))
})


test_that("unzip_recursive extracts nested folders and zip files", {
  skip_if_not(nchar(Sys.which("zip")) > 0, "system zip tool not available")

  # zip files relative to `dir` without permanently changing the working dir
  zip_in <- function(dir, zipfile, files, flags) {
    old <- setwd(dir)
    on.exit(setwd(old))
    utils::zip(zipfile, files, flags = flags)
  }

  # Build a BODS-style archive: a top-level zip of per-operator folders that
  # contain a mix of loose xml files and further zip files (nested >1 level).
  root <- file.path(tempdir(), "uzr_build")
  unlink(root, recursive = TRUE)
  dir.create(file.path(root, "OperatorA"), recursive = TRUE)
  dir.create(file.path(root, "OperatorB"), recursive = TRUE)

  # loose xml directly in an operator folder
  writeLines("<a/>", file.path(root, "OperatorA", "loose1.xml"))

  # a nested zip containing an xml
  inner <- file.path(tempdir(), "uzr_inner")
  unlink(inner, recursive = TRUE); dir.create(inner)
  writeLines("<b/>", file.path(inner, "inner1.xml"))
  zip_in(inner, "innerA.zip", "inner1.xml", flags = "-q")
  file.copy(file.path(inner, "innerA.zip"),
            file.path(root, "OperatorA", "innerA.zip"))

  # a doubly-nested zip (a zip inside a zip) in the other operator folder
  inner2 <- file.path(tempdir(), "uzr_inner2")
  unlink(inner2, recursive = TRUE); dir.create(inner2)
  writeLines("<c/>", file.path(inner2, "inner2.xml"))
  zip_in(inner2, "level2.zip", "inner2.xml", flags = "-q")
  file.remove(file.path(inner2, "inner2.xml"))
  zip_in(inner2, "level1.zip", "level2.zip", flags = "-q")
  file.copy(file.path(inner2, "level1.zip"),
            file.path(root, "OperatorB", "level1.zip"))

  top_zip <- file.path(tempdir(), "uzr_top.zip")
  unlink(top_zip)
  zip_in(root, top_zip, c("OperatorA", "OperatorB"), flags = "-qr")

  exdir <- file.path(tempdir(), "uzr_out")
  unlink(exdir, recursive = TRUE); dir.create(exdir)
  unzip_recursive(top_zip, exdir = exdir, silent = TRUE)

  xml <- list.files(exdir, pattern = "\\.xml$", full.names = TRUE,
                    recursive = TRUE, ignore.case = TRUE)
  zips_left <- list.files(exdir, pattern = "\\.zip$", full.names = TRUE,
                          recursive = TRUE, ignore.case = TRUE)

  # all three xml files (loose, singly-nested, doubly-nested) must be found
  expect_equal(length(xml), 3)
  expect_setequal(basename(xml), c("loose1.xml", "inner1.xml", "inner2.xml"))
  # no zip files should remain unextracted
  expect_equal(length(zips_left), 0)
})


test_that("gtfs_merge keeps every calendar_dates exception when condensing", {
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
      # three distinct exception dates; de-duplicating on service_id alone
      # used to discard all but the first
      calendar_dates = data.table::data.table(
        service_id = "SV1",
        date = c("20230704", "20230825", "20231225"),
        exception_type = c(1L, 2L, 2L))
    )
  }

  res <- gtfs_merge(list(mk_gtfs("x"), mk_gtfs("y")), force = FALSE, quiet = TRUE)

  # the two identical services condense to one, which must keep all three
  # exception dates exactly once each
  expect_equal(nrow(res$calendar), 1)
  expect_equal(nrow(res$calendar_dates), 3)
  expect_setequal(as.character(res$calendar_dates$date),
                  c("20230704", "20230825", "20231225"))
  expect_false(any(duplicated(
    res$calendar_dates[, c("service_id", "date", "exception_type")])))
})


test_that("gtfs_trips_per_zone applies exceptions with GTFS semantics", {
  # Mon-Fri service over a 28-day Monday-aligned window with:
  #  - a cancellation on a Saturday (calendar does not operate: must be a no-op,
  #    previously produced runs_Sat = -1)
  #  - a cancellation on a Monday (real: 4 Mondays become 3)
  #  - an extra on a Sunday (real: 0 Sundays become 1)
  gtfs <- list(
    agency = data.frame(agency_id = "A1", agency_name = "Agency",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = "S1", stop_name = "a",
                       stop_lon = -1.5, stop_lat = 53.8,
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R1", agency_id = "A1",
                        route_short_name = "1", route_type = 3L,
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = "R1", service_id = "SV1", trip_id = "T1",
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = "T1",
                            arrival_time = lubridate::hms("12:00:00"),
                            departure_time = lubridate::hms("12:00:00"),
                            stop_id = "S1", stop_sequence = 1L,
                            stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "SV1", monday = 1L, tuesday = 1L,
                          wednesday = 1L, thursday = 1L, friday = 1L,
                          saturday = 0L, sunday = 0L,
                          start_date = as.Date("2023-10-02"),
                          end_date = as.Date("2023-10-29"),
                          stringsAsFactors = FALSE),
    calendar_dates = data.frame(
      service_id = "SV1",
      date = as.Date(c("2023-10-07", "2023-10-09", "2023-10-08")),
      exception_type = c(2L, 2L, 1L),
      stringsAsFactors = FALSE)
  )

  zone <- sf::st_sf(zone_id = "Z1",
                    geometry = sf::st_sfc(sf::st_buffer(
                      sf::st_point(c(-1.5, 53.8)), 0.01), crs = 4326))

  res <- suppressWarnings(suppressMessages(
    gtfs_trips_per_zone(gtfs, zone,
                        startdate = lubridate::ymd("2023-10-02"),
                        enddate = lubridate::ymd("2023-10-29"))
  ))
  res <- as.data.frame(res)

  expect_equal(res$runs_Mon_Midday, 3)  # 4 Mondays - 1 cancellation
  expect_equal(res$runs_Tue_Midday, 4)
  expect_equal(res$runs_Sat_Midday, 0)  # no-op cancellation, was -1
  expect_equal(res$runs_Sun_Midday, 1)  # genuine extra
  # nothing anywhere may be negative
  expect_true(all(as.matrix(res[grep("^runs_", names(res))]) >= 0))
})


test_that("gtfs_compress remaps transfer stop_ids to match stops", {
  gtfs <- list(
    agency = data.frame(agency_id = "A1", agency_name = "Agency",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("A", "B", "C"), stop_name = c("A", "B", "C"),
                       stop_lon = c(-1, -2, -3), stop_lat = c(51, 52, 53),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R1", agency_id = "A1",
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = "R1", service_id = "SV1", trip_id = "T1",
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = "T1", stop_id = c("A", "B"),
                            stop_sequence = 1:2, stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "SV1", stringsAsFactors = FALSE),
    calendar_dates = data.frame(service_id = character(), stringsAsFactors = FALSE),
    transfers = data.frame(from_stop_id = "A", to_stop_id = "B",
                           transfer_type = 2L, min_transfer_time = 120L,
                           stringsAsFactors = FALSE)
  )

  res <- gtfs_compress(gtfs)

  # transfer endpoints must still resolve to real stops after id compression
  expect_true(all(res$transfers$from_stop_id %in% res$stops$stop_id))
  expect_true(all(res$transfers$to_stop_id %in% res$stops$stop_id))
  expect_true(is.integer(res$transfers$from_stop_id))
})


# Mon-Fri service over a 28-day Monday-aligned window (20 weekdays).
# T1 is a conventional trip at 12:00; T2 is frequency-based with two windows:
#   07:00-09:00 every 30 min -> 4 departures/day (Morning Peak)
#   11:00-13:00 every 60 min -> 2 departures/day (Midday)
mk_freq_gtfs <- function() {
  list(
    agency = data.frame(agency_id = "A1", agency_name = "Agency",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("S1", "S2"), stop_name = c("a", "b"),
                       stop_lon = c(-1.5, -1.5), stop_lat = c(53.8, 53.8),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R1", agency_id = "A1",
                        route_short_name = "1", route_type = 3L,
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = "R1", service_id = "SV1",
                       trip_id = c("T1", "T2"), stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = c("T1", "T2"),
                            arrival_time = lubridate::hms(c("12:00:00", "07:00:00")),
                            departure_time = lubridate::hms(c("12:00:00", "07:00:00")),
                            stop_id = c("S1", "S2"), stop_sequence = 1L,
                            stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "SV1", monday = 1L, tuesday = 1L,
                          wednesday = 1L, thursday = 1L, friday = 1L,
                          saturday = 0L, sunday = 0L,
                          start_date = as.Date("2023-10-02"),
                          end_date = as.Date("2023-10-29"),
                          stringsAsFactors = FALSE),
    calendar_dates = data.frame(service_id = character(),
                                date = as.Date(character()),
                                exception_type = integer(),
                                stringsAsFactors = FALSE),
    frequencies = data.frame(trip_id = "T2",
                             start_time = c("07:00:00", "11:00:00"),
                             end_time = c("09:00:00", "13:00:00"),
                             headway_secs = c(1800L, 3600L),
                             stringsAsFactors = FALSE)
  )
}


test_that("gtfs_stop_frequency counts frequency-based departures", {
  stops <- suppressMessages(gtfs_stop_frequency(
    mk_freq_gtfs(),
    startdate = lubridate::ymd("2023-10-02"),
    enddate = lubridate::ymd("2023-10-29")))

  # conventional trip: once per weekday
  expect_equal(stops$stops_total[stops$stop_id == "S1"], 20)
  expect_equal(stops$stops_per_week[stops$stop_id == "S1"], 5)
  # frequency-based trip: 4 + 2 departures per weekday
  expect_equal(stops$stops_total[stops$stop_id == "S2"], 120)
  expect_equal(stops$stops_per_week[stops$stop_id == "S2"], 30)
})


test_that("gtfs_trim_dates keeps frequencies consistent with trips", {
  gtfs <- mk_freq_gtfs()
  # second service entirely outside the window, also frequency-based
  gtfs$calendar <- rbind(gtfs$calendar,
                         data.frame(service_id = "SV2", monday = 1L,
                                    tuesday = 1L, wednesday = 1L,
                                    thursday = 1L, friday = 1L,
                                    saturday = 0L, sunday = 0L,
                                    start_date = as.Date("2024-01-01"),
                                    end_date = as.Date("2024-01-31"),
                                    stringsAsFactors = FALSE))
  gtfs$trips <- rbind(gtfs$trips,
                      data.frame(route_id = "R1", service_id = "SV2",
                                 trip_id = "T3", stringsAsFactors = FALSE))
  gtfs$frequencies <- rbind(gtfs$frequencies,
                            data.frame(trip_id = "T3",
                                       start_time = "07:00:00",
                                       end_time = "08:00:00",
                                       headway_secs = 1800L,
                                       stringsAsFactors = FALSE))

  trimmed <- suppressMessages(gtfs_trim_dates(
    gtfs,
    startdate = lubridate::ymd("2023-10-02"),
    enddate = lubridate::ymd("2023-10-29")))

  expect_false("T3" %in% trimmed$trips$trip_id)
  expect_setequal(unique(trimmed$frequencies$trip_id), "T2")
})


test_that("gtfs_trips_per_zone expands frequency-based trips into time bands", {
  zone <- sf::st_sf(zone_id = "Z1",
                    geometry = sf::st_sfc(sf::st_buffer(
                      sf::st_point(c(-1.5, 53.8)), 0.01), crs = 4326))

  res <- suppressWarnings(suppressMessages(
    gtfs_trips_per_zone(mk_freq_gtfs(), zone,
                        startdate = lubridate::ymd("2023-10-02"),
                        enddate = lubridate::ymd("2023-10-29"))
  ))
  res <- as.data.frame(res)

  # 4 Morning Peak departures x 4 Mondays
  expect_equal(res[["runs_Mon_Morning Peak"]], 16)
  # (2 frequency departures + conventional T1 at 12:00) x 4 Mondays
  expect_equal(res$runs_Mon_Midday, 12)
  # service does not run at weekends
  expect_equal(res$runs_Sat_Midday, 0)
  expect_true(all(as.matrix(res[grep("^runs_", names(res))]) >= 0))
})
