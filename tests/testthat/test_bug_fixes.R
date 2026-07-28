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


# GTFS allows a service_id defined only in calendar_dates.txt: it then runs on
# exactly the dates added there. The DfT's BODS GTFS uses this for
# school-holiday timetables, so dropping such services silently removes real
# service. SV1 is a conventional Mon-Fri service; SVX has no calendar.txt row
# and runs on two Mondays inside the window; SVY likewise but entirely outside.
mk_dates_only_gtfs <- function() {
  g <- mk_freq_gtfs()
  g$frequencies <- NULL
  g$trips <- rbind(g$trips,
                   data.frame(route_id = "R1", service_id = c("SVX", "SVY"),
                              trip_id = c("TX", "TY"), stringsAsFactors = FALSE))
  g$stop_times <- rbind(
    g$stop_times,
    data.frame(trip_id = c("TX", "TY"),
               arrival_time = lubridate::hms(c("12:00:00", "12:00:00")),
               departure_time = lubridate::hms(c("12:00:00", "12:00:00")),
               stop_id = "S1", stop_sequence = 1L, stringsAsFactors = FALSE))
  g$calendar_dates <- data.frame(
    service_id = c("SVX", "SVX", "SVY"),
    date = as.Date(c("2023-10-09", "2023-10-16", "2023-12-04")),
    exception_type = 1L, stringsAsFactors = FALSE)
  g
}


test_that("gtfs_trim_dates keeps services defined only in calendar_dates", {
  res <- suppressMessages(gtfs_trim_dates(
    mk_dates_only_gtfs(),
    startdate = lubridate::ymd("2023-10-02"),
    enddate = lubridate::ymd("2023-10-29")))

  # SVX runs on two dates inside the window and must survive
  expect_true("TX" %in% res$trips$trip_id)
  expect_setequal(res$calendar_dates$service_id[
    res$calendar_dates$exception_type == 1], c("SVX", "SVX"))
  expect_true(all(res$stop_times$trip_id %in% res$trips$trip_id))

  # SVY's only date is outside the window, so it goes, like a calendar.txt
  # service whose date range never reaches the window
  expect_false("TY" %in% res$trips$trip_id)
  expect_false("SVY" %in% res$calendar_dates$service_id)

  # the conventional service is unaffected
  expect_true("T1" %in% res$trips$trip_id)
})


test_that("gtfs_trips_per_zone counts calendar_dates-only services", {
  zone <- sf::st_sf(zone_id = "Z1",
                    geometry = sf::st_sfc(sf::st_buffer(
                      sf::st_point(c(-1.5, 53.8)), 0.01), crs = 4326))

  res <- suppressWarnings(suppressMessages(
    gtfs_trips_per_zone(mk_dates_only_gtfs(), zone,
                        startdate = lubridate::ymd("2023-10-02"),
                        enddate = lubridate::ymd("2023-10-29"))
  ))
  res <- as.data.frame(res)

  # T1 on 4 Mondays plus TX on its 2 added Mondays; TY is outside the window
  expect_equal(res$runs_Mon_Midday, 6)
  # neither service runs at the weekend
  expect_equal(res$runs_Sat_Midday, 0)
})


# A ServicedOrganisationDayType/DaysOfOperation reference means the journey
# runs only on that organisation's dates (typically a school's holidays). It
# must restrict the journey's calendar, not add to it: emitting
# exception_type = 1 rows while leaving the weekly calendar alone produces a
# journey that runs every week, because under GTFS semantics an added date on
# a day the calendar already operates does nothing.

mk_include_trips <- function() {
  trips <- data.frame(trip_id = c("VJ1", "VJ2"),
                      StartDate = as.Date("2024-01-01"),
                      EndDate = as.Date("2024-03-31"),
                      DaysOfWeek = "Monday Tuesday Wednesday Thursday Friday",
                      stringsAsFactors = FALSE)
  trips$exclude_days <- NA
  trips
}

# two school holiday weeks inside the trip's period
mk_include_ranges <- function() {
  inc <- data.frame(VehicleJourneyCode = "VJ1",
                    StartDate = as.Date(c("2024-02-12", "2024-03-25")),
                    EndDate   = as.Date(c("2024-02-16", "2024-03-29")),
                    stringsAsFactors = FALSE)
  split(inc, inc$VehicleJourneyCode)
}


test_that("include_trips restricts a trip to its serviced organisation dates", {
  trips <- mk_include_trips()
  out <- dplyr::bind_rows(lapply(split(trips, trips$trip_id), include_trips,
                                 trip_inc = mk_include_ranges()))

  # VJ1 is clipped to the span the holiday ranges cover
  expect_equal(out$StartDate[out$trip_id == "VJ1"], as.Date("2024-02-12"))
  expect_equal(out$EndDate[out$trip_id == "VJ1"], as.Date("2024-03-29"))

  excluded <- out$exclude_days[[which(out$trip_id == "VJ1")]]
  # the term-time weekdays between the two holiday weeks are excluded
  expect_true(all(lubridate::wday(excluded, week_start = 1) <= 5))
  expect_false(any(excluded %in% c(
    seq(as.Date("2024-02-12"), as.Date("2024-02-16"), by = "days"),
    seq(as.Date("2024-03-25"), as.Date("2024-03-29"), by = "days"))))

  # what survives is exactly the two holiday weeks: 10 weekdays
  span <- seq(as.Date("2024-02-12"), as.Date("2024-03-29"), by = "days")
  span <- span[lubridate::wday(span, week_start = 1) <= 5]
  expect_equal(length(setdiff(span, excluded)), 10)

  # a trip with no inclusion keeps its own calendar
  expect_equal(out$StartDate[out$trip_id == "VJ2"], as.Date("2024-01-01"))
  expect_equal(out$EndDate[out$trip_id == "VJ2"], as.Date("2024-03-31"))
  expect_equal(length(out$exclude_days[[which(out$trip_id == "VJ2")]]), 0)
})


test_that("include_trips drops a trip whose organisation dates do not overlap", {
  trips <- mk_include_trips()
  inc <- data.frame(VehicleJourneyCode = "VJ1",
                    StartDate = as.Date("2025-02-12"),
                    EndDate = as.Date("2025-02-16"), stringsAsFactors = FALSE)
  res <- include_trips(trips[trips$trip_id == "VJ1", ],
                       split(inc, inc$VehicleJourneyCode))
  expect_equal(nrow(res), 0)
})


test_that("include_trips keeps exclusions already applied to the trip", {
  trips <- mk_include_trips()
  trips <- trips[trips$trip_id == "VJ1", ]
  # a bank holiday inside one of the holiday weeks, excluded earlier
  trips$exclude_days <- list(as.Date("2024-02-14"))
  res <- include_trips(trips, mk_include_ranges())
  expect_true(as.Date("2024-02-14") %in% res$exclude_days[[1]])
})


test_that("include_trips leaves a trip alone when the ranges cover it", {
  trips <- mk_include_trips()
  trips <- trips[trips$trip_id == "VJ1", ]
  inc <- data.frame(VehicleJourneyCode = "VJ1",
                    StartDate = as.Date("2023-01-01"),
                    EndDate = as.Date("2025-01-01"), stringsAsFactors = FALSE)
  res <- include_trips(trips, split(inc, inc$VehicleJourneyCode))

  # clipped to the trip's own period, and nothing to exclude
  expect_equal(res$StartDate, as.Date("2024-01-01"))
  expect_equal(res$EndDate, as.Date("2024-03-31"))
  expect_equal(length(res$exclude_days[[1]]), 1)   # the untouched NA
  expect_true(is.na(res$exclude_days[[1]]))
})


# --- July 2026 fixes: Period corruption in gtfs_merge, typed gtfs_read,
# --- coach as extended route type 200

mk_period_gtfs <- function(pref, times) {
  n <- length(times)
  list(
    agency = data.frame(
      agency_id = paste0("A", pref), agency_name = paste0("Agency", pref),
      agency_url = "http://example.com", agency_timezone = "Europe/London",
      agency_lang = "en", stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = paste0("S", pref, seq_len(n)), stop_name = letters[seq_len(n)],
      stop_lat = 51 + seq_len(n), stop_lon = -1 - seq_len(n),
      stringsAsFactors = FALSE),
    routes = data.frame(
      route_id = "R1", agency_id = paste0("A", pref), route_short_name = "1",
      route_long_name = "one", route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(
      route_id = "R1", service_id = "SV1", trip_id = "T1",
      stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = "T1",
      arrival_time = lubridate::hms(times),
      departure_time = lubridate::hms(times),
      stop_id = paste0("S", pref, seq_len(n)),
      stop_sequence = seq_len(n)),
    calendar = data.frame(
      service_id = "SV1", monday = 1L, tuesday = 1L, wednesday = 1L,
      thursday = 1L, friday = 1L, saturday = 0L, sunday = 0L,
      start_date = "20230101", end_date = "20231231",
      stringsAsFactors = FALSE),
    calendar_dates = data.frame(
      service_id = "SV1", date = "20230704", exception_type = 2L,
      stringsAsFactors = FALSE)
  )
}

test_that("gtfs_merge does not corrupt lubridate Period time columns", {
  # rbindlist() used to keep only one input's S4 Period data, leaving a
  # column shorter than the table and aborting later dplyr verbs
  a <- mk_period_gtfs("x", c("08:00:00", "08:10:00", "08:20:00"))
  b <- mk_period_gtfs("y", c("21:55:00", "25:30:00")) # includes a >24h time

  res <- gtfs_merge(list(a, b), force = TRUE, quiet = TRUE)

  expect_equal(nrow(res$stop_times), 5)
  expect_s4_class(res$stop_times$arrival_time, "Period")
  expect_equal(length(res$stop_times$arrival_time), 5)
  secs <- sort(lubridate::period_to_seconds(res$stop_times$departure_time))
  expect_equal(secs, sort(c(28800, 29400, 30000, 78900, 91800)))
  # no day components: gtfs_write() rejects periods with days
  expect_true(all(res$stop_times$arrival_time@day == 0))
})

test_that("gtfs_merge reconciles mixed Period and character time columns", {
  a <- mk_period_gtfs("x", c("08:00:00", "08:10:00"))
  b <- mk_period_gtfs("y", c("09:00:00", "09:10:00"))
  b$stop_times$arrival_time <- c("09:00:00", "09:10:00")
  b$stop_times$departure_time <- c("09:00:00", "09:10:00")

  res <- gtfs_merge(list(a, b), force = TRUE, quiet = TRUE)

  expect_s4_class(res$stop_times$arrival_time, "Period")
  secs <- sort(lubridate::period_to_seconds(res$stop_times$arrival_time))
  expect_equal(secs, c(28800, 29400, 32400, 33000))
})

test_that("clean_route_type codes coach as extended type 200, not bus", {
  expect_equal(clean_route_type("coach"), 200)
  expect_equal(clean_route_type("COACH"), 200)
  expect_equal(clean_route_type("bus"), 3)
  expect_equal(clean_route_type("BUS"), 3)
  expect_equal(clean_route_type("tram"), 0)
  # NPTDR uses guess_bus = TRUE for unknown vehicle codes
  expect_equal(clean_route_type("UNKNOWN", guess_bus = TRUE), 3)
})

test_that("gtfs_read types frequencies.txt and id columns correctly", {
  gtfs <- mk_period_gtfs("x", c("08:00:00", "08:10:00"))
  gtfs$frequencies <- data.frame(
    trip_id = "T1", start_time = lubridate::hms("07:00:00"),
    end_time = lubridate::hms("09:00:00"), headway_secs = 600L)
  # a non-core table with a numeric-looking id that fread would mistype
  gtfs$transfers <- data.frame(
    from_stop_id = "1001", to_stop_id = "1002", transfer_type = 0L,
    stringsAsFactors = FALSE)

  tmp <- file.path(tempdir(), "gtfs_read_test")
  dir.create(tmp, showWarnings = FALSE)
  gtfs_write(gtfs, folder = tmp, name = "freq_test")
  res <- gtfs_read(file.path(tmp, "freq_test.zip"))
  unlink(tmp, recursive = TRUE)

  expect_s4_class(res$frequencies$start_time, "Period")
  expect_type(res$frequencies$trip_id, "character")
  expect_equal(lubridate::period_to_seconds(res$frequencies$end_time), 32400)
  expect_type(res$transfers$from_stop_id, "character")
  expect_type(res$transfers$to_stop_id, "character")
})


# --- atoc calendar overlay: entries crossing a Monday-Sunday week boundary

mk_overlay_cal <- function(uid, start, end, days, stp, rowid) {
  data.table::data.table(
    UID = uid,
    start_date = as.Date(start),
    end_date = as.Date(end),
    Days = days,
    STP = stp,
    rowID = rowid,
    originalUID = uid,
    duration = as.Date(end) - as.Date(start) + 1L
  )
}

test_that("makeAllOneDay handles entries crossing a week boundary", {
  # Wed 19th - Mon 24th, operating Mon/Wed/Thu/Fri: 6 days but touches two
  # Mon-Sun weeks. The old code recycled a 7-day mask over a 14-day window,
  # selecting 8 dates for 4 rows (data.table assignment error), and could
  # select dates outside the entry's own range.
  cal <- mk_overlay_cal("G18334", "2018-12-19", "2018-12-24", "1011100", "O", 1L)
  res <- makeAllOneDay(cal)

  expect_equal(nrow(res), 4)
  expect_equal(sort(res$start_date),
               as.Date(c("2018-12-19", "2018-12-20", "2018-12-21", "2018-12-24")))
  expect_true(all(res$start_date == res$end_date))
  # single-day bitmasks must match the weekday of each date
  expect_equal(res$Days,
               c("0010000", "0001000", "0000100", "1000000")[order(order(res$start_date))])
})

test_that("makeAllOneDay still handles whole Mon-Sun weeks", {
  cal <- mk_overlay_cal("X1", "2018-12-10", "2018-12-23", "1111100", "O", 1L)
  res <- makeAllOneDay(cal)
  expect_equal(nrow(res), 10) # Mon-Fri x 2 weeks
  expect_true(all(res$start_date >= as.Date("2018-12-10") &
                    res$start_date <= as.Date("2018-12-23")))
})

test_that("expandAllWeeks handles chunks crossing the Mon-Sun week boundary", {
  # Wed 19 Dec - Tue 1 Jan: weekly Wed-Tue chunks cross the week boundary,
  # which crashed the old window-mask implementation
  cal <- mk_overlay_cal("X2", "2018-12-19", "2019-01-01", "0111110", "O", 1L)
  res <- expandAllWeeks(cal)

  expect_equal(nrow(res), 2)
  expect_equal(res$start_date, as.Date(c("2018-12-19", "2018-12-26")))
  expect_equal(res$end_date, as.Date(c("2018-12-25", "2019-01-01")))
  expect_true(all(res$duration == res$end_date - res$start_date + 1))

  # aligned entries keep the documented weekday-span chunk semantics
  cal2 <- mk_overlay_cal("X3", "2023-01-02", "2023-01-18", "1110000", "P", 1L)
  res2 <- expandAllWeeks(cal2)
  expect_equal(res2$start_date,
               as.Date(c("2023-01-02", "2023-01-09", "2023-01-16")))
  expect_equal(res2$end_date,
               as.Date(c("2023-01-04", "2023-01-11", "2023-01-18")))
})

test_that("makeCalendarInner handles the G18334 overlay pattern (2018 CIF)", {
  cal <- rbind(
    mk_overlay_cal("G18334", "2018-12-10", "2018-12-18", "1111100", "P", 1L),
    mk_overlay_cal("G18334", "2018-12-10", "2018-12-18", "1111100", "O", 2L),
    mk_overlay_cal("G18334", "2018-12-19", "2019-05-17", "1111100", "P", 3L),
    mk_overlay_cal("G18334", "2018-12-19", "2018-12-24", "1011100", "O", 4L),
    mk_overlay_cal("G18334", "2018-12-27", "2019-05-17", "1111100", "O", 5L)
  )
  expect_no_error(res <- makeCalendarInner(cal))
  expect_true(is.data.frame(res[[1]]))
  expect_true(nrow(res[[1]]) > 0)
})


test_that("gtfs_interpolate_times interpolates only trips that need it", {
  gtfs <- list(
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T1", "T1",   # duplicated times, interpolate
                  "T2", "T2",               # unique times, untouched
                  "T3", "T3"),              # NA time, untouched
      arrival_time = c("10:00:00", "10:00:00", "10:00:00", "10:30:00",
                       "09:00:00", "09:10:00",
                       "08:00:00", NA),
      departure_time = c("10:00:00", "10:00:00", "10:00:00", "10:30:00",
                         "09:00:00", "09:10:00",
                         "08:00:00", NA),
      stop_id = c("S1", "S2", "S3", "S4", "S1", "S2", "S1", "S2"),
      stop_sequence = c(1:4, 1:2, 1:2),
      stringsAsFactors = FALSE)
  )

  res <- suppressMessages(gtfs_interpolate_times(gtfs))
  st <- res$stop_times
  st <- st[order(st$trip_id, st$stop_sequence), ]

  t1 <- lubridate::period_to_seconds(st$arrival_time[st$trip_id == "T1"])
  expect_equal(t1, c(36000, 36600, 37200, 37800)) # 10:00, 10:10, 10:20, 10:30
  t2 <- lubridate::period_to_seconds(st$arrival_time[st$trip_id == "T2"])
  expect_equal(t2, c(32400, 33000))
  t3 <- st$arrival_time[st$trip_id == "T3"]
  expect_equal(lubridate::period_to_seconds(t3[1]), 28800)
  expect_true(is.na(lubridate::period_to_seconds(t3[2])))
})


# --- July 2026 fixes: BODS TransXChange import failures -------------------

# A ServicedOrganisation may carry descriptive siblings alongside WorkingDays
# and Holidays (PrivateCode, PostalAddress, ServicedOrganisationClassification,
# NatureOfOrganisation, PhaseOfEducation, ContactPerson, ...). These were
# rejected outright, dropping the whole file, even though none of them carry
# operating dates.

so_xml <- function(extra = "") {
  xml2::read_xml(paste0(
    '<TransXChange xmlns="http://www.transxchange.org.uk/">',
    '<ServicedOrganisations><ServicedOrganisation>',
    '<OrganisationCode>SCH1</OrganisationCode>',
    '<Name>A School</Name>', extra,
    '<WorkingDays><DateRange>',
    '<StartDate>2024-01-08</StartDate><EndDate>2024-02-09</EndDate>',
    '</DateRange></WorkingDays>',
    '</ServicedOrganisation></ServicedOrganisations></TransXChange>'))
}

test_that("import_ServicedOrganisations accepts descriptive siblings", {
  extras <- c(
    "<PrivateCode>1234</PrivateCode>",
    "<ServicedOrganisationClassification>school</ServicedOrganisationClassification>",
    "<NatureOfOrganisation>LEA</NatureOfOrganisation>",
    "<PhaseOfEducation>secondary</PhaseOfEducation>",
    "<ContactPerson>A Person</ContactPerson>",
    "<ContactTelephoneNumber>01234 567890</ContactTelephoneNumber>",
    paste0('<PostalAddress><Line xmlns="http://www.govtalk.gov.uk/people/',
           'AddressAndPersonalDetails">Shirehall</Line></PostalAddress>'))

  for (extra in extras) {
    so <- xml2::xml_child(so_xml(extra), "d1:ServicedOrganisations")
    res <- import_ServicedOrganisations(so)
    expect_equal(nrow(res), 1)
    expect_equal(res$OrganisationCode, "SCH1")
    expect_equal(res$WorkingDays.StartDate, as.Date("2024-01-08"))
    expect_equal(res$WorkingDays.EndDate, as.Date("2024-02-09"))
  }
})

test_that("import_ServicedOrganisations still rejects unknown dated elements", {
  # An unrecognised element that carries dates would mean operating dates were
  # being dropped silently, so it must still stop()
  extra <- paste0("<TermTime><DateRange><StartDate>2024-03-01</StartDate>",
                  "<EndDate>2024-03-08</EndDate></DateRange></TermTime>")
  so <- xml2::xml_child(so_xml(extra), "d1:ServicedOrganisations")
  expect_error(import_ServicedOrganisations(so),
               "Unknown Structure in ServicedOrganisations")
})


# XML comments inside a JourneyPatternSection were counted as timing links by
# xml_length(only_elements = FALSE), so JPS_id came back longer than every
# other column and data.frame() aborted with "arguments imply differing
# number of rows".

test_that("import_journeypatternsections ignores XML comments", {
  jptl <- function(id, from, to) paste0(
    '<JourneyPatternTimingLink id="', id, '">',
    '<From><StopPointRef>', from, '</StopPointRef>',
    '<TimingStatus>PTP</TimingStatus></From>',
    '<To><StopPointRef>', to, '</StopPointRef>',
    '<TimingStatus>PTP</TimingStatus></To>',
    '<RunTime>PT5M</RunTime></JourneyPatternTimingLink>')

  build <- function(comments) xml2::xml_child(xml2::read_xml(paste0(
    '<TransXChange xmlns="http://www.transxchange.org.uk/">',
    '<JourneyPatternSections><JourneyPatternSection id="JPS1">',
    comments, jptl("L1", "S1", "S2"), comments, jptl("L2", "S2", "S3"),
    '</JourneyPatternSection></JourneyPatternSections></TransXChange>')),
    "d1:JourneyPatternSections")

  res <- import_journeypatternsections(build("<!-- a comment -->"))
  expect_equal(nrow(res), 2)
  expect_equal(res$JPTL_ID, c("L1", "L2"))
  expect_equal(res$JPS_id, c("JPS1", "JPS1"))
  # commenting the file changes nothing
  expect_equal(res, import_journeypatternsections(build("")))
})


# Some feeds publish a file with an empty <VehicleJourneys/> element. There is
# nothing to convert, but assigning the operating-profile columns to the
# resulting 0-row data frame aborted with "replacement has 1 row, data has 0".

test_that("transxchange_import returns NULL for an empty VehicleJourneys", {
  txc <- paste0(
    '<TransXChange xmlns="http://www.transxchange.org.uk/" ',
    'CreationDateTime="2026-01-01T00:00:00" ',
    'ModificationDateTime="2026-01-01T00:00:00">',
    '<StopPoints><AnnotatedStopPointRef>',
    '<StopPointRef>S1</StopPointRef><CommonName>Stop 1</CommonName>',
    '</AnnotatedStopPointRef></StopPoints>',
    '<JourneyPatternSections><JourneyPatternSection id="JPS1">',
    '<JourneyPatternTimingLink id="L1">',
    '<From><StopPointRef>S1</StopPointRef><TimingStatus>PTP</TimingStatus></From>',
    '<To><StopPointRef>S2</StopPointRef><TimingStatus>PTP</TimingStatus></To>',
    '<RunTime>PT5M</RunTime></JourneyPatternTimingLink>',
    '</JourneyPatternSection></JourneyPatternSections>',
    '<Operators><Operator id="OP1">',
    '<OperatorCode>OP1</OperatorCode><OperatorShortName>Op One</OperatorShortName>',
    '</Operator></Operators>',
    '<Services><Service>',
    '<ServiceCode>SVC1</ServiceCode><Mode>bus</Mode>',
    '<Description>Test</Description>',
    '<RegisteredOperatorRef>OP1</RegisteredOperatorRef>',
    '<OperatingPeriod><StartDate>2026-01-05</StartDate>',
    '<EndDate>2026-03-01</EndDate></OperatingPeriod>',
    '<OperatingProfile><RegularDayType><DaysOfWeek><Monday/>',
    '</DaysOfWeek></RegularDayType></OperatingProfile>',
    '<Lines><Line id="LN1"><LineName>1</LineName></Line></Lines>',
    '<StandardService><Origin>A</Origin><Destination>B</Destination>',
    '<JourneyPattern id="JP1"><Direction>outbound</Direction>',
    '<JourneyPatternSectionRefs>JPS1</JourneyPatternSectionRefs>',
    '</JourneyPattern></StandardService>',
    '</Service></Services>',
    '<VehicleJourneys/>',
    '</TransXChange>')

  f <- tempfile(fileext = ".xml")
  on.exit(unlink(f))
  writeLines(txc, f)

  expect_warning(res <- transxchange_import(f), "No VehicleJourneys")
  expect_null(res)
})
