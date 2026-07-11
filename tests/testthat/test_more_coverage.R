context("Additional unit coverage: summary, options, write, clean, interpolate")

test_that("gtfs_summary output is message-based and suppressable", {
  g <- list(
    agency = data.frame(agency_id = "A1"),
    stops = data.frame(stop_id = c("S1", "S2")),
    calendar = data.frame(start_date = as.Date(c("2026-01-01", "2026-02-01")),
                          end_date = as.Date(c("2026-11-30", "2026-12-31")))
  )
  expect_message(gtfs_summary(g), "Tables and number of rows")
  # everything must be suppressable (no print()/cat() to stdout)
  out <- capture.output(suppressMessages(gtfs_summary(g)), type = "output")
  expect_length(out, 0)
})


test_that("UK2GTFS_option functions get and set invisibly", {
  old_dates <- UK2GTFS_option_treatDatesAsInt()
  old_uid <- UK2GTFS_option_stopProcessingAtUid()
  old_upd <- UK2GTFS_option_updateCachedDataOnLibaryLoad()
  on.exit({
    UK2GTFS_option_treatDatesAsInt(old_dates)
    UK2GTFS_option_stopProcessingAtUid(if (is.null(old_uid)) "" else old_uid)
    UK2GTFS_option_updateCachedDataOnLibaryLoad(old_upd)
  })

  res <- withVisible(UK2GTFS_option_treatDatesAsInt(FALSE))
  expect_false(res$visible)
  expect_false(UK2GTFS_option_treatDatesAsInt())

  UK2GTFS_option_stopProcessingAtUid("C12345")
  expect_equal(UK2GTFS_option_stopProcessingAtUid(), "C12345")
  UK2GTFS_option_stopProcessingAtUid("")   # empty string clears the option
  expect_null(UK2GTFS_option_stopProcessingAtUid())

  UK2GTFS_option_updateCachedDataOnLibaryLoad(FALSE)
  expect_false(UK2GTFS_option_updateCachedDataOnLibaryLoad())
})


test_that("load_data loads into the caller's environment, not .GlobalEnv", {
  f <- function() {
    load_data("atoc_agency")
    exists("atoc_agency", inherits = FALSE)
  }
  expect_true(f())
  expect_false(exists("atoc_agency", envir = globalenv(), inherits = FALSE))
})


test_that("gtfs_write strips commas/tabs/newlines and writes NA times empty", {
  dir <- file.path(tempdir(), "write_cov")
  dir.create(dir, showWarnings = FALSE)

  times <- suppressWarnings(lubridate::hms(c("10:00:00", NA)))
  g <- list(
    stops = data.frame(stop_id = c("S1", "S2"),
                       stop_name = c("One, place\tinner", "Two\nlines"),
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = "T1",
                            arrival_time = times,
                            departure_time = times,
                            stop_id = c("S1", "S2"),
                            stop_sequence = 1:2)
  )
  gtfs_write(g, folder = dir, name = "cov1",
             stripComma = TRUE, stripTab = TRUE, stripNewline = TRUE)

  utils::unzip(file.path(dir, "cov1.zip"), exdir = file.path(dir, "x"))
  stops <- readLines(file.path(dir, "x", "stops.txt"))
  expect_false(any(grepl(",", sub("^[^,]*,", "", stops[-1]))))  # no commas in names
  expect_false(any(grepl("\t", stops)))

  st <- data.table::fread(file.path(dir, "x", "stop_times.txt"),
                          colClasses = "character")
  # an unknown time must be an empty field, never "NA:NA:NA"
  expect_equal(st$arrival_time[2], "")
  expect_equal(st$arrival_time[1], "10:00:00")
  unlink(dir, recursive = TRUE)
})


test_that("gtfs_clean removes invalid stops, trips and dangling references", {
  g <- list(
    agency = data.table::data.table(agency_id = c("A1", ""),
                                    agency_name = c("Op", "")),
    stops = data.table::data.table(
      stop_id = c("S1", "S2", "S3", "S4", "S5"),
      stop_name = paste0("Stop", 1:5),
      stop_lon = c(-1.5, -1.6, NA, -1.7, -1.8),   # S3 has no coordinates
      stop_lat = c(53.8, 53.9, NA, 53.7, 53.6)),  # S5 is never used
    routes = data.table::data.table(route_id = c("R1", "R2"),
                                    agency_id = c("A1", ""),
                                    route_type = c(3L, NA)),
    trips = data.table::data.table(route_id = c("R1", "R1", "R2"),
                                   service_id = "C1",
                                   trip_id = c("T1", "T2", "T3")),
    # T2 calls at only one stop and must be dropped
    stop_times = data.table::data.table(
      trip_id = c("T1", "T1", "T2", "T3", "T3"),
      arrival_time = "10:00:00", departure_time = "10:00:00",
      stop_id = c("S1", "S2", "S1", "S1", "S4"),
      stop_sequence = c(1L, 2L, 1L, 1L, 2L)),
    frequencies = data.table::data.table(trip_id = c("T1", "T2"),
                                         headway_secs = 600L),
    transfers = data.table::data.table(from_stop_id = c("S1", "S1"),
                                       to_stop_id = c("S2", "S99")),
    calendar = data.table::data.table(service_id = "C1")
  )

  out <- gtfs_clean(g)
  expect_setequal(out$stops$stop_id, c("S1", "S2", "S4"))   # S3, S5 gone
  expect_setequal(out$trips$trip_id, c("T1", "T3"))         # T2 gone
  expect_true(all(out$stop_times$trip_id %in% out$trips$trip_id))
  expect_equal(out$agency$agency_id[2], "MISSINGAGENCY")
  expect_equal(out$frequencies$trip_id, "T1")               # T2's frequency gone
  expect_equal(nrow(out$transfers), 1)                      # dangling S99 gone

  # public_only removes services on routes with no route_type
  out2 <- gtfs_clean(g, public_only = TRUE)
  expect_false("T3" %in% out2$trips$trip_id)
  expect_false(any(is.na(out2$routes$route_type)))
})


test_that("gtfs_interpolate_times spreads duplicated stop times", {
  st <- data.frame(
    trip_id = c(rep("T1", 4), rep("T2", 2)),
    arrival_time = c("10:00:00", "10:00:00", "10:00:00", "10:06:00",
                     "09:00:00", NA),
    departure_time = c("10:00:00", "10:00:00", "10:00:00", "10:06:00",
                       "09:00:00", NA),
    stop_id = c("A", "B", "C", "D", "A", "B"),
    stop_sequence = as.character(c(1:4, 1:2)),  # character on purpose
    stringsAsFactors = FALSE)

  out <- gtfs_interpolate_times(list(stop_times = st))
  t1 <- out$stop_times[out$stop_times$trip_id == "T1", ]
  # duplicated 10:00s interpolated evenly towards 10:06
  expect_equal(UK2GTFS:::period2gtfs(t1$arrival_time),
               c("10:00:00", "10:02:00", "10:04:00", "10:06:00"))
  # departure may never be before arrival
  expect_true(all(lubridate::period_to_seconds(t1$departure_time) >=
                    lubridate::period_to_seconds(t1$arrival_time)))
  # a trip with NA times passes through unmodified (and must not crash)
  t2 <- out$stop_times[out$stop_times$trip_id == "T2", ]
  expect_equal(UK2GTFS:::period2gtfs(t2$arrival_time), c("09:00:00", NA))
})
