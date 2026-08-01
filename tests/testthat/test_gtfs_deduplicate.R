# Tests for gtfs_deduplicate(): a duplicate is only removed when the two trips
# are the same journey AND the copy removed runs on no date the copy kept does
# not run on.

# One trip, two stops, times given as text. Everything else is built by adding
# trips to this.
dedup_gtfs <- function() {
  list(
    agency = data.frame(
      agency_id = "A1", agency_name = "One",
      agency_url = "http://example.com", agency_timezone = "Europe/London",
      stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = c("S1", "S2"), stop_name = c("s1", "s2"),
      stop_lat = c(51, 51.01), stop_lon = c(-1, -1.01),
      stringsAsFactors = FALSE),
    routes = data.frame(
      route_id = "R1", agency_id = "A1", route_short_name = "1",
      route_long_name = "one", route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(
      route_id = "R1", service_id = "SV1", trip_id = "T1",
      stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1"),
      arrival_time = c("10:00:00", "10:10:00"),
      departure_time = c("10:01:00", "10:11:00"),
      stop_id = c("S1", "S2"), stop_sequence = c(1L, 2L),
      stringsAsFactors = FALSE),
    calendar = data.frame(
      service_id = "SV1",
      monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L, friday = 1L,
      saturday = 0L, sunday = 0L,
      start_date = as.Date("2024-01-01"), end_date = as.Date("2024-01-31"),
      stringsAsFactors = FALSE),
    calendar_dates = data.frame(
      service_id = character(), date = as.Date(character()),
      exception_type = integer(), stringsAsFactors = FALSE)
  )
}

# Copy trip `from` as `to`, optionally onto a new route and service
add_trip <- function(gtfs, from, to, route_id = NULL, service_id = NULL,
                     shift_secs = 0) {
  tr <- gtfs$trips[gtfs$trips$trip_id == from, , drop = FALSE]
  tr$trip_id <- to
  if (!is.null(route_id)) tr$route_id <- route_id
  if (!is.null(service_id)) tr$service_id <- service_id
  gtfs$trips <- rbind(gtfs$trips, tr)

  st <- gtfs$stop_times[gtfs$stop_times$trip_id == from, , drop = FALSE]
  st$trip_id <- to
  if (shift_secs != 0) {
    bump <- function(x) {
      format(as.POSIXct(x, format = "%H:%M:%S", tz = "UTC") + shift_secs,
             "%H:%M:%S")
    }
    st$arrival_time <- bump(st$arrival_time)
    st$departure_time <- bump(st$departure_time)
  }
  gtfs$stop_times <- rbind(gtfs$stop_times, st)
  gtfs
}

add_service <- function(gtfs, service_id, days = rep(1L, 5),
                        start = "2024-01-01", end = "2024-01-31") {
  cal <- data.frame(
    service_id = service_id,
    monday = days[1], tuesday = days[2], wednesday = days[3],
    thursday = days[4], friday = days[5],
    saturday = 0L, sunday = 0L,
    start_date = as.Date(start), end_date = as.Date(end),
    stringsAsFactors = FALSE)
  gtfs$calendar <- rbind(gtfs$calendar, cal)
  gtfs
}


test_that("service_operating_dates applies GTFS calendar semantics", {
  gtfs <- dedup_gtfs()
  dates <- service_operating_dates(gtfs)
  expect_equal(nrow(dates), 23) # weekdays in January 2024
  expect_true(all(as.integer(format(as.Date(dates$date, origin = "1970-01-01"),
                                    "%u")) <= 5))

  # exception_type 2 removes a date, 1 adds one, even outside the range
  gtfs$calendar_dates <- data.frame(
    service_id = c("SV1", "SV1"),
    date = as.Date(c("2024-01-01", "2024-02-03")),
    exception_type = c(2L, 1L), stringsAsFactors = FALSE)
  dates <- service_operating_dates(gtfs)
  expect_equal(nrow(dates), 23)
  expect_false(as.integer(as.Date("2024-01-01")) %in% dates$date)
  expect_true(as.integer(as.Date("2024-02-03")) %in% dates$date)

  # a service defined only in calendar_dates.txt runs on the dates it adds
  gtfs2 <- dedup_gtfs()
  gtfs2$calendar <- gtfs2$calendar[0, ]
  gtfs2$calendar_dates <- data.frame(
    service_id = "SV9", date = as.Date(c("2024-05-01", "2024-05-02")),
    exception_type = 1L, stringsAsFactors = FALSE)
  expect_equal(nrow(service_operating_dates(gtfs2)), 2)

  # eight digit dates, as fread gives them on a raw feed
  gtfs3 <- dedup_gtfs()
  gtfs3$calendar$start_date <- 20240101L
  gtfs3$calendar$end_date <- 20240131L
  expect_equal(nrow(service_operating_dates(gtfs3)), 23)

  # only the services asked for
  gtfs4 <- add_service(dedup_gtfs(), "SV2")
  expect_equal(unique(service_operating_dates(gtfs4, "SV2")$service_id), "SV2")
})


test_that("an identical trip on the same calendar is removed", {
  gtfs <- add_trip(dedup_gtfs(), "T1", "T2")
  out <- gtfs_deduplicate(gtfs, quiet = TRUE)

  expect_equal(out$trips$trip_id, "T1")
  expect_equal(nrow(out$stop_times), 2)
  expect_true(all(out$stop_times$trip_id == "T1"))
  # nothing else is touched
  expect_equal(out$routes, gtfs$routes)
  expect_equal(out$calendar, gtfs$calendar)
  expect_equal(out$stops, gtfs$stops)
})


test_that("the same service published under two route_ids is removed", {
  gtfs <- dedup_gtfs()
  gtfs$routes <- rbind(gtfs$routes,
                       data.frame(route_id = "R2", agency_id = "A1",
                                  route_short_name = "1",
                                  route_long_name = "one", route_type = 3L,
                                  stringsAsFactors = FALSE))
  gtfs <- add_trip(gtfs, "T1", "T2", route_id = "R2")

  expect_equal(gtfs_deduplicate(gtfs, quiet = TRUE)$trips$trip_id, "T1")
  # match_route = "route_id" is stricter and keeps both
  expect_equal(nrow(gtfs_deduplicate(gtfs, match_route = "route_id",
                                     quiet = TRUE)$trips), 2)
})


test_that("a differently numbered route is not a duplicate", {
  gtfs <- dedup_gtfs()
  gtfs$routes <- rbind(gtfs$routes,
                       data.frame(route_id = "R2", agency_id = "A1",
                                  route_short_name = "2",
                                  route_long_name = "two", route_type = 3L,
                                  stringsAsFactors = FALSE))
  gtfs <- add_trip(gtfs, "T1", "T2", route_id = "R2")

  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)
  # match_route = "none" ignores the numbering and removes it
  expect_equal(nrow(gtfs_deduplicate(gtfs, match_route = "none",
                                     quiet = TRUE)$trips), 1)
})


test_that("a school term journey and its holiday twin are both kept", {
  # identical times, complementary calendars: correct modelling, not duplication
  gtfs <- dedup_gtfs()
  gtfs$calendar$end_date <- as.Date("2024-01-15")
  gtfs <- add_service(gtfs, "SV2", start = "2024-01-16", end = "2024-01-31")
  gtfs <- add_trip(gtfs, "T1", "T2", service_id = "SV2")

  out <- gtfs_deduplicate(gtfs, quiet = TRUE)
  expect_equal(nrow(out$trips), 2)
  expect_equal(nrow(out$stop_times), 4)
})


test_that("a copy whose dates are a subset is removed, a partial overlap is not", {
  # SV2 runs Mondays only, inside SV1's Monday-Friday: fully redundant
  gtfs <- add_service(dedup_gtfs(), "SV2", days = c(1L, 0L, 0L, 0L, 0L))
  gtfs <- add_trip(gtfs, "T1", "T2", service_id = "SV2")
  out <- gtfs_deduplicate(gtfs, quiet = TRUE)
  expect_equal(out$trips$trip_id, "T1")

  # SV3 adds a Saturday SV1 does not run: not redundant, so both stay
  gtfs2 <- add_service(dedup_gtfs(), "SV3", days = c(1L, 0L, 0L, 0L, 0L))
  gtfs2$calendar$saturday[gtfs2$calendar$service_id == "SV3"] <- 1L
  gtfs2 <- add_trip(gtfs2, "T1", "T2", service_id = "SV3")
  expect_equal(nrow(gtfs_deduplicate(gtfs2, quiet = TRUE)$trips), 2)
})


test_that("a copy covered by the union of two others is removed", {
  # SV_A Mondays, SV_B Tuesdays, SV_C both: C is covered by A and B together
  gtfs <- dedup_gtfs()
  gtfs$calendar <- gtfs$calendar[0, ]
  gtfs$trips <- gtfs$trips[0, ]
  gtfs$stop_times$trip_id <- "TA"
  gtfs$trips <- data.frame(route_id = "R1", service_id = "SVA", trip_id = "TA",
                           stringsAsFactors = FALSE)
  gtfs <- add_service(gtfs, "SVA", days = c(1L, 0L, 0L, 0L, 0L))
  gtfs <- add_service(gtfs, "SVB", days = c(0L, 1L, 0L, 0L, 0L))
  gtfs <- add_service(gtfs, "SVC", days = c(1L, 1L, 0L, 0L, 0L))
  gtfs <- add_trip(gtfs, "TA", "TB", service_id = "SVB")
  gtfs <- add_trip(gtfs, "TA", "TC", service_id = "SVC")

  out <- gtfs_deduplicate(gtfs, quiet = TRUE)
  # TC has the most dates so it ranks first and is kept; TA and TB are then
  # each fully covered by it
  expect_equal(out$trips$trip_id, "TC")

  # every date that had service before still has service
  before <- service_operating_dates(gtfs, unique(gtfs$trips$service_id))
  after <- service_operating_dates(out, unique(out$trips$service_id))
  expect_true(all(before$date %in% after$date))
})


test_that("trips that differ in any way are kept", {
  base <- add_trip(dedup_gtfs(), "T1", "T2")

  # different times
  expect_equal(nrow(gtfs_deduplicate(
    add_trip(dedup_gtfs(), "T1", "T2", shift_secs = 60),
    quiet = TRUE)$trips), 2)

  # different stops
  gtfs <- base
  gtfs$stops <- rbind(gtfs$stops,
                      data.frame(stop_id = "S3", stop_name = "s3",
                                 stop_lat = 51.02, stop_lon = -1.02,
                                 stringsAsFactors = FALSE))
  gtfs$stop_times$stop_id[gtfs$stop_times$trip_id == "T2"][2] <- "S3"
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # different accessibility: removing one would lose information
  gtfs <- base
  gtfs$trips$wheelchair_accessible <- c(1L, 0L)
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # different block: the copy belongs to another vehicle's day
  gtfs <- base
  gtfs$trips$block_id <- c("B1", "B2")
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # different boarding rules
  gtfs <- base
  gtfs$stop_times$pickup_type <- c(0L, 0L, 1L, 0L)
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # but a blank pickup_type is the same as 0 and does not block removal
  gtfs <- base
  gtfs$stop_times$pickup_type <- c(NA, 0L, 0L, NA)
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 1)
})


test_that("frequency based trips are never removed", {
  gtfs <- add_trip(dedup_gtfs(), "T1", "T2")
  gtfs$frequencies <- data.frame(
    trip_id = c("T1", "T2"), start_time = "07:00:00", end_time = "09:00:00",
    headway_secs = c(600L, 1200L), stringsAsFactors = FALSE)

  out <- gtfs_deduplicate(gtfs, quiet = TRUE)
  expect_equal(nrow(out$trips), 2)
  expect_equal(nrow(out$frequencies), 2)
})


test_that("weak or unknown trips are left alone", {
  # a single stop trip has too weak a signature
  gtfs <- add_trip(dedup_gtfs(), "T1", "T2")
  gtfs$stop_times <- gtfs$stop_times[gtfs$stop_times$stop_sequence == 1, ]
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # a service with no operating dates at all is unknown, not duplicated
  gtfs <- add_trip(dedup_gtfs(), "T1", "T2")
  gtfs$calendar[, c("monday", "tuesday", "wednesday", "thursday", "friday")] <- 0L
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)

  # untimed stops cannot be compared
  gtfs <- add_trip(dedup_gtfs(), "T1", "T2")
  gtfs$stop_times$arrival_time <- NA_character_
  gtfs$stop_times$departure_time <- NA_character_
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 2)
})


test_that("gtfs_deduplicate handles awkward inputs", {
  gtfs <- dedup_gtfs()
  expect_equal(nrow(gtfs_deduplicate(gtfs, quiet = TRUE)$trips), 1)

  empty <- gtfs
  empty$trips <- empty$trips[0, ]
  expect_equal(nrow(gtfs_deduplicate(empty, quiet = TRUE)$trips), 0)

  dup_ids <- add_trip(gtfs, "T1", "T1")
  expect_warning(out <- gtfs_deduplicate(dup_ids, quiet = TRUE),
                 "not unique")
  expect_equal(nrow(out$trips), 2)

  # lubridate Period times, as gtfs_read() supplies them
  per <- add_trip(gtfs, "T1", "T2")
  per$stop_times$arrival_time <- lubridate::hms(per$stop_times$arrival_time)
  per$stop_times$departure_time <- lubridate::hms(per$stop_times$departure_time)
  expect_equal(nrow(gtfs_deduplicate(per, quiet = TRUE)$trips), 1)

  # times past midnight stay distinct from their modulo 24 hour twin
  late <- add_trip(gtfs, "T1", "T2")
  late$stop_times$arrival_time[3:4] <- c("34:00:00", "34:10:00")
  late$stop_times$departure_time[3:4] <- c("34:01:00", "34:11:00")
  expect_equal(nrow(gtfs_deduplicate(late, quiet = TRUE)$trips), 2)

  # data.table input comes back as data.table
  dt <- add_trip(gtfs, "T1", "T2")
  dt$trips <- data.table::as.data.table(dt$trips)
  dt$stop_times <- data.table::as.data.table(dt$stop_times)
  out <- gtfs_deduplicate(dt, quiet = TRUE)
  expect_s3_class(out$trips, "data.table")
  expect_equal(nrow(out$trips), 1)

  expect_message(gtfs_deduplicate(add_trip(gtfs, "T1", "T2")),
                 "removed 1 duplicate trips")
})
