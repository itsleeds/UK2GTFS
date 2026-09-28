# One train published at two NaPTAN granularities is still one train.
#
# NaPTAN gives a metro station a station-level code and a code per platform,
# and TNDS carries the Glasgow Subway, the Docklands Light Railway and
# Sheffield Supertram twice: once against the station codes and once against
# the platform codes. The two copies run at the same minutes and call at the
# same stations, so only the stop_id tells them apart - which is exactly the
# field gtfs_deduplicate() keys its itinerary test on, so it kept both.
#
# The published effect: in tnds_20231101 the Subway's weekday service held 748
# trips for a timetable of 374 - 374 calling only at station-level codes, 374
# only at platform codes, none mixing - and a Glasgow zone reported 720 tph of
# metro where the true figure is 360. The series doubles from 2015 to 2023 and
# returns to normal in 2024, when NaPTAN stopped issuing the station-level
# code, so the defect looks like a service change and not like a bug.
#
# Note what does NOT catch this. Counting trips does not: the rows really are
# distinct. Comparing full itineraries does not: the ids differ at every stop.
# Comparing (first stop, first departure, last stop, last arrival) does not
# either, for the same reason. Only knowing that two ids name one station does.

# Two identical circuits, one against station codes and one against platforms.
subway_feed <- function(platform_suffix = TRUE, named_alike = TRUE) {
  stations <- c("BUC", "STE", "BRI")
  base <- paste0("9400ZZGL", stations)
  nm <- paste(c("Buchanan Street", "St Enoch", "Bridge Street"),
              "SPT Subway Station")

  stops <- data.frame(
    stop_id = c(base, paste0(base, "1")),
    stop_name = c(nm, if (named_alike) nm else paste(nm, "Platform 1")),
    stop_lat = 55.86, stop_lon = -4.25, stringsAsFactors = FALSE)

  times <- c("15:00:00", "15:02:00", "15:04:00")
  mk <- function(trip, ids) {
    data.frame(trip_id = trip, arrival_time = times, departure_time = times,
               stop_id = ids, stop_sequence = 1:3, stringsAsFactors = FALSE)
  }
  second_ids <- if (platform_suffix) paste0(base, "1") else base

  list(
    stops = stops,
    agency = data.frame(agency_id = "SPTU", agency_name = "SPT Subway",
                        stringsAsFactors = FALSE),
    routes = data.frame(route_id = "R_SUB", agency_id = "SPTU",
                        route_short_name = "SUB",
                        route_long_name = "Glasgow - Outer Circle",
                        route_type = 1L, stringsAsFactors = FALSE),
    trips = data.frame(trip_id = c("station", "platform"),
                       route_id = "R_SUB", service_id = "s1",
                       stringsAsFactors = FALSE),
    stop_times = rbind(mk("station", base), mk("platform", second_ids)),
    calendar = data.frame(
      service_id = "s1", monday = 1L, tuesday = 1L, wednesday = 1L,
      thursday = 1L, friday = 1L, saturday = 0L, sunday = 0L,
      start_date = as.Date("2023-09-18"), end_date = as.Date("2023-10-13"),
      stringsAsFactors = FALSE))
}

n_trips <- function(g) nrow(g$trips)


test_that("the platform-coded copy of a train is removed", {
  out <- gtfs_deduplicate(subway_feed(), quiet = TRUE)
  expect_equal(n_trips(out), 1L)
})


test_that("two genuinely different trains are both kept", {
  g <- subway_feed()
  # move the second copy ten minutes later: a real extra service
  later <- g$stop_times$trip_id == "platform"
  g$stop_times$arrival_time[later] <- c("15:10:00", "15:12:00", "15:14:00")
  g$stop_times$departure_time[later] <- c("15:10:00", "15:12:00", "15:14:00")
  out <- gtfs_deduplicate(g, quiet = TRUE)
  expect_equal(n_trips(out), 2L)
})


test_that("ids are only folded together when the names agree", {
  # a stop whose id happens to be another's prefix, but a different place:
  # it must not be treated as the same station
  out <- gtfs_deduplicate(subway_feed(named_alike = FALSE), quiet = TRUE)
  expect_equal(n_trips(out), 2L)
})


test_that("the feed itself is not rewritten", {
  # the canonicalisation is for the signature only; every stop_id that
  # survives must be the one the source published
  g <- subway_feed()
  before <- sort(unique(g$stop_times$stop_id))
  out <- gtfs_deduplicate(g, quiet = TRUE)
  expect_true(all(out$stop_times$stop_id %in% before))
  expect_setequal(sort(unique(out$stops$stop_id)), sort(unique(g$stops$stop_id)))
})
