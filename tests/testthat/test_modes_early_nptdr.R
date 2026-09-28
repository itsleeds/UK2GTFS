# Two ways a mode rule can be wrong about a system it has never seen.
#
# The rules in standard_mode_overrides() were written from the archives that
# were to hand, and both of these defects come from a rule being right about
# those and wrong about the others.
#
#  1. A stop pattern taken from the later NPTDR archives. The 2006-2011
#     archives suffix the Manchester stops "(Manchester Metrolink)", but 2004
#     and 2005 write "(Metrolink)" in upper case, so the pattern matched
#     nothing and the trams kept whatever mode the converter guessed - metro.
#     Manchester's trams sat in the metro totals for those two years, which is
#     visible in the published output as LSOA E01033681 reporting 40 tph of
#     metro in 2004-2005 and 30 tph of tram from 2006.
#
#  2. An operator code that means somebody else. `sources` keeps a rule away
#     from a converter that cannot hold the system at all, but within one
#     converter a code is still not unique: `LUL` is London Underground in
#     TNDS and NPTDR, and Lancashire United Ltd in the 2016 and 2017 Bus
#     Archive. Both are TransXChange, so `sources` cannot separate them, and
#     the rule moved 66 Lancashire bus routes into the metro totals.
#
# Both are silent. A rule that matches nothing and a rule that had nothing to
# correct produce the same output, and a rule that fires on the wrong operator
# reports a correction that looks like a success.


# One Metrolink route as NPTDR 2004 and 2005 write it: upper case, the short
# "(Metrolink)" suffix, and one unmarked mainline interchange - so the route
# is over the 0.8 threshold but not at 1.0.
metrolink_feed <- function(suffix = "(Metrolink)") {
  nm <- c("MARKET STREET", "PICCADILLY GARDENS", "WOODLANDS ROAD", "CRUMPSALL")
  stops <- data.frame(
    stop_id = c("M1", "M2", "M3", "M4", "M5"),
    stop_name = c(paste(nm, suffix), "MANCHESTER PICCADILLY"),
    stop_lat = 53.48, stop_lon = -2.24, stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = "R_MET", agency_id = "1976",
    route_short_name = "", route_long_name = "Bury - Altrincham",
    route_type = 1L,                  # metro, as the converter guessed
    stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = "t1", route_id = "R_MET", service_id = "s1",
                      stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = rep("t1", 5),
    arrival_time = sprintf("09:%02d:00", seq(0, 40, by = 10)),
    departure_time = sprintf("09:%02d:00", seq(0, 40, by = 10)),
    stop_id = c("M1", "M2", "M3", "M4", "M5"), stop_sequence = 1:5,
    stringsAsFactors = FALSE)
  list(stops = stops, routes = routes, trips = trips, stop_times = stop_times)
}

# Lancashire United: the `LUL` code, ordinary bus routes, and not one stop
# that any Underground pattern matches.
lancashire_feed <- function(underground_stops = FALSE) {
  stops <- data.frame(
    stop_id = c("L1", "L2", "L3"),
    stop_name = if (underground_stops) {
      c("Preston Bus Station", "Chorley", "Wembley Park Underground Station")
    } else {
      c("Preston Bus Station", "Chorley", "Burnley Manchester Road")
    },
    stop_lat = 53.76, stop_lon = -2.70, stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = "R_LAN", agency_id = "LUL",
    route_short_name = "152", route_long_name = "Preston - Burnley",
    route_type = 3L,                  # bus, which is what it is
    stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = "t1", route_id = "R_LAN", service_id = "s1",
                      stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = rep("t1", 3),
    arrival_time = c("09:00:00", "09:20:00", "09:40:00"),
    departure_time = c("09:00:00", "09:20:00", "09:40:00"),
    stop_id = c("L1", "L2", "L3"), stop_sequence = 1:3,
    stringsAsFactors = FALSE)
  list(stops = stops, routes = routes, trips = trips, stop_times = stop_times)
}

mode_of <- function(g, id) as.integer(g$routes$route_type[g$routes$route_id == id])


test_that("the short (Metrolink) suffix is recognised as tram", {
  g <- UK2GTFS:::apply_standard_modes(metrolink_feed(), source = "nptdr")
  expect_equal(mode_of(g, "R_MET"), 0L)
})


test_that("the long (Manchester Metrolink) suffix still is", {
  g <- UK2GTFS:::apply_standard_modes(
    metrolink_feed(suffix = "(Manchester Metrolink)"), source = "nptdr")
  expect_equal(mode_of(g, "R_MET"), 0L)
})


test_that("an unrelated bracketed suffix is left alone", {
  # the guard against the pattern being too loose: a bus route whose stops
  # happen to carry some other parenthesis must not become a tram
  g <- UK2GTFS:::apply_standard_modes(
    metrolink_feed(suffix = "(Stand C)"), source = "nptdr")
  expect_equal(mode_of(g, "R_MET"), 1L)
})


test_that("an operator code does not override a feed that contradicts it", {
  # Lancashire United under the LUL code: 66 routes of this shape were
  # relabelled metro in the 2016 Bus Archive
  g <- UK2GTFS:::apply_standard_modes(lancashire_feed(), source = "txc")
  expect_equal(mode_of(g, "R_LAN"), 3L)   # still a bus
})


test_that("the operator code still fires where the feed supports it", {
  # the case the rule exists for: an Underground route whose own stop names
  # are only partly marked. One marked stop in the feed is enough.
  g <- UK2GTFS:::apply_standard_modes(
    lancashire_feed(underground_stops = TRUE), source = "txc")
  expect_equal(mode_of(g, "R_LAN"), 1L)
})


test_that("an operator-only rule is unaffected by the guard", {
  # the Weardale Railway has no stop pattern - its stops are named as
  # ordinary rail stations - so the code is all there is and must still work
  g <- lancashire_feed()
  g$routes$agency_id <- "WRLY"
  g$routes$route_type <- 3L
  out <- UK2GTFS:::apply_standard_modes(g, source = "txc")
  expect_equal(mode_of(out, "R_LAN"), 2L)   # rail
})
