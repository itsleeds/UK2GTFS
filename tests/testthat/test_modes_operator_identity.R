# What settles which operator a shared code means.
#
# `sources` keeps a rule away from a converter that cannot hold its system at
# all, but within one converter a code is not unique: `LUL` is London
# Underground in TNDS and NPTDR, and Lancashire United Ltd in the 2016 and
# 2017 Bus Archive. Both are TransXChange. So a rule that carries a stop
# pattern has to find the system in the feed before it fires.
#
# Asking only "does one of this operator's routes call at a stop the pattern
# matches?" is too strict, because an archive can hold a system whose stops it
# never names. The 2014 and 2015 Bus Archive carry the Birmingham Air-Rail Link
# as agency BHX, correctly, with no stop anywhere in the feed named "Air-Rail
# Link" or "Skytrain" - only "Birmingham International Rail Station" and
# "Birmingham Airport". On the stop test alone that correct rule stands down
# and the Skytrain drops out of the tram totals for those two years.
#
# The name the feed gives the operator is the evidence that actually separates
# the two cases: "Air-Rail Link" is the system, "Lancashire United Ltd" is
# plainly not "London Underground". Either kind of evidence is enough; neither
# means the rule stands down.


# The Air-Rail Link as the 2014 and 2015 Bus Archive hold it: the right code,
# the right agency_name, bus as the converter left it, and stops that name the
# system nowhere.
airrail_feed <- function(agency_name = "Air-Rail Link", named_stops = FALSE) {
  stops <- data.frame(
    stop_id = c("A1", "A2"),
    stop_name = if (named_stops) {
      c("Birmingham International Rail Station", "Birmingham Airport Skytrain")
    } else {
      c("Birmingham International Rail Station", "Birmingham Airport")
    },
    stop_lat = 52.45, stop_lon = -1.73, stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = "R_AIR", agency_id = "BHX",
    route_short_name = "AIR",
    route_long_name = paste("Birmingham International Rail Station -",
                            "Birmingham Airport Skytrain"),
    route_type = 3L,                    # bus, as the converter left it
    stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = "t1", route_id = "R_AIR", service_id = "s1",
                      stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = c("t1", "t1"),
    arrival_time = c("09:00:00", "09:05:00"),
    departure_time = c("09:00:00", "09:05:00"),
    stop_id = c("A1", "A2"), stop_sequence = 1:2,
    stringsAsFactors = FALSE)
  g <- list(stops = stops, routes = routes, trips = trips,
            stop_times = stop_times)
  if (!is.null(agency_name)) {
    g$agency <- data.frame(agency_id = "BHX", agency_name = agency_name,
                           stringsAsFactors = FALSE)
  }
  g
}

# Lancashire United under the `LUL` code: ordinary buses, no Underground stop,
# and an agency_name that is nothing like "London Underground".
lancs_named_feed <- function(agency_name = "Lancashire United Ltd") {
  stops <- data.frame(
    stop_id = c("L1", "L2"),
    stop_name = c("Preston Bus Station", "Burnley Manchester Road"),
    stop_lat = 53.76, stop_lon = -2.70, stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = "R_LAN", agency_id = "LUL",
    route_short_name = "152", route_long_name = "Preston - Burnley",
    route_type = 3L, stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = "t1", route_id = "R_LAN", service_id = "s1",
                      stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = c("t1", "t1"),
    arrival_time = c("09:00:00", "09:30:00"),
    departure_time = c("09:00:00", "09:30:00"),
    stop_id = c("L1", "L2"), stop_sequence = 1:2,
    stringsAsFactors = FALSE)
  list(stops = stops, routes = routes, trips = trips,
       stop_times = stop_times,
       agency = data.frame(agency_id = "LUL", agency_name = agency_name,
                           stringsAsFactors = FALSE))
}


test_that("the feed's own name for the operator lets a correct rule fire", {
  g <- UK2GTFS:::apply_standard_modes(airrail_feed(), source = "txc")
  expect_equal(g$routes$route_type, 0)
})

test_that("punctuation in the name does not matter", {
  g <- UK2GTFS:::apply_standard_modes(
    airrail_feed(agency_name = "Air Rail Link"), source = "txc")
  expect_equal(g$routes$route_type, 0)
})

test_that("a matching stop is still enough on its own", {
  # the name is useless here, so this can only pass on the stop evidence
  g <- UK2GTFS:::apply_standard_modes(
    airrail_feed(agency_name = "Operator 12345", named_stops = TRUE),
    source = "txc")
  expect_equal(g$routes$route_type, 0)
})

test_that("with neither kind of evidence the rule stands down", {
  g <- UK2GTFS:::apply_standard_modes(
    airrail_feed(agency_name = "Operator 12345"), source = "txc")
  expect_equal(g$routes$route_type, 3)
})

test_that("a feed with no agency table keeps the stop-only behaviour", {
  g <- UK2GTFS:::apply_standard_modes(
    airrail_feed(agency_name = NULL), source = "txc")
  expect_equal(g$routes$route_type, 3)
})

test_that("the Lancashire collision is still refused", {
  g <- UK2GTFS:::apply_standard_modes(lancs_named_feed(), source = "txc")
  expect_equal(g$routes$route_type, 3)
})

test_that("a real London Underground name is accepted under the same code", {
  g <- UK2GTFS:::apply_standard_modes(
    lancs_named_feed(agency_name = "London Underground"), source = "txc")
  expect_equal(g$routes$route_type, 1)
})

test_that("a short or generic agency_name cannot let a rule through", {
  # "Link" is a substring of "Birmingham Air-Rail Link", so only the length
  # floor stops it standing in for the system
  g <- UK2GTFS:::apply_standard_modes(
    airrail_feed(agency_name = "Link"), source = "txc")
  expect_equal(g$routes$route_type, 3)
})

test_that("the guard only applies to rules carrying a stop pattern", {
  # if this stops being true, the two tests above stop covering what they say
  ov <- UK2GTFS:::standard_mode_overrides()
  both <- ov[!is.na(ov$operator) & !is.na(ov$stop_pattern), ]
  expect_setequal(both$operator, c("LUL", "BHX"))
})
