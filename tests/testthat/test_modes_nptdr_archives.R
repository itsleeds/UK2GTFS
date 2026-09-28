# The hardcoded NPTDR rules, and the two things that keep them safe.
#
# NPTDR ended in 2011 and will not be republished, so five rules are keyed
# directly on the operator codes in the archives we hold. They exist because
# the stop-name patterns were written from the 2006 and later archives: in 2004
# and 2005 the Sheffield Supertram calls at "Meadowhall Interchange" and the
# Blackpool tramway at "FLEETWOOD Rossall Lane", carrying no marker any pattern
# can find, so both kept the mode the converter guessed, which was metro.
#
# Keying on an operator code is normally the thing not to do, and these tests
# are about the two guards that make it acceptable here.
#
#  1. The year gate. NPTDR renumbers operators every archive. `1129` is the
#     Blackpool tramway's 20 routes PLUS 192 bus routes in 2005, and in 2004 it
#     is 132 bus routes and no tramway at all. A rule for 2005 must not touch
#     2004, and with no year given at all it must not fire either - no year
#     means no evidence, and these rules exist because the code is ambiguous.
#
#  2. The from_route_type restriction. Those same codes cover a tramway and a
#     bus fleet at once, so only routes already filed as metro move. Where a
#     system is split across metro and bus in one archive - Sheffield in 2005,
#     Nottingham in 2004, both with the two halves calling at the same tram
#     stops - the rule deliberately has no restriction and takes the operator
#     whole.

ov <- UK2GTFS:::standard_mode_overrides()

# A feed under one operator code: some routes metro, some bus, no stop name
# that any pattern matches - the shape of Blackpool in the 2005 archive.
coded_feed <- function(code, n_metro = 2, n_bus = 3) {
  n <- n_metro + n_bus
  stops <- data.frame(
    stop_id = paste0("s", seq_len(n)),
    stop_name = paste("FLEETWOOD Stop", seq_len(n)),
    stop_lat = 53.9, stop_lon = -3.0, stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = paste0("r", seq_len(n)),
    agency_id = code,
    route_short_name = as.character(seq_len(n)),
    route_type = c(rep(1L, n_metro), rep(3L, n_bus)),
    stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = paste0("t", seq_len(n)),
                      route_id = paste0("r", seq_len(n)),
                      service_id = "s1", stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = paste0("t", seq_len(n)),
    arrival_time = "09:00:00", departure_time = "09:00:00",
    stop_id = paste0("s", seq_len(n)), stop_sequence = 1L,
    stringsAsFactors = FALSE)
  list(stops = stops, routes = routes, trips = trips, stop_times = stop_times)
}

modes <- function(g) as.integer(g$routes$route_type)


test_that("the table carries the NPTDR rules and gates the numeric codes", {
  expect_true(all(c("years", "from_route_type") %in% names(ov)))

  # The invariant, stated over every numeric code rather than the ones that
  # happen to be here now: NPTDR renumbers its operators between archives, so
  # a rule keyed on a number is only ever safe for the archive it was measured
  # against. Any future numeric rule has to be gated the same way, and this
  # fails if one is added without the gates.
  num <- ov[!is.na(ov$operator) & grepl("^[0-9]+$", ov$operator), ]
  expect_gt(nrow(num), 0L)
  expect_true(all(!is.na(num$years)))
  expect_true(all(!is.na(num$from_route_type)))
  expect_true(all(num$sources == "nptdr"))

  # and the reverse: no rule may name a year without also being source-gated
  named <- ov[!is.na(ov$years), ]
  expect_true(all(named$sources != "any"))
})


test_that("a year-gated rule fires only on its own archive", {
  g <- coded_feed("1129")
  y2005 <- UK2GTFS:::apply_standard_modes(g, source = "nptdr", year = "2005")
  expect_equal(modes(y2005), c(0L, 0L, 3L, 3L, 3L))   # the trams, not the buses

  # 2004: the same code is a different operator, all buses
  y2004 <- UK2GTFS:::apply_standard_modes(g, source = "nptdr", year = "2004")
  expect_equal(modes(y2004), c(1L, 1L, 3L, 3L, 3L))   # untouched
})


test_that("a year-gated rule does not fire when no year is given", {
  g <- coded_feed("1129")
  out <- UK2GTFS:::apply_standard_modes(g, source = "nptdr")
  expect_equal(modes(out), c(1L, 1L, 3L, 3L, 3L))
})


test_that("from_route_type never moves the operator's buses", {
  # 192 bus routes rode on this code in the real archive; the count here is
  # token but the property is the one that matters
  g <- coded_feed("1001", n_metro = 1, n_bus = 6)
  out <- UK2GTFS:::apply_standard_modes(g, source = "nptdr", year = "2006")
  expect_equal(modes(out), c(0L, rep(3L, 6)))
})


test_that("an ungated rule takes its operator whole", {
  # Sheffield 2005 and Nottingham 2004 file one tram system partly as metro and
  # partly as bus, both halves calling at the same tram stops, so every route
  # of the operator has to move
  for (code in c("STM", "NET")) {
    g <- coded_feed(code, n_metro = 2, n_bus = 3)
    out <- UK2GTFS:::apply_standard_modes(g, source = "nptdr", year = "2005")
    expect_equal(modes(out), rep(0L, 5), info = code)
  }
})


test_that("the NPTDR rules do not reach another source", {
  # `NET`, `STM` and `TMM` appear under no other converter, but the guard has
  # to hold regardless: a TransXChange feed must be unaffected
  for (code in c("STM", "NET", "TMM")) {
    g <- coded_feed(code, n_metro = 2, n_bus = 3)
    out <- UK2GTFS:::apply_standard_modes(g, source = "txc", year = "2005")
    expect_equal(modes(out), c(1L, 1L, 3L, 3L, 3L), info = code)
  }
})


test_that("the archive year is read from the file name", {
  expect_equal(UK2GTFS:::nptdr_archive_year("October-2006.zip"), "2006")
  expect_equal(UK2GTFS:::nptdr_archive_year("x/y/October-2011.zip"), "2011")
  # no year in the name leaves the gated rules switched off rather than
  # applying them to an archive they were not measured against
  expect_true(is.na(UK2GTFS:::nptdr_archive_year("nptdr-archive.zip")))
})
