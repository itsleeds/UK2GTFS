# Tests for the standard mode rules, which are applied by every converter so
# that a route_type means the same thing whichever source a feed came from.

# A minimal feed: one tramway, one metro, one bus that happens to call at a
# tram stop, all filed the way NPTDR files them.
standard_mode_gtfs <- function() {
  stops <- data.frame(
    stop_id = c("T1", "T2", "T3", "M1", "M2", "B1", "B2",
                "D1", "D2", "H1", "H2", "A1", "A2"),
    stop_name = c("Bilston Central (Midland Metro Stop)",
                  "Black Lake (Midland Metro Stop)",
                  "Snow Hill (Midland Metro Stop)",
                  "Bank Foot (Tyne and Wear Metro Station)",
                  "Byker (Tyne and Wear Metro Station)",
                  "High Street", "Market Square",
                  "Bank DLR Station", "Beckton DLR Station",
                  "Horsted Keynes (Bluebell Railway)",
                  "Sheffield Park (Bluebell Railway)",
                  "Birmingham Airport Skytrain",
                  "Birmingham International Station"),
    stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = c("R_TRAM", "R_METRO", "R_BUS", "R_LUL",
                 "R_DLR", "R_HERITAGE", "R_BHX"),
    agency_id = c("TMM", "NXM", "BUSCO", "LUL", "DLR", "5336", "BHX"),
    route_short_name = c("MM1", "GRN", "42", "MET", "DLR", "BBR", "AIR"),
    # NPTDR files the tramway, the metro, the DLR, the heritage railway and
    # the airport people mover under one type, and in 2004 it files the
    # Underground as a bus
    route_type = c(1L, 1L, 3L, 3L, 1L, 1L, 1L),
    stringsAsFactors = FALSE)
  trips <- data.frame(
    trip_id = c("t_tram", "t_metro", "t_bus", "t_lul",
                "t_dlr", "t_heritage", "t_bhx"),
    route_id = c("R_TRAM", "R_METRO", "R_BUS", "R_LUL",
                 "R_DLR", "R_HERITAGE", "R_BHX"),
    stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = c(rep("t_tram", 3), rep("t_metro", 2),
                # the bus calls at one tram stop out of four
                "t_bus", "t_bus", "t_bus", "t_bus",
                "t_lul", "t_lul",
                "t_dlr", "t_dlr", "t_heritage", "t_heritage",
                "t_bhx", "t_bhx"),
    stop_id = c("T1", "T2", "T3", "M1", "M2",
                "B1", "B2", "T1", "M1",
                "B1", "B2",
                "D1", "D2", "H1", "H2",
                "A1", "A2"),
    stringsAsFactors = FALSE)
  list(routes = routes, trips = trips, stops = stops, stop_times = stop_times)
}


test_that("apply_standard_modes sorts trams from metros", {
  g <- standard_mode_gtfs()
  out <- UK2GTFS:::apply_standard_modes(g)
  rt <- setNames(out$routes$route_type, out$routes$route_id)

  expect_equal(unname(rt["R_TRAM"]), 0)    # Midland Metro is a tramway
  expect_equal(unname(rt["R_METRO"]), 1)   # Tyne and Wear Metro is a metro
  expect_equal(unname(rt["R_LUL"]), 1)     # filed as a bus in the 2004 archive

  # A bus that passes a couple of tram stops is still a bus: only half its
  # calls belong to a light rail system, well under the threshold
  expect_equal(unname(rt["R_BUS"]), 3)
})


test_that("apply_standard_modes gives every source the same modes", {
  # The sources disagree with each other and with themselves: the DLR is metro
  # in NPTDR and heavy rail in TNDS, the Glasgow Subway is metro in NPTDR and a
  # tram in TNDS, and the Bluebell Railway is a tram, then rail, then a bus in
  # three consecutive TNDS snapshots. One answer for all of them.
  out <- UK2GTFS:::apply_standard_modes(standard_mode_gtfs())
  rt <- setNames(out$routes$route_type, out$routes$route_id)

  expect_equal(unname(rt["R_DLR"]), 1)
  expect_equal(unname(rt["R_HERITAGE"]), 2)

  # The Air-Rail Link has two stops and one of them is a mainline station, so
  # it never reaches the threshold on its stops; it is matched on the operator
  expect_equal(unname(rt["R_BHX"]), 0)
})


test_that("apply_standard_modes leaves a feed it cannot read alone", {
  g <- standard_mode_gtfs()
  g$stops <- NULL
  # a feed missing a table it needs is returned untouched, operator rules and
  # all - there is nothing to check the stop patterns against
  expect_equal(UK2GTFS:::apply_standard_modes(g)$routes$route_type,
               c(1L, 1L, 3L, 3L, 1L, 1L, 1L))
})


test_that("standard_mode_overrides names every mode", {
  o <- standard_mode_overrides()
  expect_true(all(c("system", "stop_pattern", "route_type") %in% names(o)))
  expect_setequal(unique(o$route_type), c(0, 1, 2))
  expect_true("London Underground" %in% o$system)
  expect_true("operator" %in% names(o))
  expect_equal(o$route_type[o$system == "Docklands Light Railway"], 1)
})
