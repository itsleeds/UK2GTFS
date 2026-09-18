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
  expect_setequal(unique(o$route_type), c(0, 1, 2, 6))
  expect_true("London Underground" %in% o$system)
  expect_true("operator" %in% names(o))
  expect_equal(o$route_type[o$system == "Docklands Light Railway"], 1)
  # every column is one vector of the same length, so a row added to one and
  # not the others is caught here rather than by recycling into nonsense
  expect_length(unique(lengths(o)), 1)
  # the airport people movers are trams, whatever the source calls them
  expect_equal(o$route_type[o$system == "Luton DART"], 0)
  expect_equal(o$route_type[o$system == "Birmingham Air-Rail Link"], 0)
  # the cable car is an aerial lift, and has carried two operator codes
  expect_setequal(o$operator[o$system == "London Cable Car"], c("CAB", "EAL"))
  expect_setequal(o$route_type[o$system == "London Cable Car"], 6)
  # systems that postdate NPTDR are limited to TransXChange
  expect_true("sources" %in% names(o))
  expect_setequal(o$sources[o$system == "London Cable Car"], "txc")
  expect_equal(o$sources[o$system == "Luton DART"], "txc")
  # the operator-matched rows that DO appear in NPTDR keep firing there
  expect_equal(o$sources[o$system == "London Underground"], "any")
  expect_equal(o$sources[o$system == "Weardale Railway"], "any")
})


test_that("a rule is skipped on a source its system cannot appear in", {
  # NPTDR ends in 2011 and reuses operator codes: CAB in the 2010 archive is
  # not the London cable car, which opened in 2012. Same feed, two sources.
  g <- list(
    routes = data.frame(route_id = c("a", "b"),
                        agency_id = c("CAB", "LUL"),
                        route_type = c(3L, 3L),
                        stringsAsFactors = FALSE),
    trips = data.frame(route_id = c("a", "b"), trip_id = c("t1", "t2"),
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = c("t1", "t2"),
                            stop_id = c("s1", "s2"),
                            stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("s1", "s2"),
                       stop_name = c("Somewhere", "Somewhere Else"),
                       stringsAsFactors = FALSE))

  txc <- UK2GTFS:::apply_standard_modes(g, source = "txc")$routes$route_type
  expect_equal(txc, c(6, 1))      # cable car and Underground both applied

  np <- UK2GTFS:::apply_standard_modes(g, source = "nptdr")$routes$route_type
  expect_equal(np, c(3L, 1))      # CAB left alone, Underground still applied

  # no source given: every row applies, as before
  allr <- UK2GTFS:::apply_standard_modes(g)$routes$route_type
  expect_equal(allr, c(6, 1))
})


test_that("the operator only rules reassign just that operator", {
  g <- list(
    routes = data.frame(
      route_id = c("a", "b", "c", "d"),
      agency_id = c("DART", "CAB", "DHF", "DT"),
      route_type = c(2L, 2L, 4L, 3L),
      stringsAsFactors = FALSE),
    trips = data.frame(route_id = c("a", "b", "c", "d"),
                       trip_id = c("t1", "t2", "t3", "t4"),
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = c("t1", "t2", "t3", "t4"),
                            stop_id = c("s1", "s2", "s3", "s4"),
                            stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("s1", "s2", "s3", "s4"),
                       stop_name = c("Luton Airport DART Station",
                                     "Greenwich Peninsula",
                                     "Dartmouth Higher Ferry",
                                     "Dartmouth Road"),
                       stringsAsFactors = FALSE))
  out <- UK2GTFS:::apply_standard_modes(g)$routes$route_type
  # DART becomes a tram and CAB an aerial lift; DHF and DT merely look like
  # them by name and must not move
  expect_equal(out, c(0, 6, 4L, 3L))
})
