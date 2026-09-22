# The mode rules have to run AFTER the stop names are attached.
#
# Thirteen of the eighteen rows in standard_mode_overrides() recognise a
# system by the NAPTAN names of the stops its routes call at. Only five match
# on an operator code. So a feed whose stops are not named yet can still be
# "corrected" - by the five - while the thirteen match nothing.
#
# transxchange2gtfs() moved the NAPTAN join out of the per-file loop for
# speed and, in doing so, moved it AFTER apply_standard_modes(). Nothing
# failed. The conversion still reported corrections, the feeds still
# validated, and the stop names were present in the output because the join
# ran a few lines later. The only visible symptom was that the Docklands
# Light Railway came out as heavy rail and the Glasgow Subway as a tram - in
# the snapshots converted after the change, and not in the ones converted
# before, which made a whole published series incomparable between years.
#
# A silent failure needs a test that looks at the mechanism, not the result:
# "no route was corrected" is the same observation whether the rules ran and
# found nothing or never ran at all.

# One DLR route, filed as heavy rail the way TNDS files it. The only thing
# that can identify it is its stops' names.
dlr_feed <- function(named = TRUE) {
  stops <- data.frame(
    stop_id = c("D1", "D2", "D3"),
    stop_name = if (named) {
      c("Bank DLR Station", "Poplar DLR Station", "Beckton DLR Station")
    } else {
      # what the per-file exports emit before the NAPTAN join: bare ids
      rep(NA_character_, 3)
    },
    stop_lat = c(51.51, 51.51, 51.51),
    stop_lon = c(-0.09, -0.02, 0.06),
    stringsAsFactors = FALSE)
  routes <- data.frame(
    route_id = "R_DLR", agency_id = "DLR",
    route_short_name = "DLR", route_long_name = "Bank - Beckton",
    route_type = 2L,                      # heavy rail, as published
    stringsAsFactors = FALSE)
  trips <- data.frame(trip_id = "t1", route_id = "R_DLR", service_id = "s1",
                      stringsAsFactors = FALSE)
  stop_times <- data.frame(
    trip_id = rep("t1", 3),
    arrival_time = c("09:00:00", "09:10:00", "09:20:00"),
    departure_time = c("09:00:00", "09:10:00", "09:20:00"),
    stop_id = c("D1", "D2", "D3"), stop_sequence = 1:3,
    stringsAsFactors = FALSE)
  list(stops = stops, routes = routes, trips = trips, stop_times = stop_times)
}

mode_of <- function(g) as.integer(g$routes$route_type[g$routes$route_id == "R_DLR"])


test_that("the rule fires once the stops are named", {
  g <- UK2GTFS:::apply_standard_modes(dlr_feed(named = TRUE), source = "txc")
  expect_equal(mode_of(g), 1L)   # metro
})


test_that("an unnamed feed cannot be corrected, and says so", {
  # This is the defect's exact shape. Without the warning the caller has no
  # way to tell this apart from a feed that needed no correction.
  expect_warning(
    g <- UK2GTFS:::apply_standard_modes(dlr_feed(named = FALSE),
                                        source = "txc"),
    "stop-pattern rules cannot match")
  expect_equal(mode_of(g), 2L)   # unchanged: still heavy rail
})


test_that("an operator-code rule still fires without stop names", {
  # Why the inversion was survivable enough to go unnoticed: the rules that
  # do not need stop names kept working, so the conversion still reported
  # corrections and nothing looked broken.
  g <- dlr_feed(named = FALSE)
  g$routes$agency_id <- "DART"          # Luton DART, an operator-code rule
  suppressWarnings(
    g2 <- UK2GTFS:::apply_standard_modes(g, source = "txc"))
  expect_equal(as.integer(g2$routes$route_type[1]), 0L)   # tram
})


test_that("transxchange2gtfs names its stops before it applies the modes", {
  # The integration check. It asserts the ORDER rather than a mode, because
  # the example archive contains no light railway to be corrected - and a
  # test that can only fail when the fixture happens to contain a DLR is not
  # a test of the ordering.
  #
  # apply_standard_modes() warns when it is handed a nameless feed, so the
  # conversion raising no such warning is the evidence that the join came
  # first.
  skip_on_cran()
  data_path <- file.path(tempdir(), "uk2gtfs_data")
  dir.create(data_path, showWarnings = FALSE)
  skip_if_not(isTRUE(try(dl_example_file(data_path, "transxchange"),
                         silent = TRUE)),
              "example transxchange archive unavailable")

  cal <- get_bank_holidays()
  naptan <- get_naptan()

  warnings_seen <- character(0)
  gtfs <- withCallingHandlers(
    transxchange2gtfs(path_in = file.path(data_path, "transxchange.zip"),
                      cal = cal, naptan = naptan, ncores = 1,
                      try_mode = FALSE, force_merge = TRUE, silent = TRUE),
    warning = function(w) {
      warnings_seen <<- c(warnings_seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })

  expect_false(any(grepl("stop-pattern rules cannot match", warnings_seen)),
               info = paste("apply_standard_modes ran before the NAPTAN join;",
                            "every stop-name rule was skipped"))
  # and the names really are there in what comes back
  expect_true(any(!is.na(gtfs$stops$stop_name) & nzchar(gtfs$stops$stop_name)))
})
