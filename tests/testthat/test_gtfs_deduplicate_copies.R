# Tests for gtfs_deduplicate_copies(): one timetable published by two files,
# with different operator names and different intermediate timings.
# Fixtures are in helper-copies.R.

test_that("an X6-like copy with other timings and operator name is removed", {
  g <- copies_gtfs(
    copies_route("em", "AMID", "X6", deps10, "S1", mid_offset = 10),
    copies_route("wm", "AMD", "6-X6-0-y11-2", deps10, "S2", mid_offset = 12),
    calendar = rbind(cal_row("S1", "2026-08-22", "2027-01-02"),
                     cal_row("S2", "2026-09-01", "2027-01-02")))
  # gtfs_deduplicate cannot see it: different names, different middle times
  expect_equal(nrow(gtfs_deduplicate(g, quiet = TRUE)$trips), 20)

  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  rep <- attr(out, "copies_report")
  expect_equal(nrow(rep), 1)
  expect_equal(rep$kept_routes, "wm") # the newer calendar
  expect_equal(rep$decided_by, "newer")
  # wm runs from 1 Sept, so em keeps its journeys before that and loses the rest
  expect_equal(rep$trips_trimmed, 10)
  expect_equal(as.vector(runs_on(out, "2026-11-20")), c(0, 10))
  expect_equal(as.vector(runs_on(out, "2026-08-25")), c(10, 0))
})

test_that("a superseded open-ended copy keeps its dates before the successor", {
  g <- copies_gtfs(
    copies_route("gahl", "GAHL", "439", deps10, "S1", to = "D", short = "439"),
    copies_route("tfl", "METR", "51-439-_-y05-5", deps10, "S2", short = "439"),
    calendar = rbind(cal_row("S1", "2024-03-02", "2027-01-02"),
                     cal_row("S2", "2026-11-13", "2027-01-02")))
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_equal(as.vector(runs_on(out, "2026-11-12")), c(10, 0))
  expect_equal(as.vector(runs_on(out, "2026-11-13")), c(0, 10))
  # nothing deleted outright, the trips moved to a trimmed calendar
  expect_equal(nrow(out$trips), 20)
  expect_true(all(grepl("_xcopy", out$trips$service_id[out$trips$route_id == "gahl"])))
  expect_equal(attr(out, "copies_report")$decided_by, "newer")
})

test_that("identical calendars fall back to the copy with more stops", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "X6", deps10, "S1"),
    copies_route("b", "AMD", "6-X6", deps10, "S1", mid_offset = 12),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  # give b an extra stop on every trip
  extra <- g$stop_times[g$stop_times$trip_id %in% g$trips$trip_id[g$trips$route_id == "b"] &
                          g$stop_times$stop_sequence == 3, ]
  extra$stop_sequence <- 4L
  extra$stop_id <- "D"
  g$stop_times <- rbind(g$stop_times, extra)
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  rep <- attr(out, "copies_report")
  expect_equal(rep$kept_routes, "b")
  expect_equal(rep$decided_by, "longer")
  expect_equal(rep$trips_removed, 10)
  expect_equal(nrow(out$trips), 10)
})

test_that("a joint timetable sharing out the departures is not a copy", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "1", deps10[c(TRUE, FALSE)], "S1"),
    copies_route("b", "AMD", "2", deps10[c(FALSE, TRUE)], "S1"),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_equal(nrow(attr(out, "copies_report")), 0)
  expect_equal(nrow(out$trips), 10)
})

test_that("the same number in another town is not a copy", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "1", deps10, "S1", from = "A"),
    copies_route("b", "AMD", "2", deps10, "S1", from = "D"),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  expect_equal(nrow(gtfs_deduplicate_copies(g, quiet = TRUE)$trips), 20)
})

test_that("routes of one service are left to gtfs_deduplicate", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "X6", deps10, "S1"),
    copies_route("b", "AMID", "X6", deps10, "S1", mid_offset = 12),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  expect_equal(nrow(gtfs_deduplicate_copies(g, quiet = TRUE)$trips), 20)
})

test_that("one coincident journey does not make a copy", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "1", "07:00", "S1"),
    copies_route("b", "AMD", "2", deps10, "S1"),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  expect_equal(nrow(gtfs_deduplicate_copies(g, quiet = TRUE)$trips), 11)
  expect_equal(nrow(gtfs_deduplicate_copies(g, min_shared = 1,
                                            quiet = TRUE)$trips), 10)
})

test_that("copies on different days are both kept", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "1", deps10, "S1"),
    copies_route("b", "AMD", "2", deps10, "S2"),
    calendar = rbind(cal_row("S1", "2026-10-04", "2027-01-02"),
                     cal_row("S2", "2026-10-04", "2027-01-02",
                             days = c(0, 0, 0, 0, 0, 1, 0))))
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_equal(nrow(out$trips), 20)
  expect_equal(sum(attr(out, "copies_report")$trip_days_removed), 0)
})

test_that("scope route removes a copy's extra journeys only when it is mostly a copy", {
  # b: the 10 journeys of a plus one extra at 18:00 -> share of b is 10/11
  g <- copies_gtfs(
    copies_route("a", "METR", "51-439", deps10, "S2", short = "439"),
    copies_route("b", "GAHL", "439", c(deps10, "18:00"), "S1", short = "439"),
    calendar = rbind(cal_row("S1", "2024-03-02", "2027-01-02"),
                     cal_row("S2", "2026-11-13", "2027-01-02")))
  tr <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_equal(as.vector(runs_on(tr, "2026-11-20")), c(10, 1))
  rt <- gtfs_deduplicate_copies(g, scope = "route", quiet = TRUE)
  expect_equal(as.vector(runs_on(rt, "2026-11-20")), c(10, 0))
  expect_equal(attr(rt, "copies_report")$scope, "route")

  # the newer service is the small one: the older, larger one only loses
  # the matched journeys even with scope = "route"
  g2 <- copies_gtfs(
    copies_route("a", "METR", "51-439", deps10[1:5], "S2", short = "439"),
    copies_route("b", "GAHL", "439", deps10, "S1", short = "439"),
    calendar = rbind(cal_row("S1", "2024-03-02", "2027-01-02"),
                     cal_row("S2", "2026-11-13", "2027-01-02")))
  rt2 <- gtfs_deduplicate_copies(g2, scope = "route", quiet = TRUE)
  expect_equal(as.vector(runs_on(rt2, "2026-11-20")), c(5, 5))
  expect_equal(attr(rt2, "copies_report")$scope, "trips")
})

test_that("calendar_dates additions on removed dates are not carried over", {
  g <- copies_gtfs(
    copies_route("old", "GAHL", "439", deps10, "S1", short = "439"),
    copies_route("new", "METR", "51-439", deps10, "S2", short = "439"),
    calendar = rbind(cal_row("S1", "2026-01-05", "2026-12-31"),
                     cal_row("S2", "2026-12-01", "2026-12-31",
                             days = c(1, 1, 1, 1, 1, 1, 0))))
  # old also runs on Saturday 5 December by exception, which new covers
  g$calendar_dates <- data.frame(service_id = "S1",
                                 date = as.Date("2026-12-05"),
                                 exception_type = 1L, stringsAsFactors = FALSE)
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_equal(as.vector(runs_on(out, "2026-12-05")), c(0, 10))
  expect_equal(as.vector(runs_on(out, "2026-11-30")), c(10, 0))
  expect_equal(as.vector(runs_on(out, "2026-12-07")), c(0, 10))
  cd <- out$calendar_dates
  expect_false(any(duplicated(cd[, c("service_id", "date")])))
})

test_that("match = 'ends' needs the same last stop and arrival", {
  g <- copies_gtfs(
    copies_route("a", "GAHL", "439", deps10, "S1", to = "D", short = "439"),
    copies_route("b", "METR", "51-439", deps10, "S2", short = "439"),
    calendar = rbind(cal_row("S1", "2024-03-02", "2027-01-02"),
                     cal_row("S2", "2026-11-13", "2027-01-02")))
  expect_equal(nrow(attr(gtfs_deduplicate_copies(g, quiet = TRUE),
                         "copies_report")), 1)
  expect_equal(nrow(attr(gtfs_deduplicate_copies(g, match = "ends",
                                                 quiet = TRUE),
                         "copies_report")), 0)
})

test_that("fixed track and frequency trips are left alone", {
  g <- copies_gtfs(
    copies_route("a", "AMID", "1", deps10, "S1", route_type = 1L),
    copies_route("b", "AMD", "2", deps10, "S1", route_type = 1L),
    calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  expect_equal(nrow(gtfs_deduplicate_copies(g, quiet = TRUE)$trips), 20)
  expect_equal(nrow(gtfs_deduplicate_copies(g, fixed_track = integer(0),
                                            quiet = TRUE)$trips), 10)
})

test_that("a feed with no copies comes back unchanged with an empty report", {
  g <- copies_gtfs(copies_route("a", "AMID", "1", deps10, "S1"),
                   calendar = cal_row("S1", "2026-10-04", "2027-01-02"))
  out <- gtfs_deduplicate_copies(g, quiet = TRUE)
  expect_identical(out$trips, g$trips)
  expect_equal(nrow(attr(out, "copies_report")), 0)
})
