context("count_weekday_runs survives an impossible calendar row")

# A calendar row whose end_date precedes its start_date describes no service.
# Operators publish them - NW_05_VISB_66_2.xml in the October 2026 TNDS
# archive has an OperatingPeriod of 2026-10-25 to 2026-10-24 - and
# transxchange2gtfs can synthesise one when it intersects an operating period
# with the days a service actually runs. Before the guard in
# count_weekday_runs(), such a row reached seq(length.out = negative) and
# killed the whole of gtfs_trips_per_zone() with "length.out must be a
# non-negative number", naming a purrr index and no service_id.

test_that("a calendar row ending before it starts counts no runs", {
  cal <- data.frame(
    service_id = c("good", "inverted", "oneday"),
    monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L,
    friday = 1L, saturday = 1L, sunday = 1L,
    start_date = as.Date(c("2026-07-27", "2026-10-03", "2026-10-03")),
    end_date   = as.Date(c("2026-08-09", "2026-10-01", "2026-10-03")),
    stringsAsFactors = FALSE)

  res <- count_weekday_runs(cal)
  expect_equal(nrow(res), 3L)
  runs <- c("runs_monday", "runs_tuesday", "runs_wednesday", "runs_thursday",
            "runs_friday", "runs_saturday", "runs_sunday")

  # the healthy row: a 14-day window holds two of each weekday
  expect_equal(as.numeric(res[res$service_id == "good", runs]), rep(2, 7))
  # the inverted row: no runs at all, and no error
  expect_equal(sum(as.numeric(res[res$service_id == "inverted", runs])), 0)
  # a single-day row still counts its one day (2026-10-03 is a Saturday)
  expect_equal(res$runs_saturday[res$service_id == "oneday"], 1)
  expect_equal(sum(as.numeric(res[res$service_id == "oneday", runs])), 1)
})

test_that("an NA end_date still counts no runs rather than failing", {
  cal <- data.frame(
    service_id = "nodate",
    monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L,
    friday = 1L, saturday = 1L, sunday = 1L,
    start_date = as.Date("2026-07-27"), end_date = as.Date(NA),
    stringsAsFactors = FALSE)
  res <- count_weekday_runs(cal)
  expect_equal(res$runs_weekdays, 0)
})