# Tests for gtfs_validate.R.
#
# test_gtfs_tools.R already covers the headline cases; this file works through
# the individual checks: the optional tables, every class of field value, the
# time and calendar logic, and gtfs_force_valid().
#
# Each test starts from valid_gtfs(), which must validate with no problems at
# all, so a seeded problem is the only thing the assertions can be picking up.

context("Validating a GTFS object check by check")

# A feed that uses every table gtfs_validate_internal knows about: a station
# with two platforms and an entrance, levels, pathways, shapes, frequencies,
# transfers, feed_info, timeframes, attributions, translations and both
# generations of fare tables.
valid_gtfs <- function() {
  list(
    agency = data.frame(
      agency_id = c("A1", "A2"), agency_name = c("One", "Two"),
      agency_url = "https://example.com",
      agency_timezone = "Europe/London", stringsAsFactors = FALSE),
    stops = data.frame(
      stop_id = c("ST1", "S1", "S2", "S3", "S4", "E1"),
      stop_name = c("Station", "p1", "p2", "s3", "s4", "entrance"),
      stop_lat = c(53.795, 53.795, 53.796, 53.900, 53.901, 53.794),
      stop_lon = c(-1.548, -1.548, -1.549, -1.600, -1.601, -1.547),
      location_type = c(1L, 0L, 0L, 0L, 0L, 2L),
      parent_station = c("", "ST1", "ST1", "", "", "ST1"),
      zone_id = c("Z1", "Z1", "Z1", "Z2", "Z3", "Z1"),
      level_id = c("L1", "L1", "L1", "", "", "L1"),
      stringsAsFactors = FALSE),
    routes = data.frame(
      route_id = c("R1", "R2"), agency_id = c("A1", "A2"),
      route_short_name = c("1", "2"), route_long_name = c("one", "two"),
      route_type = 3L, route_color = "FF0000", route_text_color = "ffffff",
      continuous_pickup = 1L, continuous_drop_off = 1L,
      stringsAsFactors = FALSE),
    trips = data.frame(
      route_id = c("R1", "R2"), service_id = c("SV1", "SV2"),
      trip_id = c("T1", "T2"), shape_id = c("SH1", "SH2"),
      direction_id = 0L, wheelchair_accessible = 1L, bikes_allowed = 1L,
      stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = c("T1", "T1", "T2", "T2"),
      arrival_time = c("10:00:00", "10:10:00", "11:00:00", "11:10:00"),
      departure_time = c("10:01:00", "10:11:00", "11:01:00", "11:11:00"),
      stop_id = c("S1", "S2", "S3", "S4"),
      stop_sequence = c(1L, 2L, 1L, 2L),
      pickup_type = 0L, drop_off_type = 0L, timepoint = 1L,
      shape_dist_traveled = c(0, 1200, 0, 1300),
      stringsAsFactors = FALSE),
    calendar = data.frame(
      service_id = c("SV1", "SV2"),
      monday = 1L, tuesday = 1L, wednesday = 1L, thursday = 1L,
      friday = 1L, saturday = 0L, sunday = 0L,
      start_date = as.Date("2024-01-01"), end_date = as.Date("2024-12-31"),
      stringsAsFactors = FALSE),
    calendar_dates = data.frame(
      service_id = "SV1", date = as.Date("2024-12-25"),
      exception_type = 2L, stringsAsFactors = FALSE),
    shapes = data.frame(
      shape_id = c("SH1", "SH1", "SH2", "SH2"),
      shape_pt_lat = c(53.795, 53.796, 53.900, 53.901),
      shape_pt_lon = c(-1.548, -1.549, -1.600, -1.601),
      shape_pt_sequence = c(1L, 2L, 1L, 2L),
      shape_dist_traveled = c(0, 1200, 0, 1300), stringsAsFactors = FALSE),
    frequencies = data.frame(
      trip_id = c("T1", "T2"), start_time = "07:00:00", end_time = "09:00:00",
      headway_secs = 600L, exact_times = 0L, stringsAsFactors = FALSE),
    transfers = data.frame(
      from_stop_id = "S1", to_stop_id = "S2", transfer_type = 2L,
      min_transfer_time = 120L, stringsAsFactors = FALSE),
    pathways = data.frame(
      pathway_id = "P1", from_stop_id = "E1", to_stop_id = "S1",
      pathway_mode = 1L, is_bidirectional = 1L, stringsAsFactors = FALSE),
    levels = data.frame(level_id = "L1", level_index = 0,
                        level_name = "Street", stringsAsFactors = FALSE),
    feed_info = data.frame(
      feed_publisher_name = "UK2GTFS", feed_publisher_url = "https://example.com",
      feed_lang = "en", feed_start_date = as.Date("2024-01-01"),
      feed_end_date = as.Date("2024-12-31"), stringsAsFactors = FALSE),
    timeframes = data.frame(
      timeframe_group_id = "TF1", service_id = "SV1",
      start_time = "00:00:00", end_time = "12:00:00",
      stringsAsFactors = FALSE),
    attributions = data.frame(
      organization_name = "Example", is_producer = 1L,
      stringsAsFactors = FALSE),
    translations = data.frame(
      table_name = "stops", field_name = "stop_name", language = "cy",
      translation = "Gorsaf", record_id = "ST1", stringsAsFactors = FALSE),
    fare_attributes = data.frame(
      fare_id = c("F1", "F2"), price = c(1.5, 2),
      currency_type = "GBP", payment_method = 1L,
      transfers = NA_integer_, agency_id = c("A1", "A2"),
      stringsAsFactors = FALSE),
    fare_rules = data.frame(
      fare_id = c("F1", "F2"), route_id = c("R1", "R2"),
      origin_id = c("Z1", "Z2"), destination_id = c("Z2", "Z3"),
      stringsAsFactors = FALSE),
    areas = data.frame(area_id = c("AR1", "AR2"),
                       area_name = c("a1", "a2"), stringsAsFactors = FALSE),
    stop_areas = data.frame(area_id = c("AR1", "AR2"),
                            stop_id = c("S1", "S3"), stringsAsFactors = FALSE),
    networks = data.frame(network_id = "net", network_name = "Network",
                          stringsAsFactors = FALSE),
    route_networks = data.frame(network_id = "net", route_id = c("R1", "R2"),
                                stringsAsFactors = FALSE),
    rider_categories = data.frame(
      rider_category_id = c("adult", "child"),
      rider_category_name = c("Adult", "Child"),
      is_default_fare_category = c(1L, 0L), stringsAsFactors = FALSE),
    fare_media = data.frame(fare_media_id = "ticket", fare_media_type = 1L,
                            stringsAsFactors = FALSE),
    fare_products = data.frame(
      fare_product_id = c("P1", "P2"),
      rider_category_id = c("adult", "child"), fare_media_id = "ticket",
      amount = c(1.5, 0.75), currency = "GBP", stringsAsFactors = FALSE),
    fare_leg_rules = data.frame(
      leg_group_id = c("L1", "L2"), network_id = "net",
      from_area_id = c("AR1", "AR2"), to_area_id = c("AR2", "AR1"),
      fare_product_id = c("P1", "P2"), stringsAsFactors = FALSE),
    fare_transfer_rules = data.frame(
      from_leg_group_id = "L1", to_leg_group_id = "L2",
      fare_transfer_type = 0L, fare_product_id = "P1",
      stringsAsFactors = FALSE)
  )
}

# the problems found for one table, as a single string to match against
msgs <- function(res, table) {
  paste(res$message[res$table == table], collapse = " | ")
}

validate <- function(gtfs) suppressMessages(gtfs_validate_internal(gtfs))


test_that("a feed using every optional table validates clean", {
  res <- validate(valid_gtfs())
  # if this fails the fixture is wrong, and every other test here is suspect
  expect_equal(nrow(res), 0, info = paste(res$table, res$message,
                                          collapse = "; "))
})


test_that("gtfs_validate_internal says so when asked and the feed is clean", {
  expect_message(gtfs_validate_internal(valid_gtfs(), good_news = TRUE),
                 "No problems found")
  # silent by default
  expect_silent(gtfs_validate_internal(valid_gtfs()))
})


test_that("the summary counts each severity", {
  gtfs <- valid_gtfs()
  gtfs$routes$route_type[1] <- 99L                 # Error
  gtfs$routes$route_short_name[1] <- ""             # Warning: R1 has no name
  gtfs$routes$route_long_name[1] <- ""              #   of either kind
  gtfs$stops$extra_column <- "x"                    # Note
  expect_message(gtfs_validate_internal(gtfs),
                 "Validation found 1 errors, 1 warnings and 1 notes")
})


test_that("missing tables, empty tables and missing columns are reported", {
  gtfs <- valid_gtfs()
  gtfs$agency <- NULL
  gtfs$stops <- gtfs$stops[0, ]
  gtfs$trips$service_id <- NULL

  res <- validate(gtfs)
  expect_match(msgs(res, "agency"), "required table is missing")
  expect_match(msgs(res, "stops"), "table has no rows")
  expect_match(msgs(res, "trips"), "required column\\(s\\) missing: service_id")
})


test_that("a feed with no calendar at all is an error", {
  gtfs <- valid_gtfs()
  gtfs$calendar <- NULL
  gtfs$calendar_dates <- NULL
  gtfs$timeframes <- NULL

  res <- validate(gtfs)
  expect_match(msgs(res, "calendar"),
               "either calendar or calendar_dates is required")

  # an empty calendar with no calendar_dates is a warning, not an error
  gtfs <- valid_gtfs()
  gtfs$calendar <- gtfs$calendar[0, ]
  gtfs$calendar_dates <- NULL
  res <- validate(gtfs)
  expect_match(msgs(res, "calendar"), "no rows and there are no calendar_dates")
})


test_that("non-standard columns and tables are notes, not errors", {
  gtfs <- valid_gtfs()
  gtfs$stops$my_own_column <- "x"
  gtfs$notes_for_me <- data.frame(a = 1)

  res <- validate(gtfs)
  expect_match(msgs(res, "stops"), "non-standard column\\(s\\): my_own_column")
  expect_match(msgs(res, "notes_for_me"), "not a standard GTFS table")
  expect_equal(unique(res$severity), "Note")
})


test_that("routes must be named and must say which agency runs them", {
  gtfs <- valid_gtfs()
  gtfs$routes$route_short_name <- NULL
  gtfs$routes$route_long_name <- NULL
  expect_match(msgs(validate(gtfs), "routes"),
               "at least one of route_short_name or route_long_name")

  gtfs <- valid_gtfs()
  gtfs$routes$route_short_name <- c("1", "")
  gtfs$routes$route_long_name <- c("one", NA)
  expect_match(msgs(validate(gtfs), "routes"),
               "1 route\\(s\\) have neither route_short_name")

  gtfs <- valid_gtfs()
  gtfs$routes$agency_id <- NULL
  expect_match(msgs(validate(gtfs), "routes"),
               "agency_id is required when there is more than one agency")
})


test_that("missing values in required columns are reported", {
  gtfs <- valid_gtfs()
  gtfs$stops$stop_name[2] <- NA
  gtfs$trips$trip_id[1] <- NA

  res <- validate(gtfs)
  expect_match(msgs(res, "stops"),
               "1 missing value\\(s\\) in required column stop_name")
  expect_match(msgs(res, "trips"),
               "1 missing value\\(s\\) in required column trip_id")
})


test_that("missing stop_times and empty transfer_type are allowed", {
  # untimed intermediate stops are legal, and an empty transfer_type means
  # "recommended transfer point"
  gtfs <- valid_gtfs()
  gtfs$stop_times <- rbind(
    gtfs$stop_times,
    data.frame(trip_id = "T1", arrival_time = NA_character_,
               departure_time = NA_character_, stop_id = "S3",
               stop_sequence = 3L, pickup_type = 0L, drop_off_type = 0L,
               timepoint = 0L, shape_dist_traveled = 2400,
               stringsAsFactors = FALSE))
  gtfs$stop_times$arrival_time[2] <- "10:10:00"
  gtfs$transfers$transfer_type <- NA_integer_
  gtfs$transfers$min_transfer_time <- NA_integer_

  res <- validate(gtfs)
  expect_false(grepl("arrival_time", msgs(res, "stop_times")))
  expect_false(grepl("transfer_type", msgs(res, "transfers")))
})


test_that("duplicated primary keys are reported for every keyed table", {
  gtfs <- valid_gtfs()
  gtfs$agency <- rbind(gtfs$agency, gtfs$agency[1, ])
  gtfs$stops <- rbind(gtfs$stops, gtfs$stops[2, ])
  gtfs$calendar_dates <- rbind(gtfs$calendar_dates, gtfs$calendar_dates[1, ])
  gtfs$stop_times <- rbind(gtfs$stop_times, gtfs$stop_times[1, ])
  gtfs$shapes <- rbind(gtfs$shapes, gtfs$shapes[1, ])
  gtfs$frequencies <- rbind(gtfs$frequencies, gtfs$frequencies[1, ])
  gtfs$areas <- rbind(gtfs$areas, gtfs$areas[1, ])
  gtfs$levels <- rbind(gtfs$levels, gtfs$levels[1, ])
  gtfs$pathways <- rbind(gtfs$pathways, gtfs$pathways[1, ])
  gtfs$fare_media <- rbind(gtfs$fare_media, gtfs$fare_media[1, ])

  res <- validate(gtfs)
  for (tb in c("agency", "stops", "calendar_dates", "stop_times", "shapes",
               "frequencies", "areas", "levels", "pathways", "fare_media")) {
    expect_match(msgs(res, tb), "duplicated", info = tb)
  }
  # composite keys are named in the message
  expect_match(msgs(res, "stop_times"), "trip_id\\+stop_sequence")
  expect_match(msgs(res, "calendar_dates"), "service_id\\+date")
})


test_that("broken foreign keys are reported for every relationship", {
  gtfs <- valid_gtfs()
  gtfs$routes$agency_id[1] <- "NOPE"
  gtfs$trips$shape_id[1] <- "NOSHAPE"
  gtfs$stops$parent_station[2] <- "NOSTATION"
  gtfs$stops$level_id[2] <- "NOLEVEL"
  gtfs$frequencies$trip_id[1] <- "NOTRIP"
  gtfs$transfers$from_stop_id <- "NOSTOP"
  gtfs$pathways$to_stop_id <- "NOSTOP"
  gtfs$fare_attributes$agency_id[1] <- "NOAGENCY"
  gtfs$fare_rules$fare_id[1] <- "NOFARE"
  gtfs$fare_rules$origin_id[1] <- "NOZONE"
  gtfs$stop_areas$area_id[1] <- "NOAREA"
  gtfs$route_networks$network_id[1] <- "NONET"
  gtfs$fare_leg_rules$from_area_id[1] <- "NOAREA"
  gtfs$fare_leg_rules$fare_product_id[1] <- "NOPRODUCT"
  gtfs$fare_products$rider_category_id[1] <- "NOCATEGORY"
  gtfs$fare_products$fare_media_id[1] <- "NOMEDIA"
  gtfs$fare_transfer_rules$to_leg_group_id <- "NOGROUP"
  gtfs$timeframes$service_id <- "NOSERVICE"

  res <- validate(gtfs)
  expect_match(msgs(res, "routes"), "agency_id value\\(s\\) not found")
  expect_match(msgs(res, "trips"), "shape_id value\\(s\\) not found")
  expect_match(msgs(res, "stops"), "parent_station value\\(s\\) not found")
  expect_match(msgs(res, "stops"), "level_id value\\(s\\) not found")
  expect_match(msgs(res, "frequencies"), "trip_id value\\(s\\) not found")
  expect_match(msgs(res, "transfers"), "from_stop_id value\\(s\\) not found")
  expect_match(msgs(res, "pathways"), "to_stop_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_attributes"), "agency_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_rules"), "fare_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_rules"), "origin_id value\\(s\\) not found")
  expect_match(msgs(res, "stop_areas"), "area_id value\\(s\\) not found")
  expect_match(msgs(res, "route_networks"), "network_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_leg_rules"), "from_area_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_leg_rules"),
               "fare_product_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_products"),
               "rider_category_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_products"),
               "fare_media_id value\\(s\\) not found")
  expect_match(msgs(res, "fare_transfer_rules"),
               "to_leg_group_id value\\(s\\) not found")
  expect_match(msgs(res, "timeframes"), "service_id value\\(s\\) not found")
})


test_that("optional foreign keys accept a blank value", {
  gtfs <- valid_gtfs()
  gtfs$trips$shape_id <- ""
  gtfs$stops$level_id <- ""
  gtfs$fare_rules$route_id <- NA_character_
  gtfs$fare_transfer_rules$from_leg_group_id <- ""

  res <- validate(gtfs)
  expect_false(grepl("shape_id", msgs(res, "trips")))
  expect_false(grepl("level_id", msgs(res, "stops")))
  expect_false(grepl("route_id", msgs(res, "fare_rules")))
})


test_that("invalid enumerated values are reported", {
  gtfs <- valid_gtfs()
  gtfs$stops$location_type[4] <- 9L
  gtfs$stops$wheelchair_boarding <- 7L
  gtfs$routes$continuous_pickup[1] <- 4L
  gtfs$routes$continuous_drop_off[1] <- 4L
  gtfs$trips$direction_id[1] <- 5L
  gtfs$trips$wheelchair_accessible[1] <- 5L
  gtfs$trips$bikes_allowed[1] <- 5L
  gtfs$stop_times$pickup_type[1] <- 9L
  gtfs$stop_times$drop_off_type[1] <- 9L
  gtfs$stop_times$timepoint[1] <- 9L
  gtfs$calendar_dates$exception_type <- 3L
  gtfs$transfers$transfer_type <- 9L
  gtfs$frequencies$exact_times[1] <- 9L
  gtfs$fare_attributes$payment_method[1] <- 9L
  gtfs$fare_attributes$transfers[1] <- 9L
  gtfs$fare_media$fare_media_type <- 9L

  res <- validate(gtfs)
  expect_match(msgs(res, "stops"), "invalid location_type value")
  expect_match(msgs(res, "stops"), "invalid wheelchair_boarding value")
  expect_match(msgs(res, "routes"), "invalid continuous_pickup value")
  expect_match(msgs(res, "routes"), "invalid continuous_drop_off value")
  expect_match(msgs(res, "trips"), "invalid direction_id value")
  expect_match(msgs(res, "trips"), "invalid wheelchair_accessible value")
  expect_match(msgs(res, "trips"), "invalid bikes_allowed value")
  expect_match(msgs(res, "stop_times"), "invalid pickup_type value")
  expect_match(msgs(res, "stop_times"), "invalid drop_off_type value")
  expect_match(msgs(res, "stop_times"), "invalid timepoint value")
  expect_match(msgs(res, "calendar_dates"), "invalid exception_type value")
  expect_match(msgs(res, "transfers"), "invalid transfer_type value")
  expect_match(msgs(res, "frequencies"), "invalid exact_times value")
  expect_match(msgs(res, "fare_attributes"), "invalid payment_method value")
  expect_match(msgs(res, "fare_attributes"), "invalid transfers value")
  expect_match(msgs(res, "fare_media"), "invalid fare_media_type value")
})


test_that("route_type accepts the extended types with a note", {
  gtfs <- valid_gtfs()
  gtfs$routes$route_type <- c(100L, 3L)        # 100 is "Railway Service"
  res <- validate(gtfs)
  expect_match(msgs(res, "routes"), "extended route types")
  expect_equal(res$severity[res$table == "routes"], "Note")

  gtfs$routes$route_type <- c(1800L, NA)       # above the extended range
  res <- validate(gtfs)
  expect_match(msgs(res, "routes"), "invalid route_type value")
  expect_match(msgs(res, "routes"), "missing route_type value")
})


test_that("colours and currencies must be in the documented format", {
  gtfs <- valid_gtfs()
  gtfs$routes$route_color <- c("#FF0000", "red")
  gtfs$routes$route_text_color <- c("ffff", "")
  gtfs$fare_attributes$currency_type <- c("GB", "pounds")
  gtfs$fare_products$currency <- "GBPX"

  res <- validate(gtfs)
  expect_match(msgs(res, "routes"), "2 invalid route_color")
  expect_match(msgs(res, "routes"), "1 invalid route_text_color")
  expect_match(msgs(res, "fare_attributes"), "invalid currency_type")
  expect_match(msgs(res, "fare_products"), "invalid currency")
})


test_that("coordinates must be present, numeric and in range", {
  gtfs <- valid_gtfs()
  gtfs$stops$stop_lat <- as.character(gtfs$stops$stop_lat)
  gtfs$stops$stop_lat[2] <- "not a number"
  gtfs$stops$stop_lon[3] <- 200
  gtfs$shapes$shape_pt_lat[1] <- 91

  res <- validate(gtfs)
  expect_match(msgs(res, "stops"), "missing or non-numeric stop_lat/stop_lon")
  expect_match(msgs(res, "stops"), "outside valid ranges")
  expect_match(msgs(res, "shapes"), "outside valid ranges")
})


test_that("the station hierarchy rules are checked", {
  gtfs <- valid_gtfs()
  gtfs$stops$parent_station[6] <- ""            # entrance with no parent
  expect_match(msgs(validate(gtfs), "stops"),
               "without a parent_station")

  gtfs <- valid_gtfs()
  gtfs$stops$parent_station[1] <- "S3"          # station with a parent
  expect_match(msgs(validate(gtfs), "stops"),
               "must not have a parent_station")

  gtfs <- valid_gtfs()
  gtfs$stops$parent_station[2] <- "S3"          # parent is a plain stop
  expect_match(msgs(validate(gtfs), "stops"),
               "parent_station is not a station")
})


test_that("agencies must agree on a timezone and have an agency_id", {
  gtfs <- valid_gtfs()
  gtfs$agency$agency_timezone[2] <- "Europe/Paris"
  expect_match(msgs(validate(gtfs), "agency"),
               "must share the same agency_timezone")

  gtfs <- valid_gtfs()
  gtfs$agency$agency_id[2] <- ""
  expect_match(msgs(validate(gtfs), "agency"), "1 blank agency_id value")
})


test_that("times must be HH:MM:SS and must not go backwards", {
  gtfs <- valid_gtfs()
  gtfs$stop_times$arrival_time[1] <- "10 am"
  gtfs$stop_times$departure_time[2] <- "10:05:00"   # before its arrival
  gtfs$stop_times$arrival_time[4] <- "10:00:00"     # before T2 stop 1 departs
  gtfs$stop_times$stop_sequence[3] <- -1L

  res <- validate(gtfs)
  expect_match(msgs(res, "stop_times"), "invalid arrival_time value")
  expect_match(msgs(res, "stop_times"), "departure_time is before arrival_time")
  expect_match(msgs(res, "stop_times"), "times go backwards along 1 trip")
  expect_match(msgs(res, "stop_times"), "negative stop_sequence")
})


test_that("trips need two timed stops and a non-decreasing distance", {
  gtfs <- valid_gtfs()
  # T2 keeps one call only, and T1's last call loses both its times
  gtfs$stop_times <- gtfs$stop_times[1:3, ]
  gtfs$stop_times$arrival_time[2] <- NA
  gtfs$stop_times$departure_time[2] <- NA
  gtfs$stop_times$shape_dist_traveled[2] <- -5

  res <- validate(gtfs)
  expect_match(msgs(res, "stop_times"), "fewer than 2 stop_times")
  expect_match(msgs(res, "stop_times"),
               "missing times at their first or last stop")
  expect_match(msgs(res, "stop_times"), "shape_dist_traveled decreases")
})


test_that("trips with no calls at all are reported", {
  gtfs <- valid_gtfs()
  gtfs$trips <- rbind(gtfs$trips,
                      data.frame(route_id = "R1", service_id = "SV1",
                                 trip_id = "T9", shape_id = "SH1",
                                 direction_id = 0L,
                                 wheelchair_accessible = 1L,
                                 bikes_allowed = 1L, stringsAsFactors = FALSE))
  expect_match(msgs(validate(gtfs), "trips"), "1 trip\\(s\\) with no stop_times")
})


test_that("shapes must not run backwards", {
  gtfs <- valid_gtfs()
  gtfs$shapes$shape_dist_traveled[2] <- -1
  expect_match(msgs(validate(gtfs), "shapes"),
               "shape_dist_traveled decreases along 1 shape")
})


test_that("calendar dates and operating days are checked", {
  gtfs <- valid_gtfs()
  gtfs$calendar$start_date <- as.Date(c("2024-06-01", "2024-01-01"))
  gtfs$calendar$end_date <- as.Date(c("2024-01-31", "2024-12-31"))
  expect_match(msgs(validate(gtfs), "calendar"), "start_date after end_date")

  # a service with no operating days and no added dates can never run
  gtfs <- valid_gtfs()
  for (d in c("monday", "tuesday", "wednesday", "thursday", "friday",
              "saturday", "sunday")) {
    gtfs$calendar[[d]] <- c(0L, 1L)
  }
  expect_match(msgs(validate(gtfs), "calendar"), "these never run")

  # unless calendar_dates adds a date for it
  gtfs$calendar_dates$exception_type <- 1L
  expect_false(grepl("never run", msgs(validate(gtfs), "calendar")))

  # dates must be YYYYMMDD
  gtfs <- valid_gtfs()
  gtfs$calendar$start_date <- c("2024-01-01", "20240101")
  gtfs$calendar_dates$date <- "not a date"
  res <- validate(gtfs)
  expect_match(msgs(res, "calendar"), "1 invalid start_date value")
  expect_match(msgs(res, "calendar_dates"), "invalid date value")

  # an NA day flag is an error, not something to skip
  gtfs <- valid_gtfs()
  gtfs$calendar$monday <- c(NA_integer_, 1L)
  expect_match(msgs(validate(gtfs), "calendar"), "invalid monday value")
})


test_that("integer and character dates are accepted", {
  gtfs <- valid_gtfs()
  gtfs$calendar$start_date <- 20240101L
  gtfs$calendar$end_date <- "20241231"
  gtfs$calendar_dates$date <- 20241225L
  gtfs$feed_info$feed_start_date <- "20240101"
  gtfs$feed_info$feed_end_date <- 20241231L

  expect_equal(nrow(validate(gtfs)), 0)
})


test_that("times given as difftimes or as seconds are understood", {
  secs <- c(36000, 36600, 39600, 40200)          # 10:00, 10:10, 11:00, 11:10

  gtfs <- valid_gtfs()
  gtfs$stop_times$arrival_time <- as.difftime(secs, units = "secs")
  gtfs$stop_times$departure_time <- as.difftime(secs + 60, units = "secs")
  expect_equal(nrow(validate(gtfs)), 0)

  gtfs$stop_times$arrival_time <- secs
  gtfs$stop_times$departure_time <- secs + 60
  expect_equal(nrow(validate(gtfs)), 0)

  # and the time logic still applies to them
  gtfs$stop_times$departure_time[2] <- 36000
  expect_match(msgs(validate(gtfs), "stop_times"),
               "departure_time is before arrival_time")
})


test_that("frequencies windows and headways are checked", {
  gtfs <- valid_gtfs()
  gtfs$frequencies$headway_secs <- c(0L, NA)
  gtfs$frequencies$end_time <- c("07:00:00", "06:00:00")

  res <- validate(gtfs)
  expect_match(msgs(res, "frequencies"),
               "2 row\\(s\\) with missing or non-positive headway_secs")
  expect_match(msgs(res, "frequencies"), "end_time is not after start_time")

  gtfs <- valid_gtfs()
  gtfs$frequencies$start_time <- "7am"
  expect_match(msgs(validate(gtfs), "frequencies"), "invalid start_time value")
})


test_that("negative transfer times, prices and amounts are reported", {
  gtfs <- valid_gtfs()
  gtfs$transfers$min_transfer_time <- -1L
  gtfs$fare_attributes$price[1] <- -1
  gtfs$fare_products$amount[1] <- NA

  res <- validate(gtfs)
  expect_match(msgs(res, "transfers"), "negative min_transfer_time")
  expect_match(msgs(res, "fare_attributes"),
               "missing or negative price")
  expect_match(msgs(res, "fare_products"), "missing or negative amount")
})


test_that("feed_info dates must be the right way round", {
  gtfs <- valid_gtfs()
  gtfs$feed_info$feed_start_date <- as.Date("2025-01-01")
  expect_match(msgs(validate(gtfs), "feed_info"),
               "feed_start_date is after feed_end_date")
})


test_that("exactly one rider category must be the default", {
  gtfs <- valid_gtfs()
  gtfs$rider_categories$is_default_fare_category <- 0L
  expect_match(msgs(validate(gtfs), "rider_categories"),
               "0 default rider categories")

  gtfs$rider_categories$is_default_fare_category <- 1L
  expect_match(msgs(validate(gtfs), "rider_categories"),
               "2 default rider categories")
})


test_that("rows nothing refers to are reported as notes", {
  gtfs <- valid_gtfs()
  gtfs$routes <- rbind(gtfs$routes, gtfs$routes[1, ])
  gtfs$routes$route_id[3] <- "R9"
  gtfs$stops <- rbind(gtfs$stops, gtfs$stops[4, ])
  gtfs$stops$stop_id[7] <- "S9"
  gtfs$stops$zone_id[7] <- "Z2"
  gtfs$stops$level_id[7] <- ""
  gtfs$calendar <- rbind(gtfs$calendar, gtfs$calendar[1, ])
  gtfs$calendar$service_id[3] <- "SV9"
  gtfs$route_networks <- rbind(gtfs$route_networks,
                               data.frame(network_id = "net", route_id = "R9",
                                          stringsAsFactors = FALSE))

  res <- validate(gtfs)
  expect_match(msgs(res, "routes"), "1 route\\(s\\) with no trips: R9")
  expect_match(msgs(res, "stops"), "1 stop\\(s\\) not used by any stop_times")
  expect_match(msgs(res, "calendar"),
               "1 service\\(s\\) not used by any trips: SV9")
  expect_true(all(res$severity == "Note"))
})


test_that("long lists of offending ids are truncated", {
  gtfs <- valid_gtfs()
  gtfs$stops <- rbind(gtfs$stops, do.call(rbind, lapply(1:12, function(i) {
    s <- gtfs$stops[4, ]
    s$stop_id <- paste0("U", i)
    s$level_id <- ""
    s
  })))
  res <- validate(gtfs)
  expect_match(msgs(res, "stops"), "and 2 more")
})


# ---- gtfs_force_valid -------------------------------------------------------

test_that("gtfs_force_valid removes every dangling row", {
  gtfs <- valid_gtfs()
  gtfs$stops$stop_lat[5] <- NA               # S4 loses its location
  gtfs$routes$agency_id[2] <- "NOPE"         # R2 has no agency
  gtfs$trips <- rbind(gtfs$trips,
                      data.frame(route_id = "NOROUTE", service_id = "SV1",
                                 trip_id = "T8", shape_id = "SH1",
                                 direction_id = 0L,
                                 wheelchair_accessible = 1L,
                                 bikes_allowed = 1L, stringsAsFactors = FALSE))
  gtfs$stop_times <- rbind(
    gtfs$stop_times,
    data.frame(trip_id = "T7", arrival_time = "12:00:00",
               departure_time = "12:00:00", stop_id = "S1",
               stop_sequence = 1L, pickup_type = 0L, drop_off_type = 0L,
               timepoint = 1L, shape_dist_traveled = 0,
               stringsAsFactors = FALSE))
  gtfs$calendar <- rbind(gtfs$calendar, gtfs$calendar[1, ])
  gtfs$calendar$service_id[3] <- "SV9"
  gtfs$calendar_dates <- rbind(gtfs$calendar_dates, gtfs$calendar_dates[1, ])
  gtfs$calendar_dates$service_id[2] <- "SV9"

  expect_message(gtfs_force_valid(gtfs), "does not fix problems")
  out <- suppressMessages(gtfs_force_valid(gtfs))

  expect_false("S4" %in% out$stops$stop_id)        # no location
  expect_equal(out$routes$route_id, "R1")          # R2's agency is unknown
  expect_equal(out$trips$trip_id, "T1")            # T2 lost its route, T8 too
  expect_equal(unique(out$stop_times$trip_id), "T1")
  expect_equal(out$calendar$service_id, "SV1")
  expect_equal(out$calendar_dates$service_id, "SV1")

  # and the optional tables no longer point at anything that has gone
  expect_true(all(out$shapes$shape_id %in% out$trips$shape_id))
  expect_true(all(out$frequencies$trip_id %in% out$trips$trip_id))
  expect_true(all(out$stop_areas$stop_id %in% out$stops$stop_id))
  expect_true(all(out$fare_rules$fare_id %in% out$fare_attributes$fare_id))

  # a forced feed has no referential errors left
  res <- validate(out)
  expect_false(any(grepl("not found in", res$message)))
})
