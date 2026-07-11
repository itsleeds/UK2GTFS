context("Rail fares (RSPS5045) to GTFS")

# Build a tiny synthetic fares feed, exercising the fixed-width layouts:
# three stations (AST, BST, CST), a group station 2000 = {BST, CST}, a
# cluster Q001 = {station 1111/AST}, one reversible flow Q001 -> 2000 with
# an Anytime Day Single (SDS) and Return (SDR), one non-derivable fare
# AST -> BST, child fares at 50% and a "YNG" railcard at 34%.

pad <- function(x, n) formatC(x, width = n, flag = "-")

fares_fixture <- function() {
  dir <- file.path(tempdir(), "railfares_fixture")
  unlink(dir, recursive = TRUE)
  dir.create(dir)
  hdr <- "/!! Start of file"
  wf <- function(ext, lines) {
    writeLines(c(hdr, lines), file.path(dir, paste0("RJFAF999.", ext)))
  }

  # FFL: flow record (49 chars) + fare records (22 chars)
  flow <- function(orig, dest, dir_, id) {
    paste0("RF", orig, dest, "00000", "000", "A", dir_,
           "31122999", "01012020", "XX ", "0", "0", "Y", id)
  }
  fare <- function(id, tkt, pence, restr = "  ") {
    paste0("RT", id, tkt, formatC(pence, width = 8, flag = "0"), restr)
  }
  wf("FFL", c(
    flow("Q001", "2000", "R", "0000001"),
    # an expired flow that must be filtered out
    paste0("RF", "1111", "1113", "00000", "000", "A", "S",
           "31122010", "01012010", "XX ", "0", "0", "Y", "0000002"),
    fare("0000001", "SDS", 1000),
    fare("0000001", "SDR", 2000, "XD"),
    fare("0000001", "OPS", 700, "OP"),
    fare("0000001", "ADV", 300),
    fare("0000002", "SDS", 9999)
  ))

  # FSC: cluster Q001 contains NLC 1111
  wf("FSC", paste0("R", "Q001", "1111", "31122999", "01012020"))

  # LOC: L records need to reach col 75 (fare_group)
  locL <- function(nlc, desc, crs) {
    paste0("RL", "70", nlc, "0", "31122999", "01012020", "01012020",
           "70 ", nlc, pad(desc, 16), pad(crs, 3), "     ", "  ", "   ",
           pad(nlc, 6))
  }
  wf("LOC", c(
    locL("1111", "ALPHA", "AST"),
    locL("1112", "BRAVO", "BST"),
    locL("1113", "CHARLIE", "CST"),
    paste0("RG", "7020000", "31122999", "01012020", "01012020",
           pad("GROUP STN", 16)),
    paste0("RM", "7020000", "31122999", "7011120", "BST"),
    paste0("RM", "7020000", "31122999", "7011130", "CST")
  ))

  # TTY: 120-char ticket type records
  tty <- function(code, desc, class, type, cat) {
    paste0("R", code, "31122999", "01012020", "01012020", pad(desc, 15),
           class, type, "S", "31122999", "001001001000001000", "NNN",
           "  ", pad("", 20), "0", "N", "   ", "N", "  ", "0", " ", "N",
           "001", cat, "      ")
  }
  wf("TTY", c(
    tty("SDS", "ANYTIME DAY S", "2", "S", "01"),
    tty("SDR", "ANYTIME DAY R", "2", "R", "01"),
    tty("OPS", "OFF-PEAK S", "2", "S", "01"),
    tty("ADV", "SALE ADVANCE", "2", "S", "01"),
    tty("FOS", "FIRST ANYTIME", "1", "S", "01"),
    tty("7DS", "WEEKLY SEASON", "2", "N", "01")
  ))

  # NFO: non-derivable override AST -> BST (directional)
  wf("NFO", paste0("R", "1111", "1112", "00000", "   ", "SDS", "O",
                   "31122999", "01012020", "01012020", "N",
                   "00000500", "00000250", "  ", "Y", "N", "N"))

  # DIS: status records (95+ chars) and discount records
  dis_s <- function(status) {
    paste0("S", status, "31122999", "01012020", pad("", 11),
           strrep("00000000", 8), "YYYY")
  }
  dis_d <- function(status, pct) {
    paste0("D", status, "31122999", "01", "0", pct)
  }
  wf("DIS", c(
    dis_s("000"), dis_s("001"), dis_s("015"),
    dis_d("000", "000"), dis_d("001", "500"), dis_d("015", "340")
  ))

  # RLC: 127-char railcard records (no update marker)
  rlc <- function(code, desc, adult, child) {
    paste0(pad(code, 3), "31122999", "01012020", "01012020", "A",
           pad(desc, 20), "NNNN", pad(code, 3), "Y",
           strrep("000", 10), "00000000", "00000000", "    ", "31122999",
           "Y", "   ", adult, child, "002")
  }
  wf("RLC", c(
    rlc("", "PUBLIC", "000", "001"),
    rlc("YNG", "16-25", "015", "016")
  ))

  # RTE: route record
  wf("RTE", paste0("RR", "00000", "31122999", "01012020", "01012020",
                   pad("ANY PERMITTED", 16)))

  # RST: restriction "OP" = off-peak, not valid departing 04:30-09:29
  # Mon-Fri (time restriction + all-year date band), plus an all-day ban on
  # 25 Dec; restriction "XD" = not valid at all on 1 May.
  wf("RST", c(
    paste0("RRD", "C", "01012020", "31122999"),
    paste0("RRH", "C", "OP", pad("OFF-PEAK RESTRICTION", 30),
           pad("", 100), "N", "N", "Y"),
    paste0("RTR", "C", "OP", "0001", "O", "0430", "0929", "D", "   ",
           "T", " ", "N"),
    paste0("RTD", "C", "OP", "0001", "O", "0101", "1231", "YYYYYNN"),
    paste0("RTR", "C", "OP", "0002", "O", "0000", "2359", "D", "   ",
           "T", " ", "N"),
    paste0("RTD", "C", "OP", "0002", "O", "1225", "1225", "YYYYYYY"),
    paste0("RRH", "C", "XD", pad("DATE BAN", 30), pad("", 100), "N", "N", "Y"),
    paste0("RTR", "C", "XD", "0001", "O", "0000", "2359", "D", "   ",
           "T", " ", "N"),
    paste0("RTD", "C", "XD", "0001", "O", "0501", "0501", "YYYYYYY")
  ))

  # TAP: ADV must be booked at least 7 days before travel
  wf("TAP", paste0("ADV", "  ", "2", "  ", "31122999", "01012020",
                   "2", "00000007", "    "))

  # TOC
  wf("TOC", c("TXX Test Trains", paste0("FXX ", "XX", "Test Trains")))

  dir
}

gtfs_fixture <- function() {
  list(
    stops = data.frame(
      stop_id = c("ASTON", "BSTON", "CSTON", "JUNC"),
      stop_code = c("AST", "BST", "CST", NA),
      stop_name = c("Alpha", "Bravo", "Charlie", "A Junction"),
      stringsAsFactors = FALSE
    ),
    routes = data.frame(
      route_id = c("1", "2"),
      route_type = c(2L, 3L),
      stringsAsFactors = FALSE
    ),
    calendar = data.frame(
      service_id = "s1",
      start_date = as.Date("2026-01-01"),
      end_date = as.Date("2026-12-31"),
      stringsAsFactors = FALSE
    )
  )
}


test_that("atoc_fares_read parses the fixture correctly", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-01-01"))

  expect_equal(nrow(fares$flow), 1) # expired flow dropped
  expect_equal(fares$flow$origin, "Q001")
  expect_equal(fares$flow$direction, "R")
  expect_equal(nrow(fares$fare), 4) # fares of expired flow dropped
  expect_equal(sort(fares$fare$ticket_code), c("ADV", "OPS", "SDR", "SDS"))
  expect_equal(fares$fare[ticket_code == "SDS"]$fare, 1000L)

  expect_equal(fares$cluster$cluster_id, "Q001")
  expect_equal(fares$cluster$cluster_nlc, "1111")

  expect_equal(nrow(fares$location), 3)
  expect_equal(fares$location[nlc == "1111"]$crs, "AST")
  expect_equal(fares$group$group_nlc, "2000")
  expect_equal(sort(fares$group_member$member_crs), c("BST", "CST"))

  expect_equal(nrow(fares$ticket_type), 6)

  # restriction and advance-purchase tables
  expect_equal(nrow(fares$time_restriction), 3)
  expect_equal(nrow(fares$time_restriction_date_band), 3)
  expect_equal(fares$restriction_dates$cf_mkr, "C")
  expect_equal(nrow(fares$advance), 1)
  expect_equal(fares$advance$check_type, "2")
  expect_equal(fares$ticket_type[ticket_code == "SDS"]$ticket_class, "2")
  expect_equal(fares$ticket_type[ticket_code == "SDR"]$ticket_type, "R")

  expect_equal(nrow(fares$ndf), 1)
  expect_equal(fares$ndf$adult_fare, 500L)
  expect_equal(fares$ndf$child_fare, 250L)

  expect_equal(fares$railcard[railcard_code == ""]$child_status, "001")
  expect_equal(fares$railcard[railcard_code == "YNG"]$adult_status, "015")
  expect_equal(
    fares$status_discount[status_code == "001"]$discount_percentage, 500L)
})


test_that("gtfs_add_railfares v1 builds zones and cheapest fare rules", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-01-01"))
  gtfs <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 1)

  # zone_id = CRS, falling back to stop_id
  expect_equal(gtfs$stops$zone_id, c("AST", "BST", "CST", "JUNC"))

  # flow Q001 (cluster -> AST) to 2000 (group -> BST, CST), reversible:
  # 4 station pairs, plus the NDF fare AST -> BST overriding nothing new
  fr <- gtfs$fare_rules
  expect_true(all(c("AST_BST", "AST_CST", "BST_AST", "CST_AST") %in%
                    fr$fare_id))
  fa <- gtfs$fare_attributes
  expect_equal(nrow(fa), nrow(fr))
  # default ticket_type is "single" for v1, so the cheapest single applies
  # (OPS 7.00), but the NDF fare (5.00) undercuts it for AST -> BST
  expect_equal(fa$price[fa$fare_id == "AST_BST"], 5)
  expect_equal(fa$price[fa$fare_id == "BST_AST"], 7)
  expect_equal(fa$price[fa$fare_id == "AST_CST"], 7)
  expect_equal(unique(fa$currency_type), "GBP")

  # railcards are ignored (with a warning) in v1
  expect_warning(
    gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 1,
                       railcards = "YNG"),
    "railcards"
  )
})


test_that("gtfs_add_railfares v2 builds areas, products and leg rules", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-01-01"))
  gtfs <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 2,
                             railcards = "YNG")

  # areas: cluster, group and the NDF stations
  expect_true(all(c("rail_Q001", "rail_2000", "rail_1111", "rail_1112") %in%
                    gtfs$areas$area_id))
  expect_equal(gtfs$areas$area_name[gtfs$areas$area_id == "rail_2000"],
               "GROUP STN")
  sa <- gtfs$stop_areas
  expect_equal(sort(sa$stop_id[sa$area_id == "rail_2000"]),
               c("BSTON", "CSTON"))
  expect_equal(sa$stop_id[sa$area_id == "rail_Q001"], "ASTON")

  # network contains only rail routes
  expect_equal(gtfs$route_networks$route_id, "1")

  # rider categories: adult, child and the railcard
  expect_equal(sort(gtfs$rider_categories$rider_category_id),
               c("adult", "child", "railcard_yng"))

  fp <- gtfs$fare_products
  # adult products: SDS 10.00 + SDR 20.00 + OPS 7.00 (flow) + SDS 5.00 (NDF)
  adult <- fp[fp$rider_category_id == "adult", ]
  expect_equal(sort(adult$amount), c(5, 7, 10, 20))
  # child = 50%: 2.50 (NDF explicit), 3.50, 5.00, 10.00
  child <- fp[fp$rider_category_id == "child", ]
  expect_equal(sort(child$amount), c(2.5, 3.5, 5, 10))
  # YNG railcard = 34% off adult: 3.30, 4.62, 6.60, 13.20
  yng <- fp[fp$rider_category_id == "railcard_yng", ]
  expect_equal(sort(yng$amount), c(3.3, 4.62, 6.6, 13.2))

  # leg rules: reversible flow appears in both directions
  lr <- gtfs$fare_leg_rules
  expect_true(any(lr$from_area_id == "rail_Q001" &
                    lr$to_area_id == "rail_2000"))
  expect_true(any(lr$from_area_id == "rail_2000" &
                    lr$to_area_id == "rail_Q001"))
  # NDF fare is directional: 1111 -> 1112 only
  expect_true(any(lr$from_area_id == "rail_1111" &
                    lr$to_area_id == "rail_1112"))
  expect_false(any(lr$from_area_id == "rail_1112" &
                     lr$to_area_id == "rail_1111"))
  # all leg rule references resolve
  expect_true(all(lr$fare_product_id %in% fp$fare_product_id))
  expect_true(all(c(lr$from_area_id, lr$to_area_id) %in% gtfs$areas$area_id))
})


test_that("walkup_only excludes trade/advance tickets by default", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-01-01"))

  # default: the GBP 3.00 SALE ADVANCE fare is excluded, cheapest single is
  # the 7.00 off-peak
  g <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 1)
  expect_equal(g$fare_attributes$price[g$fare_attributes$fare_id == "AST_CST"],
               7)

  # walkup_only = FALSE lets it through
  g <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 1,
                          walkup_only = FALSE)
  expect_equal(g$fare_attributes$price[g$fare_attributes$fare_id == "AST_CST"],
               3)
})


test_that("travel date/time scenarios apply the restriction data", {
  fx <- fares_fixture()
  gtfs <- gtfs_fixture()
  prod_codes <- function(g) {
    sort(unique(sub("_.*", "", toupper(g$fare_products$fare_product_id))))
  }
  scenario <- function(date, ...) {
    fares <- atoc_fares_read(fx, date = as.Date(date))
    gtfs_add_railfares(gtfs, fares, fares_version = 2,
                       rider_categories = "adult",
                       travel_date = as.Date(date), ...)
  }

  # Monday 08:00: OPS is inside its 04:30-09:29 weekday restriction
  g <- scenario("2026-08-03", travel_time = "08:00")
  expect_equal(prod_codes(g), c("SDR", "SDS"))

  # Monday 11:00: OPS is valid again
  g <- scenario("2026-08-03", travel_time = "11:00")
  expect_equal(prod_codes(g), c("OPS", "SDR", "SDS"))

  # Saturday 08:00: the weekday restriction does not apply
  g <- scenario("2026-08-08", travel_time = "08:00")
  expect_equal(prod_codes(g), c("OPS", "SDR", "SDS"))

  # 25 Dec: all-day ban on OPS applies even without a departure time
  g <- scenario("2026-12-25")
  expect_equal(prod_codes(g), c("SDR", "SDS"))

  # 1 May: SDR carries the XD date ban
  g <- scenario("2026-05-01")
  expect_equal(prod_codes(g), c("OPS", "SDS"))

  # v1: at Monday 08:00 the cheapest single is SDS (OPS invalid);
  # NDF still undercuts on AST -> BST
  fares <- atoc_fares_read(fx, date = as.Date("2026-08-03"))
  g <- gtfs_add_railfares(gtfs, fares, fares_version = 1,
                          travel_date = as.Date("2026-08-03"),
                          travel_time = "08:00")
  expect_equal(g$fare_attributes$price[g$fare_attributes$fare_id == "AST_CST"],
               10)
})


test_that("booking_date includes Advance tickets inside their horizon", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-08-03"))
  gtfs <- gtfs_fixture()

  # booked a month ahead: the 3.00 ADV tier is bookable (needs 7+ days)
  g <- gtfs_add_railfares(gtfs, fares, fares_version = 1,
                          travel_date = as.Date("2026-08-03"),
                          booking_date = as.Date("2026-07-01"))
  expect_equal(g$fare_attributes$price[g$fare_attributes$fare_id == "AST_CST"],
               3)

  # booked 2 days ahead: outside the horizon, walk-up fares only
  g <- gtfs_add_railfares(gtfs, fares, fares_version = 1,
                          travel_date = as.Date("2026-08-03"),
                          booking_date = as.Date("2026-08-01"))
  expect_equal(g$fare_attributes$price[g$fare_attributes$fare_id == "AST_CST"],
               7)
})


test_that("impossible scenarios are rejected", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-08-03"))
  gtfs <- gtfs_fixture()

  # basic argument validation
  expect_error(gtfs_add_railfares(gtfs, fares, travel_time = "08:00"),
               "travel_date")
  expect_error(gtfs_add_railfares(gtfs, fares,
                                  travel_date = as.Date("2026-08-03"),
                                  travel_time = "8am"),
               "HH:MM")
  expect_error(gtfs_add_railfares(gtfs, fares,
                                  travel_date = as.Date("2026-08-03"),
                                  booking_date = as.Date("2026-09-01")),
               "after")

  # booking absurdly far ahead of travel
  expect_error(gtfs_add_railfares(gtfs, fares,
                                  travel_date = as.Date("2026-08-03"),
                                  booking_date = as.Date("2021-08-03")),
               "12 weeks")
  # booking further ahead than bookings open: warn but proceed
  expect_warning(gtfs_add_railfares(gtfs, fares,
                                    travel_date = as.Date("2026-08-03"),
                                    booking_date = as.Date("2026-04-01")),
                 "12 weeks")

  # travelling outside the timetable coverage (fixture calendar is 2026)
  expect_error(gtfs_add_railfares(gtfs, fares,
                                  travel_date = as.Date("2030-06-01")),
               "timetable")

  # fares list read for a different date than the travel date: warn
  expect_warning(gtfs_add_railfares(gtfs, fares,
                                    travel_date = as.Date("2026-05-01")),
                 "valid on")

  # reading the feed for a date it does not cover fails with the coverage
  expect_error(atoc_fares_read(fares_fixture(), date = as.Date("2005-01-01")),
               "covers")
  # a date with valid records but outside the feed's fares rounds warns
  expect_warning(atoc_fares_read(fares_fixture(), date = as.Date("2010-06-01")),
                 "fares rounds")

  # atoc2gtfs validates the scenario before doing any work
  tmp <- tempfile(fileext = ".zip")
  file.create(tmp)
  expect_error(atoc2gtfs(tmp, fares_travel_time = "08:00"),
               "travel_date")
  unlink(tmp)
})


test_that("ticket selection arguments work", {
  fares <- atoc_fares_read(fares_fixture(), date = as.Date("2026-01-01"))

  # explicit codes: only the return
  gtfs <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 2,
                             ticket_codes = "SDR",
                             rider_categories = "adult")
  expect_equal(unique(gtfs$fare_products$amount), 20)

  # unknown ticket code warns
  expect_warning(
    gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 2,
                       ticket_codes = c("SDR", "ZZZ")),
    "ZZZ"
  )

  # nothing selected warns and returns gtfs unchanged
  expect_warning(
    out <- gtfs_add_railfares(gtfs_fixture(), fares, fares_version = 2,
                              ticket_class = "first",
                              ticket_type = "season"),
    "No"
  )
  expect_null(out$fare_products)
})
