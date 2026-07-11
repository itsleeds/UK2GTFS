context("NeTEx fares (BODS fxc profile) to GTFS")

# Build two tiny synthetic NeTEx fare files in the DfT fxc profile shape:
# a zonal Adult Single (two zones, two price bands) and a flat-fare Child
# Single, both for operator "TEST" line "42", plus a matching minimal GTFS.

netex_fixture_dir <- file.path(tempdir(), "netex_fixture")

netex_zonal_xml <- function() {
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<PublicationDelivery xmlns="http://www.netex.org.uk/netex" version="1.1">',
    '<dataObjects><CompositeFrame id="cf1">',
    '<FrameDefaults><DefaultCurrency>GBP</DefaultCurrency></FrameDefaults>',
    '<frames>',
    '<ResourceFrame id="rf1"><organisations>',
    '<Operator id="noc:TEST"><Name>Test Buses</Name></Operator>',
    '</organisations></ResourceFrame>',
    '<ServiceFrame id="sf1">',
    '<lines><Line id="TEST:PF0000001:1:42">',
    '<Name>Line 42 Outbound</Name>',
    '<PublicCode>42</PublicCode><PrivateCode>PF0000001:1</PrivateCode>',
    '</Line></lines>',
    '<scheduledStopPoints>',
    '<ScheduledStopPoint id="atco:S1"><Name>Stop 1</Name></ScheduledStopPoint>',
    '<ScheduledStopPoint id="atco:S2"><Name>Stop 2</Name></ScheduledStopPoint>',
    '<ScheduledStopPoint id="atco:S3"><Name>Stop 3</Name></ScheduledStopPoint>',
    '</scheduledStopPoints>',
    '</ServiceFrame>',
    '<FareFrame id="ff1">',
    '<fareZones>',
    '<FareZone id="fs@001"><Name>Zone A</Name><members>',
    '<ScheduledStopPointRef ref="atco:S1"/></members></FareZone>',
    '<FareZone id="fs@002"><Name>Zone B</Name><members>',
    '<ScheduledStopPointRef ref="atco:S2"/>',
    '<ScheduledStopPointRef ref="atco:S3"/></members></FareZone>',
    '</fareZones>',
    '<tariffs><Tariff id="t1">',
    '<validityConditions><ValidBetween>',
    '<FromDate>2026-01-01T00:00:00</FromDate>',
    '<ToDate>2026-12-31T00:00:00</ToDate>',
    '</ValidBetween></validityConditions>',
    '<fareStructureElements>',
    '<FareStructureElement id="fse1">',
    '<TypeOfFareStructureElementRef ref="fxc:access"/>',
    '<distanceMatrixElements>',
    '<DistanceMatrixElement id="d1">',
    '<priceGroups><PriceGroupRef ref="price_band_1"/></priceGroups>',
    '<StartTariffZoneRef ref="fs@001"/><EndTariffZoneRef ref="fs@002"/>',
    '</DistanceMatrixElement>',
    '<DistanceMatrixElement id="d2">',
    '<priceGroups><PriceGroupRef ref="price_band_2"/></priceGroups>',
    '<StartTariffZoneRef ref="fs@002"/><EndTariffZoneRef ref="fs@002"/>',
    '</DistanceMatrixElement>',
    '</distanceMatrixElements>',
    '</FareStructureElement>',
    '<FareStructureElement id="fse2">',
    '<TypeOfFareStructureElementRef ref="fxc:eligibility"/>',
    '<GenericParameterAssignment id="g1"><limitations>',
    '<UserProfile id="u1"><Name>Adult</Name><UserType>adult</UserType></UserProfile>',
    '</limitations></GenericParameterAssignment>',
    '</FareStructureElement>',
    '<FareStructureElement id="fse3">',
    '<TypeOfFareStructureElementRef ref="fxc:travel_conditions"/>',
    '<GenericParameterAssignment id="g2"><limitations>',
    '<RoundTrip id="rt1"><TripType>single</TripType></RoundTrip>',
    '</limitations></GenericParameterAssignment>',
    '</FareStructureElement>',
    '</fareStructureElements>',
    '</Tariff></tariffs>',
    '<fareProducts><PreassignedFareProduct id="p1">',
    '<Name>Adult Single</Name><ProductType>singleTrip</ProductType>',
    '</PreassignedFareProduct></fareProducts>',
    '<priceGroups>',
    '<PriceGroup id="price_band_1"><members>',
    '<GeographicalIntervalPrice id="gp1"><Amount>2.50</Amount></GeographicalIntervalPrice>',
    '</members></PriceGroup>',
    '<PriceGroup id="price_band_2"><members>',
    '<GeographicalIntervalPrice id="gp2"><Amount>1.80</Amount></GeographicalIntervalPrice>',
    '</members></PriceGroup>',
    '</priceGroups>',
    '</FareFrame>',
    '</frames></CompositeFrame></dataObjects></PublicationDelivery>'
  )
}

netex_flat_xml <- function() {
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<PublicationDelivery xmlns="http://www.netex.org.uk/netex" version="1.1">',
    '<dataObjects><CompositeFrame id="cf1">',
    '<FrameDefaults><DefaultCurrency>GBP</DefaultCurrency></FrameDefaults>',
    '<frames>',
    '<ResourceFrame id="rf1"><organisations>',
    '<Operator id="noc:TEST"><Name>Test Buses</Name></Operator>',
    '</organisations></ResourceFrame>',
    '<ServiceFrame id="sf1">',
    '<lines><Line id="TEST:PF0000001:1:42">',
    '<Name>Line 42 Inbound</Name>',
    '<PublicCode>42</PublicCode><PrivateCode>PF0000001:1</PrivateCode>',
    '</Line></lines>',
    '</ServiceFrame>',
    '<FareFrame id="ff1">',
    '<tariffs><Tariff id="t1">',
    '<fareStructureElements>',
    '<FareStructureElement id="fse2">',
    '<TypeOfFareStructureElementRef ref="fxc:eligibility"/>',
    '<GenericParameterAssignment id="g1"><limitations>',
    '<UserProfile id="u1"><Name>Child</Name><UserType>child</UserType></UserProfile>',
    '</limitations></GenericParameterAssignment>',
    '</FareStructureElement>',
    '<FareStructureElement id="fse3">',
    '<TypeOfFareStructureElementRef ref="fxc:travel_conditions"/>',
    '<GenericParameterAssignment id="g2"><limitations>',
    '<RoundTrip id="rt1"><TripType>single</TripType></RoundTrip>',
    '</limitations></GenericParameterAssignment>',
    '</FareStructureElement>',
    '</fareStructureElements>',
    '</Tariff></tariffs>',
    '<fareProducts><PreassignedFareProduct id="p1">',
    '<Name>Child Single</Name><ProductType>singleTrip</ProductType>',
    '</PreassignedFareProduct></fareProducts>',
    '<fareTables><FareTable id="ft1"><cells><Cell id="c1">',
    '<TimeIntervalPrice id="tp1"><Amount>1.20</Amount></TimeIntervalPrice>',
    '</Cell></cells></FareTable></fareTables>',
    '</FareFrame>',
    '</frames></CompositeFrame></dataObjects></PublicationDelivery>'
  )
}

netex_fixture <- function() {
  unlink(netex_fixture_dir, recursive = TRUE)
  dir.create(netex_fixture_dir, showWarnings = FALSE)
  writeLines(netex_zonal_xml(), file.path(netex_fixture_dir, "adult_single.xml"))
  writeLines(netex_flat_xml(), file.path(netex_fixture_dir, "child_single.xml"))
  file.path(netex_fixture_dir, c("adult_single.xml", "child_single.xml"))
}

gtfs_fixture <- function() {
  list(
    agency = data.table::data.table(
      agency_id = "TEST", agency_name = "Test Buses",
      agency_url = "https://example.com", agency_timezone = "Europe/London"),
    stops = data.table::data.table(
      stop_id = c("S1", "S2", "S3", "S9"),
      stop_name = paste("Stop", c(1, 2, 3, 9))),
    routes = data.table::data.table(
      route_id = "PF0000001", agency_id = "TEST",
      route_short_name = "42", route_long_name = "A - B", route_type = 3L)
  )
}

test_that("netex_read_fares parses a zonal fxc file", {
  paths <- netex_fixture()
  nx <- netex_read_fares(paths[1])

  expect_equal(nx$meta$operator_noc, "TEST")
  expect_equal(nx$meta$line_public_code, "42")
  expect_equal(nx$meta$direction, "outbound")
  expect_equal(nx$meta$trip_type, "single")
  expect_equal(nx$meta$user_type, "adult")
  expect_equal(nx$meta$product_name, "Adult Single")
  expect_equal(nx$meta$currency, "GBP")
  expect_equal(nx$meta$fare_kind, "zonal")

  # atco: prefix stripped from stop ids
  expect_setequal(nx$stops$stop_id, c("S1", "S2", "S3"))
  # two zones, three member stops
  expect_equal(nrow(nx$zones), 3)
  expect_setequal(unique(nx$zones$zone_id), c("fs@001", "fs@002"))
  # fare triangle joined to price bands
  expect_equal(nrow(nx$fares), 2)
  expect_setequal(nx$fares$amount, c(2.5, 1.8))
})

test_that("netex_read_fares detects flat fares", {
  paths <- netex_fixture()
  nx <- netex_read_fares(paths[2])

  expect_equal(nx$meta$fare_kind, "flat")
  expect_equal(nx$meta$user_type, "child")
  expect_equal(nrow(nx$fares), 1)
  expect_equal(nx$fares$amount, 1.2)
  expect_true(is.na(nx$fares$from_zone))
  # no zones declared, but the table still has the expected columns
  expect_true(all(c("zone_id", "zone_name", "stop_id") %in% names(nx$zones)))
  expect_equal(nrow(nx$zones), 0)
})

test_that("netex_read_fares_multiple separates parse failures", {
  paths <- netex_fixture()
  bad <- file.path(netex_fixture_dir, "broken.xml")
  writeLines("this is not xml <", bad)

  expect_message(
    nx <- netex_read_fares_multiple(c(paths, bad)),
    "failed to parse")
  expect_equal(length(nx), 2)
  fails <- netex_read_failures(nx)
  expect_equal(nrow(fails), 1)
  expect_equal(fails$file, "broken.xml")
})

test_that("netex_fare_types and netex_filter_fares select products", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)

  types <- netex_fare_types(nx)
  expect_equal(nrow(types), 2)
  expect_setequal(types$user_type, c("adult", "child"))

  expect_equal(length(netex_filter_fares(nx, user_type = "adult")), 1)
  expect_equal(length(netex_filter_fares(nx, trip_type = "single")), 2)
  expect_equal(length(netex_filter_fares(nx, user_type = "senior")), 0)
  expect_equal(length(netex_filter_fares(nx, product_name = "child")), 1)
})

test_that("netex_match_routes joins on operator + line with fallback", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)
  gtfs <- gtfs_fixture()

  m <- netex_match_routes(nx, gtfs)
  expect_true(all(m$matched))
  expect_true(all(m$route_id == "PF0000001"))

  # fallback: unknown agency_id still matches by line number alone
  gtfs2 <- gtfs_fixture()
  gtfs2$routes$agency_id <- "OTHER"
  m2 <- netex_match_routes(nx, gtfs2)
  expect_true(all(m2$matched))

  # a line number that does not exist does not match
  gtfs3 <- gtfs_fixture()
  gtfs3$routes$route_short_name <- "99"
  m3 <- netex_match_routes(nx, gtfs3)
  expect_false(any(m3$matched))
})

test_that("gtfs_add_fares v1 builds zones, fare_attributes and fare_rules", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)
  gtfs <- gtfs_fixture()

  out <- gtfs_add_fares(gtfs, nx, fares_version = 1,
                        user_type = "adult", trip_type = "single")

  # stops in a fare zone got a zone_id; the unused stop did not
  expect_true(all(!is.na(out$stops$zone_id[out$stops$stop_id %in% c("S1", "S2", "S3")])))
  expect_true(is.na(out$stops$zone_id[out$stops$stop_id == "S9"]))

  # one fare per price band, each with a single price
  expect_equal(nrow(out$fare_attributes), 2)
  expect_setequal(out$fare_attributes$price, c(2.5, 1.8))
  expect_true(all(out$fare_attributes$currency_type == "GBP"))
  expect_false(any(duplicated(out$fare_attributes$fare_id)))

  # fare rules reference the route and existing fares
  expect_equal(nrow(out$fare_rules), 2)
  expect_true(all(out$fare_rules$route_id == "PF0000001"))
  expect_true(all(out$fare_rules$fare_id %in% out$fare_attributes$fare_id))
})

test_that("gtfs_add_fares v2 builds spec-compliant Fares v2 tables", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)
  gtfs <- gtfs_fixture()

  out <- gtfs_add_fares(gtfs, nx, fares_version = 2)

  # areas + stop_areas from the zonal file
  expect_equal(nrow(out$areas), 2)
  expect_equal(nrow(out$stop_areas), 3)
  expect_true(all(out$stop_areas$area_id %in% out$areas$area_id))

  # products: 2 zonal adult bands + 1 flat child fare
  expect_equal(nrow(out$fare_products), 3)
  expect_false(any(duplicated(out$fare_products$fare_product_id)))
  expect_setequal(round(out$fare_products$amount, 2), c(2.5, 1.8, 1.2))

  # leg rules: 2 zonal + 1 network-wide flat rule
  expect_equal(nrow(out$fare_leg_rules), 3)
  flat_rule <- out$fare_leg_rules[is.na(out$fare_leg_rules$from_area_id), ]
  expect_equal(nrow(flat_rule), 1)
  expect_true(all(out$fare_leg_rules$fare_product_id %in%
                    out$fare_products$fare_product_id))

  # rider_categories: unique ids, required default flag on exactly one row
  expect_false(any(duplicated(out$rider_categories$rider_category_id)))
  expect_true("is_default_fare_category" %in% names(out$rider_categories))
  expect_equal(sum(out$rider_categories$is_default_fare_category == 1L), 1)
  expect_equal(
    out$rider_categories$rider_category_id[
      out$rider_categories$is_default_fare_category == 1L], "adult")

  # network per route, and route_networks references real routes
  expect_true(all(out$route_networks$route_id %in% gtfs$routes$route_id))
  expect_true(all(out$route_networks$network_id %in% out$networks$network_id))
})

test_that("netex_fares_report summarises files and match rates", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)
  gtfs <- gtfs_fixture()

  expect_message(rep <- netex_fares_report(nx, gtfs), "NeTEx fares report")
  expect_equal(unname(rep$overview["files"]), 2)
  expect_equal(unname(rep$overview["failed_to_parse"]), 0)
  expect_equal(unname(rep$overview["operators"]), 1)
  expect_equal(unname(rep$overview["zonal_fares"]), 1)
  expect_equal(unname(rep$overview["flat_fares"]), 1)
  expect_equal(unname(rep$overview["lines_matched"]), 1)
  expect_equal(nrow(rep$unmatched), 0)
  expect_true(all(rep$match$matched))
})


test_that("gtfs_add_fares warns and returns GTFS unchanged when nothing matches", {
  paths <- netex_fixture()
  nx <- netex_read_fares_multiple(paths, quiet = TRUE)
  gtfs <- gtfs_fixture()

  expect_warning(out <- gtfs_add_fares(gtfs, nx, user_type = "senior"),
                 "No NeTEx fare files matched")
  expect_identical(out, gtfs)
})
