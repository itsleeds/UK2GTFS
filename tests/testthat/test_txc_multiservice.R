# A single TransXchange file may declare several <Service> elements. The export
# path reads Services_main$...[1] for the route id, name, agency and mode, so
# transxchange_import() splits such a file into one object per service rather
# than letting them collapse into a single route.
#
# The document is built here rather than downloaded: it has to be multi-service,
# it has to be small enough to reason about, and the assertions are about which
# journey ends up with which service, which needs known contents.

# Minimal but complete enough for txc_import_single(): two services, two
# journeys each, every journey naming its service in <ServiceRef>. Service AAA
# runs Monday-Friday from stop 1 to stop 2, BBB Saturdays from 2 to 3.
txc_two_services <- function(vj_serviceref = TRUE) {
  vj <- function(code, svc, jp, dep) {
    paste0(
      '<VehicleJourney>',
      '<PrivateCode>', code, '</PrivateCode>',
      '<VehicleJourneyCode>', code, '</VehicleJourneyCode>',
      if (vj_serviceref) paste0('<ServiceRef>', svc, '</ServiceRef>') else '',
      '<LineRef>SL_', svc, '</LineRef>',
      '<JourneyPatternRef>', jp, '</JourneyPatternRef>',
      '<DepartureTime>', dep, '</DepartureTime>',
      '</VehicleJourney>')
  }
  service <- function(code, jp, jps, day, from, to) {
    paste0(
      '<Service>',
      '<ServiceCode>', code, '</ServiceCode>',
      '<Lines><Line id="SL_', code, '"><LineName>', code, '</LineName></Line></Lines>',
      '<OperatingPeriod><StartDate>2026-01-05</StartDate>',
      '<EndDate>2026-12-20</EndDate></OperatingPeriod>',
      '<OperatingProfile><RegularDayType><DaysOfWeek><', day, '/>',
      '</DaysOfWeek></RegularDayType></OperatingProfile>',
      '<RegisteredOperatorRef>O1</RegisteredOperatorRef>',
      '<StopRequirements><NoNewStopsRequired/></StopRequirements>',
      '<Mode>bus</Mode><PublicUse>true</PublicUse>',
      '<StandardService><Origin>', from, '</Origin><Destination>', to, '</Destination>',
      '<JourneyPattern id="', jp, '">',
      '<DestinationDisplay>', to, '</DestinationDisplay>',
      '<Direction>outbound</Direction>',
      '<JourneyPatternSectionRefs>', jps, '</JourneyPatternSectionRefs>',
      '</JourneyPattern></StandardService>',
      '</Service>')
  }
  jpsection <- function(id, link, a, b) {
    paste0('<JourneyPatternSection id="', id, '">',
           '<JourneyPatternTimingLink id="JPTL_', id, '">',
           '<From><StopPointRef>', a, '</StopPointRef>',
           '<TimingStatus>PTP</TimingStatus></From>',
           '<To><StopPointRef>', b, '</StopPointRef>',
           '<TimingStatus>PTP</TimingStatus></To>',
           '<RouteLinkRef>', link, '</RouteLinkRef>',
           '<RunTime>PT10M</RunTime>',
           '</JourneyPatternTimingLink></JourneyPatternSection>')
  }
  stops <- c("0100BRP90310", "0100BRP90311", "0100BRP90312")
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<TransXChange xmlns="http://www.transxchange.org.uk/" ',
    'CreationDateTime="2026-01-01T00:00:00" ',
    'ModificationDateTime="2026-01-01T00:00:00" SchemaVersion="2.4">',
    '<StopPoints>',
    paste0('<AnnotatedStopPointRef><StopPointRef>', stops,
           '</StopPointRef><CommonName>Stop ', seq_along(stops),
           '</CommonName></AnnotatedStopPointRef>', collapse = ''),
    '</StopPoints>',
    '<JourneyPatternSections>',
    jpsection("JPS_AAA", "RL1", stops[1], stops[2]),
    jpsection("JPS_BBB", "RL2", stops[2], stops[3]),
    '</JourneyPatternSections>',
    '<Operators><Operator id="O1">',
    '<NationalOperatorCode>TSTO</NationalOperatorCode>',
    '<OperatorCode>TSTO</OperatorCode>',
    '<OperatorShortName>Test Buses</OperatorShortName>',
    '</Operator></Operators>',
    '<Services>',
    service("AAA", "JP_AAA", "JPS_AAA", "MondayToFriday", "Stop 1", "Stop 2"),
    service("BBB", "JP_BBB", "JPS_BBB", "Saturday", "Stop 2", "Stop 3"),
    '</Services>',
    '<VehicleJourneys>',
    vj("VJ1", "AAA", "JP_AAA", "07:00:00"),
    vj("VJ2", "AAA", "JP_AAA", "08:00:00"),
    vj("VJ3", "BBB", "JP_BBB", "09:00:00"),
    '</VehicleJourneys>',
    '</TransXChange>')
}

write_txc <- function(xml) {
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  f
}


test_that("a multi-service file imports as one object per service", {
  res <- transxchange_import(write_txc(txc_two_services()))

  expect_s3_class(res, "txc_multi")
  expect_length(res, 2)

  codes <- vapply(res, function(x) x$Services_main$ServiceCode, character(1))
  expect_equal(sort(codes), c("AAA", "BBB"))

  for (x in res) {
    # One service per object is what the export assumes
    expect_equal(nrow(x$Services_main), 1L)
    # and each journey must belong to it
    expect_true(all(x$VehicleJourneys$ServiceRef == x$Services_main$ServiceCode))
  }

  # The journeys are divided up, not duplicated or dropped
  n <- vapply(res, function(x) nrow(x$VehicleJourneys), integer(1))
  expect_equal(sum(n), 3L)
  expect_equal(n[codes == "AAA"], 2L, ignore_attr = TRUE)
  expect_equal(n[codes == "BBB"], 1L, ignore_attr = TRUE)

  # Each service keeps its own operating profile
  days <- vapply(res, function(x) x$Services_main$DaysOfWeek, character(1))
  expect_equal(days[codes == "AAA"], "MondayToFriday", ignore_attr = TRUE)
  expect_equal(days[codes == "BBB"], "Saturday", ignore_attr = TRUE)
})


test_that("splitting is refused when journeys cannot be attributed", {
  # Without <ServiceRef> there is no way to say which service a journey belongs
  # to, and guessing would credit one service with another's trips. Failing
  # keeps the file out of the conversion instead of corrupting it.
  f <- write_txc(txc_two_services(vj_serviceref = FALSE))
  expect_error(transxchange_import(f), "More than one service")
})


test_that("a single-service file is unaffected", {
  xml <- txc_two_services()
  # Drop the second service and the journey that references it
  xml <- sub('<Service><ServiceCode>BBB</ServiceCode>.*?</Service></Services>',
             '</Services>', xml)
  xml <- sub('<VehicleJourney><PrivateCode>VJ3</PrivateCode>.*?</VehicleJourney>',
             '', xml)
  res <- transxchange_import(write_txc(xml))

  expect_false(inherits(res, "txc_multi"))
  expect_equal(nrow(res$Services_main), 1L)
  expect_equal(res$Services_main$ServiceCode, "AAA")
  expect_equal(nrow(res$VehicleJourneys), 2L)
})


test_that("multi-service results are spliced into the list of objects", {
  single <- list(filename = "one.xml")
  multi <- structure(list(list(filename = "two.xml"), list(filename = "two.xml")),
                     class = "txc_multi")
  failed <- "three.xml with error: ..."

  out <- UK2GTFS:::txc_flatten_multiservice(list(single, multi, failed))

  expect_length(out, 4L)
  # The character element a failed import leaves behind must survive, since the
  # caller separates those out to report them
  expect_equal(vapply(out, function(x) class(x)[1], character(1)),
               c("list", "list", "list", "character"))

  # A list with nothing to splice is returned untouched
  plain <- list(single, failed)
  expect_identical(UK2GTFS:::txc_flatten_multiservice(plain), plain)
})


test_that("each service of a multi-service file exports to its own route", {
  res <- transxchange_import(write_txc(txc_two_services()))
  gtfs <- lapply(res, function(x)
    UK2GTFS:::transxchange_export(x, cal = get_bank_holidays(),
                                  naptan = get_naptan()))
  gtfs <- gtfs[!vapply(gtfs, is.null, logical(1))]
  expect_length(gtfs, 2L)

  merged <- gtfs_merge(gtfs, force = TRUE)
  # Two services in, two routes out - the failure this guards against is one
  # route carrying both services' journeys
  expect_equal(nrow(merged$routes), 2L)
  expect_equal(sort(as.character(merged$routes$route_short_name)),
               c("AAA", "BBB"))
  expect_equal(nrow(merged$trips), 3L)
})
