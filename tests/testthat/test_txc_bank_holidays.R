# TNDS contains vehicle journeys whose <BankHolidayOperation> names the same
# holiday in both <DaysOfOperation> and <DaysOfNonOperation> - TfL's night
# buses do it for the late summer bank holiday. Left alone that produces two
# calendar_dates rows for one service_id + date, one adding the date and one
# cancelling it, which breaks the GTFS primary key. TransXChange gives
# non-operation precedence, so the cancellation must be the row that survives.
#
# The document is built here rather than downloaded because the assertion is
# about a specific contradiction in a specific journey.

txc_bank_holiday_file <- function(operate = "LateSummerBankHolidayNotScotland",
                                  no_operate = "LateSummerBankHolidayNotScotland") {
  bh <- paste0(
    '<BankHolidayOperation>',
    if (!is.null(operate)) {
      paste0('<DaysOfOperation><', operate, '/></DaysOfOperation>')
    } else "",
    if (!is.null(no_operate)) {
      paste0('<DaysOfNonOperation><', no_operate, '/></DaysOfNonOperation>')
    } else "",
    '</BankHolidayOperation>')

  stops <- c("0100BRP90310", "0100BRP90311")
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
    '<JourneyPatternSection id="JPS1">',
    '<JourneyPatternTimingLink id="JPTL1">',
    '<From><StopPointRef>', stops[1], '</StopPointRef>',
    '<TimingStatus>PTP</TimingStatus></From>',
    '<To><StopPointRef>', stops[2], '</StopPointRef>',
    '<TimingStatus>PTP</TimingStatus></To>',
    '<RouteLinkRef>RL1</RouteLinkRef><RunTime>PT10M</RunTime>',
    '</JourneyPatternTimingLink></JourneyPatternSection>',
    '</JourneyPatternSections>',
    '<Operators><Operator id="O1">',
    '<NationalOperatorCode>TSTO</NationalOperatorCode>',
    '<OperatorCode>TSTO</OperatorCode>',
    '<OperatorShortName>Test Buses</OperatorShortName>',
    '</Operator></Operators>',
    '<Services><Service>',
    '<ServiceCode>AAA</ServiceCode>',
    '<Lines><Line id="SL_AAA"><LineName>AAA</LineName></Line></Lines>',
    # spans the 2026 late summer bank holiday
    '<OperatingPeriod><StartDate>2026-01-05</StartDate>',
    '<EndDate>2026-12-20</EndDate></OperatingPeriod>',
    '<OperatingProfile><RegularDayType><DaysOfWeek><Monday/>',
    '</DaysOfWeek></RegularDayType></OperatingProfile>',
    '<RegisteredOperatorRef>O1</RegisteredOperatorRef>',
    '<StopRequirements><NoNewStopsRequired/></StopRequirements>',
    '<Mode>bus</Mode><PublicUse>true</PublicUse>',
    '<StandardService><Origin>Stop 1</Origin><Destination>Stop 2</Destination>',
    '<JourneyPattern id="JP1">',
    '<DestinationDisplay>Stop 2</DestinationDisplay>',
    '<Direction>outbound</Direction>',
    '<JourneyPatternSectionRefs>JPS1</JourneyPatternSectionRefs>',
    '</JourneyPattern></StandardService>',
    '</Service></Services>',
    '<VehicleJourneys><VehicleJourney>',
    '<PrivateCode>VJ1</PrivateCode>',
    '<VehicleJourneyCode>VJ1</VehicleJourneyCode>',
    '<ServiceRef>AAA</ServiceRef><LineRef>SL_AAA</LineRef>',
    '<JourneyPatternRef>JP1</JourneyPatternRef>',
    '<OperatingProfile><RegularDayType><DaysOfWeek><Monday/>',
    '</DaysOfWeek></RegularDayType>', bh, '</OperatingProfile>',
    '<DepartureTime>07:00:00</DepartureTime>',
    '</VehicleJourney></VehicleJourneys>',
    '</TransXChange>')
}

# Enough of each lookup for transxchange_export(); using the real ones would
# make the test depend on a download and on the gov.uk feed still carrying 2026
bh_test_cal <- function() {
  data.frame(name = "LateSummerBankHolidayNotScotland",
             date = as.Date("2026-08-31"),
             EnglandWales = TRUE, Scotland = FALSE,
             stringsAsFactors = FALSE)
}

bh_test_naptan <- function() {
  data.frame(stop_id = c("0100BRP90310", "0100BRP90311"),
             stop_code = c("bstpgdm", "bstpgdp"),
             stop_name = c("Stop 1", "Stop 2"),
             stop_lon = c(-2.5, -2.51), stop_lat = c(51.4, 51.41),
             stringsAsFactors = FALSE)
}

bh_export <- function(xml) {
  f <- tempfile(fileext = ".xml")
  writeLines(xml, f)
  obj <- transxchange_import(f)
  UK2GTFS:::transxchange_export(obj, cal = bh_test_cal(),
                                naptan = bh_test_naptan())
}


test_that("a holiday named as both operating and non-operating cancels", {
  gtfs <- bh_export(txc_bank_holiday_file())
  cd <- as.data.frame(gtfs$calendar_dates)

  # one row per service_id + date, as the GTFS primary key requires
  expect_equal(anyDuplicated(paste(cd$service_id, cd$date)), 0L)

  bh <- cd[cd$date == "20260831", ]
  expect_equal(nrow(bh), 1L)
  expect_equal(as.integer(bh$exception_type), 2L)
})


test_that("an uncontradicted bank holiday keeps its own exception type", {
  # the same fixture without the contradiction, so the dedupe cannot be
  # passing the test above by discarding additions generally
  add <- as.data.frame(bh_export(txc_bank_holiday_file(no_operate = NULL))
                       $calendar_dates)
  expect_equal(as.integer(add$exception_type[add$date == "20260831"]), 1L)

  drop <- as.data.frame(bh_export(txc_bank_holiday_file(operate = NULL))
                        $calendar_dates)
  expect_equal(as.integer(drop$exception_type[drop$date == "20260831"]), 2L)
})
