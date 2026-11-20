# Stop locations in transxchange2gtfs(): NaPTAN, the TransXchange file's own
# definitions, or one filling the other's gaps.
#
# Before 0.4.1 only NaPTAN was used. A stop missing from the NaPTAN table the
# caller passed - a new stop, typically - came out without coordinates, and
# gtfs_clean() then deleted it and every call at it, silently, although
# Transport for London's files say exactly where each of their stops is. That
# is how SL12's Ferry Lane stop (270 timing links in its file) vanished from a
# November 2026 TNDS conversion.
#
# The documents are built here so the stop definitions can be set precisely.

# A stop is c(id, form, ...) where form is "grid" (StopPoint with
# Easting/Northing), "wgs" (StopPoint with Translation Longitude/Latitude) or
# "ref" (AnnotatedStopPointRef, no location).
sl_stop_xml <- function(id, form, x = NA, y = NA, name = paste("Stop", id)) {
  if (form == "ref") {
    return(paste0('<AnnotatedStopPointRef><StopPointRef>', id,
                  '</StopPointRef><CommonName>', name,
                  '</CommonName></AnnotatedStopPointRef>'))
  }
  loc <- if (form == "grid") {
    paste0('<Location><Easting>', x, '</Easting><Northing>', y,
           '</Northing></Location>')
  } else {
    paste0('<Location><Translation><Longitude>', x, '</Longitude><Latitude>',
           y, '</Latitude></Translation></Location>')
  }
  paste0('<StopPoint CreationDateTime="2026-09-01T00:00:00"><AtcoCode>', id,
         '</AtcoCode><Descriptor><CommonName>', name,
         '</CommonName></Descriptor><Place><NptgLocalityRef>E0034469',
         '</NptgLocalityRef>', loc, '</Place><StopClassification>',
         '<StopType>BCT</StopType></StopClassification></StopPoint>')
}

# One service calling at `route` in order, declaring the stops in `stops_xml`
sl_file <- function(stops_xml, route, code) {
  links <- vapply(seq_len(length(route) - 1), function(i) {
    paste0('<JourneyPatternTimingLink id="JPTL', i, '">',
           '<From><StopPointRef>', route[i], '</StopPointRef>',
           '<TimingStatus>PTP</TimingStatus></From>',
           '<To><StopPointRef>', route[i + 1], '</StopPointRef>',
           '<TimingStatus>PTP</TimingStatus></To>',
           '<RouteLinkRef>RL', i, '</RouteLinkRef><RunTime>PT5M</RunTime>',
           '</JourneyPatternTimingLink>')
  }, character(1))
  paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<TransXChange xmlns="http://www.transxchange.org.uk/" ',
    'CreationDateTime="2026-01-01T00:00:00" ',
    'ModificationDateTime="2026-01-01T00:00:00" SchemaVersion="2.4">',
    '<StopPoints>', paste(stops_xml, collapse = ''), '</StopPoints>',
    '<JourneyPatternSections><JourneyPatternSection id="JPS1">',
    paste(links, collapse = ''),
    '</JourneyPatternSection></JourneyPatternSections>',
    '<Operators><Operator id="O1">',
    '<NationalOperatorCode>TSTO</NationalOperatorCode>',
    '<OperatorCode>TSTO</OperatorCode>',
    '<OperatorShortName>Test Buses</OperatorShortName>',
    '</Operator></Operators>',
    '<Services><Service>',
    '<ServiceCode>', code, '</ServiceCode>',
    '<Lines><Line id="SL_', code, '"><LineName>', code, '</LineName></Line></Lines>',
    '<OperatingPeriod><StartDate>2026-01-05</StartDate>',
    '<EndDate>2026-12-20</EndDate></OperatingPeriod>',
    '<OperatingProfile><RegularDayType><DaysOfWeek><Monday/>',
    '</DaysOfWeek></RegularDayType></OperatingProfile>',
    '<RegisteredOperatorRef>O1</RegisteredOperatorRef>',
    '<StopRequirements><NoNewStopsRequired/></StopRequirements>',
    '<Mode>bus</Mode><PublicUse>true</PublicUse>',
    '<StandardService><Origin>A</Origin><Destination>B</Destination>',
    '<JourneyPattern id="JP1"><DestinationDisplay>B</DestinationDisplay>',
    '<Direction>outbound</Direction>',
    '<JourneyPatternSectionRefs>JPS1</JourneyPatternSectionRefs>',
    '</JourneyPattern></StandardService>',
    '</Service></Services>',
    '<VehicleJourneys><VehicleJourney>',
    '<PrivateCode>VJ1</PrivateCode><VehicleJourneyCode>VJ1</VehicleJourneyCode>',
    '<ServiceRef>', code, '</ServiceRef><LineRef>SL_', code, '</LineRef>',
    '<JourneyPatternRef>JP1</JourneyPatternRef>',
    '<DepartureTime>07:00:00</DepartureTime>',
    '</VehicleJourney></VehicleJourneys>',
    '</TransXChange>')
}

# Elm Park Station (490000074B): TfL's file gives E552567 N185780; NaPTAN's
# own position is 0.199054, 51.550287.
sl_known <- c(lon = 0.199054, lat = 51.550287)

# STP1 is in NaPTAN and defined on the grid; STP2 is not in NaPTAN and given in
# WGS84 by both files, at slightly different points; STP3 is in NaPTAN and only
# referenced; STP4 is in neither, so nothing can locate it.
sl_files <- function() {
  f1 <- sl_file(c(sl_stop_xml("490000074B", "grid", 552567, 185780),
                  sl_stop_xml("STP2", "wgs", -1.5000, 53.8000),
                  sl_stop_xml("STP3", "ref"),
                  sl_stop_xml("STP4", "ref")),
                c("490000074B", "STP2", "STP3", "STP4"), "AAA")
  f2 <- sl_file(c(sl_stop_xml("STP3", "ref"),
                  sl_stop_xml("STP2", "wgs", -1.5010, 53.8000)),
                c("STP3", "STP2"), "BBB")
  paths <- c(tempfile(fileext = ".xml"), tempfile(fileext = ".xml"))
  writeLines(f1, paths[1])
  writeLines(f2, paths[2])
  paths
}

sl_naptan <- function() {
  data.frame(stop_id = c("490000074B", "STP3"),
             stop_code = c("a", "c"),
             stop_name = c("Elm Park Station", "Stop three"),
             stop_lon = c(sl_known[["lon"]], -1.4),
             stop_lat = c(sl_known[["lat"]], 53.9),
             stringsAsFactors = FALSE)
}

sl_cal <- function() {
  data.frame(name = "LateSummerBankHolidayNotScotland",
             date = as.Date("2026-08-31"), EnglandWales = TRUE,
             Scotland = FALSE, stringsAsFactors = FALSE)
}

sl_convert <- function(opt) {
  suppressMessages(transxchange2gtfs(sl_files(), cal = sl_cal(),
                                     naptan = sl_naptan(), stop_locations = opt,
                                     force_merge = TRUE))
}

sl_pos <- function(gtfs, id) {
  s <- gtfs$stops[gtfs$stops$stop_id == id, ]
  c(lon = s$stop_lon, lat = s$stop_lat)
}


test_that("import_stop_locations reads full definitions and references", {
  f <- sl_files()[1]
  sp <- xml2::xml_child(xml2::read_xml(f), "d1:StopPoints")
  l <- UK2GTFS:::import_stop_locations(sp)
  expect_equal(l$stop_id, c("490000074B", "STP2", "STP3", "STP4"))
  expect_equal(l$form, c("StopPoint", "StopPoint", "AnnotatedStopPointRef",
                         "AnnotatedStopPointRef"))
  expect_equal(l$easting, c(552567, NA, NA, NA))
  expect_equal(l$lon, c(NA, -1.5, NA, NA))
  expect_equal(l$stop_name[1], "Stop 490000074B")
  # A file of full definitions declares its stops by AtcoCode, not
  # StopPointRef; the declared-stop list must still find them.
  expect_equal(UK2GTFS:::import_stoppoints(sp, full_import = FALSE)$StopPointRef,
               c("490000074B", "STP2", "STP3", "STP4"))
})


test_that("a grid reference converts to within a metre of NaPTAN's position", {
  r <- UK2GTFS:::txc_resolve_stop_locations(data.frame(
    stop_id = "490000074B", stop_name = "x", easting = 552567,
    northing = 185780, lon = NA_real_, lat = NA_real_, form = "StopPoint"))
  d <- UK2GTFS:::txc_distance_m(r$txc_lon, r$txc_lat,
                                sl_known[["lon"]], sl_known[["lat"]])
  expect_lt(d, 1)
})


test_that("naptan_fill keeps NaPTAN and fills only the stops it lacks", {
  g <- sl_convert("naptan_fill")
  expect_equal(sl_pos(g, "STP3"), c(lon = -1.4, lat = 53.9))
  expect_equal(sl_pos(g, "490000074B"), sl_known)
  # STP2: the median of the two files' definitions
  expect_equal(sl_pos(g, "STP2"), c(lon = -1.5005, lat = 53.8))
  expect_true(is.na(sl_pos(g, "STP4")[["lon"]]))
  expect_equal(g$stops$stop_name[g$stops$stop_id == "STP2"], "Stop STP2")

  r <- attr(g, "stop_location_report")
  expect_equal(r$source[match(c("490000074B", "STP2", "STP3", "STP4"), r$stop_id)],
               c("naptan", "txc", "naptan", "none"))
  expect_equal(r$txc_n[r$stop_id == "STP2"], 2L)
  expect_gt(r$txc_spread_m[r$stop_id == "STP2"], 30)
  expect_lt(r$dist_m[r$stop_id == "490000074B"], 1)
  expect_equal(r$calls[r$stop_id == "STP2"], 2L)
})


test_that("naptan alone loses the stops NaPTAN lacks, and says so on cleaning", {
  g <- sl_convert("naptan")
  expect_true(is.na(sl_pos(g, "STP2")[["lon"]]))
  expect_message(g2 <- gtfs_clean(g), "removed 2 stops with no coordinates and 3 stop_times")
  expect_false("STP2" %in% g2$stop_times$stop_id)
})


test_that("txc uses the files' positions and NaPTAN for nothing", {
  g <- sl_convert("txc")
  expect_true(is.na(sl_pos(g, "STP3")[["lon"]]))
  expect_lt(UK2GTFS:::txc_distance_m(sl_pos(g, "490000074B")[["lon"]],
                                     sl_pos(g, "490000074B")[["lat"]],
                                     sl_known[["lon"]], sl_known[["lat"]]), 1)
  r <- attr(g, "stop_location_report")
  # NaPTAN is still joined, so the report can compare
  expect_false(is.na(r$naptan_lon[r$stop_id == "STP3"]))
})


test_that("txc_fill prefers the files and falls back to NaPTAN", {
  g <- sl_convert("txc_fill")
  expect_equal(sl_pos(g, "STP3"), c(lon = -1.4, lat = 53.9))
  expect_equal(sl_pos(g, "STP2"), c(lon = -1.5005, lat = 53.8))
  r <- attr(g, "stop_location_report")
  expect_equal(r$source[r$stop_id == "490000074B"], "txc")
})


test_that("the conversion reports where its stop locations came from", {
  expect_message(
    transxchange2gtfs(sl_files(), cal = sl_cal(), naptan = sl_naptan(),
                      force_merge = TRUE),
    "Stop locations \\(naptan_fill\\): 2 from NaPTAN, 1 from the TransXchange files, 1 with none")
})
