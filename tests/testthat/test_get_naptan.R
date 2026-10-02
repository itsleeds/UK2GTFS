# Tests for get_naptan.R.
#
# All three entry points download from the DfT. Rather than skip them as
# network tests, these point them at small fixtures written to a temporary
# file: both download.file() and url() take a file:// URL, so the parsing,
# reprojection and unpacking are exercised for real.

context("Reading NaPTAN")

file_url <- function(path) {
  p <- normalizePath(path, winslash = "/", mustWork = TRUE)
  if (substr(p, 1, 1) == "/") paste0("file://", p) else paste0("file:///", p)
}

# Two stops in British National Grid eastings and northings, as the CSV
# download gives them, plus columns get_naptan does not use
naptan_csv <- function() {
  path <- file.path(tempdir(), "naptan_stops.csv")
  writeLines(c(
    "ATCOCode,NaptanCode,PlateCode,CommonName,Easting,Northing,StopType",
    "4500LE0001,wyajgjw,,Leeds Rail Station,429900,433500,RLY",
    "4500LE0002,wyajgjt,,City Square,429700,433700,BCT"), path)
  path
}

# Three extra stops: one is already in the CSV and must not be added twice
naptan_extras <- function() {
  data.frame(
    stop_id = c("4500LE0001", "9400ZZEXTRA1", "9400ZZEXTRA2"),
    stop_code = c("", "", ""),
    stop_name = c("Leeds Rail Station", "Extra One", "Extra Two"),
    stop_lon = c("-1.548", "-1.600", "-1.700"),   # character on purpose
    stop_lat = c("53.795", "53.900", "53.950"),
    stringsAsFactors = FALSE)
}


test_that("get_naptan reprojects to WGS84 and appends the missing stops", {
  naptan <- get_naptan(url = file_url(naptan_csv()),
                       naptan_extra = naptan_extras())

  expect_named(naptan, c("stop_id", "stop_code", "stop_name", "stop_lon",
                         "stop_lat"))
  # the two stops from the file, plus the two extras that were not in it
  expect_equal(nrow(naptan), 4)
  expect_setequal(naptan$stop_id, c("4500LE0001", "4500LE0002",
                                   "9400ZZEXTRA1", "9400ZZEXTRA2"))
  expect_equal(sum(naptan$stop_id == "4500LE0001"), 1L)

  # coordinates must stay numeric: format() would make them padded strings
  # and break everything downstream
  expect_type(naptan$stop_lon, "double")
  expect_type(naptan$stop_lat, "double")

  leeds <- naptan[naptan$stop_id == "4500LE0001", ]
  expect_equal(leeds$stop_lon, -1.548, tolerance = 1e-3)
  expect_equal(leeds$stop_lat, 53.795, tolerance = 1e-3)
  expect_equal(leeds$stop_name, "Leeds Rail Station")
  expect_equal(leeds$stop_code, "wyajgjw")

  # rounded to six decimal places, which is about 0.1 m
  expect_equal(naptan$stop_lon, round(naptan$stop_lon, 6))

  # the extras keep the coordinates they were given, as numbers
  extra <- naptan[naptan$stop_id == "9400ZZEXTRA1", ]
  expect_equal(extra$stop_lon, -1.6)
})


test_that("get_naptan leaves no temporary files behind", {
  before <- list.files(tempdir())
  get_naptan(url = file_url(naptan_csv()), naptan_extra = naptan_extras())
  expect_false("temp_naptan" %in% setdiff(list.files(tempdir()), before))
})


# ---- the XML download ------------------------------------------------------

# One rail station, with the TIPLOC and CRS codes that only the XML carries,
# and one bus stop, which has neither
naptan_xml <- function() {
  path <- file.path(tempdir(), "naptan_stops.xml")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<NaPTAN xmlns="http://www.naptan.org.uk/">',
    '<StopPoints>',
    '<StopPoint>',
    '<AtcoCode>9100LEEDS</AtcoCode>',
    '<Descriptor><CommonName>Leeds Rail Station</CommonName></Descriptor>',
    '<Place><Location><Translation>',
    '<Easting>429900</Easting><Northing>433500</Northing>',
    '<Longitude>-1.548</Longitude><Latitude>53.795</Latitude>',
    '</Translation></Location></Place>',
    '<StopClassification><StopType>RLY</StopType>',
    '<OffStreet><Rail><AccessArea/>',
    '<AnnotatedRailRef><TiplocRef>LEEDS</TiplocRef><CrsRef>LDS</CrsRef>',
    '<StationName>Leeds</StationName></AnnotatedRailRef>',
    '</Rail></OffStreet></StopClassification>',
    '<StopAreas><StopAreaRef>910GLEEDS</StopAreaRef></StopAreas>',
    '<AdministrativeAreaRef>107</AdministrativeAreaRef>',
    '</StopPoint>',
    '<StopPoint>',
    '<AtcoCode>45000010151</AtcoCode><NaptanCode>wyajgjt</NaptanCode>',
    '<Descriptor><CommonName>City Square</CommonName>',
    '<ShortCommonName>City Sq</ShortCommonName></Descriptor>',
    '<Place><Location><Translation>',
    '<Easting>429700</Easting><Northing>433700</Northing>',
    '<Longitude>-1.551</Longitude><Latitude>53.797</Latitude>',
    '</Translation></Location></Place>',
    '<StopClassification><StopType>BCT</StopType>',
    '<OnStreet><Bus><BusStopType>MKD</BusStopType></Bus></OnStreet>',
    '</StopClassification>',
    '<AdministrativeAreaRef>107</AdministrativeAreaRef>',
    '</StopPoint>',
    '</StopPoints>',
    '<StopAreas>',
    '<StopArea>',
    '<StopAreaCode>910GLEEDS</StopAreaCode>',
    '<Name>Leeds Rail Station</Name>',
    '<AdministrativeAreaRef>107</AdministrativeAreaRef>',
    '<StopAreaType>GRLS</StopAreaType>',
    '<Location><Translation>',
    '<Easting>429900</Easting><Northing>433500</Northing>',
    '<Longitude>-1.548</Longitude><Latitude>53.795</Latitude>',
    '</Translation></Location>',
    '</StopArea>',
    '</StopAreas>',
    '</NaPTAN>'), path)
  path
}


test_that("get_naptan_xml_doc reads the document and restores the timeout", {
  before <- getOption("timeout")
  doc <- get_naptan_xml_doc(url = file_url(naptan_xml()), timeout = 42L)

  expect_s3_class(doc, "xml_document")
  expect_equal(xml2::xml_name(xml2::xml_root(doc)), "NaPTAN")
  # the download timeout is a global option, so it must be put back
  expect_equal(getOption("timeout"), before)
})


test_that("as_data_table_naptan_stop_point unpacks rail stations by default", {
  doc <- get_naptan_xml_doc(url = file_url(naptan_xml()))
  dt <- as_data_table_naptan_stop_point(doc)

  expect_s3_class(dt, "data.table")
  expect_equal(nrow(dt), 1)
  expect_equal(dt$AtcoCode, "9100LEEDS")
  expect_equal(dt$CommonName, "Leeds Rail Station")
  expect_equal(dt$StopType, "RLY")
  expect_equal(dt$ModeType, "Rail")
  expect_equal(dt$RailType, "AccessArea")
  # TIPLOC and CRS are the reason to use the XML rather than the CSV
  expect_equal(dt$Tiploc, "LEEDS")
  expect_equal(dt$Crs, "LDS")
  expect_equal(dt$Easting, 429900L)
  expect_equal(dt$Northing, 433500L)
  expect_equal(dt$Longitude, -1.548)
  expect_equal(dt$Latitude, 53.795)
  expect_equal(dt$StopAreaRef, "910GLEEDS")
  expect_equal(dt$AdministrativeAreaRef, 107L)
  # rail stations rarely have a NaptanCode or a short name
  expect_true(is.na(dt$NaptanCode))
  expect_true(is.na(dt$ShortCommonName))
})


test_that("as_data_table_naptan_stop_point can take other or all stop types", {
  doc <- get_naptan_xml_doc(url = file_url(naptan_xml()))

  bus <- as_data_table_naptan_stop_point(doc, stopTypes = "BCT")
  expect_equal(bus$AtcoCode, "45000010151")
  expect_equal(bus$NaptanCode, "wyajgjt")
  expect_equal(bus$ShortCommonName, "City Sq")
  # an on-street stop has no OffStreet mode and no rail references
  expect_true(is.na(bus$ModeType))
  expect_true(is.na(bus$Tiploc))

  both <- as_data_table_naptan_stop_point(doc, stopTypes = c("RLY", "BCT"))
  expect_equal(nrow(both), 2)

  # no restriction at all takes every stop point
  expect_equal(nrow(as_data_table_naptan_stop_point(doc, stopTypes = NA)), 2)
  expect_equal(nrow(as_data_table_naptan_stop_point(doc,
                                                    stopTypes = character(0))), 2)
  expect_equal(nrow(as_data_table_naptan_stop_point(doc, stopTypes = NULL)), 2)
})


test_that("as_data_table_naptan_stop_area unpacks the stop areas", {
  doc <- get_naptan_xml_doc(url = file_url(naptan_xml()))
  dt <- as_data_table_naptan_stop_area(doc)

  expect_s3_class(dt, "data.table")
  expect_named(dt, c("StopAreaCode", "Name", "AdministrativeAreaRef",
                     "StopAreaType", "Easting", "Northing", "Longitude",
                     "Latitude"))
  expect_equal(dt$StopAreaCode, "910GLEEDS")
  expect_equal(dt$StopAreaType, "GRLS")
  expect_equal(dt$AdministrativeAreaRef, 107L)
  expect_equal(dt$Easting, 429900L)
  expect_equal(dt$Latitude, 53.795)

  # the stop points reference it, which is how platforms are grouped
  sp <- as_data_table_naptan_stop_point(doc)
  expect_true(all(sp$StopAreaRef %in% dt$StopAreaCode))
})
