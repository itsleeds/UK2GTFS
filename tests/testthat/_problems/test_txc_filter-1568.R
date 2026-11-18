# Extracted from test_txc_filter.R:1568

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "UK2GTFS", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("txc_filter_files removes superseded service versions")
make_txc <- function(dir, name, service, startdate, rev, modtime,
                     lines = character(0), enddate = NULL, desc = NULL,
                     noc = NULL, createtime = NULL, mode = NULL,
                     days = character(0)) {
  lines_xml <- if (length(lines)) {
    paste0("<Lines>",
           paste0(sprintf("<Line id=\"L%s\"><LineName>%s</LineName></Line>",
                          seq_along(lines), lines), collapse = ""),
           "</Lines>")
  } else {
    ""
  }
  end_xml <- if (is.null(enddate)) "" else sprintf("<EndDate>%s</EndDate>", enddate)
  desc_xml <- if (is.null(desc)) "" else sprintf("<Description>%s</Description>", desc)
  mode_xml <- if (is.null(mode)) "" else sprintf("<Mode>%s</Mode>", mode)
  # One vehicle journey carrying the day type, so the file declares which days
  # it runs. `days` takes the DaysOfWeek child element names, so a range such
  # as "MondayToFriday" is written as the schema allows it.
  days_xml <- if (length(days)) {
    paste0("<VehicleJourneys><VehicleJourney><OperatingProfile>",
           "<RegularDayType><DaysOfWeek>",
           paste0(sprintf("<%s/>", days), collapse = ""),
           "</DaysOfWeek></RegularDayType></OperatingProfile>",
           "</VehicleJourney></VehicleJourneys>")
  } else {
    ""
  }
  # `noc` may name more than one operator, in document order, for a jointly
  # registered service
  noc_xml <- if (is.null(noc)) "" else paste0(
    "<Operators>",
    paste0(sprintf(
      "<Operator><NationalOperatorCode>%s</NationalOperatorCode></Operator>",
      noc), collapse = ""),
    "</Operators>")
  if (is.null(createtime)) createtime <- modtime
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="%s">
  %s
  <Services>
    <Service RevisionNumber="%s" ModificationDateTime="%s">
      <ServiceCode>%s</ServiceCode>
      %s
      %s
      %s
      <OperatingPeriod><StartDate>%s</StartDate>%s</OperatingPeriod>
    </Service>
  </Services>
  %s
</TransXChange>', createtime, modtime, rev, noc_xml, rev, modtime, service,
    mode_xml, desc_xml, lines_xml, startdate, end_xml, days_xml)
  f <- file.path(dir, name)
  writeLines(xml, f)
  f
}
period_of <- function(f) {
  x <- xml2::read_xml(f)
  svc <- xml2::xml_find_first(x, "d1:Services/d1:Service")
  c(start = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:StartDate")),
    end = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:EndDate")))
}
make_txc_vj <- function(dir, name, service, startdate, enddate, createtime,
                        line, desc, noc, stop, times, so = "",
                        days = "<MondayToFriday/>", stops = character(0),
                        route = if (length(stops) > 1) stops else c(stop, "X")) {
  # `stops` are the stops the file declares, `route` the ones its journey
  # pattern calls at.
  jps <- paste0(sprintf(
    "<JourneyPatternTimingLink><From><StopPointRef>%s</StopPointRef></From><To><StopPointRef>%s</StopPointRef></To></JourneyPatternTimingLink>",
    utils::head(route, -1), route[-1]), collapse = "
    ")
  sp_xml <- if (length(stops)) paste0("<StopPoints>", paste0(
    "<AnnotatedStopPointRef><StopPointRef>", stops,
    "</StopPointRef></AnnotatedStopPointRef>", collapse = ""),
    "</StopPoints>") else ""
  # `so`, if given, is a ServicedOrganisationDayType body, e.g.
  # "<DaysOfOperation><WorkingDays><ServicedOrganisationRef>STU</ServicedOrganisationRef></WorkingDays></DaysOfOperation>"
  so_xml <- if (nzchar(so)) paste0("<ServicedOrganisationDayType>", so,
                                   "</ServicedOrganisationDayType>") else ""
  vj <- paste0(sprintf(paste0(
    "<VehicleJourney><OperatingProfile><RegularDayType><DaysOfWeek>",
    days, "</DaysOfWeek></RegularDayType>", so_xml, "</OperatingProfile>",
    "<VehicleJourneyCode>%d</VehicleJourneyCode><ServiceRef>%s</ServiceRef>",
    "<LineRef>L1</LineRef><JourneyPatternRef>JP1</JourneyPatternRef>",
    "<DepartureTime>%s:00</DepartureTime></VehicleJourney>"),
    seq_along(times), service, times), collapse = "")
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="1">
  %s
  <Operators><Operator><NationalOperatorCode>%s</NationalOperatorCode></Operator></Operators>
  <JourneyPatternSections><JourneyPatternSection id="JPS1">
    %s
  </JourneyPatternSection></JourneyPatternSections>
  <Services><Service RevisionNumber="1" ModificationDateTime="%s">
    <ServiceCode>%s</ServiceCode><Description>%s</Description>
    <Lines><Line id="L1"><LineName>%s</LineName></Line></Lines>
    <OperatingPeriod><StartDate>%s</StartDate><EndDate>%s</EndDate></OperatingPeriod>
    <StandardService><JourneyPattern id="JP1">
      <JourneyPatternSectionRefs>JPS1</JourneyPatternSectionRefs>
    </JourneyPattern></StandardService>
  </Service></Services>
  <VehicleJourneys>%s</VehicleJourneys>
</TransXChange>', createtime, createtime, sp_xml, noc, jps, createtime, service, desc,
    line, startdate, enddate, vj)
  f <- file.path(dir, name)
  writeLines(xml, f)
  f
}

# test -------------------------------------------------------------------------
t6 <- c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00")
t6b <- c("07:05", "08:05", "09:05", "10:05", "11:05", "12:05")
ends_of <- function(res) {
    e <- vapply(res, function(f) period_of(f)[["end"]], "")
    stats::setNames(e, basename(res))
  }
run <- function(files, date = "2026-11-11") {
    od <- tempfile("txcout")
    txc_filter_files(files, date = as.Date(date), out_dir = od)
  }
dir <- tempfile("txctest")
dir.create(dir)
on.exit(unlink(dir, recursive = TRUE))
f117 <- c(make_txc_vj(dir, "old.xml", "PK0002088:45", "2023-11-25", "", "2023-08-15T15:00:00",
                        "117", "Hosptial - Hounslow - Middlesex St", "LUTD", "490A", t6),
            make_txc_vj(dir, "new.xml", "PK0002088:45", "2025-12-13", "", "2025-11-24T14:08:00",
                        "117", "Ashford - Feltham - Hospital - Hounslow", "LUTD", "490A", t6b))
e <- ends_of(run(f117))
expect_true("new.xml" %in% names(e))
if ("old.xml" %in% names(e)) expect_equal(e[["old.xml"]], "2025-12-12")
dir2 <- tempfile("txctest")
dir.create(dir2)
on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
fk2 <- c(make_txc_vj(dir2, "r2.xml", "PH2077104:01010005", "2026-07-27", "", "2026-08-12T12:06:00",
                       "K2", "Chocolate Quarter", "KEMT", "018A", t6),
           make_txc_vj(dir2, "r3.xml", "PH2077104:01010005", "2026-07-27", "", "2026-09-17T11:41:00",
                       "K2", "Chocolate Quarter", "KEMT", "018A", t6b))
expect_equal(basename(run(fk2)), "r3.xml")
dir3 <- tempfile("txctest")
dir.create(dir3)
on.exit(unlink(dir3, recursive = TRUE), add = TRUE)
f7 <- c(make_txc_vj(dir3, "oct.xml", "PH0007007:33", "2026-10-18", "", "2026-10-09T12:25:00",
                      "7", "Britwell - Heathrow", "FTVA", "039A", t6),
          make_txc_vj(dir3, "nov.xml", "PH0007007:33", "2026-11-01", "", "2026-10-30T08:45:00",
                      "7", "Britwell - Heathrow", "FTVA", "039A", t6))
s <- readLines(f7[1])
writeLines(sub('<Line id="L1"><LineName>7</LineName></Line>',
                 '<Line id="L1"><LineName>7</LineName></Line><Line id="L2"><LineName>7X</LineName></Line>',
                 s, fixed = TRUE), f7[1])
e <- ends_of(run(f7))
expect_true("nov.xml" %in% names(e))
if ("oct.xml" %in% names(e)) expect_equal(e[["oct.xml"]], "2026-10-31")
dir4 <- tempfile("txctest")
dir.create(dir4)
on.exit(unlink(dir4, recursive = TRUE), add = TRUE)
fs <- c(make_txc_vj(dir4, "main.xml", "PB1069542:87", "2024-11-18", "", "2024-11-01T10:00:00",
                      "87", "Barnard Castle - Ingleton", "HDGS", "130A", t6),
          make_txc_vj(dir4, "supp.xml", "PB1069542:87", "2024-11-20", "", "2024-11-15T10:00:00",
                      "87", "Barnard Castle - Ingleton", "HDGS", "130A", c("15:40", "16:40")))
e <- ends_of(run(fs))
expect_equal(sort(names(e)), c("main.xml", "supp.xml"))
