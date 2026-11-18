context("txc_filter_files removes superseded service versions")

# helper to write a minimal TransXchange file with the header fields the
# filter reads
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

# the operating period a file ends up declaring, after any rewrite
period_of <- function(f) {
  x <- xml2::read_xml(f)
  svc <- xml2::xml_find_first(x, "d1:Services/d1:Service")
  c(start = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:StartDate")),
    end = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:EndDate")))
}


test_that("keeps the operative version and drops superseded revisions", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  files <- c(
    # SVA: v2 supersedes v1 (higher revision, same start date); a future
    # timetable is also present and must be kept
    make_txc(dir, "svcA_v1.xml", "SVA", "2025-01-01", "1", "2025-01-01T00:00:00"),
    make_txc(dir, "svcA_v2.xml", "SVA", "2025-01-01", "3", "2025-02-01T00:00:00"),
    make_txc(dir, "svcA_future.xml", "SVA", "2026-09-01", "1", "2026-08-01T00:00:00"),
    # SVB: same revision and start date, tie broken by newest modification time
    make_txc(dir, "svcB_a.xml", "SVB", "2025-05-18", "1", "2025-05-01T00:00:00"),
    make_txc(dir, "svcB_b.xml", "SVB", "2025-05-18", "1", "2025-06-01T00:00:00"),
    # SVC: an old timetable superseded by one that started more recently
    make_txc(dir, "svcC_old.xml", "SVC", "2024-01-01", "1", "2024-01-01T00:00:00"),
    make_txc(dir, "svcC_new.xml", "SVC", "2026-01-01", "1", "2026-01-01T00:00:00")
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-08")))

  expect_setequal(res,
                  c("svcA_v2.xml", "svcA_future.xml", "svcB_b.xml", "svcC_new.xml"))
})


test_that("reference date controls which version is operative", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  files <- c(
    make_txc(dir, "old.xml", "SVA", "2025-01-01", "1", "2025-01-01T00:00:00"),
    make_txc(dir, "new.xml", "SVA", "2026-06-01", "1", "2026-05-01T00:00:00")
  )

  # before the change only the old file is operative (new one is future, kept)
  early <- basename(txc_filter_files(files, date = as.Date("2025-06-01")))
  expect_setequal(early, c("old.xml", "new.xml"))

  # after the change the old file has been superseded and is dropped
  late <- basename(txc_filter_files(files, date = as.Date("2026-07-01")))
  expect_equal(late, "new.xml")
})


test_that("lines sharing a ServiceCode are all kept", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Nottingham City Transport's pattern: one registration, one file per line,
  # all with the same ServiceCode, start date and revision. Every line is
  # published exactly once, so nothing may be dropped.
  files <- c(
    make_txc(dir, "nct49.xml",  "NCT49", "2026-06-21", "1",
             "2026-07-21T10:07:20", lines = "49"),
    make_txc(dir, "nct49a.xml", "NCT49", "2026-06-21", "1",
             "2026-07-21T10:07:20", lines = "49A"),
    make_txc(dir, "nct49b.xml", "NCT49", "2026-06-21", "1",
             "2026-07-21T10:07:20", lines = "49B"),
    make_txc(dir, "nct49x.xml", "NCT49", "2026-06-21", "1",
             "2026-07-21T10:07:20", lines = "49X")
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_setequal(res, c("nct49.xml", "nct49a.xml", "nct49b.xml", "nct49x.xml"))
})


test_that("a repeated upload of the same lines is still dropped", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The other half of the same East Midlands pattern: the region zip carries the
  # file twice, identical but for the modification time.
  files <- c(
    make_txc(dir, "serta.xml",   "SERTA", "2026-01-04", "0",
             "2026-07-10T13:56:56", lines = "all"),
    make_txc(dir, "serta_1.xml", "SERTA", "2026-01-04", "0",
             "2026-07-10T13:56:59", lines = "all")
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_equal(res, "serta_1.xml")
})


test_that("a superseded revision is dropped even when it shares its lines", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Keeping a file per line must not resurrect a file whose every line is
  # covered by a higher revision of the same period.
  files <- c(
    make_txc(dir, "old.xml", "SVA", "2026-01-04", "1",
             "2026-01-04T00:00:00", lines = c("5", "5A")),
    make_txc(dir, "new.xml", "SVA", "2026-01-04", "4",
             "2026-02-04T00:00:00", lines = c("5", "5A"))
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_equal(res, "new.xml")
})


test_that("a revision that adds a line does not double-count the shared one", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # v1 publishes line 7; v2 of the same period publishes 7 and 7A. v2 is the
  # better file for line 7, so v1 is best for nothing and must go - otherwise
  # line 7 would be counted twice.
  files <- c(
    make_txc(dir, "v1.xml", "SVB", "2026-01-04", "1",
             "2026-01-04T00:00:00", lines = "7"),
    make_txc(dir, "v2.xml", "SVB", "2026-01-04", "2",
             "2026-02-04T00:00:00", lines = c("7", "7A"))
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_equal(res, "v2.xml")
})


test_that("a re-registration under a new ServiceCode truncates its predecessor", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The Transport for London pattern that the ServiceCode rules cannot see:
  # line 436 is re-registered under a brand new ServiceCode, so each code
  # appears exactly once, yet both files run to 2026-12-23 and overlap from
  # 25 July. The predecessor must be closed the day before the successor.
  files <- c(
    make_txc(dir, "old.xml", "45-436-_-y05-61477", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea Park Station", noc = "GAHL"),
    make_txc(dir, "new.xml", "45-436-_-y05-61478", "2026-07-25", "1",
             "2026-07-17T08:38:08", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea Park Station", noc = "GAHL")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(length(res), 2)

  per <- do.call(rbind, lapply(res, period_of))
  rownames(per) <- vapply(res, function(f) {
    if (grepl("61477", paste(readLines(f), collapse = ""))) "old" else "new"
  }, "")
  expect_equal(unname(per["old", "end"]), "2026-07-24")
  expect_equal(unname(per["old", "start"]), "2026-07-11")
  expect_equal(unname(per["new", "start"]), "2026-07-25")
  expect_equal(unname(per["new", "end"]), "2026-12-23")

  # the originals must be untouched
  expect_equal(unname(period_of(files[1])["end"]), "2026-12-23")
})


test_that("an open-ended predecessor gains an EndDate", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The Stagecoach pattern: the superseded registration declares no EndDate at
  # all, which is exactly what lets it go on being counted for ever.
  files <- c(
    make_txc(dir, "open.xml", "NW_02_SCME_PR3_1", "2026-01-04", "1",
             "2026-01-04T00:00:00", lines = "PR3",
             desc = "Preston - Lancaster", noc = "SCMY"),
    make_txc(dir, "later.xml", "NW_SC_SCMY_PR3_1", "2026-07-19", "1",
             "2026-07-19T00:00:00", lines = "PR3",
             desc = "Preston - Lancaster", noc = "SCMY")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(length(res), 2)
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  expect_true("2026-07-18" %in% ends)
})


test_that("identical periods keep the most recently created file", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Truncation cannot separate two registrations that declare the same period,
  # so the newer one wins outright.
  files <- c(
    make_txc(dir, "first.xml", "16-364-_-y05-61731", "2026-07-11", "1",
             "2026-07-17T08:33:05", lines = "364", enddate = "2026-12-23",
             desc = "Ilford - Dagenham East", noc = "GAHL",
             createtime = "2026-07-17T08:33:05"),
    make_txc(dir, "second.xml", "16-364-_-y05-61732", "2026-07-11", "1",
             "2026-07-17T08:33:07", lines = "364", enddate = "2026-12-23",
             desc = "Ilford - Dagenham East", noc = "GAHL",
             createtime = "2026-07-17T08:33:07")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(basename(res), "second.xml")
})


test_that("same start and a different end moves the longer period's start", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Successive registrations, so they are written at different instants. Two
  # files of one operator written at the SAME instant under different
  # ServiceCodes are parts of one export rather than competing registrations,
  # and are left alone - see the sibling test below.
  files <- c(
    make_txc(dir, "short.xml", "SVC_A", "2026-07-11", "1",
             "2026-07-01T00:00:00", lines = "9", enddate = "2026-08-31",
             desc = "Town - Village", noc = "OPER"),
    make_txc(dir, "long.xml", "SVC_B", "2026-07-11", "1",
             "2026-07-02T00:00:00", lines = "9", enddate = "2026-12-23",
             desc = "Town - Village", noc = "OPER")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(length(res), 2)
  starts <- vapply(res, function(f) period_of(f)[["start"]], "")
  expect_true("2026-09-01" %in% starts)
  expect_true("2026-07-11" %in% starts)
})


test_that("files written in one export are siblings, not registrations", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The First Essex pattern. Route X30 is published as four files - X30_1 and
  # X30_2_A to X30_4_A - with their own ServiceCodes but one operator, one
  # description, one line, one operating period and one CreationDateTime.
  # They carry different journeys between them, so they are one timetable
  # split across four files, not four competing registrations. Grouped as
  # usual, their periods are identical, every tie-break is equal, and the
  # rules kept one of the four: 126 of route X30's 301 vehicle journeys.
  # A successor is always written later than the file it replaces, so equal
  # creation times with different codes mean an export, and the pair is
  # skipped. Measured over the July 2026 TNDS archive this recovers 110
  # files and 2,603 vehicle journeys nationally.
  mk <- function(name, code, enddate) {
    make_txc(dir, name, code, "2026-07-26", "1", "2026-07-14T06:59:42",
             lines = "X30", enddate = enddate,
             desc = "Sweyne Park School", noc = "FESX")
  }
  files <- c(mk("X30_1.xml",   "SE_FG_FESX_X30_1", "2026-08-01"),
             mk("X30_2_A.xml", "SE_FG_FESX_X30_2", "2026-08-01"),
             mk("X30_3_A.xml", "SE_FG_FESX_X30_3", "2026-08-01"),
             mk("X30_4_A.xml", "SE_FG_FESX_X30_4", "2026-08-01"))

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)),
               c("X30_1.xml", "X30_2_A.xml", "X30_3_A.xml", "X30_4_A.xml"))
  # and none of them has had its period rewritten
  starts <- vapply(res, function(f) period_of(f)[["start"]], "")
  expect_true(all(starts == "2026-07-26"))

  # The same-start rule is skipped for siblings too, not only the
  # identical-period one: a shorter sibling must not push the longer one's
  # start past its end.
  dir2 <- tempfile("txctest")
  dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  sib <- c(
    make_txc(dir2, "a.xml", "SE_FG_FESX_C7_1", "2026-07-26", "1",
             "2026-07-14T06:59:42", lines = "C7", enddate = "2026-08-01",
             desc = "Chelmsford", noc = "FESX"),
    make_txc(dir2, "b.xml", "SE_FG_FESX_C7_2", "2026-07-26", "1",
             "2026-07-14T06:59:42", lines = "C7", enddate = "2026-12-23",
             desc = "Chelmsford", noc = "FESX")
  )
  res2 <- txc_filter_files(sib, date = as.Date("2026-07-26"))
  expect_equal(length(res2), 2)
  expect_true(all(vapply(res2, function(f) period_of(f)[["start"]], "") ==
                    "2026-07-26"))
})


# A file with real vehicle journeys: each departs `stop` at one of `times`
# ("HH:MM"), so that txc_journey_keys() has something to read.
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


test_that("siblings from different exports keep their periods", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # First Essex route 371 in the July 2026 TNDS archive: SE_FG_FESX_371_1
  # (one journey a day, exported 14 July, 26 Jul - 1 Aug) and
  # SE_FG_FESX_371_4 (the rest of the route, exported 7 July, 26 Jul - 29
  # Aug). Different exports, so the equal-CreationDateTime test does not
  # catch them, and "same start, different end" moved the main file's start
  # to 2 August - a week of the route deleted. They share no journeys.
  files <- c(
    make_txc_vj(dir, "371_1.xml", "SE_FG_FESX_371_1", "2026-07-26", "2026-08-01",
                "2026-07-14T06:59:42", "371", "Chelmsford - Danbury", "FESX",
                "1500A", "16:40"),
    make_txc_vj(dir, "371_4.xml", "SE_FG_FESX_371_4", "2026-07-26", "2026-08-29",
                "2026-07-07T07:12:01", "371", "Chelmsford - Danbury", "FESX",
                "1500A", c("06:30", "07:30", "08:30", "09:30", "10:30")))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("371_1.xml", "371_4.xml"))
  expect_true(all(vapply(res, function(f) period_of(f)[["start"]], "") ==
                    "2026-07-26"))
})


test_that("rule 2 keeps a line whose ServiceCode sibling starts later", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # Arriva Cymru, July 2026: ServiceCode "12/X12", line 12 from 7 June and
  # line X12 from 8 June. Keyed on the code, line 12 was "superseded".
  files <- c(make_txc(dir, "12.xml", "12/X12", "2026-06-07", "0",
                      "2026-07-14T14:19:51", lines = "12", noc = "ACYM"),
             make_txc(dir, "X12.xml", "12/X12", "2026-06-08", "0",
                      "2026-07-14T14:19:49", lines = "X12", noc = "ACYM"))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("12.xml", "X12.xml"))
})


test_that("rule 1 keeps siblings in one export that carry different journeys", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # Stirling UL: same ServiceCode, line, period and days, created 41 ms
  # apart, different journeys. A repeated upload would repeat them.
  mk <- function(name, cre, times) {
    make_txc_vj(dir, name, "MBLB_UL", "2026-07-10", "2027-07-09", cre, "UL",
                "Stirling - University", "MBLB", "S1", times)
  }
  files <- c(mk("a.xml", "2026-07-10T17:12:34.381", c("07:00", "08:00", "09:00")),
             mk("b.xml", "2026-07-10T17:12:34.422", c("07:30", "08:30", "09:30")))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("a.xml", "b.xml"))

  # and a true repeated upload, same journeys, is still reduced to one
  dir2 <- tempfile("txctest")
  dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  rep2 <- c(make_txc_vj(dir2, "a.xml", "MBLB_UL", "2026-07-10", "2027-07-09",
                        "2026-07-10T17:12:34", "UL", "Stirling", "MBLB", "S1",
                        c("07:00", "08:00")),
            make_txc_vj(dir2, "b.xml", "MBLB_UL", "2026-07-10", "2027-07-09",
                        "2026-07-11T17:12:34", "UL", "Stirling", "MBLB", "S1",
                        c("07:00", "08:00")))
  expect_length(txc_filter_files(rep2, date = as.Date("2026-07-26")), 1)
})


test_that("term and outside-term versions of one timetable both survive", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # Stirling UL, July 2026: one file on the university's working days, one on
  # every other day. Code, line, period and weekdays all equal; times close
  # enough that a dense timetable matches the sparse one.
  wd <- function(side) sprintf(paste0("<%s><WorkingDays><ServicedOrganisationRef>",
                                      "STU</ServicedOrganisationRef></WorkingDays></%s>"),
                               side, side)
  files <- c(
    make_txc_vj(dir, "term.xml", "MBLB_UL", "2026-07-10", "2027-07-09",
                "2026-07-10T17:12:34.422", "UL", "Stirling", "MBLB", "S1",
                c("07:07", "07:37", "08:07"), so = wd("DaysOfOperation")),
    make_txc_vj(dir, "hol.xml", "MBLB_UL", "2026-07-10", "2027-07-09",
                "2026-07-10T17:12:34.381", "UL", "Stirling", "MBLB", "S1",
                c("07:10", "07:30", "08:10"), so = wd("DaysOfNonOperation")))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("hol.xml", "term.xml"))
})


test_that("a term file naming no calendar is not a duplicate of its holiday file", {
  # Metrobus 100 and 200, November 2026 BODS: two weekday files with the
  # same journeys, period, revision and creation time. The holiday file runs
  # on days that are not school WorkingDays; the term file names no
  # organisation at all (it removes the half terms as special days). Only
  # one side carries a calendar, and the identical-period branch of rule 4
  # dropped whichever lost the tie-break - in BODS, the term file.
  wd <- "<DaysOfNonOperation><WorkingDays><ServicedOrganisationRef>Sch:2</ServicedOrganisationRef></WorkingDays></DaysOfNonOperation>"
  t0 <- c("07:10", "08:10", "09:10", "10:10")
  for (cre in list(c("2026-09-04T10:00:00", "2026-09-04T10:00:00"),
                   c("2026-09-04T10:00:00", "2026-09-04T11:00:00"),
                   c("2026-09-04T11:00:00", "2026-09-04T10:00:00"))) {
    dir <- tempfile("txctest")
    dir.create(dir)
    files <- c(
      make_txc_vj(dir, "term.xml", "PK0001213:113", "2026-09-05", "2026-12-24",
                  cre[1], "100", "Crawley - Gatwick", "METR", "S1", t0),
      make_txc_vj(dir, "hol.xml", "PK0001213:113", "2026-09-05", "2026-12-24",
                  cre[2], "100", "Crawley - Gatwick", "METR", "S1", t0, so = wd))
    res <- txc_filter_files(files, date = as.Date("2026-11-11"))
    expect_equal(sort(basename(res)), c("hol.xml", "term.xml"))
    unlink(dir, recursive = TRUE)
  }
})


test_that("files on different organisation calendars are not duplicates", {
  # Bee Network 143, November 2026 BODS: one weekday file per combination of
  # school and university terms, all DaysOfOperation/WorkingDays, same code,
  # line, period and journeys. Told apart by side alone, three of four went.
  t0 <- c("07:10", "08:10", "09:10", "10:10")
  so_ref <- function(code) paste0("<DaysOfOperation><WorkingDays><ServicedOrganisationRef>",
                                  code, "</ServicedOrganisationRef></WorkingDays></DaysOfOperation>")
  with_org <- function(f, code, ranges) {
    dr <- paste0("<DateRange><StartDate>", ranges[, 1], "</StartDate><EndDate>",
                 ranges[, 2], "</EndDate></DateRange>", collapse = "")
    x <- readLines(f)
    writeLines(sub("<Operators>", paste0(
      "<ServicedOrganisations><ServicedOrganisation><OrganisationCode>", code,
      "</OrganisationCode><WorkingDays>", dr,
      "</WorkingDays></ServicedOrganisation></ServicedOrganisations><Operators>"),
      x, fixed = TRUE), f)
    f
  }
  on_on <- cbind(c("2026-09-20", "2026-10-31"), c("2026-10-25", "2026-12-19"))
  off_on <- cbind(c("2026-09-06"), c("2026-09-19"))
  mk <- function(dir, name, code, ranges, cre = "2026-08-28T16:04:06") {
    with_org(make_txc_vj(dir, name, "PC2073428:18010423", "2026-08-30", "2031-08-30",
                         cre, "143", "Manchester - Didsbury", "BNML", "S1", t0,
                         so = so_ref(code)), code, ranges)
  }
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  files <- c(mk(dir, "onon.xml", "SchOnUniOn:6", on_on),
             mk(dir, "offon.xml", "SchOffUniOn:6", off_on))
  res <- txc_filter_files(files, date = as.Date("2026-11-11"))
  expect_equal(sort(basename(res)), c("offon.xml", "onon.xml"))

  # the same calendar under a renumbered code is still a repeated upload
  dir2 <- tempfile("txctest")
  dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  rep2 <- c(mk(dir2, "a.xml", "SchOnUniOn:6", on_on),
            mk(dir2, "b.xml", "SchOnUniOn:7", on_on, cre = "2026-08-29T16:04:06"))
  expect_length(txc_filter_files(rep2, date = as.Date("2026-11-11")), 1)
})


test_that("an open-ended version more than a year old is retired by a later one", {
  # London United 117, November 2026 BODS: open-ended files from 2023 and
  # 2025, retimed between them so the versions test calls them siblings.
  # With a long open-ended horizon the 2023 timetable ran beside the 2025 one.
  t23 <- c("06:03", "06:41", "07:19", "07:57", "08:35")
  t25 <- c("06:10", "06:30", "06:50", "07:10", "07:30")
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, start, times, days = "<MondayToFriday/>", code = "PK0002088:45") {
    open(make_txc_vj(dir, name, code, start, "2099-12-31", paste0(start, "T00:00:00"),
                     "117", "Isleworth - Staines", "LUTD", "S1", times, days = days,
                     stops = c("S1", "S2", "S3")))
  }
  files <- c(mk("y2023.xml", "2023-08-19", t23), mk("y2025.xml", "2025-12-13", t25))
  expect_equal(basename(txc_filter_files(files, date = as.Date("2026-11-11"))), "y2025.xml")
  # across a change of ServiceCode too
  files2 <- c(mk("c2023.xml", "2023-08-28", t23, code = "UZ000LUTD:290"),
              mk("c2025.xml", "2025-11-22", t25))
  expect_equal(basename(txc_filter_files(files2, date = as.Date("2026-11-11"))), "c2025.xml")
  # but a lone old file still runs, and so does one whose successor runs on other days
  expect_length(txc_filter_files(mk("lone.xml", "2023-01-01", t23), date = as.Date("2026-11-11")), 1)
  files3 <- c(mk("wk.xml", "2023-08-19", t23),
              mk("sun.xml", "2025-12-13", t25, days = "<Sunday/>"))
  expect_length(txc_filter_files(files3, date = as.Date("2026-11-11")), 2)
  # and a file less than a year old is left to the rules that compare journeys
  files4 <- c(mk("r1.xml", "2026-01-04", t23), mk("r2.xml", "2026-06-13", t25))
  expect_length(txc_filter_files(files4, date = as.Date("2026-11-11")), 2)
})


test_that("the stale rule needs a successor in force and of comparable size", {
  # Lynx 42, November 2026 BODS: an open-ended file from April 2025 and a
  # Christmas-week file for 29-31 December 2026; the second retired the
  # first, which was what ran in November. And Hodgsons 87: a 26-journey
  # weekday file from 18 November 2024 and a 6-journey file from two days
  # later, a supplement, not a version.
  t8 <- c("06:03", "06:41", "07:19", "07:57", "08:35", "09:13", "09:51", "10:29")
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, start, times, end = "2099-12-31") {
    f <- make_txc_vj(dir, name, "PF0001:42", start, end, paste0(start, "T00:00:00"),
                     "42", "Kings Lynn - Hunstanton", "LYNX", "S1", times,
                     stops = c("S1", "S2", "S3"))
    if (end == "2099-12-31") open(f) else f
  }
  # a successor for three days in December is not in force in November
  xmas <- c(mk("main.xml", "2025-04-20", t8), mk("xmas.xml", "2026-12-29", t8[1:5], end = "2026-12-31"))
  expect_length(txc_filter_files(xmas, date = as.Date("2026-11-11")), 2)
  # nor is one that has already ended
  gone <- c(mk("main.xml", "2025-04-20", t8),
            mk("gone.xml", "2025-06-01", c("06:10", "06:30", "06:50", "07:10", "07:30"), end = "2026-06-30"))
  expect_length(txc_filter_files(gone, date = as.Date("2026-11-11")), 2)
  # a successor a quarter the size is a supplement
  supp <- c(mk("main.xml", "2024-11-18", t8), mk("supp.xml", "2024-11-20", c("22:10", "23:10")))
  expect_length(txc_filter_files(supp, date = as.Date("2026-11-11")), 2)
  # one of comparable size, in force, retires the old file
  succ <- c(mk("main.xml", "2024-11-18", t8), mk("succ.xml", "2025-11-20", c("06:10", "07:10", "08:10", "09:10", "10:10")))
  expect_equal(basename(txc_filter_files(succ, date = as.Date("2026-11-11"))), "succ.xml")
})


test_that("a version that has ended is not the operative one", {
  # trentbarton 74, November 2026 BODS: an open-ended file from 30 August
  # and, under the same ServiceCode, a half-term week file for 25-31 October
  # with a subset of its journeys. The half-term file was "the latest
  # version" on 11 November and the open-ended one was deleted.
  t8 <- c("06:03", "06:41", "07:19", "07:57", "08:35", "09:13", "09:51", "10:29")
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  main <- open(make_txc_vj(dir, "main.xml", "PD0001111:81", "2026-08-30", "2099-12-31",
                           "2026-08-20T00:00:00", "74", "Derby - Nottingham", "TNXB", "S1", t8,
                           stops = c("S1", "S2", "S3")))
  half <- make_txc_vj(dir, "half.xml", "PD0001111:81", "2026-10-25", "2026-10-31",
                      "2026-10-20T00:00:00", "74", "Derby - Nottingham", "TNXB", "S1", t8[1:6],
                      stops = c("S1", "S2", "S3"))
  expect_equal(basename(txc_filter_files(c(main, half), date = as.Date("2026-11-11"))), "main.xml")
  # before the half-term week both are future-or-live and the versions test applies as before
  expect_length(txc_filter_files(c(main, half), date = as.Date("2026-10-01")), 2)
})


test_that("a new-code successor repeating the journeys is still reconciled", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The case the overlap rules exist for, now with journeys: a publisher
  # mints a new ServiceCode for a re-registration whose journeys move by a
  # minute or two. The journeys match, so the predecessor is still closed.
  t0 <- c("06:30", "07:30", "08:30", "09:30", "10:30")
  t1 <- c("06:31", "07:32", "08:30", "09:29", "10:33")
  files <- c(
    make_txc_vj(dir, "old.xml", "PB0001:1", "2026-01-01", "2099-12-31",
                "2026-01-01T09:00:00", "7", "Town - Village", "ABCD", "S1", t0),
    make_txc_vj(dir, "new.xml", "PB0001:9", "2026-07-01", "2099-12-31",
                "2026-06-20T09:00:00", "7", "Town - Village", "ABCD", "S1", t1))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  per <- lapply(res, period_of)
  names(per) <- sub("_[0-9a-f]{8}[.]xml$", ".xml", basename(res))
  expect_equal(per[["old.xml"]][["end"]], "2026-06-30")
  expect_equal(per[["new.xml"]][["start"]], "2026-07-01")
})


test_that("one publisher re-registering at one instant is still reconciled", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The sibling rule keys on the ServiceCode differing as well as the creation
  # time matching, so a re-upload of the SAME code at the same instant is
  # still a duplicate registration and one copy goes.
  files <- c(
    make_txc(dir, "dup1.xml", "SE_FG_FESX_55_1", "2026-07-26", "1",
             "2026-07-14T06:59:42", lines = "55", enddate = "2026-08-01",
             desc = "Chelmsford - Maldon", noc = "FESX"),
    make_txc(dir, "dup2.xml", "SE_FG_FESX_55_1", "2026-07-26", "2",
             "2026-07-14T06:59:42", lines = "55", enddate = "2026-08-01",
             desc = "Chelmsford - Maldon", noc = "FESX")
  )
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(basename(res), "dup2.xml")
})


test_that("correctly sequenced registrations are left alone", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The H13 case: the publisher already closed the first period the day before
  # the second begins. Nothing here is duplication and nothing may change.
  files <- c(
    make_txc(dir, "a.xml", "54-H13-_-y05-61367", "2026-07-11", "1",
             "2026-07-17T08:47:23", lines = "H13", enddate = "2026-08-31",
             desc = "Northwood Hills - Ruislip Lido", noc = "MTLN"),
    make_txc(dir, "b.xml", "54-H13-_-y05-61364", "2026-09-01", "1",
             "2026-07-17T08:47:26", lines = "H13", enddate = "2026-12-23",
             desc = "Northwood Hills - Ruislip Lido", noc = "MTLN")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_setequal(basename(res), c("a.xml", "b.xml"))
  # unchanged means the originals are returned, not rewritten copies
  expect_setequal(res, files)
})


test_that("different services sharing a line number are not merged", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Line 436 exists in London and in Hereford. Same number, same dates,
  # different operator and description: these must both survive untouched.
  files <- c(
    make_txc(dir, "london.xml", "45-436-_-y05-61477", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea Park Station", noc = "GAHL"),
    make_txc(dir, "hereford.xml", "30-436-0-y11-2", "2026-07-11", "1",
             "2026-06-01T00:00:00", lines = "436", enddate = "2026-12-23",
             desc = "Breinton - Hereford", noc = "YEOC")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_setequal(res, files)
})


test_that("resolve_overlaps = FALSE restores the old behaviour", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  files <- c(
    make_txc(dir, "old.xml", "CODE_A", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea", noc = "GAHL"),
    make_txc(dir, "new.xml", "CODE_B", "2026-07-25", "1",
             "2026-07-17T08:38:08", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea", noc = "GAHL")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"),
                          resolve_overlaps = FALSE)
  expect_setequal(res, files)
})


test_that("files with an unreadable ServiceCode are always kept", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  good <- make_txc(dir, "good.xml", "SVA", "2025-01-01", "1", "2025-01-01T00:00:00")
  bad <- file.path(dir, "broken.xml")
  writeLines("this is not valid xml <<<", bad)

  res <- basename(txc_filter_files(c(good, bad), date = as.Date("2026-01-01")))
  expect_true("broken.xml" %in% res)
  expect_true("good.xml" %in% res)
})


test_that("normalise_description ignores case, punctuation and word order", {
  a <- "Ealing Broadway/West Ruislip - Liverpool Street - Epping/Hainault/Woodford"
  b <- "West Ruislip/Ealing Broadway - Liverpool Street - Hainault/Woodford/Epping"
  expect_equal(normalise_description(a), normalise_description(b))
  expect_equal(normalise_description("Lewisham - Battersea"),
               normalise_description("lewisham, battersea"))
  # It cannot repair a misspelling, which is why fixed track drops the
  # description from the key entirely
  expect_false(identical(normalise_description(a),
                         normalise_description(sub("Broadway", "Broaddway", a))))
  expect_equal(normalise_description(c(NA, "")), c("", ""))
  # Different services stay different
  expect_false(identical(normalise_description("Lewisham - Battersea"),
                         normalise_description("Lewisham - Bromley")))
})


test_that("a named line on fixed track is grouped without its description", {
  dir <- tempfile("txctest"); dir.create(dir)
  out <- tempfile("txcout"); dir.create(out)
  on.exit(unlink(c(dir, out), recursive = TRUE))

  # Transport for London mints a new ServiceCode per re-registration and
  # retypes the description each time. One transposed letter used to put the
  # second file in a group of its own, where it had nothing to overlap with.
  files <- c(
    make_txc(dir, "cen_old.xml", "1-CEN-a", "2021-10-09", "3",
             "2021-10-11T16:10:12", lines = "Central", enddate = "2021-12-23",
             desc = "Ealing Broadway/West Ruislip - Epping", noc = "LUL",
             mode = "underground"),
    make_txc(dir, "cen_new.xml", "1-CEN-b", "2021-10-15", "3",
             "2021-10-11T16:10:47", lines = "Central", enddate = "2021-12-23",
             desc = "Ealing Broaddway/West Ruislip - Epping", noc = "LUL",
             mode = "underground")
  )

  res <- txc_filter_files(files, date = as.Date("2021-10-12"), quiet = TRUE,
                          resolve_overlaps = TRUE, out_dir = out)
  expect_equal(length(res), 2)
  periods <- do.call(rbind, lapply(res, period_of))
  periods <- periods[order(periods[, "start"]), , drop = FALSE]
  # The earlier registration is closed the day before its successor starts,
  # so no date is served by both
  expect_equal(unname(periods[1, "end"]), "2021-10-14")
  expect_equal(unname(periods[2, "start"]), "2021-10-15")
})


test_that("a bus route keeps its description in the key", {
  dir <- tempfile("txctest"); dir.create(dir)
  out <- tempfile("txcout"); dir.create(out)
  on.exit(unlink(c(dir, out), recursive = TRUE))

  # One operator can run a route "1" in two towns, so on the road two
  # different descriptions are two different services and both must survive
  # untouched.
  files <- c(
    make_txc(dir, "bus_a.xml", "CODE_A", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "1", enddate = "2026-12-23",
             desc = "Lewisham - Battersea", noc = "GAHL", mode = "bus"),
    make_txc(dir, "bus_b.xml", "CODE_B", "2026-07-25", "1",
             "2026-07-17T08:38:08", lines = "1", enddate = "2026-12-23",
             desc = "Hereford - Leominster", noc = "GAHL", mode = "bus")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"), quiet = TRUE,
                          resolve_overlaps = TRUE, out_dir = out)
  expect_equal(length(res), 2)
  periods <- do.call(rbind, lapply(res, period_of))
  expect_true(all(periods[, "end"] == "2026-12-23"))
})


test_that("a retyped description still groups on the road", {
  dir <- tempfile("txctest"); dir.create(dir)
  out <- tempfile("txcout"); dir.create(out)
  on.exit(unlink(c(dir, out), recursive = TRUE))

  # Same service, same words, different order and punctuation - which used to
  # be enough to stop it grouping
  files <- c(
    make_txc(dir, "a.xml", "CODE_A", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea", noc = "GAHL", mode = "bus"),
    make_txc(dir, "b.xml", "CODE_B", "2026-07-25", "1",
             "2026-07-17T08:38:08", lines = "436", enddate = "2026-12-23",
             desc = "Battersea, Lewisham", noc = "GAHL", mode = "bus")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"), quiet = TRUE,
                          resolve_overlaps = TRUE, out_dir = out)
  periods <- do.call(rbind, lapply(res, period_of))
  periods <- periods[order(periods[, "start"]), , drop = FALSE]
  expect_equal(unname(periods[1, "end"]), "2026-07-24")
})


test_that("two operators sharing a ServiceCode keep their own services", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # A ServiceCode is unique within an operator, not nationally. Five South East
  # operators publish ServiceCode "1", and keyed on the code alone they look
  # like one service with five competing revisions: the most recent start date
  # wins and the rest are dropped as superseded. Metrobus's Crawley town
  # network went that way - lines 1, 2, 10, 21 and 100, 876 of 2,892 journeys.
  files <- c(
    make_txc(dir, "metrobus.xml", "1", "2026-06-01", "1",
             "2026-06-01T00:00:00", lines = "1",
             desc = "Crawley - Gatwick", noc = "METR"),
    make_txc(dir, "gocoaches.xml", "1", "2026-07-01", "1",
             "2026-07-01T00:00:00", lines = "100",
             desc = "Chichester - Midhurst", noc = "GOCH")
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_setequal(res, c("metrobus.xml", "gocoaches.xml"))
})


test_that("a joint registration listing its operators in a different order is still deduplicated", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The operator code has to identify the operator, not the order the file
  # happens to list them in. Two revisions of one jointly registered service,
  # the second naming the same two operators the other way round, are still one
  # service and the superseded revision must go.
  files <- c(
    make_txc(dir, "v1.xml", "SVJ", "2026-06-01", "1", "2026-06-01T00:00:00",
             lines = "5", desc = "Town - City", noc = c("OPA", "OPB")),
    make_txc(dir, "v2.xml", "SVJ", "2026-06-01", "2", "2026-07-01T00:00:00",
             lines = "5", desc = "Town - City", noc = c("OPB", "OPA"))
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_equal(res, "v2.xml")
})


test_that("a joint registration that gains an operator is still deduplicated", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The harder half of the same case: the revision does not reorder the
  # operators, it adds one, so no key built from the operator codes can group
  # the two files. The overlap rule is what has to catch this - same
  # description, same line, same period - and it keeps the newer file.
  files <- c(
    make_txc(dir, "v1.xml", "SVK", "2026-06-01", "1", "2026-06-01T00:00:00",
             lines = "5", desc = "Town - City", noc = "OPA"),
    make_txc(dir, "v2.xml", "SVK", "2026-06-01", "2", "2026-07-01T00:00:00",
             lines = "5", desc = "Town - City", noc = c("OPA", "OPB"))
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-07-26")))
  expect_equal(res, "v2.xml")
})


test_that("one file per day type for the same line and period all survive", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Brighton & Hove publishes a registration as one document per day type:
  # ServiceCode PK0001213:1 line 2 for the week of 2026-10-04 arrives as a
  # weekday file, a Saturday file and a Sunday file at the same
  # RevisionNumber. Keyed on operator + ServiceCode + StartDate + line they
  # collided and two of the three were deleted, taking the weekend timetable
  # with them.
  files <- c(
    make_txc(dir, "wk.xml", "PK0001213:1", "2026-10-04", "47",
             "2026-10-01T00:00:00", lines = "2", desc = "Town - City",
             noc = "BHBC", enddate = "2026-10-10",
             days = c("Monday", "Tuesday", "Wednesday", "Thursday",
                      "Friday")),
    make_txc(dir, "sat.xml", "PK0001213:1", "2026-10-04", "47",
             "2026-10-01T00:00:00", lines = "2", desc = "Town - City",
             noc = "BHBC", enddate = "2026-10-10", days = "Saturday"),
    make_txc(dir, "sun.xml", "PK0001213:1", "2026-10-04", "47",
             "2026-10-01T00:00:00", lines = "2", desc = "Town - City",
             noc = "BHBC", enddate = "2026-10-10", days = "Sunday")
  )

  res <- sort(basename(txc_filter_files(files, date = as.Date("2026-10-05"))))
  expect_equal(res, c("sat.xml", "sun.xml", "wk.xml"))
})


test_that("a repeated upload of one day type is still deduplicated", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The day type must not become a licence to keep everything: two files with
  # the same day set, line and period are still one registration twice, and
  # the higher revision wins.
  files <- c(
    make_txc(dir, "v1.xml", "SVK", "2026-10-04", "1", "2026-10-01T00:00:00",
             lines = "7", desc = "Town - City", noc = "OPA",
             enddate = "2026-10-10", days = "Saturday"),
    make_txc(dir, "v2.xml", "SVK", "2026-10-04", "2", "2026-10-02T00:00:00",
             lines = "7", desc = "Town - City", noc = "OPA",
             enddate = "2026-10-10", days = "Saturday")
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-10-05")))
  expect_equal(res, "v2.xml")
})


test_that("rule 4 reconciles files whose day sets overlap", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Two ServiceCodes, so rules 1 to 3 keep both and the pair reaches the
  # overlap reconciliation. One operator, one description, one line, periods
  # that overlap, and day sets that differ but share the weekdays: these are
  # competing versions of one timetable, so the predecessor is still closed
  # the day before the successor starts. This is what makes the new test
  # disjointness rather than inequality.
  files <- c(
    make_txc(dir, "old.xml", "SVK1", "2026-09-01", "1",
             "2026-08-01T00:00:00", lines = "9", desc = "Town - City",
             noc = "OPA", enddate = "2026-12-31",
             days = "MondayToSaturday"),
    make_txc(dir, "new.xml", "SVK2", "2026-10-01", "1",
             "2026-09-15T00:00:00", lines = "9", desc = "Town - City",
             noc = "OPA", enddate = "2026-12-31", days = "MondayToFriday")
  )

  res <- txc_filter_files(files, date = as.Date("2026-10-05"))
  expect_equal(length(res), 2)
  # the truncated file is returned as a rewritten copy, so match on the stem
  expect_true(any(grepl("^old", basename(res))))
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  expect_true("2026-09-30" %in% ends)
})


test_that("rule 4 leaves files whose day sets do not intersect alone", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # The same shape as above but the day sets are complementary, so neither
  # file is a version of the other and both keep their full period. Without
  # the disjointness test the identical-period branch would have called the
  # older one a duplicate registration and deleted the Saturday service.
  files <- c(
    make_txc(dir, "wk.xml", "SVK1", "2026-09-01", "1",
             "2026-08-01T00:00:00", lines = "9", desc = "Town - City",
             noc = "OPA", enddate = "2026-12-31", days = "MondayToFriday"),
    make_txc(dir, "sat.xml", "SVK2", "2026-09-01", "1",
             "2026-09-15T00:00:00", lines = "9", desc = "Town - City",
             noc = "OPA", enddate = "2026-12-31", days = "Saturday")
  )

  res <- txc_filter_files(files, date = as.Date("2026-10-05"))
  expect_equal(sort(basename(res)), c("sat.xml", "wk.xml"))
  # untouched, so neither is rewritten and both keep the full period
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  expect_equal(unname(ends), c("2026-12-31", "2026-12-31"))
})


test_that("day sets written as a range and as single days compare equal", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # MondayToFriday and the five days listed separately are the same timetable.
  # Compared as the strings the publisher wrote, they would key apart and both
  # survive, which is how a duplicate would get back in.
  files <- c(
    make_txc(dir, "range.xml", "SVK", "2026-10-04", "1",
             "2026-10-01T00:00:00", lines = "3", desc = "Town - City",
             noc = "OPA", enddate = "2026-10-10", days = "MondayToFriday"),
    make_txc(dir, "listed.xml", "SVK", "2026-10-04", "2",
             "2026-10-02T00:00:00", lines = "3", desc = "Town - City",
             noc = "OPA", enddate = "2026-10-10",
             days = c("Monday", "Tuesday", "Wednesday", "Thursday",
                      "Friday"))
  )

  res <- basename(txc_filter_files(files, date = as.Date("2026-10-05")))
  expect_equal(res, "listed.xml")
})


test_that("expand_days_of_week resolves ranges and negations", {
  wk <- c("Friday", "Monday", "Thursday", "Tuesday", "Wednesday")
  expect_equal(expand_days_of_week("MondayToFriday"), wk)
  expect_equal(expand_days_of_week(c("Monday", "Tuesday", "Wednesday",
                                     "Thursday", "Friday")), wk)
  expect_equal(expand_days_of_week("Weekend"), c("Saturday", "Sunday"))
  expect_equal(expand_days_of_week("MondayToSunday"),
               sort(c(wk, "Saturday", "Sunday")))
  expect_equal(expand_days_of_week("NotSunday"), sort(c(wk, "Saturday")))
  expect_equal(expand_days_of_week(character(0)), character(0))
  expect_equal(expand_days_of_week(NA_character_), character(0))
  # anything unrecognised is kept as itself rather than dropped
  expect_equal(expand_days_of_week("Whenever"), "Whenever")
})


test_that("rule 2 keeps an earlier sibling that carries other journeys", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Arriva Cymru route 35 in the July 2026 TNDS archive: one file per depot,
  # one ServiceCode, line and day set, both open into 2027, written in the
  # same export. "The latest start" kept Rhyl and deleted Wrexham, whose
  # journeys the Rhyl file does not contain.
  files <- c(
    make_txc_vj(dir, "35_rhyl.xml", "35", "2026-01-25", "2027-07-01",
                "2026-07-14T14:20:34", "35", "Rhyl - Wrexham", "ACYM",
                "5110AWA", c("06:10", "08:10", "10:10")),
    make_txc_vj(dir, "35_wrexham.xml", "35", "2025-12-04", "2027-06-03",
                "2026-07-14T14:20:34", "35", "Rhyl - Wrexham", "ACYM",
                "6390ABC", c("07:00", "09:00", "11:00", "13:00")))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("35_rhyl.xml", "35_wrexham.xml"))

  # a real successor repeats the earlier version's journeys and still
  # supersedes it
  succ <- make_txc_vj(dir, "35_new.xml", "35", "2026-03-01", "2027-07-01",
                      "2026-07-14T14:20:34", "35", "Rhyl - Wrexham", "ACYM",
                      "6390ABC", c("07:01", "09:01", "11:02", "13:00"))
  res <- txc_filter_files(c(files[2], succ), date = as.Date("2026-07-26"))
  expect_equal(basename(res), "35_new.xml")
})


test_that("journeys only repeat each other on a shared day of the week", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Peter's Travel route 343: a weekday file and a Saturday file leaving at
  # the same times, one period, one export. Compared on times alone the
  # weekday file repeated the other and was dropped as a duplicate.
  tt <- c("07:15", "09:15", "11:15", "13:15", "15:15")
  files <- c(
    make_txc_vj(dir, "343_sat.xml", "PVTT_343", "2026-07-12", "2027-07-11",
                "2026-07-10T17:13:04", "343", "Kirkcaldy - Methil", "PVTT",
                "6250A", tt, days = "<Saturday/>"),
    make_txc_vj(dir, "343_mf.xml", "PVTT_343", "2026-07-12", "2027-07-11",
                "2026-07-10T17:13:04", "343", "Kirkcaldy - Methil", "PVTT",
                "6250A", tt))
  expect_equal(txc_journey_share(txc_journey_keys(files[1]),
                                 txc_journey_keys(files[2])), 0)
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("343_mf.xml", "343_sat.xml"))
})


test_that("a shorter period on fewer days does not stand the longer one down", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Transport for London's N97: an every-night file to 23 December and a
  # Saturday-only file to 19 December, same start. "Same start, different
  # end" moved the every-night file to 20 December and left Saturdays only.
  tt <- c("00:15", "01:15", "02:15", "03:15")
  files <- c(
    make_txc_vj(dir, "n97_all.xml", "48-N97-_-y05-14", "2026-07-11", "2026-12-23",
                "2026-07-17T08:41:32", "N97", "Trafalgar Square - Hammersmith",
                "GAHL", "490000173", tt, days = "<MondayToSunday/>"),
    make_txc_vj(dir, "n97_sat.xml", "48-N97-_-y05-15", "2026-07-11", "2026-12-19",
                "2026-07-17T08:41:33", "N97", "Trafalgar Square - Hammersmith",
                "GAHL", "490000173", tt, days = "<Saturday/>"))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("n97_all.xml", "n97_sat.xml"))
  expect_true(all(vapply(res, function(f) period_of(f)[["start"]], "") ==
                    "2026-07-11"))

  # a shorter file covering every day still governs while it runs
  tmp <- make_txc_vj(dir, "n97_tmp.xml", "48-N97-_-y05-16", "2026-07-11",
                     "2026-08-31", "2026-07-17T08:41:34", "N97",
                     "Trafalgar Square - Hammersmith", "GAHL", "490000173",
                     tt, days = "<MondayToSunday/>")
  res <- txc_filter_files(c(files[1], tmp), date = as.Date("2026-07-26"),
                          out_dir = file.path(dir, "rw"))
  starts <- vapply(res, function(f) period_of(f)[["start"]], "")
  expect_equal(sort(unname(starts)), c("2026-07-11", "2026-09-01"))
})


test_that("a re-timed successor on a frequent route is still a version", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # Transport for London's 195: two full-week files, one period, created two
  # seconds apart, re-timed from every 21 to every 22 minutes. Through the day
  # the times drift out of the three-minute match, so under half the journeys
  # match, but the journey count barely changes: a version, not a sibling.
  hm <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)
  files <- c(
    make_txc_vj(dir, "195_a.xml", "21-195-_-y05-61431", "2026-07-11", "2026-12-23",
                "2026-07-17T08:35:21", "195", "Charville Lane Estate - Brentford",
                "ABLO", "490004289C", hm(seq(360, 1380, by = 21))),
    make_txc_vj(dir, "195_b.xml", "21-195-_-y05-61432", "2026-07-11", "2026-12-23",
                "2026-07-17T08:35:23", "195", "Charville Lane Estate - Brentford",
                "ABLO", "490004289C", hm(seq(360, 1380, by = 22))))
  share <- txc_journey_share(txc_journey_keys(files[1]), txc_journey_keys(files[2]))
  expect_lt(share, 0.5)
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(basename(res), "195_b.xml")
})



test_that("a route taken over by another operator is not run twice", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # National Express West Midlands' route 74 passed to West Midlands Bus
  # Limited on 1 November 2026. The new operator published the same
  # timetable under its own code; NXWM's open-ended file stayed in BODS.
  stops <- c("43000955906", "43000201901", "43000203101", "43000208301")
  tt <- c("06:00", "06:20", "06:40", "07:00", "07:20", "07:40")
  old <- make_txc_vj(dir, "nxwm_74.xml", "PD0001111:43010081", "2026-08-30",
                     "2099-12-31", "2025-10-24T15:30:00", "74",
                     "Brickhouse Lane South - Colmore Circus", "TNXB",
                     stops[1], tt, stops = stops)
  new <- make_txc_vj(dir, "wmbl_74.xml", "PD2093720:43010081", "2026-11-01",
                     "2099-12-31", "2026-10-21T13:37:10", "74",
                     "Brickhouse Lane South - Colmore Circus", "WMBL",
                     stops[1], c("06:00", "06:21", "06:40", "07:01", "07:20",
                                 "07:40"), stops = stops)
  res <- txc_filter_files(c(old, new), date = as.Date("2026-11-11"),
                          out_dir = file.path(dir, "rw"))
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  names(ends) <- sub("_[0-9a-f]{8}[.]xml$", ".xml", basename(res))
  expect_equal(unname(ends[["nxwm_74.xml"]]), "2026-10-31")

  # the same line number in another town, with other stops, is left alone
  far <- make_txc_vj(dir, "other_74.xml", "PB0000001:74", "2026-11-01",
                     "2099-12-31", "2026-10-21T13:37:10", "74", "Somewhere else",
                     "ABCD", "1100X1", tt, stops = c("1100X1", "1100X2", "1100X3"))
  res <- txc_filter_files(c(old, far), date = as.Date("2026-11-11"))
  expect_equal(sort(basename(res)), c("nxwm_74.xml", "other_74.xml"))

  # two operators sharing a route at different times are left alone
  joint <- make_txc_vj(dir, "joint_74.xml", "PD9999999:74", "2026-11-01",
                       "2099-12-31", "2026-10-21T13:37:10", "74",
                       "Brickhouse Lane South - Colmore Circus", "DIAM",
                       stops[1], c("06:10", "06:30", "06:50", "07:10", "07:30",
                                   "07:50"), stops = stops)
  res <- txc_filter_files(c(old, joint), date = as.Date("2026-11-11"))
  expect_equal(sort(basename(res)), c("joint_74.xml", "nxwm_74.xml"))
})


test_that("a smaller file inside another operator's route is not a takeover", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # D&G's route 9 (many journeys) and First Potteries' route 9 (a few, at
  # D&G's times) share their stops. First's journeys are all in D&G's file,
  # but D&G's are mostly not in First's, so D&G's file stays open.
  stops <- c("3390A", "3390B", "3390C", "3390D")
  hm <- function(m) sprintf("%02d:%02d", m %/% 60, m %% 60)
  big <- make_txc_vj(dir, "dagc_9.xml", "60-9-0-y11-4", "2026-06-01", "2027-12-31",
                     "2026-07-20T00:57:31", "9", "Hanley - Newcastle", "DAGC",
                     stops[1], hm(seq(360, 1260, by = 20)), stops = stops)
  small <- make_txc_vj(dir, "fpot_9.xml", "PD0000003:9", "2026-07-19", "2027-12-31",
                       "2026-07-20T00:30:00", "9", "Hanley - Newcastle", "FPOT",
                       stops[1], hm(seq(360, 1260, by = 120)), stops = stops)
  res <- txc_filter_files(c(big, small), date = as.Date("2026-07-26"))
  expect_equal(sort(basename(res)), c("dagc_9.xml", "fpot_9.xml"))
})


test_that("a successor running on fewer days leaves the earlier file the other days", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  # West Midlands Bus routes X21/X22 in the November 2026 BODS archive: a
  # whole-week file from 1 November (revision 1) and a term-time weekday file
  # from 15 November (revision 7, no Saturday or Sunday journeys). Closing
  # the whole-week file on 14 November left the routes with no weekend
  # service at all.
  times <- sprintf("%02d:%02d", rep(7:9, each = 4), c(0, 15, 30, 45))
  wk <- make_txc_vj(dir, "x21_r1.xml", "PD2093720:43010039", "2026-11-01",
                    "2027-12-31", "2026-10-21T12:58:31", "X21",
                    "Birmingham - Ladywood", "WMBL", "43000001", times)
  sat <- make_txc_vj(dir, "sat.xml", "PD2093720:43010039", "2026-11-01",
                     "2027-12-31", "2026-10-21T12:58:31", "X21",
                     "Birmingham - Ladywood", "WMBL", "43000001", times,
                     days = "<Saturday/>")
  # splice the Saturday journeys into the whole-week file
  w <- paste(readLines(wk), collapse = "\n")
  s <- paste(readLines(sat), collapse = "\n")
  s_vj <- sub("(?s).*<VehicleJourneys>(.*)</VehicleJourneys>.*", "\\1", s, perl = TRUE)
  s_vj <- gsub("<VehicleJourneyCode>([0-9]+)<", "<VehicleJourneyCode>S\\1<", s_vj)
  writeLines(sub("</VehicleJourneys>", paste0(s_vj, "</VehicleJourneys>"), w,
                 fixed = TRUE), wk)
  unlink(sat)
  term <- make_txc_vj(dir, "x21_r7.xml", "PD2093720:43010039", "2026-11-15",
                      "2027-12-31", "2026-11-09T15:28:36", "X21",
                      "Birmingham - Ladywood", "WMBL", "43000001", times,
                      so = paste0("<DaysOfOperation><WorkingDays><ServicedOrganisationRef>",
                                  "E08000025</ServicedOrganisationRef></WorkingDays></DaysOfOperation>"))

  out <- file.path(dir, "out")
  res <- txc_filter_files(c(wk, term), date = "2026-11-11", out_dir = out,
                          quiet = TRUE)
  expect_length(res, 3)
  expect_true(term %in% res)
  periods <- t(vapply(res, function(f) {
    x <- xml2::read_xml(f)
    c(xml2::xml_text(xml2::xml_find_first(x, "//d1:OperatingPeriod/d1:StartDate")),
      xml2::xml_text(xml2::xml_find_first(x, "//d1:OperatingPeriod/d1:EndDate")),
      length(xml2::xml_find_all(x, "//d1:VehicleJourney")),
      length(xml2::xml_find_all(x, "//d1:VehicleJourney//d1:Saturday")))
  }, character(4)))
  periods <- periods[order(periods[, 1], -as.integer(periods[, 4])), ]
  # the whole-week file up to the day before the successor ...
  expect_equal(unname(periods[1, ]), c("2026-11-01", "2026-11-14", "24", "12"))
  # ... then only its Saturday journeys beside the term-time weekday file
  expect_equal(unname(periods[2, ]), c("2026-11-15", "2027-12-31", "12", "12"))
  expect_equal(unname(periods[3, ]), c("2026-11-15", "2027-12-31", "12", "0"))

  # a successor that runs every day the earlier file does still replaces it
  # (and one running Monday to Friday all year is taken to withdraw the
  # weekend - see "rule 4 reconciles files whose day sets overlap")
  term2 <- make_txc_vj(dir, "x21_r8.xml", "PD2093720:43010039", "2026-11-15",
                       "2027-12-31", "2026-11-09T15:28:36", "X21",
                       "Birmingham - Ladywood", "WMBL", "43000001", times,
                       days = "<MondayToSunday/>")
  res2 <- txc_filter_files(c(wk, term2), date = "2026-11-11",
                           out_dir = file.path(dir, "out2"), quiet = TRUE)
  expect_length(res2, 2)

  # The real term file is the smaller (217 journeys to 528): a successor
  # under half the size, but from a later export, so not a supplement. The
  # small-successor guard left both files running in full (221 X21 trips on
  # a term Tuesday for 112).
  term3 <- make_txc_vj(dir, "x21_r7s.xml", "PD2093720:43010039", "2026-11-15",
                       "2027-12-31", "2026-11-09T15:28:36", "X21",
                       "Birmingham - Ladywood", "WMBL", "43000001", times[1:9],
                       so = paste0("<DaysOfOperation><WorkingDays><ServicedOrganisationRef>",
                                   "E08000025</ServicedOrganisationRef></WorkingDays></DaysOfOperation>"))
  res3 <- txc_filter_files(c(wk, term3), date = "2026-11-11",
                           out_dir = file.path(dir, "out3"), quiet = TRUE)
  expect_length(res3, 3)
  ends3 <- vapply(res3, function(f) xml2::xml_text(xml2::xml_find_first(
    xml2::read_xml(f), "//d1:OperatingPeriod/d1:EndDate")), "")
  expect_true("2026-11-14" %in% ends3)
})


test_that("a successor adding term-time journeys still supersedes its predecessor", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # First Eastern Counties X2/X21/X22 (PF0000323:202) in the November 2026
  # BODS archive: revision 268 from 25 October with no term days, revision
  # 269 from 1 November with them and route 902 added. Rule 2 keyed on the
  # day set, ServicedOrganisation included, kept both and every journey ran
  # twice.
  times <- sprintf("%02d:%02d", rep(7:9, each = 4), c(0, 15, 30, 45))
  so_term <- paste0("<DaysOfOperation><WorkingDays><ServicedOrganisationRef>SCH",
                    "</ServicedOrganisationRef></WorkingDays></DaysOfOperation>")
  so_hol <- paste0("<DaysOfNonOperation><WorkingDays><ServicedOrganisationRef>SCH",
                   "</ServicedOrganisationRef></WorkingDays></DaysOfNonOperation>")
  old <- make_txc_vj(dir, "r268.xml", "PF0000323:202", "2026-10-25", "2027-12-31",
                     "2026-10-11T19:14:45", "X21", "Lowestoft - Norwich", "FECS",
                     "390070458", times)
  new <- make_txc_vj(dir, "r269.xml", "PF0000323:202", "2026-11-01", "2027-12-31",
                     "2026-10-25T14:17:23", "X21", "Lowestoft - Norwich", "FECS",
                     "390070458", times, so = so_term)
  x <- readLines(new)
  writeLines(sub("</Line></Lines>",
                 "</Line><Line id=\"L2\"><LineName>902</LineName></Line></Lines>",
                 x, fixed = TRUE), new)
  res <- txc_filter_files(c(old, new), date = "2026-11-11",
                          out_dir = file.path(dir, "o1"), quiet = TRUE)
  expect_equal(basename(res), "r269.xml")

  # but term and holiday versions on opposite sides of one calendar are
  # siblings, whatever their start dates
  hol <- make_txc_vj(dir, "hol.xml", "PF0000323:202", "2026-10-25", "2027-12-31",
                     "2026-10-11T19:14:45", "X21", "Lowestoft - Norwich", "FECS",
                     "390070458", times, so = so_hol)
  res2 <- txc_filter_files(c(hol, new), date = "2026-11-11",
                           out_dir = file.path(dir, "o2"), quiet = TRUE)
  expect_setequal(basename(res2), c("hol.xml", "r269.xml"))
})


test_that("a holidays-only file is not a duplicate of the Sunday file it copies", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # Arriva North East's X22 in the November 2026 BODS archive: a Sunday file
  # and a bank-holidays-only file with the same 19 journeys, one ServiceCode,
  # period and export. HolidaysOnly was read as "every day", the two as one
  # registration filed twice, and the Sunday file was dropped: no Sunday
  # service on the route.
  times <- sprintf("%02d:%02d", 9:16, 0)
  sun <- make_txc_vj(dir, "sun.xml", "PB0002717:335", "2026-08-30", "2027-12-31",
                     "2026-11-03T15:15:15", "X22", "Durham - Sunderland", "ANEA",
                     "13001234", times, days = "<Sunday/>")
  hol <- make_txc_vj(dir, "hol.xml", "PB0002717:335", "2026-08-30", "2027-12-31",
                     "2026-11-03T15:15:15", "X22", "Durham - Sunderland", "ANEA",
                     "13001234", times, days = "<Sunday/>")
  x <- readLines(hol)
  writeLines(gsub("<DaysOfWeek><Sunday/></DaysOfWeek>", "<HolidaysOnly/>", x,
                  fixed = TRUE), hol)
  res <- txc_filter_files(c(sun, hol), date = "2026-11-11",
                          out_dir = file.path(dir, "o"), quiet = TRUE)
  expect_setequal(basename(res), c("sun.xml", "hol.xml"))
})


test_that("the stale rule needs a successor over the same stops", {
  # Centrebus, July 2026 TNDS: a Lincolnshire route 24 ("Term 1", open-ended
  # from September 2024) and a Leicestershire 24 (Barrow-upon-Soar -
  # Leicester, from February 2026) under one NOC and line name, with not a
  # stop in common. The second is not a version of the first.
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, start, desc, code, stops) {
    open(make_txc_vj(dir, name, code, start, "2099-12-31", paste0(start, "T00:00:00"),
                     "24", desc, "CBNL", stops[1], c("07:10", "08:10", "09:10"),
                     stops = stops))
  }
  files <- c(mk("lincs.xml", "2024-09-01", "Term 1", "PF1056524:45", c("L1", "L2", "L3")),
             mk("leics.xml", "2026-02-23", "Barrow-Upon-Soar - Leicester", "PF1056524:LE",
                c("E1", "E2", "E3")))
  expect_length(txc_filter_files(files, date = as.Date("2026-07-26")), 2)
  # over the same stops the older one is retired
  files2 <- c(mk("old.xml", "2024-09-01", "Term 1", "PF1056524:45", c("L1", "L2", "L3")),
              mk("new.xml", "2026-02-23", "Term 1", "PF1056524:LE", c("L1", "L2", "L4")))
  expect_equal(basename(txc_filter_files(files2, date = as.Date("2026-07-26"))), "new.xml")
})


test_that("a term-time re-encoding of a plain file is still its version", {
  # TfL 364, July 2026 TNDS: 61731 (term days via the school calendar) and
  # 61732 (no calendar) over one period with the same journeys. And H13's
  # 61368 (term and holiday journeys) beside 61369 (term only): the
  # calendar tokens differ but the journeys are the same, so the pair is
  # for the journey test, not the Metrobus guard. Only a holidays-only file
  # beside a plain one, or two files on different calendars, is left alone.
  t0 <- c("07:10", "08:10", "09:10", "10:10")
  term <- "<DaysOfNonOperation><Holidays><ServicedOrganisationRef>SOId_XX-school</ServicedOrganisationRef></Holidays></DaysOfNonOperation>"
  hol <- "<DaysOfOperation><Holidays><ServicedOrganisationRef>SOId_XX-school</ServicedOrganisationRef></Holidays></DaysOfOperation>"
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  mk <- function(name, code, so, cre) {
    make_txc_vj(dir, name, code, "2026-07-11", "2026-12-23", cre, "364",
                "Ilford - Dagenham", "TFLO", "S1", t0, so = so)
  }
  # add a second block of the same journeys on the other side of the calendar
  both <- function(f, so) {
    x <- paste(readLines(f), collapse = "\n")
    vjs <- regmatches(x, regexpr("<VehicleJourneys>.*</VehicleJourneys>", x))
    extra <- gsub(term, so, vjs, fixed = TRUE)
    extra <- gsub("<VehicleJourneys>|</VehicleJourneys>", "", extra)
    writeLines(sub("</VehicleJourneys>", paste0(extra, "</VehicleJourneys>"), x, fixed = TRUE), f)
    f
  }
  files <- c(mk("plain.xml", "16-364-_-y05-61732", "", "2026-07-01T00:00:01"),
             mk("term.xml", "16-364-_-y05-61731", term, "2026-07-01T00:00:00"))
  expect_equal(basename(txc_filter_files(files, date = as.Date("2026-07-26"))), "plain.xml")
  files2 <- c(both(mk("mixed.xml", "54-H13-_-y05-61368", term, "2026-07-01T00:00:00"), hol),
              mk("termonly.xml", "54-H13-_-y05-61369", term, "2026-07-01T00:00:01"))
  expect_equal(basename(txc_filter_files(files2, date = as.Date("2026-07-26"))), "termonly.xml")
  # a holidays-only supplement beside the plain file is kept (Metrobus)
  files3 <- c(mk("plain.xml", "16-364-_-y05-61732", "", "2026-07-01T00:00:01"),
              mk("hol.xml", "16-364-_-y05-61731", hol, "2026-07-01T00:00:00"))
  expect_length(txc_filter_files(files3, date = as.Date("2026-07-26")), 2)
})


test_that("a whole timetable with a few calendar-bound extras is not one side of the calendar", {
  dir <- tempfile("txctest")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  # First Norfolk & Suffolk route 64, November 2026 BODS: an edition from 25
  # October and its successor from 1 November, both open-ended, each the
  # whole weekday timetable on DaysOfWeek alone plus extra journeys bound to
  # the school calendar - on holidays in the first, on school days in the
  # second. Read from the extras, the two sat on opposite sides of the
  # calendar and both ran on every weekday.
  so <- function(side) sprintf(paste0(
    "<DaysOfOperation><%s><ServicedOrganisationRef>SSD</ServicedOrganisationRef>",
    "</%s></DaysOfOperation>"), side, side)
  t8 <- c("06:10", "07:10", "08:10", "09:10", "10:10", "11:10", "12:10", "13:10")
  mk <- function(name, start, extra_side, extra_time, rev_time) {
    f <- make_txc_vj(dir, name, "PF0000323:354", start, "", rev_time, "64",
                     "Leiston - Aldeburgh", "FECS", "390S", t8)
    x <- make_txc_vj(dir, "tmp.xml", "PF0000323:354", start, "", rev_time, "64",
                     "Leiston - Aldeburgh", "FECS", "390S", extra_time,
                     so = so(extra_side))
    s <- paste(readLines(f), collapse = "\n")
    e <- paste(readLines(x), collapse = "\n")
    vj <- sub("(?s).*<VehicleJourneys>(.*)</VehicleJourneys>.*", "\\1", e, perl = TRUE)
    vj <- gsub("<VehicleJourneyCode>([0-9]+)<", "<VehicleJourneyCode>X\\1<", vj)
    writeLines(sub("</VehicleJourneys>", paste0(vj, "</VehicleJourneys>"), s,
                   fixed = TRUE), f)
    unlink(x)
    f
  }
  files <- c(mk("oct.xml", "2026-10-25", "Holidays", "14:40", "2026-10-19T05:40:27"),
             mk("nov.xml", "2026-11-01", "WorkingDays", "15:20", "2026-10-25T14:23:16"))
  od <- tempfile("txcout")
  res <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od)
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  names(ends) <- basename(res)
  # the successor runs on; the October edition, if kept, ends before it
  expect_true("nov.xml" %in% names(ends))
  if ("oct.xml" %in% names(ends)) expect_equal(ends[["oct.xml"]], "2026-10-31")

  # files wholly on one side of the calendar are still term and holiday
  # versions of one timetable, and both survive
  dir2 <- tempfile("txctest")
  dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  whole <- c(make_txc_vj(dir2, "term.xml", "PF0000323:354", "2026-11-01", "", "2026-10-25T14:23:16",
                         "64", "Leiston - Aldeburgh", "FECS", "390S", t8, so = so("WorkingDays")),
             make_txc_vj(dir2, "hol.xml", "PF0000323:354", "2026-11-01", "", "2026-10-25T14:23:16",
                         "64", "Leiston - Aldeburgh", "FECS", "390S", t8, so = so("Holidays")))
  expect_length(txc_filter_files(whole, date = as.Date("2026-11-11")), 2)
})


test_that("files of one registration are compared whatever their description or lines", {
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

  # London United 117: description retyped and every time changed between
  # the 2023 and December 2025 editions, both open-ended
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  f117 <- c(make_txc_vj(dir, "old.xml", "PK0002088:45", "2023-11-25", "", "2023-08-15T15:00:00",
                        "117", "Hosptial - Hounslow - Middlesex St", "LUTD", "490A", t6),
            make_txc_vj(dir, "new.xml", "PK0002088:45", "2025-12-13", "", "2025-11-24T14:08:00",
                        "117", "Ashford - Feltham - Hospital - Hounslow", "LUTD", "490A", t6b))
  e <- ends_of(run(f117))
  expect_true("new.xml" %in% names(e))
  if ("old.xml" %in% names(e)) expect_equal(e[["old.xml"]], "2025-12-12")

  # Kempsford K2: revision 3 retimes revision 2 from the same start date
  dir2 <- tempfile("txctest"); dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  fk2 <- c(make_txc_vj(dir2, "r2.xml", "PH2077104:01010005", "2026-07-27", "", "2026-08-12T12:06:00",
                       "K2", "Chocolate Quarter", "KEMT", "018A", t6),
           make_txc_vj(dir2, "r3.xml", "PH2077104:01010005", "2026-07-27", "", "2026-09-17T11:41:00",
                       "K2", "Chocolate Quarter", "KEMT", "018A", t6b))
  expect_equal(basename(run(fk2)), "r3.xml")
  # and the July file names the line "k2": still the same line
  fk2c <- c(make_txc_vj(dir2, "jul.xml", "PH2077104:01010005", "2026-07-27", "", "2026-07-22T14:55:16",
                        "k2", "Chocolate Quarter", "KEMT", "018A", t6),
            fk2[2])
  expect_equal(basename(run(fk2c)), "r3.xml")

  # First Berkshire 7: the successor drops line 7X
  dir3 <- tempfile("txctest"); dir.create(dir3)
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

  # but a supplement under the same code is not a version (Hodgsons 87)
  dir4 <- tempfile("txctest"); dir.create(dir4)
  on.exit(unlink(dir4, recursive = TRUE), add = TRUE)
  fs <- c(make_txc_vj(dir4, "main.xml", "PB1069542:87", "2024-11-18", "", "2024-11-01T10:00:00",
                      "87", "Barnard Castle - Ingleton", "HDGS", "130A", t6),
          make_txc_vj(dir4, "supp.xml", "PB1069542:87", "2024-11-20", "", "2024-11-15T10:00:00",
                      "87", "Barnard Castle - Ingleton", "HDGS", "130A", c("15:40", "16:40")))
  e <- ends_of(run(fs))
  expect_equal(sort(names(e)), c("main.xml", "supp.xml"))
  expect_equal(e[["main.xml"]], "")

  # nor are files of one export sharing a code (Stirling UL siblings)
  dir5 <- tempfile("txctest"); dir.create(dir5)
  on.exit(unlink(dir5, recursive = TRUE), add = TRUE)
  fsib <- c(make_txc_vj(dir5, "a.xml", "MBLB_UL", "2026-07-10", "2027-07-09", "2026-07-10T17:12:34.381",
                        "UL", "Stirling - University", "MBLB", "S1", t6),
            make_txc_vj(dir5, "b.xml", "MBLB_UL", "2026-07-10", "2027-07-09", "2026-07-10T17:12:34.422",
                        "UL", "Stirling - University", "MBLB", "S1", t6b))
  expect_length(run(fsib, "2026-07-26"), 2)
})

test_that("the parts of a split registration are not retimed versions of each other", {
  # West Midlands Bus publishes ServiceCode PD2093720:4 as two files per
  # edition, written minutes apart: "WA" (lines 8 and 9) and "WN" (line 9).
  # The 15 November WA must not be taken for a retimed 1 November WN.
  ta <- c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00")
  tn <- c("06:10", "07:20", "08:30", "09:40", "10:50", "13:10", "14:20", "15:30")
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  f <- c(make_txc_vj(dir, "3WA.xml", "PD2093720:4", "2026-11-01", "", "2026-10-22T14:01:18",
                     "9", "Inner Circle", "WMBL", "430A", ta),
         make_txc_vj(dir, "3WN.xml", "PD2093720:4", "2026-11-01", "", "2026-10-22T14:05:06",
                     "9", "Inner Circle", "WMBL", "430A", tn),
         make_txc_vj(dir, "5WA.xml", "PD2093720:4", "2026-11-15", "", "2026-11-09T15:49:06",
                     "9", "Inner Circle", "WMBL", "430A", ta),
         make_txc_vj(dir, "5WN.xml", "PD2093720:4", "2026-11-15", "", "2026-11-09T16:06:36",
                     "9", "Inner Circle", "WMBL", "430A", tn))
  # and numbers each file of an export as its own revision (2 and 3, 4 and 5)
  for (k in seq_along(f)) {
    s <- readLines(f[k])
    s <- gsub('RevisionNumber="1"', sprintf('RevisionNumber="%d"', k + 1), s, fixed = TRUE)
    if (k %in% c(1, 3))
      s <- sub('<Line id="L1"><LineName>9</LineName></Line>',
               '<Line id="L1"><LineName>9</LineName></Line><Line id="L2"><LineName>8</LineName></Line>',
               s, fixed = TRUE)
    writeLines(s, f[k])
  }
  res <- txc_filter_files(f, date = as.Date("2026-11-11"), out_dir = tempfile("txcout"))
  nm <- sub("_[0-9a-f]{8}[.]xml$", ".xml", basename(res))
  expect_true(all(c("3WA.xml", "5WA.xml", "3WN.xml", "5WN.xml") %in% nm))
  e <- stats::setNames(vapply(res, function(x) period_of(x)[["end"]], ""), nm)
  expect_equal(unname(e[c("3WA.xml", "3WN.xml")]), c("2026-11-14", "2026-11-14"))
})

test_that("one creation time does not make successive editions siblings", {
  # TNDS gives each edition of Stagecoach Midlands' route 85 its own
  # ServiceCode; all carry the registration's creation time (2021) and
  # differ in modification time. Only one edition may run on a given day.
  t6 <- c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00")
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  mk <- function(name, code, start, end, mod, times = t6) {
    f <- make_txc_vj(dir, name, code, start, end, "2021-12-08T12:40:36",
                     "85", "Coventry Pool Meadow - Rugby St Cross Hospital", "SCNH", "430B", times)
    s <- readLines(f)
    s <- gsub('ModificationDateTime="2021-12-08T12:40:36"',
              sprintf('ModificationDateTime="%s"', mod), s, fixed = TRUE)
    writeLines(s, f)
    f
  }
  f <- c(mk("85_1.xml", "WM_SC_SCNH_85_1", "2026-09-12", "", "2026-09-01T10:00:55"),
         mk("85_4_A.xml", "WM_SC_SCNH_85_4", "2026-10-31", "", "2026-10-12T10:41:57"),
         mk("85_2_A.xml", "WM_SC_SCNH_85_2", "2026-11-07", "2026-11-13", "2026-11-04T11:55:10"),
         mk("85_3_A.xml", "WM_SC_SCNH_85_3", "2026-11-14", "", "2026-11-04T12:27:55",
            c(t6, "13:00", "14:00", "15:00")))
  res <- txc_filter_files(f, date = as.Date("2026-11-11"), out_dir = tempfile("txcout"))
  p <- lapply(res, period_of)
  runs <- vapply(p, function(x) x[["start"]] <= "2026-11-17" &&
                   (x[["end"]] == "" || x[["end"]] >= "2026-11-17"), logical(1))
  expect_equal(sub("_[0-9a-f]{8}[.]xml$", ".xml", basename(res[runs])), "85_3_A.xml")
})

test_that("week variants told apart by excluded dates are both kept", {
  # Metrobus route 23 from 17 September 2026: one weekday file runs only the
  # week of 19 October, the other every week but that one and half term.
  # Same code, line, period, weekdays, revision and export, same journeys.
  t6 <- c("07:00", "08:00", "09:00", "10:00", "11:00", "12:00")
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  nonop <- function(r) paste0(
    "<OperatingProfile><RegularDayType><DaysOfWeek><MondayToFriday/></DaysOfWeek></RegularDayType>",
    "<SpecialDaysOperation><DaysOfNonOperation>",
    paste0("<DateRange><StartDate>", r[, 1], "</StartDate><EndDate>", r[, 2],
           "</EndDate></DateRange>", collapse = ""),
    "</DaysOfNonOperation></SpecialDaysOperation></OperatingProfile>")
  extra <- c("06:41", "14:35")
  mk <- function(name, ranges, times = t6, so = "") {
    f <- make_txc_vj(dir, name, "PK0001213:106", "2026-09-17", "2026-12-24",
                     "2026-11-04T11:44:24", "23", "Crawley - Worthing", "METR", "440A",
                     times, so = so)
    if (is.null(ranges)) return(f)
    s <- readLines(f)
    writeLines(sub("</OperatingPeriod>", paste0("</OperatingPeriod>", nonop(ranges)),
                   s, fixed = TRUE), f)
    f
  }
  week <- mk("2431441.xml", rbind(c("2026-09-17", "2026-10-16"), c("2026-10-26", "2026-12-24")),
             c(t6, extra))
  term <- mk("2431442.xml", rbind(c("2026-10-19", "2026-10-30"), c("2026-12-21", "2026-12-24")))
  # the school-day extras, whose journeys the one-week file also carries
  school <- mk("2431443.xml", NULL, extra, so = paste0(
    "<DaysOfOperation><WorkingDays><ServicedOrganisationRef>Sch:5",
    "</ServicedOrganisationRef></WorkingDays></DaysOfOperation>"))
  res <- txc_filter_files(c(week, term, school), date = as.Date("2026-11-11"))
  expect_true(all(c("2431442.xml", "2431443.xml") %in% basename(res)))
  # and a repeated upload of the same variant is still a duplicate
  dir2 <- tempfile("txctest"); dir.create(dir2)
  on.exit(unlink(dir2, recursive = TRUE), add = TRUE)
  file.copy(term, file.path(dir2, "copy.xml"))
  expect_length(txc_filter_files(c(term, file.path(dir2, "copy.xml")),
                                 date = as.Date("2026-11-11")), 1)
})

test_that("a small file repeating some journeys does not replace the timetable", {
  # Hodgsons' route 87, one export: a 26-journey weekday file and a 4-journey
  # Saturday morning file from 18 November 2024, and 87N, the six evening
  # journeys Monday to Saturday, from 20 November - the same times the
  # weekday file carries in the evening.
  eve <- c("17:56", "18:26", "18:56", "19:26", "19:56", "20:26")
  day <- sprintf("%02d:%02d", rep(6:16, each = 2), c(1, 31))[1:20]
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  mod <- "2026-11-05T12:31:47"
  f <- c(make_txc_vj(dir, "wk.xml", "PB1069542:25", "2024-11-18", "", mod,
                     "87", "Bishop Auckland - Toft Hill", "HDGS", "130A", c(day, eve)),
         make_txc_vj(dir, "sat.xml", "PB1069542:25", "2024-11-18", "", mod,
                     "87", "Bishop Auckland - Toft Hill", "HDGS", "130A",
                     c("06:46", "07:16", "07:46", "08:56"), days = "<Saturday/>"),
         make_txc_vj(dir, "eve.xml", "PB1069542:25", "2024-11-20", "", mod,
                     "87", "Bishop Auckland - Toft Hill", "HDGS", "130A", eve,
                     days = "<MondayToSaturday/>"))
  res <- txc_filter_files(f, date = as.Date("2026-11-11"), out_dir = tempfile("txcout"))
  expect_equal(sort(basename(res)), c("eve.xml", "sat.xml", "wk.xml"))
})


test_that("dated extras do not make a term or holiday file a whole timetable", {
  # morebus U3, November 2026 TNDS: a term file (43-U3-_, journeys off on
  # Bournemouth University's holidays) and a holiday file (43-U3-A, on them)
  # from 2 November 2026, same line, period, weekdays and times, written in
  # one export. The holiday file also has Christmas journeys on
  # MondayToSunday excluded from every date but one, and the term file a
  # journey that runs only in a few weeks. Taken for whole timetables they
  # cost both files their calendar side, and the holiday file was deleted.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  ref <- "<ServicedOrganisationRef>POBU</ServicedOrganisationRef>"
  term <- paste0("<DaysOfNonOperation><Holidays>", ref, "</Holidays></DaysOfNonOperation>")
  hol <- paste0("<DaysOfOperation><Holidays>", ref, "</Holidays></DaysOfOperation>")
  t6 <- c("07:35", "08:35", "09:35", "10:35", "11:35", "12:35")
  # a journey on every day but `on`
  dated <- function(on, time, code) {
    on <- as.Date(on)
    rng <- sprintf("<DateRange><StartDate>%s</StartDate><EndDate>%s</EndDate></DateRange>",
                   c("2026-11-02", format(on + 1)), c(format(on - 1), "2027-07-31"))
    paste0("<VehicleJourney><OperatingProfile><RegularDayType><DaysOfWeek>",
           "<MondayToSunday/></DaysOfWeek></RegularDayType><SpecialDaysOperation>",
           "<DaysOfNonOperation>", paste(rng, collapse = ""), "</DaysOfNonOperation>",
           "</SpecialDaysOperation></OperatingProfile><VehicleJourneyCode>", code,
           "</VehicleJourneyCode><ServiceRef>S</ServiceRef><LineRef>L1</LineRef>",
           "<JourneyPatternRef>JP1</JourneyPatternRef><DepartureTime>", time,
           ":00</DepartureTime></VehicleJourney>")
  }
  add <- function(f, vj) {
    x <- paste(readLines(f), collapse = "\n")
    writeLines(sub("</VehicleJourneys>", paste0(vj, "</VehicleJourneys>"), x, fixed = TRUE), f)
    f
  }
  mk <- function(name, code, so, cre) {
    make_txc_vj(dir, name, code, "2026-11-02", "2027-07-31", cre, "U3",
                "Southbourne - Bournemouth University", "YELL", "1900BU", t6, so = so)
  }
  files <- c(add(mk("term.xml", "43-U3-_-y10-1", term, "2026-11-05T07:08:05"),
                 dated("2026-12-21", "07:40", "X1")),
             add(mk("hol.xml", "43-U3-A-y10-1", hol, "2026-11-05T07:07:22"),
                 paste0(dated("2026-12-22", "09:10", "X1"), dated("2026-12-23", "09:10", "X2"))))
  res <- txc_filter_files(files, date = as.Date("2026-11-11"))
  expect_equal(sort(basename(res)), c("hol.xml", "term.xml"))
})


test_that("a retimed edition runs over the same stops", {
  # Yorkshire Coastliner 31X, November 2026 TNDS (TransXChange 2.5): one
  # ServiceCode, two files - Easingwold - Kirkbymoorside (modified 14
  # October 2025) and Easingwold - York (the next day), different journeys
  # and one stop in common. The day between them made each the other's
  # retimed edition, and Kirkbymoorside was deleted.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, desc, mod, times, stops) {
    open(make_txc_vj(dir, name, "31X", "2025-11-09", "2099-12-31", mod, "31X", desc,
                     "YCST", stops[1], times, stops = stops))
  }
  kirk <- c("E0", paste0("K", 1:10)); york <- c("E0", paste0("Y", 1:10))
  files <- c(mk("kirk.xml", "Easingwold - Kirkbymoorside", "2025-10-14T09:46:42",
                c("07:05", "09:05", "11:05", "13:05", "15:05", "17:05"), kirk),
             mk("york.xml", "Easingwold - York", "2025-10-15T09:28:15",
                c("06:40", "08:40", "10:40", "12:40", "14:40"), york))
  expect_equal(sort(basename(txc_filter_files(files, date = as.Date("2026-11-11")))),
               c("kirk.xml", "york.xml"))
  # over the same stops the later file is a retimed edition and replaces it
  files2 <- c(mk("old.xml", "Easingwold - Kirkbymoorside", "2025-10-14T09:46:42",
                 c("07:05", "09:05", "11:05", "13:05", "15:05", "17:05"), kirk),
              mk("new.xml", "Easingwold - Kirkbymoorside", "2025-10-15T09:28:15",
                 c("06:40", "08:40", "10:40", "12:40", "14:40"), kirk))
  expect_equal(basename(txc_filter_files(files2, date = as.Date("2026-11-11"))), "new.xml")
  # The stops the journeys call at, not the stops the file declares.
  # Hodgsons' 2024 export of route 79 declares 160 stops, every stop the
  # operator served, for a route calling at 50; the 2026 edition declares
  # 43. Compared by declared stops they were different routes and the old
  # edition ran beside the new one (20 journeys a day for 9).
  everything <- c(kirk, paste0("Z", 1:60))
  files3 <- c(open(make_txc_vj(dir, "old79.xml", "31X", "2025-11-09", "2099-12-31",
                               "2025-10-14T09:46:42", "31X", "Easingwold - Kirkbymoorside",
                               "YCST", kirk[1], c("07:05", "09:05", "11:05", "13:05", "15:05", "17:05"),
                               stops = everything, route = kirk)),
              mk("new79.xml", "Easingwold - Kirkbymoorside", "2025-10-15T09:28:15",
                 c("06:40", "08:40", "10:40", "12:40", "14:40"), kirk))
  expect_equal(basename(txc_filter_files(files3, date = as.Date("2026-11-11"))), "new79.xml")
})


test_that("editions renamed and recoded are still compared", {
  # Stagecoach Merseyside 33, November 2026 TNDS: NW_SC_SCMY_33_2 ("Maghull
  # Station - Maghull Station", modified 11 August 2026) and NW_SC_SCMY_33_1
  # ("Maghull - Maghull", 6 October), both open-ended, created 19 August
  # 2022, the same journeys. Neither description nor code matched, so they
  # were never compared and both ran. Route 31A, created the same second,
  # is another registration.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, code, start, desc, mod, line = "33",
                 times = c("07:41", "08:41", "09:41", "10:41")) {
    f <- open(make_txc_vj(dir, name, code, start, "2099-12-31", mod, line, desc,
                          "SCMY", "2800S1", times))
    x <- readLines(f)
    writeLines(sub('CreationDateTime="[^"]*"', 'CreationDateTime="2022-08-19T12:05:39"', x), f)
    f
  }
  files <- c(mk("33_2_A.xml", "NW_SC_SCMY_33_2", "2026-08-11", "Maghull Station - Maghull Station",
                "2026-08-11T11:19:36"),
             mk("33_1.xml", "NW_SC_SCMY_33_1", "2026-10-07", "Maghull - Maghull",
                "2026-10-06T14:44:46"),
             mk("31A_1.xml", "NW_SC_SCMY_31A_1", "2026-10-07", "Maghull - Maghull",
                "2026-10-06T14:44:46", line = "31A", times = c("08:05", "09:05", "10:05")))
  od <- tempfile("txcout")
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od)
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  names(ends) <- sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res), perl = TRUE)
  expect_true(all(c("33_1.xml", "31A_1.xml") %in% names(ends)))
  # the August edition, if kept at all, ends before the October one starts
  if ("33_2_A.xml" %in% names(ends)) expect_equal(ends[["33_2_A.xml"]], "2026-10-06")
})


test_that("two depots' documents for one registration are not editions of each other", {
  # First Essex route 3, November 2026 BODS: Chelmsford and Hadleigh depots
  # each publish the whole route under PF0000599:376 - interleaved times,
  # the same stops, separate documents (created 7 and 29 September 2021)
  # with their own revisions, both re-issued from 1 December. Hadleigh's
  # November file was taken for a retimed edition of Chelmsford's.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  rt <- c("CH1", "CH2", "CH3", "SO1", "SO2")
  mk <- function(name, start, cre, mod, rev, times) {
    f <- open(make_txc_vj(dir, name, "PF0000599:376", start, "2099-12-31", mod, "3",
                          "Chelmsford - Southend", "FESX", "CH1", times,
                          days = "<MondayToSaturday/>", stops = rt))
    x <- readLines(f)
    x <- sub('CreationDateTime="[^"]*"', sprintf('CreationDateTime="%s"', cre), x)
    writeLines(gsub('RevisionNumber="1"', sprintf('RevisionNumber="%d"', rev), x), f)
    f
  }
  ch <- sprintf("%02d:13", 7:18)
  ha <- sprintf("%02d:40", 6:13)
  files <- c(mk("ch_nov.xml", "2026-11-01", "2021-09-07T12:54:01", "2026-09-17T10:12:25", 697, ch),
             mk("ha_nov.xml", "2026-11-01", "2021-09-29T17:02:03", "2026-09-16T12:00:13", 413, ha),
             mk("ch_dec.xml", "2026-12-01", "2021-09-07T12:54:01", "2026-10-14T08:23:11", 708, ch),
             mk("ha_dec.xml", "2026-12-01", "2021-09-29T17:02:03", "2026-10-09T11:25:52", 414, ha))
  res <- txc_filter_files(files, date = as.Date("2026-11-11"))
  expect_setequal(sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res), perl = TRUE),
                  basename(files))
})


test_that("a shorter holiday edition from a later export is the next edition", {
  # First Somerset route 4, November 2026 BODS: weekly editions of one
  # registration (one ServiceCode, one creation time) - 13 December (41
  # journeys, revision 858), Christmas week from 20 December, Monday to
  # Thursday (20, revision 859, an hour later) and New Year from 27
  # December (33, revision 860, five minutes after Christmas). The Christmas
  # file was under half the size of the one before (taken for a supplement)
  # and the same export as New Year (taken for a part), so all three ran
  # from 27 December.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  mk <- function(name, start, mod, rev, times, days = "<MondayToSaturday/>") {
    f <- open(make_txc_vj(dir, name, "PH0004983:482", start, "2099-12-31", mod, "4",
                          "Bath - Wells", "FBOS", "0100BRP90310", times, days = days))
    x <- readLines(f)
    x <- sub('CreationDateTime="[^"]*"', 'CreationDateTime="2021-08-26T22:07:23"', x)
    writeLines(gsub('RevisionNumber="1"', sprintf('RevisionNumber="%d"', rev), x), f)
    f
  }
  wk <- sprintf("%02d:15", 6:22)
  files <- c(mk("wk38.xml", "2026-12-13", "2026-11-06T10:56:13", 858, wk),
             mk("xmas.xml", "2026-12-20", "2026-11-06T12:00:34", 859, wk[c(2, 4, 6, 8, 10, 12)],
                days = "<Monday/><Tuesday/><Wednesday/><Thursday/>"),
             mk("ny.xml", "2026-12-27", "2026-11-06T12:05:27", 860, sprintf("%02d:45", 7:18)))
  od <- tempfile("txcout")
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od)
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  names(ends) <- sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res), perl = TRUE)
  expect_equal(sort(names(ends)), c("ny.xml", "wk38.xml", "xmas.xml"))
  expect_equal(ends[["wk38.xml"]], "2026-12-19")
  expect_equal(ends[["xmas.xml"]], "2026-12-26")
})


test_that("a small part of one export does not replace the larger part", {
  # West Midlands Bus school route 705/788, November 2026 BODS: "5WA" (five
  # journeys, lines 705 and 788) and "5WB" (two, line 705), one ServiceCode,
  # one period from 15 November, written nine minutes apart with one
  # revision. WB's 07:54 passed the versions test against WA's 07:55 and
  # the identical-period tie-break deleted WA, and line 788 with it.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  wa <- open(make_txc_vj(dir, "5WA.xml", "PD2093720:43010129", "2026-11-15", "2099-12-31",
                         "2026-11-09T15:50:26", "705", "Walsall - Aldridge", "WMBL", "43000705",
                         c("07:55", "07:58", "15:17", "15:24", "15:30")))
  wb <- open(make_txc_vj(dir, "5WB.xml", "PD2093720:43010129", "2026-11-15", "2099-12-31",
                         "2026-11-09T15:59:01", "705", "Walsall - Aldridge", "WMBL", "43000705",
                         c("07:54")))
  # one registration: the creation time is shared, the modification times differ
  for (f in c(wa, wb)) {
    x <- readLines(f)
    writeLines(sub('CreationDateTime="[^"]*"', 'CreationDateTime="2026-10-21T12:58:31"', x), f)
  }
  # WA also publishes line 788
  x <- readLines(wa)
  writeLines(sub('<Line id="L1"><LineName>705</LineName></Line>',
                 '<Line id="L1"><LineName>705</LineName></Line><Line id="L2"><LineName>788</LineName></Line>',
                 x, fixed = TRUE), wa)
  res <- txc_filter_files(c(wa, wb), date = as.Date("2026-11-11"))
  expect_true("5WA.xml" %in% basename(res))
})


test_that("an operator's ended last edition confirms a retimed handover", {
  # National Express West Midlands route 96, November 2026 BODS: an
  # open-ended file from 30 August (revision 21) and a last edition for
  # 25-31 October (revision 22); West Midlands Bus runs the route from 1
  # November under its own code, retimed (61 of 100 departure times kept).
  # The ended edition is set aside as not in force, the timetables differ,
  # and National Express's open-ended file ran beside West Midlands Bus's.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  stops <- paste0("4300017600", 1:9)
  nx_t <- sprintf("%02d:10", 6:20); wm_t <- sprintf("%02d:25", 6:20)
  nx <- function(name, start, end, mod, rev) {
    f <- make_txc_vj(dir, name, "PD0001111:43010878", start, end, mod, "96",
                     "Birmingham - Chelmsley Wood", "TNXB", stops[1], nx_t, stops = stops)
    x <- readLines(f)
    x <- sub('CreationDateTime="[^"]*"', 'CreationDateTime="2025-10-24T15:51:58"', x)
    writeLines(gsub('RevisionNumber="1"', sprintf('RevisionNumber="%d"', rev), x), f)
    f
  }
  files <- c(open(nx("nx21.xml", "2026-08-30", "2099-12-31", "2026-08-18T13:48:08", 21)),
             nx("nx22.xml", "2026-10-25", "2026-10-31", "2026-10-14T14:12:04", 22),
             open(make_txc_vj(dir, "wm.xml", "PD2093720:43010878", "2026-11-01", "2099-12-31",
                              "2026-10-21T13:04:18", "96", "Birmingham - Chelmsley Wood",
                              "WMBL", stops[1], wm_t, stops = stops)))
  od <- tempfile("txcout")
  on.exit(unlink(od, recursive = TRUE), add = TRUE)
  res <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od)
  ends <- vapply(res, function(f) period_of(f)[["end"]], "")
  names(ends) <- sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res), perl = TRUE)
  expect_true("wm.xml" %in% names(ends))
  if ("nx21.xml" %in% names(ends)) expect_equal(ends[["nx21.xml"]], "2026-10-31")

  # without the ended edition the retimed files are two timetables, as before
  od2 <- tempfile("txcout")
  on.exit(unlink(od2, recursive = TRUE), add = TRUE)
  res2 <- txc_filter_files(files[c(1, 3)], date = as.Date("2026-11-11"), out_dir = od2)
  expect_length(res2, 2)

  # and the new operator need not publish the same line set: NX route 66
  # against West Midlands Bus's 66 and 66A in one file
  x <- readLines(files[3])
  writeLines(sub('<Line id="L1"><LineName>96</LineName></Line>',
                 '<Line id="L1"><LineName>96</LineName></Line><Line id="L2"><LineName>96A</LineName></Line>',
                 x, fixed = TRUE), files[3])
  od3 <- tempfile("txcout")
  on.exit(unlink(od3, recursive = TRUE), add = TRUE)
  res3 <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od3)
  ends3 <- vapply(res3, function(f) period_of(f)[["end"]], "")
  names(ends3) <- sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res3), perl = TRUE)
  if ("nx21.xml" %in% names(ends3)) expect_equal(ends3[["nx21.xml"]], "2026-10-31")
})


test_that("a new operator's copy dated before the takeover starts at the takeover", {
  # West Midlands Bus route 11/11A, November 2026 BODS: published on 21
  # October with National Express's start, 30 August, and the same 374
  # journeys. NX's last edition ended 31 October. Both ran from 30 August.
  dir <- tempfile("txctest"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  open <- function(f) { x <- readLines(f); writeLines(sub("<EndDate>2099-12-31</EndDate>", "", x, fixed = TRUE), f); f }
  stops <- paste0("4300017600", 1:9)
  nx_t <- sprintf("%02d:10", 6:20)
  nx <- function(name, start, end, mod, rev) {
    f <- make_txc_vj(dir, name, "PD0001111:43010066", start, end, mod, "11A",
                     "Outer Circle", "TNXB", stops[1], nx_t, stops = stops)
    x <- readLines(f)
    x <- sub('CreationDateTime="[^"]*"', 'CreationDateTime="2025-10-24T15:08:55"', x)
    writeLines(gsub('RevisionNumber="1"', sprintf('RevisionNumber="%d"', rev), x), f)
    f
  }
  wm <- function(name, times) open(make_txc_vj(dir, name, "PD2093720:43010066", "2026-08-30",
                                               "2099-12-31", "2026-10-21T13:16:03", "11A",
                                               "Outer Circle", "WMBL", stops[1], times,
                                               stops = stops))
  nxf <- c(open(nx("nx13.xml", "2026-08-30", "2099-12-31", "2026-08-18T14:18:35", 13)),
           nx("nx14.xml", "2026-10-25", "2026-10-31", "2026-10-09T12:53:21", 14))
  per <- function(files) {
    od <- tempfile("txcout")
    res <- txc_filter_files(files, date = as.Date("2026-11-11"), out_dir = od)
    p <- lapply(res, period_of)
    names(p) <- sub("_[0-9a-f]{8}(?=[.]xml$)", "", basename(res), perl = TRUE)
    p
  }
  p <- per(c(nxf, wm("copy.xml", nx_t)))
  expect_equal(p[["nx13.xml"]][["end"]], "2026-10-31")
  expect_equal(p[["copy.xml"]][["start"]], "2026-11-01")

  # another operator already running its own journeys on the route is not
  # a successor
  p2 <- per(c(nxf, wm("joint.xml", sprintf("%02d:40", 6:20))))
  expect_equal(p2[["joint.xml"]][["start"]], "2026-08-30")
  expect_false(isTRUE(p2[["nx13.xml"]][["end"]] == "2026-10-31"))
})
