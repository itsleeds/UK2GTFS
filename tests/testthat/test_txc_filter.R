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
                        line, desc, noc, stop, times, so = "") {
  # `so`, if given, is a ServicedOrganisationDayType body, e.g.
  # "<DaysOfOperation><WorkingDays><ServicedOrganisationRef>STU</ServicedOrganisationRef></WorkingDays></DaysOfOperation>"
  so_xml <- if (nzchar(so)) paste0("<ServicedOrganisationDayType>", so,
                                   "</ServicedOrganisationDayType>") else ""
  vj <- paste0(sprintf(paste0(
    "<VehicleJourney><OperatingProfile><RegularDayType><DaysOfWeek>",
    "<MondayToFriday/></DaysOfWeek></RegularDayType>", so_xml, "</OperatingProfile>",
    "<VehicleJourneyCode>%d</VehicleJourneyCode><ServiceRef>%s</ServiceRef>",
    "<LineRef>L1</LineRef><JourneyPatternRef>JP1</JourneyPatternRef>",
    "<DepartureTime>%s:00</DepartureTime></VehicleJourney>"),
    seq_along(times), service, times), collapse = "")
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="1">
  <Operators><Operator><NationalOperatorCode>%s</NationalOperatorCode></Operator></Operators>
  <JourneyPatternSections><JourneyPatternSection id="JPS1">
    <JourneyPatternTimingLink><From><StopPointRef>%s</StopPointRef></From>
    <To><StopPointRef>X</StopPointRef></To></JourneyPatternTimingLink>
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
</TransXChange>', createtime, createtime, noc, stop, createtime, service, desc,
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
