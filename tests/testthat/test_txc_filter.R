context("txc_filter_files removes superseded service versions")

# helper to write a minimal TransXchange file with the header fields the
# filter reads
make_txc <- function(dir, name, service, startdate, rev, modtime,
                     lines = character(0), enddate = NULL, desc = NULL,
                     noc = NULL, createtime = NULL) {
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
  noc_xml <- if (is.null(noc)) "" else sprintf(
    "<Operators><Operator><NationalOperatorCode>%s</NationalOperatorCode></Operator></Operators>",
    noc)
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
      <OperatingPeriod><StartDate>%s</StartDate>%s</OperatingPeriod>
    </Service>
  </Services>
</TransXChange>', createtime, modtime, rev, noc_xml, rev, modtime, service,
    desc_xml, lines_xml, startdate, end_xml)
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

  files <- c(
    make_txc(dir, "short.xml", "SVC_A", "2026-07-11", "1",
             "2026-07-01T00:00:00", lines = "9", enddate = "2026-08-31",
             desc = "Town - Village", noc = "OPER"),
    make_txc(dir, "long.xml", "SVC_B", "2026-07-11", "1",
             "2026-07-01T00:00:00", lines = "9", enddate = "2026-12-23",
             desc = "Town - Village", noc = "OPER")
  )

  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  expect_equal(length(res), 2)
  starts <- vapply(res, function(f) period_of(f)[["start"]], "")
  expect_true("2026-09-01" %in% starts)
  expect_true("2026-07-11" %in% starts)
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
