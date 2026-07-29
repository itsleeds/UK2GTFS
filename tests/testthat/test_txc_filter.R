context("txc_filter_files removes superseded service versions")

# helper to write a minimal TransXchange file with the header fields the
# filter reads
make_txc <- function(dir, name, service, startdate, rev, modtime,
                     lines = character(0)) {
  lines_xml <- if (length(lines)) {
    paste0("<Lines>",
           paste0(sprintf("<Line id=\"L%s\"><LineName>%s</LineName></Line>",
                          seq_along(lines), lines), collapse = ""),
           "</Lines>")
  } else {
    ""
  }
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="%s">
  <Services>
    <Service RevisionNumber="%s" ModificationDateTime="%s">
      <ServiceCode>%s</ServiceCode>
      %s
      <OperatingPeriod><StartDate>%s</StartDate></OperatingPeriod>
    </Service>
  </Services>
</TransXChange>', modtime, modtime, rev, rev, modtime, service, lines_xml,
    startdate)
  f <- file.path(dir, name)
  writeLines(xml, f)
  f
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
