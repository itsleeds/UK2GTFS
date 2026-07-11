context("txc_filter_files removes superseded service versions")

# helper to write a minimal TransXchange file with the header fields the
# filter reads
make_txc <- function(dir, name, service, startdate, rev, modtime) {
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="%s">
  <Services>
    <Service RevisionNumber="%s" ModificationDateTime="%s">
      <ServiceCode>%s</ServiceCode>
      <OperatingPeriod><StartDate>%s</StartDate></OperatingPeriod>
    </Service>
  </Services>
</TransXChange>', modtime, modtime, rev, rev, modtime, service, startdate)
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
