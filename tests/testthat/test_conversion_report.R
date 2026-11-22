# Tests for the record of what a conversion decided: txc_filter_files()'s
# per-file report and conversion_report() / write_conversion_report().
# make_txc() is in helper-make_txc.R, the copies fixtures in helper-copies.R.

test_that("the filter report says why each file went, and for which", {
  dir <- tempfile("txcrep")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  files <- c(
    # rule 1: same period, a higher revision wins
    make_txc(dir, "svcA_v1.xml", "SVA", "2025-01-01", "1", "2025-01-01T00:00:00"),
    make_txc(dir, "svcA_v2.xml", "SVA", "2025-01-01", "3", "2025-02-01T00:00:00"),
    # rule 2: superseded by the version operative on the date
    make_txc(dir, "svcC_old.xml", "SVC", "2024-01-01", "1", "2024-01-01T00:00:00"),
    make_txc(dir, "svcC_new.xml", "SVC", "2026-01-01", "1", "2026-01-01T00:00:00"),
    # rule 4: a re-registration under a new code closes its predecessor
    make_txc(dir, "old436.xml", "45-436-_-y05-61477", "2026-07-11", "1",
             "2026-07-17T08:38:06", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea Park Station", noc = "GAHL"),
    make_txc(dir, "new436.xml", "45-436-_-y05-61478", "2026-07-25", "1",
             "2026-07-17T08:38:08", lines = "436", enddate = "2026-12-23",
             desc = "Lewisham - Battersea Park Station", noc = "GAHL"),
    # rule 4: identical periods, the newer file wins outright
    make_txc(dir, "first.xml", "16-364-_-y05-61731", "2026-07-11", "1",
             "2026-07-17T08:33:05", lines = "364", enddate = "2026-12-23",
             desc = "Ilford - Dagenham East", noc = "GAHL"),
    make_txc(dir, "second.xml", "16-364-_-y05-61732", "2026-07-11", "1",
             "2026-07-17T08:33:07", lines = "364", enddate = "2026-12-23",
             desc = "Ilford - Dagenham East", noc = "GAHL"))
  res <- txc_filter_files(files, date = as.Date("2026-07-26"))
  rep <- attr(res, "filter_report")
  expect_equal(nrow(rep), length(files))
  row <- function(f) rep[rep$file == f, ]

  expect_equal(row("svcA_v1.xml")$decision, "dropped")
  expect_equal(row("svcA_v1.xml")$rule, "1")
  expect_equal(row("svcA_v1.xml")$by, "svcA_v2.xml")

  expect_equal(row("svcC_old.xml")$rule, "2")
  expect_equal(row("svcC_old.xml")$by, "svcC_new.xml")
  expect_match(row("svcC_old.xml")$reason, "2026-07-26")

  expect_equal(row("old436.xml")$decision, "truncated")
  expect_equal(row("old436.xml")$new_end, as.Date("2026-07-24"))
  expect_equal(row("old436.xml")$by, "new436.xml")
  expect_match(row("old436.xml")$reason, "^staggered")
  # the report points at the rewritten copy that is actually converted
  expect_true(row("old436.xml")$path %in% res)

  expect_equal(row("first.xml")$decision, "dropped")
  expect_equal(row("first.xml")$rule, "4")
  expect_equal(row("first.xml")$by, "second.xml")

  kept <- rep$file[rep$decision %in% c("kept", "truncated")]
  expect_setequal(kept, c("svcA_v2.xml", "svcC_new.xml", "old436.xml",
                          "new436.xml", "second.xml"))
})

test_that("unreadable files appear in the report as kept without checks", {
  dir <- tempfile("txcrep")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  bad <- file.path(dir, "bad.xml")
  writeLines("<not xml", bad)
  ok <- make_txc(dir, "ok.xml", "SVA", "2025-01-01", "1", "2025-01-01T00:00:00")
  rep <- attr(txc_filter_files(c(bad, ok), date = as.Date("2026-07-26")),
              "filter_report")
  expect_equal(rep$decision[rep$file == "bad.xml"], "kept")
  expect_match(rep$reason[rep$file == "bad.xml"], "unreadable")
})

test_that("reports are built, combined on merge, and written", {
  g <- copies_gtfs(
    copies_route("em", "AMID", "X6", deps10, "S1", mid_offset = 10),
    copies_route("wm", "AMD", "6-X6-0-y11-2", deps10, "S2", mid_offset = 12),
    calendar = rbind(cal_row("S1", "2026-08-22", "2027-01-02"),
                     cal_row("S2", "2026-09-01", "2027-01-02")))
  # no report on a feed that did not come from transxchange2gtfs
  expect_null(conversion_report(gtfs_deduplicate_copies(g, quiet = TRUE)))

  r <- conversion_report_new(list(path_in = "x.zip", stop_locations = "naptan_fill"))
  r$files <- data.frame(file = c("a.xml", "b.xml"), decision = c("kept", "dropped"),
                        rule = c("", "2"), reason = c("", "superseded: x"),
                        by = c("", "a.xml"), stringsAsFactors = FALSE)
  attr(g, "conversion_report") <- r
  g2 <- gtfs_deduplicate_copies(g, quiet = TRUE)
  cr <- conversion_report(g2)
  expect_s3_class(cr, "uk2gtfs_conversion_report")
  expect_equal(nrow(cr$copies), 1)
  expect_true("gtfs_deduplicate_copies" %in% cr$steps$step)
  expect_output(print(cr), "Copies of one service")

  # merging stacks the inputs' reports with the part they came from
  m <- conversion_report_combine(list(A = g2, B = g))
  expect_setequal(unique(m$files$part), c("A", "B"))
  expect_equal(nrow(m$files), 4)
  expect_true("gtfs_merge" %in% m$steps$step)

  out <- tempfile("rep")
  on.exit(unlink(out, recursive = TRUE))
  paths <- write_conversion_report(g2, out)
  expect_true(all(file.exists(paths)))
  expect_true(file.exists(file.path(out, "files.csv")))
  md <- readLines(file.path(out, "README.md"))
  expect_true(any(grepl("superseded", md)))
})
