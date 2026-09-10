
context("Get the example transXchange files")
file_path <- file.path(tempdir(),"uk2gtfs_tests")
dir.create(file_path)
data_path <- file.path(tempdir(),"uk2gtfs_data")
dir.create(data_path)

test_that("test transxchange data is there", {
  expect_true(dl_example_file(data_path, "transxchange"))
  expect_true(file.exists(file.path(data_path, "transxchange.zip")))
})


# Download Data
cal = get_bank_holidays()
naptan = get_naptan()

test_that("test file downloads", {
  expect_true("data.frame" %in% class(cal))
  expect_true("data.frame" %in% class(naptan))
})

test_that("test transxchange2gtfs singlecore", {
  gtfs <- transxchange2gtfs(path_in = file.path(data_path,"transxchange.zip"),
                            cal = cal,
                            naptan = naptan,
                            ncores = 1,
                            try_mode = FALSE,
                            force_merge = TRUE,
                            silent = FALSE)
  gtfs_write(gtfs,folder = file_path, name = "txc_gtfs2")
  expect_true(file.exists(file.path(file_path,"txc_gtfs2.zip")))

  # Every route carries the ServiceCode it was converted from, so two files
  # describing one line can be told apart once in the feed
  expect_true(all(grepl("[ServiceCode: ", gtfs$routes$route_desc, fixed = TRUE)))
  codes <- sub("^.*[[]ServiceCode: ", "", gtfs$routes$route_desc)
  codes <- sub("[]].*$", "", codes)
  expect_true(all(nzchar(codes)))
})

# mulicore test failing on windows in GitHub Actions
if(.Platform$OS.type == "unix") {
  test_that("test transxchange2gtfs multicore", {
    gtfs <- transxchange2gtfs(path_in = file.path(data_path,"transxchange.zip"),
                              cal = cal,
                              naptan = naptan,
                              ncores = 2,
                              try_mode = FALSE,
                              force_merge = TRUE,
                              silent = FALSE)
    gtfs_write(gtfs,folder = file_path, name = "txc_gtfs")
    expect_true(file.exists(file.path(file_path,"txc_gtfs.zip")))

  })
}

context("Test GTFS manipulation")
gtfs <- gtfs_read(file.path(file_path, "txc_gtfs2.zip"))

test_that("Can read GTFS", {
  expect_true(class(gtfs) == "list")
})

test_that("Can clean GTFS", {
  gtfs2 <- gtfs_clean(gtfs)
  expect_true(class(gtfs2) == "list")
})

test_that("Can subset GTFS", {
  bounds <- sf::st_polygon(list(rbind(c(-0.148989, 52.123243),
                                  c(-0.148989,52.617931),
                                  c(0.760130, 52.617931),
                                  c(0.760130,52.123243),
                                  c(-0.148989, 52.123243))))
  bounds <- sf::st_as_sfc(list(bounds))
  bounds <- sf::st_as_sf(data.frame(id = 1,
                                    geometry = bounds),
                         crs = 4326)

  gtfs4 <- gtfs_clip(gtfs, bounds)
  expect_true(class(gtfs4) == "list")
})


test_that("Can compress GTFS", {
  gtfs5 <- gtfs_compress(gtfs)
  expect_true(class(gtfs5) == "list")
  expect_true(as.numeric(object.size(gtfs5)) < as.numeric(object.size(gtfs)))
})


test_that("Can split GTFS", {
  gtfs6 <- gtfs_split(gtfs)
  expect_true(class(gtfs6) == "list")
  expect_true(length(gtfs6) == 2)
})


# test_that("Can find fast GTFS", {
#   tripids <- gtfs_fast_trips(gtfs, maxspeed = 15)
#   expect_true(length(tripids) > 1)
#   gtfs7 <- gtfs_split_ids(gtfs, tripids)
#   expect_true(class(gtfs7) == "list")
#   expect_true(length(gtfs7) == 2)
# })
