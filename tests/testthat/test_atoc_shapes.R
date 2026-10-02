# Tests for the geometry helpers in atoc_shapes.R.
#
# ATOC_shapes() itself routes every station pair over the whole UK rail network
# with dodgr, which is far too slow for a unit test. The helpers it is built
# from are pure functions, so they are tested directly: the Szudzik pair key,
# turning a dodgr path into a line, reversing a line, the cheap length
# measure, and assembling shapes.txt for one trip.

context("ATOC shape building helpers")

a_line <- function(...) {
  sf::st_linestring(matrix(c(...), ncol = 2, byrow = TRUE))
}


test_that("od_id_szudzik gives one key per unordered pair", {
  from <- c("KNGX", "YORK", "KNGX")
  to <- c("YORK", "KNGX", "LEEDS")
  key <- od_id_szudzik(from, to)

  expect_type(key, "double")
  # KNGX-YORK and YORK-KNGX are the same pair of stations
  expect_equal(key[1], key[2])
  expect_false(key[1] == key[3])
  expect_length(unique(od_id_szudzik(c("A", "B"), c("B", "A"))), 1L)
})


test_that("od_id_szudzik can keep the direction", {
  key <- od_id_szudzik(c("KNGX", "YORK"), c("YORK", "KNGX"),
                       ordermatters = TRUE)
  expect_false(key[1] == key[2])

  # and distinct pairs stay distinct in either mode
  from <- c("A", "A", "B", "C")
  to <- c("B", "C", "C", "A")
  expect_length(unique(od_id_szudzik(from, to, ordermatters = TRUE)), 4L)
})


test_that("od_id_szudzik accepts factors and rejects ragged input", {
  chr <- od_id_szudzik(c("A", "B"), c("B", "A"))
  fct <- od_id_szudzik(factor(c("A", "B")), factor(c("B", "A")))
  expect_equal(fct, chr)

  expect_error(od_id_szudzik(c("A", "B"), "B"),
               "x and y are not of equal length")
})


test_that("path_to_sf turns a list of vertex ids into a line", {
  verts <- data.frame(id = c("v1", "v2", "v3", "v4"),
                      x = c(-1.50, -1.55, -1.60, -1.70),
                      y = c(53.80, 53.85, 53.90, 54.00),
                      stringsAsFactors = FALSE)

  path <- path_to_sf(c("v1", "v2", "v3"), verts = verts)
  expect_s3_class(path, "LINESTRING")
  expect_equal(nrow(path), 3)
  expect_equal(unname(path[, 1]), c(-1.50, -1.55, -1.60))
  expect_equal(unname(path[, 2]), c(53.80, 53.85, 53.90))

  # vertices are looked up by id, not by position
  back <- path_to_sf(c("v3", "v1"), verts = verts)
  expect_equal(unname(back[, 1]), c(-1.60, -1.50))
})


test_that("path_to_sf can simplify away redundant vertices", {
  # v1, v2 and v3 are on a straight line, so v2 carries no information
  verts <- data.frame(id = c("v1", "v2", "v3"),
                      x = c(-1.50, -1.55, -1.60),
                      y = c(53.80, 53.85, 53.90),
                      stringsAsFactors = FALSE)

  plain <- path_to_sf(c("v1", "v2", "v3"), verts = verts)
  simple <- path_to_sf(c("v1", "v2", "v3"), verts = verts, simplify = TRUE)

  expect_equal(nrow(plain), 3)
  expect_equal(nrow(simple), 2)
  expect_s3_class(simple, "LINESTRING")
  # the ends are kept exactly
  expect_equal(unname(simple[1, ]), c(-1.50, 53.80), tolerance = 1e-6)
})


test_that("path_to_sf returns NA when routing found no path", {
  verts <- data.frame(id = "v1", x = -1.5, y = 53.8, stringsAsFactors = FALSE)
  expect_true(is.na(path_to_sf(character(0), verts = verts)))
  expect_true(is.na(path_to_sf(NULL, verts = verts)))
})


test_that("invert_linestring reverses a line end to end", {
  line <- a_line(-1.50, 53.80, -1.55, 53.85, -1.60, 53.90)
  rev <- invert_linestring(line)

  expect_s3_class(rev, "LINESTRING")
  expect_equal(unname(rev[, 1]), c(-1.60, -1.55, -1.50))
  expect_equal(unname(rev[, 2]), c(53.90, 53.85, 53.80))
  # reversing twice gets the original back
  expect_equal(sf::st_coordinates(invert_linestring(rev)),
               sf::st_coordinates(line))
})


test_that("st_length_cheap measures each line in metres", {
  lines <- sf::st_sf(
    id = 1:2,
    geometry = sf::st_sfc(
      a_line(-1.50, 53.80, -1.50, 53.81),                 # about 1.1 km
      a_line(-1.50, 53.80, -1.50, 53.81, -1.50, 53.82),   # about 2.2 km
      crs = 4326))

  lgth <- st_length_cheap(lines)

  expect_length(lgth, 2)
  expect_type(lgth, "double")
  expect_null(names(lgth))
  # compared against the same distance function ATOC_shapes uses elsewhere
  one_step <- sum(geodist::geodist(
    matrix(c(-1.50, 53.80, -1.50, 53.81), ncol = 2, byrow = TRUE),
    sequential = TRUE))
  expect_equal(lgth[1], one_step, tolerance = 1e-6)
  expect_equal(lgth[2], 2 * one_step, tolerance = 1e-3)
})


test_that("st_length_cheap gives a trailing empty geometry zero length", {
  # the first call of a trip has no incoming leg, so empty geometries reach
  # this function and must not shorten the result
  lines <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      a_line(-1.50, 53.80, -1.50, 53.81),
      a_line(-1.50, 53.81, -1.50, 53.82),
      sf::st_linestring(),
      crs = 4326))

  lgth <- st_length_cheap(lines)
  expect_length(lgth, 3)
  expect_equal(lgth[3], 0)
  expect_gt(lgth[1], 1000)
})


# ---- match_lines -----------------------------------------------------------

# One trip's worth of calls, each with the line it travelled to get there
trip_calls <- function(n = 3) {
  lat <- 53.80 + seq(0, by = 0.01, length.out = n + 1)
  geom <- lapply(seq_len(n), function(i) {
    a_line(-1.50, lat[i], -1.50, lat[i + 1])
  })
  out <- data.frame(
    trip_id = "T1",
    arrival_time = sprintf("1%d:00:00", seq_len(n) - 1),
    departure_time = sprintf("1%d:01:00", seq_len(n) - 1),
    stop_id = paste0("S", seq_len(n)),
    stop_sequence = seq_len(n),
    pickup_type = 0L, drop_off_type = 0L,
    stringsAsFactors = FALSE)
  out$geometry <- sf::st_sfc(geom, crs = 4326)
  out
}


test_that("match_lines returns shapes and stop_times for one trip", {
  res <- match_lines(trip_calls(3))

  expect_named(res, c("shapes", "stop_times"))

  shapes <- res$shapes
  expect_named(shapes, c("shape_pt_lon", "shape_pt_lat", "trip_id",
                         "shape_pt_sequence", "shape_dist_traveled"))
  expect_equal(shapes$trip_id, rep("T1", nrow(shapes)))
  expect_equal(shapes$shape_pt_sequence, seq_len(nrow(shapes)))
  # repeated vertices where one leg ends and the next begins are dropped
  expect_false(any(duplicated(shapes[, c("shape_pt_lon", "shape_pt_lat")])))
  # distance along the shape only ever grows, and starts at zero
  expect_equal(shapes$shape_dist_traveled[1], 0)
  expect_false(is.unsorted(shapes$shape_dist_traveled))

  st <- res$stop_times
  expect_named(st, c("trip_id", "arrival_time", "departure_time", "stop_id",
                     "stop_sequence", "pickup_type", "drop_off_type",
                     "shape_dist_traveled"))
  expect_equal(nrow(st), 3)
  expect_false(inherits(st, "sf"))
  expect_false(is.unsorted(st$shape_dist_traveled))
})


test_that("match_lines handles a trip with only two calls", {
  res <- match_lines(trip_calls(2))

  expect_equal(nrow(res$stop_times), 2)
  expect_gt(nrow(res$shapes), 1)
  expect_equal(unique(res$shapes$shape_pt_lon), -1.50)
})


test_that("match_lines skips a trip it could not route, with a warning", {
  calls <- trip_calls(3)
  calls$geometry[2] <- sf::st_sfc(sf::st_linestring(), crs = 4326)

  expect_warning(res <- match_lines(calls),
                 "Trip T1 has no geometry, skipping")
  expect_null(res)
})


test_that("match_lines refuses a trip with a single call", {
  expect_error(match_lines(trip_calls(1)),
               "unknown number of geometries")
})
