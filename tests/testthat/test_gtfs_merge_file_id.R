# file_id must identify the input feed, identically in every table.
#
# It used to be assigned per table, numbering 1..n over only the feeds that
# contained that table. A feed missing one table therefore shifted the
# numbering of every feed after it, for that table alone, and the joins keyed
# on c("file_id", "service_id") matched one feed's rows against another feed's
# services. A TNDS snapshot triggers it because the NCSD coach archive carries
# no calendar_dates.
#
# The fixture deliberately gives every feed the SAME service_id, route_id and
# trip_id. Regional TNDS feeds are converted independently and number their
# services from 1, so their ids collide; that collision is what makes
# gtfs_merge renumber at all, and the renumbering is what consumes file_id.
# With distinct ids per feed the renumbering is skipped and the defect cannot
# be reached, so a fixture using them passes on the broken code too.

make_feed <- function(tag, cancel = NULL) {
  out <- list(
    agency = data.frame(agency_id = paste0(tag, "_ag"),
                        agency_name = tag,
                        agency_url = "http://example.com",
                        agency_timezone = "Europe/London",
                        agency_lang = "en", stringsAsFactors = FALSE),
    stops = data.frame(stop_id = paste0(tag, "_s", 1:2),
                       stop_name = paste(tag, "stop", 1:2),
                       stop_lat = c(51.5, 51.6), stop_lon = c(-0.1, -0.2),
                       stringsAsFactors = FALSE),
    routes = data.frame(route_id = "1",
                        agency_id = paste0(tag, "_ag"),
                        route_short_name = tag, route_long_name = tag,
                        route_type = 3L, stringsAsFactors = FALSE),
    trips = data.frame(trip_id = "1", route_id = "1", service_id = "1",
                       stringsAsFactors = FALSE),
    stop_times = data.frame(trip_id = c("1", "1"),
                            arrival_time = c("09:00:00", "09:10:00"),
                            departure_time = c("09:00:00", "09:10:00"),
                            stop_id = paste0(tag, "_s", 1:2),
                            stop_sequence = 1:2, stringsAsFactors = FALSE),
    calendar = data.frame(service_id = "1",
                          monday = 1L, tuesday = 1L, wednesday = 1L,
                          thursday = 1L, friday = 1L, saturday = 0L,
                          sunday = 0L,
                          start_date = as.Date("2018-05-07"),
                          end_date = as.Date("2018-06-29"),
                          stringsAsFactors = FALSE))
  if (!is.null(cancel)) {
    out$calendar_dates <- data.frame(service_id = "1",
                                     date = as.Date(cancel),
                                     exception_type = 2L,
                                     stringsAsFactors = FALSE)
  }
  out
}

# which merged service each feed's trip ended up on, and what that service
# is cancelled on
trace_feeds <- function(m, tags) {
  trips <- as.data.frame(m$trips)
  st <- as.data.frame(m$stop_times)
  cd <- as.data.frame(m$calendar_dates)
  # the stop ids are the only per-feed marker that survives the merge
  first_stop <- st[!duplicated(st$trip_id), c("trip_id", "stop_id")]
  trips <- merge(trips, first_stop, by = "trip_id")
  trips$tag <- sub("_s[0-9]+$", "", as.character(trips$stop_id))
  res <- lapply(tags, function(tg) {
    svc <- as.character(trips$service_id[trips$tag == tg])
    d <- cd$date[as.character(cd$service_id) %in% svc]
    if (length(d) == 0) NA_character_ else format(as.Date(d), "%Y-%m-%d")
  })
  names(res) <- tags
  res
}


test_that("an exception stays on the feed that declared it", {
  # feed b has no calendar_dates - the NCSD case
  feeds <- list(make_feed("a", "2018-05-16"),
                make_feed("b"),
                make_feed("c", "2018-05-30"))
  m <- UK2GTFS::gtfs_merge(feeds, force = TRUE, quiet = TRUE)
  got <- trace_feeds(m, c("a", "b", "c"))

  expect_equal(got$a, "2018-05-16")
  expect_true(is.na(got$b))      # declared nothing, must have acquired nothing
  expect_equal(got$c, "2018-05-30")
})


test_that("where the incomplete feed sits does not change the answer", {
  want <- c(a = "2018-05-16", b = "2018-05-23", c = "2018-05-30")
  for (gap in 1:3) {
    tags <- c("a", "b", "c")
    feeds <- lapply(seq_along(tags), function(i) {
      make_feed(tags[i], if (i == gap) NULL else want[[tags[i]]])
    })
    m <- UK2GTFS::gtfs_merge(feeds, force = TRUE, quiet = TRUE)
    got <- trace_feeds(m, tags)
    for (i in seq_along(tags)) {
      tg <- tags[i]
      if (i == gap) {
        expect_true(is.na(got[[tg]]),
                    info = paste("gap at", gap, "- feed", tg,
                                 "acquired an exception it never declared"))
      } else {
        expect_equal(got[[tg]], want[[tg]],
                     info = paste("gap at", gap, "- feed", tg))
      }
    }
  }
})
