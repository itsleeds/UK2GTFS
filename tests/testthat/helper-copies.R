# Fixtures for test_gtfs_deduplicate_copies.R and test_conversion_report.R


# Route `rid` of service `code` (written into route_desc as transxchange2gtfs
# does) with one trip per entry of `deps`: first stop `from`, a middle stop,
# then `to`. `mid_offset` is the minutes to the middle stop, so two copies can
# be given different interpolated times.
copies_route <- function(rid, agency, code, deps, service, from = "A",
                         to = "C", mid_offset = 10, short = "X6",
                         route_type = 3L) {
  tid <- paste0(rid, "_", seq_along(deps))
  secs <- function(hhmm, add = 0) {
    t <- as.POSIXct(paste("2026-01-01", hhmm), tz = "UTC") + add * 60
    format(t, "%H:%M:%S")
  }
  list(
    routes = data.frame(route_id = rid, agency_id = agency,
                        route_short_name = short, route_long_name = "",
                        route_desc = paste0("[ServiceCode: ", code, "]"),
                        route_type = route_type, stringsAsFactors = FALSE),
    trips = data.frame(route_id = rid, service_id = service, trip_id = tid,
                       stringsAsFactors = FALSE),
    stop_times = data.frame(
      trip_id = rep(tid, each = 3),
      arrival_time = as.vector(rbind(secs(deps), secs(deps, mid_offset),
                                     secs(deps, 30))),
      departure_time = as.vector(rbind(secs(deps), secs(deps, mid_offset),
                                       secs(deps, 30))),
      stop_id = rep(c(from, "B", to), length(deps)),
      stop_sequence = rep(1:3, length(deps)),
      stringsAsFactors = FALSE))
}

copies_gtfs <- function(..., calendar) {
  parts <- list(...)
  pick <- function(nm) do.call(rbind, lapply(parts, `[[`, nm))
  list(
    agency = data.frame(agency_id = c("AMID", "AMD", "GAHL", "METR"),
                        agency_name = c("Arriva", "Arriva Midlands",
                                        "London General", "METROBUS LIMITED"),
                        agency_url = "http://example.com",
                        agency_timezone = "Europe/London",
                        stringsAsFactors = FALSE),
    stops = data.frame(stop_id = c("A", "B", "C", "D"), stop_name = c("a", "b", "c", "d"),
                       stop_lat = 52, stop_lon = -1.5, stringsAsFactors = FALSE),
    routes = pick("routes"), trips = pick("trips"),
    stop_times = pick("stop_times"),
    calendar = calendar,
    calendar_dates = data.frame(service_id = character(),
                                date = as.Date(character()),
                                exception_type = integer(),
                                stringsAsFactors = FALSE))
}

cal_row <- function(service, start, end, days = c(1, 1, 1, 1, 1, 0, 0)) {
  data.frame(service_id = service,
             monday = days[1], tuesday = days[2], wednesday = days[3],
             thursday = days[4], friday = days[5], saturday = days[6],
             sunday = days[7],
             start_date = as.Date(start), end_date = as.Date(end),
             stringsAsFactors = FALSE)
}

deps10 <- sprintf("%02d:00", 7:16)

# trips running on a date, per route
runs_on <- function(gtfs, date) {
  d <- as.integer(as.Date(date))
  sd <- service_operating_dates(gtfs)
  tr <- as.data.frame(gtfs$trips)
  tr <- tr[tr$service_id %in% sd$service_id[sd$date == d], ]
  table(factor(tr$route_id, levels = unique(gtfs$routes$route_id)))
}

