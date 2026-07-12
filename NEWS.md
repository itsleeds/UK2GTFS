# UK2GTFS 0.4.0

## New features

* Fares support. GTFS fare tables (both the original `fare_attributes`/
  `fare_rules` specification and GTFS Fares v2) can now be built from:
  * the National Rail fares feed (RSPS5045) via `atoc_fares_read()` and
    `gtfs_add_railfares()`, including child/railcard discounts and optional
    scenario snapshots (`travel_date`/`travel_time`/`booking_date`), also
    available directly from `atoc2gtfs()` via the new `fares*` arguments.
  * Bus Open Data Service NeTEx fare files via `netex_read_fares()`,
    `netex_match_routes()`, `gtfs_add_fares()` and the
    `netex_fares_from_archive()` wrapper, with parallel reading for the
    national archive.
* `atoc2gtfs()` gains `shapes = TRUE`: heavy rail services are routed over an
  internal map of the UK rail network to build `shapes.txt` (with `shape_id`
  in `trips` and `shape_dist_traveled` in `stop_times`), using the
  `ATOC_shapes()` function, which can also be run on an existing gtfs object.
* New vignettes: *Adding Fares* (NeTEx) and *NPTDR to GTFS*; expanded ATOC,
  GTFS and TransXChange vignettes.
* `transxchange2gtfs()` gains `filter_duplicate_files`/`filter_date` (and the
  underlying `txc_filter_files()`) to drop superseded revisions of the same
  service when converting archives such as the BODS change archive, and now
  extracts nested zip archives automatically.
* TransXChange services containing several `Line`s now produce one GTFS route
  per line, with journeys assigned via their `LineRef`.
* `gtfs_stop_frequency()` and `gtfs_trips_per_zone()` now support
  frequency-based services (`frequencies.txt`): every departure implied by a
  frequency window is counted, in its correct time band. `gtfs_trim_dates()`
  keeps the `frequencies` table consistent with the trimmed trips.
* The subsetting and cleaning functions (`gtfs_clip()`, `gtfs_trim_dates()`,
  `gtfs_clean()`, `gtfs_force_valid()`) now keep all the optional tables
  consistent with the subset feed: shapes, frequencies, transfers, pathways,
  the GTFS v1 fare tables (fare_attributes/fare_rules) and the GTFS Fares v2
  tables (areas, stop_areas, networks, route_networks, fare_leg_rules,
  fare_transfer_rules, fare_products, rider_categories, fare_media).
* `gtfs_compress()` now also rewrites the ids referenced by shapes,
  frequencies, pathways, stop_areas, fare_rules and route_networks (it
  previously only handled the core tables and transfers), and compresses
  `shape_id`s.
* `gtfs_validate_internal()` has been rewritten as a comprehensive validator:
  it checks required tables/columns for every GTFS table (including the fare
  tables), duplicated primary keys, referential integrity of every foreign
  key, coordinate ranges, enum values, colour/currency/date/time formats,
  time ordering along trips, calendar logic and feed logic, and reports at
  Error/Warning/Note severities. It now invisibly returns a data frame of
  the problems found.

## Bug fixes

* `importMCA()` reads TIPLOC Delete (TD) records correctly and parses
  association dates as yymmdd per RSPS5046.
* `station2transfers()` no longer emits transfers with missing stop ids, and
  writes integer `transfer_type`/`min_transfer_time`.
* `gtfs_clean()`, `gtfs_force_valid()` and `gtfs_compress()` now keep
  `transfers.txt` consistent with the stops table.
* `gtfs_merge()` no longer drops all but one `calendar_dates` exception per
  service when condensing service patterns.
* `gtfs_stop_frequency()` and `gtfs_trips_per_zone()` apply `calendar_dates`
  exceptions with correct GTFS semantics (no more negative trip counts).
* `gtfs_write()` accepts plain data.frames as well as data.tables, and writes
  unknown stop times as empty fields instead of `"NA:NA:NA"`.
* `gtfs_interpolate_times()` no longer fails when some trips contain NA times,
  and returns `stop_times` as a data.frame (Period columns are not safe to
  row-subset in a data.table).
* `get_naptan()` returns numeric coordinates.
* NPTDR conversion handles HHMM times and empty exception tables.
* Package state is kept in an internal cache environment instead of
  modifying locked namespace bindings; `load_data()` loads into the caller's
  environment instead of the global environment.
