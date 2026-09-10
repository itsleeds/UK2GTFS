# UK2GTFS 0.4.0

## Breaking changes

* Coach services are now coded with GTFS extended route type **200** instead
  of 3 (bus). This affects `transxchange2gtfs()` (e.g. the TNDS NCSD national
  coach archive) and `nptdr2gtfs()` (ATCO-CIF `COACH` vehicle types), and
  matches the coding used by the DfT's Bus Open Data Service GTFS feeds.
  Analyses that previously relied on coach being folded into
  `route_type == 3` should now select `route_type %in% c(3, 200)`.
* `gtfs_merge()` now always returns `stop_times` and `frequencies` time
  columns as lubridate Periods (the class `gtfs_read()` produces), whatever
  mix of classes the inputs used.

## New features

* `txc_filter_files()` now reconciles registrations of the same service whose
  operating periods overlap, controlled by `resolve_overlaps` (default TRUE).
  Its existing rules key on `ServiceCode`, which catches a re-upload of one
  registration but not a re-registration: some publishers, Transport for London
  among them, mint a new `ServiceCode` each time a service is re-registered, so
  every code appears once and nothing is detected even though both files
  describe the same service over overlapping dates. Files are now additionally
  grouped by `NationalOperatorCode` + `Description` + the lines they publish,
  and overlaps resolved by truncating the operating period rather than deleting
  a file: where a later registration replaces an earlier one the earlier
  `EndDate` is closed the day before the successor starts, and where two share a
  start date the longer one's `StartDate` moves past the shorter. Only where the
  periods are identical, and truncation cannot separate them, is a file dropped
  (the most recently created is kept). A file whose period is emptied is
  dropped; where one period lies wholly inside another both are kept and
  reported, as a single `OperatingPeriod` cannot express a gap. Because this
  works from declared identity and declared validity rather than from journey
  times, it cannot merge two services that merely run at similar times. Files
  whose period was truncated are written as copies to `out_dir`; the originals
  are never modified. This also removes the previous limitation that a future
  timetable and the currently operative file were both counted once the future
  timetable began.
* `gtfs_deduplicate()` removes journeys a feed describes more than once. A
  copy is removed only when the whole itinerary matches - every stop, arrival,
  departure and boarding code - and every date it runs is also run by a copy
  that is kept, so no date loses service. `match_route`, `match_operator` and
  `match_block` control how much of the route has to agree. The defaults
  ignore `block_id` (feeds routinely fill it with a per-revision hash, so two
  copies of one journey never agree on it) and group operators by name rather
  than by `agency_id` (one operator is regularly filed under several agency
  records).
* `get_noc()` downloads Traveline's National Operator Codes register, and
  `noc_operator_key()` uses it to resolve GTFS agency records to the company
  they belong to - by operator code where the feed carries one, otherwise by
  matching the trading, licence or legal name. `gtfs_deduplicate()` accepts it
  as `match_operator = "noc"`, which joins records sharing neither an id nor a
  name: TNDS files Stagecoach London both as `ELBG` and, where the operator
  reference was never resolved to a code, as `IF` "EAST LONDON BUS & COACH
  COMPANY LIMITED".
* `operator_mode_overrides()` lists operators known to declare the wrong
  `Mode` in their TransXChange registrations, and `transxchange2gtfs()` now
  applies it. The first entry is Nottingham Express Transit, whose tramway is
  registered as a bus. `clean_route_type()` is now case insensitive and
  understands `trolleyBus` (GTFS route type 11).
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

* Every converter now applies one set of mode rules, so a `route_type` means
  the same thing whichever source a feed came from. The sources disagree with
  each other and with themselves: the Docklands Light Railway is metro in
  NPTDR and heavy rail in TNDS, the Glasgow Subway is metro in NPTDR and a
  tram in TNDS, the Gatwick inter-terminal shuttle is metro in five of eight
  TNDS snapshots and a tram in the rest, and the Bluebell Railway is a tram in
  the 2018 snapshot, heavy rail in 2021 and a bus in 2025. NPTDR is worse
  still: its "METRO" vehicle type is a catch-all covering the Underground,
  every British tramway, the airport people movers and a long tail of heritage
  railways alike, and it files the London Underground as a bus in 2004 and
  Sheffield Supertram as a bus in most years.

  `standard_mode_overrides()` settles it: heritage and minor railways are rail
  (2); the London Underground, the Tyne and Wear Metro, the Glasgow Subway and
  the Docklands Light Railway are metro (1); the airport people movers and the
  street and segregated tramways are trams (0). It keys on the NaPTAN stop
  names, which name the system and are spelt the same way in every source and
  every year, where operator codes are not - Manchester Metrolink appears in
  NPTDR as 1973, 1976, 2001, 2016 and 2024 in successive archives, and some of
  those codes carry ordinary bus routes as well. A route is reassigned only
  where at least 80% of the stops it calls at belong to one system, a
  threshold a bus passing a tram stop never reaches; three systems whose stops
  cannot identify them (the Underground, the Birmingham Air-Rail Link and the
  Weardale Railway) are matched on their operator code instead.

  Applied by `transxchange2gtfs()`, `nptdr2gtfs()` and `atoc2gtfs()`. On the
  October 2025 TNDS snapshot it moves the DLR to metro (14,683 trips), the
  Glasgow Subway to metro (870), the Gatwick shuttle to tram (492) and four
  heritage railways to rail; on the 2004 NPTDR archive it moves 87 London
  Underground routes out of the bus totals. No bus operator is touched by any
  of it.

* `txc_filter_files(resolve_overlaps = TRUE)` now groups files on a normalised
  `Description`, and leaves the description out of the key altogether for a
  named line on fixed track. It grouped on the raw string, and publishers
  retype the description with each re-registration: Transport for London's four
  October 2021 Central line files say "Ealing Broadway", "Ealing Broaddway" and
  list the same places in two different orders, so three of the four sat in
  groups of their own with nothing to overlap with, and the line converted into
  the feed twice over the same dates. Case, punctuation and word order are now
  ignored, and where the `Mode` is not a bus, coach or trolleybus the operator
  code and line name identify the service on their own - a named line on rails
  is unique to its operator in a way a bus route number is not. Bus and coach
  keep the description, because one operator really can run a route "1" in two
  towns.

* `transxchange2gtfs()` now writes the TransXChange `ServiceCode` into
  `routes.route_desc`, as a `[ServiceCode: ...]` suffix. It is the only thing
  that says which registration a route was converted from, and without it two
  files describing one line are indistinguishable in the feed.

* `gtfs_deduplicate()` gains `fixed_track`, the `route_type`s whose journeys
  are identified by their two termini and the times there rather than by every
  call in between - tram, metro and rail (`c(0, 1, 2)`) by default. Two trains
  of one line cannot leave the same terminus at the same minute of the same day
  and arrive at the same terminus at the same minute and still be two trains,
  so on fixed track the exact itinerary test asks for more than the railway
  requires: an operator publishing one line twice writes the copies from
  different working timetables, and they differ by a minute here and a call
  there without being two journeys. Where Transport for London published the
  Central line twice over the same dates, the exact test removed none of the
  October 2025 duplication and this removes all of it. Buses are deliberately
  not in the default, because a bus route's own vehicles do run a minute apart
  and the same relaxation there would delete real service. Every other test is
  unchanged, including the operating-date test, so nothing is removed that
  would leave a date with less service. Pass `fixed_track = integer(0)` for the
  previous behaviour.

* `transxchange2gtfs()` no longer blanks a `route_short_name` longer than six
  characters. The name was removed to satisfy a validator notice that
  `route_short_name` should be short, but GTFS sets no length limit and the
  notice is advisory, while the blanking silently exempted the route from
  deduplication: `gtfs_deduplicate()` groups routes by operator, mode and
  `route_short_name`, and an unnamed route has to stand alone, so two copies of
  one line could never be compared. Bus route numbers are short and were
  rarely affected, but line names are not - between 88% and 95% of London
  Underground trips in every TNDS snapshot sat on a route blanked this way, and
  every Underground line name over six characters was lost (Central,
  Piccadilly, Metropolitan, Bakerloo, District, Northern, Victoria, Jubilee),
  sparing only Circle. The abbreviations and the space removal that shorten a
  name are unchanged, so no route that already had a name gets a different one;
  routes that had none now carry the published line name.

* `transxchange_import()` no longer rejects a `ServicedOrganisation` that
  carries descriptive elements alongside `WorkingDays`/`Holidays`. The
  structure check only allowed `OrganisationCode`, `Name`, `WorkingDays`,
  `Holidays` and `ParentServicedOrganisationRef`, so a valid
  `PrivateCode`, `PostalAddress`, `ServicedOrganisationClassification`,
  `NatureOfOrganisation`, `PhaseOfEducation`, `ContactPerson` or
  `ContactTelephoneNumber` failed the whole file with "Unknown Structure in
  ServicedOrganisations". An unrecognised element is now tolerated unless it
  contains dates, which would mean operating dates were being dropped
  silently. On the July 2026 BODS TransXChange archive this recovered 61
  files, mostly school and coach services.
* `transxchange_import()` no longer fails on XML comments inside a
  `JourneyPatternSection`. Timing links were counted with
  `xml_length(only_elements = FALSE)`, which counts comment nodes too, so
  `JPS_id` came back longer than every other column and the file failed with
  "arguments imply differing number of rows". Recovered 7 files in the same
  archive.
* `transxchange_import()` returns `NULL` with a warning for a file whose
  `<VehicleJourneys/>` element is empty, instead of failing with "replacement
  has 1 row, data has 0". Such a file has no trips to convert.
* `transxchange2gtfs()` treats a `ServicedOrganisationDayType/DaysOfOperation`
  reference as restrictive. It means the journey runs *only* on that
  organisation's dates — typically a school's holidays — but it was converted
  by leaving the weekly calendar untouched and adding `exception_type = 1`
  rows on those dates. Under GTFS semantics an added date on a day the
  calendar already operates does nothing, so the journey ran every week
  alongside the term-time journey it was meant to replace, and it also gained
  service on days of the week it never runs (a Saturday-only journey was
  forced to run on weekdays and Sundays inside the term ranges). The journey
  is now clipped to the span the organisation's ranges cover, with the days
  between ranges excluded, by the new `include_trips()` — the mirror of the
  existing `exclude_trips()`. `SpecialDaysOperation/DaysOfOperation` remains
  additive, as it should: it means "also run on these extra dates". On the
  TNDS London route 69 for February 2026 this takes an ordinary weekday from
  574 journeys to the published 286, leaves half-term at 288 and Saturday and
  Sunday unchanged at 272 and 204, and reduces `calendar_dates.txt` because
  the redundant additions are gone.
* `gtfs_trim_dates()` no longer discards services defined only in
  `calendar_dates.txt`. The GTFS specification allows a `service_id` with no
  `calendar.txt` row, in which case it runs on exactly the dates added with
  `exception_type = 1`. Such services (and their trips, stop times and
  exceptions) were dropped outright, so anything downstream of the trim
  under-counted them as zero — including `gtfs_trips_per_zone()` and
  `gtfs_stop_frequency()`, which trim before counting. The DfT's Bus Open
  Data Service GTFS relies on these services for school-holiday timetables:
  in its 2026-02-04 national feed 1,544 services carrying 4.9% of all trips
  have no `calendar.txt` row, and over a 28-day February window that removed
  4.3% of counted bus journeys across 4,793 of 13,153 bus routes. Services
  whose added dates all fall outside the window are still dropped, as before.
* `importMCA()` reads TIPLOC Delete (TD) records correctly and parses
  association dates as yymmdd per RSPS5046.
* `station2transfers()` no longer emits transfers with missing stop ids, and
  writes integer `transfer_type`/`min_transfer_time`.
* `gtfs_clean()`, `gtfs_force_valid()` and `gtfs_compress()` now keep
  `transfers.txt` consistent with the stops table.
* `gtfs_interpolate_times()` only splits and processes the trips that
  actually contain duplicated stop times. It previously split every trip in
  the feed into its own data frame, which built lists of millions of small
  tibbles for national feeds and exhausted memory when dispatched to
  parallel workers.
* `nptdr2gtfs()` reads ATCO-CIF files as Latin-1: under a UTF-8 locale,
  accented characters in place names produced invalid UTF-8 strings that
  aborted the import.
* ATOC calendar overlays that cross a Monday–Sunday week boundary no longer
  crash (`makeAllOneDay()`) or select operating dates outside the entry's
  own date range (`makeAllOneDay()` and `expandAllWeeks()` counted weeks
  from the raw duration instead of the Monday-aligned weeks the entry
  touches). This also affected the splitting of multi-day cancellations.
* `gtfs_merge()` no longer corrupts lubridate Period time columns:
  `data.table::rbindlist()` silently truncated the S4 columns produced by
  `gtfs_read()`, so merging feeds read from disk aborted with a vctrs size
  mismatch (or worse, mis-assigned times). Time-of-day columns are now
  normalised to seconds for the merge and restored to Periods afterwards.
* `gtfs_read()` now reads `frequencies.txt` with proper types (character
  `trip_id`, Period `start_time`/`end_time`), and coerces `*_id` columns of
  non-core tables to character so numeric-looking ids still join against the
  core tables.
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
