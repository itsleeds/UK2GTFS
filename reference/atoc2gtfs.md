# ATOC to GTFS

Convert ATOC CIF files to GTFS

## Usage

``` r
atoc2gtfs(
  path_in,
  silent = TRUE,
  ncores = 1,
  locations = "tiplocs",
  agency = "atoc_agency",
  shapes = FALSE,
  transfers = TRUE,
  missing_tiplocs = TRUE,
  working_timetable = FALSE,
  public_only = TRUE,
  fares = NULL,
  fares_version = 1,
  fares_ticket_codes = NULL,
  fares_ticket_class = "standard",
  fares_ticket_type = NULL,
  fares_walkup_only = TRUE,
  fares_rider_categories = c("adult", "child"),
  fares_railcards = NULL,
  fares_ndf = TRUE,
  fares_travel_date = NULL,
  fares_travel_time = NULL,
  fares_booking_date = NULL
)
```

## Arguments

- path_in:

  Character, path to ATOC file e.g."C:/input/ttis123.zip"

- silent:

  Logical, should progress messages be suppressed (default TRUE)

- ncores:

  Numeric, When parallel processing how many cores to use (default 1)

- locations:

  where to get tiploc locations (see details)

- agency:

  where to get agency.txt (see details)

- shapes:

  Logical, should shapes.txt be generated (default FALSE)

- transfers:

  Logical, should transfers.txt be generated (default TRUE)

- missing_tiplocs:

  Logical, if true will check for any missing tiplocs against the main
  file and add them.(default TRUE)

- working_timetable:

  Logical, should WTT times be used instead of public times (default
  FALSE)

- public_only:

  Logical, only return calls/services that are for public passenger
  pickup/set down (default TRUE)

- fares:

  Character, optional path to a National Rail fares feed (e.g.
  "RJFAF756.zip", see
  [`atoc_fares_read()`](https://itsleeds.github.io/UK2GTFS/reference/atoc_fares_read.md)).
  If provided, GTFS fare tables are built and added to the result
  (default NULL, no fares).

- fares_version:

  Numeric, `1` for the original GTFS fares tables or `2` for GTFS Fares
  v2 (default 1). See
  [`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md)
  for what each supports.

- fares_ticket_codes:

  Character, optional explicit ticket codes to convert, passed to
  [`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md)
  as `ticket_codes`.

- fares_ticket_class:

  Character, "standard" and/or "first" (default "standard").

- fares_ticket_type:

  Character, any of "single", "return", "season". Default: "single" for
  v1, singles and returns for v2.

- fares_walkup_only:

  Logical, keep only walk-up Anytime/Off-Peak/ Super Off-Peak tickets
  (default TRUE), see
  [`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md).

- fares_rider_categories:

  Character, any of "adult", "child"; GTFS Fares v2 only (default both).

- fares_railcards:

  Character, optional railcard codes (e.g. "YNG"); GTFS Fares v2 only
  (default NULL).

- fares_ndf:

  Logical, include non-derivable fare overrides (default TRUE).

- fares_travel_date:

  Date, optional: convert fares as a scenario snapshot for a journey on
  this date, applying the feed's date/time restriction data. See
  [`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md).

- fares_travel_time:

  Character "HH:MM", optional departure time for the scenario (drops
  e.g. Off-Peak tickets at peak times). Requires `fares_travel_date`.

- fares_booking_date:

  Date, optional: when the ticket is bought. Includes Advance tickets
  bookable at that horizon, at their tier prices. Requires
  `fares_travel_date`.

## Value

A gtfs object: a named list of data frames representing the tables of a
GTFS file (agency, stops, routes, trips, stop_times, calendar,
calendar_dates, and optionally transfers and fare tables)

## Details

Locations

The .msn file contains the physical locations of stations and other
TIPLOC codes (e.g. junctions). However, the quality of the locations is
often poor only accurate to about 1km and occasionally very wrong.
Therefore, the UK2GTFS package contains an internal dataset of the
TIPLOC locations with better location accuracy, which are used by
default.

However you can also specify `locations = "file"` to use the TIPLOC
locations in the ATOC data or provide an SF data frame of your own.

Or you can provide your own sf data frame of points in the same format
as `tiplocs` or a path to a csv file formatted like a GTFS stops.txt

Agency

The ATOC files do not contain the necessary information to build the
agency.txt file. Therefore this data is provided with the package. You
can also pass your own data frame of agency information.

Fares

The timetable feed contains no fares; these come in a separate fares
feed available from the same National Rail Data Portal. Pass its path as
`fares` to add GTFS fare tables to the output. See
[`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md)
for the fare model, the choices exposed by the `fares_*` arguments, and
the limitations.

Shapes

The ATOC data does not contain any shape information. If
`shapes = TRUE`, the function will attempt to build shapes.txt from an
internal map of the rail network.

## See also

Other main:
[`nr2gtfs()`](https://itsleeds.github.io/UK2GTFS/reference/nr2gtfs.md)
