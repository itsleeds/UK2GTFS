# Read a National Rail (DTD/RSPS5045) fares feed

Reads the fixed-width fares files distributed by the Rail Delivery Group
via the [National Rail Data
Portal](https://opendata.nationalrail.co.uk/) (the "Fares" download,
e.g. `RJFAF756.zip`), as documented in RSPS5045. These are the same
files used by ticket issuing systems and cover every advertised
point-to-point fare in Great Britain.

## Usage

``` r
atoc_fares_read(path, date = Sys.Date(), silent = TRUE)
```

## Arguments

- path:

  character, path to the fares zip file (e.g. `"RJFAF756.zip"`) or to a
  folder containing the extracted files.

- date:

  Date (or something coercible), keep records valid on this date.
  Default [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- silent:

  logical, suppress progress messages (default TRUE).

## Value

A named list of data.tables: `flow`, `fare`, `cluster`, `location`,
`group`, `group_member`, `ticket_type`, `ndf`, `status`,
`status_discount`, `railcard`, `route`, `toc`, `advance` and the
restriction tables `restriction_dates`, `restriction`,
`restriction_date_band`, `time_restriction` and
`time_restriction_date_band`.

## Details

Only the files needed to build GTFS fares are read: FFL (flows and
prices), FSC (station clusters), LOC (locations and group stations), TTY
(ticket types), NFO (non-derivable fare overrides), DIS (status
discounts), RLC (railcards), RTE (fare routes), TOC (operator codes),
RST (date/time restrictions) and TAP (advance purchase booking
horizons).

Records are filtered to those valid on `date`: the feed contains current
and future fares rounds, so changing `date` selects the fares round in
force on that day. A date outside the fares rounds present in the feed
raises a warning (the feed is a snapshot - reading it for a long-past or
far-future date does not give the prices of that day), and a date on
which no records at all are valid is an error.

## See also

[`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md)
to convert to GTFS fare tables,
[`atoc2gtfs()`](https://itsleeds.github.io/UK2GTFS/reference/atoc2gtfs.md)
which can do both steps in one go.

Other rail fares:
[`gtfs_add_railfares()`](https://itsleeds.github.io/UK2GTFS/reference/gtfs_add_railfares.md)
