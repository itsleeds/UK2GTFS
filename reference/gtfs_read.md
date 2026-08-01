# Read GTFS

Read in a GTFS zip file

## Usage

``` r
gtfs_read(path)
```

## Arguments

- path:

  character, path to GTFS zip folder

## Value

a gtfs object: a named list of data frames, one per GTFS table

## Details

The core tables are read with explicit column types (ids as character,
coordinates as numeric, etc.); any column type not listed is left to
fread's detection. Times of day (stop_times arrival/departure,
frequencies start/end) are returned as lubridate Periods so values past
24:00:00 are preserved. Tables without an explicit specification are
read with automatic types except that \`\*\_id\` columns are always
character.
