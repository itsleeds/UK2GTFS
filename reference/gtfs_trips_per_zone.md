# Trim a GTFS file between two dates

Trim a GTFS file between two dates

## Usage

``` r
gtfs_trips_per_zone(
  gtfs,
  zone,
  startdate = min(gtfs$calendar$start_date),
  enddate = min(gtfs$calendar$start_date) + 31,
  zone_id = 1,
  by_mode = TRUE,
  ncores = 1,
  time_bands = list(breaks = c(-1, 6, 10, 15, 18, 22, Inf), labels = c("Night",
    "Morning Peak", "Midday", "Afternoon Peak", "Evening", "Night"))
)
```

## Arguments

- gtfs:

  GTFS object from gtfs_read()

- zone:

  SF data frame of polygons

- startdate:

  Start date

- enddate:

  End date

- zone_id:

  Which column in \`zone\` is the ID column

- by_mode:

  logical, disaggregate by mode?

- ncores:

  numeric, how many cores to use in parallel processing

- time_bands:

  list with two named vectors breaks and labels. Used to define the time
  breakdown. Length of breaks must be one greater than length of labels.

## Value

a data frame of trips per zone, day of week, and time band

## Details

For frequency-based services (frequencies.txt), each departure implied
by a frequency window is counted as a separate trip in the time band of
its departure time.
