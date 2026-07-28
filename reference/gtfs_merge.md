# merge a list of gtfs files

!WARNING! only the tables: agency, stops, routes, trips, stop_times,
calendar, calendar_dates, shapes, frequencies are processed, any other
tables in the input timetables are passed through

## Usage

``` r
gtfs_merge(
  gtfs_list,
  force = FALSE,
  quiet = TRUE,
  condenseServicePatterns = TRUE
)
```

## Arguments

- gtfs_list:

  a list of gtfs objects to be merged

- force:

  logical, if TRUE duplicated values are merged taking the fist instance
  to be the correct instance, in most cases this is ok, but may cause
  some errors

- quiet:

  logical, if TRUE less messages

- condenseServicePatterns:

  logical, if TRUE service patterns across all routes are condensed into
  a unique set of patterns

## Value

a single merged gtfs object

## Details

if duplicate IDs are detected then completely new IDs for all rows will
be generated in the output.
