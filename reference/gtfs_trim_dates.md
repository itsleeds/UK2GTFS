# Trim a GTFS file between two dates

Trim a GTFS file between two dates

## Usage

``` r
gtfs_trim_dates(
  gtfs,
  startdate = lubridate::ymd("2020-03-01"),
  enddate = lubridate::ymd("2020-04-30")
)
```

## Arguments

- gtfs:

  GTFS object from gtfs_read()

- startdate:

  Start date

- enddate:

  End date

## Value

a gtfs object trimmed to services running between the two dates
