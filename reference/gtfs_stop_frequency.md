# Count the number of trips stopping at each stop between two dates

Count the number of trips stopping at each stop between two dates

## Usage

``` r
gtfs_stop_frequency(
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

the stops table with total stop counts and stops per week added

## Details

For frequency-based services (frequencies.txt), each trip is counted
once per departure implied by its frequency windows.
