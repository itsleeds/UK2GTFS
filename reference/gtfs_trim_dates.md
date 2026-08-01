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

## Details

The GTFS specification allows a \`service_id\` to be defined in
\`calendar_dates.txt\` alone, with no row in \`calendar.txt\`; the
service then runs on exactly the dates listed with \`exception_type =
1\`. Such services are kept if any of those dates falls inside the
window. The DfT's Bus Open Data Service GTFS uses them heavily (for
example for school-holiday timetables), so dropping them silently
removes real service.
