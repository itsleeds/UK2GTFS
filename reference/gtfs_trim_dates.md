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

Exceptions follow the GTFS rules rather than the calendar row's date
range. An addition (\`exception_type = 1\`) inside the window is kept
even when it falls outside its service's own
\`start_date\`-\`end_date\`: GTFS adds the date regardless. A service
whose \`calendar.txt\` row does not reach the window at all, but which
adds dates inside it, is kept too. Its row is dropped, so it comes out
as a \`calendar_dates.txt\`-only service running on exactly those dates.
A removal (\`exception_type = 2\`) is still clipped to the row's range,
because outside it the service does not run and the removal does
nothing.

Until version 0.4.0 both kinds of addition were discarded.
