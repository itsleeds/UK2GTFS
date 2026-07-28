# Force a GTFS to be valid by removing problems

Force a GTFS to be valid by removing problems

## Usage

``` r
gtfs_force_valid(gtfs)
```

## Arguments

- gtfs:

  gtfs object

## Value

a gtfs object

## Details

Actions performed 1. Remove stops with missing location 2. Remove routes
that don't exist in agency 3. Remove trips that don't exist in routes 4.
Remove stop_times(calls) that don't exist in trips 5. Remove
stop_times(calls) that don't exist in stops 6. Remove Calendar that have
service_id that doesn't exist in trips 7. Remove Calendar_dates that
have service_id that doesn't exist in trips 8. Remove rows in the
optional tables (shapes, frequencies, transfers, pathways and the GTFS
v1/Fares v2 fare tables) that reference stops, routes or trips that
don't exist
