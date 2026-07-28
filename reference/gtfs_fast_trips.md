# Find fast trips

Fast trips can identify problems with the input data or conversion
process. This function returns trip_ids for trips that exceed
\`maxspeed\`.

## Usage

``` r
gtfs_fast_trips(gtfs, maxspeed = 83, routes = TRUE)
```

## Arguments

- gtfs:

  list of gtfs tables

- maxspeed:

  the maximum allowed speed in metres per second default 83 m/s (about
  185 mph the max speed of trains on HS1 line)

- routes:

  logical, do one trip per route, faster but may miss some trips

## Value

a character vector of trip_ids that exceed \`maxspeed\`

## Details

The function looks a straight line distance between each stop and
detects the fastest segment of the journey. A common cause of errors is
that a stop is in the wrong location so a bus can appear to teleport
across the country in seconds.
