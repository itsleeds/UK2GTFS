# Find fast stops

A varient of gtfs_fast_trips that can detect stops that may be in the
wrong location

## Usage

``` r
gtfs_fast_stops(gtfs, maxspeed = 83)
```

## Arguments

- gtfs:

  list of gtfs tables

- maxspeed:

  the maximum allowed speed in metres per second default 83 m/s (about
  185 mph the max speed of trains on HS1 line)

## Value

an sf data frame of stops with speed and distance summary columns

## Details

The function looks a straight line distance between each stop and
detects the fastest segment of the journey. A common cause of errors is
that a stop is in the wrong location so a bus can appear to teleport
across the country in seconds.
