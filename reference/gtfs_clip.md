# Clip a GTFS object to a geographical area

Clips the GTFS file to only include stops within the bounds object,
trips that cross the boundary of the the object are truncated. Any trips
that stop only once in the bounds are removed completely. The optional
tables (shapes, frequencies, transfers, pathways and the GTFS v1/Fares
v2 fare tables) are pruned so they only reference the stops, routes and
trips that remain.

## Usage

``` r
gtfs_clip(gtfs, bounds)
```

## Arguments

- gtfs:

  a gtfs object

- bounds:

  an sf data frame of polygons or multi-polygons with CRS 4326

## Value

a gtfs object clipped to the bounds
