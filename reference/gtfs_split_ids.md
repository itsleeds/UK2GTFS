# Split a GTFS object based on trip_ids

Split a GTFS object based on trip_ids

## Usage

``` r
gtfs_split_ids(gtfs, trip_ids)
```

## Arguments

- gtfs:

  gtfs list

- trip_ids:

  a vector of trips ids

## Value

Returns a named list of two gtfs objects. The \`true\` list contains
trips that matched \`trip_ids\` the \`false\` list contains all other
trips.
