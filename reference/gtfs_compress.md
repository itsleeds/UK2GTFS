# Reduce file size of a GTFS object

Reduce file size of a GTFS object

## Usage

``` r
gtfs_compress(gtfs)
```

## Arguments

- gtfs:

  a gtfs object

## Value

a gtfs object

## Details

by default UK2GTFS tries to preserve id numbers during the conversion
process to allow back comparisons to the original files, e.g.
\`transxchange2gtfs()\` retains stop ids from the NAPTAN. However this
means files sizes are increased. This function replaces ids with
integers and thus reduces the size of the gtfs file.

All tables that reference the rewritten ids are updated together:
\`stop_id\` in stop_times, transfers, pathways and stop_areas;
\`trip_id\` in stop_times and frequencies; \`route_id\` in trips,
fare_rules and route_networks; and \`shape_id\` in trips and shapes.
Rows referencing an id that does not exist in the parent table are
dropped.
