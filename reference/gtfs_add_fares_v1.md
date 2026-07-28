# Add GTFS v1 fares (fare_attributes + fare_rules) from NeTEx

Builds the original-specification GTFS fare tables from parsed NeTEx
fare files and attaches them to a GTFS object. A \`zone_id\` column is
added to \`stops\` giving each stop its fare zone, and \`fare_rules\`
maps each origin-zone / destination-zone pair on a route to a fare.

## Usage

``` r
gtfs_add_fares_v1(gtfs, netex_list, payment_method = 0L)
```

## Arguments

- gtfs:

  a GTFS object (named list).

- netex_list:

  list of parsed NeTEx fare objects (already filtered to the product(s)
  you want, see \[netex_filter_fares()\]).

- payment_method:

  integer, GTFS \`payment_method\` (0 = paid on board, 1 = paid before
  boarding). Default 0, matching on-board bus ticket sales.

## Value

the GTFS object with \`fare_attributes\`, \`fare_rules\` and a
\`stops\$zone_id\` column added.

## Details

Because GTFS v1 stores a single \`zone_id\` per stop, this works cleanly
for a single route; when several routes share a stop but place it in
different fare zones only the first assignment is kept (with a warning).
