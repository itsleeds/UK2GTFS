# Add GTFS-Fares-v2 tables from NeTEx

Builds GTFS-Fares-v2 tables (areas, stop_areas, networks,
route_networks, rider_categories, fare_media, fare_products and
fare_leg_rules) from parsed NeTEx fare files and attaches them to a GTFS
object.

## Usage

``` r
gtfs_add_fares_v2(
  gtfs,
  netex_list,
  fare_media_name = "cash",
  fare_media_type = 0L
)
```

## Arguments

- gtfs:

  a GTFS object (named list).

- netex_list:

  list of parsed NeTEx fare objects.

- fare_media_name, fare_media_type:

  the ticket medium recorded in \`fare_media\`. Defaults describe a cash
  on-board paper ticket (\`fare_media_type = 0\`).

## Value

the GTFS object with GTFS-Fares-v2 tables added.

## Details

Unlike v1, this can represent several fare products (e.g. adult and
child, single and return) at the same time, because passenger type is
carried by \`rider_categories\` / \`fare_products\` rather than by the
stop.
