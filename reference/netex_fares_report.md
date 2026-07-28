# Summarise a set of NeTEx fare files (and optionally match to GTFS)

Produces a high level report of what is in a collection of NeTEx fare
files: how many files, operators and lines are covered, the mix of
products, trip types and passenger types, and how many files describe a
usable zonal fare triangle. If a GTFS object is supplied it also reports
how many lines could be matched to a GTFS route (the main scaling
bottleneck nationally).

## Usage

``` r
netex_fares_report(netex_list, gtfs = NULL)
```

## Arguments

- netex_list:

  list of parsed NeTEx fare objects, from
  \[netex_read_fares_multiple()\].

- gtfs:

  optional GTFS object; if supplied, line-to-route match rates are
  included.

## Value

a list with elements \`overview\` (named numeric summary),
\`by_product\`, \`by_trip_type\`, \`by_user_type\` (data.tables of
counts) and, when \`gtfs\` is given, \`match\` (the
\[netex_match_routes()\] table) and \`unmatched\` (distinct
operator/line pairs with no GTFS route). The list is returned invisibly
after printing a short summary.
