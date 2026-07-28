# Match NeTEx fare lines to GTFS routes

Joins each parsed NeTEx fare object to a route in a GTFS object. The
join key is the operator National Operator Code (NeTEx \`operator_noc\`
== GTFS \`agency_id\`) together with the public line number (NeTEx
\`line_public_code\` == GTFS \`route_short_name\`). The GTFS
\`route_id\` is generated from the TransXChange ServiceCode and cannot
be matched to the NeTEx line id directly, which is why the public codes
are used.

## Usage

``` r
netex_match_routes(netex_list, gtfs)
```

## Arguments

- netex_list:

  list of parsed NeTEx fare objects.

- gtfs:

  a GTFS object (named list) containing at least \`routes\` (with
  \`route_id\`, \`route_short_name\`, \`agency_id\`).

## Value

a data.table with columns \`idx\` (index into \`netex_list\`),
\`operator_noc\`, \`line_public_code\`, \`route_id\` and \`matched\`
(logical). Unmatched NeTEx files have \`route_id = NA\`.
