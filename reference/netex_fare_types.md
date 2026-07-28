# List the fare products available in a set of NeTEx files

Summarises a list of parsed NeTEx fare objects so the caller can see
which fare types (single / return, adult / child, per line and
direction) are present and therefore what can be selected for
conversion.

## Usage

``` r
netex_fare_types(netex_list)
```

## Arguments

- netex_list:

  list of parsed NeTEx fare objects, from
  \[netex_read_fares_multiple()\].

## Value

a data.table, one row per NeTEx file, with the operator, line, direction
and product attributes plus the source list index (\`idx\`) so a
selection can be mapped back to the original list.
