# Read a single NeTEx fare file

Parses one BODS NeTEx fare \`.xml\` file (DfT fxc profile) into a list
of tidy data.tables describing the line, its fare zones, the
zone-to-zone price matrix and the fare product.

## Usage

``` r
netex_read_fares(path)
```

## Arguments

- path:

  path to a NeTEx \`.xml\` file.

## Value

a list with elements:

- meta:

  one-row data.table of file / operator / line / product attributes
  (operator NOC, line public code, direction, currency, product name &
  type, trip type, user type, validity dates).

- stops:

  data.table of scheduled stop points (\`stop_id\` is the ATCO code with
  the \`atco:\` prefix removed), with name and locality.

- zones:

  long data.table mapping each fare \`zone_id\` to its member
  \`stop_id\`s (plus zone name).

- prices:

  data.table of price bands (\`price_group\`) and their \`amount\`.

- fares:

  data.table of the fare triangle: \`from_zone\`, \`to_zone\`,
  \`price_group\`, \`amount\` (one row per origin/destination zone
  pair).
