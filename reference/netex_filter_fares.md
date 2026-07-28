# Select NeTEx fare files by product attributes

Filters a list of parsed NeTEx fare objects, keeping only those whose
metadata match ALL of the supplied criteria. Any criterion left \`NULL\`
is not applied. Matching is case-insensitive.

## Usage

``` r
netex_filter_fares(
  netex_list,
  trip_type = NULL,
  user_type = NULL,
  product_name = NULL,
  direction = NULL,
  line_public_code = NULL,
  operator_noc = NULL
)
```

## Arguments

- netex_list:

  list of parsed NeTEx fare objects.

- trip_type:

  character, e.g. "single" or "return" (matches \`meta\$trip_type\`).

- user_type:

  character, e.g. "adult", "child", "senior" (matches
  \`meta\$user_type\`).

- product_name:

  character, matched as a regular expression against
  \`meta\$product_name\` (e.g. "Adult Single").

- direction:

  character, "inbound" or "outbound".

- line_public_code:

  character, the public line number (e.g. "91").

- operator_noc:

  character, the operator National Operator Code.

## Value

a filtered list of NeTEx fare objects (a subset of \`netex_list\`).

## Details

This is how the caller chooses which kind of fare to convert (the task
requires being able to pick, e.g., single tickets only). All valid
choices are supported: pass any combination of the attributes below, and
inspect \[netex_fare_types()\] to discover the values present in your
data.
