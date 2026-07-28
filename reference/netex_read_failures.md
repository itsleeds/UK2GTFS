# Failures from the last multi-file NeTEx read

Extracts the table of files that could not be parsed from an object
returned by \[netex_read_fares_multiple()\].

## Usage

``` r
netex_read_failures(netex_list)
```

## Arguments

- netex_list:

  output of \[netex_read_fares_multiple()\].

## Value

a data.table with columns \`file\` and \`error\`.
