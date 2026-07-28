# Read many NeTEx fare files

Convenience wrapper that reads a vector of NeTEx fare files and returns
a named list, one entry per file, each being the output of
\[netex_read_fares()\]. Reading is the slow, CPU-bound part of the fares
pipeline, so this function can spread the work over several cores.

## Usage

``` r
netex_read_fares_multiple(paths, ncores = 1, quiet = FALSE)
```

## Arguments

- paths:

  character vector of NeTEx \`.xml\` file paths.

- ncores:

  integer, number of cores to use for parallel reading (default \`1\`).
  Values above 1 use a \`furrr\`/\`future\` multisession backend.
  Reading the national fare archive benefits from a high core count.

- quiet:

  logical, suppress the summary message of how many files failed,
  default \`FALSE\`.

## Value

named list of parsed NeTEx fare objects (names are file basenames).
Files that fail to parse are dropped, and (unless \`quiet\`) a summary
of the failures is reported via \[netex_read_failures()\] attached as
the attribute \`"failures"\`.
