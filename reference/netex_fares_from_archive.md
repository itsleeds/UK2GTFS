# Add NeTEx fares to a GTFS object straight from a BODS archive

End-to-end convenience wrapper for the national work flow: unpack a BODS
NeTEx fare archive, read every fare file (in parallel), report what was
found and how many lines match the GTFS, then add the chosen fares to
the GTFS object. Equivalent to calling \[netex_unzip()\],
\[netex_read_fares_multiple()\], \[netex_fares_report()\] and
\[gtfs_add_fares()\] in turn.

## Usage

``` r
netex_fares_from_archive(
  archive,
  gtfs,
  fares_version = 1,
  ncores = 1,
  pattern = NULL,
  exdir = tempfile("netex_"),
  report = TRUE,
  trip_type = NULL,
  user_type = NULL,
  product_name = NULL,
  direction = NULL
)
```

## Arguments

- archive:

  path to a BODS NeTEx fare \`.zip\` archive (or a folder of NeTEx
  files).

- gtfs:

  a GTFS object (named list) to which fares will be added, for example
  the national feed produced by \[transxchange2gtfs()\].

- fares_version:

  numeric, \`1\` or \`2\` (see \[gtfs_add_fares()\]).

- ncores:

  integer, cores for the parallel read step (default \`1\`).

- pattern:

  optional regex to restrict which archive entries are extracted (e.g.
  an operator name), passed to \[netex_unzip()\].

- exdir:

  directory to extract into (default a temp folder).

- report:

  logical, print a \[netex_fares_report()\] summary (default \`TRUE\`).

- trip_type, user_type, product_name, direction:

  optional fare-type filters passed to \[gtfs_add_fares()\].

## Value

the GTFS object with fare tables added. The parsed NeTEx list and the
report are attached as attributes \`"netex"\` and \`"report"\` for
inspection.
