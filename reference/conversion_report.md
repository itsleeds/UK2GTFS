# What a conversion did: the choices made and what they removed

A conversion makes many decisions the caller never sees in the output
feed: which files were set aside as superseded or duplicate and in
favour of which, which operating periods were cut short, which files
failed to import, where each stop's position came from, and what later
steps such as \[gtfs_clean()\], \[gtfs_deduplicate()\] and
\[gtfs_deduplicate_copies()\] removed. \[transxchange2gtfs()\] records
them in a \`"conversion_report"\` attribute on the feed it returns, and
those functions add to it.

\`conversion_report()\` returns that record; printing it gives a
summary. \`write_conversion_report()\` saves it as CSV tables with a
Markdown summary, because the attribute does not survive
\[gtfs_write()\].

## Usage

``` r
conversion_report(gtfs)

write_conversion_report(x, dir)

# S3 method for class 'uk2gtfs_conversion_report'
print(x, ...)
```

## Arguments

- gtfs:

  a gtfs object returned by \[transxchange2gtfs()\] (or merged from
  several with \[gtfs_merge()\], which combines their reports)

- x:

  a gtfs object carrying a report, or a report from
  \`conversion_report()\`

- dir:

  directory to write to; created if needed

- ...:

  ignored

## Value

\`conversion_report()\`: an object of class
\`uk2gtfs_conversion_report\`, a list of

- settings:

  the arguments the conversion ran with

- files:

  one row per input file: \`decision\` (kept, dropped, truncated,
  added), the filter \`rule\` that decided it, the \`reason\`, and the
  file it gave way to (\`by\`); see \[txc_filter_files()\]

- failures:

  files that failed to import or to convert

- stop_locations:

  where each stop's position came from (see the \`stop_locations\`
  argument of \[transxchange2gtfs()\])

- steps:

  one row per later processing step and what it removed

- copies:

  the pairs of copies \[gtfs_deduplicate_copies()\] found

or \`NULL\` if the feed carries no report.

\`write_conversion_report()\`: the paths written, invisibly

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- transxchange2gtfs("region.zip", filter_duplicate_files = TRUE)
gtfs <- gtfs_deduplicate_copies(gtfs)
conversion_report(gtfs)
write_conversion_report(gtfs, "region_report")
} # }
```
