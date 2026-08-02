# Filter superseded TransXchange file versions

Given a set of TransXchange XML files, returns the subset that
represents the operative timetable for each service, discarding
superseded revisions of the same service and closing the operating
periods of registrations that a later registration has replaced.

## Usage

``` r
txc_filter_files(
  files,
  date = Sys.Date(),
  ncores = 1,
  quiet = TRUE,
  resolve_overlaps = TRUE,
  out_dir = NULL
)
```

## Arguments

- files:

  character vector of paths to TransXchange XML files

- date:

  Date, the reference date used to decide which file version is
  operative (default \`Sys.Date()\`). For historical analysis set this
  to a date within the period you are studying.

- ncores:

  numeric, number of cores used to read the file headers (default 1)

- quiet:

  logical, if FALSE a summary of removed files is printed

- resolve_overlaps:

  logical, if TRUE (default) registrations of the same service whose
  operating periods overlap are reconciled - see Details.

- out_dir:

  character, directory in which to write the rewritten copies of files
  whose operating period was truncated. Defaults to a new
  session-temporary directory. The originals are never modified.

## Value

a character vector, the subset of \`files\` to convert. Where an
operating period was truncated the returned path points at a rewritten
copy in \`out_dir\` rather than at the original file.

## Details

Archives of TransXchange data (such as the Bus Open Data Service change
archive) often contain several versions of the same registered service:
each time an operator updates a timetable a new file is uploaded for the
same \`ServiceCode\`, but the superseded files remain in the archive and
usually still declare an open-ended \`OperatingPeriod\`. If all versions
are converted, the same physical bus journey appears once per file
version, so counting trips on a given date over-estimates service
levels.

Every field used here is read from the contents of each file. Nothing is
inferred from file names: publishers prefix them inconsistently
(\`tfl\_\`, \`cen\_\`, \`swe\_\`, \`cambs\_\`) and in some regions the
name does not contain the \`ServiceCode\` at all.

The function reads the header information of each file (\`ServiceCode\`,
\`LineName\`, \`Description\`, \`NationalOperatorCode\`,
\`OperatingPeriod\` start and end dates, \`RevisionNumber\`,
\`CreationDateTime\` and \`ModificationDateTime\`) and keeps, for each
\`ServiceCode\`:

1.  For each distinct operating-period start date \*\*and line\*\*, only
    the file with the highest \`RevisionNumber\` (ties broken by the
    most recent \`ModificationDateTime\`) - repeated uploads of the same
    timetable period are duplicates. A file is kept if it is the best
    available file for at least one of the lines it publishes. The line
    matters because a \`ServiceCode\` does not identify one timetable:
    operators such as Nottingham City Transport split a single
    registration into one file per line, all sharing the \`ServiceCode\`
    and operating period, and keying on the \`ServiceCode\` alone would
    discard every line but one.

2.  Of the start dates on or before \`date\`, only the most recent -
    this is the version operative on \`date\`; earlier versions have
    been superseded.

3.  All files whose operating period starts after \`date\` - these are
    future timetables that have not yet come into effect.

## Overlapping registrations

Rules 1 to 3 key on the \`ServiceCode\`, which catches a re-upload of
one registration but not a re-registration: some publishers, Transport
for London among them, mint a \*\*new\*\* \`ServiceCode\` every time a
service is re-registered. Each code then appears exactly once and
nothing above detects it, yet both files describe the same service over
overlapping dates and both convert into the feed.

With \`resolve_overlaps = TRUE\` files are additionally grouped by
\`NationalOperatorCode\` + \`Description\` + the set of lines they
publish - the same registered service by any reading - and overlapping
operating periods within a group are reconciled:

- \*\*Staggered starts.\*\* Where a later registration runs to at least
  the end of an earlier one, the earlier one's \`EndDate\` is truncated
  to the day before the later one starts. This is the common case: the
  publisher issues the successor but leaves the predecessor open-ended.

- \*\*Same start, different end.\*\* The longer registration's
  \`StartDate\` is moved to the day after the shorter one ends, so the
  shorter, more specific period governs while it runs.

- \*\*Identical periods.\*\* Truncation cannot separate them, so the
  most recently created file is kept (\`CreationDateTime\`, falling back
  to \`ModificationDateTime\` then file mtime) and the others dropped.

- \*\*One period wholly inside another.\*\* Both are kept and reported.
  Closing the outer period would delete the service either side of the
  inner one, which a single \`OperatingPeriod\` cannot express.

Truncation is preferred to deletion throughout: a journey on a date the
successor does not cover is never removed. A file whose period is
emptied by truncation is dropped. Because this works from declared
identity and declared validity rather than from journey times, it does
not depend on two timetables resembling each other, and cannot merge two
services that merely run at similar times.

Resolving overlaps also removes the limitation that applied to rule 3 on
its own: a future timetable kept under that rule now truncates the
currently operative file at its start date, instead of both being
counted once the future timetable begins.

Files whose \`ServiceCode\` cannot be read are always kept.
