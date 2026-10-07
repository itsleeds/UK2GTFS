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
\`ServiceCode\` \*\*published by one operator\*\*:

A \`ServiceCode\` is unique within an operator, not nationally, so the
operator is part of the key. Five South East operators publish
\`ServiceCode\` `1`; keyed on the code alone they look like one service
with five competing revisions, and all but the most recently started are
discarded as superseded. The operator is taken as the whole set of
\`NationalOperatorCode\` values the file declares, sorted, so that a
jointly registered service is not split by the order its file happens to
list them in. Where no operator code can be read the code alone is used.

1.  For each distinct operating-period start date, \*\*line\*\* and
    \*\*day type\*\*, only the file with the highest \`RevisionNumber\`
    (ties broken by the most recent \`ModificationDateTime\`) - repeated
    uploads of the same timetable period are duplicates. A file is kept
    if it is the best available file for at least one of the lines it
    publishes. The line matters because a \`ServiceCode\` does not
    identify one timetable: operators such as Nottingham City Transport
    split a single registration into one file per line, all sharing the
    \`ServiceCode\` and operating period, and keying on the
    \`ServiceCode\` alone would discard every line but one. The day type
    matters for the same reason: Brighton & Hove files one document per
    day type, so a weekday, a Saturday and a Sunday file share the code,
    the line and the period and differ only in their \`RegularDayType\`.
    Day sets are read from \`DaysOfWeek\` and expanded, so that
    \`MondayToFriday\` and the five days listed separately compare
    equal.

2.  Of the start dates on or before \`date\`, only the most recent -
    this is the version operative on \`date\`; earlier versions have
    been superseded.

3.  All files whose operating period starts after \`date\` - these are
    future timetables that have not yet come into effect.

## Overlapping registrations

Rules 1 to 3 key on the operator and \`ServiceCode\`, which catches a
re-upload of one registration but not a re-registration: some
publishers, Transport for London among them, mint a \*\*new\*\*
\`ServiceCode\` every time a service is re-registered. Each code then
appears exactly once and nothing above detects it, yet both files
describe the same service over overlapping dates and both convert into
the feed.

With \`resolve_overlaps = TRUE\` files are additionally grouped by
\`NationalOperatorCode\` + \`Description\` + the set of lines they
publish - the same registered service by any reading - and overlapping
operating periods within a group are reconciled.

The \`Description\` is compared normalised rather than raw: case folded,
punctuation reduced to spaces and the words sorted, because publishers
retype it with each re-registration and it does not come back the same.
On fixed track - anything the \`Mode\` says is not a bus, coach or
trolleybus - it is left out of the key altogether, and the operator code
and line name identify the service on their own. Normalising cannot
repair a misspelling, and Transport for London's Central line files
differ by exactly that: one says "Ealing Broaddway" and another "West
Ruilsip", which was enough to put each in a group of its own where it
had nothing to overlap with. A named line on rails is unique to its
operator in a way a bus route number is not, so the description is not
needed there; on the road it is kept, because one operator really can
run a route "1" in two towns.

Two files in a group whose operating \*\*days\*\* do not intersect are
left alone, however their periods overlap: a weekday file and a Saturday
file cannot be alternative versions of one timetable. The test is
disjointness, not inequality, so a successor registration that merely
drops Saturday still shares the weekdays with the file it replaces and
is reconciled as usual. Without this, a publisher filing one document
per day type had the identical-period rule below keep the newest and
call the rest duplicates.

Within a group the overlaps are reconciled:

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
