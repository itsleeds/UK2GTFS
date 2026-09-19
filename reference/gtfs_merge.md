# merge a list of gtfs files

!WARNING! only the tables: agency, stops, routes, trips, stop_times,
calendar, calendar_dates, shapes, frequencies are processed, any other
tables in the input timetables are passed through

## Usage

``` r
gtfs_merge(
  gtfs_list,
  force = FALSE,
  quiet = TRUE,
  condenseServicePatterns = TRUE
)
```

## Arguments

- gtfs_list:

  a list of gtfs objects to be merged

- force:

  logical, if TRUE duplicated values are merged taking the fist instance
  to be the correct instance, in most cases this is ok, but may cause
  some errors

- quiet:

  logical, if TRUE less messages

- condenseServicePatterns:

  logical, if TRUE service patterns across all routes are condensed into
  a unique set of patterns

## Value

a single merged gtfs object

## Details

if duplicate IDs are detected then completely new IDs for all rows will
be generated in the output.

Feeds converted independently normally reuse the same ids - each
regional TNDS conversion numbers its services from 1 - so the
renumbering above is the usual path, not the exception. The renumbering
keys on the input feed a row came from, tracked internally as
\`file_id\`, which is therefore assigned from a feed's position in
\`gtfs_list\` and means the same thing in every table.

It is not assigned per table. Doing that numbers each table over only
the feeds that contain it, so one feed missing one optional table shifts
the numbering of every later feed in that table alone, and rows are then
renumbered against another feed's ids. A TNDS snapshot is the case that
bites: its NCSD coach archive has no \`calendar_dates\`, and eight of
the twelve regions had their cancellations applied to the preceding
region's services.
