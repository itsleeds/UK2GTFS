# Remove copies of one service published in two files

\[gtfs_deduplicate()\] recognises a journey only when two trips carry it
with identical times at every stop, under one operator name. That misses
a whole class of duplicates in merged TransXChange feeds: one timetable
written out twice by two publishers. Each publisher interpolates the
times at the stops between timing points its own way, and files the
operator under its own name, so no two copies agree call by call even
though every journey leaves the same first stop at the same minute.

This function compares \*services\* instead of trips. Where two services
of the same line and mode, from different source files, start most of
their journeys at the same stop at the same time, one is a copy of the
other: the lower ranked one loses its journeys on every date the higher
ranked one also runs them.

## Usage

``` r
gtfs_deduplicate_copies(
  gtfs,
  min_share = 0.8,
  min_shared = 2L,
  match = c("start", "ends"),
  keep = c("newer", "longer", "more_trips"),
  scope = c("trips", "route"),
  fixed_track = c(0L, 1L, 2L),
  quiet = FALSE
)
```

## Arguments

- gtfs:

  a gtfs object

- min_share:

  numeric between 0 and 1, the share of one service's distinct journey
  starts (first stop and departure time) that must also be starts of the
  other before the two are called copies. Default 0.8.

- min_shared:

  integer, the fewest shared starts that can make two services copies.
  Default 2, so that one coincident journey never condemns a service.

- match:

  \`"start"\` (the default) matches journeys on their first stop and
  departure time; \`"ends"\` also requires the same last stop and
  arrival time, which is stricter and misses a copy that was re-routed
  or re-timed towards its end.

- keep:

  character, the order in which the criteria decide which copy is kept,
  best first. \`"newer"\` prefers the service whose calendar starts
  later - the more recently published version - and only discriminates
  on a feed whose calendars have not been trimmed to a common window;
  \`"longer"\` prefers the service whose journeys call at more stops
  (finer detail, or a whole route rather than one region's share of it);
  \`"more_trips"\` prefers the service with more journeys. Remaining
  ties go to the lower \`route_id\`.

- scope:

  \`"trips"\` (the default) removes only the lower ranked copy's
  journeys that match a journey of the kept copy, on the dates both run.
  \`"route"\` also removes its unmatched journeys on dates the kept copy
  runs, treating it as a superseded version of the whole timetable. Even
  then a service is only removed wholesale if its own share of matched
  starts reaches \`min_share\`; a larger service that merely contains a
  smaller one loses only the matched journeys.

- fixed_track:

  integer vector of \`route_type\`s left out. Duplicates on rail, metro
  and tram are already handled by \[gtfs_deduplicate()\]'s termini test.
  Default \`c(0, 1, 2)\`.

- quiet:

  logical, suppress the summary message

## Value

the gtfs object with the copies' journeys removed or their dates
trimmed. A data.frame describing every pair of copies found, and what
was done about it, is attached as attribute \`"copies_report"\`.

## Details

\*\*Which routes are one service.\*\* A route is identified with its
source service: the \`\[ServiceCode: ...\]\` that
\[transxchange2gtfs()\] writes into \`route_desc\`, together with
\`agency_id\`. Routes of one service are variants of one file and are
never compared with each other - that is \[gtfs_deduplicate()\]'s job. A
feed without ServiceCodes falls back to \`route_id\`.

\*\*When two services are copies.\*\* They must share
\`route_short_name\` and \`route_type\`, and at least \`min_share\` of
the distinct journey starts of one of them (and at least \`min_shared\`
starts) must be starts of the other. The operator is deliberately not
compared: in the cases this exists for, the two copies name the operator
differently ("Arriva" against "Arriva Midlands", both NOC AMID) or not
at all the same operator (Go-Ahead's superseded export of London route
439 against Transport for London's current file naming Metrobus).

A line number alone could not make two services copies: two towns' route
1 do not leave the same stop. Two operators running a joint timetable do
not either, because they share the departures out rather than repeat
them.

\*\*What is removed.\*\* Dates are respected throughout. A journey of
the copy is removed on the dates the kept service runs a journey with
the same start (scope \`"trips"\`) or runs at all (scope \`"route"\`).
If that is every date the journey runs, the trip is deleted; if it is
some of them, the trip is moved to a new service that no longer runs on
those dates; if none, it is left alone. A superseded open-ended file
therefore loses its journeys only from the date its successor begins.

Groups of three or more copies are ranked together, and each loses its
journeys to every higher ranked service it is a copy of.

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- gtfs_merge(list(gtfs_a, gtfs_b), force = TRUE)
gtfs <- gtfs_deduplicate_copies(gtfs)
attr(gtfs, "copies_report")
} # }
```
