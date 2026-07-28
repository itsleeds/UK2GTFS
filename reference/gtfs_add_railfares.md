# Add National Rail fares to a rail GTFS object

Converts a National Rail fares feed (see
[`atoc_fares_read()`](https://itsleeds.github.io/UK2GTFS/reference/atoc_fares_read.md))
to GTFS fare tables and attaches them to a GTFS object produced by
[`atoc2gtfs()`](https://itsleeds.github.io/UK2GTFS/reference/atoc2gtfs.md).
Both the original GTFS fares specification (`fares_version = 1`) and
GTFS Fares v2 (`fares_version = 2`) are supported.

## Usage

``` r
gtfs_add_railfares(
  gtfs,
  fares,
  fares_version = 1,
  ticket_codes = NULL,
  ticket_class = "standard",
  ticket_type = NULL,
  walkup_only = TRUE,
  rider_categories = c("adult", "child"),
  railcards = NULL,
  ndf = TRUE,
  travel_date = NULL,
  travel_time = NULL,
  booking_date = NULL,
  silent = TRUE
)
```

## Arguments

- gtfs:

  a GTFS object (named list of data frames) from
  [`atoc2gtfs()`](https://itsleeds.github.io/UK2GTFS/reference/atoc2gtfs.md).

- fares:

  a fares list from
  [`atoc_fares_read()`](https://itsleeds.github.io/UK2GTFS/reference/atoc_fares_read.md),
  or a path to a fares zip/folder which will be read for you.

- fares_version:

  numeric, `1` for the original GTFS fares tables
  (`fare_attributes`/`fare_rules`) or `2` for GTFS Fares v2. Default
  `1`.

- ticket_codes:

  optional character vector of ticket codes (e.g. `c("SDS","SDR")`) to
  convert, overriding `ticket_class`/`ticket_type`. See the
  `ticket_type` table of the fares list for available codes.

- ticket_class:

  character, `"standard"` and/or `"first"`. Default `"standard"`.

- ticket_type:

  character, any of `"single"`, `"return"`, `"season"`. Default
  `"single"` for `fares_version = 1` and `c("single","return")` for
  `fares_version = 2`. Note GTFS has no native concept of a return or
  season ticket: returns are emitted as a product priced for the round
  trip, seasons (weekly price) are excluded by default.

- walkup_only:

  logical, keep only walk-up (turn-up-and-go) tickets: the Anytime,
  Off-Peak and Super Off-Peak families (default TRUE). When FALSE every
  ticket type passing the class/type filters is converted, which
  includes trade, carnet and advance-purchase tier tickets whose
  headline prices are misleading (some are as low as GBP 0.05). Ignored
  when `ticket_codes` is given.

- rider_categories:

  character, any of `"adult"`, `"child"`. Which passenger types to emit
  (GTFS Fares v2 only). Default `c("adult","child")`.

- railcards:

  optional character vector of railcard codes (e.g. `"YNG"` = 16-25,
  `"SRN"` = Senior, `"DIS"` = Disabled; see the `railcard` table of the
  fares list). Each becomes an additional GTFS Fares v2 rider category
  with discounted prices. Default `NULL`.

- ndf:

  logical, include the non-derivable fare overrides file (default TRUE).

- travel_date:

  optional Date: convert fares for a journey on this date. The date/time
  restriction data (RST file) is evaluated and fares not valid on that
  date are dropped (e.g. tickets barred on certain dates). If `fares` is
  a path, the feed is also read as of this date.

- travel_time:

  optional departure time on `travel_date`, as `"HH:MM"` (or minutes
  after midnight). Fares whose time restriction makes them invalid at
  this departure time - e.g. Off-Peak tickets during the morning peak -
  are dropped. Requires `travel_date`.

- booking_date:

  optional Date: the day the ticket is bought. When given (with
  `travel_date`), Advance tickets whose booking horizon (TAP file) is
  satisfied are *included* in addition to the walk-up tickets, at their
  tier prices. Booking is assumed to happen by the end of this day. See
  Details for the quota caveat.

- silent:

  logical, suppress progress messages (default TRUE).

## Value

the GTFS object with fare tables added.

## Details

**How rail fares map to GTFS**

Rail fares are set per *flow*: an origin/destination pair which may be a
single station, a station cluster or a group station (e.g. "LONDON
TERMINALS"), per fare route and per ticket type.

With `fares_version = 1` every station is a fare zone (`zone_id` = CRS
code) and each flow is expanded to all its member station pairs in
`fare_rules`. Because GTFS v1 has no concept of passenger types or
ticket choice, the *cheapest* fare among the selected ticket types is
emitted for each station pair, and `rider_categories`/`railcards` are
ignored. Choose e.g. `ticket_type = "single"` to control what "the fare"
means.

With `fares_version = 2` each flow endpoint becomes an area in
`areas`/`stop_areas` (clusters and group stations map directly, without
expansion), each ticket type/price becomes a `fare_products` row, and
each flow becomes rows in `fare_leg_rules`. Passenger types are
distinguished via `rider_categories`: adult fares always, child fares
and railcard-discounted fares on request. Where several fare routes link
the same pair of areas, multiple products apply and a journey planner
may choose among them.

**Prices**

Adult prices come directly from the flow file (plus the non-derivable
overrides file, which contains fares that cannot be derived, e.g. most
London fares - disable with `ndf = FALSE`). Child and railcard prices
are calculated from the status discount file: percentage discounts with
flat-fare/minimum-fare caps, as specified in RSPS5045. The industry
rounding rules file (FRR) is *not* applied, so calculated discount
prices can differ from retail prices by a few pence.

**Restrictions and scenario conversion**

GTFS has no general way to say *when* a ticket is valid, so by default
time restrictions are ignored: an Off-Peak product appears alongside the
Anytime product with no indication that it cannot be used at 08:00, and
Advance tickets are excluded entirely (their tier prices carry no
availability information).

The `travel_date`, `travel_time` and `booking_date` arguments offer an
alternative: a **scenario snapshot**. Give a date (and optionally a
departure time and a booking date) and the restriction data is evaluated
at conversion time, so the output contains exactly the fares available
for that scenario - e.g. "travelling at 08:00 on Monday 3 August, booked
on 1 July" drops the Off-Peak products (invalid before 09:30 on most
flows) and includes the Advance tiers bookable a month ahead. The output
is still plain GTFS; the selection has simply been made for you.

Caveats: Advance *prices* are the tier prices from the flow file - which
tier is actually on sale for a particular train is quota-controlled by
the reservation system and is not in any public feed, so treat Advance
fares as "best case". Restriction evaluation covers date bands and
departure-time bands (network-wide or origin-specific); arrival-based,
via-based and train-specific restrictions, easements and minimum-fare
windows are treated as "fare remains valid", so the filter errs towards
keeping a fare.

Scenarios are validated against the coverage of the data: a
`travel_date` outside the GTFS calendar is an error (the timetable
contains no trips then), a date outside the feed's fares rounds warns
(see
[`atoc_fares_read()`](https://itsleeds.github.io/UK2GTFS/reference/atoc_fares_read.md)),
booking more than a year before travel is an error and more than ~12
weeks (when bookings usually open) warns, and a fares list read for a
different date than `travel_date` warns.

## See also

Other rail fares:
[`atoc_fares_read()`](https://itsleeds.github.io/UK2GTFS/reference/atoc_fares_read.md)

## Examples

``` r
if (FALSE) { # \dontrun{
gtfs <- atoc2gtfs("ttis123.zip", ncores = 4)
fares <- atoc_fares_read("RJFAF756.zip")
# GTFS v1: cheapest standard single per station pair
gtfs_v1 <- gtfs_add_railfares(gtfs, fares, fares_version = 1)
# GTFS v2: adult/child/16-25 railcard singles and returns
gtfs_v2 <- gtfs_add_railfares(gtfs, fares, fares_version = 2,
                              railcards = "YNG")
# Scenario: departing 08:00 on 3 August, booked 1 July - peak-time
# walk-up fares plus the Advance tiers bookable a month ahead
gtfs_s <- gtfs_add_railfares(gtfs, fares, fares_version = 2,
                             travel_date = as.Date("2026-08-03"),
                             travel_time = "08:00",
                             booking_date = as.Date("2026-07-01"))
} # }
```
