# Operators whose declared mode is known to be wrong

A lookup of operators that declare the wrong \`Mode\` in their
TransXChange registrations, used by \[transxchange2gtfs()\] to correct
\`route_type\` after conversion.

## Usage

``` r
operator_mode_overrides()
```

## Value

a data frame of \`noc\`, \`route_short_name\`, \`mode\` and \`note\`

## Details

Each row names an operator by National Operator Code and gives the mode
its services should have. \`route_short_name\` restricts the correction
to one line of that operator, for the common case of an operator running
more than one mode - Blackpool Transport runs a tramway and eighteen bus
routes, and only the tramway would ever need correcting. \`NA\` applies
the correction to every route of the operator.

\`mode\` is any value \[clean_route_type()\] accepts: \`"bus"\`,
\`"coach"\`, \`"ferry"\`, \`"rail"\`, \`"metro"\`, \`"underground"\`,
\`"tram"\`, \`"trolleybus"\`, \`"air"\`.

\`route_short_name\` is matched against the name \*\*as it appears in
the converted feed\*\*, after the shortening rules
\`transxchange2gtfs()\` applies to long line names, not against the raw
\`LineName\`. Both it and \`noc\` are matched case insensitively.

The bar for adding a row is that the declaration is checkably wrong
rather than merely surprising: the operator runs a mode the file does
not claim, and some independent source says so. Corrections are
deliberately narrow — keyed to an operator, and usually to a single line
— because a wrong entry here silently re-labels real service.

Current entries:

\* \*\*NEXT / Nottingham Express Transit.\*\* Its tramway is registered
in TNDS as \`\<Mode\>bus\</Mode\>\` (\`notts_NEXT_TRAM_NETTRAM.xml\`,
service \`NETTRAM\`, line \`TRAM\`, Clifton/Toton – Phoenix
Park/Hucknall). Every other British tramway in TNDS is coded \`tram\`,
and the DfT's BODS GTFS carries this one as a tram, so any analysis
counting buses credits Nottingham with a tram network's worth of bus
service — 144,488 vehicle journeys over four weeks in the July 2026
snapshot.

## Examples

``` r
operator_mode_overrides()
#>    noc route_short_name mode
#> 1 NEXT             TRAM tram
#>                                                                note
#> 1 Nottingham Express Transit tramway registered as Mode=bus in TNDS
```
