# Systems whose declared mode is not to be trusted, and what they really are

British timetable sources disagree with each other, and with themselves
between years, about which light railways are trams, which are metros
and which are heavy rail. This table settles it, so that a mode means
the same thing in every source and a series built across them is
comparable.

## Usage

``` r
standard_mode_overrides()
```

## Value

a data frame of \`system\`, \`operator\`, \`stop_pattern\`,
\`route_type\`, \`sources\` and \`note\`

## Details

Each row names a system, a regular expression matching the NaPTAN names
of the stops it serves, and the \`route_type\` it should carry.
\`operator\` is filled in only where the stops cannot identify the
system on their own: the London Underground because not all of its stop
names are marked ("Wembley Park" carries no suffix), the Birmingham
Air-Rail Link because it has two stops one of which is a mainline
station, and the Weardale Railway because its stops are named as
ordinary rail stations.

\`sources\` limits a row to the converters whose data can contain that
system, and exists because an operator code is not a stable identifier.
NPTDR reuses codes freely between archives, so a rule keyed on one can
fire on an unrelated operator in an earlier year: \`CAB\` is the London
cable car in TransXChange from 2023, but in the 2010 and 2011 NPTDR
archives it is somebody else entirely, and without this column those
services were relabelled as an aerial lift two years before the cable
car was built. Rows matched on stop names need no such limit - a stop
called "Buchanan Street SPT Subway Station" is the Glasgow Subway in any
year - so \`"any"\` is the normal value and only the operator-matched
rows for systems that did not yet exist are restricted.

The rules are: heritage and minor railways are rail (2); the London
Underground, the Tyne and Wear Metro, the Glasgow Subway and the
Docklands Light Railway are metro (1); the airport people movers are
trams (0); and the street and segregated tramways are trams (0). The
London cable car is the one system none of those describe, and it takes
the GTFS mode that does, aerial lift (6).

## Examples

``` r
standard_mode_overrides()
#>                         system operator                           stop_pattern
#> 1           London Underground      LUL                    Underground Station
#> 2          Tyne and Wear Metro     <NA>      Tyne and Wear Metro|Metro Station
#> 3               Glasgow Subway     <NA>                             SPT Subway
#> 4      Docklands Light Railway     <NA>                            DLR Station
#> 5         Manchester Metrolink     <NA>   Manchester Metrolink|[(]Metrolink[)]
#> 6                Midland Metro     <NA>      Midland Metro|West Midlands Metro
#> 7          Sheffield Supertram     <NA>                    Sheffield Supertram
#> 8   Nottingham Express Transit     <NA>                              Tram Stop
#> 9             Croydon Tramlink     <NA>                               Tramlink
#> 10           Blackpool Tramway     <NA>                      Blackpool Tramway
#> 11             Edinburgh Trams     <NA>                         Edinburgh Tram
#> 12    Birmingham Air-Rail Link      BHX                Air.?Rail Link|Skytrain
#> 13     Gatwick Airport shuttle     <NA>                       Terminal Shuttle
#> 14                  Luton DART     DART                                   <NA>
#> 15 Heritage and minor railways     <NA> Railway[)]|Rly[)]|[(]RHDR[)]|Mull Rail
#> 16            Weardale Railway     WRLY                                   <NA>
#> 17            London Cable Car      CAB                                   <NA>
#> 18            London Cable Car      EAL                                   <NA>
#> 19         Sheffield Supertram      STM                                   <NA>
#> 20  Nottingham Express Transit      NET                                   <NA>
#> 21               Midland Metro      TMM                                   <NA>
#> 22           Blackpool Tramway     1129                                   <NA>
#> 23           Blackpool Tramway     1001                                   <NA>
#> 24        Manchester Metrolink     1973                                   <NA>
#> 25        Manchester Metrolink     1976                                   <NA>
#>    route_type sources
#> 1           1     any
#> 2           1     any
#> 3           1     any
#> 4           1     any
#> 5           0     any
#> 6           0     any
#> 7           0     any
#> 8           0     any
#> 9           0     any
#> 10          0     any
#> 11          0     any
#> 12          0     any
#> 13          0     any
#> 14          0     txc
#> 15          2     any
#> 16          2     any
#> 17          6     txc
#> 18          6     txc
#> 19          0   nptdr
#> 20          0   nptdr
#> 21          0   nptdr
#> 22          0   nptdr
#> 23          0   nptdr
#> 24          0   nptdr
#> 25          0   nptdr
#>                                                                                              note
#> 1                                  NPTDR files it as a bus in 2004; not all stop names are marked
#> 2                                                                                                
#> 3                                                                         TNDS files it as a tram
#> 4                                                                     TNDS files it as heavy rail
#> 5                                                          NPTDR operator code changes every year
#> 6                                                                called Metro, runs on the street
#> 7                                                           NPTDR files it as a bus in most years
#> 8                                                    NPTDR: tram in 2006-2008, metro in 2009-2011
#> 9                                                                                                
#> 10                                                                                               
#> 11                                                                                               
#> 12                                  two stops, one a mainline station, so matched on the operator
#> 13                                              TNDS files it as metro in five of eight snapshots
#> 14                                 TNDS files it as heavy rail; two stops, one a mainline station
#> 15                               TNDS files the Bluebell as tram, rail and bus in different years
#> 16                                                      stops are named as ordinary rail stations
#> 17                                       TNDS files it as heavy rail; stop names identify nothing
#> 18                                                  the same cable car before the sponsor changed
#> 19 stops carry no marker before 2007; metro in 2004 and 2006, split between metro and bus in 2005
#> 20                 stops carry no marker in 2004, where the system is split between metro and bus
#> 21                        13 routes filed as metro in 2004, and their stop ids resolve to nothing
#> 22                                   20 tram routes under a code that also carries 192 bus routes
#> 23                                    the same arrangement one year later under a renumbered code
#> 24                18 routes; the Eccles line carries no suffix so the stop pattern reaches only 8
#> 25                             34 routes under a renumbered code; the stop pattern reaches only 7
#>    years from_route_type
#> 1   <NA>              NA
#> 2   <NA>              NA
#> 3   <NA>              NA
#> 4   <NA>              NA
#> 5   <NA>              NA
#> 6   <NA>              NA
#> 7   <NA>              NA
#> 8   <NA>              NA
#> 9   <NA>              NA
#> 10  <NA>              NA
#> 11  <NA>              NA
#> 12  <NA>              NA
#> 13  <NA>              NA
#> 14  <NA>              NA
#> 15  <NA>              NA
#> 16  <NA>              NA
#> 17  <NA>              NA
#> 18  <NA>              NA
#> 19  <NA>              NA
#> 20  <NA>              NA
#> 21  <NA>              NA
#> 22  2005               1
#> 23  2006               1
#> 24  2004               1
#> 25  2005               1
```
