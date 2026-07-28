# Clean simple errors from GTFS files

Clean simple errors from GTFS files

## Usage

``` r
gtfs_clean(gtfs, public_only = FALSE)
```

## Arguments

- gtfs:

  gtfs list

- public_only:

  Logical, if TRUE remove routes with route_type missing

## Value

a gtfs object

## Details

Task done:

0\. Remove stops with no coordinates 1. Remove stops with no location
information 2. Remove trips with less than two stops 3. Remove stops
that are never used 4. Replace missing agency names with "MISSINGAGENCY"
5. If service is not public and public_only=TRUE then remove it
(freight, 'trips' aka charters) (these have a null route_type, so
loading into OpenTripPlanner fails if these are present) 6. If
public_only=TRUE then remove services with 'train_category' not for
public use. e.g. EE (ECS-Empty Coaching Stock) 7. Remove shapes that no
longer have any trips 8 Remove frequencies that no longer have any trips
9 Remove transfers, pathways and fare table rows (GTFS v1 and Fares v2)
that reference stops, routes or trips that no longer exist
