# Get naptan

Download the NaPTAN stop locations in CSV format. For more information
on NaPTAN see
https://data.gov.uk/dataset/ff93ffc1-6656-47d8-9155-85ea0b8f2251/national-public-transport-access-nodes-naptan

## Usage

``` r
get_naptan(
  url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv",
  naptan_extra = naptan_missing
)
```

## Arguments

- url:

  character, url to the csv format NaPTAN

- naptan_extra:

  data frame of missing stops default uses \`naptan_missing\`

## Value

data frame of stop locations

## Details

TransXchange does not store the location of bus stops, so this functions
downloads them from the offical DfT source. NaPTAN has some missing bus
stops which are added by UK2GTFS. See \`naptan_missing\`

Do not use this function for heavy rail use - download in XML format
which includes TIPLOC identifiers
