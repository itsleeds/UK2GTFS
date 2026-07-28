# Get Bank Holiday Calendar

Downloads and formats the bank holiday calendar for use with
TransXchange data.

## Usage

``` r
get_bank_holidays(
  url_ew = "https://www.gov.uk/bank-holidays/england-and-wales.ics",
  url_scot = "https://www.gov.uk/bank-holidays/scotland.ics"
)
```

## Arguments

- url_ew:

  url to ics file for England and Wales

- url_scot:

  url to ics file for Scotland

## Value

data frame

## Details

TransXchange records bank holidays by name (e.g. Christmas Day), some UK
bank holidays move around, so this function downloads the official bank
holiday calendar. The official feed only covers a short period of time
so this may not be suitable for converting files from the past / future.
