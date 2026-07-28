# Download Fares from National Rail Data Portal

Downloads fares from https://opendata.nationalrail.co.uk

## Usage

``` r
nrdp_fares(
  destfile = "fares.zip",
  username = Sys.getenv("NRDP_username"),
  password = Sys.getenv("NRDP_password"),
  url = "https://opendata.nationalrail.co.uk/api/staticfeeds/2.0/fares"
)
```

## Arguments

- destfile:

  Destination path and name of the zip file

- username:

  your username

- password:

  your password

- url:

  URL of data source

## Value

Invisibly returns NULL, called for the side effect of downloading a file
