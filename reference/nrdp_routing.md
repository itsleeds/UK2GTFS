# Download routing from National Rail Data Portal

Downloads routing from https://opendata.nationalrail.co.uk

## Usage

``` r
nrdp_routing(
  destfile = "routeing.zip",
  username = Sys.getenv("NRDP_username"),
  password = Sys.getenv("NRDP_password"),
  url = "https://opendata.nationalrail.co.uk/api/staticfeeds/2.0/routeing"
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
