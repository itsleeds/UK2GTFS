nrdp_authenticate = function(username = Sys.getenv("NRDP_username"),
                             password = Sys.getenv("NRDP_password")){

  # Make the POST request
  form_data <- list(
    username = username,
    password = password
  )

  response <- httr::POST(
    url =  'https://opendata.nationalrail.co.uk/authenticate',
    body = form_data,
    encode = "form"
  )

  json = httr::content(response)

  return(json)


}


#' Download Timetable from National Rail Data Portal
#'
#' Downloads ATOC CIF timetables from https://opendata.nationalrail.co.uk
#' @param destfile Destination path and name of the zip file
#' @param username your username
#' @param password your password
#' @param url URL of data source
#' @return Invisibly returns NULL, called for the side effect of downloading a file
#' @export

nrdp_timetable = function(destfile = "timetable.zip",
                          username = Sys.getenv("NRDP_username"),
                          password = Sys.getenv("NRDP_password"),
                          url = "https://opendata.nationalrail.co.uk/api/staticfeeds/3.0/timetable"){



  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", httr::status_code(response))
  }


}

#' Download Fares  from National Rail Data Portal
#'
#' Downloads fares from https://opendata.nationalrail.co.uk
#' @param destfile Destination path and name of the zip file
#' @param username your username
#' @param password your password
#' @param url URL of data source
#' @return Invisibly returns NULL, called for the side effect of downloading a file
#' @export
#'
nrdp_fares = function(destfile = "fares.zip",
                          username = Sys.getenv("NRDP_username"),
                          password = Sys.getenv("NRDP_password"),
                          url = "https://opendata.nationalrail.co.uk/api/staticfeeds/2.0/fares"){



  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", httr::status_code(response))
  }


}


#' Download routing  from National Rail Data Portal
#'
#' Downloads routing from https://opendata.nationalrail.co.uk
#' @param destfile Destination path and name of the zip file
#' @param username your username
#' @param password your password
#' @param url URL of data source
#' @return Invisibly returns NULL, called for the side effect of downloading a file
#' @export
#'
nrdp_routing = function(destfile = "routeing.zip",
                      username = Sys.getenv("NRDP_username"),
                      password = Sys.getenv("NRDP_password"),
                      url = "https://opendata.nationalrail.co.uk/api/staticfeeds/2.0/routeing"){



  token = nrdp_authenticate(username, password)

  response <- httr::GET(
    url =  url,
    httr::add_headers(`X-Auth-Token` = token$token)
  )

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Write the content of the response to a file
    writeBin(httr::content(response, "raw"), destfile)
    cat("File downloaded successfully to", destfile)
  } else {
    cat("Failed to download the file. Status code:", httr::status_code(response))
  }


}

