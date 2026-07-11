#' Download Test Files
#'
#' Download small examples of ATOC CIF and TransXchange files. These are mainly
#' meant for internal testing. But can also be used to check the package is
#' correctly installed.
#'
#' @param path path to folder
#' @param type If "atoc" then downloads ATOC CIF, else TransXchange
#' @return TRUE if the download succeeded, called for the side effect of
#'   downloading an example file
#' @export

dl_example_file <- function(path, type = "atoc"){
  url1 <- "https://github.com/ITSLeeds/UK2GTFS/releases/download/0.002/atoc.zip"
  url2 <- "https://github.com/ITSLeeds/UK2GTFS/releases/download/0.002/transxchange.zip"
  url3 <- "https://github.com/itsleeds/UK2GTFS/releases/download/0.002/NPTDR-sample.zip"
  if(type == "atoc"){
    utils::download.file(url = url1,
                         destfile = file.path(path,"atoc.zip"),
                         quiet = TRUE)
  } else if(type == "transxchange") {
    utils::download.file(url = url2,
                         destfile = file.path(path,"transxchange.zip"),
                         quiet = TRUE)
  } else if(type == "nptdr") {
    utils::download.file(url = url3,
                         destfile = file.path(path,"nptdr.zip"),
                         quiet = TRUE)
  } else {
    stop("Unknown type of data download: ",type)
  }
  return(TRUE)
}



