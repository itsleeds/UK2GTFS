#' Try and Import a TransXchange XML file
#'
#' Wrapper around transxchange_import()
#'
#' @param file character, path to an XML file e.g. "C:/data/file.xml"
#' @param run_debug logical, if TRUE extra checks are performed, default FALSE
#' @param full_import logical, if false data no needed for GTFS is excluded
#' @param try_mode true
#' @noRd
transxchange_import_try <- function(file,
                                    run_debug = TRUE,
                                    full_import = FALSE,
                                    try_mode = TRUE){

  if(try_mode){
    res <- try(transxchange_import(file, run_debug = run_debug, full_import = full_import))

    if("try-error" %in% class(res)){

      return(paste0(file," with error: ",res[1]))
    } else {
      return(res)
    }
  } else {
    res <- transxchange_import(file, run_debug = run_debug, full_import = full_import)
    return(res)
  }

}


#' Try and Export a TransXchange XML file
#'
#' Wrapper around transxchange_export()
#'
#' @param obj transxchange object
#' @param run_debug logical, should debugs be done?
#' @param cal calendar
#' @param naptan naptan
#' @param quiet logical should messages be displayed
#' @param scotland logical should Scottish bank holidays be used?
#' @param try_mode true
#'
#' @noRd
transxchange_export_try <- function(obj,
                                    run_debug = TRUE,
                                    cal,
                                    naptan = NULL,
                                    quiet = TRUE,
                                    scotland = FALSE,
                                    try_mode = TRUE){
  if(try_mode){
    res <- try(transxchange_export(obj = obj,
                                   run_debug = run_debug,
                                   cal = cal,
                                   naptan = naptan,
                                   quiet = quiet,
                                   scotland = scotland))
    if("try-error" %in% class(res)){
      return(paste0(obj$filename," with error ",res[1]))
    } else {
      return(res)
    }
  } else{
    res <- transxchange_export(obj = obj,
                                   run_debug = run_debug,
                                   cal = cal,
                                   naptan = naptan,
                                   quiet = quiet,
                                   scotland = scotland)
    return(res)
  }
}


