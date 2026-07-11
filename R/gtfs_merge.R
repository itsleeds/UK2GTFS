#' merge a list of gtfs files
#'
#' !WARNING! only the tables:
#' agency, stops, routes, trips, stop_times, calendar, calendar_dates, shapes, frequencies
#' are processed, any other tables in the input timetables are passed through
#'
#' if duplicate IDs are detected then completely new IDs for all rows will be generated in the output.
#'
#' @param gtfs_list a list of gtfs objects to be merged
#' @param force logical, if TRUE duplicated values are merged taking the fist
#'   instance to be the correct instance, in most cases this is ok, but may
#'   cause some errors
#' @param quiet logical, if TRUE less messages
#' @param condenseServicePatterns logical, if TRUE service patterns across all routes are condensed into a unique set of patterns
#' @return a single merged gtfs object
#' @export
gtfs_merge <- function(gtfs_list, force = FALSE, quiet = TRUE, condenseServicePatterns = TRUE) {

  # remove any empty input GTFS objects
  gtfs_list <- gtfs_list[lengths(gtfs_list) != 0]
  flattened <- unlist(gtfs_list, recursive = FALSE)
  rm(gtfs_list)

  #The Atoc code has moved from data.frame to data.table for performance reasons, but the transXchange code hasn't migrated yet.
  #this is a breaking change for some items because the behaviour for data.table isn't the same as data.frame, despite extending data.frame. nice.
  #least painful way to fix this for now is to convert to data.table if supplied data.frame

  flattened <- lapply( flattened, function(item)
    {
      if ( inherits(item, "data.table" ) ) return (item)
      return (data.table::data.table(item))
    } )

  #get unique input table names
  tableNames <- unique(names(flattened))

  grouped_list <- list()

  # Loop through table names names and group data frames
  for (tableName in tableNames) {

    matched <- purrr::imap( flattened, function( item, name ) {
      if (name == tableName) {
        return(item)
      }
    })

    #remove input tables not matching tableName
    matched <- matched[lengths(matched) != 0]

    #assign each instance of the input table a unique number
    names(matched) <- seq(1, length(matched))

    #bind the instances of this table from every input GTFS together,
    #with a column containing this unique number
    matched <- data.table::rbindlist(matched, fill = TRUE, idcol = "file_id")


    #if("calendar_dates"==tableName)
    #{
    #  #don't understand what this complex line is doing ? comment would be nice.
    #  calendar_dates <- calendar_dates[sapply(calendar_dates, function(x){ifelse(is.null(nrow(x)),0,nrow(x))}) > 0]
    #  #matched <- matched[sapply(matched, function(x){ifelse(is.null(nrow(x)),0,nrow(x))}) > 0]
    #}

    #add to map
    grouped_list[[tableName]] <-  matched
  }

  rm(flattened)

  # Split out lists
  agency <- grouped_list$agency
  stops <- grouped_list$stops
  routes <- grouped_list$routes
  trips <- grouped_list$trips
  stop_times <- grouped_list$stop_times
  calendar <- grouped_list$calendar
  calendar_dates <- grouped_list$calendar_dates
  shapes <- grouped_list$shapes
  frequencies <- grouped_list$frequencies

  #remove items from map.
  grouped_list <- grouped_list[setdiff(names(grouped_list),
          c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates", "shapes", "frequencies" ))]


  # fix typo
  agency$agency_name <- as.character(agency$agency_name)
  agency$agency_name[agency$agency_name == "Dockland Light Railway"] <- "Docklands Light Railway"
  agency$agency_name[agency$agency_name == "Edward Bros"] <- "Edwards Bros"
  agency$agency_name[agency$agency_name == "John`s Coaches"] <- "John's Coaches"
  agency$agency_name[agency$agency_name == "Stagecoach in Lancaster."] <- "Stagecoach in Lancashire"
  agency$agency_name[agency$agency_name == "Stagecoach in South Wales"] <- "Stagecoach South Wales"

  # fix duplicated agency_ids - special cases
  #agency$agency_id[agency$agency_name == "Tanat Valley Coaches"] <- "TanVaCo"

  # if agency names are same as IDs but not always
  if (any(agency$agency_name == agency$agency_id)) {
    agency_sub <- agency
    agency_sub$file_id <- NULL
    agency_sub <- unique(agency)
    id_dups <- agency_sub$agency_id[duplicated(agency_sub$agency_id)]
    if (length(id_dups) > 0) {
      agency_sub <- agency_sub[agency_sub$agency_id %in% id_dups, ]
      agency_sub <- agency_sub[agency_sub$agency_id != agency_sub$agency_name, ]
      for (i in seq(1, nrow(agency_sub))) {
        agency$agency_name[agency$agency_name == agency_sub$agency_id[i]] <- agency_sub$agency_name[i]
      }
    } else {
      rm(agency_sub, id_dups)
    }
  }


  # agency
  agency$file_id <- NULL
  agency <- unique(agency)
  if (any(duplicated(agency$agency_id))) {
    # Check for upppercase problems
    # Sometime same agency with a capital letter in the name
    agency.check <- agency
    agency.check$agency_name <- tolower(agency.check$agency_name)
    agency.check <- unique(agency.check)
    if (any(duplicated(agency.check$agency_id))) {
      if(force){
        warning(paste0("Duplicated Agency IDs ",
                       paste(unique(agency.check$agency_id[duplicated(agency.check$agency_id)]), collapse = " "),
                       " will be merged"))
        # Assume 1st Name is correct name
        agency <- dplyr::group_by(agency, agency_id)
        agency <- dplyr::summarise(agency,
                                   agency_name = agency_name[1],
                                   agency_url = agency_url[1],
                                   agency_timezone = agency_timezone[1],
                                   agency_lang = agency_lang[1]
                                   )
      } else {
        stop("Duplicated Agency IDs: ",
                    paste(unique(agency.check$agency_id[duplicated(agency.check$agency_id)]), collapse = " "))
      }
    } else {
      agency <- agency[!duplicated(agency$agency_id), ]
    }
  }

  # stops
  stops$file_id <- NULL
  stops <- unique(stops)
  if (any(duplicated(stops$stop_id))) {
    if(force){
      stops <- stops[!duplicated(stops$stop_id),]
    } else {
      stop("Duplicated Stop IDS: ", paste( unique(stops$stop_id[duplicated(stops$stop_id)]), collapse = " "))
    }
  }

  # routes
  if (any(duplicated(routes$route_id))) {
    if(!quiet){message("De-duplicating route_id")}

    retainedColumnNames <- colnames(routes)[!(colnames(routes) %in% c("route_id", "file_id"))]

    new_route_id <- routes[, c("file_id", "route_id")]
    if (any(duplicated(new_route_id))) {
      if(force){
        routes <- routes[!duplicated(new_route_id), ]
        new_route_id <- routes[, c("file_id", "route_id")]
      } else {
        stop("Duplicated route_id within the same GTFS file, try using force = TRUE :",
             paste( unique(new_route_id$route_id[duplicated(new_route_id)]), collapse = " "))
      }
    }

    new_route_id$route_id_new <- seq(1, nrow(new_route_id))
    routes <- dplyr::left_join(routes, new_route_id, by = c("file_id", "route_id"))

    routes <- routes[, c("route_id_new", retainedColumnNames), with=FALSE]
    routes <- dplyr::rename(routes, route_id = route_id_new)
  }


  # calendar
  calendar_dates_key <- paste(calendar_dates$service_id, calendar_dates$date, calendar_dates$exception_type, sep="#")

  if (any(duplicated(calendar$service_id)) || any(duplicated(calendar_dates_key))) {
    if(!quiet){message("De-duplicating service_id")}

    new_service_id <- calendar[, c("file_id", "service_id")]
    if (any(duplicated(new_service_id))) {
      stop("Duplicated service_id within the same GTFS file: ",
           paste( unique(new_service_id$service_id[duplicated(new_service_id)]), collapse = " "))
    }

    # it is valid to have calendar_dates with no associated calendar (see comments further down)
    # so create the distinct set of service_id in both calendar and calendar_dates
    new_service_id <- dplyr::union(unique(new_service_id), unique(calendar_dates[, c("file_id", "service_id")]))

    new_service_id$service_id_new <- seq(1, nrow(new_service_id))

    retainedColumnNames <- colnames(calendar)[!(colnames(calendar) %in% c("service_id", "file_id"))]
    calendar <- dplyr::left_join(calendar, new_service_id, by = c("file_id", "service_id"))
    calendar <- calendar[, c("service_id_new", retainedColumnNames), with=FALSE]
    names(calendar) <- c("service_id", retainedColumnNames)

    if (nrow(calendar_dates) > 0) {
      retainedColumnNames <- colnames(calendar_dates)[!(colnames(calendar_dates) %in% c("service_id", "file_id"))]

      calendar_dates <- dplyr::left_join(calendar_dates, new_service_id, by = c("file_id", "service_id"))
      calendar_dates <- calendar_dates[, c("service_id_new", retainedColumnNames), with=FALSE]
      calendar_dates <- dplyr::rename(calendar_dates, service_id = service_id_new)
    }
  }


  # Trips
  if (any(duplicated(trips$trip_id))) {
    if(!quiet){message("De-duplicating trip_id")}

    new_trip_id <- trips[, c("file_id", "trip_id")]
    if (any(duplicated(new_trip_id))) {
      if(force){
        trips <- unique(trips)
        stop_times <- unique(stop_times)
        new_trip_id <- trips[, c("file_id", "trip_id")]
      } else{
        stop("Duplicated trip_id within the same GTFS file :",
          paste( unique( new_trip_id$trip_id[duplicated(new_trip_id)]), collapse = " "))
      }


    }
    new_trip_id$trip_id_new <- seq(1, nrow(new_trip_id))

    retainedColumnNames <- colnames(trips)[!(colnames(trips) %in% c("trip_id"))]
    trips <- dplyr::left_join(trips, new_trip_id, by = c("file_id", "trip_id"))
    trips <- trips[, c("trip_id_new", retainedColumnNames), with=FALSE]
    trips <- dplyr::rename(trips, trip_id = trip_id_new)

    retainedColumnNames <- colnames(stop_times)[!(colnames(stop_times) %in% c("trip_id", "file_id"))]
    stop_times <- dplyr::left_join(stop_times, new_trip_id, by = c("file_id", "trip_id"))
    stop_times <- stop_times[, c("trip_id_new", retainedColumnNames), with=FALSE]
    stop_times <- dplyr::rename(stop_times, trip_id = trip_id_new)

    if ( length(frequencies) > 0 )
    {
      retainedColumnNames <- colnames(frequencies)[!(colnames(frequencies) %in% c("trip_id", "file_id"))]
      frequencies <- dplyr::left_join(frequencies, new_trip_id, by = c("file_id", "trip_id"))
      frequencies <- frequencies[, c("trip_id_new", retainedColumnNames), with=FALSE]
      frequencies <- dplyr::rename(frequencies, trip_id = trip_id_new)
    }
  }

  if (exists("new_service_id")) {
    retainedColumnNames <- colnames(trips)[!(colnames(trips) %in% c("service_id"))]
    trips <- dplyr::left_join(trips, new_service_id, by = c("file_id", "service_id"))
    trips <- trips[, c(retainedColumnNames, "service_id_new"), with=FALSE]
    trips <- dplyr::rename(trips, service_id = service_id_new)
  }

  if (exists("new_route_id")) {
    retainedColumnNames <- colnames(trips)[!(colnames(trips) %in% c("route_id"))]
    trips <- dplyr::left_join(trips, new_route_id, by = c("file_id", "route_id"))
    trips <- trips[, c("route_id_new", retainedColumnNames), with=FALSE]
    trips <- dplyr::rename(trips, route_id = route_id_new)
  }

  trips$file_id <- NULL

  # Condense Duplicate Service patterns

  # in an ideal world we should not have a trip without a service pattern, and not have calendar_dates with no associated calendar,
  # but the real world data isn't that tidy.
  # In a typical all GB BODS extract Around 0.2% of trips have a calendar ID but no row in calendar,
  # 0.2% of calendar_dates have no trips, 0.1% of calendar_dates have no corresponding calendar.
  # we need to guard against this to make sure we don't end up putting null values into any key fields
  # This documentation https://gtfs.org/schedule/reference/#calendar_datestxt specifically mentions calendar dates without calendars
  # as being a legitimate way to construct the data.
  if(!is.null(nrow(calendar_dates))){
    if (condenseServicePatterns && nrow(calendar_dates) > 0) {
      if(!quiet){message("Condensing duplicated service patterns")}

      #find every unique combination of calendar_dates and calendar values
      calendar_dates_summary <- dplyr::group_by(calendar_dates, service_id)
      if( inherits(calendar_dates_summary$date, "Date") ){
        calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                                   pattern = paste(c(as.character(date), exception_type), collapse = "")
        )
      } else {
        calendar_dates_summary <- dplyr::summarise(calendar_dates_summary,
                                                   pattern = paste(c(date, exception_type), collapse = "")
        )
      }

      #we want to keep all rows in calendar_dates even if they don't have a row in calendar
      calendar_summary <- dplyr::full_join(calendar, calendar_dates_summary, by = "service_id")
      calendar_summary <- dplyr::group_by(
        calendar_summary,
        start_date, end_date, monday, tuesday, wednesday,
        thursday, friday, saturday, sunday, pattern
      )

      #give every unique combination of dates / days / exceptions a new distinct service ID
      calendar_summary$service_id_new <- dplyr::group_indices(calendar_summary)
      calendar_summary <- calendar_summary[, c("service_id_new", "service_id")]

      retainedColumnNames <- colnames(trips)[!(colnames(trips) %in% c("service_id", "route_id"))]
      trips <- dplyr::left_join(trips, calendar_summary, by = c("service_id"))
      trips <- trips[, c("route_id", "service_id_new", retainedColumnNames), with=FALSE]
      trips <- dplyr::rename(trips, service_id = service_id_new)

      retainedColumnNames <- colnames(calendar)[!(colnames(calendar) %in% c("service_id", "file_id"))]
      calendar <- dplyr::left_join(calendar, calendar_summary, by = c("service_id"))
      calendar <- calendar[, c("service_id_new", retainedColumnNames), with=FALSE]
      calendar <- dplyr::rename(calendar, service_id = service_id_new)
      calendar <- calendar[!duplicated(calendar$service_id), ]

      retainedColumnNames <- colnames(calendar_dates)[!(colnames(calendar_dates) %in% c("service_id", "file_id"))]
      calendar_dates <- dplyr::left_join(calendar_dates, calendar_summary, by = c("service_id"))
      calendar_dates <- calendar_dates[, c("service_id_new", retainedColumnNames), with=FALSE]
      calendar_dates <- dplyr::rename(calendar_dates, service_id = service_id_new)
      # Services condensed into one contribute identical copies of their full
      # exception set, so de-duplicate whole rows. De-duplicating on service_id
      # alone would discard all but one exception date per service.
      calendar_dates <- calendar_dates[!duplicated(calendar_dates[, c("service_id", "date", "exception_type"), with = FALSE]), ]
    }
  } else {
    #TODO: should off this option
    message("No calendar_dates, skipping condensing service pattern")
  }

  # shapes in a BODS extract are keyed on a UUID type string, so fairly improbable that the keys collide unless it's actually the same object
  composite_key <- paste0(shapes$shape_id, shapes$shape_pt_sequence, sep = "#")
  if (any(duplicated(composite_key))) {
    if(force){
      shapes <- shapes[!duplicated(composite_key),]
    } else {
      stop("Duplicated Shapes IDS :", paste( unique( composite_key[duplicated(composite_key)]), collapse = " "))
    }
  }

  if ("file_id" %in% colnames(stop_times) ) stop_times$file_id <- NULL
  if ("file_id" %in% colnames(calendar) ) calendar$file_id <- NULL
  if ("file_id" %in% colnames(shapes) ) shapes$file_id <- NULL
  if ("file_id" %in% colnames(routes) ) routes$file_id <- NULL
  if ("file_id" %in% colnames(frequencies) ) frequencies$file_id <- NULL
  res_final <- list(agency, stops, routes, trips, stop_times, calendar, calendar_dates, shapes, frequencies)
  names(res_final) <- c("agency", "stops", "routes", "trips", "stop_times", "calendar", "calendar_dates", "shapes","frequencies")

  #for tables we don't explicitly process - hope items are unique
  for (item in grouped_list) {
    item$file_id <- NULL
  }

  #remove nulls (e.g. tables that are often empty like frequencies)
  res_final <- Filter(Negate(is.null), res_final)

  return (c(res_final, grouped_list))
}
