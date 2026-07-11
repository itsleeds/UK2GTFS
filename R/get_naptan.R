#' Get naptan
#'
#' Download the NaPTAN stop locations in CSV format. For more information on NaPTAN see
#' https://data.gov.uk/dataset/ff93ffc1-6656-47d8-9155-85ea0b8f2251/national-public-transport-access-nodes-naptan
#' @param url character, url to the csv format NaPTAN
#' @param naptan_extra data frame of missing stops default uses `naptan_missing`
#' @return data frame of stop locations
#' @details TransXchange does not store the location of bus stops, so this
#'   functions downloads them from the offical DfT source. NaPTAN has some
#'   missing bus stops which are added by UK2GTFS. See `naptan_missing`
#'
#'   Do not use this function for heavy rail use - download in XML format which includes TIPLOC identifiers
#'
#' @export

get_naptan <- function(url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=csv",
                       naptan_extra = naptan_missing) {

  load_data("naptan_missing")

  temp_folder <- file.path(tempdir(), "temp_naptan")
  dir.create(temp_folder, showWarnings = FALSE)

  tryCatch({
    utils::download.file(url = url, destfile = file.path(temp_folder, "Stops.csv"),
                         mode = "wb", quiet = TRUE)
    naptan <- data.table::fread(file.path(temp_folder, "Stops.csv"))
  }, finally = {
    unlink(temp_folder, recursive = TRUE)
  })

  # clean file
  naptan <- naptan[, c("ATCOCode", "NaptanCode", "CommonName", "Easting", "Northing")]
  names(naptan) <- c("stop_id", "stop_code", "stop_name", "Easting", "Northing")

  naptan <- sf::st_as_sf(naptan, coords = c("Easting", "Northing"), crs = 27700)
  naptan <- sf::st_transform(naptan, 4326)
  naptan <- cbind(sf::st_drop_geometry(naptan), sf::st_coordinates(naptan))
  names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lon", "stop_lat")

  # keep coordinates numeric: format() would convert them to space-padded
  # strings, which breaks downstream functions expecting numeric coordinates
  naptan$stop_lon <- round(naptan$stop_lon, 6)
  naptan$stop_lat <- round(naptan$stop_lat, 6)

  # Append alternative tags
  naptan_extra <- naptan_extra[!naptan_extra$stop_id %in% naptan$stop_id,]
  naptan_extra$stop_lon <- as.numeric(naptan_extra$stop_lon)
  naptan_extra$stop_lat <- as.numeric(naptan_extra$stop_lat)
  naptan <- rbind(naptan, naptan_extra)

  return(naptan)
}


# get_naptan <- function(url = "http://naptan.app.dft.gov.uk/datarequest/GTFS.ashx", naptan_extra = naptan_missing) {
#   utils::download.file(url = url, destfile = "naptan.zip", mode = "wb")
#   dir.create("temp_naptan")
#   utils::unzip("naptan.zip", exdir = "temp_naptan")
#   naptan <- utils::read.csv("temp_naptan/Stops.txt", stringsAsFactors = FALSE, sep = "\t")
#   unlink("temp_naptan", recursive = TRUE)
#   file.remove("naptan.zip")
#
#   # clean file
#   names(naptan) <- c("stop_id", "stop_code", "stop_name", "stop_lat", "stop_lon", "stop_url", "vehicle_type")
#   naptan <- naptan[, names(naptan_extra)]
#
#   # format
#   naptan$stop_lon <- format(round(naptan$stop_lon, 6), scientific = FALSE)
#   naptan$stop_lat <- format(round(naptan$stop_lat, 6), scientific = FALSE)
#
#   # Append extra data
#   naptan_extra <- naptan_extra[!naptan_extra$stop_id %in% naptan$stop_id, ]
#   naptan <- rbind(naptan, naptan_extra)
#
#   return(naptan)
# }



#' Get naptan xml doc
#'
#' Download the NaPTAN stop locations in XML format.
#' For more information on NaPTAN see https://beta-naptan.dft.gov.uk/
#' @param url character, url to the xml format NaPTAN
#' @param timeout int, timeout in seconds to wait for download to complete
#' @param method passed to url
#' @return xml document node
#' @details TransXchange does not store the location of bus stops, so this
#'   functions downloads them from the offical DfT source.
#'
#'   NrStations with multiple tiplocs seem to be represented as multiple natpan nodes,
#'   with one AnnotatedRailRef per naptan node, despite the schema supporting a multiplicity relationship.
#'   e.g. stourbridge, clapham junction.
#'   these can be joined together on the CRS
#'
#'   As of 2023 Naptan is published under a more permissive (OGL3) licence than ATOC data (creative commons licence).
#'
#' https://naptan.dft.gov.uk/naptan/schema/2.5.1/napt/NaPT_stop-v2-5-1.xsd
#'
#' <StopPoint
#'   <AtcoCode>
#'
#'   sequence of optional elements
#'   <NaptanCode>
#'   <PlateCode>
#'   <PrivateCode>
#'   <CleardownCode>
#'   <FormerStopPointRef>
#'
#'   <StopClassification
#'     <StopType (enum)
#'     <OnStreet|OffStreet
#'
#'       if 'OffStreet'
#'       <Air|Ferry|Rail|Metro|BusAndCoach|Telecabine
#'
#'         if 'Rail'
#'         <Entrance|AccessArea|Platform>
#'         <AnnotatedRailRef (0..n)
#'           <TiplocRef>
#'           <CrsRef>
#'           <StationName>
#'           <Location
#'
#'
#' StopType enum
#' =============
#' AIR, airportEntrance,
#' GAT, airAccessArea,
#' FTD, ferryTerminalDockEntrance,
#' FER, ferryDockAccessArea,
#' FBT, ferryBerth, FerryBerth,
#' RSE, railStationEntrance,
#' RLY, railAccessArea,
#' RPL, railPlatform,
#' TMU, tramMetroUndergroundStationEntrance,
#' MET, tramMetroUndergroundAccessArea,
#' PLT, tramMetroUndergroundPlatform,
#' BCE, busCoachTrolleyStationEntrance, busCoachTramStationEntrance,
#' BST, busCoachStationAccessArea,
#' BCS, busCoachTrolleyStationBay, busCoachTramStationBay,
#' BCQ, busCoachTrolleyStationVariableBay, busCoachTramStationVariableBay,
#' BCT, busCoachTrolleyOnStreetPoint, busCoachTramOnStreetPoint,
#' TXR, taxiRank,
#' STR, sharedTaxiRank,
#' SDA, carSetDownPickUpArea,
#' LSE, liftOrCableCarStationEntrance,
#' LCB, liftOrCableCarAccessArea,
#' LPL, liftOrCableCarPlatform
#'
#' @export
get_naptan_xml_doc <- function(url = "https://naptan.api.dft.gov.uk/v1/access-nodes?dataFormat=xml",
                               timeout=300L,
                               method = getOption("url.method", "default") )
{
  #shonky global option controls timeout; put it back however we leave
  currentTimeout = getOption("timeout")
  options(timeout=timeout)
  on.exit(options(timeout = currentTimeout), add = TRUE)

  stream = url(description=url, method=method, open = "rb" )
  on.exit(close( stream ), add = TRUE)

  doc = xml2::read_xml( stream, options="HUGE" )


  #debugging is a lot easier writing to a temp file
  #
  # folder = "temp_naptan"
  #
  # tryCatch({
  #   filename = paste0(folder, "/naptan_stops.xml")
  #
  #   dir.create( folder )
  #
  #   utils::download.file(url = url, destfile = filename, mode = "wb", quiet = FALSE )
  #
  #   stream = file(filename, open="rb")
  #   doc = xml2::read_xml( stream, options="HUGE" )
  #
  # }, finally = {
  #   options(timeout=currentTimeout)
  #   close( stream )
  #   unlink( folder, recursive = TRUE)
  # })

  return (doc)
}



#' as data table naptan stop point
#'
#' Unpacks selected naptan XML doc elements into data.table
#' @param doc xml document node
#' @param stopTypes list of stop types to restrict processing to (defaults to railway station)
#' @return data table of stop points
#' @details RLY stop types include TIPLOC & CRS fields. The quality of the geographic location is better than from BPLAN
#'
#' @export
as_data_table_naptan_stop_point <- function( doc, stopTypes = c("RLY") )
{
  #extract and rename the default namespace otherwise we can't select anything
  ns = xml2::xml_ns_rename( xml2::xml_ns(doc), d1 = "npt" )

  if ( length(stopTypes)<=0 || any(is.na(stopTypes)) || any(is.null(stopTypes)) )
  {
    SPnodes = xml2::xml_find_all( doc, "/*/*/npt:StopPoint", ns=ns )
  }
  else
  {
    condition = paste0("(npt:StopClassification/npt:StopType = '", stopTypes,"' )", collapse=" or " )
    SPnodes = xml2::xml_find_all( doc, paste0("/*/*/npt:StopPoint[", condition, "]"), ns=ns )
  }


  dt <- data.table::data.table(
    AtcoCode = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:AtcoCode/text()", ns = ns )),
    NaptanCode = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:NaptanCode/text()", ns = ns )),
    #railway stations don't generally have naptan code (very small number of entrances do)

    CommonName = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:Descriptor/npt:CommonName/text()", ns = ns )),
    ShortCommonName = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:Descriptor/npt:ShortCommonName/text()", ns = ns )),
    #railway stations don't generally have short common name

    Easting = xml2::xml_integer(xml2::xml_find_first( SPnodes, "./npt:Place/npt:Location/npt:Translation/npt:Easting/text()", ns = ns )),
    Northing = xml2::xml_integer(xml2::xml_find_first( SPnodes, "./npt:Place/npt:Location/npt:Translation/npt:Northing/text()", ns = ns )),
    Longitude = xml2::xml_double(xml2::xml_find_first( SPnodes, "./npt:Place/npt:Location/npt:Translation/npt:Longitude/text()", ns = ns )),
    Latitude = xml2::xml_double(xml2::xml_find_first( SPnodes, "./npt:Place/npt:Location/npt:Translation/npt:Latitude/text()", ns = ns )),

    StopType = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:StopType/text()", ns = ns )),
    ModeType = xml2::xml_name(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/*[1]", ns = ns )),
    RailType = xml2::xml_name(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/*[1]", ns = ns )),

    Tiploc = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/npt:AnnotatedRailRef/npt:TiplocRef/text()", ns = ns )),
    Crs = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/npt:AnnotatedRailRef/npt:CrsRef/text()", ns = ns )),

    #StationName = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/npt:AnnotatedRailRef/npt:StationName/text()", ns = ns )),
    #only half a dozen rows have a StationName different to CommonName - and CommonName is more consistent in having 'station' in the name

    #NrEasting = xml2::xml_integer(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/npt:AnnotatedRailRef/npt:Location/npt:Easting/text()", ns = ns )),
    #NrNorthing = xml2::xml_integer(xml2::xml_find_first( SPnodes, "./npt:StopClassification/npt:OffStreet/npt:Rail/npt:AnnotatedRailRef/npt:Location/npt:Northing/text()", ns = ns )),
    #only 18 rows have the NrEasting/NrNorthing different to the Easting/Northing
    #where there is a difference the other values are more accurate - generally where a station has been rebuilt/moved, these values are out of date
    #and the other values are correct, so difference isn't a great deal.
    #the exception is Warrington West, where the other values are correct, and these values are out by a comedy 130km.

    StopAreaRef = xml2::xml_text(xml2::xml_find_first( SPnodes, "./npt:StopAreas/npt:StopAreaRef/text()", ns = ns )),
    AdministrativeAreaRef = xml2::xml_integer(xml2::xml_find_first( SPnodes, "./npt:AdministrativeAreaRef/text()", ns = ns ))
  )

  return( dt )
}


#' as data table naptan stop area
#'
#' Unpacks selected naptan XML doc elements into data.table
#' @param doc xml document node
#' @return data table of stop areas
#'
#' @export
as_data_table_naptan_stop_area <- function( doc )
{
  #extract and rename the default namespace otherwise we can't select anything
  ns = xml2::xml_ns_rename( xml2::xml_ns(doc), d1 = "npt" )

  #the other interesting thing in the naptan doc are 'Stop Area'
  SAnodes = xml2::xml_find_all( doc, "/*/*/npt:StopArea", ns=ns )

  dt <- data.table::data.table(
    StopAreaCode = xml2::xml_text(xml2::xml_find_first( SAnodes, "./npt:StopAreaCode/text()", ns = ns )),
    Name = xml2::xml_text(xml2::xml_find_first( SAnodes, "./npt:Name/text()", ns = ns )),
    AdministrativeAreaRef = xml2::xml_integer(xml2::xml_find_first( SAnodes, "./npt:AdministrativeAreaRef/text()", ns = ns )),
    StopAreaType = xml2::xml_text(xml2::xml_find_first( SAnodes, "./npt:StopAreaType/text()", ns = ns )),

    Easting = xml2::xml_integer(xml2::xml_find_first( SAnodes, "./npt:Location/npt:Translation/npt:Easting/text()", ns = ns )),
    Northing = xml2::xml_integer(xml2::xml_find_first( SAnodes, "./npt:Location/npt:Translation/npt:Northing/text()", ns = ns )),
    Longitude = xml2::xml_double(xml2::xml_find_first( SAnodes, "./npt:Location/npt:Translation/npt:Longitude/text()", ns = ns )),
    Latitude = xml2::xml_double(xml2::xml_find_first( SAnodes, "./npt:Location/npt:Translation/npt:Latitude/text()", ns = ns ))
  )

  return( dt )
}


