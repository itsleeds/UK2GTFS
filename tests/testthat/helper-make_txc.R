# helper to write a minimal TransXchange file with the header fields the
# filter reads
make_txc <- function(dir, name, service, startdate, rev, modtime,
                     lines = character(0), enddate = NULL, desc = NULL,
                     noc = NULL, createtime = NULL, mode = NULL,
                     days = character(0)) {
  lines_xml <- if (length(lines)) {
    paste0("<Lines>",
           paste0(sprintf("<Line id=\"L%s\"><LineName>%s</LineName></Line>",
                          seq_along(lines), lines), collapse = ""),
           "</Lines>")
  } else {
    ""
  }
  end_xml <- if (is.null(enddate)) "" else sprintf("<EndDate>%s</EndDate>", enddate)
  desc_xml <- if (is.null(desc)) "" else sprintf("<Description>%s</Description>", desc)
  mode_xml <- if (is.null(mode)) "" else sprintf("<Mode>%s</Mode>", mode)
  # One vehicle journey carrying the day type, so the file declares which days
  # it runs. `days` takes the DaysOfWeek child element names, so a range such
  # as "MondayToFriday" is written as the schema allows it.
  days_xml <- if (length(days)) {
    paste0("<VehicleJourneys><VehicleJourney><OperatingProfile>",
           "<RegularDayType><DaysOfWeek>",
           paste0(sprintf("<%s/>", days), collapse = ""),
           "</DaysOfWeek></RegularDayType></OperatingProfile>",
           "</VehicleJourney></VehicleJourneys>")
  } else {
    ""
  }
  # `noc` may name more than one operator, in document order, for a jointly
  # registered service
  noc_xml <- if (is.null(noc)) "" else paste0(
    "<Operators>",
    paste0(sprintf(
      "<Operator><NationalOperatorCode>%s</NationalOperatorCode></Operator>",
      noc), collapse = ""),
    "</Operators>")
  if (is.null(createtime)) createtime <- modtime
  xml <- sprintf(
'<?xml version="1.0"?>
<TransXChange xmlns="http://www.transxchange.org.uk/" CreationDateTime="%s" ModificationDateTime="%s" RevisionNumber="%s">
  %s
  <Services>
    <Service RevisionNumber="%s" ModificationDateTime="%s">
      <ServiceCode>%s</ServiceCode>
      %s
      %s
      %s
      <OperatingPeriod><StartDate>%s</StartDate>%s</OperatingPeriod>
    </Service>
  </Services>
  %s
</TransXChange>', createtime, modtime, rev, noc_xml, rev, modtime, service,
    mode_xml, desc_xml, lines_xml, startdate, end_xml, days_xml)
  f <- file.path(dir, name)
  writeLines(xml, f)
  f
}

# the operating period a file ends up declaring, after any rewrite
period_of <- function(f) {
  x <- xml2::read_xml(f)
  svc <- xml2::xml_find_first(x, "d1:Services/d1:Service")
  c(start = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:StartDate")),
    end = xml2::xml_text(xml2::xml_find_first(svc, "d1:OperatingPeriod/d1:EndDate")))
}


