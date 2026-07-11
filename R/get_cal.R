#' Get Bank Holiday Calendar
#'
#' Downloads and formats the bank holiday calendar for use with TransXchange
#' data.
#' @param url_ew url to ics file for England and Wales
#' @param url_scot url to ics file for Scotland
#' @return data frame
#' @details TransXchange records bank holidays by name (e.g. Christmas Day),
#'   some UK bank holidays move around, so this function downloads the official
#'   bank holiday calendar. The official feed only covers a short period of time
#'   so this may not be suitable for converting files from the past / future.
#' @export
#'
get_bank_holidays <- function(url_ew = "https://www.gov.uk/bank-holidays/england-and-wales.ics",
                              url_scot = "https://www.gov.uk/bank-holidays/scotland.ics") {
  message(" ")
  message("Scottish holidays are downloaded but not properly supported")
  dir.create(file.path(tempdir(), "UK2GTFS"))
  utils::download.file(
    url = url_ew,
    destfile = file.path(
      tempdir(), "UK2GTFS",
      "bankholidays_EW.ics"
    ),
    quiet = TRUE
  )
  utils::download.file(
    url = url_scot,
    destfile = file.path(
      tempdir(), "UK2GTFS",
      "bankholidays_Scot.ics"
    ),
    quiet = TRUE
  )
  cal_ew <- as.data.frame(calendar::ic_read(file.path(
    tempdir(), "UK2GTFS",
    "bankholidays_EW.ics"
  )))
  cal_scot <- as.data.frame(calendar::ic_read(file.path(
    tempdir(), "UK2GTFS",
    "bankholidays_Scot.ics"
  )))
  names(cal_ew) <- c("start", "date", "name", "UID", "SEQUENCE", "DTSTAMP")
  names(cal_scot) <- c("start", "date", "name", "UID", "SEQUENCE", "DTSTAMP")
  cal_ew <- cal_ew[, c("name", "date")]
  cal_scot <- cal_scot[, c("name", "date")]

  # Remove duplicated days from scotland
  cal <- rbind(cal_ew, cal_scot)
  cal <- cal[!duplicated(cal$date), ]
  cal$EnglandWales <- cal$date %in% cal_ew$date
  cal$Scotland <- cal$date %in% cal_scot$date
  cal <- cal[order(cal$date), ]

  # Normalise apostrophes (the ics file uses curly quotes, which are sometimes
  # mangled by encoding issues) then rename the gov.uk holiday names to the
  # names used by the TransXchange schema (e.g. BankHolidayStructure)
  cal$name <- gsub("\U2019|\U00E2\U20AC\U2122", "'", cal$name)

  # Substitute days (e.g. "Christmas Day (substitute day)") map to the
  # TransXchange displacement holidays (e.g. "ChristmasDayHoliday")
  substitute_day <- grepl("\\(substitute day\\)", cal$name)
  cal$name <- trimws(gsub("\\(substitute day\\)", "", cal$name))

  # The August "Summer bank holiday" is a different holiday (and date) in
  # Scotland to the one in England and Wales
  summer <- cal$name == "Summer bank holiday"
  cal$name[summer & cal$Scotland & !cal$EnglandWales] <- "AugustBankHolidayScotland"
  cal$name[summer] <- ifelse(cal$name[summer] == "Summer bank holiday",
                             "LateSummerBankHolidayNotScotland", cal$name[summer])

  rename <- c("New Year's Day" = "NewYearsDay",
              "2nd January" = "Jan2ndScotland",
              "Good Friday" = "GoodFriday",
              "Easter Monday" = "EasterMonday",
              "Early May bank holiday" = "MayDay",
              "Early May bank holiday (VE day)" = "MayDay",
              "Spring bank holiday" = "SpringBank",
              "St Andrew's Day" = "StAndrewsDay",
              "Christmas Day" = "ChristmasDay",
              "Boxing Day" = "BoxingDay")
  matched <- cal$name %in% names(rename)
  cal$name[matched] <- rename[cal$name[matched]]
  cal$name[substitute_day & matched] <- paste0(cal$name[substitute_day & matched], "Holiday")

  unlink(file.path(tempdir(), "UK2GTFS"), recursive = TRUE)

  return(cal)
}
