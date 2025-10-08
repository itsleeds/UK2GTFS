#functions relating to processing timetable overlay rules



setValueInThisEnvironment <- function( name, value )
{
  env <- asNamespace("UK2GTFS")
  unlockBinding(name, env)
  assign(name, value, envir = env)
  lockBinding(name, env)
}



assign("STOP_PROCESSING_UID", NULL )

set_STOP_PROCESSING_UID <- function( value )
{
  setValueInThisEnvironment("STOP_PROCESSING_UID", value)

  if(!is.null(value))
  {
    message(paste0(Sys.time(), " Set STOP_PROCESSING_UID to [", get("STOP_PROCESSING_UID"), "]"))
  }
}



#need to ensure these get set consistently into any worker processes as well as main thread

assign("TREAT_DATES_AS_INT", FALSE )

set_TREAT_DATES_AS_INT <- function( value )
{
  setValueInThisEnvironment("TREAT_DATES_AS_INT", as.logical(value))
}


assign("WDAY_LOOKUP_MIN_VALUE", NULL )

set_WDAY_LOOKUP_MIN_VALUE <- function( value )
{
  setValueInThisEnvironment("WDAY_LOOKUP_MIN_VALUE", as.integer(as.integer(value)-1L) )
}

assign("WDAY_LOOKUP_MAX_VALUE", NULL )

set_WDAY_LOOKUP_MAX_VALUE <- function( value )
{
  setValueInThisEnvironment("WDAY_LOOKUP_MAX_VALUE", as.integer(value))
}

assign("WDAY_LOOKUP_MAP", NULL )

set_WDAY_LOOKUP_MAP <- function( value )
{
  setValueInThisEnvironment("WDAY_LOOKUP_MAP", value)
}



local_lubridate_wday <- function( date, label = FALSE, week_start=1L )
{
  # if (TRUE==TREAT_DATES_AS_INT)
  # {
  #   if( any(date<=WDAY_LOOKUP_MIN_VALUE) || any(date>WDAY_LOOKUP_MAX_VALUE) )
  #   {
  #     stop("requested value index [", date, "] is outside lookup table")
  #   }
  #
  #   return ( WDAY_LOOKUP_MAP[ date-WDAY_LOOKUP_MIN_VALUE ] )
  # }
  # else
  # {
    return ( lubridate::wday( date, label = FALSE, week_start=1L ) )
  # }
}


local_seq_date<-function( from, to, by )
{
  if (TRUE==TREAT_DATES_AS_INT)
  {
    return ( seq.int(from = from, to = to) )
  }
  else
  {
    return ( seq.Date(from = from, to = to, by = by) )
  }
}


setupDatesCache<-function( calendar )
{
  minDt = min(calendar$start_date)
  maxDt = max(calendar$end_date)

  set_WDAY_LOOKUP_MIN_VALUE( minDt )
  set_WDAY_LOOKUP_MAX_VALUE( maxDt )

  firstWeek = as.integer(lubridate::wday( seq.Date(from = minDt, to = minDt+6L, by = "day"), label = FALSE, week_start=1L ))
  allWeeks = rep( firstWeek, length.out=( as.integer(maxDt) - as.integer(minDt) +1L ) )
  set_WDAY_LOOKUP_MAP( allWeeks )
}



#performance - this is slow, might be generating on the fly each time subset happens - cache it. -
LETTERS <- letters[1L:26L]
TWO_LETTERS <- paste0(rep(letters, each = 26L), rep(letters, times = 26L))

# Append to the UID to note the changes - and ensure that all service_id's in the output file remain unique
appendLetterSuffix <- function( cal )
{
  rows = nrow(cal)

  if (rows > 1L)
  {
    if (rows <= 26L)
    {
      cal$UID <- paste0(cal$UID, " ", LETTERS[1L:rows])
    }
    else
    {
      # Cases where we need extra letters, gives up to 676 ids
      cal$UID <- paste0(cal$UID, " ", TWO_LETTERS[1L:rows])
    }
  }

  return (cal)
}


# Append to the UID to note the changes - and ensure that all service_id's in the output file remain unique
appendNumberSuffix<-function( cal, numToAppend )
{
  if( numToAppend>1L ) #don't need to append a new number if we only have one pattern
  {
    # further differentiate the UID by appending a number to the end for each different days pattern
    cal$UID <- paste0(cal$UID, numToAppend)
  }

  return (cal)
}



# in a week bitmask, if there are non-operating days between the first and last operating day of the week - will return TRUE
# e.g.    0010000 = FALSE      0011100 = FALSE       0101000 = TRUE
hasGapInOperatingDays <- function( daysBitmask )
{
  firstDay = stringi::stri_locate_first( daysBitmask, fixed = "1" )[,1L]
  lastDay = stringi::stri_locate_last( daysBitmask, fixed = "1" )[,1L]

  operatingDayCount = stringi::stri_count( daysBitmask, fixed = "1" )

  res = ( lastDay-firstDay+1L != operatingDayCount )

  res[is.na(res)] <- FALSE #shouldn't really get this, probably operating days are '0000000'

  return( res )
}



DATE_EPOC <- as.Date(lubridate::origin) # 01/01/1970
WEEKDAY_NAME_VECTOR <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")
START_PATTERN_VECTOR = c("1","01","001","0001","00001","000001","0000001")
END_PATTERN_VECTOR = c("1000000","100000","10000","1000","100","10","1")

#calendars should start on the first day they are effective, and end on the last day.
#i.e. if the first day in the day bitmask is Tuesday - then the start date should be Tuesday, not some other day.
validateCalendarDates <- function( calendar )
{
  start_day_number = local_lubridate_wday( calendar$start_date, label = FALSE, week_start=1L )
  end_day_number = local_lubridate_wday( calendar$end_date, label = FALSE, week_start=1L )

  startOk <- START_PATTERN_VECTOR[ start_day_number ] == stringi::stri_sub(calendar$Days, 1L, start_day_number)
  endOk <- END_PATTERN_VECTOR[ end_day_number ] == stringr::str_sub(calendar$Days, end_day_number, 7L)

  return (startOk & endOk)
}




#' split and rebind bitmask
#'
#' @details
#' splits 'Days' bitmask into individual logical fields called monday, tuesday, etc...
#'
#' @param calendar data.table of calendar items
#' @noRd
#'
splitAndRebindBitmask <- function( calendar )
{
  calMat = splitBitmaskMat( calendar$Days, asInteger=FALSE )

  #this function gets expensive if you call it a lot, creating data.table takes a while
  return (cbind( calendar, data.table::as.data.table(calMat) ) )
}

splitBitmaskMat <- function( bitmaskVector, asInteger=FALSE )
{
  splitDays = splitBitmask( bitmaskVector, asInteger=asInteger )

  return (matrix(splitDays, ncol=7, byrow=TRUE, dimnames=list(NULL,WEEKDAY_NAME_VECTOR)))
}

splitBitmask <- function( bitmask, asInteger=FALSE )
{
  duff = which( nchar(bitmask) != 7L )

  bitmask[duff] = "0000000"

  splitDays = ( "0"!= unlist( strsplit(bitmask, "") ) )
  #performance, calling as.integer on string is surprisingly expensive, so do it this way instead which is twice as fast overall

  if (asInteger)
  {
    splitDays = as.integer(splitDays)
  }

  return (splitDays)
}



checkOperatingDayActive <- function(calendar) {

  if (all(calendar$duration >= 7L))
  {
    return (calendar$Days!="0000000")
  }

  #get a list of days of week that the timetable is valid on
  opDays <- splitBitmaskMat( calendar$Days, asInteger=FALSE )
  opDays <- split(opDays, row(opDays))

  #performance - precalculate all the days
  veryfirstDay = min(calendar$start_date)

  if( TRUE==TREAT_DATES_AS_INT)
  {
    allDays = WDAY_LOOKUP_MAP[ (veryfirstDay - WDAY_LOOKUP_MIN_VALUE) : (max(calendar$end_date) - WDAY_LOOKUP_MIN_VALUE) ]
  }
  else
  {
    allDays = local_lubridate_wday( local_seq_date(from = veryfirstDay, to = max(calendar$end_date), by = "day")
                             , label = FALSE, week_start=1L )
  }
  veryfirstDay = veryfirstDay - 1L

  checkValid <- function(dur, sd, ed, od ){

    if (dur >= 7L)
    {
      return (any(od))
    }

    firstDay = as.integer(sd)-as.integer(veryfirstDay)
    lastDay = as.integer(ed)-as.integer(veryfirstDay)

    dayNumbers <- allDays[ firstDay:lastDay ]

    return ( any(od[dayNumbers]) )
  }

  validCalendars <- mapply( checkValid, calendar$duration,
                            calendar$start_date, calendar$end_date,
                            opDays, SIMPLIFY = TRUE )
  return (validCalendars)
}



CALENDAR_UNIQUE_CHECK_COLUMN_NAMES <- c("originalUID","start_date","end_date","Days","STP","duration" )

# We have lots of complex logic, which means that when we have multiple base timetables that are separated
# in the temporal domain e.g. march, april - we end up duplicating the overlays
#
# this is a bit of a gluey hack that could be fixed by looking in the temporal domain when deciding what overlaps
# see test case no.10 ("10:test makeCalendarInner") that triggered addition of this logic
#
makeCalendarsUnique <- function ( calendar )
{
  calendar <- calendar[ !duplicated( calendar, by=CALENDAR_UNIQUE_CHECK_COLUMN_NAMES ) ]

  return( calendar )
}




countIntersectingDayPatterns <- function( dayPatterns )
{
  unpacked = splitBitmaskMat( dayPatterns, asInteger = TRUE )
  sums = colSums(unpacked) #add up number of intersections for monday etc...
  names(sums) <- NULL #makes unit test construction easier
  return ( sums )
}

intersectingDayPattern <- function( dayPattern1, dayPattern2 )
{
  return (any( countIntersectingDayPatterns( c(dayPattern1,dayPattern2) ) > 1L) )
}


intersectingDayPatterns <- function( dayPatternBase, dayPatternOverlay )
{
  if (is.null(dayPatternOverlay) || is.null(dayPatternBase) ) return (NULL)

  unpackedOverlay = splitBitmaskMat( dayPatternOverlay, asInteger = FALSE )
  unpackedBase = splitBitmaskMat( dayPatternBase, asInteger = FALSE )

  #repeat the base for every Overlay
  unpackedBaseRep = rep( unpackedBase, length(dayPatternOverlay) )
  unpackedBaseRepmat = matrix(unpackedBaseRep, ncol=7, byrow=TRUE)

  intersects = unpackedBaseRepmat & unpackedOverlay

  res <- apply(intersects, 1L, any)

  return ( res )
}



SINGLE_DAY_PATTERN_VECTOR = c("1000000","0100000","0010000","0001000","0000100","0000010","0000001")

SINGLE_DAY_PATTERN_LIST = list(c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE),
                               c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
                               c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE),
                               c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE),
                               c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
                               c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE),
                               c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE))


makeReplicationDates <- function(cal, startDayNum, endDayNum){

  #make a sequences of dates, offsetting the start date so it's always monday (aligning with bitmask start day)
  #                           and the end date so it's always sunday
  firstDate = min(cal$start_date) - 7L
  lastDate = max(cal$end_date) + 7L
  allDates = local_seq_date(from = firstDate, to = lastDate, by = "day")

  offset = as.integer(cal$start_date)-startDayNum+2L-as.integer(firstDate)
  end = as.integer(cal$end_date)+8L-endDayNum-as.integer(firstDate)

  dates <- Map(function(o, e) allDates[o:e], offset, end)

  if (TRUE==TREAT_DATES_AS_INT)
  {
    res = unlist(dates)
  }
  else
  {
    res = as.Date( unlist(dates), origin = DATE_EPOC )
  }

  return (res)
}



#' replicates the input calendar objects into single day duration calendar objects
#' calender objects should NOT have had the 'days' bitmask field unpacked
#' (will still produce an output but the unpacked monday, tuesday etc fields will no longer be consistent with the packed 'Days' bitmask)
#'
#' @param cal data.table containing all the calendars to be split up into individual days
#' @noRd
#'
makeAllOneDay <- function( cal )
{
  duration <- cal$end_date - cal$start_date + 1L

  if ( 0L==nrow(cal) || all(1L == duration))
  {
    #nothing to do
    return (cal)
  }

  #make a list of dates for each object being replicated
  startDayNum = local_lubridate_wday( cal$start_date, label = FALSE, week_start=1L )
  endDayNum = local_lubridate_wday( cal$end_date, label = FALSE, week_start=1L )
  dateSequence = makeReplicationDates( cal, startDayNum, endDayNum )

  #work out how many time we need to replicate each item: number of operating days in week * num weeks
  bitmaskMat = splitBitmaskMat( cal$Days, asInteger=FALSE )
  dayCount = rowSums(bitmaskMat)
  numWeeks <- ceiling(as.integer(cal$duration) / 7L)
  repetitions = dayCount * numWeeks

  #replicate the calendar rows the appropriate number of times
  replicatedcal <- cal[rep(seq_len(nrow(cal)), times = repetitions),]

  #get a mask of operating days
  operatingDayLogical <- rep( split(bitmaskMat, row(bitmaskMat)), times = numWeeks)

  #set the start and end date for each calender item to the single day identified earlier
  selectedDates = dateSequence[unlist(operatingDayLogical)]
  replicatedcal$end_date <- replicatedcal$start_date <- selectedDates
  #tidy up the values so they are correct for the spilt items
  replicatedcal$duration <- difftime(Sys.Date(),Sys.Date()-1, units = "days") # make a 1 day difftime
  replicatedcal$Days = SINGLE_DAY_PATTERN_VECTOR[ local_lubridate_wday( replicatedcal$start_date, label = FALSE, week_start=1L ) ]

  return (replicatedcal)
}




#' along a similar line to 'makeAllOneDay' duplicates input calendar objects into single WEEK duration calendar objects
#'
#' @param cal data.table containing all the calendars to be split up into individual weeks
#' @noRd
#'
expandAllWeeks <- function( cal )
{
  if ( 0L==nrow(cal) )
  {
    #nothing to do
    return (cal)
  }
  #duration <- cal$end_date - cal$start_date + 1
  #make a list of dates for each object being replicated
  startDayNum = local_lubridate_wday( cal$start_date, label = FALSE, week_start=1L )
  endDayNum = local_lubridate_wday( cal$end_date, label = FALSE, week_start=1L )
  dateSequence = makeReplicationDates( cal, startDayNum, endDayNum )
  numWeeks <- ceiling(as.integer(cal$duration) / 7L)

  #replicate a logical vector for the start date and use that to select the relevant dates from the date sequence
  startDayLogical <- SINGLE_DAY_PATTERN_LIST[startDayNum]
  startDays <- rep(startDayLogical, times = numWeeks)
  startDates <- dateSequence[ unlist(startDays) ]

  #replicate a logical vector for the end date and use that to select the relevant dates from the date sequence
  endDayLogical <- SINGLE_DAY_PATTERN_LIST[endDayNum]
  endDays <- rep(endDayLogical, times = numWeeks)
  endDates <- dateSequence[ unlist(endDays) ]
  #replicate the calendar rows the appropriate number of times
  replicatedcal <- cal[rep(seq_len(nrow(cal)), times = numWeeks),]
  #set the start and end date for each calender item
  replicatedcal$start_date <- startDates
  replicatedcal$end_date <- endDates

  #tidy up the values so they are correct for the spilt items
  replicatedcal$duration <- replicatedcal$end_date - replicatedcal$start_date + 1L
  return (replicatedcal)
}



#' allocate Cancellations Across Calendars
#'
#' @details
#' expects input calendar items to have been separated out into non-overlapping dates
#' and 'Days' bitmask unpacked into separate int or logical attributes
#'
#' "originalUID" is used to identify where the cancellations originally came from
#' after allocating across the split calender items the cancellations will have an updated
#' "UID" that says which calender they are now associated with
#'
#' @param calendar data.table of calendar items that are NOT cancellations (that has 'Days' bitmask unpacked )
#' @param cancellations data.table of calender items that ARE cancellations (that has 'Days' bitmask unpacked )
#' @noRd
#'
allocateCancellationsAcrossCalendars <- function( calendar, cancellations )
{
  tempNames = names(calendar)

  #stash some join fields away because we want to keep the data from the cancellations rather than the calendar table
  #which otherwise get over-written by the join process
  cancellations$start_date2 <- cancellations$start_date
  cancellations$end_date2 <- cancellations$end_date
  cancellations$UID <- NULL #we want the new UID from the calendar entries

  #left join cancellations to calendar by the original service ID
  #and the date of the cancellation lying inside the period of the calendar
  #and the day of the cancellation is an operating day of the calendar item
  joined = cancellations[calendar, on = .(originalUID==originalUID,
                                          start_date>=start_date,
                                          end_date<=end_date), nomatch = 0L][
                                            ((i.monday&monday) | (i.tuesday&tuesday) | (i.wednesday&wednesday)
                                             | (i.thursday&thursday) | (i.friday&friday) | (i.saturday&saturday) | (i.sunday&sunday)), ]
  #revert the stashed (join) fields
  joined$start_date <- joined$start_date2
  joined$start_date2 <- NULL
  joined$end_date <- joined$end_date2
  joined$end_date2 <- NULL

  #remove joined fields we don't need
  joined <- joined[, .SD, .SDcols=tempNames]

  #belt and braces - fix any NA fields by reverting from the original UID
  joined[is.na(UID), UID := originalUID]

  return( joined )
}



CALENDAR_COLS_TO_COPY <- c("UID", "Days", "STP", "rowID" )

NOT_NEEDED <- c("__NOT_NEEDED_MARKER__~@$$%&*((")


#this function is massively performance critical - profile any changes to it
selectOverlayTimeableAndCopyAttributes <- function(cal, calNew, rowIndex)
{
  #if we have two adjacent complete items e.g. ....end 13th Jan      start 14th jan.....
  #then it's not a real gap and just an artefact of the algorithm use to generate the dates
  if( rowIndex>1L && rowIndex<nrow(calNew)
      && !is.na(calNew$UID[rowIndex-1L]) && !is.na(calNew$UID[rowIndex+1L])
      && calNew$end_date[rowIndex-1L]+1L == calNew$start_date[rowIndex+1L])
  {
    calNew$UID[rowIndex] <- NOT_NEEDED
    return (calNew)
  }

  #get candidate base timetable(s) that the new period sits within
  baseTimetableIndexes = cal[ cal$start_date <= calNew$start_date[rowIndex]     #performance - this is an expensive line
                              & cal$end_date >= calNew$end_date[rowIndex],,which=TRUE]

  #are we in a gap between two base timetables with no overlays
  if ( length(baseTimetableIndexes)<=0L )
  {
    calNew$UID[rowIndex] <- NOT_NEEDED
    return (calNew)
  }


  # apply timetable overlay selection logic - pick highest priority timetable type
  # as per https://wiki.openraildata.com/index.php/SCHEDULE
  # "Conveniently, it also means that the lowest alphabetical STP indicator wins - 'C' and 'O' are both lower in the alphabet than 'P'."

  #pick the lowest alphabetic STP (highest priority), and just in case there is more than one, the shortest duration one.

  #priorityTimetable <- baseTimetables[order(STP, duration), head(.SD, 1)]
  #performance we pre-sort all the entries by the priority & duration
  #this speeds things up when we look up the required priority overlay **SEE_NOTE**
  #so we don't need to sort again here, just pick the top filtered result

  data.table::set( calNew, i = rowIndex, j = CALENDAR_COLS_TO_COPY, value = cal[ baseTimetableIndexes[1L], CALENDAR_COLS_TO_COPY, with=FALSE ] )
  #set() runs 3x faster than this style of copy      calNew[rowIndex,] <- cal[baseTimetableIndexes[1],]

  return (calNew)
}



# triggered by test case "10:test makeCalendarInner"
# when we have a 1 day overlay sitting on the start/end date of a base timetable
# the dates overlap - fix it
fixOverlappingDates <- function( cal )
{
  rowCount = nrow(cal)

  #forwards
  for (j in seq(1L, rowCount)) {

    #adjust our end date if next item a higher priority overlay
    if (j<rowCount && !is.na(cal$UID[j]) && !is.na(cal$UID[j+1L]) )
    {
      if ( cal$STP[j+1L] < cal$STP[j] )
      {
        cal$end_date[j] <- cal$start_date[j+1L] -1L
      }

      if(j>1L && !is.na(cal$UID[j-1L]) && cal$STP[j-1L] < cal$STP[j] )
      {
        cal$start_date[j] <- cal$end_date[j-1L] +1L
      }
    }
  }

  #backwards
  for (j in seq(rowCount, 1L)) {

    #adjust our end date if previous item a higher priority overlay
    if (j>1L && !is.na(cal$UID[j]) && !is.na(cal$UID[j-1L]) )
    {
      if ( cal$STP[j-1L] < cal$STP[j] )
      {
        cal$start_date[j] <- cal$end_date[j-1L] +1L
      }

      if(j<rowCount && !is.na(cal$UID[j+1L]) && cal$STP[j+1L] < cal$STP[j] )
      {
        cal$end_date[j] <- cal$start_date[j+1L] -1L
      }
    }
  }

  return (cal)
}




#' split overlapping start and end dates
#' duplicated items have the same rowId as the original but a new UID with an alpha character appended to it.
#'
#' this function is performance critical - profile any changes
#'
#' THIS ONLY WORKS ON ITEMS WHERE THE DAY PATTERNS ARE ALL THE SAME
#' (or are only 1 day DURATION)
#'
#' @param cal calendar object
#' @details split overlapping start and end dates
#' @noRd

splitDates <- function(cal) {

  # get a vector of all the start and end dates together from all base & overlay timetables and sort them
  dates <- c(cal$start_date, cal$end_date)
  dates <- dates[order(dates)]

  # create all unique pairs so we know how to chop the dates up into non-overlapping periods
  dates.dt <- unique( data.table::data.table(
    start_date = dates[seq(1L, length(dates) - 1L)],
    end_date = dates[seq(2L, length(dates))]
  ) )

  #left join back to the source data so we can see which (if any) date segments we have already covered, and which we need to replicate
  calNew <- cal[dates.dt, on = c("start_date", "end_date")]

  #some dates may already be overlapping
  calNew <- fixOverlappingDates( calNew )

  # fill in the missing schedule parts from the original
  # the filled in parts should (if the data is correctly layered) be the highest priority part of the timetable

  # we make multiple passes over the timetable working our way outwards from completed items to NA items

  rowCount = nrow(calNew)

  for (i in seq(1L,10L)) #should really be a max of 3 passes because we can only have base, one overlay, and cancel
  {
    #forwards
    for (j in seq(1L, rowCount)) {

      #if we are not valid & the next item is already valid, fill in our details and adjust our end date
      if (j<rowCount && is.na(calNew$UID[j]) && !is.na(calNew$UID[j+1L]) )
      {
        calNew <- selectOverlayTimeableAndCopyAttributes(cal, calNew, j)

        if ( NOT_NEEDED != calNew$UID[j+1L])
        {
          calNew$end_date[j] <- calNew$start_date[j+1L] -1L
        }

        #if previous item valid adjust our start date
        if(j>1L && !is.na(calNew$UID[j-1L]) && NOT_NEEDED != calNew$UID[j-1L] )
        {
          calNew$start_date[j] <- calNew$end_date[j-1L] +1L
        }
      }
    }

    #backwards
    for (j in seq(rowCount, 1L)) {

      #if we are not valid & the previous item is already valid, fill in our details and adjust our start date
      if (j>1L && is.na(calNew$UID[j]) && !is.na(calNew$UID[j-1L]) )
      {
        calNew <- selectOverlayTimeableAndCopyAttributes(cal, calNew, j)

        if ( NOT_NEEDED != calNew$UID[j-1L])
        {
          calNew$start_date[j] <- calNew$end_date[j-1L] +1L
        }

        #if next item valid adjust our start date
        if(j<rowCount && !is.na(calNew$UID[j+1L]) && NOT_NEEDED != calNew$UID[j+1L]  )
        {
          calNew$end_date[j] <- calNew$start_date[j+1L] -1L
        }
      }
    }

    if ( !any( is.na(calNew$UID) ) ) break #if all done jump out of loop
  }

  # fix duration
  calNew$duration <- calNew$end_date - calNew$start_date + 1L

  #remove the items we know are not needed
  #  calNew <- calNew[ NOT_NEEDED != calNew$UID, ]

  # remove any gaps. this can occur when we have multiple base timetables over a period of time
  # e.g. a March timetable and a May timetable, with a gap in April where there is no base timetable
  #  calNew <- calNew[!is.na(calNew$UID), ]

  # remove cancelled trips (we just leave a gap in the calendar with nothing running)
  #  calNew <- calNew[calNew$STP != "C", ]

  # remove any zero or negative day schedules
  #  calNew <- calNew[calNew$duration > 0L, ]

  #performance, do all subsets in one go
  calNew <- calNew[ (!is.na(UID)) & (get("NOT_NEEDED") != UID) & (STP != "C") & (duration > 0L), ]

  # Append UID to note the changes
  if (nrow(calNew) > 0L)
  {
    calNew <- appendLetterSuffix( calNew )
  }
  else
  {
    calNew <- NA
  }

  return(calNew)
}





#' make calendar helper function
#' this originally expected and dealt with cancellations too. This worked ok for single day duration cancellations
#' but had problems with multi-day cancellations when combined with overlays
#' code hasn't been changed to reject / avoid cancellations but results may not be predictable / tested scenarios
#' @param calendarSub data.table containing all the calendars (aka CIF operating patterns) for a single service
#' @noRd
#'
makeCalendarInner <- function(calendarSub) {

  if ( 1L == nrow(calendarSub) )
  {
    # make into an single entry
    res = list(calendarSub, NA)
  }
  else
  {
    if (length(unique(calendarSub$UID)) > 1L)
    {
      stop(paste("Error: makeCalendarInner was passed more than one service to work on. service=", unique(calendarSub$UID)))
    }

    # check duration and types
    allTypes <- calendarSub$STP

    # as per https://wiki.openraildata.com/index.php/SCHEDULE
    # "Conveniently, it also means that the lowest alphabetical STP indicator wins - 'C' and 'O' are both lower in the alphabet than 'P'."
    baseType = max(allTypes) #usually we expect 'P' to be the base timetable... but it can also be STP service in which case it will be 'N'

    overlayDurations <- as.numeric(calendarSub$duration[calendarSub$STP != baseType])
    overlayTypes <- calendarSub$STP[calendarSub$STP != baseType]

    if( length(overlayDurations) <= 0L )
    {
      #assume the input data is good and the base timetables don't break any of the overlaying /operating day rules
      res = list( appendLetterSuffix(calendarSub), NA)
    }
    #if every overlay is a one day cancellation
    else if ( all(overlayDurations == 1L) && all(overlayTypes == "C") )
    {
      #warning("Unexpected item in the makeCalendarInner-ing area, cancellations should now be handled at a higher level (1)")

      # Apply the cancellation via entries in calendar_dates.txt
      res = list( appendLetterSuffix( calendarSub[calendarSub$STP != "C", ] ),
                  calendarSub[calendarSub$STP == "C", ])
    }
    else
    {
      uniqueDayPatterns <- unique(calendarSub$Days[calendarSub$STP != "C"])

      # if the day patterns are all identical
      if (length(uniqueDayPatterns) <= 1L )
      {
        #performance pre-sort all the entries by the priority
        #this speeds things up when we look up the required priority overlay **SEE_NOTE**
        #calendarSub = calendarSub[ order(STP, duration), ]
        data.table::setkey( calendarSub, STP, duration )
        data.table::setindex( calendarSub, start_date, end_date)

        calendar_new <- try(makeCalendarsUnique( splitDates(calendarSub) ), silent = TRUE)
        if(inherits(calendar_new,"try-error")){
          stop("Failed to split dates for UID: ",calendarSub$UID[1])
        }

        res = list(calendar_new, NA)
      }
      else # split by day pattern
      {
        res = try(makeCalendarForDifferentDayPatterns( calendarSub, uniqueDayPatterns ), silent = TRUE)
        if(inherits(res,"try-error")){
          stop("Failed to makeCalendarForDifferentDayPatterns for UID: ",calendarSub$UID[1])
        }
      }
    }
  }


  if ( !is.null(STOP_PROCESSING_UID) )
  {
    if ( any( STOP_PROCESSING_UID==calendarSub$UID) )
    {
      message(paste0(Sys.time(), " Reached STOP_PROCESSING_UID value [", unique(calendarSub$UID), "] length=", length(calendarSub$UID)))
      stop("Option:UK2GTFS_option_stopProcessingAtUid has been set: Stopped processing at UID=", STOP_PROCESSING_UID)
    }
  }

  return (res)
}




# this is a complex case where the overlays don't have the same day pattern as the base timetable
#
# e.g base is mon-sat, and we have some engineering work for 3 weeks tue-thur
#
# the approach we take is to duplicate the overlay timetables for every week they are in effect, then overlay them.
#
# aha but the complexity isn't finished. If the overlay is tue+thur then wed is the base timetable.
#
# when we get to this latter complexity we just split the overlay into individual days and apply it that way.
#
makeCalendarForDifferentDayPatterns <- function( calendar, uniqueDayPatterns )
{
  baseType = max(calendar$STP)
  baseTimetables =  calendar[calendar$STP == baseType,]
  overlayTimetables =  calendar[calendar$STP != baseType,]
  #do the day patterns overlap each other in any way ?
  #e.g. a mon-sat pattern with a wed-fri overlap.
  if ( any( countIntersectingDayPatterns(uniqueDayPatterns) > 1L) )
  {

    gappyOverlays = overlayTimetables[ hasGapInOperatingDays(overlayTimetables$Days), ]
    continiousOverlays = overlayTimetables[ !hasGapInOperatingDays(overlayTimetables$Days), ]

    gappyOverlays = try(makeAllOneDay( gappyOverlays ), silent = TRUE)
    if(inherits(gappyOverlays,"try-error")){
      warning("Failed at makeAllOneDay")
    }
    continiousOverlays = try(expandAllWeeks( continiousOverlays ), silent = TRUE)
    if(inherits(continiousOverlays,"try-error")){
      warning("Failed at expandAllWeeks")
    }

    overlayTimetables =  data.table::rbindlist( list(continiousOverlays,gappyOverlays), use.names=TRUE, fill = TRUE)

  }
  splits <- list()

  distinctBasePatterns = unique( baseTimetables$Days )

  for (k in seq(1L, length(distinctBasePatterns))) {

    theseBases = baseTimetables[baseTimetables$Days == distinctBasePatterns[k], ]

    theseOverlays = overlayTimetables[ intersectingDayPatterns( distinctBasePatterns[k], overlayTimetables$Days ), ]

    if (nrow(theseOverlays) <= 0L)
    {
      splits[[k]] <- appendNumberSuffix( appendLetterSuffix( theseBases ), k )
    }
    else
    {
      timetablesForThisPattern = data.table::rbindlist( list( theseBases, theseOverlays ), use.names=TRUE, fill = TRUE)

      #performance pre-sort all the entries by the priority
      #this speeds things up when we look up the required priority overlay **SEE_NOTE**
      #timetablesForThisPattern = timetablesForThisPattern[ order(STP, duration), ]
      data.table::setkey( timetablesForThisPattern, STP, duration )
      data.table::setindex( timetablesForThisPattern, start_date, end_date)

      thisSplit <- try(splitDates( timetablesForThisPattern ), silent = TRUE)
      if(inherits(thisSplit,"try-error")){
        stop("Failed at splitDates")
      }

      # reject NAs
      if (inherits(thisSplit, "data.frame")) {
        splits[[k]] <- appendNumberSuffix( thisSplit, k )
      }
    }
  }
  splits <- data.table::rbindlist(splits, use.names=FALSE)

  splits <- try(makeCalendarsUnique( splits ), silent = TRUE)
  if(inherits(splits,"try-error")){
    warning("Failed at makeCalendarsUnique")
  }

  # after all this faffing about and splitting and joining, it's quite likely we've created some
  # small fragments of base timetable that aren't valid (e.g mon-fri service but start and end date on weekend)
  splits <- splits[ checkOperatingDayActive( splits ) ]

  return(list(splits, NA))
}




