# this function takes in a calendar data frame that has an entry for every date since a starting point up through today
# the purpose of the function is to break the days into nice bins
makeEvenWeeks <- function(calendarFrame) {
  
  # make a year and day column
  calendarFrame[,'Year'] <- year(calendarFrame[,'Date'])
  calendarFrame[,'DayOfYear'] <- yday(calendarFrame[,'Date'])
  
  # determine which years are leap years
  years <- unique(calendarFrame[,'Year'])
  leapYears <- years[(years %% 4 == 0)]
  
  outFrame <- c()
  
  # iterate through each year
  for(i in 1:length(years)) {
    
    yearFrame <- calendarFrame[calendarFrame[,'Year'] == years[i], ]
    
    # if the year isn't a leap year, then group the extra day at the begining of the year
    if(!(years[i] %in% leapYears)) {
      
      weekBreaks <- seq(1, 365, 7)
      yearFrame[,'Week'] <- cut(yearFrame[,'DayOfYear'], weekBreaks, include.lowest = TRUE, right = TRUE, labels = seq(1, 52, 1))
      outFrame <- rbind(outFrame, yearFrame)
      
    } 
    # if the year is a leap year, then group the extra 2 days into the first and last weeks of the year (one day for each)
    else {
      
      weekBreaks <- seq(1, 365, 7)
      weekBreaks <- c(weekBreaks[1:52], 366)
      yearFrame[,'Week'] <- cut(yearFrame[,'DayOfYear'], weekBreaks, include.lowest = TRUE, right = TRUE, labels = seq(1, 52, 1))
      outFrame <- rbind(outFrame, yearFrame)
    }
  }
  
  outFrame[,'Week'] <- as.numeric(outFrame[,'Week'])
  outFrame[,'YearWeek'] <- with(outFrame, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
  return(outFrame)
}