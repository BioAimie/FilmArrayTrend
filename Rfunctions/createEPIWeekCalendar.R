createEPIWeekCalendar <- function(calFrame) {

  calFrame[,'Year'] <- year(calFrame[,'Date'])
  calFrame[,'DayOfYear'] <- yday(calFrame[,'Date'])

  return(calFrame)
  
  
  
  
  years <- unique(calFrame[,'Year'])
  leapYears <- years[(years %% 4 == 0)]

  outFrame <- c()

  for(i in 1:length(years)) {

    yearFrame <- calFrame[calFrame[,'Year'] == years[i], ]

    if(!(years[i] %in% leapYears)) {

      weekBreaks <- seq(1, 365, 7)
      yearFrame[,'Week'] <- cut(yearFrame[,'DayOfYear'], weekBreaks, include.lowest = TRUE, right = TRUE, labels = seq(1, 52, 1))
      outFrame <- rbind(outFrame, yearFrame)

    }
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
