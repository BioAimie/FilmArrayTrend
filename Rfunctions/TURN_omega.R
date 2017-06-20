# This function takes in:
# 1. a data frame containing run count ('Run'), a filterCol ('CustomerSiteId', 'Region','State', etc.), run date ('Date'), instrument ('SerialNo'), and panel type ('Panel')
# 2. a column on which to filter the dataFrame
# 3. the value of the filter
# 4. a calendar frame that is a placeholder containing all dates... this could be in calendar or epi-date format... must contain YearWeek and Days
# 5. a threshold number of runs in the rolling three-week period to eliminate periods where there is little activity
# The function performs the following:
# ..........
# The function outputs....
turn <- function(dataFrame, filterCol, filter, panel, calFrame, threshold=30) { # **Note: try removing the threshold and see how it impacts the data also add quantile as a parameter
  
  # trim the data so it only includes what is desired (typically it would be filtered by CustomerSiteId or region)
  subFrame <- dataFrame[dataFrame[,filterCol] == filter, ]
  
  # aggregate the data set, summing the total runs, the unique instruments running, panel types run, and the days in the period
  days.in.period <- with(calFrame, aggregate(Days~YearWeek, FUN=sum))
  inst.in.period <- with(unique(subFrame[,c('SerialNo','YearWeek','Run')]), aggregate(Run~YearWeek, FUN=sum))
  colnames(inst.in.period)[2] <- 'Instruments'
  runs.in.period <- with(subFrame[subFrame$Panel==panel, ], aggregate(Run~YearWeek, FUN=sum))
  colnames(runs.in.period)[2] <- 'Runs'
  pos.in.period <- with(subFrame[subFrame$Panel==panel, ], aggregate(Positive~YearWeek, FUN=sum))
  colnames(pos.in.period)[2] <- 'Positives'
  
  # create a place holder for every period, and if the period is missing data, fill it in with a zero
  fillFrame <- merge(merge(merge(days.in.period, runs.in.period, by='YearWeek', all.x=TRUE), inst.in.period, by='YearWeek', all.x=TRUE), pos.in.period, by='YearWeek', all.x=TRUE)
  fillFrame[is.na(fillFrame)] <- 0
  
  # perform a three-period centered sum of all the variables
  fillFrame <- fillFrame[with(fillFrame, order(YearWeek)), ]
  year.week <- fillFrame$YearWeek
  col.vars <- colnames(fillFrame)[2:ncol(fillFrame)]
  rollFrame <- data.frame(YearWeek = year.week[2:(length(year.week)-1)])
  
  for(c in col.vars) {
    
    roll.sum <- sapply(2:(length(year.week)-1), function(x) sum(fillFrame[(x-1):(x+1), c]))
    rollFrame <- cbind(rollFrame, roll.sum)
    colnames(rollFrame)[length(colnames(rollFrame))] <- c
  }
  rollFrame$Instruments <- ifelse(rollFrame$Instruments/3 < 1, 1, rollFrame$Instruments/3)
  rollFrame[,filterCol] <- filter
  
  # determine periods where there are gaps (defined as 3 or more periods where the three-week centered sum is below the threshold)
  gap.finder <- data.frame(Index = seq(1, length(rollFrame$YearWeek), 1))
  holes <- which(rollFrame$Runs < threshold)
  
  if(length(holes) > 0) {
    
    holes <- data.frame(Holes = holes, Gap = 1)
    gap.finder <- merge(gap.finder, holes, by.x='Index', by.y='Holes', all.x=TRUE)
    gap.finder[is.na(gap.finder$Gap), 'Gap'] <- 0
    rollFrame <- cbind(rollFrame, Gap = gap.finder$Gap)
  } else {
    
    rollFrame$Gap <- 0
  }
  
  # the site needs to have a start date prior to running the normalization... the data frame essentially has a placeholder for
  # each period starting at some arbitrary point and has zeroes where values aren't there, so this skews the linear regression
  # A true start date is needed, so find it using the gap
  start.period <- data.frame(YearWeek = rollFrame[2:(nrow(rollFrame)-2), 'YearWeek'], GapScore = sapply(2:(nrow(rollFrame)-2), function(x) sum(rollFrame[(x-2):(x+2),'Gap'])))
  start.period <- start.period[min(which(start.period$GapScore < 2)), 'YearWeek']
  rollFrame <- rollFrame[as.character(rollFrame$YearWeek) > as.character(start.period), ]
  
  # with the smoothed data, normalize the test utlization rate by accounting for changes in install base and additional running of panels
  # require at least a year of data
  if(nrow(rollFrame) < 52) { return() }
  periods <- rollFrame$YearWeek
  medFrame <- c()
  for(p in 26:51) {
    
    periodFrame <- rollFrame[1:p, ]
    
    # find the R2 values
    mod.runs <- lm(Runs~Instruments, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)

    # bind the data together
    temp.dat <- data.frame(rollFrame[p, ], runsR2 = r.runs)

    # if there aren't "enough" runs, there shouldn't be a TURN
    if(temp.dat$Runs < threshold) {
      
      temp.dat$TURNI <- NA
    }
    # otherwise, weight the panel runs per day per instrument and per runs... this should help with the panel growth as well
    else {
      
      temp.dat$TURNI <- with(temp.dat, (1-runsR2)*Runs/Days + runsR2*Runs/Days/Instruments)
    }
    
    medFrame <- rbind(medFrame, temp.dat)
  }
  
  for(p in 52:length(periods)) {
    
    periodFrame <- rollFrame[(p-51):p, ]
    
    mod.runs <- lm(Runs~Instruments, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)

    temp.dat <- data.frame(rollFrame[p, ], runsR2 = r.runs)

    if(temp.dat$Runs < threshold) {
      
      temp.dat$TURNI <- NA
    } else {
      
      temp.dat$TURNI <- with(temp.dat, (1-runsR2)*Runs/Days + runsR2*Runs/Days/Instruments)
    }

    medFrame <- rbind(medFrame, temp.dat)
  }
  
  medFrame$BaselineTURNI <- NA
  for(i in 1:nrow(medFrame)) {
    
    if (i < 52) {

      periodFrame <- medFrame[1:52, ]
      medFrame[i, 'BaselineTURNI'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURNI'], probs=0.5, na.rm=TRUE)
    } else {

      periodFrame <- medFrame[(i-51):i, ]
      medFrame[i, 'BaselineTURNI'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURNI'], probs=0.5, na.rm=TRUE)
    }
    # if (i < 52) {
    # 
    #   periodFrame <- medFrame[1:52, ]
    #   medFrame[i, 'BaselineTURNI'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURNI'], probs=0.1, na.rm=TRUE)
    # } else if (i < nrow(medFrame)-26) {
    # 
    #   periodFrame <- medFrame[(i-26):(i+26), ]
    #   medFrame[i, 'BaselineTURNI'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURNI'], probs=0.1, na.rm=TRUE)
    # } else {
    # 
    #   periodFrame <- medFrame[(nrow(medFrame)-52):nrow(medFrame), ]
    #   medFrame[i, 'BaselineTURNI'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURNI'], probs=0.1, na.rm=TRUE)
    # }
    
    medFrame$TURN <- medFrame$TURNI/medFrame$BaselineTURNI
  }
  
  return(medFrame)
}