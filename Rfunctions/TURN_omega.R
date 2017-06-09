# This function takes in:
# 1. a data frame containing run count ('Run'), a filterCol ('CustomerSiteId', 'Region','State', etc.), run date ('Date'), instrument ('SerialNo'), and panel type ('Panel')
# 2. a column on which to filter the dataFrame
# 3. the value of the filter
# 4. a calendar frame that is a placeholder containing all dates... this could be in calendar or epi-date format... must contain YearWeek and Days
# 5. a threshold number of runs in the rolling three-week period to eliminate periods where there is little activity
# The function performs the following:
# ..........
# The function outputs....
turn <- function(dataFrame, filterCol, filter, panel, calFrame, threshold=30) {
  
  # trim the data so it only includes what is desired (typically it would be filtered by CustomerSiteId or region)
  subFrame <- dataFrame[dataFrame[,filterCol] == filter, ]
  ##################### DEMO
  # return(subFrame)
  ##########################
  # aggregate the data set, summing the total runs, the unique instruments running, panel types run, and the days in the period 
  days.in.period <- with(calFrame, aggregate(Days~YearWeek, FUN=sum))
  runs.in.period <- with(subFrame, aggregate(Run~YearWeek, FUN=sum))
  colnames(runs.in.period)[2] <- 'Runs'
  inst.in.period <- with(unique(subFrame[,c('SerialNo','YearWeek','Run')]), aggregate(Run~YearWeek, FUN=sum))
  colnames(inst.in.period)[2] <- 'Instruments'
  panels.in.period <- with(unique(subFrame[,c('Panel','YearWeek','Run')]), aggregate(Run~YearWeek, FUN=sum))
  colnames(panels.in.period)[2] <- 'Panels'
  spec.in.period <- with(subFrame[subFrame$Panel==panel, ], aggregate(Run~YearWeek, FUN=sum))
  colnames(spec.in.period)[2] <- 'SpecRuns'
  pos.in.period <- with(subFrame[subFrame$Panel==panel, ], aggregate(Positive~YearWeek, FUN=sum))
  colnames(pos.in.period)[2] <- 'SpecPositives'
  
  # create a place holder for every period, and if the period is missing data, fill it in with a zero
  fillFrame <- merge(merge(merge(merge(merge(days.in.period, runs.in.period, by='YearWeek', all.x=TRUE), inst.in.period, by='YearWeek', all.x=TRUE), panels.in.period, by='YearWeek', all.x=TRUE), spec.in.period, by='YearWeek', all.x=TRUE), pos.in.period, by='YearWeek', all.x=TRUE)
  fillFrame[is.na(fillFrame)] <- 0
  ##################### DEMO
  # return(fillFrame)
  ##########################
  # perform a three-period centered sum of all the variables
  fillFrame <- fillFrame[with(fillFrame, order(YearWeek)), ]
  year.week <- fillFrame$YearWeek
  col.vars <- colnames(fillFrame)[2:length(colnames(fillFrame))]
  rollFrame <- data.frame(YearWeek = year.week[2:(length(year.week)-1)])
  
  for(c in col.vars) {
    
    roll.sum <- sapply(2:(length(year.week)-1), function(x) sum(fillFrame[(x-1):(x+1), c]))
    rollFrame <- cbind(rollFrame, roll.sum)
    colnames(rollFrame)[length(colnames(rollFrame))] <- c
  }
  rollFrame$Instruments <- ifelse(rollFrame$Instruments/3 < 1, 1, rollFrame$Instruments/3)
  rollFrame$Panels <- ifelse(rollFrame$Panels/3 < 1, 1, rollFrame$Panels/3)
  rollFrame[,filterCol] <- filter
  
  # determine periods where there are gaps (defined as 3 or more periods where the three-week centered sum is below the threshold)
  gap.finder <- data.frame(Index = seq(1, length(rollFrame$YearWeek), 1))
  holes <- which(rollFrame$SpecRuns < threshold)
  
  if(length(holes) > 0) {
    
    holes <- data.frame(Holes = holes, Gap = 1)
    gap.finder <- merge(gap.finder, holes, by.x='Index', by.y='Holes', all.x=TRUE)
    gap.finder[is.na(gap.finder$Gap), 'Gap'] <- 0
    rollFrame <- cbind(rollFrame, Gap = gap.finder$Gap)
  } else {
    
    rollFrame$Gap <- 0
  }
  ##################### DEMO
  # return(rollFrame)
  ##########################
  
  # with the smoothed data, normalize the test utlization rate by accounting for changes in install base and additional running of panels
  # require at least half a year of data and then expand after 52 weeks
  periods <- rollFrame$YearWeek
  medFrame <- c()
  for(p in 26:52) {
    
    periodFrame <- rollFrame[1:p, ]
    
    # find the R2 values
    mod.runs <- lm(SpecRuns~Instruments, data=periodFrame)
    mod.post <- lm(SpecPositives~Instruments, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)
    r.post <- ifelse(is.nan(summary(mod.post)$r.squared), 0, summary(mod.post)$r.squared)
    
    # bind the data together
    temp.dat <- data.frame(rollFrame[p, ], runsR2 = r.runs, postR2 = r.post)

    # if there aren't "enough" runs, there shouldn't be a TURN
    if(temp.dat$SpecRuns < threshold) {
      
      temp.dat$TURN <- NA
      temp.dat$TPRN <- NA
    }
    # otherwise, weight the panel runs per day per instrument and per runs... this should help with the panel growth as well
    else {
      
      # temp.dat$TURN <- with(temp.dat, (1-InstR2)*SpecRuns/RunBaseline/Days + InstR2*SpecRuns/RunBaseline/Days/Instruments)
      temp.dat$TURN <- with(temp.dat, (1-runsR2)*SpecRuns/Days + runsR2*SpecRuns/Days/Instruments)
      temp.dat$TPRN <- with(temp.dat, (1-postR2)*SpecPositives/Days + postR2*SpecPositives/Days/Instruments)
    }
    
    medFrame <- rbind(medFrame, temp.dat)
  }
  
  for(p in 53:length(periods)) {
    
    periodFrame <- rollFrame[(p-52):p, ]
    
    mod.runs <- lm(SpecRuns~Instruments, data=periodFrame)
    mod.post <- lm(SpecPositives~Instruments, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)
    r.post <- ifelse(is.nan(summary(mod.post)$r.squared), 0, summary(mod.post)$r.squared)
    
    temp.dat <- data.frame(rollFrame[p, ], runsR2 = r.runs, postR2 = r.post)

    if(temp.dat$SpecRuns < threshold) {
      
      temp.dat$TURN <- NA
      temp.dat$TPRN <- NA
    } else {
      
      # temp.dat$TURN <- with(temp.dat, (1-InstR2)*SpecRuns/RunBaseline/Days + InstR2*SpecRuns/RunBaseline/Days/Instruments)
      temp.dat$TURN <- with(temp.dat, (1-runsR2)*SpecRuns/Days + runsR2*SpecRuns/Days/Instruments)
      temp.dat$TPRN <- with(temp.dat, (1-postR2)*SpecPositives/Days + postR2*SpecPositives/Days/Instruments)
    }

    medFrame <- rbind(medFrame, temp.dat)
  }
  ##################### DEMO
  # return(periodFrame)
  # a <- turn(runs.reg.date, 'CustomerSiteId','5','RP', calendar.df, 30)
  # return(medFrame)
  # b <- turn(runs.reg.date, 'CustomerSiteId','5','RP', calendar.df, 30)
  # turn(runs.reg.date, 'CustomerSiteId','7','RP', calendar.df, 30)
  # turn(runs.reg.date, 'CustomerSiteId','24','RP', calendar.df, 30)
  # ggplot(a, aes(x=YearWeek, y=Runs, group='Runs', color='Runs')) + geom_line(lwd=1.5) + geom_line(data=a, aes(x=YearWeek, y=SpecRuns, group='RP Runs', color='RP Runs'), lwd=1.5)  + geom_line(data=a, aes(x=YearWeek, y=SpecPositives, group='RP Positives', color='RP Positives'), lwd=1.5) + geom_line(data=b, aes(x=YearWeek, y=30*TURN, group='TURN', color='TURN'), lwd=1.5) + scale_color_manual(values=c('blue','purple','black','red'))
  ##########################
  # with the test utilization normalized for instrument install base growth, now we must find a baseline such that severity can be measured 
  # relativly from year to year
  medFrame$BaselineTURN <- NA
  medFrame$BaselineTPRN <- NA
  for(i in 1:nrow(medFrame)) {
    
    if (i < 53) {
      
      periodFrame <- medFrame[1:i, ]
      medFrame[i, 'BaselineTURN'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURN'], probs=0.5, na.rm=TRUE)
      medFrame[i, 'BaselineTPRN'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TPRN'], probs=0.5, na.rm=TRUE)
    } else {
      
      periodFrame <- medFrame[(i-52):i, ]
      medFrame[i, 'BaselineTURN'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TURN'], probs=0.5, na.rm=TRUE)
      medFrame[i, 'BaselineTPRN'] <- quantile(periodFrame[periodFrame$Gap!=1, 'TPRN'], probs=0.5, na.rm=TRUE)
    }      
    
    medFrame$adjTURN <- medFrame$TURN/medFrame$BaselineTURN 
    medFrame$adjTPRN <- medFrame$TPRN/medFrame$BaselineTPRN 
  }
  
  # medFrame[1:25, 'adjTURN'] <- medFrame[1:25, 'TURN']
  # medFrame[1:25, 'adjTPRN'] <- medFrame[1:25, 'TPRN']
  
  ######################## WHAT ABOUT THIS POSSIBILITY??? NEED TO FIND A PLACE WHERE WE KNOW THIS 
  ######################## HAPPENED.... MAYBE USE MIDWEST CHILDREN'S HOSPITALS (26, maybe 13) IN 
  ######################## 2014 (EV-D68) BY CHECKING HRV/TEST DETECTION RATE AND COMPARING IT TO
  ######################## REGULAR DETECTION RATE IN "OFF-SEASON"
  # the runs are also normalized by the baseline (which is the lowest tenth quantile in the rolling period), and this could be skewed
  # if there is a summer outbreak (typically the baseine would be in the summer, thus why half a year of data are required)... to account
  # for this, we'd expect that the number of positive tests (or PositiveRatio) Winter/Summer would be lower than than the RunRatio in the same
  # timeframe, because usually the rate of positive tests is lower in the summer time. Therefore, if the positive ratio is less than the run ratio
  # we should correct for this (check to make sure the positive ratio is > 10% because it could be that they are running validations)
   

  return(medFrame)
}