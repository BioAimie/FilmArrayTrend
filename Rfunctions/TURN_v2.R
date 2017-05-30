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
  
  # # perform a smoothing cubic-spline fit with cross validation to determine the correct lambda paramter
  # fillFrame <- fillFrame[with(fillFrame, order(YearWeek)), ]
  # fillFrame$Spline <- smooth.spline(x=seq(1, length(fillFrame$YearWeek), 1), y=fillFrame$Runs, cv=FALSE)$y
  # fillFrame[fillFrame$Spline < threshold, 'Spline'] <- NA
  
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
  rollFrame$Positivity <- ifelse(rollFrame$SpecRuns==0, 0, rollFrame$SpecPositives/rollFrame$SpecRuns)
  
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
  
  # with the smoothed data, take the median in the last 52 week period, excluding gaps
  # rollFrame[rowFrame$Gap==1, 'SpecRuns'] <- NA
  periods <- rollFrame$YearWeek
  medFrame <- c()
  # what to do about the first 52 entries?? Since we start (above) at p = 53, we are discarding a lot of data
  # let's try something to preserve at least some of the data (26 weeks instead of 52)
  for(p in 26:52) { # can change to 26
    
    periodFrame <- rollFrame[(p-25):p, ]

    # find the coefficient of determination for fits with Runs, Panels, and Instruments....
    mod.runs <- lm(SpecRuns~Runs, data=periodFrame)
    mod.inst <- lm(SpecRuns~Instruments, data=periodFrame)
    mod.pans <- lm(SpecRuns~Panels, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)
    r.inst <- ifelse(is.nan(summary(mod.inst)$r.squared), 0, summary(mod.inst)$r.squared)
    r.pans <- ifelse(is.nan(summary(mod.pans)$r.squared), 0, summary(mod.pans)$r.squared)
    
    # bind the data together
    temp.dat <- data.frame(rollFrame[p, ], RunsR2 = r.runs, InstR2 = r.inst, PansR2 = r.pans)
    
    # utilize the median in the rolling period as well as a weighted average of panel runs
    # per day (or per day per inst, or per day per panels etc.)
    if(temp.dat$RunsR2 >= 0.98) {
      
      temp.dat$TURNG <- with(temp.dat, threshold*((1-InstR2)*SpecRuns/Days + InstR2*SpecRuns/Days/Instruments)) 
    } else if(is.na(temp.dat$RunsR2)) {
      
      temp.dat$TURNG <- NA
    } else {
      
      temp.dat$TURNG <- with(temp.dat, threshold*(RunsR2*SpecRuns/Days + InstR2*SpecRuns/Days/Instruments + PansR2*SpecRuns/Days/Panels)/(RunsR2+InstR2+PansR2))   
    }
    
    medFrame <- rbind(medFrame, temp.dat)
  }
  
  for(p in 53:length(periods)) { # can change to 26
    
    periodFrame <- rollFrame[(p-52):p, ] # can change to 25

    # find the coefficient of determination for fits with Runs, Panels, and Instruments....
    mod.runs <- lm(SpecRuns~Runs, data=periodFrame)
    mod.inst <- lm(SpecRuns~Instruments, data=periodFrame)
    mod.pans <- lm(SpecRuns~Panels, data=periodFrame)
    r.runs <- ifelse(is.nan(summary(mod.runs)$r.squared), 0, summary(mod.runs)$r.squared)
    r.inst <- ifelse(is.nan(summary(mod.inst)$r.squared), 0, summary(mod.inst)$r.squared)
    r.pans <- ifelse(is.nan(summary(mod.pans)$r.squared), 0, summary(mod.pans)$r.squared)
    
    # bind the data together
    temp.dat <- data.frame(rollFrame[p, ], RunsR2 = r.runs, InstR2 = r.inst, PansR2 = r.pans)
    
    # utilize the median in the rolling period as well as a weighted average of panel runs
    # per day (or per day per inst, or per day per panels etc.)
    # temp.dat$TURN <- with(temp.dat, threshold*(RunsR2*SpecRuns/SpecMed/Days + InstR2*SpecRuns/SpecMed/Days/Instruments + PansR2*SpecRuns/SpecMed/Days/Panels)/(RunsR2+InstR2+PansR2)) 
    if(temp.dat$RunsR2  >= 0.98) {
      
      temp.dat$TURNG <- with(temp.dat, threshold*((1-InstR2)*SpecRuns/Days + InstR2*SpecRuns/Days/Instruments)) 
    } else if(is.na(temp.dat$RunsR2)) {
      
      temp.dat$TURNG <- NA
    } else {
      
      temp.dat$TURNG <- with(temp.dat, threshold*(RunsR2*SpecRuns/Days + InstR2*SpecRuns/Days/Instruments + PansR2*SpecRuns/Days/Panels)/(RunsR2+InstR2+PansR2))   
    }
    
    medFrame <- rbind(medFrame, temp.dat)
  }
  
  medFrame <- medFrame[with(medFrame, order(YearWeek)), ]
  medFrame[!(is.na(medFrame$TURNG)) & medFrame$TURNG==0.0, 'TURNG'] <- NA
  medFrame[is.nan(medFrame$TURNG), 'TURNG'] <- NA
  
  # take the window of TURNG, centered about a period to try to rule out 
  
  
  
  return(medFrame)
}