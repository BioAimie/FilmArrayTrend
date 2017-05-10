# This function takes in:
# 1. a data frame containing run count ('Run'), a filterCol ('CustomerSiteId', 'Region','State', etc.), run date ('Date'), instrument ('SerialNo'), and panel type ('Panel')
# 2. a column on which to filter the dataFrame
# 3. the value of the filter
# 4. a calendar frame that is a placeholder containing all dates... this could be in calendar or epi-date format... must contain YearWeek and Days
# The function performs the following:
# ..........
# The function outputs....
turn <- function(dataFrame, filterCol, filter, calFrame, smoothPeriods=3) {
  
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
  
  # create a place holder for every period, and if the period is missing data, fill it in with a zero
  aggFrame <- merge(merge(merge(runs.in.period, days.in.period, by='YearWeek', all.x=TRUE), inst.in.period, by='YearWeek', all.x=TRUE), panels.in.period, by='YearWeek', all.x=TRUE)
  fillFrame <- merge(days.in.period, aggFrame, by=c('YearWeek','Days'), all.x=TRUE)
  fillFrame[,filterCol] <- filter
  fillFrame[is.na(fillFrame)] <- 0
  return(fillFrame)
  # this information may be sufficient to determine how install base growth and adding new panels affects test rate; however, how to account for decreased
  # utilization due to competition???
  
  
  
  
  
  aggFrame$Year <- as.integer(substring(aggFrame$YearWeek, 1, 4))
  aggFrame$Week <- as.integer(substring(aggFrame$YearWeek, 6, 7))
  
  # join the aggregated data onto the days.in.period frame, because then there will be a place holder for times with zero runs
  aggFrame.fill <- merge(days.in.period, aggFrame, by=c('YearWeek','Days'), all.x=TRUE)
  start.group <- min(aggFrame[!(is.na(aggFrame$YearWeek)), 'YearWeek'])
  aggFrame.fill <- aggFrame.fill[aggFrame.fill$YearWeek >= start.group, ]
  aggFrame.fill[is.na(aggFrame.fill$Runs), 'Runs'] <- 0
  aggFrame.fill[is.na(aggFrame.fill$Instruments), 'Instruments'] <- 1
  aggFrame.fill[is.na(aggFrame.fill[,filterCol]), filterCol] <- filter
  aggFrame.fill$Year <- as.integer(substring(aggFrame.fill$YearWeek, 1, 4))
  aggFrame.fill$Week <- as.integer(substring(aggFrame.fill$YearWeek, 6, 7))
  aggFrame.fill <- aggFrame.fill[with(aggFrame.fill, order(YearWeek)), ]
  
  # calculate the runs per day and the runs per day per instrument
  aggFrame.fill$RunRate <- with(aggFrame.fill, Runs/Days)
  aggFrame.fill$RunRateByInst <- with(aggFrame.fill, Runs/(Days*Instruments))
  
  # find gaps and then start the data set where there are not any big holes, then trim the aggregate data to only include data from the
  # identified start date
  gap.finder <- data.frame(Index = seq(1, length(aggFrame.fill$RunRateByInst), 1))
  holes <- which(aggFrame.fill$RunRateByInst <= 1)
  if(length(holes) > 0) {
    
    holes <- data.frame(Holes = which(aggFrame.fill$RunRateByInst <= 1), Count = 1)
    gap.finder <- merge(gap.finder, holes, by.x='Index', by.y='Holes', all.x=TRUE)
    gap.finder[is.na(gap.finder$Count), 'Count'] <- 0
    last.gap <- max(which(sapply(5:length(gap.finder$Index), function(x) sum(gap.finder[(x-4):x,'Count'])) == 5)) + 4  
    
    if(is.infinite(last.gap)) {
      
      aggFrame.good <- aggFrame.fill
    } else {
      
      aggFrame.good <- aggFrame.fill[last.gap:length(aggFrame.fill$YearWeek), ]  
    }
    
  } else {
    
    aggFrame.good <- aggFrame.fill
  }
  
  if(length(aggFrame.good[,1]) < 26) { return() }
  
  # to determine whether or not the runs rate needs to factor in a change in the size of the instrument fleet, the data should be smoothed
  # as a 3-week centered sum (mean for instruments) and then then the runs should be fitted as predicated by the instruments in the period
  # smoothing helps improve the model, so that is why it is used
  cols  <- c('Days','Runs')
  roll.mat <- as.data.frame(sapply(1:length(cols), function(y) sapply(2:(length(aggFrame.good[,filterCol])-1), function(x) sum(aggFrame.good[(x-1):(x+1), cols[y]]))))
  colnames(roll.mat) <- c('RollDays','RollRuns')
  roll.mat$RollInst <- sapply(2:(length(aggFrame.good[,filterCol])-1), function(x) mean(aggFrame.good[(x-1):(x+1),'Instruments']))
  rollFrame <- cbind(aggFrame.good[2:(length(aggFrame.good[,filterCol])-1), ], roll.mat)
  rollFrame$RollRate <- with(rollFrame, RollRuns/RollDays)
  rollFrame$RollRateByInst <- with(rollFrame, ifelse(RollInst >= 1, RollRuns/(RollDays*RollInst), RollRate))
  
  # determine the goodness of fit of runs with the install base by a rolling 53 week period (if there are 53 weeks)... 
  l.dates <- length(rollFrame$YearWeek)
  l.insts <- length(as.character(unique(rollFrame$Instruments)))
  
  # if there is no variation in the number of instruments, then the factor of instrument install base should not be included
  if(l.insts == 1 | l.dates < 25) {
    
    lmFrame <- data.frame(YearWeek = rollFrame$YearWeek,
                          R = 0,
                          b = 0,
                          m = 0
                          )
  } 
  
  # if there is variation of the instrument install base, then this should be factored in by looking at the install base in a rolling 52 week period
  else {
    
    if(l.dates >= 52) {
      
      lmFrame <- data.frame(YearWeek = rollFrame$YearWeek, 
                            R = sapply(1:length(unique(rollFrame$YearWeek)), function(x) ifelse(x < 52, summary(lm(RollRuns~RollInst, rollFrame[1:52, ]))$adj.r.squared, summary(lm(RollRuns~RollInst, rollFrame[(x-51):x, ]))$adj.r.squared)), 
                            b = sapply(1:length(unique(rollFrame$YearWeek)), function(x) ifelse(x < 52, summary(lm(RollRuns~RollInst, rollFrame[1:52, ]))$coeff[[1]], summary(lm(RollRuns~RollInst, rollFrame[(x-51):x, ]))$coeff[[1]])), 
                            m = sapply(1:length(unique(rollFrame$YearWeek)), function(x) ifelse(x < 52, summary(lm(RollRuns~RollInst, rollFrame[1:52, ]))$coeff[[2]], summary(lm(RollRuns~RollInst, rollFrame[(x-51):x, ]))$coeff[[2]]))
                            )
      
    } else {
      
      lmFrame <- data.frame(YearWeek = rollFrame$YearWeek,
                            R = summary(lm(RollRuns~RollInst, rollFrame))$adj.r.squared,
                            b = summary(lm(RollRuns~RollInst, rollFrame))$coeff[[1]],
                            m = summary(lm(RollRuns~RollInst, rollFrame))$coeff[[2]]
                            )
      
    }
  }
  
  # based on a lagging 52 week minimum (if it can be calculated), normalize the data for both the RunRate and the RunRateByInst
  if(l.dates >= 52) {
    
    # because the minimum rate can be small if only one is considered, in the rolling date frame, choose the 0.10 quantile as the normalizer
    norm.rate <- do.call(c, lapply(52:length(rollFrame$YearWeek), function(x) unname(quantile(rollFrame[(x-51):x, 'RollRate'], probs=0.10))))
    norm.rate <- c(rep.int(norm.rate[1], 51), norm.rate)
    # norm.rate[norm.rate < 1] <- 1
    
    norm.rateByInst <- do.call(c, lapply(52:length(rollFrame$YearWeek), function(x) unname(quantile(rollFrame[(x-51):x, 'RollRateByInst'], probs=0.10))))
    norm.rateByInst <- c(rep.int(norm.rateByInst[1], 51), norm.rateByInst)
    # norm.rateByInst[norm.rateByInst < 1] <- 1
  } else {
    
    norm.rate <- unname(quantile(rollFrame$RollRate, probs=0.15))
    norm.rate <- rep.int(norm.rate, length(rollFrame$YearWeek))
    # norm.rate[norm.rate < 1] <- 1
    
    norm.rateByInst <- unname(quantile(rollFrame$RollRateByInst, probs=0.15))
    norm.rateByInst <- rep.int(norm.rateByInst, length(rollFrame$YearWeek))
    # norm.rateByInst[norm.rateByInst < 1] <- 1
  }
  
  rollFrame <- cbind(rollFrame, norm.rate, norm.rateByInst)
  rollFrame <- merge(rollFrame, lmFrame, by='YearWeek')
  
  rollFrame$NormRollRate <- with(rollFrame, RollRate/norm.rate)
  rollFrame$NormRollRateByInst <- with(rollFrame, RollRateByInst/norm.rateByInst)
  rollFrame$NormalizedBurn <- rollFrame$R*rollFrame$NormRollRateByInst + (1 - rollFrame$R)*rollFrame$NormRollRate
  
  return(rollFrame)
}