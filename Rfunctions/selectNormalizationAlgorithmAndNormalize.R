# This function takes in a data frame that has a YearWeek that is created by merging a runs data frame to a calendar by the date. The 
# result will include a YearWeek that is made using the makeEvenWeeks user-defined function such that run data can be compared to CDC
# reported data. The other inputs are the filter column (something that will break out by site) and the filter (e.g. 2, 7, 5 etc. if the
# filterCol is CustomerSiteId).
selectNormalizationAlgorithmAndNormalize <- function(dataFrame, filterCol, filter) {
  
  # trim the data so it only includes what is desired (typically it would be filtered by Name or CustomerSiteId)
  subFrame <- dataFrame[dataFrame[,filterCol] == filter, ]
  
  # aggregate the data set, summing the total runs, the unique instruments running, and the days in the period 
  # (assuming there is a calendar.df)
  days.in.period <- with(calendar.df, aggregate(Days~YearWeek, FUN=sum))
  runs.in.period <- with(subFrame, aggregate(Record~YearWeek, FUN=sum))
  inst.in.period <- with(unique(subFrame[,c('Instrument','YearWeek','Record')]), aggregate(Record~YearWeek, FUN=sum))
  
  aggFrame <- merge(merge(runs.in.period, days.in.period, by='YearWeek'), inst.in.period, by='YearWeek')
  colnames(aggFrame) <- c('YearWeek','Runs','Days','Instruments')
  aggFrame[,filterCol] <- filter
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
  
  # to determine whether or not the runs rate needs to factor in a change in the size of the instrument fleet, the data should be smoothed
  # as a 3-week centered sum (mean for instruments) and then then the runs should be fitted as predicated by the instruments in the period
  # smoothing helps improve the model, so that is why it is used
  cols  <- c('Days','Runs')
  roll.mat <- as.data.frame(sapply(1:length(cols), function(y) sapply(2:(length(aggFrame.good[,'CustomerSiteId'])-1), function(x) sum(aggFrame.good[(x-1):(x+1), cols[y]]))))
  colnames(roll.mat) <- c('RollDays','RollRuns')
  roll.mat$RollInst <- sapply(2:(length(aggFrame.good[,'CustomerSiteId'])-1), function(x) mean(aggFrame.good[(x-1):(x+1),'Instruments']))
  rollFrame <- cbind(aggFrame.good[2:(length(aggFrame.good[,'CustomerSiteId'])-1), ], roll.mat)
  rollFrame$RollRate <- with(rollFrame, RollRuns/RollDays)
  rollFrame$RollRateByInst <- with(rollFrame, ifelse(RollInst >= 1, RollRuns/(RollDays*RollInst), RollRate))
  # quants <- do.call(rbind, lapply(1:length(unique(rollFrame$Year)), 
  #                                 function(x) data.frame(
  #                                   YearWeek = rollFrame[rollFrame$Year == unique(rollFrame$Year)[x], 'YearWeek'],
  #                                   Quant = cut(rollFrame[rollFrame$Year == unique(rollFrame$Year)[x], 'RollRuns'], breaks = quantile(rollFrame[rollFrame$Year == unique(rollFrame$Year)[x], 'RollRuns'], probs=c(0, 0.25, 0.5, 0.75, 1)), include.lowest = TRUE, right = TRUE, labels = c('lowest','low','high','highest'))
  #                                   )
  #                                 )
  #                  )
  # rollFrame <- merge(rollFrame, quants, by='YearWeek')
  
  # determine the goodness of fit of runs with the install base... if there is no variation in the number of instruments, then the factor of
  # instrument install base should not be included
  if(length(as.character(unique(rollFrame$Instruments))) == 1) {
    
    r.sq.adj <- 0
  } else {
    
    r.sq.adj <- summary(lm(RollRuns~RollInst, rollFrame))$adj.r.squared
    r.sq.adj <- ifelse(r.sq.adj < 0, 0, r.sq.adj)
  }
  
  # based on a lagging 26 week minimum, normalize the data
  if(length(rollFrame$YearWeek) >= 26) {
    
    # norm.rate <- do.call(c, lapply(26:length(rollFrame$YearWeek), function(x) min(rollFrame[(x-25):x,'RollRate']['RollRate' > 0])))
    norm.rate <- do.call(c, lapply(26:length(rollFrame$YearWeek), function(x) unname(quantile(rollFrame[(x-25):x, 'RollRate'], probs=0.15))))
    norm.rate <- c(rep.int(norm.rate[1], 25), norm.rate)
    norm.rate[norm.rate < 1] <- 1
    
    # norm.rateByInst <- do.call(c, lapply(26:length(rollFrame$YearWeek), function(x) min(rollFrame[(x-25):x,'RollRateByInst']['RollRateByInst' > 0])))
    norm.rateByInst <- do.call(c, lapply(26:length(rollFrame$YearWeek), function(x) unname(quantile(rollFrame[(x-25):x, 'RollRateByInst'], probs=0.15))))
    norm.rateByInst <- c(rep.int(norm.rateByInst[1], 25), norm.rateByInst)
    norm.rateByInst[norm.rateByInst < 1] <- 1
    
    rollFrame <- cbind(rollFrame, norm.rate, norm.rateByInst)
  } else {
    
    # norm.rate <- min(rollFrame[,'RollRate']['RollRate' > 0])
    norm.rate <- unname(quantile(rollFrame$RollRate, probs=0.15))
    norm.rate <- rep.int(norm.rate, length(rollFrame$YearWeek))
    norm.rate[norm.rate < 1] <- 1
    
    # norm.rateByInst <- min(rollFrame[,'RollRateByInst']['RollRateByInst' > 0])
    norm.rateByInst <- unname(quantile(rollFrame$RollRateByInst, probs=0.15))
    norm.rateByInst <- rep.int(norm.rateByInst, length(rollFrame$YearWeek))
    norm.rateByInst[norm.rateByInst < 1] <- 1
    
    rollFrame <- cbind(rollFrame, norm.rate, norm.rateByInst)
  }
  
  rollFrame$NormRollRate <- with(rollFrame, RollRate/norm.rate)
  rollFrame$NormRollRateByInst <- with(rollFrame, RollRateByInst/norm.rateByInst)
  rollFrame$NormalizedBurn <- r.sq.adj*rollFrame$NormRollRateByInst + (1 - r.sq.adj)*rollFrame$NormRollRate
  return(rollFrame)
}