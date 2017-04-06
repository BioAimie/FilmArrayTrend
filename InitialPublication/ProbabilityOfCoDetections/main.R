workDir <-'~/FilmArrayTrend/InitialPublication/ProbabilityOfCoDetections/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(scales)
require(dateManip)
library(dplyr)
library(tidyr)

# load custom functions
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')
source('../../Rfunctions/generateCombosBetter.R')

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- readLines('AllRespiratoryDetectionsBySiteAndDate.sql')
query <- paste(queryVector,collapse="\n")
tests.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# Create a data frame that contains detections only
detections.df <- subset(tests.df, ShortName != 'Neg')

# Create an epi calendar
calendar.df <- createCalendarLikeMicrosoft(2013, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))

# Create a generic data frame that has a place holder for each week and each bug for each site
sites <- unique(detections.df$CustomerSiteId)[order(unique(detections.df$CustomerSiteId))]
periods <- unique(calendar.df$YearWeek)[order(unique(calendar.df$YearWeek))]
bugs <- as.character(unique(detections.df$ShortName))[order(as.character(unique(detections.df$ShortName)))]
placeholder.df <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(periods), function(y) data.frame(CustomerSiteId = sites[x], YearWeek = periods[y], ShortName = bugs)))))

# Find an epi period associated with the dates of the tests for all runs
detections.df <- merge(calendar.df, detections.df, by='Date')

# How many of the detections are single detections vs. co-detections of any number? 
ggplot(unique(detections.df[,c('RunDataId','PositiveAssays')]), aes(x=PositiveAssays)) + geom_histogram(binwidth = 1)

# The overwhelming majority of positives tests have a single positive target 
single.dets <- subset(detections.df, PositiveAssays==1)

# For the other detections... let's look at 2, 3, and 4 for now
# dual.dets <- subset(detections.df, PositiveAssays==2)
# # do.call(rbind, lapply(1:10, function(x) data.frame(RunDataId = unique(dual.dets$RunDataId)[x], ShortName = paste(dual.dets[dual.dets$RunDataId==unique(dual.dets$RunDataId)[x], 'ShortName'], collapse=', '))))
# tri.dets <- subset(detections.df, PositiveAssays==3)
quad.dets <- subset(detections.df, PositiveAssays==4)

# Attempt to determine the likelihood of quad detections given what is circulating in the population in a period
single.dets.agg <- with(single.dets, aggregate(PositiveAssays~YearWeek+CustomerSiteId+ShortName, FUN=sum))
# for all sites, periods, and bugs, create a placeholder for data, if there are no positives, then create a zero entry
single.dets.fill <- merge(placeholder.df, single.dets.agg, by=c('CustomerSiteId','YearWeek','ShortName'), all.x=TRUE)
single.dets.fill[is.na(single.dets.fill$PositiveAssays),'PositiveAssays'] <- 0
# perform a 5-week centered rolling sum of positives 
single.dets.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(3:(length(periods)-3), function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(CustomerSiteId = sites[x], YearWeek = periods[y], ShortName = bugs[z], Positives = sum(single.dets.fill[single.dets.fill$CustomerSiteId==sites[x] & single.dets.fill$YearWeek %in% periods[(y-2):(y+2)] & single.dets.fill$ShortName==bugs[z], 'PositiveAssays']))))))))
# perform a 5-week centered rolling sum of tests
test.count <- unique(tests.df[,c('RunDataId','Date','CustomerSiteId')])
test.count.fill <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(merge(calendar.df[,c('Date','YearWeek')], data.frame(Date = test.count[test.count$CustomerSiteId==sites[x], c('Date')], Record = 1), by='Date', all.x=TRUE), CustomerSiteId = sites[x])))
test.count.fill[is.na(test.count.fill$Record), 'Record'] <- 0
test.count.agg <- with(test.count.fill, aggregate(Record~CustomerSiteId+YearWeek, FUN=sum)) 
test.count.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(3:(length(periods)-3), function(y) data.frame(CustomerSiteId = sites[x], YearWeek = periods[y], Tests = sum(test.count.agg[test.count.agg$CustomerSiteId==sites[x] & test.count.agg$YearWeek %in% periods[(y-2):(y+2)], 'Record']))))))

# for each site and each 5-week centered period, calculate the probability of a given quadruple detection given the single detection data, 
# then compare to what is observed and figure out if it is reasonable or not
n.det <- 4
combo.probs <- c()
for(i in 1:length(sites)) {
  
  # single.dets.site <- single.dets.fill[single.dets.fill$CustomerSiteId==sites[i], ]
  # site.periods <- unique(single.dets.site$YearWeek)
  # site.periods <- site.periods[order(site.periods)]
  single.dets.site <- single.dets.roll[single.dets.roll$CustomerSiteId==sites[i] & single.dets.roll$Positives > 0, ]
  site.periods <- unique(single.dets.site$YearWeek)
  site.periods <- site.periods[order(site.periods)]
  
  for(j in 1:length(site.periods)) {
    
    single.dets.site.period <- single.dets.site[single.dets.site$YearWeek==site.periods[j], ]
    # if there aren't 4 things that are detected in the period, then skip the period because there shouldn't (according to our assumptions)
    # be any chance of a quadruple detection in the population
    if(nrow(single.dets.site.period) < n.det) { next() }
    n.tests <- test.count.roll[test.count.roll$YearWeek==site.periods[j] & test.count.roll$CustomerSiteId==sites[i], 'Tests']
    bugs.site.period <- unique(as.character(single.dets.site.period$ShortName))[order(unique(as.character(single.dets.site.period$ShortName)))]
    n.combos <- choose(length(bugs.site.period), n.det)
    combo.list <- generateCombos(bugs.site.period, n.det, FALSE)
    
    # temp <- do.call(rbind, lapply(1:length(single.dets.site.period$ShortName), function(x) data.frame(YearWeek = site.periods[j], CustomerSiteId = sites[i],  ShortName = paste(single.dets.site.period$ShortName[x], single.dets.site.period[single.dets.site.period$ShortName!=single.dets.site.period$ShortName[x],'ShortName'], sep=', '), N = sum(single.dets.site.period$PositiveAssays), P = single.dets.site.period[single.dets.site.period$ShortName==single.dets.site.period$ShortName[x],'PositiveAssays']*single.dets.site.period[single.dets.site.period$ShortName!=single.dets.site.period$ShortName[x],'PositiveAssays']/(sum(single.dets.site.period$PositiveAssays)^2))))
    
    # for(k in 1:nrow(temp)) {
    # 
    #   r.bug.string <- strsplit(as.character(temp$ShortName), split=', ')[[k]]
    #   r.bug.string <- paste(r.bug.string[order(r.bug.string)], collapse=', ')
    #   temp[k,'ShortName'] <- r.bug.string
    # }
    # 
    # temp <- unique(temp)
    # temp$SE <- with(temp, sqrt(P*(1-P)/N))
    # temp$LL <- (temp$P - temp$SE*qnorm(1-0.05/(length(unique(single.dets.site.period$ShortName))*(length(unique(single.dets.site.period$ShortName))-1))))*temp$N
    # temp$UL <- ceiling((temp$P + temp$SE*qnorm(1-0.05/(length(unique(single.dets.site.period$ShortName))*(length(unique(single.dets.site.period$ShortName))-1))))*temp$N)
    # temp$LL <- ifelse(temp$LL < 0, 0, ceiling(temp$LL))
    
    for(k in 1:n.combos) {
      
      # calculate the expected probablity of a given quad-detection occurring at a given site at a given time
      combo.temp <- paste(combo.list[[k]], collapse=', ')
      data.temp <- single.dets.site.period[single.dets.site.period$ShortName %in% combo.list[[k]], ]
      combo.prob <- prod(data.temp$Positives)/(n.tests^n.det)
      
      # based on the sample size at the site at the given time (i.e. the number of tests), choose whether to use the 
      # exact binomial confidence interval, or if the normal approximation of the distribution of error is suitable
      if(n.tests > 30) {
        
        combo.se <- sqrt(combo.prob*(1-combo.prob)/n.tests)
        combo.ci.lower <- combo.prob - combo.se*qnorm(1-0.05/n.combos)
        combo.ci.lower <- ifelse(combo.ci.lower < 0, 0, combo.ci.lower)
        combo.ci.upper <- combo.prob + combo.se*qnorm(1-0.05/n.combos)
      } else {
        
        binom.test()
      }
      
    }
    
    combo.probs <- rbind(combo.probs, temp)
  }
  
  
} 


