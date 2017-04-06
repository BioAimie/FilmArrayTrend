# set the path and load libraries and data
workDir <-'~/FilmArrayTrend/DualDetectionInvestigation/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(devtools)
library(dateManip)

# load user-defined functions
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')

# GET DATA ---------------------------------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/DualDetectionsInvestigation/AllSitesRespiratoryTrendableRuns.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/DualDetectionsInvestigation/PositiveBugsRP.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
bugs.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/DualDetectionsInvestigation/ShortNames.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
nicknames.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in data from PMS PROD server
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- scan('../DataSources/sql/DualDetectionsInvestigation/AllSitesRegionKey.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
regions.df <- sqlQuery(PMScxn,query)
odbcClose(PMScxn)

# FORMAT DATA ------------------------------------------------------------------------------------------------------------
# add regions and epi dates to the run data
calendar.df <- createCalendarLikeMicrosoft(2012, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
runs.reg <- merge(runs.df, data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal), by='Province')
runs.reg$Record <- 1
runs.reg <- merge(runs.reg[,c('RunDataId','Date','Name','CustomerSiteId','Region','Record')], calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')
colnames(runs.reg)[grep('Record', colnames(runs.reg))] <- 'Runs'
# combine flu As into one bug and then add epi dates and sites to the bugs data
bugs.df <- unique(bugs.df)
bugs.df <- merge(bugs.df, nicknames.df, by.x='BugPositive', by.y='Organism')
bugs.df$BugPositive <- as.character(bugs.df$BugPositive)
bugs.df$ShortName <- as.character(bugs.df$ShortName)
# # remove this if we do not plan to group all Flu As... dual detections of H1-2009, H3 and H1, H3 have been seen. No subtype and Flu A haven't 
# # occurred in a dual detection with any other Flu A type in the data set as of 10/20/2016.
# bugs.df[grep('Influenza A', bugs.df$BugPositive), 'ShortName'] <- 'Influenza A (all)' 
bugs.reg <- merge(runs.reg[,c('RunDataId','YearWeek','CustomerSiteId')], bugs.df, by='RunDataId')
colnames(bugs.reg)[grep('ShortName', colnames(bugs.reg))] <- 'Bug'

# CLEAN DATA -------------------------------------------------------------------------------------------------------------
# do not keep sites that have less than 10 runs in a given week because this will skew the data set
runs.agg <- with(runs.reg, aggregate(Runs~YearWeek+CustomerSiteId+Name+Region, FUN=sum))
bugs.agg <- with(data.frame(bugs.reg, Positives = 1), aggregate(Positives~YearWeek+CustomerSiteId+Bug, FUN=sum))
runs.trim <- runs.agg[runs.agg$Runs >= 10, ]
positives.trim <- merge(runs.trim, bugs.agg, by=c('YearWeek','CustomerSiteId'))

# ANALYSIS ---------------------------------------------------------------------------------------------------------------
# For each site, count the number of distinct organisms testing positives in a period, then take an average accross all sites
positives.unique <- data.frame(positives.trim[,c('YearWeek','CustomerSiteId','Bug')], Count = 1)
distinct.postives.site <- with(positives.unique, aggregate(Count~YearWeek+CustomerSiteId, FUN=sum))
distinct.positives.avg <- with(distinct.postives.site, aggregate(Count~YearWeek, FUN=mean))

# For each site, determine the number of dual detections in a period, then take the average accross all sites 
test.detections <- with(data.frame(bugs.reg, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))
###### (this actually is taking up to 4 detections)
co.detections <- bugs.reg[bugs.reg$RunDataId %in% test.detections[test.detections$Positives > 1, 'RunDataId'], ]
co.detections <- co.detections[with(co.detections, order(RunDataId, Bug)), ]
co.detections.concat <- do.call(rbind, lapply(1:length(unique(co.detections$RunDataId)), function(x) data.frame(RunDataId = unique(co.detections$RunDataId)[x], Bugs = paste0(co.detections[co.detections$RunDataId==unique(co.detections$RunDataId)[x],'Bug'], collapse = ', '))))
co.detections.agg <- merge(co.detections.concat, runs.reg[,c('RunDataId','CustomerSiteId','YearWeek')], by='RunDataId')
distinct.cos.bugs.site <- unique(co.detections.agg[,c('YearWeek','CustomerSiteId','Bugs')])
distinct.cos.bugs.site$Count <- 1
distinct.cos.site <- with(distinct.cos.bugs.site, aggregate(Count~YearWeek+CustomerSiteId, FUN=sum))
distinct.cos.avg <- with(distinct.cos.site, aggregate(Count~YearWeek, FUN=mean))

# put all the data into one data frame
distinct.df <- merge(distinct.positives.avg, distinct.cos.avg, by='YearWeek', all.x=TRUE)
ggplot(distinct.df, aes(x=YearWeek, y=Count.x, fill='Count Unique')) + geom_bar(stat='identity') + geom_line(aes(x=YearWeek, y=Count.y, group='Count Co-Detections', group='Count Co-Detections'), data=distinct.df, lwd=1.5) + scale_color_manual(values=c('black'), name='') + scale_fill_manual(values=c('dodgerblue'), name='')

# -------------------------------------------------------------------------------------------------------------------------
# Lindsay would like to see the percent of runs that are dual detections by week and correlate that to the unique organisms
# circulating in a given week. This needs to be a rolling average for smoothing...
# -------------------------------------------------------------------------------------------------------------------------
# for each site, get the number of unique bugs showing up in each week... if there are gaps, fill them with zeros
bugs.sparse <- merge(runs.reg[,c('Year','Week','RunDataId','CustomerSiteId')], bugs.df, by='RunDataId')
bugs.sparse$Positives <- 1
calendar.alt <- data.frame(Date = calendar.df$Date, Year = calendar.df$Year, Week = calendar.df$Week, DateGroup = calendar.df$YearWeek)
calendar.alt$DateGroup <- as.character(calendar.alt$DateGroup)
bugs.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', bugs.sparse, c('CustomerSiteId','BugPositive','ShortName'), min(bugs.agg$YearWeek), 'Positives', 'sum', 0)
# remove Flu A (equivocal) and Flu A (no subtype)
##### do we still want to do this? how should co-detections that already exist be handled... e.g. if a person is HRv+RSV, should they be excluded???
bugs.fill.trim <- bugs.fill[!(bugs.fill$ShortName %in% c('Flu A','Flu A (no subtype)')), ]
bugs.fill.trim$UniquePositives <- with(bugs.fill.trim, ifelse(Positives > 0, 1, 0))
bugs.fill.agg <- with(bugs.fill.trim, aggregate(UniquePositives~DateGroup+CustomerSiteId+BugPositive+ShortName, FUN=sum))
site.bugs.unique <- with(bugs.fill.agg, aggregate(UniquePositives~DateGroup+CustomerSiteId, FUN=sum))
site.bugs.roll <- do.call(rbind, lapply(1:length(unique(site.bugs.unique$CustomerSiteId)), function(x) data.frame(DateGroup = site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'][3:(length(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(site.bugs.unique$CustomerSiteId)[x], UniquePositives = sapply(3:(length(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'UniquePositives'][(y-2):(y+2)])/5))))

# for each site, get the percent of runs that are dual detections in a given week
co.detection.runs <- unique(test.detections[test.detections$Positives > 1, 'RunDataId'])
cos.sparse <- runs.reg[runs.reg$RunDataId %in% co.detection.runs, c('Year','Week','CustomerSiteId','Runs')]
cos.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', cos.sparse, c('CustomerSiteId'), min(bugs.agg$YearWeek), 'Runs', 'sum', 0)
runs.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', runs.reg[,c('Year','Week','CustomerSiteId','Runs')], c('CustomerSiteId'), min(bugs.agg$YearWeek), 'Runs', 'sum', 0)
colnames(cos.fill) <- c('DateGroup','CustomerSiteId','CoDetections')
# find the rolling 5-week average of dual detections per total runs
runs.roll <- do.call(rbind, lapply(1:length(unique(runs.fill$CustomerSiteId)), function(x) data.frame(DateGroup = runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'][3:(length(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(runs.fill$CustomerSiteId)[x], Runs = sapply(3:(length(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'Runs'][(y-2):(y+2)])))))
cos.roll <- do.call(rbind, lapply(1:length(unique(cos.fill$CustomerSiteId)), function(x) data.frame(DateGroup = cos.fill[cos.fill$CustomerSiteId==unique(cos.fill$CustomerSiteId)[x], 'DateGroup'][3:(length(cos.fill[cos.fill$CustomerSiteId==unique(cos.fill$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(cos.fill$CustomerSiteId)[x], CoDetections = sapply(3:(length(cos.fill[cos.fill$CustomerSiteId==unique(cos.fill$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(cos.fill[cos.fill$CustomerSiteId==unique(cos.fill$CustomerSiteId)[x], 'CoDetections'][(y-2):(y+2)])))))

threshold <- 30

cos.rate <- merge(runs.roll, cos.roll, by=c('DateGroup','CustomerSiteId'))
cos.rate[cos.rate$Runs <= threshold, 'Runs'] <- NA
cos.rate$CoDetectionRate <- with(cos.rate, CoDetections/Runs)
cos.rate.avg <- with(cos.rate, aggregate(CoDetectionRate~DateGroup, FUN=mean))

# some site/date combinations have zero unique positives because they had < 10 runs... merge this to the runs.roll to check the run count
site.bugs.roll <- merge(site.bugs.roll, runs.roll, by=c('DateGroup','CustomerSiteId'))
site.bugs.roll[site.bugs.roll$Runs < threshold, 'UniquePositives'] <- NA
sites.bugs.roll <- site.bugs.roll[,c('DateGroup','CustomerSiteId','UniquePositives')]
bug.count.avg <- with(sites.bugs.roll, aggregate(UniquePositives~DateGroup, FUN=mean))

# combine the data & make a chart
rolled.df <- merge(cos.rate.avg, bug.count.avg, by='DateGroup')
p.co.count.combo <- ggplot(subset(rolled.df, as.character(DateGroup) >= '2013-33'), aes(x=DateGroup, y=CoDetectionRate*100, fill='Rate Of Co-Detections')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=UniquePositives, color='Unique Organisms', group='Unique Organisms'), data=subset(rolled.df, as.character(DateGroup) >= '2013-33'), lwd=1.5) + scale_fill_manual(values='darkgrey', name='') + scale_color_manual(values='blue', name='') + scale_x_discrete(breaks=as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2013-33')$DateGroup))[order(as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2013-33')$DateGroup)))][seq(1, length(as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2013-33')$DateGroup))),8)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=20, color='black'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + labs(title='Unique Organisms in Circulation and Rate of Co-Detections\n(Averaged Across All Participating Trend Sites)', y='Unique Circulating Organisms (Count), Co-Detections (%)', x='Date')
with(subset(rolled.df, as.character(DateGroup) >= '2013-33'), cor(UniquePositives, CoDetectionRate))
with(subset(rolled.df, as.character(DateGroup) >= '2013-33'), plot(UniquePositives, CoDetectionRate))
abline(lm(CoDetectionRate~UniquePositives, data=subset(rolled.df, as.character(DateGroup) >= '2013-33')), col='blue', lwd=1.5)

# -----------------------------------------------------------------------------------------------------------
# Create a metric that's like a risk index for susceptibility... for example, if there's 3 organisms circulating
# and one is much more prevalent than the others, this should lower the risk index from 3 to something else
# -----------------------------------------------------------------------------------------------------------
# first, create a data frame that has all the bugs by site with placeholders of 0 if there are no positive tests
bugs <- as.character(unique(bugs.fill.trim$ShortName))
sites <- as.character(unique(bugs.fill.trim$CustomerSiteId))
missing.entries <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(CustomerSiteId=sites[x], ShortName = length(bugs[!(bugs %in% unique(as.character(bugs.fill.trim[bugs.fill.trim$CustomerSiteId == sites[x], 'ShortName'])))]))))
missing.sites <- as.character(missing.entries[missing.entries$ShortName > 0, 'CustomerSiteId'])
bugs.sparse.add <- do.call(rbind, lapply(1:length(missing.sites), function(x) data.frame(CustomerSiteId=missing.sites[x], ShortName = bugs[!(bugs %in% unique(as.character(bugs.fill.trim[bugs.fill.trim$CustomerSiteId == missing.sites[x], 'ShortName'])))], Positives = 0)))
bugs.sparse.add$Year <- max(bugs.sparse$Year)
bugs.sparse.add$Week <- min(bugs.sparse$Week)
bugs.sparse.update <- rbind(bugs.sparse[,c('Year','Week','CustomerSiteId','ShortName','Positives')], bugs.sparse.add[,c('Year','Week','CustomerSiteId','ShortName','Positives')])
bugs.roll <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', bugs.sparse.update, c('CustomerSiteId','ShortName'), min(bugs.agg$YearWeek), 'Positives', 'sum', 0)
bugs.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(bugs), function(y) data.frame(DateGroup = bugs.roll[bugs.roll$CustomerSiteId==sites[x] & bugs.roll$ShortName==bugs[y], 'DateGroup'][3:(length(bugs.roll[bugs.roll$CustomerSiteId==sites[x] & bugs.roll$ShortName==bugs[y], 'DateGroup'])-2)], CustomerSiteId = sites[x], ShortName = bugs[y], Positives = sapply(3:(length(bugs.roll[bugs.roll$CustomerSiteId==sites[x] & bugs.roll$ShortName==bugs[y], 'DateGroup'])-2), function(z) sum(bugs.roll[bugs.roll$CustomerSiteId==sites[x] & bugs.roll$ShortName==bugs[y], 'Positives'][(z-2):(z+2)])))))))
# combine this with runs so that the percent detection for the period (5-week centered sums) can be calculated
percent.det.site <- merge(bugs.roll[,c('DateGroup','CustomerSiteId','ShortName','Positives')], runs.roll[,c('DateGroup','CustomerSiteId','Runs')], by=c('DateGroup','CustomerSiteId'))
# negatives should also be counted
negatives.sparse <- merge(runs.reg[,c('RunDataId','Year','Week','CustomerSiteId')], bugs.df[,c('RunDataId','ShortName')], by='RunDataId', all.x=TRUE)
negatives.sparse <- data.frame(negatives.sparse[is.na(negatives.sparse$ShortName), c('Year','Week','CustomerSiteId')], Negatives = 1)
negatives.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', negatives.sparse, c('CustomerSiteId'), min(bugs.agg$YearWeek), 'Negatives', 'sum', 0)
negatives.roll <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(DateGroup = negatives.fill[negatives.fill$CustomerSiteId==sites[x],'DateGroup'][3:(length(negatives.fill[negatives.fill$CustomerSiteId==sites[x],'DateGroup'])-2)], CustomerSiteId = sites[x], Negatives = sapply(3:(length(negatives.fill[negatives.fill$CustomerSiteId==sites[x],'DateGroup'])-2), function(y) sum(negatives.fill[negatives.fill$CustomerSiteId==sites[x],'Negatives'][(y-2):(y+2)])))))
# combine the percent.det.site with the negative tests
percent.det.site <- merge(percent.det.site, negatives.roll, by=c('DateGroup','CustomerSiteId'))
percent.det.site$PercentDetection <- with(percent.det.site, Positives/Runs)
percent.det.site$PercentNegative <- with(percent.det.site, Negatives/Runs)

# # find the risk ratio... this is essentially the percent detection of a given organism in the 5-week centered period 
# # multiplied by the total percent detection of all other organisms in the same period. The idea behind this is that
# # it will indicate the risk of a person who is positive for a single organism to contract any other organism when they
# # visit the hospital.
# periods <- as.character(unique(percent.det.site$DateGroup))[as.character(unique(percent.det.site$DateGroup)) >= '2013-33']
# riskratio.site <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(periods), function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(DateGroup = periods[y], CustomerSiteId = sites[x], ShortName = bugs[z], RiskRatio = percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection']))))))))
# # see if this will work to filter out weeks where there are very few runs
# riskratio.site.mrg <- merge(runs.roll, riskratio.site, by=c('DateGroup','CustomerSiteId'))
# riskratio.site.mrg[riskratio.site.mrg$Runs <= threshold, 'RiskRatio'] <- NA
# # aggregate the risk ratios over all sites and take the mean for each period with 'enough' data and then add in the unique
# # organism count as well as the % dual detections
# riskratio.avg <- with(riskratio.site.mrg, aggregate(RiskRatio~DateGroup+ShortName, FUN=mean))
# risk.count.cos <- merge(merge(bug.count.avg, cos.rate.avg, by='DateGroup'), riskratio.avg, by='DateGroup')
# risk.count.cos.trim <- subset(risk.count.cos, as.character(DateGroup) >= '2013-33')
# # the correlation between the risk ratio and the count of unique organsims circulating is quite good
# cor(with(risk.count.cos.trim, aggregate(UniquePositives~DateGroup, FUN=mean))$UniquePositives, with(risk.count.cos.trim, aggregate(RiskRatio~DateGroup, FUN=sum))$RiskRatio)
# ggplot(risk.count.cos.trim[with(risk.count.cos.trim, order(DateGroup, ShortName, decreasing=TRUE)), ], aes(x=DateGroup)) + geom_area(aes(y=RiskRatio*25, fill=ShortName, order=ShortName, group=ShortName), stat='identity', position='stack') + geom_line(aes(x=DateGroup, y=CoDetectionRate*100, group='Co-Detections/Total Tests', color='Co-Detections/Total Tests'), size=2, data=risk.count.cos.trim) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Detections', color='Unique Detections'), size=2, data=risk.count.cos.trim) + scale_fill_manual(values=createPaletteOfVariableLength(risk.count.cos.trim, 'ShortName'), name='') + scale_color_manual(values=c('black','blue'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = as.character(unique(risk.count.cos.trim$DateGroup))[order(as.character(unique(risk.count.cos.trim$DateGroup)))][seq(1, length(as.character(unique(risk.count.cos.trim$DateGroup))), 8)]) + labs(title='Co-Detections, Unique Organism Detections, and Risk Ratio\n(5-week centered rolling average)', x='Year-Week', y='% Co-Detections, Organism Count, Risk Ratio')
# 
# risk.count.cos.agg <- merge(with(risk.count.cos.trim, aggregate(UniquePositives~DateGroup, FUN=mean)), with(risk.count.cos.trim, aggregate(RiskRatio~DateGroup, FUN=sum)), by='DateGroup')
# risk.count.cos.agg <- merge(risk.count.cos.agg, with(risk.count.cos.trim, aggregate(CoDetectionRate~DateGroup, FUN=mean)), by='DateGroup')
# ggplot(risk.count.cos.agg, aes(x=DateGroup, y=100*CoDetectionRate, fill='Co-Detections/Total Runs')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=25*RiskRatio, group='Aggregate Risk Ratio', color='Aggregate Risk Ratio'), data=risk.count.cos.agg, size=2) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=risk.count.cos.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = as.character(unique(risk.count.cos.trim$DateGroup))[order(as.character(unique(risk.count.cos.trim$DateGroup)))][seq(1, length(as.character(unique(risk.count.cos.trim$DateGroup))), 8)]) + scale_color_manual(values=c('blue','black'), name='') + scale_fill_manual(values='grey', name='')

#### RIGHT HERE!!!
# a <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(periods), function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(DateGroup = periods[y], CustomerSiteId = sites[x], ShortName = bugs[z], RiskRatio = percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection']), mVariance = (percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*(1-percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection'])/percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'Runs'])*(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection'])^2)))))))
# a <- do.call(rbind, lapply(3:3, function(x) do.call(rbind, lapply(180:182, function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(DateGroup = periods[y], CustomerSiteId = sites[x], ShortName = bugs[z], RiskRatio = percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection']), mVariance = (percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*(1-percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection'])/percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'Runs'])*(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection'])^2)))))))
# do.call(rbind, lapply(1:length(bugs), function(x) (((b[b$ShortName==bugs[x], 'Positives']/b[b$ShortName==bugs[x], 'Runs'])*(1-b[b$ShortName==bugs[x], 'Positives']/b[b$ShortName==bugs[x], 'Runs']))/b[b$ShortName==bugs[x], 'Runs'])*(sum(b[b$ShortName!=bugs[x],'Positives'])/b[b$ShortName==bugs[x], 'Runs'])^2))
# the issue is that the part finding the associated summation is returning an array, and that blows shit up
riskratio.site.ci <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(periods), function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(DateGroup = periods[y], CustomerSiteId = sites[x], ShortName = bugs[z], RiskRatio = percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection']), mVariance = (((percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*(1-percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']))/percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'Runs'])*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection'])^2))))))))
riskratio.site.ci.mrg <- merge(runs.roll, riskratio.site.ci, by=c('DateGroup','CustomerSiteId'))
riskratio.site.ci.mrg[riskratio.site.ci.mrg$Runs <= threshold, 'RiskRatio'] <- NA
riskratio.site.ci.mrg[riskratio.site.ci.mrg$Runs <= threshold, 'mVariance'] <- NA
ci.bounds <- with(riskratio.site.ci.mrg, aggregate(mVariance~DateGroup+CustomerSiteId, FUN=sum))
ci.bounds$mVariance <- 4*ci.bounds$mVariance
ci.bounds$sdev <- sqrt(ci.bounds$mVariance)
riskratio.ci.avg <- with(riskratio.site.ci.mrg, aggregate(RiskRatio~DateGroup+ShortName, FUN=mean))
risk.count.cos.ci <- merge(merge(merge(bug.count.avg, cos.rate.avg, by='DateGroup'), riskratio.ci.avg, by='DateGroup'), with(ci.bounds, aggregate(sdev~DateGroup, FUN=mean)), by='DateGroup')
risk.count.cos.ci.trim <- subset(risk.count.cos.ci, as.character(DateGroup) >= '2013-33')
risk.count.cos.ci.agg <- merge(with(risk.count.cos.ci.trim, aggregate(UniquePositives~DateGroup, FUN=mean)), with(risk.count.cos.ci.trim, aggregate(RiskRatio~DateGroup, FUN=sum)), by='DateGroup')
risk.count.cos.ci.agg <- merge(risk.count.cos.ci.agg, with(risk.count.cos.ci.trim, aggregate(CoDetectionRate~DateGroup, FUN=mean)), by='DateGroup')
risk.count.cos.ci.agg <- merge(risk.count.cos.ci.agg, with(risk.count.cos.ci.trim, aggregate(sdev~DateGroup, FUN=mean)), by='DateGroup')
risk.count.cos.ci.agg$LL <- risk.count.cos.ci.agg$RiskRatio - qnorm(0.975)*risk.count.cos.ci.agg$sdev
risk.count.cos.ci.agg$UL <- risk.count.cos.ci.agg$RiskRatio + qnorm(0.975)*risk.count.cos.ci.agg$sdev

# # get the Euclidean distance between the vectors of Risk and CoDetection
# ed <- dist(with(risk.count.cos.ci.agg, rbind(RiskRatio, CoDetectionRate)))
# a <- risk.count.cos.ci.agg
# a$RiskFactoredByED <- risk.count.cos.ci.agg$RiskRatio/ed

# a <- matrix(0, nrow=length(risk.count.cos.ci.agg$CoDetectionRate), ncol=length(risk.count.cos.ci.agg$CoDetectionRate))
# for(i in 1:nrow(a)) { a[i, i] <- risk.count.cos.ci.agg$CoDetectionRate[i]}
# e <- eigen(a)

#####
# change this code to make everything on one axis and remove the confidence interval (grey bars, red line) - TCPM
risk.fit <- lm(RiskRatio~CoDetectionRate, data=risk.count.cos.ci.agg)
m <- round(risk.fit$coefficients[[2]], 2)
r <- round(summary(risk.fit)$r.squared, 3)
p.CosAndRiskOnSingleAxisFinalFormat <- ggplot(risk.count.cos.ci.agg, aes(x=DateGroup, y=100*CoDetectionRate, group='Co-Detection Rate', fill='Co-Detection Rate')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=100*RiskRatio/m, group='TCPM', color='TCPM'), data=risk.count.cos.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_color_manual(values=c('red','blue','black'), name='') + scale_fill_manual(values='grey', name='') + labs(y='Co-Detection Rate (%), TCPM', x='Date')
p.CoDetVsTCPM <- ggplot(risk.count.cos.ci.agg, aes(x=CoDetectionRate, y=RiskRatio)) + geom_point() + geom_abline(intercept=lm(RiskRatio~CoDetectionRate, data=risk.count.cos.ci.agg)$coefficients[[1]], slope = lm(RiskRatio~CoDetectionRate, data=risk.count.cos.ci.agg)$coefficients[[2]], size=2, color='black', alpha=0.2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_continuous(limits=c(0, 0.15), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125, 0.15), labels=c('0.0','2.5','5.0','7.5','10.0','12.5','15.0')) + scale_y_continuous(limits=c(0, 0.7), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), labels=c('0.0','10.0','20.0','30.0','40.0','50.0','60.0','70.0')) + labs(y='TCPM', x='Co-Detection Rate (%)') + annotate('text', x=0.125, y=0.625, label=paste(paste('R2 =', r, sep=' '), paste('Slope =', m, sep=' '), sep='\n'), size=5)
#####
p.CosAndRiskOnSingleAxisWithCIs <- ggplot(risk.count.cos.ci.agg, aes(x=DateGroup, y=100*CoDetectionRate, group='Co-Detection Rate', fill='Co-Detection Rate')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=100*RiskRatio/m, group='TCPM', color='TCPM'), data=risk.count.cos.agg, size=2) + geom_ribbon(aes(x=DateGroup, ymin=100*LL/4, ymax=100*UL/4), data=risk.count.cos.ci.agg, alpha=0.2, fill='red') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_color_manual(values=c('red','blue','black'), name='') + scale_fill_manual(values='grey', name='') + labs(y='Co-Detection Rate (%), TCPM', x='Date')


with(risk.count.cos.ci.agg, cor(RiskRatio, CoDetectionRate))
cos.risk.ccf <- ccf(risk.count.cos.ci.agg$RiskRatio, risk.count.cos.ci.agg$CoDetectionRate)
cos.risk.ccf.df <- data.frame(Lag = cos.risk.ccf$lag, CCF = cos.risk.ccf$acf)
cos.risk.ccf.df[cos.risk.ccf.df$CCF==max(cos.risk.ccf.df$CCF), ]
 
# dual axes for ILI overlay plots
hinvert_title_grob <- function(grob) {
  
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

dateBreaks <- c('2013-27','2013-40','2014-01', '2014-14','2014-27','2014-40','2015-01', '2015-14','2015-27','2015-40','2016-01','2016-14','2016-27','2016-40','2017-01')
dateLabels <- c('Jul-2013','-','Jan-2014','-','Jul-2014','-','Jan-2015','-','Jul-2015','-','Jan-2016','-','Jul-2016','-','Jan-2017')

# p1 <- ggplot(risk.count.cos.agg, aes(x=DateGroup, y=100*CoDetectionRate, group='Co-Detections Rate', color='Co-Detections Rate')) + geom_line(size=2) + geom_line(aes(x=DateGroup, y=25*RiskRatio, group='Aggregate Risk Ratio', color='Aggregate Risk Ratio'), data=risk.count.cos.agg, size=2) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=risk.count.cos.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,14), breaks=c(0, 2, 4, 6, 8, 10, 12, 14)) + scale_color_manual(values=c('red','blue','black'), name='') + scale_fill_manual(values='grey', name='') + labs(y='Co-Detection (%), Unique Positives', x='Date')
# p2 <- ggplot(risk.count.cos.agg, aes(x=DateGroup, y=25*RiskRatio, group=1)) + geom_line(color='transparent') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,14), breaks=c(0, 2, 4, 6, 8, 10, 12, 14), labels=c('0','8','16','24','32','40','48','56')) + theme(panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y='People at Risk per 100 Patients')
p1 <- ggplot(risk.count.cos.agg, aes(x=DateGroup, y=100*CoDetectionRate, group='Co-Detection Rate', fill='Co-Detection Rate')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=25*RiskRatio, group='TCPM', color='TCPM'), data=risk.count.cos.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,14), breaks=c(0, 2, 4, 6, 8, 10, 12, 14)) + scale_color_manual(values=c('red','blue','black'), name='') + scale_fill_manual(values='grey', name='') + labs(y='Co-Detection Rate (%)', x='Date')
p2 <- ggplot(risk.count.cos.agg, aes(x=DateGroup, y=25*RiskRatio, group=1)) + geom_line(color='transparent') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,14), breaks=c(0, 2, 4, 6, 8, 10, 12, 14), labels=c('0','8','16','24','32','40','48','56')) + theme(panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y='People at Risk per 100 Patients')

# Get the ggplot grobs
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Get the location of the plot panel in g1.
# These are used later when transformed elements of g2 are put back into g1
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

# Get the y axis title from g2
index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
ylab <- g2$grobs[[index]]                # Extract that grob
ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications

# Put the transformed label on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# First, move the axis line to the left
yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))

# Second, swap tick marks and tick mark labels
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths) 
ticks$grobs <- rev(ticks$grobs)

# Third, move the tick marks
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")

# Fourth, swap margins and fix justifications for the tick mark labels
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

# Fifth, put ticks back into yaxis
yaxis$children[[2]] <- ticks

# Put the transformed yaxis on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
tripleOverlay <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")

# Draw it
grid.newpage()
png('Figures/CoDetectionRiskMetrics.png', height=800, width=1400)
grid.draw(tripleOverlay)
dev.off()

# WIth confidence intervals
p1 <- ggplot(aes(x=DateGroup, y=25*RiskRatio, group='TCPM', color='TCPM'), data=risk.count.cos.ci.agg) + geom_line(size=2) + geom_ribbon(aes(ymin=25*LL, ymax=25*UL), alpha=0.2, data=risk.count.cos.ci.agg, color='transparent', fill='blue') + geom_line(aes(x=DateGroup, y=100*CoDetectionRate, color='Co-Detections/Total Runs', group='Co-Detections/Total Runs'), data=risk.count.cos.ci.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = dateBreaks, labels=dateLabels) + scale_color_manual(values=c('black','blue'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(title='', y='Co-Detection Rate (%)', x='Date') + scale_y_continuous(limits=c(0, 16), breaks=c(0,2,4,6,8,10,12,14,16))
p2 <- ggplot(risk.count.cos.ci.agg) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,16), breaks=c(0, 2, 4, 6, 8, 10, 12, 14, 16), labels=c('0','8','16','24','32','40','48','56','62')) + theme(panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y='People at Risk per 100 Patients')

# Get the ggplot grobs
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Get the location of the plot panel in g1.
# These are used later when transformed elements of g2 are put back into g1
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

# Get the y axis title from g2
index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
ylab <- g2$grobs[[index]]                # Extract that grob
ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications

# Put the transformed label on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# First, move the axis line to the left
yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))

# Second, swap tick marks and tick mark labels
ticks <- yaxis$children[[2]]
ticks$widths <- rev(ticks$widths) 
ticks$grobs <- rev(ticks$grobs)

# Third, move the tick marks
ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")

# Fourth, swap margins and fix justifications for the tick mark labels
ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

# Fifth, put ticks back into yaxis
yaxis$children[[2]] <- ticks

# Put the transformed yaxis on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
overlayWithCI <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")

# Draw it
grid.newpage()
png('Figures/CoDetectionRiskMetricWithConfidenceIntervals.png', height=800, width=1400)
grid.draw(overlayWithCI)
dev.off()

#  Make a chart showing the count of unique organisms circulating
risk.count.cos.agg <- merge(risk.count.cos.agg, with(sites.bugs.roll, aggregate(UniquePositives~DateGroup, FUN=sd)), by='DateGroup')
colnames(risk.count.cos.agg)[grep('Unique', colnames(risk.count.cos.agg))] <- c('UniquePositives','Sdev')
p.UniquePositives <- ggplot(aes(x=DateGroup, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=risk.count.cos.agg) + geom_line(size=2) + geom_ribbon(aes(x=DateGroup, ymin=(UniquePositives-Sdev), ymax=(UniquePositives+Sdev)), data=risk.count.cos.agg, color='transparent', fill='grey', alpha=0.5) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,16), breaks=c(0, 2, 4, 6, 8, 10, 12, 14,16)) + scale_color_manual(values=c('black'), name='', guide=FALSE) + labs(y='Circulating Pathogen Number', x='Date')

# Lindsay would like this to be rerun with batch sizes of ~100 if a site has a lot of tests in the period... b/c 1000 people probably won't come into contact with each other
runs.rand <- merge(calendar.df[,c('Date','YearWeek')], data.frame(runs.df[,c('Date','RunDataId','CustomerSiteId')], Runs = 1), by='Date')
bugs.rand <- merge(data.frame(bugs.df, Positive = 1), runs.rand[,c('RunDataId','YearWeek','CustomerSiteId')], by='RunDataId')
periods.rand <- unique(runs.fill[runs.fill$DateGroup > '2013-23','DateGroup'])

rand.sample <- c()
for(i in 3:(length(periods.rand)-2)) {
    
  # for a given five week period, keep the runs in that period with the associated site
  runs.rand.period <- runs.rand[runs.rand$YearWeek %in% periods.rand[(i-2):(i+2)], ]
  
  # loop through all sites in the period and bin runs into group of threshold or less
  period.threshold <- 100
  period.sites <- unique(runs.rand.period$CustomerSiteId)
  period.samples <- c()
  for(j in 1:length(period.sites)) {
    
    runs.rand.period.site <- runs.rand.period[runs.rand.period$CustomerSiteId==period.sites[j], ]
    
    # if there are less than threshold runs in the period (centered 5 weeks, so < threshold/5 runs/week) for a given site, skip it
    if(nrow(runs.rand.period.site) < period.threshold) { next() }
    
    # if there are more than threshold runs in the period for a given site, split them out into multiple segements
    divisor <- ceiling(nrow(runs.rand.period.site)/period.threshold)
    sample.size <- round(nrow(runs.rand.period.site)/divisor, 0)
    sample.ids.init <- sample(runs.rand.period.site$RunDataId, sample.size, FALSE)
    sample.ids.free <- runs.rand.period.site[!(runs.rand.period.site$RunDataId %in% sample.ids.init), 'RunDataId']
    
    # for the number of samples (divisor), loop through and randomly choose RunDataIds from the site in the period to determine:
    # 1. the number of unique organisms in the period, 2. the co-detection rate, and 3. the risk metric
    ######################################## MAY ALSO WANT TO ADD IN THE CALCULATION FOR THE CI OF THE RISK METRIC
    site.samples <- c()
    for(k in 1:divisor) {
      
      if(k == 1) {
        
        sample.ids <- sample.ids.init
        size.sample.period.site <- length(sample.ids)
        runs.sample.period.site <- runs.rand.period.site[runs.rand.period.site$RunDataId %in% sample.ids, ]
        bugs.sample.period.site <- bugs.df[bugs.df$RunDataId %in% runs.sample.period.site$RunDataId, ]
        unique.bugs.in.sample <- length(unique(bugs.sample.period.site$BugPositive))
        co.detections.in.sample <- sum(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))$Positives > 1)/size.sample.period.site

        bugs.sample <- as.character(unique(bugs.sample.period.site$ShortName)) 
        percent.det.sample <- data.frame(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~ShortName, FUN=sum)), Runs = nrow(runs.sample.period.site))
        percent.det.sample$PercentDetection <- with(percent.det.sample, Positives/Runs)
        risk.metric.in.sample <- sum(sapply(1:length(bugs.sample), function(x) RiskRatio = percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])))
        metric.var.in.sample <- 4*sum(sapply(1:length(bugs.sample), function(x) mVariance = (((percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*(1-percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']))/percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'Runs'])*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])^2)))
        
        temp <- data.frame(YearWeek = periods.rand[i], CustomerSiteId = period.sites[j], SampleSize = size.sample.period.site, UniquePositives = unique.bugs.in.sample, CoDetectionRate = co.detections.in.sample, RiskRatio = risk.metric.in.sample, RiskVariance = metric.var.in.sample)

        site.samples <- rbind(site.samples, temp)
      } else if (k < divisor) {
        
        sample.ids <- sample(sample.ids.free, sample.size, FALSE)
        runs.sample.period.site <- runs.rand.period.site[runs.rand.period.site$RunDataId %in% sample.ids, ]
        bugs.sample.period.site <- bugs.df[bugs.df$RunDataId %in% runs.sample.period.site$RunDataId, ]
        unique.bugs.in.sample <- length(unique(bugs.sample.period.site$BugPositive))
        co.detections.in.sample <- sum(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))$Positives > 1)/size.sample.period.site
        
        bugs.sample <- as.character(unique(bugs.sample.period.site$ShortName)) 
        percent.det.sample <- data.frame(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~ShortName, FUN=sum)), Runs = nrow(runs.sample.period.site))
        percent.det.sample$PercentDetection <- with(percent.det.sample, Positives/Runs)
        risk.metric.in.sample <- sum(sapply(1:length(bugs.sample), function(x) RiskRatio = percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])))
        metric.var.in.sample <- 4*sum(sapply(1:length(bugs.sample), function(x) mVariance = (((percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*(1-percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']))/percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'Runs'])*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])^2)))
        
        temp <- data.frame(YearWeek = periods.rand[i], CustomerSiteId = period.sites[j], SampleSize = size.sample.period.site, UniquePositives = unique.bugs.in.sample, CoDetectionRate = co.detections.in.sample, RiskRatio = risk.metric.in.sample, RiskVariance = metric.var.in.sample)
    
        sample.ids.init <- c(sample.ids.init, sample.ids)
        sample.ids.free <- sample.ids.free[!(sample.ids.free %in% sample.ids.init)]
        site.samples <- rbind(site.samples, temp)
      } else {
        
        sample.ids <- sample.ids.free
        size.sample.period.site <- length(sample.ids)
        runs.sample.period.site <- runs.rand.period.site[runs.rand.period.site$RunDataId %in% sample.ids, ]
        bugs.sample.period.site <- bugs.df[bugs.df$RunDataId %in% runs.sample.period.site$RunDataId, ]
        unique.bugs.in.sample <- length(unique(bugs.sample.period.site$BugPositive))
        co.detections.in.sample <- sum(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))$Positives > 1)/size.sample.period.site
        
        bugs.sample <- as.character(unique(bugs.sample.period.site$ShortName)) 
        percent.det.sample <- data.frame(with(data.frame(bugs.sample.period.site, Positives = 1), aggregate(Positives~ShortName, FUN=sum)), Runs = nrow(runs.sample.period.site))
        percent.det.sample$PercentDetection <- with(percent.det.sample, Positives/Runs)
        risk.metric.in.sample <- sum(sapply(1:length(bugs.sample), function(x) RiskRatio = percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])))
        metric.var.in.sample <- 4*sum(sapply(1:length(bugs.sample), function(x) mVariance = (((percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']*(1-percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'PercentDetection']))/percent.det.sample[percent.det.sample$ShortName==bugs.sample[x],'Runs'])*sum(percent.det.sample[percent.det.sample$ShortName!=bugs.sample[x],'PercentDetection'])^2)))
        
        temp <- data.frame(YearWeek = periods.rand[i], CustomerSiteId = period.sites[j], SampleSize = size.sample.period.site, UniquePositives = unique.bugs.in.sample, CoDetectionRate = co.detections.in.sample, RiskRatio = risk.metric.in.sample, RiskVariance = metric.var.in.sample)
        site.samples <- rbind(site.samples, temp)
      }
    }
    
    period.samples <- rbind(period.samples, site.samples)
  }
  rand.sample <- rbind(rand.sample, period.samples)
}

rand.agg <- with(rand.sample, aggregate(cbind(UniquePositives, CoDetectionRate, RiskRatio, RiskVariance)~YearWeek, FUN=mean))
rand.unique.sdev <- with(rand.sample, aggregate(UniquePositives~YearWeek, FUN=sd))
colnames(rand.unique.sdev)[2] <- 'SdevUniquePositives'
rand.agg <- merge(rand.agg, rand.unique.sdev, by='YearWeek')
rand.agg$LowerCI <- rand.agg$RiskRatio + qnorm(0.025)*sqrt(rand.agg$RiskVariance)
rand.agg$UpperCI <- rand.agg$RiskRatio + qnorm(0.975)*sqrt(rand.agg$RiskVariance)
# create a plot of the co-detection rate with the risk metric overlaid
p.CoDetectionsAndRiskMetricSingleAxis <- ggplot(rand.agg, aes(x=YearWeek, y=CoDetectionRate, fill='Co-Detection Rate')) + geom_bar(stat='identity') + geom_line(aes(x=YearWeek, y=RiskRatio, group='Risk Metric', color='Risk Metric', fill='Risk Metric'), lwd=1.5, color='black', data=rand.agg) + scale_fill_manual(values=c('grey','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = dateBreaks, labels=dateLabels) + labs(y='Co-Detections/Tests, Risk Metric', x='Date') + scale_y_continuous(limits=c(0, 0.6), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))
p.UniqueCirculatingOrganismsSampled <- ggplot(aes(x=YearWeek, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=rand.agg) + geom_line(size=2) + geom_ribbon(aes(x=YearWeek, ymin=(UniquePositives-SdevUniquePositives), ymax=(UniquePositives+SdevUniquePositives)), data=rand.agg, color='transparent', fill='grey', alpha=0.5) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,16), breaks=c(0, 2, 4, 6, 8, 10, 12, 14,16)) + scale_color_manual(values=c('black'), name='', guide=FALSE) + labs(y='Unique Organisms +/- 1 Standard Deviation', x='Date')

ggplot(rand.agg, aes(x=CoDetectionRate, y=RiskRatio)) + geom_point() + geom_ribbon(aes(ymin=LowerCI, ymax=UpperCI), alpha=0.2, data=rand.agg)


if(FALSE) {
  runs.rand <- merge(calendar.df[,c('Date','YearWeek')], data.frame(runs.df[,c('Date','RunDataId','CustomerSiteId')], Runs = 1), by='Date')
  bugs.rand <- merge(data.frame(bugs.df, Positive = 1), runs.rand[,c('RunDataId','YearWeek','CustomerSiteId')], by='RunDataId')
  id.big.sites <- with(runs.rand, aggregate(Runs~YearWeek+CustomerSiteId, FUN=sum))
  
  runs.df.split <- c()
  for(i in 1:length(sites)) {
    
    for(j in 1:length(periods)) {
      
      # find the median number of runs per the date period, throwing out zeros
      median.runs.in.period <- median(runs.fill[runs.fill$DateGroup==periods[j] & runs.fill$Runs > 0, 'Runs'])
      runs.site.period <- runs.fill[runs.fill$CustomerSiteId==sites[i] & runs.fill$DateGroup==periods[j], ]
      
      if(runs.site.period$Runs > (1.96*median.runs.in.period)) {
        
        divide.by <- round(runs.site.period$Runs/median.runs.in.period, 0)
        sample.split <- floor(runs.site.period$Runs/divide.by)
        run.ids.to.split <- runs.rand[runs.rand$YearWeek==periods[j] & runs.rand$CustomerSiteId==sites[i], 'RunDataId']
        
        # since there could be any number of samples within the site (sample.split is not known prior to the looping and must be flexible)...
        id.sample <- sample(run.ids.to.split, sample.split, FALSE)
        runs.site.split <- runs.df[runs.df$RunDataId %in% id.sample, ]
        runs.site.split$CustomerSiteId <- paste(runs.site.split$CustomerId, letters[1], sep='')
        
        for(k in 2:divide.by) {
          
          # if the iteration is equal to the sample split size, then just take all remaining ids and put them in the sample
          if(k == divide.by) {
            
            id.remaining <- run.ids.to.split[!(run.ids.to.split %in% id.sample)]
            runs.site.split.temp <- runs.df[runs.df$RunDataId %in% id.remaining, ]
            runs.site.split.temp$CustomerSiteId <- paste(runs.site.split.temp$CustomerId, letters[k], sep='')
            runs.site.split <- rbind(runs.site.split, runs.site.split.temp)
          } else {
            
            id.sample.iter <- sample(run.ids.to.split[!(run.ids.to.split %in% id.sample)], sample.split, FALSE)
            runs.site.split.temp <- runs.df[runs.df$RunDataId %in% id.sample.iter, ]
            runs.site.split.temp$CustomerSiteId <- paste(runs.site.split.temp$CustomerId, letters[k], sep='')  
            runs.site.split <- rbind(runs.site.split, runs.site.split.temp)
            id.sample <- c(id.sample, id.sample.iter)
          }
        }
      } else {
        
        runs.site.split <- runs.df[runs.df$RunDataId %in% runs.rand[runs.rand$YearWeek==periods[j] & runs.rand$CustomerSiteId==sites[i], 'RunDataId'], ]
        runs.site.split$CustomerSiteId <- paste(runs.site.split$CustomerId, letters[1], sep='')
      }
      
      runs.df.split <- rbind(runs.df.split, runs.site.split)
    }
  }
}