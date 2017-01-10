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
install_github('BioAimie/dateManip')
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
dual.detections <- bugs.reg[bugs.reg$RunDataId %in% test.detections[test.detections$Positives > 1, 'RunDataId'], ]
dual.detections <- dual.detections[with(dual.detections, order(RunDataId, Bug)), ]
dual.detections.concat <- do.call(rbind, lapply(1:length(unique(dual.detections$RunDataId)), function(x) data.frame(RunDataId = unique(dual.detections$RunDataId)[x], Bugs = paste0(dual.detections[dual.detections$RunDataId==unique(dual.detections$RunDataId)[x],'Bug'], collapse = ', '))))
dual.detections.agg <- merge(dual.detections.concat, runs.reg[,c('RunDataId','CustomerSiteId','YearWeek')], by='RunDataId')
distinct.duals.bugs.site <- unique(dual.detections.agg[,c('YearWeek','CustomerSiteId','Bugs')])
distinct.duals.bugs.site$Count <- 1
distinct.duals.site <- with(distinct.duals.bugs.site, aggregate(Count~YearWeek+CustomerSiteId, FUN=sum))
distinct.duals.avg <- with(distinct.duals.site, aggregate(Count~YearWeek, FUN=mean))

# put all the data into one data frame
distinct.df <- merge(distinct.positives.avg, distinct.duals.avg, by='YearWeek', all.x=TRUE)

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
bugs.fill <- bugs.fill[!(bugs.fill$ShortName %in% c('Flu A','Flu A (no subtype)')), ]
bugs.fill$UniquePositives <- with(bugs.fill, ifelse(Positives > 0, 1, 0))
bugs.fill.agg <- with(bugs.fill, aggregate(UniquePositives~DateGroup+CustomerSiteId+BugPositive+ShortName, FUN=sum))
site.bugs.unique <- with(bugs.fill.agg, aggregate(UniquePositives~DateGroup+CustomerSiteId, FUN=sum))
site.bugs.roll <- do.call(rbind, lapply(1:length(unique(site.bugs.unique$CustomerSiteId)), function(x) data.frame(DateGroup = site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'][3:(length(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(site.bugs.unique$CustomerSiteId)[x], UniquePositives = sapply(3:(length(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(site.bugs.unique[site.bugs.unique$CustomerSiteId==unique(site.bugs.unique$CustomerSiteId)[x], 'UniquePositives'][(y-2):(y+2)])/5))))

# for each site, get the percent of runs that are dual detections in a given week
dual.detection.runs <- unique(test.detections[test.detections$Positives > 1, 'RunDataId'])
duals.sparse <- runs.reg[runs.reg$RunDataId %in% dual.detection.runs, c('Year','Week','CustomerSiteId','Runs')]
duals.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', duals.sparse, c('CustomerSiteId'), min(bugs.agg$YearWeek), 'Runs', 'sum', 0)
runs.fill <- aggregateAndFillDateGroupGaps(calendar.alt, 'Week', runs.reg[,c('Year','Week','CustomerSiteId','Runs')], c('CustomerSiteId'), min(bugs.agg$YearWeek), 'Runs', 'sum', 0)
colnames(duals.fill) <- c('DateGroup','CustomerSiteId','DualPositives')
# find the rolling 5-week average of dual detections per total runs
runs.roll <- do.call(rbind, lapply(1:length(unique(runs.fill$CustomerSiteId)), function(x) data.frame(DateGroup = runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'][3:(length(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(runs.fill$CustomerSiteId)[x], Runs = sapply(3:(length(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(runs.fill[runs.fill$CustomerSiteId==unique(runs.fill$CustomerSiteId)[x], 'Runs'][(y-2):(y+2)])))))
duals.roll <- do.call(rbind, lapply(1:length(unique(duals.fill$CustomerSiteId)), function(x) data.frame(DateGroup = duals.fill[duals.fill$CustomerSiteId==unique(duals.fill$CustomerSiteId)[x], 'DateGroup'][3:(length(duals.fill[duals.fill$CustomerSiteId==unique(duals.fill$CustomerSiteId)[x], 'DateGroup'])-2)], CustomerSiteId = unique(duals.fill$CustomerSiteId)[x], DualPositives = sapply(3:(length(duals.fill[duals.fill$CustomerSiteId==unique(duals.fill$CustomerSiteId)[x], 'DateGroup'])-2), function(y) sum(duals.fill[duals.fill$CustomerSiteId==unique(duals.fill$CustomerSiteId)[x], 'DualPositives'][(y-2):(y+2)])))))

threshold <- 30

duals.rate <- merge(runs.roll, duals.roll, by=c('DateGroup','CustomerSiteId'))
duals.rate[duals.rate$Runs < threshold, 'Runs'] <- NA
duals.rate$DualRate <- with(duals.rate, DualPositives/Runs)
duals.rate.avg <- with(duals.rate, aggregate(DualRate~DateGroup, FUN=mean))

# some site/date combinations have zero unique positives because they had < 10 runs... merge this to the runs.roll to check the run count
site.bugs.roll <- merge(site.bugs.roll, runs.roll, by=c('DateGroup','CustomerSiteId'))
site.bugs.roll[site.bugs.roll$Runs < threshold, 'UniquePositives'] <- NA
sites.bugs.roll <- site.bugs.roll[,c('DateGroup','CustomerSiteId','UniquePositives')]
bug.count.avg <- with(sites.bugs.roll, aggregate(UniquePositives~DateGroup, FUN=mean))

# combine the data & make a chart
rolled.df <- merge(duals.rate.avg, bug.count.avg, by='DateGroup')
p.dual.count.combo <- ggplot(subset(rolled.df, as.character(DateGroup) >= '2013-26'), aes(x=DateGroup, y=DualRate*100, fill='RateOfDualDetections')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=UniquePositives, color='CountUniqueOrganisms', group='CountUniqueOrganisms'), data=subset(rolled.df, as.character(DateGroup) >= '2014-01'), lwd=1.5) + scale_fill_manual(values='darkgrey', name='') + scale_color_manual(values='blue', name='') + scale_x_discrete(breaks=as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2014-01')$DateGroup))[order(as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2014-01')$DateGroup)))][seq(1, length(as.character(unique(subset(rolled.df, as.character(DateGroup) >= '2014-01')$DateGroup))),8)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=20, color='black'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + labs(title='Count of Unique Organisms Detected vs. Rate of Dual Detections\n(Averaged Across All Participating Trend Sites)', y='Count Unique Positives, % Dual Detections', x='Date')
with(subset(rolled.df, as.character(DateGroup) >= '2013-26'), cor(UniquePositives, DualRate))
with(subset(rolled.df, as.character(DateGroup) >= '2013-26'), plot(UniquePositives, DualRate))
abline(lm(DualRate~UniquePositives, data=subset(rolled.df, as.character(DateGroup) >= '2013-26')), col='blue', lwd=1.5)

# -----------------------------------------------------------------------------------------------------------
# Create a metric that's like a risk index for susceptibility... for example, if there's 3 organisms circulating
# and one is much more prevalent than the others, this should lower the risk index from 3 to something else
# -----------------------------------------------------------------------------------------------------------
# first, create a data frame that has all the bugs by site with placeholders of 0 if there are no positive tests
bugs <- as.character(unique(bugs.fill$ShortName))
sites <- as.character(unique(bugs.fill$CustomerSiteId))
missing.entries <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(CustomerSiteId=sites[x], ShortName = length(bugs[!(bugs %in% unique(as.character(bugs.fill[bugs.fill$CustomerSiteId == sites[x], 'ShortName'])))]))))
missing.sites <- as.character(missing.entries[missing.entries$ShortName > 0, 'CustomerSiteId'])
bugs.sparse.add <- do.call(rbind, lapply(1:length(missing.sites), function(x) data.frame(CustomerSiteId=missing.sites[x], ShortName = bugs[!(bugs %in% unique(as.character(bugs.fill[bugs.fill$CustomerSiteId == missing.sites[x], 'ShortName'])))], Positives = 0)))
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

# find the risk ratio... this is essentially the percent detection of a given organism in the 5-week centered period 
# multiplied by the total percent detection of all other organisms in the same period. The idea behind this is that
# it will indicate the risk of a person who is positive for a single organism to contract any other organism when they
# visit the hospital.
periods <- as.character(unique(percent.det.site$DateGroup))[as.character(unique(percent.det.site$DateGroup)) >= '2013-26']
riskratio.site <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(periods), function(y) do.call(rbind, lapply(1:length(bugs), function(z) data.frame(DateGroup = periods[y], CustomerSiteId = sites[x], ShortName = bugs[z], RiskRatio = percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName==bugs[z],'PercentDetection']*sum(percent.det.site[percent.det.site$CustomerSiteId==sites[x] & percent.det.site$DateGroup==periods[y] & percent.det.site$ShortName!=bugs[z],'PercentDetection']))))))))
# see if this will work to filter out weeks where there are very few runs
riskratio.site.mrg <- merge(runs.roll, riskratio.site, by=c('DateGroup','CustomerSiteId'))
riskratio.site.mrg[riskratio.site.mrg$Runs < threshold, 'RiskRatio'] <- NA
# aggregate the risk ratios over all sites and take the mean for each period with 'enough' data and then add in the unique
# organism count as well as the % dual detections
riskratio.avg <- with(riskratio.site.mrg, aggregate(RiskRatio~DateGroup+ShortName, FUN=mean))
risk.count.duals <- merge(merge(bug.count.avg, duals.rate.avg, by='DateGroup'), riskratio.avg, by='DateGroup')
risk.count.duals.trim <- subset(risk.count.duals, as.character(DateGroup) >= '2013-26')
# the correlation between the risk ratio and the count of unique organsims circulating is quite good
cor(with(risk.count.duals.trim, aggregate(UniquePositives~DateGroup, FUN=mean))$UniquePositives, with(risk.count.duals.trim, aggregate(RiskRatio~DateGroup, FUN=sum))$RiskRatio)
ggplot(risk.count.duals.trim[with(risk.count.duals.trim, order(DateGroup, ShortName, decreasing=TRUE)), ], aes(x=DateGroup)) + geom_area(aes(y=RiskRatio*25, fill=ShortName, order=ShortName, group=ShortName), stat='identity', position='stack') + geom_line(aes(x=DateGroup, y=DualRate*100, group='Dual Detections/Total Tests', color='Dual Detections/Total Tests'), size=2, data=risk.count.duals.trim) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Detections in Period', color='Unique Detections in Period'), size=2, data=risk.count.duals.trim) + scale_fill_manual(values=createPaletteOfVariableLength(risk.count.duals.trim, 'ShortName'), name='') + scale_color_manual(values=c('black','blue'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = as.character(unique(risk.count.duals.trim$DateGroup))[order(as.character(unique(risk.count.duals.trim$DateGroup)))][seq(1, length(as.character(unique(risk.count.duals.trim$DateGroup))), 8)]) + labs(title='Dual Detections, Unique Organism Detection Count, and Risk Ratio\n(5-week centered rolling average)', x='Year-Week', y='% Dual Detections, Organism Count, Risk Ratio')

risk.count.duals.agg <- merge(with(risk.count.duals.trim, aggregate(UniquePositives~DateGroup, FUN=mean)), with(risk.count.duals.trim, aggregate(RiskRatio~DateGroup, FUN=sum)), by='DateGroup')
risk.count.duals.agg <- merge(risk.count.duals.agg, with(risk.count.duals.trim, aggregate(DualRate~DateGroup, FUN=mean)), by='DateGroup')
ggplot(risk.count.duals.agg, aes(x=DateGroup, y=100*DualRate, fill='Dual Detections/Total Runs')) + geom_bar(stat='identity') + geom_line(aes(x=DateGroup, y=25*RiskRatio, group='Aggregate Risk Ratio', color='Aggregate Risk Ratio'), data=risk.count.duals.agg, size=2) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=risk.count.duals.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=12, color='black'), axis.title.y=element_text(size=12), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), legend.position='bottom') + scale_x_discrete(breaks = as.character(unique(risk.count.duals.trim$DateGroup))[order(as.character(unique(risk.count.duals.trim$DateGroup)))][seq(1, length(as.character(unique(risk.count.duals.trim$DateGroup))), 8)]) + scale_color_manual(values=c('blue','black'), name='') + scale_fill_manual(values='grey', name='')

# dual axes for ILI overlay plots
hinvert_title_grob <- function(grob){
  
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

# p1 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='Percent Detection', fill='Percent Detection')) + geom_bar(stat='identity') + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Name=c('Percent Detection')), 'Name'), name='') + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5)  + geom_line(aes(x=YearWeek, y=10*Rate, group='CDC ILI Rate', color='CDC ILI Rate'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue'), name='') + scale_y_continuous(label=percent, breaks=c(0,.07,0.14,0.21,0.28,0.35)) + expand_limits(y=0.35) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(title='FilmArray Percent Detection of Influenza Overlaid with CDC Data', y='Percent Detection, CDC Flu Prevalence', x='Date')
# p2 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + scale_y_continuous(limits=c(0,0.35), breaks=c(0, 0.07, 0.14, 0.21, 0.28, 0.35), labels=c('0.0%','0.7%','1.4%','2.1%','2.8%','3.5%')) + theme(text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='CDC Reported ILI Rate')
p1 <- ggplot(risk.count.duals.agg, aes(x=DateGroup, y=100*DualRate, group='Co-Detections Rate', color='Co-Detections Rate')) + geom_line(size=2) + geom_line(aes(x=DateGroup, y=25*RiskRatio, group='Aggregate Risk Ratio', color='Aggregate Risk Ratio'), data=risk.count.duals.agg, size=2) + geom_line(aes(x=DateGroup, y=UniquePositives, group='Unique Positive Organisms', color='Unique Positive Organisms'), data=risk.count.duals.agg, size=2) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='transparent', fill='white'), legend.position='bottom', panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_color_manual(values=c('red','blue','black'), name='') + scale_fill_manual(values='grey', name='') + labs(y='Co-Detection (%), Unique Positives', x='Date')
p2 <- ggplot(risk.count.duals.agg, aes(x=DateGroup, y=25*RiskRatio, group=1)) + geom_line() + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','10','20','30','40','50')) + theme(panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), text=element_text(size=20, face='bold'), axis.text=element_text(size=16, color='black'), axis.title.y=element_text(size=16), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) + labs(y='People at Risk per 100 Patients')

# Get the ggplot grobs
g1 <- ggplotGrob(p1)
g2 <- ggplotGrob(p2)

# Get the location of the plot panel in g1.
# These are used later when transformed elements of g2 are put back into g1
pp <- c(subset(g1$layout, name == "panel", se = t:r))

# Overlap panel for second plot on that of the first plot
g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

# Get the y axis title from g2
index <- which(g2$layout$name == "ylab") # Which grob contains the y axis title?
ylab <- g2$grobs[[index]]                # Extract that grob
ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications

# Put the transformed label on the right side of g1
g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")

# Get the y axis from g2 (axis line, tick marks, and tick mark labels)
index <- which(g2$layout$name == "axis-l")  # Which grob
yaxis <- g2$grobs[[index]]                  # Extract the grob

# yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
# The relevant grobs are contained in axis$children:
#   axis$children[[1]] contains the axis line;
#   axis$children[[2]] contains the tick marks and tick mark labels.

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