workDir <-'~/FilmArrayTrend/TURN/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(gtable)
library(RColorBrewer)
library(devtools)
library(dplyr)
library(tidyr)
require(dateManip)

# load custom functions
source('../Rfunctions/TURN.R')
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')

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

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- readLines('../DataSources/SQL/TURN/RunDataBySite.sql')
query <- paste(queryVector,collapse="\n")
runs.df <- sqlQuery(FADWcxn,query)
queryVector <- readLines('../DataSources/CustomerSiteIdsWithNames.sql')
query <- paste(queryVector,collapse="\n")
names.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in data from PMS PROD server
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- scan('../DataSources/AllSitesRegionKey.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
regions.df <- sqlQuery(PMScxn,query)
odbcClose(PMScxn)

# read in data from Excel files
cdc.reg.df <- read.csv('../DataSources/RegionalILI.csv', header=TRUE, sep=',')

# make an epi calendar
calendar.df <- transformToEpiWeeks(createCalendarLikeMicrosoft(2012, 'Week'))
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
calendar.df <- calendar.df[calendar.df$YearWeek > '2012-51', ]
calendar.df$Days <- 1

# now rerun the code that is in the Publication_Update main.R file to see what the data looks like now
runs.reg <- merge(merge(runs.df, names.df[,c('CustomerSiteId','State')], by='CustomerSiteId'), data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal), by.x='State', by.y='Province')
runs.reg.date <- merge(runs.reg, calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')

# clean up the data frame that holds the cdc regional data
cdc.reg.df <- data.frame(Year = cdc.reg.df$YEAR, Week = cdc.reg.df$WEEK, Region = cdc.reg.df$REGION, ILITotal = cdc.reg.df$ILITOTAL, TotalPatients = cdc.reg.df$TOTAL.PATIENTS)
cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
cdc.reg.count.df <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], TotalPatients = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])), ILITotal = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])))))
cdc.reg.count.df$Rate <- with(cdc.reg.count.df, ILITotal/TotalPatients)
cdc.trend.rate <- merge(unique(runs.reg.date[,c('CustomerSiteId','Region')]), cdc.reg.count.df, by='Region')
cdc.trend.rate.nat <- with(cdc.trend.rate, aggregate(Rate~YearWeek, FUN=mean))

# ------------------------------------------ ANALYSIS -----------------------------------------------------------------------------
ggplot(runs.reg.date, aes(x=YearWeek, y=Run, fill=Panel)) + geom_bar(stat='identity') + facet_wrap(~CustomerSiteId, scale='free_y')

sites <- as.character(unique(runs.reg.date$CustomerSiteId))

# these vars were created using the 52-week version of the function without if clauses to handle RunsR2 = 1 or replacing NaNs in the fits
site.turn <- do.call(rbind, lapply(1:length(sites), function(x) turn(runs.reg.date, 'CustomerSiteId', sites[x], 'RP', calendar.df, 30)))
site.turn.adj <- site.turn
site.turn.adj[!(is.na(site.turn.adj$TURN)) & site.turn.adj$TURN==0.0, 'TURN'] <- NA
natn.turn <- with(site.turn, aggregate(TURN~YearWeek, FUN=mean))
natn.turn.adj <- with(site.turn.adj, aggregate(TURN~YearWeek, FUN=mean))
ggplot(site.turn.adj, aes(x=YearWeek, y=TURN, group='SiteTURN', color='SiteTURN')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN'), data=natn.turn.adj, size=1.5) + scale_color_manual(values=c('black','blue')) + facet_wrap(~CustomerSiteId)
ggplot(natn.turn, aes(x=YearWeek, y=TURN, group='NationalTURN')) + geom_line() + scale_x_discrete(breaks=natn.turn[seq(1, length(natn.turn$YearWeek), 12), 'YearWeek'])

# a is with the TURN.R above altered to be 26 weeks rather than 52 (by site) and b is the average
# try the overlay with TURN by site and the national average to figure out what's going on
a[!(is.na(a$TURN)) & a$TURN == 0.0, 'TURN'] <- NA
b <- with(a, aggregate(TURN~YearWeek, FUN=mean))
ggplot(a, aes(x=YearWeek, y=TURN, group='SiteTURN', color='SiteTURN')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN'), data=b, size=1.5) + scale_color_manual(values=c('black','blue')) + facet_wrap(~CustomerSiteId)
ggplot(b, aes(x=YearWeek, y=TURN, group='abvNationalTURN', color='abvNationalTURN')) + geom_line(size=1.5) + geom_line(data=natn.turn, size=1.5, aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN')) + geom_line(data=cdc.trend.rate.nat[as.character(cdc.trend.rate.nat$YearWeek) > '2013-25', ], aes(x=YearWeek, y=70*Rate, group='ILI',color='ILI'), size=1.5) + scale_x_discrete(breaks=b[seq(1, length(b$YearWeek), 12), 'YearWeek']) + scale_color_manual(values=c('red','black','blue'))

# d is the TURN.R altered to be 26 weeks for the first half year and then extended to 52 weeks later when possible
# also, NaN values in the fits have been adjusted to NA and if RunsR2 = 1 that's handled separately
# try the same as above
d <- do.call(rbind, lapply(1:length(sites), function(x) turn(runs.reg.date, 'CustomerSiteId', sites[x], 'RP', calendar.df, 30)))
f <- with(d, aggregate(TURN~YearWeek, FUN=mean))
ggplot(d, aes(x=YearWeek, y=TURN, group='SiteTURN', color='SiteTURN')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN'), data=f, size=1.5) + scale_color_manual(values=c('black','blue')) + facet_wrap(~CustomerSiteId)
ggplot(d, aes(x=YearWeek, y=TURN, group='abvNationalTURN', color='abvNationalTURN')) + geom_line(size=1.5) + geom_line(data=f, size=1.5, aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN')) + geom_line(data=cdc.trend.rate.nat[as.character(cdc.trend.rate.nat$YearWeek) > '2013-25', ], aes(x=YearWeek, y=70*Rate, group='ILI',color='ILI'), size=1.5) + scale_x_discrete(breaks=d[seq(1, length(d$YearWeek), 12), 'YearWeek']) + scale_color_manual(values=c('red','black','blue'))

# compare the models:
ggplot(f, aes(x=YearWeek, y=TURN, group='v3', color='v3')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='v1', color='v1'), data=natn.turn.adj, size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='v2', color='v2'), data=b, size=1.5) + scale_color_manual(values = c('black','blue','red'))
 # note - v3 is under v1 for the section where you can't see it at all

f <- data.frame(f, Splined = smooth.spline(x=seq(1, length(f$YearWeek), 1), y=f$TURN, cv=FALSE)$y)
ggplot(f, aes(x=YearWeek, y=Splined, group='v3', color='v3')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='v1', color='v1'), data=natn.turn.adj, size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='v2', color='v2'), data=b, size=1.5) + scale_color_manual(values = c('black','blue','red'))


# PRINT OUT ALL THE FIGURES
plots <- ls()[grep('^p\\.',ls())]
for(i in 1:length(plots)) {
  
  imgName <- paste(substring(plots[i],3),'.png',sep='')
  png(file=paste('Figures', imgName, sep='/'), width=1200, height=800, units='px')
  print(eval(parse(text = plots[i])))
  dev.off()
}