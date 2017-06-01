workDir <-'~/FilmArrayTrend/TURN/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(xlsx)
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
source('../Rfunctions/TURN_v3.R')
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
cdc.reg.df <- read.csv('../DataSources/Epidemics/CensusRegionalILI.csv', header=TRUE, sep=',')
cdc.hhs.df <- read.csv('../DataSources/Epidemics/HHSregionalILI.csv', header=TRUE, sep=',')
#### READ IN ADDITIONAL STUFF!!!!


goog.flu <- read.xlsx('../DataSources/Epidemics/GoogleFlu.xlsx', sheetName='Sheet1', colIndex = c(1,2,3))

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
natn.turn <- with(site.turn, aggregate(TURN~YearWeek, FUN=mean))
ggplot(site.turn, aes(x=YearWeek, y=TURN, group='SiteTURN', color='SiteTURN')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN'), data=natn.turn, size=1.5) + scale_color_manual(values=c('black','blue')) + facet_wrap(~CustomerSiteId, scale='free_y') + labs(title='TURN_v1')

# make an aggregate count of negative runs by period, customer, and region
neg.runs <- with(runs.reg.date[runs.reg.date$Panel=='RP' & runs.reg.date$Positive==0, ], aggregate(Run~YearWeek+CustomerSiteId, FUN=sum))
base.df <- expand.grid(YearWeek = unique(neg.runs$YearWeek), CustomerSiteId = unique(neg.runs$CustomerSiteId))
neg.runs <- merge(base.df, neg.runs, by=c('YearWeek','CustomerSiteId'), all.x=TRUE)
neg.runs[is.na(neg.runs$Run),'Run'] <- 0
neg.runs <- neg.runs[with(neg.runs, order(CustomerSiteId, YearWeek)), ]
year.weeks <- as.character(unique(base.df$YearWeek))[order(as.character(unique(base.df$YearWeek)))]
neg.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(2:(length(year.weeks)), function(y) data.frame(CustomerSiteId = sites[x], YearWeek = year.weeks[y], Negatives = sum(neg.runs[neg.runs$CustomerSiteId==sites[x],'Run'][(y-1):(y+1)]))))))
neg.roll <- merge(neg.roll, names.df[,c('CustomerSiteId','CensusRegionNational','CensusRegionLocal')], by='CustomerSiteId')
site.overview <- merge(site.turn[,c('CustomerSiteId','YearWeek','SpecRuns','SpecPositives','TURN')], neg.roll, by=c('CustomerSiteId','YearWeek'))
colnames(site.overview)[3:4] <- c('Runs','Positives')
site.overview$PositiveRate <- with(site.overview, Positives/Runs)
site.overview$NegativeRate <- with(site.overview, Negatives/Runs)
# Rotavirus isn't RP, so get rid of this...
# # check for Rotavirus at children's hospitals
# peds.sites <- unique(c(names.df[grep('Child', names.df$Name), 'CustomerSiteId'], names.df[grep('Child', names.df$Note), 'CustomerSiteId']))
# peds.overview <- site.overview[site.overview$CustomerSiteId %in% peds.sites, ]
# rota.nat.nrevss <- read.csv('../DataSources/NREVSS/Rotavirus_National.csv', header=TRUE)
# rota.nat.nrevss$Date <- as.Date(as.character(rota.nat.nrevss$Date), format = '%m/%d/%Y')
# rota.nat.nrevss <- merge(rota.nat.nrevss, calendar.df, by='Date') 
# ggplot(with(subset(peds.overview, as.character(YearWeek) >= '2014-01'), aggregate(NegativeRate~YearWeek, FUN=mean)), aes(x=YearWeek, y=NegativeRate)) + geom_point() + scale_x_discrete(breaks = unique(peds.overview$YearWeek)[seq(1, length(unique(peds.overview$YearWeek)), 12)])

# add in Google Flu

