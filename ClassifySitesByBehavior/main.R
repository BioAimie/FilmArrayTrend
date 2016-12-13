# THE PURPOSE OF THIS ANALYSIS IS TO DETERMINE A WAY TO GROUP SITES BY BEHAVIORAL PATTERNS... THIS WILL BE USEFUL, BECAUSE WHEN WE TRY TO BUILD 
# ALGORITHMS OR ANALYSES, IT WILL BE HELPFUL TO CHANGE THE TREATMENT OF SITES BASED ON THEIR BEHAVIOR. WE MAY ALSO BE ABLE TO CONFIRM THE 
# BEHAVIORS THAT MAKE SITES SIMILAR SO WE CAN SHOW THINGS LIKE THE DIFFERENCES IN POSITIVITY WHEN TESTING ALGORITMS ARE USED DURING RP SEASON, ETC.

# prior to the analysis ---------------------------------------------------------------------------------------------
# set the working environment up
setwd('~/FilmArrayTrend/ClassifySitesByBehavior/')
library(RODBC)
library(lubridate)
library(EpiWeek)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(gtable)
library(RColorBrewer)
library(devtools)
library(RCurl)
library(binom)
library(caret)
library(randomForest)
require(dateManip)

# load custom functions
source('../Rfunctions/normalizeBurnRate.R')
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')

# load the data
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/ClassifySitesByBehavior/RunDataBySite.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/ClassifySitesByBehavior/TargetDataByRun.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
bugs.df <- sqlQuery(FADWcxn,query)
bugs.df <- bugs.df[bugs.df$Target != 'Bocavirus',]
queryVector <- scan('../DataSources/ShortNames.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
shortnames.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in data from PMS PROD server
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- scan('../DataSources/AllSitesRegionKey.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
regions.df <- sqlQuery(PMScxn,query)
odbcClose(PMScxn)

# start the analysis ---------------------------------------------------------------------------------------------
# make an epi calendar
calendar.df <- transformToEpiWeeks(createCalendarLikeMicrosoft(2012, 'Week'))
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
calendar.df <- calendar.df[calendar.df$YearWeek > '2012-51', ]
calendar.df$Days <- 1

# give the run data table some information about which region each site is in as this geographic classifier may be important
runs.reg <- merge(runs.df, data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegion), by='Province')
runs.reg$Runs <- 1
runs.reg.date <- merge(runs.reg[,c('RunDataId','Instrument','Date','Name','CustomerSiteId','Region','Runs')], calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')

# give the bug data table some information about date etc...
bugs.reg.date <- merge(runs.reg.date[,c('RunDataId','Date','Year','Week','CustomerSiteId')], subset(bugs.df, ResultType != 'Control'), by='RunDataId')
controls.reg.date <- merge(runs.reg.date[,c('RunDataId','Date','Year','Week','CustomerSiteId')], subset(bugs.df, ResultType == 'Control'), by='RunDataId')

# create some summary data: run count, bug positive counts, unique organisms in the period
runs.reg.count <- with(runs.reg.date, aggregate(Runs~Year+Week+CustomerSiteId+Region, FUN=sum))
bugs.reg.count <- with(data.frame(bugs.reg.date, Positives = 1), aggregate(Positives~Year+Week+CustomerSiteId+Target+ResultType, FUN=sum))
bugs.reg.unique <- data.frame(unique(bugs.reg.count[bugs.reg.count$ResultType=='Organism', c('Year','Week','CustomerSiteId','Target')]), Organisms = 1)


