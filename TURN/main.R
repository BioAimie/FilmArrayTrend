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

# # read in data from Excel files
# cdc.reg.df <- read.csv('../DataSources/RegionalILI.csv', header=TRUE, sep=',')

# make an epi calendar
calendar.df <- transformToEpiWeeks(createCalendarLikeMicrosoft(2012, 'Week'))
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
calendar.df <- calendar.df[calendar.df$YearWeek > '2012-51', ]
calendar.df$Days <- 1

# now rerun the code that is in the Publication_Update main.R file to see what the data looks like now
runs.reg <- merge(merge(runs.df, names.df[,c('CustomerSiteId','State')], by='CustomerSiteId'), data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal), by.x='State', by.y='Province')
runs.reg.date <- merge(runs.reg, calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')

# # clean up the data frame that holds the cdc regional data
# cdc.reg.df <- data.frame(Year = cdc.reg.df$YEAR, Week = cdc.reg.df$WEEK, Region = cdc.reg.df$REGION, ILITotal = cdc.reg.df$ILITOTAL, TotalPatients = cdc.reg.df$TOTAL.PATIENTS)
# cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
# # roll the total patient count and the total ILI count such that it is a centered 3-week rolling sum
# cdc.reg.count.df <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], TotalPatients = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])), ILITotal = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])))))

# ------------------------------------------ ANALYSIS -----------------------------------------------------------------------------
b <- c()
for(i in 1:length(unique(runs.reg.date$CustomerSiteId))) {
  
  a <- turn(runs.reg.date, 'CustomerSiteId', as.character(unique(runs.reg.date$CustomerSiteId)[i]), calendar.df)
  a$Spline <- smooth.spline(x=seq(1, length(a$YearWeek), 1), y=a$Runs, cv = FALSE)$y
  a[a$Spline<0, 'Spline'] <- 0
  b <- rbind(b, a)
}





var <- 'CustomerSiteId'
sites <- unique(runs.reg.date$CustomerSiteId)

runs.reg.norm <- c()
for(i in 1:length(sites)) {
  
  site.norm <- normalizeBurnRate(runs.reg.date, var, sites[i])
  runs.reg.norm <- rbind(runs.reg.norm, site.norm)
}

site.starts <- do.call(rbind, lapply(1:length(unique(runs.reg.norm$CustomerSiteId)), function(x) data.frame(CustomerSiteId = unique(runs.reg.norm$CustomerSiteId)[x], StartDate = min(runs.reg.norm[runs.reg.norm$CustomerSiteId==unique(runs.reg.norm$CustomerSiteId)[x],'YearWeek']))))

# using only sites with enough data, find the prevalence of organisms  
# sites <- site.starts[!(is.na(site.starts$StartDate)) & substring(site.starts$StartDate, 1, 4) < 2016, 'CustomerSiteId']
sites <- as.character(unique(runs.reg.norm$CustomerSiteId))
sites <- sites[order(sites)]
bugs.reg <- c()
for(i in 1:length(sites)) {
  
  site <- sites[i]
  temp <- runs.reg.date[runs.reg.date$CustomerSiteId == site, ]
  bugs.site <- merge(temp, bugs.df, by='RunDataId')
  bugs.reg <- rbind(bugs.reg, bugs.site)
}

# make a combined category so that do.call can be used to fill in empty dates
colsToCat <- c('Region','Name','CustomerSiteId','BugPositive')
bugs.reg.trim <- bugs.reg[,c('YearWeek', colsToCat)]
bugs.reg.trim$Record <- 1
bugs <- as.character(unique(bugs.reg.trim$BugPositive))[order(as.character(unique(bugs.reg.trim$BugPositive)))]
site.missing.bugs <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(CustomerSiteId = sites[x], BugPositive = ifelse(length(bugs[!(bugs %in% unique(as.character(bugs.reg.trim[bugs.reg.trim$CustomerSiteId==sites[x], 'BugPositive'])))]) > 0, bugs[!(bugs %in% unique(as.character(bugs.reg.trim[bugs.reg.trim$CustomerSiteId==sites[x], 'BugPositive'])))], NA))))
site.missing.bugs <- site.missing.bugs[!(is.na(site.missing.bugs$BugPositive)), ]
bugs.reg.trim <- rbind(bugs.reg.trim, data.frame(YearWeek = min(bugs.reg.trim$YearWeek), merge(site.missing.bugs, unique(bugs.reg.trim[,c('CustomerSiteId','Region','Name')]), by='CustomerSiteId')[,c('Region','Name','CustomerSiteId','BugPositive')], Record = 0))
bugs.reg.trim$combocat <- do.call(paste, c(bugs.reg.trim[,colsToCat], sep=','))
bugs.reg.combo <- do.call(rbind, lapply(1:length(unique(bugs.reg.trim$combocat)), function(x) cbind(merge(unique(calendar.df[,c('YearWeek','Year')]), bugs.reg.trim[bugs.reg.trim$combocat == unique(bugs.reg.trim$combocat)[x], c('YearWeek','Record')], by='YearWeek', all.x=TRUE), ComboCat = unique(bugs.reg.trim$combocat)[x])))
deCombo <- as.data.frame(sapply(1:length(colsToCat), function(x) do.call(rbind, strsplit(as.character(bugs.reg.combo$ComboCat), split=','))[,x]))
colnames(deCombo) <- colsToCat
bugs.reg.fill <- cbind(bugs.reg.combo[,c('YearWeek','Record')], deCombo)
bugs.reg.fill[is.na(bugs.reg.fill$Record),'Record'] <- 0
bugs.reg.agg <- with(bugs.reg.fill, aggregate(Record~YearWeek+Region+Name+CustomerSiteId+BugPositive, FUN=sum))
bugs.reg.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(bugs), function(y) data.frame(YearWeek = bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x] & bugs.reg.agg$BugPositive==bugs[y], 'YearWeek'][2:(length(bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x] & bugs.reg.agg$BugPositive==bugs[y], 'YearWeek'])-1)], CustomerSiteId = sites[x], Region = unique(bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x],'Region']), Name = unique(bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x],'Name']), Bug = bugs[y], Code = letters[y], Positives = sapply(2:(length(bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x] & bugs.reg.agg$BugPositive==bugs[y], 'YearWeek'])-1), function(z) sum(bugs.reg.agg[bugs.reg.agg$CustomerSiteId==sites[x] & bugs.reg.agg$BugPositive==bugs[y],'Record'][(z-1):(z+1)])))))))
runs.reg.roll <- runs.reg.norm[,c('YearWeek','CustomerSiteId','RollRuns')]
colnames(runs.reg.roll) <- c('YearWeek','CustomerSiteId','Runs')


# PRINT OUT ALL THE FIGURES
plots <- ls()[grep('^p\\.',ls())]
for(i in 1:length(plots)) {
  
  imgName <- paste(substring(plots[i],3),'.png',sep='')
  png(file=paste('Figures', imgName, sep='/'), width=1200, height=800, units='px')
  print(eval(parse(text = plots[i])))
  dev.off()
}