# Set up environment, get and format data
if(TRUE) {

  workDir <-'~/FilmArrayTrend/InitialPublication/'
  setwd(workDir)
  
  # load libraries
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
  library(rpart)
  library(party)
  library(partykit)
  library(randomForest)
  library(dplyr)
  library(tidyr)
  require(dateManip)
  
  # load custom functions
  source('../Rfunctions/normalizeBurnRate.R')
  source('../Rfunctions/generateCombosBetter.R')
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
  queryVector <- scan('../DataSources/AllSitesRespiratoryTrendableRuns.txt', what=character(), quote="")
  query <- paste(queryVector,collapse=" ")
  runs.df <- sqlQuery(FADWcxn,query)
  queryVector <- scan('../DataSources/PositiveBugsRP.txt',what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  bugs.df <- sqlQuery(FADWcxn,query)
  bugs.df <- bugs.df[bugs.df$BugPositive != 'Bocavirus',]
  queryVector <- scan('../DataSources/ShortNames.txt',what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  shortnames.df <- sqlQuery(FADWcxn,query)
  queryVector <- scan('../DataSources/NationalDataILI.txt',what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  cdc.nat.df <- sqlQuery(FADWcxn,query)
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
  runs.reg <- merge(runs.df, data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal), by='Province')
  runs.reg$Record <- 1
  runs.reg.date <- merge(runs.reg[,c('RunDataId','Instrument','Date','Name','CustomerSiteId','Region','Record')], calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')
  
  # create a data frame that will show site name and id with its region and census region
  sitesByCensusRegions.etc <- unique(merge(unique(runs.reg.date[,c('CustomerSiteId','Name','Region')]), regions.df, by.x='Region', by.y='CensusRegionLocal')[,c('CustomerSiteId','Name','Region','CensusRegionNational')])
  
  # clean up the data frame that holds the cdc regional data
  cdc.reg.df <- data.frame(Year = cdc.reg.df$YEAR, Week = cdc.reg.df$WEEK, Region = cdc.reg.df$REGION, ILITotal = cdc.reg.df$ILITOTAL, TotalPatients = cdc.reg.df$TOTAL.PATIENTS)
  cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
  # roll the total patient count and the total ILI count such that it is a centered 3-week rolling sum
  cdc.reg.count.df <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], TotalPatients = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])), ILITotal = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])))))
}

# ------------------------------------------ MAKE FIGURES -----------------------------------------------------------------------------
# TIME SERIES OF PREVALENCE
if(TRUE) {
  
  # find out which sites should be used calling the normalizeBurnRate function (which looks for gaps in the site data) and then using the output
  # to determine when the start date is of 'good' data contribution for each site
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
  
  # get the 3-week centered moving sum of bug positives and runs
  positives.count.all <- merge(runs.reg.roll, bugs.reg.roll, by=c('YearWeek','CustomerSiteId'))
  decoder <- data.frame(Bug = bugs, Code = letters[1:length(bugs)])
  prevalence.reg.agg <- merge(subset(positives.count.all, as.character(YearWeek) >= '2013-25'), cdc.reg.count.df, by=c('YearWeek','Region'), all.x=TRUE)
  prevalence.reg.agg$Rate <- with(prevalence.reg.agg, ILITotal/TotalPatients)
  prevalence.reg.agg$Prevalence <- with(prevalence.reg.agg, Positives/Runs)
  # DO I NEED TO ADD SOMETHING TO TAKE OUT WEEKS WHERE THERE ARE LESS THAN 10 ROLLING RUNS????
  prevalence.reg.agg[prevalence.reg.agg$Runs < 30, 'Prevalence'] <- NA
  # DO I NEED TO ADD SOMETHING TO TAKE OUT WEEKS WHERE THERE ARE LESS THAN 10 ROLLING RUNS????
  prevalence.reg.wrap <- merge(prevalence.reg.agg[,c('Bug','Code','YearWeek','CustomerSiteId','Rate','Prevalence')], shortnames.df, by.x='Bug', by.y='Organism')
  prevalence.nat.individual.wrap <- with(prevalence.reg.wrap, aggregate(cbind(Rate, Prevalence)~YearWeek+Bug+Code+ShortName, FUN=mean, na.action=na.omit))
  
  # now make the figures and break out by organisms and type
  bacterias <- as.character(decoder[decoder$Code %in% c('b','c','p'), 'Bug'])
  rhino <- 'Human Rhinovirus/Enterovirus'
  fluAs <- as.character(decoder[grep('Influenza A', decoder$Bug),'Bug'])
  fluBs <- as.character(decoder[grep('Influenza B', decoder$Bug),'Bug'])
  rsv <- 'Respiratory Syncytial Virus'
  flus <- as.character(decoder[grep('Influenza', decoder$Bug),'Bug'])
  pivs <- as.character(decoder[grep('Parainfluenza', decoder$Bug),'Bug'])
  corona <- as.character(decoder[grep('Corona', decoder$Bug),'Bug'])
  adeno <- 'Adenovirus'
  hmp <- 'Human Metapneumovirus'
  
  # create beautiful time-series prevalence chart
  bug.individual.Pal <- createPaletteOfVariableLength(prevalence.nat.individual.wrap, 'ShortName')
  # dateBreaks <- as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]
  dateBreaks <- c('2013-27','2013-40','2014-01', '2014-14','2014-27','2014-40','2015-01', '2015-14','2015-27','2015-40','2016-01','2016-14','2016-27','2016-40','2017-01')
  dateLabels <- c('Jul-2013','-','Jan-2014','-','Jul-2014','-','Jan-2015','-','Jul-2015','-','Jan-2016','-','Jul-2016','-','Jan-2017')
  
  p.PercentDetectionTrend <- ggplot(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')
  
  # Reorder the bugs
  a <- prevalence.nat.individual.wrap
  # a$ShortName <- as.character(a$ShortName)
  a$Name <- factor(a$ShortName, levels=c('RSV','PIV4','PIV3','PIV2','PIV1','hMPV','FluB','FluA H3','FluA H1-09','FluA H1','Flu A','CoV OC43','CoV NL63','CoV HKU1','CoV 229E','HRV/EV','M. pne','C. pne','B. per','Adeno'))
  p.PercentDetectionTrend_ReorderedPretty <- ggplot(a, aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')
  
  # now do the same thing, but weight by population
  # https://www.census.gov/popclock/data_tables.php?component=growth
  population.dist.2016 <- data.frame(CensusRegionNational = c('Northeast','Midwest','West','South'), PopulationPercent = c(0.174, 0.21, 0.237, 0.379))
  prevalence.reg.census.wrap <- merge(merge(prevalence.reg.agg[,c('Bug','Code','YearWeek','CustomerSiteId','Rate','Runs','Positives')], shortnames.df, by.x='Bug', by.y='Organism'), unique(sitesByCensusRegions.etc[,c('CustomerSiteId','CensusRegionNational')]), by='CustomerSiteId')
  prevalence.reg.census.agg <- with(prevalence.reg.census.wrap, aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId+ShortName, FUN=sum))
  prevalence.reg.census.agg$Prevalence <- with(prevalence.reg.census.agg, Positives/Runs)
  prevalence.reg.census.agg <- with(prevalence.reg.census.agg, aggregate(Prevalence~YearWeek+CensusRegionNational+ShortName, FUN=mean))
  prevalence.nat.census.individual <- merge(prevalence.reg.census.agg, population.dist.2016, by='CensusRegionNational') 
  prevalence.nat.census.individual$WeightedPrevalence <- with(prevalence.nat.census.individual, Prevalence*PopulationPercent)
  prevalence.nat.census.individual <- with(prevalence.nat.census.individual, aggregate(WeightedPrevalence~YearWeek+ShortName, FUN=sum))  
  
  p.PercentDetectionTrend_Weighted <- ggplot(prevalence.nat.census.individual[with(prevalence.nat.census.individual, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=WeightedPrevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')
  
  # create the same chart, but group the organims into families
  positives.count.all <- positives.count.all[with(positives.count.all, order(CustomerSiteId, Code, YearWeek)), ]
  sites <- sites[order(as.numeric(sites))]
  prevalence.reg.count <- data.frame(do.call(rbind, lapply(1:length(sites), function(x) do.call(cbind, lapply(1:length(bugs), function(y) positives.count.all[positives.count.all$CustomerSiteId==sites[x] & positives.count.all$Code==letters[y], 'Positives'])))))
  colnames(prevalence.reg.count) <- letters[1:length(prevalence.reg.count[1,])]
  prevalence.reg.count <- data.frame(unique(positives.count.all[,c('YearWeek','CustomerSiteId','Region','Name','Runs')]), prevalence.reg.count)
  # sum by family
  prevalence.nat.fluA <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Influenza A', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% fluAs,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.fluB <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Influenza B', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% fluBs,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.bacteria <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Bacteria', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% bacterias,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.rhino <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'HRV/EV', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% rhino,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.adeno <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Adeno', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% adeno,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.pivs <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Parainfluenza', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% pivs,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.corona <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'Coronavirus', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% corona,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.hmp <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'hMPV', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% hmp,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.rsv <- with(do.call(rbind, lapply(1:length(unique(prevalence.reg.count$CustomerSiteId)), function(x) data.frame(YearWeek = prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'], CustomerSiteId = unique(prevalence.reg.count$CustomerSiteId)[x], Key = 'RSV', Prevalence = sapply(1:length(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek']), function(y) sum(prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y], as.character(decoder[decoder$Bug %in% rsv,'Code'])])/prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x] & prevalence.reg.count$YearWeek==prevalence.reg.count[prevalence.reg.count$CustomerSiteId==unique(prevalence.reg.count$CustomerSiteId)[x],'YearWeek'][y],'Runs'])))), aggregate(Prevalence~YearWeek+Key, FUN=mean))
  prevalence.nat.families <- rbind(prevalence.nat.fluA, prevalence.nat.fluB, prevalence.nat.bacteria, prevalence.nat.rhino, prevalence.nat.adeno, prevalence.nat.hmp, prevalence.nat.pivs, prevalence.nat.corona, prevalence.nat.rsv)
  bug.family.Pal <- c(bug.individual.Pal[names(bug.individual.Pal) == 'Flu A'], bug.individual.Pal[names(bug.individual.Pal) == 'FluB'], bug.individual.Pal[names(bug.individual.Pal) %in% as.character(unique(prevalence.nat.families$Key))], bug.individual.Pal[names(bug.individual.Pal) == 'CoV NL63'], bug.individual.Pal[names(bug.individual.Pal) == 'PIV3'], bug.individual.Pal[names(bug.individual.Pal) == 'C. pne'])
  names(bug.family.Pal) <- c('Influenza A', 'Influenza B', 'Adeno', 'hMPV', 'HRV/EV', 'RSV', 'Coronavirus', 'Parainfluenza', 'Bacteria')
  prevalence.nat.families$Name <- factor(prevalence.nat.families$Key, levels=unique(prevalence.nat.families[with(prevalence.nat.families, order(as.character(Key))), 'Key']))
  prevalence.nat.families <- prevalence.nat.families[as.character(prevalence.nat.families$YearWeek) >= '2013-26', ]
  
  p.PercentDetectionTrend_Grouped <- ggplot(prevalence.nat.families[with(prevalence.nat.families, order(Name, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=Name, group=Name, order=Name), stat='identity', position='stack') + scale_fill_manual(values=bug.family.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.8), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + labs(title='', y='Detection (%)', x='Date')
  
  # now do the same thing, but weight by population
  prevalence.reg.census.agg.fam <- with(prevalence.reg.census.wrap, aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId+ShortName, FUN=sum))
  prevalence.census.fluA <- with(prevalence.reg.census.agg.fam[grep('Flu A|FluA', prevalence.reg.census.agg.fam$ShortName), ], aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId, FUN=sum))
  prevalence.census.fluA$ShortName <- 'Influenza A'
  prevalence.census.corona <- with(prevalence.reg.census.agg.fam[grep('CoV', prevalence.reg.census.agg.fam$ShortName), ], aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId, FUN=sum))
  prevalence.census.corona$ShortName <- 'Coronavirus'
  prevalence.census.pivs <- with(prevalence.reg.census.agg.fam[grep('PIV', prevalence.reg.census.agg.fam$ShortName), ], aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId, FUN=sum))
  prevalence.census.pivs$ShortName <- 'Parainfluenza'
  prevalence.census.bacteria <- with(prevalence.reg.census.agg.fam[grep('B. per|C. pne|M. pne', prevalence.reg.census.agg.fam$ShortName), ], aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId, FUN=sum))
  prevalence.census.bacteria$ShortName <- 'Bacteria'
  prevalence.census.other <- with(prevalence.reg.census.agg.fam[grep('Adeno|FluB|hMPV|HRV/EV|RSV', prevalence.reg.census.agg.fam$ShortName), ], aggregate(cbind(Runs, Positives)~YearWeek+CensusRegionNational+CustomerSiteId+ShortName, FUN=sum))[,c('YearWeek','CensusRegionNational','CustomerSiteId','Runs','Positives','ShortName')]
  prevalence.census.families <- rbind(prevalence.census.fluA, prevalence.census.corona, prevalence.census.pivs, prevalence.census.bacteria, prevalence.census.other)
  prevalence.census.families$Prevalence <- with(prevalence.census.families, Positives/Runs)
  prevalence.census.families <- with(prevalence.census.families, aggregate(Prevalence~YearWeek+CensusRegionNational+ShortName, FUN=mean))
  
  # now merge with population fraction and weight... then make the chart
  prevalence.census.families <- merge(prevalence.census.families, population.dist.2016, by='CensusRegionNational')
  prevalence.census.families$WeightedPrevalence <- with(prevalence.census.families, Prevalence*PopulationPercent)
  prevalence.census.nat.families <- with(prevalence.census.families, aggregate(WeightedPrevalence~YearWeek+ShortName, FUN=sum))
  prevalence.census.nat.families[grep('FluB', prevalence.census.nat.families$ShortName), 'ShortName'] <- 'Influenza B'
  
  p.PercentDetectionTrend_Grouped_Weighted <- ggplot(prevalence.census.nat.families[with(prevalence.census.nat.families, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=WeightedPrevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.family.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.8), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + labs(title='', y='Detection (%)', x='Date')
}

# PREVALENCE OF ORGANISMS - PARETO-ISH TYPE CHARTS
if(FALSE) {
  
  start.year <- 2013
  
  # use data from all time and show a pareto of prevalence (collapsing fluA, coronas, pivs, and bacterias)
  positives.count.trim <- positives.count.all[as.character(positives.count.all$YearWeek) >= '2013-01', ]
  
  # need to sum up flu As, CoVs, PIVs, and Bacterias by customer site Id... then join these onto the positives.count.trim data frame
  positives.count.fluas <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'v', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% fluAs,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.covs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'w', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% corona,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.pivs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'x', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% pivs,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.bacteria <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'y', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% bacterias,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.agg <- rbind(positives.count.trim[,c('YearWeek','CustomerSiteId','Code','Positives','Runs')], positives.count.fluas, positives.count.covs, positives.count.pivs, positives.count.bacteria)
  decoder.agg <- rbind(decoder, data.frame(Bug='Influenza A (all)', Code='v'), data.frame(Bug='Coronavirus (all)', Code='w'), data.frame(Bug='Parainfluenza (all)', Code='x'), data.frame(Bug='Bacteria (all)', Code='y'))
  positives.count.agg <- merge(positives.count.agg, decoder.agg, by='Code')
  positives.count.agg <- merge(positives.count.agg, shortnames.df, by.x='Bug', by.y='Organism', all.x=TRUE)
  positives.count.agg$ShortName <- as.character(positives.count.agg$ShortName)
  positives.count.agg[as.character(positives.count.agg$Code)=='v', 'ShortName'] <- 'Flu A (all)'
  positives.count.agg[as.character(positives.count.agg$Code)=='w', 'ShortName'] <- 'CoV (all)'
  positives.count.agg[as.character(positives.count.agg$Code)=='x', 'ShortName'] <- 'PIV (all)'
  positives.count.agg[as.character(positives.count.agg$Code)=='y', 'ShortName'] <- 'Bacteria (all)'
  
  # Make various paretos... 
  prev.pareto.all <- merge(positives.count.agg, unique(runs.reg.date[,c('YearWeek','Year')]), by='YearWeek')
  prev.pareto.all <- subset(prev.pareto.all, Year >= start.year)
  
  # start with all data from the 8 sites starting in 2014-present showing all organisms and then grouping by family
  prev.pareto.all.nat <- with(prev.pareto.all, aggregate(cbind(Runs, Positives)~ShortName+Code+CustomerSiteId, FUN=sum))
  prev.pareto.all.nat$Prevalence <- with(prev.pareto.all.nat, Positives/Runs)
  prev.pareto.all.nat <- with(prev.pareto.all.nat, aggregate(Prevalence~ShortName+Code, FUN=mean))
  prev.pareto.all.nat.ind <- prev.pareto.all.nat[!(prev.pareto.all.nat$Code %in% c('v','w','x','y')), ]
  prev.pareto.all.nat.fam <- prev.pareto.all.nat[!(prev.pareto.all.nat$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  label.order.all <- prev.pareto.all.nat[with(prev.pareto.all.nat, order(Prevalence, decreasing = TRUE)), 'ShortName']
  label.order.all <- label.order.all[c(1,2,3,8,17,18,19,4,10,13,16,20,5,9,11,21,24,6,7,12,14,15,22,23)]
  prev.pareto.all.nat$Name <- factor(prev.pareto.all.nat$ShortName, levels = label.order.all)
  label.order.ind <- prev.pareto.all.nat.ind[with(prev.pareto.all.nat.ind, order(Prevalence, decreasing = TRUE)), 'ShortName']
  prev.pareto.all.nat.ind$Name <- factor(prev.pareto.all.nat.ind$ShortName, levels = label.order.ind)
  label.order.fam <- prev.pareto.all.nat.fam[with(prev.pareto.all.nat.fam, order(Prevalence, decreasing = TRUE)), 'ShortName']
  prev.pareto.all.nat.fam$Name <- factor(prev.pareto.all.nat.fam$ShortName, levels = label.order.fam)
  
  p.PercentDetectionPareto <- ggplot(prev.pareto.all.nat, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionPareto_Individual <- ggplot(prev.pareto.all.nat.ind, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionPareto_Family <- ggplot(prev.pareto.all.nat.fam, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')

  # subset the data into children's hospitals and mixed population hospitals
  sites.mixed <- unique(runs.reg.date[!(runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name)[grep('Children', unique(runs.reg.date$Name))])),'CustomerSiteId'])
  sites.child <- unique(runs.reg.date[runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name))[grep('Children', unique(runs.reg.date$Name))],'CustomerSiteId'])
  prev.pareto.all.pop <- prev.pareto.all
  prev.pareto.all.pop$Key <- with(prev.pareto.all.pop, ifelse(CustomerSiteId %in% sites.mixed, 'Mixed', 'Pediatric'))
  
  prev.pareto.all.nat.pop <- with(prev.pareto.all.pop, aggregate(cbind(Runs, Positives)~ShortName+Code+Key+CustomerSiteId, FUN=sum))
  prev.pareto.all.nat.pop$Prevalence <- with(prev.pareto.all.nat.pop, Positives/Runs)
  prev.pareto.all.nat.pop <- with(prev.pareto.all.nat.pop, aggregate(Prevalence~ShortName+Code+Key, FUN=mean))
  prev.pareto.all.nat.pop$Name <- factor(prev.pareto.all.nat.pop$ShortName, levels=label.order.all)
  prev.pareto.all.nat.pop.ind <- prev.pareto.all.nat.pop[!(prev.pareto.all.nat.pop$Code %in% c('v','w','x','y')), ]
  prev.pareto.all.nat.pop.fam <- prev.pareto.all.nat.pop[!(prev.pareto.all.nat.pop$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  prev.pareto.all.nat.pop.ind$Name <- factor(prev.pareto.all.nat.pop.ind$ShortName, levels=label.order.ind)
  prev.pareto.all.nat.pop.fam$Name <- factor(prev.pareto.all.nat.pop.fam$ShortName, levels=label.order.fam)
  
  p.PercentDetectionParetoByPopulation <- ggplot(prev.pareto.all.nat.pop, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.pop, 'Key'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionParetoByPopulation_Individual <- ggplot(prev.pareto.all.nat.pop.ind, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.pop, 'Key'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionParetoByPopulation_Family <- ggplot(prev.pareto.all.nat.pop.fam, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.pop, 'Key'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  
  # subset by year (2014, 2015, 2016)
  prev.pareto.all.year <- with(prev.pareto.all, aggregate(Prevalence~Year+ShortName+Code, FUN=mean))
  prev.pareto.all.year$Name <- factor(prev.pareto.all.year$ShortName, levels = label.order.all)
  prev.pareto.all.year.ind <- prev.pareto.all.year[!(prev.pareto.all.year$Code %in% c('v','w','x','y')), ]
  prev.pareto.all.year.fam <- prev.pareto.all.year[!(prev.pareto.all.year$Code %in% c('b','c','d','e','f','g','j','k','l','m','n','p','q','r','s','t')), ]
  prev.pareto.all.year.ind$Name <- factor(prev.pareto.all.year.ind$ShortName, levels = label.order.ind)
  prev.pareto.all.year.fam$Name <- factor(prev.pareto.all.year.fam$ShortName, levels = label.order.fam)
  
  p.PercentDetectionParetoAnnual <- ggplot(prev.pareto.all.year, aes(x=Name, y=Prevalence, fill=as.factor(Year))) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.year, 'Year'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionParetoAnnual_Individual <- ggplot(prev.pareto.all.year.ind, aes(x=Name, y=Prevalence, fill=as.factor(Year))) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.year, 'Year'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
  p.PercentDetectionParetoAnnual_Family <- ggplot(prev.pareto.all.year.fam, aes(x=Name, y=Prevalence, fill=as.factor(Year))) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.year, 'Year'), name='') + scale_y_continuous(label=percent) + labs(title='', x='', y='Detection (%)')
}

# CHANGE TO SEASON-YEARS
if(TRUE) {
  
  start.yearweek <- '2013-26'
  
  # use data from all time and show a pareto of prevalence (collapsing fluA, coronas, pivs, and bacterias)
  positives.count.seasonal.trim <- merge(positives.count.all[as.character(positives.count.all$YearWeek) >= start.yearweek, ], unique(runs.reg.date[,c('YearWeek','Year','Week')]), by='YearWeek', all.x=TRUE)
  positives.count.seasonal.trim$SeasonWeek <- with(positives.count.seasonal.trim, ifelse(Week <= 26, Week + 26, Week - 26))
  positives.count.seasonal.trim$SeasonYear <- with(positives.count.seasonal.trim, ifelse(Week <= 26, paste(Year-1,Year,sep='-'), paste(Year,Year+1, sep='-')))
  positives.count.seasonal.trim <- positives.count.seasonal.trim[positives.count.seasonal.trim$SeasonYear >= '2013-2014', ]
  positives.count.seasonal.trim$YearWeek <- with(positives.count.seasonal.trim, ifelse(SeasonWeek < 10, paste(substring(SeasonYear, 6, 10), SeasonWeek, sep='-0'), paste(substring(SeasonYear, 6, 10), SeasonWeek, sep='-')))
  
  # need to sum up flu As, CoVs, PIVs, and Bacterias by customer site Id... then join these onto the positives.count.trim data frame
  decoder.agg <- rbind(decoder, data.frame(Bug='Influenza A (all)', Code='v'), data.frame(Bug='Coronavirus (all)', Code='w'), data.frame(Bug='Parainfluenza (all)', Code='x'), data.frame(Bug='Bacteria (all)', Code='y'))
  positives.count.seasonal.fluas <- merge(do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.seasonal.trim$CustomerSiteId)[x], Code = 'v', Positives = sum(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x] & positives.count.seasonal.trim$Code %in% as.character(decoder[decoder$Bug %in% fluAs,'Code']) & positives.count.seasonal.trim$YearWeek == unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(positives.count.seasonal.trim[,c('YearWeek','CustomerSiteId','SeasonYear','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.seasonal.covs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.seasonal.trim$CustomerSiteId)[x], Code = 'w', Positives = sum(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x] & positives.count.seasonal.trim$Code %in% as.character(decoder[decoder$Bug %in% corona,'Code']) & positives.count.seasonal.trim$YearWeek == unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(positives.count.seasonal.trim[,c('YearWeek','CustomerSiteId','SeasonYear','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.seasonal.pivs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.seasonal.trim$CustomerSiteId)[x], Code = 'x', Positives = sum(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x] & positives.count.seasonal.trim$Code %in% as.character(decoder[decoder$Bug %in% pivs,'Code']) & positives.count.seasonal.trim$YearWeek == unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(positives.count.seasonal.trim[,c('YearWeek','CustomerSiteId','SeasonYear','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.seasonal.bacteria <- merge(do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.seasonal.trim$CustomerSiteId)[x], Code = 'y', Positives = sum(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x] & positives.count.seasonal.trim$Code %in% as.character(decoder[decoder$Bug %in% bacterias,'Code']) & positives.count.seasonal.trim$YearWeek == unique(positives.count.seasonal.trim[positives.count.seasonal.trim$CustomerSiteId==unique(positives.count.seasonal.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), unique(positives.count.seasonal.trim[,c('YearWeek','CustomerSiteId','SeasonYear','Runs')]), by=c('YearWeek','CustomerSiteId'))
  positives.count.seasonal.agg <- rbind(positives.count.seasonal.trim[,c('YearWeek','CustomerSiteId','Code','Positives','SeasonYear','Runs')], positives.count.seasonal.fluas, positives.count.seasonal.covs, positives.count.seasonal.pivs, positives.count.seasonal.bacteria)
  positives.count.seasonal.agg <- merge(positives.count.seasonal.agg, decoder.agg, by='Code')
  positives.count.seasonal.agg <- merge(positives.count.seasonal.agg, shortnames.df, by.x='Bug', by.y='Organism', all.x=TRUE)
  positives.count.seasonal.agg$ShortName <- as.character(positives.count.seasonal.agg$ShortName)
  positives.count.seasonal.agg[as.character(positives.count.seasonal.agg$Code)=='v', 'ShortName'] <- 'Flu A (all)'
  positives.count.seasonal.agg[as.character(positives.count.seasonal.agg$Code)=='w', 'ShortName'] <- 'CoV (all)'
  positives.count.seasonal.agg[as.character(positives.count.seasonal.agg$Code)=='x', 'ShortName'] <- 'PIV (all)'
  positives.count.seasonal.agg[as.character(positives.count.seasonal.agg$Code)=='y', 'ShortName'] <- 'Bacteria (all)'
  
  # Make various paretos...
  prev.pareto.seasonal.all <- positives.count.seasonal.agg
  
  # start with all data from summer 2013-present showing all organisms and then grouping by family
  prev.pareto.seasonal.all.nat <- with(prev.pareto.seasonal.all, aggregate(cbind(Runs, Positives)~ShortName+Code+CustomerSiteId, FUN=sum))
  prev.pareto.seasonal.all.nat$Prevalence <- with(prev.pareto.seasonal.all.nat, Positives/Runs)
  prev.pareto.seasonal.all.nat <- with(prev.pareto.seasonal.all.nat, aggregate(Prevalence~ShortName+Code, FUN=mean))
  prev.pareto.seasonal.all.nat.ind <- prev.pareto.seasonal.all.nat[!(prev.pareto.seasonal.all.nat$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.nat.fam <- prev.pareto.seasonal.all.nat[!(prev.pareto.seasonal.all.nat$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  label.order.seasonal.all <- prev.pareto.seasonal.all.nat[with(prev.pareto.seasonal.all.nat, order(Prevalence, decreasing = TRUE)), 'ShortName']
  label.order.seasonal.all <- label.order.seasonal.all[c(1,2,3,6,11,21,24,4,9,14,15,19,5,10,16,18,20,7,8,12,13,17,22,23)]
  prev.pareto.seasonal.all.nat$Name <- factor(prev.pareto.seasonal.all.nat$ShortName, levels = label.order.seasonal.all)
  label.order.season.ind <- prev.pareto.seasonal.all.nat.ind[with(prev.pareto.seasonal.all.nat.ind, order(Prevalence, decreasing = TRUE)), 'ShortName']
  prev.pareto.seasonal.all.nat.ind$Name <- factor(prev.pareto.seasonal.all.nat.ind$ShortName, levels = label.order.season.ind)
  label.order.seasonal.fam <- prev.pareto.seasonal.all.nat.fam[with(prev.pareto.seasonal.all.nat.fam, order(Prevalence, decreasing = TRUE)), 'ShortName']
  prev.pareto.seasonal.all.nat.fam$Name <- factor(prev.pareto.seasonal.all.nat.fam$ShortName, levels = label.order.seasonal.fam)
  
  p.PercentDetectionParetoSeasonal <- ggplot(prev.pareto.seasonal.all.nat, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoSeasonal_Individual <- ggplot(prev.pareto.seasonal.all.nat.ind, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoSeasonal_Family <- ggplot(prev.pareto.seasonal.all.nat.fam, aes(x=Name, y=Prevalence, fill='Key')) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Key='Key'), 'Key'), guide=FALSE) + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  
  # subset the data into children's hospitals and mixed population hospitals
  # NEEDS TO BE UPDATED WITH PRIMARY CHILDRENS!!!!
  sites.mixed <- unique(runs.reg.date[!(runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name)[grep('Children', unique(runs.reg.date$Name))])),'CustomerSiteId'])
  sites.child <- unique(runs.reg.date[runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name))[grep('Children', unique(runs.reg.date$Name))],'CustomerSiteId'])
  prev.pareto.seasonal.all.pop <- prev.pareto.seasonal.all
  prev.pareto.seasonal.all.pop$Key <- with(prev.pareto.seasonal.all.pop, ifelse(CustomerSiteId %in% sites.mixed, 'Mixed', 'Pediatric'))
  prev.pareto.seasonal.all.pop <- with(prev.pareto.seasonal.all.pop, aggregate(cbind(Runs, Positives)~ShortName+Code+Key+CustomerSiteId, FUN=sum))
  prev.pareto.seasonal.all.pop$Prevalence <- with(prev.pareto.seasonal.all.pop, Positives/Runs)
  prev.pareto.seasonal.all.nat.pop <- with(prev.pareto.seasonal.all.pop, aggregate(Prevalence~ShortName+Code+Key, FUN=mean))
  prev.pareto.seasonal.all.nat.pop$Name <- factor(prev.pareto.seasonal.all.nat.pop$ShortName, levels=label.order.seasonal.all)
  prev.pareto.seasonal.all.nat.pop.ind <- prev.pareto.seasonal.all.nat.pop[!(prev.pareto.seasonal.all.nat.pop$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.nat.pop.fam <- prev.pareto.seasonal.all.nat.pop[!(prev.pareto.seasonal.all.nat.pop$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  prev.pareto.seasonal.all.nat.pop.ind$Name <- factor(prev.pareto.seasonal.all.nat.pop.ind$ShortName, levels=label.order.season.ind)
  prev.pareto.seasonal.all.nat.pop.fam$Name <- factor(prev.pareto.seasonal.all.nat.pop.fam$ShortName, levels=label.order.seasonal.fam)
  
  p.PercentDetectionParetoByPopulationSeasonal <- ggplot(prev.pareto.seasonal.all.nat.pop, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection')
  p.PercentDetectionParetoByPopulationSeasonal_Individual <- ggplot(prev.pareto.seasonal.all.nat.pop.ind, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoByPopulationSeasonal_Family <- ggplot(prev.pareto.seasonal.all.nat.pop.fam, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection (%)')
  
  # subset by year (2014, 2015, 2016)
  prev.pareto.seasonal.all.year <- with(prev.pareto.seasonal.all, aggregate(cbind(Runs, Positives)~SeasonYear+ShortName+Code+CustomerSiteId, FUN=sum))
  prev.pareto.seasonal.all.year$Prevalence <- with(prev.pareto.seasonal.all.year, Positives/Runs)
  prev.pareto.seasonal.all.year <- with(prev.pareto.seasonal.all.year, aggregate(Prevalence~SeasonYear+ShortName+Code, FUN=mean))
  prev.pareto.seasonal.all.year$Name <- factor(prev.pareto.seasonal.all.year$ShortName, levels = label.order.seasonal.all)
  prev.pareto.seasonal.all.year.ind <- prev.pareto.seasonal.all.year[!(prev.pareto.seasonal.all.year$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.year.fam <- prev.pareto.seasonal.all.year[!(prev.pareto.seasonal.all.year$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  prev.pareto.seasonal.all.year.ind$Name <- factor(prev.pareto.seasonal.all.year.ind$ShortName, levels = label.order.season.ind)
  prev.pareto.seasonal.all.year.fam$Name <- factor(prev.pareto.seasonal.all.year.fam$ShortName, levels = label.order.seasonal.fam)
  
  p.PercentDetectionParetoAnnualSeasonal <- ggplot(subset(prev.pareto.seasonal.all.year, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoAnnualSeasonal_Individual <- ggplot(subset(prev.pareto.seasonal.all.year.ind, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoAnnualSeasonal_Family <- ggplot(subset(prev.pareto.seasonal.all.year.fam, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  
  # make a table using the seasonal year data
  prev.table.seasonal.all <- do.call(cbind, lapply(1:length(unique(prev.pareto.seasonal.all.year$SeasonYear)), function(x) data.frame(ShortName = prev.pareto.seasonal.all.year[prev.pareto.seasonal.all.year$SeasonYear==unique(prev.pareto.seasonal.all.year$SeasonYear)[x],'ShortName'], Prevalence = prev.pareto.seasonal.all.year[prev.pareto.seasonal.all.year$SeasonYear==unique(prev.pareto.seasonal.all.year$SeasonYear)[x],'Prevalence'])))
  prev.table.seasonal.all.pop <- do.call(cbind, lapply(1:length(unique(prev.pareto.seasonal.all.nat.pop$Key)), function(x) data.frame(Key = unique(prev.pareto.seasonal.all.nat.pop$Key)[x], ShortName = prev.pareto.seasonal.all.nat.pop[prev.pareto.seasonal.all.nat.pop$Key == unique(prev.pareto.seasonal.all.nat.pop$Key)[x], 'ShortName'], Prevalence = prev.pareto.seasonal.all.nat.pop[prev.pareto.seasonal.all.nat.pop$Key == unique(prev.pareto.seasonal.all.nat.pop$Key)[x],'Prevalence'])))
  prev.pareto.side.by.side <- with(prev.pareto.seasonal.all, aggregate(cbind(Runs, Positives)~ShortName+Code+CustomerSiteId, FUN=sum))
  prev.pareto.side.by.side <- merge(prev.pareto.side.by.side, sitesByCensusRegions.etc[,c('CustomerSiteId','CensusRegionNational')], by='CustomerSiteId')
  prev.pareto.side.by.side <- with(prev.pareto.side.by.side, aggregate(cbind(Runs, Positives)~CensusRegionNational+ShortName+Code, FUN=sum))
  prev.pareto.side.by.side$Prevalence <- with(prev.pareto.side.by.side, Positives/Runs)
  prev.pareto.side.by.side <- merge(prev.pareto.side.by.side, population.dist.2016, by='CensusRegionNational')
  prev.pareto.side.by.side$WeightedPrevalence <- with(prev.pareto.side.by.side, Prevalence*PopulationPercent)
  prev.pareto.side.by.side <- with(prev.pareto.side.by.side, aggregate(WeightedPrevalence~ShortName+Code, FUN=sum))
  colnames(prev.pareto.side.by.side) <- c('ShortName','Code','Prevalence')
  
  # ALL PARETOS SHOULD BE WEIGHTED BY POPULATION OF REGION 
  prev.pareto.seasonal.all.census <- with(prev.pareto.seasonal.all, aggregate(cbind(Runs, Positives)~CustomerSiteId+Code+ShortName, FUN=sum))
  prev.pareto.seasonal.all.census$Prevalence <- with(prev.pareto.seasonal.all.census, Positives/Runs)
  prev.pareto.seasonal.all.census <- merge(prev.pareto.seasonal.all.census, sitesByCensusRegions.etc[,c('CustomerSiteId','CensusRegionNational')], by='CustomerSiteId')
  prev.pareto.seasonal.all.census <- with(prev.pareto.seasonal.all.census, aggregate(Prevalence~CensusRegionNational+Code+ShortName, FUN=mean))
  prev.pareto.seasonal.all.census <- merge(prev.pareto.seasonal.all.census, population.dist.2016, by='CensusRegionNational')
  prev.pareto.seasonal.all.census$WeightedPrevalence <- with(prev.pareto.seasonal.all.census, Prevalence*PopulationPercent)
  prev.pareto.seasonal.all.census <- with(prev.pareto.seasonal.all.census, aggregate(WeightedPrevalence~Code+ShortName, FUN=sum))
  colnames(prev.pareto.seasonal.all.census)[3] <- 'Prevalence'
  prev.pareto.seasonal.all.census.ind <- prev.pareto.seasonal.all.census[!(prev.pareto.seasonal.all.census$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.census.fam <- prev.pareto.seasonal.all.census[!(prev.pareto.seasonal.all.census$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s')), ]
  prev.pareto.seasonal.all.census$Name <- factor(prev.pareto.seasonal.all.census$ShortName, levels = label.order.seasonal.all)
  prev.pareto.seasonal.all.census.ind$Name <- factor(prev.pareto.seasonal.all.census.ind$ShortName, levels = label.order.season.ind)
  prev.pareto.seasonal.all.census.fam$Name <- factor(prev.pareto.seasonal.all.census.fam$ShortName, levels = label.order.seasonal.fam)
  
  prev.pareto.seasonal.all.sbys <- rbind(data.frame(prev.pareto.seasonal.all.nat, Key = 'No Weighting'), data.frame(prev.pareto.seasonal.all.census, Key = 'Population Weighted'))
  prev.pareto.seasonal.all.sbys.ind <- rbind(data.frame(prev.pareto.seasonal.all.nat.ind, Key = 'No Weighting'), data.frame(prev.pareto.seasonal.all.census.ind, Key = 'Population Weighted'))
  prev.pareto.seasonal.all.sbys.fam <- rbind(data.frame(prev.pareto.seasonal.all.nat.fam, Key = 'No Weighting'), data.frame(prev.pareto.seasonal.all.census.fam, Key = 'Population Weighted'))
  
  p.PercentDetectionParetoSeasonal_SideBySide <- ggplot(prev.pareto.seasonal.all.sbys, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.sbys, 'Key'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoSeasonal_Individual_SideBySide <- ggplot(prev.pareto.seasonal.all.sbys.ind, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.sbys.ind, 'Key'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoSeasonal_Family_SideBySide <- ggplot(prev.pareto.seasonal.all.sbys.fam, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.sbys.fam, 'Key'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
}

# ILI VS. RP NORMALIZED BURN RATE
if(TRUE) {
  
  runs.reg.norm.trim <- merge(runs.reg.norm[, c('YearWeek','Year','Week','CustomerSiteId','RollRuns','NormRollRate','NormRollRateByInst','NormalizedBurn')], unique(runs.reg[,c('CustomerSiteId','Name','Region')]), by='CustomerSiteId')
  runs.reg.norm.ili <- merge(runs.reg.norm.trim, data.frame(YearWeek = cdc.reg.count.df$YearWeek, Region = cdc.reg.count.df$Region, Rate = with(cdc.reg.count.df, ILITotal/TotalPatients)), by=c('YearWeek','Region'), all.x=TRUE)
  ili.burn.nat <- with(runs.reg.norm.ili, aggregate(cbind(Rate, NormalizedBurn)~YearWeek, FUN=mean))
  add.back.burn <- data.frame(with(runs.reg.norm.ili[!(runs.reg.norm.ili$YearWeek %in% ili.burn.nat$YearWeek) & runs.reg.norm.ili$YearWeek > max(ili.burn.nat$YearWeek), ], aggregate(NormalizedBurn~YearWeek, FUN=mean)), Rate = NA)
  ili.burn.nat <- rbind(ili.burn.nat, add.back.burn[,c('YearWeek','Rate','NormalizedBurn')])  
  # dateBreaksAlt <- c('2013-26','2013-39','2013-52',dateBreaks)
  # dateLabelsAlt <- c('--','','',dateLabels)

  ccf.ili.turn <- data.frame(Lag = ccf(ili.burn.nat$Rate, ili.burn.nat$NormalizedBurn, na.action=na.pass)$lag, CCF = ccf(ili.burn.nat$Rate, ili.burn.nat$NormalizedBurn, na.action=na.pass)$acf)
  
  # # This is some code to check out the BURN rate vs. Run rates so that we can see if there are weird sites and how the algorithm may need to be adjusted  
  # ggplot(runs.reg.norm, aes(x=YearWeek, y=NormRollRateByInst, group=as.factor(CustomerSiteId), color='NormRunByInst')) + geom_line() + geom_line(aes(x=YearWeek, y=NormRollRate, group=as.factor(CustomerSiteId), color='NormRun'), data=runs.reg.norm) + geom_line(aes(x=YearWeek, y=NormalizedBurn, group=as.factor(CustomerSiteId), color='Burn'), data=runs.reg.norm) + facet_wrap(~CustomerSiteId, scale='free_y') + scale_x_discrete(breaks=dateBreaks) + theme(axis.text.x=element_text(angle=90)) + scale_color_manual(values=c('purple','blue','black'))
  # ggplot(runs.reg.norm, aes(x=YearWeek, y=RollRate, group=as.factor(CustomerSiteId), color='RunRate')) + geom_line() + geom_line(aes(x=YearWeek, y=RollRateByInst, group=as.factor(CustomerSiteId), color='RunRateByInst'), data=runs.reg.norm) + geom_line(aes(x=YearWeek, y=8*NormalizedBurn, group=as.factor(CustomerSiteId), color='Burn'), data=runs.reg.norm) + facet_wrap(~CustomerSiteId, scale='free_y') + scale_x_discrete(breaks=dateBreaks) + theme(axis.text.x=element_text(angle=90)) + scale_color_manual(values=c('purple','blue','black'))
  
  p1 <- ggplot(subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), aes(x=YearWeek, y=Rate, color='CDC ILI', group='CDC ILI')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=NormalizedBurn/100, group='FilmArray Utilization', color='FilmArray Utilization'), subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), lwd=1.5) + scale_color_manual(values=c('black','purple'), name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.05), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, face='bold', color='black'), axis.text.x=element_text(angle=90, hjust=1), axis.ticks.x=element_blank(), panel.background=element_rect(fill='white', color='white'), legend.position = 'bottom') + labs(x='Date', y='ILI (%)')
  p2 <- ggplot(subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), aes(x=YearWeek, y=NormalizedBurn/100, group='FilmArray Utilization', color='FilmArray Utilization')) + geom_line(lwd=1.5, color='purple') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0, 0.05), breaks=c(0.01, 0.02, 0.03, 0.04, 0.05), labels=c('1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Utilization')
  
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  
  # Get the location of the plot panel in g1.
  # These are used later when transformed elements of g2 are put back into g1
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  # Overlap panel for second plot on that of the first plot
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Get the y axis title from g2
  # index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
  index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
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
  ili.burn.nat.compare <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/NationalILIvsBURN.png', height=800, width=1400)
  grid.draw(ili.burn.nat.compare)
  dev.off()
}

# TIME SERIES OF PREVALENCE WITH CDC OVERLAY + FILMARRAY RP NORMALIZED TEST RATE
if(TRUE) {
  
  # BFDx - All fluAs and fluB percent detection
  bfdx.flu.reg <- with(prevalence.reg.count, aggregate(cbind(Runs, j, k, l, m, n)~YearWeek+Region+CustomerSiteId, FUN=sum))
  bfdx.flu.reg$FluDetections <- with(bfdx.flu.reg, j+k+l+m+n)

  # CDC - Clinical Labs (only has data from 2015+... excludes public health lab data since I don't know how to add that in)
  cdc.flu.reg <- read.csv('../DataSources/RegionalInfluenzaByType_ClinicalLabs.csv', header=TRUE, sep=',')
  cdc.flu.reg <- data.frame(YearWeek = with(cdc.flu.reg, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-'))), Region = cdc.flu.reg$REGION, TotalPatients = cdc.flu.reg$TOTAL.SPECIMENS, TotalFluObservations = cdc.flu.reg$TOTAL.A + cdc.flu.reg$TOTAL.B)
  cdc.flu.reg <- do.call(rbind, lapply(1:length(unique(cdc.flu.reg$Region)), function(x) data.frame(YearWeek = cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'YearWeek'][2:(length(cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'YearWeek'])-1)], Region = unique(cdc.flu.reg$Region)[x], TotalPatients = sapply(2:(length(cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'YearWeek'])-1), function(y) sum(cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'TotalPatients'][(y-1):(y+1)])), TotalFluObservations = sapply(2:(length(cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'YearWeek'])-1), function(y) sum(cdc.flu.reg[cdc.flu.reg$Region == unique(cdc.flu.reg$Region)[x],'TotalFluObservations'][(y-1):(y+1)])))))
  
  # # CDC - Public Health labs (2015+) and data from both Public Health and Clinical labs prior to 2015
  # cdc.flu.sub.reg <- read.csv('../../DataSources/RegionalInfluenzaBySubType.csv', header=TRUE, sep=',')
  # cdc.flu.sub.reg.prior <- read.csv('../../DataSources/RegionalInfluenzaBySubType_PriorYears.csv', header=TRUE, sep=',')
  # cdc.flu.sub.reg <- data.frame(YearWeek = with(cdc.flu.sub.reg, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-'))), Region = cdc.flu.sub.reg$REGION, TotalPatients = cdc.flu.sub.reg$TOTAL.SPECIMENS, TotalFluObservations = cdc.flu.sub.reg$A..H3.+cdc.flu.sub.reg$A..2009.H1N1.+cdc.flu.sub.reg$A..Subtyping.not.Performed.+cdc.flu.sub.reg$B+cdc.flu.sub.reg$BVic+cdc.flu.sub.reg$BYam+cdc.flu.sub.reg$H3N2v)
  # cdc.flu.sub.reg.prior <- data.frame(YearWeek = with(cdc.flu.sub.reg.prior, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-'))), Region = cdc.flu.sub.reg.prior$REGION, TotalPatients = cdc.flu.sub.reg.prior$TOTAL.SPECIMENS, TotalFluObservations = cdc.flu.sub.reg.prior$A..H3.+cdc.flu.sub.reg.prior$A..2009.H1N1.+cdc.flu.sub.reg.prior$A..H1.+cdc.flu.sub.reg.prior$A..Subtyping.not.Performed.+cdc.flu.sub.reg.prior$B+cdc.flu.sub.reg.prior$H3N2v+cdc.flu.sub.reg.prior$A..Unable.to.Subtype.)
  
  cdc.bfdx.flu.reg <- merge(cdc.flu.reg, bfdx.flu.reg, by=c('YearWeek','Region'))[ ,c('YearWeek','Region','TotalPatients','TotalFluObservations','Runs','FluDetections')]
  cdc.bfdx.flu.reg$FluPrevalence <- with(cdc.bfdx.flu.reg, TotalFluObservations/TotalPatients)
  cdc.bfdx.flu.reg$FluPercentDetection <- with(cdc.bfdx.flu.reg, FluDetections/Runs)
  cdc.bfdx.flu.nat <- with(cdc.bfdx.flu.reg, aggregate(cbind(FluPrevalence, FluPercentDetection)~YearWeek, FUN=mean))
  cdc.bfdx.flu.nat <- merge(cdc.bfdx.flu.nat, unique(prevalence.nat.individual.wrap[,c('YearWeek','Rate')]), by='YearWeek')
  cdc.bfdx.flu.nat <- merge(cdc.bfdx.flu.nat, ili.burn.nat[,c('YearWeek','NormalizedBurn')], by='YearWeek')
  
  dateBreaksAlt2 <- c('2015-41','2016-01','2016-14','2016-27','2016-40','2016-52')
  dateLabelsAlt2 <- c('-','Jan-2016','-','Jul-2016','-','Jan-2017')
  
  # p1 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='Percent Detection', fill='FilmArray Detection')) + geom_bar(stat='identity') + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Name=c('FilmArray Detection')), 'Name'), name='') + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5)  + geom_line(aes(x=YearWeek, y=10*Rate, group='CDC ILI Rate', color='CDC ILI Rate'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue'), name='') + scale_y_continuous(breaks=c(0,.07,0.14,0.21,0.28,0.35), limits=c(0,0.35), labels=c(0, 7, 14, 21, 28, 35)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%), CDC Flu Prevalence (%)', x='Date')
  # p1 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='FilmArrary Detection', color='FilmArray Detection')) + geom_line(size=2) + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5)  + geom_line(aes(x=YearWeek, y=10*Rate, group='CDC ILI Rate', color='CDC ILI Rate'), cdc.bfdx.flu.nat, lwd=1.5) + geom_line(aes(x=YearWeek, y=NormalizedBurn/10, group='FilmArray Utilization', color='FilmArray Utilization'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(breaks=c(0,.07,0.14,0.21,0.28,0.35), limits=c(0,0.35), labels=c(0, 7, 14, 21, 28, 35)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%), Flu Prevalence (%)', x='Date')
  p1 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='FilmArrary Detection', color='FilmArray Detection')) + geom_line(size=2) + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5)  + geom_line(aes(x=YearWeek, y=8*Rate, group='CDC ILI Rate', color='CDC ILI Rate'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(breaks=c(0,.07,0.14,0.21,0.28,0.35), limits=c(0,0.35), labels=c(0, 7, 14, 21, 28, 35)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%), Flu Prevalence (%)', x='Date')
  p2 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + scale_y_continuous(limits=c(0,0.32), breaks=c(0, 0.04, 0.08, 0.12, 0.16, 0.20, 0.24, 0.28, 0.32), labels=c('0.0','0.5','1.0','1.5','2.0','2.5','3.0','3.5','4.0')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%)')
  
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
  fluTriple <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/InfluenzaPrevalencePercentDetectionBurnAndILI.png', height=800, width=1400)
  grid.draw(fluTriple)
  dev.off()
  
  p.InfluezaPrevalencePercentDetectionNoILI <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='FilmArrary Detection', color='FilmArray Detection')) + geom_line(size=2) + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(breaks=c(0,.07,0.14,0.21,0.28,0.35), limits=c(0,0.35), labels=c(0, 7, 14, 21, 28, 35)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%), Flu Prevalence (%)', x='Date')
  
  # lindsay also wants to see this by summing the detections and runs by region and then getting the percent detection & comparing to CDC reported regional data
  bfdx.flu.reg.agg <- with(bfdx.flu.reg, aggregate(cbind(Runs, FluDetections)~YearWeek+Region, FUN=sum))
  cdc.bfdx.flu.agg <- merge(cdc.flu.reg, bfdx.flu.reg.agg, by=c('YearWeek','Region'))
  cdc.bfdx.flu.agg$FluPrevalence <- with(cdc.bfdx.flu.agg, TotalFluObservations/TotalPatients)
  cdc.bfdx.flu.agg$FluPercentDetection <- with(cdc.bfdx.flu.agg, FluDetections/Runs)
  cdc.bfdx.flu.agg <- with(cdc.bfdx.flu.agg, aggregate(cbind(FluPrevalence, FluPercentDetection)~YearWeek, FUN=mean))
  
  # cross-correlation
  ccf.chart <- ccf(cdc.bfdx.flu.nat$FluPrevalence, cdc.bfdx.flu.nat$FluPercentDetection)
  ccf.frame <- data.frame(Lag = ccf(cdc.bfdx.flu.nat$FluPrevalence, cdc.bfdx.flu.nat$FluPercentDetection)$lag, CCF = ccf(cdc.bfdx.flu.nat$FluPrevalence, cdc.bfdx.flu.nat$FluPercentDetection)$acf)
  fluAll.cor <- with(cdc.bfdx.flu.nat, cor(FluPrevalence, FluPercentDetection))
  ccf.frame[ccf.frame$CCF==max(ccf.frame$CCF), ]
  
  # make time series percent detection by organism family with ILI and Normalized Utilization Overlays
  prevalence.nat.individual.wrap <- prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, YearWeek)), ]
  # - FLU As------------------------------------------------------------------------------------------------------
  if(TRUE) {
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluAs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.25), breaks=c(0, 0.05, 0.10, 0.15, 0.2, 0.25), labels=c(0, 5, 10, 15, 20, 25)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='Detection (%)', x='Date')
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluAs), aes(x=YearWeek, y=5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.25), breaks=c(0, 0.05, 0.10, 0.15, 0.2, 0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.fluAs <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/FluAPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.fluAs)
    dev.off()
  }
  # - FLU B_------------------------------------------------------------------------------------------------------
  if(TRUE) {
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluBs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c(0,2.5,5,7.5,10,12.5)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluBs), aes(x=YearWeek, y=2.5*Rate, group=1))  + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=2.5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.fluBs <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/FluBPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.fluBs)
    dev.off()
  }  
  # - RSV---------------------------------------------------------------------------------------------------------
  if(TRUE) {
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rsv), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.25), breaks=c(0, 0.05, 0.10, 0.15, 0.20, 0.25), labels=c('0','5','10','15','20','25')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date')
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rsv), aes(x=YearWeek, y=5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.25), breaks=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.rsv <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/RSVPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.rsv)
    dev.off()
  }
  # - PIVs--------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% pivs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','3','6','9','12','15')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% pivs), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.pivs <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/PIVsPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.pivs)
    dev.off()
  }
  # - CoVs--------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% corona), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','3','6','9','12','15')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% corona), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.covs <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/CoVsPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.covs)
    dev.off()
  }
  # - Rhino-------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rhino), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.6), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c(0, 10, 20, 30, 40, 50, 60)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rhino), aes(x=YearWeek, y=12*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=12*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.6), breaks=c(0, 0.12, 0.24, 0.36, 0.48, 0.60), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.rhino <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/RhinoPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.rhino)
    dev.off()
  }
  # - Adeno-------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% adeno), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','3','6','9','12','15')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% adeno), aes(x=YearWeek, y=3*Rate, group=1))+ geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.adeno <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/AdenoPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.adeno)
    dev.off()
  }
  # - HMPV_-------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% hmp), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','3','6','9','12','15')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% hmp), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.hmp <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/HMPvPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.hmp)
    dev.off()
  }
  # - Bacteria-------------------------------------------------------------------------------------------------------
  if(TRUE) {
    
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% bacterias), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','2.5','5','7.5','10','12.5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(y='Detection (%)', x='Date') 
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% bacterias), aes(x=YearWeek, y=2.5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=2.5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='purple', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    overlay.bacteria <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    
    # Draw it
    grid.newpage()
    png('Figures/BacteriaPercentDetectionWithOverlayTrend.png', height=800, width=1400)
    grid.draw(overlay.bacteria)
    dev.off()
  }
  # # - Negative-------------------------------------------------------------------------------------------------------
  if(FALSE) {
    # # negatives.nat <- data.frame(Bug='Negatives', YearWeek = with(prevalence.nat.individual.wrap, aggregate(Prevalence~YearWeek, FUN=sum))$YearWeek, Prevalence = 1-with(prevalence.nat.individual.wrap, aggregate(Prevalence~YearWeek, FUN=sum))$Prevalence, Rate=with(prevalence.nat.individual.wrap, aggregate(Rate~YearWeek, FUN=mean))$Rate, ShortName='Negative')
    # if(FALSE) {
    # 
    #   p1 <- ggplot(negatives.nat, aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values='grey', name='') + scale_x_discrete(breaks = as.character(unique(negatives.nat$YearWeek))[order(as.character(unique(negatives.nat$YearWeek)))][seq(1, length(as.character(unique(negatives.nat$YearWeek))), 8)]) + scale_y_continuous(label=percent, breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(title='Percent Detection of Human Metapneumovirus in Trend Population with ILI Overlay', y='Percent Detection of Organism', x='Date')
    #   p2 <- ggplot(negatives.nat, aes(x=YearWeek, y=10*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=10*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2014-01'), color='purple', lwd=2) + scale_x_discrete(breaks = as.character(unique(negatives.nat$YearWeek))[order(as.character(unique(negatives.nat$YearWeek)))][seq(1, length(as.character(unique(negatives.nat$YearWeek))), 8)]) + scale_y_continuous(limits=c(0,10*max(negatives.nat$Rate)), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c('0%','1%','2%','3%','4%','5%')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black), FilmArray Test Utilization (red)')
    # 
    #   # Get the ggplot grobs
    #   g1 <- ggplotGrob(p1)
    #   g2 <- ggplotGrob(p2)
    # 
    #   # Get the location of the plot panel in g1.
    #   # These are used later when transformed elements of g2 are put back into g1
    #   pp <- c(subset(g1$layout, name == "panel", se = t:r))
    # 
    #   # Overlap panel for second plot on that of the first plot
    #   g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
    # 
    #   # Get the y axis title from g2
    #   index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
    #   ylab <- g2$grobs[[index]]                # Extract that grob
    #   ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications
    # 
    #   # Put the transformed label on the right side of g1
    #   g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    #   g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")
    # 
    #   # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
    #   index <- which(g2$layout$name == "axis-l")  # Which grob
    #   yaxis <- g2$grobs[[index]]                  # Extract the grob
    # 
    #   # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
    #   # The relevant grobs are contained in axis$children:
    #   #   axis$children[[1]] contains the axis line;
    #   #   axis$children[[2]] contains the tick marks and tick mark labels.
    # 
    #   # First, move the axis line to the left
    #   yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
    # 
    #   # Second, swap tick marks and tick mark labels
    #   ticks <- yaxis$children[[2]]
    #   ticks$widths <- rev(ticks$widths)
    #   ticks$grobs <- rev(ticks$grobs)
    # 
    #   # Third, move the tick marks
    #   ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
    # 
    #   # Fourth, swap margins and fix justifications for the tick mark labels
    #   ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
    # 
    #   # Fifth, put ticks back into yaxis
    #   yaxis$children[[2]] <- ticks
    # 
    #   # Put the transformed yaxis on the right side of g1
    #   g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
    #   overlay.negatives <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
    # 
    #   # Draw it
    #   grid.newpage()
    #   png('Figures/NegativePercentDetectionWithOverlayTrend.png', height=800, width=1400)
    #   grid.draw(overlay.negatives)
    #   dev.off()
    # }
  }  
  
  # Mark and Lindsay would like to see charts that show prevalence of Flu A by subtypes vs. FilmArray percent detection as well as FluB
  public.health.flu <- read.csv('../DataSources/RegionalInfluenzaBySubType_PublicHealthLabs.csv', header=TRUE, sep=',')
  public.health.flu <- data.frame(Region = public.health.flu$REGION, YearWeek = ifelse(public.health.flu$WEEK < 10, paste(public.health.flu$YEAR, public.health.flu$WEEK, sep='-0'), paste(public.health.flu$YEAR, public.health.flu$WEEK, sep='-')), TotalPatients = public.health.flu$TOTAL.SPECIMENS, FluAH109 = public.health.flu$A..2009.H1N1., FlAH3 = public.health.flu$A..H3., FluANoSubtype = public.health.flu$A..Subtyping.not.Performed., FluB = public.health.flu$B)
  prev.reg.wrap <- merge(prevalence.reg.wrap, unique(runs.reg[,c('CustomerSiteId','Region')]), by='CustomerSiteId')
  public.health.flu <- public.health.flu[public.health.flu$Region %in% unique(prev.reg.wrap$Region), ]
  regions <- as.character(unique(public.health.flu$Region))
  yearweeks <- as.character(unique(public.health.flu$YearWeek))
  public.health.flu <- do.call(rbind, lapply(1:length(regions), function(x) do.call(rbind, lapply(2:(length(yearweeks)-1), function(y) data.frame(YearWeek = yearweeks[y], Region = regions[x], TotalPatients = sum(filter(public.health.flu, Region==regions[x])[, 'TotalPatients'][(y-1):(y+1)]), FluAH109 = sum(filter(public.health.flu, Region==regions[x])[, 'FluAH109'][(y-1):(y+1)]), FlAH3 = sum(filter(public.health.flu, Region==regions[x])[, 'FlAH3'][(y-1):(y+1)]), FluANoSubtype = sum(filter(public.health.flu, Region==regions[x])[, 'FluANoSubtype'][(y-1):(y+1)]), FluB = sum(filter(public.health.flu, Region==regions[x])[, 'FluB'][(y-1):(y+1)]))))))
  
  prev.reg.ph.flu <- merge(prev.reg.wrap, public.health.flu, by=c('YearWeek','Region'))
  prev.reg.ph.flu$FluAH109 <- with(prev.reg.ph.flu, FluAH109/TotalPatients)
  prev.reg.ph.flu$FlAH3 <- with(prev.reg.ph.flu, FlAH3/TotalPatients)
  prev.reg.ph.flu$FluANoSubtype <- with(prev.reg.ph.flu, FluANoSubtype/TotalPatients)
  prev.reg.ph.flu$FluB <- with(prev.reg.ph.flu, FluB/TotalPatients)
  prev.nat.ph.flu <- with(prev.reg.ph.flu, aggregate(cbind(Prevalence, Rate, FluAH109, FlAH3, FluANoSubtype, FluB)~YearWeek+ShortName, FUN=mean))
  
  # correlation and cross-correlation
  fluAH3.cor <- with(subset(prev.nat.ph.flu, ShortName=='FluA H3'), cor(FlAH3, Prevalence))
  fluAH3.ccf <- with(subset(prev.nat.ph.flu, ShortName=='FluA H3'), ccf(FlAH3, Prevalence))
  fluAH3.ccf.df <- data.frame(Lag = fluAH3.ccf$lag, CCF = fluAH3.ccf$acf)
  fluAH3.ccf.df[fluAH3.ccf.df$CCF==max(fluAH3.ccf.df$CCF), ]
  FluAH109.cor <- with(subset(prev.nat.ph.flu, ShortName=='FluA H1-09'), cor(FluAH109, Prevalence))
  FluAH109.ccf <- with(subset(prev.nat.ph.flu, ShortName=='FluA H1-09'), ccf(FluAH109, Prevalence))
  FluAH109.ccf.df <- data.frame(Lag = FluAH109.ccf$lag, CCF = FluAH109.ccf$acf)
  FluAH109.ccf.df[FluAH109.ccf.df$CCF==max(FluAH109.ccf.df$CCF), ]
  FluB.cor <- with(subset(prev.nat.ph.flu, ShortName=='FluB'), cor(FluB, Prevalence))
  FluB.ccf <- with(subset(prev.nat.ph.flu, ShortName=='FluB'), ccf(FluB, Prevalence))
  FluB.ccf.df <- data.frame(Lag = FluB.ccf$lag, CCF = FluB.ccf$acf)
  FluB.ccf.df[FluB.ccf.df$CCF==max(FluB.ccf.df$CCF), ]
  
  # FLU A H1-2009
  p1 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluA H1-09'), aes(x=YearWeek, y=4*Prevalence, group='FilmArray Percent Detection of FluA H1-09', color='FilmArray Percent Detection of FluA H1-09')) + geom_line(lwd=2) + geom_line(aes(x=YearWeek, y=FluAH109, group='CDC Reported Rate of FluA H1-09', color='CDC Reported Rate of FluA H1-09'), data=prev.nat.ph.flu, lwd=2) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(limits=c(0,0.6), breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c('10','20','30','40','50','60')) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='CDC Reported Detection (%)', x='Date')
  p2 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluA H1-09'), aes(x=YearWeek, y=4*Prevalence, group=1), color='transparent') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.6), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c('0','2.5','5.0','7.5','10.0','12.5','15.0')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%)')
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  index <- which(g2$layout$name == "ylab-l")
  ylab <- g2$grobs[[index]]           
  ylab <- hinvert_title_grob(ylab)
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")
  index <- which(g2$layout$name == "axis-l")
  yaxis <- g2$grobs[[index]] 
  yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  # make the dual axes look nice
  overlay.fluAH109 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  grid.newpage()
  png('Figures/FluAH109PercentDetectionWithOverlay.png', height=800, width=1400)
  grid.draw(overlay.fluAH109)
  dev.off()
  
  # FLU A H3
  p1 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluA H3'), aes(x=YearWeek, y=4*Prevalence, group='FilmArray Percent Detection of FluA H3', color='FilmArray Percent Detection of FluA H3')) + geom_line(lwd=2) + geom_line(aes(x=YearWeek, y=FlAH3, group='CDC Reported Rate of FluA H3', color='CDC Reported Rate of FluA H3'), data=prev.nat.ph.flu, lwd=2) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(limits=c(0,0.6), breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c('10','20','30','40','50','60')) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='CDC Reported Detection (%)', x='Date')
  p2 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluA H3'), aes(x=YearWeek, y=4*Prevalence, group=1), color='transparent') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.6), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), labels=c('0','2.5','5.0','7.5','10.0','12.5','15.0')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%)')
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  index <- which(g2$layout$name == "ylab-l")
  ylab <- g2$grobs[[index]]           
  ylab <- hinvert_title_grob(ylab)
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")
  index <- which(g2$layout$name == "axis-l")
  yaxis <- g2$grobs[[index]] 
  yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  # make the dual axes look nice
  overlay.fluAH3 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  grid.newpage()
  png('Figures/FluAH3PercentDetectionWithOverlay.png', height=800, width=1400)
  grid.draw(overlay.fluAH3)
  dev.off()
  
  # FLU B
  p1 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluB'), aes(x=YearWeek, y=Prevalence, group='FilmArray Percent Detection of FluB', color='FilmArray Percent Detection of FluB')) + geom_line(lwd=2) + geom_line(aes(x=YearWeek, y=FluB, group='CDC Reported Rate of FluB', color='CDC Reported Rate of FluB'), data=prev.nat.ph.flu, lwd=2) + scale_color_manual(values=c('black','blue','purple','darkgreen'), name='') + scale_y_continuous(limits=c(0,0.1), breaks=c(0.0, 0.02, 0.04, 0.06, 0.08, 0.1), labels=c('0','2','4','6','8','10')) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='CDC Reported Detection (%)', x='Date')
  p2 <- ggplot(subset(prev.nat.ph.flu, ShortName=='FluB'), aes(x=YearWeek, y=Prevalence, group=1), color='transparent') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.1), breaks=c(0, 0.02, 0.04, 0.06, 0.08, 0.1), labels=c('0','2','4','6','8','10')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%)')
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  index <- which(g2$layout$name == "ylab-l")
  ylab <- g2$grobs[[index]]           
  ylab <- hinvert_title_grob(ylab)
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")
  index <- which(g2$layout$name == "axis-l")
  yaxis <- g2$grobs[[index]] 
  yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  # make the dual axes look nice
  overlay.fluB <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  grid.newpage()
  png('Figures/FluBPercentDetectionWithOverlay.png', height=800, width=1400)
  grid.draw(overlay.fluB)
  dev.off()
}

# CO-DETECTIONS
if(TRUE) {
  
  # create a co-detection chart that will show all organisms broken out in order of highest -> lowest precent detection over all the data
  # with an overlay of the percent dual detection of that organism
  run.positive.count <- with(data.frame(merge(runs.reg.date[runs.reg.date$YearWeek >= '2013-26' & runs.reg.date$CustomerSiteId %in% sites, c('RunDataId','Year')], bugs.df, by='RunDataId'), Record=1), aggregate(Record~RunDataId, FUN=sum))
  dual.detection.runs <- data.frame(bugs.df[bugs.df$RunDataId %in% run.positive.count[run.positive.count$Record>1, 'RunDataId'], ], Record = 1)
  dual.detection.runs <- merge(dual.detection.runs, runs.reg.date[runs.reg.date$YearWeek >= '2013-26' & runs.reg.date$CustomerSiteId %in% sites, c('RunDataId','Year','CustomerSiteId')], by='RunDataId')
  total.runs <- with(runs.reg.date[runs.reg.date$YearWeek >= '2013-26' & runs.reg.date$CustomerSiteId %in% sites, c('RunDataId','Year','CustomerSiteId','Record')], sum(Record))
  dual.detection.agg <- with(dual.detection.runs, aggregate(Record~BugPositive, FUN=sum))
  dual.detection.agg$PercentOfDuals <- with(dual.detection.agg, Record/total.runs)
  dual.detection.agg <- merge(dual.detection.agg, shortnames.df, by.x='BugPositive', by.y='Organism')
  prev.pareto.all.duals <- merge(prev.pareto.seasonal.all.nat.ind, dual.detection.agg, by='ShortName', all.x=TRUE)
  prev.pareto.all.duals[is.na(prev.pareto.all.duals$PercentOfDuals),'PercentOfDuals'] <- 0
  prev.pareto.all.duals$Name <- factor(prev.pareto.all.duals$ShortName, levels=prev.pareto.all.duals[with(prev.pareto.all.duals, order(Prevalence, decreasing = TRUE)),'ShortName'])
  length(unique(run.positive.count[run.positive.count$Record > 1, 'RunDataId']))/total.runs
  coParticipation.DetectionCount <- merge(with(data.frame(bugs.df[bugs.df$RunDataId %in% run.positive.count$RunDataId, ], Positives=1), aggregate(Positives~BugPositive, FUN=sum)), with(data.frame(bugs.df[bugs.df$RunDataId %in% dual.detection.runs$RunDataId, ], CoDetections=1), aggregate(CoDetections~BugPositive, FUN=sum)), by='BugPositive')
  coParticipation.DetectionCount$PercentOfDetectionsWithACo <- with(coParticipation.DetectionCount, CoDetections/Positives)
  prev.pareto.all.duals <- merge(prev.pareto.all.duals, coParticipation.DetectionCount, by='BugPositive')
  
  # histogram of % of detection that are co-detections (colored by bug, needs text labels)
  elementary.block.pal <- bug.individual.Pal
  # reassign colors based on bug family
    # make Adeno, HRV/EV, hMPV, and RSV distinct
  elementary.block.pal[names(elementary.block.pal) %in% c('Adeno','hMPV','HRV/EV','RSV')] <- c('#F9F516','#1616F9','#F92016','#16F9F9') 
    # make corons purple
  elementary.block.pal[names(elementary.block.pal) %in% c('CoV NL63','CoV HKU1','CoV OC43','CoV 229E')] <- c('#E5CBEB','#DB90EB','#A83CC1','#8B20D8')
    # make flus blue
  elementary.block.pal[names(elementary.block.pal) %in% c('FluA H3','FluA H1-09','Flu A','FluB')] <- c('#ABCEF0','#6794C1','#1469BD','#037FFA')
    # make bacteria green
  elementary.block.pal[names(elementary.block.pal) %in% c('B. per','C. pne', 'M. pne')] <- c('#ADE6B2','#4DB556','#2C8943')
    # make PIVs pink
  elementary.block.pal[names(elementary.block.pal) %in% c('PIV1','PIV2','PIV3','PIV4')] <- c('#F6BBDA','#EA84BA','#C44287','#F9168E')

  block.labels <- as.character(subset(prev.pareto.all.duals, PercentOfDetectionsWithACo < 0.7)$Name)[order(as.character(subset(prev.pareto.all.duals, PercentOfDetectionsWithACo < 0.7)$PercentOfDetectionsWithACo))]
  block.labels <- block.labels[c(1, 4, 8, 2, 3, 5, 9, 10, 6, 7, 11, 14, 15, 16, 12, 13, 17, 18, 19)]
  # p.PercentOfDetectionsWithACoDetectionFrequency <- ggplot(subset(prev.pareto.all.duals, PercentOfDetectionsWithACo < 0.7), aes(x=PercentOfDetectionsWithACo, fill=ShortName)) + geom_histogram(binwidth=0.05) + scale_fill_manual(values=elementary.block.pal, guide=FALSE) + annotate('text',x=c(0.1, 0.15, 0.15, 0.2, 0.2, 0.25, 0.25, 0.25, 0.3, 0.3, 0.35, 0.35, 0.35, 0.4, 0.4, 0.4, 0.45, 0.5, 0.55), y=c(0.5, 0.5, 1.5, 0.5, 1.5, 0.5, 1.5, 2.5, 0.5, 1.5, 0.5, 1.5, 2.5, 0.5, 1.5, 2.5, 0.5, 0.5, 0.5), label=block.labels, size=6) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(hjust=0.5, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(x='Percent of Detections with a Co-Detection (%)', y='Frequency') + scale_x_continuous(breaks=c(0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5,0.55), labels=c('10','15','20','25','30','35','40','45','50','55'))
  p.PercentOfDetectionsWithACoDetectionFrequency <- ggplot(subset(prev.pareto.all.duals, PercentOfDetectionsWithACo < 0.7), aes(x=PercentOfDetectionsWithACo, fill=ShortName)) + geom_histogram(binwidth=0.1) + scale_fill_manual(values=elementary.block.pal, guide=FALSE) + annotate('text',x=c(0.1, 0.2, 0.2, 0.2, 0.2, 0.2, 0.3, 0.3, 0.3, 0.3, 0.3, 0.4, 0.4, 0.4, 0.4, 0.4, 0.5, 0.5, 0.6), y=c(0.5, 0.5, 1.5, 2.5, 3.5, 4.5, 0.5, 1.5, 2.5, 3.5, 4.5, 0.5, 1.5, 2.5, 3.5, 4.5, 0.5, 1.5, 0.5), label=block.labels, size=6) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(hjust=0.5, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(x='Percent of Detections with a Co-Detection (%)', y='Frequency') + scale_x_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6), labels=c('0','10','20','30','40','50','60'))
  
  # make a nifty dual-axis chart
  # p1 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=Prevalence)) + geom_bar(stat='identity') + scale_fill_manual(values='grey', guide=FALSE) + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.10,0.15,0.20,0.25,0.30), labels=c('0','5','10','15','20','25','30')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(y='Detection (%)', x='')
  # p2 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=5*PercentOfDuals, color='Percent of Dual Detections')) + geom_point(size=4) + scale_color_manual(values='black', guide=FALSE) + scale_y_continuous(limits=c(0,5*max(prev.pareto.all.duals$PercentOfDuals)), breaks=c(0, 0.05, 0.1, 0.15,0.2,0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank()) + labs(y='Co-Detection Occurrence (%)')
  p1 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=Prevalence, fill='Percent Detection')) + geom_bar(stat='identity') + scale_fill_manual(values=c('grey','transparent'), guide=FALSE) + geom_point(aes(x=Name, y=PercentOfDetectionsWithACo/4, color='Percent of Detections with a Co-Detection Present'), data=prev.pareto.all.duals, size=4, color='black') + scale_y_continuous(limits=c(0,0.25), breaks=c(0,0.05,0.10,0.15,0.20,0.25), labels=c('0','5','10','15','20','25')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(y='Detection (%)', x='')
  p2 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=PercentOfDetectionsWithACo/4)) + geom_point() + scale_y_continuous(limits=c(0,0.25), breaks=c(0,0.05,0.1,0.15,0.2,0.25), labels=c('0','20','40','60','80','100')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank()) + labs(y='Detections with a Co-Detection (%)')
  
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
  paretoCoDets2 <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/PercentDetectionParetoWithCoDetections2.png', height=800, width=1400)
  grid.draw(paretoCoDets2)
  dev.off()
  
  coParticipation.DetectionCount.fillByCount <- data.frame(BugPositive = coParticipation.DetectionCount$BugPositive, Key = 'Single Detection', Positives = coParticipation.DetectionCount$Positives, Detections = coParticipation.DetectionCount$Positives - coParticipation.DetectionCount$CoDetections) 
  coParticipation.DetectionCount.fillByCount <- merge(coParticipation.DetectionCount.fillByCount, shortnames.df, by.x='BugPositive', by.y='Organism')
  coParticipation.DetectionCount.fillByCount$Name <- factor(coParticipation.DetectionCount.fillByCount$ShortName, levels=coParticipation.DetectionCount.fillByCount[with(coParticipation.DetectionCount.fillByCount, order(Positives, decreasing = TRUE)), 'ShortName'])
  coParticipation.DetectionCount.fillByCount.2 <- merge(data.frame(BugPositive = coParticipation.DetectionCount$BugPositive, Key = 'Co-Detection', Positives = coParticipation.DetectionCount$Positives, Detections = coParticipation.DetectionCount$CoDetections), shortnames.df, by.x='BugPositive', by.y='Organism')
  coParticipation.DetectionCount.fillByCount.2$Name <- factor(coParticipation.DetectionCount.fillByCount.2$ShortName, levels = levels(coParticipation.DetectionCount.fillByCount$Name))
  coParticipation.DetectionCount.fillByCount <- rbind(coParticipation.DetectionCount.fillByCount, coParticipation.DetectionCount.fillByCount.2)
  rm(coParticipation.DetectionCount.fillByCount.2)
  coParticipation.DetectionCount.fillByCount$Percentage <- coParticipation.DetectionCount.fillByCount$Detections/total.runs
  p.SingleAndCoDetectionPercentOfRuns <- ggplot(coParticipation.DetectionCount.fillByCount, aes(x=Name, y=Percentage, fill=Key)) + geom_bar(stat='identity') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(coParticipation.DetectionCount.fillByCount, 'Key'), name='') + scale_y_continuous(limits=c(0,0.25), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.SingleAndCoDetectionPercentOfRuns_NoHRV <- ggplot(subset(coParticipation.DetectionCount.fillByCount, ShortName!='HRV/EV'), aes(x=Name, y=Percentage, fill=Key)) + geom_bar(stat='identity') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(coParticipation.DetectionCount.fillByCount, 'Key'), name='') + scale_y_continuous(limits=c(0,0.10), breaks=c(0,0.025,0.05,0.075,0.10), labels=c('0.0','2.5','5.0','7.5','10.0')) + labs(x='', y='Detection (%)')
  p.SingleAndCoDetectionPercentOfRuns_NoHRVorRSV <- ggplot(subset(coParticipation.DetectionCount.fillByCount, !(ShortName %in% c('HRV/EV','RSV'))), aes(x=Name, y=Percentage, fill=Key)) + geom_bar(stat='identity') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(coParticipation.DetectionCount.fillByCount, 'Key'), name='') + scale_y_continuous(limits=c(0,0.05), breaks=c(0,0.01,0.02,0.03,0.04,0.05), labels=c('0.0','1.0','2.0','3.0','4.0','5.0')) + labs(x='', y='Detection (%)')
  
  b <- prev.pareto.all.duals
  b$Name <- factor(b$ShortName, levels = b[with(b, order(PercentOfDetectionsWithACo, decreasing = TRUE)), 'ShortName'])
  p.CosPerTotalDetections <- ggplot(subset(b, Name!='FluA H1'), aes(x=Name, y=PercentOfDetectionsWithACo)) + geom_bar(stat='identity', fill='#FF7F24') + scale_y_continuous(limits=c(0,0.6), breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6), labels=c('0','10','20','30','40','50','60')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(y='Co-Detections/Total Detections (%)', x='')
  
  checker <- merge(bugs.df, runs.reg.date, by='RunDataId')
  checker.detection.count <- with(subset(checker, YearWeek >= '2013-26'), aggregate(Record~BugPositive, FUN=sum))
  checker.agg <- merge(checker.detection.count, dual.detection.agg[,c('BugPositive','Record')], by='BugPositive')
  checker.agg$Rate <- checker.agg$Record.y/checker.agg$Record.x
  
}

# CORRELATION OF SUMMED PERCENT DETECTION WITH ILI AND TURN
if(TRUE) {
  
  # Get the prevalence of all organisms with all Flu A grouped together
  cor.prev.noFluA <- prevalence.nat.individual.wrap[!(prevalence.nat.individual.wrap$ShortName %in% as.character(unique(prevalence.nat.individual.wrap[grep('Influenza A', prevalence.nat.individual.wrap$Bug), 'ShortName']))), c('YearWeek','ShortName','Prevalence')]
  cor.prev.FluA <- prevalence.nat.families[prevalence.nat.families$Key=='Influenza A', c('YearWeek','Name','Prevalence')]
  colnames(cor.prev.noFluA) <-  c('YearWeek','Name','Prevalence')
  cor.prev <- rbind(cor.prev.noFluA, cor.prev.FluA)
  cor.prev$YearWeek <- as.character(cor.prev$YearWeek)
  cor.prev$Name <- as.character(cor.prev$Name)
  cor.prev <- merge(cor.prev[cor.prev$YearWeek >= '2013-26', ], ili.burn.nat, by='YearWeek')
  cor.prev[grep('Influenza A', cor.prev$Name), 'Name'] <- 'FluA'
  weeks.in.cor <- unique(cor.prev$YearWeek)
  weeks.in.cor <- weeks.in.cor[sapply(1:length(weeks.in.cor), function(x) length(cor.prev[cor.prev$YearWeek==weeks.in.cor[x], 'Name']))==17]
  cor.prev <- cor.prev[cor.prev$YearWeek %in% weeks.in.cor, ]
  
  # next, determine the corelation of each of the organisms above with ILI AND TURN
  cor.vars <- unique(cor.prev$Name)
  cor.vars <- cor.vars[order(cor.vars)]
  single.var.cor <- data.frame(Name = cor.vars, CorILI = sapply(1:length(cor.vars), function(x) cor(cor.prev[cor.prev$Name==cor.vars[x], 'Prevalence'], cor.prev[cor.prev$Name==cor.vars[x], 'Rate'])), CorTURN = sapply(1:length(cor.vars), function(x) cor(cor.prev[cor.prev$Name==cor.vars[x], 'Prevalence'], cor.prev[cor.prev$Name==cor.vars[x], 'NormalizedBurn'])), R2ILI = sapply(1:length(cor.vars), function(x) summary(lm(Rate~Prevalence, cor.prev[cor.prev$Name==cor.vars[x], ]))$adj.r.squared), R2TURN = sapply(1:length(cor.vars), function(x) summary(lm(NormalizedBurn~Prevalence, cor.prev[cor.prev$Name==cor.vars[x], ]))$adj.r.squared))
  
  # now iterate to find the best correlation/adjusted R2 with random combinations of N organisms (Flu A grouped) and stop when the correlation is not improved by the addition of variables
  multiple.var.cor <- data.frame(VariableCount = 1, 
                                 BestCorComboILI = as.character(single.var.cor[single.var.cor$CorILI==max(single.var.cor$CorILI), 'Name']),
                                 BestCorILI  = max(single.var.cor$CorILI),
                                 BestCorComboTURN = as.character(single.var.cor[single.var.cor$CorTURN==max(single.var.cor$CorTURN), 'Name']), 
                                 BestCorTURN  = max(single.var.cor$CorTURN),
                                 BestR2ComboILI = as.character(single.var.cor[single.var.cor$R2ILI==max(single.var.cor$R2ILI), 'Name']), 
                                 BestR2ILI  = max(single.var.cor$R2ILI),
                                 BestR2ComboTURN = as.character(single.var.cor[single.var.cor$R2TURN==max(single.var.cor$R2TURN), 'Name']),
                                 BestR2TURN  = max(single.var.cor$R2TURN)
                                 )
  # STOPPED HERE... SO I NEED TO RESTART HERE SO THAT I CAN MAKE THE CHARTS ASSOCIATED WITH ITEM 5 ON THE TRELLO CARD
  for(i in 2:length(cor.vars)) { 
    
    combos <- generateCombos(cor.vars, i, FALSE)
    var.cor <- c()
    for(j in 1:length(combos)) {
      
      temp <- cor.prev[cor.prev$Name %in% combos[[j]], ]
      temp.summed <- data.frame(YearWeek = unique(temp$YearWeek), SummedPrev = with(temp, aggregate(Prevalence~YearWeek, FUN=sum))$Prevalence, ILI = with(temp, aggregate(Rate~YearWeek, FUN=mean))$Rate, TURN = with(temp, aggregate(NormalizedBurn~YearWeek, FUN=mean))$NormalizedBurn)
      temp.var.cor <- data.frame(Name = paste(combos[[j]], collapse=', '), CorILI = cor(temp.summed$SummedPrev, temp.summed$ILI), CorTURN = cor(temp.summed$SummedPrev, temp.summed$TURN), R2ILI = summary(lm(ILI~SummedPrev, data=temp.summed))$adj.r.squared, R2TURN = summary(lm(TURN~SummedPrev, data=temp.summed))$adj.r.squared)
      var.cor <- rbind(var.cor, temp.var.cor)
    }
    
    multiple.var.cor <- rbind(multiple.var.cor, data.frame(VariableCount = i, 
                                                           BestCorComboILI = as.character(var.cor[var.cor$CorILI==max(var.cor$CorILI), 'Name']),
                                                           BestCorILI  = max(var.cor$CorILI),
                                                           BestCorComboTURN = as.character(var.cor[var.cor$CorTURN==max(var.cor$CorTURN), 'Name']), 
                                                           BestCorTURN  = max(var.cor$CorTURN),
                                                           BestR2ComboILI = as.character(var.cor[var.cor$R2ILI==max(var.cor$R2ILI), 'Name']), 
                                                           BestR2ILI  = max(var.cor$R2ILI),
                                                           BestR2ComboTURN = as.character(var.cor[var.cor$R2TURN==max(var.cor$R2TURN), 'Name']),
                                                           BestR2TURN  = max(var.cor$R2TURN)
                                                           )
    )
  }
  
  # THE BEST COMBINATION OF N ORGANISMS IN TERMS OF CORRELATION AND R2 HAVE BEEN DETERMINED, NOW FIGURE OUT WHICH N-COMBO PROVIDES THE BEST OVERALL
  multiple.var.cor[multiple.var.cor$BestCorILI==max(multiple.var.cor$BestCorILI), c('VariableCount','BestCorComboILI','BestCorILI')]
  # multiple.var.cor[multiple.var.cor$BestCorTURN==max(multiple.var.cor$BestCorTURN), c('VariableCount','BestCorComboTURN','BestCorTURN')]
  # multiple.var.cor[multiple.var.cor$BestR2ILI==max(multiple.var.cor$BestR2ILI), c('VariableCount','BestR2ComboILI','BestR2ILI')]
  # multiple.var.cor[multiple.var.cor$BestR2TURN==max(multiple.var.cor$BestR2TURN), c('VariableCount','BestR2ComboTURN','BestR2TURN')]
  # write.csv(multiple.var.cor, 'Figures/summedPrevalenceCorrelation.csv', row.names = FALSE)
  # 
  # now re-do the same sort of thing, but just do it with a count of positives correlated with the total run count (days don't matter b/c it would be runs/day and positives/day)
  # first, lump together Flu A
  cor.count.fluA <- positives.count.all[grep('Influenza A', positives.count.all$Bug), ]
  cor.count.fluA <- with(cor.count.fluA, aggregate(Positives~YearWeek+CustomerSiteId+Runs+Region+Name, FUN=sum))
  cor.count.fluA <- data.frame(cor.count.fluA, Code = 'v', Bug = 'FluA')[,c('YearWeek','CustomerSiteId','Runs','Region','Name','Bug','Code','Positives')]
  cor.count.noFluA <- positives.count.all[!(as.character(positives.count.all$Bug) %in% as.character(unique(positives.count.all$Bug))[grep('Influenza A', unique(as.character(positives.count.all$Bug)))]), ]
  cor.count <- rbind(cor.count.noFluA, cor.count.fluA)
  cor.count <- merge(cor.count, shortnames.df, by.x='Bug', by.y='Organism', all.x=TRUE)
  cor.count$ShortName <- as.character(cor.count$ShortName)
  cor.count[is.na(cor.count$ShortName), 'ShortName'] <- 'FluA'
  
  # sum everything nationally, because I don't really know how to do it otherwise... I'd get like 20 different values...
  cor.count.agg <- with(cor.count, aggregate(cbind(Positives, Runs)~YearWeek+ShortName, FUN=sum))
  single.var.cor.count <- do.call(rbind, lapply(1:length(unique(cor.count.agg$ShortName)), function(x) data.frame(ShortName = unique(cor.count.agg$ShortName)[x], CorTUR = cor(cor.count.agg[cor.count.agg$ShortName==unique(cor.count.agg$ShortName)[x], 'Positives'], cor.count.agg[cor.count.agg$ShortName==unique(cor.count.agg$ShortName)[x], 'Runs']), R2TUR = summary(lm(Runs~Positives, data = cor.count.agg[cor.count.agg$ShortName==unique(cor.count.agg$ShortName)[x], ]))$adj.r.squared)))
  
  multiple.var.cor.count <- data.frame(VariableCount = 1, 
                                       BestCorCombo = as.character(single.var.cor.count[single.var.cor.count$CorTUR==max(single.var.cor.count$CorTUR), 'ShortName']),
                                       BestCor = max(single.var.cor.count$CorTUR),
                                       BestR2Combo = as.character(single.var.cor.count[single.var.cor.count$R2TUR==max(single.var.cor.count$R2TUR), 'ShortName']),
                                       BestR2 = max(single.var.cor.count$R2TUR)
                                       )
  for(i in 2:length(cor.vars)) {
    
    combos <- generateCombos(cor.vars, i, FALSE)
    var.cor <- c()
    for(j in 1:length(combos)) {
      
      temp <- cor.count.agg[cor.count.agg$ShortName %in% combos[[j]], ]
      temp.summed <- data.frame(YearWeek = unique(temp$YearWeek), SummedPositives = with(temp, aggregate(Positives~YearWeek, FUN=sum))$Positives, SummedRuns = with(temp, aggregate(Runs~YearWeek, FUN=mean))$Runs)
      temp.var.cor <- data.frame(ShortName = paste(combos[[j]], collapse=', '), CorTUR = cor(temp.summed$SummedPositives, temp.summed$SummedRuns), R2TUR = summary(lm(SummedRuns~SummedPositives, data=temp.summed))$adj.r.squared)
      var.cor <- rbind(var.cor, temp.var.cor)
    }
    
    multiple.var.cor.count <- rbind(multiple.var.cor.count, data.frame(VariableCount = i, 
                                                                       BestCorCombo = as.character(var.cor[var.cor$CorTUR==max(var.cor$CorTUR), 'ShortName']),
                                                                       BestCor = max(var.cor$CorTUR),
                                                                       BestR2Combo = as.character(var.cor[var.cor$R2TUR==max(var.cor$R2TUR), 'ShortName']),
                                                                       BestR2 = max(var.cor$R2TUR)
    ))
  }

  # evaluate the results
  multiple.var.cor[multiple.var.cor$BestCorILI==max(multiple.var.cor$BestCorILI), c('VariableCount','BestCorComboILI','BestCorILI')]
  multiple.var.cor.count[multiple.var.cor.count$BestCor==max(multiple.var.cor.count$BestCor), c('VariableCount','BestCorCombo','BestCor')]
  # multiple.var.cor.count[multiple.var.cor.count$BestR2==max(multiple.var.cor.count$BestR2), c('VariableCount','BestR2Combo','BestR2')]
  
  # MAKE A CHART OF TUR TO PLACE UNDER TURN
  tur.nat <- with(with(positives.count.all, aggregate(Runs~YearWeek+CustomerSiteId, FUN=mean)), aggregate(Runs~YearWeek, FUN=sum)) 
  tur.positives.nat <- merge(tur.nat, with(positives.count.all, aggregate(Positives~YearWeek+Bug, FUN=sum)), by='YearWeek')
  tur.positives.nat <- merge(tur.positives.nat, shortnames.df, by.x='Bug', by.y='Organism')
  p.DetectionsWithTUR_NoYAxis <- ggplot(subset(tur.positives.nat[with(tur.positives.nat, order(ShortName, decreasing=TRUE)),], YearWeek >= '2013-26'), aes(x=YearWeek)) + geom_area(aes(y=Positives, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + geom_line(aes(x=YearWeek, y=Runs, group='TUR'), data=subset(tur.positives.nat[with(tur.positives.nat, order(ShortName, decreasing=TRUE)),], YearWeek >= '2013-26'), size=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Test Utilization, Detections', x='Date')
  p.TUR_NoYAxis <- ggplot(aes(x=YearWeek, y=Runs, group='TUR'), data=subset(tur.positives.nat[with(tur.positives.nat, order(ShortName, decreasing=TRUE)),], YearWeek >= '2013-26')) + geom_line(size=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Test Utilization', x='Date')

  # MAKE SOME CHARTS WITH TUR AND ILL WITH CORRELATION VARIABLES - see the # evaluate the results above...
  p1 <- ggplot(cor.prev[cor.prev$Name %in% strsplit(as.character(multiple.var.cor[multiple.var.cor$BestCorILI==max(multiple.var.cor$BestCorILI), 'BestCorComboILI']), split = ', ')[[1]], ], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=createPaletteOfVariableLength(cor.prev, 'Name'), name='') + geom_line(aes(x=YearWeek, y=6*Rate, group='ILI', color='ILI'), data=cor.prev, size=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='FilmArray Percent Detection (%)', x='Date') + scale_y_continuous(limits=c(0, 0.3), breaks=c(0.0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30'))
  p2 <- ggplot(cor.prev) + theme(panel.grid=element_blank(), plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='transparent'), axis.ticks.x=element_blank()) + labs(title='', y='ILI (%)', x='Date') + scale_y_continuous(limits=c(0, 0.3), breaks=c(0.0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0.00','0.83','1.67','2.50','3.33','4.17','5.00'))
  
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
  iliBestCor <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/BestCorrelatedSummedPrevalenceWithILI.png', height=800, width=1400)
  grid.draw(iliBestCor)
  dev.off()
  
  # Same thing, but with TUR... So which summed prevalence has best correlation with TUR
  new.pal <- bug.individual.Pal
  new.pal <- c(new.pal, '#FFA500')
  names(new.pal)[21]  <- 'FluA'
  p.BestCorrelatedSummedPrevalenceWithTUR <- ggplot(cor.count.agg[cor.count.agg$ShortName %in% strsplit(as.character(multiple.var.cor.count[multiple.var.cor.count$BestCor==max(multiple.var.cor.count$BestCor), 'BestCorCombo']), split = ', ')[[1]] & cor.count.agg$YearWeek>='2013-26', ], aes(x=YearWeek)) + geom_area(aes(y=Positives, fill=ShortName, group=ShortName), stat='identity', position='stack') + scale_fill_manual(values=createPaletteOfVariableLength(cor.count.agg, 'ShortName'), name='') + geom_line(aes(x=YearWeek, y=Runs, group='TUR', color='TUR'), data=cor.count.agg, size=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', x='Date', y='')
  
  cor.count.agg$Name <- factor(cor.count.agg$ShortName, levels=c('RSV','PIV4','PIV3','PIV2','PIV1','hMPV','FluB','FluA','CoV OC43','CoV NL63','CoV HKU1','CoV 229E','HRV/EV','M. pne','C. pne','B. per','Adeno'))
  p.AllSummedPrevalenceWithTUR <- ggplot(subset(cor.count.agg, YearWeek >= '2013-26'), aes(x=YearWeek)) + geom_area(aes(y=Positives, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=new.pal, name='') + geom_line(aes(x=YearWeek, y=Runs, group='TUR', color='TUR'), data=subset(cor.count.agg, YearWeek>='2013-26'), size=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', x='Date', y='')
  
  # count of detections divided by TURN
  # cor.count.agg.mrg$FactoredCount <- with(cor.count.agg.mrg, Positives/NormalizedBurn)
  # prev.nat.ind.wrap.turn <- merge(prevalence.nat.individual.wrap, ili.burn.nat, by='YearWeek')
  # prev.nat.ind.wrap.turn$FactPrev <- with(prev.nat.ind.wrap.turn, Prevalence*NormalizedBurn)
  cor.count.agg$Prev <- with(cor.count.agg, Positives/Runs)
  cor.count.agg.mrg <- merge(subset(cor.count.agg, YearWeek >= '2013-26'), ili.burn.nat, by='YearWeek')
  cor.count.agg.mrg$FactPrev <- with(cor.count.agg.mrg, NormalizedBurn*Prev)
  p.SeasonallyAdjustedPercentDetection <- ggplot(cor.count.agg.mrg, aes(x=YearWeek)) + geom_area(aes(y=FactPrev, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=new.pal, name='') + geom_line(aes(x=YearWeek, y=NormalizedBurn, group='TURN'), data=cor.count.agg.mrg, size=1.5) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', x='Date', y='TURN, Seasonally Adjusted Detections')
  
  rhino.fact.prev <- merge(subset(cor.count.agg.mrg, ShortName=='HRV/EV'), subset(prevalence.nat.individual.wrap, ShortName=='HRV/EV')[,c('YearWeek','Prevalence')], by='YearWeek')
  m.rhino <- lm(FactPrev~Prevalence, data=rhino.fact.prev)$coefficients[2]
  ccf.rhino <- data.frame(Lag = ccf(rhino.fact.prev$FactPrev, rhino.fact.prev$Prevalence)$lag, CCF = ccf(rhino.fact.prev$FactPrev, rhino.fact.prev$Prevalence)$acf)
  
  p1 <- ggplot(rhino.fact.prev, aes(x=YearWeek)) + geom_area(aes(y=FactPrev, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=new.pal, name='') + geom_line(aes(x=YearWeek, y=m.rhino*Prevalence, group=1), data=rhino.fact.prev, size=1.5) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank(), axis.ticks.y=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', x='Date', y='Seasonally Adjusted Detections') + scale_y_continuous(limits=c(0, 1.4), breaks=c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4), labels=c('0.0','0.2','0.4','0.6','0.8','1.0','1.2','1.4'))
  p2 <- ggplot(rhino.fact.prev) + theme(panel.grid=element_blank(), plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='transparent'), axis.ticks.x=element_blank()) + labs(title='', y='Detection (%)', x='Date') + scale_y_continuous(limits=c(0, 1.4), breaks=c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4), labels=c('0','14','28','42','56','70','84','98'))
  
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
  rsadwpdo <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/RhinoSeaonallyAdjustedDetectionsWithPercentDetectionOverlay.png', height=800, width=1400)
  grid.draw(rsadwpdo)
  dev.off()
  
  # Cross-correlation
  pos.count <- with(cor.count.agg[cor.count.agg$ShortName %in% strsplit(as.character(multiple.var.cor.count[multiple.var.cor.count$BestCor==max(multiple.var.cor.count$BestCor), 'BestCorCombo']), split = ', ')[[1]], ], aggregate(Positives~YearWeek, FUN=sum))
  run.count <- with(cor.count.agg[cor.count.agg$ShortName %in% strsplit(as.character(multiple.var.cor.count[multiple.var.cor.count$BestCor==max(multiple.var.cor.count$BestCor), 'BestCorCombo']), split = ', ')[[1]], ], aggregate(Runs~YearWeek, FUN=mean))
  run.pos.count <- merge(run.count, pos.count, by='YearWeek')
  tur.summed.cor <- with(run.pos.count, cor(Runs, Positives))
  tur.summed.ccf <- with(run.pos.count, ccf(Runs, Positives))
  tur.summed.ccf.df <- data.frame(Lag = tur.summed.ccf$lag, CCF = tur.summed.ccf$acf)
  tur.summed.ccf.df[tur.summed.ccf.df$CCF==max(tur.summed.ccf.df$CCF), ]
}

# REGRESSION ANALYSIS - ILI AND TURN vs. PERCENT DETECTION OF ORGANISMS
if(TRUE) {
  
  prev.predict <- prev.pareto.all[prev.pareto.all$YearWeek >= '2013-26', ]
  prev.predict$Prevalence <- with(prev.predict, Positives/Runs)
  prev.predict[prev.predict$Runs < 30, 'Prevalence'] <- NA
  prev.predict <- with(prev.predict, aggregate(Prevalence~YearWeek+Code+ShortName, FUN=mean))
  prev.predict <- prev.predict[with(prev.predict, order(Code, YearWeek)), ]
  prev.predict <- data.frame(YearWeek = unique(prev.predict$YearWeek), do.call(cbind, lapply(1:length(unique(prev.predict$Code)), function(x) prev.predict[prev.predict$Code==unique(prev.predict$Code)[x],'Prevalence'])))
  colnames(prev.predict)[grep('X', colnames(prev.predict))] <- as.character(unique(prev.pareto.seasonal.all$Code)) # letters[1:(length(colnames(prev.predict))-1)]
  prev.predict <- merge(prev.predict, ili.burn.nat, by='YearWeek')
  prev.predict <- prev.predict[!(is.na(prev.predict$Rate)), ]
  
  fit.vars <- c('a','b','c','d','e','f','g','h','i','n','o','p','q','r','s','t','v')
  
  # MACHINE LEARNING ALGORITHMS ----------------------------------------------------------------------------------------------
  # DO CART and RANDOM FOREST AND SOMEHOW COMPARE THE TWO METHODS... CAN ALSO COMPARE REGRESSION MODELS AS WELL
  # CART modeling (rpart)
  ili.model.cart <- rpart(as.formula(paste('Rate', paste(fit.vars, collapse='+'), sep='~')), data=prev.predict, method='anova')
  burn.model.cart <- rpart(as.formula(paste('NormalizedBurn', paste(fit.vars, collapse='+'), sep='~')), data=prev.predict, method='anova')
  # cp is the complexity parameter (amount by which splitting at the node improved the relative error b/t the model and the actual values)
  printcp(ili.model.cart) # actual used in tree construction = g, i, u, v (Corona OC43, HRV/Entero, RSV, and Flu A)
  printcp(burn.model.cart) # actual used in tree = i, o, s, u (HRV/Entero, Flu B, PIV 3, RSV)  
  plotcp(ili.model.cart)
  plotcp(burn.model.cart)
  # r-squared and relative error for different splits
  rsq.rpart(ili.model.cart)
  rsq.rpart(burn.model.cart) 
  # use party
  ili.cart.party <- as.party(ili.model.cart)
  burn.cart.party <- as.party(burn.model.cart)
  plot(ili.cart.party)
  plot(burn.cart.party)
  # prune the tree to minimize the cross-validated error (xerror column in printcp(model))
  ili.prune.cart <- prune(ili.model.cart, ili.model.cart$cptable[which.min(ili.model.cart$cptable[,'xerror']),'CP'])
  burn.prune.cart <- prune(burn.model.cart, burn.model.cart$cptable[which.min(burn.model.cart$cptable[,'xerror']),'CP'])
  # use party
  ili.prune.party <- as.party(ili.prune.cart)
  burn.prune.party <- as.party(burn.prune.cart)
  plot(ili.prune.party)
  plot(burn.prune.party)
  # these trees are the same with or without pruning... see if I can rename the letters in the model based on decoder
  # --------------------------
  # !!!!!! should probably rename the variables as their shortnames so that the tree looks better... not just letters !!!!
  # --------------------------
  decoder.abv <- merge(decoder.agg, shortnames.df, by.x='Bug', by.y='Organism', all.x=TRUE)
  decoder.abv$Bug <- as.character(decoder.abv$Bug)
  decoder.abv$ShortName <- as.character(decoder.abv$ShortName)
  decoder.abv$ShortName <- gsub('/', '_', gsub(' ', '', decoder.abv$ShortName))
  decoder.abv[is.na(decoder.abv$ShortName), 'ShortName'] <- c('FluA','CoV','PIV','Bacteria')
  replace.codes <- as.character(decoder.abv[decoder.abv$Code %in% fit.vars, 'ShortName'])
  prev.predict.trim <- prev.predict[,c('YearWeek', fit.vars, 'Rate', 'NormalizedBurn')]
  colnames(prev.predict.trim) <- c('YearWeek', replace.codes, 'Rate', 'NormalizedBurn')
  ili.model.cart.named <- rpart(as.formula(paste('Rate', paste(replace.codes, collapse='+'), sep='~')), data=prev.predict.trim, method='anova')
  burn.model.cart.named <- rpart(as.formula(paste('NormalizedBurn', paste(replace.codes, collapse='+'), sep='~')), data=prev.predict.trim, method='anova')
  ili.cart.party.named <- as.party(ili.model.cart.named)
  burn.cart.party.named <- as.party(burn.model.cart.named)
  p.CART_ILI <- plot(ili.cart.party.named)
  p.CART_Burn <- plot(burn.cart.party.named)
  
  ili.rate.cart <- predict(ili.model.cart, prev.predict)
  burn.rate.cart <- predict(burn.model.cart, prev.predict)
  cor(ili.rate.cart, prev.predict$Rate)
  cor(burn.rate.cart, prev.predict$NormalizedBurn)
  hist(prev.predict$Rate)
  hist(ili.rate.cart, breaks = hist(prev.predict$Rate, plot = FALSE)$breaks)
  prev.modeled <- data.frame(prev.predict, iliCART = ili.rate.cart)
  prev.modeled <- data.frame(prev.modeled, burnCART = burn.rate.cart)
  # ggplot(prev.modeled, aes(x=YearWeek, y=Rate, group='Reported ILI', color='Reported ILI')) + geom_line() + geom_line(aes(x=YearWeek, y=iliCART, group='Predicted ILI (CART)', color='Predicted ILI (CART)'), data=prev.modeled)
  
  # use randomForest package instead...
  # %IncMSE (if predictor is important in RF model, then assigining other values for that predictor randomly but 'realistically' should have a negative influence on prediction, i.e. using the same model to predict from data that is the same except for the given variable should give a worse prediction)
  # IncNodePurity (at each split, calculate how much the split reduces node impurity, or the difference between RSS- residual sum of squares or sum of squared errors) before and after the split... this is summed over all splits for that variable, over all trees)
  set.seed(4042)
  ili.model.rf <- randomForest(Rate~a+b+c+d+e+f+g+h+i+o+p+q+r+s+t+u+v, data=prev.predict, mtry=3, importance=TRUE)
  ili.model.rf.df <- data.frame(importance(ili.model.rf))
  ili.model.rf.df$Code <- rownames(ili.model.rf.df)
  ili.model.rf.df <- merge(ili.model.rf.df, decoder.agg, by='Code')
  ili.model.rf.df[with(ili.model.rf.df, order(IncNodePurity, decreasing = TRUE)), ]
  
  set.seed(4042)
  burn.model.rf <- randomForest(NormalizedBurn~a+b+c+d+e+f+g+h+i+o+p+q+r+s+t+u+v, data=prev.predict, mtry=3, importance=TRUE)
  burn.model.rf.df <- data.frame(importance(burn.model.rf))
  burn.model.rf.df$Code <- rownames(burn.model.rf.df)
  burn.model.rf.df <- merge(burn.model.rf.df, decoder.agg, by='Code')
  burn.model.rf.df[with(burn.model.rf.df, order(IncNodePurity, decreasing = TRUE)), ]
  
  ili.rate.rf <- predict(ili.model.rf, prev.predict)
  burn.rate.rf <- predict(burn.model.rf, prev.predict)
  prev.modeled <- data.frame(prev.modeled, iliRF = ili.rate.rf)
  prev.modeled <- data.frame(prev.modeled, burnRF = burn.rate.rf)
  cor(prev.modeled$Rate, ili.rate.rf)
  cor(prev.modeled$NormalizedBurn, burn.rate.rf)
  
  set.seed(4042)
  ili.model.cforest <- ctree(as.formula(paste('Rate', paste(replace.codes, collapse='+'), sep='~')), data=prev.predict.trim)
  burn.model.cforest <- ctree(as.formula(paste('NormalizedBurn', paste(replace.codes, collapse='+'), sep='~')), data=prev.predict.trim)
  p.RandomForestILI <- plot(ili.model.cforest)
  p.RandomForestBurn <- plot(burn.model.cforest)
  # not as good as the randomForest package, but it does make a nice plot... may need to learn how to manually extract plot from randomForest results
  cor(prev.predict.trim$Rate, predict(ili.model.cforest, prev.predict.trim))
  cor(prev.predict.trim$NormalizedBurn, predict(burn.model.cforest, prev.predict.trim))
  
  # REGRESSION MODELING -------------------------------------------------------------------------------------------------------
  fit.ili.all <- lm(as.formula(paste('Rate', paste(fit.vars, collapse='+'), sep='~')), prev.predict)
  fit.burn.all <- lm(as.formula(paste('NormalizedBurn', paste(fit.vars, collapse='+'), sep='~')), prev.predict)
  
  # create model possibilities
  combos.eight <- generateCombos(fit.vars, 8)
  combos.nine <- generateCombos(fit.vars, 9)
  combos.ten <- generateCombos(fit.vars, 10)
  combos.eleven <- generateCombos(fit.vars, 11)
  #### RESTART HERE!!!
  # Model ILI with prevalence of diseases in FilmArray test population (p-value = 5.00e-2 is established stop)
  ili.model.eval.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict)), prev.predict$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict), fit.ili.all)[,'Pr(>F)'][2])))
  ili.model.eval.eight[ili.model.eval.eight$anova.all == max(ili.model.eval.eight$anova.all), ]
  
  ili.model.eval.nine <- do.call(rbind, lapply(1:length(combos.nine[,'Combo']), function(x) data.frame(Model = combos.nine[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict)), prev.predict$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prev.predict), fit.ili.all)[,'Pr(>F)'][2])))
  ili.model.eval.nine[ili.model.eval.nine$anova.all == max(ili.model.eval.nine$anova.all), ]
  # ili.model.eval.nine has been "good enough" the last several times it's been ran
  
  # ili.model.eval.ten <- do.call(rbind, lapply(1:length(combos.ten[,'Combo']), function(x) data.frame(Model = combos.ten[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict)), prev.predict$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.ten[x,]), sep='~')), prev.predict), fit.ili.all)[,'Pr(>F)'][2])))
  # ili.model.eval.ten[ili.model.eval.ten$anova.all == max(ili.model.eval.ten$anova.all), ]
  
  # Model Burn with prevalence of diseases in FilmArray test population
  # burn.model.eval.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict)), prev.predict$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prev.predict), fit.burn.all)[,'Pr(>F)'][2])))
  # burn.model.eval.eight[burn.model.eval.eight$anova.all == max(burn.model.eval.eight$anova.all), ]
  # 
  # burn.model.eval.nine <- do.call(rbind, lapply(1:length(combos.nine[,'Combo']), function(x) data.frame(Model = combos.nine[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict)), prev.predict$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10, 4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict))$coeff[2:10,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.nine[x,]), sep='~')), prev.predict), fit.burn.all)[,'Pr(>F)'][2])))
  # burn.model.eval.nine[burn.model.eval.nine$anova.all == max(burn.model.eval.nine$anova.all), ]
  # 
  # burn.model.eval.ten <- do.call(rbind, lapply(1:length(combos.ten[,'Combo']), function(x) data.frame(Model = combos.ten[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict)), prev.predict$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.ten[x,]), sep='~')), prev.predict), fit.burn.all)[,'Pr(>F)'][2])))
  # burn.model.eval.ten[burn.model.eval.ten$anova.all == max(burn.model.eval.ten$anova.all), ]
  
  burn.model.eval.eleven <- do.call(rbind, lapply(1:length(combos.eleven[,'Combo']), function(x) data.frame(Model = combos.eleven[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict)), prev.predict$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict))$coeff[2:12,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict))$coeff[2:11,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.eleven[x,]), sep='~')), prev.predict), fit.burn.all)[,'Pr(>F)'][2])))
  burn.model.eval.eleven[burn.model.eval.eleven$anova.all == max(burn.model.eval.eleven$anova.all), ]
  # burn.model.eval.eleven is "good enough"
  
  ili.rate.regression <- predict(lm(as.formula(paste('Rate', as.character(ili.model.eval.nine[ili.model.eval.nine$anova.all == max(ili.model.eval.nine$anova.all), 'Model']), sep='~')), data=prev.predict), prev.predict)
  burn.rate.regression <- predict(lm(as.formula(paste('NormalizedBurn', as.character(burn.model.eval.eleven[burn.model.eval.eleven$anova.all == max(burn.model.eval.eleven$anova.all), 'Model']), sep='~')), data=prev.predict), prev.predict)
  prev.modeled <- data.frame(prev.modeled, iliReg = ili.rate.regression, burnReg = burn.rate.regression)
  
  # make some charts of actual vs. model overlays... chart R2 maybe
  
  # RESULTS WILL CHANGE (OR CAN) EVERYTIME THE CODE IS RUN
  # 20161214
  # ILI --------
  # 9 model is good enough with p-value = 8.75e-1 | B. pertussis, CoV 229E, CoV HKU1, HRV/Entero, Flu B, PIV 1, PIV 4, RSV, Flu A (last two times running, the results have remained the same)
  # B. pertussis, CoV 229E, CoV HKU1, and PIV 4 have negative coeffs, 
  # t values in order (high - low): Flu A, RSV, HRV/Entero, Flu B, PIV 1 (positive), then CoV 229E, PIV 4, B. pertussis, CoV HKU1 (negative) (determines the significance of a regression coefficient by comparing the sample and hypothesized mean or target value... the procedure compare the data to what is expected under the null hypothesis... for our test with ~151 data points, the 99% confidence interval indicates that we expect the sample estimate to include the population parameter 99% of the time with a t-value of 2.326, 95% requires a t-value of 1.960)
  # Std. Error in order (low - high): HRV/Entero, Flu A, RSV, Flu B, PIV 1, CoV 229E, PIV 4, CoV HKU1, B. pertussis (goodness of fit of line made with estimated coefficient vs. actual value)
  # BURN -------
  # 11 model is good enough with p-value = 5.46e-1 | B. pertussis, C. pneumoniae, hMPV, HRV/Entero, Flu B, M. pneumoniae, PIV 1, PIV 2, PIV 4, RSV, Flu A (all)
  # B. pertussis, PIV 4, Flu B, PIV 2, and C. pneumoniae have negative coeffs,
  # t values in order (high - low): Flu A, HRV/Entero, hMPV, RSV, M. pneumoniae, PIV 1, C. pnuemoniae, PIV 2, PIV 4, B. pertussis, Flu B (2.326, -2.326 are 99% confidence intervals such that only C. pnueomniae sample not 99% of sample estimates are likely to be within the population value)
  # Std. Error in order (low - high): HRV/Entero, RSV, Flu A, hMPV, Flu B, PIV 2, PIV 1, M. pneuomoniae, PIV 4, B. pertussis, C. pneumoniae (only first 3 have st. error < 1)
  
  # summary(lm(as.formula(paste('Rate', as.character(ili.model.eval.ten[Kili.model.eval.ten$anova.all == max(ili.model.eval.ten$anova.all), 'Model']), sep='~')), data=prev.predict))
  # summary(lm(as.formula(paste('NormalizedBurn', as.character(burn.model.eval.ten[burn.model.eval.ten$anova.all == max(burn.model.eval.ten$anova.all), 'Model']), sep='~')), data=prev.predict))
  # 
  a <- summary(lm(as.formula(paste('Rate', as.character(ili.model.eval.nine[ili.model.eval.nine$anova.all == max(ili.model.eval.nine$anova.all), 'Model']), sep='~')), data=prev.predict))$coeff
  a <- a[row.names(a) != '(Intercept)', ]
  a <- merge(data.frame(a, Code = row.names(a)), decoder.agg, by='Code')
  a <- a[order(a[,'Pr...t..']), ]
  
  b <- summary(lm(as.formula(paste('NormalizedBurn', as.character(burn.model.eval.eleven[burn.model.eval.eleven$anova.all == max(burn.model.eval.eleven$anova.all), 'Model']), sep='~')), data=prev.predict))$coeff
  b <- b[row.names(b) != '(Intercept)', ]
  b <- merge(data.frame(b, Code = row.names(b)), decoder.agg, by='Code')
  b <- b[order(b[,'Pr...t..']), ]
  
  a <- a[,c('Bug','Estimate','Std..Error','t.value','Pr...t..')]
  b <- b[,c('Bug','Estimate','Std..Error','t.value','Pr...t..')]
}

# FOR YAREMA
if(FALSE) {
  p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(Bug, decreasing=TRUE)),], as.character(ShortName)!='HRV/EV' ), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=Bug, group=Bug), stat='identity', position='stack') + scale_fill_manual(values=createPaletteOfVariableLength(prevalence.nat.individual.wrap, 'Bug'), name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(label=percent, limits=c(0,0.8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank())  + labs(title='National Percent Detection of Organisms with ILI Overlay', y='Percent Detection', x='Date') + geom_line(aes(x=YearWeek, y=15*Rate, group='Reported ILI'), data=subset(ili.burn.nat, YearWeek %in% unique(prevalence.nat.individual.wrap$YearWeek)), color='black', lwd=1.5)
  p2 <- ggplot(subset(ili.burn.nat,YearWeek %in% prevalence.nat.individual.wrap$YearWeek), aes(x=YearWeek, y=15*Rate, group='Reported ILI', color='Reported ILI')) + geom_line(lwd=1.5, color='black') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0, 0.8), breaks=c(0.0, 0.2, 0.4, 0.6, 0.8), labels=c('1%','2%','3%','4%','5%')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI Prevalence (CDC)')
  
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  
  # Get the location of the plot panel in g1.
  # These are used later when transformed elements of g2 are put back into g1
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  
  # Overlap panel for second plot on that of the first plot
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Get the y axis title from g2
  # index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
  index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?
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
  yarema.all <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/Yarema_Subset.png', height=800, width=1400)
  grid.draw(yarema.all)
  dev.off()
}

# FOR LAB MEETING
if(FALSE) {
  negatives.df <- runs.df[!(runs.df$RunDataId %in% bugs.df$RunDataId), ]
  negatives.df <- merge(negatives.df, calendar.df[,c('Date','YearWeek')], by='Date')
  negatives.agg <- with(data.frame(negatives.df, Negatives = 1), aggregate(Negatives~YearWeek+CustomerSiteId, FUN=sum))
  negatives.fill <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(merge(data.frame(YearWeek = unique(calendar.df$YearWeek)), negatives.agg[negatives.agg$CustomerSiteId==sites[x], c('YearWeek','Negatives')], by='YearWeek', all.x=TRUE), CustomerSiteId = sites[x])))
  negatives.fill[is.na(negatives.fill$Negatives), 'Negatives'] <- 0
  neg.periods <- as.character(unique(negatives.fill$YearWeek))[order(as.character(unique(negatives.fill$YearWeek)))]
  negatives.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(2:(length(neg.periods)-1), function(y) data.frame(YearWeek = neg.periods[y], CustomerSiteId = sites[x], Negatives = sum(negatives.fill[negatives.fill$CustomerSiteId==sites[x], 'Negatives'][(y-1):(y+1)]))))))
  negatives.roll[is.na(negatives.roll$Negatives), 'Negatives'] <- 0
  negatives.prev <- merge(runs.reg.roll, negatives.roll, by=c('YearWeek','CustomerSiteId'))
  negatives.prev$NegRate <- with(negatives.prev, Negatives/Runs)
  neg.burn.df <- merge(runs.reg.norm[,c('YearWeek','CustomerSiteId','NormalizedBurn')], negatives.prev, by=c('YearWeek','CustomerSiteId'))
  p.NegativesAndTURNBySite <- ggplot(neg.burn.df, aes(x=YearWeek, y=NormalizedBurn, group='TURN', color='TURN')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=10*NegRate, group='NegRate', color='NegRate'), lwd=1.5, data=neg.burn.df) + facet_wrap(~CustomerSiteId) + scale_color_manual(values=c('black','purple'), name='') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + labs(y='TURN, 10*NegRate', x='Date') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels)
  neg.burn.df$TURNByNegs <- with(neg.burn.df, NegRate*NormalizedBurn)
  p.NegativesFactoredByTURNBySite <- ggplot(neg.burn.df, aes(x=YearWeek, y=TURNByNegs, group='TURN Factored by Negative Test Rate')) + geom_line(color='black', lwd=1.5) + facet_wrap(~CustomerSiteId) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + labs(y='TURN*NegRate', x='Date') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels)
  neg.burn.nat <- with(neg.burn.df, aggregate(cbind(NegRate, NormalizedBurn, TURNByNegs)~YearWeek, FUN=mean))
  p.NegativesFactoredByTURN <- ggplot(neg.burn.nat, aes(x=YearWeek, y=TURNByNegs, group='TURN Factored by Negative Test Rate')) + geom_line(color='black', lwd=1.5) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + labs(y='TURN*NegRate', x='Date') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels)
  
  prevalence.cdc.fluA <- read.csv('../DataSources/RegionalInfluenzaByType.csv', header=TRUE, sep=',')
  prevalence.cdc.fluA <- data.frame(YearWeek = with(prevalence.cdc.fluA, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-'))), Region = prevalence.cdc.fluA$REGION, TotalPatients = prevalence.cdc.fluA$TOTAL.SPECIMENS, TotalFluObservations = prevalence.cdc.fluA$TOTAL.A)
  prevalence.cdc.fluA <- do.call(rbind, lapply(1:length(unique(prevalence.cdc.fluA$Region)), function(x) data.frame(YearWeek = prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'YearWeek'][2:(length(prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'YearWeek'])-1)], Region = unique(prevalence.cdc.fluA$Region)[x], TotalPatients = sapply(2:(length(prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'YearWeek'])-1), function(y) sum(prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'TotalPatients'][(y-1):(y+1)])), TotalFluObservations = sapply(2:(length(prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'YearWeek'])-1), function(y) sum(prevalence.cdc.fluA[prevalence.cdc.fluA$Region == unique(prevalence.cdc.fluA$Region)[x],'TotalFluObservations'][(y-1):(y+1)])))))
  prevalence.cdc.fluA$Prevalence <- with(prevalence.cdc.fluA, TotalFluObservations/TotalPatients)
  prevalence.cdc.fluA <- with(prevalence.cdc.fluA, aggregate(Prevalence~YearWeek, FUN=mean))
  prev.cdc.bfdx.fluA <- merge(prevalence.cdc.fluA, prevalence.nat.fluA, by='YearWeek')
  corAnnotation <- as.character(round(with(prev.cdc.bfdx.fluA, cor(Prevalence.y, Prevalence.x)), 3))
  summary(lm(Prevalence.y~Prevalence.x, data=prev.cdc.bfdx.fluA))
  rSqAnnotation <- '0.985'
  
  ggplot(prev.cdc.bfdx.fluA, aes(x=YearWeek, y=Prevalence.x, fill='CDC Flu A Prevalence')) + geom_bar(stat='identity') + geom_line(aes(x=YearWeek, y=Prevalence.y, group='FilmArray Flu A Detection', color='FilmArray Flu A Detection'), prev.cdc.bfdx.fluA, lwd=1.5) + scale_fill_manual(values=c('dodgerblue'), name='') + scale_color_manual(values=c('black'), name='') + scale_y_continuous(label=percent) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection, Flu Prevalence', x='Date') + scale_x_discrete(breaks=c('2015-41','2016-01','2016-14','2016-26','2016-40','2017-01'), labels=c('Oct-2015','Jan-2016','Mar-2016','Jul-2016','Oct-2016','Jan-2017')) + annotate('text', x='2016-35', y=0.15, label=paste('R2 = ', rSqAnnotation, sep=''), size=6)  + annotate('text', x='2016-35', y=0.14, label=paste('Cor = ', corAnnotation, sep=''), size=6)
}

# FOR STEFAN
if(FALSE) {
  
  avg.prev.northwell <- with(subset(prevalence.reg.wrap, CustomerSiteId %in% unique(runs.df[runs.df$Name=='North Shore LIJ Health System ', 'CustomerSiteId'])), aggregate(Prevalence~YearWeek+ShortName, FUN=mean))
  p.PercentDetectionTrend_Northwell <- ggplot(avg.prev.northwell[with(avg.prev.northwell, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,1), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')
}

# PRINT OUT ALL THE FIGURES
plots <- ls()[grep('^p\\.',ls())]
for(i in 1:length(plots)) {

  imgName <- paste(substring(plots[i],3),'.png',sep='')
  png(file=paste('Figures', imgName, sep='/'), width=1200, height=800, units='px')
  print(eval(parse(text = plots[i])))
  dev.off()
}