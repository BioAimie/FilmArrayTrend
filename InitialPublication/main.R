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
  require(dateManip)
  
  # load custom functions
  source('../Rfunctions/normalizeBurnRate.R')
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
  queryVector <- scan('../DataSources/AllSitesRespiratoryTrendableRuns.txt',what=character(),quote="")
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
}

# PREVALENCE OF ORGANISMS - PARETO-ISH TYPE CHARTS
if(FALSE) {
  
  start.year <- 2014
  
  # use data from all time and show a pareto of prevalence (collapsing fluA, coronas, pivs, and bacterias)
  positives.count.trim <- positives.count.all[as.character(positives.count.all$YearWeek) >= '2014-01', ]
  
  # need to sum up flu As, CoVs, PIVs, and Bacterias by customer site Id... then join these onto the positives.count.trim data frame
  positives.count.fluas <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'v', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% fluAs,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')], by=c('YearWeek','CustomerSiteId'))
  positives.count.covs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'w', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% corona,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')], by=c('YearWeek','CustomerSiteId'))
  positives.count.pivs <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'x', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% pivs,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')], by=c('YearWeek','CustomerSiteId'))
  positives.count.bacteria <- merge(do.call(rbind, lapply(1:length(unique(positives.count.trim$CustomerSiteId)), function(x) do.call(rbind, lapply(1:length(unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])), function(y) data.frame(YearWeek = unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], CustomerSiteId = unique(positives.count.trim$CustomerSiteId)[x], Code = 'y', Positives = sum(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x] & positives.count.trim$Code %in% as.character(decoder[decoder$Bug %in% bacterias,'Code']) & positives.count.trim$YearWeek == unique(positives.count.trim[positives.count.trim$CustomerSiteId==unique(positives.count.trim$CustomerSiteId)[x],'YearWeek'])[y], 'Positives'])))))), prevalence.reg.agg[,c('YearWeek','CustomerSiteId','Runs')], by=c('YearWeek','CustomerSiteId'))
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
  prev.pareto.all$Prevalence <- with(prev.pareto.all, Positives/Runs)
  
  # start with all data from the 8 sites starting in 2014-present showing all organisms and then grouping by family.
  prev.pareto.all.nat <- with(prev.pareto.all, aggregate(Prevalence~ShortName+Code, FUN=mean))
  prev.pareto.all.nat.ind <- prev.pareto.all.nat[!(prev.pareto.all.nat$Code %in% c('v','w','x','y')), ]
  prev.pareto.all.nat.fam <- prev.pareto.all.nat[!(prev.pareto.all.nat$Code %in% c('b','c','d','e','f','g','j','k','l','m','n','p','q','r','s','t')), ]
  label.order.all <- prev.pareto.all.nat[with(prev.pareto.all.nat, order(Prevalence, decreasing = TRUE)), 'ShortName']
  label.order.all <- label.order.all[c(1,2,3,6,16,17,18,4,5,10,14,19,20,7,9,13,21,24,25,8,11,15,22,23,12)]
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
  prev.pareto.all.nat.pop <- with(prev.pareto.all.pop, aggregate(Prevalence~ShortName+Code+Key, FUN=mean))
  prev.pareto.all.nat.pop$Name <- factor(prev.pareto.all.nat.pop$ShortName, levels=label.order.all)
  prev.pareto.all.nat.pop.ind <- prev.pareto.all.nat.pop[!(prev.pareto.all.nat.pop$Code %in% c('v','w','x','y')), ]
  prev.pareto.all.nat.pop.fam <- prev.pareto.all.nat.pop[!(prev.pareto.all.nat.pop$Code %in% c('b','c','d','e','f','g','j','k','l','m','n','p','q','r','s','t')), ]
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
  prev.pareto.seasonal.all$Prevalence <- with(prev.pareto.seasonal.all, Positives/Runs)
  
  # start with all data from summer 2013-present showing all organisms and then grouping by family.
  prev.pareto.seasonal.all.nat <- with(prev.pareto.seasonal.all, aggregate(Prevalence~ShortName+Code, FUN=mean))
  prev.pareto.seasonal.all.nat.ind <- prev.pareto.seasonal.all.nat[!(prev.pareto.seasonal.all.nat$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.nat.fam <- prev.pareto.seasonal.all.nat[!(prev.pareto.seasonal.all.nat$Code %in% c('b','c','d','e','f','g','j','k','l','m','o','p','q','r','s','t')), ]
  label.order.seasonal.all <- prev.pareto.seasonal.all.nat[with(prev.pareto.seasonal.all.nat, order(Prevalence, decreasing = TRUE)), 'ShortName']
  label.order.seasonal.all <- label.order.seasonal.all[c(1,2,3,7,14,17,18,4,5,11,16,19,20,6,9,12,21,24,8,10,15,22,23,13)]
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
  prev.pareto.seasonal.all.nat.pop <- with(prev.pareto.seasonal.all.pop, aggregate(Prevalence~ShortName+Code+Key, FUN=mean))
  prev.pareto.seasonal.all.nat.pop$Name <- factor(prev.pareto.seasonal.all.nat.pop$ShortName, levels=label.order.seasonal.all)
  prev.pareto.seasonal.all.nat.pop.ind <- prev.pareto.seasonal.all.nat.pop[!(prev.pareto.seasonal.all.nat.pop$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.nat.pop.fam <- prev.pareto.seasonal.all.nat.pop[!(prev.pareto.seasonal.all.nat.pop$Code %in% c('b','c','d','e','f','g','j','k','l','m','n','p','q','r','s','t')), ]
  prev.pareto.seasonal.all.nat.pop.ind$Name <- factor(prev.pareto.seasonal.all.nat.pop.ind$ShortName, levels=label.order.season.ind)
  prev.pareto.seasonal.all.nat.pop.fam$Name <- factor(prev.pareto.seasonal.all.nat.pop.fam$ShortName, levels=label.order.seasonal.fam)
  
  p.PercentDetectionParetoByPopulationSeasonal <- ggplot(prev.pareto.seasonal.all.nat.pop, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection')
  p.PercentDetectionParetoByPopulationSeasonal_Individual <- ggplot(prev.pareto.seasonal.all.nat.pop.ind, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoByPopulationSeasonal_Family <- ggplot(prev.pareto.seasonal.all.nat.pop.fam, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.pop, 'Key'), name='') + scale_y_continuous(limits=c(0,0.35), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35), labels=c('0','5','10','15','20','25','30','35')) + labs(x='', y='Detection (%)')
  
  # subset by year (2014, 2015, 2016)
  prev.pareto.seasonal.all.year <- with(prev.pareto.seasonal.all, aggregate(Prevalence~SeasonYear+ShortName+Code, FUN=mean))
  prev.pareto.seasonal.all.year$Name <- factor(prev.pareto.seasonal.all.year$ShortName, levels = label.order.seasonal.all)
  prev.pareto.seasonal.all.year.ind <- prev.pareto.seasonal.all.year[!(prev.pareto.seasonal.all.year$Code %in% c('v','w','x','y')), ]
  prev.pareto.seasonal.all.year.fam <- prev.pareto.seasonal.all.year[!(prev.pareto.seasonal.all.year$Code %in% c('b','c','d','e','f','g','j','k','l','m','n','p','q','r','s','t')), ]
  prev.pareto.seasonal.all.year.ind$Name <- factor(prev.pareto.seasonal.all.year.ind$ShortName, levels = label.order.season.ind)
  prev.pareto.seasonal.all.year.fam$Name <- factor(prev.pareto.seasonal.all.year.fam$ShortName, levels = label.order.seasonal.fam)
  
  p.PercentDetectionParetoAnnualSeasonal <- ggplot(subset(prev.pareto.seasonal.all.year, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoAnnualSeasonal_Individual <- ggplot(subset(prev.pareto.seasonal.all.year.ind, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  p.PercentDetectionParetoAnnualSeasonal_Family <- ggplot(subset(prev.pareto.seasonal.all.year.fam, SeasonYear!='2016-2017'), aes(x=Name, y=Prevalence, fill=SeasonYear)) + geom_bar(stat='identity', position='dodge') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.seasonal.all.year, 'SeasonYear'), name='') + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')
  
  # make a table using the seasonal year data
  prev.table.seasonal.all <- do.call(cbind, lapply(1:length(unique(prev.pareto.seasonal.all.year$SeasonYear)), function(x) data.frame(ShortName = prev.pareto.seasonal.all.year[prev.pareto.seasonal.all.year$SeasonYear==unique(prev.pareto.seasonal.all.year$SeasonYear)[x],'ShortName'], Prevalence = prev.pareto.seasonal.all.year[prev.pareto.seasonal.all.year$SeasonYear==unique(prev.pareto.seasonal.all.year$SeasonYear)[x],'Prevalence'])))
  prev.table.seasonal.all.pop <- do.call(cbind, lapply(1:length(unique(prev.pareto.seasonal.all.nat.pop$Key)), function(x) data.frame(Key = unique(prev.pareto.seasonal.all.nat.pop$Key)[x], ShortName = prev.pareto.seasonal.all.nat.pop[prev.pareto.seasonal.all.nat.pop$Key == unique(prev.pareto.seasonal.all.nat.pop$Key)[x], 'ShortName'], Prevalence = prev.pareto.seasonal.all.nat.pop[prev.pareto.seasonal.all.nat.pop$Key == unique(prev.pareto.seasonal.all.nat.pop$Key)[x],'Prevalence'])))
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

  # # This is some code to check out the BURN rate vs. Run rates so that we can see if there are weird sites and how the algorithm may need to be adjusted  
  # ggplot(runs.reg.norm, aes(x=YearWeek, y=NormRollRateByInst, group=as.factor(CustomerSiteId), color='NormRunByInst')) + geom_line() + geom_line(aes(x=YearWeek, y=NormRollRate, group=as.factor(CustomerSiteId), color='NormRun'), data=runs.reg.norm) + geom_line(aes(x=YearWeek, y=NormalizedBurn, group=as.factor(CustomerSiteId), color='Burn'), data=runs.reg.norm) + facet_wrap(~CustomerSiteId, scale='free_y') + scale_x_discrete(breaks=dateBreaks) + theme(axis.text.x=element_text(angle=90)) + scale_color_manual(values=c('red','blue','black'))
  # ggplot(runs.reg.norm, aes(x=YearWeek, y=RollRate, group=as.factor(CustomerSiteId), color='RunRate')) + geom_line() + geom_line(aes(x=YearWeek, y=RollRateByInst, group=as.factor(CustomerSiteId), color='RunRateByInst'), data=runs.reg.norm) + geom_line(aes(x=YearWeek, y=8*NormalizedBurn, group=as.factor(CustomerSiteId), color='Burn'), data=runs.reg.norm) + facet_wrap(~CustomerSiteId, scale='free_y') + scale_x_discrete(breaks=dateBreaks) + theme(axis.text.x=element_text(angle=90)) + scale_color_manual(values=c('red','blue','black'))
  
  p1 <- ggplot(subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), aes(x=YearWeek, y=Rate, color='CDC ILI', group='CDC ILI')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=NormalizedBurn/100, group='FilmArray Utilization', color='FilmArray Utilization'), subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), lwd=1.5) + scale_color_manual(values=c('black','red'), name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.05), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, face='bold', color='black'), axis.text.x=element_text(angle=90, hjust=1), axis.ticks.x=element_blank(), panel.background=element_rect(fill='white', color='white'), legend.position = 'bottom') + labs(x='Date', y='ILI (%)')
  p2 <- ggplot(subset(ili.burn.nat, as.character(YearWeek) > '2013-26'), aes(x=YearWeek, y=NormalizedBurn/100, group='FilmArray Utilization', color='FilmArray Utilization')) + geom_line(lwd=1.5, color='red') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0, 0.05), breaks=c(0.01, 0.02, 0.03, 0.04, 0.05), labels=c('1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Utilization')
  
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
  bfdx.flu.reg <- with(prevalence.reg.count, aggregate(cbind(Runs, j, k, l, m, n, o)~YearWeek+Region+CustomerSiteId, FUN=sum))
  bfdx.flu.reg$FluDetections <- with(bfdx.flu.reg, j+k+l+m+n+o)

  # CDC - Clinical Labs (only has data from 2015+)
  cdc.flu.reg <- read.csv('../DataSources/RegionalInfluenzaByType.csv', header=TRUE, sep=',')
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
  p1 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek, y=FluPercentDetection, group='FilmArrary Detection', color='FilmArray Detection')) + geom_line(size=2) + geom_line(aes(x=YearWeek, y=FluPrevalence, group='CDC Flu Prevalence', color='CDC Flu Prevalence'), cdc.bfdx.flu.nat, lwd=1.5)  + geom_line(aes(x=YearWeek, y=10*Rate, group='CDC ILI Rate', color='CDC ILI Rate'), cdc.bfdx.flu.nat, lwd=1.5) + geom_line(aes(x=YearWeek, y=NormalizedBurn/10, group='FilmArray Utilization', color='FilmArray Utilization'), cdc.bfdx.flu.nat, lwd=1.5) + scale_color_manual(values=c('black','blue','red','darkgreen'), name='') + scale_y_continuous(breaks=c(0,.07,0.14,0.21,0.28,0.35), limits=c(0,0.35), labels=c(0, 7, 14, 21, 28, 35)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='FilmArray Detection (%), Flu Prevalence (%)', x='Date')
  p2 <- ggplot(cdc.bfdx.flu.nat, aes(x=YearWeek)) + scale_x_discrete(breaks = dateBreaksAlt2, labels = dateLabelsAlt2) + scale_y_continuous(limits=c(0,0.35), breaks=c(0, 0.07, 0.14, 0.21, 0.28, 0.35), labels=c('0.0','0.7','1.4','2.1','2.8','3.5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%), FilmArray Utilization')
  
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
  
  # make time series percent detection by organism family with ILI and Normalized Utilization Overlays
  prevalence.nat.individual.wrap <- prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, YearWeek)), ]
  # - FLU As------------------------------------------------------------------------------------------------------
  if(TRUE) {
    p1 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluAs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.25), breaks=c(0, 0.05, 0.10, 0.15, 0.2, 0.25), labels=c(0, 5, 10, 15, 20, 25)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='Detection (%)', x='Date')
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluAs), aes(x=YearWeek, y=5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.25), breaks=c(0, 0.05, 0.10, 0.15, 0.2, 0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluBs), aes(x=YearWeek, y=2.5*Rate, group=1))  + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=2.5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rsv), aes(x=YearWeek, y=5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.25), breaks=c(0, 0.05, 0.1, 0.15, 0.2, 0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% pivs), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% corona), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rhino), aes(x=YearWeek, y=12*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=12*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.6), breaks=c(0, 0.12, 0.24, 0.36, 0.48, 0.60), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% adeno), aes(x=YearWeek, y=3*Rate, group=1))+ geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% hmp), aes(x=YearWeek, y=3*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=3*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.15), breaks=c(0, 0.03, 0.06, 0.09, 0.12, 0.15), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
    p2 <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% bacterias), aes(x=YearWeek, y=2.5*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=2.5*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2013-26'), color='red', lwd=2) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0,0.125), breaks=c(0, 0.025, 0.05, 0.075, 0.1, 0.125), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black, %), FilmArray Utilization (red)')
    
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
  # # negatives.nat <- data.frame(Bug='Negatives', YearWeek = with(prevalence.nat.individual.wrap, aggregate(Prevalence~YearWeek, FUN=sum))$YearWeek, Prevalence = 1-with(prevalence.nat.individual.wrap, aggregate(Prevalence~YearWeek, FUN=sum))$Prevalence, Rate=with(prevalence.nat.individual.wrap, aggregate(Rate~YearWeek, FUN=mean))$Rate, ShortName='Negative')
  # if(FALSE) {
  # 
  #   p1 <- ggplot(negatives.nat, aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values='grey', name='') + scale_x_discrete(breaks = as.character(unique(negatives.nat$YearWeek))[order(as.character(unique(negatives.nat$YearWeek)))][seq(1, length(as.character(unique(negatives.nat$YearWeek))), 8)]) + scale_y_continuous(label=percent, breaks=c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank())  + labs(title='Percent Detection of Human Metapneumovirus in Trend Population with ILI Overlay', y='Percent Detection of Organism', x='Date')
  #   p2 <- ggplot(negatives.nat, aes(x=YearWeek, y=10*Rate, group=1)) + geom_line(color='black', lwd=2) + geom_line(aes(x=YearWeek, y=10*NormalizedBurn/100, group=2), subset(ili.burn.nat, as.character(ili.burn.nat$YearWeek) >='2014-01'), color='red', lwd=2) + scale_x_discrete(breaks = as.character(unique(negatives.nat$YearWeek))[order(as.character(unique(negatives.nat$YearWeek)))][seq(1, length(as.character(unique(negatives.nat$YearWeek))), 8)]) + scale_y_continuous(limits=c(0,10*max(negatives.nat$Rate)), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5), labels=c('0%','1%','2%','3%','4%','5%')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (black), FilmArray Test Utilization (red)')
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

# CO-DETECTIONS
if(TRUE) {
  
  # create a dual detection chart that will show all organisms broken out in order of highest -> lowest precent detection over all the data
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
  
  # make a nifty dual-axis chart
  p1 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=Prevalence)) + geom_bar(stat='identity') + scale_fill_manual(values='grey', guide=FALSE) + scale_y_continuous(limits=c(0,0.3), breaks=c(0,0.5,0.10,0.15,0.20,0.25,0.30), labels=c('0','5','10','15','20','25','30')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank()) + labs(y='Detection (%)', x='')
  p2 <- ggplot(prev.pareto.all.duals, aes(x=Name, y=5*PercentOfDuals, color='Percent of Dual Detections')) + geom_point(size=4) + scale_color_manual(values='black', guide=FALSE) + scale_y_continuous(limits=c(0,5*max(prev.pareto.all.duals$PercentOfDuals)), breaks=c(0, 0.05, 0.1, 0.15,0.2,0.25), labels=c('0','1','2','3','4','5')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank()) + labs(y='Co-Detection Occurrence (%)')
  
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
  paretoCoDets <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  
  # Draw it
  grid.newpage()
  png('Figures/PercentDetectionParetoWithCoDetections.png', height=800, width=1400)
  grid.draw(paretoCoDets)
  dev.off()
}

# REGRESSION ANALYSIS - ILI AND BURN vs. PERCENT DETECTION OF ORGANISMS
if(TRUE) {
  
  prev.predict <- with(prev.pareto.all, aggregate(Prevalence~YearWeek+Code, FUN=mean))
  prev.predict <- prev.predict[with(prev.predict, order(Code, YearWeek)), ]
  prev.predict <- data.frame(YearWeek = unique(prev.predict$YearWeek), do.call(cbind, lapply(1:length(unique(prev.predict$Code)), function(x) prev.predict[prev.predict$Code==unique(prev.predict$Code)[x],'Prevalence'])))
  colnames(prev.predict)[grep('X', colnames(prev.predict))] <- letters[1:(length(colnames(prev.predict))-1)]
  prev.predict <- merge(prev.predict, ili.burn.nat, by='YearWeek')
  prev.predict <- prev.predict[!(is.na(prev.predict$Rate)), ]
  
  fit.vars <- c('a','b','c','d','e','f','g','h','i','o','p','q','r','s','t','u','v')
 
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

  # read in model possibilities
  combos.eight <- read.csv('../DataSources/compactCombos8.csv', header=TRUE, sep=',')
  combos.nine <- read.csv('../DataSources/compactCombos9.csv', header=TRUE, sep=',')
  combos.ten <- read.csv('../DataSources/compactCombos10.csv', header=TRUE, sep=',')
  combos.eleven <- read.csv('../DataSources/compactCombos11.csv', header=TRUE, sep=',')
  # have to create eleven combos... ugh.
  
  # Model ILI with prevalence of diseases in FilmArray test population (p-value = 5.00e-2 is established stop)
  # ili.model.eval.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict)), prev.predict$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict))$coeff[2:9,4] <= 0.05), alpha1 = 8, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prev.predict), fit.ili.all)[,'Pr(>F)'][2])))
  # ili.model.eval.eight[ili.model.eval.eight$anova.all == max(ili.model.eval.eight$anova.all), ]
  
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

# REMAKE SOME OF THE PARETO CHARTS USING A WIEGHTING BY POPULATION OF THE REGION
a <- merge(positives.count.seasonal.agg, unique(runs.df[,c('CustomerSiteId','Province')]), by='CustomerSiteId')
a <- merge(a, unique(regions.df[,c('StateAbv','CensusRegionNational')]), by.x='Province', by.y='StateAbv')
# https://www.census.gov/popclock/data_tables.php?component=growth
population.dist.2016 <- data.frame(CensusRegionNational = c('Northeast','Midwest','West','South'),
                                   PopulationPercent = c(0.174, 0.21, 0.237, 0.379))
# a.annual <- with(a, aggregate(cbind(Runs, Positives)~SeasonYear+CustomerSiteId+CensusRegionNational+ShortName, FUN=sum))
# a.avg <- a
# a.annual$Rate <- with(a.annual, Positives/Runs)
# a.avg$Rate <- with(a, Positives/Runs)
# a.annual.agg <- with(a.annual, aggregate(Rate~SeasonYear+CustomerSiteId+CensusRegionNational+ShortName, FUN=mean))
# a.avg.agg <- with(a.avg, aggregate(Rate~SeasonYear+CustomerSiteId+CensusRegionNational+ShortName, FUN=mean))
# a.annual.census <- with(a.annual.agg, aggregate(Rate~SeasonYear+CensusRegionNational+ShortName, FUN=mean))
# a.avg.census <- with(a.avg.agg, aggregate(Rate~SeasonYear+CensusRegionNational+ShortName, FUN=mean))
# a.annual.census <- merge(a.annual.census, population.dist.2016, by='CensusRegionNational')
# a.avg.census <- merge(a.avg.census, population.dist.2016, by='CensusRegionNational')
# a.annual.census$WeightedRate <- with(a.annual.census, Rate*PopulationPercent)
# a.avg.census$WeightedRate <- with(a.avg.census, Rate*PopulationPercent)
# a.annual.census.agg <- with(a.annual.census, aggregate(WeightedRate~SeasonYear+ShortName, FUN=sum))
# a.avg.census.agg <- with(a.avg.census, aggregate(WeightedRate~SeasonYear+ShortName, FUN=sum))
a.annual <- with(a, aggregate(cbind(Runs, Positives)~CustomerSiteId+CensusRegionNational+ShortName, FUN=sum))
a.avg <- a
a.annual$Rate <- with(a.annual, Positives/Runs)
a.avg$Rate <- with(a, Positives/Runs)
a.annual.agg <- with(a.annual, aggregate(Rate~CustomerSiteId+CensusRegionNational+ShortName, FUN=mean))
a.avg.agg <- with(a.avg, aggregate(Rate~CustomerSiteId+CensusRegionNational+ShortName, FUN=mean))
a.annual.census <- with(a.annual.agg, aggregate(Rate~CensusRegionNational+ShortName, FUN=mean))
a.avg.census <- with(a.avg.agg, aggregate(Rate~CensusRegionNational+ShortName, FUN=mean))
a.annual.census <- merge(a.annual.census, population.dist.2016, by='CensusRegionNational')
a.avg.census <- merge(a.avg.census, population.dist.2016, by='CensusRegionNational')
a.annual.census$WeightedRate <- with(a.annual.census, Rate*PopulationPercent)
a.avg.census$WeightedRate <- with(a.avg.census, Rate*PopulationPercent)
a.annual.census.agg <- with(a.annual.census, aggregate(WeightedRate~ShortName, FUN=sum))
a.avg.census.agg <- with(a.avg.census, aggregate(WeightedRate~ShortName, FUN=sum))
a.annual.census.agg$Name <- factor(a.annual.census.agg$ShortName, levels=a.annual.census.agg[with(a.annual.census.agg, order(WeightedRate, decreasing = TRUE)),'ShortName'])
a.avg.census.agg$Name <- factor(a.avg.census.agg$ShortName, levels=a.avg.census.agg[with(a.avg.census.agg, order(WeightedRate, decreasing = TRUE)),'ShortName'])
shortnames.grouped <- c('Adeno','Bacteria (all)','CoV (all)','Flu A (all)','FluB','hMPV','HRV/EV','PIV (all)','RSV')
p.a.cdc <- ggplot(subset(a.annual.census.agg, ShortName %in% shortnames.grouped), aes(x=Name, y=WeightedRate, fill='Name')) + geom_bar(stat='identity') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(data.frame(Name = 'Name'), 'Name'), guide=FALSE) + scale_y_continuous(limits=c(0,0.30), breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3), labels=c('0','5','10','15','20','25','30')) + labs(x='', y='Detection (%)')



# PRINT OUT ALL THE FIGURES
plots <- ls()[grep('^p\\.',ls())]
for(i in 1:length(plots)) {
  
  imgName <- paste(substring(plots[i],3),'.png',sep='')
  png(file=paste('Figures', imgName, sep='/'), width=1200, height=800, units='px')
  print(eval(parse(text = plots[i])))
  dev.off()
}
