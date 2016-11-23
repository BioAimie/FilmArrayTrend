# set the path and load libraries and functions
if(TRUE) {
  workDir <-'G:/Departments/PostMarket/DataScienceGroup/Data Science Products/InProcess/Aimie/20160721_TrendExternal2016/IDWeek/'
  setwd(workDir)
  
  # load libraries
  library(RODBC)
  library(lubridate)
  library(ggplot2)
  library(grid)
  library(scales)
  library(GGally)
  library(RColorBrewer)
  library(car)
  library(caret)
  library(rpart)
  library(party)
  library(randomForest)
  library(nnet)
  library(devtools)
  library(RCurl)
  
  # load user-defined functions
  source('../ArchivedFunctions/makeEvenWeeks.R')
  source('../NewFunctions/normalizeBurnRate.R')
  source('../NewFunctions/rollPositivesBySite.R')
  source('../NewFunctions/generateCombos.R')
  source('../NewFunctions/createPaletteOfVariableLength.R')
  # source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
  # source_url('https://gist.githubusercontent.com/fawda123/6206737/raw/d6f365c283a8cae23fb20892dc223bc5764d50c7/gar_fun.r')
  # root.url<-'https://gist.githubusercontent.com/fawda123'
  # raw.fun<-paste(
  #   root.url,
  #   '5086859/raw/cc1544804d5027d82b70e74b83b3941cd2184354/nnet_plot_fun.r',
  #   sep='/'
  # )
  # script<-getURL(raw.fun, ssl.verifypeer = FALSE)
  # eval(parse(text = script))
  # rm('script','raw.fun')
}

# GET DATA ---------------------------------------------------------------------------------------------------------------
if(TRUE) {
  # read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
  FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
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
  queryVector <- scan('../DataSources/calendarDates.txt',what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  calendar.df <- sqlQuery(PMScxn,query)
  odbcClose(PMScxn)
  
  # read in data from Excel files
  cdc.reg.df <- read.csv('../DataSources/RegionalILI.csv', header=TRUE, sep=',')
  # ------------------------------------------------------------------------------------------------------------------------
}

# add regions to the runs.df data frame as well as "even" weeks and years
calendar.df <- makeEvenWeeks(calendar.df)
runs.reg <- merge(runs.df, data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal), by='Province')
runs.reg$Record <- 1
runs.reg.date <- merge(runs.reg[,c('RunDataId','Instrument','Date','Name','CustomerSiteId','Region','Record')], calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')

# create a data frame that will show site name and id with its region and census region
sitesByCensusRegions.etc <- unique(merge(unique(runs.reg.date[,c('CustomerSiteId','Name','Region')]), regions.df, by.x='Region', by.y='CensusRegionLocal')[,c('CustomerSiteId','Name','Region','CensusRegionNational')])
# midwest: southbend (2), detroit (29), nationwide (13), children's mercy (26)
# northeast: albany (10), winthrop (5), northshore (36, 39), stony brook (9)
# issues with data: Stony Brook (9) only has data starting in April of 2016... so this is potentially problematic...

# clean up the data frame that holds the cdc regional data
cdc.reg.df <- data.frame(Year = cdc.reg.df$YEAR, Week = cdc.reg.df$WEEK, Region = cdc.reg.df$REGION, ILITotal = cdc.reg.df$ILITOTAL, TotalPatients = cdc.reg.df$TOTAL.PATIENTS)
cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
cdc.reg.count.df <- cdc.reg.count.df <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], TotalPatients = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])), ILITotal = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])))))
cdc.reg.rolled <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], Rate = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])/sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])))))

# determine which sites should be kept in the data set (i.e. what the start date is for each)... as of 9/12 at 10am, site 9 only has data for 2016
# starting in April... 
if(FALSE){

  sites <- unique(runs.reg.date$CustomerSiteId)
  site.starts <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    first.date <- min(runs.reg.date[runs.reg.date$CustomerSiteId == site,'Date'])
    temp <- data.frame(SiteId = site, StartDate = first.date)
    site.starts <- rbind(site.starts, temp)
  }
}

# -------------------------------------------------------- FIGURES FOR PUBLICATION -------------------------------------------------------------------------
# PREVALENCE OF ORGANISMS - PARETO-ISH TYPE CHARTS
if(TRUE) {
  
  start.year <- 2014
  
  # use data from all time and show a pareto of prevalence (collapsing fluA, coronas, pivs, and bacterias)
  prev.pareto.all <- data.frame(Year = unique(runs.reg.date$Year), TotalRuns = with(runs.reg.date, aggregate(Record~Year, FUN=sum))$Record)
  prev.pareto.all <- merge(prev.pareto.all, with(merge(runs.reg.date[, c('Year','RunDataId')], data.frame(RunDataId = bugs.df$RunDataId, Organism = bugs.df$BugPositive, Positive = 1), by='RunDataId'), aggregate(Positive~Year+Organism, FUN=sum)))
  prev.pareto.all <- subset(prev.pareto.all, Year >= start.year)
  prev.pareto.all <- with(prev.pareto.all, aggregate(cbind(TotalRuns, Positive)~Organism, FUN=sum))
  prev.pareto.all <- merge(prev.pareto.all, shortnames.df, by='Organism')
  prev.pareto.all <- rbind(prev.pareto.all, data.frame(with(prev.pareto.all[grep('Corona', prev.pareto.all$Organism),], aggregate(Positive~TotalRuns, FUN=sum)), Organism = 'Coronavirus', ShortName = 'Coronavirus (all)')[,c('TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.all <- rbind(prev.pareto.all, data.frame(with(prev.pareto.all[grep('Influenza A', prev.pareto.all$Organism),], aggregate(Positive~TotalRuns, FUN=sum)), Organism = 'Influenza A', ShortName = 'Influenza A (all)')[,c('TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.all <- rbind(prev.pareto.all, data.frame(with(prev.pareto.all[grep('Parainfluenza', prev.pareto.all$Organism),], aggregate(Positive~TotalRuns, FUN=sum)), Organism = 'Parainfluenza', ShortName = 'Parainfluenza (all)')[,c('TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.all <- rbind(prev.pareto.all, data.frame(with(prev.pareto.all[grep('pneumoniae|pertussis', prev.pareto.all$Organism),], aggregate(Positive~TotalRuns, FUN=sum)), Organism = 'Bacteria', ShortName = 'Bacteria (all)')[,c('TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.all$Prevalence <- with(prev.pareto.all, Positive/TotalRuns)
  # use this output to determine the order of the levels
  determine.order.all <- merge(prev.pareto.all, with(prev.pareto.all, aggregate(Positive~ShortName, FUN=sum)), by='ShortName')
  label.order.all <- as.character(unique(determine.order.all[with(determine.order.all, order(Positive.y, decreasing = TRUE)), 'ShortName']))  
  label.order.all <- label.order.all[c(1, 2, 3, 9, 10, 20, 24, 25, 4, 5, 8, 17, 18, 19, 6, 12, 14, 16, 21, 7, 11, 13, 15, 22, 23)]
  prev.pareto.all$Name <- factor(prev.pareto.all$ShortName, levels = label.order.all)
  prev.pareto.all$Key <- 'AllData'
  p.prev.pareto.all <- ggplot(prev.pareto.all, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all, 'Key'), guide=FALSE) + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims') + labs(x='')
  # create a version of the same chart, but collapse the cornonas, pivs, bacterias, and influenza As
  prev.pareto.all.collapsed <- prev.pareto.all[!(prev.pareto.all$ShortName %in% prev.pareto.all[grep('Flu A|FluA|PIV|CoV|pertussis|pneumo', prev.pareto.all$ShortName),'ShortName']), ]
  determine.order.all.collapsed <- merge(prev.pareto.all.collapsed, with(prev.pareto.all.collapsed, aggregate(Positive~ShortName, FUN=sum)), by='ShortName')
  label.order.all.collapsed <- as.character(unique(determine.order.all.collapsed[with(determine.order.all.collapsed, order(Positive.y, decreasing = TRUE)), 'ShortName']))  
  prev.pareto.all.collapsed$Name <- factor(prev.pareto.all.collapsed$ShortName, levels = label.order.all.collapsed)
  p.prev.pareto.all.collapsed <- ggplot(prev.pareto.all.collapsed, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all, 'Key'), guid=FALSE) + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims by Organism Type') + labs(x='')

  # subset the data into children's hospitals and mixed population hospitals
  sites.mixed <- unique(runs.reg.date[!(runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name)[grep('Children', unique(runs.reg.date$Name))])),'CustomerSiteId'])
  sites.child <- unique(runs.reg.date[runs.reg.date$Name %in% as.character(unique(runs.reg.date$Name))[grep('Children', unique(runs.reg.date$Name))],'CustomerSiteId'])
    # mixed
  prev.pareto.mixed <- data.frame(Year = unique(runs.reg.date$Year), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites.mixed), aggregate(Record~Year, FUN=sum))$Record)
  prev.pareto.mixed <- merge(prev.pareto.mixed, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites.mixed, c('Year','RunDataId')], data.frame(RunDataId = bugs.df$RunDataId, Organism = bugs.df$BugPositive, Positive = 1), by='RunDataId'), aggregate(Positive~Year+Organism, FUN=sum)), by='Year', all.x=TRUE)
  prev.pareto.mixed <- subset(prev.pareto.mixed, Year >= start.year)
  prev.pareto.mixed <- merge(prev.pareto.mixed, shortnames.df, by='Organism')
  prev.pareto.mixed <- rbind(prev.pareto.mixed, data.frame(with(prev.pareto.mixed[grep('Corona', prev.pareto.mixed$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Coronavirus', ShortName = 'Coronavirus (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.mixed <- rbind(prev.pareto.mixed, data.frame(with(prev.pareto.mixed[grep('Influenza A', prev.pareto.mixed$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Influenza', ShortName = 'Influenza A (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.mixed <- rbind(prev.pareto.mixed, data.frame(with(prev.pareto.mixed[grep('Parainfluenza', prev.pareto.mixed$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Parainfluenza', ShortName = 'Parainfluenza (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.mixed <- rbind(prev.pareto.mixed, data.frame(with(prev.pareto.mixed[grep('pneumoniae|pertussis', prev.pareto.mixed$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Bacteria', ShortName = 'Bacteria (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.mixed.all <- with(prev.pareto.mixed, aggregate(cbind(TotalRuns, Positive)~Organism+ShortName, FUN=sum))
  prev.pareto.mixed$Prevalence <- with(prev.pareto.mixed, Positive/TotalRuns)
  prev.pareto.mixed.all$Prevalence <- with(prev.pareto.mixed.all, Positive/TotalRuns)
  # childrens
  prev.pareto.child <- data.frame(Year = unique(runs.reg.date$Year), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites.child), aggregate(Record~Year, FUN=sum))$Record)
  prev.pareto.child <- merge(prev.pareto.child, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites.child, c('Year','RunDataId')], data.frame(RunDataId = bugs.df$RunDataId, Organism = bugs.df$BugPositive, Positive = 1), by='RunDataId'), aggregate(Positive~Year+Organism, FUN=sum)), by='Year', all.x=TRUE)
  prev.pareto.child <- subset(prev.pareto.child, Year >= start.year)
  prev.pareto.child <- merge(prev.pareto.child, shortnames.df, by='Organism')
  prev.pareto.child <- rbind(prev.pareto.child, data.frame(with(prev.pareto.child[grep('Corona', prev.pareto.child$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Coronavirus', ShortName = 'Coronavirus (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.child <- rbind(prev.pareto.child, data.frame(with(prev.pareto.child[grep('Influenza A', prev.pareto.child$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Influenza', ShortName = 'Influenza A (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.child <- rbind(prev.pareto.child, data.frame(with(prev.pareto.child[grep('Parainfluenza', prev.pareto.child$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Parainfluenza', ShortName = 'Parainfluenza (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.child <- rbind(prev.pareto.child, data.frame(with(prev.pareto.child[grep('pneumoniae|pertussis', prev.pareto.child$Organism),], aggregate(Positive~Year+TotalRuns, FUN=sum)), Organism = 'Bacteria', ShortName = 'Bacteria (all)')[,c('Year','TotalRuns','Organism','Positive','ShortName')])
  prev.pareto.child.all <- with(prev.pareto.child, aggregate(cbind(TotalRuns, Positive)~Organism+ShortName, FUN=sum))
  prev.pareto.child$Prevalence <- with(prev.pareto.child, Positive/TotalRuns)
  prev.pareto.child.all$Prevalence <- with(prev.pareto.child.all, Positive/TotalRuns)
  # make a chart that shows the prevalence of organisms in all sites year-over-year (both collapsed and expanded)
  prev.pareto.all.year <- rbind(prev.pareto.mixed, prev.pareto.child)
  prev.pareto.all.year <- with(prev.pareto.all.year, aggregate(cbind(TotalRuns, Positive)~Organism+ShortName+Year, FUN=sum))
  prev.pareto.all.year$Prevalence <- with(prev.pareto.all.year, Positive/TotalRuns)
  determine.order.all.year <- merge(prev.pareto.all.year, with(prev.pareto.all.year, aggregate(Positive~ShortName, FUN=sum)), by='ShortName')
  label.order.all.year <- as.character(unique(determine.order.all.year[with(determine.order.all.year, order(Positive.y, decreasing = TRUE)), 'ShortName']))  
  label.order.all.year <- label.order.all.year[c(1, 2, 3, 9, 10, 20, 24, 25, 4, 5, 8, 17, 18, 19, 6, 12, 14, 16, 21, 7, 11, 13, 15, 22, 23)]
  prev.pareto.all.year$Name <- factor(prev.pareto.all.year$ShortName, levels = label.order.all.year)
  p.prev.pareto.all.year <- ggplot(prev.pareto.all.year, aes(x=Name, y=Prevalence, fill=as.factor(Year))) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.year, 'Year'), guide=FALSE) + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims by Year') + labs(x='')
  # create a version of the same chart, but collapse the cornonas, pivs, bacterias, and influenza As
  prev.pareto.all.year.collapsed <- prev.pareto.all.year[!(prev.pareto.all.year$ShortName %in% prev.pareto.all.year[grep('Flu A|FluA|PIV|CoV|pertussis|pneumo', prev.pareto.all.year$ShortName),'ShortName']), ]
  determine.order.all.collapsed <- merge(prev.pareto.all.year.collapsed, with(prev.pareto.all.year.collapsed, aggregate(Positive~ShortName, FUN=sum)), by='ShortName')
  label.order.all.collapsed <- as.character(unique(determine.order.all.collapsed[with(determine.order.all.collapsed, order(Positive.y, decreasing = TRUE)), 'ShortName']))  
  prev.pareto.all.year.collapsed$Name <- factor(prev.pareto.all.year.collapsed$ShortName, levels = label.order.all.collapsed)
  p.prev.pareto.all.year.collapsed <- ggplot(prev.pareto.all.year.collapsed, aes(x=Name, y=Prevalence, fill=as.factor(Year))) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.year.collapsed, 'Year'), guid=FALSE) + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims by Organism Type and Year') + labs(x='')
  # make a chart that shows the difference in prevalence for children and mixed populations (both collapsed and expanded)
  prev.pareto.mixed.all$Key <- 'Peds+Adult'
  prev.pareto.mixed.all$Name <- factor(prev.pareto.mixed.all$ShortName, levels = label.order.all)
  prev.pareto.child.all$Key <- 'Pediatric'
  prev.pareto.child.all$Name <- factor(prev.pareto.child.all$ShortName, levels = label.order.all)
  prev.pareto.all.pop <- rbind(prev.pareto.mixed.all, prev.pareto.child.all)
  p.prev.all.pop <- ggplot(prev.pareto.all.pop, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.pop, 'Key'), name='') + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims by Hospital Population Type') + labs(x='')
  prev.pareto.mixed.all.collapsed <- prev.pareto.mixed.all[!(prev.pareto.mixed.all$ShortName %in% prev.pareto.mixed.all[grep('Flu A|FluA|PIV|CoV|pertussis|pneumo', prev.pareto.mixed.all$ShortName),'ShortName']), ]
  prev.pareto.mixed.all.collapsed$Name <- factor(prev.pareto.mixed.all.collapsed$ShortName, levels = label.order.all.collapsed)
  prev.pareto.child.all.collapsed <- prev.pareto.child.all[!(prev.pareto.child.all$ShortName %in% prev.pareto.child.all[grep('Flu A|FluA|PIV|CoV|pertussis|pneumo', prev.pareto.child.all$ShortName),'ShortName']), ]
  prev.pareto.child.all.collapsed$Name <- factor(prev.pareto.child.all.collapsed$ShortName, levels = label.order.all.collapsed)
  prev.pareto.all.pop.collapsed <- rbind(prev.pareto.mixed.all.collapsed, prev.pareto.child.all.collapsed)  
  p.prev.pareto.all.pop.collapsed <- ggplot(prev.pareto.all.pop.collapsed, aes(x=Name, y=Prevalence, fill=Key)) + geom_bar(stat='identity', position='dodge') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_fill_manual(values=createPaletteOfVariableLength(prev.pareto.all.pop.collapsed, 'Key'), guid=FALSE) + scale_y_continuous(label=percent) + labs(title='National Prevalence of Organsims by Organism Type and Hospital Population Type') + labs(x='')  
}

# USE PREVALENCE TO MODEL ILI IN POPULATION
if(TRUE) {
  
  
  rbind(runs.reg.date[,c('YearWeek','CustomerSiteId','Name','Region','Record','RunDataId')], bugs.df, by='RunDataId')
}



# MULTI-VARIATE REGRESSION MODEL OF ILI WITH RP PREVALENCE
if(TRUE) {
  sites <- unique(runs.reg.date$CustomerSiteId)
  bugs.reg <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    temp <- runs.reg.date[runs.reg.date$CustomerSiteId == site, ]
    bugs.site <- merge(temp, bugs.df, by='RunDataId')
    bugs.reg <- rbind(bugs.reg, bugs.site)
  }
  
  # remove Bocavirus as it only appears at Nationwide and there are only 63 positives over all time (also, this bug isn't in the RP panel latest rev)
  bugs.reg <- bugs.reg[bugs.reg$BugPositive != 'Bocavirus',]
  
  # make a combined category so that do.call can be used to fill in empty dates
  colsToCat <- c('Region','Name','CustomerSiteId','BugPositive')
  bugs.reg.trim <- bugs.reg[,c('YearWeek',colsToCat)]
  bugs.reg.trim$combocat <- do.call(paste, c(bugs.reg.trim[,colsToCat], sep=','))
  bugs.reg.trim$Record <- 1
  bugs.reg.combo <- do.call(rbind, lapply(1:length(unique(bugs.reg.trim$combocat)), function(x) cbind(merge(unique(calendar.df[,c('YearWeek','Year')]), bugs.reg.trim[bugs.reg.trim$combocat == unique(bugs.reg.trim$combocat)[x], c('YearWeek','Record')], by='YearWeek', all.x=TRUE), ComboCat = unique(bugs.reg.trim$combocat)[x])))
  deCombo <- as.data.frame(sapply(1:length(colsToCat), function(x) do.call(rbind, strsplit(as.character(bugs.reg.combo$ComboCat), split=','))[,x]))
  colnames(deCombo) <- colsToCat
  bugs.reg.fill <- cbind(bugs.reg.combo[,c('YearWeek','Record')], deCombo)
  bugs.reg.fill[is.na(bugs.reg.fill$Record),'Record'] <- 0
  bugs.reg.agg <- with(bugs.reg.fill, aggregate(Record~YearWeek+Region+Name+CustomerSiteId+BugPositive, FUN=sum))
  runs.reg.agg <- with(subset(runs.reg.date, CustomerSiteId %in% sites), aggregate(Record~YearWeek+Region+Name+CustomerSiteId, FUN=sum))
  colnames(runs.reg.agg)[grep('Record', colnames(runs.reg.agg))] <- 'Runs'
  
  # use the rollPositivesBySite user defined funtion to roll the positive count of tests about the center week (3-week moving average)
  bugs <- as.character(unique(bugs.reg.agg$BugPositive))[order(as.character(unique(bugs.reg.agg$BugPositive)))]
  prevalence.reg.count <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    temp <- rollPositivesBySite(bugs.reg.agg, runs.reg.agg, 'BugPositive', 'Record', site, bugs)
    prevalence.reg.count <- rbind(prevalence.reg.count, temp)
  }
  
  decoder <- data.frame(Bug = bugs, Code = letters[1:length(bugs)])

  # use the counts of positives and the run count to get a prevalence (for all data, pediatric, and mixed), but only keep the data that are between 2013-26 and 2016-26
  # all data
  prevalence.nat.rate <- with(prevalence.reg.count, aggregate(as.formula(paste('cbind(', paste('Runs', paste(colnames(prevalence.reg.count)[grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.count))], collapse = ','), sep=','), ')~YearWeek',sep='')), FUN=sum))
  prevalence.nat.rate.div <- do.call(rbind, lapply(1:length(unique(prevalence.reg.count$YearWeek)), function(x) sapply(1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.count))), function(y) sum(prevalence.reg.count[prevalence.reg.count$YearWeek == unique(prevalence.reg.count$YearWeek)[x],letters[y]])/sum(prevalence.reg.count[prevalence.reg.count$YearWeek == unique(prevalence.reg.count$YearWeek)[x],'Runs']))))
  prevalence.nat.rate.div <- as.data.frame(prevalence.nat.rate.div)
  colnames(prevalence.nat.rate.div) <- letters[1:length(colnames(prevalence.nat.rate.div))]
  prevalence.nat.rate <- cbind(YearWeek = prevalence.nat.rate$YearWeek, prevalence.nat.rate.div)
  cdc.reg.count.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(prevalence.reg.count$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
  cdc.reg.count.agg$Rate <- with(cdc.reg.count.agg, ILITotal/TotalPatients)
  prevalence.cdc.nat <- merge(prevalence.nat.rate, cdc.reg.count.agg[,c('YearWeek','Rate')], by='YearWeek')
  prevalence.cdc.nat <- prevalence.cdc.nat[as.character(prevalence.cdc.nat$YearWeek) > '2013-25' & as.character(prevalence.cdc.nat$YearWeek) < '2016-27', ]
  if(FALSE) {
    # mixed sites only
    prevalence.reg.mixed.count <- subset(prevalence.reg.count, CustomerSiteId %in% sites.mixed)
    prevalence.nat.mixed.rate <- with(prevalence.reg.mixed.count[prevalence.reg.mixed.count$CustomerSiteId %in% sites.mixed, ], aggregate(as.formula(paste('cbind(', paste('Runs', paste(colnames(prevalence.reg.mixed.count)[grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.mixed.count))], collapse = ','), sep=','), ')~YearWeek',sep='')), FUN=sum))
    prevalence.nat.mixed.rate.div <- do.call(rbind, lapply(1:length(unique(prevalence.reg.mixed.count$YearWeek)), function(x) sapply(1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.mixed.count))), function(y) sum(prevalence.reg.mixed.count[prevalence.reg.mixed.count$YearWeek == unique(prevalence.reg.mixed.count$YearWeek)[x],letters[y]])/sum(prevalence.reg.mixed.count[prevalence.reg.mixed.count$YearWeek == unique(prevalence.reg.mixed.count$YearWeek)[x],'Runs']))))
    prevalence.nat.mixed.rate.div <- as.data.frame(prevalence.nat.mixed.rate.div)
    colnames(prevalence.nat.mixed.rate.div) <- letters[1:length(colnames(prevalence.nat.mixed.rate.div))]
    prevalence.nat.mixed.rate <- cbind(YearWeek = prevalence.nat.mixed.rate$YearWeek, prevalence.nat.mixed.rate.div)
    cdc.reg.count.mixed.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(prevalence.reg.mixed.count$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
    cdc.reg.count.mixed.agg$Rate <- with(cdc.reg.count.mixed.agg, ILITotal/TotalPatients)
    prevalence.cdc.mixed.nat <- merge(prevalence.nat.mixed.rate, cdc.reg.count.mixed.agg[,c('YearWeek','Rate')], by='YearWeek')
    prevalence.cdc.mixed.nat <- prevalence.cdc.mixed.nat[as.character(prevalence.cdc.mixed.nat$YearWeek) > '2013-25' & as.character(prevalence.cdc.mixed.nat$YearWeek) < '2016-27', ]
    # # pediatric sites only
    # prevalence.reg.child.count <- subset(prevalence.reg.count, CustomerSiteId %in% sites.child)
    # prevalence.nat.child.rate <- with(prevalence.reg.child.count[prevalence.reg.child.count$CustomerSiteId %in% sites.child, ], aggregate(as.formula(paste('cbind(', paste('Runs', paste(colnames(prevalence.reg.child.count)[grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.child.count))], collapse = ','), sep=','), ')~YearWeek',sep='')), FUN=sum))
    # prevalence.nat.child.rate.div <- do.call(rbind, lapply(1:length(unique(prevalence.reg.child.count$YearWeek)), function(x) sapply(1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.reg.child.count))), function(y) sum(prevalence.reg.child.count[prevalence.reg.child.count$YearWeek == unique(prevalence.reg.child.count$YearWeek)[x],letters[y]])/sum(prevalence.reg.child.count[prevalence.reg.child.count$YearWeek == unique(prevalence.reg.child.count$YearWeek)[x],'Runs']))))
    # prevalence.nat.child.rate.div <- as.data.frame(prevalence.nat.child.rate.div)
    # colnames(prevalence.nat.child.rate.div) <- letters[1:length(colnames(prevalence.nat.child.rate.div))]
    # prevalence.nat.child.rate <- cbind(YearWeek = prevalence.nat.child.rate$YearWeek, prevalence.nat.child.rate.div)
    # cdc.reg.count.child.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(prevalence.reg.child.count$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
    # cdc.reg.count.child.agg$Rate <- with(cdc.reg.count.child.agg, ILITotal/TotalPatients)
    # prevalence.cdc.child.nat <- merge(prevalence.nat.child.rate, cdc.reg.count.child.agg[,c('YearWeek','Rate')], by='YearWeek')
    # prevalence.cdc.child.nat <- prevalence.cdc.child.nat[as.character(prevalence.cdc.child.nat$YearWeek) > '2013-25' & as.character(prevalence.cdc.child.nat$YearWeek) < '2016-27', ]
  }  
  # using this data, create a regression model to predict ILI rate with organisms as the independant variables
  # load every combination possible for six, seven, eight, and nine organisms
  combos.six <- read.csv('../DataSources/modelCombos6.csv', header=TRUE, sep=',')
  combos.seven <- read.csv('../DataSources/modelCombos7.csv', header=TRUE, sep=',')
  combos.eight <- read.csv('../DataSources/modelCombos8.csv', header=TRUE, sep=',')
  combos.nine <- read.csv('../DataSources/modelCombos9.csv', header=TRUE, sep=',')
  
  # --------------------- all data --------------------------------------------------------------------------------------------------------------
  fit.all <- lm(as.formula(paste('Rate', paste(letters[1:21], collapse='+'), sep='~')), prevalence.cdc.nat)
  
  # choose the "best" model from model.eval.six and run ANOVA between that and the model including all organisms
  model.eval.six <- do.call(rbind, lapply(1:length(combos.six[,'Combo']), function(x) data.frame(Model = combos.six[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat)), prevalence.cdc.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat), fit.all)[,'Pr(>F)'][2])))
  as.character(model.eval.six[model.eval.six$anova.all == max(model.eval.six$anova.all), 'Model'])
  as.character(model.eval.six[model.eval.six$corr == max(model.eval.six$corr), 'Model'])
  as.character(model.eval.six[model.eval.six$adjR2 == max(model.eval.six$adjR2), 'Model'])
  # the best six-organisms model is f + n + o + g + c + h (Corona NL63, FluA H3, FluB, Corona OC43, Chlamydophila pneumoniae, Human Metapneumovirus)
  
  # however, the best six-organism model still has p-value indicating that all >>>> six, so we must try again with a seven organism model
  model.eval.seven <- do.call(rbind, lapply(1:length(combos.seven[,'Combo']), function(x) data.frame(Model = combos.seven[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat)), prevalence.cdc.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.nat), fit.all)[,'Pr(>F)'][2])))
  as.character(model.eval.seven[model.eval.seven$anova.all == max(model.eval.seven$anova.all), 'Model'])
  as.character(model.eval.seven[model.eval.seven$corr == max(model.eval.seven$corr), 'Model'])
  as.character(model.eval.seven[model.eval.seven$adjR2 == max(model.eval.seven$adjR2), 'Model'])
  # the best seven-organism model is f + n + o + g + c + h + l (Corona NL63, FluA H3, FluB, Corona OC43, Chlamy pneumoniae, Human Metapneumo, FluA H1)
    
  # however, the best seven-organism model still has a p-value indicating that all >>> seven, so we must try again with an eight organism model
  model.eval.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat)), prevalence.cdc.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.nat), fit.all)[,'Pr(>F)'][2])))
  as.character(model.eval.eight[model.eval.eight$anova.all == max(model.eval.eight$anova.all), 'Model'])
  as.character(model.eval.eight[model.eval.eight$corr == max(model.eval.eight$corr), 'Model'])
  as.character(model.eval.eight[model.eval.eight$adjR2 == max(model.eval.eight$adjR2), 'Model'])
  # the best eight-organism model is f + n + o + g + c + h + l + m (Corona NL63, FluA H3, FluB, Corona OC43, C pneumoniae, Human Metapneumo, FluA H1, FluA H1-2009)
  
  # however, the best eight-organism model still has a p-value indicating that all >>> eight, so we must try again with a nine organism model
  model.eval.nine <- do.call(rbind, lapply(1:length(combos.nine[,'Combo']), function(x) data.frame(Model = combos.nine[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat)), prevalence.cdc.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.nine[x,]), sep='~')), prevalence.cdc.nat), fit.all)[,'Pr(>F)'][2])))
  as.character(model.eval.nine[model.eval.nine$anova.all == max(model.eval.nine$anova.all), 'Model'])
  as.character(model.eval.nine[model.eval.nine$corr == max(model.eval.nine$corr), 'Model'])
  as.character(model.eval.nine[model.eval.nine$adjR2 == max(model.eval.nine$adjR2), 'Model'])
  anova(lm(as.formula(paste('Rate', as.character(model.eval.nine[model.eval.nine$anova.all == max(model.eval.nine$anova.all), 'Model']), sep='~')), prevalence.cdc.nat), fit.all)
  # the best nine-organism model is f + n + o + g + c + h + l + m + a (Corona NL63, FluA H3, FluB, Corona OC43, C pneumoniae, Human Metapneumo, FluA H1, FluA H1-2009, Adeno)
  
  # however, the best nine-organism model still has a p-value indicating that all >> nine, so we must try again with a ten organism model
  
  # try the fit with just flu organisms
  summary(lm(Rate~j+k+l+m+n+o, prevalence.cdc.nat))
  summary(lm(Rate~j+l+m+n+o, prevalence.cdc.nat)) # remove FluA no subtype detected
  
  # -------------------- JUST WITH SITES CONTRIBUTING PRIOR TO 2016
  sites.good <- c(2,5,7,10,13,26,29,36)
  bugs.good.reg <- c()
  for(i in 1:length(sites.good)) {
    
    site <- sites.good[i]
    temp <- runs.reg.date[runs.reg.date$CustomerSiteId == site, ]
    bugs.site <- merge(temp, bugs.df, by='RunDataId')
    bugs.good.reg <- rbind(bugs.reg, bugs.site)
  }
  
  # remove Bocavirus as it only appears at Nationwide and there are only 63 positives over all time (also, this bug isn't in the RP panel latest rev)
  bugs.good.reg <- bugs.good.reg[bugs.good.reg$BugPositive != 'Bocavirus',]
  
  # make a combined category so that do.call can be used to fill in empty dates
  colsToCat <- c('Region','Name','CustomerSiteId','BugPositive')
  bugs.good.reg.trim <- bugs.good.reg[,c('YearWeek',colsToCat)]
  bugs.good.reg.trim$combocat <- do.call(paste, c(bugs.good.reg.trim[,colsToCat], sep=','))
  bugs.good.reg.trim$Record <- 1
  bugs.good.reg.combo <- do.call(rbind, lapply(1:length(unique(bugs.good.reg.trim$combocat)), function(x) cbind(merge(unique(calendar.df[,c('YearWeek','Year')]), bugs.good.reg.trim[bugs.good.reg.trim$combocat == unique(bugs.good.reg.trim$combocat)[x], c('YearWeek','Record')], by='YearWeek', all.x=TRUE), ComboCat = unique(bugs.good.reg.trim$combocat)[x])))
  deCombo <- as.data.frame(sapply(1:length(colsToCat), function(x) do.call(rbind, strsplit(as.character(bugs.good.reg.combo$ComboCat), split=','))[,x]))
  colnames(deCombo) <- colsToCat
  bugs.good.reg.fill <- cbind(bugs.good.reg.combo[,c('YearWeek','Record')], deCombo)
  bugs.good.reg.fill[is.na(bugs.good.reg.fill$Record),'Record'] <- 0
  bugs.good.reg.agg <- with(bugs.good.reg.fill, aggregate(Record~YearWeek+Region+Name+CustomerSiteId+BugPositive, FUN=sum))
  runs.good.reg.agg <- with(subset(runs.reg.date, CustomerSiteId %in% sites.good), aggregate(Record~YearWeek+Region+Name+CustomerSiteId, FUN=sum))
  colnames(runs.good.reg.agg)[grep('Record', colnames(runs.good.reg.agg))] <- 'Runs'
  
  # use the rollPositivesBySite user defined funtion to roll the positive count of tests about the center week (3-week moving average)
  bugs <- as.character(unique(bugs.good.reg.agg$BugPositive))[order(as.character(unique(bugs.good.reg.agg$BugPositive)))]
  prevalence.good.reg.count <- c()
  for(i in 1:length(sites.good)) {
    
    site <- sites.good[i]
    temp <- rollPositivesBySite(bugs.good.reg.agg, runs.good.reg.agg, 'BugPositive', 'Record', site, bugs)
    prevalence.good.reg.count <- rbind(prevalence.good.reg.count, temp)
  }
  
  # use the counts of positives and the run count to get a prevalence (for all data, pediatric, and mixed), but only keep the data that are between 2013-26 and 2016-26
  # all data
  prevalence.nat.rate.good <- with(prevalence.good.reg.count, aggregate(as.formula(paste('cbind(', paste('Runs', paste(colnames(prevalence.good.reg.count)[grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.good.reg.count))], collapse = ','), sep=','), ')~YearWeek',sep='')), FUN=sum))
  prevalence.nat.rate.good.div <- do.call(rbind, lapply(1:length(unique(prevalence.good.reg.count$YearWeek)), function(x) sapply(1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevalence.good.reg.count))), function(y) sum(prevalence.good.reg.count[prevalence.good.reg.count$YearWeek == unique(prevalence.good.reg.count$YearWeek)[x],letters[y]])/sum(prevalence.good.reg.count[prevalence.good.reg.count$YearWeek == unique(prevalence.good.reg.count$YearWeek)[x],'Runs']))))
  prevalence.nat.rate.good.div <- as.data.frame(prevalence.nat.rate.good.div)
  colnames(prevalence.nat.rate.good.div) <- letters[1:length(colnames(prevalence.nat.rate.good.div))]
  prevalence.nat.rate.good <- cbind(YearWeek = prevalence.nat.rate.good$YearWeek, prevalence.nat.rate.good.div)
  cdc.good.reg.count.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(prevalence.good.reg.count$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
  cdc.good.reg.count.agg$Rate <- with(cdc.good.reg.count.agg, ILITotal/TotalPatients)
  prevalence.cdc.nat.good <- merge(prevalence.nat.rate.good, cdc.good.reg.count.agg[,c('YearWeek','Rate')], by='YearWeek')
  prevalence.cdc.nat.good <- prevalence.cdc.nat.good[as.character(prevalence.cdc.nat.good$YearWeek) > '2013-25' & as.character(prevalence.cdc.nat.good$YearWeek) < '2016-27', ]
  
  # choose the "best" model from model.eval.six and run ANOVA between that and the model including all organisms
  fit.all.good <- lm(Rate~., prevalence.cdc.nat.good[,2:23])
  model.eval.six.good <- do.call(rbind, lapply(1:length(combos.six[,'Combo']), function(x) data.frame(Model = combos.six[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good)), prevalence.cdc.nat.good$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.nat.good), fit.all.good)[,'Pr(>F)'][2])))
  as.character(model.eval.six[model.eval.six$anova.all == max(model.eval.six$anova.all), 'Model'])
  as.character(model.eval.six[model.eval.six$corr == max(model.eval.six$corr), 'Model'])
  as.character(model.eval.six[model.eval.six$adjR2 == max(model.eval.six$adjR2), 'Model'])
  
  # Run Regional ILI fit with Site Prevalence (6 organism)
  all.site.models <- c()
  for(i in 1:length(sites.good)) {
    
    site.bug.count <- prevalence.good.reg.count[prevalence.good.reg.count$CustomerSiteId == sites.good[i], ]
    site.bug.rate <- cbind(YearWeek = site.bug.count$YearWeek, Region = unique(site.bug.count$Region), CustomerSiteId = sites.good[i], data.frame(do.call(cbind, lapply(1:21, function(x) site.bug.count[,letters[x]]/site.bug.count$Runs))))
    colnames(site.bug.rate)[4:24] <- letters[1:21]
    prev.ili.site <- merge(site.bug.rate, cdc.reg.df, by=c('YearWeek','Region'))
    fit.site.all <- lm(Rate~., prev.ili.site[4:25])
    site.models <- do.call(rbind, lapply(1:length(combos.six[,'Combo']), function(x) data.frame(CustomerSiteId = sites.good[i], Model = combos.six[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site)), prev.ili.site$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prev.ili.site), fit.site.all)[,'Pr(>F)'][2])))
    all.site.models <- rbind(all.site.models, site.models)
  }
 
  all.site.models[all.site.models$CustomerSiteId==2 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==2,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==5 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==5,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==7 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==7,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==10 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==10,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==13 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==13,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==26 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==26,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==29 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==29,'anova.all']), ]
  all.site.models[all.site.models$CustomerSiteId==36 & all.site.models$anova.all==max(all.site.models[all.site.models$CustomerSiteId==36,'anova.all']), ]
  # 2:  f+m+n+o+s+u, p-val: 0.013e0  ~
  # 5:  h+k+l+m+n+u, p-val: 2.607e-5 **
  # 7:  f+k+m+n+p+t, p-val: 3.409e-4 **
  # 10: e+f+i+j+n+u, p-val: 0.294e0  =
  # 13: a+f+h+k+l+n, p-val: 8.148e-9 ***
  # 26: d+f+g+k+o+u, p-val: 0.019e0 ~
  # 29: g+m+n+o+s+u, p-val: 7.126e-4 **
  # 36: j+m+n+o+r+u, p-val: 8.895e-3 *
  # sites should contain: m (5/8), n (7/8), u (6/8), f (5/8) - Flu A (H1 2009), Flu A (H3), RSV, Corona NL63
  # but sites 13, 26 contain only 2 of these... all other sites contain 3 or 4 of them... of these, sites 10, 2, and 36 have best fits and 5, 7, and 29 are okay.  
  # flus are j, k, l, m, n, o (with o being FluB) - let's look to see if the trim sites contain flus... 
  # 2 (2 fluAs, 1 fluB, rsv), 5 (4 fluAs, rsv), 7 (3 fluAs), 10 (2 fluAs, rsv), 13 (3 fluAs), 26 (1 fluA, 1 fluB, rsv), 
  # 29 (2 fluAs, 1 fluB, rsv), 36 (3 fluAs, 1 fluB, rsv)
  
  if(FALSE) {
    # --------------------- mixed -----------------------------------------------------------------------------------------------------------------
    fit.mixed.all <- lm(as.formula(paste('Rate', paste(letters[1:21], collapse='+'), sep='~')), prevalence.cdc.mixed.nat)

    # choose the "best" model from model.eval.six and run ANOVA between that and the model including all organisms
    model.eval.mixed.six <- do.call(rbind, lapply(1:length(combos.six[,'Combo']), function(x) data.frame(Model = combos.six[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat)), prevalence.cdc.mixed.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.six[x,]), sep='~')), prevalence.cdc.mixed.nat), fit.mixed.all)[,'Pr(>F)'][2])))
    as.character(model.eval.mixed.six[model.eval.mixed.six$anova.all == max(model.eval.mixed.six$anova.all), 'Model'])
    as.character(model.eval.mixed.six[model.eval.mixed.six$corr == max(model.eval.mixed.six$corr), 'Model'])
    as.character(model.eval.mixed.six[model.eval.mixed.six$adjR2 == max(model.eval.mixed.six$adjR2), 'Model'])
    # the best six-organisms model is n + t + g + m + u + a (FluA H3, PIV 4, Corona OC43, FluA H1-2009, RSV, Adeno)
    # the best six-organism model is statistically the same as the model containing all the organisms... can I remove any? No, they are all important
    summary(lm(as.formula(paste('Rate', as.character(model.eval.mixed.six[model.eval.mixed.six$anova.all == max(model.eval.mixed.six$anova.all), 'Model']), sep='~')), prevalence.cdc.mixed.nat))
    anova(lm(as.formula(paste('Rate', as.character(model.eval.mixed.six[model.eval.mixed.six$anova.all == max(model.eval.mixed.six$anova.all), 'Model']), sep='~')), prevalence.cdc.mixed.nat), fit.mixed.all)

    # # however, the best six-organism model still has p-value indicating that all >>>> six, so we must try again with a seven organism model
    # model.eval.mixed.seven <- do.call(rbind, lapply(1:length(combos.seven[,'Combo']), function(x) data.frame(Model = combos.seven[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat)), prevalence.cdc.mixed.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.seven[x,]), sep='~')), prevalence.cdc.mixed.nat), fit.mixed.all)[,'Pr(>F)'][2])))
    # as.character(model.eval.mixed.seven[model.eval.mixed.seven$anova.all == max(model.eval.mixed.seven$anova.all), 'Model'])
    # as.character(model.eval.mixed.seven[model.eval.mixed.seven$corr == max(model.eval.mixed.seven$corr), 'Model'])
    # as.character(model.eval.mixed.seven[model.eval.mixed.seven$adjR2 == max(model.eval.mixed.seven$adjR2), 'Model'])
    # # the best seven-organism model is n + t + g + m + u + a + f (FluA H3, PIV 4, Corona OC43, FluA H1-2009, RSV, Adeno, Corona NL63)
    # 
    # # however, the best seven-organism model still has a p-value indicating that all >>> seven, so we must try again with an eight organism model
    # model.eval.mixed.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat)), prevalence.cdc.mixed.nat$Rate), alpha.0 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('Rate', as.character(combos.eight[x,]), sep='~')), prevalence.cdc.mixed.nat), fit.mixed.all)[,'Pr(>F)'][2])))
    # as.character(model.eval.mixed.eight[model.eval.mixed.eight$anova.all == max(model.eval.mixed.eight$anova.all), 'Model'])
    # as.character(model.eval.mixed.eight[model.eval.mixed.eight$corr == max(model.eval.mixed.eight$corr), 'Model'])
    # as.character(model.eval.mixed.eight[model.eval.mixed.eight$adjR2 == max(model.eval.mixed.eight$adjR2), 'Model'])
    # # the best eight-organism model is n + t + g + m + u + a + f + o (FluA H3, PIV 4, Corona OC43, FluA H1-2009, RSV, Adeno, Corona NL63, FluB)
    
    # --------------------- pediatric -------------------------------------------------------------------------------------------------------------
    # not doing separation of mixed and peds... only doing everything as a group
  }
}
# CLASSIFICATION AND REGRESSION TREE MODELING OF ILI WITH RP PREVALENCE
if(TRUE) {
  
  set.seed(2)
  # regression tree model
  rename.cols <- merge(decoder, shortnames.df, by.x='Bug', by.y='Organism')
  anova.tree.data <- prevalence.cdc.nat
  colnames(anova.tree.data)[2:22] <- as.character(rename.cols$ShortName)
  ili.anova.tree <- rpart(Rate~., data=anova.tree.data[, 2:23], method='anova')
  printcp(ili.anova.tree)
  plotcp(ili.anova.tree)
  rsq.rpart(ili.anova.tree)
  print(ili.anova.tree)
  summary(ili.anova.tree)
  plot(ili.anova.tree)
  text(ili.anova.tree, use.n=TRUE, pretty=TRUE, xpd=TRUE)  

  ili.c.tree <- ctree(Rate~., data=anova.tree.data[, 2:23])  
  plot(ili.c.tree)
  
  colnames(anova.tree.data)[2:22] <- gsub(' ', '', colnames(anova.tree.data[2:22]))
  colnames(anova.tree.data)[grep('HRV', colnames(anova.tree.data))] <- 'HRV_Entero'
  colnames(anova.tree.data)[grep('nosubtype', colnames(anova.tree.data))] <- 'FluA_noSubtype'
  colnames(anova.tree.data)[grep('H1-09', colnames(anova.tree.data))] <- 'FluAH1_09'
  ili.random.forest <- randomForest(Rate~., data=anova.tree.data[, 2:23])
  importance(ili.random.forest)
  plot(ili.random.forest)
  varImpPlot(ili.random.forest)
  
  
  ili.nueral.net <- nnet(Rate~., data=anova.tree.data[,2:23], size=20, linout=T)
  plot(ili.nueral.net)
  plot.nnet(ili.nueral.net, pos.col='black', neg.col='grey', circle.col='lightskyblue')
  p <- gar.fun('Rate', ili.nueral.net, bar.plot = TRUE, y.lab = 'Relative Importance')
  p + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), panel.background=element_rect(color='white',fill='white'), legend.position='none') + labs(title='Relative Importance of Organisms in Neural Net Model\n(Varaibles close to Zero signify minimal importance)')
  
  p.ili.models <- ggplot(anova.tree.data, aes(x=YearWeek, y=Rate, group='Actual ILI (Reported)', color='Actual ILI (Reported)')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=predict(ili.nueral.net), group='Neural Net Model', color='Neural Net Model'), anova.tree.data) + geom_point(aes(x=YearWeek, y=predict(ili.nueral.net), group='Neural Net Model', color='Neural Net Model'), anova.tree.data) + geom_line(aes(x=YearWeek, y=predict(ili.anova.tree), group='CART Model', color='CART Model'), anova.tree.data) + geom_point(aes(x=YearWeek, y=predict(ili.anova.tree), group='CART Model', color='CART Model'), anova.tree.data) + geom_line(aes(x=YearWeek, y=predict(ili.c.tree), group='Inference Tree Model', color='Inference Tree Model'), anova.tree.data) + geom_point(aes(x=YearWeek, y=predict(ili.c.tree), group='Inference Tree Model', color='Inference Tree Model'), anova.tree.data) + scale_color_manual(values=createPaletteOfVariableLength(data.frame(Key=c('Actual ILI (Reported)','CART Model','Inference Tree Model','Neural Net Model'), Record=1),'Key'), name='') + annotate('text', label=paste(paste(paste('CART Correlation: ', round(cor(anova.tree.data$Rate, predict(ili.anova.tree)),3), sep=''),paste('Inference Tree Correlation: ', round(cor(anova.tree.data$Rate, predict(ili.c.tree)),3), sep=''),sep='\n'),paste('Neural Net Correlation: ', round(cor(anova.tree.data$Rate, predict(ili.nueral.net)),3), sep=''),sep='\n'), x=as.factor('2016-01'), y=0.05) + scale_y_continuous(labels=percent) + scale_x_discrete(breaks=as.character(anova.tree.data$YearWeek)[order(as.character(anova.tree.data$YearWeek))][seq(1, length(as.character(anova.tree.data$YearWeek)),8)]) + theme(panel.background=element_rect(color='white',fill='white'), text=element_text(size=20, face='bold'), axis.text=element_text(color='black', face='bold', size=14), axis.text.x=element_text(angle=90, hjust=1)) + labs(title='CDC Reported ILI Rate Overlaid\n with FilmArray RP Prevalence Models', y='ILI Rate', x='Year-Week')
  
  # regression tree model.... mixed only
  anova.tree.data.mixed <- prevalence.cdc.mixed.nat
  colnames(anova.tree.data.mixed)[2:22] <- as.character(rename.cols$ShortName)
  ili.mixed.anova.tree <- rpart(Rate~., data=anova.tree.data.mixed[, 2:23], method='anova')
  printcp(ili.mixed.anova.tree)
  plotcp(ili.mixed.anova.tree)
  rsq.rpart(ili.mixed.anova.tree)
  print(ili.mixed.anova.tree)
  summary(ili.mixed.anova.tree)
  plot(ili.mixed.anova.tree)
  text(ili.mixed.anova.tree, use.n=TRUE, pretty=TRUE, xpd=TRUE)  
  
  ili.mixed.c.tree <- ctree(Rate~., data=anova.tree.data[, 2:23])  
  plot(ili.mixed.c.tree)
  
  colnames(anova.tree.data.mixed)[2:22] <- gsub(' ', '', colnames(anova.tree.data.mixed[2:22]))
  colnames(anova.tree.data.mixed)[grep('HRV', colnames(anova.tree.data.mixed))] <- 'HRV_Entero'
  colnames(anova.tree.data.mixed)[grep('nosubtype', colnames(anova.tree.data.mixed))] <- 'FluA_noSubtype'
  colnames(anova.tree.data.mixed)[grep('H1-09', colnames(anova.tree.data.mixed))] <- 'FluAH1_09'
  ili.mixed.random.forest <- randomForest(Rate~., data=anova.tree.data.mixed[, 2:23])
  importance(ili.mixed.random.forest)
  plot(ili.mixed.random.forest)
  varImpPlot(ili.mixed.random.forest)
  
  
  ili.nueral.net <- nnet(Rate~., data=anova.tree.data[,2:23], size=20, linout=T)
  plot(ili.nueral.net)
  plot.nnet(ili.nueral.net, pos.col='black', neg.col='grey', circle.col='lightskyblue')
  p <- gar.fun('Rate', ili.nueral.net, bar.plot = TRUE, y.lab = 'Relative Importance')
  p + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), panel.background=element_rect(color='white',fill='white'), legend.position='none') + labs(title='Relative Importance of Organisms in Neural Net Model\n(Varaibles close to Zero signify minimal importance)')
  
}


# ILI VS. RP NORMALIZED BURN RATE
if(TRUE) {
  # --------------------- all data --------------------------------------------------------------------------------------------------------------
  var <- 'CustomerSiteId'
  
  runs.reg.norm <- c()
  for(i in 1:length(sites)) {
    
    site.norm <- normalizeBurnRate(runs.reg.date, var, sites[i])
    runs.reg.norm <- rbind(runs.reg.norm, site.norm)
  }
  
  runs.reg.norm <- merge(runs.reg.norm[, c('YearWeek','Year','Week','CustomerSiteId','RollRuns','NormRollRate','NormRollRateByInst','NormalizedBurn')], unique(runs.reg[,c('CustomerSiteId','Name','Region')]), by='CustomerSiteId')
  cdc.reg.count.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(runs.reg.norm$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
  cdc.reg.count.agg$Rate <- with(cdc.reg.count.agg, ILITotal/TotalPatients)
  runs.cdc.reg <- merge(runs.reg.norm, cdc.reg.count.agg[,c('YearWeek','Rate')], by=c('YearWeek'))
  ili.rates.nat <- with(runs.cdc.reg, aggregate(cbind(NormalizedBurn, Rate)~YearWeek, FUN=mean))

  # create an overlay plot
  x1 <- as.character(unique(ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2014-01', 'YearWeek']))
  y1 <- ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2014-01','Rate']
  z1 <- ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2014-01','NormalizedBurn']
  # plot.new() #-------------------
  par(mfrow=c(1,1))
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(as.factor(x1), y1, type='n', col='black')
  lines(as.factor(x1), y1, col='black', lwd=3)
  mtext("ILI Patients per Patients Observed", side=2, line=2.5, col='black');
  axis(2, col.axis='black'); par(new = TRUE)
  plot(as.factor(x1), z1, col='red', type = 'n', axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(as.factor(x1), z1, col='red', lwd=3)
  axis(side=4, at = pretty(range(z1)), col.axis='red')
  mtext("Normalized RP Test Run Rate", side=4, line=3, col='red');
  #  axis(4, col='black',col.axis='black');
  title(main='National Weekly CDC ILI vs. RP Test Normalized Run Rate\n(Averaged Over All Participating Regions)')
  mtext("Year-Week", side=1, line=3, col='black')
  legend("topleft", legend=c('CDC','FilmArray RP'), text.col=c('black','red'), pch=c(16,15), col=c('black','red'), cex=0.8)
  p.nation.overlay <- recordPlot()
  
  site.cor.nat.ili <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    temp <- merge(runs.reg.norm[runs.reg.norm$CustomerSiteId == site, ], cdc.reg.count.agg, by='YearWeek')
    temp <- data.frame(CustomerSiteId = site, CorrelationToILI = cor(temp$Rate, temp$NormalizedBurn))
    site.cor.nat.ili <- rbind(site.cor.nat.ili, temp)
  }
  site.cor.nat.ili <- merge(site.cor.nat.ili, unique(runs.reg.date[,c('CustomerSiteId','Name')], by='CustomerSiteId'))
  site.cor.nat.ili <- site.cor.nat.ili[with(site.cor.nat.ili, order(CorrelationToILI, decreasing = TRUE)), ]
  
  if(TRUE) {
    # p.all.overlay <- ggplot(ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2014-01', ], aes(x=YearWeek, y=Rate*100, group='CDC', color='CDC')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=NormalizedBurn, group='FilmArray Trend', color='FilmArray Trend'), data=ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2013-26', ]) + geom_point(aes(x=YearWeek, y=NormalizedBurn, group='FilmArray Trend', color='FilmArray Trend'), data=ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2013-26', ]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.35), panel.background=element_rect(color='white', fill='white')) + scale_x_discrete(breaks=as.character(unique(ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2013-26', 'YearWeek']))[order(as.character(unique(ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2013-26', 'YearWeek'])))][seq(1,length(as.character(unique(ili.rates.nat[as.character(ili.rates.nat$YearWeek) >= '2013-26', 'YearWeek']))),8)]) + labs(title='CDC Reported ILI vs. Trend RP Nomalized Burn Rate\n(CDC Regions Participating in Trend)', y='Normalized Run Rate, Percent ILI', x='Year-Week') + scale_color_manual(values=c('black','red'), name='')
    # --------------------- mixed -----------------------------------------------------------------------------------------------------------------
    runs.reg.mixed.norm <- c()
    for(i in 1:length(sites.mixed)) {

      site.mixed.norm <- normalizeBurnRate(runs.reg.date, var, sites.mixed[i])
      runs.reg.mixed.norm <- rbind(runs.reg.mixed.norm, site.mixed.norm)
    }

    runs.reg.mixed.norm <- merge(runs.reg.mixed.norm[,c('YearWeek','Year','Week','CustomerSiteId','RollRuns','NormRollRate','NormRollRateByInst','NormalizedBurn')], unique(runs.reg[,c('CustomerSiteId','Name','Region')]), by='CustomerSiteId')
    cdc.reg.count.mixed.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(runs.reg.mixed.norm$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
    cdc.reg.count.mixed.agg$Rate <- with(cdc.reg.count.mixed.agg, ILITotal/TotalPatients)
    runs.cdc.mixed.reg <- merge(runs.reg.mixed.norm, cdc.reg.count.mixed.agg[,c('YearWeek','Rate')], by=c('YearWeek'))
    ili.rates.mixed.nat <- with(runs.cdc.mixed.reg, aggregate(cbind(NormalizedBurn, Rate)~YearWeek, FUN=mean))

    # create an overlay plot
    x1.mixed <- as.character(unique(ili.rates.mixed.nat[as.character(ili.rates.mixed.nat$YearWeek) >= paste(start.year, '-01', sep=''), 'YearWeek']))
    y1.mixed <- ili.rates.mixed.nat[as.character(ili.rates.mixed.nat$YearWeek) >= paste(start.year, '-01', sep=''),'Rate']
    z1.mixed <- ili.rates.mixed.nat[as.character(ili.rates.mixed.nat$YearWeek) >= paste(start.year, '-01', sep=''),'NormalizedBurn']
    # plot.new() #-------------------
    par(mfrow=c(1,1))
    par(mar = c(5, 4, 4, 4) + 0.3)
    plot(as.factor(x1.mixed), y1.mixed, type='n', col='black')
    lines(as.factor(x1.mixed), y1.mixed, col='black', lwd=3)
    mtext("ILI Patients per Patients Observed", side=2, line=2.5, col='black');
    axis(2, col.axis='black'); par(new = TRUE)
    plot(as.factor(x1.mixed), z1.mixed, col='red', type = 'n', axes = FALSE, bty = "n", xlab = "", ylab = "")
    lines(as.factor(x1.mixed), z1.mixed, col='red', lwd=3)
    axis(side=4, at = pretty(range(z1.mixed)), col.axis='red')
    mtext("Normalized RP Test Run Rate", side=4, line=3, col='red');
    #  axis(4, col='black',col.axis='black');
    title(main='National Weekly CDC ILI vs. RP Test Normalized Run Rate\n(Mixed Hospital Populations Averaged Over All Participating Regions)')
    mtext("Year-Week", side=1, line=3, col='black')
    legend("topleft", legend=c('CDC','FilmArray RP'), text.col=c('black','red'), pch=c(16,15), col=c('black','red'), cex=0.8)
    p.nation.mixed.overlay <- recordPlot()

    # --------------------- pediatric -------------------------------------------------------------------------------------------------------------
    runs.reg.child.norm <- c()
    for(i in 1:length(sites.child)) {

      site.child.norm <- normalizeBurnRate(runs.reg.date, var, sites.child[i])
      runs.reg.child.norm <- rbind(runs.reg.child.norm, site.child.norm)
    }

    runs.reg.child.norm <- merge(runs.reg.child.norm[,c('YearWeek','Year','Week','CustomerSiteId','RollRuns','NormRollRate','NormRollRateByInst','NormalizedBurn')], unique(runs.reg[,c('CustomerSiteId','Name','Region')]), by='CustomerSiteId')
    cdc.reg.count.child.agg <- with(cdc.reg.count.df[as.character(cdc.reg.count.df$Region) %in% as.character(unique(runs.reg.child.norm$Region)), ], aggregate(cbind(TotalPatients, ILITotal)~YearWeek, FUN=sum))
    cdc.reg.count.child.agg$Rate <- with(cdc.reg.count.child.agg, ILITotal/TotalPatients)
    runs.cdc.child.reg <- merge(runs.reg.child.norm, cdc.reg.count.child.agg[,c('YearWeek','Rate')], by=c('YearWeek'))
    ili.rates.child.nat <- with(runs.cdc.child.reg, aggregate(cbind(NormalizedBurn, Rate)~YearWeek, FUN=mean))

    # create an overlay plot
    x1.child <- as.character(unique(ili.rates.child.nat[as.character(ili.rates.child.nat$YearWeek) >= paste(start.year, '-01', sep=''), 'YearWeek']))
    y1.child <- ili.rates.child.nat[as.character(ili.rates.child.nat$YearWeek) >= paste(start.year, '-01', sep=''),'Rate']
    z1.child <- ili.rates.child.nat[as.character(ili.rates.child.nat$YearWeek) >= paste(start.year, '-01', sep=''),'NormalizedBurn']
    # plot.new() #-------------------
    par(mfrow=c(1,1))
    par(mar = c(5, 4, 4, 4) + 0.3)
    plot(as.factor(x1.child), y1.child, type='n', col='black')
    lines(as.factor(x1.child), y1.child, col='black', lwd=3)
    mtext("ILI Patients per Patients Observed", side=2, line=2.5, col='black');
    axis(2, col.axis='black'); par(new = TRUE)
    plot(as.factor(x1.child), z1.child, col='red', type = 'n', axes = FALSE, bty = "n", xlab = "", ylab = "")
    lines(as.factor(x1.child), z1.child, col='red', lwd=3)
    axis(side=4, at = pretty(range(z1.child)), col.axis='red')
    mtext("Normalized RP Test Run Rate", side=4, line=3, col='red');
    #  axis(4, col='black',col.axis='black');
    title(main='National Weekly CDC ILI vs. RP Test Normalized Run Rate\n(child Hospital Populations Averaged Over All Participating Regions)')
    mtext("Year-Week", side=1, line=3, col='black')
    legend("topleft", legend=c('CDC','FilmArray RP'), text.col=c('black','red'), pch=c(16,15), col=c('black','red'), cex=0.8)
    p.nation.child.overlay <- recordPlot()
  }
}

# MULTI-VARIATE REGRESSION MODEL ON NORMALIZED BURN WITH RP PREVALENCE
if(TRUE) {
  
  prevalence.burn <- merge(with(runs.reg.norm, aggregate(NormalizedBurn~YearWeek, FUN=mean)), prevalence.cdc.nat, by='YearWeek')
  fit.burn.all <- lm(as.formula(paste('NormalizedBurn', paste(colnames(prevalence.burn)[grep(paste('^',paste(letters, collapse='|^'),sep=''), colnames(prevalence.burn))], collapse='+'), sep='~')), prevalence.burn)
  
  # choose the "best" model from model.eval.six and run ANOVA between that and the model including all organisms
  model.eval.burn.six <- do.call(rbind, lapply(1:length(combos.six[,'Combo']), function(x) data.frame(Model = combos.six[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn)), prevalence.burn$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.six[x,]), sep='~')), prevalence.burn), fit.burn.all)[,'Pr(>F)'][2])))
  as.character(model.eval.burn.six[model.eval.burn.six$anova.all == max(model.eval.burn.six$anova.all), 'Model'])
  as.character(model.eval.burn.six[model.eval.burn.six$corr == max(model.eval.burn.six$corr), 'Model'])
  as.character(model.eval.burn.six[model.eval.burn.six$adjR2 == max(model.eval.burn.six$adjR2), 'Model'])
  # the best six-organisms model is m + u + b + n + o + i (FluA H1-2009, RSV, Bordetella, FluA H3, FluB, HRV)
  
  # however, the best six-organism model still has p-value indicating that all >>>> six, so we must try again with a seven organism model
  model.eval.burn.seven <- do.call(rbind, lapply(1:length(combos.seven[,'Combo']), function(x) data.frame(Model = combos.seven[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn)), prevalence.burn$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.seven[x,]), sep='~')), prevalence.burn), fit.burn.all)[,'Pr(>F)'][2])))
  as.character(model.eval.burn.seven[model.eval.burn.seven$anova.all == max(model.eval.burn.seven$anova.all), 'Model'])
  as.character(model.eval.burn.seven[model.eval.burn.seven$corr == max(model.eval.burn.seven$corr), 'Model'])
  as.character(model.eval.burn.seven[model.eval.burn.seven$adjR2 == max(model.eval.burn.seven$adjR2), 'Model'])
  # the best seven-organism model is m + u + b + n + o + i + a (FluA H1-2009, RSV, Bordetella, FluA H3, FluB, HRV, Adeno)
  
  # however, the best seven-organism model still has a p-value indicating that all >>> seven, so we must try again with an eight organism model
  model.eval.burn.eight <- do.call(rbind, lapply(1:length(combos.eight[,'Combo']), function(x) data.frame(Model = combos.eight[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn)), prevalence.burn$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn))$coeff[2:7,4] <= 0.05), alpha1 = 6, anova.all = anova(lm(as.formula(paste('NormalizedBurn', as.character(combos.eight[x,]), sep='~')), prevalence.burn), fit.burn.all)[,'Pr(>F)'][2])))
  as.character(model.eval.burn.eight[model.eval.burn.eight$anova.all == max(model.eval.burn.eight$anova.all), 'Model'])
  as.character(model.eval.burn.eight[model.eval.burn.eight$corr == max(model.eval.burn.eight$corr), 'Model'])
  as.character(model.eval.burn.eight[model.eval.burn.eight$adjR2 == max(model.eval.burn.eight$adjR2), 'Model'])
  # the best eight-organism model is f + n + o + g + c + h + l + m (Corona NL63, FluA H3, FluB, Corona OC43, C pneumoniae, Human Metapneumo, FluA H1, FluA H1-2009)
  
  
  
}

# NREVSS OVERLAID WITH PREVALENCE DATA FOR RSV, HMP, ADENO, AND PIV AT THE MOST SPECIFIC LEVEL AVAILABLE
if(TRUE) {
  
  # get the data
  rsv.ny <- read.csv('../DataSources/NREVSS/RSV.csv', header=TRUE, sep=',')
  hmp.ne <- read.csv('../DataSources/NREVSS/HumanMetapneumo_NorthEastRegion.csv', header=TRUE, sep=',')
  hmp.mw <- read.csv('../DataSources/NREVSS/HumanMetapneumo_MidWestRegion.csv', header=TRUE, sep=',')
  adeno.nat <- read.csv('../DataSources/NREVSS/Adeno_National.csv', header=TRUE, sep=',')
  para1.nat <- read.csv('../DataSources/NREVSS/ParaFlu1_National.csv', header=TRUE, sep=',')
  para2.nat <- read.csv('../DataSources/NREVSS/ParaFlu2_National.csv', header=TRUE, sep=',')
  para3.nat <- read.csv('../DataSources/NREVSS/ParaFlu3_National.csv', header=TRUE, sep=',')
  
  # reformat it so it's nice to work with
  rsv.ny <- data.frame(YearWeek = with(rsv.ny, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalence = rsv.ny$Percent.Positive/100)
  hmp.ne <- data.frame(YearWeek = with(hmp.ne, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = hmp.ne$PCR/100, ReportedPrevalenceAntigen = hmp.ne$AntigenDetection/100)
  hmp.mw <- data.frame(YearWeek = with(hmp.mw, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = hmp.mw$PCR/100, ReportedPrevalenceAntigen = hmp.mw$AntigenDetection/100)
  adeno.nat <- data.frame(YearWeek = with(adeno.nat, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = adeno.nat$PCR, ReportedPrevalenceAntigen = adeno.nat$AntigenDetection, ReportedPrevalenceVI = adeno.nat$VirusIsolation)
  para1.nat <- data.frame(YearWeek = with(para1.nat, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = para1.nat$PCR, ReportedPrevalenceAntigen = para1.nat$AntigenDetection, ReportedPrevalenceVI = para1.nat$VirusIsolation)
  para2.nat <- data.frame(YearWeek = with(para2.nat, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = para2.nat$PCR, ReportedPrevalenceAntigen = para2.nat$AntigenDetection, ReportedPrevalenceVI = para2.nat$VirusIsolation)
  para3.nat <- data.frame(YearWeek = with(para3.nat, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-'))), ReportedPrevalencePCR = para3.nat$PCR, ReportedPrevalenceAntigen = para3.nat$AntigenDetection, ReportedPrevalenceVI = para3.nat$VirusIsolation)
  
  # RSV 
  prevalence.bfdx.rsv <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$CustomerSiteId %in% c(10, 5, 9, 36, 39), 'YearWeek']), TotalRuns = with(runs.reg.date[runs.reg.date$CustomerSiteId %in% c(10, 5, 9, 36), ], aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.rsv <- merge(prevalence.bfdx.rsv, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% c(10, 5, 9, 36, 39), c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Respiratory Syncytial Virus','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.rsv[is.na(prevalence.bfdx.rsv$Positive), 'Positive'] <- 0
  prevalence.bfdx.rsv <- data.frame(YearWeek =  prevalence.bfdx.rsv[2:(length(prevalence.bfdx.rsv$YearWeek)-1),'YearWeek'],
                                    TotalRuns = sapply(2:(length(prevalence.bfdx.rsv$YearWeek)-1), function(x) sum(prevalence.bfdx.rsv[(x-1):(x+1), 'TotalRuns'])), 
                                    Positives = sapply(2:(length(prevalence.bfdx.rsv$YearWeek)-1), function(x) sum(prevalence.bfdx.rsv[(x-1):(x+1), 'Positive']))
  )
  prevalence.bfdx.rsv$BFDXPrevalence <- with(prevalence.bfdx.rsv, Positives/TotalRuns)
  prevalence.rsv.ny <- merge(prevalence.bfdx.rsv, rsv.ny, by='YearWeek')
  p.rsv.ny <- ggplot(prevalence.rsv.ny, aes(x=YearWeek, y=ReportedPrevalence, group='NREVSS Reported Data', color='NREVSS Reported Data')) + geom_line() + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.rsv.ny) + scale_color_manual(values=c('red','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.rsv.ny$YearWeek)[order(as.character(prevalence.rsv.ny$YearWeek))][seq(1, length(as.character(prevalence.rsv.ny$YearWeek)), 4)]) + labs(title='Prevalence of Respiratory Syncytial Virus in New York\n(Centered 3-Week Moving Average)', x='Year-Week', y='Prevalence')
  
  # Human Metapneumovirus
  prevalence.bfdx.hmp <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Northeast', 'CensusRegionLocal']), 'YearWeek']), TotalRuns = with(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Northeast', 'CensusRegionLocal']), ], aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.hmp <- merge(prevalence.bfdx.hmp, with(merge(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Northeast', 'CensusRegionLocal']), c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Human Metapneumovirus','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.hmp[is.na(prevalence.bfdx.hmp$Positive), 'Positive'] <- 0
  prevalence.bfdx.hmp$BFDXPrevalence <- with(prevalence.bfdx.hmp, Positive/TotalRuns)
  prevalence.hmp.ne <- merge(prevalence.bfdx.hmp, hmp.ne, by='YearWeek')
  p.hmp.ne <- ggplot(prevalence.hmp.ne, aes(x=YearWeek, y=ReportedPrevalencePCR, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.hmp.ne) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.hmp.ne) + scale_color_manual(values=c('red','blue','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.hmp.ne$YearWeek)[order(as.character(prevalence.hmp.ne$YearWeek))][seq(1, length(as.character(prevalence.hmp.ne$YearWeek)), 4)]) + labs(title='Prevalence of Human Metapneumovirus in Northeast Region\n(Weekly Average)', x='Year-Week', y='Prevalence')
  
  prevalence.bfdx.hmp.mw <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Midwest', 'CensusRegionLocal']), 'YearWeek']), TotalRuns = with(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Midwest', 'CensusRegionLocal']), ], aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.hmp.mw <- merge(prevalence.bfdx.hmp.mw, with(merge(runs.reg.date[runs.reg.date$Region %in% as.character(regions.df[regions.df$CensusRegionNational == 'Midwest', 'CensusRegionLocal']), c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Human Metapneumovirus','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.hmp.mw[is.na(prevalence.bfdx.hmp.mw$Positive), 'Positive'] <- 0
  prevalence.bfdx.hmp.mw$BFDXPrevalence <- with(prevalence.bfdx.hmp.mw, Positive/TotalRuns)
  prevalence.hmp.mw <- merge(prevalence.bfdx.hmp.mw, hmp.mw, by='YearWeek')
  p.hmp.mw <- ggplot(prevalence.hmp.mw, aes(x=YearWeek, y=ReportedPrevalencePCR, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.hmp.mw) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.hmp.mw) + scale_color_manual(values=c('red','blue','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.hmp.mw$YearWeek)[order(as.character(prevalence.hmp.mw$YearWeek))][seq(1, length(as.character(prevalence.hmp.mw$YearWeek)), 4)]) + labs(title='Prevalence of Human Metapneumovirus in Midwest Region\n(Weekly Average)', x='Year-Week', y='Prevalence')
  
  # Adeno
  prevalence.bfdx.adeno <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, 'YearWeek']), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites), aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.adeno <- merge(prevalence.bfdx.adeno, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Adenovirus','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.adeno[is.na(prevalence.bfdx.adeno$Positive), 'Positive'] <- 0
  prevalence.bfdx.adeno$BFDXPrevalence <- with(prevalence.bfdx.adeno, Positive/TotalRuns)
  prevalence.adeno <- merge(prevalence.bfdx.adeno, adeno.nat, by='YearWeek')
  p.adeno <- ggplot(prevalence.adeno, aes(x=YearWeek, y=ReportedPrevalencePCR/100, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen/100, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.adeno) + geom_line(aes(x=YearWeek, y=ReportedPrevalenceVI/100, group='NREVSS Reported Viral Isolation Data', color='NREVSS Reported Viral Isolation Data'), prevalence.adeno) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.adeno) + scale_color_manual(values=c('red','blue','green','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.adeno$YearWeek)[order(as.character(prevalence.adeno$YearWeek))][seq(1, length(as.character(prevalence.adeno$YearWeek)), 4)]) + labs(title='National Prevalence of Adenovirus\n(Weekly Average)', x='Year-Week', y='Prevalence')
  
  # PIV1
  prevalence.bfdx.para1 <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, 'YearWeek']), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites), aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.para1 <- merge(prevalence.bfdx.para1, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Parainfluenza Virus 1','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.para1[is.na(prevalence.bfdx.para1$Positive), 'Positive'] <- 0
  prevalence.bfdx.para1$BFDXPrevalence <- with(prevalence.bfdx.para1, Positive/TotalRuns)
  prevalence.para1 <- merge(prevalence.bfdx.para1, para1.nat, by='YearWeek')
  p.para1 <- ggplot(prevalence.para1, aes(x=YearWeek, y=ReportedPrevalencePCR/100, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen/100, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.para1) + geom_line(aes(x=YearWeek, y=ReportedPrevalenceVI/100, group='NREVSS Reported Viral Isolation Data', color='NREVSS Reported Viral Isolation Data'), prevalence.para1) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.para1) + scale_color_manual(values=c('red','blue','green','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.para1$YearWeek)[order(as.character(prevalence.para1$YearWeek))][seq(1, length(as.character(prevalence.para1$YearWeek)), 4)]) + labs(title='National Prevalence of Parainfluenza I\n(Weekly Average)', x='Year-Week', y='Prevalence')
  
  # PIV2
  prevalence.bfdx.para2 <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, 'YearWeek']), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites), aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.para2 <- merge(prevalence.bfdx.para2, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Parainfluenza Virus 2','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.para2[is.na(prevalence.bfdx.para2$Positive), 'Positive'] <- 0
  prevalence.bfdx.para2$BFDXPrevalence <- with(prevalence.bfdx.para2, Positive/TotalRuns)
  prevalence.para2 <- merge(prevalence.bfdx.para2, para2.nat, by='YearWeek')
  p.para2 <- ggplot(prevalence.para2, aes(x=YearWeek, y=ReportedPrevalencePCR/100, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen/100, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.para2) + geom_line(aes(x=YearWeek, y=ReportedPrevalenceVI/100, group='NREVSS Reported Viral Isolation Data', color='NREVSS Reported Viral Isolation Data'), prevalence.para2) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.para2) + scale_color_manual(values=c('red','blue','green','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.para2$YearWeek)[order(as.character(prevalence.para2$YearWeek))][seq(2, length(as.character(prevalence.para2$YearWeek)), 4)]) + labs(title='National Prevalence of Parainfluenza II\n(Weekly Average)', x='Year-Week', y='Prevalence')
  
  # PIV3
  prevalence.bfdx.para3 <- data.frame(YearWeek = unique(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, 'YearWeek']), TotalRuns = with(subset(runs.reg.date, CustomerSiteId %in% sites), aggregate(Record~YearWeek, FUN=sum))$Record)
  prevalence.bfdx.para3 <- merge(prevalence.bfdx.para3, with(merge(runs.reg.date[runs.reg.date$CustomerSiteId %in% sites, c('YearWeek','RunDataId')], data.frame(RunDataId = bugs.df[bugs.df$BugPositive == 'Parainfluenza Virus 3','RunDataId'], Positive = 1), by='RunDataId'), aggregate(Positive~YearWeek, FUN=sum)), by='YearWeek', all.x=TRUE)
  prevalence.bfdx.para3[is.na(prevalence.bfdx.para3$Positive), 'Positive'] <- 0
  prevalence.bfdx.para3$BFDXPrevalence <- with(prevalence.bfdx.para3, Positive/TotalRuns)
  prevalence.para3 <- merge(prevalence.bfdx.para3, para3.nat, by='YearWeek')
  p.para3 <- ggplot(prevalence.para3, aes(x=YearWeek, y=ReportedPrevalencePCR/100, group='NREVSS Reported PCR Data', color='NREVSS Reported PCR Data')) + geom_line() + geom_line(aes(x=YearWeek, y=ReportedPrevalenceAntigen/100, group='NREVSS Reported Antigen Data', color='NREVSS Reported Antigen Data'), prevalence.para3) + geom_line(aes(x=YearWeek, y=ReportedPrevalenceVI/100, group='NREVSS Reported Viral Isolation Data', color='NREVSS Reported Viral Isolation Data'), prevalence.para3) + geom_line(aes(x=YearWeek, y=BFDXPrevalence, group='FilmArray Observed Data', color='FilmArray Observed Data'), prevalence.para3) + scale_color_manual(values=c('red','blue','green','black'), name='') + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + scale_x_discrete(breaks = as.character(prevalence.para3$YearWeek)[order(as.character(prevalence.para3$YearWeek))][seq(3, length(as.character(prevalence.para3$YearWeek)), 4)]) + labs(title='National Prevalence of Parainfluenza III\n(Weekly Average)', x='Year-Week', y='Prevalence')
}

# TIME SERIES OF PREVALENCE
if(TRUE) {
  
  # ------ use the prevalence.cdc.nat to create this figure... will have to be reformatted to be one prevalance col with a bug name column
  bacterias <- as.character(decoder[decoder$Code %in% c('b','c','p'), 'Bug'])
  viruses <- as.character(decoder[!(as.character(decoder$Bug) %in% c('Human Rhinovirus/Enterovirus', bacterias)), 'Bug']) 
  rhino <- 'Human Rhinovirus/Enterovirus'
  fluAs <- as.character(decoder[grep('Influenza A', decoder$Bug),'Bug'])
  fluBs <- as.character(decoder[grep('Influenza B', decoder$Bug),'Bug'])
  rsv <- 'Respiratory Syncytial Virus'
  flus <- as.character(decoder[grep('Influenza', decoder$Bug),'Bug'])
  flusRSV <- c(flus, 'Respiratory Syncytial Virus')
  pivs <- as.character(decoder[grep('Parainfluenza', decoder$Bug),'Bug'])
  corona <- as.character(decoder[grep('Corona', decoder$Bug),'Bug'])
  adeno <- 'Adenovirus'
  hmp <- 'Human Metapneumovirus'
  
  prevalence.nat.individual.count <- with(prevalence.reg.count, aggregate(cbind(Runs, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)~YearWeek, FUN=sum))
  prevalence.nat.individual.bugs <- do.call(rbind, lapply(1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.nat.individual.count))), function(x) data.frame(YearWeek = prevalence.nat.individual.count$YearWeek, Code = letters[x], Prevalence = prevalence.nat.individual.count[,letters[x]]/prevalence.nat.individual.count$Runs)))
  prevalence.nat.individual.wrap <- merge(prevalence.nat.individual.bugs, decoder, by='Code', all.x=TRUE)
  prevalence.nat.individual.wrap <- merge(prevalence.nat.individual.wrap, cdc.reg.count.agg[,c('YearWeek','Rate')], by='YearWeek')
  prevalence.nat.individual.wrap$Bug <- as.character(prevalence.nat.individual.wrap$Bug)
  prevalence.nat.individual.wrap <- merge(prevalence.nat.individual.wrap, shortnames.df, by.x='Bug', by.y='Organism')
  prevalence.nat.individual.wrap <- prevalence.nat.individual.wrap[as.character(prevalence.nat.individual.wrap$YearWeek) >= '2014-01', ]
  bug.individual.Pal <- createPaletteOfVariableLength(prevalence.nat.individual.wrap, 'ShortName')
  
  p.bugs.individual.area <- ggplot(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + labs(title='Prevalence of Organisms in Trend Population', y='Prevalence of Organism', x='Year-Week') # + geom_line(aes(x=YearWeek, y=Rate*10, group=1), color='black', lwd=2)
  p.bacteria.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% bacterias), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Bacterial Organisms in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=2.5*Rate, group=1), color='black', lwd=2)
  p.viruses.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% viruses), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Viral Organisms in Tested Population with ILI Trend Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=10*Rate, group=1), color='black', lwd=2)
  
  p.fluAs.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluAs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Influenza A in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=5*Rate, group=1), color='black', lwd=2)
  p.fluBs.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% fluBs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Influenza B in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=2.5*Rate, group=1), color='black', lwd=2)
  p.rsv.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rsv), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of RSV in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=6*Rate, group=1), color='black', lwd=2)
  
  p.flusRSV.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% flusRSV), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Influenza and RSV in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=10*Rate, group=1), color='black', lwd=2)
  p.pivs.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% pivs), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Parainfluenza in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=3*Rate, group=1), color='black', lwd=2)
  p.corona.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% corona), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Coronavirus in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=3*Rate, group=1), color='black', lwd=2)
  p.rhino.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% rhino), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + labs(title='Prevalence of Human Rhino/Enterovirus in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=Rate*12, group=1), color='black', lwd=2)
  p.adeno.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% adeno), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Adenovirus in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=3*Rate, group=1), color='black', lwd=2)
  p.hmp.area <- ggplot(subset(prevalence.nat.individual.wrap[with(prevalence.nat.individual.wrap, order(ShortName, decreasing=TRUE)),], Bug %in% hmp), aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.individual.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.individual.wrap$YearWeek))[order(as.character(unique(prevalence.nat.individual.wrap$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.individual.wrap$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))  + labs(title='Prevalence of Human Metapneumovirus in Trend Population with ILI Overlay', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=3*Rate, group=1), color='black', lwd=2)
  
  # ------ try to show the differences of onset between regions of different organisms and burn rate/ili in general
  # runs.reg.norm$Record <- 1
  # reg.norm.overlay <- with(runs.reg.norm, aggregate(NormalizedBurn~YearWeek+Region, FUN=mean))
  # reg.norm.overlay <- merge(reg.norm.overlay, with(runs.reg.norm, aggregate(Record~YearWeek+Region, FUN=sum)), by=c('YearWeek','Region'))
  # reg.norm.overlay <- reg.norm.overlay[reg.norm.overlay$Record >= 3, ] # this isn't useful b/c there's only one region with enough data...
  # meaning there is no way to overlay regions
}

# FOR SEASONAL ORGAISMS, SHOW OVERLAYS OF ONSET OF DISEASE BY LOCATION
prevalence.reg.rate <- data.frame(YearWeek = prevalence.reg.count$YearWeek, Region = prevalence.reg.count$Region, Name = prevalence.reg.count$Name, CustomerSiteId = prevalence.reg.count$CustomerSiteId,
                                  do.call(cbind, lapply(1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.reg.count))), function(x) prevalence.reg.count[,letters[x]]/prevalence.reg.count$Runs)))
colnames(prevalence.reg.rate)[grep('^X', colnames(prevalence.reg.rate))] <- letters[1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.reg.count)))]
prevalence.reg.rate.agg <- with(prevalence.reg.rate, aggregate(cbind(d, e, f, g, j, k, l, m, n, o, q, r, s, t, u)~YearWeek+Region, FUN=mean))
# ggplot(prevalence.reg.rate.agg[as.character(prevalence.reg.rate.agg$YearWeek) >= '2015-01' & prevalence.reg.rate.agg$Region %in% c('Mid-Atlantic','East North Central'), ], aes(x=YearWeek, y=u, group=Region, color=Region)) + geom_line() + geom_point()
p.regional.rsv.overlay <- ggplot(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], aes(x=YearWeek, y=u, group=as.factor(CustomerSiteId), color=as.factor(CustomerSiteId))) + stat_smooth(se=FALSE, method='lm', formula = y~poly(x, 9), size=1.5) + scale_y_continuous(labels=percent, limits=c(0,NA)) + scale_color_manual(values=createPaletteOfVariableLength(prevalence.reg.rate[prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], 'CustomerSiteId'), name='', labels=c('Midwest','Northeast','Mid-Atlantic','North Central')) + scale_x_discrete(breaks=as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))[order(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29),'YearWeek' ])))][seq(1, length(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))), 8)]) + theme(text=element_text(face='bold', size=20), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), panel.background=element_rect(color='white', fill='white')) + labs(title='Ninth-Order Quadratic Smoothed Trend of RSV Prevalence\nat Various Locations in the U.S.', x='YearWeek', y='Prevalence (3-Week Centered Average)')
p.regional.oc43.overlay <- ggplot(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], aes(x=YearWeek, y=g, group=as.factor(CustomerSiteId), color=as.factor(CustomerSiteId))) + stat_smooth(se=FALSE, method='lm', formula = y~poly(x, 9), size=1.5) + scale_y_continuous(labels=percent, limits=c(0,NA)) + scale_color_manual(values=createPaletteOfVariableLength(prevalence.reg.rate[prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], 'CustomerSiteId'), name='', labels=c('Midwest','Northeast','Mid-Atlantic','North Central')) + scale_x_discrete(breaks=as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))[order(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29),'YearWeek' ])))][seq(1, length(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))), 8)]) + theme(text=element_text(face='bold', size=20), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), panel.background=element_rect(color='white', fill='white')) + labs(title='Ninth-Order Quadratic Smoothed Trend of Corona OC43 Prevalence\nat Various Locations in the U.S.', x='YearWeek', y='Prevalence (3-Week Centered Average)')
p.regional.flua.overlay <- ggplot(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], aes(x=YearWeek, y=j, group=as.factor(CustomerSiteId), color=as.factor(CustomerSiteId))) + stat_smooth(se=FALSE, method='lm', formula = y~poly(x, 9), size=1.5) + scale_y_continuous(labels=percent, limits=c(0,NA)) + scale_color_manual(values=createPaletteOfVariableLength(prevalence.reg.rate[prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), ], 'CustomerSiteId'), name='', labels=c('Midwest','Northeast','Mid-Atlantic','North Central')) + scale_x_discrete(breaks=as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))[order(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29),'YearWeek' ])))][seq(1, length(as.character(unique(prevalence.reg.rate[as.character(prevalence.reg.rate$YearWeek) >= '2015-01' & as.character(prevalence.reg.rate$YearWeek) <= '2016-31' & prevalence.reg.rate$CustomerSiteId %in% c(2, 5, 7, 29), 'YearWeek']))), 8)]) + theme(text=element_text(face='bold', size=20), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), panel.background=element_rect(color='white', fill='white')) + labs(title='Ninth-Order Quadratic Smoothed Trend of Flu A Prevalence\nat Various Locations in the U.S.', x='YearWeek', y='Prevalence (3-Week Centered Average)')

# FMSI calculation sample
    # abs.log.pval <- abs(log(summary(fit.best)$coeff[2:7,4]))
    # abs.log.cont <- abs.log.pval/sum(abs.log.pval) # this could be some indicator of contribution to the fit... 
    #                                                # should look up something scientifically correct
    # abs.log.cont <- merge(data.frame(Code = names(abs.log.cont), Contribution = unname(abs.log.cont)), decoder, by='Code')
    # abs.log.cont <- abs.log.cont[with(abs.log.cont, order(Contribution, decreasing = TRUE)), ]

# sample of fit plotting
  # p.mod.fit.6 <- ggplot(prev.nat.fit, aes(x=YearWeek, y=Rate, group='ILI Reported', color='ILI Reported')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=Predict.mod.6, group='Model Prediction', color='Model Prediction'), prev.nat.fit) + geom_point(aes(x=YearWeek, y=Predict.mod.6, group='Model Prediction', color='Model Prediction'), prev.nat.fit) + scale_color_manual(values=c('black','red'), name='') + annotate('text', x=130, y=0.05, label = paste(paste('R2 = ',round(summary(fit.best.six)$adj.r.squared, 3), sep=''), paste('Correlation = ', round(cor(prev.nat.fit$Rate, prev.nat.fit$Predict.mod.6),3), sep=''), sep='\n')) + scale_x_discrete(breaks=as.character(unique(prev.nat.fit$YearWeek))[order(as.character(unique(prev.nat.fit$YearWeek)))][seq(1, length(as.character(unique(prev.nat.fit$YearWeek))), 6)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90)) +scale_y_continuous(label=percent) + labs(title='ILI Rate:\nReported by CDC vs. Predicted by Regression Model', y='ILI Rate', x='Year-Week')
  # p.mod.fit.7 <- ggplot(prev.nat.fit, aes(x=YearWeek, y=Rate, group='ILI Reported', color='ILI Reported')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=Predict.mod.7, group='Model Prediction', color='Model Prediction'), prev.nat.fit) + geom_point(aes(x=YearWeek, y=Predict.mod.7, group='Model Prediction', color='Model Prediction'), prev.nat.fit) + scale_color_manual(values=c('black','red'), name='') + annotate('text', x=130, y=0.05, label = paste(paste('R2 = ',round(summary(fit.best.seven)$adj.r.squared, 3), sep=''), paste('Correlation = ', round(cor(prev.nat.fit$Rate, prev.nat.fit$Predict.mod.7),3), sep=''), sep='\n')) + scale_x_discrete(breaks=as.character(unique(prev.nat.fit$YearWeek))[order(as.character(unique(prev.nat.fit$YearWeek)))][seq(1, length(as.character(unique(prev.nat.fit$YearWeek))), 7)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + scale_y_continuous(label=percent) + labs(title='ILI Rate:\nReported by CDC vs. Predicted by Regression Model', y='ILI Rate', x='Year-Week')

# FIGURE THREE: The viruses contributing to ILI may be a small part of overall disease in the population
if(TRUE) {

  runs.nat.norm <- with(runs.reg.norm, aggregate(NormalizedBurn~YearWeek, FUN=mean))
  prev.bfdx.nat <- merge(prev.nat.fit[,1:22], runs.nat.norm, by='YearWeek')
  
  # to illustrate that the organsisms that make up the best model for ILI are low contributors to overall disease, make some nice
  # visual aides; also, potentially combine these bugs so there is an overall contriubution which is easy to see...
  prevalence.nat.count <- with(subset(prevalence.reg.count,CustomerSiteId %in% sites), aggregate(cbind(Runs, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)~YearWeek, FUN=sum))
  prevalence.nat.wrap <- do.call(rbind, lapply(1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.nat.count))), function(x) data.frame(YearWeek = prevalence.nat.count$YearWeek, Code = letters[x], Prevalence = prevalence.nat.count[,letters[x]]/prevalence.nat.count$Runs)))
  prevalence.nat.count$v <- prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][1]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][2]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][3]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][4]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][5]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][6]] +
                            prevalence.nat.count[,strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)][7]] 
  prevalence.nat.wrap <- do.call(rbind, lapply(1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.nat.count))), function(x) data.frame(YearWeek = prevalence.nat.count$YearWeek, Code = letters[x], Prevalence = prevalence.nat.count[,letters[x]]/prevalence.nat.count$Runs)))
  prevalence.nat.wrap <- merge(prevalence.nat.wrap, decoder, by='Code', all.x=TRUE)
  prevalence.nat.wrap <- merge(prevalence.nat.wrap, cdc.reg.count.agg[,c('YearWeek','Rate')], by='YearWeek')
  prevalence.nat.wrap$Bug <- as.character(prevalence.nat.wrap$Bug)
  prevalence.nat.wrap[is.na(prevalence.nat.wrap$Bug), 'Bug'] <- 'Regression Organisms'
  take.out <- strsplit(model.best.seven,'+')[[1]][seq(1, length(strsplit(model.best.seven,'+')[[1]]), 2)]
  prevalence.nat.sub <- prevalence.nat.wrap[!(prevalence.nat.wrap$Code %in% take.out), ]
  prevalence.nat.sub <- merge(prevalence.nat.sub, shortnames.df, by.x='Bug', by.y='Organism', all.x=TRUE)
  prevalence.nat.sub$ShortName <- as.character(prevalence.nat.sub$ShortName)
  prevalence.nat.sub[is.na(prevalence.nat.sub$ShortName), 'ShortName'] <- 'Regression Organisms'
  bug.Pal <- createPaletteOfVariableLength(prevalence.nat.sub, 'ShortName')
  
  p.bugs.showdiff.area <- ggplot(prevalence.nat.sub[with(prevalence.nat.sub, order(ShortName, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=ShortName, group=ShortName, order=ShortName), stat='identity', position='stack') + scale_fill_manual(values=bug.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.sub$YearWeek))[order(as.character(unique(prevalence.nat.sub$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.sub$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + labs(title='Prevalence of Organisms in Tested Population with ILI Trend Overlay\n(Organisms in Regression Model Grouped)', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=Rate*10, group=1), color='black', lwd=2)
 
  # # ----------------------------------- THIS IS TEDIOUS AND TIME-CONSUMING... A FUNCTION MAY BE BUILT IN THE FUTURE ---------------
  # # this chart demonstrates that the "Regression Organisms" have high seasonality that coincides with ILI season,
  # # but that the maximum prevalence of all organisms in this group is < 40% at peaks, despite peak overall
  # # positivity of RP tests being between 20-40% higher. This indicates that there are other drivers of resipirtory
  # # disease in the population that drive testing in hospitals and clinics.
  fit.all.burn <- lm(as.formula(paste('NormalizedBurn', paste(letters[1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prev.bfdx.nat)))], collapse='+'), sep='~')), prev.bfdx.nat)
  
  # do the same as above and test all 6-organism models
  fit.all.burn <- lm(NormalizedBurn~., prev.bfdx.nat[,2:23])
  cor.all.burn <- cor(fitted(fit.all.burn), prev.bfdx.nat$NormalizedBurn)
  aR2.all.burn <- summary(fit.all.burn)$adj.r.squared
  
  # model.eval.burn <- do.call(rbind, lapply(1:length(combos[,'Combo']), function(x) data.frame(Model = combos[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat)), prev.bfdx.nat$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6)))
  six.models.burn <- model.eval.burn[model.eval.burn$alpha.0 >= 4 & model.eval.burn$corr >= quantile(model.eval.burn$corr, 0.99) & model.eval.burn$adjR2 >= quantile(model.eval.burn$adjR2, 0.99), ]
  six.models.burn.cor.rank <- data.frame(Model = six.models.burn[with(six.models.burn, order(corr, decreasing = TRUE)), 'Model'], cor.rank = seq(1, length(six.models.burn$cor), 1))
  six.models.burn.ar2.rank <- data.frame(Model = six.models.burn[with(six.models.burn, order(adjR2, decreasing = TRUE)), 'Model'], ar2.rank = seq(1, length(six.models.burn$cor), 1))
  six.models.burn.rank <- merge(merge(six.models.burn, six.models.burn.cor.rank, by='Model'), six.models.burn.ar2.rank, by='Model')
  six.models.burn.rank$score <- with(six.models.burn.rank, cor.rank+ar2.rank)
  six.models.burn.rank <- six.models.burn.rank[with(six.models.burn.rank, order(score)), ]
  model.best.burn.six <- as.character(six.models.burn.rank[six.models.burn.rank$score == min(six.models.burn.rank$score), ][1,'Model'])
  fit.best.burn.six <- lm(as.formula(paste('NormalizedBurn', model.best.burn.six, sep='~')), prev.bfdx.nat)
  
  if(FALSE) {
    # 
    # # based on the p-values, the correlations, and the R2 values, choose:
    # # p-values: b, i, u; then e, g, m; then a, j
    # # correlation: u, g, f, j, e, h; then k, m, n (cor(prev.bfdx.nat[,c(2:22, 25)])[,'NormalizedBurn'])
    # # R2: g, u, j; then k, n, i, m, h
    # fit.1.burn <- lm(NormalizedBurn~u, prev.bfdx.nat) # anova(fit.0.burn, fit.1.burn) # adding u is very significant
    # fit.2.burn <- lm(NormalizedBurn~g, prev.bfdx.nat) # anova(fit.0.burn, fit.2.burn) # adding g is even more significant
    # fit.3.burn <- lm(NormalizedBurn~g+u, prev.bfdx.nat) # anova(fit.0.burn, fit.2.burn, fit.3.burn) # fit with g+u is improved over just g or u
    # fit.4.burn <- lm(NormalizedBurn~g+u+m, prev.bfdx.nat); anova(fit.0.burn, fit.3.burn, fit.4.burn, fit.all.burn)
    # fit.5.burn <- lm(NormalizedBurn~g+u+j, prev.bfdx.nat); anova(fit.0.burn, fit.3.burn, fit.5.burn, fit.all.burn)
    # fit.6.burn <- lm(NormalizedBurn~g+u+k, prev.bfdx.nat); anova(fit.0.burn, fit.3.burn, fit.6.burn, fit.all.burn) 
    # # m is better than j (fit.4 is better than fit.5 vs. fit.3 base), and fit.6 implies that k is slightly better than m
    # fit.7.burn <- lm(NormalizedBurn~g+u+k+m, prev.bfdx.nat); anova(fit.0.burn, fit.3.burn, fit.6.burn, fit.7.burn, fit.all.burn) 
    # # this implies that the improvement from adding both m and k is ** significant, so add back k for sure and maybe m
    # fit.8.burn <- lm(NormalizedBurn~u+k, prev.bfdx.nat) 
    # # surprisingly, fit.8.burn that doesn't include g is better than the fit that includes g
    # fit.9.burn <- lm(NormalizedBurn~u+k+j, prev.bfdx.nat) # adding back m or j is not useful
    # 
    # # a, b, c, d, e, f, h, i, l, n, o, p, q, r, s, t are left to investigate
    # fit.10.burn <- lm(NormalizedBurn~u+k+a, prev.bfdx.nat) 
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.10.burn) # the addition of a is very slightly useful (0.028), so let's try some others
    # fit.11.burn <- lm(NormalizedBurn~u+k+b, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.11.burn) # the addition of b is very significant (p-value < 1.6e-9)
    # fit.12.burn <- lm(NormalizedBurn~u+k+c, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.12.burn) # the addition of c is not useful
    # fit.13.burn <- lm(NormalizedBurn~u+k+d, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.13.burn) # addition of d to the model is not significant
    # fit.14.burn <- lm(NormalizedBurn~u+k+e, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.14.burn) # addition of e is not useful seemingly
    # fit.15.burn <- lm(NormalizedBurn~u+k+f, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.8.burn, fit.15.burn) # addition of f is not useful at all
    # fit.16.burn <- lm(NormalizedBurn~u+k+h, prev.bfdx.nat) # addition of h is not useful at all
    # fit.17.burn <- lm(NormalizedBurn~u+k+i, prev.bfdx.nat) # addition of i is highly significant (p-value < 8.4e-06)
    # # also, the adjusted R2 is 0.7084 and every varable is at *** level significance vs. fit.8 with adj.R2 = 0.67 with all significant)
    # 
    # cor(prev.bfdx.nat$NormalizedBurn, fitted(fit.8.burn)) # the correlation for fit.8 is 0.8211347
    # cor(prev.bfdx.nat$NormalizedBurn, fitted(fit.17.burn)) # the correlation for fit.17 is 0.8449902
    # cor(prev.bfdx.nat$NormalizedBurn, fitted(fit.all.burn)) # 0.9145046
    # 
    # # l, n, o, p, q, r, s, t are still left to investigate
    # fit.18.burn <- lm(NormalizedBurn~u+k+i+l, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.18.burn) # addition of l is * level
    # fit.19.burn <- lm(NormalizedBurn~u+k+i+n, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.19.burn) # addition of n is insignificant
    # fit.20.burn <- lm(NormalizedBurn~u+k+i+o, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.20.burn) # addition of o is insignificant
    # fit.21.burn <- lm(NormalizedBurn~u+k+i+p, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.21.burn) # addition of p is insignificant
    # fit.22.burn <- lm(NormalizedBurn~u+k+i+q, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.22.burn) # addition of q is insignificant
    # fit.23.burn <- lm(NormalizedBurn~u+k+i+r, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.23.burn) # addition of r is insignificant
    # fit.24.burn <- lm(NormalizedBurn~u+k+i+s, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.24.burn) # addition of s is insignificant
    # fit.25.burn <- lm(NormalizedBurn~u+k+i+t, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.25.burn) # addition of t is significant
    # # fit.25 has an adjusted R2 = 0.7375 and every variable is at *** level
    # cor(prev.bfdx.nat$NormalizedBurn, fitted(fit.25.burn)) # 0.8627041
    # 
    # # fit.25.burn is the best so far, and it only includes 4 variables. I will look at the impact of layering on some of the others that
    # # maybe didn't work well alone, but could in combination with fit.25 (just cycle letters in after t and keep checking)
    # fit.26.burn <- lm(NormalizedBurn~u+k+i+t+s, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.25.burn, fit.26.burn)
    # # results: a-no, b-yes, c-no, d-no, e-no, f-no, g-no, h-no, j-no, l-no, m-no, n-no, o-no, p-no, q-maybe, r-no, s-no
    # # b is actually a much better model than q, so choose b
    # fit.27.burn <- lm(NormalizedBurn~u+k+i+t+b, prev.bfdx.nat); anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.25.burn, fit.27.burn)
    # # fit.27 has an adjusted R2 = 0.7736 and 5 varaibles at *** (t is at **) and a correlation of 0.8836371
    # 
    # # do the same cycling, but with b now as part of the model
    # fit.28.burn <- lm(NormalizedBurn~u+k+i+t+b+a, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.25.burn, fit.27.burn, fit.28.burn)
    # # a-maybe, c-no, d-no, e-no, f-no, g-no, h-no, j-no, l-no, m-no, o-no, p-no, q-yes,r-no,s-no: impact of q and a are approximately equal
    # fit.29.burn <- lm(NormalizedBurn~u+k+i+t+b+a, prev.bfdx.nat) # adj R2 = 0.7833, 4 *** and 2 **, cor = 0.8897315
    # fit.30.burn <- lm(NormalizedBurn~u+k+i+t+b+q, prev.bfdx.nat) # adj R2 = 0.7836, 5 *** and 1 **, cor = 0.8899081
    # 
    # # so q is better than a... is adding a going to be beneficial? ANOVA shows that adding a is slightly beneficial (*) and from 31 to all is *
    # fit.31.burn <- lm(NormalizedBurn~u+k+i+t+b+q+a, prev.bfdx.nat)
    # anova(fit.0.burn, fit.1.burn, fit.17.burn, fit.25.burn, fit.27.burn, fit.30.burn, fit.all.burn)
    
    # keep fit.30.burn as the best because it has 6 organisms just like the other, so we can compare....
    # fit.best.burn <- fit.30.burn
  } # old
  
  # try the same thing again, but with seven and eight bugs, just to see what it's like, just for fun
  # model.eval.seven.burn <- do.call(rbind, lapply(1:length(seven.combos[,'Combo']), function(x) data.frame(Model = seven.combos[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat)), prev.bfdx.nat$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(seven.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6)))
  seven.models.burn <- model.eval.seven.burn[model.eval.seven.burn$alpha.0 >= 5 & model.eval.seven.burn$adjR2 > quantile(model.eval.seven.burn$adjR2, 0.99) & model.eval.seven.burn$corr > quantile(model.eval.seven.burn$corr, 0.99), ]
  seven.models.burn.cor.rank <- data.frame(Model = seven.models.burn[with(seven.models.burn, order(corr, decreasing = TRUE)), 'Model'], cor.rank = seq(1, length(seven.models.burn$cor), 1))
  seven.models.burn.ar2.rank <- data.frame(Model = seven.models.burn[with(seven.models.burn, order(adjR2, decreasing = TRUE)), 'Model'], ar2.rank = seq(1, length(seven.models.burn$cor), 1))
  seven.models.burn.rank <- merge(merge(seven.models.burn, seven.models.burn.cor.rank, by='Model'), seven.models.burn.ar2.rank, by='Model')
  seven.models.burn.rank$score <- with(seven.models.burn.rank, cor.rank+ar2.rank)
  seven.models.burn.rank <- seven.models.burn.rank[with(seven.models.burn.rank, order(score)), ]
  model.best.burn.seven <- as.character(seven.models.burn.rank[seven.models.burn.rank$score == min(seven.models.burn.rank$score), ][1,'Model'])
  fit.best.burn.seven <- lm(as.formula(paste('NormalizedBurn', model.best.burn.seven, sep='~')), prev.bfdx.nat)
  
  # Do I need to run it with 8 organisms in the model??
  anova(fit.best.burn.six, fit.best.burn.seven, fit.all.burn)
  
  # the ANOVA analysis shows that there is significant improvement between the six and seven organisms models, but the model including all variables
  # is still significantly better than the model with seven... so try with eight
  # eight.combos <- generateCombos(letters[1:21], 8)
  # write.csv(eight.combos,'../DataSources/modelCombos8.csv', row.names=FALSE)
  # eight.combos <- read.csv('../DataSources/modelCombos8.csv', header=TRUE, sep=',')
  # model.eval.eight.burn <- do.call(rbind, lapply(1:length(eight.combos[,'Combo']), function(x) data.frame(Model = eight.combos[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat)), prev.bfdx.nat$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(eight.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6)))
  eight.models.burn <- model.eval.eight.burn[model.eval.eight.burn$alpha.0 >= 5 & model.eval.eight.burn$adjR2 > quantile(model.eval.eight.burn$adjR2, 0.99) & model.eval.eight.burn$corr > quantile(model.eval.eight.burn$corr, 0.99), ]
  eight.models.burn.cor.rank <- data.frame(Model = eight.models.burn[with(eight.models.burn, order(corr, decreasing = TRUE)), 'Model'], cor.rank = seq(1, length(eight.models.burn$cor), 1))
  eight.models.burn.ar2.rank <- data.frame(Model = eight.models.burn[with(eight.models.burn, order(adjR2, decreasing = TRUE)), 'Model'], ar2.rank = seq(1, length(eight.models.burn$cor), 1))
  eight.models.burn.rank <- merge(merge(eight.models.burn, eight.models.burn.cor.rank, by='Model'), eight.models.burn.ar2.rank, by='Model')
  eight.models.burn.rank$score <- with(eight.models.burn.rank, cor.rank+ar2.rank)
  eight.models.burn.rank <- eight.models.burn.rank[with(eight.models.burn.rank, order(score)), ]
  model.best.burn.eight <- as.character(eight.models.burn.rank[eight.models.burn.rank$score == min(eight.models.burn.rank$score), ][1,'Model'])
  fit.best.burn.eight <- lm(as.formula(paste('NormalizedBurn', model.best.burn.eight, sep='~')), prev.bfdx.nat)
  
  anova(fit.best.burn.six, fit.best.burn.seven, fit.best.burn.eight, fit.all.burn) # this shows that all > 8 >> 7 >>> 6 >
  
  # I guess that based on this, I should re-run for all combos of 9 organisms...
  fit.burn.nine <- lm(as.formula(paste('NormalizedBurn', paste(model.best.burn.eight, 'c' ,sep='+'), sep='~')), prev.bfdx.nat)
  anova(fit.best.burn.six, fit.best.burn.seven, fit.best.burn.eight, fit.burn.nine, fit.all.burn)
  # eight + []:
  # b -  p-value from nine to all = 0.0512294
  # c -  p-value from nine to all = 0.0763680
  # d -  p-value from nine to all = 0.0140944
  # e -  p-value from nine to all = 0.0084242
  # h -  p-value from nine to all = 0.0101629
  # i -  p-value from nine to all = 0.0287205
  # j -  p-value from nine to all = 0.0083119
  # k -  p-value from nine to all = 0.0092933
  # o -  p-value from nine to all = 0.0513592
  # r -  p-value from nine to all = 0.0091476
  # s -  p-value from nine to all = 0.0175814
  # t -  p-value from nine to all = 0.0465862
  # 
  # based on this, the best addition would be c, b, or o to make all ~ 9 >> 8 >> 7 >>> 6
  # nine.combos <- generateCombos(letters[1:21], 9)
  # write.csv(nine.combos,'../DataSources/modelCombos9.csv', row.names=FALSE)
  # nine.combos <- read.csv('../DataSources/modelCombos9.csv', header=TRUE, sep=',')
  # model.eval.nine.burn <- do.call(rbind, lapply(1:length(nine.combos[,'Combo']), function(x) data.frame(Model = nine.combos[x,'Combo'], adjR2 = summary(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat))$adj.r.squared, corr = cor(fitted(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat)), prev.bfdx.nat$NormalizedBurn), alpha.0 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] < 0.0001), alpha.001 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.001), alpha.01 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.01), alpha.05 = sum(summary(lm(as.formula(paste('NormalizedBurn', as.character(nine.combos[x,]), sep='~')), prev.bfdx.nat))$coeff[2:7,4] <= 0.05), alpha1 = 6)))
  nine.models.burn <- model.eval.nine.burn[model.eval.nine.burn$alpha.0 >= 5 & model.eval.nine.burn$adjR2 > quantile(model.eval.nine.burn$adjR2, 0.99) & model.eval.nine.burn$corr > quantile(model.eval.nine.burn$corr, 0.99), ]
  nine.models.burn.cor.rank <- data.frame(Model = nine.models.burn[with(nine.models.burn, order(corr, decreasing = TRUE)), 'Model'], cor.rank = seq(1, length(nine.models.burn$cor), 1))
  nine.models.burn.ar2.rank <- data.frame(Model = nine.models.burn[with(nine.models.burn, order(adjR2, decreasing = TRUE)), 'Model'], ar2.rank = seq(1, length(nine.models.burn$cor), 1))
  nine.models.burn.rank <- merge(merge(nine.models.burn, nine.models.burn.cor.rank, by='Model'), nine.models.burn.ar2.rank, by='Model')
  nine.models.burn.rank$score <- with(nine.models.burn.rank, cor.rank+ar2.rank)
  nine.models.burn.rank <- nine.models.burn.rank[with(nine.models.burn.rank, order(score)), ]
  model.best.burn.nine <- as.character(nine.models.burn.rank[nine.models.burn.rank$score == min(nine.models.burn.rank$score), ][1,'Model'])
  fit.best.burn.nine <- lm(as.formula(paste('NormalizedBurn', model.best.burn.nine, sep='~')), prev.bfdx.nat)
  anova(fit.best.burn.six, fit.best.burn.seven, fit.best.burn.eight, fit.best.burn.nine, fit.all.burn) # all ~ 9 >> 8 >> 7 >>> 6
  
  if(TRUE) {
    # plot.new()
    # par(mfrow=c(2,2))
    # plot(fit.best.burn)
    # p.best.burn.dx <- recordPlot()
    # plot.new()
    # par(mfrow=c(2,2))
    # plot(fit.all.burn)
    # p.all.burn.dx <- recordPlot()
    # 
    # prev.bfdx.nat <- cbind(prev.bfdx.nat, Predict.all = fitted(fit.all.burn))
    # prev.bfdx.nat <- cbind(prev.bfdx.nat, Predict.mod = fitted(fit.best.burn))
    # 
    # abs.log.cont.burn <- merge(data.frame(Code = names(abs(log(summary(fit.best.burn)$coeff[2:7,4]))/sum(abs(log(summary(fit.best.burn)$coeff[2:7,4])))), Contribution = abs(log(summary(fit.best.burn)$coeff[2:7,4]))/sum(abs(log(summary(fit.best.burn)$coeff[2:7,4])))), decoder, by='Code')
    # abs.log.cont.burn <- abs.log.cont.burn[with(abs.log.cont.burn, order(Contribution, decreasing = TRUE)), ]
    # 
    # p.burn.mod.fit <- ggplot(prev.bfdx.nat, aes(x=YearWeek, y=NormalizedBurn, group='Normalized Utilization', color='Normalized Utilization')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=Predict.mod, group='Model Prediction', color='Model Prediction'), prev.bfdx.nat) + geom_point(aes(x=YearWeek, y=Predict.mod, group='Model Prediction', color='Model Prediction'), prev.bfdx.nat) + scale_color_manual(values=c('black','red'), name='') + annotate('text', x=130, y=0.05, label = paste(paste('R2 = ',round(summary(fit.best.burn)$adj.r.squared, 3), sep=''), paste('Correlation = ', round(cor(prev.bfdx.nat$NormalizedBurn, prev.bfdx.nat$Predict.mod),3), sep=''), sep='\n')) + scale_x_discrete(breaks=as.character(unique(prev.bfdx.nat$YearWeek))[order(as.character(unique(prev.bfdx.nat$YearWeek)))][seq(1, length(as.character(unique(prev.bfdx.nat$YearWeek))), 6)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90)) + labs(title='Utilization Rate:\nObserved Utilization vs. Predicted by Regression Model', y='Normalized Utilization Rate', x='Year-Week')
    # 
    # prevalence.nat.bfdx.count <- prevalence.nat.count
    # prevalence.nat.bfdx.count$v <- with(prevalence.nat.count, u+b+k+t+i+q)
    # prevalence.nat.bfdx.wrap <- do.call(rbind, lapply(1:length(grep(paste('^', paste(letters, collapse='|^'), sep=''), colnames(prevalence.nat.bfdx.count))), function(x) data.frame(YearWeek = prevalence.nat.bfdx.count$YearWeek, Code = letters[x], Prevalence = prevalence.nat.bfdx.count[,letters[x]]/prevalence.nat.bfdx.count$Runs)))
    # prevalence.nat.bfdx.wrap <- merge(prevalence.nat.bfdx.wrap, decoder, by='Code', all.x=TRUE)
    # prevalence.nat.bfdx.wrap <- merge(prevalence.nat.bfdx.wrap, cdc.nat.df, by='YearWeek')
    # prevalence.nat.bfdx.wrap$Bug <- as.character(prevalence.nat.bfdx.wrap$Bug)
    # prevalence.nat.bfdx.wrap[is.na(prevalence.nat.bfdx.wrap$Bug), 'Bug'] <- 'Regression Organisms'
    # take.out <- c('b','u','k','i','t','q')
    # prevalence.nat.bfdx.sub <- prevalence.nat.bfdx.wrap[!(prevalence.nat.bfdx.wrap$Code %in% take.out), ]
    # bug.Pal <- createPaletteOfVariableLength(prevalence.nat.bfdx.sub, 'Bug')
    # 
    # # THIS DOESN'T LOOK RIGHT... NEED TO OVERLAY BURN RATE AND HAVE REGRESSION ORGANISMS gROUPED... the DATA SHOULD LOOK LIKE THE PIOR CHART SO DOUBLE CHECK
    # p.bugs.showdiff.area <- ggplot(prevalence.nat.bfdx.sub[with(prevalence.nat.bfdx.sub, order(Bug, decreasing=TRUE)),], aes(x=YearWeek)) + geom_area(aes(y=Prevalence, fill=Bug, group=Bug, order=Bug), stat='identity', position='stack') + scale_fill_manual(values=bug.Pal, name='') + scale_x_discrete(breaks = as.character(unique(prevalence.nat.sub$YearWeek))[order(as.character(unique(prevalence.nat.sub$YearWeek)))][seq(1, length(as.character(unique(prevalence.nat.sub$YearWeek))), 8)]) + scale_y_continuous(label=percent) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1)) + labs(title='Prevalence of Organisms in Tested Population with ILI Trend Overlay\n(Organisms in Regression Model Grouped)', y='Prevalence of Organism', x='Year-Week') + geom_line(aes(x=YearWeek, y=Rate*10, group=1), color='black', lwd=2)
  }
  
  burn.nat.fit <- cbind(prev.bfdx.nat, Predict.mod.9 = fitted(fit.best.burn.nine))
  p.burn.mod.fit.9 <- ggplot(burn.nat.fit, aes(x=YearWeek, y=NormalizedBurn, group='Normalized Burn Rate', color='Normalized Burn Rate')) + geom_line() + geom_point() + geom_line(aes(x=YearWeek, y=Predict.mod.9, group='Model Prediction', color='Model Prediction'), burn.nat.fit) + geom_point(aes(x=YearWeek, y=Predict.mod.9, group='Model Prediction', color='Model Prediction'), burn.nat.fit) + scale_color_manual(values=c('black','red'), name='') + annotate('text', x=130, y=4, label = paste(paste('R2 = ',round(summary(fit.best.burn.nine)$adj.r.squared, 3), sep=''), paste('Correlation = ', round(cor(burn.nat.fit$NormalizedBurn, burn.nat.fit$Predict.mod.9),3), sep=''), sep='\n')) + scale_x_discrete(breaks=as.character(unique(burn.nat.fit$YearWeek))[order(as.character(unique(burn.nat.fit$YearWeek)))][seq(1, length(as.character(unique(burn.nat.fit$YearWeek))), 7)]) + theme(text=element_text(size=20, face='bold'), axis.text=element_text(size=14, color='black', face='bold'), axis.text.x=element_text(angle=90), legend.position='bottom', panel.background=element_rect(color='white', fill='white')) + labs(title='Normalized Burn Rate:\nObserved vs. Predicted by Regression Model', y='Normalized Burn Rate', x='Year-Week')
}
