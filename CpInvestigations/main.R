# THIS CODE TAKES QUITE A LONG TIME TO RUN... ESPECIALLY FOR RP... IT IS BEST TO LET IT RUN OVERNIGHT AND COME BACK FOR RESULTS
setwd('~/FilmArrayTrend/CpInvestigations/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(devtools)
require(dateManip)

# create an Epi date calendar that will be used by all the data sets
startYear <- 2013
calendar.df <- createCalendarLikeMicrosoft(startYear, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))

# set up some constants
imgDir <- 'Figures/'
dateBreaks <- unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])[order(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek']))][seq(1, length(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])), 8)]

# set some query variables, like the customer site and the panel by querying the panels run by site
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/CpInvestigation/panelsBySite.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
filters.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# start a loop to gather Cp data and make charts
panels <- as.character(unique(filters.df$PouchTitle))
for(i in 1:length(panels)) {
  
  # load the data from SQL... if the panel is Respiratory, then that requires some special attention
  choose.panel <- switch(panels[i],
                         'Respiratory Panel v1.7' = 'RP',
                         'BCID Panel v2.0' = 'BCID',
                         'GI Panel v2.1' = 'GI',
                         'ME Panel v1.4' = 'ME'
  )
  
  # If the panel is not RP, then use all data and don't split out by site
  if(choose.panel != 'RP') {
    
    FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
    queryVector <- scan(paste('../DataSources/SQL/CpInvestigation/AssayCpByTarget_', choose.panel, '.txt', sep=''), what=character(), quote="")
    query <- paste(queryVector, collapse=" ")
    cp.df <- sqlQuery(FADWcxn, query)
    odbcClose(FADWcxn)
    
    # work on the data --------------------------------------------------------------------------------
    # first, loop through the runs, interpretations, and assays and determine the median Cp for all each assay
    runs <- unique(cp.df$RunDataId)
    cp.median <- do.call(rbind, lapply(1:length(runs), function(x) 
      do.call(rbind, lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])), function(y) 
        do.call(rbind,  lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y],'AssayName'])), function(z) 
          data.frame(RunDataId = runs[x], 
                     TargetName = unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y], 
                     AssayName = unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y], 'AssayName'])[z], 
                     MedianCp = median(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y] & cp.df$AssayName==unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y],'AssayName'])[z], 'Cp'])
          )
        ))
      ))
    ))
    
    # next, for each interpretation, find the minimum median Cp of all assays contributing to the interpretation
    cp.targets <- do.call(rbind, lapply(1:length(runs), function(x)
      do.call(rbind, lapply(1:length(unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])), function(y)
        data.frame(RunDataId = runs[x],
                   TargetName = unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y],
                   TargetTriggerAssay = cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y] & cp.median$MedianCp==min(cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp']),'AssayName'],
                   TargetMedianCp = min(cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp'])
        )
      ))
    ))
    
    # using the RunDataId, join on the Epi date and the number of positive targets in the test, then create an index of positives
    # do this for both the assay data and the target data
    cp.assays <- merge(unique(cp.median[,c('RunDataId','AssayName','MedianCp')]), unique(cp.df[,c('RunDataId','AssayName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','AssayName'))
    cp.targets <- merge(cp.targets, unique(cp.df[,c('RunDataId','TargetName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','TargetName'))
    cp.assays <- merge(cp.assays, calendar.df[,c('Date','YearWeek')], by='Date')
    cp.targets <- merge(cp.targets, calendar.df[,c('Date','YearWeek')], by='Date')
    
    # because the runs that happen prior to the start year are not in the data set (per the merge to calendar.df), it's neccessary to update the runs variable
    runs.keep <- unique(cp.targets$RunDataId)
    cp.assays.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.assays[cp.assays$RunDataId==runs.keep[x], ][order(cp.assays[cp.assays$RunDataId==runs.keep[x], 'MedianCp']), ], Index = seq(1, length(cp.assays[cp.assays$RunDataId==runs.keep[x], 'MedianCp']), 1))))
    cp.targets.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.targets[cp.targets$RunDataId==runs.keep[x], ][order(cp.targets[cp.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), ], Index = seq(1, length(cp.targets[cp.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), 1))))
    
    # with the ordered data, make some time series plots by Assay
    assays <- as.character(unique(cp.assays.ordered$AssayName))
    for(k in 1:length(assays)) {
      
      temp.dat <- merge(data.frame(YearWeek = unique(calendar.df[calendar.df$YearWeek >= dateBreaks[1], c('YearWeek')])), subset(cp.assays.ordered, AssayName==assays[k]), by='YearWeek', all.x=TRUE)
      temp.dat[is.na(temp.dat$RunDataId), 'MedianCp'] <- NA
      temp.dat[is.na(temp.dat$RunDataId), 'Index'] <- 1
      color.count <- max(temp.dat$Index, na.rm=TRUE)
      temp.plot <- ggplot(temp.dat, aes(x=YearWeek, y=MedianCp, color=as.factor(Index))) + geom_point(size=1.5) + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=1, angle=90), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + scale_x_discrete(breaks=dateBreaks) + labs(title=paste('Median Cp of', assays[k],'Assay in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Year-Week') + scale_color_manual(values=c(heat.colors(color.count)), labels=c('First','Second','Third','Fourth','Fifth','Sixth','Seventh','Eighth','Ninth','Tenth'), name='Detection Order')
      temp.file <- paste(imgDir, choose.panel, '_', gsub('\\/','-', assays[k]), '_TimeSeriesCps.png', sep='') 
      png(temp.file, width = 1400, height = 800)
      print(temp.plot)
      dev.off()
    }
    
    # make boxplot charts that are independent of time by Assay
    for(k in 1:length(assays)) {
      
      temp.dat <- subset(cp.assays.ordered, AssayName==assays[k])
      temp.plot <- ggplot(temp.dat, aes(as.factor(Index), MedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', assays[k],'in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Order of Detection')
      temp.file <- paste(imgDir, choose.panel, '_', gsub('\\/','-', assays[k]), '_DistributionCps.png', sep='')
      png(temp.file, width = 1400, height = 800)
      print(temp.plot)
      dev.off()
    }
    
    # make a facet-wraped chart by TargetName that shows the TargetTriggerAssay and associated TargetMedianCp
    targets <- as.character(unique(cp.targets.ordered$TargetName))
    for(k in 1:length(targets)) {
      
      temp.dat <- subset(cp.targets.ordered, TargetName==targets[k])
      if(length(as.character(unique(temp.dat$TargetTriggerAssay)))==1) { 
        
        break
      } else {
        
        temp.plot <- ggplot(temp.dat, aes(TargetTriggerAssay, TargetMedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', targets[k],'by','First Assay\nin Trend', choose.panel, 'Tests', sep=' '), y='Median Cp', x='')
        temp.file <- paste(imgDir, choose.panel, '_', gsub('\\/','-', targets[k]), '_DistributionCpsByTarget.png', sep='')
        png(temp.file, width = 1400, height = 800)
        print(temp.plot)
        dev.off()
      }
    }
    
    if(choose.panel=='BCID') {
      # when a gene is the target, then take the difference between the median Cp of the gene assay and all the other assays
      genes <- as.character(unique(cp.df[cp.df$AssayType=='Gene', 'AssayName']))
      for(k in 1:length(genes)) {
        
        temp.dat <- cp.median[cp.median$TargetName==genes[k], ]
        runs.temp <- unique(temp.dat$RunDataId)
        gene.temp <- do.call(rbind, lapply(1:length(runs.temp), function(x) data.frame(RunDataId = runs.temp[x], MedianGeneCp = subset(temp.dat[temp.dat$RunDataId==runs.temp[x], ], AssayName==genes[k])$MedianCp)))
        temp.mrg <- merge(temp.dat, gene.temp, by='RunDataId')
        temp.mrg$MedianCpDelta <- temp.mrg$MedianCp - temp.mrg$MedianGeneCp
        temp.plot <- ggplot(subset(temp.mrg, AssayName != genes[k]), aes(AssayName, MedianCpDelta)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + labs(title=paste('Median Cp of Assay - Median Cp of', genes[k], '\nin Trend', choose.panel ,'Tests', sep=' '), y='Median Cp', x='')
        temp.file <- paste(imgDir, choose.panel, '_', gsub('\\/','-', genes[k]), '_DistributionDeltaCps.png', sep='')
        png(temp.file, width = 1400, height = 800)
        print(temp.plot)
        dev.off()
      }
    }
  } 
  # if the panel is RP, then loop through the sites and make the charts by site... also, store all cp data by site so thumbnails can be made.
  else {
    
    choose.sites <- as.character(unique(filters.df[filters.df$PouchTitle == panels[i], 'CustomerSiteId']))[1:3]
    cp.df.all <- c()
    cp.assays.ordered.all <- c()
    cp.targets.ordered.all <- c()
    for(j in 1:length(choose.sites)) {
      
      FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
      queryVector <- scan(paste('../DataSources/SQL/CpInvestigation/AssayCpByTarget_', choose.panel, '.txt', sep=''), what=character(), quote="")
      query <- paste(c(queryVector, choose.sites[j]), collapse=" ")
      cp.df <- sqlQuery(FADWcxn, query)
      odbcClose(FADWcxn)
      
      cp.df.all <- rbind(cp.df.all, cp.df)
     
      # work on the data --------------------------------------------------------------------------------
      # first, loop through the runs, interpretations, and assays and determine the median Cp for all each assay
      runs <- unique(cp.df$RunDataId)
      cp.median <- do.call(rbind, lapply(1:length(runs), function(x) 
        do.call(rbind, lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])), function(y) 
          do.call(rbind,  lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y],'AssayName'])), function(z) 
            data.frame(RunDataId = runs[x], 
                       TargetName = unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y], 
                       AssayName = unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y], 'AssayName'])[z], 
                       MedianCp = median(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y] & cp.df$AssayName==unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y],'AssayName'])[z], 'Cp'])
            )
          ))
        ))
      ))
      
      # next, for each interpretation, find the minimum median Cp of all assays contributing to the interpretation
      cp.targets <- do.call(rbind, lapply(1:length(runs), function(x)
        do.call(rbind, lapply(1:length(unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])), function(y)
          data.frame(RunDataId = runs[x],
                     TargetName = unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y],
                     TargetTriggerAssay = cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y] & cp.median$MedianCp==min(cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp']),'AssayName'],
                     TargetMedianCp = min(cp.median[cp.median$RunDataId==runs[x] & cp.median$TargetName==unique(cp.median[cp.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp'])
          )
        ))
      ))
      
      # using the RunDataId, join on the Epi date and the number of positive targets in the test, then create an index of positives
      # do this for both the assay data and the target data
      cp.assays <- merge(unique(cp.median[,c('RunDataId','AssayName','MedianCp')]), unique(cp.df[,c('RunDataId','AssayName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','AssayName'))
      cp.targets <- merge(cp.targets, unique(cp.df[,c('RunDataId','TargetName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','TargetName'))
      cp.assays <- merge(cp.assays, calendar.df[,c('Date','YearWeek')], by='Date')
      cp.targets <- merge(cp.targets, calendar.df[,c('Date','YearWeek')], by='Date')
      
      # because the runs that happen prior to the start year are not in the data set (per the merge to calendar.df), it's neccessary to update the runs variable
      runs.keep <- unique(cp.targets$RunDataId)
      cp.assays.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.assays[cp.assays$RunDataId==runs.keep[x], ][order(cp.assays[cp.assays$RunDataId==runs.keep[x], 'MedianCp']), ], Index = seq(1, length(cp.assays[cp.assays$RunDataId==runs.keep[x], 'MedianCp']), 1))))
      cp.targets.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.targets[cp.targets$RunDataId==runs.keep[x], ][order(cp.targets[cp.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), ], Index = seq(1, length(cp.targets[cp.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), 1))))
      
      cp.assays.ordered.all <- rbind(cp.assays.ordered.all, cp.assays.ordered)
      cp.targets.ordered.all <- rbind(cp.targets.ordered.all, cp.targets.ordered)
      
      # with the ordered data, make some time series plots by Assay
      assays <- as.character(unique(cp.assays.ordered$AssayName))
      for(k in 1:length(assays)) {
        
        temp.dat <- merge(data.frame(YearWeek = unique(calendar.df[calendar.df$YearWeek >= dateBreaks[1], c('YearWeek')])), subset(cp.assays.ordered, AssayName==assays[k]), by='YearWeek', all.x=TRUE)
        temp.dat[is.na(temp.dat$RunDataId), 'MedianCp'] <- NA
        temp.dat[is.na(temp.dat$RunDataId), 'Index'] <- 1
        color.count <- max(temp.dat$Index, na.rm=TRUE)
        temp.plot <- ggplot(temp.dat, aes(x=YearWeek, y=MedianCp, color=as.factor(Index))) + geom_point(size=1.5) + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=1, angle=90), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + scale_x_discrete(breaks=dateBreaks) + labs(title=paste('Median Cp of', assays[k],'Assay in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Year-Week') + scale_color_manual(values=c(heat.colors(color.count)), labels=c('First','Second','Third','Fourth','Fifth','Sixth','Seventh','Eighth','Ninth','Tenth'), name='Detection Order')
        temp.file <- paste(imgDir, choose.panel, '_site', choose.sites[j], '_', gsub('\\/','-', assays[k]), '_TimeSeriesCps.png', sep='')
        png(temp.file, width = 1400, height = 800)
        print(temp.plot)
        dev.off()
      }
      
      # make boxplot charts that are independent of time by Assay
      for(k in 1:length(assays)) {
        
        temp.dat <- subset(cp.assays.ordered, AssayName==assays[k])
        temp.plot <- ggplot(temp.dat, aes(as.factor(Index), MedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', assays[k],'in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Order of Detection')
        temp.file <- paste(imgDir, choose.panel, '_site', choose.sites[j], '_', gsub('\\/','-', assays[k]), '_DistributionCps.png', sep='')
        png(temp.file, width = 1400, height = 800)
        print(temp.plot)
        dev.off()
      }
      
      # make a facet-wraped chart by TargetName that shows the TargetTriggerAssay and associated TargetMedianCp
      targets <- as.character(unique(cp.targets.ordered$TargetName))
      for(k in 1:length(targets)) {
        
        temp.dat <- subset(cp.targets.ordered, TargetName==targets[k])
        if(length(as.character(unique(temp.dat$TargetTriggerAssay)))==1) { 
          
          break
        } else {
          
          temp.plot <- ggplot(temp.dat, aes(TargetTriggerAssay, TargetMedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', targets[k],'by','First Assay\nin Trend', choose.panel, 'Tests', sep=' '), y='Median Cp', x='')
          temp.file <- paste(imgDir, choose.panel, '_site', choose.sites[j], '_', gsub('\\/','-', targets[k]), '_DistributionCpsByTarget.png', sep='')
          png(temp.file, width = 1400, height = 800)
          print(temp.plot)
          dev.off()
        }
      }
    }
    
    # now make some thumbnail charts
    cp.assays.ordered.all <- merge(cp.assays.ordered.all, unique(cp.df.all[,c('RunDataId','CustomerSiteId')]), by='RunDataId')
    cp.targets.ordered.all <- merge(cp.targets.ordered.all, unique(cp.df.all[,c('RunDataId','CustomerSiteId')]), by='RunDataId')
    
    assays <- as.character(unique(cp.assays.ordered.all$AssayName))
    for(k in 1:length(assays)) {
      
      temp.dat <- subset(cp.assays.ordered.all, AssayName==assays[k])
      temp.plot <- ggplot(temp.dat, aes(as.factor(Index), MedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=1, angle=45), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', assays[k],'at Trend Sites\nby Order of Detection', sep=' '), y='Median Cp', x='Order of Detection')
      temp.file <- paste(imgDir, choose.panel, '_allSites', '_', gsub('\\/','-', assays[k]), '_DistributionCps.png', sep='')
      png(temp.file, width = 1400, height = 800)
      print(temp.plot)
      dev.off()
    }
    
    targets <- as.character(unique(cp.targets.ordered.all$TargetName))
    for(k in 1:length(targets)) {
      
      temp.dat <- subset(cp.targets.ordered.all, TargetName==targets[k])
      if(length(as.character(unique(temp.dat$TargetTriggerAssay)))==1) { 
        
        break
      } else {
        
        temp.plot <- ggplot(temp.dat, aes(TargetTriggerAssay, TargetMedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=1, angle=45), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', targets[k],'by','First Assay\nin Trend', choose.panel, 'Tests by Site', sep=' '), y='Median Cp', x='') + facet_wrap(~CustomerSiteId)
        temp.file <- paste(imgDir, choose.panel, '_allSites', '_', gsub('\\/','-', targets[k]), '_DistributionCpsByTarget.png', sep='')
        png(temp.file, width = 1400, height = 800)
        print(temp.plot)
        dev.off()
      }
    }
  }
}

