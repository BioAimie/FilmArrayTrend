setwd('~/FilmArrayTrend/CpInvestigations/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(devtools)
require(dateManip)

# load data from SQL
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/CpInvestigation/AssayCpByTarget_BCID.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
cp.bcid.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# create an Epi date calendar
startYear <- 2013
calendar.df <- createCalendarLikeMicrosoft(startYear, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))

# work on the data --------------------------------------------------------------------------------
# I NEED LINDSAY'S HELP BECAUSE I AM NOT ENTIRELY SURE HOW TO HANDLE GENES.... AND I'M NOT SURE HOW TO HANDLE MULTIPLE TARGETS HAVING A SINGLE ASSAY
# first, loop through the runs, interpretations, and assays and determine the median Cp for all each assay
runs <- unique(cp.bcid.df$RunDataId)
cp.bcid.median <- do.call(rbind, lapply(1:length(runs), function(x) 
  do.call(rbind, lapply(1:length(unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x], 'TargetName'])), function(y) 
    do.call(rbind,  lapply(1:length(unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x] & cp.bcid.df$TargetName==unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x], 'TargetName'])[y],'AssayName'])), function(z) 
      data.frame(RunDataId = runs[x], 
                 TargetName = unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x], 'TargetName'])[y], 
                 AssayName = unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x] & cp.bcid.df$TargetName==unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x], 'TargetName'])[y], 'AssayName'])[z], 
                 MedianCp = median(cp.bcid.df[cp.bcid.df$RunDataId==runs[x] & cp.bcid.df$TargetName==unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x],'TargetName'])[y] & cp.bcid.df$AssayName==unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x] & cp.bcid.df$TargetName==unique(cp.bcid.df[cp.bcid.df$RunDataId==runs[x],'TargetName'])[y],'AssayName'])[z], 'Cp'])
      )
    ))
  ))
))

# next, for each interpretation, find the minimum median Cp of all assays contributing to the interpretation
cp.bcid.targets <- do.call(rbind, lapply(1:length(runs), function(x)
  do.call(rbind, lapply(1:length(unique(cp.bcid.median[cp.bcid.median$RunDataId==runs[x], 'TargetName'])), function(y)
    data.frame(RunDataId = runs[x],
               TargetName = unique(cp.bcid.median[cp.bcid.median$RunDataId==runs[x], 'TargetName'])[y],
               TargetTriggerAssay = cp.bcid.median[cp.bcid.median$RunDataId==runs[x] & cp.bcid.median$TargetName==unique(cp.bcid.median[cp.bcid.median$RunDataId==runs[x], 'TargetName'])[y] & cp.bcid.median$MedianCp==min(cp.bcid.median[cp.bcid.median$RunDataId==runs[x] & cp.bcid.median$TargetName==unique(cp.bcid.median[cp.bcid.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp']),'AssayName'],
               TargetMedianCp = min(cp.bcid.median[cp.bcid.median$RunDataId==runs[x] & cp.bcid.median$TargetName==unique(cp.bcid.median[cp.bcid.median$RunDataId==runs[x], 'TargetName'])[y], 'MedianCp'])
    )
  ))
))

# using the RunDataId, join on the Epi date and the number of positive targets in the test, then create an index of positives
# do this for both the assay data and the target data
cp.bcid.assays <- merge(unique(cp.bcid.median[,c('RunDataId','AssayName','MedianCp')]), unique(cp.bcid.df[,c('RunDataId','AssayName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','AssayName'))
cp.bcid.targets <- merge(cp.bcid.targets, unique(cp.bcid.df[,c('RunDataId','TargetName','PositiveAssays','PositiveGenes','Date')]), by=c('RunDataId','TargetName'))
cp.bcid.assays <- merge(cp.bcid.assays, calendar.df[,c('Date','YearWeek')], by='Date')
cp.bcid.targets <- merge(cp.bcid.targets, calendar.df[,c('Date','YearWeek')], by='Date')

# because the runs that happen prior to 2013 are not in the data set (per the merge to calendar.df), it's neccessary to update the runs variable
runs.keep <- unique(cp.bcid.targets$RunDataId)
cp.bcid.assays.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.bcid.assays[cp.bcid.assays$RunDataId==runs.keep[x], ][order(cp.bcid.assays[cp.bcid.assays$RunDataId==runs.keep[x], 'MedianCp']), ], Index = seq(1, length(cp.bcid.assays[cp.bcid.assays$RunDataId==runs.keep[x], 'MedianCp']), 1))))
cp.bcid.targets.ordered <- do.call(rbind, lapply(1:length(runs.keep), function(x) data.frame(cp.bcid.targets[cp.bcid.targets$RunDataId==runs.keep[x], ][order(cp.bcid.targets[cp.bcid.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), ], Index = seq(1, length(cp.bcid.targets[cp.bcid.targets$RunDataId==runs.keep[x], 'TargetMedianCp']), 1))))

# create the figures --------------------------------------------------------------------------------
imgDir <- 'Figures/'
dateBreaks <- unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])[order(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek']))][seq(1, length(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])), 8)]

# with the ordered data, make some time series plots by Assay
assays <- as.character(unique(cp.bcid.assays.ordered$AssayName))
for(i in 1:length(assays)) {
  
  temp.dat <- merge(data.frame(YearWeek = unique(calendar.df[calendar.df$YearWeek >= dateBreaks[1], c('YearWeek')])), subset(cp.bcid.assays.ordered, AssayName==assays[i]), by='YearWeek', all.x=TRUE)
  temp.dat[is.na(temp.dat$RunDataId), 'MedianCp'] <- NA
  temp.dat[is.na(temp.dat$RunDataId), 'Index'] <- 1
  temp.plot <- ggplot(temp.dat, aes(x=YearWeek, y=MedianCp, color=as.factor(Index))) + geom_point() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=1, angle=90), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + scale_x_discrete(breaks=dateBreaks) + labs(title=paste('Median Cp of', assays[i],'Assay in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Year-Week') + scale_color_manual(values=c('black','grey29','grey42','grey48','grey71','grey87'), labels=c('First','Second','Third','Fourth','Fifth','Sixth'), name='Detection Order')
  temp.file <- paste(imgDir, gsub('\\/','-', assays[i]), '_TimeSeriesCps.png', sep='')
  png(temp.file, width = 1400, height = 800)
  print(temp.plot)
  dev.off()
}

# make boxplot charts that are independent of time by Assay
for(i in 1:length(assays)) {
  
  temp.dat <- subset(cp.bcid.assays.ordered, AssayName==assays[i])
  temp.plot <- ggplot(temp.dat, aes(as.factor(Index), MedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', assays[i],'in Trend Population\nby Order of Detection', sep=' '), y='Median Cp', x='Order of Detection')
  temp.file <- paste(imgDir, gsub('\\/','-', assays[i]), '_DistributionCps.png', sep='')
  png(temp.file, width = 1400, height = 800)
  print(temp.plot)
  dev.off()
}

# make a facet-wraped chart by TargetName that shows the TargetTriggerAssay and associated TargetMedianCp
targets <- as.character(unique(cp.bcid.targets.ordered$TargetName))
for(i in 1:length(targets)) {
  
  temp.dat <- subset(cp.bcid.targets.ordered, TargetName==targets[i])
  if(length(as.character(unique(temp.dat$TargetTriggerAssay)))==1) { 
    
    break
  } else {
    
    temp.plot <- ggplot(temp.dat, aes(TargetTriggerAssay, TargetMedianCp)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + scale_y_continuous(breaks=c(0,5,10,15,20,25,30), limits=c(0,30)) + labs(title=paste('Median Cp of', targets[i],'by','First Assay\nin Trend BCID Tests', sep=' '), y='Median Cp', x='')
    temp.file <- paste(imgDir, gsub('\\/','-', targets[i]), '_DistributionCpsByTarget.png', sep='')
    png(temp.file, width = 1400, height = 800)
    print(temp.plot)
    dev.off()
  }
}

# when a gene is the target, then take the difference between the median Cp of the gene assay and all the other assays
genes <- as.character(unique(cp.bcid.df[cp.bcid.df$AssayType=='Gene', 'AssayName']))
for(i in 1:length(genes)) {
  
  temp.dat <- cp.bcid.median[cp.bcid.median$TargetName==genes[i], ]
  runs.temp <- unique(temp.dat$RunDataId)
  gene.temp <- do.call(rbind, lapply(1:length(runs.temp), function(x) data.frame(RunDataId = runs.temp[x], MedianGeneCp = subset(temp.dat[temp.dat$RunDataId==runs.temp[x], ], AssayName==genes[i])$MedianCp)))
  temp.mrg <- merge(temp.dat, gene.temp, by='RunDataId')
  temp.mrg$MedianCpDelta <- temp.mrg$MedianCp - temp.mrg$MedianGeneCp
  temp.plot <- ggplot(subset(temp.mrg, AssayName != genes[i]), aes(AssayName, MedianCpDelta)) + geom_boxplot() + theme(plot.title=element_text(hjust=0.5), text=element_text(size=20, color='black', face='bold'), axis.text=element_text(size=18, color='black', face='bold'), axis.text.x=element_text(hjust=0.5), panel.background=element_rect(fill='white', color='transparent')) + labs(title=paste('Median Cp of Assay - Median Cp of', genes[i], '\nin Trend BCID Tests', sep=' '), y='Median Cp', x='')
  temp.file <- paste(imgDir, gsub('\\/','-', genes[i]), '_DistributionDeltaCps.png', sep='')
  png(temp.file, width = 1400, height = 800)
  print(temp.plot)
  dev.off()
}

