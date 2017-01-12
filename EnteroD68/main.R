setwd('~/FilmArrayTrend/EnteroD68/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(devtools)
require(dateManip)
library(cluster)
library(caret)
library(dbscan)

# create an Epi date calendar that will be used by all the data sets
startYear <- 2013
calendar.df <- createCalendarLikeMicrosoft(startYear, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))

# set up some constants
imgDir <- 'Figures/'
dateBreaks <- unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])[order(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek']))][seq(1, length(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])), 8)]

# set some query variables, like the customer site... also, get the number of RP runs by site
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/EnteroD68/sitesRunningRP.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
sites.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/EnteroD68/rpRunsBySite.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# start a loop to gather Cp data for all sites running RP
cp.df <- c()
choose.sites <- as.character(sites.df[,'CustomerSiteId'])
for(j in 1:length(choose.sites)) {
  
  FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD') 
  queryVector <- scan('../DataSources/SQL/EnteroD68/rpDataBySite.sql', what=character(), quote="")
  query <- paste(gsub('SITE_INDEX', choose.sites[j], queryVector), collapse=" ")
  cp.site.df <- sqlQuery(FADWcxn, query)
  odbcClose(FADWcxn)
  
  cp.df <- rbind(cp.df, cp.site.df)
}

# next, create a master data frame that combines a lot of parameters which may work for identifying patterns in Rhino/Entero target
#### QUESTION : SHOULD THIS BE DONE BY TARGET SUCH THAT CO-DETECTIONS ARE HANDLED DIFFERENTLY?!?! MAY NEED TO DO ANALYSIS TO FIND OUT
# ----------------------------------------------------------------------------------------------------------------------------------
# 1. Find the sequence related to each positive target in RP
rhino.only <- TRUE
if(rhino.only) {
  # find the median Cp of each positive assay in the HRV/Entero target
  cp.rhino <- subset(cp.df, TargetName=='Human Rhinovirus/Enterovirus')
  cp.rhino.assays <- subset(cp.rhino, AssayType=='Organism')
  run.ids <- unique(cp.rhino$RunDataId)

  cp.rhino.median <- do.call(rbind, lapply(1:length(run.ids), function(x)
      do.call(rbind,  lapply(1:length(unique(cp.rhino[cp.rhino$RunDataId==run.ids[x],'AssayName'])), function(z)
        data.frame(RunDataId = run.ids[x],
                   TargetName = 'Human Rhinovirus/Enterovirus',
                   AssayName = unique(cp.rhino[cp.rhino$RunDataId==run.ids[x], 'AssayName'])[z],
                   MedianCp = median(cp.rhino[cp.rhino$RunDataId==run.ids[x] & cp.rhino$AssayName==unique(cp.rhino[cp.rhino$RunDataId==run.ids[x],'AssayName'])[z], 'Cp'])
        )
      ))
  ))
  # order the Cps from least to greatest
  cp.rhino.median <- merge(cp.rhino.median, unique(cp.rhino.assays[,c('RunDataId','CustomerSiteId')]))
  cp.rhino.ordered <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(cp.rhino.median[cp.rhino.median$RunDataId==run.ids[x], ][order(cp.rhino.median[cp.rhino.median$RunDataId==run.ids[x], 'MedianCp']), ], Index = seq(1, length(cp.rhino.median[cp.rhino.median$RunDataId==run.ids[x], 'MedianCp']), 1))))
  # generate a signature
  cp.rhino.sequence <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(RunDataId = run.ids[x], Sequence = paste(as.character(cp.rhino.ordered[cp.rhino.ordered$RunDataId==run.ids[x], 'AssayName']), collapse=', '))))
}

if(FALSE) {
  # find the median Cp of each positive assay in the target
  cp.assays <- subset(cp.df, AssayType=='Organism')
  runs <- unique(cp.assays$RunDataId)
  cp.assays.median <- do.call(rbind, lapply(1:length(runs), function(x)
    do.call(rbind, lapply(1:length(unique(cp.assays[cp.assays$RunDataId==runs[x], 'TargetName'])), function(y)
      do.call(rbind,  lapply(1:length(unique(cp.assays[cp.assays$RunDataId==runs[x] & cp.assays$TargetName==unique(cp.assays[cp.assays$RunDataId==runs[x], 'TargetName'])[y],'AssayName'])), function(z)
        data.frame(RunDataId = runs[x],
                   TargetName = unique(cp.assays[cp.assays$RunDataId==runs[x], 'TargetName'])[y],
                   AssayName = unique(cp.assays[cp.assays$RunDataId==runs[x] & cp.assays$TargetName==unique(cp.assays[cp.assays$RunDataId==runs[x], 'TargetName'])[y], 'AssayName'])[z],
                   MedianCp = median(cp.assays[cp.assays$RunDataId==runs[x] & cp.assays$TargetName==unique(cp.assays[cp.assays$RunDataId==runs[x],'TargetName'])[y] & cp.assays$AssayName==unique(cp.assays[cp.assays$RunDataId==runs[x] & cp.assays$TargetName==unique(cp.assays[cp.assays$RunDataId==runs[x],'TargetName'])[y],'AssayName'])[z], 'Cp'])
        )
      ))
    ))
  ))
  # order the Cps from least to greatest
  cp.assays.ordered <- do.call(rbind, lapply(1:length(runs), function(x) data.frame(cp.assays.median[cp.assays.median$RunDataId==runs[x], ][order(cp.assays.median[cp.assays.median$RunDataId==runs[x], 'MedianCp']), ], Index = seq(1, length(cp.assays.median[cp.assays.median$RunDataId==runs[x], 'MedianCp']), 1))))
  # generate a signature
  cp.assays.sequence <- do.call(rbind, lapply(1:length(runs), function(x) data.frame(RunDataId = runs[x], Sequence = paste(as.character(cp.assays.ordered[cp.assays.ordered$RunDataId==runs[x], 'AssayName']), collapse=', '))))
  sequence.index <- data.frame(Sequence = unique(cp.assays.sequence$Sequence), SequenceIndex = seq(1, length(unique(cp.assays.sequence$Sequence)), 1))
  cp.assays.sequence <- merge(cp.assays.sequence, sequence.index, by='Sequence')
  
  # 2. Find the delta between the minimum Cp and the first & last amplifying assay
  cp.assays.delta <- do.call(rbind, lapply(1:length(runs), function(x) data.frame(RunDataId = runs[x], Index = cp.assays.ordered[cp.assays.ordered$RunDataId==runs[x],'Index'], DeltaCp = (cp.assays.ordered[cp.assays.ordered$RunDataId==runs[x], 'MedianCp'] - cp.assays.ordered[cp.assays.ordered$RunDataId==runs[x] & cp.assays.ordered$Index==1, 'MedianCp']))))
  cp.assays.ordered <- merge(cp.assays.ordered, cp.assays.delta, by=c('RunDataId','Index'))
  cp.second.assay <- cp.assays.ordered[cp.assays.ordered$Index==2, c('RunDataId','MedianCp')]
  
  # 3. Create a data frame to feed into the algorithm
  cp.assays.min <- with(cp.assays.ordered, aggregate(MedianCp~RunDataId, FUN=min))
  cp.assays.max <- with(cp.assays.ordered, aggregate(MedianCp~RunDataId, FUN=max))
  cp.assays.count <- with(cp.assays.ordered, aggregate(Index~RunDataId, FUN=max))
  cp.assays.avg <- with(cp.assays.ordered, aggregate(MedianCp~RunDataId, FUN=mean))
  cp.assays.sd <- with(cp.assays.ordered, aggregate(MedianCp~RunDataId, FUN=sd))
  cp.delta.avg <- with(cp.assays.ordered, aggregate(DeltaCp~RunDataId, FUN=mean))
  cp.delta.sd <- with(cp.assays.ordered, aggregate(DeltaCp~RunDataId, FUN=sd))
  
  cp.learn <- merge(merge(merge(merge(cp.assays.count, cp.assays.min, by='RunDataId'), cp.assays.max, by='RunDataId'), cp.assays.avg, by='RunDataId'), cp.assays.sd, by='RunDataId')
  colnames(cp.learn) <- c('RunDataId', 'AssayCount', 'CpMin', 'CpMax', 'CpMean', 'CpSdev')
  cp.learn <- merge(cp.learn, cp.second.assay, by='RunDataId', all.x=TRUE)
  colnames(cp.learn)[length(cp.learn)] <- 'DeltaCpMin'
  cp.learn[cp.learn$AssayCount >= 2, 'DeltaCpMax'] <- cp.learn[cp.learn$AssayCount >= 2, 'CpMax'] - cp.learn[cp.learn$AssayCount >= 2, 'CpMin'] 
  cp.learn <- merge(cp.learn, cp.delta.avg, by='RunDataId', all.x=TRUE)
  colnames(cp.learn)[length(cp.learn)] <- 'DeltaCpMean'
  cp.learn <- merge(cp.learn, cp.delta.sd, by='RunDataId', all.x=TRUE)
  colnames(cp.learn)[length(cp.learn)] <- 'DeltaCpSdev'
  cp.learn <- merge(cp.learn, cp.assays.sequence[,c('RunDataId','SequenceIndex')], by='RunDataId')
  # handle NAs
  cp.learn[is.na(cp.learn$CpSdev), 'CpSdev'] <- 0
  cp.learn[is.na(cp.learn$DeltaCpMin), 'DeltaCpMin'] <- 0
  cp.learn[is.na(cp.learn$DeltaCpMax), 'DeltaCpMax'] <- 0
  cp.learn[is.na(cp.learn$DeltaCpSdev), 'DeltaCpSdev'] <- 0
  cp.learn.obs <- cp.learn[, colnames(cp.learn)!='RunDataId']
  
  # 4. Try some machine learning
  cp.cluster.alg.1 <- kmeans(cp.learn.obs, centers = 100, iter.max = 10)
}

# -----------------------------------------------------------------------------------------------------
# ISOLATE TO JUST RHINO/ENTERO FOR NOW
# -----------------------------------------------------------------------------------------------------
# generate a signature
rhino.sequence.index <- data.frame(Sequence = unique(cp.rhino.sequence$Sequence), SequenceIndex = seq(1, length(unique(cp.rhino.sequence$Sequence)), 1))
cp.rhino.sequence <- merge(cp.rhino.sequence, rhino.sequence.index, by='Sequence')

# try to make a data frame with a lot of features... 
rhino.assays <- data.frame(AssayName = as.character(unique(cp.rhino.ordered$AssayName))[order(as.character(unique(cp.rhino.ordered$AssayName)))], AssayIndex = seq(1, 6, 1))
first.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==1, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
first.rhino.assay <- merge(first.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
second.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==2, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
second.rhino.assay <- merge(second.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
third.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==3, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
third.rhino.assay <- merge(third.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
fourth.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==4, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
fourth.rhino.assay <- merge(fourth.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
fifth.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==5, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
fifth.rhino.assay <- merge(fifth.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
sixth.rhino.assay <- cp.rhino.ordered[cp.rhino.ordered$Index==6, c('RunDataId','AssayName','MedianCp','CustomerSiteId')]
sixth.rhino.assay <- merge(sixth.rhino.assay, rhino.assays, by='AssayName')[,c('RunDataId','AssayIndex','MedianCp','CustomerSiteId')]
rhino.features <- merge(first.rhino.assay, merge(second.rhino.assay, merge(third.rhino.assay, merge(merge(fourth.rhino.assay, fifth.rhino.assay, by=c('RunDataId','CustomerSiteId'), all.x=TRUE), sixth.rhino.assay, by=c('RunDataId','CustomerSiteId'), all.x=TRUE), by=c('RunDataId','CustomerSiteId'), all.x=TRUE), by=c('RunDataId','CustomerSiteId'), all.x=TRUE), by=c('RunDataId','CustomerSiteId'), all.x=TRUE)
colnames(rhino.features) <- c('RunDataId','CustomerSiteId','Assay1','Cp1','Assay2','Cp2','Assay3','Cp3','Assay4','Cp4','Assay5','Cp5','Assay6','Cp6')
sparse.cp <- 50
rhino.features[is.na(rhino.features$Cp2), 'Cp2'] <- sparse.cp
rhino.features[is.na(rhino.features$Cp3), 'Cp3'] <- sparse.cp
rhino.features[is.na(rhino.features$Cp4), 'Cp4'] <- sparse.cp
rhino.features[is.na(rhino.features$Cp5), 'Cp5'] <- sparse.cp
rhino.features[is.na(rhino.features$Cp6), 'Cp6'] <- sparse.cp
sparse.assay <- 0
rhino.features[is.na(rhino.features$Assay2), 'Assay2'] <- sparse.assay
rhino.features[is.na(rhino.features$Assay3), 'Assay3'] <- sparse.assay
rhino.features[is.na(rhino.features$Assay4), 'Assay4'] <- sparse.assay
rhino.features[is.na(rhino.features$Assay5), 'Assay5'] <- sparse.assay
rhino.features[is.na(rhino.features$Assay6), 'Assay6'] <- sparse.assay

# macro features
run.count <- with(runs.df, aggregate(Run~CustomerSiteId+Date, FUN=sum))
positive.count <- with(data.frame(unique(cp.df[,c('RunDataId','CustomerSiteId','Date')]), Positives=1), aggregate(Positives~Date+CustomerSiteId, FUN=sum))
rhino.positives <- with(data.frame(merge(unique(runs.df[,c('RunDataId','Date')]), unique(cp.rhino.median[,c('RunDataId','CustomerSiteId')]), by='RunDataId'), RhinoPositives = 1), aggregate(RhinoPositives~Date+CustomerSiteId, FUN=sum))
rhino.features <- merge(rhino.features, unique(runs.df[,c('RunDataId','Date')]), by='RunDataId')
macro.features <- merge(run.count, merge(positive.count, rhino.positives, by=c('Date','CustomerSiteId'), all.x=TRUE), by=c('Date','CustomerSiteId'), all.x=TRUE)
macro.features[is.na(macro.features$Positives),'Positives'] <- 0
macro.features[is.na(macro.features$RhinoPositives),'RhinoPositives'] <- 0
macro.features$Negatives <- macro.features$Run - macro.features$Positives
macro.features$PositiveRate <- macro.features$Positives/macro.features$Run
macro.features$NegativeRate <- macro.features$Negatives/macro.features$Run
macro.features$RhinoRate <- macro.features$RhinoPositives/macro.features$Run
rhino.features <- merge(rhino.features, macro.features, by=c('Date','CustomerSiteId'))
rhino.features <- merge(rhino.features, cp.rhino.sequence[,c('RunDataId','SequenceIndex')], by='RunDataId')
rhino.features.neat <- rhino.features[,!(colnames(rhino.features) %in% c('Date','RunDataId','CustomerSiteId'))]

# clustering
rhino.cluster.alg.1 <- kmeans(rhino.features.neat, centers = 20, iter.max = 20)
dbscan(rhino.features.neat, eps = 0.4, minPts = 5)
dbscan(rhino.features.neat[,c('SequenceIndex','PositiveRate','NegativeRate','RhinoRate')], eps=1, minPts=5)

# plot by cluster
cluster.alg.1.df <- cbind(rhino.features.neat, Cluster = rhino.cluster.alg.1$cluster)


