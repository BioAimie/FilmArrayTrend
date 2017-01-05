setwd('~/FilmArrayTrend/EnteroD68/')

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
# ----------------------------------------------------------------------------------------------------------------------------------
# 1. Find the sequence related to each positive target in RP
rhino.only <- FALSE
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

# find the median Cp of each positive assay in the target
cp.assays <- subset(cp.df, AssayType=='Organism')
runs <- unique(cp.assays$RunDataId)
cp.median <- do.call(rbind, lapply(1:length(runs), function(x)
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
cp.assays.ordered <- do.call(rbind, lapply(1:length(runs), function(x) data.frame(cp.assays[cp.assays$RunDataId==runs[x], ][order(cp.assays[cp.assays$RunDataId==runs.keep[x], 'MedianCp']), ], Index = seq(1, length(cp.assays[cp.assays$RunDataId==runs[x], 'MedianCp']), 1))))
# generate a signature
cp.assays.sequence <- do.call(rbind, lapply(1:length(runs), function(x) data.frame(RunDataId = runs[x], Sequence = paste(as.character(cp.assays.ordered[cp.assays.ordered$RunDataId==runs[x], 'AssayName']), collapse=', '))))


