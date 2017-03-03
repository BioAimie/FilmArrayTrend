workDir <-'~/FilmArrayTrend/DataCleaningOfQcRuns/'
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
require(dateManip)
library(dplyr)
library(tidyr)

# load custom functions
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')
source('../Rfunctions/generateCombosBetter.R')

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- readLines('../DataSources/SQL/DataCleaningOfQcRuns/AllSitesRespiratoryRuns.sql')
query <- paste(queryVector,collapse="\n")
runs.df <- sqlQuery(FADWcxn,query)
runs.df$Instrument <- as.character(runs.df$Instrument)
queryVector <- readLines('../DataSources/SQL/DataCleaningOfQcRuns/PositiveBugsRP.sql')
query <- paste(queryVector,collapse="\n")
bugs.df <- sqlQuery(FADWcxn,query)
bugs.df <- bugs.df[bugs.df$BugPositive != 'Bocavirus',]
queryVector <- readLines('../DataSources/ShortNames.sql')
query <- paste(queryVector,collapse="\n")
shortnames.df <- sqlQuery(FADWcxn,query)
queryVector <- readLines('../DataSources/SQL/DataCleaningOfQcRuns/maineMolecular.sql')
query <- paste(queryVector,collapse="\n")
mm.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in the data about dates of instrument shipments from PMS_PROD
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- readLines('../DataSources/SQL/DataCleaningOfQcRuns/ShipDatesOfInstruments.sql')
query <- paste(queryVector, collapse="\n")
shipments.df <- sqlQuery(PMScxn, query)
shipments.df <- shipments.df[shipments.df$SerialNo %in% runs.df$Instrument, ]
shipments.df$SerialNo <- as.character(shipments.df$SerialNo)
odbcClose(PMScxn)

# add the number of positive assays per run
positives.per.run <- with(data.frame(bugs.df, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))
runs.df <- merge(runs.df, positives.per.run, by='RunDataId', all.x=TRUE)
runs.df[is.na(runs.df$Positives),'Positives'] <- 0

# do some work to determine the dates of arival of instruments at a site, including rearrival after a service event
serials <- unique(runs.df$Instrument)[order(unique(runs.df$Instrument))]
inst.arrival.marked <- c()
for(i in 1:length(serials)) {
  
  print(serials[i])
  inst.ship.dates <- shipments.df[shipments.df$SerialNo==serials[i], ][order(shipments.df[shipments.df$SerialNo==serials[i], 'Date']), ]
  inst.run.dates <- runs.df[runs.df$Instrument==serials[i], ][order(runs.df[runs.df$Instrument==serials[i], 'Date']), ]
  
  inst.first.runs <- c()
  for(j in 1:length(inst.ship.dates$Date)) {
    
    inst.first.run <- min(inst.run.dates[inst.run.dates$Date >= inst.ship.dates$Date[j], 'Date'])
    if(is.na(inst.first.run)) { break() }
    days.since.shipped <- as.numeric(inst.first.run - inst.ship.dates$Date[j])
    
    temp <- data.frame(Instrument = serials[i], ShipDate = inst.ship.dates$Date[j], FirstRunDate = inst.first.run, DaysSinceShipment = days.since.shipped)
    inst.first.runs <- rbind(inst.first.runs, temp)
  }
  print('made it through first j loop')
  inst.first.runs.agg <- with(inst.first.runs[!(is.infinite(inst.first.runs$DaysSinceShipment)), ], aggregate(DaysSinceShipment~FirstRunDate, FUN=min))
  temp <- merge(inst.first.runs, inst.first.runs.agg, by=c('FirstRunDate','DaysSinceShipment'))[,c('Instrument','ShipDate','FirstRunDate', 'DaysSinceShipment')]
  temp <- unique(temp)
  
  # now there is a data frame called temp that contains the shipment dates and run dates for each instrument... but I need to determine the days since 
  # arrival for each run, which will require determining the most recent arrival date
  inst.run.dates <- inst.run.dates[with(inst.run.dates, order(Date)), ]
  inst.runs.before <- inst.run.dates[inst.run.dates$Date < temp$FirstRunDate[1], ]
  inst.runs.arrival <- inst.runs.before
  if(nrow(inst.runs.arrival)!=0) { inst.runs.arrival$ArrivalDate <- as.Date('2000-01-01') }
  
  for(j in 1:length(temp$FirstRunDate)) {
    
    inst.runs.temp <- inst.run.dates[inst.run.dates$Date >= temp$FirstRunDate[j], ]
    if(nrow(inst.runs.temp)==0) { break() }
    
    if(length(temp$FirstRunDate)==1) {
      
      inst.runs.temp$ArrivalDate <- temp$FirstRunDate[j]
    } else {
      
      if(j < length(temp$FirstRunDate)) {
        
        inst.runs.temp <- inst.runs.temp[inst.runs.temp$Date >= temp$FirstRunDate[j] & inst.runs.temp$Date < temp$FirstRunDate[j+1], ]
        inst.runs.temp$ArrivalDate <- temp$FirstRunDate[j]
      } else {
        
        inst.runs.temp <- inst.runs.temp[inst.runs.temp$Date >= temp$FirstRunDate[j], ]
        inst.runs.temp$ArrivalDate <- temp$FirstRunDate[j]
      }
    }
    
    inst.runs.arrival <- rbind(inst.runs.arrival, inst.runs.temp)
  }
  
  inst.arrival.marked <- rbind(inst.arrival.marked, inst.runs.arrival)
}

inst.arrival.marked$DaysSinceArrival <- with(inst.arrival.marked, as.numeric(Date - ArrivalDate))





# identify runs as being Maine Molecular if they meet a certain criteria
mm.assays <- unique(mm.df[mm.df$ResultType=='Organism', c('RunDataId','AssayName')])
mm.assays <- mm.assays[with(mm.assays, order(RunDataId, AssayName)), ]
mm.assays.combo <- do.call(rbind, lapply(1:length(unique(mm.assays$RunDataId)), function(x) data.frame(RunDataId = unique(mm.assays$RunDataId)[x], AssayCombo = paste(as.character(mm.assays[mm.assays$RunDataId==unique(mm.assays$RunDataId)[x], 'AssayName']), collapse=', '))))
# since some might have false negatives, check for runs that match the signiture of Maine Molecular with a few missing
m.211 <- c('Adeno','Adeno2','Entero1','Entero2','FluA-H1-2009','FluA-H1-pan','FluA-H3','FluA-pan2','hMPV','HRV1','HRV2','HRV3','HRV4','PIV1','PIV4')
m.211.combos <- c(generateCombos(m.211, 11, FALSE), generateCombos(m.211, 12, FALSE), generateCombos(m.211, 13, FALSE), generateCombos(m.211, 14, FALSE), generateCombos(m.211, 15, FALSE))
m.211.combos <- do.call(rbind, lapply(1:length(m.211.combos), function(x) data.frame(PossibleCombo = paste(m.211.combos[[x]], collapse=', '))))
m.211.runs <- do.call(rbind, lapply(1:length(m.211.combos$PossibleCombo), function(x) data.frame(mm.assays.combo[grep(m.211.combos[x,'PossibleCombo'], mm.assays.combo[, 'AssayCombo']), ])))
m.211.runs$Key <- 'M211v1.1'
m.211.runs.distinct <- unique(m.211.runs)
m.212 <- c('Bper','CoV-229E','CoV-HKU1','CoV-NL63','CoV-OC43','Cpne','FluA-H1-pan','FluA-pan1','FluB','Mpne','PIV2','PIV3','RSV')
m.212.combos <- c(generateCombos(m.212, 9, FALSE), generateCombos(m.212, 10, FALSE), generateCombos(m.212, 11, FALSE), generateCombos(m.212, 12, FALSE), generateCombos(m.212, 13, FALSE))
m.212.combos <- do.call(rbind, lapply(1:length(m.212.combos), function(x) data.frame(PossibleCombo = paste(m.212.combos[[x]], collapse=', '))))
m.212.runs <- do.call(rbind, lapply(1:length(m.212.combos$PossibleCombo), function(x) data.frame(mm.assays.combo[grep(m.212.combos[x,'PossibleCombo'], mm.assays.combo[, 'AssayCombo']), ])))
m.212.runs$Key <- 'M212v1.1'
m.212.runs.distinct <- unique(m.212.runs)
mm.runs.distinct <- rbind(m.211.runs.distinct, m.212.runs.distinct)
# in the data frame of possible runs that are Maine Molecular with false negatives, join these onto a data frame that would be the result if all the assays were positive
mm.qc.runs <- merge(mm.df, mm.runs.distinct[,c('RunDataId','Key')], by='RunDataId')
mm.qc.cps.agg <- do.call(rbind, lapply(1:length(unique(mm.qc.runs$RunDataId)), function(x) do.call(rbind, lapply(1:length(unique(mm.qc.runs[mm.qc.runs$RunDataId==unique(mm.qc.runs$RunDataId)[x],'AssayName'])), function(y) data.frame(RunDataId = unique(mm.qc.runs$RunDataId)[x], Mix = unique(mm.qc.runs[mm.qc.runs$RunDataId==unique(mm.qc.runs$RunDataId)[x], 'Key']), AssayName = unique(mm.qc.runs[mm.qc.runs$RunDataId==unique(mm.qc.runs$RunDataId)[x],'AssayName'])[y], Cps = paste(mm.qc.runs[mm.qc.runs$RunDataId==unique(mm.qc.runs$RunDataId)[x] & mm.qc.runs$AssayName==unique(mm.qc.runs[mm.qc.runs$RunDataId==unique(mm.qc.runs$RunDataId)[x],'AssayName'])[y], 'Cp'], collapse=', '))))))
mm.qc.cps.211 <- mm.qc.cps.agg[mm.qc.cps.agg$Mix=='M211v1.1', ]
mm.qc.cps.211.base <- do.call(rbind, lapply(1:length(unique(mm.qc.cps.211$RunDataId)), function(x) data.frame(RunDataId = unique(mm.qc.cps.211$RunDataId)[x], Mix = 'M211v1.1', AssayName = c(m.211, 'yeastRNA'), Cps = NA)))
mm.qc.cps.211 <- merge(mm.qc.cps.211.base, mm.qc.cps.211, by=c('RunDataId','Mix','AssayName'), all.x=TRUE)
mm.qc.cps.211[!(is.na(mm.qc.cps.211$Cps.y)), 'Cps'] <- as.character(mm.qc.cps.211[!(is.na(mm.qc.cps.211$Cps.y)), 'Cps.y'])
mm.qc.cps.211[is.na(mm.qc.cps.211$Cps.y), 'Cps'] <- 'SuspectedFalseNegative'
mm.qc.cps.212 <- mm.qc.cps.agg[mm.qc.cps.agg$Mix=='M212v1.1', ]
mm.qc.cps.212.base <- do.call(rbind, lapply(1:length(unique(mm.qc.cps.212$RunDataId)), function(x) data.frame(RunDataId = unique(mm.qc.cps.212$RunDataId)[x], Mix = 'M212v1.1', AssayName = c(m.212, 'yeastRNA'), Cps = NA)))
mm.qc.cps.212 <- merge(mm.qc.cps.212.base, mm.qc.cps.212, by=c('RunDataId','Mix','AssayName'), all.x=TRUE)
mm.qc.cps.212[!(is.na(mm.qc.cps.212$Cps.y)), 'Cps'] <- as.character(mm.qc.cps.212[!(is.na(mm.qc.cps.212$Cps.y)), 'Cps.y'])
mm.qc.cps.212[is.na(mm.qc.cps.212$Cps.y), 'Cps'] <- 'SuspectedFalseNegative'
mm.runs.cps <- rbind(mm.qc.cps.211[,c('RunDataId','Mix','AssayName','Cps')], mm.qc.cps.212[,c('RunDataId','Mix','AssayName','Cps')])
write.csv(mm.runs.cps, 'PossibleMaineMolecularRunsWithFalseNegatives_TrendCpData.csv', row.names = FALSE)





# hist(inst.arrival.dates$DaysSinceShipment)
# there are several instruments where the days since it shipped to when the first run occurred are > 30, which is unexpected...
# it looks like this could be due to the instrument shipping and running for some time at the institution prior to Trend exports
# to figure out what the "correct" number of days should be to consider that this is happening, try to see the distribution of QC runs
# of omega at the institutions
inst.arrival.dates.trim <- inst.arrival.dates[inst.arrival.dates$DaysSinceShipment < 30, ]
runs.arrivals.marked <- merge(runs.df, inst.arrival.dates.trim, by.x=c('Instrument','Date'), by.y=c('Instrument','FirstRunDate'), all.x=TRUE)
serials.trim <- as.character(unique(runs.arrivals.marked$Instrument))

