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

# load custom functions
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')

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
odbcClose(FADWcxn)

# read in the data about dates of instrument shipments from PMS_PROD
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- readLines('../DataSources/SQL/DataCleaningOfQcRuns/ShipDatesOfInstruments.sql')
query <- paste(queryVector, collapse="\n")
shipments.df <- sqlQuery(PMScxn, query)
shipments.df <- shipments.df[shipments.df$SerialNo %in% runs.df$Instrument, ]
shipments.df$SerialNo <- as.character(shipments.df$SerialNo)
odbcClose(PMScxn)

# do some work to determine the dates of arival of instruments at a site, including rearrival after a service event
serials <- unique(runs.df$Instrument)[order(unique(runs.df$Instrument))]
inst.arrival.dates <- c()
for(i in 1:length(serials)) {
  
  print(serials[i])
  inst.ship.dates <- shipments.df[shipments.df$SerialNo==serials[i], ]
  inst.run.dates <- runs.df[runs.df$Instrument==serials[i], ]
  
  inst.first.runs <- c()
  for(j in 1:length(inst.ship.dates$Date)) {
    
    inst.first.run <- min(inst.run.dates[inst.run.dates$Date >= inst.ship.dates$Date[j], 'Date'])
    days.since.shipped <- as.numeric(inst.first.run - inst.ship.dates$Date[j])
    
    temp <- data.frame(Instrument = serials[i], ShipDate = inst.ship.dates$Date[j], FirstRunDate = inst.first.run, DaysSinceShipment = days.since.shipped)
    inst.first.runs <- rbind(inst.first.runs, temp)
  }
  
  inst.first.runs.agg <- with(inst.first.runs[!(is.infinite(inst.first.runs$DaysSinceShipment)), ], aggregate(DaysSinceShipment~FirstRunDate, FUN=min))
  temp <- merge(inst.first.runs, inst.first.runs.agg, by=c('FirstRunDate','DaysSinceShipment'))[,c('Instrument','ShipDate','FirstRunDate', 'DaysSinceShipment')]
  
  # now there is a data frame called temp that contains the shipment dates and run dates for each instrument... but I need to determine the days since arrival for
  # each run
  for(j in 1:length(temp$ShipDate)) {
    
    inst.runs.keep <- inst.run.dates[inst.run.dates$Date > temp$ShipDate[j], ]
    if(nrow(inst.runs.keep)==0) { break() }
    
    if(length(temp$ShipDate)==1) {
      
      inst.runs.keep$ArrivalDate <- temp$FirstRunDate[j]
    } else {
      
      
    }
  }
  
  inst.arrival.dates <- rbind(inst.arrival.dates, temp)
}

inst.arrival.dates <- inst.arrival.dates[with(inst.arrival.dates, order(Instrument, ShipDate)), ]
# hist(inst.arrival.dates$DaysSinceShipment)
# there are several instruments where the days since it shipped to when the first run occurred are > 30, which is unexpected...
# it looks like this could be due to the instrument shipping and running for some time at the institution prior to Trend exports
# to figure out what the "correct" number of days should be to consider that this is happening, try to see the distribution of QC runs
# of omega at the institutions
positives.per.run <- with(data.frame(bugs.df, Positives = 1), aggregate(Positives~RunDataId, FUN=sum))
runs.df <- merge(runs.df, positives.per.run, by='RunDataId', all.x=TRUE)
runs.df[is.na(runs.df$Positives),'Positives'] <- 0
inst.arrival.dates.trim <- inst.arrival.dates[inst.arrival.dates$DaysSinceShipment < 30, ]
runs.arrivals.marked <- merge(runs.df, inst.arrival.dates.trim, by.x=c('Instrument','Date'), by.y=c('Instrument','FirstRunDate'), all.x=TRUE)
serials.trim <- as.character(unique(runs.arrivals.marked$Instrument))

