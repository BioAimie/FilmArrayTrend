# set the path and load libraries and data
workDir <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161107_LifeHistoryOfInstrument'
sql_path <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/DataSources/SQL/20161107_LifeHistoryOfInstrument'

setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(dplyr)
library(grid)
library(reshape2)

# Enter the Instrument Serial Number that has >= 3 RMA's for same customer  ----------------------------------------------
# SOUBEN: #2:  FA2813
# WINHOS: #5: FA2390
# MEDUSC: #7: FA3511
# SUNYHOS: #9: FA2641
# ALBMED: #10: FA1322
# NATCHI: #13: FA2985, 2FA00411
# CHIMER: #26: FA2354
# NORLAB: #36: 2FA00279, 2FA00286, 2FA00300, 2FA00458, 2FA00482
# NORLAB: #39: FA00313
# INTHEA: #41: FA2125, FA2127, FA2148, FA2581, FA2582
serialNumber <- "FA2127" # "FA2127" 

# Enter parameters for output plot to be saved as jpg
plot_printout_length <- 48; plot_printout_height <- 4; plot_resolution <- 300 # inches, inches, dpi

# GET DATA ---------------------------------------------------------------------------------------------------------------

# read in data from PMS PROD server (first to get date of manufacture)
PMScxn <- odbcConnect('PMS_PROD')

queryVector <- scan(paste(sql_path,'/query_RMATracker-2.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", "%", serialNumber,  "%", "\'", sep= ""), query)
rma.tracker.df <- sqlQuery(PMScxn,query)

queryVector <- scan(paste(sql_path,'/query_DateofMfg.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", serialNumber, "\'", sep= ""), query)
date.of.mfg.df <- sqlQuery(PMScxn,query)

queryVector <- scan(paste(sql_path,'/query_inst_trans.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", serialNumber, "\'", sep= ""), query)
inst.trans.df <- sqlQuery(PMScxn,query)

odbcClose(PMScxn)

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')

queryVector <- scan(paste(sql_path,'/query_freq_runsperday-updated.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", serialNumber, "\'", sep= ""), query)
query <- gsub("(\'[0-9]{4}-[0-9]{2}-[0-9]{2}\')", paste("\'", date.of.mfg.df$ManufactureDate, "\'", sep= ""), query)
freq.runs.per.day.df <- sqlQuery(FADWcxn,query)

queryVector <- scan(paste(sql_path,'/query_EpiWeeksCalendar.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
calendar <- sqlQuery(FADWcxn,query)

odbcClose(FADWcxn)

# ------------------------------------------------------------------------------------------------------------------------

# convert to date
rma.tracker.df$CreatedDate <- as.Date(rma.tracker.df$CreatedDate)
rma.tracker.df$ReceiveDate <- as.Date(rma.tracker.df$ReceiveDate)
rma.tracker.df$ServiceDate <- as.Date(rma.tracker.df$ServiceDate)
rma.tracker.df$QcDate <- as.Date(rma.tracker.df$QcDate)
rma.tracker.df$ShipDate <- as.Date(rma.tracker.df$ShipDate)
date.of.mfg.df$ManufactureDate <- as.Date(date.of.mfg.df$ManufactureDate)
inst.trans.df$TranDate <- as.Date(inst.trans.df$TranDate)
freq.runs.per.day.df$Date <- as.Date(freq.runs.per.day.df$Date)

# round decimals for 'HoursRun'
rma.tracker.df$HoursRun <- round(rma.tracker.df$HoursRun)
# convert to character
inst.trans.df$TranTypeDesc <- as.character(inst.trans.df$TranTypeDesc)

# filter flagged RMAs
#rma.tracker.df <- rma.tracker.df[rma.tracker.df$Failure == 1,] #filter out non-failure RMA
rma.tracker.df <- unique(rma.tracker.df)

# extract patient runs and validation runs
patient.runs.df <- data.frame(Type = 'PatientRuns', PatientSampleRunDate = freq.runs.per.day.df[grepl('PatientRun', freq.runs.per.day.df$PatientRuns) ,'Date'])
validation.runs.df <- data.frame(Type = 'validate', ValidationDate = freq.runs.per.day.df[grepl('Validation', freq.runs.per.day.df$Status), 'Date'])

# plot time line -----------------------------------------------------------------------------------------------------------
# # 1st ship date
first_shipDate <- min(inst.trans.df[grepl('Shipment', inst.trans.df$TranTypeDesc) ,'TranDate']) # using 'min' because FA1322 has two ship dates
first_validation_date <- min(validation.runs.df[validation.runs.df$ValidationDate >= date.of.mfg.df$ManufactureDate, 'ValidationDate'])
first_run_date <- min(patient.runs.df$PatientSampleRunDate)
first_RMA_date <- min(rma.tracker.df[rma.tracker.df$CreatedDate >= first_validation_date, 'CreatedDate'])
first_validation_date <- min(validation.runs.df[validation.runs.df$ValidationDate >= date.of.mfg.df$ManufactureDate ,'ValidationDate'])
last_run_date <- max(freq.runs.per.day.df$Date)

rma.tracker.df <- rma.tracker.df[rma.tracker.df$CreatedDate >= first_validation_date, ]

# using do.call to create a meta table ..............
ship.val.run.create.link <- 
  do.call(rbind, lapply(1:length(rma.tracker.df$TicketString), function(x) data.frame(
    RMA = rma.tracker.df$TicketString[x], 
    ship_date = rma.tracker.df[rma.tracker.df$TicketString == rma.tracker.df$TicketString[x], 'ShipDate'],
    rma_created_date = rma.tracker.df[rma.tracker.df$TicketString == rma.tracker.df$TicketString[x], 'CreatedDate'], 
    run_start_date = min(freq.runs.per.day.df[freq.runs.per.day.df$Date >= rma.tracker.df[rma.tracker.df$TicketString == rma.tracker.df$TicketString[x], 'ShipDate'], 'Date']), 
    run_end_date = max(freq.runs.per.day.df[freq.runs.per.day.df$Date <= rma.tracker.df[rma.tracker.df$TicketString == rma.tracker.df$TicketString[x], 'CreatedDate'], 'Date']), 
    validation_date = min(validation.runs.df[validation.runs.df$ValidationDate >= rma.tracker.df[rma.tracker.df$TicketString == rma.tracker.df$TicketString[x], 'ShipDate'], 'ValidationDate']),
    patient_run_date = min(patient.runs.df[patient.runs.df$PatientSampleRunDate >= min(validation.runs.df[validation.runs.df$ValidationDate >= rma.tracker.df[rma.tracker.df$TicketString==rma.tracker.df$TicketString[x], 'ShipDate'], 'ValidationDate']),'PatientSampleRunDate'])
  )))

ship.val.run.create.link$validation_date <- as.Date(as.character(ship.val.run.create.link$validation_date), origin = 1970-01-01)
ship.val.run.create.link <- rbind(NA, ship.val.run.create.link)

# remove duplicate run_end_date
length_link <- nrow(ship.val.run.create.link)

ship.val.run.create.link <- do.call(rbind, lapply((length_link-1):1, function(x) data.frame(
  RMA = ship.val.run.create.link$RMA[length_link+1-x],
  ship_date = ship.val.run.create.link$ship_date[length_link+1-x],
  rma_created_date = ship.val.run.create.link$rma_created_date[length_link+1-x],
  run_start_date = ship.val.run.create.link$run_start_date[length_link+1-x],
  run_end_date = ifelse(!is.na(ship.val.run.create.link$run_end_date[length_link+1-x]) & !is.na(ship.val.run.create.link$run_end_date[length_link+1-x-1]) & ((length_link+1-x-1) >= 1),
                        ifelse(ship.val.run.create.link$run_end_date[length_link+1-x] == ship.val.run.create.link$run_end_date[length_link+1-x-1], NA, format(ship.val.run.create.link$run_end_date[length_link+1-x], "%Y-%m-%d")), format(ship.val.run.create.link$run_end_date[length_link+1-x], "%Y-%m-%d")),
  validation_date = ship.val.run.create.link$validation_date[length_link+1-x],
  patient_run_date = ship.val.run.create.link$patient_run_date[length_link+1-x]
)))
ship.val.run.create.link$run_end_date <- as.Date(as.character(ship.val.run.create.link$run_end_date), origin = "1970-01-01")

# # add 1st ship, run date  and validation dates to above table
if(first_shipDate < min(ship.val.run.create.link[!is.na(as.character(ship.val.run.create.link$ship_date)), 'ship_date'])){
  ship.val.run.create.link <- rbind(NA, ship.val.run.create.link)
  ship.val.run.create.link$ship_date[1] <- first_shipDate
}
ship.val.run.create.link$run_start_date[1] <- min(freq.runs.per.day.df$Date)
if(first_validation_date < min(ship.val.run.create.link[!is.na(as.character(ship.val.run.create.link$validation_date)), 'ship_date'])){
  ship.val.run.create.link$validation_date[1] <- first_validation_date
}

# check for runs before validation date and move validation date to that date. 
# Also move patient run dates according to the moved validation date
ship.val.run.create.link <- do.call(rbind, lapply(1:length(ship.val.run.create.link$validation_date), function(x) data.frame(
  RMA = ship.val.run.create.link$RMA[x],
  run_end_date = ship.val.run.create.link$run_end_date[x],
  rma_created_date = ship.val.run.create.link$rma_created_date[x],  
  ship_date = ship.val.run.create.link$ship_date[x],
  run_start_date = ship.val.run.create.link$run_start_date[x],
  validation_date = if_else(!is.na(ship.val.run.create.link$validation_date[x]) & (ship.val.run.create.link$validation_date[x] <= ship.val.run.create.link$run_start_date[x]), ship.val.run.create.link$validation_date[x], ship.val.run.create.link$run_start_date[x]),
  patient_run_date = if_else(!is.na(ship.val.run.create.link$validation_date[x]) & (ship.val.run.create.link$patient_run_date[x] <= ship.val.run.create.link$run_start_date[x]), ship.val.run.create.link$patient_run_date[x], ship.val.run.create.link$run_start_date[x])
)))

# find last validation date
last_validation_date <- max(as.Date(ship.val.run.create.link[!is.na(ship.val.run.create.link$validation_date), 'validation_date'], origin = '1970-01-01'))

# check for '>0' runs between two RMAs
ship.val.run.create.link  <- do.call(rbind, lapply(1:nrow(ship.val.run.create.link), function(x) data.frame(
  RMA = ship.val.run.create.link$RMA[x],
  run_end_date = ifelse(!is.na(ship.val.run.create.link$run_end_date[x]), (ifelse(ship.val.run.create.link$run_end_date[x] < ship.val.run.create.link$ship_date[x-1], NA, format(ship.val.run.create.link$run_end_date[x], '%Y-%m-%d'))), NA),
  rma_created_date = ship.val.run.create.link$rma_created_date[x],  
  ship_date = ship.val.run.create.link$ship_date[x],
  run_start_date = ifelse(x == 1, format(ship.val.run.create.link$run_start_date[x], '%Y-%m-%d'), ifelse(x < nrow(ship.val.run.create.link) & (ship.val.run.create.link$rma_created_date[x+1] > ship.val.run.create.link$run_start_date[x]), format(ship.val.run.create.link$run_start_date[x], '%Y-%m-%d'), ifelse(x == nrow(ship.val.run.create.link), format(ship.val.run.create.link$run_start_date[x], '%Y-%m-%d'),NA))),
  validation_date = ifelse(x < nrow(ship.val.run.create.link) & ship.val.run.create.link$rma_created_date[x+1] > ship.val.run.create.link$validation_date[x], format(ship.val.run.create.link$validation_date[x], '%Y-%m-%d'), ifelse(x == nrow(ship.val.run.create.link), format(ship.val.run.create.link$validation_date[x], '%Y-%m-%d'),NA)),
  patient_run_date = ifelse(x < nrow(ship.val.run.create.link) & ship.val.run.create.link$rma_created_date[x+1] > ship.val.run.create.link$patient_run_date[x], format(ship.val.run.create.link$patient_run_date[x], '%Y-%m-%d'), ifelse(x == nrow(ship.val.run.create.link), format(ship.val.run.create.link$patient_run_date[x], '%Y-%m-%d'),NA))
)))
ship.val.run.create.link$run_end_date <- as.Date(as.character(ship.val.run.create.link$run_end_date), origin = 1970-01-01)
ship.val.run.create.link$run_start_date <- as.Date(as.character(ship.val.run.create.link$run_start_date), origin = 1970-01-01)
ship.val.run.create.link$validation_date <- as.Date(as.character(ship.val.run.create.link$validation_date), origin = 1970-01-01)
ship.val.run.create.link$patient_run_date <- as.Date(as.character(ship.val.run.create.link$patient_run_date), origin = 1970-01-01)




# remove runs between in RMA Created Date and Ship Date'
calendar.df <- data.frame(Date = calendar[as.Date(calendar$Date) >= min(as.Date(freq.runs.per.day.df$Date)) & as.Date(calendar$Date) <= max(as.Date(freq.runs.per.day.df$Date)), 'Date'])

calendar.df$Date <- as.Date(calendar.df$Date)
temp_rma <- na.omit(rma.tracker.df[,c('CreatedDate', 'ShipDate')])
temp_rma$date_diff <- abs(as.numeric(difftime(temp_rma$CreatedDate, temp_rma$ShipDate)))

calendar.rma.merge.df <- merge(x = calendar.df, y = temp_rma, by.x = 'Date', by.y = 'CreatedDate', all.x = TRUE)
calendar.rma.merge.df$type <- as.character(NA)

for(i in 1:nrow(calendar.rma.merge.df)){
  if(!is.na(calendar.rma.merge.df$date_diff[i])){
    count <- calendar.rma.merge.df$date_diff[i]
    for(j in 0:(count)){
      calendar.rma.merge.df$type[i+j] <- 'blackoutdays'
    }
  }
  i <- i+ count
}

runs <- freq.runs.per.day.df
runs$Date <- as.Date(runs$Date)

calendar.rma.runs.merge.df <- merge(x = calendar.rma.merge.df, y = freq.runs.per.day.df, by.x = 'Date', by.y ='Date', all.x = TRUE)
calendar.rma.runs.merge.df$Date <- as.Date(calendar.rma.runs.merge.df$Date)
calendar.rma.runs.merge.df <- calendar.rma.runs.merge.df[ , c('Date', 'SerialNo', 'Status', 'PatientRuns', 'Runs', 'type')]

for(i in 1:nrow(calendar.rma.runs.merge.df)){
  if(!is.na(calendar.rma.runs.merge.df$type[i])){
     calendar.rma.runs.merge.df$Runs[i] <- 0
  }
}
runs <- calendar.rma.runs.merge.df


# create plot
windows() # create new window for each plot ....................................................................
g2 <- ggplot() + ylim(0, 70) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
g2 <- g2 + ggtitle(paste("Life History of Instrument:", serialNumber, sep = " "))

# plot 'blue' and 'red' boxes 
g2 <- g2 + geom_rect(aes(xmin = date.of.mfg.df$ManufactureDate, xmax = first_shipDate, ymin = 0, ymax = 50), fill = I("lightblue"), alpha = 0.2, show.legend=FALSE)
g2 <- g2 + geom_rect(aes(xmin = rma.tracker.df$CreatedDate, xmax = rma.tracker.df$ShipDate,   ymin = 0, ymax = 50),   fill = "red", alpha = 0.2, show.legend=FALSE) 

# plot 'green' boxes ..........................................................................................
shift_val_run <- do.call(rbind, lapply(1:length(ship.val.run.create.link$validation_date)-1, function(x) data.frame(
  shift_val = as.Date(ifelse(!is.na(ship.val.run.create.link$validation_date[x]), as.Date(ship.val.run.create.link$validation_date[x]), NA), origin = '1970-01-01'),
  shift_run = as.Date(ifelse(!is.na(ship.val.run.create.link$validation_date[x]), as.Date(ship.val.run.create.link$run_end_date[x+1]), NA), origin = '1970-01-01')
)))

g2 <- g2 + geom_rect(aes(xmin = shift_val_run$shift_val, xmax = shift_val_run$shift_run, ymin = 0, ymax = 50), fill = "lightgreen", alpha = 0.2, show.legend = FALSE)

# last green box for current runs
if(last_run_date > max(ship.val.run.create.link$ship_date)) {
  g2 <- g2 + geom_rect(aes(xmin = last_validation_date, xmax= last_run_date, ymin = 0, ymax = 50), fill = "lightgreen", alpha = 0.2, show.legend=FALSE)
}

# # plot golden boxes before RMA ....................................................................................
# edit---  !is.na(as.character(run_end_date)
g2 <- g2 + geom_rect(data = subset(ship.val.run.create.link, !is.na(as.character(run_end_date))), aes(xmin = run_end_date, xmax = rma_created_date, ymin = 0, ymax = 50), fill = "goldenrod2", alpha = 0.5, show.legend = FALSE)

# plot golden boxes after RMA ....................................................................................
if(first_shipDate %m+% months(6) >= min(freq.runs.per.day.df$Date)){
  g2 <- g2 + geom_rect(data = subset(ship.val.run.create.link, !is.na(as.character(validation_date))), aes(xmin = ship_date, xmax = validation_date, ymin = 0, ymax = 50), fill = "goldenrod2", alpha = 0.5, show.legend = FALSE)
} else {
  g2 <- g2 + geom_rect(data = subset(ship.val.run.create.link[2:nrow(ship.val.run.create.link),], !is.na(as.character(validation_date))), aes(xmin = ship_date, xmax = validation_date, ymin = 0, ymax = 50), fill = "goldenrod2", alpha = 0.5, show.legend = FALSE)
}

# plot all runs and check if the instrument has an instrument error in runs for 'fill='
#runs <- freq.runs.per.day.df
runs$Status <- gsub('Validation', 'Completed', runs$Status)
g2 <- g2 + geom_bar(data = runs, aes(x = Date, y = Runs, yend = 0, fill = Status),  size = 1, stat = "identity") + scale_fill_manual(values= c("darkgreen", "red"), name = "", labels=c("Completed Runs","Instrument Error")) + theme(legend.position="bottom")

# plot 'geom_segment' labels
g2 <- g2 + geom_segment(data = date.of.mfg.df, aes(x = ManufactureDate,y = 70,     xend = ManufactureDate, yend = 0), color = 'darkgreen') + geom_text(data=date.of.mfg.df, aes(x = ManufactureDate,y = 70, xend = ManufactureDate, yend = 0, label="Birth"))
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate,y = 50, xend = CreatedDate, yend = 0), color = 'red') + geom_text(data=rma.tracker.df, aes(x = CreatedDate,y = 50,xend = CreatedDate, yend = 0,  label="Failure RMA"))
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = ReceiveDate,y = 45, xend = ReceiveDate, yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = ReceiveDate,y = 45,xend = ReceiveDate, yend = 0,  label="Received"))
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = ServiceDate,y = 40, xend = ServiceDate, yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = ServiceDate,y = 40,xend = ServiceDate, yend = 0,  label="Serviced"))
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = QcDate,y = 35, xend = QcDate, yend = 0), color = 'black')+ geom_text(data=rma.tracker.df, aes(x = QcDate,y = 35,xend = QcDate, yend = 0,  label="QC"))
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = ShipDate,y = 30, xend = ShipDate, yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = ShipDate,y = 30,xend = ShipDate, yend = 0,  label="Shipped"))
g2 <- g2 + geom_segment(data = inst.trans.df, aes(x = first_shipDate,y = 65, xend = first_shipDate, yend = 0), color = 'black') + geom_text(data=inst.trans.df, aes(x = first_shipDate,y = 65,xend = first_shipDate, yend = 0,  label="1st Shipped"))


# Early failure type: DOA, ELF, SDOA or SELF
for(x in 1:nrow(rma.tracker.df)){
if((!is.na(rma.tracker.df$EarlyFailure[x]) | !is.null(rma.tracker.df$EarlyFailure[x])) & rma.tracker.df$EarlyFailure[x] != "N/A"){
    if(rma.tracker.df$EarlyFailure[x] == 'DOA'){
      i <- x
      g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0,  label="DOA"))
    } else if(rma.tracker.df$EarlyFailure[x] == 'ELF'){
      i <- x
      g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0,  label="ELF"), size=10)
    } else if(rma.tracker.df$EarlyFailure[x] == 'SDOA'){
      i <- x
      g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0,  label="SDOA"))
    } else if(rma.tracker.df$EarlyFailure[x] == 'SELF'){
      i <- x
      g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0), color = 'black') + geom_text(data=rma.tracker.df, aes(x = CreatedDate[i], y = 15, xend = CreatedDate[i], yend = 0,  label="SELF"))
    }
   }
}

# hours run
g2 <- g2 + geom_segment(data = rma.tracker.df, aes(x = CreatedDate,y = 60, xend = CreatedDate, yend = 0), color = 'red') + geom_text(data=rma.tracker.df, aes(x = CreatedDate,y = 60,xend = CreatedDate, yend = 0,  label=paste(HoursRun, 'hours', sept = " ")))

g2 <- g2 + geom_segment(data = subset(ship.val.run.create.link, !is.na(as.character(validation_date))), aes(x = validation_date, y = 25, xend = validation_date, yend = 0), color = 'black') + geom_text(data=subset(ship.val.run.create.link, !is.na(as.character(validation_date))), aes(x = validation_date, y = 25, xend = validation_date, yend = 0,  label="Validation"))

# 1st patient run as 1st completed run after validation date

g2 <- g2 + geom_segment(data = data.frame(patient_run_date = ship.val.run.create.link[!is.na(as.character(ship.val.run.create.link$patient_run_date)), 'patient_run_date']), aes(x = patient_run_date,y = 20, xend = patient_run_date, yend = 0), color = 'black') + geom_text(data = data.frame(patient_run_date = ship.val.run.create.link[!is.na(as.character(ship.val.run.create.link$patient_run_date)), 'patient_run_date']), aes(x = patient_run_date,y = 20,xend = patient_run_date, yend = 0,  label = "Patient Run"))

# calculate days ----------------------------------------------------------------------------------------------------------------------------
# function to plot days
function_plot_days <- function(date1, date2, y_value){
  interval_RMA_days <- abs(as.numeric(difftime(date1, date2, units = 'days')))
  interval_RMA_mid <- date2 + floor((date1 - date2)/2)
  interval_RMA_df <- na.omit(as.data.frame(cbind(days = interval_RMA_days, mid = format(interval_RMA_mid, '%Y-%m-%d'))))
  interval_RMA_df$mid <- as.Date(as.character(interval_RMA_df$mid), origin = '1970-01-01')
  g2 <- g2 + geom_text(data=interval_RMA_df, aes(x = mid, y = y_value,xend = mid, yend = 0,  label=paste(days, 'days', sept = " ")))
  return(g2)
}

y_value <- 55 #height at which the text is displayed
# Days: 'Birth' to '1st Shipped'
g2 <- function_plot_days(first_shipDate, min(inst.trans.df$TranDate), y_value)

# Days: 'Failure RMA' to 'Shipped'
g2 <- function_plot_days(ship.val.run.create.link$ship_date, ship.val.run.create.link$rma_created_date, y_value)

# Days: validation to end run
g2 <- function_plot_days(shift_val_run$shift_val, shift_val_run$shift_run, y_value)

# Days: current run 
if(last_run_date > max(ship.val.run.create.link$ship_date)){
  interval_current_run_days <- abs(as.numeric(difftime(last_run_date, last_validation_date, units = "days")))
  interval_current_run_mid <- na.omit(last_validation_date + floor((last_run_date - last_validation_date)/2))
  g2 <- g2 + geom_text(aes(x = interval_current_run_mid , y = y_value,xend = interval_current_run_mid , yend = 0,  label=paste(interval_current_run_days, 'days', sept = " ")))
}

# Days: gap before RMA Created
g2 <- function_plot_days(ship.val.run.create.link$rma_created_date,ship.val.run.create.link$run_end_date, y_value)

# Days: gap after Ship date to validation date 
if(first_shipDate %m+% months(6) >= min(freq.runs.per.day.df$Date)){
  g2 <- function_plot_days(ship.val.run.create.link$ship_date,ship.val.run.create.link$validation_date, y_value)
}

# display plot on screen
g2

# print plot
jpeg(paste("Life_History_Plot-", serialNumber, ".jpg", sep=""), width = plot_printout_length, height = plot_printout_height, units = 'in', res = plot_resolution)
g2 # Make plot
dev.off()





