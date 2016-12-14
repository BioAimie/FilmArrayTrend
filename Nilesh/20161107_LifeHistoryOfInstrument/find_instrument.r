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

# serialNumber <- "FA2127" # "FA2127" 
# # Enter parameters for output plot to be saved as jpg
# plot_printout_length <- 48; plot_printout_height <- 4; plot_resolution <- 300 # inches, inches, dpi
CustomerSiteId = 13
# GET DATA ---------------------------------------------------------------------------------------------------------------
# read in data from PMS PROD server (first to get date of manufacture)
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- scan(paste(sql_path,'/query_DateofMfg.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
#query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", serialNumber, "\'", sep= ""), query)
date.of.mfg.df <- sqlQuery(PMScxn,query)
odbcClose(PMScxn)


# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/query_InstrumentSerialNo_for_Cust.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
query <- gsub("\\[CustomerSiteId\\] = [0-9]+", paste("[CustomerSiteId] = ", CustomerSiteId, " ", sep= ""), query)
cust.inst.ser.no.df <- sqlQuery(FADWcxn,query)

queryVector <- scan(paste(sql_path,'/query_EpiWeeksCalendar.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
calendar <- sqlQuery(FADWcxn,query)

odbcClose(FADWcxn)


# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/query_AllRuns.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
#query <- gsub("(\'FA+[0-9]{4}\')", paste("\'", serialNumber, "\'", sep= ""), query)
query <- gsub("(\'[0-9]{4}-[0-9]{2}-[0-9]{2}\')", paste("\'", date.of.mfg.df$ManufactureDate, "\'", sep= ""), query)
freq.runs.per.day.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)



cust.runs.df <- freq.runs.per.day.df
temp <- subset(cust.runs.df, SerialNo == as.character(cust.inst.ser.no.df$InstrumentSerialNumber[1]))

 for (i in 1:nrow(cust.inst.ser.no.df)){
   cust.runs.df <- freq.runs.per.day.df
   temp_subset <- subset(cust.runs.df, SerialNo == as.character(cust.inst.ser.no.df$InstrumentSerialNumber[i]))
   temp <- rbind(temp, temp_subset)
 }
cust.runs.df <- temp

dateBreaks <- unique(as.character(cust.runs.df$Date))[order(unique(as.character(cust.runs.df$Date)))][seq(1, length(unique(as.character(cust.runs.df$Date))), 300)]
g1 <- ggplot() +  xlab("Year-Week") +  ylab("Runs") + ggtitle(paste("Runs for [CustomerSiteId] #", CustomerSiteId, ".", sep = ""))
g1 <- g1 + geom_bar(data = cust.runs.df, aes(x = Date, y = Runs, fill = SerialNo), stat = "identity") + scale_x_discrete(breaks=dateBreaks)
g1 <- g1 + facet_wrap(~ SerialNo)
g1 <- g1 + theme(text = element_text(size=20), axis.text.x = element_text(angle=90, hjust=1)) 
g1


# print plot
jpeg(paste("CustomerSiteId -", CustomerSiteId, ".jpg", sep=""), width = 10, height = 5, units = 'in', res = 300)
print(g1) # Make plot
dev.off()

