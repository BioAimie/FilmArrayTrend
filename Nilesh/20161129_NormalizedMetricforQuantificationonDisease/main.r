
# set the path and load libraries and data
workDir <-'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161129_NormalizedMetricforQuantificationofDisease'
sql_path <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/DataSources/SQL/20161129_NormalizedMetricforQuantificationonDisease'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Rmisc)


# ===============================================================================================================
# Code: Normalizing Cp
# ===============================================================================================================
# GET DATA - 1: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/FADataWarehouse_query.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
pouch.lot.number.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)
# ------------------------------------------------------------------------------------------------------------------------

# convert_to_factor <- function(df){
#   df$PouchSerialNumber <- as.factor(df$PouchSerialNumber)
#   df$PouchLotNumber <- as.factor(df$PouchLotNumber)
#   return(df)
# }


for(i in 1:nrow(pouch.lot.number.df)){
  print(i)
  # read in data from PMS PROD server 
  PMScxn <- odbcConnect('PMS_PROD')
  queryVector <- scan(paste(sql_path,'/FilmArray2_query-4.txt', sep=""),what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  query <- gsub("([PouchLotNumber] LIKE \'%[0-9].*%\')", paste("[PouchLotNumber] LIKE ", "\'", "%", pouch.lot.number.df$PouchLotNumber[i],  "%", "\'", sep= ""), query)
  cp.df.name <- paste("cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep="")
  assign(cp.df.name,sqlQuery(PMScxn,query))
  odbcClose(PMScxn)
  #if(i%%3 == 0){Sys.sleep(3)} # momentarily stop the loop for '3 seconds' after '3' pouch lot numbers are scanned
  
 # assign(cat(cp.df.name, "$PouchSerialNumber", sep=""), convert_to_factor(cat(cp.df.name, "$PouchSerialNumber", sep = "")))
 # assign(cat("cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], '$PouchLotNumber', sep = ""), convert_to_factor(cat("cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], '$PouchLotNumber', sep = "")))
}

# ===============================================================================================================
# Code: Plotting Cp
# ===============================================================================================================

# # Enter parameters for output plot to be saved as jpg
# width <- 12; height <- 15; plot_resolution <- 300 # inches, inches, dpi
#
#
# # GET DATA - 2: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/query_RP_assayList.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
assay.list.df <- sqlQuery(FADWcxn,query)

queryVector <- scan(paste(sql_path,'/query_CpRuns-WellData.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
#query <- gsub("\\[(TargetName)\\] LIKE \'[a-zA-Z]+\'", paste("\\[TargetName\\] LIKE ", TargetName, " ", sep= ""), query)
cust.cp.runs.df <- sqlQuery(FADWcxn,query)

odbcClose(FADWcxn)
# # ------------------------------------------------------------------------------------------------------------------------




# INTERNAL Cp data ---------------------------------------------------------------------------------------------------------------
# convert pouch lot and serial numbers to factors for plotting
cp.pouch.lot.number.df.332916$PouchSerialNumber <- as.factor(cp.pouch.lot.number.df.332916$PouchSerialNumber)
cp.pouch.lot.number.df.332916$PouchLotNumber <- as.factor(cp.pouch.lot.number.df.332916$PouchLotNumber)

# calculate mean and standard deviation 
cp.pouch.lot.number.df.332916.avg <- aggregate(Cp ~ Name, cp.pouch.lot.number.df.332916, mean)
summary.cp.pouch.lot.number.df.332916 <- summarySE(cp.pouch.lot.number.df.332916, measurevar="Cp", groupvars=c("Name"), conf.interval = 0.95, na.rm=TRUE)

# plot the Cp values for internal controls
dev.new()
g1 <- ggplot() + ylab(cat("Cp", "\u00B1", "std.dev.")) + ggtitle("Internal Control Cp data for [PouchLotNumber] 332916")
g1 <- g1 + geom_point(data = cp.pouch.lot.number.df.332916, aes(x = Name, y = Cp, fill = PouchSerialNumber), size = 1, alpha = 0.25)
g1 <- g1 + geom_point(data = cp.pouch.lot.number.df.332916.avg, aes(x = Name, y = Cp), color = 'red', size = 2)
g1 <- g1 + geom_errorbar(data = summary.cp.pouch.lot.number.df.332916, aes(x = Name, ymin = Cp - sd, ymax = Cp + sd))
g1

# plot Cp histogram
dev.new()
gh1 <- ggplot() 
gh1 <- gh1 + geom_histogram(data = cp.pouch.lot.number.df.332916, aes(Cp)) + facet_wrap(~Name)
gh1

# CUSTOMER Cp data ---------------------------------------------------------------------------------------------------------------
# convert pouch lot and serial numbers to factors for plotting
cust.cp.runs.df$PouchLotNumber <- as.factor(cp.runs.df$PouchLotNumber)
cust.cp.runs.df$PouchSerialNumber <- as.factor(cp.runs.df$PouchSerialNumber)

# calculate mean and standard deviation 
cp.runs.control.avg <- aggregate(Cp ~ AssayName, cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], mean)
summary.cp.runs.control <- summarySE(cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], measurevar="Cp", groupvars=c("AssayName"), conf.interval = 0.95, na.rm=TRUE)

# plot using standard deviation error bar
dev.new()
g3 <- ggplot() + ylab(cat("Cp", "\u00B1", "std.dev.")) + ggtitle("Customer control Cp data for [PouchLotNumber] 332916")
g3 <- g3 + geom_point(data = cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], aes(x = AssayName, y = Cp), color = 'blue', size = 1, alpha = 0.25)
g3 <- g3 + geom_point(data = cp.runs.control.avg, aes(x = AssayName, y = Cp), color = 'red', size = 2)
g3 <- g3 + geom_errorbar(data = summary.cp.runs.control, aes(x = AssayName, ymin = Cp - sd, ymax = Cp + sd))
g3

# plot the Cp values for customer controls
dev.new()
gh3 <- ggplot() 
gh3 <- gh3 + geom_histogram(data = cust.cp.runs.df, aes(Cp)) + facet_wrap(~TargetName)
gh3


# plot internal vs. customer controls PCR1, PCR2, RNA
internal_cp <- cp.pouch.lot.number.df.332916$Cp[cp.pouch.lot.number.df.332916$Name == "PCR2"]
customer_cp <- cust.cp.runs.df$Cp[cust.cp.runs.df$TargetName == 'PCR2 Control']
customer_cp <- customer_cp[100:120]
qplot(internal_cp, customer_cp)

















#
#
# # function to plot Cp
plot_cp <- function(TargetName, CustomerSiteId, cp_median_df, cp_avg_min_df){
  dateBreaks <- unique(as.character(cp_median_df$ConcatDate))[order(unique(as.character(cp_median_df$ConcatDate)))][seq(1, length(unique(as.character(cp_median_df$ConcatDate))), 6)]
  g1 <- ggplot() +  xlab("Year-Week") +  ylab("Median Cp") + ggtitle(paste("Cp values for ", TargetName, ".", sep = ""))
  #  g1 <- g1 + geom_errorbar(data = summary_df, aes(x =  as.factor(ConcatDate), ymin = Cp - ci, ymax = Cp + ci)) + scale_x_discrete(breaks=dateBreaks)
  g1 <- g1 + geom_point(data = cp_median_df, aes(x = as.factor(ConcatDate), y = Cp), color = 'blue', size = 1, alpha = 0.25)
  g1 <- g1 + geom_point(data = cp_avg_min_df, aes(x = as.factor(ConcatDate), y = Cp), color = 'red', size = 2, alpha = 0.5)
  g1 <- g1 + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  g1 <- g1 + facet_wrap(~CustomerSiteId)
  return(g1)
}
#
# for (i in 1:nrow(assay.list.df)){
#   TargetName <- paste("\'",as.character(assay.list.df$Interpretation[i]),"\'", sep="")
#   # inside loop GET DATA - 2: Cp values for each assay name --------------------------------------------------------------------------
#   # read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
#   FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
#   queryVector <- scan(paste(sql_path,'/query_CpRuns-3-RP_allCust.txt', sep=""),what=character(),quote="")
#   query <- paste(queryVector,collapse=" ")
#   query <- gsub("\\[(TargetName)\\] LIKE \'[a-zA-Z]+\'", paste("\\[TargetName\\] LIKE ", TargetName, " ", sep= ""), query)
#   cp.runs.df <- sqlQuery(FADWcxn,query)
#   odbcClose(FADWcxn)
#
#
#
#   # calculate values
  if(TargetName == "\'Human Rhinovirus/Enterovirus\'"){
    # calculate medianCp each AssayName of a TargetName over weeks
    cp.assay.name.median.df <- aggregate(Cp ~ CustomerSiteId + ConcatDate + RunDataId + AssayName, cp.runs.df, median)
    # find minimum of the median Cp
    cp.min.median.df <- aggregate(Cp ~ CustomerSiteId + ConcatDate, cp.assay.name.median.df, function(x) min(x))
    # calculate confidence interval
    #summary.cp.median.df <- summarySE(cp.assay.name.median.df, measurevar="Cp", groupvars=c("CustomerSiteId","ConcatDate"), conf.interval = 0.95, na.rm=TRUE)
    # plot
    g1 <- plot_cp(TargetName, CustomerSiteId, cp.assay.name.median.df, cp.min.median.df)
  } else {
    cp.median.df <- aggregate(Cp ~ CustomerSiteId + ConcatDate + RunDataId, cp.runs.df, median)
    cp.avg.median.df <- aggregate(Cp ~ CustomerSiteId + ConcatDate, cp.median.df, mean)
    # calculate confidence interval
    #summary.cp.median.df <- summarySE(cp.median.df, measurevar="Cp", groupvars=c("CustomerSiteId","ConcatDate"), conf.interval = 0.95, na.rm=TRUE)
    # plot
    g1 <- plot_cp(TargetName, CustomerSiteId, cp.median.df, cp.avg.median.df)
  }
  #g1

  # print plot
  file_name <- paste("Cp30_for_", as.character(assay.list.df$Interpretation[i]), ".jpg", sep = "")
  jpeg(paste(workDir, '/Figures/', file_name, sep= ""), width = width, height = height, units = 'in', res = plot_resolution)
  print(g1) # Make plot
  dev.off()
}
#
#
