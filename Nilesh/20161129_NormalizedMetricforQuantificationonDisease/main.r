
# set the path and load libraries and data
workDir <-'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161129_NormalizedMetricforQuantificationonDisease'
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
  query <- gsub("\\[PouchLotNumber\\] LIKE \'%[0-9].*%\'", paste("[PouchLotNumber] LIKE ", "\'", "%", pouch.lot.number.df$PouchLotNumber[i],  "%", "\'", sep= ""), query)
  internal.cp.df.name <- paste("internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep="")
  assign(internal.cp.df.name,sqlQuery(PMScxn,query))
  odbcClose(PMScxn)
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
# queryVector <- scan(paste(sql_path,'/query_RP_assayList.txt', sep=""),what=character(),quote="")
# query <- paste(queryVector,collapse=" ")
# assay.list.df <- sqlQuery(FADWcxn,query)

queryVector <- scan(paste(sql_path,'/query_CpRuns-WellData.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
#query <- gsub("\\[(TargetName)\\] LIKE \'[a-zA-Z]+\'", paste("\\[TargetName\\] LIKE ", TargetName, " ", sep= ""), query)
cust.cp.runs.df <- sqlQuery(FADWcxn,query)

odbcClose(FADWcxn)


for(i in 1:nrow(pouch.lot.number.df)){
  print(i)
  # read in data from PMS PROD server 
  FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
  queryVector <- scan(paste(sql_path,'/query_CpRuns-WellData.txt', sep=""),what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  query <- gsub("\\[PouchLotNumber\\] LIKE \'%[0-9].*%\'", paste("[PouchLotNumber] LIKE ", "\'", "%", pouch.lot.number.df$PouchLotNumber[i],  "%", "\'", sep= ""), query)
  customer.cp.df.name <- paste("customer.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep="")
  assign(customer.cp.df.name,sqlQuery(FADWcxn,query))
  odbcClose(FADWcxn)

  # 
  # FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
  # queryVector <- scan(paste(sql_path,'/query_CpRuns-WellData.txt', sep=""),what=character(),quote="")
  # query <- paste(queryVector,collapse=" ")
  # cust.cp.runs.df <- sqlQuery(FADWcxn,query)
  # odbcClose(FADWcxn)
}



# # ------------------------------------------------------------------------------------------------------------------------



# 
# # INTERNAL Cp data ---------------------------------------------------------------------------------------------------------------
# # convert pouch lot and serial numbers to factors for plotting
# internal.cp.pouch.lot.number.df.332916$PouchSerialNumber <- as.factor(internal.cp.pouch.lot.number.df.332916$PouchSerialNumber)
# internal.cp.pouch.lot.number.df.332916$PouchLotNumber <- as.factor(internal.cp.pouch.lot.number.df.332916$PouchLotNumber)
# 
# # calculate mean and standard deviation 
# internal.cp.pouch.lot.number.df.332916.avg <- aggregate(Cp ~ Name, internal.cp.pouch.lot.number.df.332916, mean)
# summary.internal.cp.pouch.lot.number.df.332916 <- summarySE(internal.cp.pouch.lot.number.df.332916, measurevar="Cp", groupvars=c("Name"), conf.interval = 0.95, na.rm=TRUE)
# 
# # plot the Cp values for internal controls
# dev.new()
# g1 <- ggplot() + ylab(cat("Cp", "\u00B1", "std.dev.")) + ggtitle("Internal Control Cp data for [PouchLotNumber] 332916")
# g1 <- g1 + geom_point(data = internal.cp.pouch.lot.number.df.332916, aes(x = Name, y = Cp, fill = PouchSerialNumber), size = 1, alpha = 0.25)
# g1 <- g1 + geom_point(data = internal.cp.pouch.lot.number.df.332916.avg, aes(x = Name, y = Cp), color = 'red', size = 2)
# g1 <- g1 + geom_errorbar(data = summary.internal.cp.pouch.lot.number.df.332916, aes(x = Name, ymin = Cp - sd, ymax = Cp + sd))
# g1
# 
# # plot Cp histogram
# dev.new()
# gh1 <- ggplot() 
# gh1 <- gh1 + geom_histogram(data = internal.cp.pouch.lot.number.df.332916, aes(Cp)) + facet_wrap(~Name)
# gh1
# 
# # CUSTOMER Cp data ---------------------------------------------------------------------------------------------------------------
# # convert pouch lot and serial numbers to factors for plotting
# cust.cp.runs.df$PouchLotNumber <- as.factor(cust.cp.runs.df$PouchLotNumber)
# cust.cp.runs.df$PouchSerialNumber <- as.factor(cust.cp.runs.df$PouchSerialNumber)
# 
# # calculate mean and standard deviation 
# cust.cp.runs.control.avg <- aggregate(Cp ~ AssayName, cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], mean)
# summary.cust.cp.runs.control <- summarySE(cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], measurevar="Cp", groupvars=c("AssayName"), conf.interval = 0.95, na.rm=TRUE)
# 
# # plot using standard deviation error bar
# dev.new()
# g3 <- ggplot() + ylab(cat("Cp", "\u00B1", "std.dev.")) + ggtitle("Customer control Cp data for [PouchLotNumber] 332916")
# g3 <- g3 + geom_point(data = cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], aes(x = AssayName, y = Cp), color = 'blue', size = 1, alpha = 0.25)
# g3 <- g3 + geom_point(data = cust.cp.runs.control.avg, aes(x = AssayName, y = Cp), color = 'red', size = 2)
# g3 <- g3 + geom_errorbar(data = summary.cust.cp.runs.control, aes(x = AssayName, ymin = Cp - sd, ymax = Cp + sd))
# g3
# 
# # plot the Cp values for customer controls
# dev.new()
# gh3 <- ggplot() 
# gh3 <- gh3 + geom_histogram(data = cust.cp.runs.df, aes(Cp)) + facet_wrap(~TargetName)
# gh3
# 
# 
# # plot internal vs. customer controls PCR1, PCR2, RNA
# internal_cp <- internal.cp.pouch.lot.number.df.332916$Cp[internal.cp.pouch.lot.number.df.332916$Name == "PCR2"]
# customer_cp <- cust.cp.runs.df$Cp[cust.cp.runs.df$TargetName == 'PCR2 Control']
# customer_cp <- customer_cp[100:120]
# qplot(internal_cp, customer_cp)
# 
# 
# 

# extract the internal Cp data for the lot numbers from customer Cp data -------------------------------------------------------------------------------------------
count <- 0
for (i in 1:nrow(pouch.lot.number.df)){
#for (i in 1:50){
  internal_cp_temp <- get(paste0("internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep = ""))

  pouch_lot_number <- pouch.lot.number.df$PouchLotNumber[i]
  
  
  if(nrow(internal_cp_temp) > 0){
    #count <- count + 1
    print(i)
    print(pouch_lot_number)
  
    customer_cp_temp <- get(paste0("customer.cp.pouch.lot.number.df.", pouch_lot_number, sep = ""))
    print("head(customer_cp_temp)")
    print(head(customer_cp_temp))
    
    #extracted.internal.cp.controls.df <- assign(paste0("internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep = ""), internal_cp_temp)
    extracted.internal.cp.controls.df <- internal_cp_temp
    extracted.internal.cp.controls.df$PouchSerialNumber <- as.factor(extracted.internal.cp.controls.df$PouchSerialNumber)
    extracted.internal.cp.controls.df$PouchLotNumber <- as.factor(extracted.internal.cp.controls.df$PouchLotNumber)
    extracted.internal.cp.controls.df$type <- 'internal'
    extracted.internal.cp.controls.df <- subset(extracted.internal.cp.controls.df, select = -c(SampleType))
    names(extracted.internal.cp.controls.df)[names(extracted.internal.cp.controls.df) == 'Name'] <- 'AssayName'
    
    #customer.cp.controls.df <- assign(paste0("customer.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep = ""), customer_cp_temp)
    #customer.cp.controls.df <- assign(paste0("customer.cp.pouch.lot.number.df.", pouch_lot_number, sep = ""), customer_cp_temp)
    #customer.cp.controls.df <- customer_cp_temp
    customer.cp.controls.df <- customer_cp_temp[customer_cp_temp$ResultType == 'control', c('PouchSerialNumber', 'PouchLotNumber', 'AssayName', 'Cp')]
    #customer.cp.controls.df <- cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', c('PouchSerialNumber', 'PouchLotNumber', 'AssayName', 'Cp')]
    customer.cp.controls.df$type <- 'customer'
    
    print("head(customer.cp.controls.df)")
    print(head(customer.cp.controls.df))
    
    internal.and.customer.cp.controls <- rbind(extracted.internal.cp.controls.df, customer.cp.controls.df)
  
    
    g5 <- ggplot()
    g5 <- g5 + geom_histogram(data = internal.and.customer.cp.controls, aes(Cp, fill = type), stat = "bin", bins = 10) 
    g5 <- g5 + facet_wrap(type~AssayName )
    # g5
    # print(i)
    # print(head(temp))
    # 
    # print plot
    jpeg(filename = paste("Control_Cp_dist-", pouch.lot.number.df$PouchLotNumber[i], ".jpg", sep="")) #, width = 480, height = 480, units = 'px', res = 300)
    print(g5) # Make plot
    dev.off()
    
    # calculate mean and summarySE
    internal.and.customer.cp.controls.avg <- aggregate(Cp ~ AssayName + type, internal.and.customer.cp.controls, mean)
    summary.internal.and.customer.cp.controls <- summarySE(internal.and.customer.cp.controls, measurevar="Cp", groupvars=c("AssayName", "type"), conf.interval = 0.95, na.rm=TRUE)
    
    g6 <- ggplot() + ylab(paste("Cp", "\u00B1", "std.dev.")) + ggtitle(paste('Customer control Cp data for pouch lot number ',  pouch.lot.number.df$PouchLotNumber[i], sep = ""))
    g6 <- g6 + geom_point(data = internal.and.customer.cp.controls, aes(x = AssayName, y = Cp), color = 'blue', size = 1, alpha = 0.25)
    g6 <- g6 + facet_wrap(~ type)
    g6 
    
    # g6 <- g6 + geom_point(data = cust.cp.runs.df[cust.cp.runs.df$ResultType == 'control', ], aes(x = AssayName, y = Cp), color = 'blue', size = 1, alpha = 0.25)
    g6 <- g6 + geom_point(data = internal.and.customer.cp.controls.avg, aes(x = AssayName, y = Cp), color = 'red', size = 2)
    g6 <- g6 + geom_errorbar(data = summary.internal.and.customer.cp.controls, aes(x = AssayName, ymin = Cp - sd, ymax = Cp + sd))
    #g6
    
    # print plot
    jpeg(filename = paste("Control_Cp_errorbar-", pouch.lot.number.df$PouchLotNumber[i], ".jpg", sep="")) #, width = 480, height = 480, units = 'px', res = 300)
    print(g6) # Make plot
    dev.off()
    
  }
}


# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------



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
  jpeg(paste(workDir, '/Figures/', file_name, sep= ""), width = width, height = height, units = 'in', res = 300)
  print(g1) # Make plot
  dev.off()
}
#
#
