
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
library(reshape2)

# OPTIONAL ---------------------------------------------------
# Enter 'NA' or a customer site id to get results only for this customer
customer_site_id = 7 # NA, 7, 33, 2
# ------------------------------------------------------------

# GET DATA - 1: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/FADataWarehouse_query.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
if (!is.na(customer_site_id)){
query <- gsub("\\[CustomerSiteId\\] = [0-9]+", paste("[CustomerSiteId] = ", customer_site_id, sep= ""), query)
}else {
query <- gsub("AND S.\\[CustomerSiteId\\] = [0-9]+", "",query) 
}
pouch.lot.number.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)
# ------------------------------------------------------------------------------------------------------------------------


filmarray1.all.internal.cp.pouch.lot.number.df <- NULL
filmarray2.all.internal.cp.pouch.lot.number.df <- NULL
for(i in 1:nrow(pouch.lot.number.df)){
  print(i)
  # read in data from PMS PROD server 
  PMScxn <- odbcConnect('PMS_PROD')
  
  queryVector <- scan(paste(sql_path,'/FilmArray2_query-4.txt', sep=""),what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  query <- gsub("\\[PouchLotNumber\\] LIKE \'%[0-9].*%\'", paste("[PouchLotNumber] LIKE ", "\'", "%", pouch.lot.number.df$PouchLotNumber[i],  "%", "\'", sep= ""), query)
  internal.cp.df.name <- paste("filmarray2.internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep="")
  assign(internal.cp.df.name,sqlQuery(PMScxn,query))
  
  queryVector <- scan(paste(sql_path,'/query_FilmArray1.txt', sep=""),what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  query <- gsub("\\[PouchLotNumber\\] LIKE \'%[0-9].*%\'", paste("[PouchLotNumber] LIKE ", "\'", "%", pouch.lot.number.df$PouchLotNumber[i],  "%", "\'", sep= ""), query)
  internal.cp.df.name <- paste("filmarray1.internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep="")
  assign(internal.cp.df.name,sqlQuery(PMScxn,query))
  
  odbcClose(PMScxn)
  # OPTIONAL: combine into dataframe 
  filmarray1.all.internal.cp.pouch.lot.number.df <- rbind(filmarray1.all.internal.cp.pouch.lot.number.df, get(internal.cp.df.name))
  filmarray2.all.internal.cp.pouch.lot.number.df <- rbind(filmarray2.all.internal.cp.pouch.lot.number.df, get(internal.cp.df.name))
}
all.internal.cp.pouch.lot.number.df <- rbind(filmarray1.all.internal.cp.pouch.lot.number.df, filmarray2.all.internal.cp.pouch.lot.number.df)

# # GET DATA - 2: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
main.all.customer.pouch.lot.number.df <- NULL
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
  # combine into a dataframe if selection based on customer site id
  main.all.customer.pouch.lot.number.df <- rbind(main.all.customer.pouch.lot.number.df, get(customer.cp.df.name))
}
all.customer.pouch.lot.number.df <- main.all.customer.pouch.lot.number.df

# OPTIONAL: (if customer site id is used then) subset data
if (!is.na(customer_site_id)){
  all.customer.pouch.lot.number.df <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$CustomerSiteId == customer_site_id & !is.na(all.customer.pouch.lot.number.df$PouchSerialNumber), ]
  pouch.lot.number.df <- pouch.lot.number.df[pouch.lot.number.df$PouchLotNumber %in% all.customer.pouch.lot.number.df$PouchLotNumber, ]
}
all.customer.pouch.lot.number.df <- all.customer.pouch.lot.number.df[!is.na(all.customer.pouch.lot.number.df$PouchSerialNumber), ]



# ===============================================================================================================
# Code: Normalize Cp
# ===============================================================================================================

# take 'median' of Cp for each assay if >= 2 wells are positive
all.customer.pouch.lot.number.df <- all.customer.pouch.lot.number.df[order(all.customer.pouch.lot.number.df$RunDataId, all.customer.pouch.lot.number.df$TargetName, all.customer.pouch.lot.number.df$AssayName, all.customer.pouch.lot.number.df$WellDataId), ]
# flag assay result
all.customer.pouch.lot.number.df$FlagNUM_AssayPositive <- ifelse(all.customer.pouch.lot.number.df$AssayResult == 'Positive', 1, 0)
#
temp.all.customer <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$ResultType == 'organism', ]
agg.temp.all.customer <- aggregate(FlagNUM_AssayPositive ~ RunDataId + AssayName, temp.all.customer, sum)
names(agg.temp.all.customer)[names(agg.temp.all.customer) == 'FlagNUM_AssayPositive'] <- 'SUM_FlagNUM_AssayPositive'
agg.temp.all.customer$FlagSTR_AssayPositive <- ifelse(agg.temp.all.customer$SUM_FlagNUM_AssayPositive >= 2, 'Yes', 'No')

merge.agg.temp.all.customer <- merge(x = temp.all.customer, y = agg.temp.all.customer, by.x = c('RunDataId', 'AssayName'), by.y = c('RunDataId', 'AssayName'))
new.all.customer.pouch.lot.number.df <- merge.agg.temp.all.customer[merge.agg.temp.all.customer$FlagSTR_AssayPositive == 'Yes', ]

# ASSAY: find median cp for each assay that is flagged as 'Yes' for >= 2 wells 'positive'
median.organism.customer.df <- aggregate(Cp ~ RunDataId + AssayName, new.all.customer.pouch.lot.number.df, median)
names(median.organism.customer.df)[names(median.organism.customer.df) == 'Cp'] <- 'MedianCp'

# merge back to the main table which has controls
new.all.customer.pouch.lot.number.df <- merge (x = all.customer.pouch.lot.number.df, y = median.organism.customer.df, by.x = c('RunDataId', 'AssayName'), by.y = c('RunDataId', 'AssayName'), all.x = TRUE)
# OPTIONAL: just to be sure, replace 'MedianCp values by 'NA' for controls
new.all.customer.pouch.lot.number.df$MedianCp <- ifelse(new.all.customer.pouch.lot.number.df$ResultType == 'control', NA, new.all.customer.pouch.lot.number.df$MedianCp)
# order
new.all.customer.pouch.lot.number.df <- new.all.customer.pouch.lot.number.df[order(new.all.customer.pouch.lot.number.df$RunDataId, new.all.customer.pouch.lot.number.df$TargetName, new.all.customer.pouch.lot.number.df$AssayName, new.all.customer.pouch.lot.number.df$WellDataId), ]


# --------------------------------------------------------------------------------------------------------------------------------------------------
# NORMALIZE 
# get all 'controls'
control.PCR1 <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == 'PCR1', ]
control.PCR2 <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == 'PCR2', ]
control.yeastRNA <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == 'yeastRNA', ]

# find divisor
div_PCR1 <- aggregate(Cp ~ RunDataId + AssayName, control.PCR1, median)
div_PCR2 <- aggregate(Cp ~ RunDataId + AssayName, control.PCR2, median)
div_yeastRNA <- aggregate(Cp ~ RunDataId + AssayName, control.yeastRNA, median)

# rename columns
names(div_PCR1)[names(div_PCR1) == 'Cp'] <- 'divisor_PCR1'
names(div_PCR2)[names(div_PCR2) == 'Cp'] <- 'divisor_PCR2'
names(div_yeastRNA)[names(div_yeastRNA) == 'Cp'] <- 'divisor_yeastRNA'

# for ASSAY_NAME ----------------------------------------------------------------------------------------------------------------
# join PCR1 divisor to main table
merge_PCR1_median <- merge(x = new.all.customer.pouch.lot.number.df, y = div_PCR1, by.x = c('RunDataId'), by.y = c('RunDataId'), all.x = TRUE)
names(merge_PCR1_median)[names(merge_PCR1_median) == 'AssayName.x'] <- 'AssayName'
names(merge_PCR1_median)[names(merge_PCR1_median) == 'AssayName.y'] <- 'Normalized_By'

# normalize by dividing 'Cp' by 'divisor_PCR1'
merge_PCR1_median$NormalizedCp <- (merge_PCR1_median$MedianCp / merge_PCR1_median$divisor_PCR1)

# AGGREGATE Normalized Cp: find mean of normalized Cp for each ConcatDate
mean_normalizedCp <- aggregate(NormalizedCp ~ ConcatDate + AssayName, merge_PCR1_median, mean)
names(mean_normalizedCp)[names(mean_normalizedCp) == 'NormalizedCp'] <- 'MeanNormalizedCp'

# plot the mean of median cp for one assay for along ConcatDate (for an assay)
dateBreaks <- unique(as.character(mean_target_normalizedCp$ConcatDate))[order(unique(as.character(mean_target_normalizedCp$ConcatDate)))][seq(1, length(unique(as.character(mean_target_normalizedCp$ConcatDate))), 20)]
g2 <- ggplot() +  scale_x_discrete(breaks=dateBreaks)
g2 <- g2 + geom_point(data = mean_normalizedCp[grep("HRV1", mean_normalizedCp$AssayName),], aes(x = as.factor(ConcatDate), y = MeanNormalizedCp))
g2

# print plot
jpeg(paste("Assay_Cp", ".jpg", sep=""), width = 1500, height = 480, units = 'px')
print(g2) # Make plot
dev.off()




# for TARGET_NAME ----------------------------------------------------------------------------------------------------------------
# find MINIMUM of assays ---------------
min.median.cp.for.target <- aggregate(MedianCp ~ RunDataId + TargetName, new.all.customer.pouch.lot.number.df, min)
names(min.median.cp.for.target)[names(min.median.cp.for.target) == 'MedianCp'] <- 'MinMedianCp'
# merge with the main table
merge.min.median.cp.for.target <- merge(x = new.all.customer.pouch.lot.number.df, y = min.median.cp.for.target, by.x = c('RunDataId', 'TargetName'), by.y = c('RunDataId', 'TargetName'), all.x = TRUE )

# merge with PCR1 
merge_PCR1_min_median <- merge(x = merge.min.median.cp.for.target, y = div_PCR1, by.x = 'RunDataId', by.y = 'RunDataId', all.x = TRUE)
names(merge_PCR1_median)[names(merge_PCR1_median) == 'AssayName.x'] <- 'AssayName'
names(merge_PCR1_median)[names(merge_PCR1_median) == 'AssayName.y'] <- 'Normalized_By'

# normalize by dividing 'Cp' by 'divisor_PCR1'
merge_PCR1_min_median$NormalizedCp <- (merge_PCR1_min_median$Cp / merge_PCR1_min_median$divisor_PCR1)

# AGGREGATE Normalized Cp: find mean of normalized Cp for each ConcatDate
mean_target_normalizedCp <- aggregate(NormalizedCp ~ ConcatDate + TargetName, merge_PCR1_min_median, mean)
names(mean_target_normalizedCp)[names(mean_target_normalizedCp) == 'NormalizedCp'] <- 'MeanNormalizedCp'

# plot the mean of median cp for one assay for along ConcatDate (for an assay)
dateBreaks <- unique(as.character(mean_target_normalizedCp$ConcatDate))[order(unique(as.character(mean_target_normalizedCp$ConcatDate)))][seq(1, length(unique(as.character(mean_target_normalizedCp$ConcatDate))), 20)]
g3 <- ggplot() + ggtitle("Normalized Target Cp") + scale_x_discrete(breaks=dateBreaks)
g3 <- g3 + geom_point(data = mean_target_normalizedCp[grep(".*Rhino.*", mean_target_normalizedCp$TargetName),], aes(x = as.factor(ConcatDate), y = MeanNormalizedCp))
g3

# print plot
jpeg(paste("Target_Cp", ".jpg", sep=""), width = 1500, height = 480, units = 'px')
print(g3) # Make plot
dev.off()





#










# ===============================================================================================================
# Code: Plotting Cp
# ===============================================================================================================
# Compare the Cp controls for customer and internal ------------------------------------------------------------------------
if(nrow(pouch.lot.number.df) > 0){
  # extract the internal Cp data for the lot numbers from customer Cp data ---
  for (i in 1:nrow(pouch.lot.number.df)){
    internal_cp_temp <- get(paste0("internal.cp.pouch.lot.number.df.", pouch.lot.number.df$PouchLotNumber[i], sep = ""))
    pouch_lot_number <- pouch.lot.number.df$PouchLotNumber[i]
    
    # check if the pouch_lot_number is present in 'internal' data
    if(nrow(internal_cp_temp) > 0){
      #customer_cp_temp <- get(paste0("customer.cp.pouch.lot.number.df.", pouch_lot_number, sep = ""))
      
      # read cp for either all or just one customer
      if(!is.na(customer_site_id)){
        customer_cp_temp <- na.omit(all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$PouchLotNumber == pouch_lot_number,])
      } else {
        customer_cp_temp <- get(paste0("customer.cp.pouch.lot.number.df.", pouch_lot_number, sep = ""))
      }
      
      # internal cp
      extracted.internal.cp.controls.df <- internal_cp_temp
      extracted.internal.cp.controls.df$PouchSerialNumber <- as.factor(extracted.internal.cp.controls.df$PouchSerialNumber)
      extracted.internal.cp.controls.df$PouchLotNumber <- as.factor(extracted.internal.cp.controls.df$PouchLotNumber)
      extracted.internal.cp.controls.df$type <- 'internal'
      extracted.internal.cp.controls.df <- subset(extracted.internal.cp.controls.df, select = -c(SampleType))
      names(extracted.internal.cp.controls.df)[names(extracted.internal.cp.controls.df) == 'Name'] <- 'AssayName'
      # customer cp
      customer.cp.controls.df <- customer_cp_temp[customer_cp_temp$ResultType == 'control', c('PouchSerialNumber', 'PouchLotNumber', 'AssayName', 'Cp')]
      customer.cp.controls.df$type <- 'customer'
      # combined
      internal.and.customer.cp.controls <- rbind(extracted.internal.cp.controls.df, customer.cp.controls.df)
      
      # plot and save histogram to see distribution
      # format plot title
      if(!is.na(customer_site_id)){
        cp_title <- paste('Cp distribution comparison for pouch lot number ', pouch_lot_number, '[CustomerSiteId] = ', customer_site_id, sep = "")
      } else{
        cp_title <- paste('Cp distribution comparison for pouch lot number ', pouch_lot_number, sep = "")
      }
      g5 <- ggplot() + ggtitle(cp_title)
      g5 <- g5 + geom_histogram(data = internal.and.customer.cp.controls, aes(Cp, fill = type), stat = "bin", bins = 10) 
      g5 <- g5 + facet_wrap(type ~ AssayName )
      jpeg(filename = paste("Control_Cp_dist-", pouch_lot_number, ".jpg", sep="")) #, width = 480, height = 480, units = 'px', res = 300)
      print(g5) # Make plot
      dev.off()
      
      # calculate mean and summarySE
      internal.and.customer.cp.controls.avg <- aggregate(Cp ~ AssayName + type, internal.and.customer.cp.controls, mean)
      summary.internal.and.customer.cp.controls <- summarySE(internal.and.customer.cp.controls, measurevar="Cp", groupvars=c("AssayName", "type"), conf.interval = 0.95, na.rm=TRUE)
      
      # plot and save error bar plot 
      # format plot title
      if(!is.na(customer_site_id)){
        cp_title <- paste('Cp mean comparison for pouch lot number ', pouch_lot_number, '[CustomerSiteId] = ', customer_site_id, sep = "")
      } else{
        cp_title <- paste('Cp mean comparison for pouch lot number ', pouch_lot_number, sep = "")
      }
      g6 <- ggplot() + ylab(paste("Cp", "\u00B1", "95% CI.")) + ggtitle(cp_title)
      g6 <- g6 + geom_point(data = internal.and.customer.cp.controls, aes(x = AssayName, y = Cp), color = 'blue', size = 1, alpha = 0.25)
      g6 <- g6 + facet_wrap(~ type)
      g6 <- g6 + geom_point(data = internal.and.customer.cp.controls.avg, aes(x = AssayName, y = Cp), color = 'red', size = 2)
      # standard deviation error bars
      #g6 <- g6 + geom_errorbar(data = summary.internal.and.customer.cp.controls, aes(x = AssayName, ymin = Cp - sd, ymax = Cp + sd))
      
      # 95% CI error bars
      g6 <- g6 + geom_errorbar(data = summary.internal.and.customer.cp.controls, aes(x = AssayName, ymin = Cp - ci, ymax = Cp + ci))
      jpeg(filename = paste("Control_Cp_errorbar-", pouch_lot_number, ".jpg", sep="")) #, width = 480, height = 480, units = 'px', res = 300)
      print(g6)
      dev.off()
    }
  }
} else{
  print(paste0("NOTE: Pouch lot numbers not available for customer site id # ", customer_site_id, sep = ""))
}


