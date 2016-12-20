
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
customer_site_id = 33 # NA, 7, 33, 2
# ------------------------------------------------------------

# GET DATA - 1: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/FADataWarehouse_query.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
pouch.lot.number.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)
# ------------------------------------------------------------------------------------------------------------------------


all.internal.cp.pouch.lot.number.df <- NULL
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
  # OPTIONAL: combine into dataframe 
  all.internal.cp.pouch.lot.number.df <- rbind(all.internal.cp.pouch.lot.number.df, get(internal.cp.df.name))
}

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



# ===============================================================================================================
# Code: Normalize Cp
# ===============================================================================================================
# normalize -------------------------------
# create divisor column
# using PCR1 -----(using the minimum of assay Cps) for a positive Target-----------------------------------------------------------------------------------------------------------------------------------
div_PCR1 <- aggregate(Cp ~ RunDataId + AssayName, all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "PCR1", ], median)
names(div_PCR1)[names(div_PCR1) == 'Cp'] <- 'divisor_PCR1'
all.customer.pouch.lot.number.df <- merge(x = all.customer.pouch.lot.number.df, y = div_PCR1, by.x = c("RunDataId"), by.y = c("RunDataId"), all.x = TRUE)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(AssayName.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'AssayName.x'] <- 'AssayName'

# create a multiplier (standard deviation)
# variation in PCR1 for each pouch lot number in customer + internal values of Cp
multi.PCR1 <- merge(x = all.internal.cp.pouch.lot.number.df[all.internal.cp.pouch.lot.number.df$Name == "PCR1", c('PouchLotNumber', 'Cp')], y = all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "PCR1", c('PouchLotNumber', 'Cp')], by.x = 'PouchLotNumber', by.y = 'PouchLotNumber')
melt.multi.PCR1 <- melt(multi.PCR1)
melt.multi.PCR1 <- subset(melt.multi.PCR1, select = -c(variable))
multi.PCR1.sd <- aggregate(value ~ PouchLotNumber, melt.multi.PCR1, sd)

# add the multiplier column to main table
# NOTE: 'na.omit' removes some pouch lot numbers
all.customer.pouch.lot.number.df <- na.omit(unique(merge(x = all.customer.pouch.lot.number.df, y = multi.PCR1.sd, by.x = 'PouchLotNumber', by.y = 'PouchLotNumber', all.x = TRUE)))
# all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(value.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'value'] <- 'MultiplierPCR1'
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'TargetName.x'] <- 'TargetName'

# normalize using PCR1 control
all.customer.pouch.lot.number.df$NormalizedCp_PCR1 <- round((all.customer.pouch.lot.number.df$MultiplierPCR1 * all.customer.pouch.lot.number.df$Cp)/ all.customer.pouch.lot.number.df$divisor_PCR1, 4)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(TargetName.y))

# find minimum of normalized values for each RunDataId, Target
cust.min.of.normalizedCp <- aggregate(NormalizedCp_PCR1 ~ RunDataId + TargetName, all.customer.pouch.lot.number.df, min)
names(cust.min.of.normalizedCp)[names(cust.min.of.normalizedCp) == 'NormalizedCp_PCR1'] <- 'MinNormalizedCp_PCR1'

cust.min.of.normalizedCp.merge <- merge(x = cust.min.of.normalizedCp, y = all.customer.pouch.lot.number.df, by.x = c('RunDataId', 'TargetName'), by.y = c('RunDataId', 'TargetName'), all.x = TRUE)

# plot
g8 <- ggplot()
g8 <- g8 + geom_point(data = all.customer.pouch.lot.number.df, aes(x = RunDate, y = NormalizedCp_PCR1, colour = factor(TargetName)), alpha = 0.2, size = 1)  + coord_cartesian(ylim=c(0,2.25))
g8 <- g8 + geom_point(data = cust.min.of.normalizedCp.merge, aes(x = RunDate, y = MinNormalizedCp_PCR1, color = factor(TargetName), size = 4))
g8 <- g8 + facet_wrap(~ PouchLotNumber)
g8


jpeg(filename = paste("min_NormalizedCp_PCR1",".jpg", sep=""), width = 5000, height = 3000, units = 'px', res = 300)
print(g8)
dev.off()



# -----------------# -----------------# -----------------# -----------------# -----------------
# create divisor column
# using PCR2 -----(using the minimum of assay Cps) for a positive Target-----------------------------------------------------------------------------------------------------------------------------------
div_PCR2 <- aggregate(Cp ~ RunDataId + AssayName, all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "PCR2", ], median)
names(div_PCR2)[names(div_PCR2) == 'Cp'] <- 'divisor_PCR2'
all.customer.pouch.lot.number.df <- merge(x = all.customer.pouch.lot.number.df, y = div_PCR2, by.x = c("RunDataId"), by.y = c("RunDataId"), all.x = TRUE)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(AssayName.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'AssayName.x'] <- 'AssayName'

# create a multiplier (standard deviation)
# variation in PCR2 for each pouch lot number in customer + internal values of Cp
multi.PCR2 <- merge(x = all.internal.cp.pouch.lot.number.df[all.internal.cp.pouch.lot.number.df$Name == "PCR2", c('PouchLotNumber', 'Cp')], y = all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "PCR2", c('PouchLotNumber', 'Cp')], by.x = 'PouchLotNumber', by.y = 'PouchLotNumber')
melt.multi.PCR2 <- melt(multi.PCR2)
melt.multi.PCR2 <- subset(melt.multi.PCR2, select = -c(variable))
multi.PCR2.sd <- aggregate(value ~ PouchLotNumber, melt.multi.PCR2, sd)

# add the multiplier column to main table
# NOTE: 'na.omit' removes some pouch lot numbers
all.customer.pouch.lot.number.df <- na.omit(unique(merge(x = all.customer.pouch.lot.number.df, y = multi.PCR2.sd, by.x = 'PouchLotNumber', by.y = 'PouchLotNumber', all.x = TRUE)))
# all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(value.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'value'] <- 'MultiplierPCR2'
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'TargetName.x'] <- 'TargetName'

# normalize using PCR2 control
all.customer.pouch.lot.number.df$NormalizedCp_PCR2 <- round((all.customer.pouch.lot.number.df$MultiplierPCR2 * all.customer.pouch.lot.number.df$Cp)/ all.customer.pouch.lot.number.df$divisor_PCR2, 4)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(TargetName.y))

# find minimum of normalized values for each RunDataId, Target
cust.min.of.normalizedCp <- aggregate(NormalizedCp_PCR2 ~ RunDataId + TargetName, all.customer.pouch.lot.number.df, min)
names(cust.min.of.normalizedCp)[names(cust.min.of.normalizedCp) == 'NormalizedCp_PCR2'] <- 'MinNormalizedCp_PCR2'

cust.min.of.normalizedCp.merge <- merge(x = cust.min.of.normalizedCp, y = all.customer.pouch.lot.number.df, by.x = c('RunDataId', 'TargetName'), by.y = c('RunDataId', 'TargetName'), all.x = TRUE)

# plot
g9 <- ggplot()
g9 <- g9 + geom_point(data = all.customer.pouch.lot.number.df, aes(x = RunDate, y = NormalizedCp_PCR2, colour = factor(TargetName)), alpha = 0.2, size = 1)  + coord_cartesian(ylim=c(0,2.25))
g9 <- g9 + geom_point(data = cust.min.of.normalizedCp.merge, aes(x = RunDate, y = MinNormalizedCp_PCR2, color = factor(TargetName), size = 4))
g9 <- g9 + facet_wrap(~ PouchLotNumber)
g9


jpeg(filename = paste("min_NormalizedCp_PCR2",".jpg", sep=""), width = 5000, height = 3000, units = 'px', res = 300)
print(g9)
dev.off()



# -----------------# -----------------# -----------------# -----------------# -----------------
# create divisor column
# using yeastRNA -----(using the minimum of assay Cps) for a positive Target-----------------------------------------------------------------------------------------------------------------------------------
div_yeastRNA <- aggregate(Cp ~ RunDataId + AssayName, all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "yeastRNA", ], median)
names(div_yeastRNA)[names(div_yeastRNA) == 'Cp'] <- 'divisor_yeastRNA'
all.customer.pouch.lot.number.df <- merge(x = all.customer.pouch.lot.number.df, y = div_yeastRNA, by.x = c("RunDataId"), by.y = c("RunDataId"), all.x = TRUE)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(AssayName.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'AssayName.x'] <- 'AssayName'

# create a multiplier (standard deviation)
# variation in yeastRNA for each pouch lot number in customer + internal values of Cp
multi.yeastRNA <- merge(x = all.internal.cp.pouch.lot.number.df[all.internal.cp.pouch.lot.number.df$Name == "yeastRNA", c('PouchLotNumber', 'Cp')], y = all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$AssayName == "yeastRNA", c('PouchLotNumber', 'Cp')], by.x = 'PouchLotNumber', by.y = 'PouchLotNumber')
melt.multi.yeastRNA <- melt(multi.yeastRNA)
melt.multi.yeastRNA <- subset(melt.multi.yeastRNA, select = -c(variable))
multi.yeastRNA.sd <- aggregate(value ~ PouchLotNumber, melt.multi.yeastRNA, sd)

# add the multiplier column to main table
# NOTE: 'na.omit' removes some pouch lot numbers
all.customer.pouch.lot.number.df <- na.omit(unique(merge(x = all.customer.pouch.lot.number.df, y = multi.yeastRNA.sd, by.x = 'PouchLotNumber', by.y = 'PouchLotNumber', all.x = TRUE)))
# all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(value.y))
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'value'] <- 'MultiplieryeastRNA'
names(all.customer.pouch.lot.number.df)[names(all.customer.pouch.lot.number.df) == 'TargetName.x'] <- 'TargetName'

# normalize using yeastRNA control
all.customer.pouch.lot.number.df$NormalizedCp_yeastRNA <- round((all.customer.pouch.lot.number.df$MultiplieryeastRNA * all.customer.pouch.lot.number.df$Cp)/ all.customer.pouch.lot.number.df$divisor_yeastRNA, 4)
all.customer.pouch.lot.number.df <- subset(all.customer.pouch.lot.number.df, select = -c(TargetName.y))

# find minimum of normalized values for each RunDataId, Target
cust.min.of.normalizedCp <- aggregate(NormalizedCp_yeastRNA ~ RunDataId + TargetName, all.customer.pouch.lot.number.df, min)
names(cust.min.of.normalizedCp)[names(cust.min.of.normalizedCp) == 'NormalizedCp_yeastRNA'] <- 'MinNormalizedCp_yeastRNA'

cust.min.of.normalizedCp.merge <- merge(x = cust.min.of.normalizedCp, y = all.customer.pouch.lot.number.df, by.x = c('RunDataId', 'TargetName'), by.y = c('RunDataId', 'TargetName'), all.x = TRUE)

# plot
g10 <- ggplot()
g10 <- g10 + geom_point(data = all.customer.pouch.lot.number.df, aes(x = RunDate, y = NormalizedCp_yeastRNA, colour = factor(TargetName)), alpha = 0.2, size = 1)  + coord_cartesian(ylim=c(0,4.5))
g10 <- g10 + geom_point(data = cust.min.of.normalizedCp.merge, aes(x = RunDate, y = MinNormalizedCp_yeastRNA, color = factor(TargetName), size = 4))
g10 <- g10 + facet_wrap(~ PouchLotNumber)
g10


jpeg(filename = paste("min_NormalizedCp_yeastRNA",".jpg", sep=""), width = 5000, height = 3000, units = 'px', res = 300)
print(g10)
dev.off()


