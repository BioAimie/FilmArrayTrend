
# set the path and load libraries and data
workDir <-'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161129_NormalizedMetricforQuantificationonDisease'
sql_path <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/DataSources/SQL/20161129_NormalizedMetricforQuantificationonDisease'
figures_path <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161129_NormalizedMetricforQuantificationonDisease/Figures/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Rmisc)
library(reshape2)
library(gtable)
library(grid)
library(data.table)
library(scales)
# OPTIONAL ---------------------------------------------------
# Enter 'NA' or a customer site id to get results only for this customer
customer_site_id = 7 # NA, 7, 33, 2
# ------------------------------------------------------------

# dual axes for ILI overlay plots (Aimie Code)
hinvert_title_grob <- function(grob){
  
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]
  
  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust 
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust 
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}




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

queryVector <- scan(paste(sql_path,'/query_CenteredRate.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
if (!is.na(customer_site_id)){
  query <- gsub("\\[CustomerSiteId\\] = [0-9]+", paste("[CustomerSiteId] = ", customer_site_id, sep= ""), query)
}else {
  query <- gsub("AND S.\\[CustomerSiteId\\] = [0-9]+", "",query) 
}
centered.rate.df <- sqlQuery(FADWcxn,query)

queryVector <- scan(paste(sql_path,'/query_EpiWeeksCalendar.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
calendar.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# rename 'AssayName' to 'TargetName'
names(centered.rate.df)[names(centered.rate.df) == 'AssayName'] <- 'TargetName'
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

# Calculate 'median' of Cp for each assay if >= 2 wells are positive .........
# sort columns
all.customer.pouch.lot.number.df <- all.customer.pouch.lot.number.df[order(all.customer.pouch.lot.number.df$RunDataId, all.customer.pouch.lot.number.df$TargetName, all.customer.pouch.lot.number.df$AssayName, all.customer.pouch.lot.number.df$WellDataId), ]
# flag assay result for 'positive' assay as '1'
all.customer.pouch.lot.number.df$FlagNUM_AssayPositive <- ifelse(all.customer.pouch.lot.number.df$AssayResult == 'Positive', 1, 0)
# subset non-control data and flag assay as 'Yes' if >=2 wells are positive
temp.all.customer <- all.customer.pouch.lot.number.df[all.customer.pouch.lot.number.df$ResultType == 'organism', ]
agg.temp.all.customer <- aggregate(FlagNUM_AssayPositive ~ RunDataId + AssayName, temp.all.customer, sum)
names(agg.temp.all.customer)[names(agg.temp.all.customer) == 'FlagNUM_AssayPositive'] <- 'SUM_FlagNUM_AssayPositive'
agg.temp.all.customer$FlagSTR_AssayPositive <- ifelse(agg.temp.all.customer$SUM_FlagNUM_AssayPositive >= 2, 'Yes', 'No')
# merge the 'non-control' data back onto to the main table
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


# # implementing "do.call" to identify >= 2 (+)ve wells -----------
# wells.df <- all.customer.pouch.lot.number.df
# assay_name <- unique(wells.df$AssayName)
# target_name <- unique(wells.df$TargetName)
# run_data_id <- unique(wells.df$RunDataId)
# temp <- do.call(rbind, lapply(1:length(run_data_id), function(x)
#     do.call(rbind, lapply(1:length(target_name), function(y)
#       do.call(rbind, lapply(1:length(assay_name), function(z)
#           data.frame(
#           RunDataId = wells.df$RunDataId[x],
#           TargetName = wells.df$TargetName[y],
#           AssayName = wells.df$AssayName[z],
#           #Flag = ifelse(length(grep("Positive", unlist(strsplit(paste(wells.df$AssayResult[z], wells.df$AssayResult[z+1], wells.df$AssayResult[z+2], sep=" "), split=" ")))) >= 2, 'Yes', 'No')
#           Flag = if_else(length(grep("Positive", wells.df$AssayResult[wells.df$RunDataId == run_data_id[x] & wells.df$TargetName == target_name[y] & wells.df$AssayName == assay_name[z]])) >= 2, "Yes", "No")
#         )
# ))))))
# 
#
# # implementing "do.call" to calculate median ----------
# cp.df <- all.customer.pouch.lot.number.df
# #
# cp.controls <- subset(cp.df, ResultType == 'control')
# cp.df <- subset(cp.df, ResultType != 'control')
# runs <- unique(cp.df$RunDataId)
# cp.median <- do.call(rbind, lapply(1:length(runs), function(x)
#   do.call(rbind, lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])), function(y)
#     do.call(rbind,  lapply(1:length(unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y],'AssayName'])), function(z)
#       data.frame(RunDataId = runs[x],
#                  TargetName = unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y],
#                  AssayName = unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x], 'TargetName'])[y], 'AssayName'])[z],
#                  MedianCp = median(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y] & cp.df$AssayName==unique(cp.df[cp.df$RunDataId==runs[x] & cp.df$TargetName==unique(cp.df[cp.df$RunDataId==runs[x],'TargetName'])[y],'AssayName'])[z], 'Cp'])
#       )
#     ))
#   ))
# ))
# # -----------------------------------------------------


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

# find standard deviation from mean
# summary_PCR1 <- summarySE(div_PCR1, measurevar="Cp", groupvars=c("AssayName"), conf.interval = 0.95, na.rm=TRUE)
# # add ConcatDate to the control table
# qc.control.cp.df <- unique(merge(x = div_PCR1, y = all.customer.pouch.lot.number.df[,c('RunDataId', 'AssayName', 'ConcatDate')], by.x = c('RunDataId', 'AssayName'), by.y = c('RunDataId', 'AssayName'), all.x = TRUE))
# #summary_PCR1 <- summarySE(qc.control.cp.df, measurevar="Cp", groupvars=c("ConcatDate", "AssayName"), conf.interval = 0.95, na.rm=TRUE)
# div_PCR1$CpMinusMeanCp <- div_PCR1$Cp - summary_PCR1$Cp
# div_PCR1$new_Cp <- div_PCR1$Cp - div_PCR1$CpMinusMeanCp
# 
# div_PCR1_1 <- unique(merge(x = qc.control.cp.df, y = summary_PCR1[, c('ConcatDate', 'sd')], by.x =  'ConcatDate', by.y = 'ConcatDate', all.x = TRUE))

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
#merge_PCR1_median$NormalizedCp <- (merge_PCR1_median$MedianCp / mean(!is.na(merge_PCR1_median$divisor_PCR1)))

# AGGREGATE Normalized Cp: find mean of normalized Cp for each ConcatDate
mean_normalizedCp <- aggregate(NormalizedCp ~ ConcatDate + AssayName, merge_PCR1_median, mean)
names(mean_normalizedCp)[names(mean_normalizedCp) == 'NormalizedCp'] <- 'MeanNormalizedCp'
dateBreaks <- unique(as.character(mean_target_normalizedCp$ConcatDate))[order(unique(as.character(mean_target_normalizedCp$ConcatDate)))][seq(1, length(unique(as.character(mean_target_normalizedCp$ConcatDate))), 20)]

# assays for target
# Create list of TargetName and AssayName for plotting
assay_target_names <- unique(new.all.customer.pouch.lot.number.df[new.all.customer.pouch.lot.number.df$ResultType == 'organism', c('TargetName', 'AssayName')])
all_targets <- unique(assay_target_names$TargetName)

for (i in 1:length(all_targets)){
  assays_for_target <- unique(assay_target_names[assay_target_names$TargetName == all_targets[i], 'AssayName'])
  # plot the mean of median cp for one assay for along ConcatDate (for an assay)
  g2 <- ggplot()  +  xlab("Year-Week") + ylab(paste("Mean Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Assay Cp for ", all_targets[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
  g2 <- g2 + geom_point(data = subset(mean_normalizedCp, AssayName %in% assays_for_target), aes(x = as.factor(ConcatDate), y = MeanNormalizedCp, color = AssayName), alpha = 0.5, size = 3)
  g2 <- g2 + geom_bar(data = centered.rate.df[grep(all_targets[i], centered.rate.df$TargetName), ], aes(x = as.factor(ConcatDate), y = CenteredRate*10), stat = "identity", alpha = 0.5) 
  g2
    
  target <- if(grepl("/", target_name[i])){gsub("/", " ",all_targets[i])} else {all_targets[i]}
  print(target)
  # print plot
  jpeg(filename = paste("Assay_Cp--", target, ".jpg", sep=""), width = 1500, height = 480, units = 'px')
  print(g2) # Make plot
  dev.off()
}

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
# in case of 'Influenza A%' target consider the 'MedianCp' of only the 'specific' assays for target
# Influenza A H1: 'H1-pan' as positive
# Influenza A H1-2009: 'H1-2009' as positive
# Influenza A (no subtype detected): 'Apan1' and 'Apan2' as positive
# Influenza A H3: 'H3' as positive
temp_influ <- new.all.customer.pouch.lot.number.df
temp_influ$MedianCp[temp_influ$TargetName == "Influenza A H1" & temp_influ$AssayName != "FluA-H1-pan" & temp_influ$ResultType != "control"] <- NA
temp_influ$MedianCp[temp_influ$TargetName == "Influenza A H3" & temp_influ$AssayName != "FluA-H3" & temp_influ$ResultType != "control"] <- NA
temp_influ$MedianCp[temp_influ$TargetName == 'Influenza A H1-2009' & temp_influ$AssayName != 'FluA-H1-2009' & temp_influ$ResultType != 'control'] <- NA
temp_influ$MedianCp[temp_influ$TargetName == "Influenza A (no subtype detected)" & temp_influ$AssayName != "FluA-pan1" & temp_influ$AssayName != "FluA-pan2" & temp_influ$ResultType != "control"] <- NA




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
merge_PCR1_min_median$NormalizedCp <- (merge_PCR1_min_median$Cp / merge_PCR1_median$divisor_PCR1)
#merge_PCR1_min_median$NormalizedCp <- (merge_PCR1_min_median$Cp / mean(!is.na(merge_PCR1_median$divisor_PCR1)))

# AGGREGATE Normalized Cp: find mean of normalized Cp for each ConcatDate
mean_target_normalizedCp <- aggregate(NormalizedCp ~ ConcatDate + TargetName, merge_PCR1_min_median, mean)
names(mean_target_normalizedCp)[names(mean_target_normalizedCp) == 'NormalizedCp'] <- 'MeanNormalizedCp'

# count 'RunDataId' per week
count_rundataid_per_week <- count(unique(all.customer.pouch.lot.number.df[, c('ConcatDate', 'RunDataId')]), c('ConcatDate'))
merge_PCR1_min_median <- merge(x = merge_PCR1_min_median, y = count_rundataid_per_week, by.x = 'ConcatDate', by.y = 'ConcatDate', all.x = TRUE)
merge_PCR1_min_median_organism <- merge_PCR1_min_median[merge_PCR1_min_median$ResultType == 'organism', ]
#merge_PCR1_min_median_organism <- subset(merge_PCR1_min_median_organism, select = -c(freq.x))
names(merge_PCR1_min_median_organism)[names(merge_PCR1_min_median_organism) == 'AssayName.x'] <- 'AssayName'
names(merge_PCR1_min_median_organism)[names(merge_PCR1_min_median_organism) == 'freq.y'] <- 'freq'

# summary for CI error bars
summary_merge_PCR1_min_median_organism <- summarySE(merge_PCR1_min_median_organism, measurevar="NormalizedCp", groupvars=c("ConcatDate", "TargetName"), conf.interval = 0.95, na.rm=TRUE)

# Create list of TargetName and AssayName for plotting
assay_name <- unique(merge_PCR1_min_median_organism$AssayName[merge_PCR1_min_median_organism$ResultType == 'organism'])
target_name <- unique(merge_PCR1_min_median_organism$TargetName[merge_PCR1_min_median_organism$ResultType == 'organism'])

# add ConcatDate to the control table
qc.control.cp.df <- unique(merge(x = div_PCR1, y = merge_PCR1_median[,c('RunDataId', 'AssayName', 'ConcatDate')], by.x = c('RunDataId', 'AssayName'), by.y = c('RunDataId', 'AssayName'), all.x = TRUE))

# internal QC cp
internal_cp <- all.internal.cp.pouch.lot.number.df
internal_cp$RunDate <- as.Date(internal_cp$RunDate)
internal_cp$PouchSerialNumber <- as.character(internal_cp$PouchSerialNumber)


calendar.df$Date <- as.Date(calendar.df$Date)
calendar.df <- calendar.df[calendar.df$Date >= min(internal_cp$RunDate) & calendar.df$Date <= max(internal_cp$RunDate), ]

internal_cp <- unique(merge(x = calendar.df, y = internal_cp[internal_cp$Name == "PCR1", ], by.x = "Date", by.y = "RunDate", all.x = TRUE))
# find internal median cp for each week
internal_cp$ConcatDate <- as.numeric(ifelse(internal_cp$EpiWeek > 9, paste(internal_cp$EpiYear, internal_cp$EpiWeek, sep=""), paste(internal_cp$EpiYear, "0", internal_cp$EpiWeek, sep="")))
internal_median_cp <- aggregate(Cp ~ ConcatDate + PouchLotNumber, internal_cp, median)

dateBreaks <- unique(as.character(mean_target_normalizedCp$ConcatDate))[order(unique(as.character(mean_target_normalizedCp$ConcatDate)))][seq(1, length(unique(as.character(mean_target_normalizedCp$ConcatDate))), 20)]

for (i in 1:length(target_name)){
#for (i in 1:1){   
  temp <- merge_PCR1_min_median_organism[grep(target_name[i], merge_PCR1_min_median_organism$TargetName), ]
  temp_merge_internal <- merge(x = temp, y = internal_median_cp, by.x = 'ConcatDate', by.y = 'ConcatDate', all.x = TRUE)
  temp_merge_internal <- temp_merge_internal[!is.na(temp_merge_internal$RunDataId), ]
  temp_merge_internal$newNormCp <- (temp_merge_internal$NormalizedCp * temp_merge_internal$Cp.y)/10
  summary_temp_merge_internal <- summarySE(temp_merge_internal, measurevar="newNormCp", groupvars=c("ConcatDate"), conf.interval = 0.95, na.rm=TRUE)
  
  # calculate freq
  # count 'RunDataId' per week
  count_rundataid <- count(unique(temp_merge_internal[, c('ConcatDate', 'RunDataId')]), c('ConcatDate'))
  temp_merge_internal <- merge(x = temp_merge_internal, y = count_rundataid, by.x = 'ConcatDate', by.y = 'ConcatDate', all.x = TRUE)
  temp_merge_internal <- temp_merge_internal[temp_merge_internal$ResultType == 'organism', ]
  temp_merge_internal <- subset(temp_merge_internal, select = -c(freq.x))
  names(temp_merge_internal)[names(temp_merge_internal) == 'freq.y'] <- 'freq'
  
  target <- target_name[i]
  # plot the mean of median cp for one assay for along ConcatDate (for an assay)
  # # # ----------------------works without legends-------------------------------------
  # g3 <- ggplot() +  xlab("Year-Week") + ylab(paste("Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Target Cp for ", target_name[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
  # g3 <- g3 + geom_bar(data = centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ], aes(x = as.factor(ConcatDate), y = CenteredRate*10), stat = "identity", alpha = 0.5)
  # g3 <- g3 + geom_point(data = qc.control.cp.df[qc.control.cp.df$AssayName == "PCR1",  ], aes(x = as.factor(ConcatDate), y = divisor_PCR1/10), color = 'green', alpha = 0.5, size = 1)
  # g3 <- g3 + geom_point(data = internal_median_cp, aes(x = as.factor(ConcatDate), y = Cp/10), color = 'orange', alpha = 0.5, size = 2)
  # g3 <- g3 + geom_point(data = temp_merge_internal, aes(x = as.factor(ConcatDate), y = newNormCp, color = freq), alpha = 0.05, size = 2)
  # g3 <- g3 + geom_errorbar(data = summary_temp_merge_internal, aes(x = as.factor(ConcatDate), ymin = newNormCp - ci, ymax = newNormCp + ci))
  # g3
  # # ------------------------------------------------------------------------

  # # -----PLOT without 'gradient fill' for count AND with legends-------------------
  # g3 <- ggplot() +  xlab("Year-Week") + ylab(paste("Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Target Cp for ", target_name[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
  # g3 <- g3 + scale_fill_manual(name = "", values = c(SP ="grey"), labels=c("Site Percent Detection"))
  # g3 <- g3 + scale_color_manual(name = "", values = c(BLUE = "blue", GREEN = "green", ORANGE ="orange", BLACK = "black"), labels=c("Cp", "Customer Controls","Internal Controls", "NULL"))
  # g3 <- g3 + scale_color_gradient(name = "", low="#3B4FB8", high="#B71B1A", limits = c(min(temp_merge_internal$freq), max(temp_merge_internal$freq)))
  # 
  # if(length(!is.na(centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ])) > 0){
  #   g3 <- g3 + geom_bar(data = centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ], aes(x = as.factor(ConcatDate), y = CenteredRate*10, fill = "SP"), stat = "identity", alpha = 0.5) #+  scale_fill_manual(name = "", values = c(SP ="grey"), labels=c("Site Percent Detection"))
  # }
  # g3 <- g3 + scale_color_manual(name = "", values = c(GREEN = "green", ORANGE ="orange"), labels=c("Customer Controls","Internal Controls"))
  # g3 <- g3 + geom_point(data = qc.control.cp.df[qc.control.cp.df$AssayName == "PCR1",  ], aes(x = as.factor(ConcatDate), y = divisor_PCR1/10, color = "GREEN"), alpha = 0.5, size = 1) #+ scale_color_manual(name = "", values = c(GREY = "grey", GREEN = "green", ORANGE ="orange"), labels=c("Site Percent Detection", "Customer Controls","Internal Controls"))
  # g3 <- g3 + geom_point(data = internal_median_cp, aes(x = as.factor(ConcatDate), y = Cp/10, color = "ORANGE"), alpha = 0.5, size = 2) #+ scale_color_manual(name = "", values = c(GREY = "grey", GREEN = "green", ORANGE ="orange"), labels=c("Site Percent Detection", "Customer Controls","Internal Controls"))
  # g3 <- g3 + geom_errorbar(data = summary_temp_merge_internal, aes(x = as.factor(ConcatDate), ymin = newNormCp - ci, ymax = newNormCp + ci))
  # g3 <- g3 + geom_point(data = temp_merge_internal, aes(x = as.factor(ConcatDate), y = newNormCp), color = 'blue',  alpha = 0.1, size = 3) #+ scale_color_gradientn(name = "A", low="blue", high="darkblue", limits = c(min(temp_merge_internal$freq), max(temp_merge_internal$freq)))
  # g3
  # # ------------------------------------------------------------------------

  # ##-----PLOT with gradient and ALL legends -------------------------------------------------------
  # ## set colors for discrete scale as an alternative to gradient scale
  # colfunc <- colorRampPalette(c("lightblue", "darkblue"))
  # x <- colfunc(length(unique(temp_merge_internal$freq)))
  # y <- c(1:length(unique(temp_merge_internal$freq)))
  # #x_label <- vector(mode="character", length = length(unique(temp_merge_internal$freq)))
  # # set title and labels
  # g3 <- ggplot() +  xlab("Year-Week") + ylab(paste("Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Target Cp for ", target_name[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
  # g3 <- g3 + theme(legend.position="bottom", legend.box = "horizontal",legend.justification = 0.1)
  # # set manual scale for 'fill' and 'color'
  # g3 <- g3 + scale_fill_manual(name = "", values = c(SP ="grey"), labels=c("Site Percent Detection"))
  # g3 <- g3 + scale_color_manual(name = "", values = c(x, "green", "orange"), labels = c(y, "Customer Controls", "Internal Controls")) # creates all labels
  # 
  # g3 <- g3 + geom_point(data = temp_merge_internal, aes(x = as.factor(ConcatDate), y = newNormCp, color = as.factor(freq)),alpha = 0.1, size = 3)
  # if(length(!is.na(centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ])) > 0){
  #   g3 <- g3 + geom_bar(data = centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ], aes(x = as.factor(ConcatDate), y = CenteredRate*10, fill = "SP"), stat = "identity", alpha = 0.5)
  # }
  # g3 <- g3 + geom_point(data = qc.control.cp.df[qc.control.cp.df$AssayName == "PCR1",  ], aes(x = as.factor(ConcatDate), y = divisor_PCR1/10, color = "Customer Controls"), alpha = 0.5, size = 1)
  # g3 <- g3 + geom_point(data = internal_median_cp, aes(x = as.factor(ConcatDate), y = Cp/10, color = "Internal Controls"), alpha = 0.5, size = 2)
  # g3 <- g3 + geom_errorbar(data = summary_temp_merge_internal, aes(x = as.factor(ConcatDate), ymin = newNormCp - ci, ymax = newNormCp + ci))
  # g3
  # # ------------------------------------------------------------------------------------------------------------
  

# ==========================================================================================================================================
# (Aimie Code) Plot dual y-axis
# ==========================================================================================================================================
  temp_centered <- centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ]
  
  temp <- do.call(rbind, lapply(1:nrow(temp_centered), function(x) data.frame(
    CustomerSiteId = temp_centered$CustomerSiteId[x],
    ConcatDate = temp_centered$ConcatDate[x],
    TargetName = temp_centered$TargetName[x],
    CenteredRate = temp_centered$CenteredRate[x],
    NormalizedCR = abs(temp_centered$CenteredRate[x] - mean(temp_centered$CenteredRate))/(sd(temp_centered$CenteredRate))
  )))
  
  
  p1 <- ggplot()+  xlab("Year-Week") + ylab(paste("Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Target Cp for ", target_name[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
  p1 <- p1 + geom_point(data = temp_merge_internal, aes(x = as.factor(ConcatDate), y = newNormCp, color = freq), alpha = 0.05, size = 2)
  p1 <- p1 + geom_errorbar(data = summary_temp_merge_internal, aes(x = as.factor(ConcatDate), ymin = newNormCp - ci, ymax = newNormCp + ci))
  p1 <- p1 + expand_limits(y=0.35) 
  p1 <- p1 + theme(plot.title=element_text(hjust=0.5),text=element_text(size=12, face='bold'), axis.text=element_text(size=12, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) 
  
  if(length(!is.na(centered.rate.df[grep(target_name[i], centered.rate.df$TargetName), ])) > 0){
    p2 <- ggplot() +  xlab("Year-Week") + ylab(paste("Normalized Cp ", "\u00B1", "95% CI.")) + ggtitle(paste("Normalized Target Cp for ", target_name[i], sep="")) + scale_x_discrete(breaks=dateBreaks)
    p2 <- p2 + geom_line(data = temp, aes(x = as.factor(ConcatDate), y = NormalizedCR, group=1), stat='summary', fun.y=sum) + stat_summary(fun.y=sum, geom="line")
    p2 <- p2 + expand_limits(y=0)
    p2 <- p2 + theme(plot.title=element_text(hjust=0.5),text=element_text(size=12, face='bold'), axis.text=element_text(size=12, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) 
    p2 <- p2 + labs(y='Percent Detection')
 }  
  
  # Get the ggplot grobs
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g1 <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)
  index <- which(g2$layout$name == "ylab") # Which grob contains the y axis title?
  ylab <- g2$grobs[[index]]                # Extract that grob
  ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  g1 <- gtable_add_grob(g1, ylab, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "ylab-r")
  index <- which(g2$layout$name == "axis-l")  # Which grob
  yaxis <- g2$grobs[[index]]                  # Extract the grob
  yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + unit(3, "pt")
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])
  yaxis$children[[2]] <- ticks
  g1 <- gtable_add_cols(g1, g2$widths[g2$layout[index, ]$l], pp$r)
  ax <- gtable_add_grob(g1, yaxis, pp$t, pp$r + 1, pp$b, pp$r + 1, clip = "off", name = "axis-r")
  grid.draw(ax)
  
  
  
  
  target <- ifelse(grepl("/", target_name[i]), gsub("/", " ",target_name[i]), target_name[i])
  target <- if(grepl("/", target_name[i])){gsub("/", " ",target_name[i])} else {target_name[i]}
  print(target)
  # print plot
  jpeg(paste(figures_path,"Target_Cp--", target, ".jpg", sep=""), width = 1500, height = 480, units = 'px')
  grid.draw(ax)
  dev.off()
}






# ==========================================================================================================================================









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


