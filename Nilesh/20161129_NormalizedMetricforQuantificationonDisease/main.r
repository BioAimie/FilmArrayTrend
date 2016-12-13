
# set the path and load libraries and data
workDir <-'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/20161129_NormalizedMetricforQuantificationofDisease'
sql_query <- 'C:/Users/nilesh_ingle/Documents/FilmArrayTrend/Nilesh/DataSources/SQL/20161129_NormalizedMetricforQuantificationonDisease'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(dplyr)
library(Rmisc)

# Enter parameters for output plot to be saved as jpg
width <- 12; height <- 15; plot_resolution <- 300 # inches, inches, dpi


# GET DATA - 1: list of assay names ---------------------------------------------------------------------------------------
# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan(paste(sql_path,'/query_RP_assayList.txt', sep=""),what=character(),quote="")
query <- paste(queryVector,collapse=" ")
assay.list.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)
# ------------------------------------------------------------------------------------------------------------------------


# function to plot Cp
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

for (i in 1:nrow(assay.list.df)){
  TargetName <- paste("\'",as.character(assay.list.df$Interpretation[i]),"\'", sep="") 
  # inside loop GET DATA - 2: Cp values for each assay name --------------------------------------------------------------------------
  # read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
  FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
  queryVector <- scan(paste(sql_path,'/query_CpRuns-3-RP_allCust.txt', sep=""),what=character(),quote="")
  query <- paste(queryVector,collapse=" ")
  query <- gsub("\\[(TargetName)\\] LIKE \'[a-zA-Z]+\'", paste("\\[TargetName\\] LIKE ", TargetName, " ", sep= ""), query)
  cp.runs.df <- sqlQuery(FADWcxn,query)
  odbcClose(FADWcxn)
  
  
  
  # calculate values 
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


