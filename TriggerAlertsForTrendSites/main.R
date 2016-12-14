# TRIGGER ALERTS 
setwd('~/FilmArrayTrend/TriggerAlertsForTrendSites/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(devtools)
require(dateManip)

# load the centered percent positivity by site and bug into the session
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/TriggerAlertsForTrendSites/rpSitePercentDetection.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
prev.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# exploratory analysis ---------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------
# 1. look at a histogram of percent detection (by target), then limit to last 52 weeks, then break out by site

