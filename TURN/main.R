workDir <-'~/FilmArrayTrend/TURN/'
setwd(workDir)

# load libraries
library(RODBC)
library(lubridate)
library(xlsx)
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
library(gtable)
library(RColorBrewer)
library(devtools)
library(dplyr)
library(tidyr)
require(dateManip)
library(glmnet)

# load custom functions
source('../Rfunctions/TURN_omega.R')
source('../Rfunctions/generateCombosBetter.R')
source('~/WebHub/AnalyticsWebHub/Rfunctions/createPaletteOfVariableLength.R')

# dual axes for ILI overlay plots
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

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- readLines('../DataSources/SQL/TURN/RunDataBySite.sql')
query <- paste(queryVector,collapse="\n")
runs.df <- sqlQuery(FADWcxn,query)
queryVector <- readLines('../DataSources/SQL/TURN/rpBugData.sql')
query <- paste(queryVector,collapse="\n")
bugs.df <- sqlQuery(FADWcxn,query)
bugs.df <- bugs.df[bugs.df$Target != 'Bocavirus',]
queryVector <- readLines('../DataSources/CustomerSiteIdsWithNames.sql')
query <- paste(queryVector,collapse="\n")
names.df <- sqlQuery(FADWcxn,query)
queryVector <- readLines('../DataSources/ShortNames.sql')
query <- paste(queryVector,collapse="\n")
abvs.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in data from PMS PROD server
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- scan('../DataSources/AllSitesRegionKey.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
regions.df <- sqlQuery(PMScxn,query)
odbcClose(PMScxn)

# read in data from Excel files
# census regional ILI for 2013-2017 (can compare to TURN in regions and over all time)
cdc.reg.df <- read.csv('../DataSources/Epidemics/CensusRegionalILI.csv', header=TRUE, sep=',')
# HHS region data for ILI and influenza for 2013-2014 season (can compare three reported metrics to TURN)
cdc.ili.df <- read.csv('../DataSources/Epidemics/HHSregionalILI.csv', header=TRUE, sep=',')
goog.flu.df <- read.xlsx('../DataSources/Epidemics/GoogleFlu.xlsx', sheetName='Sheet1', colIndex = c(1,2,3))
goog.flu.df$Date <- as.character(goog.flu.df$Date)
goog.flu.df[grep('41896', goog.flu.df$Date), 'Date'] <- '2014-09-14'
goog.flu.df$Date <- as.Date(goog.flu.df$Date, format='%Y-%m-%d')
goog.flu.df <- goog.flu.df[!(is.na(goog.flu.df$Date)), ]
cdc.flu.df <- read.csv('../DataSources/Epidemics/HHSregionalInfluenza_CombinedFor2013-14.csv', header=TRUE, sep=',')

# make an epi calendar
calendar.df <- transformToEpiWeeks(createCalendarLikeMicrosoft(2012, 'Week'))
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
calendar.df <- calendar.df[calendar.df$YearWeek > '2012-51', ]
calendar.df$Days <- 1

# add on regions and date information to the data frames
runs.reg <- merge(merge(runs.df, names.df[,c('CustomerSiteId','State')], by='CustomerSiteId'), data.frame(Province = regions.df$StateAbv, Region = regions.df$CensusRegionLocal, hhsRegion = regions.df$CDCRegion), by.x='State', by.y='Province')
runs.reg <- merge(runs.reg, names.df[,c('CustomerSiteId','CensusRegionNational')], by='CustomerSiteId')
runs.reg.date <- merge(runs.reg, calendar.df[,c('Date','Year','Week','YearWeek')], by='Date')
cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-')))
# cdc.reg.df <- data.frame(YearWeek = cdc.reg.df$YearWeek, Region = cdc.reg.df$REGION, iliRate = cdc.reg.df$ILITOTAL/cdc.reg.df$TOTAL.PATIENTS)
cdc.flu.df$YearWeek <- with(cdc.flu.df, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-')))
cdc.flu.df <- data.frame(YearWeek = cdc.flu.df$YearWeek, hhsRegion = as.numeric(substring(cdc.flu.df$REGION, 8, 10)), TotalObs = cdc.flu.df$TOTAL.SPECIMENS, aH1N109 = cdc.flu.df$A..2009.H1N1., aH1 = cdc.flu.df$A..H1., aH3 = cdc.flu.df$A..H3., aNoSubtype = cdc.flu.df$A..Subtyping.not.Performed., aUnableToSubtype = cdc.flu.df$A..Unable.to.Subtype., b = cdc.flu.df$B, H3N2v = cdc.flu.df$H3N2v)
cdc.ili.df$YearWeek <- with(cdc.ili.df, ifelse(WEEK < 10, paste(YEAR, WEEK, sep='-0'), paste(YEAR, WEEK, sep='-')))
cdc.ili.df <- data.frame(YearWeek = cdc.ili.df$YearWeek, hhsRegion = as.numeric(substring(cdc.ili.df$REGION, 8, 10)), iliRate = cdc.ili.df$ILITOTAL/cdc.ili.df$TOTAL.PATIENTS)
goog.flu.df <- merge(calendar.df, goog.flu.df, by='Date')

# clean up the data frame that holds the cdc regional data
cdc.reg.df <- data.frame(Year = cdc.reg.df$YEAR, Week = cdc.reg.df$WEEK, Region = cdc.reg.df$REGION, ILITotal = cdc.reg.df$ILITOTAL, TotalPatients = cdc.reg.df$TOTAL.PATIENTS)
cdc.reg.df$YearWeek <- with(cdc.reg.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
cdc.reg.count.df <- do.call(rbind, lapply(1:length(unique(cdc.reg.df$Region)), function(x)  data.frame(YearWeek =  cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'][2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x], 'YearWeek'])-1)], Region = unique(cdc.reg.df$Region)[x], TotalPatients = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'TotalPatients'])), ILITotal = sapply(2:(length(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],'YearWeek'])-1), function(y) sum(cdc.reg.df[cdc.reg.df$Region == unique(cdc.reg.df$Region)[x],][(y-1):(y+1), 'ILITotal'])))))
cdc.reg.count.df$Rate <- with(cdc.reg.count.df, ILITotal/TotalPatients)
cdc.trend.rate <- merge(unique(regions.df[,c('CensusRegionLocal','CensusRegionNational')]), cdc.reg.count.df, by.y='Region', by.x='CensusRegionLocal') # merge(unique(runs.reg.date[,c('CustomerSiteId','Region')]), cdc.reg.count.df, by='Region')
cdc.trend.rate.reg <- with(cdc.trend.rate, aggregate(Rate~YearWeek+CensusRegionNational, FUN=mean)) # with(cdc.trend.rate, aggregate(Rate~YearWeek+Region, FUN=mean))
cdc.trend.rate.nat <- with(cdc.trend.rate, aggregate(Rate~YearWeek, FUN=mean))

# ------------------------------------------ ANALYSIS -----------------------------------------------------------------------------
sites <- as.character(unique(runs.reg.date$CustomerSiteId))
sites <- sites[order(as.numeric(sites))]
site.turn <- do.call(rbind, lapply(1:length(sites), function(x) turn(runs.reg.date, 'CustomerSiteId', sites[x], 'RP', calendar.df, 30)))
regn.turn <- merge(site.turn[,c('CustomerSiteId','YearWeek','TURN')], names.df[,c('CustomerSiteId','CensusRegionNational')], by='CustomerSiteId') # merge(site.turn[,c('CustomerSiteId','YearWeek','TURN')], unique(runs.reg[,c('CustomerSiteId','Region')]), by='CustomerSiteId')
regn.turn <- with(regn.turn, aggregate(TURN~YearWeek+CensusRegionNational, FUN=mean)) # with(regn.turn, aggregate(TURN~YearWeek+Region, FUN=mean))
natn.turn <- with(site.turn, aggregate(TURN~YearWeek, FUN=mean))

# Make a chart showing TURN and ILI overlaid (national)
dateBreaks <- c('2013-28','2013-40','2014-01', '2014-14','2014-27','2014-40','2015-01', '2015-14','2015-27','2015-40','2016-01','2016-14','2016-27','2016-40','2017-01','2017-14','2017-27','2017-40')
dateLabels <- c('Jul-2013','-','Jan-2014','-','Jul-2014','-','Jan-2015','-','Jul-2015','-','Jan-2016','-','Jul-2016','-','Jan-2017','-','Jul-2017','-')

ggplot(site.turn, aes(x=YearWeek, y=TURN, group='SiteTURN', color='SiteTURN')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=TURN, group='NationalTURN', color='NationalTURN'), data=natn.turn, size=1.5) + scale_color_manual(values=c('black','blue')) + facet_wrap(~CustomerSiteId, scale='free_y') + labs(title='TURN_v3') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + theme(axis.text.x=element_text(angle=90, hjust=1))
ggplot(site.turn, aes(x=YearWeek, y=TURN, group=CustomerSiteId, color=CustomerSiteId)) + geom_line(size=1.5) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + theme(axis.text.x=element_text(angle=90, hjust=1)) + scale_color_manual(values=createPaletteOfVariableLength(site.turn, 'CustomerSiteId'))

if(FALSE) {
####################################
# WORK ON A WAY TO HANDLE DATA WHERE THE DERIVATIVE OF DETECTION IS ABNORMAL (ESD)... AS MAY OCCUR AT SITES WITH BIG TURN SPIKES???
# COULD BE INCLUDED INTO TURN_omega.R
a <- subset(site.turn, CustomerSiteId==38)
a$Detection <- with(a, Positives/Runs)
a[2:(nrow(a)-1),'dD'] <- sapply(2:(nrow(a)-1), function(x) (a[(x+1),'Detection']-a[(x-1),'Detection'])/2)
b <- a[!(is.nan(a$dD)) & !(is.na(a$dD)), 'dD']
# hist(b, 30) # the distribution is approximately normal with some outliers
# ESD test - one iteration
# b <- c(-0.25, 0.68, 0.94, 1.15, 1.20, 1.26, 1.26, 1.34, 1.38, 1.43, 1.49, 1.49, 1.55, 1.56, 1.58, 1.65, 1.69, 1.70, 1.76, 1.77, 1.81, 1.91, 1.94, 1.96, 1.99, 2.06, 2.09, 2.10, 2.14, 2.15, 2.23, 2.24, 2.26, 2.35, 2.37, 2.40, 2.47, 2.54, 2.62, 2.64, 2.90, 2.92, 2.92, 2.93, 3.21, 3.26, 3.30, 3.59, 3.68, 4.30, 4.64, 5.34, 5.42, 6.01)
esd.test <- c()
n <- length(b)
# s <- sd(b)
# xbar <- mean(b)
alpha <- 0.05
for(i in 1:floor(sqrt(n))) {
  
  s <- sd(b)
  xbar <- mean(b)
  R <- max(abs(b-xbar))/s
  p <- 1 - alpha/(2*(n-i+1))
  v <- n - i - 1
  t <- qt(p = p, df = v)
  lambda <- (n-i)*t/sqrt((n-i-1+t^2)*(n-i+1))
  R > lambda # FIND THE MAX i FOR WHICH R > lambda... THAT IS THE NUMBER OF OUTLIERS IN THE DATA SET
  temp <- data.frame(Outliers = i, testValue = R, criticalValue = lambda)
  esd.test <- rbind(esd.test, temp)
  
  # adjust b to remove the observation
  remove.index <- which(abs(b-mean(b)) == max(abs(b-mean(b))))
  b <- b[!(abs(b-mean(b)) == max(abs(b-mean(b))))]
}
####################################
}

# Add TURN and ILI together so that there's national and regional data for both
ili.turn.mrg <- merge(regn.turn, cdc.trend.rate.reg, by=c('YearWeek','CensusRegionNational'), all.x=TRUE)
ili.turn.mrg.nat <- merge(natn.turn, cdc.trend.rate.nat, by=c('YearWeek'), all.x=TRUE)

# regional and national plots of ILI vs. TURN for summer 2013 - present
p.RegionalOverlayOfILIandTURN <- ggplot(ili.turn.mrg, aes(x=YearWeek, y=Rate, color='ILI (CDC)', group='ILI (CDC)')) + geom_line(data=ili.turn.mrg, aes(x=YearWeek, y=TURN/50, color='TURN', group='TURN'), lwd=1.5) + geom_line(lwd=1.5) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.08), breaks=c(0, 0.02, 0.04, 0.06, 0.08), labels=c(0, 2, 4, 6, 8), sec.axis = sec_axis(~.*50, name='TURN')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%)', x='Date') + scale_color_manual(values=c('black','red'), name='') + facet_wrap(~CensusRegionNational)
p.NationalOverlayOfILIandTURN <- ggplot(ili.turn.mrg.nat, aes(x=YearWeek, y=Rate, color='ILI (CDC)', group='ILI (CDC)')) + geom_line(data=ili.turn.mrg.nat, aes(x=YearWeek, y=TURN/50, color='TURN', group='TURN'), lwd=1.5) + geom_line(lwd=1.5) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.06), breaks=c(0, 0.02, 0.04, 0.06), labels=c(0, 2, 4, 6), sec.axis = sec_axis(~.*50, name='TURN')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%)', x='Date') + scale_color_manual(values=c('black','red'), name='')

if(FALSE) {
# grob method
p1 <- ggplot(ili.turn.mrg, aes(x=YearWeek, y=Rate, color='ILI (CDC)', group='ILI (CDC)')) + geom_line(data=ili.turn.mrg, aes(x=YearWeek, y=TURN/100, color='TURN', group='TURN'), lwd=1.5) + geom_line(lwd=1.5) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.08), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), labels=c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%)', x='Date') + scale_color_manual(values=c('black','red'), name='') + facet_wrap(~CensusRegionNational)
p2 <- ggplot(ili.turn.mrg, aes(x=YearWeek, y=TURN/100, color='TURN', group='TURN'), lwd=1.5) + geom_line(color='red') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.08), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), labels=c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks = dateBreaks[5:length(dateBreaks)], labels = dateBreaks[5:length(dateBreaks)]) + labs(y='') + facet_wrap(~CensusRegionNational)
  
## Putting plots together ##################
# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, grepl("panel",name) , se = t:r))
g <- gtable_add_grob(g1, g2$grobs[grep("panel",g2$layout$name)], pp$t, pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(grepl("axis_l",g2$layout$name) | grepl("axis-l",g2$layout$name))
ga <- g2$grobs[ia]

axis_idx <- as.numeric(which(sapply(ga,function(x) !is.null(x$children$axis))))

for(i in 1:length(axis_idx)){
  ax <- ga[[axis_idx[i]]]$children$axis
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia[axis_idx[i]], ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t[axis_idx[i]], length(g$widths) - i, pp$b[axis_idx[i]])
}
g$grobs[[which(g$layout$name =='ylab-r')]] <- g2$grobs[[which(g2$layout$name =='ylab-l')]]

# Plot!
grid.newpage()
png('Figures/RegionalOverlayOfILIandTURN.png', height=800, width=1400)
grid.draw(g)
dev.off()
}

if(FALSE) {
p1 <- ggplot(ili.turn.mrg.nat, aes(x=YearWeek, y=Rate, color='ILI (CDC)', group='ILI (CDC)')) + geom_line(data=ili.turn.mrg.nat, aes(x=YearWeek, y=TURN/100, color='TURN', group='TURN'), lwd=1.5) + geom_line(lwd=1.5) + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.08), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), labels=c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(color='transparent', fill='white'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + labs(y='ILI (%)', x='Date') + scale_color_manual(values=c('black','red'), name='')
p2 <- ggplot(ili.turn.mrg.nat, aes(x=YearWeek, y=TURN/100, color='TURN', group='TURN'), lwd=1.5) + geom_line(color='red') + scale_x_discrete(breaks = dateBreaks, labels = dateLabels) + scale_y_continuous(limits=c(0, 0.08), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08), labels=c(0, 1, 2, 3, 4, 5, 6, 7, 8)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1), legend.position='bottom', panel.background=element_rect(fill='transparent', color='transparent'), panel.grid=element_blank(), axis.ticks.x=element_blank()) + scale_x_discrete(breaks = dateBreaks[5:length(dateBreaks)], labels = dateBreaks[5:length(dateBreaks)]) + labs(y='')

## Putting plots together ##################
# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, grepl("panel",name) , se = t:r))
g <- gtable_add_grob(g1, g2$grobs[grep("panel",g2$layout$name)], pp$t, pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(grepl("axis_l",g2$layout$name) | grepl("axis-l",g2$layout$name))
ga <- g2$grobs[ia]

axis_idx <- as.numeric(which(sapply(ga,function(x) !is.null(x$children$axis))))

for(i in 1:length(axis_idx)){
  ax <- ga[[axis_idx[i]]]$children$axis
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia[axis_idx[i]], ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t[axis_idx[i]], length(g$widths) - i, pp$b[axis_idx[i]])
}
g$grobs[[which(g$layout$name =='ylab-r')]] <- g2$grobs[[which(g2$layout$name =='ylab-l')]] # add the axis label from p2 to the right side of the plot
# g$layout$r[[which(g$layout$name =='background')]] <- g$layout$r[[which(g$layout$name =='background')]] + 1
# g$layout$l[[which(g$layout$name =='ylab-r')]] <- g$layout$l[[which(g$layout$name =='ylab-r')]] + 3
# g$layout$r[[which(g$layout$name =='ylab-r')]] <- g$layout$r[[which(g$layout$name =='ylab-r')]] + 3

# Plot!
grid.newpage()
png('Figures/NationalOverlayOfILIandTURN.png', height=800, width=1400)
grid.draw(g)
dev.off()
}

# Laffy taffy
bugs.df <- merge(bugs.df, runs.reg.date[,c('RunDataId','YearWeek','CustomerSiteId')], by='RunDataId')
bugs.base <- expand.grid(YearWeek = unique(calendar.df$YearWeek), CustomerSiteId = unique(bugs.df$CustomerSiteId), Target = unique(bugs.df$Target))
bugs.agg <- with(bugs.df, aggregate(Positive~YearWeek+CustomerSiteId+Target, FUN=sum))
site.bugs <- merge(bugs.base, bugs.agg, by=c('YearWeek','CustomerSiteId','Target'), all.x=TRUE)
site.bugs[is.na(site.bugs$Positive), 'Positive'] <- 0
site.bugs <- site.bugs[with(site.bugs, order(CustomerSiteId, YearWeek, Target)),]
bugs <- as.character(unique(site.bugs$Target))
year.weeks <- as.character(unique(site.bugs$YearWeek))
site.bugs.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(1:length(bugs), function(y) do.call(rbind, lapply(2:(length(year.weeks)-1), function(z) data.frame(YearWeek = year.weeks[z], CustomerSiteId = sites[x], Target = bugs[y], Positives = sum(site.bugs[site.bugs$CustomerSiteId==sites[x] & site.bugs$Target==bugs[y], 'Positive'][(z-1):(z+1)]))))))))
site.prev <- merge(site.bugs.roll, site.turn[,c('YearWeek','CustomerSiteId','Runs')], by=c('YearWeek','CustomerSiteId'))
site.prev <- merge(site.prev, abvs.df, by.x='Target', by.y='Organism')
site.prev$Detection <- with(site.prev, Positives/Runs)
site.prev[site.prev$Runs < 30, 'Detection'] <- NA
regn.prev <- merge(site.prev, names.df[,c('CustomerSiteId','CensusRegionNational')], by='CustomerSiteId')
regn.prev <- with(regn.prev, aggregate(Detection~YearWeek+CensusRegionNational+Target+ShortName, FUN=mean))
natn.prev <- with(site.prev, aggregate(Detection~YearWeek+Target+ShortName, FUN=mean))
taffy.pal <- createPaletteOfVariableLength(regn.prev, 'ShortName')
# reorder so that the pathogens that are relatively constant throughout the year are on the bottom
regn.prev$Name <- factor(regn.prev$ShortName, levels=c('RSV','PIV4','PIV3','PIV2','PIV1','hMPV','FluB','FluA H3','FluA H1-09','FluA H1','Flu A','CoV OC43','CoV NL63','CoV HKU1','CoV 229E','HRV/EV','M. pne','C. pne','B. per','Adeno'))
natn.prev$Name <- factor(natn.prev$ShortName, levels=c('RSV','PIV4','PIV3','PIV2','PIV1','hMPV','FluB','FluA H3','FluA H1-09','FluA H1','Flu A','CoV OC43','CoV NL63','CoV HKU1','CoV 229E','HRV/EV','M. pne','C. pne','B. per','Adeno'))
p.RegionalLaffyTaffy <- ggplot(regn.prev, aes(x=YearWeek)) + geom_area(aes(y=Detection, fill=Name, group=Name), stat='identity', position='stack') + facet_wrap(~CensusRegionNational) + scale_fill_manual(values=taffy.pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,1), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')
p.NationalLaffyTaffy <- ggplot(natn.prev, aes(x=YearWeek)) + geom_area(aes(y=Detection, fill=Name, group=Name), stat='identity', position='stack') + scale_fill_manual(values=taffy.pal, name='') + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,1), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')


# show differences between influenza, ILI, Google Flu, and TURN
colnames(goog.flu.df)[grep('Region', colnames(goog.flu.df))] <- 'hhsRegion'
goog.flu.df$Rate <- goog.flu.df$Rate/100
cdc.flu.df$TotalFlu <- sapply(1:nrow(cdc.flu.df), function(x) sum(cdc.flu.df[x,4:ncol(cdc.flu.df)]))
cdc.flu.df$fluRate <- with(cdc.flu.df, TotalFlu/TotalObs)
comp.df <- merge(merge(cdc.ili.df, cdc.flu.df, by=c('YearWeek','hhsRegion')), goog.flu.df, by=c('YearWeek','hhsRegion'))
hhs.turn <- merge(site.turn[,c('YearWeek','CustomerSiteId','TURN')], names.df[,c('CustomerSiteId','hhsRegion')], by='CustomerSiteId')
hhs.turn <- with(hhs.turn, aggregate(TURN~YearWeek+hhsRegion, FUN=mean))
comp.df <- merge(comp.df, hhs.turn, by=c('YearWeek','hhsRegion'), all.x=TRUE)
# ggplot(comp.df, aes(x=YearWeek, y=iliRate, group='ILI', color='ILI')) + geom_line() + geom_line(aes(x=YearWeek, y=fluRate/5, group='CDC Flu', color='CDC Flu'), data=comp.df) + geom_line(aes(x=YearWeek, y=Rate, group='Google Flu', color='Google Flu'), data=comp.df) +  geom_line(aes(x=YearWeek, y=TURN/100, group='TURN', color='TURN'), data=comp.df) + facet_wrap(~hhsRegion)
# comp.trim <- comp.df[comp.df$hhsRegion %in% c(1, 2, 4, 5, 7, 8), ]
comp.trim <- comp.df[comp.df$hhsRegion %in% c(2, 5), ]
# ggplot(comp.trim, aes(x=YearWeek, y=iliRate, group='ILI', color='ILI')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=fluRate/5, group='CDC Flu', color='CDC Flu'), data=comp.trim, lwd=1.5) + geom_line(aes(x=YearWeek, y=Rate, group='Google Flu', color='Google Flu'), data=comp.trim, lwd=1.5) +  geom_line(aes(x=YearWeek, y=TURN/100, group='TURN', color='TURN'), data=comp.trim, lwd=1.5) + facet_wrap(~hhsRegion) + scale_x_discrete(breaks=dateBreaks, labels=dateLabels) + scale_y_continuous(limits=c(0,0.1), labels=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1)) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + scale_color_manual(values=c('black','darkgreen','blue','red'), name='') + labs(y='Google Flu (%), ILI (%), TURN', x='') 
dateBreaks.alt <- c('2013-41','2014-01','2014-14','2014-27','2014-39')
dateLabels.alt <- c('Oct-2013','Jan-2014','Mar-2014','Jul-2014','Oct-2014')
p.TripleOverlay201314 <- ggplot(comp.trim, aes(x=YearWeek, y=iliRate, group='ILI', color='ILI')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=Rate, group='Google Flu', color='Google Flu'), data=comp.trim, lwd=1.5) +  geom_line(aes(x=YearWeek, y=TURN/50, group='TURN', color='TURN'), data=comp.trim, lwd=1.5) + facet_wrap(~hhsRegion, ncol=1) + scale_x_discrete(breaks=dateBreaks.alt, labels=dateLabels.alt) + scale_y_continuous(limits=c(0,0.05), labels=c(0, 1, 2, 3, 4, 5), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05), sec.axis=sec_axis(~.*50, name='TURN')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + scale_color_manual(values=c('blue','black','red'), name='') + labs(y='Google Flu (%), ILI (%)', x='')

# show the HRV/EV percent detection for 2013-2014 season by HHS region
ev.d68 <- merge(subset(site.prev, ShortName=='HRV/EV'), names.df[names.df$hhsRegion %in% c(2, 5),c('CustomerSiteId','hhsRegion')], by='CustomerSiteId')
ev.d68 <- with(ev.d68, aggregate(Detection~YearWeek+hhsRegion+ShortName, FUN=mean))
ev.d68$hhsRegion <- paste('HHS Region', ev.d68$hhsRegion, sep=' ')
comp.trim$hhsRegion <- paste('HHS Region', comp.trim$hhsRegion, sep=' ') 
ggplot(subset(ev.d68, as.character(YearWeek) >= '2013-41' & as.character(YearWeek) <= '2014-39'), aes(x=YearWeek)) + geom_area(aes(y=Detection, fill=ShortName, group=ShortName), stat='identity', position='stack') + geom_line(aes(x=YearWeek, y=TURN/3, group='TURN', color='TURN'), data=comp.trim, lwd=1.5) + geom_line(aes(x=YearWeek, y=18*iliRate, group='ILI', color='ILI'), data=comp.trim, lwd=1.5) + geom_line(aes(x=YearWeek, y=18*Rate, group='Google Flu', color='Google Flu'), data=comp.trim, lwd=1.5) + facet_wrap(~hhsRegion, ncol=1) + scale_fill_manual(values=taffy.pal, name='') + scale_color_manual(values=c('black','blue','red'), name='') + scale_x_discrete(breaks=dateBreaks.alt, labels=dateLabels.alt) + scale_y_continuous(limits=c(0,0.8), labels=c(0, 10, 20, 30, 40, 50, 60, 70, 80), breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), sec.axis = sec_axis(~.*3, name='TURN, Rescaled ILI and Google Flu')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank()) + guides(fill=guide_legend(ncol=7, bycol=TRUE)) + labs(title='', y='Detection (%)', x='Date')

# get the correlation between ILI and Google Flu vs. TURN... do this regionally and nationally
# regionally
regions <- unique(comp.trim$hhsRegion)
# cor.region <- do.call(rbind, lapply(1:length(regions), function(x) data.frame(Region = regions[x], Key = 'TURN vs ILI', Cor = cor(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate']))))
# cor.region <- rbind(cor.region, do.call(rbind, lapply(1:length(regions), function(x) data.frame(Region = regions[x], Key = 'TURN vs Google Flu', Cor = cor(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'])))))
ccf.region <- do.call(rbind, lapply(1:length(regions), function(x) data.frame(Region = regions[x], Key = 'TURN vs ILI', CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[1], 'iliRate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], plot = FALSE)$lag)[which(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], plot = FALSE)$lag)$CCF == max(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], plot = FALSE)$lag)$CCF)), ]))
ccf.region <- rbind(ccf.region, do.call(rbind, lapply(1:length(regions), function(x) data.frame(Region = regions[x], Key = 'TURN vs Google Flu', CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[1], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)[which(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)$CCF == max(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'TURN'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)$CCF)), ])))
ccf.region <- rbind(ccf.region, do.call(rbind, lapply(1:length(regions), function(x) data.frame(Region = regions[x], Key = 'ILI vs Google Flu', CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[1], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)[which(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)$CCF == max(data.frame(CCF = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$acf, Lag = ccf(comp.trim[comp.trim$hhsRegion==regions[x], 'iliRate'], comp.trim[comp.trim$hhsRegion==regions[x], 'Rate'], plot = FALSE)$lag)$CCF)), ])))

#nationally
comp.nat <- merge(ili.turn.mrg.nat, goog.flu.df[goog.flu.df$hhsRegion==0, c('YearWeek','Rate')], by='YearWeek')
colnames(comp.nat)[grep('Rate\\.', colnames(comp.nat))] <- c('iliRate','googRate')
p.TripleOverlayNational201314 <- ggplot(comp.nat, aes(x=YearWeek, y=iliRate, group='ILI', color='ILI')) + geom_line(lwd=1.5) + geom_line(aes(x=YearWeek, y=googRate, group='Google Flu', color='Google Flu'), data=comp.nat, lwd=1.5) + geom_line(aes(x=YearWeek, y=TURN/50, group='TURN', color='TURN'), data=comp.nat, lwd=1.5) + scale_color_manual(values=c('blue','black','red'), name='') + scale_x_discrete(breaks=dateBreaks.alt, labels=dateLabels.alt) + scale_y_continuous(limits=c(0,0.05), labels=c(0, 1, 2, 3, 4, 5), breaks=c(0, 0.01, 0.02, 0.03, 0.04, 0.05), sec.axis=sec_axis(~.*50, name='TURN')) + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'), axis.ticks.x=element_blank())
ccf.nat <- data.frame(Key = 'TURN vs ILI', CCF = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$lag)[which(data.frame(Key = 'TURN vs ILI', CCF = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$lag)$CCF==max(data.frame(Key = 'TURN vs ILI', CCF = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$iliRate, plot = FALSE)$lag)$CCF)), ]
ccf.nat <- rbind(ccf.nat, data.frame(Key = 'TURN vs Google Flu', CCF = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$lag)[which(data.frame(Key = 'TURN vs Google Flu', CCF = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$lag)$CCF==max(data.frame(Key = 'TURN vs Google Flu', CCF = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$TURN, comp.nat$googRate, plot = FALSE)$lag)$CCF)), ])
ccf.nat <- rbind(ccf.nat, data.frame(Key = 'ILI vs Google Flu', CCF = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$lag)[which(data.frame(Key = 'ILI vs Google Flu', CCF = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$lag)$CCF==max(data.frame(Key = 'ILI vs Google Flu', CCF = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$acf, Lag = ccf(comp.nat$iliRate, comp.nat$googRate, plot = FALSE)$lag)$CCF)), ])

# which organisms "drive" TURN... let's use the t-value from a linear regression model
# I think that I should group all Flu A together first...
site.prev.agg <- site.prev
site.prev.agg$ShortName <- as.character(site.prev.agg$ShortName)
site.prev.agg[grep('FluA|Flu A', site.prev.agg$ShortName), 'ShortName'] <- 'FluA'
site.prev.agg[grep('CoV', site.prev.agg$ShortName), 'ShortName'] <- 'CoV'
site.prev.agg[grep('PIV', site.prev.agg$ShortName), 'ShortName'] <- 'PIV'
site.prev.agg[grep('pne|per', site.prev.agg$ShortName), 'ShortName'] <- 'Bacteria'
site.prev.agg <- merge(with(site.prev.agg, aggregate(Runs~YearWeek+CustomerSiteId, FUN=mean)), with(site.prev.agg, aggregate(Positives~YearWeek+CustomerSiteId+ShortName, FUN=sum)), by=c('YearWeek','CustomerSiteId'))
site.prev.agg$Detection <- with(site.prev.agg, Positives/Runs)
site.prev.agg[site.prev.agg$Runs < 30, 'Detection'] <- NA
natn.prev.agg <- with(site.prev.agg, aggregate(Detection~YearWeek+ShortName, FUN=mean))
natn.prev.spread <- spread(natn.prev.agg[,c('YearWeek','ShortName','Detection')], key=ShortName, value=Detection)
decoder.df <- data.frame(ShortName = colnames(natn.prev.spread)[2:ncol(natn.prev.spread)], Code = letters[1:(ncol(natn.prev.spread)-1)])
colnames(natn.prev.spread)[2:ncol(natn.prev.spread)] <- letters[1:(ncol(natn.prev.spread)-1)]
organism.drivers.df <- merge(ili.turn.mrg.nat, natn.prev.spread, by='YearWeek')

# loop through combinations of organisms to find the best predictors of TURN... then rank contribution using t-value of the coefficients
best.fits <- c()
for (i in 3:length(decoder.df$Code)) {
  
  combos <- generateCombos(decoder.df$Code, i)
  fits.df <- c()
  for(j in 1:length(combos$Combo)) {
    
    fit.sum <- summary(lm(as.formula(paste('TURN', combos[j,'Combo'], sep='~')), data=organism.drivers.df))
    fit.df  <- data.frame(Vars = i, Combo = combos$Combo[j], RSE = fit.sum$sigma, R2 = fit.sum$adj.r.squared, 
               Fstat = fit.sum$fstatistic[1], pFstat = pf(fit.sum$fstatistic[1],fit.sum$fstatistic[2],fit.sum$fstatistic[3],lower=FALSE),
               Coefp05 = sum(fit.sum$coeff[2:(i+1),4] < 0.05), Coefp01 = sum(fit.sum$coeff[2:(i+1),4] < 0.01), Coefp001 = sum(fit.sum$coeff[2:(i+1),4] < 0.001))
    fits.df <- rbind(fits.df, fit.df)
  }
  
  best.fit <- fits.df[fits.df$Fstat == max(fits.df$Fstat), ]
  best.fits <- rbind(best.fits, best.fit)
}

# use the below to (editing the lm call to use the correct 3 organisms as identified in best.fits) 
# to make table of t-value, resdiual standard error, etc. (see excel file for this table)
ggplot(best.fits, aes(x=Vars, y=RSE, group='Residual Standard Error')) + geom_point(size=5, color='red') + labs(x='Variables in Model', y='Residual Standard Error') + theme(plot.title=element_text(hjust=0.5),text=element_text(size=22, face='bold'), axis.text=element_text(size=22, color='black', face='bold'), axis.text.x=element_text(hjust=1, vjust=0.5), legend.position='bottom', panel.background=element_rect(color='white', fill='white'))
summary(lm(TURN~d+g+i, data=organism.drivers.df))

# PRINT OUT ALL THE FIGURES
plots <- ls()[grep('^p\\.',ls())]
for(i in 1:length(plots)) {
  
  imgName <- paste(substring(plots[i],3),'.png',sep='')
  png(file=paste('Figures', imgName, sep='/'), width=1200, height=800, units='px')
  print(eval(parse(text = plots[i])))
  dev.off()
}

if(FALSE) {
# make an aggregate count of negative runs by period, customer, and region
neg.runs <- with(runs.reg.date[runs.reg.date$Panel=='RP' & runs.reg.date$Positive==0, ], aggregate(Run~YearWeek+CustomerSiteId, FUN=sum))
base.df <- expand.grid(YearWeek = unique(neg.runs$YearWeek), CustomerSiteId = unique(neg.runs$CustomerSiteId))
neg.runs <- merge(base.df, neg.runs, by=c('YearWeek','CustomerSiteId'), all.x=TRUE)
neg.runs[is.na(neg.runs$Run),'Run'] <- 0
neg.runs <- neg.runs[with(neg.runs, order(CustomerSiteId, YearWeek)), ]
year.weeks <- as.character(unique(base.df$YearWeek))[order(as.character(unique(base.df$YearWeek)))]
neg.roll <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(2:(length(year.weeks)), function(y) data.frame(CustomerSiteId = sites[x], YearWeek = year.weeks[y], Negatives = sum(neg.runs[neg.runs$CustomerSiteId==sites[x],'Run'][(y-1):(y+1)]))))))
neg.roll <- merge(neg.roll, names.df[,c('CustomerSiteId','CensusRegionNational','CensusRegionLocal')], by='CustomerSiteId')
site.overview <- merge(site.turn[,c('CustomerSiteId','YearWeek','SpecRuns','SpecPositives','TURN')], neg.roll, by=c('CustomerSiteId','YearWeek'))
colnames(site.overview)[3:4] <- c('Runs','Positives')
site.overview$PositiveRate <- with(site.overview, Positives/Runs)
site.overview$NegativeRate <- with(site.overview, Negatives/Runs)
# Rotavirus isn't RP, so get rid of this...
# # check for Rotavirus at children's hospitals
# peds.sites <- unique(c(names.df[grep('Child', names.df$Name), 'CustomerSiteId'], names.df[grep('Child', names.df$Note), 'CustomerSiteId']))
# peds.overview <- site.overview[site.overview$CustomerSiteId %in% peds.sites, ]
# rota.nat.nrevss <- read.csv('../DataSources/NREVSS/Rotavirus_National.csv', header=TRUE)
# rota.nat.nrevss$Date <- as.Date(as.character(rota.nat.nrevss$Date), format = '%m/%d/%Y')
# rota.nat.nrevss <- merge(rota.nat.nrevss, calendar.df, by='Date') 
# ggplot(with(subset(peds.overview, as.character(YearWeek) >= '2014-01'), aggregate(NegativeRate~YearWeek, FUN=mean)), aes(x=YearWeek, y=NegativeRate)) + geom_point() + scale_x_discrete(breaks = unique(peds.overview$YearWeek)[seq(1, length(unique(peds.overview$YearWeek)), 12)])

# add in Google Flu
}
