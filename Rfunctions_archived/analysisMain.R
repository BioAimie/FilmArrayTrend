# set the path and load libraries and data
workDir <-'G:/Departments/PostMarket/DataScienceGroup/Data Science Products/InProcess/Aimie/20160425_DataAnalysisForPreliminaryTrendPublication/Master/'
setwd(workDir)

library(RODBC)
library(lubridate)
library(car)
library(ggplot2)
library(GGally)
library(scales)

# read in the data from FilmArray Data Warehouse DB (ODBC object in Windows "FA_DW" with Lindsay's credentials)
FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'lmeyers', pwd = 'Idaho1Tech')
queryVector <- scan('calendarDates.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
calendar.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('rpRuns.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('bugs.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
bugs.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('dualDetections.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
dual.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('rpRunsByInstrumentAndSite.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
instRuns.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# read in data from the CDC and also make a data frame that makes it easy to join CDC regional data with run data
cdc.df <- read.csv('cdcRegionData.csv', header = TRUE, sep= ',')
cdc.df <- data.frame(Year = cdc.df[,'YEAR'], Week = cdc.df[,'WEEK'], Region = cdc.df[,'REGION'], iliTotal = cdc.df[,'ILITOTAL'], totalPatients = cdc.df[,'TOTAL.PATIENTS'])
cdc.df[,'iliRate'] <- with(cdc.df, iliTotal/totalPatients)
region.key <- data.frame(CustomerSiteId = c(5, 2, 7, 13, 9), Region = c('Region 2', 'Region 5', 'Region 4', 'Region 5', 'Region 5'))

# load user-defined functions that are necessary for the analysis
source('makeEvenWeeks.R')
source('normalizeRunsAndOverlay.R')
source('integrateToFindSeason.R')
source('reformatBugsForAnova.R')
source('normalizeRunsAndPositivity.R')
source('formatForBugBarsWithOverlay.R')
source('runFitsOnNormalizedData.R')
source('installBaseRunsAdjuster.R')

# ANALYSIS 1: Show that the CDC data and the normalized run rates are good matches... also try with positive run normalization. Note that the nation 
#             overlay only includes regions that contain sites that are incldued in the analysis.
calendar.df <- makeEvenWeeks(calendar.df)
runs.df <- merge(runs.df, calendar.df, by = c('Date'))
nation.overlay <- normalizeRunsAndOverlay(runs.df, calendar.df, cdc.df, region.key, TRUE, TRUE)
site.overlay <- normalizeRunsAndOverlay(runs.df, calendar.df, cdc.df, region.key, TRUE, FALSE)
# site.overlay.pa <- normalizeRunsAndOverlay(runs.df, calendar.df, cdc.df, region.key, FALSE, FALSE)
# nation.overlay.pa <- normalizeRunsAndOverlay(runs.df, calendar.df, cdc.df, region.key, FALSE, TRUE)

# # take a look at the site and national overlays, both by run normalization and positive test normalization
p.site.nrili <- ggplot(site.overlay, aes(x=NormRunRate, y=iliRate, color=as.factor(Year))) + geom_point() + facet_wrap(~SiteName) + labs(title='ILI Rate vs. Normalized Run Rate by Site')
p.nat.nrili <- ggplot(nation.overlay, aes(x=NormRunRate, y=iliRate, color=as.factor(Year))) + geom_point() + labs(title='ILI Rate vs. Normalized Run Rate Nationwide')
# # p.site.npili <- ggplot(site.overlay.pa, aes(x=NormRunRate, y=iliRate, color=as.factor(Year))) + geom_point() + facet_wrap(~SiteName) + labs(title='ILI Rate vs. Normalized Positve Run Rate by Site')
# # p.nat.npili <- ggplot(nation.overlay.pa, aes(x=NormRunRate, y=iliRate, color=as.factor(Year))) + geom_point() + labs(title='ILI Rate vs. Normalized Positive Run Rate Nationwide')
# 
# # make a chart
nation.overlay$iliRate100 <- nation.overlay$iliRate*100
p.overlay <- ggplot(nation.overlay, aes(x=YearWeek, y=NormRunRate, color='FilmArray RP', group='FilmArray RP')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=iliRate100, group='CDC ILI', color='CDC ILI'), size=1.5) + scale_color_manual(values=c('black','red'), name='') + scale_x_discrete(breaks=as.character(unique(nation.overlay$YearWeek))[order(as.character(unique(nation.overlay$YearWeek)))][seq(1, length(as.character(unique(nation.overlay$YearWeek))), 8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text=element_text(color='black'), axis.text.x=element_text(angle=90)) + labs(title='Weekly CDC ILI vs RP Test Normalized Run Rate', y='RP Run Rate, ILI Cases per 100 Patients')
# 
# # -------------------------------------------------------------------------------------------------------------------------------------------------
# # attempt to run some fits on the normalized run rate vs. ili rate by year... but just do this nationally for now
# r <- nation.overlay[nation.overlay$Year < 2016, ]
# s <- data.frame(Year = as.character(unique(r$Year)),
#                 expDenom = sapply(1:length(as.character(unique(r$Year))), function(x) max(r[as.character(r$Year) == as.character(unique(r$Year))[x], 'NormRunRate'])))
# r <- merge(r, s, by='Year')
# fit.r.13 <- lm(iliRate~NormRunRate, data = r[r$Year == 2013, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.13)
# fit.r.14 <- lm(iliRate~NormRunRate, data = r[r$Year == 2014, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.14)
# fit.r.15 <- lm(iliRate~NormRunRate, data = r[r$Year == 2015, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.15)
# 
# # it appears there is an underlying exponential trend in the data set so attempt to change the fit to iliRate as some function of exp(NormRunRate/max(NormRunRate)) by year
# fit.r.13.e <- lm(iliRate~exp(NormRunRate/expDenom), data = r[r$Year == 2013, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.13.e)
# fit.r.14.e <- lm(iliRate~exp(NormRunRate/expDenom), data = r[r$Year == 2014, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.14.e)
# fit.r.15.e <- lm(iliRate~exp(NormRunRate/expDenom), data = r[r$Year == 2015, ])
# plot.new()
# par(mfrow=c(2,2))
# plot(fit.r.15.e)

# ---------------------------------------------------------------------------------------------------------------------------------------------------
# the national ili rate vs. normalized rate is difficult in 2015, because of abnormalities in run data from South Bend and Winthrop... additional work
# should be conducted to remove noise due to addition of instruments... show this in a chart.
# ---------------------------------------------------------------------------------------------------------------------------------------------------
instRuns.df <- merge(instRuns.df, calendar.df, by='Date')
instRuns.df <- instRuns.df[with(instRuns.df, order(CustomerSiteId, InstrumentSerialNumber)), ]
keepSites <- as.character(unique(site.overlay$CustomerSiteId))
myPal <- colorRampPalette(c('red','blue','orange','green','grey','yellow','purple','cyan','magenta','chocolate3','mediumseagreen','pink','dodgerblue','gold','darkgreen','violet','lightskyblue','orchid','midnightblue','darkolivegreen','indianred','mistyrose','thistle','tan','orangered','slategrey','brown','darkred'))(length(unique(instRuns.df$InstrumentSerialNumber)))
names(myPal) <- factor(unique(instRuns.df$InstrumentSerialNumber), levels = unique(instRuns.df$InstrumentSerialNumber))
p.instRuns.grid <- ggplot(subset(instRuns.df, CustomerSiteId %in% keepSites), aes(x=YearWeek)) + geom_bar(aes(y=Runs, fill=InstrumentSerialNumber), stat='identity') + facet_wrap(~SiteName,ncol=1) + scale_fill_manual(values=myPal, guide=FALSE) + scale_x_discrete(breaks = as.character(unique(instRuns.df$YearWeek))[order(as.character(unique(instRuns.df$YearWeek)))][seq(1, length(as.character(unique(instRuns.df$YearWeek))),8)]) + theme(text=element_text(size=20, face='bold', color='black'), axis.text=element_text(color='black'), axis.text.x=element_text(angle=90)) + labs(title='RP Test Runs by Institution\nas Colored by Instrument Serial Number', x='Year-Week', y='Run Count')

# -------------------------------------------------------------------------------------------------------------------------------------------------
# ANALYSIS 2: Reformat the data so that there is a column for each bug next to the run info from each site and then fit the ili rate data by the 
#             prevalence of organisms in FilmArray runs. Determine the best fit by running ANOVA analyses.
bugs.df <- merge(bugs.df, runs.df[runs.df$CustomerSiteId %in% keepSites ,c('RunDataId','SiteName','Year','Week','YearWeek')], by=c('RunDataId'))
bugs.anova <- reformatBugsForAnova(runs.df, bugs.df, calendar.df)
bug.names <- as.character(unique(bugs.df[,'Bug']))
colnames(bugs.anova)[colnames(bugs.anova) %in% bug.names] <- letters[1:length(bug.names)]
decoder <- data.frame(bugName = bug.names, bugId = letters[1:length(bug.names)])
p.RunsBySite <- ggplot(bugs.anova, aes(x=Week, y=Runs, group=Year, color=as.factor(Year))) + geom_line() + geom_point() + facet_wrap(~SiteName, ncol=1)

# Run the CDC data through an algorithm to determine the ILI season (just so the charts can be colored by that). Then, join the CDC ILI rate onto the 
# bugs.anova frame by year, week, and site. A fit can then be applied between the ILI rate and the percent positivity of each organism in FilmArray tests.
# NATIONAL FIT!!!
cdc.seasons <- integrateToFindSeason(cdc.df, keepSites, c(2013, 2014, 2015), region.key, TRUE)
# cdc.seasons <- integrateToFindSeason(cdc.df, keepSites, c(2013, 2014, 2015, 2016), region.key, TRUE)
ili.nat <- merge(cdc.seasons[,c('Year','Week','iliSeason','iliRate')], nation.overlay[,c('YearWeek','Year','Week','NormRunRate')], by=c('Year','Week'))
bugs.anova.nat <- with(bugs.anova[bugs.anova$CustomerSiteId %in% keepSites & bugs.anova$Year %in% c(2013, 2014, 2015), ], aggregate(cbind(Runs, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)~YearWeek, FUN=sum))
master.nat <- merge(ili.nat, bugs.anova.nat, by=c('YearWeek'))
master.nat[,8:28] <- master.nat[,8:28]/master.nat$Runs
# IT IS VERY IMPORTANT TO NOTE THAT THE NormRuns COLUMN IS ACTUALLY ILI RATE!!!!-------------------------------------
colnames(master.nat)[grep('iliRate', colnames(master.nat))] <- 'NormRuns' 
nat.fits <- runFitsOnNormalizedData(master.nat, decoder, TRUE, TRUE, TRUE)
bug.overlay <- formatForBugBarsWithOverlay(master.nat, decoder)

# Make a chart that shows the organism positivity overlaid with the ILI Rate and colored by in or out of season
p.organism.thumbs <- ggplot(bug.overlay, aes(x=YearWeek, y=Positivity, color=iliSeason)) + geom_point() + geom_line(aes(x=YearWeek, y=iliRate, group=1), data=bug.overlay, color='black') + scale_x_discrete(breaks = as.character(unique(bug.overlay$YearWeek))[seq(1, length(as.character(unique(bug.overlay$YearWeek))),8)]) + theme(legend.position = 'bottom', axis.text.x=element_text(angle=90)) + labs(title='Organism Positivity overlaid with CDC ILI Rate x 10', x='Year-Week', y='Positivity') + facet_wrap(~Key, scales='free_y') + scale_color_manual(values=c('grey','lightskyblue'))

# using the national fit, make a fit that's not by year, just by bug
nat.fit.all <- runFitsOnNormalizedData(master.nat, decoder, TRUE, FALSE, TRUE)
str.all <- paste('NormRuns', paste(as.character(nat.fit.all$bugId), collapse='+'), sep='~')
fit.all <- lm(as.formula(str.all), data = master.nat)
master.nat[,'allFit'] <- fitted(fit.all)

# keep only viruses
noRhino <- c('Human Rhinovirus/Enterovirus')
pneumons <- bug.names[grep('pneumoniae', bug.names)]
bacteria <- c(pneumons, 'Bordetella pertussis')
justVirus <- c('Human Rhinovirus/Enterovirus', bacteria, 'Influenza A', 'Influenza A (no subtype detected)')
justVirus <- bug.names[!(bug.names %in% justVirus)]
justVirus <- justVirus[order(justVirus)]
virusAnova <- as.character(decoder[decoder$bugName %in% justVirus, 'bugId'])
bugs.anova.virus <- bugs.anova.nat[,c('YearWeek','Runs', virusAnova)]
master.nat.virus <- merge(ili.nat, bugs.anova.virus, by=c('YearWeek'))
master.nat.virus[,8:22] <- master.nat.virus[,8:22]/master.nat.virus$Runs

# IT IS VERY IMPORTANT TO NOTE THAT THE NormRuns COLUMN IS ACTUALLY ILI RATE!!!!-------------------------------------
colnames(master.nat.virus)[grep('iliRate', colnames(master.nat.virus))] <- 'NormRuns' 
nat.fits.virus <- runFitsOnNormalizedData(master.nat.virus, decoder, TRUE, FALSE, TRUE)
str.virus.all <- paste('NormRuns', paste(as.character(nat.fits.virus$bugId), collapse='+'), sep='~')
fit.virus.all <- lm(as.formula(str.virus.all), data = master.nat.virus)
master.nat.virus[,'allFit'] <- fitted(fit.virus.all)

# make some charts using the generalized fit-------------
# IT IS VERY IMPORTANT TO NOTE THAT THE NormRuns COLUMN IS ACTUALLY ILI RATE!!!!---------------
# this is an artifact of the runFitsOnNormalizedData function expecting a column named that, but it's now replaced by ILI Rate
p.genfitoverlay.line <- ggplot(master.nat, aes(x=YearWeek, y=NormRuns, color=iliSeason)) + geom_point() + geom_line(aes(x=YearWeek, y=allFit, group=1), color='grey', data=master.nat) + scale_x_discrete(breaks = as.character(unique(master.nat$YearWeek))[seq(1, length(as.character(unique(master.nat$YearWeek))),8)]) + theme(text=element_text(color='black', size=20), axis.text=element_text(color='black', size=16), axis.text.x=element_text(angle=90)) + labs(title='National ILI Cases per Observed Patients vs. Generalized Model Fit Prediction', x='Year-Week', y='National ILI Rate,\nRate Predicted by Organism Prevalence')
p.genfitoverlay.point <- ggplot(master.nat, aes(x=YearWeek, y=NormRuns)) + geom_point() + geom_point(aes(x=YearWeek, y=allFit), color='blue', data=master.nat, shape=2)+ scale_x_discrete(breaks = as.character(unique(master.nat$YearWeek))[seq(1, length(as.character(unique(master.nat$YearWeek))),8)]) + theme(text=element_text(color='black', size=20), axis.text=element_text(color='black', size=16), axis.text.x=element_text(angle=90)) + labs(title='National ILI Cases per Observed Patients vs. Generalized Model Fit Prediction', x='Year-Week', y='National ILI Rate,\nRate Predicted by Organism Prevalence')
p.genfitoverlay.point.virus <- ggplot(master.nat.virus, aes(x=YearWeek, y=NormRuns, color='Actual')) + geom_point() + geom_point(aes(x=YearWeek, y=allFit, color='Regression Model'), data=master.nat.virus, shape=2) + scale_color_manual(values=c('black','blue'), name='') + scale_x_discrete(breaks = as.character(unique(master.nat$YearWeek))[seq(1, length(as.character(unique(master.nat$YearWeek))),8)]) + theme(text=element_text(color='black', size=20), axis.text=element_text(color='black', size=16), axis.text.x=element_text(angle=90)) + labs(title='National ILI Cases per Observed Patients vs. Generalized Model Fit Prediction', x='Year-Week', y='National ILI Rate,\nRate Predicted by Virus Prevalence')

# Perform the same thing but by year and season....
cdc.seasons.reg <- integrateToFindSeason(cdc.df, keepSites, c(2013, 2014, 2015), region.key, FALSE)
cdc.seasons.reg <- merge(region.key[region.key$CustomerSiteId %in% keepSites, ], cdc.seasons.reg, by='Region')
ili.reg <- merge(cdc.seasons.reg[,c('Year','Week','CustomerSiteId','iliSeason','iliRate')], site.overlay[,c('YearWeek','Year','Week','CustomerSiteId','NormRunRate')], by=c('Year','Week','CustomerSiteId'))
bugs.anova.reg <- bugs.anova[,c('YearWeek','CustomerSiteId','Runs', virusAnova)]
master.reg <- merge(ili.reg, bugs.anova.reg, by=c('YearWeek','CustomerSiteId'))
master.reg[,9:23] <- master.reg[,9:23]/master.reg$Runs
# IT IS VERY IMPORTANT TO NOTE THAT THE NormRuns COLUMN IS ACTUALLY ILI RATE!!!!-------------------------------------
colnames(master.reg)[grep('iliRate', colnames(master.reg))] <- 'NormRuns' 
reg.fits <- runFitsOnNormalizedData(master.reg, decoder, TRUE, TRUE, FALSE) # THIS DOESN'T WORK YET!!


# -----------------------------------------------------------------------------------------------------------------------------------------------
# This is added to make a three week rolling average count centered about the current week and produce a nice chart for the poster
cols <- letters[1:21]
cols <- c('Runs',cols)
cdc.seasons <- integrateToFindSeason(cdc.df, keepSites, c(2013, 2014, 2015, 2016), region.key, TRUE)
ili.nat <- merge(cdc.seasons[,c('Year','Week','iliSeason','iliRate')], nation.overlay[,c('YearWeek','Year','Week','NormRunRate')], by=c('Year','Week'))
bugs.anova.nat <- with(bugs.anova[bugs.anova$CustomerSiteId %in% keepSites & bugs.anova$Year %in% c(2013, 2014, 2015, 2016), ], aggregate(cbind(Runs, a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)~YearWeek, FUN=sum))
master.nat <- merge(ili.nat, bugs.anova.nat, by=c('YearWeek'), all.y=TRUE)
master.nat <- master.nat[with(master.nat, order(as.character(YearWeek))), ]
master.nat.roll <- master.nat[,!(colnames(master.nat) %in% cols)]  
master.nat.roll <- master.nat.roll[with(master.nat.roll, order(as.character(YearWeek))), ]
# master.nat.adj <- c()
for(i in 1:length(cols)) {
  
  # print(cols[i])
  # print(as.character(decoder[decoder$bugId == cols[i], 'bugName']))
  roll <- sapply(2:(length(master.nat[,cols[i]])-1), function(x) sum(master.nat[(x-1):(x+1),cols[i]]))
  roll <- c(roll[1], roll, roll[length(roll)])
  master.nat.roll <- cbind(master.nat.roll, roll)
  colnames(master.nat.roll)[grep('roll', colnames(master.nat.roll))] <- cols[i]
}
master.nat.roll[,8:28] <- master.nat.roll[,8:28]/master.nat.roll$Runs
colnames(master.nat.roll)[grep('iliRate', colnames(master.nat.roll))] <- 'NormRuns'
bug.overlay <- formatForBugBarsWithOverlay(master.nat.roll, decoder)
bug.overlay$YearWeek <- factor(bug.overlay$YearWeek, levels = bug.overlay[with(bug.overlay, order(as.character(YearWeek))),'YearWeek'])

dateBreaks <- as.character(unique(bug.overlay$YearWeek))
myPal <- colorRampPalette(c('red','blue','orange','green','grey','yellow','purple','cyan','magenta','chocolate3','mediumseagreen','pink','dodgerblue','gold','darkgreen','violet','lightskyblue'))(15)
names(myPal) <- factor(justVirus, levels = justVirus[order(justVirus)])
p.area.justvirus <- ggplot(subset(bug.overlay, Key %in% justVirus), aes(YearWeek)) + geom_area(aes(y=Positivity, fill=Key, group=Key, order=Key), stat='identity', position='stack') + geom_line(aes(x=YearWeek, y=10*iliRate, group=1), data=bug.overlay, color='black', size=1.25) + scale_x_discrete(breaks = as.character(unique(bug.overlay$YearWeek))[seq(1, length(as.character(unique(bug.overlay$YearWeek))),8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text.x=element_text(angle=90), axis.text=element_text(color='black', face='bold')) + labs(title='RP Virus Prevalence (no HRV/Entero) overlaid with CDC ILI Rate', x='Year-Week', y='RP Prevalence, ILI Cases per 10 Patients') + scale_fill_manual(values=myPal, name='')
p.area.rhino <- ggplot(subset(bug.overlay, Key %in% noRhino), aes(YearWeek)) + geom_area(aes(y=Positivity, fill=Key, group=Key, order=Key), stat='identity', position='stack') + geom_line(aes(x=YearWeek, y=10*iliRate, group=1), data=bug.overlay, color='black', size=1.25) + scale_x_discrete(breaks = as.character(unique(bug.overlay$YearWeek))[seq(1, length(as.character(unique(bug.overlay$YearWeek))),8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text.x=element_text(angle=90), axis.text=element_text(color='black', face='bold')) + labs(title='RP HRV/Entero Prevalence overlaid with CDC ILI Rate', x='Year-Week', y='RP Prevalence, ILI Cases per 10 Patients') + scale_fill_manual(values=c('red'), name='')
p.area.bacteria <- ggplot(subset(bug.overlay, Key %in% bacteria), aes(YearWeek)) + geom_area(aes(y=Positivity, fill=Key, group=Key, order=Key), stat='identity', position='stack') + geom_line(aes(x=YearWeek, y=iliRate, group=1), data=bug.overlay, color='black', size=1.25) + scale_x_discrete(breaks = as.character(unique(bug.overlay$YearWeek))[seq(1, length(as.character(unique(bug.overlay$YearWeek))),8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text.x=element_text(angle=90), axis.text=element_text(color='black', face='bold')) + labs(title='RP Bacteria Prevalence overlaid with CDC ILI Rate', x='Year-Week', y='RP Prevalence, ILI Cases') +  scale_fill_manual(values=c('mediumseagreen','dodgerblue','coral2'), name='')
# -----------------------------------------------------------------------------------------------------------------------------------------------

# -------------------------------------------------------------------------------------------------------------------------------------------------
# ANALYSIS 3: Use Lindsay's dual detection data to find the confidence intervals for dual detections, using a binomial method both unadjusted and
#             with a Bonferroni adjustment to account for the multiplicity of observed detection (false discovery rate)
# Dual detection... non-adjusted
library(binom)
n <- mean(dual.df$N) # this is the number of samples
dual.df$p_lower <- binom.confint(dual.df$Xpredicted, n, conf.level=0.95, method='exact')[,5] #*n
dual.df$p_upper <- binom.confint(dual.df$Xpredicted, n, conf.level=0.95, method='exact')[,6] #*n
p.dual.unadjusted <- ggplot(dual.df, aes(x=Name, y=R)) + geom_point(color='blue') + geom_errorbar(aes(ymin=p_lower, ymax=p_upper), data=dual.df) + coord_flip()

# dual detection adjusted
p.adj <- (1-0.025/400)
dual.df$p_lower_adj <- binom.confint(dual.df$Xpredicted, n, conf.level=p.adj, method='exact')[,5] #*n
dual.df$p_upper_adj <- binom.confint(dual.df$Xpredicted, n, conf.level=p.adj, method='exact')[,6] #*n
dual.df$Name <- factor(dual.df$Name, levels = dual.df[with(dual.df, order(p_upper_adj, decreasing = TRUE)), 'Name'])
plot.dual <- dual.df[with(dual.df, order(p_upper_adj, decreasing = TRUE)), ]
plot.dual <- plot.dual[1:20, ]
plot.dual$Name <- factor(plot.dual$Name, levels = plot.dual[with(plot.dual, order(p_upper_adj)), 'Name'])
plot.dual$Color <- with(plot.dual, ifelse(R < p_lower_adj | R > p_upper_adj, 'Outside', 'Inside'))
p.dual <- ggplot(plot.dual, aes(x=Name, y=R, color=Color)) + geom_point(size=3) + scale_color_manual(values=c('blue','darkgreen'), guide=FALSE) + geom_errorbar(aes(ymin=p_lower_adj, ymax=p_upper_adj), data=plot.dual, color='black') + theme(axis.text.x=element_text(angle=90, hjust=1, size=14), text=element_text(size=20, color='black'), axis.text=element_text(color='black')) + labs(title='Dual Detection Normality (60 Day)', x='', y='Dual Detection Rate') + coord_flip() + scale_y_continuous(labels=percent)

# make a simple pareto of the top dual detections
cutOff <- dual.df$R[order(dual.df$R, decreasing = TRUE)][10]
dual.df$Name <- factor(dual.df$Name, levels = dual.df[with(dual.df, order(R, decreasing = TRUE)), 'Name'])
dual.pareto <- dual.df[dual.df[,'R'] >= cutOff, ]
p.dual.detection.pareto <- ggplot(dual.pareto, aes(x=Name, y=R)) + geom_bar(stat='identity', fill='red') + scale_y_continuous(labels=percent) + theme(text=element_text(size=20), axis.text=element_text(size=20, color='black'), axis.text.x=element_text(angle=70, size=14, hjust=1)) + labs(title='Top 10 Dual Detections as Percent of Tests (Last 60 Days)', x='', y='Dual Detection Rate')

# -------------------------------------------------------------------------------------------------------------------------------------------------
# WORK ON ADJUSTING THE EFFECT OF ADDITIONAL INSTRUMENTS CREATING NOISE
cdc.keep <- merge(cdc.df, region.key, by='Region')
nat.adj <- installBaseRunsAdjuster(instRuns.df, calendar.df, cdc.keep, makeNational = TRUE)
p.adj.rate <- ggplot(nat.adj, aes(x=YearWeek, y=adjRate, color='FilmArray RP', group='FilmArray RP')) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=iliRate*100, group='CDC ILI', color='CDC ILI'), size=1.5) + scale_color_manual(values=c('black','red'), name='') + scale_x_discrete(breaks=as.character(unique(nat.adj$YearWeek))[order(as.character(unique(nat.adj$YearWeek)))][seq(1, length(as.character(unique(nat.adj$YearWeek))), 8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text=element_text(color='black'), axis.text.x=element_text(angle=90)) + labs(title='Weekly CDC ILI vs RP Test Normalized Run Rate', y='RP Run Rate, ILI Cases per 100 Patients')
site.adj <- installBaseRunsAdjuster(instRuns.df, calendar.df, cdc.keep, makeNational = FALSE)
p.adj.rate.site <- ggplot(site.adj, aes(x=YearWeek, y=adjRate, color=SiteName, group=SiteName)) + geom_line(size=1.5) + geom_line(aes(x=YearWeek, y=iliRate*100, group='CDC ILI'), color='black', size=1.5) + scale_x_discrete(breaks=as.character(unique(nat.adj$YearWeek))[order(as.character(unique(nat.adj$YearWeek)))][seq(1, length(as.character(unique(nat.adj$YearWeek))), 8)]) + theme(text=element_text(size=20, color='black', face='bold'), axis.text=element_text(color='black'), axis.text.x=element_text(angle=90)) + facet_wrap(~SiteName, ncol=1) + labs(title='Weekly CDC ILI vs RP Test Normalized Run Rate', y='RP Run Rate, ILI Cases per 100 Patients') + scale_color_brewer(palette = 'Paired', guide=FALSE)


# make charts
if(TRUE) {
  
  # make a chart for each site with a regional overlay and put all of these into one chart frame  
  site.overlay <- site.overlay[with(site.overlay, order(YearWeek, CustomerSiteId)), ]
  x1 <- as.character(unique(site.overlay[site.overlay$CustomerSiteId == '2', 'YearWeek']))
  y1 <- site.overlay[site.overlay$CustomerSiteId == '2','iliRate']
  z1 <- site.overlay[site.overlay$CustomerSiteId == '2','NormRunRate']
  x2 <- as.character(unique(site.overlay[site.overlay$CustomerSiteId == '5', 'YearWeek']))
  y2 <- site.overlay[site.overlay$CustomerSiteId == '5','iliRate']
  z2 <- site.overlay[site.overlay$CustomerSiteId == '5','NormRunRate']
  x3 <- as.character(unique(site.overlay[site.overlay$CustomerSiteId == '7', 'YearWeek']))
  y3 <- site.overlay[site.overlay$CustomerSiteId == '7','iliRate']
  z3 <- site.overlay[site.overlay$CustomerSiteId == '7','NormRunRate']
  plot.new() #-------------------
  par(mfrow=c(3,1))
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(as.factor(x1), y1, col='blue')
  lines(as.factor(x1),y1, col='blue')
  mtext("ILI Case Rate", side=2, line=2.5, col='blue');
  axis(2, col.axis='blue'); par(new = TRUE)
  plot(as.factor(x1), z1, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(as.factor(x1),z1);
  axis(side=4, at = pretty(range(z1)))
  mtext("Normalized Test Rate", side=4, line=3, col='black');
  axis(4, col='black',col.axis='black');
  title(main='CDC ILI in Region vs Normalized Test Rate at SouthBend')
  legend("topleft",legend=c('CDC','BFDx'), text.col=c('blue','black'), pch=c(16,15), col=c('blue','black'), cex=0.8)
  plot(as.factor(x2), y2, col='blue')
  lines(as.factor(x2),y2, col='blue')
  mtext("ILI Case Rate", side=2, line=2.5, col='blue');
  axis(2, col.axis='blue'); par(new = TRUE)
  plot(as.factor(x2), z2, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(as.factor(x2),z2);
  axis(side=4, at = pretty(range(z2)))
  mtext("Normalized Test Rate", side=4, line=3, col='black');
  axis(4, col='black',col.axis='black');
  title(main='CDC ILI in Region vs Normalized Test Rates at Winthrop')
  legend("topleft",legend=c('CDC','BFDx'), text.col=c('blue','black'), pch=c(16,15), col=c('blue','black'), cex=0.8)
  plot(as.factor(x3), y3, col='blue')
  lines(as.factor(x3),y3, col='blue')
  mtext("ILI Case Rate", side=2, line=2.5, col='blue');
  axis(2, col.axis='blue'); par(new = TRUE)
  plot(as.factor(x3), z3, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(as.factor(x3),z3);
  axis(side=4, at = pretty(range(z3)))
  mtext("Normalized Test Rate", side=4, line=3, col='black');
  axis(4, col='black',col.axis='black');
  title(main='CDC ILI in Region vs Normalized Test Rate at MUSC')
  legend("topleft",legend=c('ILI CDC','BFDx'), text.col=c('blue','black'), pch=c(16,15), col=c('blue','black'), cex=0.8)
  p.site.overlay <- recordPlot()
  
  # make a chart for the entire nation
  nation.overlay <- nation.overlay[with(nation.overlay, order(YearWeek)), ]
  x1 <- as.character(unique(nation.overlay[, 'YearWeek']))
  y1 <- nation.overlay[,'iliRate']
  z1 <- nation.overlay[,'NormRunRate']
  plot.new() #-------------------
  par(mfrow=c(1,1))
  par(mar = c(5, 4, 4, 4) + 0.3)
  plot(as.factor(x1), y1, type='n', col='black')
  lines(as.factor(x1),y1, col='black', lwd=3)
  mtext("ILI Patients per Patients Observed", side=2, line=2.5, col='black');
  axis(2, col.axis='black'); par(new = TRUE)
  plot(as.factor(x1), z1, col='red', type = 'n', axes = FALSE, bty = "n", xlab = "", ylab = "")
  lines(as.factor(x1),z1, col='red', lwd=3)
  axis(side=4, at = pretty(range(z1)))
  mtext("Normalized RP Test Run Rate", side=4, line=3, col='red');
  axis(4, col='red',col.axis='red');
  title(main='Weekly CDC ILI vs RP Test Normalized Run Rate')
  legend("topleft",legend=c('CDC','FilmArray RP'), text.col=c('black','red'), pch=c(16,15), col=c('black','red'), cex=0.8)
  p.nation.overlay <- recordPlot()

}