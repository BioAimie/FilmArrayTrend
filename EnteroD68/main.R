# Set the environment
# ===========================================================================================
setwd('~/FilmArrayTrend/EnteroD68/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(mgcv)
library(devtools)
require(dateManip)
library(cluster)
library(caret)
library(dbscan)
library(C50)
library(tidyr)
library(dplyr)
library(rgl)
library(AnomalyDetection)

# Set up variables needed later in the analysis
# ===========================================================================================
# create an Epi date calendar that will be used by all the data sets
startYear <- 2013
calendar.df <- createCalendarLikeMicrosoft(startYear, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))
calendar.df$Days <- 1

# set up some constants
imgDir <- 'Figures/'
dateBreaks <- unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])[order(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek']))][seq(1, length(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])), 8)]

# Load in the data
# ===========================================================================================
# set some query variables, like the customer site... also, get the number of RP runs by site
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- readLines('../DataSources/CustomerSiteIdsWithNames.sql')
query <- paste(queryVector ,collapse="\n")
custnames.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/EnteroD68/sitesRunningRP.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
sites.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/EnteroD68/rpRunsBySite.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)
PMScxn <- odbcConnect('PMS_PROD')
queryVector <- readLines('../DataSources/SQL/EnteroD68/qcMedianCpRP.sql')
query <- paste(queryVector, collapse = '\n')
qc.lot.cps <- sqlQuery(PMScxn, query)
queryVector <- readLines('../DataSources/SQL/EnteroD68/qcMedianTmRP.sql')
query <- paste(queryVector, collapse = '\n')
qc.lot.tms <- sqlQuery(PMScxn, query)
queryVector <- readLines('../DataSources/SQL/EnteroD68/rhinoDataAtCHLA.sql')
query <- paste(queryVector, collapse = '\n')
chla.df <- sqlQuery(PMScxn, query)
queryVector <- readLines('../DataSources/SQL/EnteroD68/CMH_SequencedRuns.sql')
query <- paste(queryVector, collapse = '\n')
cmh.df <- sqlQuery(PMScxn, query)
odbcClose(PMScxn)

# start a loop to gather Cp, Tm, and MaxFluor data for all HRV/Entero Assays by site
dat.df <- c()
choose.sites <- as.character(sites.df[,'CustomerSiteId'])
for(j in 1:length(choose.sites)) {
  
  FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD') 
  queryVector <- readLines('../DataSources/SQL/EnteroD68/rhinoDataBySite.sql')
  query <- paste(gsub('SITE_INDEX', choose.sites[j], queryVector), collapse="\n")
  dat.site.df <- sqlQuery(FADWcxn, query)
  odbcClose(FADWcxn)
  
  dat.df <- rbind(dat.df, dat.site.df)
}

rm(dat.site.df)

# Clean the data
# ============================================================================================
# with the data, determine the median value of Tm, Cp, and Max Fluor for each assay in the HRV/EV target
cp.median <- aggregate(Cp~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=dat.df)
cp.median.chla <- aggregate(Cp~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=chla.df)
cp.median.cmh <- aggregate(Cp~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=cmh.df)
cp.median <- rbind(rbind(cp.median, cp.median.chla), cp.median.cmh)
cp.spread <- spread(data = cp.median, key = AssayName, value = Cp)
tm.median <- aggregate(Tm~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=dat.df)
tm.median.chla <- aggregate(Tm~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=chla.df)
tm.median.cmh <- aggregate(Tm~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=cmh.df)
tm.median <- rbind(rbind(tm.median, tm.median.chla), tm.median.cmh)
tm.spread <- spread(data = tm.median, key = AssayName, value = Tm)
mf.median <- aggregate(MaxFluor~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=dat.df)
mf.median.chla <- aggregate(MaxFluor~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=chla.df)
mf.median.cmh <- aggregate(MaxFluor~RunDataId+LotNo+CustomerSiteId+Date+AssayName, FUN=median, data=cmh.df)
mf.median <- rbind(rbind(mf.median, mf.median.chla), mf.median.cmh)
mf.spread <- spread(data = mf.median, key = AssayName, value = MaxFluor)

# Scale assay median Cp and Tm data using the yeast control as well as QC data where applicable
# ============================================================================================
if(FALSE) {
# cp.norm <- merge(data.frame(cp.spread[,1:4], cp.spread[,5:10]/cp.spread$yeastRNA), qc.lot.cps[qc.lot.cps$Name=='yeastRNA',c('PouchLotNumber','MedianCp')], by.x='LotNo', by.y='PouchLotNumber')
# cp.norm <- data.frame(cp.norm[,1:4], cp.norm[,5:10]*cp.norm$MedianCp)
# colnames(cp.norm)[5:10] <- paste(colnames(cp.norm[5:10]), 'Cp', sep='_')
# cp.norm[,c(5:10)][is.na(cp.norm[,c(5:10)])] <- cp.sparse.handler
# ### WHAT IS A BETTER NORMALIZATION TECHNIQUE FOR Cp AND Tm PROBABLY ALSO????
# qc.yeast.cp.avg <- mean(subset(qc.lot.cps, Name=='yeastRNA')$MedianCp)
# a <- merge(cp.spread, subset(qc.lot.cps, Name=='yeastRNA'), by.x='LotNo', by.y='PouchLotNumber')
# a$TryOneHRV1 <- a$HRV1/a$yeastRNA*a$MedianCp/qc.yeast.cp.avg
# ggplot(a, aes(x=Date, y=HRV1/yeastRNA, color='RunNorm (Cp/CpYeast)')) + geom_point() + geom_point(data=a, aes(x=Date, y=1.5*TryOneHRV1, color='YeastNormRunNorm (1.5*Cp/CpYeast*CpYeastQC/CpYeastQCAvg)'), alpha=0.1) + geom_point(data=a, aes(x=Date, y=yeastRNA/35, color='YeastRaw (CpYeast/35)'), alpha=0.1) + scale_color_manual(values=c('black','blue','red'))
# ### YES I LIKE THIS NORMALIZATION BETTER
} # old normalization method
qc.yeast.cp.avg <- mean(subset(qc.lot.cps, Name=='yeastRNA')$MedianCp)
cp.norm <- merge(data.frame(cp.spread[,1:4], cp.spread[,5:10]/cp.spread$yeastRNA), qc.lot.cps[qc.lot.cps$Name=='yeastRNA',c('PouchLotNumber','MedianCp')], by.x='LotNo', by.y='PouchLotNumber')
cp.norm <- data.frame(cp.norm[,1:4], cp.norm[,5:10]*cp.norm$MedianCp/qc.yeast.cp.avg)
colnames(cp.norm)[5:10] <- paste(colnames(cp.norm[5:10]), 'Cp', sep='_')
### WHAT IS THE APPROPRIATE SPARSE HANDLER.... CAN I PLAY WITH THIS??? MAY ONLY WANT TO DO FOR A FEW SITES....
# which sites are the "right sites"
# maybe the children's hospitals??? 13, 25, 26, 33... with 7 as a control or something like that???
#############################
cp.sparse.handler <- 3 # TRY TUNING THIS????
cp.norm[,c(5:10)][is.na(cp.norm[,c(5:10)])] <- cp.sparse.handler

if(FALSE) {
# tm.norm <- data.frame(RunDataId = tm.spread$RunDataId, tm.spread[,5:10]/tm.spread$yeastRNA)
# colnames(tm.norm)[2:7] <- paste(colnames(tm.norm[2:7]), 'Tm', sep='_')
# tm.norm[,c(2:7)][is.na(tm.norm[,c(2:7)])] <- tm.sparse.handler
# dat.norm <- merge(cp.norm, tm.norm, by='RunDataId')
} # old normalization method
qc.yeast.tm.avg <- mean(subset(qc.lot.tms, Name=='yeastRNA')$MedianTm)
tm.norm <- merge(data.frame(tm.spread[,1:4], tm.spread[,5:10]/tm.spread$yeastRNA), qc.lot.tms[qc.lot.tms$Name=='yeastRNA',c('PouchLotNumber','MedianTm')], by.x='LotNo', by.y='PouchLotNumber')
tm.norm <- data.frame(RunDataId = tm.spread$RunDataId, tm.spread[,5:10]*tm.norm$MedianTm/qc.yeast.tm.avg)
colnames(tm.norm)[2:7] <- paste(colnames(tm.norm[2:7]), 'Tm', sep='_')
tm.sparse.handler <- 100 # TRY TUNING THIS????
tm.norm[,c(2:7)][is.na(tm.norm[,c(2:7)])] <- tm.sparse.handler
dat.norm <- merge(cp.norm, tm.norm, by='RunDataId')

# Since the proposed algorithm requires a certain amount of training data, figure out which sites are "eligible" for detection
# ============================================================================================
site.rhino.count <- with(merge(data.frame(cp.spread, Positive=1), calendar.df, by='Date'), aggregate(Positive~YearWeek+CustomerSiteId, FUN=sum))
sites <- as.character(unique(site.rhino.count$CustomerSiteId))[order(as.character(unique(site.rhino.count$CustomerSiteId)))]
site.rhino.count <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(merge(data.frame(YearWeek = unique(calendar.df[,c('YearWeek')]), CustomerSiteId = sites[x]), site.rhino.count[site.rhino.count$CustomerSiteId==sites[x], c('YearWeek','Positive')], by='YearWeek', all.x=TRUE))))
site.rhino.count[is.na(site.rhino.count$Positive), 'Gap'] <- 1
site.rhino.count[is.na(site.rhino.count$Gap), 'Gap'] <- 0
periods <- unique(as.character(site.rhino.count$YearWeek))
site.gaps <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(5:length(periods), function(y) data.frame(YearWeek = periods[y], CustomerSiteId = sites[x], MissingPeriods = sum(site.rhino.count[site.rhino.count$CustomerSiteId==sites[x], 'Gap'][(y-4):y]))))))
site.starts <- do.call(rbind, lapply(1:length(sites), function(x) site.gaps[site.gaps$CustomerSiteId==sites[x],][max(which(site.gaps[site.gaps$CustomerSiteId==sites[x], 'MissingPeriods']==5)), c('CustomerSiteId','YearWeek')]))

# I think that here I may need to keep the pouch lot number so I can easily tie it to run observations from sequenced samples
dat.features <- merge(dat.norm, calendar.df[,c('Date','YearWeek')], by='Date')
dat.trim <- do.call(rbind, lapply(1:length(site.starts$CustomerSiteId), function(x) filter(dat.features, (CustomerSiteId == site.starts[x,'CustomerSiteId'] & as.character(dat.features$YearWeek) > as.character(site.starts[x,'YearWeek'])))))

# Apply a clustering algorithm TO EACH SITE that loops through each test with positive HRV/EV target with a defined train & test size
# ============================================================================================
initial.window <- 100
test.horizon <- 10

# sites.all <- sites
# sites <- sites.all[c(2, 5, 6, 10, 18)]
# sites <- sites.all[!(sites.all %in% sites)]

scored.df <- c()
for (i in 1:length(sites)) {
  
  # partition the data by site and then order by the test date
  site.features <- filter(dat.trim, CustomerSiteId == sites[i])
  site.features <- site.features[with(site.features, order(Date)), ]
  if(nrow(site.features)==0) { next }
  site.features$Obs <- seq(1, length(site.features$Date), 1)
  
  site.df <- c()
  # site.start.time <- Sys.time()
  for(j in (initial.window+1):(length(site.features$Obs)-test.horizon)) {
    
    # split into train and test data
    site.train <- site.features[site.features$Obs < j & site.features$Obs >= (j - initial.window), ]
    site.test <- site.features[site.features$Obs < (j + test.horizon) & site.features$Obs >= j, ]
    seq.id <- site.test[1,'RunDataId']
    
    if(TRUE)  {
      # get the near zero variance of the train set so that these variables can be removed from the analysis???
      # NOT SURE IF I WANT TO KEEP THIS BECAUSE WHAT IF THE VARIABLE DOES HAVE VARIANCE IN THE TEST SET... AM I GETTING RID OF THAT???
      train.nzv <- nearZeroVar(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], saveMetrics = TRUE)
      train.remove.vars <- row.names(train.nzv[train.nzv$nzv==TRUE,])
      # agg.nzv <- nearZeroVar(rbind(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], site.test[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.test))]), saveMetrics = TRUE)
      # agg.remove.vars <- row.names(agg.nzv[agg.nzv$nzv==TRUE,])
      # # nzv_score <- ifelse(length(agg.remove.vars) > length(train.remove.vars), 1, 0)
      site.train <- site.train[,!(colnames(site.train) %in% train.remove.vars)]
      site.test <- site.test[,!(colnames(site.test) %in% train.remove.vars)]
    }
    
    # transform the data... center, scale, and use Box-Cox to transform the data using PCA or Box-Cox
    #### I DON'T KNOW IF THIS IS NECCESSARY AFTER NORMALIZING Cp AND Tm BECAUSE THE DISTRIBUTIONS ARE SUPER NORMAL
    pca.transform <- preProcess(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], method = c('BoxCox','center','scale','pca'))
    site.train.trans <- predict(pca.transform, site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))])
    site.test.trans <- predict(pca.transform, site.test[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.test))])
        
    # bc.trans <- preProcess(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], method=c('center','scale','BoxCox'))
    # site.train.trans <- predict(bc.trans, site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))])
    # site.test.trans <- predict(bc.trans, site.test[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.test))])
    
    # site.train.trans <- site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))]
    # site.test.trans <- site.test[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.test))]
    
    # apply dbscan to the train data set... determine eps based on the point where there are 2 clusters (1 cluster + noise)
    guess.eps <- 0.1
    guess.mpt <- 90
    eps.interval <- 0.1
    guess.res <- dbscan(site.train.trans, eps = guess.eps, minPts = guess.mpt)
    cluster.int <- max(guess.res$cluster)
    
    # iter.start.time <- Sys.time()
    while(cluster.int < 1) {
      
      guess.eps <- guess.eps + eps.interval
      guess.res <- dbscan(site.train.trans, eps = guess.eps, minPts = guess.mpt)
      noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)
      cluster.int <- max(guess.res$cluster)
    }
    # print(Sys.time() - iter.start.time)
    
    # with the "correct" dbscan clustering, predict the clusters for the test data
    site.train.trans$Cluster <- as.factor(guess.res$cluster)
    # site.test.trans$Cluster <- unname(predict(guess.res, site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], site.test.trans))
    site.test.trans$Cluster <- unname(predict(guess.res, site.train.trans[, grep('PC', colnames(site.train.trans))], site.test.trans))
    
    # count the number of clusters in the test set that are considered noise
    train.noise <- nrow(site.train.trans[site.train.trans$Cluster==0, ])
    test.noise <- nrow(site.test.trans[site.test.trans$Cluster==0, ])
    
    # figure out the number of Principle Components needed to explain at least 95% of the variance for the train set and then the
    # train + test set...
    pca.train <- princomp(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))])
    pca.train.var <- 1-sapply(1:12, function(x) pca.train$sdev[x]^2/sum(pca.train$sdev^2))[min(which(sapply(1:12, function(x) pca.train$sdev[x]^2/sum(pca.train$sdev^2)) <= 0.05))]
    pca.train.count <- min(which(sapply(1:12, function(x) pca.train$sdev[x]^2/sum(pca.train$sdev^2)) <= 0.05))
    pca.test <- princomp(rbind(site.train[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.train))], site.test[, grep(paste(as.character(unique(cp.median$AssayName)), collapse='|'), colnames(site.test))]))
    pca.test.var <- 1-sapply(1:12, function(x) pca.test$sdev[x]^2/sum(pca.test$sdev^2))[min(which(sapply(1:12, function(x) pca.test$sdev[x]^2/sum(pca.test$sdev^2)) <= 0.05))]
    pca.test.count <- min(which(sapply(1:12, function(x) pca.test$sdev[x]^2/sum(pca.test$sdev^2)) <= 0.05))
    pca.test.var.at.train.pca <- 1 - pca.test$sdev[pca.train.count]^2/sum(pca.test$sdev^2)
    
    # create some data frame that contains information about the timeslice
    temp <- data.frame(CustomerSiteId = sites[i], Seq = j, RunDataId = seq.id, TestStartDate = site.test[site.test$Obs==min(site.test$Obs), 'Date'],
                       TrainNoise = train.noise, TestNoise = test.noise,
                       TrainPCA = pca.train.count, TrainVar = pca.train.var, TestPCA = pca.test.count, TestVar = pca.test.var, 
                       TestVarWithTrainPCA = pca.test.var.at.train.pca)
    site.df <- rbind(site.df, temp)
  }
  
  # print(Sys.time() - site.start.time)
  scored.df <- rbind(scored.df, site.df)
}
# write.csv(scored.df, file = '~/FilmArrayTrend/EnteroD68/scoredOutput_20170412.csv', row.names = FALSE)

scored.df <- read.csv('scoredOutput_20170412.csv', header = TRUE)
cmh.seq.runs <- read.csv('../DataSources/SQL/EnteroD68/ChilderensMercy26_SequencedEVD68.csv')
cmh.seq.runs <- merge(unique(cmh.df[,c('PouchSerialNumber','RunDataId','CustomerSiteId')]), cmh.seq.runs, by.x='PouchSerialNumber', by.y='SerialNumber')
scored.seq.id <- merge(scored.df, cmh.seq.runs[,c('RunDataId','CustomerSiteId','Alg1_Present')], by=c('RunDataId','CustomerSiteId'), all.x=TRUE)
scored.seq.id[is.na(scored.seq.id$Alg1_Present), 'Alg1_Present'] <- 0
# ggplot(scored.seq.id, aes(x=TestStartDate, y=TestNoise/TrainNoise, color=as.factor(Alg1_Present))) + geom_point() + facet_wrap(~CustomerSiteId, scale='free_y') + theme(axis.text.x=element_text(angle=90))

# USE THE TWITTER ALGORITHM TO DETECT ANOMALIES IN TESTING PATTERNS 
positive.freq <- data.frame(Date = as.POSIXct(scored.df$TestStartDate), CustomerSiteId = scored.df$CustomerSiteId, Record = 1)
positive.freq <- with(positive.freq, aggregate(Record~Date+CustomerSiteId, FUN=sum))
scored.sites <- unique(scored.df$CustomerSiteId)
positive.anoms <- c()
for(i in 1:length(scored.sites)) {
  
  temp <- AnomalyDetectionTs(subset(positive.freq, CustomerSiteId==scored.sites[i])[,c('Date','Record')], direction='pos', max_anoms=0.01, plot=FALSE)$anoms
  if(nrow(temp)==0) { next() }
  temp$CustomerSiteId <- scored.sites[i]
  positive.anoms <- rbind(positive.anoms, temp)
}

scored.freq.seq.id <- merge(scored.seq.id, positive.anoms, by.x=c('TestStartDate','CustomerSiteId'), by.y=c('timestamp','CustomerSiteId'), all.x=TRUE)
scored.freq.seq.id[!(is.na(scored.freq.seq.id$anoms)), 'anoms'] <- 1
scored.freq.seq.id[is.na(scored.freq.seq.id$anoms), 'anoms'] <- 0

ggplot(scored.freq.seq.id, aes(x=as.Date(TestStartDate), y=TestNoise/TrainNoise+anoms)) + geom_point() + facet_wrap(~CustomerSiteId) + theme(axis.text.x=element_text(angle=90))


# ggplot(merge(scored.df, custnames.df[,c('CustomerSiteId','Name')], by='CustomerSiteId'), aes(x=TestStartDate, y=test.horizon*TestNoise/TrainNoise, color=TestVarWithTrainPCA)) + geom_point() + facet_wrap(~Name)
# 
# # ADD SOME QUANTIFIER OF FREQUENCY OF TESTING... I AM NOT SURE HOW TO DO THIS SINCE THE GROWTH COULD BE DUE TO
# # THE INSTALL BASE OR TO AN "OUTBREAK" OR SOMETHING ELSE.... MAYBE DO POSITIVE TEST FREQUENCY/TUR IN THE PERIOD? 
# scored.freq <- merge(scored.df, with(data.frame(scored.df, Freq=1), aggregate(Freq~CustomerSiteId+TestStartDate, FUN=sum)), by=c('CustomerSiteId','TestStartDate'))
# scored.freq <- merge(scored.freq, with(scored.freq, aggregate(Freq~CustomerSiteId, FUN=mean)), by='CustomerSiteId')
# scored.freq$FreqRatio <- with(scored.freq, Freq.x/Freq.y)
# 
# # TWITTER ALGORITHM ... AUTOMATION OF ALGORITHM 1
# # For each test, determine the sequence pattern and then test for anomalies
# # a good source is http://www.itl.nist.gov/div898/handbook/eda/section3/eda35h3.htm for Generalized Extreme Studentized Deviate method
# cp.assay.median <- cp.median[!(cp.median$AssayName %in% c('yeastRNA','PCR2')), ]
# cp.yeast.median <- cp.median[cp.median$AssayName == 'yeastRNA', c('RunDataId','Cp')]
# colnames(cp.yeast.median) <- c('RunDataId','YeastCp')
# cp.assay.norm <- merge(cp.assay.median, cp.yeast.median, by='RunDataId')
# cp.assay.norm <- merge(cp.assay.norm, qc.lot.cps[qc.lot.cps$Name=='yeastRNA', c('MedianCp','PouchLotNumber')], by.x='LotNo', by.y='PouchLotNumber')
# cp.assay.norm$NormCp <- cp.assay.norm$Cp/cp.assay.norm$YeastCp*cp.assay.norm$MedianCp
# cp.assay.norm <- cp.assay.norm[with(cp.assay.norm, order(RunDataId, NormCp, AssayName)), ]
# cp.assay.norm.seq <- do.call(rbind, lapply(1:length(unique(cp.assay.norm$RunDataId)), function(x) data.frame(RunDataId = unique(cp.assay.norm$RunDataId)[x], Sequence = paste(cp.assay.norm[cp.assay.norm$RunDataId==unique(cp.assay.norm$RunDataId)[x], 'AssayName'], collapse=', '))))
# cp.assay.norm.seq <- merge(unique(cp.assay.norm[,c('RunDataId','CustomerSiteId','Date')]), cp.assay.norm.seq, by='RunDataId')
# sequences <- as.character(unique(cp.assay.norm.seq$Sequence))
# 
# seq.anoms <- c()
# for(i in 1:length(sites)) {
# 
#   site.date <- min(cp.assay.norm.seq[cp.assay.norm.seq$CustomerSiteId==sites[i], 'Date'])
#   site.sequences <- as.character(unique(cp.assay.norm.seq[cp.assay.norm.seq$CustomerSiteId==sites[i], 'Sequence']))
#   site.anoms <- c()
#   for(j in 1:length(site.sequences)) {
#     
#     sequence.freq <- cp.assay.norm.seq[cp.assay.norm.seq$CustomerSiteId==sites[i] & cp.assay.norm.seq$Sequence==site.sequences[j], ]
#     sequence.freq.fill <- merge(data.frame(Date = calendar.df[calendar.df$Date > site.date, 'Date']), data.frame(Date = sequence.freq[,c('Date')], Freq = 1), by='Date', all.x=TRUE)
#     sequence.freq.fill[is.na(sequence.freq.fill$Freq), 'Freq'] <- 0
#     sequence.freq.fill <- with(sequence.freq.fill, aggregate(Freq~Date, FUN=sum))
#     # I don't think it's appropriate to use a count, because that is skewed over time by changes in utlization... it should be a rate of positive HRV/EV tests or something
#     run.freq.fill <- merge(data.frame(Date = calendar.df[calendar.df$Date > site.date, 'Date']), runs.df[runs.df$CustomerSiteId==sites[i], c('Date','Run')], by='Date', all.x=TRUE)
#     run.freq.fill[is.na(run.freq.fill$Run), 'Run'] <- 0
#     run.freq.fill <- with(run.freq.fill, aggregate(Run~Date, FUN=sum))
#     sequence.freq.fill <- merge(run.freq.fill, sequence.freq.fill, by='Date')
#     sequence.freq.fill$Rate <- with(sequence.freq.fill, Freq/Run)
#     sequence.freq.fill[is.nan(sequence.freq.fill$Rate), 'Rate'] <- 0
#     # update
#     sequence.freq.fill$Date <- as.POSIXct(sequence.freq.fill$Date)
#     site.seq.anoms <- AnomalyDetectionTs(sequence.freq.fill[,c('Date','Rate')], max_anoms=0.01, direction='both', plot=FALSE)
#     if(nrow(site.seq.anoms$anoms)==0) { next() }
#     site.seq.anoms <- data.frame(CustomerSiteId = sites[i], Sequence = site.sequences[j], site.seq.anoms$anoms)
#     site.seq.anoms$Date <- as.Date(site.seq.anoms$timestamp)
#     
#     site.anoms <- rbind(site.anoms, site.seq.anoms)
#   }
#   
#   # site.anoms$Date <- as.Date(site.anoms$timestamp)
#   # site.anom.agg.date <- with(site.anoms, aggregate(anoms~Date, FUN=sum))
#   # site.anom.agg.seq <- with(site.anoms, aggregate(anoms~Sequence, FUN=sum))
#   # site.anom.agg.date[site.anom.agg.date$anoms > (mean(site.anom.agg.date$anoms) + 3*sd(site.anom.agg.date$anoms)), ]
#   # site.anom.agg.seq[site.anom.agg.seq$anoms > (mean(site.anom.agg.seq$anoms) + 3*sd(site.anom.agg.seq$anoms)), ]
#   
#   seq.anoms <- rbind(seq.anoms, site.anoms)
# }
# 
# # Make some plots to explore what has been made:
# # for site 13 (Nationwide Childrens)
# movie.sites <- as.character(unique(scored.df$CustomerSiteId))
# for(h in 1:length(movie.sites)) {
#   
#   site.to.gif <- movie.sites[h]
#   site.date.breaks <- unique(scored.df[scored.df$CustomerSiteId==site.to.gif,'TestStartDate'])[seq(30, length(unique(scored.df[scored.df$CustomerSiteId==site.to.gif,'TestStartDate'])), 30)]
#   
#   for(i in 1:length(site.date.breaks)) {
#     
#     if(length(site.date.breaks) >= 100) {
#       
#       i.extension <- ifelse(i < 10, paste('00',i, sep=''), ifelse(i < 100, paste('0',i, sep=''), paste(i, sep='')))
#     } else if (length(site.date.breaks) >= 10) {
#       
#       i.extension <- ifelse(i < 10, paste('0',i, sep=''), paste(i, sep=''))
#     } else {
#       
#       i.extension <- paste(i, sep='')
#     }
#     
#     site.date.temp <- subset(scored.df, CustomerSiteId==site.to.gif & TestStartDate <= site.date.breaks[i])
#     # print(nrow(site.date.temp))
#     png(paste('Movie/Scored/',site.to.gif,'/','CustomerSite',site.to.gif,'_',i.extension,'.png', sep=''))
#     print(ggplot(site.date.temp, aes(x=TestStartDate, y=test.horizon*TestNoise/TrainNoise, color=TestVarWithTrainPCA)) + geom_point() + labs(title=paste('Customer Site', site.to.gif, 'at date <=', site.date.breaks[i], sep=' '), y='Score', x='Date')) # + geom_line(aes(x=TestStartDate, y=mean(10*TestNoise/TrainNoise)+5*sd(10*TestNoise/TrainNoise)), data=site.date.temp, color='red', lty='dashed'))
#     dev.off()
#   }
#   # create a gif by using command prompt, cd into the dir with the pngs then run magick convert *.png -delay x -loop y name.gif (run with x=3, y=5)
# }
# 
# ggplot(subset(seq.anoms, CustomerSiteId=='13'), aes(x=Date, y=anoms, fill=Sequence)) + geom_bar(stat='identity') + theme(legend.position='bottom')
# 
# 
# if(FALSE) {
#   # start a loop to gather Cp data for all sites running RP
#   cp.df <- c()
#   choose.sites <- as.character(sites.df[,'CustomerSiteId'])
#   for(j in 1:length(choose.sites)) {
#     
#     FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD') 
#     queryVector <- scan('../DataSources/SQL/EnteroD68/rhinoCpDataBySite.sql', what=character(), quote="")
#     query <- paste(gsub('SITE_INDEX', choose.sites[j], queryVector), collapse=" ")
#     cp.site.df <- sqlQuery(FADWcxn, query)
#     odbcClose(FADWcxn)
#     
#     cp.df <- rbind(cp.df, cp.site.df)
#   }
#   
#   rm(cp.site.df)
#   
#   # Clean the data
#   # ============================================================================================
#   # with the cp data, determine the median Cp of each assay in the HRV/EV target
#   cp.median <- aggregate(Cp~RunDataId+CustomerSiteId+Date+AssayName, FUN=median, data=cp.df)
#   cp.spread <- spread(data = cp.median, key = AssayName, value = Cp)
#   sparse.handler <- 40
#   cp.spread[,c(4:9)][is.na(cp.spread[,c(4:9)])] <- sparse.handler
#   
#   # determine the sequence associated with each HRV/EV positive
#   run.ids <- unique(cp.median$RunDataId)
#   cp.ordered <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(cp.median[cp.median$RunDataId==run.ids[x], ][order(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), ], Index = seq(1, length(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), 1))))
#   cp.sequence <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(RunDataId = run.ids[x], Sequence = paste(as.character(cp.ordered[cp.ordered$RunDataId==run.ids[x], 'AssayName']), collapse=', '))))
# 
#   # since the final algorithm should be run on a site-by-site basis, determine which data by site are "eligible"
#   site.rhino.count <- with(merge(data.frame(cp.spread, Positive=1), calendar.df, by='Date'), aggregate(Positive~YearWeek+CustomerSiteId, FUN=sum))
#   sites <- as.character(unique(site.rhino.count$CustomerSiteId))[order(as.character(unique(site.rhino.count$CustomerSiteId)))]
#   site.rhino.count <- do.call(rbind, lapply(1:length(sites), function(x) data.frame(merge(data.frame(YearWeek = unique(calendar.df[,c('YearWeek')]), CustomerSiteId = sites[x]), site.rhino.count[site.rhino.count$CustomerSiteId==sites[x], c('YearWeek','Positive')], by='YearWeek', all.x=TRUE))))
#   site.rhino.count[is.na(site.rhino.count$Positive), 'Gap'] <- 1
#   site.rhino.count[is.na(site.rhino.count$Gap), 'Gap'] <- 0
#   periods <- unique(as.character(site.rhino.count$YearWeek))
#   site.gaps <- do.call(rbind, lapply(1:length(sites), function(x) do.call(rbind, lapply(5:length(periods), function(y) data.frame(YearWeek = periods[y], CustomerSiteId = sites[x], MissingPeriods = sum(site.rhino.count[site.rhino.count$CustomerSiteId==sites[x], 'Gap'][(y-4):y]))))))
#   site.starts <- do.call(rbind, lapply(1:length(sites), function(x) site.gaps[site.gaps$CustomerSiteId==sites[x],][max(which(site.gaps[site.gaps$CustomerSiteId==sites[x], 'MissingPeriods']==5)), c('CustomerSiteId','YearWeek')]))
# 
#   # check to see if any of the features have near-zero variance (i.e. variables with very few unique values, which can skew results when data are split for train/test)
#   nzv <- nearZeroVar(cp.spread[,c(4:9)], saveMetrics = TRUE)
#   remove.vars <- row.names(nzv[nzv$nzv==TRUE,])
#   cp.clean <- cp.spread[,!(colnames(cp.spread) %in% remove.vars)]
# 
#   # check to see if any of the variables have very strong correlation (cut off of 0.8)
#   keep.vars <- cor(cp.clean[,c(4:7)])[,-findCorrelation(cor(cp.clean[,c(4:7)]), cutoff=0.8)]
#   cp.clean <- cp.clean[,colnames(cp.clean) %in% c('RunDataId','Date','YearWeek','CustomerSiteId',row.names(keep.vars))]
# 
#   # Apply machine learning
#   # ===========================================================================================
#   # data should be analyzed on a site-by-site basis
#   # cp.features <- merge(cp.clean, calendar.df[,c('Date','YearWeek')], by='Date') 
#   cp.features <- merge(cp.spread, calendar.df[,c('Date','YearWeek')], by='Date') 
#   cp.features <- cp.features[with(cp.features, order(CustomerSiteId, Date)), ]
#   sites <- unique(cp.features$CustomerSiteId)[order(unique(cp.features$CustomerSiteId))]
#   
#   # set up the intial window and horizon for time slices
#   initial.window <- 100
#   test.horizon <- 10
#   
#   # sites <- c(13, 25, 26) # , 33, 36, 38)
#   scored.df <- c()
#   for (i in 1:length(sites)) {
#     
#     # parition the data by site and set up a timeframe
#     site.start <- as.character(site.starts[site.starts$CustomerSiteId==sites[i], 'YearWeek'])
#     site.features <- cp.features[cp.features$CustomerSiteId==sites[i] & as.character(cp.features$YearWeek) > site.start, ]
#     site.features <- site.features[with(site.features, order(Date)), ]
#     if(nrow(site.features)==0) { next }
#     site.features$Obs <- seq(1, length(site.features$Date), 1)
#     site.features$DaysBetween <- c(0, as.numeric(sapply(2:length(site.features$Date), function(x) site.features[x,'Date']-site.features[(x-1),'Date'])))
#     
#     site.df <- c()
#     site.start.time <- Sys.time()
#     for(j in (initial.window+1):(length(site.features$Obs)-test.horizon)) {
#     
#       site.train <- site.features[site.features$Obs < j & site.features$Obs >= (j - initial.window), ]
#       site.test <- site.features[site.features$Obs < (j + test.horizon) & site.features$Obs >= j, ]
#       
#       train.nzv <- nearZeroVar(site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))], saveMetrics = TRUE)
#       train.remove.vars <- row.names(train.nzv[train.nzv$nzv==TRUE,])
#       site.train <- site.train[,!(colnames(site.train) %in% train.remove.vars)]
#       site.test <- site.test[,!(colnames(site.test) %in% train.remove.vars)]
#       
#       pca.tranform <- preProcess(site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))], method = 'pca')
#       site.train.pca <- predict(pca.tranform, site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))])
#       site.test.pca <- predict(pca.tranform, site.test[,(colnames(site.test) %in% as.character(unique(cp.df$AssayName)))])
#       
#       # apply dbscan to the train data set... determine eps based on the point where there are 2 clusters (1 cluster + noise)
#       guess.eps <- 0.01
#       guess.mpt <- 90
#       eps.interval <- 0.01
#       guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#       cluster.int <- max(guess.res$cluster)
#       
#       iter.start.time <- Sys.time()
#       while(cluster.int < 1) {
#         
#         guess.eps <- guess.eps + eps.interval
#         guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#         noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)
#         cluster.int <- max(guess.res$cluster)
#       }
#       print(Sys.time() - iter.start.time)
#       
#       # with the "correct" dbscan clustering, predict the clusters for the test data
#       site.train.pca$Cluster <- as.factor(guess.res$cluster)
#       site.test.pca$Cluster <- unname(predict(guess.res, site.train.pca[,grep('^PC', colnames(site.train.pca))], site.test.pca))
#       
#       # count the number of clusters in the test set that are considered noise
#       pca.count <- max(grep('^PC', colnames(site.train.pca)))
#       train.noise <- nrow(site.train.pca[site.train.pca$Cluster==0, ])
#       test.noise <- nrow(site.test.pca[site.test.pca$Cluster==0, ])
#       train.days.mean <- mean(site.train$DaysBetween)
#       test.days.mean <- mean(site.test$DaysBetween)
#       train.days.median <- median(site.train$DaysBetween)
#       test.days.median <- median(site.test$DaysBetween)
#       temp <- data.frame(CustomerSiteId = sites[i], Seq = j, TestStartDate = site.test[site.test$Obs==min(site.test$Obs), 'Date'],
#                          TrainNoise = train.noise, TestNoise = test.noise, PCAs = pca.count, TrainDaysMean = train.days.mean,
#                          TrainDaysMedian = train.days.median, TestDaysMean = test.days.mean, TestDaysMedian = test.days.median)
#       temp$Score <- with(temp, ifelse(TrainNoise > 0, TestNoise/TrainNoise*pca.count, TestNoise*pca.count))
#       site.df <- rbind(site.df, temp)
#     }
#   
#     print(Sys.time() - site.start.time)
#     scored.df <- rbind(scored.df, site.df)
#   }
#   
#   # ATTEMPT TO DO SOME SORT OF ROLLING SUM OF THE SCORE AT A SITE... THIS SHOULD HELP IDENTIFY PERIODS WHERE SCORES ARE
#   # SUSTAINED AT A HIGHER LEVEL
#   scored.df[is.na(scored.df$Score), 'Score']  <- scored.df[is.na(scored.df$Score), 'TestNoise']*scored.df[is.na(scored.df$Score), 'PCAs']
#   a <- scored.df[scored.df$CustomerSiteId==13 &  scored.df$TestStartDate < as.Date('2017-03-01'), ]
#   ggplot(a, aes(x=TestStartDate, y=Score)) + geom_point() + geom_line(aes(x=TestStartDate, y=mean(Score)+3*sd(Score)), data=a, color='red', lwd=1.25)
# }
# if(FALSE) {
#   # parition the data by site and set up a timeframe
#   site.start <- as.character(site.starts[site.starts$CustomerSiteId==sites[i], 'YearWeek'])
#   site.features <- cp.features[cp.features$CustomerSiteId==sites[i] & as.character(cp.features$YearWeek) > site.start, ]
#   site.features <- site.features[with(site.features, order(Date)), ]
#   site.features$Obs <- seq(1, length(site.features$Date), 1)
#   
#   # this section was using weeks as the breaks, which is arbitrary... Try rolling by sequence instead
#   site.df <- c()
#   for(j in (initial.window+1):(length(site.features$Obs)-test.horizon)) {
#     
#     site.train <- site.features[site.features$Obs < j & site.features$Obs >= (j - initial.window), ]
#     site.test <- site.features[site.features$Obs < (j + test.horizon) & site.features$Obs >= j, ]
#     
#       # preprocess data using the PCA method
#       set.seed(1234)
#       period.pca.trans <- preProcess(site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))], method = 'pca')
#       site.train.pca <- predict(period.pca.trans, site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))])
#       site.test.pca <- predict(period.pca.trans, site.test[,(colnames(site.test) %in% as.character(unique(cp.df$AssayName)))])
#      
#       # apply dbscan to the train data set... determine eps based on reducing the ratio of points considered as noise to some threshold
#       guess.eps <- 0.01
#       guess.mpt <- 10
#       noise.threshold <- 0.10
#       eps.interval <- 0.01
#       guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#       noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)
# 
#       while(noise.threshold < noise.ratio) {
# 
#         guess.eps <- guess.eps + eps.interval
#         guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#         noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)
#       }
#       
#       # with the "correct" dbscan clustering, predict the clusters for the test data
#       site.train.pca$Cluster <- as.factor(guess.res$cluster)
#       site.test.pca$Cluster <- unname(predict(guess.res, site.train.pca[,colnames(site.train.pca)[grep('^PC', colnames(site.train.pca))]],
#                                               site.test.pca[,colnames(site.test.pca)[grep('^PC', colnames(site.test.pca))]]))
#       
#       # this scoring algorithm could work, but the issue is with the time period of the observations... it needs to be normalized
#       test.noise <- length(site.test.pca[site.test.pca$Cluster==0, 'Cluster'])
#       clust.count <- max(as.numeric(as.character(site.train.pca$Cluster)))+1
#       temp.df <- data.frame(Index = j, CustomerSiteId = sites[i], ClusterCount = clust.count, NoisePoints = test.noise, Eps = guess.eps, NoiseRation = noise.ratio, StartDate = min(site.train$Date), StopDate = max(site.test$Date))
#       temp.df$ObsWindowInDays <- as.numeric(temp.df$StopDate - temp.df$StartDate)
#       temp.df$Score <- (temp.df$ClusterCount + 1/temp.df$Eps*5 + temp.df$NoisePoints)/temp.df$ObsWindowInDays
#       site.df <- rbind(site.df, temp.df)
# 
#       # print(paste(test.noise, 'observations are noise in the test set at j=', j, ' and there are', clust.count, 'clusters.', sep=' '))
#       
#       # site.period.df <- rbind(data.frame(cbind(site.train[,c('YearWeek','CustomerSiteId','Obs')], site.train.pca)), data.frame(cbind(site.test[,c('YearWeek','CustomerSiteId','Obs')], site.test.pca)))
#       # site.period.df$PCs <- length(colnames(site.period.df)[grep('^PC', colnames(site.period.df))])
#       # site.period.df$Key <- j
#       # site.period.df <- site.period.df[,c('YearWeek','Obs','Cluster','Key')]
#       # site.df <- rbind(site.df, site.period.df)
#       # ggplot(site.period.df, aes(x=as.factor(Obs), fill=Cluster)) + geom_bar() + labs(title = paste('Cluster Prediction at j =', j, sep=' '), x='Observation')
#   }
#   }
# if(FALSE) {
#   # # every "train.weeks" consecutive period will be used as the train set and then the forward "test.weeks" period will be predicted
#   # site.train.periods <- unique(cp.features[cp.features$YearWeek > site.start, 'YearWeek'])
#   # 
#   # for(j in (train.weeks+2):(length(site.train.periods)-test.weeks)) {
#   # 
#   #   site.train <- site.features[site.features$YearWeek >= (site.train.periods[j-train.weeks]) & site.features$YearWeek <= site.train.periods[j], ]
#   #   site.test <- site.features[site.features$YearWeek == site.train.periods[j+1], ]
#   #   
#   #   if(nrow(site.train) < 50) { break() }
#   #   
#   #   # preprocess data using the PCA method
#   #   set.seed(1234)
#   #   period.pca.trans <- preProcess(site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))], method = 'pca')
#   #   site.train.pca <- predict(period.pca.trans, site.train[,(colnames(site.train) %in% as.character(unique(cp.df$AssayName)))])
#   #   site.test.pca <- predict(period.pca.trans, site.test[,(colnames(site.test) %in% as.character(unique(cp.df$AssayName)))])
#   #   
#   #   # apply dbscan to the train data set... determine eps based on minimizing the ratio of points considered as noise
#   #   guess.eps <- 0.01
#   #   guess.mpt <- 10
#   #   noise.threshold <- 0.10
#   #   eps.interval <- 0.01
#   #   guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#   #   noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)
#   #   
#   #   while(noise.threshold < noise.ratio) {
#   #     
#   #     guess.eps <- guess.eps + eps.interval
#   #     guess.res <- dbscan(site.train.pca, eps = guess.eps, minPts = guess.mpt)
#   #     noise.ratio <- sum(guess.res$cluster==0)/length(guess.res$cluster)        
#   #   }
#   #   
#   #   # with the "correct" dbscan clustering, predict the clusters for the test data
#   #   site.train.pca$Cluster <- as.factor(guess.res$cluster)
#   #   site.test.pca$Cluster <- unname(predict(guess.res, site.train.pca[,colnames(site.train.pca)[grep('^PC', colnames(site.train.pca))]],
#   #                                    site.test.pca[,colnames(site.test.pca)[grep('^PC', colnames(site.test.pca))]]))
#   #     
#   #   
#   #   site.period.df <- rbind(data.frame(cbind(site.train[,c('YearWeek','CustomerSiteId')], site.train.pca)), data.frame(cbind(site.test[,c('YearWeek','CustomerSiteId')], site.test.pca)))
#   #   ggplot(site.period.df, aes(x=YearWeek, fill=Cluster)) + geom_bar()
#   #   
#   #   
#   #   # I THINK THAT THERE IS SOMETHING HERE, BUT USING THE 95% CI ON THE NOISE CLUSTER DENSITY IS WAY TOO SENSITIVE
#   #   site.agg <- with(data.frame(site.period.df, Count=1), aggregate(Count~YearWeek+Cluster, FUN=sum))
#   #   fill.periods <- unique(site.agg$YearWeek)[order(unique(site.agg$YearWeek))]
#   #   clusters <- max(as.numeric(as.character(site.agg$Cluster)))
#   #   place.hold <- do.call(rbind, lapply(1:length(fill.periods), function(x) data.frame(YearWeek = fill.periods[x], Cluster = as.factor(seq(0, clusters, 1)))))
#   #   site.agg.fill <- merge(place.hold, site.agg, by=c('YearWeek','Cluster'), all=TRUE)
#   #   site.agg.fill[is.na(site.agg.fill$Count), 'Count'] <- 0
#   #   site.agg.fill <- merge(site.agg.fill, with(site.agg.fill, aggregate(Count~YearWeek, FUN=sum)), by='YearWeek')
#   #   colnames(site.agg.fill) <- c('YearWeek','Cluster','ClusterCount','Observations')
#   #   site.agg.fill$Portion <- with(site.agg.fill, ClusterCount/Observations)
#   #   site.ci.info <- merge(merge(with(site.agg.fill[as.character(site.agg.fill$YearWeek)!=max(as.character(site.agg.fill$YearWeek)), ], aggregate(Portion~Cluster, FUN=mean)), with(site.agg.fill[as.character(site.agg.fill$YearWeek)!=max(as.character(site.agg.fill$YearWeek)), ], aggregate(Portion~Cluster, FUN=sd)), by='Cluster'), with(site.agg.fill[as.character(site.agg.fill$YearWeek)!=max(as.character(site.agg.fill$YearWeek)), ], aggregate(ClusterCount~Cluster, FUN=sum)), by='Cluster')
#   #   colnames(site.ci.info) <- c('Cluster','sMean','sSdev','n')
#   #   flag <- (site.ci.info[site.ci.info$Cluster==0,'sMean'] + qnorm(0.975)*site.ci.info[site.ci.info$Cluster==0, 'sSdev']/sqrt(site.ci.info[site.ci.info$Cluster==0, 'n'])) < site.agg.fill[site.agg.fill$Cluster==0 & as.character(site.agg.fill$YearWeek)==max(as.character(site.agg.fill$YearWeek)),'Portion']
#   #   
#   #   if(flag) {
#   #     
#   #     message <- paste('For site', sites[i], 'at time period', site.train.periods[j+1], '(j = ', j, ')','there would be an anomaly.', sep=' ')
#   #     print(message)
#   #     break()
#   #   }
#   # }
#   }
# if(FALSE) {
#   #   determine labels
#   run.ids <- unique(cp.median$RunDataId)
#   cp.ordered <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(cp.median[cp.median$RunDataId==run.ids[x], ][order(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), ], Index = seq(1, length(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), 1))))
#   cp.sequence <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(RunDataId = run.ids[x], Sequence = paste(as.character(cp.ordered[cp.ordered$RunDataId==run.ids[x], 'AssayName']), collapse=', '))))
#   
#   #   experiment with creating labels in different ways
#   cp.labels <- cp.spread
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='HRV1', 'RunDataId'],'F1'] <- 'H1'
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='HRV2', 'RunDataId'],'F1'] <- 'H2'
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='HRV3', 'RunDataId'],'F1'] <- 'H3'
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='HRV4', 'RunDataId'],'F1'] <- 'H4'
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='Entero1', 'RunDataId'],'F1'] <- 'E1'
#   cp.labels[cp.labels$RunDataId %in% cp.ordered[cp.ordered$Index==1 & cp.ordered$AssayName=='Entero2', 'RunDataId'],'F1'] <- 'E2'
#   # cp.labels$F2 <- round(cp.ordered[cp.ordered$Index==1, 'Cp'])
#   # cp.labels$F2 <- ifelse(nchar(cp.labels$F2) < 2, paste('0',cp.labels$F2,sep=''), cp.labels$F2)
#   cp.labels$F2 <- cut(cp.ordered[cp.ordered$Index==1, 'Cp'], breaks=c(0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30), labels=c('01','02','03','04','05','06','07','08','09','10'))
#   cp.labels$F3 <- paste('H', unname(apply(cp.labels[,colnames(cp.labels)[grep('HRV', colnames(cp.labels))]] < 40, 1, sum)), sep='')
#   cp.labels$F4 <- paste('E', unname(apply(cp.labels[,colnames(cp.labels)[grep('Entero', colnames(cp.labels))]] < 40, 1, sum)), sep='')
#   cp.labels$Label <- paste(cp.labels$F1, cp.labels$F2, cp.labels$F3, cp.labels$F4, sep='')
#   cp.labels <- merge(cp.labels, calendar.df[,c('Date','YearWeek')], by='Date')
#   
#   #   the first method will utilize a user-defined time slicing technique to create a train & test set by site
#   cp.features <- merge(cp.spread, calendar.df[,c('Date','YearWeek')], by='Date')
#   train.weeks <- 12
#   test.weeks <- 1
#   max.clusters <- 20
#   
#   set.seed(1234)
#   site.rolling.density <- c()
#   for(i in 1:length(sites)) {
#   
#     print(i)
#     site.periods <- periods[periods > as.character(site.starts[site.starts$CustomerSiteId==sites[i],'YearWeek'])]
#     l <- length(site.periods)
#     
#     if(l < (train.weeks+test.weeks)) { break() }
#     
#     rolling.density <- c()
#     for(j in (train.weeks+1):(l-1)) {
#       
#       # create the training set
#       # print(site.periods[j])
#       train.set <- cp.features[cp.features$CustomerSiteId==sites[i] & cp.features$YearWeek <= site.periods[j] & cp.features$YearWeek >= site.periods[(j-train.weeks)], ]
#       
#       # if the data set is too small, then skip forward in time
#       if(nrow(train.set) < 40) { break() }
#       
#       # on the fly, determine the number of clusters for this data set
#       # print('entering section to determine k')
#       wss <- (nrow(train.set[, as.character(unique(cp.df$AssayName))])-1)*sum(apply(train.set[, as.character(unique(cp.df$AssayName))], 2, var))
#       # for (k in 2:max.clusters) wss[k] <- sum(kmeans(train.set[, as.character(unique(cp.df$AssayName))], centers=k)$withinss)
#       for (k in 2:max.clusters) wss[k] <- (kmeans(train.set[, as.character(unique(cp.df$AssayName))], centers=k)$betweenss/kmeans(train.set[, as.character(unique(cp.df$AssayName))], centers=k)$totss)
#       k.curve <- sapply(2:length(wss), function(x) (wss[x-1] - wss[x])/wss[x-1])
#       if(length(which(k.curve < 0))==0) {
#   
#         k.centers <- max.clusters
#       } else {
#   
#         k.centers <- min(which(k.curve < 0)) - 1
#       }
#       # k.centers = 10
#       
#       # perform k-means on the training set to get a cluster
#       # print('entering section to cluster using k-means')
#       k.fit <- kmeans(train.set[, as.character(unique(cp.df$AssayName))], centers = k.centers, iter.max = 100)
#       train.set$Cluster <- as.factor(unname(k.fit$cluster))
#       
#       # use kNN with these labels and then predict the clustering of the next week
#       # print('entering section to predict cluster using kNN as a training method')
#       k.knn <- train(Cluster~., data = train.set[, c(as.character(unique(cp.df$AssayName)),'Cluster')], method='knn')
#       predicted.set <- cp.features[cp.features$CustomerSiteId==sites[i] & cp.features$YearWeek <= site.periods[j+1] & cp.features$YearWeek >= site.periods[(j-train.weeks)], as.character(unique(cp.df$AssayName))]
#       
#       # try to rerun kmeans on the new data set and see if the clusters remain similar or change
#       wss <- (nrow(predicted.set[, as.character(unique(cp.df$AssayName))])-1)*sum(apply(predicted.set[, as.character(unique(cp.df$AssayName))], 2, var))
#       for (k in 2:max.clusters) wss[k] <- sum(kmeans(predicted.set[, as.character(unique(cp.df$AssayName))], centers=k)$withinss)
#       k.curve <- sapply(2:length(wss), function(x) (wss[x-1] - wss[x])/wss[x-1])
#       if(length(which(k.curve < 0))==0) {
#         
#         k.centers.new <- max.clusters
#       } else {
#         
#         k.centers.new <- min(which(k.curve < 0)) - 1
#       }
#       k.fit.new <- kmeans(predicted.set[, as.character(unique(cp.df$AssayName))], centers = k.centers.new, iter.max = 100)
#       # ----------------------
#       
#       predicted.set$Cluster <- predict(k.knn, newdata = predicted.set)
#       
#       # find the change in density
#       # print('entering section to find the change in cluster density')
#       train.total <- nrow(train.set)
#       train.density <- with(data.frame(train.set, Count = 1), aggregate(Count~Cluster, FUN=sum))
#       predict.total <- nrow(predicted.set)
#       shift.density <- with(data.frame(predicted.set, Count = 1), aggregate(Count~Cluster, FUN=sum))
#       train.density$Density <- train.density$Count/train.total
#       shift.density$Density <- shift.density$Count/predict.total
#      
#       # bind the data together to get a temporal view of density changes by site
#       # print('entering section to bind data')
#       temp <- data.frame(CustomerSiteId = sites[i], YearWeek = site.periods[j+1], NoCluster = k.centers, TrainingSetSize = train.total, NewSetSize = predict.total, MedianDensityShift = median(abs((train.density$Density - shift.density$Density))/train.density$Density), SdDensityShift = sd(abs((train.density$Density - shift.density$Density))/train.density$Density), MaxDensityShift=max(abs((train.density$Density - shift.density$Density))/train.density$Density), MinDensityShift=min(abs((train.density$Density - shift.density$Density))/train.density$Density))
#       # temp <- data.frame(CustomerSiteId = sites[i], YearWeek = site.periods[j+1], NoClusters = max(as.numeric(as.character(train.density$Cluster))), merge(train.density, shift.density, by='Cluster'))
#       rolling.density <- rbind(rolling.density, temp)
#     }
#     
#     site.rolling.density <- rbind(site.rolling.density, rolling.density)
#   }
#   
#   a <- merge(site.rolling.density, with(data.frame(cp.features, Count = 1), aggregate(Count~YearWeek+CustomerSiteId, FUN=sum)), by=c('YearWeek','CustomerSiteId'))
#   
#   
#   # theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
# }
# if(FALSE) {
#   #   ************* NOTE: MAY WANT TO DO THIS WITH A MOVING WINDOW OF THE DATA SET (e.g. THE LAST 52 WEEKS??)
#   #   ************* LOOK INTO DATA SPLITTING USING createTimeSlices INSTEAD OF createDataPartition!!!
#   #   ************* http://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series
#   # -------------------------------------------------------------------------------------------
#   #   start by ordering the data in the cp.clean data frame by date (time-series = ts)
#   cp.clean.ts <- cp.clean[with(cp.clean, order(Date)), ]
#   if(FALSE) {
#     #   next, take the last 26 weeks of data and use create clusters on a rolling-basis
#     year.weeks <- data.frame(YearWeek = unique(cp.clean.ts$YearWeek), ywIndex = seq(1, length(unique(cp.clean.ts$YearWeek)), 1))
#     rolling.summary <- c()
#     for(i in 26:length(year.weeks$YearWeek)) {
#       
#       train.period <- as.character(year.weeks[year.weeks$ywIndex<=i & year.weeks$ywIndex>=(i-25), 'YearWeek'])
#       train.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% train.period, ]
#       #   pre-process the data in this period's training set
#       train.base <- train.df[,c(1:3,8)]
#       train.features <- train.df[,c(4:7)]
#       period.pca.trans <- preProcess(train.features, method = 'pca')
#       train.pca <- predict(period.pca.trans, train.features)
#       
#       k.period <- 15
#       k.fit.period <- kmeans(train.pca, k.period, 100)
#       
#       train.clustered <- data.frame(train.base, train.pca, Cluster = k.fit.period$cluster)
#       train.clustered <- merge(train.clustered, with(data.frame(train.clustered, Frequency = 1), aggregate(Frequency~YearWeek+Cluster, FUN=sum)), by=c('YearWeek','Cluster'))
#       train.clustered <- merge(train.clustered, with(data.frame(train.clustered, Count = 1), aggregate(Count~YearWeek, FUN=sum)), by='YearWeek')
#       train.clustered$Density <- with(train.clustered, Frequency/Count)
#       
#       period.densities <- unique(train.clustered[train.clustered$YearWeek==max(train.period), c('Cluster','Density')])
#       left.out <- seq(1, k.period, 1)[!(seq(1, k.period, 1) %in% period.densities$Cluster)]
#       
#       if(length(left.out) > 0) {
#        
#         period.densities <- rbind(period.densities, data.frame(Cluster = left.out, Density = 0))
#       }
#       
#       period.summary <- do.call(rbind, lapply(1:k.period, function(x) data.frame(YearWeek = max(train.period), Cluster = x, Density = period.densities[period.densities$Cluster==x, 'Density'], Center = sqrt(sum(k.fit.period$centers[x, ]^2)), WithinSS = k.fit.period$withinss[x])))
#       rolling.summary <- rbind(rolling.summary, period.summary)
#     }
#   }
#   
#   # method 1: see how the predicted values of the next week alter the cluster density
#   ### JUST DO A SAMPLE ON THE FIRST "OUTBREAK" WHICH HAPPENED AROUND 2014-33
#   # i <- 63 # 57 (this is the first onset of big EVD68 in 2014)
#   k.period <- 15
#   year.weeks <- data.frame(YearWeek = unique(cp.clean.ts$YearWeek), ywIndex = seq(1, length(unique(cp.clean.ts$YearWeek)), 1))
#   rolling.density <- c()
#   rolling.center <- c()
#   for(i in 26:length(year.weeks$YearWeek)) {
#     
#     train.period <- as.character(year.weeks[year.weeks$ywIndex<=i & year.weeks$ywIndex>=(i-25), 'YearWeek'])
#     train.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% train.period, ]
#     train.base <- train.df[,c(1:3,8)]
#     train.features <- train.df[,c(4:7)]
#     period.pca.trans <- preProcess(train.features, method = 'pca')
#     train.pca <- predict(period.pca.trans, train.features)
#     
#     k.fit.period <- kmeans(train.pca, k.period, 100)
#     train.clustered <- data.frame(train.base, train.pca, Cluster = k.fit.period$cluster)
#     train.clustered$Cluster <- as.factor(train.clustered$Cluster)
#     
#     knn.fit <- train(Cluster~., data=train.clustered[,colnames(train.clustered)[grep('^PC|Cluster',colnames(train.clustered))]], method='knn')
#     
#     test.period <- as.character(year.weeks[year.weeks$ywIndex>i & year.weeks$ywIndex<=(i+3), 'YearWeek'])
#     test.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% test.period, ]
#     test.base <- test.df[,c(1:3,8)]
#     test.features <- test.df[,c(4:7)]
#     test.pca <- predict(period.pca.trans, test.features)
#     test.clustered <- data.frame(test.base, test.pca, Cluster = predict(knn.fit, newdata=test.pca))
#     
#     train.density <- with(data.frame(train.clustered, Record = 1), aggregate(Record~Cluster, FUN=sum))
#     train.density$Density <- train.density$Record/sum(train.density$Record)
#     train.density$Key <- 'train'
#     agg.clustered <- rbind(train.clustered, test.clustered)
#     agg.density <- with(data.frame(agg.clustered, Record = 1), aggregate(Record~Cluster, FUN=sum))
#     agg.density$Density <- agg.density$Record/sum(agg.density$Record)
#     agg.density$Key <- 'agg'
#     density.df <- rbind(train.density, agg.density)
#     # ggplot(a, aes(x=Cluster, y=Density, fill=Key)) + geom_bar(stat='identity', position='dodge') + labs(title=paste('Delta in Cluster Density for Period Ending', max(test.period), sep=' '))
#     temp.density <- spread(density.df[,c(1, 3:4)], key = Key, value = Density)
#     temp.density$Delta <- (temp.density$train - temp.density$agg)^2
#     temp.density$YearWeek <- max(test.period)
#     rolling.density <- rbind(rolling.density, temp.density)
#     
#     # maybe also try with correlation and/or variance of all clusters over time???
#     
#     # method 2: see how the centers of the clusters change from 26 weeks to 27-29 weeks...
#     train.extend.period <- as.character(year.weeks[year.weeks$ywIndex<=(i+2) & year.weeks$ywIndex>=(i-25), 'YearWeek'])
#     train.extend.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% train.extend.period, ]
#     train.extend.base <- train.extend.df[,c(1:3,8)]
#     train.extend.features <- train.extend.df[,c(4:7)]
#     train.extend.pca <- predict(period.pca.trans, train.extend.features)
#     k.fit.extend.period <- kmeans(train.extend.pca, k.period, 100)
#     # train.extend.clustered <- data.frame(train.extend.base, train.extend.pca, Cluster = k.fit.extend.period$cluster)
#     # train.extend.clustered$Cluster <- as.factor(train.extend.clustered$Cluster)
#     
#     cluster.shift <- data.frame(Cluster = rownames(k.fit.period$centers),
#                                 Shift = sqrt((data.frame(k.fit.period$centers)[,1] - data.frame(k.fit.extend.period$centers)[,1])^2 +
#                                                (data.frame(k.fit.period$centers)[,2] - data.frame(k.fit.extend.period$centers)[,2])^2 +
#                                                (data.frame(k.fit.period$centers)[,3] - data.frame(k.fit.extend.period$centers)[,3])^2
#                                              )
#     )
#     
#     temp.shift <- data.frame(YearWeek = max(train.extend.period), cluster.shift)
#     rolling.center <- rbind(rolling.center, temp.shift)
#   }
#   
#   clustered.df <- cp.spread
#   run.ids <- unique(cp.median$RunDataId)
#   cp.ordered <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(cp.median[cp.median$RunDataId==run.ids[x], ][order(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), ], Index = seq(1, length(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), 1))))
#   cp.sequence <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(RunDataId = run.ids[x], Sequence = paste(as.character(cp.ordered[cp.ordered$RunDataId==run.ids[x], 'AssayName']), collapse=', '))))
#   clustered.df$SequenceFlag <- NA
#   clustered.df[clustered.df$RunDataId %in% cp.sequence[grep('^HRV4$|^HRV4, HRV1, HRV2, HRV3$|^HRv4, HRV1, HRV2$|^HRV4, HRV1$', cp.sequence$Sequence), 'RunDataId'], 'SequenceFlag'] <- 'Positive'
#   clustered.df[is.na(clustered.df$SequenceFlag), 'SequenceFlag'] <- 'Negative'
#   clustered.df <- merge(clustered.df, unique(calendar.df[,c('Date','YearWeek')]), by='Date')
#   clustered.df$Record <- 1
#   
#   ggplot(subset(clustered.df, YearWeek >= as.character(year.weeks[year.weeks$ywIndex==26, 'YearWeek'])), aes(x=YearWeek, y=Record, fill=SequenceFlag)) + geom_bar(stat='identity')+ scale_x_discrete(breaks = as.character(unique(clustered.df$YearWeek))[order(as.character(unique(clustered.df$YearWeek)))][seq(1, length(as.character(unique(clustered.df$YearWeek))), 12)]) + labs(title='HRV/EV Positive Test Count with Positive/Negative Sequences for EV-D68\nby Week')
#   ggplot(rolling.density, aes(x=YearWeek, y=Delta, fill=Cluster)) + geom_bar(stat='identity') + scale_x_discrete(breaks = as.character(unique(rolling.density$YearWeek))[order(as.character(unique(rolling.density$YearWeek)))][seq(1, length(as.character(unique(rolling.density$YearWeek))), 12)]) + theme(axis.text.x=element_text(angle=90)) + labs(title='Delta in Densities between Training and Aggregate Data Sets\non Rolling Basis by Week')
#   
#   # method 3: find the frequency of sequences and use twitter's AnomalyDetection package on GitHub
#   sequence.freq <- merge(clustered.df[clustered.df$YearWeek>='2013-26', c('Date','RunDataId')], cp.sequence, by='RunDataId') 
#   sequence.freq <- with(data.frame(sequence.freq, Count = 1), aggregate(Count~Date+Sequence, FUN=sum))
#   
#   # the issue with this is that the count will go up over time due to growth of the data set... so change to a fraction
#   sequence.freq <- merge(sequence.freq, with(sequence.freq, aggregate(Count~Date, FUN=sum)), by='Date')
#   sequence.freq$Rate <- sequence.freq$Count.x/sequence.freq$Count.y
#   sequence.freq <- sequence.freq[,c('Date','Sequence','Rate')]
#   sequence.freq$Date <- as.POSIXct(sequence.freq$Date)
#   AnomalyDetectionTs(sequence.freq[sequence.freq$Sequence=='HRV4, HRV1', c('Date','Rate')], max_anoms=0.01, direction='both', plot=TRUE)
#   
#   # other ideas: attempt potentially something that will just look at how sequences move together... maybe 
#   #              label by sequence and have it use random forest... then see which ones follow a similar path
#   #               until the lowest level (these potentially could be similar)
#   head(cp.spread)
# }
