setwd('~/FilmArrayTrend/EnteroD68/')

# load the neccessary libraries
library(RODBC)
library(lubridate)
library(ggplot2)
library(devtools)
require(dateManip)
library(cluster)
library(caret)
library(dbscan)
library(C50)
library(tidyr)
library(dplyr)
library(plot3D)
library(AnomalyDetection)

# create an Epi date calendar that will be used by all the data sets
startYear <- 2013
calendar.df <- createCalendarLikeMicrosoft(startYear, 'Week')
calendar.df <- transformToEpiWeeks(calendar.df)
calendar.df$YearWeek <- with(calendar.df, ifelse(Week < 10, paste(Year, Week, sep='-0'), paste(Year, Week, sep='-')))

# set up some constants
imgDir <- 'Figures/'
dateBreaks <- unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])[order(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek']))][seq(1, length(unique(calendar.df[calendar.df$Year >= startYear, 'YearWeek'])), 8)]

# set some query variables, like the customer site... also, get the number of RP runs by site
FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('../DataSources/SQL/EnteroD68/sitesRunningRP.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
sites.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('../DataSources/SQL/EnteroD68/rpRunsBySite.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# start a loop to gather Cp data for all sites running RP
cp.df <- c()
choose.sites <- as.character(sites.df[,'CustomerSiteId'])
for(j in 1:length(choose.sites)) {
  
  FADWcxn <- odbcConnect('FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD') 
  queryVector <- scan('../DataSources/SQL/EnteroD68/rhinoDataBySite.sql', what=character(), quote="")
  query <- paste(gsub('SITE_INDEX', choose.sites[j], queryVector), collapse=" ")
  cp.site.df <- sqlQuery(FADWcxn, query)
  odbcClose(FADWcxn)
  
  cp.df <- rbind(cp.df, cp.site.df)
}

rm(cp.site.df)

# first, clean up the data to be in a format that will work for feature analysis (machine learning)
# ===========================================================================================
#   with the cp data, determine the median Cp of each assay in the HRV/EV target
cp.median <- aggregate(Cp~RunDataId+CustomerSiteId+Date+AssayName, FUN=median, data=cp.df)
cp.spread <- spread(data = cp.median, key = AssayName, value = Cp)
sparse.handler <- 40
cp.spread[,c(4:9)][is.na(cp.spread[,c(4:9)])] <- sparse.handler

#   only consider "good" data (assume a start of mid-2013)
cp.clean <- merge(cp.spread, filter(calendar.df[,c('Date','YearWeek')], YearWeek >= '2013-26'), by='Date')

#   check to see if any of the features have near-zero variance (i.e. variables with very few unique values, which can skew results when data are split for train/test)
nzv <- nearZeroVar(cp.clean[,c(4:9)], saveMetrics = TRUE)
remove.vars <- row.names(nzv[nzv$nzv==TRUE,])
cp.clean <- cp.clean[,!(colnames(cp.clean) %in% remove.vars)]

#   check to see if any of the variables have very strong correlation (cut off of 0.8)
keep.vars <- cor(cp.clean[,c(4:7)])[,-findCorrelation(cor(cp.clean[,c(4:7)]), cutoff=0.8)]
cp.clean <- cp.clean[,colnames(cp.clean) %in% c('RunDataId','Date','YearWeek','CustomerSiteId',row.names(keep.vars))]

# second, try a variety of methods for predicting anomalies using machine learning on different data set constructions
# ===========================================================================================
if(FALSE) {
  # Method 1: Include all data (do not split into train and test using createDataPartition from the caret package)
  set.seed(3456)
  if(FALSE) {
    # trainIndex <- createDataPartition(cp.clean$RunDataId, p=0.6, list=FALSE, times=1)
    # cp.train <- cp.clean[trainIndex, ]
    # cp.test <- cp.clean[-trainIndex, ]
    # base.train <- cp.clean[trainIndex, c('RunDataId','Date','YearWeek','CustomerSiteId')] 
    # features.train <- cp.clean[trainIndex, row.names(keep.vars)]
    # base.test <- cp.clean[-trainIndex, c('RunDataId','Date','YearWeek','CustomerSiteId')]
    # features.test <- cp.clean[-trainIndex, row.names(keep.vars)]
  } # if the data do need to be split into train and test....
  base <- cp.clean[, c('RunDataId','Date','YearWeek','CustomerSiteId')] 
  features <- cp.clean[, row.names(keep.vars)] 
  
  #   preprocess the data using PCA option in caret... use the transformation on the training and test sets
  preProcValues <- preProcess(features, method = 'pca')
  features.pca <- predict(preProcValues, features)
  
  #   some resampling could be done, but I will not do that at this point because the number of "positives" may be small and diluted by resampling
  #     if resampling is performed on the training set, perhaps use bootstrap to randomly sample (fitControl function)
  
  #   use k-means to cluster and then label the clusters
  if(FALSE) {
    # wss <- (nrow(features.train.trans)-1)*sum(apply(features.train.trans, 2, var))
    # max.clusters <- 20
    # for (i in 2:max.clusters) wss[i] <- sum(kmeans(features.train.trans, centers=i)$withinss)
    # plot(1:max.clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    # k <- min(which(sapply(2:length(wss), function(x) (wss[x-1] - wss[x])/wss[x-1]) < 0)) - 1
    # k.fit <- kmeans(features.train.trans, centers=k, nstart=10, iter.max=100)
    # features.train.trans$Label <- k.fit$cluster
    # k.labels <- data.frame(Label = seq(1, k, 1), Class = letters[1:k])
    # features.train.trans <- merge(features.train.trans, k.labels, by='Label')[,c(2:5)]
  } # if the clustering is performed on a training set rather than the whole set
  wss <- (nrow(features.pca)-1)*sum(apply(features.pca, 2, var))
  max.clusters <- 20
  for (i in 2:max.clusters) wss[i] <- sum(kmeans(features.pca, centers=i)$withinss)
  plot(1:max.clusters, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
  k <- min(which(sapply(2:length(wss), function(x) (wss[x-1] - wss[x])/wss[x-1]) < 0)) - 1
  k.fit <- kmeans(features.pca, centers=k, nstart=10, iter.max=100)
  features.pca$Label <- k.fit$cluster
  k.labels <- data.frame(Label = seq(1, k, 1), Class = letters[1:k])
  
  #   bind the feature data back to the base data set and generate a signiture for each run
  clustered.df <- cbind(base, features.pca)
  run.ids <- unique(cp.median$RunDataId)
  cp.ordered <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(cp.median[cp.median$RunDataId==run.ids[x], ][order(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), ], Index = seq(1, length(cp.median[cp.median$RunDataId==run.ids[x], 'Cp']), 1))))
  cp.sequence <- do.call(rbind, lapply(1:length(run.ids), function(x) data.frame(RunDataId = run.ids[x], Sequence = paste(as.character(cp.ordered[cp.ordered$RunDataId==run.ids[x], 'AssayName']), collapse=', '))))
  clustered.df$SequenceFlag <- NA
  clustered.df[clustered.df$RunDataId %in% cp.sequence[grep('^HRV4$|^HRV4, HRV1, HRV2, HRV3$|^HRv4, HRV1, HRV2$|^HRV4, HRV1$', cp.sequence$Sequence), 'RunDataId'], 'SequenceFlag'] <- 'Positive'
  clustered.df[is.na(clustered.df$SequenceFlag), 'SequenceFlag'] <- 'Negative'
  clustered.df <- merge(clustered.df, k.labels, by='Label')
  
  clustered.df$Record <- 1
  
  ggplot(clustered.df, aes(x=Date, y=Record, fill=SequenceFlag)) + geom_bar(stat='identity')
  ggplot(subset(clustered.df, SequenceFlag=='Positive'), aes(x=Date, y=Record, fill=Class)) + geom_bar(stat='identity')
  with(with(clustered.df, aggregate(Record~Class, FUN=sum)), plot(x=Class, y=Record))
  par(mfrow=c(3,1))
  with(with(subset(clustered.df, year(Date)==2014), aggregate(Record~Class, FUN=sum)), plot(x=Class, y=Record))
  with(with(subset(clustered.df, year(Date)==2015), aggregate(Record~Class, FUN=sum)), plot(x=Class, y=Record))
  with(with(subset(clustered.df, year(Date)==2016), aggregate(Record~Class, FUN=sum)), plot(x=Class, y=Record))
  par(mfrow=c(1,1))
  with(subset(clustered.df, year(Date)==2014 & SequenceFlag=='Positive'), aggregate(Record~Class, FUN=sum))
  with(subset(clustered.df, year(Date)==2015 & SequenceFlag=='Positive'), aggregate(Record~Class, FUN=sum))
  with(subset(clustered.df, year(Date)==2016 & SequenceFlag=='Positive'), aggregate(Record~Class, FUN=sum))
  
  a <- with(clustered.df, aggregate(Record~YearWeek+Class, FUN=sum))
  b <- with(clustered.df, aggregate(Record~YearWeek, FUN=sum))
  d <- merge(a, b, by='YearWeek')
  d$Density <- with(d, Record.x/Record.y)
  ggplot(d, aes(x=YearWeek, y=Density, group=Class, color=Class)) + geom_line(size=1.25) + facet_wrap(~Class)
  
  s <- merge(clustered.df, cp.sequence, by='RunDataId')
}

#   ************* NOTE: MAY WANT TO DO THIS WITH A MOVING WINDOW OF THE DATA SET (e.g. THE LAST 52 WEEKS??)
#   ************* LOOK INTO DATA SPLITTING USING createTimeSlices INSTEAD OF createDataPartition!!!
#   ************* http://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series
# -------------------------------------------------------------------------------------------
#   start by ordering the data in the cp.clean data frame by date (time-series = ts)
cp.clean.ts <- cp.clean[with(cp.clean, order(Date)), ]
if(FALSE) {
  #   next, take the last 26 weeks of data and use create clusters on a rolling-basis
  year.weeks <- data.frame(YearWeek = unique(cp.clean.ts$YearWeek), ywIndex = seq(1, length(unique(cp.clean.ts$YearWeek)), 1))
  rolling.summary <- c()
  for(i in 26:length(year.weeks$YearWeek)) {
    
    train.period <- as.character(year.weeks[year.weeks$ywIndex<=i & year.weeks$ywIndex>=(i-25), 'YearWeek'])
    train.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% train.period, ]
    #   pre-process the data in this period's training set
    train.base <- train.df[,c(1:3,8)]
    train.features <- train.df[,c(4:7)]
    period.pca.trans <- preProcess(train.features, method = 'pca')
    train.pca <- predict(period.pca.trans, train.features)
    
    # if(i==26) {
    #   wss.period <- (nrow(train.pca)-1)*sum(apply(train.pca, 2, var))
    #   max.clusters <- 20
    #   for (j in 2:max.clusters) wss.period[j] <- sum(kmeans(train.pca, centers=j)$withinss)
    #   # plot(1:max.clusters, wss.period, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
    #   k.period <- min(which(sapply(2:length(wss.period), function(x) (wss.period[x-1] - wss.period[x])/wss.period[x-1]) < 0)) - 1
    # }
    k.period <- 15
    k.fit.period <- kmeans(train.pca, k.period, 100)
    
    train.clustered <- data.frame(train.base, train.pca, Cluster = k.fit.period$cluster)
    train.clustered <- merge(train.clustered, with(data.frame(train.clustered, Frequency = 1), aggregate(Frequency~YearWeek+Cluster, FUN=sum)), by=c('YearWeek','Cluster'))
    train.clustered <- merge(train.clustered, with(data.frame(train.clustered, Count = 1), aggregate(Count~YearWeek, FUN=sum)), by='YearWeek')
    train.clustered$Density <- with(train.clustered, Frequency/Count)
    
    period.densities <- unique(train.clustered[train.clustered$YearWeek==max(train.period), c('Cluster','Density')])
    left.out <- seq(1, k.period, 1)[!(seq(1, k.period, 1) %in% period.densities$Cluster)]
    
    if(length(left.out) > 0) {
     
      period.densities <- rbind(period.densities, data.frame(Cluster = left.out, Density = 0))
    }
    
    period.summary <- do.call(rbind, lapply(1:k.period, function(x) data.frame(YearWeek = max(train.period), Cluster = x, Density = period.densities[period.densities$Cluster==x, 'Density'], Center = sqrt(sum(k.fit.period$centers[x, ]^2)), WithinSS = k.fit.period$withinss[x])))
    rolling.summary <- rbind(rolling.summary, period.summary)
  }
}

# method 1: see how the predicted values of the next week alter the cluster density
### JUST DO A SAMPLE ON THE FIRST "OUTBREAK" WHICH HAPPENED AROUND 2014-33
# i <- 63 # 57 (this is the first onset of big EVD68 in 2014)
k.period <- 15
year.weeks <- data.frame(YearWeek = unique(cp.clean.ts$YearWeek), ywIndex = seq(1, length(unique(cp.clean.ts$YearWeek)), 1))
rolling.density <- c()
for(i in 26:length(year.weeks$YearWeek)) {
  
  train.period <- as.character(year.weeks[year.weeks$ywIndex<=i & year.weeks$ywIndex>=(i-25), 'YearWeek'])
  train.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% train.period, ]
  train.base <- train.df[,c(1:3,8)]
  train.features <- train.df[,c(4:7)]
  period.pca.trans <- preProcess(train.features, method = 'pca')
  train.pca <- predict(period.pca.trans, train.features)
  
  k.fit.period <- kmeans(train.pca, k.period, 100)
  train.clustered <- data.frame(train.base, train.pca, Cluster = k.fit.period$cluster)
  train.clustered$Cluster <- as.factor(train.clustered$Cluster)
  
  knn.fit <- train(Cluster~., data=train.clustered[,colnames(train.clustered)[grep('^PC|Cluster',colnames(train.clustered))]], method='knn')
  
  test.period <- as.character(year.weeks[year.weeks$ywIndex>i & year.weeks$ywIndex<=(i+3), 'YearWeek'])
  test.df <- cp.clean.ts[cp.clean.ts$YearWeek %in% test.period, ]
  test.base <- test.df[,c(1:3,8)]
  test.features <- test.df[,c(4:7)]
  test.pca <- predict(period.pca.trans, test.features)
  test.clustered <- data.frame(test.base, test.pca, Cluster = predict(knn.fit, newdata=test.pca))
  
  train.density <- with(data.frame(train.clustered, Record = 1), aggregate(Record~Cluster, FUN=sum))
  train.density$Density <- train.density$Record/sum(train.density$Record)
  train.density$Key <- 'train'
  agg.clustered <- rbind(train.clustered, test.clustered)
  agg.density <- with(data.frame(agg.clustered, Record = 1), aggregate(Record~Cluster, FUN=sum))
  agg.density$Density <- agg.density$Record/sum(agg.density$Record)
  agg.density$Key <- 'agg'
  a <- rbind(train.density, agg.density)
  # ggplot(a, aes(x=Cluster, y=Density, fill=Key)) + geom_bar(stat='identity', position='dodge') + labs(title=paste('Delta in Cluster Density for Period Ending', max(test.period), sep=' '))
  temp <- spread(a[,c(1, 3:4)], key = Key, value = Density)
  temp$Delta <- (temp$train - temp$agg)^2
  temp$YearWeek <- max(test.period)
  rolling.density <- rbind(rolling.density, temp)
  
  # maybe also try with correlation and/or variance of all clusters over time???
}
ggplot(subset(clustered.df, YearWeek >= as.character(year.weeks[year.weeks$ywIndex==26, 'YearWeek'])), aes(x=YearWeek, y=Record, fill=SequenceFlag)) + geom_bar(stat='identity')+ scale_x_discrete(breaks = as.character(unique(clustered.df$YearWeek))[order(as.character(unique(clustered.df$YearWeek)))][seq(1, length(as.character(unique(clustered.df$YearWeek))), 12)]) + labs(title='HRV/EV Positive Test Count with Positive/Negative Sequences for EV-D68\nby Week')
ggplot(rolling.density, aes(x=YearWeek, y=Delta, fill=Cluster)) + geom_bar(stat='identity') + scale_x_discrete(breaks = as.character(unique(rolling.density$YearWeek))[order(as.character(unique(rolling.density$YearWeek)))][seq(1, length(as.character(unique(rolling.density$YearWeek))), 12)]) + theme(axis.text.x=element_text(angle=90)) + labs(title='Delta in Densities between Training and Aggregate Data Sets\non Rolling Basis by Week')

# method 2: see how the centers of the clusters change from 26 weeks to 27-29 weeks...

# method 3: find the frequency of sequences and use twitter's AnomalyDetection package on GitHub
sequence.freq <- merge(cp.clean.ts, cp.sequence, by='RunDataId')
sequence.freq <- with(data.frame(sequence.freq, Count = 1), aggregate(Count~Date+Sequence, FUN=sum))

