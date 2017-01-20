library(tidyr)
library(dplyr)
library(cluster)
library(ggplot2)
library(RODBC)

setwd()

FADWcxn <- odbcConnect()
queryVector <- scan('sitesRunningRP.txt',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
sites.df <- sqlQuery(FADWcxn,query)
queryVector <- scan('rpRunsBySite.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
runs.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

# start a loop to gather Cp data for all sites running RP
cp.df <- c()
choose.sites <- as.character(sites.df[,'CustomerSiteId'])
for(j in 1:length(choose.sites)) {
  
  FADWcxn <- odbcConnect('FilmArrayTrend', uid = 'fadwcollector', pwd = 'DataCollector%')
  queryVector <- scan('rpDataBySite.sql', what=character(), quote="")
  query <- paste(gsub('SITE_INDEX', choose.sites[j], queryVector), collapse=" ")
  cp.site.df <- sqlQuery(FADWcxn, query)
  odbcClose(FADWcxn)
  
  cp.df <- rbind(cp.df, cp.site.df)
}


# setwd("G:/Departments/BioChem/People/Andrew Wallin/R/Aimie Faucett/New folder/")
# load("TrendDataHRV.Rda")
cp.df.HRV <- cp.df[cp.df$TargetName == "Human Rhinovirus/Enterovirus",]

# Get data frame in spreaded format
HRV.spread <- aggregate(Cp~RunDataId+AssayName, data = cp.df.HRV, FUN = median)
HRV.spread <- spread(HRV.spread, AssayName, Cp) 
HRV.spread[,c(2:7)][is.na(HRV.spread[,c(2:7)])] <- 32

modelData <- HRV.spread

mydata <- modelData[,-1]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

#Running Kmeans Model
set.seed(8)
kModel <- kmeans(modelData[,-1],12 ,nstart = 10, iter.max = 100)

clusterData <- cbind(modelData, kModel$cluster)
names(clusterData)[length(clusterData)] <- "Cluster"
comboData <- inner_join(cp.df.HRV[,c("RunDataId","Date")], clusterData, by = "RunDataId")
names(comboData)[length(names(comboData))] <- "Cluster"
comboData <- comboData[!duplicated(interaction(comboData$RunDataId, comboData$Cluster)),]
comboData$Month <-  as.POSIXlt(as.Date(comboData$Date, "%Y-%m-%d"))
comboData$Month <- strftime(comboData$Month, format = "%Y-%W")

spreadTable <-  as.data.frame(table(comboData$Month, comboData$Cluster))
names(spreadTable) <- c("Date", "Cluster", "Freq")
spreadTable <- spread(spreadTable,Cluster, Freq)

#Making Frequency dataframe of clusters over time
for(i in 1:nrow(spreadTable)){
  
  sumRow = sum(spreadTable[i,c(2:length(spreadTable))])
  spreadTable[i,c(2:length(spreadTable))] <- spreadTable[i,c(2:length(spreadTable))] / sumRow
  
}

gathered <- gather(spreadTable, Cluster, Freq, 2:length(spreadTable))
gathered$Date <- unlist(lapply(gathered$Date, as.character))

g <- ggplot(gathered,aes(x = Date, y = Freq, colour = Cluster , group = Cluster)) + geom_line() + 
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Rate of Clusters", x = "Date (Year-Week Number)", y = "Rate")
plotly::plotly_build(g)

signature <- 1

#set Seed for reproducible results
set.seed(1)

# Splitting Data for train and test of models
s <- sample(nrow(HRV.spread[,-1]), .6 * nrow(HRV.spread[,-1]))
train <- clusterData[s,]
test <- clusterData[-s,]

trainData  <- train
trainData$Call <- NA
trainData$Call[trainData$Cluster == signature] <- "Positive"
trainData$Call[trainData$Cluster != signature] <- "Negative"

#Making C5.0 Model
randomModel <- C50::C5.0(x = trainData[,c(2:7)], y = as.factor(trainData[,9]))

testData <- test

testData$Call <- NA
testData$Call[testData$Cluster== signature] <- "Positive"
testData$Call[testData$Cluster != signature] <- "Negative"

testData$Prediction = predict(randomModel,test)

#Accuracy 
paste(100-(100*(length(which(testData$Call != testData$Prediction))/nrow(testData))),"%")


predFreq <- testData[,c(1,9,10)]

JoinedData <- inner_join(comboData[,c(1,10)],predFreq, by = "RunDataId")
JoinedData <- inner_join(runs.df[,c(1,2)], JoinedData, by = "RunDataId")
JoinedData <- JoinedData[!duplicated(interaction(JoinedData$RunDataId, JoinedData$Call)),]

tableData <- as.data.frame(table(JoinedData$Month,JoinedData$CustomerSiteId,JoinedData$Prediction))

names(tableData) <- c("Date","Site" ,"Call", "Freq")

#Dates for ploting 
Dates <- unique(tableData$Date)[seq(1,204,12)]
indexDates <- seq(1,204,12)


plotData <- tableData[tableData$Call == "Positive",]

g2 <- ggplot(data = plotData[,-2]) + 
  geom_line(  aes(x = Date, y = Freq), colour = "#217a74") + 
  theme(axis.text.x = element_text(angle = 90, vjust = .35),
        text = element_text(size =10)) + 
  scale_x_discrete(breaks = Dates, label = Dates) +
  labs(title = "Count of Predicted Signature", y = "Count", x = "Date (Year-Week Number") +
  facet_wrap(~Site) 

############### tune c5.0 ######################
library(caret)
library(C50)
library(mlbench)

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10, returnResamp="all")

# Choose the features and classes

x <- trainData[,c(2:7)]
y <- as.factor(trainData[,9])

grid <- expand.grid( .winnow = c(TRUE,FALSE), .trials=c(1,5,10,15,20), .model="tree" )

mdl<- train(x=x,y=y,tuneGrid=grid,trControl=fitControl,method="C5.0",verbose=FALSE)

mdl

# visualize the resample distributions
xyplot(mdl,type = c("g", "p", "smooth"))



