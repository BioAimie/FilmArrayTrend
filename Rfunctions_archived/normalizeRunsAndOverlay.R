# This function takes in the following parameters:
# 1. dataFrame - This is a data frame of RP runs that must have a Year, Week, YearWeek, DayOfYear, CustomerSiteId, SiteName, Runs, and Days columns
# 2. calendarFrame - This is a data frame that contains Year, Week, YearWeek, and Days columns 
# 3. cdcFrame - This is a data frame that contains CDC reported data, including Year, Week, Region, iliTotal, totalPatients, and iliRate
# 4. keyFrame - This is a data frame that contains the CustomerSiteId and corresponding Region
# 5. national - This is a binary parameter that tells the function whether national or site data are wanted

normalizeRunsAndOverlay <- function(dataFrame, calendarFrame, cdcFrame, keyFrame, byRuns = TRUE, national = FALSE) {
  
  # based on whether the national parameter is true or false, find the right cdc data to include
  if(national) {
    
    cdcFrame.agg <- with(cdcFrame, aggregate(cbind(iliTotal, totalPatients)~Year+Week, FUN=sum))
    cdcFrame.agg[,'iliRate'] <- with(cdcFrame.agg, iliTotal/totalPatients)
  } else {
    
    cdcFrame.agg <- merge(keyFrame, cdcFrame, by='Region')
  }
  
  # determine the current year, how many sites are in the dataFrame, and how many days are in each week
  thisYear <- year(Sys.Date())
  calendar.agg <- with(calendarFrame, aggregate(Days~Year+Week+YearWeek, FUN=sum))
  sites <- as.character(unique(dataFrame[,'CustomerSiteId']))
  
  # for each site, center the runs and days about the week of interest (one week on either side) and find a run rate in the period
  normFrame <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    name <- as.character(unique(dataFrame[dataFrame[,'CustomerSiteId'] == site, 'SiteName']))
    siteFrame <- dataFrame[dataFrame[,'CustomerSiteId'] == site, ]
    
    if(byRuns == FALSE) {
      
      siteFrame[,'Runs'] <- with(siteFrame, ifelse(PositiveAssays > 0, 1, 0))
    }
    
    siteFrame.agg <- with(siteFrame, aggregate(Runs~Year+Week+YearWeek+CustomerSiteId+SiteName, FUN=sum))
    
    # ------------------- ADDED
    siteFrame.agg[,'Record'] <- 1
    
    # determine which years and sites can be kept for the analysis
    years <- as.character(unique(siteFrame.agg[,'Year']))
    metaData <- data.frame(Year = years,
                           weekCount = sapply(1:length(years), function(x) sum(siteFrame.agg[siteFrame.agg[,'Year'] == years[x], 'Record'])))
    thisYear <- year(Sys.Date())
    metaData[,'keepYear'] <- with(metaData, ifelse(weekCount < 26 & Year != thisYear, 0, 1))
    
    if(length(metaData[,'Year']) >= 2) {
     
      metaData[metaData[,'Year'] == thisYear,'keepYear'] <- ifelse(metaData[metaData[,'Year'] == thisYear - 1, 'keepYear'] == 1, 1, 0)
      goodYears <- metaData[metaData[,'keepYear'] == 1, 'Year']
    } else {
      
      goodYears <- c()
    }
    # if there are no 'good' years in the site data set, move to the next site
    if(length(goodYears) == 0) { 
      
      print(paste(site, ' does not have enough good year data to be included!', sep=''))
      next() 
    } else {
      
      startDate <- min(as.character(siteFrame[siteFrame[,'Year'] == min(as.character(goodYears)),'YearWeek']))
      siteFrame.agg <- merge(calendar.agg[as.character(calendar.agg[,'YearWeek']) >= startDate, ], siteFrame.agg, by=c('Year','Week','YearWeek'), all.x=TRUE)
      siteFrame.agg[is.na(siteFrame.agg[,'CustomerSiteId']), 'CustomerSiteId'] <- site
      siteFrame.agg[is.na(siteFrame.agg[,'SiteName']), 'SiteName'] <- name
      siteFrame.agg[is.na(siteFrame.agg[,'Runs']),'Runs'] <- 0
      siteFrame.agg <- siteFrame.agg[with(siteFrame.agg, order(Year, Week)), ]
      centeredFrame <- data.frame(Year = siteFrame.agg[2:(length(siteFrame.agg[,1])-1),'Year'],
                                  Week = siteFrame.agg[2:(length(siteFrame.agg[,1])-1),'Week'],
                                  YearWeek = siteFrame.agg[2:(length(siteFrame.agg[,1])-1),'YearWeek'],
                                  CustomerSiteId = siteFrame.agg[2:(length(siteFrame.agg[,1])-1),'CustomerSiteId'],
                                  SiteName = siteFrame.agg[2:(length(siteFrame.agg[,1])-1),'SiteName'],
                                  Days = sapply(2:(length(siteFrame.agg[,1])-1), function(x) sum(siteFrame.agg[(x-1):(x+1), 'Days'])),
                                  Runs = sapply(2:(length(siteFrame.agg[,1])-1), function(x) sum(siteFrame.agg[(x-1):(x+1), 'Runs'])))
      centeredFrame[,'RunRate'] <- with(centeredFrame, Runs/Days)
    
      # for each year, find the minimum RunRate, unless the year is this year and the day of year < 183, then normalize the RunRate
      # by the minimum RunRate
      years <- unique(centeredFrame[,'Year'])
      metaData <- data.frame(Year = years, 
                             MinRate = sapply(1:length(years), function(x) min(centeredFrame[centeredFrame[,'Year'] == years[x], 'RunRate'])),
                             MaxDays = sapply(1:length(years), function(x) max(siteFrame[siteFrame[,'Year'] == years[x], 'DayOfYear'])))
    
      metaData[,'MinRate'] <- with(metaData, ifelse(Year == thisYear & MaxDays < 183, metaData[metaData[,'Year'] == thisYear - 1, 'MinRate'], MinRate))
      centeredFrame <- merge(centeredFrame, metaData[,c('Year','MinRate')], by='Year')
      centeredFrame[,'NormRunRate'] <- with(centeredFrame, ifelse(MinRate == 0, 0, RunRate/MinRate))
    
      # if the data are wanted by site, then merge the cdc data by region onto the centeredFrame
      if(national == FALSE) {
      
        centeredFrame <- merge(centeredFrame, cdcFrame.agg[,c('Year','Week','CustomerSiteId','iliRate')], by=c('Year','Week','CustomerSiteId'))
      } 
    
    normFrame <- rbind(normFrame, centeredFrame)
    }
  }
  
  # if the data are wanted as a national picture, then it is necessary to do an averaging
  if(national) {
  
    normFrame <- with(normFrame, aggregate(NormRunRate~Year+Week+YearWeek, FUN=mean))
    normFrame <- merge(normFrame, cdcFrame.agg[,c('Year','Week','iliRate')], by=c('Year','Week'), all.x=TRUE)
  }
  
  return(normFrame)
}