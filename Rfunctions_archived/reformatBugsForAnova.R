reformatBugsForAnova <- function(runFrame, bugsFrame, dateHolder) {
  
  sites <- as.character(unique(bugsFrame[,'CustomerSiteId']))
  bugs <- as.character(unique(bugsFrame[,'Bug']))
  
  # count the number of positive tests
  runFrame[,'PositiveAssays'] <- with(runFrame, ifelse(PositiveAssays > 0, 1, 0))
  runFrame.agg <- with(runFrame, aggregate(cbind(Runs,PositiveAssays)~Year+Week+YearWeek+CustomerSiteId+SiteName, FUN=sum))
  
  # for each site rotate the count of bugs into their own column
  out <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    name <- as.character(unique(bugsFrame[bugsFrame[,'CustomerSiteId'] == site, 'SiteName']))
    siteFrame <- bugsFrame[bugsFrame[,'CustomerSiteId'] == site, ]
    
    siteRuns.agg <- runFrame.agg[runFrame.agg[,'CustomerSiteId'] == site, ]
    siteRuns.agg[,'Record'] <- 1
    
    # determine which years and sites can be kept for the analysis
    years <- as.character(unique(siteRuns.agg[,'Year']))
    metaData <- data.frame(Year = years,
                           weekCount = sapply(1:length(years), function(x) sum(siteRuns.agg[siteRuns.agg[,'Year'] == years[x], 'Record'])))
    thisYear <- year(Sys.Date())
    metaData[,'keepYear'] <- with(metaData, ifelse(weekCount < 26 & Year != thisYear, 0, 1))
    metaData[metaData[,'Year'] == thisYear,'keepYear'] <- ifelse(metaData[metaData[,'Year'] == thisYear - 1, 'keepYear'] == 1, 1, 0)
    goodYears <- metaData[metaData[,'keepYear'] == 1, 'Year']
    
    # if there are no 'good' years in the site data set, move to the next site
    if(length(goodYears) == 0) { 
      
      next() 
    } else {
    
      startDate <- min(as.character(siteFrame[siteFrame[,'Year'] == min(as.character(goodYears)),'YearWeek']))
      baseFrame <- siteRuns.agg[as.character(siteRuns.agg[,'YearWeek']) >= startDate, ]
      baseFrame <- merge(unique(dateHolder[as.character(dateHolder[,'YearWeek']) >= startDate ,c('YearWeek','Year','Week')]), baseFrame, by=c('YearWeek','Year','Week'), all.x=TRUE)
      baseFrame[is.na(baseFrame[,'CustomerSiteId']), 'CustomerSiteId'] <- site
      baseFrame[is.na(baseFrame[,'SiteName']), 'SiteName'] <- name
      baseFrame[is.na(baseFrame[,'Runs']), 'Runs'] <- 0
      
      for(j in 1:length(bugs)) {
    
        # make a name for the new column that will be pushed into the data frame
        bugCol <- bugs[j]
      
        bugFrame <- siteFrame[siteFrame[,'Bug'] == bugs[j], ]
      
        # if the bug doesn't exist in the data set, skip to the next bug
        if(length(bugFrame[,1]) == 0 ) {
        
          baseFrame[,bugCol] <- 0  
        } else {
        
          # otherwise, find the total count of bugs in the period
          bugFrame.agg <- with(bugFrame, aggregate(BugCount~YearWeek, FUN=sum))
          baseFrame <- merge(baseFrame, bugFrame.agg, by='YearWeek', all.x=TRUE)
          baseFrame[is.na(baseFrame[,'BugCount']),'BugCount'] <- 0
          colnames(baseFrame)[grep('BugCount', colnames(baseFrame))] <- bugCol
        }
      }
    }
    out <- rbind(out, baseFrame)
  }
  
  return(out)
}