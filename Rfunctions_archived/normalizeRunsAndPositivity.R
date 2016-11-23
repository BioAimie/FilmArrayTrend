# This function expects a data frame that has the following:
# YearWeek, Year, Week, CustomerSiteId, SiteName, Runs, Record, and bugs by code (a-u)
normalizeRunsAndPositivity <- function(formattedBugFrame, makeNational = FALSE) {
  
  # loop through each site
  if(makeNational) {
    
    formattedBugFrame[,'CustomerSiteId'] <- 'National'
    sites <- 'National'
  } else {
  
    sites <- as.character(unique(formattedBugFrame[,'CustomerSiteId']))
  }
  
  norm <- c()
  for(i in 1:length(sites)) {
   
    site <- sites[i]
    siteFrame <- formattedBugFrame[formattedBugFrame[,'CustomerSiteId'] == site, ]
    
    # the run count needs to be normalized to be between 0 and 1 so normalize by the max runs in the year
    years <- as.character(unique(siteFrame[,'Year']))
    l <- length(years)
    normalizer <- data.frame(Year = years,
                             Norm = sapply(1:l, function(x) max(siteFrame[siteFrame[,'Year'] == years[x], 'Runs'])))
    siteFrame <- merge(siteFrame, normalizer, by='Year')
    siteFrame[,'NormRuns'] <- siteFrame[,'Runs']/siteFrame[,'Norm']
    siteFrame[,letters[1:21]] <- siteFrame[,letters[1:21]]/siteFrame[,'Runs']
    
    # trim the data frame
    siteFrame <- siteFrame[,c('YearWeek','Year','Week','CustomerSiteId','SiteName','Runs','NormRuns', letters[1:21])]
    norm <- rbind(norm, siteFrame)
  }  
  
  return(norm)
}



