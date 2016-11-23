integrateToFindSeason <- function(cdcData, sitesToInclude, yearsToInclude, regionKeys, makeNational = TRUE) {
  
  regions <- as.character(regionKeys[regionKeys[,'CustomerSiteId'] %in% sitesToInclude, 'Region'])
  dataFrame <- cdcData[cdcData[,'Region'] %in% regions, ]
  
  if(makeNational) {
    
    dataFrame[,'Region'] <- 'National'
    dataFrame <- with(dataFrame, aggregate(iliRate~Year+Week+Region, FUN=mean))
    regions <- 'National'
  }
  
  out <- c()
  for(i in 1:length(regions)) {
  
    region <- regions[i]
    regionFrame <- dataFrame[dataFrame[,'Region'] == region, ]
    
    for(j in 1:length(yearsToInclude)) {
      
      year <- yearsToInclude[j]
      yearFrame <- regionFrame[regionFrame[,'Year'] == year, ]
      
      # find the earliest week that has the minimum rate of ILI in the dataset for that region for that year
      minWeek <- min(yearFrame[which(yearFrame[,'iliRate'] == min(yearFrame[,'iliRate'])),'Week'])
      
      # reorder the weeks about the peak
      yearFrame[,'seasonWeek'] <- with(yearFrame, ifelse(Week - minWeek >= 0, Week - minWeek + 1, 53 - abs(Week - minWeek)))
      
      # now that the weeks have been reordered, interatively integrate 13 weeks under the curve (centered)
      yearFrame <- yearFrame[with(yearFrame, order(seasonWeek)), ]
      l <- length(yearFrame[,'seasonWeek'])
      int <- sapply(2:l, function(x) (yearFrame[x,'iliRate'] + yearFrame[(x-1),'iliRate'])/2)
      int <- c(0, int)
      yearFrame <- cbind(yearFrame, int)
      yearFrame[,'area'] <- sapply(1:l, function(x) sum(yearFrame[1:x, 'int']))
      seasonFrame <- data.frame(seasonWeek = 7:(l-6),
                                seasonArea = sapply(7:(l-6), function(x) sum(yearFrame[(x-6):(x+6), 'int'])))
      peakWeek <- seasonFrame[min(which(seasonFrame[,'seasonArea'] == max(seasonFrame[,'seasonArea']))),'seasonWeek']      
      
      # the maximum area under the curve is centered around the peak week as described above
      yearFrame[,'iliSeason'] <- with(yearFrame, ifelse(abs(seasonWeek - peakWeek) < 6, 'yes', 'no'))
      out <- rbind(out, yearFrame)
    }
  }
  
  return(out)
}