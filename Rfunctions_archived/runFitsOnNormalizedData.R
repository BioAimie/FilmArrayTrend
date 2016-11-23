runFitsOnNormalizedData <- function(dataFrame, decoderFrame, isNorm = FALSE, makeByYear = FALSE, makeNational = FALSE) {
  
  thisYear <- year(Sys.Date())
  
  # if the site should be ignored, then override the CustomerSiteId so there is only one
  if(makeNational) {
    
    dataFrame[,'CustomerSiteId'] <- 'National'
    sites <- 'National'
  } else {
    
    sites <- as.character(unique(dataFrame[,'CustomerSiteId']))
  }
  
  # determine the number of organims used in the anlaysis
  bugCols <- colnames(dataFrame)[colnames(dataFrame) %in% letters[1:21]]
  
  # iterate through each site
  out <- c()
  for(i in 1:length(sites)) {
    
    site <- sites[i]
    
    if(makeNational) {
      
      name <- 'National'
    } else {
    
      name <- as.character(unique(dataFrame[dataFrame[,'CustomerSiteId'] == site, 'SiteName']))
    }
    
    siteFrame <- dataFrame[dataFrame[,'CustomerSiteId'] == site, ]
  
    if(makeByYear) {
      
      years <- as.character(unique(siteFrame[,'Year']))
      years <- years[years != thisYear]
      
      for(j in 1:length(years)) {
        
        year <- years[j]
        yearFrame <- siteFrame[siteFrame[,'Year'] == year, ]
        
        # if the analysis should be done for normalized data, then fit NormRuns, otherwise fit Runs
        if(isNorm) {
          
          # yearFit <- lm(NormRuns~., data = yearFrame[,c('NormRuns',letters[1:21])])
          yearFit <- lm(NormRuns~., data = yearFrame[,c('NormRuns',bugCols)])
          
        } else {
          
          # yearFit <- lm(Runs~., data = yearFrame[,c('Runs',letters[1:21])])
          yearFit <- lm(Runs~., data = yearFrame[,c('Runs',bugCols)])
        }
        # from the yearFit pull out the p-values for each variable
        pValues <- data.frame(summary(yearFit)$coeff)[,4]
        
        # divvy up the pValues into quantiles... recall that the fit includes the Intercept as entry 1 and should be neglected
        l <- length(pValues)
        quants <- quantile(pValues, probs = c(0,0.2,0.4,0.6,0.8,1))
        pValues <- data.frame(Var = rownames(summary(yearFit)$coeff)[2:l],
                              Bin = cut(pValues, breaks = quants, include.lowest = TRUE, right = TRUE, labels = seq(1,5,1))[2:l],
                              Value = pValues[2:l])
  
        # loop through the Bins in the pValues data frame and make a fit by each bin
        bins <- as.character(unique(pValues[,'Bin']))[order(as.character(unique(pValues[,'Bin'])))]
        for(k in 1:length(bins)) {
          
          # use the bins formed above to pull out variables in the fit by p-Value (ascending)
          # if the analysis should be done for normalized data, then fit NormRuns, otherwise fit Runs
          if(isNorm) {
            
            form.str <- paste('NormRuns', paste(pValues[as.character(pValues[,'Bin']) <= bins[k], 'Var'], collapse = '+'), sep='~')
          } else {
            
            form.str <- paste('Runs', paste(pValues[as.character(pValues[,'Bin']) <= bins[k], 'Var'], collapse = '+'), sep='~')
          }
          fit <- lm(as.formula(form.str), data = yearFrame)  
          
          if(k == 1) { 
            yearFit1 <- fit
          } else if(k == 2) { 
            yearFit2 <- fit
          } else if(k == 3) { 
            yearFit3 <- fit
          } else if(k == 4) { 
            yearFit4 <- fit
          } else { 
            yearFit5 <- fit
          }
        }
          
        # now compare the fits and determine which on is the "best," meaning that the change in p-value from one to the next is
        # significant (< 0.05)
        anovaResults <- data.frame(anova(yearFit1, yearFit2, yearFit2, yearFit3, yearFit4, yearFit5))
        rowIndex <- which(anovaResults[,1] == min(anovaResults[anovaResults[,6] < 0.05 & !(is.na(anovaResults[,6])), 1], na.rm=TRUE))
        rowIndex <- ifelse(length(rowIndex) == 0, 1, rowIndex)
        binCodes <- substr(pValues[as.character(pValues[,'Bin']) <= rowIndex, 'Var'], 1, 1)
        binBugs <- as.character((decoderFrame[decoderFrame[,'bugId'] %in% binCodes, 'bugName']))
          
        rSquared <- switch(rowIndex,
                           '1' = summary(yearFit1)$r.squared,
                           '2' = summary(yearFit2)$r.squared,
                           '3' = summary(yearFit3)$r.squared,
                           '4' = summary(yearFit4)$r.squared,
                           '5' = summary(yearFit5)$r.squared
        )
          
        temp <- data.frame(siteId = site, siteName = name, Year = year, bestFit = rowIndex, r2 = rSquared, bugId = binCodes, bugName = binBugs, record = 1)
        out <- rbind(out, temp)
      }
    }
    
    # if the year is not of interest, then just do the fit over all years instead of by each year
    else {
    
        # if the analysis should be done for normalized data, then fit NormRuns, otherwise fit Runs
        if(isNorm) {
          
          # siteFit <- lm(NormRuns~., data = siteFrame[,c('NormRuns',letters[1:21])])
          siteFit <- lm(NormRuns~., data = siteFrame[,c('NormRuns',bugCols)])
        } else {
          
          # siteFit <- lm(Runs~., data = siteFrame[,c('Runs',letters[1:21])])
          siteFit <- lm(Runs~., data = siteFrame[,c('Runs',bugCols)])
        }
        # from the siteFit pull out the p-values for each variable
        pValues <- data.frame(summary(siteFit)$coeff)[,4]
        
        # divvy up the pValues into quantiles... recall that the fit includes the Intercept as entry 1 and should be neglected
        l <- length(pValues)
        quants <- quantile(pValues, probs = c(0,0.2,0.4,0.6,0.8,1))
        pValues <- data.frame(Var = rownames(summary(siteFit)$coeff)[2:l],
                              Bin = cut(pValues, breaks = quants, include.lowest = TRUE, right = TRUE, labels = seq(1,5,1))[2:l],
                              Value = pValues[2:l])
        return(pValues)
        # loop through the Bins in the pValues data frame and make a fit by each bin
        bins <- as.character(unique(pValues[,'Bin']))[order(as.character(unique(pValues[,'Bin'])))]
        for(k in 1:length(bins)) {
          
          # use the bins formed above to pull out variables in the fit by p-Value (ascending)
          # if the analysis should be done for normalized data, then fit NormRuns, otherwise fit Runs
          if(isNorm) {
            
            form.str <- paste('NormRuns', paste(pValues[as.character(pValues[,'Bin']) <= bins[k], 'Var'], collapse = '+'), sep='~')
          } else {
            
            form.str <- paste('Runs', paste(pValues[as.character(pValues[,'Bin']) <= bins[k], 'Var'], collapse = '+'), sep='~')
          }
          fit <- lm(as.formula(form.str), data = siteFrame)  
          
          if(k == 1) { 
            siteFit1 <- fit
          } else if(k == 2) { 
            siteFit2 <- fit
          } else if(k == 3) { 
            siteFit3 <- fit
          } else if(k == 4) { 
            siteFit4 <- fit
          } else { 
            siteFit5 <- fit
          }
        }
    
        # now compare the fits and determine which on is the "best," meaning that the change in p-value from one to the next is
        # significant (< 0.05)
        anovaResults <- data.frame(anova(siteFit1, siteFit2, siteFit2, siteFit3, siteFit4, siteFit5))
        rowIndex <- which(anovaResults[,1] == min(anovaResults[anovaResults[,6] < 0.05 & !(is.na(anovaResults[,6])), 1], na.rm=TRUE))
        rowIndex <- ifelse(length(rowIndex) == 0, 1, rowIndex)
        binCodes <- substr(pValues[as.character(pValues[,'Bin']) <= rowIndex, 'Var'], 1, 1)
        binBugs <- as.character((decoderFrame[decoderFrame[,'bugId'] %in% binCodes, 'bugName']))
        
        rSquared <- switch(rowIndex,
                           '1' = summary(siteFit1)$r.squared,
                           '2' = summary(siteFit2)$r.squared,
                           '3' = summary(siteFit3)$r.squared,
                           '4' = summary(siteFit4)$r.squared,
                           '5' = summary(siteFit5)$r.squared
        )
        
        temp <- data.frame(siteId = site, siteName = name, bestFit = rowIndex, r2 = rSquared, bugId = binCodes, bugName = binBugs, record = 1)
        out <- rbind(out, temp)
    }
  }
  
  return(out)
}