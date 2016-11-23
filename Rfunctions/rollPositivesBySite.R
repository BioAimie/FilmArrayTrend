rollPositivesBySite <- function(bugFrame, runFrame, bugColName, bugColCount, siteId, listOfBugs) {
  
  siteBugs <- bugFrame[bugFrame[,'CustomerSiteId'] == siteId, ]
  l <- length(listOfBugs)
  colNamesEasy <- letters[1:l]
  
  # NEED TO ADD IN THE ROLLING OF THE POSITIVES!!!
  outFrame <- subset(runFrame, CustomerSiteId == siteId)
  
  for(i in 1:length(listOfBugs)) {
    
    temp <- siteBugs[siteBugs[,bugColName] == listOfBugs[i], c('YearWeek', bugColCount)]
    
    if(length(temp[,1]) == 0) {
      
      outFrame[,colNamesEasy[i]] <- 0
    } else {
      
      outFrame <- merge(outFrame, temp, by='YearWeek')
      colnames(outFrame)[length(colnames(outFrame))] <- colNamesEasy[i] 
    }
  }
  
  roll.mat <- do.call(cbind, lapply(1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(outFrame))), function(x) sapply(2:(length(outFrame[,'YearWeek'])-1), function(y) sum(outFrame[,letters[x]][(y-1):(y+1)]))))
  roll.bugs <- as.data.frame(roll.mat)
  colnames(roll.bugs) <- colNamesEasy
  Runs <- sapply(2:(length(outFrame[,'YearWeek'])-1), function(x) sum(outFrame[(x-1):(x+1), 'Runs']))
  replaceCols <- colnames(outFrame)[grep(paste('Runs|^', paste(letters, collapse = '|^'), sep=''), colnames(outFrame))]
  outFrame <- cbind(outFrame[2:(length(outFrame[,'YearWeek'])-1),!(colnames(outFrame) %in% replaceCols)], Runs, roll.bugs)
  return(outFrame)
}