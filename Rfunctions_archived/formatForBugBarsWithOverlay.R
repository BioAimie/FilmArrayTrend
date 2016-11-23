formatForBugBarsWithOverlay <- function(dataFrame = master.nat, decoderFrame = decoder) {
  
  bugIds <- as.character(unique(decoderFrame[,'bugId']))
  
  out <- c()
  for(i in 1:length(bugIds)) {
    
    bugFrame <- dataFrame[,c('YearWeek','Year','Week','iliSeason','NormRuns','NormRunRate','Runs', bugIds[i])]
    colnames(bugFrame) <- c('YearWeek','Year','Week','iliSeason','iliRate','NormRunRate','Runs', 'Positivity')
    Bug <- decoderFrame[decoderFrame[,'bugId'] == bugIds[i], 'bugName']
    bugFrame[,'Key'] <- Bug
    out <- rbind(out, bugFrame)
  }
  
  return(out)
}