
file.move <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.rename(from = from,  to = to)
}

regionalReportRP <- function(dsnName, serverUser, serverPW, outputDir, packageDir, scriptDir){
  repo <- "http://cran.r-project.org/"
  #package load with path
  if(!any(list.files(packageDir) == "rmarkdown")){install.packages("rmarkdown", lib = packageDir, repos = repo, type = "binary")}
  library(rmarkdown, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "RODBC")){install.packages("RODBC", lib = packageDir, repos = repo, type = "binary")}
  library(RODBC, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "knitr")){install.packages("knitr", lib = packageDir, repos = repo, type = "binary")}
  library(knitr, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "xtable")){install.packages("xtable", lib = packageDir, repos = repo, type = "binary")}
  library(xtable, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "zoo")){install.packages("zoo", lib = packageDir, repos = repo, type = "binary")}
  library(zoo, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "maps")){install.packages("maps", lib = packageDir, repos = repo, type = "binary")}
  library(maps, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "ggplot2")){install.packages("ggplot2", lib = packageDir, repos = repo, type = "binary")}
  library(ggplot2, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "dplyr")){install.packages("dplyr", lib = packageDir, repos = repo, type = "binary")}
  library(dplyr, lib.loc = packageDir)
  if(!any(list.files(packageDir) == "tidyr")){install.packages("tidyr", lib = packageDir, repos = repo, type = "binary")}
  library(tidyr, lib.loc = packageDir)

  options(useFancyQuotes = FALSE)
  
   
  
  trendConnect <- odbcConnect(dsnName,serverUser, serverPW)
  
  startWeekData <- sqlQuery(trendConnect, paste("  
                                              SELECT *
                                              FROM [FADataWarehouse].[dbo].[EpiWeeks] E 
                                              WHERE E.Date =",sQuote(Sys.Date() -7) ,sep = " "))
  
  close(trendConnect)
  
  inputYear = startWeekData$EpiYear
  inputWeek = startWeekData$EpiWeek
  
  startDateFolder = as.Date(paste(inputYear, inputWeek,"1", sep = "-"), format = "%Y-%W-%w")
  
  startDate = format(startDateFolder, format= "%B %d %Y")
  
  # for each type of car in the data create a report
  # these reports are saved in output_dir with the name specified by output_file
  # Set working directory
  
  for (region in c('Midwest', 'Northeast', 'West', 'South')){
    #for pdf reports
    
    rmarkdown::render(input = paste(scriptDir,"/RegionalReportLaffy.Rmd",sep = ""),
                      output_format = "pdf_document",
                      output_file = paste("report_Region_",region,"_", Sys.Date(), ".pdf", sep='')
    )
    
    file.move(paste("report_Region_",region,"_", Sys.Date(), ".pdf", sep=''),
              paste(outputDir,paste("/report_Region_",region,"_", Sys.Date(), ".pdf", sep=""), sep=""))
    
  }
}
