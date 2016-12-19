setwd('~/FilmArrayTrend/AlertCamilleAboutMissingRuns/')

library(RODBC)
library(sendmailR)

FADWcxn <- odbcConnect(dsn = 'FA_DW', uid = 'afaucett', pwd = 'ThisIsAPassword-BAD')
queryVector <- scan('query.sql',what=character(),quote="")
query <- paste(queryVector,collapse=" ")
results.df <- sqlQuery(FADWcxn,query)
odbcClose(FADWcxn)

if(nrow(results.df) > 0) {
  
  write.csv(results.df, '~/RunsNotUpdating.csv', row.names = FALSE)
  
  from <- 'aimie.faucett@biofiredx.com'
  to <- 'camille.cook@biofiredx.com'
  subject <- 'Institutions not Uploading Runs'
  body <- 'Based on analysis of run data, the following institutions may not be uploading their runs (see attachment).'
  mailControl <- list(smtpServer="webmail.biofiredx.com")
  attachmentPath <- '~/RunsNotUpdating.csv'
  attachmentName <- 'DataAlerts.csv'
  attachmentObject <- mime_part(x=attachmentPath, name=attachmentName)
  bodyWithAttachment <- list(body, attachmentObject)
  sendmail(from=from, to=to, subject=subject, msg=bodyWithAttachment, control=mailControl)
}

rm(list=ls())