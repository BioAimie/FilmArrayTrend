layerOrganismsToDetermineFit <- function(prevFrame, bugList) {
  
  colAliases <- letters[1:length(bugList)] 
  
  # to begin, include all variables in the model and determine which fit best with the rate of ILI as reported by the CDC
  # holding all other variables constant
  fit.all <- lm(formula = as.formula(paste("Rate", paste(letters[1:length(bugList)], collapse = "+"), sep = "~")), data = prevFrame)
  fit.all.info <- summary(fit.all)$coeff[2:(length(bugList)+1), ]

  coeff <- data.frame(bugAlias = names(fit.all.info[,1][order(unname(fit.all.info[,1]), decreasing=TRUE)]), coeff = seq(length(bugList), 1, -1))
  stder <- data.frame(bugAlias = names(fit.all.info[,2][order(unname(fit.all.info[,2]), decreasing=TRUE)]), stder = seq(length(bugList), 1, -1))
  tval <- data.frame(bugAlias = names(fit.all.info[,3][order(unname(fit.all.info[,3]), decreasing=TRUE)]), tval = seq(length(bugList), 1, -1))
  pval <- data.frame(bugAlias = names(fit.all.info[,4][order(unname(fit.all.info[,4]), decreasing=FALSE)]), pval = seq(length(bugList), 1, -1))
  corr <- data.frame(bugAlias = row.names(data.frame(cor(prevFrame[,c(colAliases,'Rate')], method='pearson')[,'Rate'][order(unname(cor(prevFrame[,c(colAliases,'Rate')], method='pearson')[,'Rate']), decreasing=TRUE)][2:length(cor(prevFrame[,c(colAliases,'Rate')], method='pearson')[,'Rate'])])), corr = seq(length(bugList), 1, -1))
  adjR2 <- c()
  for(i in 1:length(grep(paste('^', paste(letters, collapse = '|^'), sep=''), colnames(prevFrame)))) {
    
    r2 <- summary(lm(as.formula(paste('Rate', letters[i], sep='~')), prevFrame))$adj.r.squared
    temp <- data.frame(bugAlias = letters[i], R2 = r2)
    adjR2 <- rbind(adjR2, temp)
  }
  adjR2 <- data.frame(bugAlias = as.character(adjR2[with(adjR2, order(R2, decreasing = TRUE)), 'bugAlias']), adjR2 = seq(length(bugList), 1, -1))
  
  # rank the variables based on the coefficient, the standard deviation of the sample distrubtion, the t-value, and the p-value of the fit.all (no intercept)
  # weight the adjusted R2 and the correlation between the variable and the Rate higher than the other values (*2)
  var.rank <- merge(merge(merge(merge(merge(coeff, stder, by='bugAlias'), tval, by='bugAlias'), pval, by='bugAlias'), corr, by='bugAlias'), adjR2, by='bugAlias')
  var.rank$score <- with(var.rank, corr+2*adjR2)
  var.rank <- var.rank[with(var.rank, order(score, decreasing = TRUE)), ]
  var.rank$rank <- seq(1, length(bugList), 1) 
  
  # implement a forward selection and backward elinimation algorithm
  l <- length(bugList)
  
  # perform fits with all variables taking away only one per iteration and ANOVA it against the fit.all to see if the removal of the variable is important
  back.anova <- c()
  for(i in 0:(l-1)) {
    
    strip.bug <- var.rank[(l-i), 'bugAlias']
    include.bugs <- colAliases[!(colAliases %in% strip.bug)]
    fit <- lm(as.formula(paste('Rate',paste(include.bugs, collapse='+'), sep='~')), prevFrame)
    temp <- data.frame(bugAlias = var.rank[(l-i),'bugAlias'], pAnova = anova(fit, fit.all)[[6]][2])
    back.anova <- rbind(back.anova, temp)
  }
  back.anova <- back.anova[with(back.anova, order(pAnova, decreasing = FALSE)), ]
  
  # based on the backward removal and the rank (which is best guess for forward selection), choose a starting variable
  var.rank <- merge(var.rank, back.anova, by='bugAlias')
  if(length(var.rank[var.rank$pAnova < 0.01, 'rank']) > 0) {
    
    base.bug <- as.character(var.rank[var.rank$rank == min(var.rank[var.rank$pAnova < 0.01, 'rank']), 'bugAlias'])
  } else {
    
    stop('This model is irregular and not handled well by this function. Please investigate.')
  }
  
  # create the base fit
  fit.base <- lm(as.formula(paste('Rate', base.bug, sep='~')), prevFrame)
  
  # layer each variable on top of the base fit and determine which are 'best' and 'worst'
  vars.check <- as.character(var.rank[as.character(var.rank$bugAlias) != base.bug, ][with(var.rank[as.character(var.rank$bugAlias) != base.bug, ], order(rank)), 'bugAlias'])
  anova.check <- c()
  for(i in 1:length(vars.check)) {
    
    fit <- lm(as.formula(paste('Rate', paste(base.bug, vars.check[i], sep='+'), sep='~')), prevFrame)
    temp <- data.frame(bugAlias = vars.check[i], pAnova = anova(fit.base, fit)[[6]][2])
    anova.check <- rbind(anova.check, temp)
  }
  
  var.next <- as.character(anova.check[anova.check$pAnova == min(anova.check$pAnova), 'bugAlias'])
  vars.unsure <- as.character(anova.check[with(anova.check, order(pAnova)), 'bugAlias'])
  vars.unsure <- vars.unsure[vars.unsure != var.next]
  
  # layer on the next 'best' variable
  fit.1 <- lm(as.formula(paste('Rate', paste(base.bug, var.next, sep='+'), sep='~')), prevFrame)
  
  # loop through the other variables again, but this time, layer each onto the fit.1 model
  anova.check <- c()
  for(i in 1:length(vars.unsure)) {
    
    fit <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.unsure[i], sep='+'), sep='~')), prevFrame)
    temp <- data.frame(bugAlias = vars.unsure[i], pAnova = anova(fit.base, fit.1, fit)[[6]][3])
    anova.check <- rbind(anova.check, temp)
  }
  anova.check <- anova.check[with(anova.check, order(pAnova)), ]
  
  # based on this analysis, pick the next three 'best' variables
  vars.pool.1 <- as.character(anova.check[1:3, 'bugAlias'])
  fit.one <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.pool.1[1], sep='+'), sep='~')), prevFrame)
  fit.two <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.pool.1[1], vars.pool.1[2], sep='+'), sep='~')), prevFrame)
  fit.two.alt <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.pool.1[1], vars.pool.1[3], sep='+'), sep='~')), prevFrame)
  fit.three <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.pool.1[1], vars.pool.1[2], vars.pool.1[3], sep='+'), sep='~')), prevFrame)
  fit.three.alt <- lm(as.formula(paste('Rate', paste(base.bug, var.next, vars.pool.1[1], vars.pool.1[3], vars.pool.1[2], sep='+'), sep='~')), prevFrame)
  
  return(anova(fit.1, fit.one, fit.two.alt, fit.three.alt))
  
  
  vars.unsure <- as.character(var.rank[var.rank$rank <= 6 & var.rank$pAnova >= 0.05, 'bugAlias'])
  
  # for variables that are ranked highly and are statistically significant when removed serially, rank these 
  vars.f1 <- as.character(var.rank[var.rank$rank <= 6 & var.rank$pAnova < 0.05, ][with(var.rank[var.rank$rank <= 6 & var.rank$pAnova < 0.05, ], order(pAnova)), 'bugAlias'])
  if(length(vars.f1) < 2) { 
    
    stop('This model is irregular and not handled well by this function. Please investigate.')
  }
  
  # before running models, get the correlation between the model with all and the Rate as well as the adjusted R2 value
  r2.all <- summary(fit.all)$adj.r.squared
  corr.all <- cor(prevFrame$Rate, fitted(fit.all))
  
  # create the first forward selection model
  fit.f1 <- lm(as.formula(paste('Rate', paste(0,paste(as.character(vars.f1), collapse='+'),sep='+'), sep='~')), prevFrame)
  
  # check to see if all the variables in the model are statistically significant, if so, then check the adjusted R2 value as well as the correlation between the predicted
  # values of the model and the Rate... if there are variables that are not significant, then add these to the vars.unsure list
  vars.f1.check <- row.names(data.frame(summary(fit.f1)$coeff)[data.frame(summary(fit.f1)$coeff)$Pr...t.. < 0.05, ])
  if(length(vars.f1.check) == length(vars.f1)) {
    
    r2.f1 <- summary(fit.f1)$adj.r.squared
    corr.f1 <- cor(prevFrame$Rate, fitted(fit.f1))
    # if the coefficient of determination and the correlation for the model are within 90% of the accuracy of the model with all variables, then return this model
    if(r2.f1 >= 0.9*r2.all & corr.f1 >= 0.9*corr.all) { return(fit.f1) }
    
  } else if(length(vars.f1.check < 2)) {
    
    stop('This model is irregular and not handled well by this function. Please investigate.')
  } else {
    
    vars.unsure <- c(vars.unsure, vars.f1[!(vars.f1 %in% vars.f1.check)])
    fit.f1 <- lm(as.formula(paste('Rate', paste(0,paste(as.character(vars.f1.check), collapse='+'),sep='+'), sep='~')), prevFrame)
    vars.f1 <- vars.f1.check
  }
    
  # scrape the var.rank table to look for any variables that have pAnova less than 0.05 but are not included in the fit.f1 model
  vars.unsure <- c(vars.unsure, as.character(var.rank[var.rank$pAnova < 0.05, 'bugAlias'])[!(as.character(var.rank[var.rank$pAnova < 0.05, 'bugAlias']) %in% vars.f1)])
  
  # scrape the var.rank table and look for any variables that are not significant and have a rank outside of the top 10, then, layer each of these onto the fit.f1 and check to 
  # see if any of them significantly improve the model
  vars.scrub <- as.character(var.rank[var.rank$rank > 10 & var.rank$pAnova >= 0.05, 'bugAlias'])
  scrub.check <- c()
  for(i in 1:length(vars.scrub)) {
    
    fit <- lm(as.formula(paste('Rate', paste(paste(0, paste(vars.f1, collapse='+'), sep='+'), vars.scrub[i], sep='+'), sep='~')), prevFrame)
    temp <- data.frame(bugAlias = vars.scrub[i], pAnova = anova(fit.f1, fit)[[6]][2])
    scrub.check <- rbind(scrub.check, temp)
  } 
  
  return(scrub.check)
}

