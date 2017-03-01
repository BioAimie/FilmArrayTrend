generateCombos <- function(catVec, n, lmFormat=TRUE) {
  
  # determine the number of combinations given a vector of unique variables and selecting n items at a time
  possible.combos <- choose(length(catVec), n)
  
  # find the unique combinations and store in a list
  store.list <- combn(catVec, n, simplify = FALSE)
  
  if(lmFormat) {
    
    store <- do.call(rbind, lapply(1:possible.combos, function(x) data.frame(Combo = paste(as.character(store.list[[x]]), collapse='+'))))
    return(store)
  } else {
    
    store.list <- lapply(1:possible.combos, function(x) paste0(as.character(store.list[[x]])))
    return(store.list)
  }
}