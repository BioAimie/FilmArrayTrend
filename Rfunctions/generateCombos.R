generateCombos <- function(catVec, n) {
 
  possible.combos <- choose(length(catVec), n)
  store <- data.frame(Combo = paste(catVec[1:n], collapse='+'))
  
  ptm <- proc.time()
  while(length(store$Combo) < possible.combos) {
    
    Combo <- data.frame(Combo = paste(catVec[sample.int(length(catVec), n, replace=FALSE)], collapse='+'))
    
    if(length(grep(Combo, as.character(store$Combo))) == 0) {
      
      store <- rbind(store, Combo)
    } 
  }
  print(proc.time() - ptm)
  return(store)
}