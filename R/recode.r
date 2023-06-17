#added 5/6/23 
 #code is truly ugly
 "recode" <- function(x,where,isvalue,newvalue) {
  if (missing(where))  where <-1:NCOL(x)
    all.values <- unique(unlist(x[,where]))
    if(!any(all.values %in% isvalue)) {stop ("data has more values than specified in isvalue")
    }
  maxv <- max(isvalue)
  minv <- min(isvalue)
  tempv <- isvalue + maxv + minv
  x[,where ]<- x[,where] + maxv + minv  #make these larger to allow swaps in place
  for (k in 1:length(where)) {where1 <- where[k]  #begin the where lop
 	 for(j in 1:NROW(x)) {   #replace across all cases
		for (i in (1: length(isvalue)) ) {#search one column for each possible value
		if(!is.na(x[j,where1])) {
 		 if((x[j,where1] == tempv[i])) x[j,where1]   <- newvalue[i]}
 		 } #end of values loop
  		}  #end of subjects loop
  		}  #end of where loop
  return(x)
}