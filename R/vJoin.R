#Created 05/04/23  to combine two data frames byrows and columns
#slight modification 05/06/23 to create column names if missing
#think about adding a key variable instead of using colnames
#need to check for all unique x[key.variable,] and y[key.variable,]
#added the by option 12/28/24
#Finally got it to work 2/19/25

vJoin <- function(x,y, rnames=TRUE,cnames=TRUE,key.name=NULL,by=NULL) {

  n.x <- NCOL(x)
  n.y <- NCOL(y)
  n.obsx <- NROW(x)
  n.obsy <- NROW(y)
 
  if(!is.null(key.name)) { #match based upon key.name instead of rownames
     all.unique.x <- all.unique.y<- NA
     if(key.name %in% colnames(x))   all.unique.x <- sum(duplicated(x[,key.name]))
     if(key.name %in% colnames(y)) all.unique.y <- sum(duplicated(y[,key.name]))
     if (all.unique.x != 0 ) stop  ("key.name values in x  must be unique")
     if (all.unique.y != 0  ) stop ("key.name values in y must be unique")
     rnx <- rownames(x)
     rny <- rownames(y)
 
     rownames(x) <- x[,key.name]
     rownames(y) <- y[,key.name]
     x <- cbind(x,rnx)  #keeps the original rownames for checking
     y <- cbind(y,rny)
  }     #end key.name 

  #make sure the grouping variable is upper case
  x.by <- x
  y.by <- y
  if(!is.null(by )){
   x.by[,by] <- toupper(x[,by])
   y.by[,by] <- toupper(y[,by]) 
   }
   
   #find the cases where the by variables match
   if (!is.null(by) ) {
    by.x <- table(x.by[,by])
    by.y <- table(y.by[,by])
    matched.by <- names(by.x) %in% names(by.y)
    by.x <- names(by.x)[matched.by]
    n.matches <- length(by.x)
   n.obs <- sum(by.y)   #number of cases that match in y
   }  else {n.obs <- NROW(y)}
    
   if(is.null(colnames(x.by))) colnames(x.by) <- paste0("x",1:n.x)
  if(is.null(colnames(y.by))) if(cnames) {colnames(y.by) <- paste0("y",1:n.y) 
          } else {colnames(y.by) <- paste0("x",1:n.y)}
          
    if(is.null(rownames(x.by))) rownames(x.by) <- paste0("Sx",1:n.obsx)
   if(is.null(rownames(y.by))) rownames(y.by) <- paste0("Sy",1:n.obsy)
    
  xy <- unique(c(colnames(x.by),colnames(y.by)))
  n.xy.col <- length(xy)
  final <- data.frame(matrix(NA, ncol=n.xy.col,nrow=0))
  colnames(final) <- xy
   if(is.null(by)) n.matches=1
  for(i in 1:n.matches) {
  # repeat the next steps for all n.matches
  if(!is.null(by)) {
x <- x.by[x.by[, "study"] == by.x[i], ]
y <- y.by[y.by[, "study"] == by.x[i], ]
  } else {
  x <- x.by
  y <- y.by}

 
  
  if(is.null(rownames(x))) rownames(x) <- paste0("Sx",1:n.obsx)
  if(is.null(rownames(y))) rownames(y) <- paste0("Sy",1:n.obsy)
 
  if(!rnames) { new.names <- n.obsx + 1:n.obsy
		rownames(y) <- new.names}
  xy.rows  <- unique(c(rownames(x),rownames(y)))
  n.xy.rows <- length(xy.rows)
  
  #now, put them together -- matching on row and column names
  new <- data.frame(matrix(NA, ncol=n.xy.col,nrow = n.xy.rows))
  colnames(new) <- xy
  rownames(new) <- xy.rows
  new[rownames(x),colnames(x)] <- x[rownames(x),colnames(x)]
  new[rownames(y),colnames(y)] <- y[rownames(y),colnames(y)]
 
  final <- rbind(final,new)
  }
  return(final)
  }
  
  
  
combineMatrices <- function(x,y, r=NULL) {
#check dimensionality
if(NROW(x) != NROW(y)) {stop( "y needs the same number of rows as x") }
colnames(y) <- paste0("P.",colnames(y))
result <- cbind(x,y)
ncy <- NCOL(y)
if(is.null(r)) {y <- rbind(y,diag(ncy))} else {y <- rbind(y,r)}
result <-rbind(result,t(y))
colnames(result)  <- rownames(result)
return(result)}
       