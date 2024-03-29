#Created 05/04/23  to combine two data frames byrows and columns
#slight modification 05/06/23 to create column names if missing
#think about adding a key variable instead of using colnames
#need to check for all unique x[key.variable,] and y[key.variable,]
vJoin <- function(x,y, rnames=TRUE,cnames=TRUE,key.name=NULL) {
  n.x <- NCOL(x)
  n.y <- NCOL(y)
  n.obsx <- NROW(x)
  n.obsy <- NROW(y)
  if(!is.null(key.name)) {
     all.unique.x <- all.unique.y<- NA
     if(key.name %in% colnames(x))   all.unique.x <- sum(duplicated(x[,key.name]))
     if(key.name %in% colnames(y)) all.unique.y <- sum(duplicated(y[,key.name]))
     if (all.unique.x != 0 ) stop  ("key.name values in x  must be unique")
     if (all.unique.y != 0  ) stop ("key.name values in y must be unique")
     rnx <- rownames(x)
     rny <- rownames(y)
    #  x.ord <- order(x[,key.name])
#      x <- x[x.ord,]
#      y.ord <- order(y[,key.name])
#      y <- y[y.ord,]
     rownames(x) <- x[,key.name]
     rownames(y) <- y[,key.name]
     x <- cbind(x,rnx)
     y <- cbind(y,rny)
  }
  if(is.null(colnames(x))) colnames(x) <- paste0("x",1:n.x)
  if(is.null(colnames(y))) if(cnames) {colnames(y) <- paste0("y",1:n.y) 
          } else {colnames(y) <- paste0("x",1:n.y)}
  if(is.null(rownames(x))) rownames(x) <- paste0("Sx",1:n.obsx)
   if(is.null(rownames(y))) rownames(y) <- paste0("Sy",1:n.obsy)
  xy <- unique(c(colnames(x),colnames(y)))
  n.xy.col <- length(xy)
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
  return(new)
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
       