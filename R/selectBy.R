
#written May 20, 2023
"selectBy" <- function(x,by) {#use a quasi formula input

   by <- gsub(" ","", by)   #this removes the spaces
     
   if(grepl("\\|",by)) {  AND <- FALSE
                    bb <- unlist(strsplit(by,"\\|"))} else {  #note to search for a | we have to escape it!
     
                    AND  <- TRUE
                     if(grepl(",",by)) {
                       bb <- unlist(strsplit(by,","))} else {
                       bb <- unlist(strsplit(by,"&"))} }
                      
    n.by <- length(bb)
    by <- isvalue <- notvalue <- lessthan <- morethan <- matrix(NA,ncol= n.by)
   
    eq <- grep("=",bb)   #find which operators were used
    lt <- grep("<",bb)   #returns a vector
    gr <- grep(">",bb)
    ne  <- grep("!=",bb)
   #prepare the relevant search parameters
    if(length(eq ) >0)  {temp <- unlist(strsplit(bb[eq],"="))
               by[eq] <- temp[1]
               isvalue[eq] <-as.numeric( temp[2])
               } 
    if( length(lt)  >0)  {temp <- unlist(strsplit(bb[lt],"<"))
               by[lt] <- temp[1]
               lessthan[lt] <- as.numeric( temp[2])
               } 
     if(length(gr) >0)  {temp <- unlist(strsplit(bb[gr],">"))
               by[gr] <- temp[1]
               morethan[gr] <- as.numeric( temp[2])
               } 
      if(length(ne) >0)  {temp <- unlist(strsplit(bb[eq],"!="))
               by[ne] <- temp[1]
               notvalue[ne] <-as.numeric( temp[2])
               } 

#make sure that the variable names are correct
 if(!all(by %in% colnames(x)))  {
                                 cat("\n Offending variables are ",by[!by %in% colnames(x) ],"\n")
                                 stop("Variables specified do not match the variables in the data. \nFix the names and try again")}  
#do this on y which serves as  pointers to x, rather than x  #then combine the pointers for & (and) | (or)
y <- matrix(TRUE,nrow=NROW(x), ncol=n.by)

for(i in 1:length(by)) {
	if(!is.na(isvalue[,i])) y[,i] <-  x[,by[i]]==isvalue[i]
	if(!is.na(notvalue[,i])) y[,i] <- (x[,by[i]]!= notvalue[i])
	if(!is.na(lessthan[,i])) y[,i] <- (x[,by[i]]< lessthan[i])
	if(!is.na(morethan[,i])) y[,i] <- (x[,by[i]] >  morethan[i])
}
if(AND ) {y <-apply(y,1,all)} else {y <- apply(y,1,any)} 
y[is.na(y) ] <- FALSE
return(x[y,])
}


#written May 20, 2023
"splitBy" <- function(x,by,new=FALSE) {#use a quasi formula input

   by <- gsub(" ","", by)   #this removes the spaces
     
   bb <- unlist(strsplit(by,",")) 
                     
                      
    n.by <- length(bb)
    by <- isvalue <- notvalue <- lessthan <- morethan <- matrix(NA,ncol= n.by)
   
    eq <- grep("=",bb)   #find which operators were used
    lt <- grep("<",bb)   #returns a vector
    gr <- grep(">",bb)
    ne  <- grep("!=",bb)
   #prepare the relevant search parameters
    if(length(eq ) >0)  {temp <- unlist(strsplit(bb[eq],"="))
               by[eq] <- temp[1]
               isvalue[eq] <-as.numeric( temp[2])
               } 
    if( length(lt)  >0)  {temp <- unlist(strsplit(bb[lt],"<"))
               by[lt] <- temp[1]
               lessthan[lt] <- as.numeric( temp[2])
               } 
     if(length(gr) >0)  {temp <- unlist(strsplit(bb[gr],">"))
               by[gr] <- temp[1]
               morethan[gr] <- as.numeric( temp[2])
               } 
      if(length(ne) >0)  {temp <- unlist(strsplit(bb[eq],"!="))
               by[ne] <- temp[1]
               notvalue[ne] <-as.numeric( temp[2])
               } 
     
#do this on y which serves as  pointers to x, rather than x  #then combine the pointers for & (and) | (or)
if(!all(by %in% colnames(x)))  {
                                 cat("\n Offending variables are ",by[!by %in% colnames(x) ],"\n")
                                 stop("Variables specified do not match the variables in the data. \nFix the names and try again")}  

y <- matrix(TRUE,nrow=NROW(x), ncol=n.by)
colnames(y) <- paste0(c(by),"2")
for(i in 1:length(by)) {
	if(!is.na(isvalue[,i])) y[,i] <-  x[,by[i]]==isvalue[i]
	if(!is.na(notvalue[,i])) y[,i] <- (x[,by[i]]!= notvalue[i])
	if(!is.na(lessthan[,i])) y[,i] <- (x[,by[i]]< lessthan[i])
	if(!is.na(morethan[,i])) y[,i] <- (x[,by[i]] >  morethan[i])
}
#convert to numeric
y <- y +0
if(new){return(y)} else {return(cbind(x,y))}
}


"wide2long" <- function(x,y, newnames=NULL) {
   colnames(x) <- rep(newnames,y)
   nvar <- NCOL(x)
   nrep <- nvar/y
   new <- NULL
   for(i in 0:(nrep-1)){
       new <- rbind(new,x[((y*i)+1):(y*(i+1))])
       }
   return(new)}
