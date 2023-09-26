
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

#revised 8/26/23 
"wide2long" <- function(x,width, cname=NULL, idname = NULL, idvalues=NULL ,pattern=NULL) {
   if(!is.null(pattern)) x <- rearrange(x,pattern) #this organizes the data before converting to long
   nvar <- NCOL(x)
   n.obs <- NROW(x)
    nrep <- nvar/width
    if(is.null(cname)){ cname <- paste0("V",1:width)}
    if(is.null(idname) ) idname="C"
   if(is.null(idvalues)) {idvalues <- 1:nrep}
    idvalues <- rep(idvalues,each=n.obs)
 
   if(round(nrep)!=nrep) stop("x must be an even multiple of width")
   new <- NULL
   for(i in 0:(nrep-1)){  #create long data set
       new <- rbind(new,x[,((width*i)+1):(width*(i+1))])
       }
   new.df <-data.frame(idname=idvalues,new)
   colnames(new.df )<- c(idname, cname)
   return(new.df)}

"rearrange" <- function(x, pattern ) {
   nvar <- ncol(x)
   y <- length(pattern)
   if(round(nvar/y)!=nvar/y) stop("length of pattern must be a multiple of the number of variables")
   ord <- NULL 
   for (i in (0: (y-2))) {
   ord <- c(ord,pattern+i)}
   x[,]  <- x[,ord]
 return(x)}
   