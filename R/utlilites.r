#Various useful utility functions


 # list the files in a directory holding a particular file, or a particular directory
"filesList" <- function(f=NULL) {
 if(is.null(f)) { f <- file.choose()} 
   if(dir.exists(f)) {dir <- f } else {dir <- dirname(f)}  #find a file in the directory you want
  files.list <- list.files(dir)
  message("\nFiles in the directory", dir, "\n")  #although I prefer cat, CRAN seems to prefer message
  return(files.list)
  }
  
  "filesInfo" <- function(f=NULL,max=NULL) {
   if(is.null(f)) { f <- file.choose()}
   if(dir.exists(f)) {dir <- f } else {dir <- dirname(f)}
    files.list <- list.files(dir)
   if(is.null(max)) max <- length(files.list)
   info <- list(max)
   for(i in 1:max) {
   info[[i]] <- file.info(file.path(dir,files.list[i]))}
   info.df <- info[[1]]
   for (i in 2:max) {
   info.df <- rbind(info.df,info[[i]])}
   info.df <-cbind(file=1:max,info.df)
   return(info.df)
  }
  
  
  "fileScan" <- function(f=NULL,nlines=3,max=NULL,from=1,filter=NULL) {
  cat("\n Just the content of files will be shown (not directories)\n")
  if(is.null(f)) {f <- file.choose()}  #find a file in the directory you want
   dir <- dirname(f)  #the directory where the file was found
   files.list <- list.files(dir)
   dir.list <-  list.dirs(dir,full.names=FALSE)
   files.list <- files.list[!files.list  %in% dir.list]  #get rid of directories
   if(!is.null(filter)) {select <- grep(filter,files.list,ignore.case=TRUE) #these are the ones that match filter            
               files.list <- files.list[select]}
   n.files <- length(files.list)
   if(!is.null(max)) n.files <- max + from
   for (i in from:n.files) {
   file <- files.list[i]
     path <- file.path(dir,file)
     suffix <- file_ext(file)
      if(suffix %in% c("xls","xlsx","doc","sav","data","dat","rds","R","r","RDS", "XPT","xpt","Rda","rda","Rdata","RData","rdata","SYD","syd","sys","jmp","sas7bdat")) {
      cat("\nFile = ",i, "Name = ", file, "Was skipped") } else {
    # temp <- scan(path,what="raw",nlines=nlines)
    temp <- readLines(path,n=nlines)
    cat("\nFile = ",i, "Name = ", file, "\n",temp,"\n")}
    }
    return(dir)
    }
   
  
  #a work around the failure of file.choose(new=TRUE) to work in Rstudio
  
"fileCreate" <- function(newName="new.file") {
cat("Search for a file in the directory where you want to create a new file")
  fn <- file.choose()
  dir <- dirname(fn)
  new.path <- file.path(dir,newName) 
  message("\nAre you sure you want to create a new file named ",new.path,"?\n")
  ok <- readline(prompt="Yes or No  ")
  if(any(c("Y","y") %in%  ok)) {
  if(!file.exists(new.path)) {
 
  file.create(new.path)
  return(new.path) } else {message('\nFile already exists, try a different name')} 
  }else {message("fileCreate was cancelled")}
  }
  
 #Completely rewritten 1/20/18 to follow  the help pages for order more closely
#sort a data frame according to one or multiple columns 
#will only work for data.frames (not matrices)
#needs to not quit if there is nothing to do
#Then rewritten again 01/02/22 to allow sorting correlation matrices as well
#Minor tweak 2/21/22 for the case of a single column
#There are actually two cases; for data.frames (select=null) and for correlations (select = column names)
dfOrder <- function(object,columns=NULL,absolute=FALSE,ascending=TRUE) {
  if(is.matrix(object)) {mat<- TRUE
             object <- as.data.frame(object)} else {mat<-FALSE}
    if(is.null(ncol(object))| NROW(object) ==1) {return(object)} else {
   
   if(is.null(columns)) columns <- colnames(object)
    if(psych::isCorrelation(object)) {select <- columns} else {select<- NULL}
	 nc <- length(columns)
	 cn <- colnames(object)
	 if(is.null(select)) {
	 #this allows us to sort columns independently of each other 
 	 if(ascending) {temp <- rep(1,nc)} else {temp <- rep(-1,nc)}    
 	  if(is.character(columns)) {  #treat character strings 
   		 temp [strtrim(columns,1)=="-"] <- -1
    	 if(any(temp < 0  ) )  {columns <- sub("-","",columns) }
    	 
    	} else {temp[columns < 0] <- -1
    	        columns <- abs(columns) }
    
   if(is.character(columns) ) {  for (i in 1:length(columns)) {columns[i] <- (which(colnames(object) == columns[i]))
       }
       columns <- as.numeric(columns)
       }
   
   
    if(absolute) {  temp.object<- t(t(abs(psych::char2numeric(object[columns]))) * temp)  } else {
    	  temp.object<- t(t(psych::char2numeric(object[columns])) * temp)}
   #   if(absolute) {temp.object <-  psych::char2numeric(object[columns])} else {
    #  temp.object <- psych::char2numeric(object[columns])}
   	  temp.object <- data.frame(temp.object)

}  else { #the correlation case
if(!is.numeric(select)) {if (!all(select %in% cn)) stop ('Variable names are incorrect')}
   # if(absolute) object <- abs(object)
  temp.ord <- apply(abs(object[,select,drop=FALSE]),1,which.max)
  if(!ascending) temp.ord <- length(select)- temp.ord
if(absolute) { t.m <- apply(abs(object[,select,drop=FALSE]) ,1,max)} else {
  temp.max <- apply(object[,select,drop=FALSE] ,1,max)
  temp.min <- apply(object[,select,drop=FALSE],1,min) 
  abs.max <- apply(abs(object[,select,drop=FALSE]),1,max)
  t.m <- abs.max 
  t.m[abs.max > temp.max] <- temp.min[abs.max > temp.max]}
  temp.max <- t.m + 3*(length(select)-1+ temp.ord)
  # else {temp.ord <- apply(object[,select],1,which.min)
#  temp.max <- apply(object[,select],1,min)}
# temp.max <- temp.max + 3 * (length(select) + 1 +temp.ord) #this takes into account the possibility of signed values
  ord <- order(temp.max,decreasing=!ascending)

  if(NCOL(object) == NROW(object)) {return(object[ord,ord])} else {return(object[ord,])}
 }
   	 ord <- do.call(order,temp.object) 
   	 if(mat) object <- as.matrix(object)
   	 
     if(length(ord) > 1) {
   	   return(object[ord,]) } else {return(object)}   #added length test 4/26/18
       }
       }
       
#two unpublished functions 
"bullseye" <- function(x,y,n) {
for(i in 1:n) {psych::dia.ellipse(x,y,e.size=i)}
}


"dartBoard" <- function(n,sdx=.2,sdj=.3) {
plot(NA,xlim=c(0,10),ylim=c(0,10),axes=FALSE,xlab="",ylab="",main="Reliability and Validity as dart throwing")
if(n>20) {pc <- "."} else {pc <- 16}
if(missing(sdj)) sdj=sdx*1.5

#Reliable and valid
x=3
y=3


bullseye(x,y,4)
points(x+rnorm(n,0,sdx),y+rnorm(n,0,sdx),pch=pc)
text(x,y-2,"Reliable and Valid")

#reliable and invalid
x=7
y=8
bullseye(x,y,4)
points(x+rnorm(n,1,sdx),y+rnorm(n,1,sdx),pch=pc)
text(x,y-2,"Reliable and Invalid")


#unreliable and invalid
x=3
y=8
bullseye(x,y,4)
points(x+rnorm(n,1,sdj),y+rnorm(n,1,sdj),pch=pc)
text(x,y-2,"Unreliable and Invalid")

#unreliable, but valid
x=7
y=3
sdx=1
bullseye(x,y,4)
points(x+rnorm(n,0,sdj),y+rnorm(n,0,sdj),pch=pc)
text(x,y-2,"Unreliable but Valid")
}


#dartBoardl(6,.3,.5)


