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
dfOrder <- function(object,columns=NULL,absolute=FALSE,ascending=TRUE) {
  if(is.matrix(object)) {mat<- TRUE
             object <- as.data.frame(object)} else {mat<-FALSE}
    if(is.null(ncol(object))) {return(object)} else {
   if(is.null(columns)) columns <- 1:ncol(object)
	 nc <- length(columns)
	 cn <- colnames(object)
 	 if(ascending) {temp <- rep(1,nc)} else {temp <- rep(-1,nc)}    
 	 if(is.character(columns)) {  #treat character strings 
   		 temp [strtrim(columns,1)=="-"] <- -1
    	 if(any(temp < 0  ) )  {columns <- sub("-","",columns) }
    	 
    	} else {temp[columns < 0] <- -1
    	        columns <- abs(columns) }
    
   if(is.character(columns) ) {  for (i in 1:length(columns)) {columns[i] <- (which(colnames(object) == columns[i]))
       }
       }
      
   	  columns <- colnames(object)[as.numeric(columns)]
    if(absolute) {  temp.object<- t(t(abs(psych::char2numeric(object[columns]))) * temp)  } else {
   	  temp.object<- t(t(psych::char2numeric(object[columns])) * temp)}
   	  temp.object <- data.frame(temp.object)
 
   	 ord <- do.call(order,temp.object)
   	 if(mat) object <- as.matrix(object)
     if(length(ord) > 1) {
   	   return(object[ord,]) }else {return(object)}   #added length test 4/26/18
       }
       }

