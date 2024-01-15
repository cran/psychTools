
"rd2html" <- function(inDir =NULL,outDir=NULL, nfiles=NULL,package="psych",file=NULL) {
#two cases; 1  one input file
#           2  multiple files

if(!is.null(file)) {  #case 1
                inList<- file
                outList <- gsub("Rd","html",file) 
                outList <- basename(outList) 
				if(is.null(outDir)) {cat("select the output directory")
				outDir <- file.choose(TRUE) }
				outDir <- dirname(outDir)
				out.path <- file.path(outDir,outList)
				tools::Rd2HTML(inList,out.path,package=package)
                   
                } else { #case 2
if(is.null(inDir)) {
				cat("select the input directory")
                fn <- file.choose()
                } else {fn<- inDir
                #check if this is directory or a file
                 if(grep("Rd",fn)>0) {dir <- dirname(fn)} else {dir<- fn}
                 }
				
       			inDir <- filesList(dir) #find the input directory
       
if(is.null(outDir)) {cat("select the output directory")
		outDir <- file.choose(TRUE) 
 		} else {
        outDir <- dirname(outDir)}

 if(is.null(nfiles)) nfiles <- length(inDir)
   outList <- gsub("Rd","html",inDir) 
   for(i in 1:nfiles) { 
     new.path <- file.path(dir,inDir[[i]])
     out.path <- file.path(outDir,outList[[i]])
    tools::Rd2HTML(new.path,out.path,package=package)         
 		}
 	}# end of case 2
 	}	
