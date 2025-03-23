
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
				cat("Select the input directory (not a file)")
                fn <- file.choose()
                if(!dir.exists(fn)) {dir <- dirname(fn) 
                    inlist <- fn
                    nfiles <- 1 
                    inDir <- dir
                } else {fn<- inDir
                    inDir <- filesList(dir) #find the input directory
                 }
                 }
				
       			
       
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
