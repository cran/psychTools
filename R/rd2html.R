rd2html <- function(fin=NULL,fout=NULL,package="psych" ) {
if(is.null(fin)) {
cat("select the input directory")
 fn <- file.choose()
   fin <- filesList(fn)
   dirIn <- dirname(fn)}
 #dynamically search for Rd file folder
nfiles <- length(fin)
if(is.null(fout)) {
cat("select the output directory")
fn <- file.choose()
                   dirOut <- dirname(fn)}
                   

for(i in 1:nfiles) {
fni <- fin[i]
fn <-  paste0(dirIn,"/",fin[i])
fout <- paste0(dirOut,"/",fni)
fout <- gsub(".Rd",".html",fout)
file.create(fout)
tools::Rd2HTML(fn,fout,package)
}


}