\name{read.file}
\alias{read.clipboard}
\alias{read.clipboard.csv}
\alias{read.clipboard.tab}
\alias{read.clipboard.lower}
\alias{read.clipboard.upper}
\alias{read.clipboard.fwf}
\alias{read.file}
\alias{read.file.csv}
\alias{write.file}
\alias{write.file.csv}
\alias{read.https}
\title{Shortcuts for reading from the clipboard or a file}
\description{Input from a variety of sources may be read. Matrices or data.frames may be read from files with suffixes of .txt, .text, .TXT, .dat, .DATA,.data, .csv, .rds, rda, .xpt, XPT, or  .sav  
 (i.e., data from SPSS sav files may be  read as can files saved by SAS using the .xpt option).
  Data exported by JMP or EXCEL in the csv format are also able to be read.
   Fixed Width Files saved in .txt mode may be read if the  widths parameter is specified. 
   Files saved with writeRDS have suffixes of  .rds or  Rds,  and are read using readRDS. 
    Files associated with objects with suffixes .rda and .Rda are  loaded (following a security prompt).
     The default values for read.spss are adjusted for more standard input from SPSS files.  
Input from the clipboard is easy but a bit obscure, particularly for Mac users.
 \code{\link{read.clipboard}} and its variations are just an easier way to do so.  
 Data may be copied to the clipboard from Excel spreadsheets, csv files, or fixed width formatted files and then into a data.frame.
  Data may also be read from lower  (or upper) triangular matrices and filled out to square matrices.  
  Writing text files may be done using \code{\link{write.file}} which
 will prompt for a file name (if not given) and then write or save to that file depending upon the
  suffix (text, txt, or csv will call write.table, R, or r will dput, rda, Rda will save,  Rds,rds will saveRDS).
}
\usage{
read.file(file=NULL,header=TRUE,use.value.labels=FALSE,to.data.frame=TRUE,sep=",",
    quote="\"", widths=NULL,f=NULL, filetype=NULL,...)
   #for .txt, .text, TXT, .csv, .sav, .xpt, XPT,  R, r, Rds, .rds, or .rda, 
  # .Rda, .RData, .Rdata, .dat and .DAT  files

read.clipboard(header = TRUE, ...)   #assumes headers and tab or space delimited
read.clipboard.csv(header=TRUE,sep=',',...)   #assumes headers and comma delimited
read.clipboard.tab(header=TRUE,sep='\t',...)   #assumes headers and tab delimited
 #read in a matrix given the lower off diagonal
 read.clipboard.lower(diag=TRUE,names=FALSE,...) 
 read.clipboard.upper(diag=TRUE,names=FALSE,...)

#read in data using a fixed format width (see read.fwf for instructions)
read.clipboard.fwf(header=FALSE,widths=rep(1,10),...)  

read.https(filename,header=TRUE)
read.file.csv(file=NULL,header=TRUE,f=NULL,...)

#For output: 
#be sure to specify the file type in name
write.file(x,file=NULL,row.names=FALSE,f=NULL,...)
write.file.csv(x,file=NULL,row.names=FALSE,f=NULL,...)


}

\arguments{
  \item{header}{Does the first row have variable labels (generally assumed to be TRUE). }
  \item{sep}{What is the designated separater between data fields? For typical csv files, this will be a comma, but if commas designate decimals, then a ; can be used to designate different records.  }
  \item{quote}{Specified to }
  \item{diag}{for upper or lower triangular matrices, is the diagonal specified or not}
  \item{names}{for read.clipboard.lower or upper, are colnames in the  the first column}
  \item{widths}{how wide are the columns in fixed width input.  The default is to read 10 columns of size 1. }
  \item{filename}{Name or address of remote https file to read.}
  \item{\dots}{ Other parameters to pass to read }
  \item{f}{A file name to read from or write to.  If omitted, \code{\link{file.choose}} is called to dynamically get the file name.}
  \item{file}{A file name to read from or write to. (same as f, but perhaps more intuitive).  If omitted and if f is omitted,then \code{\link{file.choose}} is called to dynamically get the file name.}
  \item{x}{The data frame or matrix to write to f}
  \item{row.names}{Should the output file include the rownames? By default, no.} \item{to.data.frame}{Should the spss input be converted to a data frame?}
 \item{use.value.labels}{Should the SPSS input values be converted to numeric?}

 \item{filetype}{If specified the reading will use this term rather than the suffix.}

}
\details{A typical session of R might involve data stored in text files, generated online, etc. 
Although it is easy to just read from a file (particularly if using \code{\link{read.file}}), an alternative is to use one's local system to copy from the file to the clipboard and then read from the clipboard using  \code{\link{read.clipboard}}. This is  very convenient (and somewhat more intuitive to the naive user). This is particularly useful when copying from a text book or article and just moving a section of text into R.  However, copying from a file and then reading the clipboard is hard to automate in a script.  Thus, \code{\link{read.file}} will read from a file.

The \code{\link{read.file}} function combines the \code{\link{file.choose}} 
and either \code{\link{read.table}}, \code{\link{read.fwf}}, \code{\link[foreign]{read.spss}} or  
     \code{\link[foreign]{read.xport}}(from foreign) 
or \code{\link{load}} or \code{\link{readRDS}} commands. By examining the file suffix, it chooses the appropriate way to read the file. For more complicated file structures, see the foreign package. For even more complicated file structures, see the rio or haven packages. 

Note that \code{\link{read.file}} assumes by default that the first row has column labels (header =TRUE).  If this is not true, then make sure to specify header = FALSE.  If the file is fixed width, the assumption is that it does not have a header field.  In the unlikely case that a fwf file does have a header, then you probably should try fn <- file.choose() and then my.data <- read.fwf(fn,header=TRUE,widths=   widths).

Further note:  If the file is a .Rda, .rda, etc. file, the read.file command will return the name and location of the file. It will prompt the user to load this file.  In this case, it is necessary to either assign the output (the file name) to an object that has a different name than any of the objects in the file, or to call read.file() without any specification. Notice that loading an .Rda file can overwrite existing objects.  Thus the warning and the need to do the second step.

If the file has no suffix the default action is to quit with a warning.  However, if the filetype is specified, it will use that type in the reading (e.g. filetype="txt" will read as text file, even if there is no suffix).

If the file is specified and has a prefix of http:// or https:// it will be downloaded and then read.


Currently supported input formats are 
\tabular{ll}{
.sav       \tab      SPSS.sav files\cr
.csv       \tab      A comma separated file (e.g. from Excel or Qualtrics)\cr
.txt       \tab      A typical text file \cr
.TXT        \tab      A typical text file \cr
.text      \tab     A typical text file \cr
.data     \tab       A data file \cr
.dat      \tab       A data file \cr
.rds       \tab      A R data file \cr
.Rds      \tab       A R data file  (created by a write) \cr
.Rda      \tab       A R data structure (created using save) \cr
.rda      \tab       A R data structure (created using save) \cr
.RData     \tab       A R data structure (created using save) \cr
.rdata     \tab       A R data structure (created using save) \cr
.R        \tab       A R data structure created using dput \cr
.r        \tab       A R data structure created using dput \cr
.xpt      \tab       A SAS data file in xport format \cr
.XPT      \tab       A SAS data file in XPORT format  \cr
}

Some data files have an extra ' in the data ( e.g. the NYT covid data base).  These files can be read specifying quote ""

The foreign function \code{\link[foreign]{read.spss}} is used to read SPSS .sav files using the most common options.  Just as  \code{\link[foreign]{read.spss}}  issues various warnings, so does  \code{\link{read.file}}.   In general, these can be ignored.  For more detailed information about using  \code{\link[foreign]{read.spss}}, see the help pages in the foreign package.

If you have a file written by JMP, you must first export to a csv or text file.


The \code{\link{write.file}} function combines the \code{\link{file.choose}} and either \code{\link{write.table}} or \code{\link{saveRDS}}. By examining the file suffix, it chooses the appropriate way to write. For more complicated file structures, see the foreign package, or the save function in R Base.  If no suffix is added, it will write as a .txt file.  \code{\link{write.file.csv}} will write in csv format to an arbitrary file name.

Currently supported output formats are 
\tabular{ll}{
.csv       \tab      A comma separated file (e.g. for reading into Excel)\cr
.txt       \tab      A typical text file \cr
.text      \tab     A typical text file \cr
.rds       \tab      A R data file \cr
.Rds      \tab       A R data file  (created by a write) \cr
.Rda      \tab       A R data structure (created using save) \cr
.rda      \tab       A R data structure (created using save) \cr
.R        \tab       A R data structure created using dput \cr
.r        \tab       A R data structure created using dput \cr
}


Many Excel based files specify missing values as a blank field. When reading from the clipboard, using \code{\link{read.clipboard.tab}} will change these blank fields to NA.

Sometimes missing values are specified as "." or "999", or some other values. These can be converted by the read.file command specifying what values are missing  (e.g., na ="."). See the example for the reading from the remote mtcars.csv file.

\code{\link{read.clipboard}} was based upon a suggestion by Ken Knoblauch to the R-help listserve.

If the input file that was copied into the clipboard was an Excel file with blanks for missing data, then read.clipboard.tab() will correctly replace the blanks with NAs.  Similarly for a csv file with blank entries, read.clipboard.csv will replace empty fields with NA.  

\code{\link{read.clipboard.lower}} and \code{\link{read.clipboard.upper}} are adapted from John Fox's read.moments function in the sem package.  They will read a lower (or upper) triangular matrix from the clipboard and return a full, symmetric matrix for use by factanal, 
\code{\link[psych]{fa}} , \code{\link[psych]{ICLUST}}, \code{\link[psych]{pca}}. \code{\link[psych]{omega}} , etc.  If the diagonal is false, it will be replaced by 1.0s.  These two function were added to allow easy reading of examples from various texts and manuscripts with just triangular output. 

Many articles will report lower triangular matrices with variable labels in the first column.  read.clipboard.lower  will handle this case. Names must be in the first column if names=TRUE is specified.

Other articles will report upper triangular matrices with variable labels in the first row.  read.clipboard.upper will handle this.  Note that labels in the first column will not work for read.clipboard.upper.  The names, if present, must be in the first row.

Consider the following lower triangular matrix.  To read it, copy it to the clipboard and read.clipboard.lower(names=TRUE)

\tabular{lrrrrrr}{
 A1 1.00 \cr
A2 -0.34  1.00 \cr
A3 -0.27 0.49  1.00 \cr
A4 -0.15  0.34  0.36  1.00 \cr
A5 -0.18  0.39  0.50  0.31  1.00 \cr
C1 0.03   0.09  0.10  0.09  0.12  1.00\cr
}


However, if the data are strung out e.g.,
\tabular{r}{
-.34  \cr
-.27  \cr
-.15  \cr
-.18  \cr
.03\cr
.49\cr
.34 \cr
.39 \cr
.09 \cr
.36\cr
.50 \cr
.10 \cr
.31\cr
.09 \cr
.12\cr
}
  
Then one needs to read it using the read.clipboard.upper(names=FALSE,diag=FALSE) option.  




read.clipboard.fwf will read fixed format files from the clipboard.  It includes a patch to read.fwf which will not read from the clipboard or from remote file.  See read.fwf for documentation of how to specify the widths. }

\value{The contents of the file to be read or of the clipboard. Saved as a data.frame.
}

\author{ William Revelle}

\examples{
#All of these functions are meant for interactive Input
#Because these are dynamic functions, they need to be run interactively and 
# can not be run as examples.
#Thus they are not to be tested by CRAN
\donttest{
if(interactive()) {
 my.data <- read.file()  #search the directory for a file and then read it.
                         #return the result into an object 
#or, if the file is a rda, etc. file
my.data <- read.file()  #return the path and instructions of how to load
  # without assigning a value.

filesList()  #search the system for a particular file and then list all the files in that directory
fileCreate() #search for a particular directory and create a file there.
write.file(Thurstone) #open the search window, choose a location and name the output file,
# write the data file (e.g., Thurstone ) to the file chosen

#the example data set from read.delim in the readr package to read a remote csv file
my.data <-read.file(
"https://github.com/tidyverse/readr/raw/master/inst/extdata/mtcars.csv", 
na=".")   #the na option is used for an example, but is not needed for these data


#These functions read from the local clipboard and thus are interactive
my.data <- read.clipboard()   #space delimited columns
my.data <- read.clipboard.csv()  # , delimited columns 
my.data <- read.clipboard.tab()  #typical input if copied from a spreadsheet
my.data <- read.clipboad(header=FALSE)  #data start on line 1
my.matrix <- read.clipboard.lower()
}
}
}
\keyword{ multivariate }
\keyword{ IO }
