\name{dfOrder}
\alias{dfOrder}

\title{Sort (order) a dataframe or matrix by multiple columns
}
\description{Although \code{\link{order}} will order a vector, and it is possible to order several columns of a data.frame by specifying each column individually in the call to order, \code{\link{dfOrder}} will order a dataframe or matrix by as many columns as desired.  The default is to sort by columns in lexicographic order. If the object  is a correlation matrix, then the selected columns are sorted by the (abs) max value across the columns (similar to fa.lookup in psych).   If object is a correlation matrix,  rows and columns are sorted.   
}
\usage{
dfOrder(object, columns,absolute=FALSE,ascending=TRUE)
}

\arguments{
  \item{object}{The data.frame or matrix to be sorted}

  \item{columns}{Column numbers or names to use for sorting.  If positive, then they will be sorted in increasing order. If negative, then in decreasing order}
  \item{absolute}{If TRUE, then sort the absolute values}
  \item{ascending}{By default, order from smallest to largest.}
}
\details{
This is just a simple helper function to reorder data.frames and correlation matrices.  Originally developed to organize IRT  output from the ltm package. It is a basic add on to the order function.  

(Completely rewritten for version 1.8.1. and then again for 2.2.1 to allow sorting correlation matrices by numeric values.) 
}
\value{
The original data frame is now in sorted order. If the input is a correlation matrix, the output is sorted by rows and columns.
}

\author{William Revelle
}


\seealso{ Other useful file manipulation functions include \code{\link{read.file}} to read in data from a file or \code{\link{read.clipboard}} from the clipboard,  \code{\link{fileScan}}, \code{\link{filesList}}, \code{\link{filesInfo}},  and \code{\link{fileCreate}}

\code{\link{dfOrder}} code is used in the  \code{\link[psych]{test.irt}} function to combine ltm and \code{\link[psych]{sim.irt}} output.

}
\examples{
#create a data frame and then sort it in lexicographic order
set.seed(42)
x <- matrix(sample(1:4,64,replace=TRUE),ncol=4)
dfOrder(x)  # sort by all columns
dfOrder(x,c(1,4))  #sort by the first and 4th column
x.df <- data.frame(x)
dfOrder(x.df,c(1,-2))  #sort by the first in increasing order, 
   #the second in decreasing order

#now show sorting correlation matrices  
r <- cor(sat.act,use="pairwise")
r.ord <- dfOrder(r,columns=c("education","ACT"),ascending=FALSE)
psych::corPlot(r.ord)
}

\keyword{manip }% use one of  RShowDoc("KEYWORDS")
\keyword{utilities }% __ONLY ONE__ keyword per line
