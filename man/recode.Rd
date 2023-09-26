\name{recode}
\alias{recode}
\alias{rearrange}
\alias{wide2long}

\title{Recode or rearrange or reshape variables or values to new values}

\description{
Given a set of numeric codes, change their values to different values given a mapping function.  Also included are the ability to reorder columns or to convert wide sets of columns  to long form 
}
\usage{
rearrange(x,pattern)   #reorder the variables
wide2long(x,width, cname=NULL, idname = NULL, idvalues=NULL ,pattern=NULL) 
recode(x, where, isvalue, newvalue)  #recode text values to numeric values
}

\arguments{
  \item{x}{A matrix or data frame of numeric values
}
  \item{where}{The column numbers to fix}
  \item{isvalue}{A vector of values to change}
  \item{newvalue}{A vector of the new values}
  \item{pattern}{column order of repeating patterns}
  \item{width}{width of long format }
  \item{cname}{Variable names of long format }
  \item{idname}{Name of first column}
  \item{idvalues}{Values to fill first column }
}
\details{
Three functions for basic recoding are included.

recode: Sometime, data are entered as levels in an incorrect order.  Once converted to numeric values, this can lead to confusion.  recoding of the data to the correct order is straightforward, if tedious.

rearrange: Another tedious problem is when the output of one function needs to be arranged for better data handling in subsequent function. Specify a pattern of choosing the new columns. 

wide2long: And then, having rearranged the data, perhaps convert the file to long format.

}
\value{
The reordered data
}

\author{William Revelle

}
\note{
Although perhaps  useful, the recode function is definitely ugly code. For smaller data sets, the results from char2numeric back to the original will not work.  char2numeric works column wise and orders the data in each column.  
}


\seealso{mlArrange in the psych package for a more general version of wide2long}

\examples{
x <- matrix(1:120,ncol=12) 
new <- rearrange(x,pattern = c(1,4, 7,10))
new 
long <- wide2long(x,width=3,pattern=c(1,4, 7,10))  #rearrange and then make wide


temp <- bfi[1:100,1:5]
isvalue <- 1:6
newvalue <- psych::cs(one,two,three,four,five,six)
newtemp <- recode(temp,1:5,isvalue,newvalue)
newtemp  #characters
temp.num <- psych::char2numeric(newtemp) #convert to numeric
temp.num  #notice the numerical values have changed
new.temp.num <- recode(temp.num, 1:5, isvalue=c(3,6,5,2,1,4), newvalue=1:6)
#note that because char2numeric works column wise, this will fail for small sets

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ multivariate }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
