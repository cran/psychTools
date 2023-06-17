\name{recode}
\alias{recode}

\title{Recode variable values to new values
}
\description{
Given a set of numeric codes, change their values to different values given a mapping function
}
\usage{
recode(x, where, isvalue, newvalue)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A matrix or data frame of numeric values
}
  \item{where}{The column numbers to fix
}
  \item{isvalue}{A vector of values to change
}
  \item{newvalue}{A vector of the new values
}
}
\details{
Sometime, data are entered as levels in an incorrect order.  Once converted to numeric values, this can lead to confusion.  recoding of the data to the correct order is straightforward, if tedious.
}
\value{
The reordered daa
}

\author{William Revelle

}
\note{
Perhaps useful, but definitely ugly code. For smaller data sets, the results from char2numeric back to the original will not work.  char2numeric works column wise and orders the data in each column.  
}


\examples{
temp <- bfi[1:100,1:5]
isvalue <- 1:6
newvalue <- psych::cs(one,two,three,four,five,six)
newtemp <- recode(temp,1:5,isvalue,newvalue)
newtemp  #characters
temp.num <- psych::char2numeric(newtemp) #convert to numeric
temp.num  #notice the numerical values have changed
new.temp.num <- recode(temp.num, 1:5, isvalue=c(3,6,5,2,1,4), newvalue=1:6)
#note that because char2numeric works colum wise, this will fail for small sets

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
