\name{selectBy}
\alias{selectBy}
\alias{splitBy}
\title{Select a subset of rows (subjects) meeting one or more criteria for columns
}
\description{Select a subset of a data.frame or matrix for columns meeting specific criteria.  Can do logical AND (default) or OR  of the resulting search.  Columns (variables) are specified by name and the conditions to meet include equality, less than, more than or inequality to a specified set of values.  SplitBy creates new dichotomous variables based on the splitting criteria.
}
\usage{
selectBy(x, by)
splitBy(x, by, new=FALSE)
}

\arguments{
  \item{x}{A data frame or matrix
}
  \item{by}{A quote delimited string of variables and criteria values.  Multiple variables may be separated by commas (default to AND)  }
  \item{new}{If true, return a new data frame with just the dichotomous variables otherwise concatenate the new variables to the right margin of x}
}
\details{Two relatively trivial functions to help those less familiar with the subset function or how to use [] to select variables. 
}
\value{The subset of the original data.frame with just the cases that meet the criteria (selectBy) or new variables, recoded 0,1

\code{\link{selectBy}}  is equivalent to subsetting x by an x value:  small <- x[x[by=criterion]]  or the subset function  small <- subset(x, x$variable == value)
}

\author{William Revelle
}


\seealso{
\code{\link{vJoin}}  for another data manipulation function.

}
\examples{
testand <- selectBy(attitude, 'rating < 70 & complaints > 60')  #AND
dim(testand)
testor <- selectBy(attitude, 'rating < 60 | complaints > 60')  #OR
dim(testor)
test <- splitBy(attitude, 'rating > 70 , complaints > 60')  
psych::headTail(test)
}

\keyword{multivariate}