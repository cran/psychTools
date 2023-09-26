\name{vJoin}
\alias{vJoin}
\alias{combineMatrices}

\title{Combine two matrices or data frames into one based upon variable labels

}
\description{A typical problem in data analysis is to combine two data sets into one. vJoin will combine two matrices or data.frames into one data.frame.  Unique column names from set 1 and set 2 are combined as are unique rows. Column names can differ, as can row names. Will match on rownames or a unique key vector. Basically an extension of rbind and cbind without the requirement of matching column and row names. combineMatrices solves a similar problem for correlation matrices.
}
\usage{
vJoin(x, y, rnames = TRUE, cnames=TRUE, key.name= NULL)
combineMatrices(x,y, r=NULL)
}

\arguments{
  \item{x}{a matrix or data frame with column and row names.}
  \item{y}{a matrix or data frame with column and row names}
  \item{rnames}{If TRUE, the default, match on row names, extend to new names.  If FALSE then add the y data following the x data.}
  \item{cnames}{If TRUE colnames are NULL then create unique colnames for x and y}
  \item{key.name}{if NULL, match on rownames, otherwise, match on the values of the key.name column -- must be unique}
  \item{r}{shoule we add the diagonal of y?}
  }
\details{
For an X and Y matrices/data.frames with column and row names, combine the two data sets.  Match  on column and row names  if they exist, extend to unique names if they do not match.  Can also match on a column in each set (key.name)

Matrices by default do not have column or rownames.  They will be created for  x and for y  (depending upon the rnames and cnames options).

combineMatrices takes a square matrix (x) and combines with a rectangular matrix y to produce a larger xy matrix. }

\value{xy: a data frame}

\author{William Revelle
}
\note{Inspired by the functionality of full_join and the other related dplyr functions.
}

\examples{
X1 <- bfi[1:10,1:5]
Y1 <- bfi[6:15,4:10]
xy <- vJoin(X1,Y1) #match on rownames
xy1 <- vJoin(X1,Y1,rnames=FALSE) #add Y1 items after X1 items

x <- matrix(1:30, ncol=5)
y <- matrix(1:40, ncol=8)
vJoin(x,y)
vJoin(x,y,cnames=FALSE)
vJoin(x,y, rnames= FALSE, cnames=FALSE)


R <- cor(sat.act,use="pairwise")
r1 <- R[1:4,1:4]
r2 <- R[1:4,5:6] 
newr <- combineMatrices(r1,r2)

}
\keyword{ multivariate }
\keyword{ IO }


