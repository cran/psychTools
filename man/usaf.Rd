\name{usaf}
\alias{usaf}
\alias{USAF}
\docType{data}
\title{17 anthropometric measures from the USAF showing a general factor}

\description{The correlation matrix of 17 anthropometric measures from the United States Air Force survey of 2420 airmen.  The data are taken from the Anthropometry package and included here as a demonstration of a hierarchical factor structure suitable for analysis by the \code{\link{omega}} or \code{\link{omegaSem}}.
}
\usage{data("USAF")}
\format{
  The format is:
 num [1:17, 1:17] 1 0.1148 -0.0309 -0.028 -0.0908 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:17] "age" "weight" "grip" "height" ...
  ..$ : chr [1:17] "age" "weight" "grip" "height" ...
}
\details{
The original data were collected by the USAF and reported in Churchill et al, 1977.  They are included as a data file of 2420 participants and 202 variables (the first being an id) in the Anthropometry package.  The list of variable names may be found in Churchill et al, on pages 99-103.

The three (correlated) factor structure shows a clear height, bulk, and head size structure with an overall general factor (g)  which may be interpreted as body size. 

The variables included (and their variable numbers in Anthropometry) are:

\tabular{ll}{
age \tab  V1\cr
weight \tab V2 \cr
grip strength \tab V12 \cr
height (stature) \tab V13 \cr
leg length \tab V26  \cr
knee height \tab V37 \cr
upper arm \tab  V42 \cr
thumb tip reach \tab V47 \cr
in sleeve  \tab V49 \cr
chest breadth \tab  V52\cr
hip breadth \tab  V55 \cr
waist circumference \tab V71 \cr
thigh circumference \tab V97 \cr
scye circumference \tab V103\cr
head circumference \tab V141 \cr
bitragion coronal \tab V145 \cr
head length \tab V150 \cr
glabella to wall \tab  V181 \cr
external canthus to wall \tab V183 \cr

}
Note that these numbers are equivalant to the numbers in Churchill et al. The numbers in Anthropometry are these + 1.

}
\source{
Guillermo Vinue, Anthropometry: An R Package for Analysis of Anthropometric Data, Journal of Statistical Software, (2017), 77, 6.  data set = USAFsurvey}

\references{
Edmund Churchill, Thomas Churchill, Paul Kikta (1977) The AMRL anthropmetric data bank library, volumes I-V.  (Technical report AMRL-TR-77-1)  ) https://apps.dtic.mil/dtic/tr/fulltext/u2/a047314.pdf

Guillermo Vinue, Anthropometry: An R Package for Analysis of Anthropometric Data, Journal of Statistical Software, (2017), 77, 6.

}
\examples{
data(USAF)
psych::corPlot(USAF,xlas=3)
psych::omega(USAF[c(4:8,10:19),c(4:8,10:19)])   #just the size variables
}
\keyword{datasets}
