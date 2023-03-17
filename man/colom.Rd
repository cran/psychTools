\name{colom}
\alias{colom}
\alias{colom.ed0}
\alias{colom.ed1}
\alias{colom.ed2}
\alias{colom.ed3}
\docType{data}
\title{Correlations of 14 ability tests from the Spanish version of the WAIS (taken from Colom et al. 2002.)
}
\description{
Colom et al. analyze 14 tests from the Spanish version of the WAIS. This is a nice example of a hierarchical structure using the \link[psych]{omega} function.  Here are the correlation matrices of the variables (colom), for 4 levels of education.   
}
\usage{data("colom")
 data("colom.ed0")
 data("colom.ed1")
 data("colom.ed2")
 data("colom.ed3")
 }
\format{
  The format is:
 num [1:14, 1:14] 1 0.755 0.608 0.555 0.715 0.729 0.627 0.616 0.606 0.598 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:14] "Vocabulary" "Similarities" "Arithmetic" "Digit_span" ...
  ..$ : chr [1:14] "Vocabulary" "Similarities" "Arithmetic" "Digit_span" ...
}
\details{The Wechsler Adult Intelligence Scale (WAIS)  is the "gold standard" measure of intelligence.  Here is an example of the correlational structure of 14 tests.  It was used by Colom and his colleagues to find correlations of WAIS scores as a function of education.  Here we show the complete standardization sample.  

The \link{colom} data set is the complete correlation matrix for all subjects (703 females, 666 males).  The four subset data sets for four levels of education. Ns = 301, 	432,  525, and  111.    

}
\source{
Colom et al, 2002

}
\references{Roberto Colom and Francisco J Abad and Luis F Garc  and Manuel Juan-Espinosa, 2002, Education, Wechsler's Full Scale IQ, and g.  Intelligence, 30, 449-462,
}
\examples{
data(colom)
psych::lowerMat(colom)
psych::omega(colom, 4)    #do the omega analysis  
}
\keyword{datasets}