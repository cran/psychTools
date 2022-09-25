\name{zola}
\alias{zola}
\alias{zola.keys}
\alias{zola.dictionary}
\docType{data}
\title{Correlation matrix of 135 self report and 30 peer report personality items} 

\description{
Zola et al.,  (2021) reported the validity of self report personality items from the SAPA personality inventory (SPI) (Condon, 2018) in terms of 30 peer reports on 8 dimensions. Here are the polychoric correlations of these items.  spi items  were collected using SAPA procedures for 158,631 participants (mean n/item = 18,180), 908 of whom received peer ratings.  }

\usage{data("zola")}
\format{
  The format is:
 num [1:165, 1:165] 1 -0.242 0.282 0.65 0.223 ...
 - attr(*, "dimnames")=List of 2
  ..$ : chr [1:165] "q_253" "q_4296" "q_1855" "q_90" ...
  ..$ : chr [1:165] "q_253" "q_4296" "q_1855" "q_90" ...
}
\details{
The polychoric correlation matrix of the spi and peer report data. To see the item labels, use the \code{\link{lookupFromKeys}} .

This data set is a nice example of a multi-trait, multi-method correlation matrix. (see the scoring  example).  Five dimensions of self report show high correlations with the corresonding peer report scales.    
}
\source{
A. Zola, D.M. Condon, and W. Revelle, (2021)
}
\references{
A. Zola, D.M. Condon, and W. Revelle, (2021) The Convergence of Self and Informant Reports in a Large Online Sample, Collabra: Psychology, 7, 1. doi:  10.1525/collabra.25983
}
\examples{
data(zola)
psych::lookupFromKeys(zola.keys,zola.dictionary)
scores <- psych::scoreOverlap(zola.keys[c(1:5,33:37)],zola) #MTMM of Big 5
scores
}
\keyword{datasets}
