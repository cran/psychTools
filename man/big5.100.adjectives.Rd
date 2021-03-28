\name{big5.100.adjectives}
\alias{big5.100.adjectives}
\alias{bfi.adjectives}
\alias{big5.adjectives.keys}
\alias{bfi.adjectives.keys}
\docType{data}
\title{100 adjectives describing the "big 5" for 502 subjects}
\description{Lew Goldberg organized 100 adjectives to measure 5 factors of personality (The Big5).  500 hundred participants were given these adjectives along with other personality measures in the Personality, Motivation and Cognition (PMC) lab.  This data set is for demonstrations of factor and cluster analysis.
}
\usage{data("big5.100.adjectives")}
\format{
  A data frame with 554 observations on the following 102 variables.
  \describe{
    \item{\code{study}}{a character vector}
    \item{\code{id}}{a numeric vector}
    \item{\code{V1}}{numeric vector  (see big5.adjectives.dictionary) }
    \item{\code{V100}}{A numeric vector. (see big5.adjectives.dictionary)}
    \item{bfi.adjectives.keys}{a key list}
  }
}
\details{

Procedure. The data were collected over nine years in the Personality, Motivation and Cognition laboratory at Northwestern, as part of a series of studies examining the effects of personality and situational factors on motivational state and subsequent cognitive performance. In each of 38 studies, prior to any manipulation of motivational state, participants signed a consent form and in some studies, consumed 0 or 4mg/kg of caffeine. In caffeine studies, they waited 30 minutes and then filled out the MSQ as well as other personality trait measures (e.g. the Big 5 adjectives)
}
\source{Data collected at the Personality, Motivation, and Cognition Laboratory, Northwestern University.}
\references{
Lewis R. Goldberg,(1992) The development of markers for the Big-Five factor structure, Psychological Assessment, 4 (1) 26-42.

Revelle, W. and  Anderson, K.J. (1998) Personality, motivation and cognitive performance: Final report to the Army Research Institute on  contract MDA 903-93-K-0008. (\url{https://www.personality-project.org/revelle/publications/ra.ari.98.pdf}).
}
\examples{
data(big5.100.adjectives)
five.scores <- psych::scoreItems(big5.adjectives.keys,big5.100.adjectives)
summary(five.scores)
}
\keyword{datasets}
