\name{globalWarm}
\alias{globalWarm}
\alias{glbwarm}
\docType{data}
\title{7 attitude items about Global Warming policy from Erik Nisbet
}
\description{Erik Nisbet reported the relationship between emotions, ideology, and party affiliation as predictors of attitudes towards government action on climate change. The data were used by Hayes (2013) in a discussion of regression.  They are available as the glbwarm data set in the processR package.  They are copied here for examples of mediation.
}
\usage{data("globalWarm")}
\format{
  A data frame with 815 observations on the following 7 variables.
  \describe{
    \item{\code{govact}}{a numeric}
    \item{\code{posemot}}{a numeric}
    \item{\code{negemot}}{a numeric}
    \item{\code{ideology}}{a numeric}
    \item{\code{age}}{a numeric}
    \item{\code{sex}}{a numeric}
    \item{\code{partyid}}{a numeric}
  }
}
\details{More details to follow
}
\source{  The raw data are available from the processR package (Keon-Woong Moon, 2020). as the glbwarm data set.  The data set is used by Hayes in several examples. Used here by kind permission of Erik Nisbet. 
}
\references{
Nisbet, E. (?) 

Hayes, Andrew F. (2013) Introduction to mediation, moderation, and conditional process analysis: A regression-based approach. Guilford Press.
}
\examples{
data(globalWarm)
psych::lowerCor(globalWarm)
}
\keyword{datasets}
