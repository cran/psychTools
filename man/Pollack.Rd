\name{Pollack}
\alias{Pollack}
\alias{estress}
\docType{data}
\title{Pollack et al (2012) correlation matrix for mediation example
}
\description{A correlation matrix taken from Pollack (2012) with 9 variables.  Primarily used as an example for setCor and mediation.}
\usage{data("Pollack")}

\format{A correlation matrix based upon 262 participants.
  \describe{
    \item{\code{sex}}{Male = 1, Female = 0, 62\% male}
    \item{\code{age}}{mean =33}
    \item{\code{tenure}}{length of employent, mean = 5.9 years}
    \item{\code{self.efficacy}}{self ratings}
    \item{\code{competence}}{self rating of competence}
    \item{\code{social.ties}}{Contact with business-related social ties}
    
    \item{\code{economic.stress}}{mean of two items on economic stress}
    \item{\code{depression}}{6 items from MAACL measuring depression}
    \item{\code{withdrawal}}{Withdrawal intentions in domain of entrepreneurship}
  }

  
  
}
\details{This is the correlation matrix from Pollack et al. (2012) p 797.  The raw data are available from the processR package (Keon-Woong Moon, 2020). The data set is used by Hayes in example p 179 in example 3.
}
\source{Pollack et al. 2012
}
\references{Pollack, Jeffrey M. and Vanepps, Eric M. and Hayes, Andrew F. (2012). The moderating role of social ties on entrepreneurs' depressed affect and withdrawal intentions in response to economic stress, Journal of Organizational Behavior 33 (6) 789-810.

Hayes, Andrew F. (2013) Introduction to mediation, moderation, and conditional process analysis: A regression-based approach. Guilford Press.


}

\examples{
psych::lowerMat(Pollack)
}
\keyword{datasets}

