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
    \item{\code{govact}}{Support for govermment action}
    \item{\code{posemot}}{Positive emotions about climate change}
    \item{\code{negemot}}{Negative emotions about climate change} 
    \item{\code{ideology}}{Political ideology (Liberal to conservative)}
    \item{\code{age}}{age}
    \item{\code{sex}}{female =0, male =1}
    \item{\code{partyid}}{Democratic =1, Independent =2, Republican =3}
  }
  }

\details{This data set is discussed as an example of regression in Hayes (2013) p 24 - 30 and elsewhere.   It is a nice example of moderated regression.   It was collected by Erik Nisbet (no citation) who studies communication and the media. E. Nisbet is currently on the faculty at Northwestern School of Communication.  
}
\source{The raw data are available from the processR package (Keon-Woong Moon, 2020) as the glbwarm data set as well as from Hayes' website.  The data set is used by Hayes in several examples. Used here by kind permission of Erik Nisbet. 

Although the processR package has been removed from CRAN, an earlier version had the data. 
}
\references{

Hayes, Andrew F. (2013) Introduction to mediation, moderation, and conditional process analysis: A regression-based approach. Guilford Press.

Moon K (2023). processR: Implementation of the 'PROCESS' Macro_. R package version 0.2.8,


}
\examples{
data(globalWarm)
psych::lowerCor(globalWarm)
#compare to Hayes p 254-258
psych::lmCor(govact ~ negemot * age + posemot +ideology+sex,data=globalWarm,std=FALSE)
}
\keyword{datasets}
