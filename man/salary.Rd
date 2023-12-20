\name{salary}
\alias{salary}
\docType{data}
\title{Salary example from Cohen, Cohen, Aiken and West (2003)}

\description{
Four predictors of academic salary are used as examples in Cohen, Cohen, Aiken, and West (2003) may be used for demonstration purposes of multiple regression and multiple correlation.  
}
\usage{data("salary")}
\format{
  A data frame with 62 observations on the following 5 variables.
  \describe{
    \item{\code{time}}{Time since Ph.D.}
    \item{\code{publications}}{Number of publications}
    \item{\code{female}}{gender Male=0, Female =1}
    \item{\code{citations}}{Number of citations}
    \item{\code{salary}}{Salary}
  }
}
\details{
Two extended examples multiple regression in CCAW are discussed in Chapter 3.

These are nice examples of the use of the \code{link{psych::lmCor}} and \code{link{psych::partial.r}} functions. 

Note that example data set in Table 3.2.1 (p 67)  is just the first 15 cases of the complete data set used in Table 3.5.1 (page 81) and included in this data set. 
}
\source{
CD accompanying Cohen, Cohen, Aiken and West (2003) (used with the kind permission of Leona Aiken and Steven West)
}
\references{
Cohen, J., Cohen, P., West, S. G., & Aiken, L. S. (2003). Applied multiple regression/correlation analysis for the behavioral sciences (3rd ed.). Lawrence Erlbaum Associates Publishers.
}
\examples{
data(salary)
psych::describe(salary)
psych::pairs.panels(salary)
#the standardized coefficients
psych::lmCor(salary ~ time + publications, data=salary)
#or the raw coefficients
mod <- psych::lmCor(salary ~ time + publications, data=salary, std=FALSE)
mod 
#show the part correlations 
psych::partial.r(salary ~ time - publications, data=salary, part=TRUE)
psych::partial.r(salary ~ -time + publications, data=salary, part=TRUE)
#show the predicted salaries based upon the model 
mod <- psych::lmCor(salary ~ time + publications+ citations + female,
      data=salary, std=FALSE)
predicted.salary <- psych::predict.psych(mod,salary)
head(predicted.salary)#compare to CCAW p 81
## 

}
\keyword{datasets}
