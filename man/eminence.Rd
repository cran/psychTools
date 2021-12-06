\name{eminence}
\alias{eminence}
\docType{data}
\title{Eminence of 69 American Psychologists}

\description{
Marco Del Giudice criticized an earlier study by Simonton for using partial regression weights to estimate the importance of various predictors of rated eminence. This is a nice example of the (mis)interpretation of beta weights of highly correlated predictors.}
\usage{data("eminence")}
\format{
  A data frame with 69 observations on the following 9 variables.
  \describe{
    \item{\code{name}}{a character vector}
    \item{\code{reputation}}{Log of rated reputation}
    \item{\code{birth.year}}{Year of birth}
    \item{\code{first.year}}{Year of first cited publicatin}
    \item{\code{last.year}}{Year of last cited publication}
    \item{\code{works}}{Log of number of publications}
    \item{\code{citations}}{Log of number of citations}
    \item{\code{composite}}{A composite index of publications}
    \item{\code{h}}{The 'h' index of citations}
}
}
\details{Simonton (1997, 2014) discusses various estimates of eminence among 69  psychologists born between 1842 and 1912 and reports that the regression weights are small and interprets this as meaning  number of publications and citations are not very important.  Del Giudice (2020) points out that  citations and the number of publications are highly collinear and thus while their independent contributioons are small, their joint effect is quite large (R= .69 ).  These data are given here as an example of multiple correlation and partial correlation
}
\source{
Del Giudice (2020) links to a web page with the data. 
}
\references{

Marco Del Giudice (2020).   How Well Do Bibliometric Indicators Correlate With Scientific Eminence? A Comment on Simonton (2016).  Perspective in Psychological Science, 15, 202-203.

Simonton, D. K. (1992). Leaders of American psychology, 1879-1967: Career development, creative output, and professional achievement. Journal of Personality and Social Psychology, 62, 5-17.

Simonton, D. K. (2016). Giving credit where credit is due: Why it's so hard to do in psychological science. Perspectives on Psychological Science, 11, 888-892.

 }

\examples{
data(eminence)
psych::lowerCor(eminence)
cs <- psych::cs
psych::partial.r(eminence, x= cs(reputation, works, citations),y=cs(birth.year))
psych::setCor(reputation ~ works + h +  first.year,data=eminence)
}
\keyword{datasets}