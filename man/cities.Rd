\name{cities}
\alias{cities}
\alias{city.location}
\docType{data}
\title{Distances between 11 US cities}
\description{Airline distances between 11 US cities may be used as an example for multidimensional scaling or cluster analysis.
}
\usage{data(cities)}
\format{
  A data frame with 11 observations on the following 11 variables.
  \describe{
    \item{\code{ATL}}{Atlana, Georgia}
    \item{\code{BOS}}{Boston, Massachusetts}
    \item{\code{ORD}}{Chicago, Illinois}
    \item{\code{DCA}}{Washington, District of Columbia}
    \item{\code{DEN}}{Denver, Colorado}
    \item{\code{LAX}}{Los Angeles, California}
    \item{\code{MIA}}{Miami, Florida}
    \item{\code{JFK}}{New York, New York}
    \item{\code{SEA}}{Seattle, Washington}
    \item{\code{SFO}}{San Francisco, California}
    \item{\code{MSY}}{New Orleans, Lousianna}
    
  }
}
\details{An 11 x11  matrix of distances between major US airports. This is a useful demonstration of multiple dimensional scaling. 

city.location is a dataframe of longitude and latitude for those cities.

Note that the 2 dimensional MDS solution does not perfectly capture the data from these city distances.   Boston, New York and Washington, D.C. are located slightly too far west, and Seattle and LA are slightly too far south.
}
\source{ \url{https://www.timeanddate.com/worldclock/distance.html}
}

\examples{

data(cities)
city.location[,1] <- -city.location[,1] #included in the cities data set
plot(city.location, xlab="Dimension 1", ylab="Dimension 2",
   main ="Multidimensional scaling of US cities")
#do the mds   
city.loc <- cmdscale(cities, k=2) #ask for a 2 dimensional solution  round(city.loc,0) 
city.loc <- -city.loc  #flip the axes
 city.loc <- psych::rescale(city.loc,apply(city.location,2,mean),apply(city.location,2,sd))
points(city.loc,type="n") #add the date point to the map
text(city.loc,labels=names(cities))

\dontrun{   #we need the maps package to be available
#an overlay map can be added if the package maps is available
if(require(maps)) {
  map("usa",add=TRUE)
}
}
}
\keyword{datasets}
