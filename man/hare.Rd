\name{hare}
\alias{hare}

\title{
Counting Hare votes (Ranked choice voting)
}
\description{Ranked Choice Voting may be counted using the Hare system. All voter choices are considered, the votes for the candidate with the lowest first choice votes are distributed to the remaining candidates.  This procedure is repeated until the number to be selected is determined.
}
\usage{
hare(x, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A data frame or matrix of rank orders for candidates (columns) by voters (rows).
}
  \item{n}{How many winners to report}
}
\details{A short function to convert rank orders into preferences.  Ranked Choice Voting is used by many small societies to choose officers. 

A more elegant approach may be found in the vote package.

For more information, see the help pages for vote or "for a description of the single transferable vote system see https://imstat.org/elections/single-transferable-voting-system/.
" from the vote help page.
}
\value{
A vector of the winning candidates and their votes.
}

\author{
William Revelle
}
\examples{
##-Load the votes package and compare the results to hare.
#not run
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
 \keyword{cluster }
\keyword{misc }


