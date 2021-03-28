\name{BFI.adjectives.dictionary}
\alias{BFI.adjectives.dictionary}
\alias{bfi.adjectives.dictionary}
\alias{big5.adjectives.dictionary}

\docType{data}
\title{Dictionary for the  100 Big Five Adjectives
}
\description{Lew Goldberg organized 100 adjectives to measure 5 factors of personality (The Big5).  500 hundred participants were given these adjectives along with other personality measures.  This dictionary allows for easy item labeling of the results. 
~
}
\usage{data("BFI.adjectives.dictionary")
 }
\format{
  A data frame with 100 observations on the following 2 variables.
  \describe{
    \item{\code{numer}}{a character vector of the item label}
    \item{\code{Item}}{a character vector of the actual adjectives}
  }
}
\details{
Keying information for the 100 adjectives:


}
\source{Data collected at the Personality, Motivation, and Cognition Laboratory, Northwestern University.}
\references{Lewis R. Goldberg,(1992) The development of markers for the Big-Five factor structure, Psychological Assessment, 4 (1) 26-42.
}

\seealso{\code{\link{big5.100.adjectives}} for examples of the data. 
\code{\link{msqR}} for 3896 participants with scores on five scales of the EPI.  \code{\link{affect}} for an example of the use of some of these adjectives in a mood manipulation study.} 

\examples{
data(BFI.adjectives.dictionary) #this includes the bfi.adjectives.keys
bfi.adjectives.keys <- list(
Agreeableness =																				
psych::cs(V2,	-V11, V14, V15, -V19,	-V21, V29,	-V31, V32,	V48, V55,-V61,	-V63,	
V69, V76, -V78,	-V79, -V90,	-V94,	V99), 
Conscientiousness	= psych::cs(V9,	-V10,	V13, -V20,	V22, -V30, -V37, -V38, -V39,	
     V50,  -V51, V53, V56, V57, -V67,	V68, V70, V73, -V82, -V95),
Extraversion = psych::cs(V1,V5,	-V6,V7,	V17, V24, V26, -V40,-V45, -V58,	-V60,-V65,
     V71,  -V74,	-V77,	V92, -V96,	V97, V98, -V100),
Neuroticism= psych::cs(V3, V23, V25, V27,V28, V33,-V36, V42, V46,V47, V49, V52,-V59,V62,
 V72, V75, -V81,-V83,-V84, -V85),
Openness = psych::cs(V4,V8,V12, V16, V18,V34, -V35,V41, V43, V44, V54,	-V64,-V66, -V80,
-V86, -V87, -V88, -V89, -V91, -V93)
	)
	
psych::lookupFromKeys(bfi.adjectives.keys,bfi.adjectives.dictionary,20)

}
\keyword{datasets}
