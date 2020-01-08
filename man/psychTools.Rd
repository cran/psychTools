\name{psychTools}
\alias{psychTools}
\docType{data}
\title{psychTools:  datasets and utility functions to accompany the psych package
}
\description{
PsychTools includes the larger data sets used by the \code{\link[psych]{psych}} package and also includes a few general utility functions such as the \code{\link{read.file}} and \code{\link{read.clipboard}} functions. The data sets ara made available for demonstrations of a variety of psychometric functions.
}

\details{
See the various helpfiles listed in the index or as links from here.  Also see the main functions in the psych package \code{\link[psych]{00.psych-package}}.

Data sets from the SAPA/ICAR project:

\tabular{ll}{
\code{\link{ability}} \tab 16 ICAR ability items scored as correct or incorrect for 1525 participants.  \cr
\code{\link{iqitems}} \tab multiple choice IQ items (raw responses) \cr
\code{\link{affect}}   \tab  Two data sets of affect and arousal scores as a function of personality and movie conditions \cr

\code{\link{bfi}}  \tab 25 Personality items representing 5 factors from the SAPA project for 2800 participants \cr
bfi.dictionary \tab Dictionary of the bfi \cr

\code{\link{epi}}  \tab Eysenck Personality Inventory (EPI) data for 3570 participants \cr
epi.dictionary \tab The items for the epi \cr
\code{\link{epi.bfi}}  \tab 13 personality scales from the Eysenck Personality Inventory and Big 5 inventory  \cr

\code{\link{epiR}}  \tab 474 participants took the epi twice \cr


\code{\link{msq}}  \tab 75 mood items from the Motivational State Questionnaire for 3896 participants \cr
\code{\link{msqR}} \tab  75 mood items from the Motivational State Questionnaire for 3032 unique participants \cr
\code{\link{tai}}  \tab Trait Anxiety data from the PMC lab matching the sai sample. 3032 unique subjects \cr
\code{\link{sai}}  \tab  State Anxiety data from the PMC lab over multiple occasions. 3032 unique subjects. \cr
sai.dictionary \tab  items used in the sai \cr

\code{\link{spi}}  \tab 4000 cases  from the SAPA Personality Inventory including an item dictionary and scoring keys. \cr
spi.dictionary \tab The items for the spi \cr
spi.keys \tab  Scoring keys for the spi \cr



}  

Historically interesting data sets 
\tabular{ll}{

\code{\link{burt}}  \tab 11 emotional variables from Burt (1915) \cr
\code{\link{galton}}  \tab Galtons Mid parent child height data \cr
\code{\link{heights}}  \tab  A data.frame of the Galton (1888) height and cubit data set \cr
\code{\link{cubits}} \tab Galtons example of the relationship between height and cubit or forearm length \cr
\code{\link{peas}} \tab Galtons Peas \cr
\code{\link{cushny}}  \tab The data set from Cushny and Peebles (1905) on the effect of three drugs on hours of sleep, used by Student (1908) \cr
\code{\link{holzinger.swineford}} \tab 26 cognitive variables + 7 demographic variables for   301 cases from Holzinger and Swineford. 
}

Miscellaneous example data sets

\tabular{ll}{
\code{\link{blant}}  \tab A 29 x 29 matrix that produces weird factor analytic results \cr
\code{\link{blot}}  \tab Bonds Logical Operations Test - BLOT \cr
\code{\link{cities}}  \tab Distances between 11 US cities \cr 
city.location \tab and their geograpical location \cr
\code{\link{income}}   \tab US family income from US census 2008 \cr
all.income  \tab US family income from US census 2008 \cr
\code{\link{neo}}  \tab NEO correlation matrix from the NEO_PI_R manual \cr
\code{\link{Schutz}} \tab The Schutz correlation matrix example from Shapiro and ten Berge \cr
\code{\link{Spengler}} \tab The Spengler and Damian correlation matrix example from Spengler, Damian and Roberts (2018) \cr
\code{\link{Damian}} \tab Another correlation matrix from Spengler, Damian and Roberts (2018) \cr
\code{\link{usaf}} \tab A correlation of  17 body size (anthropometric) measures from the US Air Force. Adapted from the Anthropometric package.\cr
veg \tab Paired comparison of preferences for 9 vegetables (scaling example) \cr
}


Functions to convert various objects to latex

\tabular{ll}{
\code{\link{fa2latex}}  \tab Convert a data frame, correlation matrix, or factor analysis output to a LaTeX table \cr
\code{\link{df2latex}}  \tab Convert a data frame, correlation matrix, or factor analysis output to a LaTeX table \cr
\code{\link{ICC2latex}} \tab Convert an ICC analyssis output to a LaTeX table \cr
\code{\link{irt2latex}} \tab Convert an irt analysis output to a LaTeX table \cr
\code{\link{cor2latex}} \tab Convert a  correlation matrix output to a LaTeX table \cr
omega2latex \tab Convert a data frame, correlation matrix, or factor analysis output to a LaTeX table \cr
}

File manipulation functions
\tabular{ll}{
\code{\link{fileCreate}}  \tab  Create a file \cr
fileScan \tab Show the first few lines of multitple files \cr
filesInfo \tab  Show the information for all files in a directory \cr
filesList \tab Show the names of all files in a directory \cr
}

\code{\link{dfOrder}}  Sorts a data frame

File input/output functions 

\tabular{ll}{
\code{\link{read.clipboard}}  \tab Shortcuts for reading from the clipboard or a file \cr
read.clipboard.csv \tab \cr
read.clipboard.fwf \tab \cr
read.clipboard.lower \tab \cr
read.clipboard.tab \tab \cr
read.clipboard.upper \tab \cr
\code{\link{read.file}} \tab Read a file according to its suffix \cr
read.file.csv \tab \cr
read.https \tab \cr
\code{\link{write.file}} \tab Write data to a file \cr
write.file.csv \tab \cr
}

}



\examples{
psych::describe(ability)
}
\keyword{datasets}
