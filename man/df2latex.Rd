\name{df2latex}
\alias{df2latex}
\alias{cor2latex}
\alias{fa2latex}
\alias{omega2latex}
\alias{irt2latex}
\alias{ICC2latex}
\alias{df2rtf}
\alias{cor2rtf}
\alias{fa2rtf}

\title{Convert a data frame, correlation matrix, or factor analysis output to a LaTeX or rtf table}

\description{A set of handy helper functions to convert data frames or matrices to LaTeX or rtf tables. Although Sweave is the preferred means of converting R output to LaTeX, it is sometimes useful to go directly from a data.frame or matrix to a LaTeX table.   cor2latex will find the correlations and then create a lower (or upper) triangular matrix for latex output. cor2rtf will do the same for rtf output. fa2latex and fa2rtf will create the latex commands for showing the loadings and factor intercorrelations. As the default option, tables are prepared in an approximation of APA format. 
}
\usage{
df2latex(x,digits=2,rowlabels=TRUE,apa=TRUE,short.names=TRUE,font.size ="scriptsize",
       big.mark=NULL,drop.na=TRUE, heading="A table from the psych package in R",
   caption="df2latex",label="default", char=FALSE, 
    stars=FALSE,silent=FALSE,file=NULL,append=FALSE,cut=0,big=0,abbrev=NULL,long=FALSE)
cor2latex(x,use = "pairwise", method="pearson", adjust="holm",stars=FALSE,
       digits=2,rowlabels=TRUE,lower=TRUE,apa=TRUE,short.names=TRUE,
     font.size ="scriptsize", heading="A correlation table from the psych package in R.",
      caption="cor2latex",label="default",silent=FALSE,file=NULL,append=FALSE,cut=0,big=0)
fa2latex(f,digits=2,rowlabels=TRUE,apa=TRUE,short.names=FALSE,cumvar=FALSE,
       cut=0,big=.3,alpha=.05,font.size ="scriptsize",long=FALSE,
       heading="A factor analysis table from the psych package in R",
       caption="fa2latex",label="default",silent=FALSE,file=NULL,append=FALSE) 
omega2latex(f,digits=2,rowlabels=TRUE,apa=TRUE,short.names=FALSE,cumvar=FALSE,cut=.2,
        big=.3,font.size ="scriptsize", 
        heading="An omega analysis table from the psych package in R",
        caption="omega2latex",label="default",silent=FALSE,file=NULL,append=FALSE)

irt2latex(f,digits=2,rowlabels=TRUE,apa=TRUE,short.names=FALSE,
       font.size ="scriptsize", heading="An IRT factor analysis table from R",
       caption="fa2latex",label="default",silent=FALSE,file=NULL,append=FALSE)
ICC2latex(icc,digits=2,rowlabels=TRUE,apa=TRUE,ci=TRUE,
   font.size ="scriptsize",big.mark=NULL, drop.na=TRUE,
    heading="A table from the psych package in R",
   caption="ICC2latex",label="default",char=FALSE,silent=FALSE,file=NULL,append=FALSE) 

#not all options are yet implemented in these next three functions.   
df2rtf(x,file=NULL, digits=2,rowlabels=TRUE,width=8.5,old=NULL, apa=TRUE,short.names=TRUE,
	font.size =10,big.mark=NULL, drop.na=TRUE, 
	heading="A table from the psych package in R",
	caption="Created with df2rtf",label="default",char=FALSE,stars=FALSE,silent=FALSE,
	append=FALSE,cut=0,big=.0,abbrev=NULL,long=FALSE) 

cor2rtf(x,file=NULL, use = "pairwise", method="pearson", adjust="holm", digits=2,
	rowlabels=TRUE,width=8.5,lower=TRUE,old=NULL, apa=TRUE,short.names=TRUE,
	font.size =10,big.mark=NULL, drop.na=TRUE, 
	heading="A correlation matrix from the psych package in R",
	caption="Created with cor2rtf.   left justify output if stars", 
	label="default",char=FALSE,stars=FALSE,silent=FALSE,
	append=FALSE,cut=0,big=.0,abbrev=NULL,long=FALSE) 
	
fa2rtf(f,file=NULL, use = "pairwise", method="pearson", adjust="holm", digits=2,
	rowlabels=TRUE,width=8.5,lower=TRUE,old=NULL, apa=TRUE,short.names=TRUE,
	font.size =10,big.mark=NULL, drop.na=TRUE, 
	heading="A Factor analysis   from the psych package in R",
	caption="Created with fa2rtf. ",label="default",char=FALSE,silent=FALSE,
	append=FALSE,cut=0,big=.0,abbrev=NULL)  
}

\arguments{
  \item{x}{A data frame or matrix to convert to LaTeX. If non-square, then correlations will be found prior to printing in cor2latex}
  \item{digits}{Round the output to digits of accuracy.  NULL for formatting character data}
  \item{abbrev}{How many characters should be used in column names --defaults to digits + 3}
  \item{rowlabels}{If TRUE, use the row names from the matrix or data.frame}
  \item{short.names}{Name the columns with abbreviated rownames to save space}
  \item{apa}{If TRUE formats table in APA style}
  \item{cumvar}{For factor analyses, should we show the cumulative variance accounted for?}
  \item{font.size}{e.g., "scriptsize", "tiny" or anyother acceptable LaTeX font size.}
  \item{heading}{The label appearing at the top of the table}
  \item{caption}{The table caption}
  \item{lower}{in cor2latex, just show the lower triangular matrix}
  \item{f}{The object returned from a factor analysis using \code{\link[psych]{fa}} or \code{\link[psych]{irt.fa}}. }
  \item{label}{The label for the table}
  \item{big.mark}{Comma separate numbers large numbers (big.mark=",")}
  \item{drop.na}{Do not print NA values}
  \item{method}{When finding correlations, which method should be used (pearson)}
  \item{use}{use="pairwise" is the default when finding correlations in cor2latex}
  \item{adjust}{If showing probabilities, which adjustment should be used (holm)}
  \item{stars}{Should probability 'magic astericks' be displayed in cor2latex (FALSE)}
  \item{char}{char=TRUE allows printing tables with character information, but does not allow for putting in commas into numbers}
  \item{cut}{In omega2latex, df2latex and fa2latex, do not print abs(values) < cut }
  \item{big}{In fa2latex and df2latex boldface those abs(values) > big}
  \item{alpha}{If fa has returned confidence intervals, then what values of loadings should be boldfaced?}
  \item{icc}{Either the output of an ICC, or the data to be analyzed.}
  \item{ci}{Should confidence intervals of the ICC be displayed}
  \item{silent}{If TRUE, do not print any output, just return silently -- useful if using Sweave}
  \item{file}{If specified, write the output to this file}
  \item{append}{If file is specified, then should we append (append=TRUE) or just write to the file}
  \item{long}{if TRUE, then do long tables.  (requires the longtable package in latex)}
  \item{old}{When appending output with df2rtf, old is the output from the prior run.}
  \item{width}{page width in inches for df2rtf }
  }
\value{A LaTeX table.  Note that if showing "stars" for correlations, then one needs to use the siunitx  package in LaTex. 
  The entire LaTeX output is also returned invisibly.  If using Sweave to create tables, then the silent option should be set to TRUE and the returned object saved as a file.  See the last example.
  
  Finally, some users have asked for the ability to convert these output tables into HTML.  This may be done using the tth package. 
  
  Three functions to write to rtf files (for use in various proprietary word processing languages) have been added with version 2.4.3.  These will write to an rtf file and may be formatted directly.  df2rtf takes a data frame and writes it as a table with header information.
  
  cor2rtf will take either a data matrix (and find the correlations) or just a correlation matrix.  "magic astericks " can be added to the correlations using the stars=TRUE option.  In this case, the result table can be left justified in a word processing language to get the numbers to appear correctly justified.
  
  fa2latex and fa2rtf can take the output from  either a factor analysis or from fa.lookup.
  }

\author{William Revelle with suggestions from Jason French and David Condon and Davide Morselli}

\seealso{
The many LaTeX conversion routines in Hmisc.

To convert these LaTex objects to HTML, you should install the tth package.

Consider the last example for creating HTML

 
}
\examples{
df2latex(psych::Thurstone,rowlabels=FALSE,apa=FALSE,short.names=FALSE,
        caption="Thurstone Correlation matrix")
df2latex(psych::Thurstone,heading="Thurstone Correlation matrix in APA style")

df2latex(psych::describe(psych::sat.act)[2:10],short.names=FALSE)
cor2latex(psych::Thurstone)
cor2latex(psych::sat.act,short.names=FALSE)
fa2latex(psych::fa(psych::Thurstone,3),heading="Factor analysis from R in quasi APA style")


#to write to rtf file
#replace the temporary file name with something more useful
fn <- tempfile(pattern="example",fileext=".rtf")  #create a temporary file
#better is to create a local file
# e.g. fn <- "rtf_example.rtf"

cor2rtf(sat.act, file=fn)   #write to the file

dd <- psych::describe(sat.act)
temp <- df2rtf(dd, file=fn, append=TRUE, width=12) #write and keep open
temp1 <-  cor2rtf(sat.act,old=temp,caption=date(), append=TRUE)  #use date as caption 
cor2rtf(sat.act, old=temp1, stars=TRUE) #close the file
#now open this with your word processor and reformat with left justify

#now write a factor analysis output to an output file
# e.g. fn <- "rtf_example.rtf"
f5 <- psych::fa(bfi,5)
temp <- fa2rtf(f5, width=12, file=fn, append=TRUE)  #a normal fa output
fl <- psych::fa.lookup(f5, dictionary=bfi.dictionary)
fa2rtf(fl, old = temp)
##now open this with your word processor

#To convert these latex tables to HTML

#f3.lat <- fa2latex(psych::fa(psych::Thurstone,3),
#    heading="Factor analysis from R in quasi APA style")
#library(tth)
#f3.ht <- tth(f3.lat)
#print(as.data.frame(f3.ht),row.names=FALSE)

###

 #If using Sweave to create a LateX table as a separate file then set silent=TRUE
#e.g., 
#LaTex preamble 
#....
#<<print=FALSE,echo=FALSE>>= 
#f3 <- fa(Thurstone,3)
#fa2latex(f3,silent=TRUE,file='testoutput.tex')
#@
#
#\input{testoutput.tex}

 }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ utilities }

