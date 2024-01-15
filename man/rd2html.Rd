\name{rd2html}
\alias{rd2html}

\title{Convert all Rd files in a directory to HTML files in a new directory}

\description{Just a wrapper for tools::RdHTML to find a directory (e.g., the Man directory of help files) and convert them to HTML files in a new directory.  Useful for adding HTML help files to a local web page.
}
\usage{
rd2html(inDir =NULL,outDir=NULL, nfiles=NULL,package="psych",file=NULL)
}

\arguments{
  \item{inDir}{The input directory.  If NULL,then a file in a directory will be searched for using file.choose()}
  \item{outDir}{Where to write the output files}

  \item{nfiles}{If not NULL, then how many files should be written}

  \item{package}{name of package }
  \item{file}{If specified, just convert this one file to HTML}
}
\details{Just a wrapper for Rd2HTML calling some file tools.  An interesting use of the function is to precheck whether all the help files are syntactically correct.
}


\author{William Revelle
}

\seealso{See Also as \code{\link{filesList}}, \code{\link{filesInfo}}
}
\examples{
if(interactive()) {
#This is an  interactive function whic require interactive input and thus is not given as examples
rd2html()

}
}
\keyword{utilities}
\keyword{package}
