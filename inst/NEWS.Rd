\name{NEWS}
\title{News for Package 'psychTools'}

\section{Changes in psychTools version 2.4.1 (2024-01-14)}{ 
  \subsection{Introduction}{
    \itemize{
      \item Version 2.4.2 is the development release of the psychTools package. It is available as a source file for Macs or PCs in the repository at \url{https://personality-project.org/r/}. The released version on CRAN is 2.4.1  The second digit reflects the year (i.e., 2024), the third set the month (i.e., 2.3.12 was released in December of 2023, the last two digits of development versions reflect  either an minor change or the day of any modifications, e.g. 1.8.3.3 was the third attempt to get 1.8.3 released.  1.7.8 was  released in August, 2017.
    \item To install the development version, use the command: install.packages("psychTools", repos="https://personality-project.org/r/", type="source"). Remember to restart R and library(psych) and library(psychTools) to make the new version active.
    \item To see the date of the new version, use packageDate("psychTools").  (I sometimes update the packages on my server without changing the version number.)

       
      \item The psychTools package includes functions and data sets to accompany the psych package which does classic and modern psychometrics and to analyze personality  and experimental psychological data sets. The psych package has been developed as a supplement to courses in research methods in psychology, personality research, and graduate level psychometric theory. The functions are a supplement to the text (in progress): An introduction to psychometric theory with applications in R. 
      \item These data sets are meant to be useful adjuncts to teaching and research.
      \item Additional functions are added sporadically.
      \item This NEWS file reports changes that have been made as the package has been developed.
      \item To report bugs, send email to \url{mailto:revelle@northwestern.edu} using bug.report.  Remember to include the systemInfo() information and a Minimal Working Example. 
   }
    }
 \subsection{Additions}{
  \itemize{
      \item Added rd2html to allow for conversion of Rd files to HTML.  This allows for single files or complete directories (e.g. the help files )
      \item Various improvements to vignettes to make them clearer and updated the links
    
    }
    }
  }  
 
 \section{Changes in psychTools version 2.3.12 (2023-12-22 )}{
       \subsection{Additions}{
         \itemize{
         \item Added the salary dataset from Cohen, Cohen, Aiken and West (2003), chapter 3.
 
  }
 }
 }
 
   \section{Changes in psychTools version 2.3.8 (2023-08-22 )}{
       \subsection{Additions}{
         \itemize{
         \item Added the key.name parameter to vJoin
         \item Added function rearrange to allow new ordering of dataframe columns
         \item Added function wide2long to convert wide output to long output
 
  }
 }
 }
  \section{Changes in psychTools version 2.3.6 (2023-06-20 )}{
       \subsection{Additions}{
         \itemize{
          \item Added vJoin and combineMatrices to allow for some basic file manipulation.
          \item Added recode  to allow recoding of variables (requested by Yaelle Pierre).
          \item Added selectBy and splitBy to implement trivial but helpful actions.
          \item Improved help for globalWarm and updated various vignettes.
 }
 }
 }
 
 \section{Changes in psychTools version 2.2.10 (2021-09-12 )}{
       \subsection{Additions}{
         \itemize{
          \item Added the colom data set 
 }
 }
 }
 
  \section{Changes in psychTools version 2.2.9 (2021-09-12 )}{
    \subsection{Additions}{
     \itemize{
     \item Modified fa2latex to handle output from faLookup.
     \item Added more scoring keys to the Athenstaedt data to allow for demonstrations of alpha and omega
     \item Add cross references from Athenstaedt and GERAS (Gruber) data sets.
     \item Added a correlation matrix from Ann Zola et al. (2021) showing the validity of self reports compared to peer ratings. 
     }
   }
   
   	}
 
  \section{Changes in psychTools version 2.2.5 (2021-05-01 )}{
    \subsection{Additions}{
     \itemize{
     \item Added the Athenstaedt data set as well as a dictionary and scoring keys.
     }
   }
   
   	}
 
  \section{Changes in psychTools version 2.2.4 (2021-04-10 )}{
    \subsection{Additions}{
     \itemize{
     \item Improved the GERAS dictionary to make the graphics match manuscript by Eagly and Revelle (2022)
     }
   }
   
   	}
 
  \section{Changes in psychTools version 2.2.3 (2021-02-21 )}{
    \subsection{Additions}{
     \itemize{
     \item Improved dfOrder to treat matrices as well.  This quasi mimics fa.lookup in that it sorts numeric values by largest in the row rather than lexicographically.
    \item Added the long option to fa2latex (requested by Alexander Weiss)
     }
   }
   
   	}
 
 \section{Changes in psychTools version 2.1.12 (2021-12-06 )}{
    \subsection{Additions}{
     \itemize{
     \item Added the eminence data set from Simonton and del Giudice as a nice example of misinterpretation of beta weights in the presence of highly colinear predictors.
     \item Updated this news file for version 2.1.6 to reflect datasets added.
    
     }
   }
   
   	}
 
 \section{Changes in psychTools version 2.1.6 (2021-06-20 )}{
    \subsection{Additions}{
     \itemize{
      \item Added the GERAS dataset from Gruber et al. (2020) to show Mahalobnis distances in scatterHist.  
     \item Added the Spengeler and Damian data sets from Project Talent (nice example of mediation)
     \item Added the USAF data set of anthropometric measurements  (to help understand what a g factor does or does not mean)
     \item Added the globalWarm data set from Erik Nisbett (to help in mediation)
     \item Added the ability to do long tables in df2latex (requested by Lizz Dworak)
   
     }
   }
   

   	}
  \section{Changes in psychTools version 2.1.3 (2021-03-14)}{
    \subsection{Additions}{
     \itemize{
     \item Moved four vignettes over from psych to make psych smaller.
     \item Current vignettes here include overview, omega, factor, and mediation
     \item Checked against psych_2.0.12 and psych_2.1.3
     \item Added B5 and L27 columns to the spi.dictionary.
     }
   }
   
    \subsection{Bugs Fixed}{
    	\itemize{
   \item Nothing yet
   	}
   	}
   	}
 
  \section{Changes in psychTools version 2.0.9 (2020-09-14)}{
    \subsection{Additions}{
     \itemize{
     \item Added the BFI 100 items as a data set
     }
   }
   
    \subsection{Bugs Fixed}{
    	\itemize{
   \item .dat and RData files were not being read in read.file following a clean up in code.
   	}
   	}
   	}
 
 
 \section{Changes in psychTools version 2.0.8 (2020-08-04)}{
    \subsection{Additions}{
    \itemize{\item ability.keys to the ability.rda file
    \item Modified df2latex to allow mixed numeric and character with rounding of numeric

   }
   }
    \subsection{Bugs Fixed}{
    	\itemize{
   \item None yet
   	}
   	}
   	}
 
 \section{Changes in psychTools version 2.0.6 (2020-1-12)}{
    \subsection{Additions}{
    \itemize{
    \item Modified df2latex to allow mixed numeric and character with rounding of numeric

   }
   }
    \subsection{Bugs Fixed}{
    	\itemize{
   \item Fixed dfOrder to handle Null data
   \item Fixed holzinger.swineford and holzinger.raw for two cases (180 and 231) where K. Widaman had provided incorrect values (thanks to Keith for finding this).
   	}
    	}
    	}
 \section{Changes in psychTools version 1.9.10 (2018-06-24)}{
    \subsection{Additions}{
    \itemize{
    \item Added the holzinger.raw, holzinger.swineford and holzinger.dictionary data sets. The data come from Keith Widaman.

   }
   }
    \subsection{Bugs Fixed}{
    	\itemize{
   \item None yet.
   	}
    	}
    	}
 
 \section{Changes in psychTools version 1.9.6 (2018-06-24)}{
    \subsection{Additions}{
    \itemize{
    \item Added bfi.keys to the bfi data set 
    \item Added examples to the sai data set to match Revelle and Condon 2019
    \item Added spengler data set 
   }
   }
    \subsection{Bugs Fixed}{
    	\itemize{
   \item Minor correction to the cities help file  
   	}
    	}
    	}
  
   \section{Changes in psychTools version 1.9.5 (2018-05-25)}{
    \subsection{Additions}{
    \itemize{
    \item Data sets and a few helper functions switched over from psych to psychTools to make psych a smaller package.
    \item Data sets included are: ability, bfi, epi.bfi,income, iqitems, msq, msqR, neo, sai, spi, and tai.
    \item Helper functions include the df2latex set, dfOrder, and the various file utilities such as read.clipboard.
   \item Version number increased to 1.9.5.18 as we work through minor fixes to the submission to meet the newly enforced more stringent requirements of CRAN
   \item Changed cat and print in interactive functions  (fileCreate) to message() following request from CRAN
   \item Following yet another request from CRAN, changed the read.file function to not automatically load an .rda file, but rather suggest how to load it.
   \item Changed the use of  \%in\% to is.element to get around some problems in the msqR help file
   \item Changed the examples in read.clipboard to donttest instead of dontrun because they are interactive
   \item Changed all dontrun to donttest following request from CRAN.
    
      }
       }
    \subsection{Bugs Fixed}{
    	\itemize{
  \item None yet
   	}
    	}
    	}
  
