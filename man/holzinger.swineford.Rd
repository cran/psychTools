\name{holzinger.swineford}
\alias{holzinger.swineford}
\alias{holzinger.raw}
\alias{holzinger.dictionary}
\docType{data}
\title{
The raw and transformed data from Holzinger and Swineford, 1939
}
\description{
A classic data set in psychometrics is that from Holzinger and Swineford (1939).  A 4 and 5 factor solution to 24 of these variables problem is presented by Harman (1976), and 9 of these are used by the lavaan package.  The two data sets were supplied by Keith Widaman.  
}
\usage{data(holzinger.swineford)
     data(holzinger.raw)
     data(holzinger.dictionary)
     }
\format{
  A data frame with 301 observations on the following 33 variables. Longer descriptions taken from Thompson, (1998).
  \describe{
    \item{\code{case}}{a numeric vector}
    \item{\code{school}}{School Pasteur  or Grant-White}
    \item{\code{grade}}{Grade (7 or 8)}
    \item{\code{female}}{male = 1, female = 2}
    \item{\code{ageyr}}{age in years}
    \item{\code{mo}}{months over year}
    \item{\code{agemo}}{Age in months }
    \item{\code{t01_visperc}}{Visual perception test from Spearman VPT  Part I}
    \item{\code{t02_cubes}}{Cubes, Simplification of Brighams Spatial Relations Test}
    \item{\code{t03_frmbord}}{Paper formboard-Shapes that can be combined to form a target}
    \item{\code{t04_lozenges}}{Lozenges from Thorndike-Shapes flipped over then identify target}
    \item{\code{t05_geninfo}}{General Information Verbal Test}
    \item{\code{t06_paracomp}}{Paragraph Comprehension Test}
    \item{\code{t07_sentcomp}}{Sentence Completion Test}
    \item{\code{t08_wordclas}}{Word clasification-Which word not belong in set}
    \item{\code{t09_wordmean}}{Word Meaning Test}
    \item{\code{t10_addition}}{Speeded addition test}
    \item{\code{t11_code}}{Speeded codetest-Transform shapes into alpha with code}
    \item{\code{t12_countdot}}{Speeded counting of dots in shap}
    \item{\code{t13_sccaps}}{Speeded discrimation of straight and curved caps}
    \item{\code{t14_wordrecg}}{Memory of Target Words}
    \item{\code{t15_numbrecg}}{Memory of Target Numbers}
    \item{\code{t16_figrrecg}}{Memory of Target Shapes}
    \item{\code{t17_objnumb}}{Memory of object-Number association targets}
    \item{\code{t18_numbfig}}{Memory of number-Object association targets}
    \item{\code{t19_figword}}{Memory of figure-Word association target}
    \item{\code{t20_deduction}}{Deductive Math Ability}
    \item{\code{t21_numbpuzz}}{Math number puzzles}
    \item{\code{t22_probreas}}{Math word problem reasoning}
    \item{\code{t23_series}}{Completion of a Math Number Series}
    \item{\code{t24_woody}}{Woody-McCall mixed math fundamentals test}
    \item{\code{t25_frmbord2}}{Revision of t3-Paper form board}
    \item{\code{t26_flags}}{Flags-possible substitute for t4 lozenges}
  }
}
\details{The following commentary was provided by Keith Widaman:

``The Holzinger and Swineford (1939) data have been used as a model data set by many investigators. For example, Harman (1976) used the ``24 Psychological Variables" example prominently in his authoritative text on multiple factor analysis, and the data presented under this rubric consisted of 24 of the variables from the Grant-White school (N = 145). Meredith (1964a, 1964b) used several variables from the Holzinger and Swineford study in his work on factorial invariance under selection. Joreskog (1971) based his work on multiple-group confirmatory factor analysis using the Holzinger and Swineford data, subsetting the data into four groups.

Rosseel, who developed the `lavaan' package for  R , included 9 of the manifest variables from Holzinger and Swineford (1939) as a ``resident" data set when one downloads the `lavaan' package. Several background variables are included in this ``resident" data set in addition to 9 of the psychological tests (which are named x1 -- x9 in the data set). When analyzing these data, I found the distributions of the variables (means, SDs) did not match the sample statistics from the original article. For example, in the ``resident" data set in `lavaan', scores on all manifest variables ranged between 0 and 10, sample means varied between 3 and 6, and sample SDs varied between 1.0 and 1.5. In the original data set, scores ranges were rather different across tests, with some variables having scores that ranged between 0 and 20, but other manifest variables having scores ranging from 50 to over 300 -- with obvious attendant differences in sample means and SDs.

After a bit of snooping (i.e., data analysis), I discovered that the 9 variables in the ``resident" data set in `lavaan' had been rescored through ratio transformations. The ratio transformations involved dividing the raw score for each person on a given test by a particular constant for that test that transformed scores on the test to have the desired range.

I decided to perform transformations of all 26 variables so that two data sets could be available to interested researchers:"

 holzinger.raw  are the raws scores on all variables from Holzinger & Swineford (1939) 
 
 holzinger.swineford are  rescaled scores on all variables from Holzinger & Swineford.
 
 holzinger.dictionary is a list of the variable names in short and long form.
 
 ... Widaman continues:
 
 ``As several persons have noted, Harman (1976) used data only from the Grant-White school (N = 145) for his 24 Psychological Variables data set. In doing so, Harman replaced t03_frmbord and t04_lozenges with t25_frmbord2 and t26_flags, because the latter two tests were experimental tests that were designed to be more appropriate for this age level. This substitution is fine, as long as one analyzes data from only the Grant- White school. If one wishes to perform multiple-group analyses and uses school as a grouping variable (as Meredith, 1964a, 1964b, and Joreskog, 1971, did), then tests 25 and 26 should not be used."
 
 ``As have others, Gorsuch (1983) mentioned that analyses based on the raw data reported by Holzinger and Swineford (1939) will not produce statistics (means, SDs, correlations) that match precisely the values reported by Holzinger and Swineford or Harman (1976). Following Gorsuch, I have assumed that the raw data are correct. Applying factor analytic techniques to the raw data from the Grant-White school and to the summary data reported by Harman (1976) will produce slightly different results, but results that differ in only minor, unimportant details."
 
 These data are interesting not just for the historical completeness of having the orinal data, but also as an example of suppressor variables.  Age and grade are positively correlated, and scores are higher in the 8th grade than in the 7th grade. But age (particularly in months) is negatively correlated with many of the cognitive tasks, and when grade and age are both entered into regression, this negative correlation is enhanced.  That is, although increasing grade increases cognitive perforamnce, younger children in both grades do better than the older children.  
 
}
\source{
Keith Widaman (2019, personal communication).  Original data from Holzinger and Swineford (1939). 
}
\references{

Gorsuch, R. L. (1983). Factor analysis (2nd ed.). Hillsdale, NJ: Erlbaum.

Harman, Harry Horace (1967), Modern factor analysis. Chicago, University of Chicago Press.

Holzinger, K. J., & Swineford, F. (1939). A study in factor analysis: The stability of a bi-factor solution. Supplementary Educational Monographs, no. 48. Chicago: University of
Chicago, Department of Education.

Joreskog, K. G. (1971). Simultaneous factor analysis in several populations. Psychometrika, 36, 409-426.

Meredith, W. (1964a). Notes on factorial invariance. Psychometrika, 29, 177-185.

Meredith, W. (1964b). Rotation to achieve factorial invariance. Psychometrika, 29, 177-206.

Meredith, W. (1977). On weighted Procrustes and hyperplane fitting in factor analytic rotation. Psychometrika, 42, 491-522.

Thompson, Bruce. Five Methodology Errors in Educational  Research:The Pantheon of Statistical Significance and Other Faux Pas. Paper presented at the Annual Meeting of the American Educational Research Association(San Diego, CA, April 13-17,1998)
}

\note{As discussed by Widaman, the descriptive values reported in Harman (1967) (p 124) do not quite match the descriptive statistics in \code{\link{holzinger.raw}}.  Further note that the correlation matrix and factor loadings are trivially different from the Harman.24 factor loadings in the GPA rotation package. 

The purpose behind presenting both the raw and transformed data is to show that the fit statistics from factor analysis are identical for these two data sets.  }
\examples{
data(holzinger.raw)
psych::describe(holzinger.raw)
data(holzinger.dictionary)
holzinger.dictionary  #to see the longer names for these data (taken from Thompson)

#show suppression effects
psych::setCor(t01_visperc + t05_geninfo + t08_wordclas ~ grade + agemo,data = holzinger.raw)
psych::mediate(t01_visperc  ~ grade + (agemo),data = holzinger.raw,std=TRUE)
}
\keyword{datasets}
