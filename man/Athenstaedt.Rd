\name{Athenstaedt}
\alias{Athenstaedt}
\docType{data}
\alias{Athenstaedt.dictionary}
\alias{Athenstaedt.keys}
\title{Gender Role Self Concept data from Athenstaedt (2003)}

\description{
Athenstaedt (2003) examined Gender Role Self-Concept. She reports two independent dimensions of Male and Female behaviors.  While there are large gender/sex differences on both of these dimensions, the two represent independent factorsl  Eagly and Revelle (2022) have used these data to explore the power of aggregation when examining sex differences.  This data set is also useful to show various graphical display procedures.
}
\usage{data("Athenstaedt")}
\format{
  A data frame with 576 observations on the following 117 variables.
  \describe{
    \item{\code{STUDIE}}{a numeric vector}
    \item{\code{gender}}{Male =1, Female= 2}
    \item{V1 - V74}{self report items (see Athenstaedt.dictionary)}
          \item{V1}{Gender   (Male = 1,  Female =2)}
         \item{V2}{To pay attention to ones appearance in the office}
                     \item{V3}{Offer fire to somebody}
                     \item{V4}{Paint an Apartment}
                       \item{V5}{Mow the Lawn}
                       \item{V6}{Make the Bed}
            \item{V7}{Hold the Door Open for your Partner}
                      \item{V8}{Do the Dishes}
                      \item{V9}{Do Extreme Sports}
                   \item{V10}{Tinker with the Car}
                     \item{V11}{Talk about Sports}
              \item{V12}{Assemble Prefabricated Furniture}
                \item{V13}{Drive a Car in a Risky Way}
                  \item{V14}{Listen Attentively to Others}
          \item{V15}{Tell your Partner about Problems at Work}
                    \item{V16}{Play on a Computer}
                     \item{V17}{Set the Table}
                     \item{V18}{Watch ones Weight}
               \item{V19}{Care for a Partner if he/she is Ill}
                        \item{V20}{Play Chess}
             \item{V21}{Meet with friends at a Regulars Table}
                     \item{V22}{Watch Soap Operas}
                    \item{V23}{Take a Friends Arm}
                 \item{V24}{Wrap Presents Beautifully}
 \item{V25}{In case of Vacation with Partner Packing the Luggage for Both}
            \item{V26}{To admit own Occupational Weekness}
                     \item{V27}{Work Overtime}
                 \item{V28}{Openly Show Vulnerability}
                           \item{V29}{Babysit}
                      \item{V30}{Change Fuses}
                     \item{V31}{Clean a Drain}
                     \item{V32}{Take Care of Somebody}
                        \item{V33}{Do Repair Work}
                    \item{V34}{Change Light Bulbs}
                      \item{V35}{Wash the Car}
                     \item{V36}{Ride a Motorcycle}
                    \item{V37}{Cook Meat on the Grill}
                     \item{V38}{Thump Carpets}
                    \item{V39}{Dust the Furniture}
                   \item{V40}{Buy Electric Appliances}
                        \item{V41}{Go Dancing}
                \item{V42}{Go for a Walk through Town}
                      \item{V43}{Go to the Ballet}
                      \item{V44}{Hug a Friend}
                  \item{V45}{Do Handiwork (e.g. Knitting)}
                     \item{V46}{Change Bed Sheets}
                       \item{V47}{Sew on a Button}
                       \item{V48}{Do Aerobics}
                \item{V49}{Watch Sports on Television}
                   \item{V50}{Talk about Problems}
                     \item{V51}{Play Parlor Games}
                   \item{V52}{Talk about Politics}
                  \item{V53}{Take Care of Flowers}
                 \item{V54}{Make Coffee in the Office}
                       \item{V55}{Shovel Snow}
                    \item{V56}{Read non-Fiction Books}
                  \item{V57}{Organize Company Parties}
                  \item{V58}{Do Home Improvement Jobs}
              \item{V59}{Plead for the Socially Disadvantaged}
                 \item{V60}{Buy a Present for a Colleague}
          \item{V61}{To Talk with Colleagues about Family Matters}
                          \item{V62}{Make Jam}
            \item{V63}{Frquently Ask Colleagues Questions}
              \item{V64}{Decorate the Office with Flowers}
                   \item{V65}{Pick up the Dinner Bill}
                   \item{V66}{Shop for the Family}
              \item{V67}{Have Problem using Technical Devices}
                 \item{V68}{Care for Family Besides a Job}
                   \item{V69}{Watch Action Movies}
                          \item{V70}{Cook}
          \item{V71}{Help your Partner Put on His or Her Coat}
                      \item{V72}{Wash Windows}
                        \item{V73}{Do the Ironing}
                        \item{V74}{Do the Laundry}
                        \item{V75}{Put on Make-up}
                      \item{V76}{Femininity Scale}
                     \item{V77}{Masculinity Scale}
                      \item{V78}{Femininity Scale}
                     \item{V79}{Masculinity Scale}
                      \item{V80}{Pooled Scale}
    \item{MMINUS1 - MPLUS}{see the original Athenstaedt paper}

    \item{\code{FBEHAV}}{a numeric vector}
    \item{\code{MBEHAV}}{a numeric vector}
     \item{\code{Femininity}}{a numeric vector}
    \item{\code{Masculinity}}{a numeric vector}
     \item{\code{MF}}{a numeric vector}
    
  }
}
\details{Ursala Athenstaedt (2003) reported several analyses of items and scales measuring  Gender Role Self-Concept.  Eagly and Revelle (2022) have used these data in an analysis of the power of aggregation.  Here are the original items as well as the three scales Eagly and Revelle (2022).  The accompanying Athenstaedt.dictionary may be used to see the items.

See the \code{\link{GERAS}} data set for a related example.
}
\source{Ursala Athenstaedt, personal communication, 2022, provided a SPSS sav file with the original data from which the complete cases in this set were selected.
}
\references{
Ursula Athenstaedt (2003) On the Content and Structure of the Gender Role Self-Concept: Including Gender-Stereotypical Behaviors in Addition to Traits.  Psychology of Women Quarterly, 27, 309-318. doi: 10.1111/1471-6402.00111.

Alice Eagly and William Revelle (2022) Understanding the Magnitude of Psychological Differences Between Women and Men Requires Seeing the Forest and the Trees. Perspectives in Psychological Science doi:10.1177/17456916211046006.
}
\examples{
data(Athenstaedt)
psych::scatterHist(Femininity ~ Masculinity + gender, data =Athenstaedt,
cex.point=.4,smooth=FALSE, correl=FALSE,d.arrow=TRUE,col=c("red","blue"),
   lwd=4,  cex.main=1.5,main="Scatter Plot and Density",cex.axis=2)
   
psych::cohen.d(Athenstaedt[2:76], group="gender", dictionary=Athenstaedt.dictionary)
#show the top 5 items for each scale
select <- c(psych::selectFromKeys(Athenstaedt.keys$MF10),"gender")
psych::corPlot(Athenstaedt[,select], main="F and M items from Athenstaedt")

}
\keyword{datasets}
