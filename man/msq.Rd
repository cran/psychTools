\name{msq}
\Rdversion{1.1}
\alias{msq}
\docType{data}
\title{75 mood items from the Motivational State Questionnaire for 3896 participants}
\description{Emotions may be described either as discrete emotions or in dimensional terms.  
The Motivational State Questionnaire (MSQ) was developed to study emotions in laboratory and field settings.  
The data can be well described in terms of a two dimensional solution of energy vs tiredness and tension versus calmness.  
Additional items include what time of day the data were collected and a few personality questionnaire scores.
}
\usage{data(msq)}
\format{
  A data frame with 3896 observations on the following 92 variables.
  \describe{
    \item{\code{active}}{a numeric vector}
    \item{\code{afraid}}{a numeric vector}
    \item{\code{alert}}{a numeric vector}
    \item{\code{angry}}{a numeric vector}
     \item{\code{anxious}}{a numeric vector}
    \item{\code{aroused}}{a numeric vector}
    \item{\code{ashamed}}{a numeric vector}
    \item{\code{astonished}}{a numeric vector}
    \item{\code{at.ease}}{a numeric vector}
    \item{\code{at.rest}}{a numeric vector}
    \item{\code{attentive}}{a numeric vector}
    \item{\code{blue}}{a numeric vector}
    \item{\code{bored}}{a numeric vector}
    \item{\code{calm}}{a numeric vector}
    \item{\code{cheerful}}{a numeric vector}
    \item{\code{clutched.up}}{a numeric vector}
    \item{\code{confident}}{a numeric vector}
    \item{\code{content}}{a numeric vector}
    \item{\code{delighted}}{a numeric vector}
    \item{\code{depressed}}{a numeric vector}
    \item{\code{determined}}{a numeric vector}
    \item{\code{distressed}}{a numeric vector}
    \item{\code{drowsy}}{a numeric vector}
    \item{\code{dull}}{a numeric vector}
    \item{\code{elated}}{a numeric vector}
    \item{\code{energetic}}{a numeric vector}
    \item{\code{enthusiastic}}{a numeric vector}
    \item{\code{excited}}{a numeric vector}
    \item{\code{fearful}}{a numeric vector}
    \item{\code{frustrated}}{a numeric vector}
    \item{\code{full.of.pep}}{a numeric vector}
    \item{\code{gloomy}}{a numeric vector}
    \item{\code{grouchy}}{a numeric vector}
    \item{\code{guilty}}{a numeric vector}
    \item{\code{happy}}{a numeric vector}
    \item{\code{hostile}}{a numeric vector}
     \item{\code{idle}}{a numeric vector}
    \item{\code{inactive}}{a numeric vector}
    \item{\code{inspired}}{a numeric vector}
    \item{\code{intense}}{a numeric vector}
    \item{\code{interested}}{a numeric vector}
    \item{\code{irritable}}{a numeric vector}
    \item{\code{jittery}}{a numeric vector}    
    \item{\code{lively}}{a numeric vector}
    \item{\code{lonely}}{a numeric vector}
    \item{\code{nervous}}{a numeric vector}
    \item{\code{placid}}{a numeric vector}
    \item{\code{pleased}}{a numeric vector}
    \item{\code{proud}}{a numeric vector}
    \item{\code{quiescent}}{a numeric vector}
    \item{\code{quiet}}{a numeric vector}
    \item{\code{relaxed}}{a numeric vector}
    \item{\code{sad}}{a numeric vector}
    \item{\code{satisfied}}{a numeric vector}
    \item{\code{scared}}{a numeric vector}  
    \item{\code{serene}}{a numeric vector}
    \item{\code{sleepy}}{a numeric vector}
    \item{\code{sluggish}}{a numeric vector}
    \item{\code{sociable}}{a numeric vector}
    \item{\code{sorry}}{a numeric vector}
    \item{\code{still}}{a numeric vector}
    \item{\code{strong}}{a numeric vector}
    \item{\code{surprised}}{a numeric vector}
    \item{\code{tense}}{a numeric vector}
    \item{\code{tired}}{a numeric vector}
    \item{\code{tranquil}}{a numeric vector}
    \item{\code{unhappy}}{a numeric vector}
    \item{\code{upset}}{a numeric vector}
    \item{\code{vigorous}}{a numeric vector}
    \item{\code{wakeful}}{a numeric vector}
    \item{\code{warmhearted}}{a numeric vector}
    \item{\code{wide.awake}}{a numeric vector}
     \item{\code{alone}}{a numeric vector}
   \item{\code{kindly}}{a numeric vector}
   \item{\code{scornful}}{a numeric vector}
    \item{\code{EA}}{Thayer's Energetic Arousal Scale}
    \item{\code{TA}}{Thayer's Tense Arousal Scale}
    \item{\code{PA}}{Positive Affect scale}
    \item{\code{NegAff}}{Negative Affect scale}
    \item{\code{Extraversion}}{Extraversion from the Eysenck Personality Inventory}
    \item{\code{Neuroticism}}{Neuroticism from the Eysenck Personality Inventory}
    \item{\code{Lie}}{Lie from the EPI}
    \item{\code{Sociability}}{The sociability subset of the Extraversion Scale}
    \item{\code{Impulsivity}}{The impulsivity subset of the Extraversions Scale}
     \item{\code{MSQ_Time}}{Time of day the data were collected}
    \item{\code{MSQ_Round}}{Rounded time of day}
    \item{\code{TOD}}{a numeric vector}
    \item{\code{TOD24}}{a numeric vector}
    \item{\code{ID}}{subject ID}
     \item{\code{condition}}{What was the experimental condition after the msq was given}
    
     \item{\code{scale}}{a factor with levels \code{msq} \code{r} original or revised msq}
    \item{\code{exper}}{Which study were the data collected: a factor with levels 
\code{AGES} \code{BING} \code{BORN} \code{CART} \code{CITY} \code{COPE} \code{EMIT} \code{FAST} \code{Fern} \code{FILM} \code{FLAT} \code{Gray} \code{imps} \code{item} \code{knob} \code{MAPS} \code{mite} \code{pat-1} \code{pat-2} \code{PATS} \code{post} \code{RAFT} \code{Rim.1} \code{Rim.2} \code{rob-1} \code{rob-2} \code{ROG1} \code{ROG2} \code{SALT} \code{sam-1} \code{sam-2} \code{SAVE/PATS} \code{sett} \code{swam} \code{swam-2} \code{TIME} \code{VALE-1} \code{VALE-2} \code{VIEW}}
   
  }
}
\details{The Motivational States Questionnaire (MSQ) is composed of 72 items, which represent the full affective space (Revelle & Anderson, 1998). The MSQ consists of 20 items taken from the Activation-Deactivation Adjective Check List (Thayer, 1986), 18 from the Positive and Negative Affect Schedule (PANAS, Watson, Clark, & Tellegen, 1988) along with the items used by Larsen and Diener (1992). The response format was a four-point scale that corresponds to Russell and Carroll's (1999) "ambiguous--likely-unipolar format" and that asks the respondents to indicate their current standing (``at this moment") with the following rating scale:\cr
0----------------1----------------2----------------3
\cr
Not at all		A little		Moderately     	Very much \cr

The original version of the MSQ included 70 items. Intermediate analyses (done with 1840 subjects)
 demonstrated a concentration of items in some sections of the two dimensional space, and a paucity of items in others.
  To begin correcting this, 3 items from redundantly measured sections (alone, kindly, scornful) were removed,
   and 5 new ones (anxious, cheerful, idle, inactive, and tranquil) were added.  T
   hus, the correlation matrix is missing the correlations between items anxious, cheerful, idle, inactive, and tranquil with alone, kindly, and scornful. 

Procedure. The data were collected over nine years, as part of a series of studies examining 
the effects of personality and situational factors on motivational state and subsequent cognitive performance.
 In each of 38 studies, prior to any manipulation of motivational state, participants signed a consent form and filled out the MSQ.
  (The procedures of the individual studies are irrelevant to this data set and could not affect the responses to the MSQ, since this instrument was completed before any further instructions or tasks).  Some MSQ post test (after manipulations) is available in \code{\link{affect}}.

The EA and TA scales are from Thayer, the PA and NA scales are from Watson et al. (1988).
Scales and items:

Energetic Arousal: active, energetic, vigorous, wakeful, wide.awake, full.of.pep, lively, -sleepy, -tired, - drowsy  (ADACL)

Tense Arousal: Intense, Jittery, fearful, tense, clutched up, -quiet, -still, - placid, - calm, -at rest  (ADACL)

Positive Affect: active, alert, attentive, determined, enthusiastic, excited, inspired,  interested,  proud, strong  (PANAS)

Negative Affect: afraid, ashamed,   distressed,  guilty,  hostile, irritable , jittery, nervous, scared, upset (PANAS)

The PA and NA scales can in turn can be thought of as having subscales:  (See the PANAS-X)
Fear:  afraid, scared, nervous, jittery    (not included  frightened, shaky)
Hostility: angry, hostile, irritable, (not included:   scornful,  disgusted, loathing 
guilt: ashamed, guilty,   (not included: blameworthy, angry at self, disgusted with self, dissatisfied with self)
sadness: alone,  blue,  lonely, sad,  (not included: downhearted) 
joviality: cheerful, delighted, energetic, enthusiastic, excited,  happy, lively,     (not included:  joyful)
self-assurance: proud, strong, confident,       (not included: bold,  daring, fearless )   
attentiveness:  alert, attentive,  determined  (not included: concentrating)

The next set of circumplex scales were taken (I think) from Larsen and Diener (1992).  
High activation: active, aroused, surprised, intense, astonished
Activated PA: elated, excited, enthusiastic, lively
Unactivated NA : calm, serene, relaxed, at rest, content, at ease
PA: happy, warmhearted, pleased, cheerful, delighted
Low Activation: quiet, inactive, idle, still, tranquil
Unactivated PA: dull, bored, sluggish, tired, drowsy
NA: sad, blue, unhappy, gloomy, grouchy
Activated NA: jittery, anxious, nervous, fearful, distressed.

Keys for these separate scales are shown in the examples.  

In addition to the MSQ, there are 5 scales from the Eysenck Personality Inventory (Extraversion, Impulsivity, Sociability, Neuroticism, Lie).  The Imp and Soc are subsets of the the total extraversion scale. 

}
\source{Data collected at the Personality, Motivation, and Cognition Laboratory, Northwestern University. 
}
\references{

Larsen, R. J., & Diener, E. (1992). Promises and problems with the circumplex model of emotion. In M. S. Clark (Ed.), Review of personality and social psychology, No. 13. Emotion (pp. 25-59). Thousand Oaks, CA, US: Sage Publications, Inc.

Rafaeli, Eshkol and Revelle, William (2006), A premature consensus: Are happiness and sadness truly opposite affects? Motivation and Emotion, 30, 1, 1-12.

Revelle, W. and  Anderson, K.J. (1998) Personality, motivation and cognitive performance: Final report to the Army Research Institute on  contract MDA 903-93-K-0008. (\url{https://www.personality-project.org/revelle/publications/ra.ari.98.pdf}).

Thayer, R.E. (1989)  The biopsychology of mood and arousal.
Oxford University Press. New York, NY. 

Watson,D., Clark,  L.A.  and Tellegen, A. (1988)  Development and validation of brief measures of positive and negative affect: The PANAS scales. Journal of Personality and Social Psychology, 54(6):1063-1070.

}	

\seealso{\code{\link{msqR}} for a larger data set with repeated measures for 3032 participants measured at least once, 2753 measured twice, 446 three times and 181 four times.  \code{\link{affect}} for an example of the use of some of these adjectives in a mood manipulation study.

\code{\link[psych]{make.keys}}, \code{\link[psych]{scoreItems}} and \code{\link[psych]{scoreOverlap}} for instructions on how to score multiple scales with and without item overlap. Also see  
\code{\link[psych]{fa}} and \code{\link[psych]{fa.extension}} for instructions on how to do factor analyses or factor extension.
}

\examples{
data(msq)
\donttest{   #in in the interests of time
#basic descriptive statistics
psych::describe(msq)
}
#score them for 20 short scales -- note that these have item overlap
#The first 2 are from Thayer
#The next 2 are classic positive and negative affect
#The next 9 are circumplex scales
#the last 7 are msq estimates of PANASX scales (missing some items)
keys.list <- list(
EA = c("active", "energetic", "vigorous", "wakeful", "wide.awake", "full.of.pep",
       "lively", "-sleepy", "-tired", "-drowsy"),
TA =c("intense", "jittery", "fearful", "tense", "clutched.up", "-quiet", "-still", 
       "-placid", "-calm", "-at.rest") ,
PA =c("active", "excited", "strong", "inspired", "determined", "attentive", 
          "interested", "enthusiastic", "proud", "alert"),
NAf =c("jittery", "nervous", "scared", "afraid", "guilty", "ashamed", "distressed",  
         "upset", "hostile", "irritable" ),
HAct = c("active", "aroused", "surprised", "intense", "astonished"),
aPA = c("elated", "excited", "enthusiastic", "lively"),
uNA = c("calm", "serene", "relaxed", "at.rest", "content", "at.ease"),
pa = c("happy", "warmhearted", "pleased", "cheerful", "delighted" ),
LAct = c("quiet", "inactive", "idle", "still", "tranquil"),
uPA =c( "dull", "bored", "sluggish", "tired", "drowsy"),
naf = c( "sad", "blue", "unhappy", "gloomy", "grouchy"),
aNA = c("jittery", "anxious", "nervous", "fearful", "distressed"),
Fear = c("afraid" , "scared" , "nervous" , "jittery" ) ,
Hostility = c("angry" ,  "hostile", "irritable", "scornful" ), 
Guilt = c("guilty" , "ashamed" ),
Sadness = c( "sad"  , "blue" , "lonely",  "alone" ),
Joviality =c("happy","delighted", "cheerful", "excited", "enthusiastic", "lively", "energetic"), 
Self.Assurance=c( "proud","strong" , "confident" , "-fearful" ),
Attentiveness = c("alert" , "determined" , "attentive" )
#, acquiscence = c("sleepy" ,  "wakeful" ,  "relaxed","tense")   
#dropped because it has a negative alpha and throws warnings
   )
       
msq.scores <- psych::scoreItems(keys.list,msq)

#show a circumplex structure for the non-overlapping items
fcirc <- psych::fa(msq.scores$scores[,5:12],2)  
psych::fa.plot(fcirc,labels=colnames(msq.scores$scores)[5:12])

\donttest{#now, find the correlations corrected for item overlap
msq.overlap <- psych::scoreOverlap(keys.list,msq)
#a warning is thrown by smc  because of some NAs in the matrix

f2 <- psych::fa(msq.overlap$cor,2)
psych::fa.plot(f2,labels=colnames(msq.overlap$cor),
      title="2 dimensions of affect, corrected for overlap")

#extend this solution to EA/TA  NA/PA space
fe  <- psych::fa.extension(cor(msq.scores$scores[,5:12],msq.scores$scores[,1:4]),fcirc)
psych::fa.diagram(fcirc,fe=fe,
          main="Extending the circumplex structure to  EA/TA and PA/NA ")

#show the 2 dimensional structure
f2 <- psych::fa(msq[1:72],2)
psych::fa.plot(f2,labels=colnames(msq)[1:72],
     title="2 dimensions of affect at the item level",cex=.5)

#sort them by polar coordinates
round(psych::polar(f2),2)
}
            

}
\keyword{datasets}
