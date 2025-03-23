### R code from vignette source 'omega.Rnw'

###################################################
### code chunk number 1: omega.Rnw:162-166
###################################################
library(psych)    #make the psych package active 
library(psychTools) #make psychTools active
om <- omega(Thurstone)  #do the analysis 
om  #show it 


###################################################
### code chunk number 2: Thurstone
###################################################
png('Thurstone.png')
omega.diagram(om)
dev.off()


###################################################
### code chunk number 3: omega
###################################################
omega(Thurstone)


###################################################
### code chunk number 4: anxiety
###################################################

anxiety <- sai[c("anxious", "jittery", "nervous" ,"tense", "upset","at.ease" ,  "calm" , 
   "confident", "content","relaxed")]
describe(anxiety)
lowerCor(anxiety)
om <- omega(anxiety,2)  #specify a two factor solution
summary(om)   #summarize the output


###################################################
### code chunk number 5: anxietyplot
###################################################
png('anxiety.png')
omega.diagram(om, main="Omega analysis of  two factors of anxiety")
dev.off()


###################################################
### code chunk number 6: unidim
###################################################
anxiety.keys <- list(all =cs(anxious, jittery, nervous, tense, upset, 
            at.ease, calm, confident, content, relaxed),
              negative = cs(anxious, jittery, nervous, tense, upset),
              positive = cs(at.ease, calm, confident, content, relaxed))
rel <- reliability(anxiety.keys,anxiety)
rel



###################################################
### code chunk number 7: relplot
###################################################
png('rel.png')
plot(rel)
dev.off()


###################################################
### code chunk number 8: direct
###################################################
om <- omegaDirect(Thurstone)  
om


###################################################
### code chunk number 9: drawdirect
###################################################
png('direct.png')
omega.diagram(om, main="Direct Schmid Leihman solution")
dev.off()


###################################################
### code chunk number 10: omega.Rnw:766-769
###################################################
om <- omega(holzinger.swineford[8:31],4)  #the exploratory solution

omegaSem(holzinger.swineford[8:31],4) #the confirmatory solution


###################################################
### code chunk number 11: holzinger
###################################################



###################################################
### code chunk number 12: omega.Rnw:863-866
###################################################
jen <- sim.hierarchical()  #use the default values
om <- omega(jen)
om


###################################################
### code chunk number 13: jensen
###################################################
png('jensen.png' )
omega.diagram(om)
dev.off()


###################################################
### code chunk number 14: Simulate1
###################################################
fx <- matrix(c(.7,.6,.5,.7,.6,.5,.8,.7,.6, 
    .6,.6,.6,rep(0,9),c(.6,.5,.6),rep(0,9),.6,.6,.6),ncol=4)
 simx <-sim.structure(fx)

om <- omega(simx$model)  #defaults to population model
dsl <- omegaDirect(simx$model) #the direct SL approach


###################################################
### code chunk number 15: Simulate.2
###################################################
 lowerMat(simx$model)
summary(om)
summary(dsl)
fa.congruence(list(om,dsl,fx))  #compare the two solutions


###################################################
### code chunk number 16: omega.Rnw:1052-1053
###################################################
sessionInfo()


