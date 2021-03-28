### R code from vignette source 'mediation.rnw'

###################################################
### code chunk number 1: library
###################################################
library(psych)
library(psychTools)


###################################################
### code chunk number 2: attitude
###################################################
psych::describe(attitude)


###################################################
### code chunk number 3: attitude
###################################################
 #do not standardize
mod1 <- setCor(rating ~ complaints + privileges, data=attitude,std=FALSE)
mod1


###################################################
### code chunk number 4: attitudelm
###################################################
summary(lm(rating ~ complaints + privileges, data=attitude))


###################################################
### code chunk number 5: attitude
###################################################
png('attitude.png')
# standardize by default
mod2 <- setCor(rating ~ complaints + privileges, data=attitude) 
mod2
setCor.diagram(mod2, main="A simple regression model")
dev.off()


###################################################
### code chunk number 6: attitudeR
###################################################
R <- lowerCor(attitude) 

 setCor(rating ~ complaints + privileges, data=R, n.obs =30)


###################################################
### code chunk number 7: kelley
###################################################

#the second Kelley data from Hotelling
kelley <- structure(list(speed = c(1, 0.4248, 0.042, 0.0215, 0.0573), power = c(0.4248, 
1, 0.1487, 0.2489, 0.2843), words = c(0.042, 0.1487, 1, 0.6693, 
0.4662), symbols = c(0.0215, 0.2489, 0.6693, 1, 0.6915), meaningless = c(0.0573, 
0.2843, 0.4662, 0.6915, 1)), .Names = c("speed", "power", "words", 
"symbols", "meaningless"), class = "data.frame", row.names = c("speed", 
"power", "words", "symbols", "meaningless"))

#first show the correlations
lowerMat(kelley)

#now find and draw the regression
sc <- setCor(power + speed ~ words + symbols + meaningless,data=kelley)  #formula mode
sc  #show it


###################################################
### code chunk number 8: kelly
###################################################
png('hotelling.png')
setCor.diagram(sc, main="The  Kelley data set")
dev.off()


###################################################
### code chunk number 9: satact
###################################################
png('satact.png')
mod3 <- setCor(SATV + SATQ + ACT ~ gender + education + age, data = sat.act)
dev.off()


###################################################
### code chunk number 10: moderation
###################################################
mod <- setCor(SATQ ~ SATV*gender + ACT, data=sat.act, std =FALSE, plot=FALSE)
mod
mod0 <- setCor(SATQ ~ SATV*gender + ACT, data=sat.act, zero=FALSE, std=FALSE,plot=FALSE)
mod0


###################################################
### code chunk number 11: modplot
###################################################
png('moderation.png')
setCor.diagram(mod, main="not zero centered")
dev.off()



###################################################
### code chunk number 12: modplot
###################################################
png('moderation0.png')
setCor.diagram(mod0, main="zero centered")
dev.off()



###################################################
### code chunk number 13: plotting
###################################################
both <- cbind(mod$data[-1],mod0$data[-1])
png('splom.png')
pairs.panels(both[,-c(4,9)],gap=0)  #show the mean centered data
dev.off()


###################################################
### code chunk number 14: setcorvslm
###################################################

lm(SATQ ~ SATV*gender + ACT, data=sat.act)
mod <- setCor(SATQ ~ SATV*gender + ACT, data=(sat.act), zero=FALSE, std=FALSE,use="complete")
print(mod,digits=5)


###################################################
### code chunk number 15: Tak_Or
###################################################
data(Tal.Or) 
psych::describe(Tal_Or)  #descriptive statistics

mod4.4 <- mediate(reaction ~ cond + (pmi), data =Tal_Or)
mod4.4
#print(mod4.4, digits = 4) # in order to get the precision of the Hayes (2013) p 99 example


###################################################
### code chunk number 16: ned99
###################################################
png('mediate99.png')
mediate.diagram(mod4.4)
dev.off()


###################################################
### code chunk number 17: garcia
###################################################
data(GSBE)  #alias to Garcia data set
#compare two models  (bootstrapping n.iter set to 50 for speed
# 1) mean center the variables prior to taking product terms
mod1 <- mediate(respappr ~ prot2 * sexism +(sexism),data=Garcia,n.iter=50
 ,main="Moderated mediation (mean centered)")
# 2) do not mean center
mod2 <- mediate(respappr ~ prot2 * sexism +(sexism),data=Garcia,zero=FALSE, n.iter=50,   
    main="Moderated  mediation (not centered")
summary(mod1)
summary(mod2)


###################################################
### code chunk number 18: Tal.or
###################################################

mod5.4 <- mediate(reaction ~ cond  + (import) + (pmi), data = Tal_Or)
print(mod5.4, digits=4)  #to compare with Hayes



###################################################
### code chunk number 19: ned131
###################################################
png('mediate131.png')
mediate.diagram(mod5.4, digits=3, main="Hayes example 5.3")
dev.off()


###################################################
### code chunk number 20: ned131
###################################################
png('mediate131.png')
mediate.diagram(mod5.4, digits=3, main="Hayes example 5.3")
dev.off()


###################################################
### code chunk number 21: Tal.or54
###################################################



#model 5.4 + mod5.7 is the two chained mediator model
mod5.7 <- mediate(pmi ~ cond + (import) , data = Tal_Or)
summary(mod5.7, digits=4)


###################################################
### code chunk number 22: Pollack
###################################################
lowerMat(Pollack)
mod6.2 <- mediate(withdrawal ~ economic.stress + self.efficacy + sex + tenure + (depression), 
      data=Pollack, n.obs=262)
summary(mod6.2) 


###################################################
### code chunk number 23: Pollackgraph
###################################################
png('mediate177.png')
mediate.diagram(mod6.2, digits=3, main = "Simple mediation, 3 covariates")
dev.off()


###################################################
### code chunk number 24: Pollack
###################################################
mod6.2a <- mediate(withdrawal ~ economic.stress -self.efficacy - sex - tenure + (depression),
      data=Pollack, n.obs=262)
summary(mod6.2a) 


###################################################
### code chunk number 25: Pollackgraph
###################################################
png('mod62partial.png')
mediate.diagram(mod6.2a, digits=3, main = "Simple mediation, 3 covariates (partialled out)")
dev.off()


###################################################
### code chunk number 26: interacions
###################################################
summary(lm(respappr ~ prot2 * sexism,data = Garcia))  #show the lm results for comparison
#show the setCor analysis
setCor(respappr ~ prot2* sexism ,data=Garcia,zero=FALSE,main="Moderation",std=FALSE)

#then show the mediate results

modgarcia <-mediate(respappr ~ prot2 * sexism +(sexism),data=Garcia,zero=FALSE,main="Moderated mediation")
summary(modgarcia)



###################################################
### code chunk number 27: interacionsplot
###################################################
png('moderatedmediation.png')
mediate.diagram(modgarcia, main= "An example of moderated mediation")
dev.off()


###################################################
### code chunk number 28: zeri
###################################################

lm(govact ~ age * negemot + posemot + ideology + sex, data=globalWarm)
# but  zero center and try again
glbwarmc <-data.frame(scale(globalWarm,scale=FALSE))
lm(govact ~ age * negemot + posemot + ideology + sex, data=glbwarmc)
mod.glb <- setCor(govact ~ age * negemot + posemot + ideology + sex, data=globalWarm,zero=FALSE,std=FALSE)
print(mod.glb,digits=6)
mod.glb0 <- setCor(govact ~ age * negemot + posemot + ideology + sex, data=globalWarm,std=FALSE)
print(mod.glb0,digits=6)


###################################################
### code chunk number 29: izero2
###################################################
#by default, mediate zero centers before finding the products
 mod.glb <- mediate(govact ~ age * negemot + posemot + ideology + sex + (age), data=globalWarm,zero=TRUE)
summary(mod.glb,digits=4)


###################################################
### code chunk number 30: garcia2t
###################################################

psych::describe(Garcia)
lm(liking ~ prot2* sexism + respappr, data=Garcia)
setCor(liking ~ prot2* sexism + respappr, data = Garcia, zero=FALSE,std=FALSE)
mod7.4 <- mediate(liking ~ prot2 * sexism +respappr, data = Garcia, zero=FALSE)
summary(mod7.4)


###################################################
### code chunk number 31: modertionplot
###################################################
png('moderation.png')
mediate.diagram(mod7.4, main= "Another example of moderated mediation")
dev.off()


###################################################
### code chunk number 32: modertionplot
###################################################
png('garciainteraction.png')
plot(respappr ~ sexism, pch = 23- protest, bg = c("black","red", "blue")[protest], 
data=Garcia, main = "Response to sexism varies as type of protest")
by(Garcia,Garcia$protest, function(x) abline(lm(respappr ~ sexism,
   data =x),lty=c("solid","dashed","dotted")[x$protest+1])) 
text(6.5,3.5,"No protest")
text(3,3.9,"Individual")
text(3,5.2,"Collective")
dev.off()



###################################################
### code chunk number 33: oartial
###################################################

 #first,  the more complicated model
 mod.glb <- setCor(govact ~ age * negemot + posemot + ideology + sex, 
                   data=globalWarm,std=FALSE)
print(mod.glb,digits=3)

# compare this to the partialled model

mod.glb.partialled <- setCor(govact ~ age * negemot - posemot - ideology - sex,data = globalWarm)
 


###################################################
### code chunk number 34: partial.all
###################################################

upper <-partial.r(globalWarm) 
lowerMat(upper)  #show it
lower <- lowerCor(globalWarm)
lowup <- lowerUpper(lower,upper)



###################################################
### code chunk number 35: partial.plot
###################################################
png('partials.png')
psych::corPlot(lowup,numbers = TRUE)
dev.off()


###################################################
### code chunk number 36: sessionInfo
###################################################

sessionInfo()


