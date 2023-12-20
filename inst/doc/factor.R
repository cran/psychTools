### R code from vignette source 'factor.Rnw'

###################################################
### code chunk number 1: factor.Rnw:384-389
###################################################
options(width=160)
library(psych)
library(psychTools)
data(sat.act) 
describe(sat.act)  #basic descriptive statistics


###################################################
### code chunk number 2: pairspanels
###################################################
png( 'pairspanels.png' )
pairs.panels(sat.act,pch='.')
dev.off()


###################################################
### code chunk number 3: factor.Rnw:564-565
###################################################
lowerCor(sat.act)


###################################################
### code chunk number 4: factor.Rnw:572-578
###################################################
female <- subset(sat.act,sat.act$gender==2)
 male <- subset(sat.act,sat.act$gender==1)
lower <- lowerCor(male[-1])
upper <- lowerCor(female[-1])
both <- lowerUpper(lower,upper)
round(both,2)


###################################################
### code chunk number 5: factor.Rnw:584-586
###################################################
diffs <-  lowerUpper(lower,upper,diff=TRUE)
round(diffs,2)


###################################################
### code chunk number 6: corplot.png
###################################################
png('corplot.png')
cor.plot(Thurstone,numbers=TRUE,main="9 cognitive variables from Thurstone")
dev.off()


###################################################
### code chunk number 7: circplot.png
###################################################
png('circplot.png')
circ <- sim.circ(24)
r.circ <- cor(circ)
cor.plot(r.circ,main='24 variables in a circumplex')
dev.off()


###################################################
### code chunk number 8: factor.Rnw:808-810
###################################################
f3t <- fa(Thurstone,3,n.obs=213)
f3t 


###################################################
### code chunk number 9: factor.Rnw:830-833
###################################################
f3 <- fa(Thurstone,3,n.obs = 213,fm="pa")
f3o <- target.rot(f3)
f3o


###################################################
### code chunk number 10: factor.Rnw:854-856
###################################################
f3w <- fa(Thurstone,3,n.obs = 213,fm="wls")
print(f3w,cut=0,digits=3)


###################################################
### code chunk number 11: factor.Rnw:868-869
###################################################
plot(f3t)


###################################################
### code chunk number 12: factor.Rnw:881-882
###################################################
fa.diagram(f3t)


###################################################
### code chunk number 13: factor.Rnw:901-903
###################################################
p3p <-principal(Thurstone,3,n.obs = 213,rotate="Promax")
p3p


###################################################
### code chunk number 14: factor.Rnw:922-924
###################################################
om.h <- omega(Thurstone,n.obs=213,sl=FALSE)
op <- par(mfrow=c(1,1))


###################################################
### code chunk number 15: factor.Rnw:935-936
###################################################
om <- omega(Thurstone,n.obs=213)


###################################################
### code chunk number 16: factor.Rnw:969-971
###################################################
data(bfi)
ic <- iclust(bfi[1:25])


###################################################
### code chunk number 17: factor.Rnw:983-984
###################################################
summary(ic)  #show the results


###################################################
### code chunk number 18: factor.Rnw:997-999
###################################################
data(bfi)
r.poly <- polychoric(bfi[1:25]) #the ... indicate the progress of the function


###################################################
### code chunk number 19: factor.Rnw:1012-1014
###################################################
ic.poly <- iclust(r.poly$rho,title="ICLUST using polychoric correlations")
iclust.diagram(ic.poly) 


###################################################
### code chunk number 20: factor.Rnw:1025-1027
###################################################
ic.poly <- iclust(r.poly$rho,5,title="ICLUST using polychoric correlations for nclusters=5")
iclust.diagram(ic.poly) 


###################################################
### code chunk number 21: factor.Rnw:1038-1039
###################################################
ic.poly <- iclust(r.poly$rho,beta.size=3,title="ICLUST beta.size=3")


###################################################
### code chunk number 22: factor.Rnw:1051-1052
###################################################
print(ic,cut=.3)


###################################################
### code chunk number 23: factor.Rnw:1069-1071
###################################################
fa(bfi[1:10],2,n.iter=20)



###################################################
### code chunk number 24: factor.Rnw:1084-1086
###################################################
f4 <- fa(bfi[1:25],4,fm="pa")
factor.congruence(f4,ic)


###################################################
### code chunk number 25: factor.Rnw:1095-1096
###################################################
factor.congruence(list(f3t,f3o,om,p3p))


###################################################
### code chunk number 26: factor.Rnw:1111-1113
###################################################
 faCor(Thurstone,c(3,3),fm=c("minres","pca"), rotate=c("oblimin","oblimin"))



###################################################
### code chunk number 27: factor.Rnw:1159-1160
###################################################
vss <- vss(bfi[1:25],title="Very Simple Structure of a Big 5 inventory")


###################################################
### code chunk number 28: factor.Rnw:1168-1169
###################################################
vss


###################################################
### code chunk number 29: factor.Rnw:1179-1180
###################################################
fa.parallel(bfi[1:25],main="Parallel Analysis of a Big 5 inventory")


###################################################
### code chunk number 30: factor.Rnw:1198-1203
###################################################
v16 <- sim.item(16)
s <- c(1,3,5,7,9,11,13,15)
f2 <- fa(v16[,s],2)
fe <- fa.extension(cor(v16)[s,-s],f2)
fa.diagram(f2,fe=fe)


###################################################
### code chunk number 31: factor.Rnw:1216-1218
###################################################
fe <- fa.extend(bfi,5,ov=1:25,ev=26:28)
extension.diagram(fe)   


###################################################
### code chunk number 32: factor.Rnw:1236-1238
###################################################
ba5 <- bassAckward(bfi[1:25], nfactors =c(2,3,4,5),plot=FALSE)
baf <- bassAckward.diagram(ba5)


###################################################
### code chunk number 33: factor.Rnw:1252-1254
###################################################
# fa.lookup(baf$bass.ack[[5]],dictionary=bfi.dictionary[2])



###################################################
### code chunk number 34: factor.Rnw:1307-1311
###################################################
set.seed(17)
r9 <- sim.hierarchical(n=500,raw=TRUE)$observed
round(cor(r9),2)
alpha(r9, discrete=FALSE)


###################################################
### code chunk number 35: factor.Rnw:1319-1321
###################################################
keys <- c(1,-1,1,1,1,1,1)
alpha(attitude,keys, discrete=FALSE)


###################################################
### code chunk number 36: factor.Rnw:1328-1330
###################################################
keys <- c(1,1,1,1,1,1,1)
alpha(attitude,keys, discrete=FALSE)


###################################################
### code chunk number 37: factor.Rnw:1337-1339
###################################################
items <- sim.congeneric(N=500,short=FALSE,low=-2,high=2,categorical=TRUE) #500 responses to 4 discrete items
alpha(items$observed)  #item response analysis of congeneric measures


###################################################
### code chunk number 38: factor.Rnw:1392-1393
###################################################
om.9 <- omega(r9,title="9 simulated variables")


###################################################
### code chunk number 39: factor.Rnw:1404-1405
###################################################
om.9


###################################################
### code chunk number 40: factor.Rnw:1413-1414
###################################################
omegaSem(r9,n.obs=500)


###################################################
### code chunk number 41: factor.Rnw:1423-1424
###################################################
splitHalf(r9)


###################################################
### code chunk number 42: factor.Rnw:1446-1451
###################################################
 keys <- make.keys(nvars=28,list(Agree=c(-1,2:5),Conscientious=c(6:8,-9,-10),
 Extraversion=c(-11,-12,13:15),Neuroticism=c(16:20),
 Openness = c(21,-22,23,24,-25)),
 item.labels=colnames(bfi))
 keys


###################################################
### code chunk number 43: factor.Rnw:1458-1462
###################################################
 keys.1<- make.keys(10,list(Agree=c(-1,2:5),Conscientious=c(6:8,-9,-10)))
keys.2 <- make.keys(15,list(Extraversion=c(-1,-2,3:5),Neuroticism=c(6:10),
 Openness = c(11,-12,13,14,-15)))
 keys.25 <- superMatrix(list(keys.1,keys.2))


###################################################
### code chunk number 44: factor.Rnw:1472-1474
###################################################
 scores <- scoreItems(keys,bfi)
 scores


###################################################
### code chunk number 45: scores
###################################################
png('scores.png')
pairs.panels(scores$scores,pch='.',jiggle=TRUE)
dev.off()


###################################################
### code chunk number 46: factor.Rnw:1500-1503
###################################################
r.bfi <- cor(bfi,use="pairwise")
scales <- cluster.cor(keys,r.bfi)
summary(scales)


###################################################
### code chunk number 47: factor.Rnw:1513-1519
###################################################
data(iqitems)
iq.keys <- c(4,4,4, 6,6,3,4,4,  5,2,2,4,  3,2,6,7)
score.multiple.choice(iq.keys,iqitems)
#just convert the items to true or false 
iq.tf <- score.multiple.choice(iq.keys,iqitems,score=FALSE)
describe(iq.tf)  #compare to previous results


###################################################
### code chunk number 48: factor.Rnw:1537-1543
###################################################
data(iqitems)
iq.keys <- c(4,4,4, 6,6,3,4,4,  5,2,2,4,  3,2,6,7)
scores <- score.multiple.choice(iq.keys,iqitems,score=TRUE,short=FALSE)
#note that for speed we can just do this on simple item counts rather than IRT based scores.
op <- par(mfrow=c(2,2))  #set this to see the output for multiple items
irt.responses(scores$scores,iqitems[1:4],breaks=11)


###################################################
### code chunk number 49: factor.Rnw:1569-1573
###################################################
set.seed(17)
d9 <- sim.irt(9,1000,-2.,2.,mod="normal") #dichotomous items
test <- irt.fa(d9$items)
test 


###################################################
### code chunk number 50: factor.Rnw:1580-1585
###################################################
op <- par(mfrow=c(3,1))
plot(test,type="ICC")
plot(test,type="IIC")
plot(test,type="test")
op <- par(mfrow=c(1,1))


###################################################
### code chunk number 51: factor.Rnw:1596-1599
###################################################
data(bfi)
e.irt <- irt.fa(bfi[11:15])
e.irt 


###################################################
### code chunk number 52: factor.Rnw:1606-1607
###################################################
e.info  <- plot(e.irt,type="IIC")


###################################################
### code chunk number 53: factor.Rnw:1618-1619
###################################################
print(e.info,sort=TRUE)


###################################################
### code chunk number 54: factor.Rnw:1633-1634
###################################################
iq.irt <- irt.fa(iq.tf)


###################################################
### code chunk number 55: factor.Rnw:1644-1645
###################################################
iq.irt 


###################################################
### code chunk number 56: factor.Rnw:1651-1652
###################################################
om <- omega(iq.irt$rho,4)


###################################################
### code chunk number 57: factor.Rnw:1666-1680
###################################################
v9 <- sim.irt(9,1000,-2.,2.,mod="normal") #dichotomous items
items <- v9$items
test <- irt.fa(items)
total <- rowSums(items)
ord <- order(total)
items <- items[ord,]
#now delete some of the data - note that they are ordered by score
items[1:333,5:9] <- NA
items[334:666,3:7] <- NA
items[667:1000,1:4] <- NA
scores <- scoreIrt(test,items)
unitweighted <- scoreIrt(items=items,keys=rep(1,9))
scores.df <- data.frame(true=v9$theta[ord],scores,unitweighted)
colnames(scores.df) <- c("True theta","irt theta","total","fit","rasch","total","fit")


###################################################
### code chunk number 58: factor.Rnw:1689-1691
###################################################
 pairs.panels(scores.df,pch='.',gap=0)
 title('Comparing true theta for IRT, Rasch and  classically based scoring',line=3)


###################################################
### code chunk number 59: factor.Rnw:1740-1744
###################################################

C <- cov(sat.act,use="pairwise")
model1 <- lm(ACT~ gender + education + age, data=sat.act)
summary(model1)


###################################################
### code chunk number 60: factor.Rnw:1747-1749
###################################################
#compare with mat.regress
lmCor(c(4:6),c(1:3),C, n.obs=700)


###################################################
### code chunk number 61: factor.Rnw:1834-1858
###################################################
xlim=c(0,10)
ylim=c(0,10)
plot(NA,xlim=xlim,ylim=ylim,main="Demontration of dia functions",axes=FALSE,xlab="",ylab="")
ul <- dia.rect(1,9,labels="upper left",xlim=xlim,ylim=ylim)
ll <- dia.rect(1,3,labels="lower left",xlim=xlim,ylim=ylim)
lr <- dia.ellipse(9,3,"lower right",xlim=xlim,ylim=ylim)
ur <- dia.ellipse(9,9,"upper right",xlim=xlim,ylim=ylim)
ml <- dia.ellipse(3,6,"middle left",xlim=xlim,ylim=ylim)
mr <- dia.ellipse(7,6,"middle right",xlim=xlim,ylim=ylim)
bl <- dia.ellipse(1,1,"bottom left",xlim=xlim,ylim=ylim)
br <- dia.rect(9,1,"bottom right",xlim=xlim,ylim=ylim)
dia.arrow(from=lr,to=ul,labels="right to left")
dia.arrow(from=ul,to=ur,labels="left to right")
dia.curved.arrow(from=lr,to=ll$right,labels ="right to left")
dia.curved.arrow(to=ur,from=ul$right,labels ="left to right")
dia.curve(ll$top,ul$bottom,"double")  #for rectangles, specify where to point 
dia.curved.arrow(mr,ur,"up")  #but for ellipses, just point to it.
dia.curve(ml,mr,"across")
dia.arrow(ur,lr,"top down")
dia.curved.arrow(br$top,lr$bottom,"up")
dia.curved.arrow(bl,br,"left to right")
dia.arrow(bl,ll$bottom)
dia.curved.arrow(ml,ll$right)
dia.curved.arrow(mr,lr$top)


###################################################
### code chunk number 62: factor.Rnw:1934-1935
###################################################
sessionInfo()


