##Principal Component Analysis
install.packages("stats")
install.packages("dplyr")

##Loading Required Packages
library(stats)
library(dplyr)

View(iris)
mydata=select(iris,c(1:4))
mydata=iris[,-5]
View(mydata)

##Checking PCA Eligibility
cor(mydata)
mean(cor(mydata))

#Principal Component Analysis
PCA=princomp(mydata)

##Evaluating PCA
##There are two way to evaluate PCA
#1. Check whether PCs capture the essence of the original variable.
#2. Check whether the PCs are independent.

##PC Loadings
PCA$loadings

PC=PCA$scores
View(PC)
cor(PC)

###Exploratory Factor Analysis
install.packages("psych")
library(psych)

mydata<-bfi
View(mydata)
mydata<-mydata[complete.cases(mydata),]
KMO(mydata)
#mydata<-mydata[,KMO(mydata)& MSA>0.5]
cortest.bartlett(mydata)

##Factor Analysis
factanal(mydata,3,rotation="none")
fit<-factanal(mydata,3,rotation="varimax")
fit
fit1<-fa(mydata,nfactors=3,rotate="varimax",fm="pa",residuals=T)
fit1
print(fit1$loadings, cutoff=0.3,short=T)

##Determining the number of factors
install.packages("nFactors")
library(nFactors)
ev<-eigen(cor(mydata))
ev
ap<-parallel(subject=nrow(mydata), var=ncol(mydata),
             rep=100,cent=0.05)
nS<-nScree(x=ev$values, aparellel=ap$eigen$qevpea)
plotnScree(nS)

fit2<-fa(mydata,nfactors=7,rotate="varimax",
         fm="pa",residuals=T)
print(fit2$loadings, cutoff=0.3,short=T)

##Factor Map of the Variables
install.packages("FactomineR")
install.packages('Factoshiny')
library(Factoshiny)
library(FactoMineR)
fa.diagram(fit2$loadings)
fa.diagram(fit1$loadings)
