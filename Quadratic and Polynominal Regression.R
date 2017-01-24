library(MASS)
library(ISLR)
library(car)
##Set your working directory
setwd("D:/Lab")
Auto <-read.csv("Auto.csv",header=TRUE,na.strings="?")
Auto=na.omit(Auto)
attach(Auto)

nonlin<- lm(mpg~horsepower+I(horsepower^2))

##Standard Dignostic Plots
par(mfrow=c(2,2)) 
plot(nonlin) 
##Look at the results
summary(nonlin)
anova(nonlin)
coef(nonlin)
confint(nonlin)
## Calculate the prediction
predict(nonlin)
##Look at the residuals
plot(predict (nonlin), residuals (nonlin)) 
plot(predict (nonlin), rstudent (nonlin))

## Leverage info 

plot(hatvalues(nonlin))
## This may be buggy as it suggest obs 116 when obs 117 is probably
## the culprit so be careful
which.max(hatvalues(nonlin))

##Multi-Colinary, (P+1)/n.  
vif(nonlin) 

##Polinominal example
mypoly <- lm(mpg~ poly(horsepower,5))
summary(mypoly)
