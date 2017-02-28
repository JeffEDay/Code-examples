
# Author: Jeffrey Day

# Runs a series of bivariate logregs using glm calculates probabilities and 
# Lift and places it in a data frame and writes the results to excel (lift.csv)
# Near the bottom is contingency table code that can be used to QA results

library (ISLR) 
library(dplyr)
library(plyr)


Lift <- function(myglmfit) {
# Calculates the odds ratio and lift from a bivariate logstic regression
# Assumes indendent variable is coded 1/0. 
#
# Args:
# myglmfit: name of the glm output
#
# Returns: dataframe of B,z,odds,odds_ratio, prob if, prob not, lift
d <-myglmfit$terms[1]
d<-substring(d,1,10)
dep<-d[2]
B<-myglmfit$coefficients[2]
z<-myglmfit$coefficients[1]+myglmfit$coefficients[2]
odds<-exp(z)
odds_ratio<-exp(myglmfit$coefficients[2])
Prob_if<-odds/(1+odds)
Prob_not<-exp(myglmfit$coefficients[1])/(1+exp(myglmfit$coefficients[1]))
lift<-Prob_if-Prob_not
result2<- data.frame(B,z,odds,odds_ratio,Prob_if,Prob_not,lift)
return(result2)
}



Runlr<-function(dep,ind,df) {
# Bivariate Logreg models
# Logistic Regression using the glm package 
#
# Args:
# dep: dependent variable
# ind: independentvariable 
# df:  data frame
#
# Returns: glm output
glmfit<-glm(dep~ind, data=df,family=binomial ) 
return(glmfit)
}



# Read in the data
data <- read.csv("file name.csv",header=TRUE, sep=",")

attach(data)
# Create a list if independent variables
ind <- list(var1,var2,var3)
vlist <- c("var1","var2","var3")
names(ind) <- vlist



# Do a series of Bivariate Logistic regressions from list above
# Calculate Prob, But let's call a function to do it for us

result <- lapply(ind,Runlr,dep=var18,df=data)
Lift1 <- lapply(result,Lift)

# Convert to a data frame that can be written out to .csv

final_results <- ldply(Lift1, data.frame)

write.csv(final_results,file="lift.csv")

# As a double check, since this is relatively simple bivariate with 1/0 
# on independent variable
# A simple contingency table with simple math yeilds the same thing. 
# A quality control check can be done by substituting in a table and 
# comparing with Logreg output
# table(A,B)  # A is dependent variable, B is independent
mytable <- table(var18,var1) # A willprop.table(mytable, 1) row % 

dblchk<-prop.table(mytable, 2)  # column percentages 
# subtract the bottom left number from the bottom right number 
# and should equal lift from logreg
dblchk