library (ISLR) 
summary(Smarket)
##Look at correlations removing the 9th variables which is qualitative.
cor(Smarket [,-9])
attach(Smarket)
plot(Volume)
##Logistic Regression using the glm package
##Create a 1/0 response variable from a qualitative variable
contrasts(Direction)
glm.fit=glm(Direction ~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,family=binomial )
summary (glm.fit)
##Just look at the coefficients 
coef(glm.fit)
## More indepth look at the coef with std err and p. 
summary(glm.fit)$coef
## Now just the p values for the coef
summary(glm.fit)$coef[,4]

## Calulate the probabilities
glm.probs=predict(glm.fit,type="response") 

##look at first 10
glm.probs [1:10] 

##Now classify into predicted class
## Set to Down (0) then Up (1) if prob > .5
glm.pred=rep("Down",1250)
glm.pred[glm.probs >.5]="Up"

##Confusion table
## Diagonal is correctly classified
table(glm.pred,Direction ) 
##Proportion correctly classified
mean(glm.pred==Direction ) 

## Train and test 
## Hold out 2005, use early years to train
train=(Year <2005) 
## This becomes the test data
Smarket.2005= Smarket [!train ,] 
##Just take a look at the number of variables and obs
dim(Smarket.2005) 
##Create a response variable for the test file
Direction.2005= Direction [!train] 
##Run on Train data
glm.fit=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
data=Smarket,family=binomial,subset=train) 
##Predict on Test data
glm.probs=predict(glm.fit ,Smarket.2005, type="response")
##Now classify into predicted class
## Set to Down (0) then Up (1) if prob > .5
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
#Confusion table
## Diagonal is correctly classified
table(glm.pred,Direction.2005)
##Proportion correctly classified
mean(glm.pred==Direction.2005)
##Compare to train
mean(glm.pred!=Direction.2005) 
