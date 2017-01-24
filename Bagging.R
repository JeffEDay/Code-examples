library(MASS)

##Bagging Example
set.seed(10)  
y<-c(1:1000)  
x1<-c(1:1000)*runif(1000,min=0,max=2)  
x2<-c(1:1000)*runif(1000,min=0,max=2)  
x3<-c(1:1000)*runif(1000,min=0,max=2)  

## Fit a liner model using all the data
lm_fit<-lm(y~x1+x2+x3)  
summary(lm_fit)  

## Create a training dataset with 3/4 of the data and a test 
## data set with the rest

set.seed(10)  
all_data<-data.frame(y,x1,x2,x3)  
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))  
training<- all_data[positions,]  
testing<- all_data[-positions,]  

## Run the regression on the training set and apply to test
## calculate the prediction error by subtracting the actual values 
## from the predicted values 
##  (the error calculation here is root mean squared error)

lm_fit<-lm(y~x1+x2+x3,data=training)  
predictions<-predict(lm_fit,newdata=testing)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error

library(foreach)  
length_divisor<-4  
iterations<-1000  
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {  
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))  
  train_pos<-1:nrow(training) %in% training_positions  
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])  
  predict(lm_fit,newdata=testing)  
}  
predictions<-rowMeans(predictions)  
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))  
error
## The above code randomly samples 1/4 of the training set in 
## each iteration, and generates predictions for the testing set 
##  based on the sample. 
## It will execute the number of time specified by iterations. 

