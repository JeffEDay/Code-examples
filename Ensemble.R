set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

library(MASS)
##Run a Linear Model
lm_fit<-lm(y~x1+x2+x3)
summary(lm_fit)

## Set up and use a test and training dataset
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]

lm_fit<-lm(y~x1+x2+x3,data=training)
predictions<-predict(lm_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error


## Do some bagging, averaging the results and see what we get
library(foreach)
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
}
predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

##See what Random Forest gives us


library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
predictions<-predict(rf_fit,newdata=testing)
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

##Combine LM and Random Forest (using equal weights)

length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,])
  predict(lm_fit,newdata=testing)
}
lm_predictions<-rowMeans(predictions)

library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
rf_predictions<-predict(rf_fit,newdata=testing)
predictions<-(lm_predictions+rf_predictions)/2
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

##Change the weights given that LM is less accurate.
predictions<-(lm_predictions+rf_predictions*9)/10
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error

## Try Support Vesctor Machine
library(e1071)
svm_fit<-svm(y~x1+x2+x3,data=training)
svm_predictions<-predict(svm_fit,newdata=testing)
error<-sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error
##Try it with Bagging
length_divisor<-6
iterations<-5000
predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor)))
  train_pos<-1:nrow(training) %in% training_positions
  svm_fit<-svm(y~x1+x2+x3,data=training[train_pos,])
  predict(svm_fit,newdata=testing)
}
svm2_predictions<-rowMeans(predictions)
error<-sqrt((sum((testing$y-svm2_predictions)^2))/nrow(testing))
error

##Let's stick with a single SVM
predictions<-(svm_predictions+rf_predictions)/2
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
##Let's combine the svm and random forest
predictions<-(svm_predictions*2+rf_predictions)/3
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
error
