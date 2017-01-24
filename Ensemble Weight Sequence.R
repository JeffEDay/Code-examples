##Set-up simulated data to use

set.seed(10)
y<-c(1:1000)
x1<-c(1:1000)*runif(1000,min=0,max=2)
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2
x3<-log(c(1:1000)*runif(1000,min=0,max=2))

library(MASS)

## Set up and use a test and training dataset
set.seed(10)
all_data<-data.frame(y,x1,x2,x3)
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3))
training<- all_data[positions,]
testing<- all_data[-positions,]

##Run Linear Model
lm_fit<-lm(y~x1+x2+x3,data=training)
lm_predictions<-predict(lm_fit,newdata=testing)
error_lm<-sqrt((sum((testing$y-lm_predictions)^2))/nrow(testing))
error_lm

## Run a randomforest
library(randomForest)
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500)
rf_predictions<-predict(rf_fit,newdata=testing)
error_rf<-sqrt((sum((testing$y-rf_predictions)^2))/nrow(testing))
error_rf

## Try Support Vesctor Machine
library(e1071)
svm_fit<-svm(y~x1+x2+x3,data=training)
svm_predictions<-predict(svm_fit,newdata=testing)
error_sv<-sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing))
error_sv


## Simple function to calculate the predited RMSE with every combo of weight

Gen_pred<- function(weights) {
###Need to expand and contract per specific models this section of the function  
  lm_wt <- weights$lmWeight
  rf_wt <-  weights$rfWeight  
  sv_wt <-weights$svWeight

predictions<-((lm_predictions*lm_wt)+(rf_wt*rf_predictions)+(svm_predictions*sv_wt))
error2<-sqrt((sum((testing$y-predictions)^2))/nrow(testing))
 presult <-data.frame(error2,lm_wt,rf_wt,sv_wt)

}






################################CREATE A SEQUENCE OF WEIGHTS#############################################################################
##Modify the expand.grid function as needed for specific and number of models
weights = expand.grid(lmWeight = seq(0, 1, .1), rfWeight = seq(0, 1, .1),svWeight= seq(0,1,.1))
weights <- weights[rowSums(weights) == 1, ]
  
## Now use lappy to go through each combination 
  results <- do.call('rbind', lapply(seq(1, nrow(weights)),
                                     function(i){Gen_pred(weights =weights[i,]
                                     )
                                     }))
 
  
  ##Sort from best to worse and take the result.
  
  results_ordered <- results[order(results$error2, na.last=NA, decreasing = FALSE),]
  best_weight <- head(results_ordered,1)
  
##Final Prediction using the best weights
  predictions<-((lm_predictions*best_weight$lm_wt)+(best_weight$rf_wt*rf_predictions)+
                  (svm_predictions*best_weight$sv_wt))
  final_error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
  final_error