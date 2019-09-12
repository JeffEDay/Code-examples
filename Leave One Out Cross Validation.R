# This program demonstrates the use of Leave-One-Out-Cross-Validation (LOOCV)
# In general is a good technique and preferably over the typical 
# random split of the data into train and test sets.  K-folds cross 
# validation where k=5 or 10 might be better than LOOCV, but 
# when you have a small dataset and estimates might suffer by dividing the dataset in 5ths 
# 10ths at a time, this is preferable.  We can use the glm package to do
# the CV. GLM is eqivalent to Liner Regression.

# This example uses data provvided with R so it can be run by anyone.
set.seed (2)
options(digits = 4)
library (ISLR) # Get the Auto data

# First do a regression Using all of the data 
# In this case there is absolutly no cross-validation at all
lm.fit =lm(mpg~horsepower ,data=Auto)

# Look at Coeficients
lm.fit$coefficients
# Get MSE as that is the key stat we look at for cross validation
mean((Auto$mpg -predict (lm.fit ,Auto))^2)

# Show R-square and other stats
summary(lm.fit)

# Now use GLM on all the data to show it is equivalent 
glm.fit=glm(mpg~horsepower ,data=Auto)

# Look at the coefficients and see they are the same
glm.fit$coefficients

# Look at Mean Square Error and see it is the same
mean((mpg -predict (glm.fit ,Auto))^2)

# Caluclate R-square (GLM does not do it directly) and see if it is the same
Rsq <- 1 - (glm.fit$deviance/glm.fit$null.deviance)
Rsq
# Now Let's perform Cross Validation

library (boot) # contains the cv.glm we need

# We use the already glm fitted model
cv.err =cv.glm(Auto ,glm.fit)

# We look at the calulated MSE in delta.  For this particular use case, we
# are only interested in how similiar our MSE is to the glm or lm model.  
# The closer they are the more confidence we have in our original results.  
cv.err$delta

# Let's do k-folds of 10
cv.err2 =cv.glm(Auto, glm.fit, K=10)

# We look at the calulated MSE in delta.  For this particular use case, we
# are only interested in how similiar our MSE is to the glm or lm model.  The closer 
# they are the more confidence we have in our original results.  
# The first fold is treated as a validation set, and the method
# is fit on the remaining k folds. The mean squared error, MSE, is
# then computed on the observations in the held-out fold.
cv.err2$delta

