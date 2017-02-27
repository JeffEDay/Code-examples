


# Decision Trees

# Fitting Classification Trees
# Alternative to Logreg and/or DFA 
# In theory dependent variable can have upto 32 levels
# but becomes problematic before that.
# This technique is greedy in that it will find the variable 
# at each step that does the most for where the tree now
# stands, regardless of whether doing so would improve
# future splits. 

# One major benefit is that it will automatically collapse/group levels of a factor
# as well as find the split point in a continious variable
# An open question as to whether grouping is forced to be  adjcent or not. 
# I've used SPSS decion trees and groups could be Groups 1,3, 7
# versus Groups 2,4,5,6, and continous could be >2 and < 5, >20,
# for example.  I'll see if I can find an answer. 

library(tree) # library needs to be installed
library(ISLR)
attach(Carseats)

# Create a qualitative dependent variable
# High Volume of Sales (Yes/NO)
High=ifelse(Sales<=8,"No","Yes")

# Create a table to see overall
# distribution on dep variable (or root node
# equivalent from tree)
mytable <- table(High)
prop.table(mytable)
# Overall all 59% were not sales of 8 or more units
# 41% were sales of 8 or more units
# When we run the tree we will see if the
# values of the independent variables
# increase and decrease that figure
Carseats=data.frame(Carseats,High)

# Run the tree with High as dep and all other
# Variables except Sales as independent variables
# and set some 
# control parameters.  nobs is Training set N. size
# Set the mincut (min number of obs to include in either child
# node) and minsize of a node.  We should make this rather large
# as the default is 5 and 10 respectively so we should be at least 50 
# and 100.
# But as purchase behavior is rare, it might be okay for a smaller size
# than one would think.  For feature usage, depending on root node usgae 
# and overall N it may be higher, maybe 500 and 1,000 to avoid parsing 
# down to absurd levels

tree.carseats=tree(High~.-Sales, Carseats, control = tree.control(nobs=400,mincut=20, minsize=40))

# Summary tells us which variables entered, ordered by
# entry as well as the Misclassification rate
summary(tree.carseats)

# plot the tree structure

plot(tree.carseats)
# Add the node labels, pretty=0 uses 
# variable names, 
text(tree.carseats)

# Let's see the result

tree.carseats
# The above is hard to follow but the identation 
# is the key to following the path and mapping it to
# the structure.  If Shelve Location is bad or medium (Node 2)
# the lack of High volume sales climbs to 69% and
# High Volume sales dips to 31%.  However, bad or medium shelve location
# can be overcome by a lower price (<92.5) as 69.6% are Higher Volume sales
# (Node 4) Conversely, a higher price (>92.5) (Node 5) coupled with a bad
# or medium shelve location tanks High Volume sales as 75% are low volume
# sales. 
# Node n tells you the population and when divided by root node n is 
# % of the population.  This is key as there may be a great lift or
# decrease but the population that has this combination of features
# may not be great (and hence the need to control the node size
# tree.control parameters).  

# Do train and and test
# Should absolutely do a train and test
# to avoid overfit issues
# The code below sets a seed (needed for reproducability)
# creates a tain/test df, runs the tree and checks the results
# by applying the tree.predict to the test dataset


set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

# We should use a cross-validation technique and
# prune the tree to both simplify the tree and 
# increase are accuracy.  It is a Goldilocks type
# thing and too much pruning or too little may not
# results in the best tree, so looking for the just
# right porridge (prune)

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
# In the above, size is the number of terminal nodes
# dev is actually the misclssification error (number not %)
# k is the cost complexity parameter
# 9 terminal nodes appears to be Goldilocks as it has the lowest
# misclassification rate (50)
par(mfrow=c(1,2))
# Plot the error rate as a function of size and k

plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
# Now prune the tree with the optimal number of nodes (9)
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200


# Just an example for regression trees with a continous 
# variable
# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

