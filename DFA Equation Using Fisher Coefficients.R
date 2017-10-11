# example of scoring a dataset with standardized Fisher Coefficients
# Uses the iris dataset included in r
# should run for all as long as the DiscriMiner package is
# installed and loaded

library(DiscriMiner)
# Run the discriminant Analsysis
mylda = linDA(iris[,1:4], iris[,5])
# Viw the model
summary(mylda)
mylda$confusion
# Take a look at the  values of the Fisher Coefficients
mylda$functions

# This manually grabs the values.  Really should just reference the values in the mylda object
# but a part of this exercise is showing the values so that they can be used in other software and other versions 
# of a dataset, e.g., socring new data that arrives into a database. 
iris$P_setosa <- -86.31 + iris$Sepal.Length*23.54 +
                    iris$Sepal.Width*23.59 + 
                    iris$Petal.Length*-16.43 +
                    iris$Petal.Width*-17.40
iris$P_versicolor <- -72.85 + iris$Sepal.Length*15.70 +
                        iris$Sepal.Width*7.07 + 
                        iris$Petal.Length*5.21 +
                        iris$Petal.Width*6.43
  
iris$P_virginica <- -104.37 + iris$Sepal.Length*12.45 +
                          iris$Sepal.Width*3.69 + 
                          iris$Petal.Length*12.77 +
                          iris$Petal.Width*21.0 
# subset the data 
newdata <- iris[c(6:8)] 
# find the maximum value out for each row across the group equations
iris$max_value <- apply(newdata, 1, max)
# Now place into a specific group, i.e., classify them.
iris$Group <-ifelse(iris$max_value == iris$P_setosa,'Setosa',
                ifelse(iris$max_value == iris$P_versicolor,'Versicolor',
                       ifelse(iris$max_value == iris$P_virginica,'Virginica',0)))
# Now we should see the same results as our confusion table.  This double checks are work
# and if it matches the equation can be applied on different data. 
table(iris$Species,iris$Group) 