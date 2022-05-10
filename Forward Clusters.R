# Jeffrey Day
# 2022-05-10
# Perform a cluster analysis to determine if these player stats really
# do separate players into meaningful buckets.  

# load the needed libraries
library(dplyr)
library(factoextra)
library(randomForest)
library(caret)

df <- read.csv("Player Season Totals - Natural Stat Trick Rates All Situtations.csv", header=T, sep=",", stringsAsFactors = F)

# Restrict the data to only those who played 41 or more games and just forwards

fdf <- filter(df, GP >= 41 & (Position == "L" | Position == "R" | Position == "C"))

stats <- fdf[c(2,7:35)]
set.seed(12345677)
# Only numeric fields
df1 <- data.frame(stats[,-1], row.names = stats[,1])

# Clean up fields mistakenly set to character back to numeric
df1$IPP <- as.numeric(df1$IPP)
df1$SHpct <- as.numeric(df1$SHpct)

# Standardize the data
df1 <-scale(df1)
# I suspect I should be a little more diligent in chosing the basis variables
# then I am, but not going to worry about right now
segments <- kmeans(df1, 4, iter.max = 20, nstart = 50)

# get the size of the segments
print(segments$size)

# Profile the segments
print(segments$centers)

# Ideally within sum of squares is low
print(sum(segments$withinss/segments$totss))

# And the between sum of squares explains the variance
print(segments$betweenss/segments$totss)


newdf <- cbind(fdf,cluster = segments$cluster)
newdf[, c(2,37)]


fviz_cluster(segments, data =df1,
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)


# See what the within ss says is the optimal number of clusters
my_data <- scale(df1)
fviz_nbclust(my_data, kmeans, method = "wss")



# x <- filter(newdf, segments$cluster ==10)

# Does it vaildate

# Set the random seed for reproducibility.
set.seed(32984)

#rename the dependent variable and make it a factor
newdf$Label <- as.factor(newdf$cluster)

# Keep just the clusters and the basis variables
test <- newdf[c(38,5:36)]

# I suppose we should do a train/test but RF is pretty robust so 
# not going to worry about it 
rf <- randomForest(Label ~ . , data=test, ntree = 800, mtry = 6)
plot(rf)
print(rf)

confusionMatrix(test$Label, rf$predicted)
