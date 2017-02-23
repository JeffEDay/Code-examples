# This is an example of using Estimated Marginal Means
# Load the lsmeans library
library("lsmeans")
# Use the datset oranges that comes with it, run linear model
oranges.lm1 <- lm(sales1 ~ price1 + price2 + day + store, data = oranges) 
anova(oranges.lm1)
# Set-up a referencem grid
( oranges.rg1 <- ref.grid(oranges.lm1) )
# Now get the estimated sales price for oranges by day  controling for the 
# covariates (price1, price2 ) in the model and averaged over store.
lsmeans(oranges.rg1, "day")