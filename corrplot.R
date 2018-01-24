# devtools::install_github("kassambara/ggcorrplot")
library(ggplot2)
library(ggcorrplot)


# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars, use = "pairwise.complete.obs"), 1)


# Plot
ggcorrplot(corr, hc.order = TRUE, 
           type = "upper", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars dataframe", 
           ggtheme=theme_classic)