# Iterative Proportional Fit for marginal weigthing
library(dplyr)
library(weights)
library(anesrake)

data <- read.csv("demo data.csv", sep=",", header = T)
table(data$F_AGECAT)
data$F_AGECAT <- factor(data$F_AGECAT, levels = c("1","2","3","4"),
                        labels = c("18-29", "30-49", "50-64", "65+"))
data$F_SEX <- factor(data$F_SEX, levels = c("1", "2"), labels = c("Male",
                                                                "Female"))                           
data$F_HISP <- factor(data$F_HISP, levels = c("1", "2"), labels = c("Hisp",
                                                                    "Non-Hisp"))

# Let's set targets. 

T_AGE <- c("18-29" = .195, "30-49"= .339, "50-64"=.263, "65+"=.203)
T_SEX <- c("Male"= .476, "Female"= .524)
T_HISP <- c("Hisp"= .147, "Non-Hisp"= .853)

# Combine into a list, making sure each variable is named
# to correspond to the variable in the data

target <- list(F_AGECAT=T_AGE,F_SEX=T_SEX,F_HISP=T_HISP)
str(target)

# Call the raking procedure
raking <- anesrake(target,
                           data,
                           data$QKEY,
                           cap = 5,                      # Maximum allowed weight per iteration
                           choosemethod = "total",       # How are parameters compared for selection?
                           type = "pctlim",              # What selection criterion is used?
                           pctlim = 0.01                 # Threshold for selection
                                       )
summary(raking)

data$weights <- raking$weightvec
  
wpct(data$F_SEX, data$weights)

