
# install.packages("jsonlite")
# install.packages("curl")
# devtools::install_github('PMassicotte/gtrendsR')
# load libraries 
library(gtrendsR)
library(httr)
library(Rcpp)
library(plyr)
library(dplyr)

# grab the trends
 
# gtrends for function args 

stdate <- "2017-07-16" 
endate <-as.character(Sys.Date()-8) 
mytime <- paste(stdate,endate)
GTdata <- gtrends("Hurricane", time = mytime , gprop = "web", hl = "en" )

# plot interest over time

plot(GTdata)

# Convert to data frames that can be written out to .csv

interest_over_time <- ldply(GTdata[1], data.frame)
interest_by_region <- ldply(GTdata[2], data.frame)
interest_by_dma <- ldply(GTdata[3], data.frame)
interest_by_city <- ldply(GTdata[4], data.frame)
related_topics <- ldply(GTdata[5], data.frame)
related_queries <- ldply(GTdata[6], data.frame)

write.csv(interest_over_time, file="Hurrican Search Trends.csv")
