library(MASS)
library(ISLR)
library(car)
setwd("D:/Lab")
attach(Carseats ) 
##Assumes a qualitive/factor variable
contrasts (ShelveLoc ) 