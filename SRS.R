# specify dir path where data file is saved
mydata<- read.csv("C:/Users/Zada/Desktop/Sampling technique/day2/mydata.csv") 
# Withdrawal of total personal income
mdata<-mydata$INCTOT
# Import data into a data frame to form a list
mdata<-data.frame(mdata)
# Replace 999999 with NA
mdata[mdata=="9999999"] <-NA
# Delete rows with NA
mdata<- subset(mdata,mdata!="NA")
# Delete rows with 0000000
mdata<- subset(mdata,mdata!="0000000")
# see the data, first 6 records shown by default
head(mdata) 
# install this package
library(survey) 
# install this package
library(sampling) 
# a seed number for reproducibility purposes
set.seed(1234)
# 718 samples were taken(sampling with replacement)
mata<-sample(nrow(mdata), 718, replace = TRUE, prob = NULL)
# Total population size
N = nrow(mdata) 
# Import data into a data frame to form a list
mata<-data.frame(mata)
# Sample size
n = nrow(mata) 
# Import data into a data frame to form a list
mata<-data.frame(mata)
# SRS Design
des = svydesign(ids = ~1, fpc = rep(N, n), data = mata) 
svytot.est <- svytotal(~mata,des, data=~mata)
svytot.est
# CI for total mdata
confint(svytotal(~mata, des))
# sampling weight
# Sample proportion, same for all obs with SRS
p.i = n/N 
# sampling weight (self-weight)
w.i = 1/p.i
# Calculate estimates of the total income of the population
# sum(w.i*x.i) = N*mean(mdata$mdata) 
t.hat = sum(w.i*mata$mata)
t.hat
