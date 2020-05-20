
#############################################
#Project Management Module 
#ECOL 8540 Computational Workshop
#Author: William Norfolk
#Date: 5/20/2020
#############################################

#Exercise. Create a new script following the prototype we introduced. 
#Your script should load the MERS data and make a plot.

#load libs
library(lubridate)
library(tidyverse)

#load mers data via relative path to raw data repository
mers <- read.csv("../raw_data/cases.csv")

#quick data clean to make some more robust plotting data
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
day0 <- min(na.omit(mers$onset2))
mers$epi.day <- as.numeric(mers$onset2 - day0)
#create a boxplot to see if there was a gender bias in epidemic day
ggplot(fix_cols, aes(gender, epi.day)) + geom_boxplot()

