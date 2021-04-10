
#set the working directory
setwd("C:/Users/Madhur.Nema/Desktop/R programs/Coursera Project")
#read the csv file from working directory
outcome <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
#basic data sanity check
head(outcome)
ncol(outcome)
nrow(outcome)
names(outcome)
#to change the class numeric of column from character to numeric
outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])