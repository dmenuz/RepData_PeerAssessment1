#Script for analyzing activity data

##Reproducible Research Project 1 June 29, 2016

###Download data, unzip, and read data

setwd("C:\\Users\\diane\\Google Drive\\DianeSharedDrive\\DataScienceSpecialization\\gitRepo\\ReproducibleResearch")
datfile="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
destfile="dat.zip"
download.file(datfile, destfile)
zipList=unzip(destfile, list=TRUE)
unzip(destfile)
act=read.csv("activity.csv")
