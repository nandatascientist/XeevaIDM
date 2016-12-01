
#######################################
##### O - INITIALIZE SCRIPT PARAMETERS
#######################################

## Load the needed libraries and functions
library(caret)
library(ggplot2)
library(ROCR)
library(Hmisc)

source("Sigmoid.R")

## Set script parameters
dataFileLocation<-"c:\\idm\\VSM_ID_5c_Analysis.csv" # source file

################################################################
##### 1 - READ DATA FILE INTO R AND PERFORM DATA PREPROCESSING
################################################################

## Read file and subset the rows needed for analysis
dataFile<-read.csv(dataFileLocation)

## just work with ~ 75% of the data
includeRows<-which(dataFile$TotalUniverseRows>0 & 
                           dataFile$TotalUniverseRows<11000)

workingData<-dataFile[includeRows,] # subset operation


## Convert  pct Xeeva field into a nominal field with 3 divisions
workingData$PX<-cut2(workingData$PercentXeeva,seq(0,1,by=0.25))

summary(workingData$PX) # helps view the distribution of values



qplot(TotalUniverseRows,PercentCorrect,data=workingData,color=PX,
      geom="smooth",method="loess")

