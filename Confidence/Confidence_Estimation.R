#######################################
##### O - INITIALIZE SCRIPT PARAMETERS
#######################################

## Load the needed libraries and functions
library(caret)
library(ggplot2)
library(ROCR)

source("ConfidenceUtilities.R")

## Set script parameters
set.seed(12345) # for repeatability of random numbers 
datafileName<-"c:\\idm\\ID5cRunResults.csv" # source file

#########################################################
##### 1 - READ DATA FILE INTO R AND PERFORM PREPROCESSING
#########################################################

## Read file and subset the columns needed for analysis
dataFile<-read.csv(datafileName)
usefulCols<-c(3,5,6,8,11,14,15,18) 
workingData<-dataFile[,usefulCols]

workingData[,5]<-as.numeric(workingData[,5])
workingData[,6]<-as.numeric(workingData[,6])
workingData[,7]<-as.numeric(workingData[,7])

## Clean data set for analysis
workingData<-workingData[complete.cases(workingData),]

####################################################################
##### 2 - PERFORM SAMPLING AND CREATE  TRAINING, CV AND TESTING SETS
####################################################################

## Get training and cross validation in one set
inTrainCV<-createDataPartition(y=workingData$L3,p=0.8,list=FALSE)
traincv<-workingData[inTrainCV,]

# 75% of 80% is 60%
inTrain<-createDataPartition(y=traincv$L3,p=0.75,list=FALSE)
# Get the individual data sets
trainingData<-traincv[inTrain,]
cvData<-traincv[-inTrain,]

## Get testing data
testingData<-workingData[-inTrainCV,]

## clean out workspace
rm(traincv,dataFile) 

##############################################
##### 3 - TRAIN DIFFERENT MODELS ON THIS DATA
##############################################

# Train on Bayes GLM
bglm1fit<-train(L3~.,data=trainingData,method="bayesglm") 
# Get predicted values
bglm1val<-predict(bglm1fit,newdata=cvData[,-8])
# Scale predictions
outputs<-fitInZeroOneScale(bglm1val)

#Get Threshold
confidenceThreshold<-getBestThreshold(outputs,cvData$L3)

# convert values to predictions
predVector<-rep(0,length(outputs)) 
predVector[which(outputs>=confidenceThreshold)]<-1

# get results
results<-confusionMatrix(data=predVector,cvData$L3,positive='1')
# Output Sensitivity & Specificity
results[[3]][1] 
results[[4]][1] 
results[[4]][2] 
