
#######################################
##### O - INITIALIZE SCRIPT PARAMETERS
#######################################


## Load the needed libraries and functions

library(tm)
library(SnowballC)
library(caret)
library(ggplot2)
library(ROCR)

## Set script parameters
set.seed(12345) # for repeatability of random numbers 
datafileName<-"c:\\idm\\ID5cRunResults.csv" # source file
confidenceThreshold<-0.64


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

bglm1fit<-train(L3~.,data=trainingData,method="bayesglm") # Train
bglm1val<-predict(bglm1fit,newdata=cvData[,-8]) # Get prediction values
bglm1predictions<-rep(0,length(bglm1val)) # convert values to predictions
bglm1predictions[which(bglmval>=0.1665070)]<-1 # -do- 
# get results
bglm1results<-confusionMatrix(data=bglm1predictions,cvData$L3,positive='1')
# Output Sensitivity & Specificity
bglm1results[[3]][1] 
bglm1results[[4]][1] 
bglm1results[[4]][2] 

## Scale and center the variables 
range<-max(bglm1val)-min(bglm1val)
minval<-min(bglm1val)
t<-bglm1val-minval
bglmval<-t/range

## create predictions object
pred<-prediction(bglmval,cvData$L3)

## create different performance objects
acc<-performance(pred,"acc") # Accuracy
f1score<-performance(pred,"f") # F1 Score
sens<-performance(pred,"sens") # Sensitivity


## plot different performance objects relative to cutoffs
plot(acc,avg="vertical",
     spread.estimate="boxplot",show.spread.at=seq(0.1,0.9,by=0.1))

plot(f1score,avg="vertical",
     spread.estimate="boxplot",show.spread.at=seq(0.1,0.9,by=0.1))

plot(sens,avg="vertical",
     spread.estimate="boxplot",show.spread.at=seq(0.1,0.9,by=0.1))

#predictedValues<-bglm1val
#steps<-100

cutoffs <- data.frame(cutoff=f1score@x.values[[1]], score=f1score@y.values[[1]])

cutoffs[which.max(cutoffs$score),1]

cutoffs <- cutoffs[order(cutoffs$score, decreasing=TRUE),]

getBestThreshold<-function(){

        library(ROCR)
        
        
}
