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
datafileName<-"c:\\idm\\TopicSearchwithL1only.csv" # source file

################################################################
##### 1 - READ DATA FILE INTO R AND PERFORM DATA PREPROCESSING
################################################################

## Read file and subset the columns needed for analysis
dataFile<-read.csv(datafileName)
usefulCols<-c(3,8,11,14,15,17) 
workingData<-dataFile[,usefulCols]

## Set the distance columns as numeric,
## they were read in as  factors during file read
workingData$Dist.1.and.2<-as.numeric(workingData$Dist.1.and.2)
workingData$Dist.1.and.3<-as.numeric(workingData$Dist.1.and.3)
workingData$Dist.2.and.3<-as.numeric(workingData$Dist.2.and.3)

## Clean data set for analysis
workingData<-workingData[complete.cases(workingData),]

####################################################################
##### 2 - PERFORM SAMPLING AND CREATE  TRAINING, CV AND TESTING SETS
####################################################################

## Get training and cross validation in one set
inTrainCV<-createDataPartition(y=workingData$L2,p=0.8,list=FALSE)
traincv<-workingData[inTrainCV,]

## 75% of 80% is 60%: Split traincv to training and crossvalidation
inTrain<-createDataPartition(y=traincv$L2,p=0.75,list=FALSE)
trainingData<-traincv[inTrain,]
cvData<-traincv[-inTrain,]

## Get testing data
testingData<-workingData[-inTrainCV,]

## clean out workspace
rm(traincv,dataFile) 

###################################################################
##### 3 - TRAIN MODEL AND SELECT THRESHOLD FROM CROSSVALIDATION SET
####################################################################

## Train on Bayes GLM model using training set data
bglm1fit<-train(L2~.,data=trainingData,method="bayesglm") 

## Apply model on cross-validation data set and get ouputs
bglm1val<-predict(bglm1fit,newdata=cvData[,-6])

## Scale prediction values to  fall in [0,1] 
obj<-fitInZeroOneScale(bglm1val)
outputs<-obj[[1]]

## Seek threshold that maximizes F1 score {given skewed classes}
confidenceThreshold<-getBestThreshold(outputs,cvData$L3)

## Using threshold, convert prediction values to true classifications
cvPredVector<-rep(0,length(outputs)) 
cvPredVector[which(outputs>=confidenceThreshold)]<-1

##  Compare Predictions to actual values or labels in cross validation set
cvResults<-confusionMatrix(data=cvPredVector,cvData$L3,positive='1')

## Output performance measures for the run
cvResults[[3]][1] # Accuracy
cvResults[[4]][1] # Sensitivity
cvResults[[4]][2] # Specificity


###################################################################
##### 4 - CHECK GENERALIZATION ERROR OF MODEL ON TEST SET
###################################################################

## Using the trained model, get prediction values on test set examples 
bglm2val<-predict(bglm1fit,newdata=testingData[,-8])

## Scale the prediction values by the same range and min values obtained from
## cross validation set
obj2<-fitInZeroOneScale(bglm2val,obj[[2]],obj[[3]])
testVal<-obj2[[1]]

## Apply previously obtained threshold to convert values to true classifications
testPredVector<-rep(0,length(testVal)) 
testPredVector[which(testVal>=confidenceThreshold)]<-1

## Compare actual results to predicted values
testResults<-confusionMatrix(data=testPredVector,testingData$L3,positive='1')

## Output performance measures for the run
testResults[[3]][1] # Accuracy
testResults[[4]][1] # Sensitivity
testResults[[4]][2] # Specificity


###################################################################
##### 5 - EXTRACT MODEL COEFFICIENTS FOR OFFLINE USE
###################################################################


a<-bglm1fit$finalModel
coef(a)
