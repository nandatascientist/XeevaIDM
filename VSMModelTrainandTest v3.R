
## Load the needed libraries and functions

library(tm)
library(SnowballC)
library(caret)

source("preProcessFileData.R")
source("splitDataSet.R")
source("trainVSMonData.R")
source("classifyItem.R")
source("predictOutput.R")

## Set script parameters
set.seed(12345) # for repeatability of random numbers 
rawDatafileName<-"amazon_supply_Demo_Output_03-07-15.csv" # name of file in current dir
minimumRowsForTraining<-10 # min representation in data for consideration in training
trainingSetFraction<-0.9 # amount of data set used for training Vs testing
numRecords<-50000 #number of lines to read
level<-1 # 1= Segment,2 = Family, 3 = Class, 4 = Commodity


## get usable data from file
p1<-Sys.time()
processedData<-preprocessFileData(rawDatafileName,level,
                                              minimumRowsForTraining,numRecords)


## segment data into testing and training sets
p2<-Sys.time()
segList<-splitDataSet(processedData,trainingSetFraction)
trainingSet<-segList[[1]]
testingSet<-segList[[2]]

## Train a VSM model based on training data
p3<-Sys.time()
trainedVSMModel<-trainVSMonData(trainingSet)


## Predict classification on test data using trained model
p4<-Sys.time()
predictions<-predictOutput(testingSet,trainedVSMModel)
p5<-Sys.time()

p5-p1
predictions[[2]]$overall[1]
