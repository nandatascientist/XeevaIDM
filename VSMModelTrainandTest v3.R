
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
rawDatafileName<-"c:\\idm\\amazon_supply_Demo_Output_03-07-15.csv" # source file
minimumRowsForTraining<-40 # min rows in data need to consider for training
trainingSetFraction<-0.8 # amount of data set used for training Vs testing
numRecords<-10000 #number of lines to read
level<-4 # 1= Segment,2 = Family, 3 = Class, 4 = Commodity
testsPerGroup<-10 # number of records per group that testing will be performed on
runBookName<-"AmazonDataRunBook.csv" # name of file to record results

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
predictions<-predictOutput(testingSet,trainedVSMModel,testsPerGroup)
p5<-Sys.time()

cat("From start to training completion:",as.double(p4-p1))
cat("Just for testing:",as.double(p5-p4))

predictions[[2]]$overall

## write results into run book
runResults<-paste(as.character(level),numRecords,
                  length(testingSet),minimumRowsForTraining,
                       predictions[[2]]$overall[1],
                       as.double(difftime(p5,p1,units="mins")),sep=',')


write.table(runResults,runBookName,append=TRUE,col.names=FALSE,quote=FALSE)
