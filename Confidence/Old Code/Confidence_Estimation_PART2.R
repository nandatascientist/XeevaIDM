###################################################################################
#### SECOND CONFIGURATION
###################################################################################

ppbglm2<-preProcess(trainingData[,-8],method=c("center","scale"))
traintransformed<-predict(ppbglm2,trainingData)
cvtransformed<-predict(ppbglm2,cvData)

bglm2fit<-train(L3~.,data=traintransformed,method="bayesglm") # Train

bglm2val<-predict(bglm2fit,newdata=cvtransformed[,-8]) # Get prediction values
bglm2predictions<-rep(0,length(bglm2val)) # convert values to predictions
bglm2predictions[which(bglm2val>=confidenceThreshold)]<-1 # -do- 
# get results
bglm2results<-confusionMatrix(data=bglm2predictions,cvData$L3,positive='1')
# Output Sensitivity & Specificity
bglm2results[[4]][1] 
bglm2results[[4]][2] 

###################################################################################
#### HYPOTHESIS EXPECTATION
###################################################################################



### Create a list of possible threshold values for a given model

## calculate step size
stepSize<-(max(predictedValues) - min (predictedValues))/steps

## initialize vector of possible values
thresholdValues<-rep(0,steps)

## seed with lowest value
thresholdValues[1]<-min(predictedValues)+0.001

## loop through and populate rest of threshold vector by increasing one stepsize
## at a time
for (i in 2:steps){
        
        thresholdValues[i]<-thresholdValues[i-1]+stepSize
        
}

## Create a Matrix of predictions that correspond to each value of the 
## threshold in the threshold Vector
thresholdPredMatrix<-matrix(rep(0,steps*length(predictedValues)),
                            nrow=length(predictedValues),ncol=steps)

## For each value of threshold, fill in the predictions based on the values from
## model
for (j in 1:steps){
        
        workingThreshold<-thresholdValues[j]
        predIdx<-which(predictedValues>=workingThreshold)
        thresholdPredMatrix[predIdx,j]<-1
}

## Create a Matrix to store accuracy, Sensitivity, and specificity for 
## each set of predictions

resultMatrix<-matrix(rep(0,2*steps),ncol=2,nrow=steps)

## Populate the resultMatrix

for (k in 1:steps){
        
        confMat<-confusionMatrix(data=thresholdPredMatrix[,k],
                                 cvData$L3,positive='1')
        
        #resultMatrix[k,2]<-which() # accuracy
        resultMatrix[k,1]<-confMat[[4]][1] # Sensitivity
        resultMatrix[k,2]<-confMat[[4]][2] # specificity
}

#testdf<-cbind(bayesglmpred,cvData$L3)
#write.csv(testdf,"results.csv")


#seekThreshold<-function(predictedValues,correctValues,steps){

#        stepSize<-(max(predictedValues) - min (predictedValues))/steps

#}
