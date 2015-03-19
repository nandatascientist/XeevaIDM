library(caret)
library(tm)
########## Step #1: setting the script parameters ###############################

set.seed(12345)
rawDatafileName<-"c:\\idm\\PreProcessedInputP2R1.0.csv" # source file
minimumRowsForTraining<-50 # min rows in data need to consider for training
trainingSetFraction<-0.8 # amount of data set used for training Vs testing
numRecords<-100000 #number of lines to read
level<-1 # 1= Category,2 = Sub-Category
testsPerGroup<-5 # number of records per group that testing will be performed on
runBookName<-"XeevaClientDataRunBook.csv" # name of file to record results


########## Step #1: Perform Data Pre-processing from input files ################

t1<-Sys.time()

## Read datafile into R memory as a R dataframe and name columns
rawData<-read.csv(rawDatafileName,nrows=numRecords,
                  stringsAsFactors=FALSE)

names(rawData)<-c("cat.code","cat.subcat","subcat.code",
                  "item.name","item.mpn","mfg.name")

## Change all classification codes into factors
rawData$cat.code<-as.factor(rawData$cat.code)
rawData$subcat.code<-as.factor(rawData$subcat.code)

## clean the dataframe of Columns & Rows that we are not going use
rawData$cat.subcat<-NULL

## concatenate the data we are going to use into one variable & drop individual fields
rawData$text<-paste(rawData$item.name,
                    rawData$item.mpn,
                    rawData$mfg.name, sep=" ")


# get rid of the fields now that we have it in the text column
rawData$item.name<-NULL
rawData$item.mpn<-NULL
rawData$mfg.name<-NULL

## split the dataset by the classification level - CAT  and store the resulting 
## list for further downstream processing

if(level==1){

        resultList<-split(rawData$text,rawData$cat.code,drop=TRUE)
        
}else if(level==2){
        
        resultList<-split(rawData$text,rawData$subcat.code,drop=TRUE)
}


counts<-sapply(resultList,length)
idx<-which(counts>=minimumRowsForTraining)
resultList<-resultList[idx]

rm(rawData)

########## Step #2: Segment data into training and testing sets ##################

## initialize variables for splitting data into testing and training
numDocs<-length(resultList)

training<-list()
testing<-list()


for (i in 1:numDocs){
        
        numElements<-length(resultList[[i]])        
        ## enforce that atleast minRows need to be there for training /testing
        
                
        # calculate Number of training examples needed
        numTrain<-round(trainingSetFraction*numElements,0) 
        
        # randomly select numTrain number of items 
        idx<-sample(1:numElements,numTrain,replace=F)
        
        # add the randomly selected items to training and testing sets
        training[[i]]<-paste(resultList[[i]][idx],collapse='')
        testing[[i]]<-as.character(resultList[[i]][-idx])
                
}

names(training)<-names(resultList)
names(testing)<-names(resultList)

rm(resultList,numElements,trainingFraction,idx,i,numTrain)

########## Step #3: Train the model on training data set  ######################

library(tm)
library(SnowballC)

## A Corpus is created from the training set where each element of the list is one document that represents one segment

docList<-VectorSource(training)
documentCorpus<-Corpus(docList)

## Punctuation is removed , white space is stripped, and lower case conversion is performed
documentCorpus<-tm_map(documentCorpus,removePunctuation)
documentCorpus<-tm_map(documentCorpus,tolower)
documentCorpus<-tm_map(documentCorpus,stripWhitespace)

## Optionally perform word stemming on corpus
# segmentCorpus<-wordStem(segmentCorpus)

## create sparse matrix by converting the document to Matrix
termDocMatrix<-as.matrix(TermDocumentMatrix(documentCorpus))

## store the number of terms and dictionary for use in testing
numWords<-dim(termDocMatrix)[1]
dictionaryOfTerms<-attributes(termDocMatrix)[[2]]$Terms

## Define a function that computes tfidf weights from a term frequency vector 
## and a document frequency scalar

getTfIdfWeights<-function(tfVector,df){
        
        weight = rep(0, length(tfVector)) # initialize weights to zero
        weight[tfVector > 0] = (1 + log2(tfVector[tfVector > 0])) * log2(numDocs/df)
        weight
        
}

## Define a function returns the weights for every term vector

getWeightsperTermVector<-function(tfIdfRow){
        
        termDf<-sum(tfIdfRow[1:numDocs]>0)
        tdIdfVector<-getTfIdfWeights(tfIdfRow,termDf)
        return(tdIdfVector)
}


## Obtain the Matrix that houses the weighted values for the term vectors
tfIdfMatrix<-t(apply(termDocMatrix,c(1),FUN=getWeightsperTermVector))
colnames(tfIdfMatrix)<-colnames(termDocMatrix)


## Remove non numeric values - general cleanup
tfIdfMatrix[which(!is.finite(tfIdfMatrix))]<-0

## norm each vector to one
tfIdfMatrix<-scale(tfIdfMatrix,center=FALSE,scale=
                           sqrt(colSums(tfIdfMatrix^2)))

rm(docList,documentCorpus,termDocMatrix)

########## Step #4: Test the model on test data  ################################

## Define a function that classifies  input itemText using a trained 
## tfIdfMatrix

classifyItem<-function(itemText,trainedTfIdfMatrix){
        
        ## create the input Vector by following the same pre-processing 
        ## used in training
        
        queryList<-VectorSource(itemText)
        testCorpus<-Corpus(queryList)
        testCorpus<-tm_map(testCorpus,removePunctuation)
        testCorpus<-tm_map(testCorpus,tolower)
        testCorpus<-tm_map(testCorpus,stripWhitespace)
        
        
        ## Compute the termDocumentMatrix for the query 
        termDocMatrixTest<- as.matrix(TermDocumentMatrix(testCorpus,
                                                         control=list(dictionary=dictionaryOfTerms))  
        )
        
        
        
        
        ## To compute the weights for terms, initialize to zero, and calculate
        ## the weight for all non-zero occurence terms
        queryTfIdf<-rep(0, numWords) 
        
        nDocs<-dim(trainedTfIdfMatrix)[2]
        
        for (l in 1:numWords){
                
                if(termDocMatrixTest[l]>0){
                        
                        queryTfIdf[l]<- 
                                (1 + log2(termDocMatrixTest[l])) *
                                log2(nDocs)
                        
                }
                
                
        }
        
        
        ## clean-up any non numeric values
        queryTfIdf[which(!is.finite(queryTfIdf))]<-0
        
        ## norm vector values to one as before
        queryTfIdf<-scale(queryTfIdf,center=FALSE,scale=
                                  sqrt(sum(queryTfIdf^2)))
        
        
        
        ## compute the angle beween query and training vectors
        queryscores<-t(queryTfIdf) %*% trainedTfIdfMatrix
        
        ## rank in order of closeness
        rd<-data.frame(doc=names(training),score=t(queryscores))
        rd<-rd[order(rd$score,decreasing=TRUE),]
        
        
        return(as.character(rd[1,1]))
}


## Define a test bed of data and store classification results



testListSize<-length(testing)

testBedvone<-data.frame()

for (z in 1:testListSize){
        
        listOfItemInGroup<-testing[[z]]
        numItemsInGroup<-length(listOfItemInGroup)
        
        numItemsToSample<-numItemsInGroup
        
        if(numItemsInGroup>testsPerGroup){
                numItemsToSample<-testsPerGroup
        }  
        
        idx<-sample(1:numItemsInGroup,numItemsToSample,replace=F)
        itemCategory<-rep(names(testing[z]),numItemsToSample)
        testBedRows<-data.frame(itemCategory,
                                listOfItemInGroup[idx],stringsAsFactors=FALSE)
        
        testBedvone<-rbind(testBedvone,testBedRows)
        
}

numTestExamples<-nrow(testBedvone)
testBedvone$output<-c(" ")
colnames(testBedvone)<-c("group","itemtext","output")

### Individual sample testing
#testBedvone$itemtext[2]
#testBedvone$group[2]

#classifyItem("Trigger 600R-025  ",tfIdfMatrix)

### Testing a list

t2<-Sys.time()

for (ctr in 1:numTestExamples) {
        
        testBedvone$output[ctr]<-classifyItem(
                testBedvone$itemtext[ctr],tfIdfMatrix)
        
}

## check if there are groups in the prediction set not in the test set.
groupDiff<-setdiff(testBedvone$group,testBedvone$output)
numMissingGroups<-length(groupDiff)

if(numMissingGroups>0){
        
        for (cleanCtr in 1:numMissingGroups){
                
                testBedvone<-rbind(testBedvone,
                                   c(groupDiff[cleanCtr],"dummy text",
                                     c(groupDiff[cleanCtr])))        
        }
        
}

testBedvone$group<-as.factor(testBedvone$group)
testBedvone$output<-as.factor(testBedvone$output)

accuracy<-confusionMatrix(testBedvone$output,testBedvone$group)$overall[1]

t3<-Sys.time()

runResults<-paste(as.character(level),numRecords,
                  length(testingSet),minimumRowsForTraining,
                  accuracy,
                  as.double(difftime(t3,t1,units="mins")),sep=',')
