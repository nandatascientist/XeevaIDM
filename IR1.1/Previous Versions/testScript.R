
###### Preprocessing

## Read datafile into R memory as a R dataframe and name columns
rawData<-read.csv("testamznfile.csv",stringsAsFactors=FALSE)

names(rawData)<-c("productname","productid","specifications","features","description","image","URL","commodity","class","family","segment")

## Change all classification codes into factors
rawData$segment<-as.factor(rawData$segment)
rawData$family<-as.factor(rawData$family)
rawData$class<-as.factor(rawData$class)
rawData$commodity<-as.factor(rawData$commodity)

## clean the dataframe of Columns & Rows that we are not going use
rawData$productid<-NULL
rawData$specifications<-NULL
rawData$image<-NULL
rawData$URL<-NULL

dirtyRows<-which(rawData$segment=="#VALUE!")
rawData<-rawData[-dirtyRows,]


## concatenate the data we are going to use into one variable & drop individual fields
rawData$text<-paste(rawData$productname,rawData$features,rawData$description, sep=" ")

rawData$productname<-NULL
rawData$features<-NULL
rawData$description<-NULL

## split the dataset by the classification level - segment and store the resulting 
## list for further downstream processing
resultList<-split(rawData$text,rawData$segment, drop=TRUE)


rm(rawData,dirtyRows)

###################### Training and test set segmentation ##################333

## initialize variables for splitting data into testing and training
numDocs<-length(resultList)

training<-list()
testing<-list()
minRows<-10
trainingFraction<-0.8

for (i in 1:numDocs){
        
        numElements<-length(resultList[[i]])        
        ## enforce that atleast minRows need to be there for training /testing
        
        if(numElements>=minRows){
                
                # calculate Number of training examples needed
                numTrain<-round(trainingFraction*numElements,0) 
                
                # randomly select numTrain number of items 
                idx<-sample(1:numElements,numTrain,replace=F)
                
                # add the randomly selected items to training and testing sets
                training[[i]]<-paste(resultList[[i]][idx],collapse='')
                testing[[i]]<-as.character(resultList[[i]][-idx])
                
        } else {
                
                # add empty elements to the testing and training sets 
                # if threshold for number of elements are not met
                
                training[[i]]<-c('')
                testing[[i]]<-c('')
                
        }
}

names(training)<-names(resultList)
names(testing)<-names(resultList)

rm(resultList,numElements,minRows,trainingFraction,idx,i,numTrain)

###################### Training algorithm ##################

library(tm)
library(SnowballC)

## A Corpus is created from the training set where each element of the list is one document that represents one segment

docList<-VectorSource(c(training,testing[[1]][1]))
docList$Names<-c(names(training),"query")

documentCorpus<-Corpus(docList)

## Punctuation is removed , white space is stripped, and lower case conversion is performed
documentCorpus<-tm_map(documentCorpus,removePunctuation)
documentCorpus<-tm_map(documentCorpus,tolower)
documentCorpus<-tm_map(documentCorpus,stripWhitespace)

## Optionally perform word stemming on corpus
# segmentCorpus<-wordStem(segmentCorpus)

## create sparse matrix by converting the document to Matrix
termDocMatrix<-as.matrix(TermDocumentMatrix(documentCorpus))

## save the dictionary of terms for use in testing
dictionaryOfTerms<-attributes(termDocMatrix)[[2]]$Terms

getTfIdfWeights<-function(tfVector,df){
        
        # Computes tfidf weights from a term frequency vector and a document
        # frequency scalar
        weight = rep(0, length(tfVector))
        weight[tfVector > 0] = (1 + log2(tfVector[tfVector > 0])) * log2(numDocs/df)
        weight
        
}

getWeightsperTermVector<-function(tfIdfRow){
        
        termDf<-sum(tfIdfRow[1:numDocs]>0)
        tdIdfVector<-getTfIdfWeights(tfIdfRow,termDf)
        return(tdIdfVector)
}


tfIdfMatrix<-t(apply(termDocMatrix,c(1),FUN=getWeightsperTermVector))
colnames(tfIdfMatrix)<-colnames(termDocMatrix)

tfIdfMatrix<-scale(tfIdfMatrix,center=FALSE,scale=
                           sqrt(colSums(tfIdfMatrix^2)))



qVector<- tfIdfMatrix[,numDocs+1]
tfIdfMatrix<-tfIdfMatrix[,1:numDocs]

qVector[which(!is.finite(qVector))]<-0
tfIdfMatrix[which(!is.finite(tfIdfMatrix))]<-0

computedScore<- t(qVector) %*% tfIdfMatrix

finalresult<-data.frame(doc=names(training),score=t(computedScore))
finalresult<-finalresult[order(finalresult$computedScore,decreasing=TRUE),]


###################### Testing ##################

queryList<-VectorSource(list(testing[[2]][1]))
#queryList$Names<-c(names(testing[2]),names(testing[4]))
testCorpus<-Corpus(queryList)

testCorpus<-tm_map(testCorpus,removePunctuation)
testCorpus<-tm_map(testCorpus,tolower)
testCorpus<-tm_map(testCorpus,stripWhitespace)

#queryTermVector<-termFreq(testCorpus,)

termDocMatrixTest<- as.matrix(TermDocumentMatrix(testCorpus,
                                                 control=list(dictionary=dictionaryOfTerms))  
)

#queryMatrix<-t(apply(termDocMatrixTest,c(1),FUN=getWeightsperTermVector))
queryVector<-getTfIdfWeights(t(termDocMatrixTest),1)
queryVector<-scale(queryVector,center=FALSE,scale=
                           sqrt(sum(queryVector^2)))


queryscore<-t(queryVector) %*% tfIdfMatrix

rd<-data.frame(doc=names(training),score=t(queryscore))
rd<-rd[order(rd$score,decreasing=TRUE),]

