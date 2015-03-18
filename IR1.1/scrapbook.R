
########## Step #1: setting the script parameters ###############################

set.seed(12345)

########## Step #1: Perform Data Pre-processing from input files ################


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
resultList<-split(rawData$text,rawData$segment,drop=TRUE)

counts<-sapply(resultList,length)
idx<-which(counts>=10)
resultList<-resultList[idx]

rm(rawData,dirtyRows)

########## Step #2: Segment data into training and testing sets ##################

## initialize variables for splitting data into testing and training
numDocs<-length(resultList)

training<-list()
testing<-list()
trainingFraction<-0.9

for (i in 1:numDocs){
        
        numElements<-length(resultList[[i]])        
        ## enforce that atleast minRows need to be there for training /testing
        
                
        # calculate Number of training examples needed
        numTrain<-round(trainingFraction*numElements,0) 
        
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
        
        listItem<-testing[[z]]
        listItemLength<-length(listItem)
        itemCategory<-rep(names(testing[z]),listItemLength)
        testBedRows<-data.frame(itemCategory,listItem,stringsAsFactors=FALSE)
        testBedvone<-rbind(testBedvone,testBedRows)
        
}

numTestExamples<-nrow(testBedvone)
testBedvone$output<-c(" ")

testBedvone$listItem[2]
testBedvone$itemCategory[2]



queryList<-VectorSource(testBedvone$listItem[2])
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

nDocs<-dim(tfIdfMatrix)[2]

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
queryscores<-t(queryTfIdf) %*% tfIdfMatrix

matchLoc<-which.max(queryscores)

dimnames(tfIdfMatrix)[2][matchLoc]

## rank in order of closeness
rd<-data.frame(doc=names(training),score=t(queryscores))
rd<-rd[order(rd$score,decreasing=TRUE),]

