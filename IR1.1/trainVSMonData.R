
trainVSMonData<-function(trainingData){
        
        ## A Corpus is created from the training set where each element 
        ## of the list is one document that represents one item group 
        numDocs<-length(trainingData)
        docList<-VectorSource(trainingData)
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
        docFrequency<-rep(0,length(dictionaryOfTerms))
        
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
        
        
        trainedVSMModel<-list(tfIdfMatrix,dictionaryOfTerms,docFrequency,trainingData)
        
        trainedVSMModel
        
}

