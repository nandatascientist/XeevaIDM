
classifyItem<-function(itemText,trainedModel){
        
        ## create the input Vector by following the same pre-processing 
        ## used in training
        
        
        queryList<-VectorSource(itemText)
        testCorpus<-Corpus(queryList)
        testCorpus<-tm_map(testCorpus,removePunctuation)
        testCorpus<-tm_map(testCorpus,tolower)
        testCorpus<-tm_map(testCorpus,stripWhitespace)
        
        ## Extract model components for use
        trainedTfIdfMatrix<-trainedModel[[1]]
        dictionaryFromModel<-trainedModel[[2]]
        trainingData<-trainedModel[[4]]
        
        
        ## Compute the termDocumentMatrix for the query 
        termDocMatrixTest<- as.matrix(TermDocumentMatrix(testCorpus,
                                                         control=list(dictionary=dictionaryFromModel))  
        )
        
        
        ## To compute the weights for terms, initialize to zero, and calculate
        ## the weight for all non-zero occurence terms
        numWords<-length(dictionaryFromModel)
        
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
        matchLoc<-which.max(queryscores)        
        
        return(as.character(names(trainingData)[matchLoc]))
}

