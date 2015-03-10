
splitDataSet<-function(documentList,trainingFraction){
        
        ## initialize variables for splitting data into testing and training
        numDocs<-length(documentList)
        training<-list()
        testing<-list()
        
        ## for each document in input list
        for (i in 1:numDocs){
                
                ## get number of elements in given item group
                numElements<-length(documentList[[i]])        
                        
                ## calculate Number of training examples needed
                numTrain<-round(trainingFraction*numElements,0) 
                
                ## randomly select numTrain number of items 
                idx<-sample(1:numElements,numTrain,replace=F)
                
                ## add the randomly selected items to training  set
                ## & merge all the elements belonging to one category
                training[[i]]<-paste(documentList[[i]][idx],collapse='')
                
                ## add the remaining elements to the testing set 
                ## keeping them seperate 
                testing[[i]]<-as.character(documentList[[i]][-idx])
                        
        }
        
        names(training)<-names(documentList)
        names(testing)<-names(documentList)
        
        segmentedDataSet<-list(training,testing)
        
        segmentedDataSet
        
}
