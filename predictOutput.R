
source("classifyItem.R")

predictOutput<-function(testDataSet,trainedModel){
        
        testListSize<-length(testDataSet)
        testBed<-data.frame()
        
        for (z in 1:testListSize){
                
                listItem<-testDataSet[[z]]
                listItemLength<-length(listItem)
                itemCategory<-rep(names(testingSet[z]),listItemLength)
                testBedRows<-data.frame(itemCategory,listItem,stringsAsFactors=FALSE)
                testBed<-rbind(testBed,testBedRows)
                
        }
        
        numTestExamples<-nrow(testBed)
        
        testBed$output<-c(" ")
        
        for (ctr in 1:numTestExamples) {
                
                testBed$output[ctr]<-classifyItem(testBed$listItem[ctr],
                                                  trainedModel)
                
        }
        
        return(list(testBed,
                    confusionMatrix(testBed$output,testBed$itemCategory)))
        
}
