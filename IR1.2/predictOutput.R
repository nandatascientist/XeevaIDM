
source("classifyItem.R")

predictOutput<-function(testDataSet,trainedModel,sampleSizePerGroup){
      
        # get the number of groups which is same as testing data set length
        testListSize<-length(testDataSet) 
        testBed<-data.frame()
        
        for (z in 1:testListSize){
                
                # extract the query text array for each group 
                listOfItemInGroup<-testDataSet[[z]]
                # get the number of items in current group
                numItemsInGroup<-length(listOfItemInGroup)
                # default number of items to test per group to existing size
                numItemsToSample<-numItemsInGroup
                # if existing size is greater than suggested sample size, override
                if(numItemsInGroup>sampleSizePerGroup){
                        numItemsToSample<-sampleSizePerGroup
                }                        
                
                # get the ramdomized indexes of items to use in testing
                idx<-sample(1:numItemsInGroup,numItemsToSample,replace=F)
                # fill in group names as many times as there are test examples
                itemCategory<-rep(names(testingSet[z]),numItemsToSample)
                #create a data frame with group name + ramdomly picked items
                testBedRows<-data.frame(itemCategory,listOfItemInGroup[idx],stringsAsFactors=FALSE)
                #add to testbed
                testBed<-rbind(testBed,testBedRows)
                
                
                
        }
        
        # compute the number of examples in test bed
        numTestExamples<-nrow(testBed)
        # create an empty column for classification output
        testBed$output<-c(" ")
        colnames(testBed)<-c("group","itemtext","output")
        
        for (ctr in 1:numTestExamples) {
                
                # for each item in the test bed, perform classification using 
                # trained model
                testBed$output[ctr]<-classifyItem(testBed$itemtext[ctr],
                                                  trainedModel)
                
        }
        
        
        ## check if there are groups in the prediction set not in the test set.
        ## and if there are equalize the factor levels between prediction and 
        ## test set by inserting dummy records for the missing levels
        
        groupDiff<-setdiff(testBed$group,testBed$output)
        numMissingGroups<-length(groupDiff)
        
        if(numMissingGroups>0){
                
                for (cleanCtr in 1:numMissingGroups){
                        
                        testBed<-rbind(testBed,
                                           c(groupDiff[cleanCtr],"dummy text",
                                             c(groupDiff[cleanCtr])))        
                }
                
        }
        
        
        print("got here without troubles: test bed is ready" )
        
        # return updated test bed with associated confusion matrix
        return(list(testBed,
                    confusionMatrix(testBed$output,testBed$group)))
        
}
