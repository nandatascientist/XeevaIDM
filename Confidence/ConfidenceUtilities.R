library(ROCR)

getBestThreshold<-function(predictionValues,labels){
        
        
        ## create predictions object
        pred<-prediction(predictionValues,labels)
        
        ## create a performance object for F1 score
        f1score<-performance(pred,"f") 
        
        cutoffs <- data.frame(cutoff=f1score@x.values[[1]], 
                              score=f1score@y.values[[1]])
        
        cutoffs[which.max(cutoffs$score),1]
        
        
}

fitInZeroOneScale<-function(vectorvalues,providedRange=0,providedMin=0){
        
        ## Scale and center the variables 
        if(providedRange==0){
                
                range<-max(vectorvalues)-min(vectorvalues)        
        } else {
                
                range<-providedRange
        }
        
        if(providedMin==0){
        
                minVal<-min(vectorvalues)
                
        }else {
                
                minVal<-providedMin
        }
        
        t<-vectorvalues-minVal
        output<-t/range       
        
        returnVal<-list(output,range,minVal)
        returnVal
}