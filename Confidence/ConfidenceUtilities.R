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

fitInZeroOneScale<-function(vectorvalues){
        
        ## Scale and center the variables 
        range<-max(vectorvalues)-min(vectorvalues)
        minval<-min(vectorvalues)
        t<-vectorvalues-minval
        output<-t/range       
        
        output
}