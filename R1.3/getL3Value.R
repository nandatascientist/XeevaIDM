

getL3Value<-function(inputText){
        
        
        ## initialize Return value
        returnL3Value<-0
        
        ## remove square braces in data
        inputText<-gsub("\\[|\\]","",inputText)
        
        if (nchar(inputText)==8){
                
                if(substr(inputText,5,8)!=c("0000")){
                        
                        returnL3Value<-as.numeric(
                                paste0(substr(inputText,1,6),"00"))
                }
                
        }
        
        returnL3Value
        
}

getL3ValueVector<-function(inputVector){
 
        l<-length(inputVector)
        returnVector<-rep(0,l)
        
        for (i in 1:l){
                
                returnVector[i]<-getL3Value(inputVector[i])
                
        }
        
        returnVector
}