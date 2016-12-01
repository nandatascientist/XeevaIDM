

getL2Value<-function(inputText){
        
        
        ## initialize Return value
        returnL2Value<-0
        
        ## remove square braces in data
        inputText<-gsub("\\[|\\]","",inputText)
        
        if (nchar(inputText)==8){
                
                if(substr(inputText,3,8)!=c("000000")){
                        
                        returnL2Value<-as.numeric(
                                paste0(substr(inputText,1,4),"0000"))
                }
                
        }
        
        returnL2Value
        
}

getL2ValueVector<-function(inputVector){
 
        l<-length(inputVector)
        returnVector<-rep(0,l)
        
        for (i in 1:l){
                
                returnVector[i]<-getL2Value(inputVector[i])
                
        }
        
        returnVector
}