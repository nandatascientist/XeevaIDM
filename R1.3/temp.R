
testinput<-"[12345678]"
returnL3Value<-0


inputText<-gsub("\\[|\\]","",testinput)

if (nchar(inputText)==8){
        
        if(substr(inputText,5,8)!=c("0000")){
                
                returnL3Value<-as.numeric(
                        paste0(substr(inputText,1,4),"0000"))
        }
        
}