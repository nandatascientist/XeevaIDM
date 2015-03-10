
preprocessFileData<-function(fileName,level,minimumRows,rowsToRead){
        
        ## assuming level=1
        
        ## Read datafile into R memory as a R dataframe and name columns
        rawData<-read.csv(fileName,nrows=rowsToRead,stringsAsFactors=FALSE)
        
        names(rawData)<-c("productname","productid","specifications",
                          "features","description","image","URL",
                          "commodity","class","family","segment")
        
        ## Change all classification codes into factors
        rawData$segment<-as.factor(rawData$segment)
        rawData$family<-as.factor(rawData$family)
        rawData$class<-as.factor(rawData$class)
        rawData$commodity<-as.factor(rawData$commodity)
        
        ## clean the dataframe of Columns & Rows that we are not going use
        rawData$productid<-NULL
        rawData$specifications<-NULL
        rawData$image<-NULL
        rawData$URL<-NULL

        # concatenate useful item related text into a large text fiele
        rawData$text<-paste(rawData$productname,
                            rawData$features,
                            rawData$description, sep=" ")

        # get rid of the fields now that we have it in the text column
        rawData$productname<-NULL
        rawData$features<-NULL
        rawData$description<-NULL
        
        if(level==1){
                
                dirtyRows<-which(rawData$segment=="#VALUE!")
                rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$segment,drop=TRUE)                        
                
                
        }else if(level==2){
                
                dirtyRows<-which(rawData$family=="#VALUE!")
                rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$family,drop=TRUE)                        
                
        }else if(level==3){
                
                dirtyRows<-which(rawData$class=="#VALUE!")
                rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$class,drop=TRUE)                        
                
        }else if(level==4){

                dirtyRows<-which(rawData$commodity=="#VALUE!")
                rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$commodity,drop=TRUE)                        
                
                
        }

        counts<-sapply(resultList,length)
        idx<-which(counts>minimumRows)
        resultList<-resultList[idx]
        
        resultList
        
}