
preprocessFileData<-function(fileName,level,minimumRows,rowsToRead){
        
        
        ## Read datafile into R memory as a R dataframe and name columns
        rawData<-read.csv(fileName,nrows=rowsToRead,stringsAsFactors=FALSE)
        
        names(rawData)<-c("cat.code","cat.subcat","subcat.code",
                          "item.name","item.mpn","mfg.name")
        
        ## Change all classification codes into factors
        rawData$cat.code<-as.factor(rawData$cat.code)
        rawData$subcat.code<-as.factor(rawData$subcat.code)
        
        ## clean the dataframe of Columns & Rows that we are not going use
        rawData$cat.subcat<-NULL
        
        # concatenate useful item related text into a large text fiele
        rawData$text<-paste(rawData$item.name,
                            rawData$item.mpn,
                            rawData$mfg.name, sep=" ")

        # get rid of the fields now that we have it in the text column
        rawData$item.name<-NULL
        rawData$item.mpn<-NULL
        rawData$mfg.name<-NULL
        
        if(level==1){
                
                #dirtyRows<-which(rawData$segment=="#VALUE!")
                #rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$cat.code,drop=TRUE)                        
                
                
        }else if(level==2){
                
                #dirtyRows<-which(rawData$family=="#VALUE!")
                #rawData<-rawData[-dirtyRows,]
                resultList<-split(rawData$text,rawData$subcat.code,drop=TRUE)                        
                
        }

        # get number of members within each item group
        counts<-sapply(resultList,length)
        
        # which groups have at least minRows members
        idx<-which(counts>=minimumRows)
        resultList<-resultList[idx]
        
        resultList
        
}