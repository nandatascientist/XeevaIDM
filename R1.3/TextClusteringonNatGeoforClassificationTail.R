
##################################################################################
##### 1 - INITIALIZE SCRIPT PARAMETERS
##################################################################################

## Load the needed libraries and functions
library(gdata)
library(tm)

## Set script parameters
set.seed(12345) # for repeatability of random numbers 
#datafileName<-"c:\\idm\\Step4ReducedNatGeoFileAP1.xlsx" # source file
datafileName<-"c:\\idm\\Step4ReducedNatGeoFileAPminusModelAandB1.xlsx" # source file
outputFile<-"c:\\idm\\term.csv"

##################################################################################
##### 2 - READ DATA FILE INTO R AND PERFORM DATA PREPROCESSING
##################################################################################

## Read file into R
df<-read.xls(datafileName,sheet=1,header=TRUE)
numSuppliers<-length(unique(df$Supplier.ID))

## Create blank dataframe
supplier<-sort(unique(df$Supplier.ID),decreasing=FALSE) # supplierIDs in asc order
description<-c(rep("",numSuppliers))
procdf<-data.frame(supplier,description)
procdf$description<-as.character(procdf$description)

## populate dataframe with text from each supplier from original file
## Loop below operates once for each unique supplier referenced in both
## supplier and procdf variables

for (ctr in 1:numSuppliers){
        
        # Loop is running for supplier referenced by supplier[ctr] 
        
        # Retrieve rows from original dataframe that have 
        # suppliers matching with supplier[ctr]
        idx<-which(df$Supplier.ID==supplier[ctr])
        
        # how many rows have this supplier?
        l<-length(idx)
        
        # initialize working variable
        workingText<-c("") 
        
        # Loop through the matching records and concatenate 
        # text of interest into workingText variable
        
        for (j in 1:l){
                
                # get row reference
                rowidx<-idx[j]
                
                # retrieve and concatenate text
                workingText<-paste0(workingText,df[rowidx,4])
        }
        
        # perform stemming on the text
        workingText<-wordStem(workingText,language='porter')
        
        # remove duplicate words from the text post stemming and assign 
        # back into procdf 
        procdf[ctr,2]<-
                vapply(lapply(strsplit(workingText, " "), unique),
                       paste, character(1L), collapse = " ")
}


##################################################################################
##### 3 - PREPARE CORPUS FOR PROCESSING
##################################################################################

## Create corpus object and remove english stop words 
corpus<-Corpus(VectorSource(procdf$description))
cleancorpus<-tm_map(corpus,removeWords,stopwords("english"))
cleancorpus<-tm_map(cleancorpus,removeNumbers)
cleancorpus<-tm_map(cleancorpus,removePunctuation)


##################################################################################
##### 4 - GET TO TERMS OF INTEREST AND CORRESPONDING SUPPLIERS
##################################################################################

tdm<-TermDocumentMatrix(cleancorpus)
workingm<-as.matrix(tdm)
v <- sort(rowSums(workingm),decreasing=TRUE)
termsFreqdf <- data.frame(word = names(v),freq=v)
terms<-as.character(tdm[[6]][1])
termofInterest<-c("digital")
container<-subset(workingm,rownames(workingm)==termofInterest)
docsWithTerm<-which(container>0)
qualifyingSuppliers<-supplier[docsWithTerm]

#write.csv(qualifyingSuppliers,outputFile)
write.csv(procdf,"c:\\idm\\procdf.csv")