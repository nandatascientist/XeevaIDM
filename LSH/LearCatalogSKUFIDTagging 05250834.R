
library(stringr)
library(textreuse)

set.seed(123456789)
SKUFID_CTR<-1000000000


mpnlengththreshold<-5 # At least
mpnnumcharthreshold<-0.1 # At least


workingDirectory<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\LSH"
fileLocation<-
"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\LSH\\RawInputCatalogFile.csv"

###################################################################################
######## STEP 0. READING AND PRE-PROCESSING THE INPUT FILE
###################################################################################


setwd(workingDirectory)

## 0.1 Read the file into a dataframe
rawdf = read.csv(fileLocation,stringsAsFactors=FALSE)
numRec <- nrow(rawdf) # number of rows

rm(workingDirectory,fileLocation)

## 0.2 Remove unwanted fields from the dataframe
rawdf$Active.Inactive.Status=NULL
rawdf$Createddate=NULL
rawdf$Created.Before.2016=NULL
rawdf$Max..Contract.Fromdate=NULL
rawdf$Max..Contract.Todate=NULL
rawdf$Total.SKU.Usage=NULL
rawdf$Ytd.2015.Spend=NULL

## 0.3 Replace full text fields with IDs whereever possible

# 0.3.1 Spend Type FIeld
Types<-unique(rawdf$Type.Name)
TypeID<-seq(1,length(Types))
TypeMaster<-cbind.data.frame(TypeID,Types,stringsAsFactors=FALSE)
rawdf$TypeID<-TypeMaster[match(rawdf$Type.Name,TypeMaster$Types),1]
rawdf$Type.Name<-NULL
rm(Types,TypeID)

# 0.3.2 Categories  Field
Categories<-unique(rawdf$Category.Name)
CategoryID<-seq(1,length(Categories))
CategoryMaster<-cbind.data.frame(CategoryID,Categories,stringsAsFactors=FALSE)
rawdf$CategoryID<-CategoryMaster[
        match(rawdf$Category.Name,CategoryMaster$Categories),1]
rawdf$Category.Name<-NULL
rm(Categories,CategoryID)

# 0.3.3 Regions  Field
Regions<-unique(rawdf$Region.Name)
RegionID<-seq(1,length(Regions))
RegionMaster<-cbind.data.frame(RegionID,Regions,stringsAsFactors=FALSE)
rawdf$RegionID<-RegionMaster[match(rawdf$Region.Name,RegionMaster$Regions),1]
rawdf$Region.Name<-NULL
rm(Regions,RegionID)


# 0.3.4 countries Field
Countries<-unique(rawdf$Country.Name)
CountryID<-seq(1,length(Countries))
CountryMaster<-cbind.data.frame(CountryID,Countries,stringsAsFactors=FALSE)
rawdf$CountryID<-CountryMaster[match(rawdf$Country.Name,CountryMaster$Countries),1]
rawdf$Country.Name<-NULL
rm(Countries,CountryID)


###################################################################################
######## STEP 1. MPN VALIDATION
###################################################################################

## 1.1 Extract  unique MPNs and their lengths into a seperate dataframe 
MPN<-unique(rawdf$Product.Mpn)
MPN.Length<-seq(1,length(MPN))
MPNMaster<-cbind.data.frame(MPN,MPN.Length,stringsAsFactors=FALSE)
MPNMaster$MPN.Length<-str_length(MPNMaster$MPN)

## 1.2 Add count  of numerals, hyphens and characters for each MPN into data frame
MPNMaster$NumCount<-str_count(MPNMaster$MPN,"[0-9]")
MPNMaster$HyphenCount<-str_count(MPNMaster$MPN,"-")
MPNMaster$CharCount<-MPNMaster$MPN.Length-MPNMaster$NumCount-MPNMaster$HyphenCount
MPNMaster$NumCharRatio<-MPNMaster$NumCount/(1+MPNMaster$CharCount)

## 1.3 Validate MPN using its length and NumChar ratio and flag valid MPNs
validMPNidx<-which(MPNMaster$MPN.Length>=mpnlengththreshold & 
                           MPNMaster$NumCharRatio>=mpnnumcharthreshold)

MPNMaster$IsValid<-0
MPNMaster$IsValid[validMPNidx]<-1

## 1.4 Add MPN validity information back to working master dataframe
rawdf$ValidMPN<-MPNMaster[match(rawdf$Product.Mpn,MPNMaster$MPN),7]

## 1.5 Clean up
rm(MPN,MPN.Length,validMPNidx)
#write.csv(MPNMaster,"MPN.csv")

###################################################################################
######## STEP 2. IDENTIFY DUPES IN SOURCE AND REMOVE
###################################################################################


## 2.1 Extract unique SKUs and frequency of occurence into a seperate dataframe
SKUMaster<-as.data.frame(table(rawdf$Product.Sku),stringsAsFactors=FALSE)

## 2.2 Flag SKUs occuring more than once in data and add dupe resolution marker
dupeSKUIdx<-which(SKUMaster$Freq>1)
numDupes<-length(dupeSKUIdx)

## 2.2.1 Add Dupe Flag
SKUMaster$dupeFlag<-0
SKUMaster$dupeFlag[dupeSKUIdx]<-1

# 2.2.2 Add flag to track if SKU dupes have been resolved
SKUMaster$dupeResolved<-0 


## 2.3 Add  product name length  to original data frame to help with dedupe
rawdf$ProductNameLength<-str_length(rawdf$Product.Name)

## 2.4 Add dupeFlag to original data frame to help with dedupe
rawdf$dupeFlag<-0

## 2.5 For each dupeSKU, retrieve the corresponding product names from 
##     original data frame and check for character length equality.
##     If name lengths are same, assume they are identical

for (ctr in 1:numDupes){
        
        # Retrieve the ctr-th index from dupeSKUIdx and get corresponding SKU
        dupeSKU<-SKUMaster$Var1[dupeSKUIdx[ctr]]
        
        # Get the indices of dupeSKU's occurence in original dataframe
        rawdfidx<-which(rawdf$Product.Sku==dupeSKU)
        
        # Mark the retrieved indices as dupes in original dataframe
        rawdf$dupeFlag[rawdfidx]<-1
        
        # Get  lengths of product names for these SKUs from original dataframe
        lengths<-rawdf$ProductNameLength[rawdfidx]
        
        # Check if all elements of the lengths vector are equal
        if(diff(range(lengths)) < .Machine$double.eps ^ 0.5){
                
                # If so retain the first one in original df by marking it
                rawdf$dupeFlag[rawdfidx[1]]<-2
                
                # Mark the dupeSKU tracking flag as processed
                SKUMaster$dupeResolved[dupeSKUIdx[ctr]]<-1
        }
        
}


## 2.6 Check for dupeSKUs that have not been resolved

if(length(which(SKUMaster$dupeFlag==1 & SKUMaster$dupeResolved==0))>0){
        
        break
        ## need some error handling in case we have Same SKU 
        ## with different descriptions in the source data
        
}

## 2.7 Write out an error file with details of the dupeSKUs
rawdfDupeIdx<-which(rawdf$dupeFlag>0)
dupeDetails<-rawdf[rawdfDupeIdx,c(1,2,17)]
write.csv(dupeDetails,"DuplicateSKUs.csv")

## 2.9 Move forward with deduped records

# 2.9.1 get locations of dupes that we dont want to retain
rawdfDupeIdx<-which(rawdf$dupeFlag==1)

#2.9.2 Subset dataframe to records of interest
df<-rawdf[-rawdfDupeIdx,]

## 2.10 Clean up
rm(dupeSKU,dupeSKUIdx,lengths,rawdfidx,rawdfDupeIdx,rawdf)