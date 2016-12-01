
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


## 2.1 Hash Product names and add into the original dataframe

rawdf$Hash.Value<-NULL

for (ctr in 1:numRec){
        
        rawdf$Hash.Value[ctr]<-hash_string(rawdf$Product.Name[ctr])
        
}


## 2.2 Create a Key with SKU + Hash and count  occurences in orginal dataframe
##      A unique SKU+Hash Key indicates unique SKU+Description combination

# 2.2.1 Create the Key
rawdf$SKU.Hash.Key<-paste0(rawdf$Product.Sku,"|",rawdf$Hash.Value)

# 2.2.2 Summarize frequency of occurence of each key
SKUHashKeys<-as.data.frame(table(rawdf$SKU.Hash.Key),
                           stringsAsFactors=FALSE)

## 2.3 Check for occurence of Dupe SKUs with different descriptions

if(length(unique(rawdf$Product.Sku))!=nrow(SKUHashKeys)){
        
        # We are checking if number of unique SKUs is the same as number of 
        # unique SKU Description combinations. This will only hold true if
        # one SKU has a single description regardelss of how often it repeats
        
        break
        
        ## need some error handling in case we have Same SKU repeating
        ## with different descriptions in the source data
        
        
}


## 2.4 Extract keys that have more than one occurence as well we their count

# 2.4.1 Get location of dupeKeys
dupeKeyIdx<-which(SKUHashKeys$Freq>1)

# 2.4.2 Get number of dupeKeys
numDupes<-length(dupeKeyIdx) 

## 2.5 Add dupeFlag to original data frame to help with dedupe
rawdf$dupeFlag<-0


## 2.6 For each dupeSKU, retrieve the corresponding product names from 
##     original data frame and check for character length equality.
##     If name lengths are same, assume they are identical

for (ctr in 1:numDupes){
        
        # Retrieve the ctr-th index from dupeKeyIdx and get corresponding Key
        dupeKey<-SKUHashKeys$Var1[dupeKeyIdx[ctr]]
        
        # Get the indices of dupeKey's occurence in original dataframe
        rawdfidx<-which(rawdf$SKU.Hash.Key==dupeKey)
        
        # Mark the retrieved indices as dupes in original dataframe
        rawdf$dupeFlag[rawdfidx]<-1
        
        # Retain the first one in original dataframe by marking it differently
        rawdf$dupeFlag[rawdfidx[1]]<-2
        
        
}


## 2.7 Write out an error file with details of the dupeSKUs
rawdfDupeIdx<-which(rawdf$dupeFlag>0)
dupeDetails<-rawdf[rawdfDupeIdx,c(1,2,18)]
write.csv(dupeDetails,"DuplicateSKUs.csv")

## 2.9 Move forward with deduped records

# 2.9.1 get locations of dupes that we dont want to retain
rawdfDupeIdx<-which(rawdf$dupeFlag==1)

#2.9.2 Subset dataframe to records of interest
df<-rawdf[-rawdfDupeIdx,]
numRec<-nrow(df)

## 2.10 Clean up
rm(dupeDetails,rawdf,SKUHashKeys,dupeKey,dupeKeyIdx,
   numDupes,rawdfDupeIdx,rawdfDupeIdx)

df$SKU.Hash.Key<-NULL
df$dupeFlag<-NULL

