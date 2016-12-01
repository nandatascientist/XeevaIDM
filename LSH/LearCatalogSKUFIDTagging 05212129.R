
library(stringr)
library(textreuse)

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

## Read the file into a dataframe
rawdf = read.csv(fileLocation,stringsAsFactors=FALSE)
numRec <- nrow(rawdf) # number of rows

rm(workingDirectory,fileLocation)

## Remove unwanted fields from the dataframe
rawdf$Active.Inactive.Status=NULL
rawdf$Createddate=NULL
rawdf$Created.Before.2016=NULL
rawdf$Max..Contract.Fromdate=NULL
rawdf$Max..Contract.Todate=NULL
rawdf$Total.SKU.Usage=NULL
rawdf$Ytd.2015.Spend=NULL

## Replace full text fields with IDs whereever possible

Types<-unique(rawdf$Type.Name)
TypeID<-seq(1,length(Types))
TypeMaster<-cbind.data.frame(TypeID,Types,stringsAsFactors=FALSE)
rawdf$TypeID<-TypeMaster[match(rawdf$Type.Name,TypeMaster$Types),1]
rawdf$Type.Name<-NULL
rm(Types,TypeID)


Categories<-unique(rawdf$Category.Name)
CategoryID<-seq(1,length(Categories))
CategoryMaster<-cbind.data.frame(CategoryID,Categories,stringsAsFactors=FALSE)
rawdf$CategoryID<-CategoryMaster[
        match(rawdf$Category.Name,CategoryMaster$Categories),1]
rawdf$Category.Name<-NULL
rm(Categories,CategoryID)

Regions<-unique(rawdf$Region.Name)
RegionID<-seq(1,length(Regions))
RegionMaster<-cbind.data.frame(RegionID,Regions,stringsAsFactors=FALSE)
rawdf$RegionID<-RegionMaster[match(rawdf$Region.Name,RegionMaster$Regions),1]
rawdf$Region.Name<-NULL
rm(Regions,RegionID)


Countries<-unique(rawdf$Country.Name)
CountryID<-seq(1,length(Countries))
CountryMaster<-cbind.data.frame(CountryID,Countries,stringsAsFactors=FALSE)
rawdf$CountryID<-CountryMaster[match(rawdf$Country.Name,CountryMaster$Countries),1]
rawdf$Country.Name<-NULL
rm(Countries,CountryID)


###################################################################################
######## STEP 1. MPN VALIDATION
###################################################################################

MPN<-unique(rawdf$Product.Mpn)
MPN.Length<-seq(1,length(MPN))
MPNMaster<-cbind.data.frame(MPN,MPN.Length,stringsAsFactors=FALSE)
MPNMaster$MPN.Length<-str_length(MPNMaster$MPN)
MPNMaster$NumCount<-str_count(MPNMaster$MPN,"[0-9]")
MPNMaster$HyphenCount<-str_count(MPNMaster$MPN,"-")
MPNMaster$CharCount<-MPNMaster$MPN.Length-MPNMaster$NumCount-MPNMaster$HyphenCount
MPNMaster$NumCharRatio<-MPNMaster$NumCount/(1+MPNMaster$CharCount)

## use MPN Length and NumChar Ratio based thresholds to determine validity
validMPNidx<-which(MPNMaster$MPN.Length>=mpnlengththreshold & 
                           MPNMaster$NumCharRatio>=mpnnumcharthreshold)

MPNMaster$IsValid<-0
MPNMaster$IsValid[validMPNidx]<-1

## Add MPN validity information back to working dataframe
rawdf$ValidMPN<-MPNMaster[match(rawdf$Product.Mpn,MPNMaster$MPN),7]

rm(MPN,MPN.Length,validMPNidx)
#write.csv(MPNMaster,"MPN.csv")

###################################################################################
######## STEP 2. IDENTIFY DUPES IN SOURCE AND REMOVE
###################################################################################

## Create a key field with SKU + MPN + Hash value
rawdf$DupeErrKey<-paste0(rawdf$Product.Sku,rawdf$Product.Mpn,rawdf$Product.Name)

## Hash the DupeError Key and store back in the dataframe
set.seed(123456789)

rawdf$DupeHash<-NULL

starthash<-proc.time()

for (ctr in 1:numRec){
        
        rawdf$DupeHash[ctr]<-hash_string(rawdf$DupeErrKey[ctr])
        
}

endhash<-proc.time()

hashKey<-unique(rawdf$DupeHash)
numDupeErrKeys<-length(hashKey)
countDEK<-numeric(numDupeErrKeys)


dupedf<-cbind.data.frame(hashKey,countDEK)
rawdf$dupeFlag<-0

startdupecount<-proc.time()
for (ctr in 1:numDupeErrKeys){
        
        dupeIdx<-which(rawdf$DupeHash==dupedf$hashKey)
        dupeCount<-length(dupeIdx)
        
        if(dupeCount>1){
                
                rawdf$dupeFlag[dupeIdx]<-1
                        
        }
        
        dupedf$countDEK[ctr]<-dupeCount
        
}

enddupecount<-proc.time()

