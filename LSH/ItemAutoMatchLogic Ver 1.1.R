
library(stringr)
library(textreuse)

workingDirectory<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\LSH"
fileLocation<-
        "C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\LSH\\InputUniqueSKUMPN.csv"
SKUFID_CTR<-1000000000

###################################################################################
######## PRE-REQUISITE
###################################################################################

## PP1 Key field is created in Catalog file by joining MPN + Price
## PP2 All MPN + Price Keys that have > 1 records associated as well as 
##    those MPNs with lengths >= MPN_LEN_THRESHOLD are considered for analysis 
## PP3 The records where Product_SKU is not unique are also discarded from analysis
## PP4 Commas are replaced by empty space in above to get input file 

###################################################################################
######## STEP 0. READING AND PRE-PROCESSING THE INPUT FILE
###################################################################################

## Set working directory
setwd(workingDirectory)

## Read the file into a dataframe
rawdf = read.csv(fileLocation,stringsAsFactors=FALSE)
numRec <- nrow(rawdf) # number of rows

## Remove unwanted fields from the dataframe
rawdf$Active.Inactive.Status=NULL
rawdf$Createddate=NULL
rawdf$Created.Before.2016=NULL
rawdf$Max..Contract.Fromdate=NULL
rawdf$Max..Contract.Todate=NULL

## Initialize new fields for use
rawdf$Word.Count<-NULL
rawdf$Hash.Value<-NULL
rawdf$SKUFID<-0

## Add word count and Hash Value parameters to the dataframe
for (ctr in 1:numRec){
        
        rawdf$Word.Count[ctr]<-wordcount(rawdf$Product.Name[ctr])
        rawdf$Hash.Value[ctr]<-hash_string(rawdf$Product.Name[ctr])
        
}

## Create a dataframe that contains the unique MPN+Price Key clusters

## Simplify Keys to cluster IDs
keyClusters<-unique(rawdf$MPN_PRICE)
numClusters<-length(keyClusters)
clusterID<-seq(1,numClusters)
clusterMaster<-cbind.data.frame(clusterID,keyClusters,stringsAsFactors=FALSE)

## Add cluster IDs back to rawdf
rawdf$CID<-clusterMaster[match(rawdf$MPN_PRICE,clusterMaster$keyClusters),1]

## Add cluster size to ClusterMaster
clusterMaster$size<-0

for(ctr in 1:numClusters){
        
        clusterMaster$size[ctr]<-length(
                which(rawdf$CID==clusterMaster$clusterID[ctr]))
        
}


###################################################################################
######## STEP 1. TAGGING EXACT MATCH DESCRIPTIONS MATCHING MPN+ITEM
###################################################################################

## Retrieve the unique hashes and the count of their occurence
Unique.Hashes<-unique(rawdf$Hash.Value)
numHash<-length(Unique.Hashes)
Ocurrence<-numeric(numHash)

for (ctr in 1:numHash){
        
        Ocurrence[ctr]<-sum(rawdf$Hash.Value==Unique.Hashes[ctr])
        
}


## Create a dataframe with Unique Hashes  + # of their Occurence in rawdf
hashMaster<-cbind.data.frame(Unique.Hashes,Ocurrence)

## Get indexes of hashes that occur more than once from the above numeric array
exactDupeIdx<-which(hashMaster$Ocurrence>1)
## Quantify the actual number of exact matches
numExactDupes<-length(exactDupeIdx)

## From the dataframe hashMaster, retrieve each hash value that
## occurs more than once. Then look up the original dataframe to get all the rows
## where a given dupeHash occurs.
## Tag all these rows with a common SKUFID number

for (ctr in 1:numExactDupes){
        
        dupeHash<-hashMaster$Unique.Hashes[exactDupeIdx[ctr]]
        sourceDupeHashIdx<-which(rawdf$Hash.Value==dupeHash)
        rawdf$SKUFID[sourceDupeHashIdx]<-SKUFID_CTR
        SKUFID_CTR<-SKUFID_CTR + 1
}

## Write out the file with the tag
write.csv(rawdf,"output.csv")


## Add count of records tagged per cluster  to ClusterMaster
clusterMaster$tagged<-0

for(ctr in 1:numClusters){
        clusterMaster$tagged[ctr]<-
                length(which(rawdf$CID==clusterMaster$clusterID[ctr]&&
                              rawdf$SKUFID!=0))
}







###################################################################################
######## STEP 2. TAGGING SIMILAR DESCRIPTIONS WITH MATCHING MPN+ITEM
###################################################################################


###################################################################################
######## STEP 3. TAGGING SIMILAR DESCRIPTIONS 
###################################################################################

rmidx<-which(rawdf$Word.Count<minWords)

df<-rawdf[-rmidx,]


corpus<-TextReuseCorpus(text=df$Product.Name,
                        meta= list("id"= df$Product.Sku),
                        tokenizer = tokenize_ngrams, n = 2,
                        minhash_func = minhash_generator())

result<-lsh(corpus,40,progress = interactive())






# Comparing hash values does not seem to help
# rawdf$Hash<-hash_string(rawdf$Product.Name)

#test<-TextReuseTextDocument(rawdf[1,2],meta = list("id" = rawdf[1,1]))


var<-"This is a 1234 test that 56 are using in MPN-2343-45"
MPNLength<-str_length(var)
countNum<-str_count(var,"[0-9]")
countHyphen<-str_count(var,"-")
countChar<-MPNLength - countNum - countHyphen



num_loc<-str_locate_all(var,"[0-9]+")
