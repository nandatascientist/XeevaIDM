
library(textreuse)

fileLocation<-"C:\\Users\\koushikk\\repos\\XeevaIDM\\LSH\\InputUniqueSKUMPN.csv"
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
rawdf$SKUFID<-NULL

## Add word count and Hash Value parameters to the dataframe
for (ctr in 1:numRec){
        
        rawdf$Word.Count[ctr]<-wordcount(rawdf$Product.Name[ctr])
        rawdf$Hash.Value[ctr]<-hash_string(rawdf$Product.Name[ctr])
        
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


## Create a Numeric array with Uniques + their Occurences
exactDupeTracker<-cbind(Unique.Hashes,Ocurrence)

## Get indexes of hashes that occur more than once from the above numeric array
dupeIdx<-which(exactDupeTracker[,2]>1)
## Quantify the actual number of exact matches
numExactDupes<-length(dupeIdx)

## From the numeric array exactDupeTracker, retrieve each hash value that
## occurs more than once. Then look up the original dataframe to get all the rows
## where a given dupeHash occurs.
## Tag all these rows with a common SKUFID number

for (ctr in 1:numExactDupes){
        
        dupeHash<-exactDupeTracker[dupeIdx[ctr],1]
        dupeHashIdx<-which(rawdf$Hash.Value==dupeHash)
        rawdf[dupeHashIdx,23]<-SKUFID_CTR
        SKUFID_CTR<-SKUFID_CTR + 1
}


## Write out the file with the tag
write.csv(rawdf,"output.csv")


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