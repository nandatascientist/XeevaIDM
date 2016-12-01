
library(textreuse)
library(gdata)

fileLocation<-"C:\\Users\\koushikk\\repos\\XeevaIDM\\LSH\\ActiveSKUDescInput.csv"
minWords<-3

###################################################################################
######## READING AND PRE-PROCESSING THE INPUT FILE
###################################################################################

## Read the file into a dataframe
rawdf = read.csv(fileLocation,stringsAsFactors=FALSE)
numRec <- nrow(rawdf)
rawdf$Word.Count<-NULL
rawdf$Hash.Value<-NULL

## Add word count and Hash Value parameters to the dataframe
for (ctr in 1:numRec){
        
        rawdf$Word.Count[ctr]<-wordcount(rawdf$Product.Name[ctr])
        rawdf$Hash.Value[ctr]<-hash_string(rawdf$Product.Name[ctr])
        
}




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