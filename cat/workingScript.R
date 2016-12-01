
library(stringr)
library(textreuse)

workingDirectory<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\cat"

# contains all SKU from inactive sheet from Todd
file1<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\cat\\ALLSKUD.csv"

# contains all SKUs that were identified for Decomm
file2<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\cat\\DecommSKUList1.csv"


setwd(workingDirectory)

# read both files into dataframes
rawdf1 = read.csv(file1)
rawdf2<-read.csv(file2,stringsAsFactors=FALSE)

# Count frequency of occurence of a SKU in original Todd file
freqSKU<-as.data.frame(table(rawdf1$ALLSKU),
                           stringsAsFactors=FALSE)

# Get index of SKUs that occur more than once
multiSKUIdx<-which(freqSKU$Freq>1)
# Get list of such SKUs
multiSKU<-freqSKU$Var1[multiSKUIdx]

# How many multi-times occuring SKus figure in the decomm list?
a<-intersect(multiSKU,rawdf2$Product.Sku) 

length(a)

#write.csv(freqSKU,"ALLSKUcounts.csv")


################################################################################

#rawdf3<-merge(rawdf1[,c("Product.Sku","Count_Loc")],
#              rawdf2[,c("Product.Sku","CountOfProduct.Sku")])

#decommSKU<-unique(rawdf1$Product.Sku)
#activeSKU<-unique(rawdf2$ActiveSKU)

#a<-intersect(rawdf1$Product.Sku,rawdf2$DecommSKU)
#a<-intersect(rawdf$DecommSKU,rawdf$DecommSKU)

#t<-match(rawdf$ALL.REC,rawdf$UNQSKU,nomatch=-1)
