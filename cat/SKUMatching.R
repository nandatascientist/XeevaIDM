

library(stringr)
library(textreuse)

workingDirectory<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\cat"

# contains SKUs that have to be matched
file1<-"C:\\Users\\koushikk\\repos\\XeevaIDMPrototypes\\cat\\ROW.csv"



setwd(workingDirectory)

# read both files into dataframes
rawdf1 = read.csv(file1,stringsAsFactors=FALSE)

matchResults<-match(rawdf1$PRODUCT_SKU,rawdf1$DECOMM_SKU,nomatch = -1)
idx<-which(matchResults>0)
matchResults[idx]<-1

write.csv(matchResults,"MatchResultsrow.csv")