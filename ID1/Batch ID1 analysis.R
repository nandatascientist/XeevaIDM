
library(ggplot2)

minRows<-700
fileLocation<-"C:\\Users\\koushikk\\repos\\XeevaIDM\\ID1Batch1results.csv"

# Read file
runResults<-read.csv(fileLocation)

# Process records above threshold of representation
idx<-which(runResults$Univrep>=minRows)
subsetResults<-runResults[idx,]


qplot(Univrep,Accuracy,
      data=subsetResults,geom="smooth",
      method="loess",main="Accuracy Vs Universe Representation")

qplot(Univrep,Accuracy,
      data=subsetResults,,geom="smooth",
      method="lm",main="Accuracy Vs Universe Representation")

#qplot(Num.Records,Accuracy,data=runResults,color=Level,geom="smooth",method="lm",main="Straight Line Classifier Characteristic")

hist(runResults$Univrep)