
## Start the script execution clock
ptm<-proc.time()

##################################################################################
##### 1 - INITIALIZE SCRIPT PARAMETERS
##################################################################################

## Load the needed libraries and functions
library(reshape)

source("getL3Value.R")

## Set script parameters
set.seed(12345) # for repeatability of random numbers 
#datafileName<-"c:\\idm\\Step4ReducedNatGeoFileAP1.xlsx" # source file
datafileName<-"c:\\idm\\ModelB\\ModelBaggregationInputNonPOUNSPSC2.csv" # source file
outputfileName<-"c:\\idm\\ModelB\\ModelBOutput2.csv"

##################################################################################
##### 2 - READ DATA FILE INTO R AND PERFORM DATA PREPROCESSING
##################################################################################

## Read file into R
df<-read.csv(datafileName,header=TRUE,stringsAsFactors=FALSE)

## convert the matches into Level 3 UNSPSC Values
df$R1M1<-getL3ValueVector(df$R1M1)
df$R1M2<-getL3ValueVector(df$R1M2)
df$R1M3<-getL3ValueVector(df$R1M3)

df$R2M1<-getL3ValueVector(df$R2M1)
df$R2M2<-getL3ValueVector(df$R2M2)
df$R2M3<-getL3ValueVector(df$R2M3)

df$R3M1<-getL3ValueVector(df$R3M1)
df$R3M2<-getL3ValueVector(df$R3M2)
df$R3M3<-getL3ValueVector(df$R3M3)


## Melt dataframe by all numeric values
meltdf<-melt(df,id=c(1))

## Create a key value as combo of RecordId and L3Class as a factor
meltdf$key<-as.factor(paste(meltdf$RecordID,meltdf$value,sep="|"))

## count values by factor level
outputTable<-table(meltdf$key)


## write into csv file
write.csv(outputTable,file=outputfileName)

## Stop  the script execution clock
proc.time()-ptm