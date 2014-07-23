# use load info to pull in precipitation data from ADAPS and calculate storm info using rainmaker
library(stringr)
library(dataRetrieval)
library(Rainmaker)
siteNo <- "433615088202501"
StartDt <- "2012-07-01"
EndDt <- "2014-07-16"
#adaps_precip_in <- retrieveUnitNWISData(siteNo,'00045',StartDt,EndDt,format="tsv")
adaps_precip_in <- read.table(file="adaps_precip.txt",header=TRUE,sep="\t",stringsAsFactors=FALSE,colClasses=c("character","character","POSIXct","character","numeric","character"))
df <- adaps_precip_in[,c(5,3)]
df <- df[!duplicated(df),]
colnames(df) <- c("rain","pdate")
source("/Users/jlthomps/Desktop/git/GLRIBMPs/RMErosivityIndex.R")
# choose desired dry interval between storms
stormInt <- 1.8
# choose desired rain amount threshold, in units of precip values
rainAmt <- 0
rainmaker_out <- as.data.frame(RMevents(df,ieHr=stormInt,rainthresh=rainAmt,rain="rain",time="pdate")[1])
colnames(rainmaker_out) <- c("stormnum","StartDate","EndDate","rain")
# choose desired intensity values (ie 5 minute, 10 minute, 60 minute)
intens <- c(5,10,15,30,60)
storm_rainmaker <- RMIntense(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",edate="EndDate",depth="rain",xmin=intens)
# calculate Erosivity Index for storm_rainmaker
storm_rainmaker <- RMErosivityIndex(df,storm_rainmaker)
# choose antecedent rain intervals (ie 1 day, 5 days)
arfDays <- c(1,3,5,7)
antecedent_rain <- RMarf(df,date="pdate",rain="rain",rainmaker_out,sdate="StartDate",days=arfDays,varnameout="ARF")
storm_rainmaker <- merge(storm_rainmaker,antecedent_rain,by.x="stormnum",by.y="stormnum")