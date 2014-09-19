library(googleVis)
source("C:/Users/jlthomps/Desktop/git/RainmakerJLT/R/RMeventsBuckets.R")
source("C:/Users/jlthomps/Desktop/git/RainmakerJLT/R/RMIntenseBuckets.R")

swaletop <- read.table(file="swaletopuv.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swaletop$pdate <- strptime(as.character(paste(swaletop$DATE,swaletop$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swaletop$tips <- as.numeric(swaletop$VALUE)

swaletopStorms <- RMeventsBuckets(swaletop,ieHr=6,tips="tips",time="pdate",x2Coef=-.8577,xCoef=12.517,bCoef=450.11)
swaletopIntense <- RMIntenseBuckets(swaletop,date="pdate",tips = "tips",swaletopStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.8577,xCoef=12.517,bCoef=450.11)

swalemid <- read.table(file="swalemiduv.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swalemid$pdate <- strptime(as.character(paste(swalemid$DATE,swalemid$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swalemid$tips <- as.numeric(swalemid$VALUE)

swalemidStorms <- RMeventsBuckets(swalemid,ieHr=6,tips="tips",time="pdate",x2Coef=-.8577,xCoef=12.517,bCoef=450.11)
swalemidIntense <- RMIntenseBuckets(swalemid,date="pdate",tips = "tips",swalemidStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.8577,xCoef=12.517,bCoef=450.11)

swalebot <- read.table(file="swalebotuv.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swalebot$pdate <- strptime(as.character(paste(swalebot$DATE,swalebot$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swalebot$tips <- as.numeric(swalebot$VALUE)

swalebotStorms <- RMeventsBuckets(swalebot,ieHr=6,tips="tips",time="pdate",x2Coef=-.0012,xCoef=.2851,bCoef=53.002)
swalebotIntense <- RMIntenseBuckets(swalebot,date="pdate",tips = "tips",swalebotStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.0012,xCoef=.2851,bCoef=53.002)

swaletopSmall <- swaletop[swaletop$pdate>=strptime("20140601",format="%Y%m%d"),]
swalemidSmall <- swalemid[swalemid$pdate>=strptime("20140601",format="%Y%m%d"),]
swalebotSmall <- swalebot[swalebot$pdate>=strptime("20140601",format="%Y%m%d"),]

fileToSave <- "swalebotIntense.csv"
write.table(swalebotIntense, fileToSave, row.names=FALSE, sep=",")
fileToSave <- "swalemidIntense.csv"
write.table(swalemidIntense, fileToSave, row.names=FALSE, sep=",")
fileToSave <- "swaletopIntense.csv"
write.table(swaletopIntense, fileToSave, row.names=FALSE, sep=",")


events <- unique(rbind(swaletopStorms[,c(2:3)],swalebotStorms[,c(2:3)],swalemidStorms[,c(2:3)]))
top_data <- data.frame(swaletopSmall[,c("pdate","tips")],rep("Top",nrow(swaletopSmall)),rep(NA,nrow(swaletopSmall)),stringsAsFactors=FALSE)
names(top_data) <- c("datetime","value","name","label")
mid_data <- data.frame(swalemidSmall[,c("pdate","tips")],rep("Mid",nrow(swalemidSmall)),rep(NA,nrow(swalemidSmall)),stringsAsFactors=FALSE)
names(mid_data) <- c("datetime","value","name","label")
bot_data <- data.frame(swalebotSmall[,c("pdate","tips")],rep("Bottom",nrow(swalebotSmall)),rep(NA,nrow(swalebotSmall)),stringsAsFactors=FALSE)
names(bot_data) <- c("datetime","value","name","label")
event_data <- data.frame(events[,c("StartDate")],rep(100,nrow(events)),rep("Event",nrow(events)),rep(paste(events$StartDate,events$EndDate,sep=" "),nrow(events)),stringsAsFactors=FALSE)
names(event_data) <- c("datetime","value","name","label")
plot_data <- rbind(top_data,mid_data,bot_data,event_data)
hydrographInteractive <- gvisAnnotatedTimeLine(plot_data,datevar="datetime",numvar="value",idvar="name",annotationvar="label",options=list(colors="['blue','green','red','white']",displayAnnotations=TRUE,legendPosition="newRow",scaleColumns="[0,2]",scaleType='allfixed',width="1300px", height="700px",thickness="[2,2,2,.5]"))
plot(hydrographInteractive)

siteNo <- "WDOT Grass Swale"
dateInt <- 24
startDt <- strftime(min(plot_data$datetime),format="%Y.%m.%d")
endDt <- strftime(max(plot_data$datetime), format="%Y.%m.%d")
ytop <- max(plot_data[plot_data$name %in% c("Top","Mid"),]$value)
pdf(paste(siteNo,".",startDt,".",endDt,"hydrograph.pdf",sep=""),width=10,height=8)
par(mar=c(8,4,5,4),xpd=T)
plot(top_data$datetime,top_data$value,xaxt="n",xlab="",ylim=c(0,ytop),ylab="Tips - Top and Middle",col="red",type="l",main=paste(siteNo,startDt,"-",endDt,sep=" "))
lines(mid_data$datetime,mid_data$value,xlab="",ylab="",col="blue",type="l")
a<-seq(min(plot_data$datetime),max(plot_data$datetime),dateInt*3600)
axis.POSIXct(1,at=a,format="%m/%d %H:%M",las=2)
mtext("Datetime",side=1,line=6)
par(new=T)
plot(bot_data$datetime,bot_data$value,axes=F,xlab="",ylab="",col="green",type="l")
points(event_data$datetime,event_data$value,xlab="",ylab="",col="purple",pch="o")
axis(side=4)
mtext("Tips - Bottom",side=4,line=2)
legend("topleft",c("Tips - Bottom","Tips - Middle","Tips - Top","Events"),lty=c(1,1,1,NA),lwd=c(2.5,2.5,2.5),pch=c(NA,NA,NA,1),col=c("green","red","blue","purple"))
dev.off()
