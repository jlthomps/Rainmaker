library(googleVis)
source("/Users/jlthomps/Desktop/git/RainmakerJLT/R/RMeventsBuckets.R")
source("/Users/jlthomps/Desktop/git/RainmakerJLT/R/RMeventsBucketsln.R")

#source("C:/Users/jlthomps/Desktop/git/RainmakerJLT/R/RMIntenseBuckets.R")
setwd("/Users/jlthomps/Desktop/git/RainmakerJLT")
swaletop <- read.table(file="swaletopuv2.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swaletop$pdate <- strptime(as.character(paste(swaletop$DATE,swaletop$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swaletop$tips <- as.numeric(swaletop$VALUE)

swaletopStorms <- RMeventsBuckets(swaletop,ieHr=6,tips="tips",time="pdate",x2Coef=-.8577,xCoef=12.517,bCoef=450.11)
swaletopStormsln <- RMeventsBucketsln(swaletop,ieHr=6,tips="tips",time="pdate",x2Coef=0,xCoef=20.415,bCoef=467.36)
#swaletopIntense <- RMIntenseBuckets(swaletop,date="pdate",tips = "tips",swaletopStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.8577,xCoef=12.517,bCoef=450.11)

swalemid <- read.table(file="swalemiduv2.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swalemid$pdate <- strptime(as.character(paste(swalemid$DATE,swalemid$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swalemid$tips <- as.numeric(swalemid$VALUE)

swalemidStorms <- RMeventsBuckets(swalemid,ieHr=6,tips="tips",time="pdate",x2Coef=-.8577,xCoef=12.517,bCoef=450.11)
#swalemidIntense <- RMIntenseBuckets(swalemid,date="pdate",tips = "tips",swalemidStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.8577,xCoef=12.517,bCoef=450.11)

swalebot <- read.table(file="swalebotuv2.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
swalebot$pdate <- strptime(as.character(paste(swalebot$DATE,swalebot$TIME,sep=" ")),format="%Y%m%d %H%M%S")
swalebot$tips <- as.numeric(swalebot$VALUE)

swalebotStorms <- RMeventsBuckets(swalebot,ieHr=6,tips="tips",time="pdate",x2Coef=-.0012,xCoef=.2851,bCoef=53.002)
#swalebotIntense <- RMIntenseBuckets(swalebot,date="pdate",tips = "tips",swalebotStorms,sdate="StartDate",edate="EndDate",xmin=c(5,15,60,180),x2Coef=-.0012,xCoef=.2851,bCoef=53.002)

swaletopSmall <- swaletop[swaletop$pdate>=strptime("20140601",format="%Y%m%d"),]
swalemidSmall <- swalemid[swalemid$pdate>=strptime("20140601",format="%Y%m%d"),]
swalebotSmall <- swalebot[swalebot$pdate>=strptime("20140601",format="%Y%m%d"),]

fileToSave <- "swalebotStorms.csv"
write.table(swalebotStorms, fileToSave, row.names=FALSE, sep=",")
fileToSave <- "swalemidStorms.csv"
write.table(swalemidStorms, fileToSave, row.names=FALSE, sep=",")
fileToSave <- "swaletopStorms.csv"
write.table(swaletopStorms, fileToSave, row.names=FALSE, sep=",")

# library(dataRetrieval)
# precipSite <- '430356089183502'
# precipData <- readNWISdv(precipSite,'00045','2014-06-01','2014-09-21',statCd='00006')
# precip_data <- precipData[precipData$Date<=max(as.Date(swaletop$pdate)),]

precip_data <- read.table(file="precipdata.rdb",header=TRUE,sep="\t",comment.char="#",stringsAsFactors=FALSE,colClasses="character")
precip_data$pdate <- strptime(as.character(paste(precip_data$DATE,precip_data$TIME,sep=" ")),format="%Y%m%d %H%M%S")
precip_data$value <- as.numeric(precip_data$VALUE)


#interactive Google chart
events <- unique(rbind(swaletopStorms[,c(1:3)],swalebotStorms[,c(1:3)],swalemidStorms[,c(1:3)]))
top_data <- data.frame(swaletopSmall[,c("pdate","tips")],rep("Top",nrow(swaletopSmall)),rep(NA,nrow(swaletopSmall)),stringsAsFactors=FALSE)
names(top_data) <- c("datetime","value","name","label")
#top_data$datetime <- as.Date(top_data$datetime)
mid_data <- data.frame(swalemidSmall[,c("pdate","tips")],rep("Mid",nrow(swalemidSmall)),rep(NA,nrow(swalemidSmall)),stringsAsFactors=FALSE)
names(mid_data) <- c("datetime","value","name","label")
#mid_data$datetime <- as.Date(mid_data$datetime)
bot_data <- data.frame(swalebotSmall[,c("pdate","tips")],rep("Bottom",nrow(swalebotSmall)),rep(NA,nrow(swalebotSmall)),stringsAsFactors=FALSE)
names(bot_data) <- c("datetime","value","name","label")
#bot_data$datetime <- as.Date(bot_data$datetime)
event_data <- data.frame(events[,c("StartDate")],rep(20,nrow(events)),rep("Event",nrow(events)),events[,c("stormnum")],stringsAsFactors=FALSE)
names(event_data) <- c("datetime","value","name","label")
#event_data$datetime <- as.Date(event_data$datetime)
precip_data <- data.frame(precip_data[,c("pdate","value")],rep("Precip",nrow(precip_data)),rep(NA,nrow(precip_data)),stringsAsFactors=FALSE)
names(precip_data) <- c("datetime","value","name","label")
tzone <- unclass(top_data$datetime[1]$zone)
precip_data$datetime <- as.POSIXct(precip_data$datetime,tz=tzone)
plot_dataAll <- rbind(top_data,mid_data,bot_data,precip_data,event_data)
# hydrographInteractive <- gvisAnnotatedTimeLine(plot_data,datevar="datetime",numvar="value",idvar="name",annotationvar="label",options=list(colors="['blue','green','red','black','white']",displayAnnotations=TRUE,legendPosition="newRow",scaleColumns="[0,2]",scaleType='allfixed',width="1300px", height="700px",thickness="[2,2,2,2,.5]"))
# plot(hydrographInteractive)

#limit amount of time for plotting
for (i in min(plot_dataAll$label,na.rm=TRUE):max(plot_dataAll$label,na.rm=TRUE)) {
  stormstart <- min(plot_dataAll[which(plot_dataAll$label==i),]$datetime)
  stormend <- max(plot_dataAll[which(plot_dataAll$label==i),]$datetime)
  stormstart <- stormstart - 60*60
  stormend <- stormend + 60*60
  plot_data <- plot_dataAll[which(plot_dataAll$datetime<=stormend & plot_dataAll$datetime>=stormstart),]
  swaletopSmalla <- swaletopSmall[which(swaletopSmall$pdate<=stormend & swaletopSmall$pdate>=stormstart),]
  swalemidSmalla <- swalemidSmall[which(swalemidSmall$pdate<=stormend & swalemidSmall$pdate>=stormstart),] 
  swalebotSmalla <- swalebotSmall[which(swalebotSmall$pdate<=stormend & swalebotSmall$pdate>=stormstart),]
  
  siteNo <- "WDOT Grass Swale filter strip"
  dateInt <- as.numeric(difftime(max(swaletopSmalla$pdate),min(swaletopSmalla$pdate),units="sec"))/20
  startDt <- strftime(min(swaletopSmalla$pdate),format="%Y.%m.%d %H:%M")
  endDt <- strftime(max(swaletopSmalla$pdate), format="%Y.%m.%d %H:%M")
  ytop <- max(plot_data[plot_data$name %in% c("Top","Mid"),]$value)
  pdf(paste(siteNo,".event",i,".","hydrograph.pdf",sep=""),width=10,height=8)
  par(mar=c(8,4,5,4),xpd=T)
  plot(swaletopSmalla$pdate,swaletopSmalla$VALUE,xaxt="n",xlab="",ylim=c(0,ytop),ylab="Tips - Top and Middle",col="red",type="l",main=paste(siteNo,startDt,"-",endDt,sep=" "))
  lines(swalemidSmalla$pdate,swalemidSmalla$VALUE,xlab="",ylab="",col="blue",type="l")
  a<-seq(min(swaletopSmalla$pdate),max(swaletopSmalla$pdate),dateInt)
  axis.POSIXct(1,at=a,format="%m/%d %H:%M",las=2)
  mtext("Datetime",side=1,line=6)
  par(new=T)
  plot(swalebotSmalla$pdate,swalebotSmalla$VALUE,axes=F,xlab="",ylab="",col="green",type="l")
  points(events$StartDate,rep(max(as.numeric(swalebotSmalla$VALUE))/2,nrow(events)),xlab="",ylab="",col="purple",pch="o")
  axis(side=4)
  mtext("Tips - Bottom",side=4,line=2)
  par(new=T)
  plot(precip_data$datetime,precip_data$value,axes=F,xlab="",ylab="",col="black",type="l")
  legend("topleft",c("Tips - Bottom","Tips - Middle","Tips - Top","Precip","Events"),lty=c(1,1,1,1,NA),lwd=c(2.5,2.5,2.5,2.5),pch=c(NA,NA,NA,NA,1),col=c("green","red","blue","black","purple"))
  dev.off()
}


# #pdf of plot
# siteNo <- "WDOT Grass Swale"
# dateInt <- 24 
# startDt <- strftime(min(swaletopSmall$pdate),format="%Y.%m.%d")
# endDt <- strftime(max(swaletopSmall$pdate), format="%Y.%m.%d")
# ytop <- max(plot_data[plot_data$name %in% c("Top","Mid"),]$value)
# pdf(paste(siteNo,".",startDt,".",endDt,"hydrograph.pdf",sep=""),width=10,height=8)
# par(mar=c(8,4,5,4),xpd=T)
# plot(swaletopSmall$pdate,swaletopSmall$VALUE,xaxt="n",xlab="",ylim=c(0,ytop),ylab="Tips - Top and Middle",col="red",type="l",main=paste(siteNo,startDt,"-",endDt,sep=" "))
# lines(swalemidSmall$pdate,swalemidSmall$VALUE,xlab="",ylab="",col="blue",type="l")
# a<-seq(min(swaletopSmall$pdate),max(swaletopSmall$pdate),dateInt*7200)
# axis.POSIXct(1,at=a,format="%m/%d %H:%M",las=2)
# mtext("Datetime",side=1,line=6)
# par(new=T)
# plot(swalebotSmall$pdate,swalebotSmall$VALUE,axes=F,xlab="",ylab="",col="green",type="l")
# points(events$StartDate,rep(max(as.numeric(swalebotSmall$VALUE))/2,nrow(events)),xlab="",ylab="",col="purple",pch="o")
# axis(side=4)
# mtext("Tips - Bottom",side=4,line=2)
# par(new=T)
# plot(precip_data$datetime,precip_data$value,axes=F,xlab="",ylab="",col="black",type="l")
# legend("topleft",c("Tips - Bottom","Tips - Middle","Tips - Top","Precip","Events"),lty=c(1,1,1,1,NA),lwd=c(2.5,2.5,2.5,2.5),pch=c(NA,NA,NA,NA,1),col=c("green","red","blue","black","purple"))
# dev.off()
# 
# 
# # Graph of all 4 stacked
# pdf(paste(siteNo,".",startDt,".",endDt,"hydrographStack.pdf",sep=""),width=10,height=8)
# par(pty="m",plt=c(0.1,1,0,1),omd=c(0.1,0.9,0.1,0.9))
# par(mfrow=c(4,1))
# 
# pardat <- par()
# xlim <- as.POSIXct(c(min(swaletopSmall$pdate),max(swaletopSmall$pdate)))
# plot(swaletopSmall$pdate,swaletopSmall$VALUE,xlim=xlim,xaxt="n",yaxt="n",xlab="",ylab="",type="n",main="")
# mtext(paste(siteNo,"Tips and Precip graphs",sep=" "),3,line=1,cex=1.5)
# pardat <- par()
# #xaxisdat <- seq(pardat$xaxp[1],pardat$xaxp[2],(pardat$xaxp[2]-pardat$xaxp[1])/pardat$xaxp[3])
# xaxisdat <- seq(min(swaletopSmall$pdate),max(swaletopSmall$pdate),dateInt*7200)
# yaxisdat <- seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
# axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
# abline(v=xaxisdat, col="lightgray")
# abline(h=yaxisdat, col="lightgray")
# mtext("top",2,line=2.3)
# lines(swaletopSmall$pdate,swaletopSmall$VALUE,col="red")
# 
# plot(swalemidSmall$pdate,swalemidSmall$VALUE,xlim=xlim,xaxt="n",yaxt="n",xlab="",ylab="",type="n",main="")
# pardat<-par()
# yaxisdat <- seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
# axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
# abline(v=xaxisdat, col="lightgray")
# abline(h=yaxisdat, col="lightgray")
# mtext("mid",2,line=2.3)
# lines(swalemidSmall$pdate,swalemidSmall$VALUE,col="blue")
# 
# plot(swalebotSmall$pdate,swalebotSmall$VALUE,xlim=xlim,xaxt="n",yaxt="n",xlab="",ylab="",type="n",main="")
# pardat<-par()
# yaxisdat <- seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
# axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
# abline(v=xaxisdat, col="lightgray")
# abline(h=yaxisdat, col="lightgray")
# mtext("bot",2,line=2.3)
# lines(swalebotSmall$pdate,swalebotSmall$VALUE,col="green")
# 
# plot(as.POSIXlt(precip_data$datetime),precip_data$value,xlim=xlim,xaxt="n",yaxt="n",xlab="",ylab="",type="n",main="")
# pardat<-par()
# yaxisdat <- seq(pardat$yaxp[1],pardat$yaxp[2],(pardat$yaxp[2]-pardat$yaxp[1])/pardat$yaxp[3])
# axis(2, at=yaxisdat, las=2, padj=0.5, cex.axis=0.8, hadj=0.5, tcl=-0.3)
# abline(v=xaxisdat, col="lightgray")
# abline(h=yaxisdat, col="lightgray")
# mtext("precip",2,line=2.3)
# lines(as.POSIXlt(precip_data$datetime),precip_data$value,col="darkgray")
# 
# a<-seq(min(swaletopSmall$pdate),max(swaletopSmall$pdate),dateInt*7200)
# axis.POSIXct(1,at=a,format="%m/%d/%Y",las=2,padj=-1.4, cex.axis=0.9, hadj=1, tcl=-0.3)
# #axis(1, at=xaxisdat, padj=-1.4, cex.axis=0.9, hadj=0.5, tcl=-0.3)
# mtext("Datetime",1,line=6)
# dev.off()
