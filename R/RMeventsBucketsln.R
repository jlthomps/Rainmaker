#' Rainfall event determination from tipping bucket data
#' 
#' @description
#' Compute rainfall event variables based on time series of tipping bucket data with only one bucket.
#'
#' @param df dataframe with tipping bucket data
#' @param ieHr numeric Interevent period in hours, defaults to 6, 
#' @param tips string Column name of tipping bucket unit values, defaults to "tips"
#' @param time string column with as.POSIXctdate, defaults to "pdate"
#' @param x2Coef x-squared coefficient in best fit equation for tip volume by tip rate
#' @param xCoef x coefficient in best fit equation for tip volume by tip rate
#' @param bCoef y-intercept coefficient in best fit equation for tip volume by tip rate
#' @return list of storms
#' @export
#' @examples
RMeventsBucketsln <- function(df,ieHr=6,tips="tips",time="pdate",x2Coef=0,xCoef=0,bCoef=0){
  
  ieSec <- ieHr * 3600 # compute interevent period in seconds to use with POSIX
  
  if (x2Coef+xCoef+bCoef==0) {cat("error in regression coefficients")}
  else {
    dfT <- df[,c(time,tips)]
    diffobj<-tail(dfT,-1)-head(dfT,-1)
    colnames(diffobj) <- c("minutes","tipdiff")
    units(diffobj$minutes) <- "mins"
    diffobj$tipdiff <- ifelse(diffobj$tipdiff<0,0,diffobj$tipdiff)
    diffobj$tipdiffln <- ifelse(diffobj$tipdiff==0,0,log(diffobj$tipdiff))
    tipdiff="tipdiff"
    diffobj <- rbind(NA,diffobj)
    dfT <- cbind(df,diffobj)
    #dfT$meanTr <- dfT$tipdiff/as.numeric(dfT$minutes)
    dfT$VperTip <- (x2Coef*(dfT$tipdiffln^2))+(xCoef*dfT$tipdiffln)+bCoef
    dfT$Vol <- dfT$VperTip*dfT$tipdiff
    volume="Vol"
  
  # Initiate variables
  StartRow <- 1
  EndRow <- 1
  StartDryRow <- 1
  dry <- TRUE
  stormnum <- 0
  continue.dry <- TRUE
  first.dry <- TRUE
  sumrain <- 0
  numTips <- 0
  tipdiff="tipdiff"
  
  # Loop through rain data and define event periods
  for (i in 3:nrow(dfT)) {
    
    # During dry period, look for start of event
    if(dry) {
      
      # Event initiation
      if(dfT[i,tipdiff]>0) {  
        dry=FALSE
        StartRow <- i
      }
    }
    # Define event period
    if(!dry) {
      
      # Search for end of event period
      if(dfT[i,tipdiff]==0) {
        if(!continue.dry){
          continue.dry <- TRUE
          dryduration <- difftime(dfT[i,time],
                                  dfT[StartDryRow,time],
                                  units="secs")
        }
        
        # Continue checking for end of event (dry duration >= interevent period)
        if(continue.dry){                   
          dryduration <- difftime(dfT[i,time],dfT[StartDryRow,time],units="secs")
          if (!first.dry){
            if(dryduration >= ieSec) {
              
              first.dry <- TRUE
              EndRow <- StartDryRow
              stormnum <- stormnum + 1
            
            # After event period ends, save start and end dates/times and rain depth
            current.storm <- data.frame(stormnum=stormnum,
                                        StartDate=dfT[StartRow+1,time],
                                        EndDate=dfT[EndRow,time],
                                        volume=sumrain,
                                        tips=numTips)
            dry <- TRUE
            if(stormnum>1) storms <- rbind(storms,current.storm)
            else storms <- current.storm        
            sumrain <- 0
            numTips <- 0
          }
        }}
      }
      if (dfT[i,tipdiff]!=0) {
        sumrain <- sumrain + dfT[i,volume]
        numTips <- numTips + dfT[i,tipdiff]
        EndRow <- i
        StartDryRow <- EndRow
        continue.dry <- FALSE
        first.dry <- FALSE
      }
    }
  }
  } 
  storms$duration <- abs(as.numeric(difftime(storms[,2],storms[,3],units="secs")))
  storms$VolperTip <- storms$volume/storms$tips
  #storms <- storms[which(storms$duration>ieSec),]
  return(storms)
}