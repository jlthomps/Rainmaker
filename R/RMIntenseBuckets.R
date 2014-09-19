#' RMIntenseBuckets
#'
#' Function to compute maximum x-minute runoff intensities in units of mL/hr
#'
#' @param df dataframe
#' @param date string Date column name in df as POSIX
#' @param tips string Column name in df with instantaneous rain values
#' @param df.events dateframe with start and end dates/times for events
#' @param sdate string Start date column in df.events rain file as POSIX
#' @param edate string End date column in df.events rain file as POSIX 
#' @param xmin vector vector of values representing X-minute max rainfall requested
#' @param x2Coef x-squared coefficient in best fit equation for tip volume by tip rate
#' @param xCoef x coefficient in best fit equation for tip volume by tip rate
#' @param bCoef y-intercept coefficient in best fit equation for tip volume by tip rate
#' @return df.events dataframe, X-hour maximum runoff intensities
#' @export
#' @examples
RMIntenseBuckets <- function(df,date="pdate",tips = "tips",
                      df.events,sdate="StartDate",edate="EndDate",
                      xmin=c(60,180,360),x2Coef=0,xCoef=0,bCoef=0) {
  
  if (x2Coef+xCoef+bCoef==0) {cat("error in regression coefficients")}
  else {
  dfT <- df[,c(date,tips)]
    diffobj<-tail(dfT,-1)-head(dfT,-1)
    colnames(diffobj) <- c("minutes","tipdiff")
    units(diffobj$minutes) <- "mins"
    diffobj$tipdiff <- ifelse(diffobj$tipdiff<0,0,diffobj$tipdiff)
    diffobj <- rbind(NA,diffobj)
    dfT <- cbind(df,diffobj)
  dfT$meanTr <- dfT$tipdiff/as.numeric(dfT$minutes)
  dfT$VperTip <- (x2Coef*(dfT$meanTr^2))+(xCoef*dfT$meanTr)+bCoef
  dfT$Vol <- dfT$VperTip*dfT$tipdiff
  
  # Compute overall event duration
  df.events$duration <- (as.numeric(difftime(df.events[,edate],df.events[,sdate],units="hours")))
  df.events$Ievent <- sum(dfT$Vol,na.rm=TRUE)/df.events$duration
  
  # Determine x-minute intensities for each of the intensities specified  
  
  for (i in 1:length(xmin)){
    x <- xmin[i]*60
    intensity.var <- paste("I",xmin[i],sep="")
    df.events[,intensity.var] <- NA
    
    #   Isolate individual events and Compute max x-min intensity for each event 
    #   period: compute sum rain and divide by duration. Report x-min intensity 
    #   in units/hr
    
    for (j in 1:nrow(df.events)) {
      subdf <- subset(dfT,dfT[,date] >= df.events[j,sdate] & dfT[,date] <= df.events[j,edate])
      # Initialize intensity vector
      intensity <- numeric(length=nrow(subdf))
      
      for (k in 1:nrow(subdf)){
        enddate <- subdf[k,date]+x
        bdate <- subdf[k,date]       
        subdf2 <- subset(subdf,subdf[,date] >= bdate & subdf[,date] < enddate)
        intensity[k] <- sum(subdf2$Vol)/(x/60/60)
        
      }
      df.events[j,intensity.var] <- max(intensity,na.rm=TRUE)
    }
  }
}
  return(df.events)
}
