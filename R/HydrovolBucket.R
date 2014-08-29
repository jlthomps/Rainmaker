#' HydrovolBucket
#'
#' Computes volumes for storm events given the tip rate time series and 
#' the begin and end dates and times of the events. Dates must be in POSIXct format.
#'
#' @param dfT dataframe with Q and time
#' @param Tr string name of column in dfQ with Q, defaults to "Tr"
#' @param time string name of column in dfT with POSIXct time, defaults to "pdate"
#' @param df.dates dataframe with begin and end dates/times in POSIXct format
#' @param bdate string begin date in POSIXct column name, defaults to "bpdate"
#' @param edate string end date in POSIXct column name, defaults to "epdate" 
#' @param volume string name of resulting volume variable, defaults to "event.vol"
#' @param Tmax string name of Tmax variable, defaults to "Tmax"
#' @param duration string name of resulting duration variable, defaults to "Eduration"
#' @param x2Coef x-squared coefficient in best fit equation for tip volume by tip rate
#' @param xCoef x coefficient in best fit equation for tip volume by tip rate
#' @param bCoef y-intercept coefficient in best fit equation for tip volume by tip rate
#' @export 
#' @return df.dates2 dataframe
HydrovolBucket <- function(dfT, Tr="Tr", time="pdate", df.dates, bdate="bpdate",edate="epdate",
                     volume="event.vol",Tmax="Tmax",duration="Eduration",x2Coef,xCoef,bCoef){
  
  # Compute volumes and max tip rate for each hydrograph defined in the df.dates dataframe
  event.vol <- numeric()
  event.max <- numeric()
  for (i in 1:nrow(df.dates)){
    
    #Determine rows with range of times from last time step before hydrograph to one time step
    #after hydrograph ends and subset that time series
    begin.row <- max(which(dfT[,time]<=df.dates[i,bdate]))
    end.row <- min(which(dfT[,time]>=df.dates[i,edate]))
    subdfT <- dfT[begin.row:end.row,]
    
    sub.rows <- nrow(subdfT)
    if(sub.rows<3) {
      event.vol[i] <- NA
      event.max[i] <- NA
      next
    }
    
    #Estimate begining tip rate
    if(subdfT[1,time] != df.dates[i,bdate]){
      Tr1 <- subdfT[1,Tr]
      Tr2 <- subdfT[2,Tr]
      time1 <- subdfQ[1,time]
      time2 <- subdfQ[2,time]
      stime <- df.dates[i,bdate]
      test <- (Tr2-Tr1)*(as.numeric(difftime(stime,time1)))/(as.numeric(difftime(time2,time1)))+Tr1
      
      subdfT[1,Tr] <- test
      subdfT[1,time] <- df.dates[i,bdate]
    }
    
    #Estimate ending tip rate
    if(subdfT[sub.rows,time] != df.dates[i,edate]){
      Tr1 <- subdfT[(sub.rows-1),Tr]
      Tr2 <- subdfT[sub.rows,Tr]
      time1 <- subdfT[(sub.rows-1),time]
      time2 <- subdfT[sub.rows,time]
      stime <- df.dates[i,edate]
      test <- (Tr2-Tr1)*(as.numeric(difftime(stime,time1)))/(as.numeric(difftime(time2,time1)))+Tr1
      
      subdfT[sub.rows,Tr] <- test
      subdfT[sub.rows,time] <- df.dates[i,edate]
    }
    
    #sum individual volumes
    Volumes <- numeric()
    for (j in 2:sub.rows){
      meanTr <- mean(subdfT[c(j-1,j),Tr])
      timeGapMins <- difftime(subdfT[j,time],subdfT[(j-1),time],units="mins")
      VperTip <- (x2Coef*(meanTr^2))+(xCoef*meanTr)+bCoef
      Volumes[j] <- VperTip*meanTr*timeGapMins
    }
    
    
    event.vol[i] <- sum(Volumes,na.rm=T)
    event.max[i] <- max(subdfT[,Tr])
  }
  
  Eduration <- as.numeric(difftime(df.dates[,edate],df.dates[,bdate],units="hours"))
  df.dates2 <- cbind(df.dates,data.frame(event.vol=event.vol,Tmax=event.max,duration=Eduration))
  colnames(df.dates2) <- c(names(df.dates),volume,Tmax,duration)
  return(df.dates2)
}

