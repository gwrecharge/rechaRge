#Processing of the river flow to compute the baseflow with Lyne and Hollick and resample them into monthly observations

#1-Observations data processing ####
  #1.1-Select the observed flow for the simulation period and interpolate the gaps ####
    observed_flow<-observed_flow[,which(unlist(lapply(observed_flow, function(x)!all(is.na(x))))),with=F]
    
    if(ncol(observed_flow)<4){
      print("error - no observed data on the simulation period")
    }else{
      for (c in 4:ncol(observed_flow)){
        observed_flow[[c]][2:(nrow(observed_flow)-1)]<-na.approx(observed_flow[[c]][2:(nrow(observed_flow)-1)],
                                                                 maxgap = 5,na.rm = FALSE)
      } #fill up the gap in the observed flow up to 5 days
      rm(c)
      observed_flow$date<-as.POSIXct(paste(observed_flow$year, observed_flow$month, observed_flow$day, sep = "-"),
                                     format = "%Y-%m-%d", tz = "UTC")
      observed_flow<-observed_flow[,c(ncol(observed_flow), 1:3, 4:(ncol(observed_flow)-1)), with = FALSE]
      observed_flow_month <- data.table(year = c(rep(unique(observed_flow$year),each = 12)), 
                                        month = c(rep(c(1:12),length(unique(observed_flow$year)))))
    }
    
  #1.2-List of the available gauging station for the simulation period ####
    if(ncol(observed_flow)>4){
      gauging<-as.numeric(colnames(observed_flow)[5:ncol(observed_flow)])
    }
    
  #1.3-compute baseflow with Lyne and Hollick (alpha calibrated independently) ####
    if(ncol(observed_flow)<5){
      print("error - no observed river flow on the simulation period - baseflow computation impossible")
    }else{
      for(c in 5:ncol(observed_flow)){
        #c<-5
        bf<-observed_flow[,c(1:3,c), with = FALSE]
        bf<-na.contiguous(bf)                             #select the longest period without NA
        colnames(bf)<-c("Date", "year", "month", "Q")
        bf$bf_lh<-baseflows(bf[,c(1,4), with = FALSE],
                            alpha_lyne_hollick$alpha[which(alpha_lyne_hollick$station == colnames(observed_flow)[c])], 
                            n.reflected = 30, ts="daily")[,3]
        q_month<-bf[,.(qmonth = sum(Q, na.rm = TRUE), bf_lh_month = sum(bf_lh, na.rm = TRUE)),.(year, month)]
        colnames(q_month)[3:4]<-c(colnames(observed_flow)[c],paste(colnames(observed_flow)[c],"_bf", sep = ""))
        observed_flow_month<- merge(observed_flow_month, q_month, by = c("year", "month"), all.x = TRUE)
      }
      rm(alpha_lyne_hollick, c, bf, q_month, observed_flow)
    }
