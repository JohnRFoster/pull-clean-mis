#----------------------------------
#
# Pre-Process Function for MIS data
#
# Ryan Miller
#
# Last upadated: 7 July 2017
#----------------------------------


#--FNC Preprocess and Convert data from long to wide format ----

pre.process.data <- function(in.dat){
  
  #----Load Libraries----
  #pkg <- c("reshape2","tidyr","plyr")
  #for(i in length(pkg)){require(pkg[i], quite = TRUE)}
  
  #--Alter USET_NAME for convenience
  tmp<-in.dat$USET_NAME
  tmp[tmp=="APPLIED/USED"]<-"APPLIED.USED"
  in.dat$USET_NAME <- tmp
  
  #Convert Work Time to Hours
  in.dat[in.dat$UOM_NAME=="MINUTES","WTM_QTY"] <- in.dat[in.dat$UOM_NAME=="MINUTES","WTM_QTY"] / 60
  in.dat$UOM_NAME <- "HOURS"
  
  #count(in.dat[,c("ID","AGRP_PRP_ID","UOM_NAME","WTCM_QTY","CMP_NAME","CMP_TYPE","DA_NAME","WT_WORK_DATE","USET_NAME")])
  
  in.dat <- aggregate(WTM_QTY~AGRP_PRP_ID+ALWS_AGRPROP_ID+UOM_NAME+WTCM_QTY+CMP_NAME+CMP_TYPE+WT_WORK_DATE+USET_NAME, data=in.dat, FUN=sum)
  
  #Restrict Columns
  #tmp<-in.dat[,c("ID","AGRP_PRP_ID","ID1","WTCM_QTY","CMP_NAME","WT_WORK_DATE","USET_NAME")]
  #tmp<-unique(tmp)
  
  #When seperate but multiple checks occur assume they are for different traps and sum trap count
  tmp <- aggregate(cbind(WTCM_QTY,WTM_QTY)~AGRP_PRP_ID+ALWS_AGRPROP_ID+WT_WORK_DATE+CMP_NAME+USET_NAME, data=in.dat, FUN=sum)
  
  #Convert to wide with count of traps for USET 
  data.wide <- spread(tmp, USET_NAME, WTCM_QTY)
  
  #Add empty USET names if not present for convience of coding below
  ifelse("RESET" %in% colnames(data.wide),print("RESET in dataframe"),RESET<-rep(0,nrow(data.wide)))
  ifelse("UNSET" %in% colnames(data.wide),print("UNSET in dataframe"),UNSET<-rep(0,nrow(data.wide)))
  ifelse("SET" %in% colnames(data.wide),print("SET in dataframe"),SET<-rep(0,nrow(data.wide)))
  ifelse("REMOVED" %in% colnames(data.wide),print("REMOVED in dataframe"),REMOVED<-rep(0,nrow(data.wide)))
  ifelse("APPLIED.USED" %in% colnames(data.wide),print("Applied.Used in dataframe"),APPLIED.USED<-rep(0,nrow(data.wide)))
  #ifelse("PREBAIT" %in% colnames(data.wide),print("PREBAIT in dataframe"),APPLIED.USED<-rep(0,nrow(data.wide)))
  
  #ifelse("PREBAIT" %in% colnames(data.wide),print("Added REMOVED to dataframe"),data.wide<-cbind(data.wide,PREBAIT))
  ifelse("RESET" %in% colnames(data.wide),print("Added RESET to dataframe"),data.wide<-cbind(data.wide,RESET))
  ifelse("UNSET" %in% colnames(data.wide),print("Added UNSET in dataframe"),data.wide<-cbind(data.wide,UNSET))
  ifelse("SET" %in% colnames(data.wide),print("Added SET to dataframe"),data.wide<-cbind(data.wide,SET))
  ifelse("REMOVED" %in% colnames(data.wide),print("Added REMOVED to dataframe"),data.wide<-cbind(data.wide,REMOVED))

  ifelse("APPLIED.USED" %in% colnames(data.wide),print("Did not add Applied.Used in dataframe"),data.wide<-cbind(data.wide,APPLIED.USED))
  
  #Redice columns of interest
  #data.wide<-data.wide[,c("ID","AGRP_PRP_ID","CMP_NAME","WT_WORK_DATE", "PREBAIT", "SET","CHECKED","APPLIED.USED", "RESET", "UNSET", "REMOVED")]
  data.wide<-data.wide[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","CMP_NAME","WT_WORK_DATE", "SET","CHECKED","APPLIED.USED", "RESET", "UNSET", "REMOVED")]
  
  #Order data to make chronology of activity
  data.wide <- data.wide[order(data.wide$AGRP_PRP_ID,data.wide$WT_WORK_DATE),, drop=FALSE]
  
  #Make NA values 0 for trap counts
  #data.wide[is.na(data.wide$PREBAIT),"PREBAIT"]<-0
  data.wide[is.na(data.wide$SET),"SET"]<-0
  data.wide[is.na(data.wide$CHECKED),"CHECKED"]<-0
  data.wide[is.na(data.wide$REMOVED),"REMOVED"]<-0
  data.wide[is.na(data.wide$APPLIED.USED),"APPLIED.USED"]<-0
  data.wide[is.na(data.wide$UNSET),"UNSET"]<-0
  data.wide[is.na(data.wide$RESET),"RESET"]<-0
  
  #Sum trap counts by AGRP_PRP and Workdate - generate single record for each WT_Work_Date
  #out.dat <- aggregate(cbind(PREBAIT,SET,CHECKED,APPLIED.USED,RESET,UNSET,REMOVED)~ID+AGRP_PRP_ID+CMP_NAME+WT_WORK_DATE, data=data.wide, FUN=sum)
  out.dat <- aggregate(cbind(SET,CHECKED,APPLIED.USED,RESET,UNSET,REMOVED)~AGRP_PRP_ID+ALWS_AGRPROP_ID+CMP_NAME+WT_WORK_DATE, data=data.wide, FUN=sum)
  
  out.dat <- out.dat[order(out.dat$AGRP_PRP_ID, out.dat$WT_WORK_DATE),, drop=FALSE]
  
  
  return(out.dat)
} #END FUNCTION



#--FNC Calculate days elapsed and time since event

calc.days.elapsed<-function(in.dat){
  
  #Add fields
  in.dat$day.diff <- 0
  in.dat$time.since.event <- 0
  in.dat$time.since.event.type <- NA
  
  #----Calculate days elapsed and time since event
  for(i in 2:nrow(in.dat)){
    
    in.dat[i,"day.diff"]<-in.dat[i,"WT_WORK_DATE"]-in.dat[i-1,"WT_WORK_DATE"]
    
    set.flag<-in.dat[i,"SET"]
    
    #if(is.na(set.flag)!=TRUE){in.dat[i,"time.since.event"] <- in.dat[i,"day.diff"]}
    if(set.flag==1){in.dat[i,"time.since.event"] <- in.dat[i,"day.diff"]
                    in.dat[i,"time.since.event.type"] <- "NEW SET"}
  }#END Loop
  
  return(in.dat)
}#END Function

#----END FUNCTION



# event.time.threshold=mlv.thershold

#--FNC Add Trap Count and Event Ids
add.trap.count<-function(in.dat, event.time.threshold=25){
  
  #in.dat<-calc.days.elapsed(in.dat)
  
  #Add fields
  in.dat$trap.count <- 0
  in.dat$trap.count.event <- 0
  in.dat$days.active <- in.dat$day.diff
  in.dat$event.id <- NA
  
  #Make NA values 0 for trap counts
  in.dat[is.na(in.dat$SET),"SET"]<-0
  in.dat[is.na(in.dat$CHECKED),"CHECKED"]<-0
  in.dat[is.na(in.dat$REMOVED),"REMOVED"]<-0
  in.dat[is.na(in.dat$"APPLIED.USED"),"APPLIED.USED"]<-0
  in.dat[is.na(in.dat$UNSET),"UNSET"]<-0
  in.dat[is.na(in.dat$RESET),"RESET"]<-0
  #in.dat[is.na(in.dat$PREBAIT),"PREBAIT"]<-0
  
  in.dat[in.dat$PREBAIT!=0,"APPLIED.USED"]<-1
  
  
  
  #----Calculate trap count
  #for(i in 1:10){
  
  for(i in 1:nrow(in.dat)){
    
    #Set accounting variables
    if(i==1){trap.count <- vector(length=nrow(in.dat))
             trap.count.event <- vector(length=nrow(in.dat))
             trap.count[1:nrow(in.dat)]<-0
             event.id<-1
             time.since.event<-0}
    
    #flags for current USET Name and time since event
    set.flag<-in.dat[i,"SET"]
    time.since.event<-in.dat[i,"day.diff"]
    increment.event=FALSE
    
    
    #--Accounting of trap number
    
    #Calculate number of traps at time i
    if(i==1){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
             trap.count.event[i] <- trap.count[i]}
    
    if(in.dat[i,"REMOVED"]>0 && in.dat[i,"UNSET"]>0){
      
      trap.reduction.difference <- in.dat[i,"REMOVED"]-in.dat[i,"UNSET"]
      
      if(trap.reduction.difference>0){
        
        #If time since event < threshold tally traps
        if(i>1 && time.since.event < event.time.threshold){trap.count[i] <- (trap.count[i-1] + in.dat[i,"SET"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                           trap.count.event[i] <- trap.count[i]}
        
        #If trap count = 0 but number of traps checked or applied is > 0 set trap count to # checked traps
        if(trap.count[i]==0 && in.dat[i,"CHECKED"]>0){trap.count[i] <- (in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                      trap.count.event[i] <- trap.count[i]}
        
        #If time since event < threshold tally traps
        if(i>1 && time.since.event > event.time.threshold){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                           trap.count.event[i] <- trap.count[i]}
      }
      
      if(trap.reduction.difference<1){
        
        #If time since event < threshold tally traps
        if(i>1 && time.since.event < event.time.threshold){trap.count[i] <- (trap.count[i-1] + in.dat[i,"SET"]) - (in.dat[i,"REMOVED"])
                                                           trap.count.event[i] <- trap.count[i]}
        
        #If trap count = 0 but number of traps checked or applied is > 0 set trap count to # checked traps
        if(trap.count[i]==0 && in.dat[i,"CHECKED"]>0){trap.count[i] <- (in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"])
                                                      trap.count.event[i] <- trap.count[i]}
        
        #If time since event < threshold tally traps
        if(i>1 && time.since.event > event.time.threshold){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"])
                                                           trap.count.event[i] <- trap.count[i]}
      }
    }
    
    if(in.dat[i,"REMOVED"]<1 || in.dat[i,"UNSET"]<1){
      
      #If time since event < threshold tally traps
      if(i>1 && time.since.event < event.time.threshold){trap.count[i] <- (trap.count[i-1] + in.dat[i,"SET"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                         trap.count.event[i] <- trap.count[i]}
      
      #If trap count = 0 but number of traps checked or applied is > 0 set trap count to # checked traps
      if(trap.count[i]==0 && in.dat[i,"CHECKED"]>0){trap.count[i] <- (in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                    trap.count.event[i] <- trap.count[i]}
      
      #If time since event < threshold tally traps
      if(i>1 && time.since.event > event.time.threshold){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                         trap.count.event[i] <- trap.count[i]}
    } 
    
    
    #If time since event is large then assume "SET" is missing and reset trap count
    if(in.dat[i,"SET"]<1 && time.since.event > event.time.threshold){
      if(in.dat[i,"RESET"]>0){trap.count[i] <- in.dat[i,"RESET"]}
      if(in.dat[i,"RESET"]<1){trap.count[i] <- in.dat[i,"CHECKED"]}
      trap.count.event[i] <- trap.count[i]
    }
    
    #Adjust total trap count for estimating trap nights
    #If trap count is negative then set to 0
    if(trap.count[i]<0){trap.count[i] <- 0}
    
    
    #--Assign trap count
    
    #Assign trap.count
    if(trap.count[i]>0){ in.dat[i,"trap.count"] <- trap.count[i]
                         in.dat[i,"trap.count.event"] <- trap.count.event[i] }
    
    if(trap.count[i]==0 && i>1){ in.dat[i,"trap.count"] <- trap.count[i-1]
                                 in.dat[i,"trap.count.event"] <- trap.count.event[i] }
    
    if(trap.count[i]==0 && i==1){ in.dat[i,"trap.count"] <- trap.count[i]
                                  in.dat[i,"trap.count.event"] <- trap.count.event[i] }
    
    #--Generate event id
    
    in.dat[i,"event.id"] <- event.id
    
    #Increment for new event assuming 0 traps indicates new event.
    if(trap.count[i]==0){event.id <- event.id + 1; increment.event=TRUE}
    if(trap.count[i]==0 && in.dat[i,"SET"]>0){event.id <- event.id + 1; in.dat[i,"event.id"] <- event.id; increment.event=TRUE}
    
    #Increment for new event if time since event is larger than threshold.
    if(increment.event==FALSE && time.since.event > event.time.threshold){event.id <- event.id + 1; in.dat[i,"event.id"] <- event.id; increment.event=FALSE}
    
    
    #--Accounting of days active
    last.event.id <- in.dat[i-1,"event.id"]
    current.event.id <- in.dat[i,"event.id"]
    
    if(i>1 && last.event.id!=current.event.id){in.dat[i,"days.active"] <- 0}
    
    #--Accounting of time since event
    if(time.since.event > event.time.threshold){in.dat[i,"time.since.event"] <- time.since.event
                                                in.dat[i,"time.since.event.type"] <- "THRESHOLD"}
    
  }#--End Loop--#
  
  #Trap Nights
  in.dat$trap.nights <- in.dat$days.active * in.dat$trap.count
  
  #Cummulative Trap Nights
  #in.dat$cum.trap.nights <- cumsum(in.dat$trap.nights)
  
  return(in.dat)
} #END Function

#----END FUNCTION
