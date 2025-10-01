#----------------------------------
#
# Function for calculating trapping effort using MIS data
#
# Ryan Miller
#
# Last upadated: 7 July 2017
#----------------------------------


#--FNC Add Trap Count and Event Ids
add.trap.count<-function(in.dat, use.resets=FALSE, event.time.threshold=25){
  
  #----Load Libraries----
  #pkg <- c("reshape2","tidyr","plyr")
  #for(i in length(pkg)){suppressPackageStartupMessages(library(pkg[i],character.only=TRUE))}
  
  
  #in.dat<-calc.days.elapsed(in.dat)
    
  #in.dat<-tmp.dat
  
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
    
    
    #--Accounting of trap number for each event
    
    if(use.resets==TRUE){
            
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
    } #END USE RESET = TRUE        
    
    
    if(use.resets==FALSE){
      
      #Calculate number of traps at time i
      if(i==1){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"])
               trap.count.event[i] <- trap.count[i]}
      
      if(in.dat[i,"REMOVED"]>0 && in.dat[i,"UNSET"]>0){
        
        trap.reduction.difference <- in.dat[i,"REMOVED"]-in.dat[i,"UNSET"]
        
        if(trap.reduction.difference>0){
          
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
        if(i>1 && time.since.event < event.time.threshold){trap.count[i] <- (trap.count[i-1] + in.dat[i,"SET"]) - (in.dat[i,"REMOVED"])
                                                           trap.count.event[i] <- trap.count[i]}
        
        #If trap count = 0 but number of traps checked or applied is > 0 set trap count to # checked traps
        if(trap.count[i]==0 && in.dat[i,"CHECKED"]>0){trap.count[i] <- (in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"] + in.dat[i,"UNSET"])
                                                      trap.count.event[i] <- trap.count[i]}
        
        #If time since event < threshold tally traps
        if(i>1 && time.since.event > event.time.threshold){trap.count[i] <- (in.dat[i,"SET"] + in.dat[i,"CHECKED"]) - (in.dat[i,"REMOVED"])
                                                           trap.count.event[i] <- trap.count[i]}
      } 
      
      
      #If time since event is large then assume "SET" is missing and reset trap count
      if(in.dat[i,"SET"]<1 && time.since.event > event.time.threshold){
        trap.count[i] <- in.dat[i,"CHECKED"]
        trap.count.event[i] <- trap.count[i]
      }
    } #END USE RESET = FALSE  
    
    
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
    if(time.since.event > event.time.threshold){event.id <- event.id + 1; in.dat[i,"event.id"] <- event.id; increment.event=FALSE}
    
    
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



      


