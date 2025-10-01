#----------------------------------
#
# Function for calculating aerial chronology
#
# Ryan Miller
#
# Last upadated: 7 July 2017
#----------------------------------


#--FNC Add Trap Count and Event Ids
aerial.chronology<-function(tmp.dat, time.thershold=25){
  
  #----Load Libraries----
  pkg <- c("reshape2","tidyr","plyr")
  for(i in length(pkg)){suppressPackageStartupMessages(library(pkg[i],character.only=TRUE))}
  
if(nrow(tmp.dat)==1){
  tmp.dat[,"time.since.event"] <- 0
  tmp.dat[,"event.id"] <- 1
  tmp.dat[,"within.event.id"] <- 1
}

if(nrow(tmp.dat)>1){
  
  #Ensure order is correct
  tmp.dat<-tmp.dat[order(tmp.dat$WT_WORK_DATE),]
  
  #Set Counters
  event.id=1
  current.event<-event.id
  within.event.id=2
  
  #----Calculate days elapsed and time since event
  for(i in 2:nrow(tmp.dat)){
    
    tmp.dat[i,"day.diff"]<-tmp.dat[i,"WT_WORK_DATE"]-tmp.dat[i-1,"WT_WORK_DATE"]
    
    if(tmp.dat[i,"day.diff"]>time.thershold){tmp.dat[i,"time.since.event"] <- tmp.dat[i,"day.diff"]}
    
    #--Assign event ID
    if(i==2){tmp.dat[i-1,"event.id"] <- event.id}
    if(tmp.dat[i,"time.since.event"]<time.thershold){tmp.dat[i,"event.id"] <- event.id}
    if(tmp.dat[i,"time.since.event"]>=time.thershold){event.id=event.id+1; tmp.dat[i,"event.id"] <- event.id}
    
    #--Add time for within event 
    if(i==2){tmp.dat[i-1,"within.event.id"] <- 1}
    if(current.event!=event.id){within.event.id <- 1; current.event<-event.id}
    
    #Assign within event ID
    tmp.dat[i,"within.event.id"] <- within.event.id
    
    #Increment counter
    within.event.id=within.event.id+1
    
  }#END Loop
  
}#END IF STATEMENT

return(tmp.dat)
}#END FUNCTION

##----END----##