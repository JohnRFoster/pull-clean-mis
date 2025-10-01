#----------------------------------
#
# Function for calculating elasped days between events in MIS data
#
# Ryan Miller
#
# Last upadated: 7 July 2017
#----------------------------------


#--FNC Calculate days elapsed and time since event

calc.days.elapsed<-function(in.dat){

  #Add fields
  in.dat$day.diff <- 0
  in.dat$time.since.event <- 0
  in.dat$time.since.event.type <- NA

  #Ensure data is ordered
  in.dat <- in.dat[order(in.dat$AGRP_PRP_ID, in.dat$WT_WORK_DATE),, drop=FALSE]

  if(in.dat[1,"SET"]==1){in.dat[1,"time.since.event.type"] <- "NEW SET"}

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



