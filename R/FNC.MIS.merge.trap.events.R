#--FNC Adjust and Merge Trap Events

#in.dat=chronology.course;event.time.threshold=25;max.time=40

merge.trap.events <- function(in.dat,event.time.threshold,max.time){
  
  prop.vec <- unique(in.dat$AGRP_PRP_ID)
  
  for(j in 1:length(prop.vec)){
    prop.dat <- in.dat[in.dat$AGRP_PRP_ID==prop.vec[j],]
    
    for(i in 2:nrow(prop.dat)){
      if(prop.dat[i,"time.since.event"]>event.time.threshold && prop.dat[i,"time.since.event"]<max.time){
        if(prop.dat[i,"Take"]>0){
          if(location.i<=nrow(prop.dat)){
            
            location.i <- i
            
            if(location.i==nrow(prop.dat)){
              prop.dat[location.i,"event.id"] <- prop.dat[location.i-1,"event.id"]
              prop.dat[location.i,"time.since.event.type"]=="MERGED WITH PREVIOUS"
            }
            
            if(location.i!=nrow(prop.dat)){
              repeat{
                prop.dat[location.i,"event.id"] <- prop.dat[location.i-1,"event.id"]
                prop.dat[location.i,"time.since.event.type"]=="MERGED WITH PREVIOUS"
                location.i=location.i+1
                
                if(prop.dat[location.i,"Take"]==0){break}
              }
              
              prop.dat[location.i,"time.since.event.type"] <- "NEW INFERRED"
              
              i=location.i
            }#Last Row Check
          }#TAKE Constraint
        }
      }
    }#END Individual Property Loop
    
    if(j==1){out.dat<-prop.dat}
    if(j>1){out.dat <- rbind.data.frame(out.dat,prop.dat)}
    
  }#END All Data Loop
  
  return(out.dat)
  
}#END FUNCITON
