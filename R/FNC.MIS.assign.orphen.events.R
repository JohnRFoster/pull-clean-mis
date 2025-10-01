##-------------------------
##----FNC Orphen Events----

#master.dat=trap.harvest.chronology; max.time=6

assign.orphen.events <- function(master.dat, max.time){

  #Identify data with Orphens

  property.freq<-plyr::count(master.dat[,c("AGRP_PRP_ID","event.id")])

  property.vec <- unique(property.freq[property.freq$freq==1,"AGRP_PRP_ID"])

  process.dat <- master.dat[master.dat$AGRP_PRP_ID %in% property.vec,]
  not.process.dat <- master.dat[master.dat$AGRP_PRP_ID %not in% property.vec,]

  pb <- txtProgressBar(min = 0, max = length(property.vec), style = 3)

  for(j in 1:length(property.vec)){

  in.dat <- process.dat[process.dat$AGRP_PRP_ID==property.vec[j],]

  #Establish events to check
  event.vec <- unique(plyr::count(in.dat[,"event.id"]))
  event.vec <- event.vec[event.vec$freq==1,"x"]

  if(length(event.vec)!=0){

  index.lut <- cbind.data.frame(row.number=seq(1,nrow(in.dat),1),rowname=rownames(in.dat))

  for(k in 1:length(event.vec)){


    if(length(rownames(in.dat[in.dat$event.id==event.vec[k],]))==1){

      row.location <- index.lut[index.lut$rowname==rownames(in.dat[in.dat$event.id==event.vec[k],]),"row.number"]

      #Check previous time step
      if(row.location!=1){length.t.minus1 <- abs((in.dat[row.location,"WT_WORK_DATE"] - in.dat[row.location-1,"WT_WORK_DATE"]))}

      #Check next time step
      if(row.location!=nrow(in.dat)){length.t.plus1 <- abs((in.dat[row.location,"WT_WORK_DATE"] - in.dat[row.location+1,"WT_WORK_DATE"]))}

      #For First Row Orphen
      if(row.location==1){
        if(length.t.plus1<max.time){
          in.dat[row.location,"event.id"] <- in.dat[row.location+1,"event.id"]
          in.dat[row.location,"time.since.event.type"]<-"MERGED WITH NEXT"
        }
      }

      #For Last Row Orphen
      if(row.location==nrow(in.dat)){
        if(length.t.minus1<max.time){
          in.dat[row.location,"event.id"] <- in.dat[row.location-1,"event.id"]
          in.dat[row.location,"time.since.event.type"]<-"MERGED WITH PREVIOUS"
        }
      }

      #If Both Below Max Time
      if(row.location!=1 & row.location!=nrow(in.dat)){

        if(length.t.minus1<max.time && length.t.plus1<max.time){
          if(length.t.minus1<length.t.plus1){
            if(length.t.minus1<max.time){
              in.dat[row.location,"event.id"] <- in.dat[row.location-1,"event.id"]
              in.dat[row.location,"time.since.event.type"]=="MERGED WITH PREVIOUS"
            }
          }

          if(length.t.minus1>length.t.plus1){
            if(length.t.plus1<max.time){
              in.dat[row.location,"event.id"] <- in.dat[row.location+1,"event.id"]
              in.dat[row.location,"time.since.event.type"]<-"MERGED WITH NEXT"
            }
          }
        }#END BOTH COMPARE

        if(length.t.minus1<max.time){
          in.dat[row.location,"event.id"] <- in.dat[row.location-1,"event.id"]
          in.dat[row.location,"time.since.event.type"]<-"MERGED WITH PREVIOUS"
        }

        if(length.t.plus1<max.time){
          in.dat[row.location,"event.id"] <- in.dat[row.location+1,"event.id"]
          in.dat[row.location,"time.since.event.type"]<-"MERGED WITH NEXT"
        }
      }
    }
  }#END Unique Property Loop

  }#END if statement

  if(j==1){out.dat<-rbind.data.frame(not.process.dat,in.dat)}
  if(j>1){out.dat<-rbind.data.frame(out.dat,in.dat)}

  setTxtProgressBar(pb, j)

  }#END Loop over all data
  close(pb)

  print(plyr::count(out.dat$time.since.event.type))

  return(out.dat)

}#END FUNCTION


