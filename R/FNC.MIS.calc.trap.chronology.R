#----Generate trap chrnology for each trap type

# in.dat=trap.dat;event.time.threshold=15;use.mlv.thershold=TRUE;use.stat.fudge=TRUE; fudge.user=5

trap.chronology <- function(in.dat,event.time.threshold=15,use.mlv.thershold=TRUE, use.stat.fudge=TRUE, fudge.user=5){

  threshold.mat<-matrix(ncol=5,nrow=nrow(in.dat))

  if(nrow(in.dat)!=0){

    in.dat <- pre.process.data(in.dat)

    freq.prop<-plyr::count(in.dat$AGRP_PRP_ID)
    freq.prop.gt1<-freq.prop[freq.prop$freq>1,]

    #limit to those with more then 1 record
    in.dat <- in.dat[in.dat$AGRP_PRP_ID %in% freq.prop.gt1[,1],]

    #Logical to catch rows that =0
    if(nrow(in.dat)!=0){

      #Generate Agreement List
      agrp_prp.list <- plyr::count(in.dat$AGRP_PRP_ID)
      prop.list <- agrp_prp.list[,1]

      pb <- txtProgressBar(min = 0, max = length(prop.list), style = 3)

      #--Run For Each Agreement
      for(i in 1:length(prop.list)){

        #Subset Property
        tmp.dat <- in.dat[in.dat$AGRP_PRP_ID==prop.list[i],]
        tmp.dat <- tmp.dat[order(tmp.dat$WT_WORK_DATE),, drop=FALSE]

        if(sum(tmp.dat[,c("SET","CHECKED","REMOVED")])!=0){

          #CALC Elapsed Days
          tmp.dat <- calc.days.elapsed(tmp.dat)

          #CALC Event Thershold
          if(use.mlv.thershold==TRUE){

            my_mlv <- function(v, m, i, ...){
              tryCatch(
                expr = {
                  mlv(v, method = m, na.rm = TRUE, ...)
                },

                warning = function(w){
                  message(w)
                  message("\nproperty ", i)
                }

              )
            }

            ml.value <- my_mlv(tmp.dat$day.diff, "mfv", i)


            if(length(ml.value)>1){
              ml.value <- my_mlv(tmp.dat$day.diff, "Venter", i, tie.limit = 1)
            }

            if(use.stat.fudge==TRUE){
              stat.fudge<-mean(tmp.dat$day.diff)+sd(tmp.dat$day.diff)/4
              if(stat.fudge<fudge.user){fudge.amount<-stat.fudge}
              if(stat.fudge>fudge.user){fudge.amount<-fudge.user}
            }
            if(use.stat.fudge==FALSE){fudge.amount<-fudge.user}

            if(abs(ml.value-mean(tmp.dat$day.diff))<=2){mlv.thershold <- fudge.amount}
            if(abs(ml.value-mean(tmp.dat$day.diff))>2){mlv.thershold <- ml.value}
            if(mlv.thershold>event.time.threshold){mlv.thershold<-event.time.threshold}
          }

          threshold.mat[i,1]<-prop.list[i]
          threshold.mat[i,2]<-ml.value
          threshold.mat[i,3]<-stat.fudge
          threshold.mat[i,4]<-fudge.user
          threshold.mat[i,5]<-mlv.thershold

          #CALC Event Trap Count
          tmp.dat <- add.trap.count(tmp.dat, event.time.threshold=mlv.thershold)

          if(any(ls() %in% "tmp.out.dat")==FALSE){tmp.out.dat=tmp.dat}
          if(any(ls() %in% "tmp.out.dat")==TRUE && i>1){tmp.out.dat=rbind(tmp.out.dat,tmp.dat)}
        }

        #PRINT STATUS
        #status <- (i/length(prop.list))*100
        #print(paste(round(status,0),"% completed"))
        setTxtProgressBar(pb, i)

      } #END Loop for i

      #CLEAN UP
      out.dat<-tmp.out.dat
      rm(tmp.out.dat)

      #----Post Process Results

      #Generate Take by property
      #kill.by.prop<-dat.PropKill[dat.PropKill$CMP_NAME %in% trap.vec,]

      kill.by.prop<-dat.PropKill
      kill.by.prop<-unique(kill.by.prop[,c("ALWS_AGRPROP_ID","AGRP_PRP_ID","ST_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD","WTCM_QTY","CMP_NAME","WKR_QTY","WT_WORK_DATE")])
      kill.by.prop<-kill.by.prop[,c("AGRP_PRP_ID","WT_WORK_DATE","CMP_NAME","WKR_QTY")]

      #Merge trap chronology and take data
      trap.harvest.chronology <- merge(out.dat, kill.by.prop, by=c("AGRP_PRP_ID","WT_WORK_DATE","CMP_NAME"),all.x = TRUE)
      colnames(trap.harvest.chronology)[ncol(trap.harvest.chronology)] <- "Take"

      #Ensure NAs in Take are 0
      tmp<-trap.harvest.chronology$Take
      tmp[is.na(tmp)] <- 0
      trap.harvest.chronology$Take <- tmp

      #Merge trap events to minimize orfen take
      #trap.harvest.chronology <- merge.trap.events(trap.harvest.chronology,event.time.threshold=event.time.threshold)

    }

  } #Logical Assessment for trap method
  close(pb)

  return(trap.harvest.chronology)

}#END FUNCTION





#par(mfrow=c(2,2))

#hist(threshold.mat[,2],breaks=2000, xlim=c(0,50))
#hist(threshold.mat[,3],breaks=1000, xlim=c(0,50))
#hist(threshold.mat[,4],breaks=1000, xlim=c(0,50))
#hist(threshold.mat[,5],breaks=200)



