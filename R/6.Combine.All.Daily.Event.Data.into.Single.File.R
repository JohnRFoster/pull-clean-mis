

rm(list=ls())

#----Set Write Paths----
write.path<-"data/processed"

#----get correct data pull----
pull.date <- config::get("pull.date")

##----SNARE
dat.snare<-read.csv(file.path(write.path, paste0("feral.swine.effort.take.snare.ALL.daily.", pull.date,".csv")))

dat.snare<-dat.snare[is.na(dat.snare$AGRP_PRP_ID)==FALSE,]
nrow(dat.snare)

#####This scaling needs some revisiting

dat.snare$CMP.Qty <- dat.snare$trap.nights / dat.snare$event.length
dat.snare$HOURS <- dat.snare$event.length*24
dat.snare$CMP.Days <- dat.snare$trap.nights
dat.snare$CMP.Hours <- dat.snare$trap.nights*24
dat.snare$WT_WORK_DATE <- dat.snare$start.date

dat.snare<-dat.snare[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID",
                        "ST_NAME","CNTY_NAME","ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD","FIPS","WT_WORK_DATE",
                        "start.date","end.date","TOTAL.LAND",
                        "CMP_NAME","CMP.Qty","HOURS","CMP.Hours","CMP.Days","Take")]
colnames(dat.snare)<-tolower(colnames(dat.snare))

nrow(dat.snare)







##----TRAP
dat.trap<-read.csv(file.path(write.path, paste0("feral.swine.effort.take.trap.ALL.daily.events.",pull.date,".csv")))
dat.trap<-dat.trap[is.na(dat.trap$AGRP_PRP_ID)==FALSE,]
nrow(dat.trap)

dat.trap$CMP.Qty <- dat.trap$trap.count
dat.trap$HOURS <- dat.trap$event.length*24
dat.trap$CMP.Days <- dat.trap$trap.nights
dat.trap$CMP.Hours <- dat.trap$trap.nights*24
dat.trap$WT_WORK_DATE <- dat.trap$start.date

dat.trap<-dat.trap[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID",
                      "ST_NAME","CNTY_NAME","ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD","FIPS","WT_WORK_DATE",
                      "start.date","end.date","TOTAL.LAND",
                      "CMP_NAME","CMP.Qty","HOURS","CMP.Hours","CMP.Days","Take")]
colnames(dat.trap)<-tolower(colnames(dat.trap))

nrow(dat.trap)




##----FIREARMS
dat.firearms<-read.csv(file.path(write.path, paste0("feral.swine.effort.take.firearms.ALL.daily.",pull.date,".csv")))
dat.firearms<-dat.firearms[is.na(dat.firearms$AGRP_PRP_ID)==FALSE,]
nrow(dat.firearms)

dat.firearms$Start.Date <- dat.firearms$WT_WORK_DATE
dat.firearms$End.Date <- dat.firearms$WT_WORK_DATE

dat.firearms[,"unk.prp.event.id"] <-seq(1,nrow(dat.firearms),1)
dat.firearms$CMP.Qty <- dat.firearms$FIREARMS
dat.firearms$CMP.Days <- dat.firearms$Hunt.Days
dat.firearms$CMP.Hours <- dat.firearms$Hunt.Hours

colnames(dat.firearms)[which(colnames(dat.firearms) %in% c("ST_FIPS","CNTY_FIPS"))] <-c("ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD")

dat.firearms<- dat.firearms[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID",
                               "ST_NAME","CNTY_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD","FIPS","WT_WORK_DATE",
                               "Start.Date","End.Date","TOTAL.LAND",
                               "CMP_NAME","CMP.Qty","HOURS","CMP.Hours","CMP.Days","Take")]
colnames(dat.firearms)<-tolower(colnames(dat.firearms))

nrow(dat.firearms)



##----AERIAL
dat.aerial<-read.csv(file.path(write.path, paste0("feral.swine.effort.take.aerial.ALL.daily.",pull.date,".csv")))
dat.aerial<-dat.aerial[is.na(dat.aerial$AGRP_PRP_ID)==FALSE,]
nrow(dat.aerial)

dat.aerial$CMP.Qty <- dat.aerial$VEHICLES
dat.aerial$CMP.Days <- dat.aerial$Flight.Days
dat.aerial$CMP.Hours <- dat.aerial$Flight.Hours
dat.aerial$WT_WORK_DATE <- dat.aerial$Start.Date

dat.aerial<- dat.aerial[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID",
                           "ST_NAME","CNTY_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD","FIPS","WT_WORK_DATE",
                           "Start.Date","End.Date","TOTAL.LAND",
                           "CMP_NAME","CMP.Qty","HOURS","CMP.Hours","CMP.Days","Take")]
colnames(dat.aerial)<-tolower(colnames(dat.aerial))

nrow(dat.aerial)



ncol(dat.aerial)
ncol(dat.firearms)
ncol(dat.trap)
ncol(dat.snare)


##----MERGE ALL INTO SINGLE FILE
All.Methods<-rbind.data.frame(dat.aerial, dat.firearms, dat.trap, dat.snare)

All.Methods$end.date<-as.Date(All.Methods$end.date)
All.Methods$start.date<-as.Date(All.Methods$start.date)


colnames(All.Methods)[which(colnames(All.Methods) %in% "total.land")]<-"property.size"

sum(c(
  nrow(dat.aerial),
  nrow(dat.trap),
  nrow(dat.snare),
  nrow(dat.firearms)
))

nrow(All.Methods)

All.Methods<-All.Methods[complete.cases(All.Methods),]
nrow(All.Methods)

write.csv(All.Methods, file.path(write.path, paste0("MIS.Effort.Take.All.Methods.Daily.Events.",pull.date,".csv")))
nrow(All.Methods)

#----END









