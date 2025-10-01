
rm(list=ls())

#----Set Write Paths----
write.path<-"data/processed"


#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(modeest)
library(readr)
library(operators)

#----Required Functions
source("R/FNC.MIS.Pre.Process.R")
source("R/FNC.MIS.calc.trap.effort.R")
source("R/FNC.MIS.calc.days.elapsed.R")
source("R/FNC.MIS.calc.trap.chronology.R")
source("R/FNC.Misc.Utilities.R")

#----Prep Data ----

#----get correct data pull----
pull.date <- config::get("pull.date")

# Read data
dat.Agr <- read_csv(file.path(write.path, paste0("processed_fs_national_property_", pull.date, ".csv")))
dat.Kill <- read_csv(file.path(write.path, paste0("processed_fs_national_take_by_method_", pull.date, ".csv")))
dat.Eff <- read_csv(file.path(write.path, paste0("processed_fs_national_effort_", pull.date, ".csv")))
dat.PropKill <- read_csv(file.path(write.path, paste0("processed_fs_national_take_by_property_", pull.date, ".csv")))
lut.property.acres <- read_csv(file.path(write.path, "processed_lut_property_acres.csv"))


##----END DATA PREP----



#--Subset Data to Only Ground Hunting Using FireArms

#--Identify only those events that used firearms
firearms.vec <- c("FIREARMS")

tmp<-dat.Eff[dat.Eff$CMP_NAME %in% firearms.vec,]

unk.events <- unique(tmp[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE")])

unk.events <- cbind.data.frame(unk.events,firearms.used=rep("firearms",nrow(unk.events)))

#Add unique event ID
unk.events$event.id <- seq(1, nrow(unk.events), 1)

tmp<-merge(dat.Eff, unk.events, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE"), all.y=TRUE)
head(tmp);nrow(tmp)

#Determine Events that Are Ground Hunting
plyr::count(tmp$CMP_NAME)

cmp.name <- c("SPOTLIGHT","CALLING DEVICE, MANUAL(HAND,BLOWN)","NIGHT VISION/INFRARED EQUIPMENT",
               "CALLING DEVICE, ELECTRONIC","BAIT STATION","MONITORING CAMERA","CAR/TRUCK","TELEMETRY EQUIPMENT",
               "BAIT STATION")

event.vec<-tmp[tmp$CMP_NAME %in% cmp.name,"event.id"]

firearms.associated<-tmp[tmp$event.id %in% event.vec,]
nrow(firearms.associated)

#Remove any events associated with other methods
cmp.name <- c("FIXED WING","HELICOPTER","SNARES, FOOT/LEG","SNARES, NECK",
                        "SNARES, NECK MECHANICAL (COLLARUM)","TRAPS, BODY GRIP",
                        "TRAPS, BODY GRIP","TRAPS, CAGE","TRAPS, CORRAL","TRAPS, DECOY",
                        "TRAPS, FOOTHOLD","TRAPS, FOOTHOLD (PADDED)","TRAPS, FOOTHOLD DOG PROOF",
                        "TRAPS, LIVE, FERAL HOGS","TRAPS, OTHER","TRAPS, RAPTOR (OTHER)",
                        "TRAPS, RAPTOR (SWEDISH GOSHAWK)","M-44 CYANIDE CAPSULE")

event.vec<-firearms.associated[firearms.associated$CMP_NAME %in% cmp.name,"event.id"]

firearms.associated<-firearms.associated[firearms.associated$event.id %!in% event.vec,]

nrow(firearms.associated)
head(firearms.associated)
plyr::count(firearms.associated$CMP_NAME)

#Reset CMP_NAME
firearms.associated[firearms.associated$CMP_NAME!="FIREARMS","CMP_NAME"]<-"OTHER"
plyr::count(firearms.associated$CMP_NAME)

#Set CMP_NAME = other to 0 for counting purposes
firearms.associated[firearms.associated$CMP_NAME=="OTHER","WTCM_QTY"]<-0

#Set WTM_QTY = 0 using UOM_NAME
uom.name <- c("HOURS","MINUTES","ATV HOURS", "ATV DAY","DRIVE HOURS", "DOG DAY")

firearms.associated[firearms.associated$UOM_NAME %!in% uom.name,"WTM_QTY"]<-0

plyr::count(firearms.associated[,c("UOM_NAME","WTM_QTY")])

tmp.dat<-firearms.associated

#--END Restrict Data


#--Convert to minutes to hours
tmp.dat[tmp.dat$UOM_NAME=="MINUTES","WTM_QTY"] <- tmp.dat[tmp.dat$UOM_NAME=="MINUTES","WTM_QTY"] /60
tmp.dat[tmp.dat$UOM_NAME=="MINUTES","UOM_NAME"] <- "HOURS"

count(tmp.dat$CMP_NAME);count(tmp.dat$CMP_TYPE);count(tmp.dat$USET_NAME)

#--Remove implosable values
summary(tmp.dat$WTM_QTY)
summary(tmp.dat$WTCM_QTY)

#Set USET_NAME = DISCHARGED to 0
tmp.dat[tmp.dat$USET_NAME=="DISCHARGED","WTCM_QTY"] <- 0



#--Rework duplicate hours in WTM_QTY

#Drop unneeded columns
tmp.dat<-tmp.dat[,colnames(tmp.dat) %!in% c("CMP_TYPE","X")]

#Remove easy duplicates
tmp.dat<-unique(tmp.dat)

event.vec<-unique(tmp.dat$event.id)

#--Loop over unique events
for(i in 1:length(event.vec)){
  tmp<-tmp.dat[tmp.dat$event.id==event.vec[i],]

  fire.vec<-tmp[tmp$CMP_NAME=="FIREARMS","WTM_QTY"]

  other.vec<-tmp[tmp$CMP_NAME=="OTHER","WTM_QTY"]

  other.vec[other.vec %in% fire.vec] <- 0

  tmp.dat[tmp.dat$event.id==event.vec[i] & tmp.dat$CMP_NAME=="OTHER","WTM_QTY"] <- other.vec
}#END Loop

#--END Rework duplicate hours



#-- Adjust number of firearms
#tmp.dat<-adjust.firearm.data(tmp.dat,thershold=5)


#Aggregate time
tmp <-aggregate(cbind(WTM_QTY)~ALWS_AGRPROP_ID+AGRP_PRP_ID+CMP_NAME+WT_WORK_DATE, data=tmp.dat, FUN=sum)

tmp.time<-spread(tmp, CMP_NAME,WTM_QTY)

tmp.time$HOURS <- tmp.time$FIREARMS + tmp.time$OTHER

tmp.time[tmp.time$FIREARMS==tmp.time$OTHER,"HOURS"] <- tmp.time[tmp.time$FIREARMS==tmp.time$OTHER,"FIREARMS"]

#Aggegate # firearms
tmp <-aggregate(cbind(WTCM_QTY)~ALWS_AGRPROP_ID+AGRP_PRP_ID+WT_WORK_DATE, data=tmp.dat, FUN=sum)

tmp<-merge(tmp.time, tmp, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE"), all.x=TRUE, all.y=TRUE)

#Ensure data is ordered
wide.data <- tmp[order(tmp$AGRP_PRP_ID, tmp$WT_WORK_DATE),, drop=FALSE]

#Drop those with WTCM = 0
wide.data <- wide.data[wide.data$WTCM_QTY!=0,]

#--Rename columns
colnames(wide.data)[which(colnames(wide.data)=="FIREARMS")] <- "WTM.FIREARMS"
colnames(wide.data)[which(colnames(wide.data)=="OTHER")] <- "WTM.OTHER"
colnames(wide.data)[which(colnames(wide.data)=="WTCM_QTY")] <- "FIREARMS"

#--Add CMP_NAME
wide.data$CMP_NAME <- "FIREARMS"

in.dat<-wide.data
##END


#----Add Event Ids


#Generate Take by property
kill.by.prop<-dat.PropKill[dat.PropKill$CMP_NAME %in% c("FIREARMS"),]
kill.by.prop<-unique(kill.by.prop[,c("ALWS_AGRPROP_ID","AGRP_PRP_ID","ST_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD","WTCM_QTY","CMP_NAME","WKR_QTY","WT_WORK_DATE")])
kill.by.prop<-kill.by.prop[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE","CMP_NAME","WKR_QTY")]
colnames(kill.by.prop)[ncol(kill.by.prop)] <- "Take"

#Merge trap chronology and take data
harvest.chronology <- merge(in.dat, kill.by.prop, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE","CMP_NAME"),all.x = TRUE)
harvest.chronology[is.na(harvest.chronology$Take),"Take"]<-0

harvest.chronology[order(-harvest.chronology$Take),]


#--Assume when Firearms = Take and firearms > 1 then assume WTCM_QTY = number of shots and assign 1 firearm
harvest.chronology[harvest.chronology$FIREARMS == harvest.chronology$Take & harvest.chronology$FIREARMS>1,"FIREARMS"] <- 1


#----Calculate Hunt Hours and Hunt Days
harvest.chronology$Hunt.Hours <- harvest.chronology$HOURS * harvest.chronology$FIREARMS
harvest.chronology$Hunt.Days <- (harvest.chronology$Hunt.Hours/24)

#--Limit to events with hours < 24
harvest.chronology <- harvest.chronology[harvest.chronology$HOURS<24,]


#--Plot
hist(harvest.chronology$Take,breaks=300,xlim=c(0,50))
summary(harvest.chronology$Take)

plot(log(harvest.chronology$Hunt.Days),harvest.chronology$Take)
plot(harvest.chronology$Hunt.Days,harvest.chronology$Take)


#Remove Implosible Data
#harvest.chronology<-harvest.chronology[harvest.chronology$Take<40,]


#----Merge County location data

#Generate final data
final.agg.out.dat <- merge(harvest.chronology, lut.property.acres, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID"),all.x=TRUE)
final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE", "ST_NAME","CNTY_NAME", "ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD","FIPS","TOTAL.LAND",
                                              "CMP_NAME","HOURS","FIREARMS","Hunt.Hours","Hunt.Days","Take")]
final.agg.out.dat <- final.agg.out.dat[order(final.agg.out.dat$AGRP_PRP_ID, final.agg.out.dat$WT_WORK_DATE),]
nrow(final.agg.out.dat)

#Limit to only those with acreage
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$TOTAL.LAND>0,]
nrow(final.agg.out.dat)

final.agg.out.dat<-final.agg.out.dat[is.na(final.agg.out.dat$AGRP_PRP_ID)==FALSE,]
nrow(final.agg.out.dat)

#--Add unk id
unk.prp.event.id <- paste0(final.agg.out.dat$AGRP_PRP_ID,".",seq(1,nrow(final.agg.out.dat),1))

#--Reorder
final.agg.out.dat <- data.frame(append(final.agg.out.dat, list(unk.prp.event.id=unk.prp.event.id), after=match("ALWS_AGRPROP_ID", names(final.agg.out.dat))))

#----Write Data
write.csv(final.agg.out.dat, file.path(write.path,paste0("feral.swine.effort.take.firearms.ALL.daily.",pull.date,".csv")))
##----END----##










missing.agrp.id<-unique(harvest.chronology$AGRP_PRP_ID[harvest.chronology$AGRP_PRP_ID %!in% lut.property.acres$AGRP_PRP_ID])

write.csv(missing.agrp.id, file.path(write.path, paste0("missing.agrp.id.", pull.date, ".csv")), row.names=FALSE)



dat.Eff[dat.Eff$AGRP_PRP_ID=="285573" & dat.Eff$WT_WORK_DATE=="2008-06-16",]














