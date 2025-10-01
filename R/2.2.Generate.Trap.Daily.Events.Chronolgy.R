
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

# Restrict to properties to those with feral swine and exclude potential bias in trapping
dat.Agr<-dat.Agr[dat.Agr$DA_NAME_TYPE %!in% c("small mammal","rodent"),]

dat.Agr<-aggregate(PRPS_QTY ~ AGRP_PRP_ID+ALWS_AGRPROP_ID+ALWS_DA_ID+PRP_NAME+ST_NAME+ST_GSA_STATE_CD+CNTY_NAME+CNTY_GSA_CNTY_CD+PRPS_PROP_TYPE, data=dat.Agr, FUN=max)

# Add unique id
dat.Agr$unk.id <- paste0(dat.Agr$AGRP_PRP_ID,".",dat.Agr$ALWS_AGRPROP_ID)
dat.PropKill$unk.id <- paste0(dat.PropKill$AGRP_PRP_ID,".",dat.PropKill$ALWS_AGRPROP_ID)
dat.Eff$unk.id <- paste0(dat.Eff$AGRP_PRP_ID,".",dat.Eff$ALWS_AGRPROP_ID)
dat.Kill$unk.id <- paste0(dat.Kill$AGRP_PRP_ID,".",dat.Kill$ALWS_AGRPROP_ID)
lut.property.acres$unk.id <- paste0(lut.property.acres$AGRP_PRP_ID,".",lut.property.acres$ALWS_AGRPROP_ID)

#Limit based on species lut
dat.PropKill <- dat.PropKill[dat.PropKill$unk.id %in% unique(dat.Agr$unk.id),]
dat.Eff <- dat.Eff[dat.Eff$unk.id %in% unique(dat.Agr$unk.id),]
dat.Kill <- dat.Kill[dat.Kill$unk.id %in% unique(dat.Agr$unk.id),]
lut.property.acres <- lut.property.acres[lut.property.acres$unk.id %in% unique(dat.Agr$unk.id),]

##----END DATA PREP----




#-----------------------------
#----Generate trap effort ----


#-- Remove properties with rodent control

#Set CMP names associated with rodent control
cmp.vec<-c("RAMIK MINI BARS :HI-9 (ANTICOAG)","TRAPS, SNAP (RAT, MOUSE, ETC.)")

prp.vec<-unique(dat.Eff[dat.Eff$CMP_NAME %in% cmp.vec,"AGRP_PRP_ID"])

trap.dat<-dat.Eff[dat.Eff$AGRP_PRP_ID %!in% prp.vec,]




#--Restrict to those with trapping

#Generate trap type list to process
trap.vec <- c("TRAPS, LIVE, FERAL HOGS","TRAPS, CAGE","TRAPS, CORRAL")

#Limit Trap Data
trap.dat<-trap.dat[trap.dat$CMP_NAME %in% trap.vec,]

#Remove those with WTCM_QTY > max WTCM_QTY for Corral Traps

corral.max <- max(trap.dat[trap.dat$CMP_NAME=="TRAPS, CORRAL","WTCM_QTY"])

trap.dat <- trap.dat[trap.dat$WTCM_QTY < corral.max, ]

#Convert all trap types to the same type
trap.dat[,"CMP_NAME"] <- "TRAPS, CAGE"








#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

#Read in Harvest Chronology
file.name <- file.path(write.path,paste0("feral.swine.effort.take.traps.chronology.limited.ALL.",pull.date,".csv"))
trap.harvest.chronology<-read.csv(file.name,stringsAsFactors=FALSE)
trap.harvest.chronology <- trap.harvest.chronology[,-1]
nrow(trap.harvest.chronology)

trap.harvest.chronology$WT_WORK_DATE <- as.Date(as.character(trap.harvest.chronology$WT_WORK_DATE,"%Y-%m-%d"))

#Adjust for Daily Trapping Summary
trap.harvest.chronology <- calc.days.between.records(trap.harvest.chronology)
nrow(trap.harvest.chronology)

trap.harvest.chronology <- calc.start.stop.by.record(trap.harvest.chronology, adjustment=0)
nrow(trap.harvest.chronology)

trap.harvest.chronology <- add.within.event.id(trap.harvest.chronology)
nrow(trap.harvest.chronology)

#tmp <- trap.harvest.chronology[trap.harvest.chronology$within.id!=1,]

#Calculate days active
trap.harvest.chronology$days.active <- trap.harvest.chronology$within.event.end.date - trap.harvest.chronology$within.event.str.date

#Assume first day active is 1
trap.harvest.chronology[trap.harvest.chronology$days.active==0,"days.active"] <- 1

#Calculate trap nights
trap.harvest.chronology$trap.nights <- as.numeric(trap.harvest.chronology$days.active) * trap.harvest.chronology$trap.count

#Remake Unique ID
trap.harvest.chronology$event.id <- paste0(trap.harvest.chronology$event.id,".",trap.harvest.chronology$within.id)

#Reorder
trap.harvest.chronology<-trap.harvest.chronology[order(-trap.harvest.chronology$AGRP_PRP_ID,trap.harvest.chronology$WT_WORK_DATE),]

trap.harvest.chronology<-trap.harvest.chronology[trap.harvest.chronology$trap.count!=0,]

#Remake unique ID
trap.harvest.chronology$unk.prp.event.id <- paste0(trap.harvest.chronology$AGRP_PRP_ID,"-",trap.harvest.chronology$event.id)

#Reorder things
agg.out.dat <- trap.harvest.chronology[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME","within.event.str.date","within.event.end.date","days.active","trap.count","trap.nights","Take")]
colnames(agg.out.dat) <- c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME","start.date","end.date","event.length","trap.count","trap.nights","Take")


#----Merge County location data

#Generate final data
#lut.property.acres <- unique(lut.property.acres)

final.agg.out.dat <- merge(agg.out.dat, lut.property.acres, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID"),all.x=TRUE)
final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","ST_NAME","CNTY_NAME", "ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD","FIPS", "TOTAL.LAND","CMP_NAME", "start.date","end.date", "trap.count","event.length", "trap.nights", "Take")]
final.agg.out.dat <- final.agg.out.dat[order(-final.agg.out.dat$AGRP_PRP_ID, final.agg.out.dat$start.date),]

nrow(final.agg.out.dat)
head(final.agg.out.dat)

#Remove events with zero trap nights
non.zero.lut <- rownames(final.agg.out.dat[final.agg.out.dat$trap.nights!=0,])

#Limit to those with non-zero trap nights
final.agg.out.dat <- final.agg.out.dat[rownames(final.agg.out.dat) %in% non.zero.lut,]
nrow(final.agg.out.dat)

#Limit Event Length
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$event.length<90,]
nrow(final.agg.out.dat)

#Limit to only those with acreage
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$TOTAL.LAND>0,]
nrow(final.agg.out.dat)

#Remove NA values
final.agg.out.dat<-final.agg.out.dat[complete.cases(final.agg.out.dat$AGRP_PRP_ID),]
nrow(final.agg.out.dat)
head(final.agg.out.dat)


#----Write Data
write.csv(final.agg.out.dat, file.path(write.path,paste0("feral.swine.effort.take.trap.ALL.daily.events.",pull.date,".csv")))
##----END----##





##---- MAKE PLOTS ----
par(mfrow=c(2,2))

hist(final.agg.out.dat$Take, xlab="Take", breaks=20,main=NULL)

summary(final.agg.out.dat$Take)

plot(log(final.agg.out.dat$TOTAL.LAND),final.agg.out.dat$Take,xlab="log Property Size",ylab="Take")
plot(log(final.agg.out.dat$TOTAL.LAND),log(final.agg.out.dat$trap.nights),xlab="log Property Size",ylab="log Trap Nights")
abline(a=0,b=1,col="red")
#plot(log(final.agg.out.dat$TOTAL.LAND),final.agg.out.dat$event.length,xlab="log Property Size",ylab="Event Length")
plot(log(final.agg.out.dat$trap.nights),log(final.agg.out.dat$Take),xlab="log Trap Nights",ylab="log Take")
abline(a=0,b=1,col="red")

dev.off()
plot(final.agg.out.dat$event.length,final.agg.out.dat$Take)




tmp<-trap.harvest.chronology[trap.harvest.chronology$AGRP_PRP_ID==95314,]

tmp<-tmp[order(-tmp$AGRP_PRP_ID,tmp$WT_WORK_DATE),]

tmp[tmp$event.id %in% c(80.1,80.2,80.3,80.4),]





















agg.out.dat <- aggregate(cbind(trap.nights,Take)~AGRP_PRP_ID+event.id+CMP_NAME, data=trap.harvest.chronology, FUN=sum)
agg.out.dat <- agg.out.dat[order(agg.out.dat$AGRP_PRP_ID, agg.out.dat$event.id),]
nrow(agg.out.dat)

#----Determine uncertainity

#Determine Trap count at end of trapping
date.lut <- calc.event.length(trap.harvest.chronology)
tmp.merge <- merge(date.lut, trap.harvest.chronology, by=c("AGRP_PRP_ID","event.id","WT_WORK_DATE","CMP_NAME"),all.x=TRUE)
tmp.merge <- tmp.merge[,c("AGRP_PRP_ID", "event.id", "WT_WORK_DATE", "CMP_NAME", "trap.count.event")]
tmp.merge <- unique(tmp.merge)

#If Traps are zeroed out = high; if traps left = moderate; if traps negative = low
certainty.flag <- tmp.merge[,"trap.count.event"]
certainty.flag[certainty.flag>0] <- "Moderate"
certainty.flag[certainty.flag==0] <- "High"
certainty.flag[certainty.flag<0] <- "low"

tmp.merge$trap.night.certainty <- certainty.flag

agg.out.dat<-merge(agg.out.dat,tmp.merge,by=c("AGRP_PRP_ID","event.id","CMP_NAME"), all.x=TRUE)


#Check number of rows
nrow(agg.out.dat);nrow(date.lut);length(certainty.flag)

#Merge data
date.lut <- subset(date.lut, select=-c(WT_WORK_DATE))

agg.out.dat<-merge(agg.out.dat,date.lut,by=c("AGRP_PRP_ID","event.id","CMP_NAME"), all.x=TRUE)

#Reorder things
agg.out.dat <- agg.out.dat[,c("AGRP_PRP_ID","event.id","CMP_NAME","start.date","end.date","event.length","trap.nights","Take","trap.night.certainty")]


#----Merge County location data

#Generate final data
final.agg.out.dat <- merge(agg.out.dat, lut.property.acres, by="AGRP_PRP_ID",all.x=TRUE)
final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","event.id","ST_NAME","CNTY_NAME", "ST_GSA_STATE_CD",
                                          "CNTY_GSA_CNTY_CD", "FIPS", "COUNTY.OR.CITY.LAND","MILITARY.LAND",
                                          "PRIVATE.LAND","STATE.LAND","TRIBAL.LAND","TOTAL.LAND",
                                          "CMP_NAME", "start.date","end.date", "event.length",
                                          "trap.nights", "Take", "trap.night.certainty")]
final.agg.out.dat <- final.agg.out.dat[order(final.agg.out.dat$AGRP_PRP_ID,final.agg.out.dat$event.id),]

nrow(final.agg.out.dat)
head(final.agg.out.dat)

#Remove events with zero trap nights
non.zero.lut <- rownames(final.agg.out.dat[final.agg.out.dat$trap.nights!=0,])

#Limit to those with non-zero trap nights
final.agg.out.dat <- final.agg.out.dat[rownames(final.agg.out.dat) %in% non.zero.lut,]
nrow(final.agg.out.dat)
head(final.agg.out.dat)

#Limit to high and moderate certainity
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$trap.night.certainty!="low",]
nrow(final.agg.out.dat)
head(final.agg.out.dat)

#Limit Event Length
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$event.length<90,]
nrow(final.agg.out.dat)

#Limit to only those with acreage
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$TOTAL.LAND>1,]
nrow(final.agg.out.dat)

final.agg.out.dat<-final.agg.out.dat[complete.cases(final.agg.out.dat$AGRP_PRP_ID),]
nrow(final.agg.out.dat)

#----Write Data
write.csv(final.agg.out.dat, file.path(write.path,paste0("feral.swine.effort.take.traps.ALL",pull.date,".csv")))
write.csv(trap.harvest.chronology, file.path(write.path,paste0("feral.swine.effort.take.traps.chronology.ALL",pull.date,".csv")))

##----END----##
