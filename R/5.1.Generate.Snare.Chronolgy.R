#----------------------------------
#
# Generate chronology of activity by property
#
# BETA Version
#
# Ryan Miller, John Foster
#----------------------------------

rm(list=ls())
gc()

#----Set Write Paths----
write.path<-"data/processed"

#----get correct data pull----
pull.date <- config::get("pull.date")

#----Load Libraries----
library(reshape2)
library(tidyr)
library(readr)
library(modeest)
library(plyr)
library(dplyr)
library(operators)

#----Required Functions
source("R/FNC.MIS.Pre.Process.R")
source("R/FNC.MIS.calc.trap.effort.R")
source("R/FNC.MIS.calc.days.elapsed.R")
source("R/FNC.MIS.calc.trap.chronology.R")
source("R/FNC.Misc.Utilities.R")
source("R/FNC.MIS.assign.orphen.events.R")
source("R/FNC.MIS.merge.trap.events.R")

#----Prep Data ----

# Read data
dat.Agr <- read_csv(file.path(write.path, paste0("processed_fs_national_property_", pull.date, ".csv")))
dat.Kill <- read_csv(file.path(write.path, paste0("processed_fs_national_take_by_method_", pull.date, ".csv")))
dat.Eff <- read_csv(file.path(write.path, paste0("processed_fs_national_effort_", pull.date, ".csv")))
dat.PropKill <- read_csv(file.path(write.path, paste0("processed_fs_national_take_by_property_", pull.date, ".csv")))
lut.property.acres <- read_csv(file.path(write.path, "processed_lut_property_acres.csv"))

# Restrict to properties to those with feral swine and exclude potential bias in snaring
dat.Agr<-dat.Agr[dat.Agr$DA_NAME_TYPE %!in% c("small mammal","predator"),]

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



#----Subset for testing ----

#in.dat<-dat.Eff[dat.Eff$AGRP_PRP_ID==314856,]
#in.dat<-in.dat[in.dat$CMP_NAME=="TRAPS, CAGE",]
#in.dat <- in.dat[order(in.dat$WT_WORK_DATE,in.dat$USET_NAME),]


#in.dat<-dat.Eff[dat.Eff$AGRP_PRP_ID==314882,]
#in.dat<-in.dat[in.dat$CMP_NAME=="TRAPS, LIVE, FERAL HOGS",]
#in.dat <- in.dat[order(in.dat$WT_WORK_DATE,in.dat$USET_NAME),]



#-----------------------------
#----Generate trap effort ----


#Remove those with WTCM_QTY > max WTCM_QTY in PropKill
trap.vec <- c("SNARES, NECK","SNARES, FOOT/LEG","TRAPS, FOOTHOLD","TRAPS, BODY GRIP","SNARES, NECK MECHANICAL (COLLARUM)","TRAPS, FOOTHOLD (PADDED)","TRAPS, FOOTHOLD DOG PROOF")
dat.PropKill<-dat.PropKill[dat.PropKill$CMP_NAME %in% trap.vec,]

max.vals <- aggregate(WTCM_QTY~CMP_NAME, data=dat.PropKill, FUN=max)

trap.dat<-data.frame()

for(i in 1:nrow(max.vals)){
  tmp<-dat.Eff[dat.Eff$CMP_NAME==max.vals[i,"CMP_NAME"] & dat.Eff$WTCM_QTY < max.vals[i,"WTCM_QTY"], ]
  trap.dat<-rbind.data.frame(trap.dat,tmp)
}#END Loop

#Plot WTCM by CMP Name
library(ggplot2)
p <- ggplot(data = trap.dat, aes(x = WTCM_QTY)) + geom_histogram(binwidth = 1)
p + facet_wrap(~CMP_NAME, scales = "free_y")

#Convert all trap types to the same type
trap.dat[,"CMP_NAME"] <- "SNARE"
dat.PropKill[,"CMP_NAME"] <- "SNARE"


#--Make Chronology using course thershold
chronology.course<-trap.chronology(trap.dat,event.time.threshold=10,use.mlv.thershold=TRUE,use.stat.fudge=TRUE, fudge.user=5)
#chronology.course<-merge.trap.events(chronology.course,event.time.threshold=25,max.time=40)
chronology.course<-assign.orphen.events(chronology.course, max.time=10)

chronology.course$unk.prp.event.id <- paste0(chronology.course$AGRP_PRP_ID,"-",chronology.course$event.id)
date.lut <- calc.event.length(chronology.course)

#Determine Length
hist(as.numeric(date.lut$event.length),breaks=300)
mean(date.lut$event.length);median(date.lut$event.length);mean(date.lut$event.length)+sd(date.lut$event.length)
#Use 5 days as cut point for below




#----Make Chronology using fine thershold----
property.vec <- unique(date.lut[date.lut$event.length>5,"AGRP_PRP_ID"])

new.dat <- trap.dat[trap.dat$AGRP_PRP_ID %in% property.vec,]
chronology.fine<-trap.chronology(new.dat,event.time.threshold=5,use.mlv.thershold=TRUE,use.stat.fudge=TRUE, fudge.user=1)
chronology.fine<-assign.orphen.events(chronology.fine, max.time=5)

chronology.fine$event.id<-chronology.fine$event.id+1000

chronology.fine$unk.prp.event.id <- paste0(chronology.fine$AGRP_PRP_ID,"-",chronology.fine$event.id)

#--Reconstruct
chronology.course.adj <- chronology.course[chronology.course$AGRP_PRP_ID %not in% property.vec,]
trap.harvest.chronology <- rbind(chronology.course.adj,chronology.fine)

#trap.harvest.chronology<-trap.harvest.chronology[order(-trap.harvest.chronology$AGRP_PRP_ID,trap.harvest.chronology$WT_WORK_DATE),]

trap.harvest.chronology<-assign.orphen.events(trap.harvest.chronology, max.time=8)
trap.harvest.chronology$unk.prp.event.id <- paste0(trap.harvest.chronology$AGRP_PRP_ID,"-",trap.harvest.chronology$event.id)
nrow(trap.harvest.chronology)
nrow(chronology.fine)+nrow(chronology.course.adj)

#trap.harvest.chronology$unk.prp.event.id <- paste0(trap.harvest.chronology$AGRP_PRP_ID,"-",trap.harvest.chronology$event.id)
date.lut <- calc.event.length(trap.harvest.chronology)
##----END----




#----Assign Unassigned Orphens events trap nights----
cnt.1<-plyr::count(trap.harvest.chronology[,c("AGRP_PRP_ID","unk.prp.event.id")])
cnt.1[cnt.1$freq==1,"Orphen.Flag"] <- "Orphen"
cnt.1<-cnt.1[,c("AGRP_PRP_ID","unk.prp.event.id","Orphen.Flag")]
cnt.1[is.na(cnt.1$Orphen.Flag),"Orphen.Flag"]<-"Not Orphen"
plyr::count(cnt.1$Orphen.Flag)
#count(cnt.1[,c("AGRP_PRP_ID","Orphen.Flag")])

#Investigate results
trap.harvest.chronology<-merge(trap.harvest.chronology,cnt.1, by=c("AGRP_PRP_ID","unk.prp.event.id"),all.x=TRUE)
mlv(trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen","day.diff"], method = "shorth")
mean(trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen","day.diff"])
median(trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen","day.diff"])

#trap.harvest.chronology<-trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Not Orphen",]
#trap.harvest.chronology <- trap.harvest.chronology[,-ncol(trap.harvest.chronology)]
#----END----




#----Remove Trapping events that are 1 day in length
#events.1day <- date.lut[date.lut$start.date==date.lut$end.date,c("unk.prp.event.id")]
#nrow(trap.harvest.chronology)

#trap.harvest.chronology<-trap.harvest.chronology[trap.harvest.chronology$unk.prp.event.id %not in% events.1day,]
#nrow(trap.harvest.chronology)
#----END----




#----Break up long events----

trap.harvest.chronology <- break.up.long.events(trap.harvest.chronology, long.event.thershold=5)

#----END----

##----Set Start and End dates----

trap.harvest.chronology <- set.start.and.end.dates(trap.harvest.chronology)

#----END----

#----Recalculate Trap Nights----

#Assume difference in days that are 0 than they are 1 day in length
#trap.harvest.chronology[trap.harvest.chronology$day.diff==0,"day.diff"] <- 1

tmp.vec <- rowSums(trap.harvest.chronology[trap.harvest.chronology$trap.count==0,c("SET","CHECKED","RESET")])
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,"trap.count"]<-tmp.vec
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,]

tmp.vec <- rowSums(trap.harvest.chronology[trap.harvest.chronology$trap.count==0,c("UNSET","REMOVED")])
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,"trap.count"]<-tmp.vec
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,]

tmp.vec <- trap.harvest.chronology[trap.harvest.chronology$trap.count==0,c("APPLIED.USED")]
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,"trap.count"]<-tmp.vec
trap.harvest.chronology[trap.harvest.chronology$trap.count==0,]

#Set first day to 0
trap.harvest.chronology$days.active <- trap.harvest.chronology$day.diff
trap.harvest.chronology[trap.harvest.chronology$event.type=="Event Start","days.active"] <- 0

##--Deal with Orphens

#Assume Orphens with 0 days are 1 day of effort
trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen" & trap.harvest.chronology$day.diff==0,"days.active"] <- 1

#Assume Orphens with less than 5 days difference is the trap nights for event
day.diff.vec<-trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen" & trap.harvest.chronology$day.diff<=5 & trap.harvest.chronology$day.diff>0,"day.diff"]
trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen" & trap.harvest.chronology$day.diff<=5 & trap.harvest.chronology$day.diff>0,"days.active"] <- day.diff.vec

#Assume Orphens with >5 days difference are 1 trap night
trap.harvest.chronology[trap.harvest.chronology$Orphen.Flag=="Orphen" & trap.harvest.chronology$day.diff>=5,"days.active"] <- 1

##--Calc trap nights
trap.harvest.chronology$trap.nights <- trap.harvest.chronology$trap.count * trap.harvest.chronology$days.active
#----END----

date.lut <- calc.event.length(trap.harvest.chronology)

##----END Harvest Chronology----
##------------------------------



##----REMOVE Unreliable Data----
poor.dat <- date.lut[date.lut$information.quaility<0.15,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID")]

if(nrow(poor.dat)>0){
  poor.dat$Drop.Flag <- 1
  trap.harvest.chronology<-merge(trap.harvest.chronology,poor.dat,by=c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID"),all.x=TRUE)

  trap.harvest.chronology<-trap.harvest.chronology[is.na(trap.harvest.chronology$Drop.Flag),]
  trap.harvest.chronology<-trap.harvest.chronology[,-ncol(trap.harvest.chronology)]
}
#----END----



#trap.harvest.chronology[order(-trap.harvest.chronology$trap.nights),]

#trap.harvest.chronology[order(-trap.harvest.chronology$days.active),]







##---- REDO for Those with Long Event Times
#Determine Appropriate Thershold for long events
#property.vec <- unique(date.lut[date.lut$event.length>200,"AGRP_PRP_ID"])

#tmp<-trap.harvest.chronology[trap.harvest.chronology$AGRP_PRP_ID %in% property.vec,]

#hist(tmp$day.diff,breaks=100000,xlim=c(0,50))

#mlv(tmp$day.diff, method = "mfv")
#mean(tmp$day.diff)
#median(tmp$day.diff)

date.lut[order(-date.lut$event.length),]

#tmp<-trap.harvest.chronology[trap.harvest.chronology$trap.count==1,]


#tmp[order(-tmp$trap.nights),]

#trap.harvest.chronology[order(-trap.harvest.chronology$trap.nights),]
par(mfrow=c(2,2))
hist(as.numeric(date.lut$event.length),breaks=100,xlim=c(0,90))
median(as.numeric(date.lut$event.length));mean(as.numeric(date.lut$event.length))

#plot(as.numeric(date.lut$event.length),log(date.lut$information.quaility))
plot(log(as.numeric(date.lut$event.length)),log(date.lut$information.quaility))
abline(h=log(.05),col="red")
abline(h=log(.15),col="blue")
abline(h=log(.4),col="orange")



###### Deal with aggregates and columns to use.



#----Generate summary of trap nights and kill by each trapping event
agg.out.dat <- aggregate(cbind(trap.nights,Take)~AGRP_PRP_ID+unk.prp.event.id+ALWS_AGRPROP_ID+CMP_NAME, data=trap.harvest.chronology, FUN=sum)
agg.out.dat <- agg.out.dat[order(agg.out.dat$AGRP_PRP_ID, agg.out.dat$unk.prp.event.id),]
nrow(agg.out.dat)

#----Determine uncertainity

#Determine Trap count at end of trapping
tmp.merge <- merge(date.lut, trap.harvest.chronology, by=c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","WT_WORK_DATE","CMP_NAME"),all.x=TRUE)
tmp.merge <- tmp.merge[,c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID","WT_WORK_DATE", "CMP_NAME", "trap.count.event")]
tmp.merge <- unique(tmp.merge)

#If Traps are zeroed out = high; if traps left = moderate; if traps negative = low
certainty.flag <- tmp.merge[,"trap.count.event"]
certainty.flag[certainty.flag>0] <- "Moderate"
certainty.flag[certainty.flag==0] <- "High"
certainty.flag[certainty.flag<0] <- "low"

tmp.merge$trap.night.certainty <- certainty.flag

agg.out.dat<-merge(agg.out.dat,tmp.merge,by=c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME"), all.x=TRUE)


#Check number of rows
nrow(agg.out.dat);nrow(date.lut);length(certainty.flag)

#Merge data
date.lut <- subset(date.lut, select=-c(WT_WORK_DATE))

agg.out.dat<-merge(agg.out.dat,date.lut,by=c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME"), all.x=TRUE)

#Reorder things
agg.out.dat <- agg.out.dat[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME","start.date","end.date","event.length","trap.nights","Take","trap.night.certainty")]


#----Merge County location data

#Generate final data
#lut.property.acres <- unique(lut.property.acres)

final.agg.out.dat <- merge(agg.out.dat, lut.property.acres, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID"),all.x=TRUE)
#final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","event.id","ST_NAME","CNTY_NAME", "ST_FIPS", "CNTY_FIPS", "COUNTY.OR.CITY.LAND","MILITARY.LAND","PRIVATE.LAND","STATE.LAND","TRIBAL.LAND","TOTAL.LAND","CMP_NAME", "start.date","end.date", "event.length", "trap.nights", "Take", "trap.night.certainty")]
final.agg.out.dat <- final.agg.out.dat[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID","ST_NAME","CNTY_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD",
                                          "TOTAL.LAND","CMP_NAME", "start.date","end.date", "event.length", "trap.nights", "Take", "trap.night.certainty")]
final.agg.out.dat <- final.agg.out.dat[order(final.agg.out.dat$AGRP_PRP_ID, final.agg.out.dat$unk.prp.event.id),]

nrow(final.agg.out.dat)




#Remove events with zero trap nights
non.zero.lut <- rownames(final.agg.out.dat[final.agg.out.dat$trap.nights!=0,])

#Limit to those with non-zero trap nights
final.agg.out.dat <- final.agg.out.dat[rownames(final.agg.out.dat) %in% non.zero.lut,]
nrow(final.agg.out.dat)

#Limit to high and moderate certainity
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$trap.night.certainty!="low",]
nrow(final.agg.out.dat)

#Limit Event Length
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$event.length<90,]
nrow(final.agg.out.dat)

#Limit to only those with acreage
final.agg.out.dat <- final.agg.out.dat[final.agg.out.dat$TOTAL.LAND>0,]
nrow(final.agg.out.dat)

#----Write Data
write.csv(final.agg.out.dat, file.path(write.path,paste0("feral.swine.effort.take.snare.ALL",pull.date,".csv")))
write.csv(trap.harvest.chronology, file.path(write.path,paste0("feral.swine.effort.take.snare.chronology.ALL",pull.date,".csv")))

##----END----##





