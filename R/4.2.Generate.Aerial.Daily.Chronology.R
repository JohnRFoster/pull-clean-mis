
#---------
#
# Make Daily Chronology for Aerial Data
#
#---------




rm(list = ls())
#.rs.restartR()
gc()

#----Set Write Paths----
write.path<-"data/processed"

#----get correct data pull----
pull.date <- config::get("pull.date")


#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(modeest)
library(operators)
library(utils)


#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")




#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

#Read in Harvest Chronology
trap.harvest.chronology<-read.csv(file.path(write.path,paste0("feral.swine.effort.take.aerial.chronology.ALL.", pull.date,".csv")),stringsAsFactors=FALSE)
trap.harvest.chronology <- trap.harvest.chronology[,-1]
trap.harvest.chronology$WT_WORK_DATE <- as.Date(as.character(trap.harvest.chronology$WT_WORK_DATE,"%y-%m-%d"))

#Add unk.prp.event.id
trap.harvest.chronology$unk.prp.event.id <- paste0(trap.harvest.chronology$AGRP_PRP_ID,"-",trap.harvest.chronology$event.id)

date.lut <- calc.event.length(trap.harvest.chronology)

#Adjust for Daily Trapping Summary

trap.harvest.chronology<-trap.harvest.chronology[order(trap.harvest.chronology$AGRP_PRP_ID,trap.harvest.chronology$WT_WORK_DATE),]


trap.harvest.chronology <- calc.days.between.records(trap.harvest.chronology)
trap.harvest.chronology <- calc.start.stop.by.record(trap.harvest.chronology, adjustment=0)
trap.harvest.chronology <- add.within.event.id(trap.harvest.chronology)



#Assume first day active is 1

#Remake Event ID
trap.harvest.chronology$event.id <- paste0(trap.harvest.chronology$event.id,".",trap.harvest.chronology$within.id)

#Remake Unique Event ID
trap.harvest.chronology$unk.prp.event.id <- paste0(trap.harvest.chronology$AGRP_PRP_ID,"-",trap.harvest.chronology$event.id)

#Sort Data
trap.harvest.chronology <- trap.harvest.chronology[order(-trap.harvest.chronology$AGRP_PRP_ID,trap.harvest.chronology$WT_WORK_DATE),]



#Reorder things
tmp <- trap.harvest.chronology[,c("AGRP_PRP_ID","unk.prp.event.id","ALWS_AGRPROP_ID",
                                  "CMP_NAME","within.event.str.date","within.event.end.date",
                                  "HOURS","VEHICLES","Flight.Hours","Flight.Days","Take")]

colnames(tmp)[which(colnames(tmp)=="within.event.str.date")] <- "Start.Date"
colnames(tmp)[which(colnames(tmp)=="within.event.end.date")] <- "End.Date"


#Generate final data
lut.property.acres <- readr::read_csv(file.path(write.path, "processed_lut_property_acres.csv"))
tmp <- merge(tmp, lut.property.acres, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID"),all.x=TRUE)
tmp <- tmp[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","unk.prp.event.id","ST_NAME","CNTY_NAME", "ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD","FIPS",
                                          "Start.Date","End.Date",
                                          "TOTAL.LAND",
                                          "CMP_NAME", "HOURS", "VEHICLES", "Flight.Hours","Flight.Days","Take")]
tmp <- tmp[order(tmp$AGRP_PRP_ID,tmp$unk.prp.event.id),]
nrow(tmp)


tmp<-tmp[is.na(tmp$AGRP_PRP_ID)==FALSE,]
nrow(tmp)

#Remove those with no FIPS Code thus no area values
tmp <- check.all.properties(tmp)
tmp<-tmp[tmp$TOTAL.LAND!=1,]
tmp<-tmp[is.na(tmp$FIPS)==FALSE,]
nrow(tmp)

#----END fill in missing values

#----Write Data
write.csv(tmp, file.path(write.path,paste0("feral.swine.effort.take.aerial.ALL.daily.", pull.date,".csv")), row.names=FALSE)

#----END END


