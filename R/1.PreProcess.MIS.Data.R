
#------------------------
#
# Preprocess MIS Data
#
# Ryan Miller, John Foster
#------------------------

rm(list = ls())
#.rs.restartR()
gc()

#---- read path ----
read.path <- "data/raw"

#---- write path ----
write.path <- "data/processed"
processed <- "processed_"

#----Load Libraries----
library(reshape2)
library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(modeest)
library(operators)
library(utils)
library(anytime)

#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")
source("R/FNC.MIS.Pre.Process.R")

#----get correct data pull----
pull.date <- config::get("pull.date")

#----Prep Data ----

#--Property Data
csv.name<-paste0("fs_national_take_by_property_", pull.date, ".csv")
file.name <- file.path(read.path, csv.name)
df <- read_csv(file.name)
dat.Agr.take <- df |>
  distinct() |>
  select(-PRPS_QTY) |>
  mutate(AGRPROP_ID = WT_AGRPROP_ID)

csv.name<-paste0("fs_national_property_", pull.date, ".csv")
file.name <- file.path(read.path, csv.name)
df <- read_csv(file.name)
dat.Agr.property <- df |>
  distinct() |>
  group_by(AGRP_PRP_ID, ALWS_AGRPROP_ID, ALWS_DA_ID, PRP_NAME, ST_NAME, ST_GSA_STATE_CD, CNTY_NAME, CNTY_GSA_CNTY_CD, PRPS_PROP_TYPE) |>
  filter(PRPS_QTY == max(PRPS_QTY)) |> # Assume max PRPS_QTY is property size
  ungroup() |>
  mutate(AGRPROP_ID = ALWS_AGRPROP_ID)

dat.Agr <- left_join(dat.Agr.take, dat.Agr.property)

#--Add combined id for convience
#dat.Agr$unk.id <- paste0(dat.Agr$AGRP_PRP_ID,".",dat.Agr$ALWS_AGRPROP_ID,".",dat.Agr$ALWS_DA_ID)

#dat.Agr<-dat.Agr[dat.Agr$DA_NAME=="SWINE, FERAL",]

dat.Agr2 <- dat.Agr[complete.cases(dat.Agr$ST_GSA_STATE_CD),]

dat.Agr3 <- alter.columns(dat.Agr2)

#Assign
csv.name <- "species.look.up.csv"
spc.lut <- read_csv(file.path("data", csv.name))

tmp <- merge(dat.Agr3, spc.lut, by.x = "DA_NAME", by.y = "species", all.x = TRUE)

colnames(tmp)[ncol(tmp)] <- "DA_NAME_TYPE"

file.name<-paste0("fs_national_property_", pull.date, ".csv")
out.name <- paste0(processed, file.name)
write_csv(tmp, file.path(write.path, out.name))

#--Make property lut
dat.Agr4 <- dat.Agr3[dat.Agr3$DA_NAME=="SWINE, FERAL",]
lut.property.acres <- make.property.lut(dat.Agr4)
lut.property.acres <- lut.property.acres[lut.property.acres$TOTAL.LAND > 0, ]
out.name <- paste0(processed, "lut_property_acres.csv")
write_csv(lut.property.acres, file.path(write.path, out.name))

#--Take Data
csv.name<-paste0("fs_national_take_by_method_", pull.date, ".csv")
file.name <- file.path(read.path, csv.name)
dat.Kill<-read_csv(file.name)
dat.Kill<-distinct(dat.Kill)
dat.Kill<-dplyr::rename(dat.Kill, ALWS_AGRPROP_ID = WT_AGRPROP_ID)

# Convert Dates to R Dates
dat.Kill$WT_WORK_DATE <- as.Date(dat.Kill$WT_WORK_DATE,"%d-%b-%y")
out.name <- paste0(processed, csv.name)
write_csv(dat.Kill, file.path(write.path, out.name))




#--Effort
file.name<-paste0("fs_national_effort_", pull.date, ".csv")

dat.Eff<-read_csv(file.path(read.path, file.name))
dat.Eff<-distinct(dat.Eff)
dat.Eff<-dplyr::rename(dat.Eff, ALWS_AGRPROP_ID = WT_AGRPROP_ID)
dat.Eff<-alter.column.names(dat.Eff)

# Convert Dates to R Dates
dat.Eff$WT_WORK_DATE <- as.Date(dat.Eff$WT_WORK_DATE,"%d-%b-%y")
out.name <- paste0(processed, file.name)
write_csv(dat.Eff, file.path(write.path, out.name))



#--Take by Property
file.name<-paste0("fs_national_take_by_property_", pull.date, ".csv")

dat.PropKill<-read_csv(file.path(read.path, file.name))
dat.PropKill<-distinct(dat.PropKill)
dat.PropKill<-dplyr::rename(dat.PropKill, ALWS_AGRPROP_ID = WT_AGRPROP_ID)

# Convert Dates to R Dates
dat.PropKill$WT_WORK_DATE <- as.Date(as.character(dat.PropKill$WT_WORK_DATE),"%d-%b-%y")
out.name <- paste0(processed, file.name)
write_csv(dat.PropKill, file.path(write.path, out.name))


##----END DATA PREP----







