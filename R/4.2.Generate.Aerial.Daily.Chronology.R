#---------
#
# Make Daily Chronology for Aerial Data
#
#---------

rm(list = ls())
# .rs.restartR()
gc()

#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(modeest)
library(operators)
library(utils)


#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")

#----get latest data pull----
readRenviron(".env")
data_path <- Sys.getenv("dataPath")

paths <- make_paths(data_path)
pull_date <- paths$pull_date
read_path <- paths$read_path
processed_path <- paths$processed_path
processed <- "processed_"

# look up table property acres
lut_property_acres <- read_csv(file.path(
  processed_path,
  "processed_lut_property_acres.csv"
))

#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

# Read in Harvest Chronology
trap_harvest_chronology <- read.csv(
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.aerial.chronology.ALL.csv"
  ),
  stringsAsFactors = FALSE
)

trap_harvest_chronology <- trap_harvest_chronology[, -1]
trap_harvest_chronology$WT_WORK_DATE <- as.Date(as.character(
  trap_harvest_chronology$WT_WORK_DATE,
  "%y-%m-%d"
))

# Add unk.prp.event.id
trap_harvest_chronology$unk.prp.event.id <- paste0(
  trap_harvest_chronology$AGRP_PRP_ID,
  "-",
  trap_harvest_chronology$event.id
)

# Adjust for Daily Trapping Summary
trap_harvest_chronology <- trap_harvest_chronology[
  order(
    trap_harvest_chronology$AGRP_PRP_ID,
    trap_harvest_chronology$WT_WORK_DATE
  ),
]


trap_harvest_chronology <- calc.days.between.records(trap_harvest_chronology)
trap_harvest_chronology <- calc.start.stop.by.record(
  trap_harvest_chronology,
  adjustment = 0
)
trap_harvest_chronology <- add.within.event.id(trap_harvest_chronology)


# Assume first day active is 1

# Remake Event ID
trap_harvest_chronology$event.id <- paste0(
  trap_harvest_chronology$event.id,
  ".",
  trap_harvest_chronology$within.id
)

# Remake Unique Event ID
trap_harvest_chronology$unk.prp.event.id <- paste0(
  trap_harvest_chronology$AGRP_PRP_ID,
  "-",
  trap_harvest_chronology$event.id
)

# Sort Data
trap_harvest_chronology <- trap_harvest_chronology[
  order(
    -trap_harvest_chronology$AGRP_PRP_ID,
    trap_harvest_chronology$WT_WORK_DATE
  ),
]


# Reorder things
tmp <- trap_harvest_chronology[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "CMP_NAME",
  "within.event.str.date",
  "within.event.end.date",
  "HOURS",
  "VEHICLES",
  "Flight.Hours",
  "Flight.Days",
  "Take"
)]

colnames(tmp)[which(colnames(tmp) == "within.event.str.date")] <- "Start.Date"
colnames(tmp)[which(colnames(tmp) == "within.event.end.date")] <- "End.Date"

tmp <- merge(
  tmp,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)
tmp <- tmp |>
  select(
    c(
      AGRP_PRP_ID,
      ALWS_AGRPROP_ID,
      unk.prp.event.id,
      ST_NAME,
      CNTY_NAME,
      ST_GSA_STATE_CD,
      CNTY_GSA_CNTY_CD,
      FIPS,
      Start.Date,
      End.Date,
      TOTAL.LAND,
      CMP_NAME,
      HOURS,
      VEHICLES,
      Flight.Hours,
      Flight.Days,
      Take
    )
  )

tmp <- tmp[order(tmp$AGRP_PRP_ID, tmp$unk.prp.event.id), ]
nrow(tmp)


tmp <- tmp[is.na(tmp$AGRP_PRP_ID) == FALSE, ]
nrow(tmp)

# Remove those with no FIPS Code thus no area values
tmp <- check.all.properties(tmp)
tmp <- tmp[tmp$TOTAL.LAND != 1, ]
tmp <- tmp[is.na(tmp$FIPS) == FALSE, ]
nrow(tmp)

#----END fill in missing values

#----Write Data
write.csv(
  tmp,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.aerial.ALL.daily.csv"
  ),
  row.names = FALSE
)

#----END END
