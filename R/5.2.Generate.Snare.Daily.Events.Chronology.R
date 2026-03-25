rm(list = ls())

#----Load Libraries----
library(reshape2)
library(tidyr)
library(plyr)
library(dplyr)
library(modeest)
library(readr)
library(operators)

#----Required Functions
source("R/FNC.MIS.Pre.Process.R")
source("R/FNC.MIS.calc.trap.effort.R")
source("R/FNC.MIS.calc.days.elapsed.R")
source("R/FNC.MIS.calc.trap.chronology.R")
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
)) |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID))

# Read data
raw_data_file <- "fs_national_all.csv"
raw_data <- file.path(read_path, raw_data_file)
df <- read_csv(raw_data)
dat_agr_csv <- df |> raw_filter()
dat_agr_csv2 <- alter.column.names(dat_agr_csv)

dat_Agr <- dat_agr_csv2 |>
  group_by(
    AGRP_PRP_ID,
    ALWS_AGRPROP_ID,
    #  ALWS_DA_ID,
    PRP_NAME,
    ST_NAME,
    ST_GSA_STATE_CD,
    CNTY_NAME,
    CNTY_GSA_CNTY_CD,
    PRPS_PROP_TYPE
  ) |>
  reframe(
    PRPS_QTY = max(PRPS_QTY)
  ) |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID))

agr_unk_ids <- unique(dat_Agr$unk.id)

effort_file <- "processed_fs_national_effort.csv"
dat_eff <- read_csv(file.path(
  processed_path,
  effort_file
)) |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID)) |>
  filter(unk.id %in% agr_unk_ids)

kill_prop_file <- "processed_kill_by_prop.csv"
kill_by_prop <- read_csv(file.path(
  processed_path,
  kill_prop_file
)) |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID)) |>
  filter(unk.id %in% agr_unk_ids)


lut_property_acres <- lut_property_acres[
  lut_property_acres$unk.id %in% unique(dat_Agr$unk.id),
]

## ----END DATA PREP----

#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

# Read in Harvest Chronology
trap_harvest_chronology <- read_csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.snare.chronology.ALL.csv"
))

trap_harvest_chronology <- trap_harvest_chronology[, -1]

# Adjust for Daily Trapping Summary
trap_harvest_chronology <- calc.days.between.records(trap_harvest_chronology)
trap_harvest_chronology <- calc.start.stop.by.record(
  trap_harvest_chronology,
  adjustment = 0
)
trap_harvest_chronology <- add.within.event.id(trap_harvest_chronology)

trap_harvest_chronology <- trap_harvest_chronology[
  trap_harvest_chronology$within.id != 1,
]

# Days active
trap_harvest_chronology$days.active <- as.numeric(
  trap_harvest_chronology$within.event.end.date -
    trap_harvest_chronology$within.event.str.date
)

# Assume first day active is 1 (for daily summary)
trap_harvest_chronology[
  trap_harvest_chronology$days.active == 0,
  "days.active"
] <- 1

# Calc trap nights
trap_harvest_chronology$trap.nights <- as.numeric(
  trap_harvest_chronology$days.active
) *
  trap_harvest_chronology$trap.count

# Remake Unique ID
trap_harvest_chronology$event.id <- paste0(
  trap_harvest_chronology$event.id,
  ".",
  trap_harvest_chronology$within.id
)

trap_harvest_chronology <- trap_harvest_chronology[
  order(
    -trap_harvest_chronology$AGRP_PRP_ID,
    trap_harvest_chronology$WT_WORK_DATE
  ),
]

trap_harvest_chronology <- trap_harvest_chronology[
  trap_harvest_chronology$trap.count != 0,
]


# Reorder things
agg_out_dat <- trap_harvest_chronology[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "CMP_NAME",
  "within.event.str.date",
  "within.event.end.date",
  "days.active",
  "trap.count",
  "trap.nights",
  "Take"
)]

agg_out_dat <- agg_out_dat |>
  dplyr::rename(
    start.date = within.event.str.date,
    end.date = within.event.end.date,
    event.length = days.active
  )
#----Merge County location data

# Generate final data
# lut.property.acres <- unique(lut.property.acres)

final_agg_out_dat <- merge(
  agg_out_dat,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)

final_agg_out_dat <- final_agg_out_dat[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
  "FIPS",
  "TOTAL.LAND",
  "CMP_NAME",
  "start.date",
  "end.date",
  "trap.count",
  "event.length",
  "trap.nights",
  "Take"
)]

final_agg_out_dat <- final_agg_out_dat[
  order(-final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$start.date),
]

nrow(final_agg_out_dat)
head(final_agg_out_dat)

# Remove events with zero trap nights
non_zero_lut <- rownames(final_agg_out_dat[
  final_agg_out_dat$trap.nights != 0,
])

# Limit to those with non-zero trap nights
final_agg_out_dat <- final_agg_out_dat[
  rownames(final_agg_out_dat) %in% non_zero_lut,
]
nrow(final_agg_out_dat)

# Limit Event Length
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$event.length < 90, ]
nrow(final_agg_out_dat)

# Limit to only those with acreage
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$TOTAL.LAND > 0, ]
nrow(final_agg_out_dat)

final_agg_out_dat <- final_agg_out_dat[
  complete.cases(final_agg_out_dat$AGRP_PRP_ID),
]
nrow(final_agg_out_dat)

#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.snare.ALL.daily.csv"
  )
)

## ----END----##

## ---- MAKE PLOTS ----
# par(mfrow=c(2,2))
#
# hist(final_agg_out_dat$Take, xlab="Take", breaks=20,main=NULL)
#
# summary(final_agg_out_dat$Take)
#
# plot(log(final_agg_out_dat$TOTAL.LAND),final_agg_out_dat$Take,xlab="log Property Size",ylab="Take")
# plot(log(final_agg_out_dat$TOTAL.LAND),log(final_agg_out_dat$trap.nights),xlab="log Property Size",ylab="log Trap Nights")
# abline(a=0,b=1,col="red")
# #plot(log(final_agg_out_dat$TOTAL.LAND),final_agg_out_dat$event.length,xlab="log Property Size",ylab="Event Length")
# plot(log(final_agg_out_dat$trap.nights),log(final_agg_out_dat$Take),xlab="log Trap Nights",ylab="log Take")
# abline(a=0,b=1,col="red")
#
#
# plot(final_agg_out_dat$event.length,final_agg_out_dat$Take)
