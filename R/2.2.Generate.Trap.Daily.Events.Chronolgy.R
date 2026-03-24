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
))

#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

# Read in Harvest Chronology
file_name <- file.path(
  processed_path,
  "dev_feral.swine.effort.take.traps.chronology.limited.ALL.csv"
)
trap_harvest_chronology <- read.csv(file_name, stringsAsFactors = FALSE)
trap_harvest_chronology <- trap_harvest_chronology[, -1]
nrow(trap_harvest_chronology)

trap_harvest_chronology$WT_WORK_DATE <- as.Date(as.character(
  trap_harvest_chronology$WT_WORK_DATE,
  "%Y-%m-%d"
))

# Adjust for Daily Trapping Summary
trap_harvest_chronology <- calc.days.between.records(trap_harvest_chronology)
nrow(trap_harvest_chronology)

trap_harvest_chronology <- calc.start.stop.by.record(
  trap_harvest_chronology,
  adjustment = 0
)
nrow(trap_harvest_chronology)

trap_harvest_chronology <- add.within.event.id(trap_harvest_chronology)
nrow(trap_harvest_chronology)

# tmp <- trap_harvest_chronology[trap_harvest_chronology$within.id!=1,]

# Calculate days active
trap_harvest_chronology$days.active <- trap_harvest_chronology$within.event.end.date -
  trap_harvest_chronology$within.event.str.date

# Assume first day active is 1
trap_harvest_chronology[
  trap_harvest_chronology$days.active == 0,
  "days.active"
] <- 1

# Calculate trap nights
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

# Reorder
trap_harvest_chronology <- trap_harvest_chronology[
  order(
    -trap_harvest_chronology$AGRP_PRP_ID,
    trap_harvest_chronology$WT_WORK_DATE
  ),
]

trap_harvest_chronology <- trap_harvest_chronology[
  trap_harvest_chronology$trap.count != 0,
]

# Remake unique ID
trap_harvest_chronology$unk.prp.event.id <- paste0(
  trap_harvest_chronology$AGRP_PRP_ID,
  "-",
  trap_harvest_chronology$event.id
)

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
# lut_property_acres <- unique(lut_property_acres)

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

# Remove NA values
final_agg_out_dat <- final_agg_out_dat[
  complete.cases(final_agg_out_dat$AGRP_PRP_ID),
]
nrow(final_agg_out_dat)
head(final_agg_out_dat)


#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.trap.ALL.daily.events.csv"
  )
)
## ----END----##

## ---- MAKE PLOTS ----
par(mfrow = c(2, 2))

hist(final_agg_out_dat$Take, xlab = "Take", breaks = 20, main = NULL)

summary(final_agg_out_dat$Take)

plot(
  log(final_agg_out_dat$TOTAL.LAND),
  final_agg_out_dat$Take,
  xlab = "log Property Size",
  ylab = "Take"
)
plot(
  log(final_agg_out_dat$TOTAL.LAND),
  log(final_agg_out_dat$trap.nights),
  xlab = "log Property Size",
  ylab = "log Trap Nights"
)
abline(a = 0, b = 1, col = "red")
# plot(log(final_agg_out_dat$TOTAL.LAND),final_agg_out_dat$event.length,xlab="log Property Size",ylab="Event Length")
plot(
  log(final_agg_out_dat$trap.nights),
  log(final_agg_out_dat$Take),
  xlab = "log Trap Nights",
  ylab = "log Take"
)
abline(a = 0, b = 1, col = "red")

dev.off()
plot(final_agg_out_dat$event.length, final_agg_out_dat$Take)


tmp <- trap_harvest_chronology[trap_harvest_chronology$AGRP_PRP_ID == 95314, ]

tmp <- tmp[order(-tmp$AGRP_PRP_ID, tmp$WT_WORK_DATE), ]

tmp[tmp$event.id %in% c(80.1, 80.2, 80.3, 80.4), ]


agg_out_dat <- aggregate(
  cbind(trap.nights, Take) ~ AGRP_PRP_ID + event.id + CMP_NAME,
  data = trap_harvest_chronology,
  FUN = sum
)
agg_out_dat <- agg_out_dat[
  order(agg_out_dat$AGRP_PRP_ID, agg_out_dat$event.id),
]
nrow(agg_out_dat)

#----Determine uncertainity

# Determine Trap count at end of trapping
date_lut <- calc.event.length(trap_harvest_chronology)
tmp_merge <- merge(
  date_lut,
  trap_harvest_chronology,
  by = c("AGRP_PRP_ID", "event.id", "WT_WORK_DATE", "CMP_NAME"),
  all.x = TRUE
)
tmp_merge <- tmp_merge[, c(
  "AGRP_PRP_ID",
  "event.id",
  "WT_WORK_DATE",
  "CMP_NAME",
  "trap.count.event"
)]
tmp_merge <- unique(tmp_merge)

# If Traps are zeroed out = high; if traps left = moderate; if traps negative = low
certainty_flag <- tmp_merge[, "trap.count.event"]
certainty_flag[certainty_flag > 0] <- "Moderate"
certainty_flag[certainty_flag == 0] <- "High"
certainty_flag[certainty_flag < 0] <- "low"

tmp_merge$trap.night.certainty <- certainty_flag

agg_out_dat <- merge(
  agg_out_dat,
  tmp_merge,
  by = c("AGRP_PRP_ID", "event.id", "CMP_NAME"),
  all.x = TRUE
)


# Check number of rows
nrow(agg_out_dat)
nrow(date_lut)
length(certainty_flag)

# Merge data
date_lut <- subset(date_lut, select = -c(WT_WORK_DATE))

agg_out_dat <- merge(
  agg_out_dat,
  date_lut,
  by = c("AGRP_PRP_ID", "event.id", "CMP_NAME"),
  all.x = TRUE
)

# Reorder things
agg_out_dat <- agg_out_dat[, c(
  "AGRP_PRP_ID",
  "event.id",
  "CMP_NAME",
  "start.date",
  "end.date",
  "event.length",
  "trap.nights",
  "Take",
  "trap.night.certainty"
)]


#----Merge County location data

# Generate final data
final_agg_out_dat <- merge(
  agg_out_dat,
  lut_property_acres,
  by = "AGRP_PRP_ID",
  all.x = TRUE
)
final_agg_out_dat <- final_agg_out_dat[, c(
  "AGRP_PRP_ID",
  "event.id",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
  "FIPS",
  "COUNTY.OR.CITY.LAND",
  "MILITARY.LAND",
  "PRIVATE.LAND",
  "STATE.LAND",
  "TRIBAL.LAND",
  "TOTAL.LAND",
  "CMP_NAME",
  "start.date",
  "end.date",
  "event.length",
  "trap.nights",
  "Take",
  "trap.night.certainty"
)]
final_agg_out_dat <- final_agg_out_dat[
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$event.id),
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
head(final_agg_out_dat)

# Limit to high and moderate certainity
final_agg_out_dat <- final_agg_out_dat[
  final_agg_out_dat$trap.night.certainty != "low",
]
nrow(final_agg_out_dat)
head(final_agg_out_dat)

# Limit Event Length
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$event.length < 90, ]
nrow(final_agg_out_dat)

# Limit to only those with acreage
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$TOTAL.LAND > 1, ]
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
    "dev_feral.swine.effort.take.traps.ALL.csv"
  )
)

write.csv(
  trap_harvest_chronology,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.traps.chronology.ALL.csv"
  )
)

## ----END----##
