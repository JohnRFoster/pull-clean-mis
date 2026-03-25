#----------------------------------
#
# Generate chronology of activity by property
#
# BETA Version
#
# Ryan Miller, John Foster
#----------------------------------

rm(list = ls())
gc()

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
    # ALWS_DA_ID,
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


# Limit lut_property_acres on species lut
lut_property_acres <- lut_property_acres[
  lut_property_acres$unk.id %in% unique(dat_Agr$unk.id),
]

## ----END DATA PREP----

#----Subset for testing ----

# in.dat<-dat.Eff[dat.Eff$AGRP_PRP_ID==314856,]
# in.dat<-in.dat[in.dat$CMP_NAME=="TRAPS, CAGE",]
# in.dat <- in.dat[order(in.dat$WT_WORK_DATE,in.dat$USET_NAME),]

# in.dat<-dat.Eff[dat.Eff$AGRP_PRP_ID==314882,]
# in.dat<-in.dat[in.dat$CMP_NAME=="TRAPS, LIVE, FERAL HOGS",]
# in.dat <- in.dat[order(in.dat$WT_WORK_DATE,in.dat$USET_NAME),]

#-----------------------------
#----Generate trap effort ----

# Remove those with WTCM_QTY > max WTCM_QTY in PropKill
trap_vec <- c(
  "SNARES, NECK",
  "SNARES, FOOT/LEG",
  "TRAPS, FOOTHOLD",
  "TRAPS, BODY GRIP",
  "SNARES, NECK MECHANICAL (COLLARUM)",
  "TRAPS, FOOTHOLD (PADDED)",
  "TRAPS, FOOTHOLD DOG PROOF"
)

kill_by_prop <- kill_by_prop[kill_by_prop$CMP_NAME %in% trap_vec, ]

max_vals <- aggregate(WTCM_QTY ~ CMP_NAME, data = kill_by_prop, FUN = max)

trap_dat <- data.frame()

for (i in seq_len(nrow(max_vals))) {
  tmp <- dat_eff[
    dat_eff$CMP_NAME == max_vals[i, "CMP_NAME"] &
      dat_eff$WTCM_QTY < max_vals[i, "WTCM_QTY"],
  ]
  trap_dat <- rbind.data.frame(trap_dat, tmp)
} # END Loop

# Plot WTCM by CMP Name
library(ggplot2)
p <- ggplot(data = trap_dat, aes(x = WTCM_QTY)) +
  geom_histogram(binwidth = 1)
p + facet_wrap(~CMP_NAME, scales = "free_y")

# Convert all trap types to the same type
trap_dat <- trap_dat |>
  filter(!is.na(UOM_NAME)) |>
  mutate(CMP_NAME = "SNARE")

kill_by_prop[, "CMP_NAME"] <- "SNARE"

#--Make Chronology using course thershold
chronology_course <- trap.chronology(
  trap_dat,
  kill_by_prop,
  event.time.threshold = 10,
  use.mlv.thershold = TRUE,
  use.stat.fudge = TRUE,
  fudge.user = 5
)

glimpse(chronology_course)

# chronology_course<-merge.trap.events(chronology_course,event.time.threshold=25,max.time=40)
chronology_course <- assign.orphen.events(chronology_course, max.time = 10)

chronology_course$unk.prp.event.id <- paste0(
  chronology_course$AGRP_PRP_ID,
  "-",
  chronology_course$event.id
)

date_lut <- calc.event.length(chronology_course)

# Determine Length
hist(as.numeric(date_lut$event.length), breaks = 300)
mean(date_lut$event.length)
median(date_lut$event.length)
mean(date_lut$event.length) + sd(date_lut$event.length)
# Use 5 days as cut point for below

#----Make Chronology using fine thershold----
property_vec <- unique(date_lut[date_lut$event.length > 5, "AGRP_PRP_ID"])

new_dat <- trap_dat[trap_dat$AGRP_PRP_ID %in% property_vec, ]
chronology_fine <- trap.chronology(
  new_dat,
  kill_by_prop,
  event.time.threshold = 5,
  use.mlv.thershold = TRUE,
  use.stat.fudge = TRUE,
  fudge.user = 1
)

chronology_fine <- assign.orphen.events(chronology_fine, max.time = 5)

chronology_fine$event.id <- chronology_fine$event.id + 1000

chronology_fine$unk.prp.event.id <- paste0(
  chronology_fine$AGRP_PRP_ID,
  "-",
  chronology_fine$event.id
)

#--Reconstruct
chronology_course_adj <- chronology_course[
  chronology_course$AGRP_PRP_ID %not in% property_vec,
]
trap_harvest_chronology <- rbind(chronology_course_adj, chronology_fine)

# trap_harvest_chronology<-trap_harvest_chronology[order(-trap_harvest_chronology$AGRP_PRP_ID,trap_harvest_chronology$WT_WORK_DATE),]

trap_harvest_chronology <- assign.orphen.events(
  trap_harvest_chronology,
  max.time = 8
)
trap_harvest_chronology$unk.prp.event.id <- paste0(
  trap_harvest_chronology$AGRP_PRP_ID,
  "-",
  trap_harvest_chronology$event.id
)
nrow(trap_harvest_chronology)
nrow(chronology_fine) + nrow(chronology_course_adj)

# trap_harvest_chronology$unk.prp.event.id <- paste0(trap_harvest_chronology$AGRP_PRP_ID,"-",trap_harvest_chronology$event.id)
date_lut <- calc.event.length(trap_harvest_chronology)
## ----END----

#----Assign Unassigned Orphens events trap nights----
cnt_1 <- plyr::count(trap_harvest_chronology[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id"
)])
cnt_1[cnt_1$freq == 1, "Orphen.Flag"] <- "Orphen"
cnt_1 <- cnt_1[, c("AGRP_PRP_ID", "unk.prp.event.id", "Orphen.Flag")]
cnt_1[is.na(cnt_1$Orphen.Flag), "Orphen.Flag"] <- "Not Orphen"
plyr::count(cnt_1$Orphen.Flag)
# count(cnt_1[,c("AGRP_PRP_ID","Orphen.Flag")])

# Investigate results
trap_harvest_chronology <- merge(
  trap_harvest_chronology,
  cnt_1,
  by = c("AGRP_PRP_ID", "unk.prp.event.id"),
  all.x = TRUE
)
mlv(
  trap_harvest_chronology[
    trap_harvest_chronology$Orphen.Flag == "Orphen",
    "day.diff"
  ],
  method = "shorth"
)
mean(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen",
  "day.diff"
])
median(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen",
  "day.diff"
])

# trap_harvest_chronology<-trap_harvest_chronology[trap_harvest_chronology$Orphen.Flag=="Not Orphen",]
# trap_harvest_chronology <- trap_harvest_chronology[,-ncol(trap_harvest_chronology)]
#----END----

#----Remove Trapping events that are 1 day in length
# events.1day <- date_lut[date_lut$start.date==date_lut$end.date,c("unk.prp.event.id")]
# nrow(trap_harvest_chronology)

# trap_harvest_chronology<-trap_harvest_chronology[trap_harvest_chronology$unk.prp.event.id %not in% events.1day,]
# nrow(trap_harvest_chronology)
#----END----

#----Break up long events----

trap_harvest_chronology <- break.up.long.events(
  trap_harvest_chronology,
  long.event.thershold = 5
)

#----END----

## ----Set Start and End dates----

trap_harvest_chronology <- set.start.and.end.dates(trap_harvest_chronology)

#----END----

#----Recalculate Trap Nights----

# Assume difference in days that are 0 than they are 1 day in length
# trap_harvest_chronology[trap_harvest_chronology$day.diff==0,"day.diff"] <- 1

tmp_vec <- rowSums(trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  c("SET", "CHECKED", "RESET")
])
trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  "trap.count"
] <- tmp_vec
# trap_harvest_chronology[trap_harvest_chronology$trap.count == 0, ]

tmp_vec <- rowSums(trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  c("UNSET", "REMOVED")
])

trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  "trap.count"
] <- tmp_vec
trap_harvest_chronology[trap_harvest_chronology$trap.count == 0, ]

tmp_vec <- trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  c("APPLIED.USED")
]
trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  "trap.count"
] <- tmp_vec
trap_harvest_chronology[trap_harvest_chronology$trap.count == 0, ]

# Set first day to 0
trap_harvest_chronology$days.active <- trap_harvest_chronology$day.diff
trap_harvest_chronology[
  trap_harvest_chronology$event.type == "Event Start",
  "days.active"
] <- 0

## --Deal with Orphens

# Assume Orphens with 0 days are 1 day of effort
trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff == 0,
  "days.active"
] <- 1

# Assume Orphens with less than 5 days difference is the trap nights for event
day_diff_vec <- trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff <= 5 &
    trap_harvest_chronology$day.diff > 0,
  "day.diff"
]
trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff <= 5 &
    trap_harvest_chronology$day.diff > 0,
  "days.active"
] <- day_diff_vec

# Assume Orphens with >5 days difference are 1 trap night
trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff >= 5,
  "days.active"
] <- 1

## --Calc trap nights
trap_harvest_chronology$trap.nights <- trap_harvest_chronology$trap.count *
  trap_harvest_chronology$days.active
#----END----

date_lut <- calc.event.length(trap_harvest_chronology)

## ----END Harvest Chronology----
## ------------------------------

## ----REMOVE Unreliable Data----
poor_dat <- date_lut[
  date_lut$information.quaility < 0.15,
  c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID")
]

if (nrow(poor_dat) > 0) {
  poor_dat$Drop.Flag <- 1
  trap_harvest_chronology <- merge(
    trap_harvest_chronology,
    poor_dat,
    by = c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID"),
    all.x = TRUE
  )

  trap_harvest_chronology <- trap_harvest_chronology[
    is.na(trap_harvest_chronology$Drop.Flag),
  ]
  trap_harvest_chronology <- trap_harvest_chronology[,
    -ncol(trap_harvest_chronology)
  ]
}
#----END----

# trap_harvest_chronology[order(-trap_harvest_chronology$trap.nights),]

# trap_harvest_chronology[order(-trap_harvest_chronology$days.active),]

## ---- REDO for Those with Long Event Times
# Determine Appropriate Thershold for long events
# property_vec <- unique(date_lut[date_lut$event.length>200,"AGRP_PRP_ID"])

# tmp<-trap_harvest_chronology[trap_harvest_chronology$AGRP_PRP_ID %in% property_vec,]

# hist(tmp$day.diff,breaks=100000,xlim=c(0,50))

# mlv(tmp$day.diff, method = "mfv")
# mean(tmp$day.diff)
# median(tmp$day.diff)

date_lut[order(-date_lut$event.length), ]

# tmp<-trap_harvest_chronology[trap_harvest_chronology$trap.count==1,]

# tmp[order(-tmp$trap.nights),]

# trap_harvest_chronology[order(-trap_harvest_chronology$trap.nights),]
par(mfrow = c(2, 2))
hist(as.numeric(date_lut$event.length), breaks = 100, xlim = c(0, 90))
median(as.numeric(date_lut$event.length))
mean(as.numeric(date_lut$event.length))

# plot(as.numeric(date_lut$event.length),log(date_lut$information.quaility))
plot(log(as.numeric(date_lut$event.length)), log(date_lut$information.quaility))
abline(h = log(.05), col = "red")
abline(h = log(.15), col = "blue")
abline(h = log(.4), col = "orange")


###### Deal with aggregates and columns to use.

#----Generate summary of trap nights and kill by each trapping event
agg_out_dat <- aggregate(
  cbind(trap.nights, Take) ~
    AGRP_PRP_ID + unk.prp.event.id + ALWS_AGRPROP_ID + CMP_NAME,
  data = trap_harvest_chronology,
  FUN = sum
)
agg_out_dat <- agg_out_dat[
  order(agg_out_dat$AGRP_PRP_ID, agg_out_dat$unk.prp.event.id),
]
nrow(agg_out_dat)

#----Determine uncertainity

# Determine Trap count at end of trapping
tmp_merge <- merge(
  date_lut,
  trap_harvest_chronology,
  by = c(
    "AGRP_PRP_ID",
    "unk.prp.event.id",
    "ALWS_AGRPROP_ID",
    "WT_WORK_DATE",
    "CMP_NAME"
  ),
  all.x = TRUE
)
tmp_merge <- tmp_merge[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
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
  by = c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID", "CMP_NAME"),
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
  by = c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID", "CMP_NAME"),
  all.x = TRUE
)

# Reorder things
agg_out_dat <- agg_out_dat[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
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
# lut.property.acres <- unique(lut.property.acres)

final_agg_out_dat <- merge(
  agg_out_dat,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)
# final_agg_out_dat <- final_agg_out_dat[,c("AGRP_PRP_ID","event.id","ST_NAME","CNTY_NAME", "ST_FIPS", "CNTY_FIPS", "COUNTY.OR.CITY.LAND","MILITARY.LAND","PRIVATE.LAND","STATE.LAND","TRIBAL.LAND","TOTAL.LAND","CMP_NAME", "start.date","end.date", "event.length", "trap.nights", "Take", "trap.night.certainty")]
final_agg_out_dat <- final_agg_out_dat[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
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
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$unk.prp.event.id),
]

nrow(final_agg_out_dat)


# Remove events with zero trap nights
non_zero_lut <- rownames(final_agg_out_dat[
  final_agg_out_dat$trap.nights != 0,
])

# Limit to those with non-zero trap nights
final_agg_out_dat <- final_agg_out_dat[
  rownames(final_agg_out_dat) %in% non_zero_lut,
]
nrow(final_agg_out_dat)

# Limit to high and moderate certainity
final_agg_out_dat <- final_agg_out_dat[
  final_agg_out_dat$trap.night.certainty != "low",
]
nrow(final_agg_out_dat)

# Limit Event Length
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$event.length < 90, ]
nrow(final_agg_out_dat)

# Limit to only those with acreage
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$TOTAL.LAND > 0, ]
nrow(final_agg_out_dat)

#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.snare.ALL.csv"
  )
)
write.csv(
  trap_harvest_chronology,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.snare.chronology.ALL.csv"
  )
)

## ----END----##
