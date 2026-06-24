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

#----get latest data pull----
readRenviron(".env")
data_path <- Sys.getenv("dataPath")

mis_path <- file.path(data_path, "MIS")
paths <- make_paths(mis_path)
pull_date <- paths$pull_date
read_path <- paths$read_path
processed_path <- paths$processed_path

message("Pull Date: ", pull_date)
processed <- "processed_"

# look up table property acres
lut_property_acres <- read.csv(file.path(
  processed_path,
  "processed_lut_property_acres.csv"
))

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

guam_unk_ids <- kill_by_prop |>
  filter(ST_NAME == "GUAM") |>
  pull(unk.id) |>
  unique()

lut_property_acres <- lut_property_acres |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID)) |>
  filter(unk.id %in% agr_unk_ids)

## ----END DATA PREP----

#-----------------------------
#----Generate trap effort ----

#-- Remove properties with rodent control

# Restrict to those with trapping
# Generate trap type list to process
# Dropping "TRAPS, BODY GRIP", "TRAPS, FOOTHOLD",
#          "TRAPS, OTHER", "TRAPS, FOOTHOLD (PADDED)", "TRAPS, CULVERT"
trap_vec <- c("TRAPS, LIVE, FERAL HOGS", "TRAPS, CAGE", "TRAPS, CORRAL")

trap_dat_tmp <- dat_eff |>
  subset_methods(trap_vec, "TRAPS, CAGE")

kill_dat_tmp <- kill_by_prop |>
  subset_methods(trap_vec, "TRAPS, CAGE")

# Remove those with CMP_QTY > max CMP_QTY for Corral Traps
corral_max <- dat_eff |>
  filter(CMP_NAME == "TRAPS, CORRAL") |>
  pull(WTCM_QTY) |>
  max(na.rm = TRUE)

trap_dat <- trap_dat_tmp |>
  filter(WTCM_QTY <= corral_max)

kill_dat <- kill_dat_tmp |>
  filter(WTCM_QTY <= corral_max)

# trap_dat |>
# 	filter(CMP_NAME == "TRAPS, LIVE, FERAL HOGS") |>
# 	pull(CMP_QTY) |>
# 	hist(
# 		breaks = 1000,
# 		main = "CMP_NAME == TRAPS, LIVE, FERAL HOGS",
# 		xlab = "CMP_QTY"
# 	)
# trap_dat |>
# 	filter(CMP_NAME == "TRAPS, CAGE") |>
# 	pull(CMP_QTY) |>
# 	hist(breaks = 1000, main = "CMP_NAME == TRAPS, CAGE", xlab = "CMP_QTY")
# trap_dat |>
# 	filter(CMP_NAME == "TRAPS, CORRAL") |>
# 	pull(CMP_QTY) |>
# 	hist(breaks = 1000, main = "CMP_NAME == TRAPS, CORRAL", xlab = "CMP_QTY")

# Convert all trap types to the same type

# trap_dat |>
#   filter(CMP_NAME == "TRAPS, CAGE") |>
#   pull(CMP_QTY) |>
#   hist(breaks=1000, main="ALL TRAPS", xlab="CMP_QTY")

#--Make Chronology using course thershold
chronology_course <- trap.chronology(
  trap_dat,
  kill_dat,
  event.time.threshold = 10,
  use.mlv.thershold = TRUE,
  use.stat.fudge = TRUE,
  fudge.user = 5
)
# chronology_course<-merge.trap.events(chronology_course,event.time.threshold=25,max.time=40)
chronology_course <- assign.orphen.events(chronology_course, max.time = 15)

chronology_course$unk.prp.event.id <- paste0(
  chronology_course$AGRP_PRP_ID,
  "-",
  chronology_course$event.id
)
date_lut <- calc.event.length(chronology_course)

#--Make Chronology using fine thershold
property_vec <- unique(date_lut[date_lut$event.length > 30, "AGRP_PRP_ID"])

new_dat <- trap_dat[trap_dat$AGRP_PRP_ID %in% property_vec, ]
chronology_fine <- trap.chronology(
  new_dat,
  kill_dat,
  event.time.threshold = 30,
  use.mlv.thershold = TRUE,
  use.stat.fudge = TRUE,
  fudge.user = 4
)
chronology_fine <- assign.orphen.events(chronology_fine, max.time = 15)

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

trap_harvest_chronology <- assign.orphen.events(
  trap_harvest_chronology,
  max.time = 20.5
)

trap_harvest_chronology$unk.prp.event.id <- paste0(
  trap_harvest_chronology$AGRP_PRP_ID,
  "-",
  trap_harvest_chronology$event.id
)
nrow(trap_harvest_chronology)
nrow(chronology_fine) + nrow(chronology_course_adj)

date_lut <- calc.event.length(trap_harvest_chronology)
## ----END----

#----Assign Unassigned Orphens events trap nights----
cnt_1 <- plyr::count(trap_harvest_chronology[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID"
)])
cnt_1[cnt_1$freq == 1, "Orphen.Flag"] <- "Orphen"
cnt_1 <- cnt_1[, c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "Orphen.Flag"
)]
cnt_1[is.na(cnt_1$Orphen.Flag), "Orphen.Flag"] <- "Not Orphen"
plyr::count(cnt_1$Orphen.Flag)
# count(cnt_1[,c("AGRP_PRP_ID","Orphen.Flag")])

# INvestigate results
trap_harvest_chronology <- merge(
  trap_harvest_chronology,
  cnt_1,
  by = c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID"),
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

hist(
  trap_harvest_chronology[
    trap_harvest_chronology$Orphen.Flag == "Orphen",
    "day.diff"
  ],
  breaks = 1500,
  xlim = c(0, 50)
)

plyr::count(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen",
  "day.diff"
])

sum(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen",
  "Take"
])

# trap_harvest_chronology<-trap_harvest_chronology[trap_harvest_chronology$Orphen.Flag=="Not Orphen",]
# trap_harvest_chronology <- trap_harvest_chronology[,-ncol(trap_harvest_chronology)]
#----END----

#----Remove Trapping events that are 1 day in length
events_1day <- date_lut[
  date_lut$start.date == date_lut$end.date,
  c("AGRP_PRP_ID", "unk.prp.event.id", "ALWS_AGRPROP_ID")
]
nrow(trap_harvest_chronology)

trap_harvest_chronology <- trap_harvest_chronology[
  trap_harvest_chronology$unk.prp.event.id %not in% events_1day,
]
nrow(trap_harvest_chronology)


#----Break up long events----

trap_harvest_chronology <- break.up.long.events(
  trap_harvest_chronology,
  long.event.thershold = 30
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

# trap_harvest_chronology[trap_harvest_chronology$trap.count == 0, ]

tmp_vec <- trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  c("APPLIED.USED")
]
trap_harvest_chronology[
  trap_harvest_chronology$trap.count == 0,
  "trap.count"
] <- tmp_vec

# trap_harvest_chronology[trap_harvest_chronology$trap.count == 0, ]

# Set first day to 0
trap_harvest_chronology$days.active <- trap_harvest_chronology$day.diff
trap_harvest_chronology[
  trap_harvest_chronology$event.type == "Event Start",
  "days.active"
] <- 0

## --Deal with Orphens

# Assume Orphens with 0 days are 1 day of effort
nrow(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff == 0,
])
sum(trap_harvest_chronology[
  trap_harvest_chronology$Orphen.Flag == "Orphen" &
    trap_harvest_chronology$day.diff == 0,
  "take"
])
# trap_harvest_chronology[trap_harvest_chronology$Orphen.Flag=="Orphen" & trap_harvest_chronology$day.diff==0,"days.active"] <- 0

# Assume Orphens with less than 5 days difference is the trap nights for event
nrow(
  day.diff.vec <- trap_harvest_chronology[
    trap_harvest_chronology$Orphen.Flag == "Orphen" &
      trap_harvest_chronology$day.diff <= 5 &
      trap_harvest_chronology$day.diff > 0,
  ]
)
day.diff.vec <- trap_harvest_chronology[
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
] <- day.diff.vec

# Assume Orphens with >5 days difference are 1 trap night
trap_harvest_chronology[
  trap_harvest_chronology$event.type == "Single Day Event",
  "days.active"
] <- 1

## --Calc trap nights
trap_harvest_chronology$trap.nights <- trap_harvest_chronology$trap.count *
  trap_harvest_chronology$days.active
#----END----

date_lut <- calc.event.length(trap_harvest_chronology)

## ----END Harvest Chronology----
## ------------------------------

## ----Check Results
# trap_harvest_chronology[order(-trap_harvest_chronology$trap.nights),]
# trap_harvest_chronology[order(-trap_harvest_chronology$days.active),]

# Determine Appropriate Thershold for long events
# property_vec <- unique(date_lut[date_lut$event.length>200,"AGRP_PRP_ID"])

# tmp<-trap_harvest_chronology[trap_harvest_chronology$AGRP_PRP_ID %in% property_vec,]

# hist(tmp$day.diff,breaks=100000,xlim=c(0,50))

# mlv(tmp$day.diff, method = "mfv")
# mean(tmp$day.diff)
# median(tmp$day.diff)

date_lut[order(-date_lut$event.length), ]
trap_harvest_chronology[order(-trap_harvest_chronology$trap.nights), ]

# tmp<-trap_harvest_chronology[trap_harvest_chronology$trap.count==1,]

# tmp[order(-tmp$trap.nights),]

# trap_harvest_chronology[order(-trap_harvest_chronology$trap.nights),]
hist(as.numeric(date_lut$event.length), breaks = 100, xlim = c(0, 90))
plot(as.numeric(date_lut$event.length), log(date_lut$information.quaility))
plot(log(as.numeric(date_lut$event.length)), log(date_lut$information.quaility))
abline(h = log(.05), col = "red")
abline(h = log(.1), col = "blue")
abline(h = log(.2), col = "orange")


#-------------------------------------------------------------------
#----Generate summary of trap nights and kill by each trapping event

# trap_harvest_chronology<-read.csv("feral.swine.effort.take.traps.chronology.ALL2018-01-30.csv",stringsAsFactors=FALSE)
# trap_harvest_chronology <- trap_harvest_chronology[,-1]
# trap_harvest_chronology$WT_WORK_DATE <- as.Date(as.character(trap_harvest_chronology$WT_WORK_DATE,"%d-%b-%y"))
date_lut <- calc.event.length(trap_harvest_chronology)

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
final_agg_out_dat <- merge(
  agg_out_dat,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)

select_cols <- c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
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
)

final_agg_out_dat <- final_agg_out_dat |>
  select(all_of(select_cols))

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

final_agg_out_dat <- final_agg_out_dat[
  complete.cases(final_agg_out_dat$AGRP_PRP_ID),
]

final_agg_out_dat <- final_agg_out_dat[
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$start.date),
]


# Remove unreliable data from full chronology
remove_vec <- final_agg_out_dat$unk.prp.event.id

trap_harvest_chronology_limited <- trap_harvest_chronology[
  trap_harvest_chronology$unk.prp.event.id %in% remove_vec,
]
nrow(trap_harvest_chronology_limited)

#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.traps.aggregated.ALL.csv"
  )
)
write.csv(
  trap_harvest_chronology,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.traps.chronology.ALL.csv"
  )
)
write.csv(
  trap_harvest_chronology_limited,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.traps.chronology.limited.ALL.csv"
  )
)

## ----END----##
