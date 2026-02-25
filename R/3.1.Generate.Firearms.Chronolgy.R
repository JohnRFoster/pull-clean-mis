rm(list = ls())

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
raw_dir <- "data/raw"

pull_dates <- list.files(raw_dir)
pull_dates_num <- as.numeric(gsub("-", "", pull_dates))
pull_date <- pull_dates[which.max(pull_dates_num)]

raw_data_dir <- "data/raw"
raw_data_file <- "fs_national_all.csv"
raw_data <- file.path(raw_data_dir, pull_date, raw_data_file)


#---- processed path ----
processed_path <- file.path("data/processed", pull_date)
processed <- "processed_"

# look up table property acres
lut_property_acres <- read_csv(file.path(
  processed_path,
  "processed_lut_property_acres.csv"
))

# Read data
df <- read_csv(raw_data)
dat_agr_csv <- df |> raw_filter()
dat_agr_csv2 <- alter.column.names(dat_agr_csv)


##----END DATA PREP----

#--Subset Data to Only Ground Hunting Using FireArms

#--Identify only those events that used firearms
firearms_vec <- c("FIREARMS")

tmp <- dat_agr_csv2 |>
  filter(CMP_NAME %in% firearms_vec)

unk_events <- unique(tmp[, c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "WT_WORK_DATE")])

unk_events <- cbind.data.frame(
  unk_events,
  firearms.used = rep("firearms", nrow(unk_events))
)

#Add unique event ID
unk_events$event.id <- seq(1, nrow(unk_events), 1)

tmp <- merge(
  dat_agr_csv2,
  unk_events,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "WT_WORK_DATE"),
  all.y = TRUE
)
head(tmp)
nrow(tmp)

#Determine Events that Are Ground Hunting
plyr::count(tmp$CMP_NAME)

cmp_name <- c(
  "SPOTLIGHT",
  "CALLING DEVICE, MANUAL(HAND,BLOWN)",
  "NIGHT VISION/INFRARED EQUIPMENT",
  "CALLING DEVICE, ELECTRONIC",
  "BAIT STATION",
  "MONITORING CAMERA",
  "CAR/TRUCK",
  "TELEMETRY EQUIPMENT",
  "BAIT STATION"
)

event_vec <- tmp |>
  filter(CMP_NAME %in% cmp_name) |>
  pull(event.id)

firearms_associated <- tmp |>
  filter(event.id %in% event_vec)

nrow(firearms_associated)

#Remove any events associated with other methods
cmp_name <- c(
  "FIXED WING",
  "HELICOPTER",
  "SNARES, FOOT/LEG",
  "SNARES, NECK",
  "SNARES, NECK MECHANICAL (COLLARUM)",
  "TRAPS, BODY GRIP",
  "TRAPS, BODY GRIP",
  "TRAPS, CAGE",
  "TRAPS, CORRAL",
  "TRAPS, DECOY",
  "TRAPS, FOOTHOLD",
  "TRAPS, FOOTHOLD (PADDED)",
  "TRAPS, FOOTHOLD DOG PROOF",
  "TRAPS, LIVE, FERAL HOGS",
  "TRAPS, OTHER",
  "TRAPS, RAPTOR (OTHER)",
  "TRAPS, RAPTOR (SWEDISH GOSHAWK)",
  "M-44 CYANIDE CAPSULE"
)


event_vec <- firearms_associated |>
  filter(CMP_NAME %in% cmp_name) |>
  pull(event.id)

firearms_associated <- firearms_associated |>
  filter(!event.id %in% event_vec)

nrow(firearms_associated)
head(firearms_associated)
plyr::count(firearms_associated$CMP_NAME)

#Reset CMP_NAME
firearms_associated[
  firearms_associated$CMP_NAME != "FIREARMS",
  "CMP_NAME"
] <- "OTHER"
plyr::count(firearms_associated$CMP_NAME)

#Set CMP_NAME = other to 0 for counting purposes
firearms_associated <- firearms_associated |>
  mutate(WTCM_QTY = ifelse(CMP_NAME == "OTHER", 0, WTCM_QTY))

#Set WTM_QTY = 0 using WORK_TASK_UOM
uom_name <- c(
  "HOURS",
  "MINUTES",
  "ATV HOURS",
  "ATV DAY",
  "DRIVE HOURS",
  "DOG DAY"
)

firearms_associated <- firearms_associated |>
  mutate(WTM_QTY = ifelse(WORK_TASK_UOM %in% uom_name, WTM_QTY, 0))

plyr::count(firearms_associated[, c("WORK_TASK_UOM", "WTM_QTY")])

tmp_dat <- firearms_associated

#--END Restrict Data

#--Convert to minutes to hours
tmp_dat <- tmp_dat |>
  mutate(
    WTM_QTY = ifelse(
      WORK_TASK_UOM == "MINUTES",
      WTM_QTY / 60,
      WTM_QTY
    ),
    WORK_TASK_UOM = ifelse(
      WORK_TASK_UOM == "MINUTES",
      "HOURS",
      WORK_TASK_UOM
    )
  )

count(tmp_dat$CMP_NAME)
count(tmp_dat$CMP_TYPE)
count(tmp_dat$USET_NAME)

#--Remove implosable values
summary(tmp_dat$WTM_QTY)
summary(tmp_dat$WTCM_QTY)

#Set USET_NAME = DISCHARGED to 0
tmp_dat <- tmp_dat |>
  mutate(WTCM_QTY = ifelse(USET_NAME == "DISCHARGED", 0, WTCM_QTY))

#--Rework duplicate hours in WTM_QTY

#Drop unneeded columns
tmp_dat <- tmp_dat[, colnames(tmp_dat) %!in% c("CMP_TYPE", "X")]

#Remove easy duplicates
tmp_dat <- unique(tmp_dat)

event_vec <- unique(tmp_dat$event.id)

#--Loop over unique events
for (i in seq_along(event_vec)) {
  tmp <- tmp_dat[tmp_dat$event.id == event_vec[i], ]

  fire_vec <- tmp[tmp$CMP_NAME == "FIREARMS", "WTM_QTY"]

  other_vec <- tmp[tmp$CMP_NAME == "OTHER", "WTM_QTY"]

  other_vec[other_vec %in% fire_vec] <- 0

  tmp_dat[
    tmp_dat$event.id == event_vec[i] & tmp_dat$CMP_NAME == "OTHER",
    "WTM_QTY"
  ] <- other_vec
} #END Loop

#--END Rework duplicate hours

#-- Adjust number of firearms
#tmp_dat<-adjust.firearm.data(tmp_dat,thershold=5)

#Aggregate time
tmp <- aggregate(
  cbind(WTM_QTY) ~ ALWS_AGRPROP_ID + AGRP_PRP_ID + CMP_NAME + WT_WORK_DATE,
  data = tmp_dat,
  FUN = sum
)

tmp_time <- spread(tmp, CMP_NAME, WTM_QTY)

tmp_time$HOURS <- tmp_time$FIREARMS + tmp_time$OTHER

tmp_time[tmp_time$FIREARMS == tmp_time$OTHER, "HOURS"] <- tmp_time[
  tmp_time$FIREARMS == tmp_time$OTHER,
  "FIREARMS"
]

#Aggegate # firearms
tmp <- aggregate(
  cbind(WTCM_QTY) ~ ALWS_AGRPROP_ID + AGRP_PRP_ID + WT_WORK_DATE,
  data = tmp_dat,
  FUN = sum
)

tmp <- merge(
  tmp_time,
  tmp,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "WT_WORK_DATE"),
  all.x = TRUE,
  all.y = TRUE
)

#Ensure data is ordered
wide_data <- tmp[order(tmp$AGRP_PRP_ID, tmp$WT_WORK_DATE), , drop = FALSE]

#Drop those with WTCM = 0
wide_data <- wide_data[wide_data$WTCM_QTY != 0, ]

#--Rename columns
colnames(wide_data)[which(colnames(wide_data) == "FIREARMS")] <- "WTM.FIREARMS"
colnames(wide_data)[which(colnames(wide_data) == "OTHER")] <- "WTM.OTHER"
colnames(wide_data)[which(colnames(wide_data) == "WTCM_QTY")] <- "FIREARMS"

#--Add CMP_NAME
wide_data$CMP_NAME <- "FIREARMS"

in.dat <- wide_data
##END

#----Add Event Ids

#Generate Take by property
kill_by_prop <- dat_agr_csv2 |>
  select(
    ALWS_AGRPROP_ID,
    AGRP_PRP_ID,
    ST_NAME,
    ST_GSA_STATE_CD,
    CNTY_GSA_CNTY_CD,
    WTCM_QTY, # CMP_QTY
    CMP_NAME,
    WKR_QTY, # TAKE
    WT_WORK_DATE
  ) |>
  distinct() |>
  select(
    AGRP_PRP_ID,
    ALWS_AGRPROP_ID,
    WT_WORK_DATE,
    WKR_QTY,
    WTCM_QTY,
    CMP_NAME
  ) |>
  dplyr::rename(Take = WKR_QTY)

#Merge trap chronology and take data
harvest_chronology <- merge(
  in.dat,
  kill_by_prop,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "WT_WORK_DATE", "CMP_NAME"),
  all.x = TRUE
)

harvest_chronology <- harvest_chronology |>
  mutate(Take = ifelse(is.na(Take), 0, Take))

harvest_chronology[order(-harvest_chronology$Take), ]

#--Assume when Firearms = Take and firearms > 1 then assume WTCM_QTY = number of shots and assign 1 firearm
#--Calculate Hunt Hours and Hunt Days
#--Limit to events with hours < 24
harvest_chronology <- harvest_chronology |>
  mutate(
    FIREARMS = ifelse(FIREARMS == Take & FIREARMS > 1, 1, FIREARMS),
    HOURS = HOURS * FIREARMS,
    Hunt.Days = HOURS / 24
  ) |>
  filter(HOURS < 24)

#--Plot
hist(harvest_chronology$Take, breaks = 300, xlim = c(0, 50))
summary(harvest_chronology$Take)

plot(log(harvest_chronology$Hunt.Days), harvest_chronology$Take)
plot(harvest_chronology$Hunt.Days, harvest_chronology$Take)


#Remove Implosible Data
#harvest_chronology<-harvest_chronology[harvest_chronology$Take<40,]

#----Merge County location data

#Generate final data
final_agg_out_dat <- merge(
  harvest_chronology,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)

final_agg_out_dat <- final_agg_out_dat[, c(
  "AGRP_PRP_ID",
  "ALWS_AGRPROP_ID",
  "WT_WORK_DATE",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
  "FIPS",
  "TOTAL.LAND",
  "CMP_NAME",
  "HOURS",
  "FIREARMS",
  "Hunt.Hours",
  "Hunt.Days",
  "Take"
)]

final_agg_out_dat <- final_agg_out_dat[
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$WT_WORK_DATE),
]
nrow(final_agg_out_dat)

#Limit to only those with acreage
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$TOTAL.LAND > 0, ]
nrow(final_agg_out_dat)

final_agg_out_dat <- final_agg_out_dat[
  is.na(final_agg_out_dat$AGRP_PRP_ID) == FALSE,
]
nrow(final_agg_out_dat)

#--Add unk id
unk_prp_event_id <- paste0(
  final_agg_out_dat$AGRP_PRP_ID,
  ".",
  seq(1, nrow(final_agg_out_dat), 1)
)

#--Reorder
final_agg_out_dat <- data.frame(append(
  final_agg_out_dat,
  list(unk_prp_event_id = unk_prp_event_id),
  after = match("ALWS_AGRPROP_ID", names(final_agg_out_dat))
))

#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.firearms.ALL.daily.csv"
  )
)
##----END----##

missing_agrp_id <- unique(harvest_chronology$AGRP_PRP_ID[
  harvest_chronology$AGRP_PRP_ID %!in% lut_property_acres$AGRP_PRP_ID
])

write.csv(
  missing_agrp_id,
  file.path(processed_path, "dev_missing_agrp_id.csv"),
  row.names = FALSE
)
