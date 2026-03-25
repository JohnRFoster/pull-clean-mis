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

# Read data
raw_data_file <- "fs_national_all.csv"
raw_data <- file.path(read_path, raw_data_file)
df <- read_csv(raw_data)
dat_agr_csv <- df |> raw_filter()
dat_agr_csv2 <- alter.column.names(dat_agr_csv)

effort_file <- "processed_fs_national_effort.csv"
dat_eff <- read_csv(file.path(
  processed_path,
  effort_file
)) |>
  mutate(unk.id = paste0(AGRP_PRP_ID, ".", ALWS_AGRPROP_ID))

# Generate Take by property
kill_prop_file <- "processed_kill_by_prop.csv"
kill_by_prop <- read_csv(file.path(
  processed_path,
  kill_prop_file
)) |>
  dplyr::rename(Take = WKR_QTY)

## ----END DATA PREP----

#--Subset Data to Only Ground Hunting Using FireArms

#--Identify only those events that used firearms
firearms_vec <- c("FIREARMS")

tmp <- dat_eff |>
  filter(CMP_NAME %in% firearms_vec)

unk_events <- tmp |>
  select(AGRP_PRP_ID, ALWS_AGRPROP_ID, WT_WORK_DATE) |>
  distinct() |>
  mutate(firearms.used = "firearms")

# Add unique event ID
unk_events$event.id <- 1:nrow(unk_events)

tmp <- left_join(unk_events, dat_eff)
glimpse(tmp)
nrow(tmp)

tmp_kill <- left_join(kill_by_prop, unk_events) |>
  filter(!is.na(event.id))

# Determine Events that Are Ground Hunting
plyr::count(tmp$CMP_NAME)
plyr::count(tmp_kill$CMP_NAME)

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

firearms_associated_kill <- tmp_kill |>
  filter(event.id %in% event_vec)
nrow(firearms_associated_kill)

# Remove any events associated with other methods
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
glimpse(firearms_associated)
plyr::count(firearms_associated$CMP_NAME)

firearms_associated_kill <- firearms_associated_kill |>
  filter(!event.id %in% event_vec)

nrow(firearms_associated_kill)
glimpse(firearms_associated_kill)
plyr::count(firearms_associated_kill$CMP_NAME)

# Reset CMP_NAME
firearms_associated <- firearms_associated |>
  mutate(CMP_NAME = if_else(CMP_NAME != "FIREARMS", "OTHER", CMP_NAME))
firearms_associated_kill <- firearms_associated_kill |>
  mutate(CMP_NAME = if_else(CMP_NAME != "FIREARMS", "OTHER", CMP_NAME))

plyr::count(firearms_associated$CMP_NAME)
plyr::count(firearms_associated_kill$CMP_NAME)

# Set CMP_NAME = other to 0 for counting purposes
firearms_associated <- firearms_associated |>
  mutate(WTCM_QTY = ifelse(CMP_NAME == "OTHER", 0, WTCM_QTY))
firearms_associated_kill <- firearms_associated_kill |>
  mutate(WTCM_QTY = ifelse(CMP_NAME == "OTHER", 0, WTCM_QTY))

# Set WTM_QTY = 0 using WORK_TASK_UOM
uom_name <- c(
  "HOURS",
  "MINUTES",
  "ATV HOURS",
  "ATV DAY",
  "DRIVE HOURS",
  "DOG DAY"
)

firearms_associated <- firearms_associated |>
  mutate(WTM_QTY = ifelse(UOM_NAME %in% uom_name, WTM_QTY, 0))

plyr::count(firearms_associated[, c("UOM_NAME", "WTM_QTY")])

tmp_dat <- firearms_associated

#--END Restrict Data

#--Convert to minutes to hours
tmp_dat <- tmp_dat |>
  mutate(
    WTM_QTY = ifelse(
      UOM_NAME == "MINUTES",
      WTM_QTY / 60,
      WTM_QTY
    ),
    UOM_NAME = ifelse(
      UOM_NAME == "MINUTES",
      "HOURS",
      UOM_NAME
    )
  )

plyr::count(tmp_dat$CMP_NAME)
plyr::count(tmp_dat$CMP_TYPE)
plyr::count(tmp_dat$USET_NAME)

#--Remove implausable values
summary(tmp_dat$WTM_QTY)
summary(tmp_dat$WTCM_QTY)

# Set USET_NAME = DISCHARGED to 0
tmp_dat <- tmp_dat |>
  mutate(WTCM_QTY = ifelse(USET_NAME == "DISCHARGED", 0, WTCM_QTY))

#--Rework duplicate hours in WTM_QTY

# Drop unneeded columns
tmp_dat <- tmp_dat[, colnames(tmp_dat) %!in% c("CMP_TYPE", "X")]

# Remove easy duplicates
tmp_dat <- distinct(tmp_dat)

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
} # END Loop

#--END Rework duplicate hours

#-- Adjust number of firearms
# tmp_dat<-adjust.firearm.data(tmp_dat,thershold=5)

# Aggregate time
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

# Aggregate # firearms
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

# Ensure data is ordered
wide_data <- tmp[order(tmp$AGRP_PRP_ID, tmp$WT_WORK_DATE), , drop = FALSE]

# Drop those with WTCM = 0
wide_data <- wide_data[wide_data$WTCM_QTY != 0, ]

#--Rename columns
colnames(wide_data)[which(colnames(wide_data) == "FIREARMS")] <- "WTM.FIREARMS"
colnames(wide_data)[which(colnames(wide_data) == "OTHER")] <- "WTM.OTHER"
colnames(wide_data)[which(colnames(wide_data) == "WTCM_QTY")] <- "FIREARMS"

#--Add CMP_NAME
wide_data$CMP_NAME <- "FIREARMS"
firearms_associated_kill$CMP_NAME <- "FIREARMS"

x_dat <- lut_property_acres |>
  select(
    AGRP_PRP_ID,
    ALWS_AGRPROP_ID,
    ST_NAME,
    CNTY_NAME,
    ST_GSA_STATE_CD,
    CNTY_GSA_CNTY_CD
  ) |>
  distinct()
in_dat <- wide_data

in_dat2 <- left_join(x_dat, in_dat) |>
  filter(!is.na(CMP_NAME)) |>
  glimpse()

## END

# Merge trap chronology and take data
harvest_chronology <- merge(
  in_dat2,
  firearms_associated_kill,
  # by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "WT_WORK_DATE", "CMP_NAME"),
  all.x = TRUE
)

harvest_chronology <- harvest_chronology |>
  mutate(Take = ifelse(is.na(Take), 0, Take))
glimpse(harvest_chronology)

harvest_chronology[order(-harvest_chronology$Take), ]

#--Assume when Firearms = Take and firearms > 1 then assume WTCM_QTY = number of shots and assign 1 firearm
#--Calculate Hunt Hours and Hunt Days
#--Limit to events with hours < 24
harvest_chronology <- harvest_chronology |>
  mutate(
    FIREARMS = ifelse(FIREARMS == Take & FIREARMS > 1, 1, FIREARMS),
    Hunt.Hours = HOURS * FIREARMS,
    Hunt.Days = HOURS / 24
  ) |>
  filter(HOURS < 24)

#--Plot
hist(harvest_chronology$Take, breaks = 300, xlim = c(0, 50))
summary(harvest_chronology$Take)

plot(log(harvest_chronology$Hunt.Days), harvest_chronology$Take)
plot(harvest_chronology$Hunt.Days, harvest_chronology$Take)


# Remove Implosible Data
# harvest_chronology<-harvest_chronology[harvest_chronology$Take<40,]

#----Merge County location data

harvest_chronology <- harvest_chronology |>
  select(-ST_NAME, -ST_GSA_STATE_CD, -CNTY_GSA_CNTY_CD)

# Generate final data
final_agg_out_dat <- merge(
  harvest_chronology,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID", "CNTY_NAME"),
  all.x = TRUE
)

final_agg_out_dat <- final_agg_out_dat |>
  select(
    AGRP_PRP_ID,
    ALWS_AGRPROP_ID,
    WT_WORK_DATE,
    ST_NAME,
    CNTY_NAME,
    ST_GSA_STATE_CD,
    CNTY_GSA_CNTY_CD,
    FIPS,
    TOTAL.LAND,
    CMP_NAME,
    HOURS,
    FIREARMS,
    Hunt.Hours,
    Hunt.Days,
    Take
  )

final_agg_out_dat <- final_agg_out_dat[
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$WT_WORK_DATE),
]
nrow(final_agg_out_dat)

# Limit to only those with acreage
final_agg_out_dat <- final_agg_out_dat[final_agg_out_dat$TOTAL.LAND > 0, ]
nrow(final_agg_out_dat)

final_agg_out_dat <- final_agg_out_dat[
  !is.na(final_agg_out_dat$AGRP_PRP_ID),
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
## ----END----##

missing_agrp_id <- unique(harvest_chronology$AGRP_PRP_ID[
  harvest_chronology$AGRP_PRP_ID %!in% lut_property_acres$AGRP_PRP_ID
])

write.csv(
  missing_agrp_id,
  file.path(processed_path, "dev_missing_agrp_id.csv"),
  row.names = FALSE
)
