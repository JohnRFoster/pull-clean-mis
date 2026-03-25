rm(list = ls())
gc()

#----Load Libraries----
library(reshape2)
library(tidyr)
library(readr)
library(plyr)
library(dplyr)
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
))

kill_prop_file <- "processed_kill_by_prop.csv"
kill_by_prop <- read_csv(file.path(
  processed_path,
  kill_prop_file
))

## ----END DATA PREP----

#--Subset Data
aerial_vec <- c("HELICOPTER", "FIXED WING")

tmp <- dat_eff |>
  filter(
    CMP_NAME %in% aerial_vec,
    UOM_NAME == "HOBBS METER",
    USET_NAME != "DISCHARGED"
  )

unique(tmp$CMP_NAME)
unique(tmp$CMP_TYPE)
table(tmp$USET_NAME)

#--Remove implosable values
summary(tmp$WTM_QTY)
summary(tmp$WTCM_QTY)

tmp <- tmp |>
  mutate(
    Flight.Hours = WTCM_QTY * WTM_QTY,
    Flight.Days = (WTCM_QTY / 24) * WTM_QTY
  )

wide_data <- aggregate(
  cbind(WTM_QTY, WTCM_QTY, Flight.Hours, Flight.Days) ~
    ALWS_AGRPROP_ID + AGRP_PRP_ID + CMP_NAME + WT_WORK_DATE,
  data = tmp,
  FUN = sum
)

# Ensure data is ordered
wide_data <- wide_data[
  order(
    wide_data$AGRP_PRP_ID,
    wide_data$ALWS_AGRPROP_ID,
    wide_data$WT_WORK_DATE
  ),
  ,
  drop = FALSE
]

wide_data <- wide_data |>
  dplyr::rename(
    HOURS = WTM_QTY,
    VEHICLES = WTCM_QTY
  )

in_dat <- wide_data

head(wide_data[order(-wide_data$VEHICLES), ])

## END

#----Generate chronology

#------------------------
#----Generate effort ----

#-----------------------------
#----Generate trap effort ----

# Subset to area of interest
# trap_dat<-in_dat[in_dat$AGRP_PRP_ID %in% unique.properties,]

trap_dat <- in_dat

# Generate trap type list to process
trap_vec <- unique(in_dat$CMP_NAME)

#--Remove implosable values
summary(trap_dat$HOURS)
summary(trap_dat$VEHICLES)
summary(trap_dat$Flight.Hours)

hist(trap_dat$HOURS, breaks = 100, xlab = "Hours", main = "Hours")
hist(trap_dat$VEHICLES, breaks = 100, xlab = "Vehicles", main = "Vehicles")
plot(
  trap_dat$HOURS,
  trap_dat$VEHICLES,
  xlab = "Hours",
  ylab = "Vehicles",
  main = "Vehicles vrs Hours"
)

# Restrict number of vehicles
# nrow(trap_dat[trap_dat$VEHICLES>3,])
# trap_dat <- trap_dat[trap_dat$VEHICLES<=3,]
# nrow(trap_dat)

# Restrict hours
# nrow(trap_dat[trap_dat$HOURS>10,])
# trap_dat <- trap_dat[trap_dat$HOURS<=10,]
# nrow(trap_dat)

# trap_dat[trap_dat$ALWS_AGRPROP_ID=="366874" & trap_dat$AGRP_PRP_ID=="370276",]

#----Generate trap chronology for each trap type
harvest_chronology <- generate.trap.chronology(
  trap_dat,
  kill_by_prop,
  trap_vec,
  time.thershold = 25
)
nrow(harvest_chronology)

#----Generate summary of trap nights and kill by each trapping event
agg_out_dat <- aggregate(
  cbind(HOURS, VEHICLES, Flight.Hours, Flight.Days, Take) ~
    AGRP_PRP_ID + ALWS_AGRPROP_ID + event.id + CMP_NAME,
  data = harvest_chronology,
  FUN = sum
)

agg_out_dat <- agg_out_dat[
  order(agg_out_dat$AGRP_PRP_ID, agg_out_dat$event.id),
]

nrow(agg_out_dat)
# agg_out_dat[agg_out_dat$AGRP_PRP_ID==224386,]

#----Make start and end dates for aggregated data
str_date <- aggregate(
  WT_WORK_DATE ~ event.id + AGRP_PRP_ID + ALWS_AGRPROP_ID + CMP_NAME,
  data = harvest_chronology,
  FUN = min
)

end_date <- aggregate(
  WT_WORK_DATE ~ event.id + AGRP_PRP_ID + ALWS_AGRPROP_ID + CMP_NAME,
  data = harvest_chronology,
  FUN = max
)

dates_event <- merge(
  str_date,
  end_date,
  by = c("event.id", "AGRP_PRP_ID", "ALWS_AGRPROP_ID", "CMP_NAME")
)

dates_event <- dates_event |>
  dplyr::rename(
    Start.Date = WT_WORK_DATE.x,
    End.Date = WT_WORK_DATE.y
  ) |>
  mutate(
    event.length = End.Date - Start.Date,
    event.length = ifelse(event.length == 0, 1, event.length)
  )

agg_out_dat <- merge(
  agg_out_dat,
  dates_event,
  by = c("event.id", "AGRP_PRP_ID", "ALWS_AGRPROP_ID", "CMP_NAME")
)

agg_out_dat <- agg_out_dat[, c(
  "AGRP_PRP_ID",
  "ALWS_AGRPROP_ID",
  "event.id",
  "CMP_NAME",
  "Start.Date",
  "End.Date",
  "event.length",
  "HOURS",
  "VEHICLES",
  "Flight.Hours",
  "Flight.Days",
  "Take"
)]

agg_out_dat <- agg_out_dat[
  order(agg_out_dat$AGRP_PRP_ID, agg_out_dat$event.id),
  ,
  drop = FALSE
]


#----Merge County location data

# Generate final data
final_agg_out_dat <- merge(
  agg_out_dat,
  lut_property_acres,
  by = c("AGRP_PRP_ID", "ALWS_AGRPROP_ID"),
  all.x = TRUE
)

final_agg_out_dat <- final_agg_out_dat[, c(
  "AGRP_PRP_ID",
  "ALWS_AGRPROP_ID",
  "event.id",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
  "FIPS",
  "Start.Date",
  "End.Date",
  "COUNTY.OR.CITY.LAND",
  "FEDERAL.LAND",
  "MILITARY.LAND",
  "PRIVATE.LAND",
  "STATE.LAND",
  "TRIBAL.LAND",
  "TOTAL.LAND",
  "CMP_NAME",
  "HOURS",
  "VEHICLES",
  "Flight.Hours",
  "Flight.Days",
  "Take"
)]

final_agg_out_dat <- final_agg_out_dat[
  order(final_agg_out_dat$AGRP_PRP_ID, final_agg_out_dat$event.id),
]
nrow(final_agg_out_dat)

final_agg_out_dat <- final_agg_out_dat[
  is.na(final_agg_out_dat$AGRP_PRP_ID) == FALSE,
]
nrow(final_agg_out_dat)

# Remove those with no FIPS Code thus no area values
final_agg_out_dat <- check.all.properties(final_agg_out_dat)
nrow(final_agg_out_dat)

final_agg_out_dat <- final_agg_out_dat[is.na(final_agg_out_dat$FIPS) == FALSE, ]
nrow(final_agg_out_dat)

#----END fill in missing values

#----Write Data
write.csv(
  final_agg_out_dat,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.aerial.ALL.csv"
  )
)

write.csv(
  harvest_chronology,
  file.path(
    processed_path,
    "dev_feral.swine.effort.take.aerial.chronology.ALL.csv"
  )
)

## ----END----##

plyr::count(final_agg_out_dat$ST_NAME)
length(unique(final_agg_out_dat$AGRP_PRP_ID))
nrow(final_agg_out_dat)

summary(final_agg_out_dat$Take)

## ---- MAKE PLOTS ----
# par(mfrow = c(2, 2))

# hist(final_agg_out_dat$Take, xlab = "Take", breaks = 30, main = NULL)

# summary(final_agg_out_dat$Take)

# plot(
#   log(final_agg_out_dat$TOTAL.LAND),
#   final_agg_out_dat$Take,
#   xlab = "log Property Size",
#   ylab = "Take"
# )
# plot(
#   log(final_agg_out_dat$TOTAL.LAND),
#   final_agg_out_dat$Flight.Days,
#   xlab = "log Property Size",
#   ylab = "Flight Days"
# )
# plot(
#   log(final_agg_out_dat$Flight.Days),
#   log(final_agg_out_dat$Take),
#   xlab = "log Flight Days",
#   ylab = "log Take"
# )
