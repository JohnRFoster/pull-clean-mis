#------------------------
#
# Preprocess MIS Data
#
# Ryan Miller, John Foster
#------------------------

rm(list = ls())
# .rs.restartR()
gc()

#----Load Libraries----
library(reshape2)
library(readr)
library(tidyr)
library(plyr)
library(dplyr)
library(modeest)
library(operators)
library(utils)

source("R/FNC.Misc.Utilities.R")

#----get latest data pull----
readRenviron(".env")
data_path <- Sys.getenv("dataPath")

paths <- make_paths(data_path)
pull_date <- paths$pull_date
read_path <- paths$read_path
processed_path <- paths$processed_path

message("Pull Date: ", pull_date)

processed <- "processed_"

if (!dir.exists(processed_path)) {
  dir.create(processed_path, recursive = TRUE)
}

#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")
source("R/FNC.MIS.Pre.Process.R")

#----Prep Data ----
csv_name <- "fs_national_all.csv"
file_name <- file.path(read_path, csv_name)
df <- read_csv(file_name)
dat <- df |> raw_filter()

territories <- c("PUERTO RICO", "VIRGIN ISLANDS", "GUAM")

all_state_codes <- dat |>
  select(ST_NAME, ST_GSA_STATE_CD) |>
  distinct() |>
  filter(!ST_NAME %in% territories)

territory_codes <- tibble(
  ST_NAME = territories,
  ST_GSA_STATE_CD = c("61", "62", "63")
)

states_and_territories <- bind_rows(all_state_codes, territory_codes)

dat <- left_join(
  select(dat, -ST_GSA_STATE_CD),
  states_and_territories,
  by = "ST_NAME"
)

#--Property Data
kill_by_prop <- dat |>
  distinct()

out_name <- paste0(processed, "kill_by_prop.csv")
write_csv(kill_by_prop, file.path(processed_path, out_name))

#--Effort
file_name <- paste0("fs_national_effort.csv")

dat_eff <- read_csv(file.path(read_path, file_name))
dat_eff <- distinct(dat_eff)
dat_eff <- dplyr::rename(dat_eff, ALWS_AGRPROP_ID = WT_AGRPROP_ID)
dat_eff <- alter.column.names(dat_eff)

# Convert Dates to R Dates
dat_eff$WT_WORK_DATE <- as.Date(dat_eff$WT_WORK_DATE, "%d-%b-%y")
out_name <- paste0(processed, file_name)
write_csv(dat_eff, file.path(processed_path, out_name))

#--Make property tables
file_name <- paste0("fs_national_property.csv")

dat_prop <- read_csv(file.path(read_path, file_name))
dat_prop <- distinct(dat_prop)
dat_prop <- alter.column.names(dat_prop)

dat_prop <- left_join(
  select(dat_prop, -ST_GSA_STATE_CD),
  states_and_territories
)

out_name <- paste0(processed, file_name)
write_csv(dat_prop, file.path(processed_path, out_name))

# look up table
lut <- make.property.lut(dat_prop)
lut_property_acres <- lut |>
  filter(TOTAL.LAND > 0)

out_name <- paste0(processed, "lut_property_acres.csv")
write_csv(lut_property_acres, file.path(processed_path, out_name))

## ----END DATA PREP----
