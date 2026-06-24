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

mis_path <- file.path(data_path, "MIS")
paths <- make_paths(mis_path)
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
dat <- df |>
  raw_filter()

# do our best to fill in missing state and county codes so these records
# don't get dropped because of NAs
all_fips <- read_csv(file.path(data_path, "counties", "fips.csv")) |>
  rename(
    ST_GSA_STATE_CD = st_gsa_state_cd,
    CNTY_NAME = cnty_name,
    ST_ABBR = state_abr
  ) |>
  mutate(ST_GSA_STATE_CD = as.character(ST_GSA_STATE_CD))

state_abbr <- read_csv("data/stateAbbreviations.csv") |>
  mutate(ST_NAME = toupper(ST_NAME))

# need to seperate states from territories
territories <- c(
  "AMERICAN SAMOA",
  "GUAM",
  "NORTHERN MARIANA ISLANDS",
  "PUERTO RICO",
  "VIRGIN ISLANDS"
)

all_state_codes <- dat |>
  filter(!ST_NAME %in% territories) |>
  select(ST_NAME, CNTY_NAME, ST_GSA_STATE_CD, CNTY_GSA_CNTY_CD) |>
  distinct() |>
  left_join(state_abbr)

state_lut <- left_join(all_fips, state_abbr) |>
  mutate(
    ST_GSA_STATE_CD = sprintf("%02d", as.numeric(ST_GSA_STATE_CD)),
    CNTY_NAME = case_when(
      (ST_NAME == "VIRGIN ISLANDS" &
        grepl(" ISLAND", CNTY_NAME)) ~ stringr::str_replace(
        CNTY_NAME,
        " ISLAND",
        ""
      ),
      .default = CNTY_NAME
    )
  )

correct_s_codes <- left_join(all_state_codes, state_lut) |>
  select(-CNTY_GSA_CNTY_CD) |>
  rename(CNTY_GSA_CNTY_CD = countyfp)

all_territory_codes <- dat |>
  filter(ST_NAME %in% territories) |>
  select(ST_NAME, CNTY_NAME) |>
  distinct()

# Puerto Rico is being considered as one county
# need to manually adjust the codes

correct_t_codes <- left_join(all_territory_codes, state_lut) |>
  mutate(
    ST_ABBR = case_when(ST_NAME == "PUERTO RICO" ~ "PR", .default = ST_ABBR),
    ST_GSA_STATE_CD = case_when(
      ST_NAME == "PUERTO RICO" ~ "72",
      .default = ST_GSA_STATE_CD
    ),
    countyfp = case_when(ST_NAME == "PUERTO RICO" ~ "010", .default = countyfp),
  ) |>
  rename(CNTY_GSA_CNTY_CD = countyfp)

states_and_territories <- bind_rows(correct_s_codes, correct_t_codes) |>
  distinct()

prop_info <- dat |>
  select(ST_NAME, CNTY_NAME, WT_ID, AGRP_PRP_ID, WT_AGRPROP_ID, PRP_NAME) |>
  distinct() |>
  left_join(states_and_territories)

# now we have filled in as much state and county information as we can
# can't do anything about the records that don't have state/county information
dat <- left_join(
  select(dat, -ST_GSA_STATE_CD, -CNTY_GSA_CNTY_CD, -ST_NAME, -CNTY_NAME),
  prop_info
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
  filter(TOTAL.LAND > 0) |>
  as_tibble()

prop_info <- lut_property_acres |>
  select(ST_NAME, CNTY_NAME, AGRP_PRP_ID) |>
  distinct() |>
  left_join(states_and_territories)

# now we have filled in as much state and county information as we can
# can't do anything about the records that don't have state/county information
lut_property_acres2 <- left_join(
  select(
    lut_property_acres,
    -ST_GSA_STATE_CD,
    -CNTY_GSA_CNTY_CD,
    -ST_NAME,
    -CNTY_NAME
  ),
  prop_info
) |>
  mutate(FIPS = paste0(ST_GSA_STATE_CD, CNTY_GSA_CNTY_CD))


out_name <- paste0(processed, "lut_property_acres.csv")
write_csv(lut_property_acres2, file.path(processed_path, out_name))

## ----END DATA PREP----
