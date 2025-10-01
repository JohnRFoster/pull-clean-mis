
#----------------------------------
#
# Generate a table that has all property info
#
# John Foster
#----------------------------------


library(readr)
library(dplyr)
library(tidyr)

#----get correct data pull----
pull.date <- config::get("pull.date")

county_areas <- read_csv("data/countyArea.csv")
county_areas <- county_areas |>
  filter(grepl(",", Areaname)) |>
  group_by(Areaname) |>
  filter(LND110210D == max(LND110210D)) |>
  ungroup() |>
  select(Areaname, STCOU, LND110210D) |>
  dplyr::rename(FIPS = STCOU) |>
  select(Areaname, FIPS) |>
  separate(Areaname, c("CNTY_NAME", "ST_ABBR"), sep = ", ") |>
  distinct()

state_abbr <- read_csv("data/stateAbbreviations.csv")

all_county_info <- left_join(county_areas, state_abbr) |>
  mutate(CNTY_NAME = toupper(CNTY_NAME),
                ST_NAME = toupper(ST_NAME)) |>
  distinct()

all_properties <- read_csv(paste0("data/processed/processed_fs_national_property_", pull.date, ".csv"))
all_properties <- all_properties |>
  select(AGRP_PRP_ID, ALWS_AGRPROP_ID, PRPS_QTY, ST_NAME, CNTY_NAME) |>
  dplyr::rename(TOTAL.LAND = PRPS_QTY) |>
  distinct()

propertyFIPS <- left_join(all_properties, all_county_info) |>
  filter(!is.na(FIPS)) |>
  group_by(AGRP_PRP_ID, ALWS_AGRPROP_ID) |>
  filter(TOTAL.LAND == max(TOTAL.LAND)) |>
  ungroup()

write_csv(propertyFIPS, "data/propertyFIPS.csv")
