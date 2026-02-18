#------------------------
#
# Preprocess MIS Data
#
# Ryan Miller, John Foster
#------------------------

rm(list = ls())
#.rs.restartR()
gc()

#----Load Libraries----
library(reshape2)
library(readr)
library(tidyr)
library(dplyr)
library(plyr)
library(modeest)
library(operators)
library(utils)
library(anytime)

#----Required Functions
source("R/FNC.MIS.calc.aerial.chronology.R")
source("R/FNC.Misc.Utilities.R")
source("R/FNC.MIS.Pre.Process.R")

#----get correct data pull----
raw_dir <- "data/raw"
pull_date <- get_latest_pull_date(raw_dir)

#---- read path ----
read_path <- file.path("data/raw", pull_date)

#---- write path ----
write_path <- file.path("data/processed", pull_date)
processed <- "processed_"

if (!dir.exists(write_path)) {
  dir.create(write_path, recursive = TRUE)
}

#----Prep Data ----

#--Property Data
csv_name <- "fs_national_all.csv"
file_name <- file.path(read_path, csv_name)
df <- read_csv(file_name)
dat <- df |>
  filter(
    DA_NAME == "SWINE, FERAL",
    WT_WORK_DATE >= "2001-01-01",
    FATE_FATE == "KILLED"
  )


#--Make property lut
lut <- make.property.lut(dat)
lut_property_acres <- lut |>
  filter(TOTAL.LAND > 0)

out_name <- paste0(processed, "lut_property_acres.csv")
write_csv(lut_property_acres, file.path(write_path, out_name))

##----END DATA PREP----
