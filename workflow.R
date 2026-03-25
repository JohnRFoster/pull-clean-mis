#----------------------------------
#
# Workflow to clean MIS data
#
# To be run as a background job or on SciComp HPC
#
# John Foster
#----------------------------------

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

readRenviron(".env")
data_path <- Sys.getenv("dataPath")

paths <- make_paths(data_path)
pull_date <- paths$pull_date
processed_path <- paths$processed_path

start_time <- Sys.time()

# preprocess ----
message("\n\n=============    Preprocess   =============")
check_file <- file.exists(file.path(
	processed_path,
	"processed_lut_property_acres.csv"
))
if (check_file) {
	message("Preprocessed data already exists. Skipping preprocessing step.")
} else {
	message("Running preprocessing step...")

	source("R/1.PreProcess.MIS.Data.R")
	message("\nPreprocessing done")
}

# traps
message("\n\n=============      Traps      =============")
message("Trap Chronology")
check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.traps.chronology.limited.ALL.csv"
))
if (check_file) {
	message("Trap chronology already exists. Skipping trap chronology step.")
} else {
	message("Running trap chronology step...")
	source("R/2.1.Generate.Trap.Chronolgy.R")
}
message("Trap Chronology done")

check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.traps.chronology.ALL.csv"
))
if (check_file) {
	message(
		"Trap daily events already exists. Skipping trap daily events step."
	)
} else {
	message("Running trap daily events step...")
	source("R/2.1.Generate.Trap.Chronolgy.ALL.R")
}
message("Trap Daily Events done")

# firearms ----
message("\n\n=============     Firearms    =============")
check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.firearms.ALL.daily.csv"
))
if (check_file) {
	message(
		"Firearm chronology already exists. Skipping firearm chronology step."
	)
} else {
	message("Running firearm chronology step...")
	source("R/3.1.Generate.Firearms.Chronolgy.R")
}
message("Firearm Chronology done")

# aerial ----
message("\n\n=============      Aerial     =============")
check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.aerial.chronology.ALL.csv"
))
if (check_file) {
	message("Aerial chronology already exists. Skipping aerial chronology step.")
} else {
	message("Running aerial chronology step...")
	source("R/4.1.Generate.Aerial.Chronolgy.R")
}
message("Aerial Chronology done")

check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.aerial.ALL.daily.csv"
))
if (check_file) {
	message(
		"Aerial daily events already exists. Skipping aerial daily events step."
	)
} else {
	message("Running aerial daily events step...")
	source("R/4.2.Generate.Aerial.Daily.Chronology.R")
}
message("Aerial Daily Events done")

# snares ----
message("\n\n=============      Snare      =============")
check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.snare.chronology.ALL.csv"
))
if (check_file) {
	message("Snare chronology already exists. Skipping snare chronology step.")
} else {
	message("Running snare chronology step...")
	source("R/5.1.Generate.Snare.Chronolgy.R")
}
message("Snare Chronology done")

check_file <- file.exists(file.path(
	processed_path,
	"dev_feral.swine.effort.take.snare.ALL.daily.csv"
))
if (check_file) {
	message(
		"Snare daily events already exists. Skipping snare daily events step."
	)
} else {
	message("Running snare daily events step...")
	source("R/5.2.Generate.Snare.Daily.Events.Chronology.R")
}
message("Snare Daily Events done")

# combine ----
message("\n\n=============     Combine     =============")
source("R/6.Combine.All.Daily.Event.Data.into.Single.File.R")

end_time <- Sys.time()
total_time <- end_time - start_time
message(
	"\nTotal execution time: ",
	round(total_time, 2),
	" ",
	units(total_time)
)

message("\n\n*** DONE ***")
