#----------------------------------
#
# Workflow to clean MIS data
#
# To be run as a background job or on NBAF HPC
#
# John Foster
#----------------------------------


message("\n\n=============    Preprocess   =============")
source("R/1.PreProcess.MIS.Data.R")
message("\nPreprocessing done")



message("\n\n=============      Traps      =============")
message("Trap Chronology")
source("R/2.1.Generate.Trap.Chronolgy.R")
message("Trap Chronology done")

message("\nTrap Daily Events")
source("R/2.2.Generate.Trap.Daily.Events.Chronolgy.R")
message("Trap Daily Events done")



message("\n\n=============     Firearms    =============")
message("Firearm Chronology")
source("R/3.1.Generate.Firearms.Chronolgy.R")
message("Firearm Chronology done")

message("\nFirearms Daily Events")
source("R/3.2.Generate.Firearms.Daily.Chronology.R")
message("Firearm Daily Events done")



message("\n\n=============      Aerial     =============")
message("Aerial Chronology")
source("R/4.1.Generate.Aerial.Chronolgy.R")
message("Aerial Chronology done")

message("\nAerial Daily Events")
source("R/4.2.Generate.Aerial.Daily.Chronology.R")
message("Aerial Daily Events done")



message("\n\n=============      Snare      =============")
message("Snare Chronology")
source("R/5.1.Generate.Snare.Chronolgy.R")
message("Snare Chronology done")

message("\nSnare Daily Events")
source("R/5.2.Generate.Snare.Daily.Events.Chronology.R")
message("Snare Daily Events done")



message("\n\n=============     Combine     =============")
source("R/6.Combine.All.Daily.Event.Data.into.Single.File.R")
message("\n\n*** DONE ***")

