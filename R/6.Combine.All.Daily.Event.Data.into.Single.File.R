rm(list = ls())

#----Prep Data ----
raw_dir <- "data/raw"

pull_dates <- list.files(raw_dir)
pull_dates_num <- as.numeric(gsub("-", "", pull_dates))
pull_date <- pull_dates[which.max(pull_dates_num)]

processed_path <- file.path("data/processed", pull_date)
processed <- "processed_"

##----SNARE
dat_snare <- read.csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.snare.ALL.daily.csv"
))

dat_snare <- dat_snare[!is.na(dat_snare$AGRP_PRP_ID), ]
nrow(dat_snare)

#####This scaling needs some revisiting

dat_snare$CMP.Qty <- dat_snare$trap.nights / dat_snare$event.length
dat_snare$HOURS <- dat_snare$event.length * 24
dat_snare$CMP.Days <- dat_snare$trap.nights
dat_snare$CMP.Hours <- dat_snare$trap.nights * 24
dat_snare$WT_WORK_DATE <- dat_snare$start.date

select_cols <- c(
  "AGRP_PRP_ID",
  "unk.prp.event.id",
  "ALWS_AGRPROP_ID",
  "ST_NAME",
  "CNTY_NAME",
  "ST_GSA_STATE_CD",
  "CNTY_GSA_CNTY_CD",
  "FIPS",
  "WT_WORK_DATE",
  "start.date",
  "end.date",
  "TOTAL.LAND",
  "CMP_NAME",
  "CMP.Qty",
  "HOURS",
  "CMP.Hours",
  "CMP.Days",
  "Take"
)

dat_snare <- dat_snare |>
  select(all_of(select_cols))
colnames(dat_snare) <- tolower(colnames(dat_snare))

nrow(dat_snare)


##----TRAP
dat_trap <- read.csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.trap.ALL.daily.events.csv"
))

dat_trap <- dat_trap[!is.na(dat_trap$AGRP_PRP_ID), ]
nrow(dat_trap)

dat_trap$CMP.Qty <- dat_trap$trap.count
dat_trap$HOURS <- dat_trap$event.length * 24
dat_trap$CMP.Days <- dat_trap$trap.nights
dat_trap$CMP.Hours <- dat_trap$trap.nights * 24
dat_trap$WT_WORK_DATE <- dat_trap$start.date

dat_trap <- dat_trap |>
  select(all_of(select_cols))
colnames(dat_trap) <- tolower(colnames(dat_trap))

nrow(dat_trap)


##----FIREARMS
dat_firearms <- read.csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.firearms.ALL.daily.csv"
))
dat_firearms <- dat_firearms[!is.na(dat_firearms$AGRP_PRP_ID), ]
nrow(dat_firearms)

dat_firearms$start.date <- dat_firearms$WT_WORK_DATE
dat_firearms$end.date <- dat_firearms$WT_WORK_DATE

dat_firearms[, "unk.prp.event.id"] <- seq(1, nrow(dat_firearms), 1)
dat_firearms$CMP.Qty <- dat_firearms$FIREARMS
dat_firearms$CMP.Days <- dat_firearms$Hunt.Days
dat_firearms$CMP.Hours <- dat_firearms$Hunt.Hours

colnames(dat_firearms)[which(
  colnames(dat_firearms) %in% c("ST_FIPS", "CNTY_FIPS")
)] <- c("ST_GSA_STATE_CD", "CNTY_GSA_CNTY_CD")

dat_firearms <- dat_firearms |>
  select(all_of(select_cols))
colnames(dat_firearms) <- tolower(colnames(dat_firearms))

nrow(dat_firearms)


##----AERIAL
dat_aerial <- read.csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.aerial.ALL.daily.csv"
))
dat_aerial <- dat_aerial[!is.na(dat_aerial$AGRP_PRP_ID), ]
nrow(dat_aerial)

dat_aerial$CMP.Qty <- dat_aerial$VEHICLES
dat_aerial$CMP.Days <- dat_aerial$Flight.Days
dat_aerial$CMP.Hours <- dat_aerial$Flight.Hours
dat_aerial$WT_WORK_DATE <- dat_aerial$Start.Date

dat_aerial <- dat_aerial |>
  rename(start.date = Start.Date, end.date = End.Date) |>
  select(all_of(select_cols))
colnames(dat_aerial) <- tolower(colnames(dat_aerial))

nrow(dat_aerial)


ncol(dat_aerial)
ncol(dat_firearms)
ncol(dat_trap)
ncol(dat_snare)


##----MERGE ALL INTO SINGLE FILE
all_methods <- rbind.data.frame(dat_aerial, dat_firearms, dat_trap, dat_snare)

all_methods$end.date <- as.Date(all_methods$end.date)
all_methods$start.date <- as.Date(all_methods$start.date)


colnames(all_methods)[which(
  colnames(all_methods) %in% "total.land"
)] <- "property.size"

sum(c(
  nrow(dat_aerial),
  nrow(dat_trap),
  nrow(dat_snare),
  nrow(dat_firearms)
))

nrow(all_methods)

all_methods <- all_methods[complete.cases(all_methods), ]
nrow(all_methods)

write.csv(
  all_methods,
  file.path(
    processed_path,
    "dev_MIS.Effort.Take.all_methods.Daily.Events.csv"
  )
)
nrow(all_methods)

#----END
