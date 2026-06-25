rm(list = ls())

library(dplyr)
source("R/FNC.Misc.Utilities.R")

#----get latest data pull----
readRenviron(".env")
data_path <- Sys.getenv("dataPath")

mis_path <- file.path(data_path, "MIS")
paths <- make_paths(mis_path)
pull_date <- paths$pull_date
read_path <- paths$read_path
processed_path <- paths$processed_path
processed <- "processed_"

## ----SNARE
dat_snare <- read.csv(file.path(
  processed_path,
  "dev_feral.swine.effort.take.snare.ALL.daily.csv"
))

dat_snare <- dat_snare[!is.na(dat_snare$AGRP_PRP_ID), ]
nrow(dat_snare)

##### This scaling needs some revisiting

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


## ----TRAP
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


## ----FIREARMS
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


## ----AERIAL
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
  dplyr::rename(start.date = Start.Date, end.date = End.Date) |>
  select(all_of(select_cols))
colnames(dat_aerial) <- tolower(colnames(dat_aerial))

nrow(dat_aerial)


ncol(dat_aerial)
ncol(dat_firearms)
ncol(dat_trap)
ncol(dat_snare)


## ----MERGE ALL INTO SINGLE FILE
all_methods <- rbind.data.frame(dat_aerial, dat_firearms, dat_trap, dat_snare)

all_methods$end.date <- as.Date(all_methods$end.date)
all_methods$start.date <- as.Date(all_methods$start.date)

all_methods <- all_methods |>
  as_tibble() |>
  mutate(
    st_gsa_state_cd = as.character(st_gsa_state_cd),
    cnty_gsa_cnty_cd = as.character(cnty_gsa_cnty_cd),
    st_gsa_state_cd = sprintf("%02s", st_gsa_state_cd),
    cnty_gsa_cnty_cd = sprintf("%03s", cnty_gsa_cnty_cd)
  ) |>
  mutate(
    cnty_name = stringr::str_replace(cnty_name, "^ST ", "ST. "),
    cnty_name = if_else(cnty_name == "SAINT CROIX", "ST. CROIX", cnty_name)
  ) |>
  mutate(
    cnty_name = replace_when(cnty_name, st_name == "PUERTO RICO" ~ "PUERTO RICO"),
    cnty_name = replace_when(cnty_name, st_name == "GUAM" ~ "GUAM"),
    cnty_name = replace_when(cnty_name, st_name == "HAWAII" & cnty_name == "OAHU" ~ "HONOLULU")
  )

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

all_fips <- readr::read_csv(file.path(data_path, "counties", "fips.csv")) |>
  mutate(st_gsa_state_cd = as.character(st_gsa_state_cd)) |>
  dplyr::rename(st_abbr = state_abr)

state_abbr <- readr::read_csv("data/stateAbbreviations.csv") |>
  mutate(ST_NAME = toupper(ST_NAME)) |>
  dplyr::rename(st_name = ST_NAME, st_abbr = ST_ABBR)

# need to seperate states from territories
territories <- c(
  "AMERICAN SAMOA",
  "GUAM",
  "NORTHERN MARIANA ISLANDS",
  "PUERTO RICO",
  "VIRGIN ISLANDS"
)

all_state_codes <- all_methods |>
  as_tibble() |>
  filter(!st_name %in% territories) |>
  select(st_name, cnty_name, st_gsa_state_cd, cnty_gsa_cnty_cd) |>
  distinct() |>
  left_join(state_abbr)

state_lut <- left_join(all_fips, state_abbr) |>
  mutate(
    st_gsa_state_cd = sprintf("%02d", as.numeric(st_gsa_state_cd)),
    cnty_name = case_when(
      (st_name == "VIRGIN ISLANDS" &
        grepl(" ISLAND", cnty_name)) ~ stringr::str_replace(
        cnty_name,
        " ISLAND",
        ""
      ),
      .default = cnty_name
    )
  )

correct_s_codes <- left_join(all_state_codes, state_lut) |>
  select(-cnty_gsa_cnty_cd) |>
  dplyr::rename(cnty_gsa_cnty_cd = countyfp)

all_territory_codes <- all_methods |>
  filter(st_name %in% territories) |>
  select(st_name, cnty_name) |>
  distinct()

# Puerto Rico is being considered as one county
# need to manually adjust the codes

correct_t_codes <- left_join(all_territory_codes, state_lut) |>
  mutate(
    st_abbr = case_when(st_name == "PUERTO RICO" ~ "PR", .default = st_abbr),
    st_gsa_state_cd = case_when(
      st_name == "PUERTO RICO" ~ "72",
      .default = st_gsa_state_cd
    ),
    countyfp = case_when(st_name == "PUERTO RICO" ~ "010", .default = countyfp),
  ) |>
  dplyr::rename(cnty_gsa_cnty_cd = countyfp)

states_and_territories <- bind_rows(correct_s_codes, correct_t_codes) |>
  distinct()

prop_info <- all_methods |>
  select(st_name, cnty_name, agrp_prp_id, alws_agrprop_id) |>
  distinct() |>
  left_join(states_and_territories)

# now we have filled in as much state and county information as we can
# can't do anything about the records that don't have state/county information
all_methods <- left_join(
  select(all_methods, -st_gsa_state_cd, -cnty_gsa_cnty_cd, -st_name, -cnty_name),
  prop_info
)

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
