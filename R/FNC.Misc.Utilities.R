


calc.event.length <- function(in.dat){
  #----Calculate start and end dates
  count.within.event.actions <- plyr::count(in.dat[,c("AGRP_PRP_ID","event.id","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME")])
  colnames(count.within.event.actions)[ncol(count.within.event.actions)]<-"num.actions"

  max.date <- aggregate(WT_WORK_DATE ~ AGRP_PRP_ID+event.id+unk.prp.event.id+ALWS_AGRPROP_ID+CMP_NAME, data=in.dat, FUN=max)
  max.date$end.date <- max.date$WT_WORK_DATE

  min.date <- aggregate(WT_WORK_DATE ~ AGRP_PRP_ID+event.id+unk.prp.event.id+ALWS_AGRPROP_ID+CMP_NAME, data=in.dat, FUN=min)
  colnames(min.date)[ncol(min.date)] <- "start.date"

  date.lut<- merge(max.date, min.date, by=c("AGRP_PRP_ID","event.id","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME"))

  date.lut<- merge(date.lut,count.within.event.actions, by=c("AGRP_PRP_ID","event.id","unk.prp.event.id","ALWS_AGRPROP_ID","CMP_NAME"))

  #----Determine Event Length
  event.length <- date.lut[,"end.date"]-date.lut[,"start.date"]
  event.length[event.length==0]<-1
  date.lut$event.length <- event.length


  date.lut$information.quaility <- date.lut$num.actions/as.numeric(date.lut$event.length)

  return(date.lut)
}#END Function




#----FNC Alter Columns
alter.columns <- function(dat){

  col.names<-names(dat)

  if("ID" %!in% col.names){
    dat$ID <- dat$ALWS_DA_ID
  }
  if("WT_AGRPROP_ID" %!in% col.names){
    dat$WT_AGRPROP_ID <- dat$ALWS_AGRPROP_ID
  }

  return(dat)
}#END Function



#----FNC Alter Columns
alter.column.names <- function(dat){

  colnames(dat)[which(colnames(dat) %in% c("UOM_NAME1"))] <- "UOM_NAME"

  return(dat)
}#END Function





#----FNC Calculate days elapsed since last record
calc.days.between.records <- function(in.dat){

  property.vec<-unique(in.dat$AGRP_PRP_ID)

  pb <- txtProgressBar(min = 0, max = length(property.vec), style = 3)

  for(k in 1:length(property.vec)){

    tmp<-in.dat[in.dat$AGRP_PRP_ID==property.vec[k],]

    day.diff.vec<-vector()
    if(nrow(tmp)>1){
      for(i in 2:nrow(tmp)){day.diff.vec[i]<-tmp$WT_WORK_DATE[i]-tmp$WT_WORK_DATE[i-1]}
    }

    if(length(day.diff.vec)>0){
      day.diff.vec[is.na(day.diff.vec)]<-0
    }
    if(length(day.diff.vec)==0){
      day.diff.vec<-0
    }

    in.dat[in.dat$AGRP_PRP_ID==property.vec[k],"day.diff"] <-day.diff.vec
    #print(paste0(round( (k/length(property.vec))*100,digits=1), " %"))
    setTxtProgressBar(pb, k)
  }#END Loop
  return(in.dat)
}#END FUNCTION



#----FNC Calculates Start and Stop Dates for Each Record
calc.start.stop.by.record <- function(in.dat, adjustment=0){

  property.event.vec<-unique(in.dat$unk.prp.event.id)

  in.dat$within.event.str.date <- NA
  in.dat$within.event.end.date <- NA
  pb <- txtProgressBar(min = 0, max = length(property.event.vec), style = 3)

  for(k in 1:length(property.event.vec)){

    tmp<-in.dat[in.dat$unk.prp.event.id==property.event.vec[k],]

    within.event.str.date<-vector()
    within.event.end.date<-vector()

    for(i in 1:nrow(tmp)){
      if(i==1){
        within.event.str.date[i] <- as.character(tmp$WT_WORK_DATE[i])
        within.event.end.date[i] <- as.character(tmp$WT_WORK_DATE[i])
      }

      if(i>1){
        within.event.str.date[i] <- as.character(tmp$WT_WORK_DATE[i]-(tmp$day.diff[i]-adjustment))
        within.event.end.date[i] <- as.character(tmp$WT_WORK_DATE[i])
      }
    }#END LOOP

    in.dat[in.dat$unk.prp.event.id==property.event.vec[k],"within.event.str.date"] <- within.event.str.date
    in.dat[in.dat$unk.prp.event.id==property.event.vec[k],"within.event.end.date"] <- within.event.end.date

    setTxtProgressBar(pb, k)

  }#END LOOP

  in.dat$within.event.str.date <- lubridate::ymd(in.dat$within.event.str.date)
  in.dat$within.event.end.date <- lubridate::ymd(in.dat$within.event.end.date)

  return(in.dat)
}#END FUNCTION




#----FNC Calculates Start and Stop Dates for Each Record
add.within.event.id <- function(in.dat){

  num.records<-plyr::count(in.dat[,"unk.prp.event.id"])
  num.records<-num.records[order(num.records$freq),]

  for(k in 1:nrow(num.records)){
    num.vec<-seq(1,num.records[k,"freq"],1)
    if(k==1){out.vec<-num.vec}
    if(k>1){out.vec<-c(out.vec,num.vec)}
  }

  in.dat<-in.dat[order(in.dat$unk.prp.event.id),]
  in.dat$within.id <- out.vec

  #Reorder
  in.dat<-in.dat[order(in.dat$AGRP_PRP_ID,in.dat$event.id,in.dat$WT_WORK_DATE),]

  return(in.dat)
}#END FUNCTION





#--FNC - Make property lut
make.property.lut<-function(dat.Agr){

  #--Address missing property type
  letters_only <- function(x) !grepl("[^A-Za-z]", x)

  let.vec<-letters_only(dat.Agr$PRPS_PROP_TYPE)

  dat.Agr[let.vec,"PRPS_PROP_TYPE"] <- "UNKNOWN"


  ##--Make Unique Properties--

  #Simplify land list
  fed.land.vec<-c("BLM LAND","FISH AND WILDLIFE SRVC","FOREST SERVICE LAND","OTHER FEDERAL LAND","OTHER PUBLIC LAND")
  dat.Agr[dat.Agr$PRPS_PROP_TYPE %in% fed.land.vec,"PRPS_PROP_TYPE"] <- "FEDERAL LAND"

  #Aggregate properties with same AGRP_PRP_ID
  tmp<-aggregate(as.numeric(PRPS_QTY)~AGRP_PRP_ID+ALWS_AGRPROP_ID+ST_NAME+CNTY_NAME+ST_GSA_STATE_CD+CNTY_GSA_CNTY_CD+PRPS_PROP_TYPE, data=dat.Agr, FUN=max, na.action = na.pass)
  colnames(tmp)[ncol(tmp)]<-"PRPS_QTY"
  lut.property.acres <- spread(tmp, PRPS_PROP_TYPE, PRPS_QTY)
  #lut.property.acres <- lut.property.acres[ , -which(names(lut.property.acres) %in% c("V1"))]

  colnames(lut.property.acres)<-gsub(" ",".",colnames(lut.property.acres))

  #colnames(lut.property.acres)<-c("AGRP_PRP_ID","ALWS_AGRPROP_ID","ST_NAME","CNTY_NAME","ST_FIPS","CNTY_FIPS","COUNTY.OR.CITY.LAND","FEDERAL.LAND", "MILITARY.LAND","PRIVATE.LAND","STATE.LAND","TRIBAL.LAND","UNKNOWN")

  lut.property.acres$TOTAL.LAND <- rowSums(lut.property.acres[,grepl("LAND", colnames(lut.property.acres))], na.rm = TRUE)

  ##--Add FIPS Codes--

  #Convert codes to text
  lut.property.acres$ST_GSA_STATE_CD <- as.character(lut.property.acres$ST_GSA_STATE_CD)
  lut.property.acres$CNTY_GSA_CNTY_CD <- as.character(lut.property.acres$CNTY_GSA_CNTY_CD)

  vec<-lut.property.acres[nchar(lut.property.acres$ST_GSA_STATE_CD)==1,"ST_GSA_STATE_CD"]
  lut.property.acres[nchar(lut.property.acres$ST_GSA_STATE_CD)==1,"ST_GSA_STATE_CD"] <- paste0("0",vec)

  vec<-lut.property.acres[nchar(lut.property.acres$CNTY_GSA_CNTY_CD)==1,"CNTY_GSA_CNTY_CD"]
  lut.property.acres[nchar(lut.property.acres$CNTY_GSA_CNTY_CD)==1,"CNTY_GSA_CNTY_CD"] <- paste0("00",vec)

  vec<-lut.property.acres[nchar(lut.property.acres$CNTY_GSA_CNTY_CD)==2,"CNTY_GSA_CNTY_CD"]
  lut.property.acres[nchar(lut.property.acres$CNTY_GSA_CNTY_CD)==2,"CNTY_GSA_CNTY_CD"] <- paste0("0",vec)


  lut.property.acres$FIPS <- paste0(lut.property.acres$ST_GSA_STATE_CD,lut.property.acres$CNTY_GSA_CNTY_CD)

  #Reorder columns
  lut.property.acres <- subset(lut.property.acres, select=c(AGRP_PRP_ID:CNTY_GSA_CNTY_CD,FIPS,COUNTY.OR.CITY.LAND:TOTAL.LAND))


  #--Fill in missing acerages with county mean property size

  lut.property.acres <- generate.mean.property.size(lut.property.acres, area.col="TOTAL.LAND",
                                                   area.thershold=2, adj.file="data/county_adjacency2010.csv")

  return(lut.property.acres)

  }#--END Make Acres




#--FNC - Generate trap chronology for each trap type


generate.trap.chronology <- function(trap.dat, dat.PropKill, trap.vec, time.thershold){

  require(utils)

  #----Required Functions
  source("R/FNC.MIS.calc.aerial.chronology.R")

  #Clean things up
  if(exists(x="out.harvest.chronology")){
    rm(out.harvest.chronology)
    }

  for(k in 1:length(trap.vec)){

    in.dat.sub<-trap.dat[trap.dat$CMP_NAME %in% trap.vec[k],]

    #Generate Agreement List
    agrp_prp.list <- plyr::count(in.dat.sub$AGRP_PRP_ID)
    prop.list <- agrp_prp.list[,1]

    #Add fields
    in.dat.sub$day.diff <- 0
    in.dat.sub$time.since.event <- 0
    in.dat.sub$event.id <- NA

    pb <- txtProgressBar(min = 0, max = length(prop.list), style = 3)

    #Run For All Agreements
    for(j in 1:length(prop.list)){

      #tmp.dat<-in.dat[in.dat$AGRP_PRP_ID==359251,]

      tmp.dat<-in.dat.sub[in.dat.sub$AGRP_PRP_ID==prop.list[j],]
      tmp.dat <- tmp.dat[order(tmp.dat$WT_WORK_DATE),, drop=FALSE]

      tmp.dat<-aerial.chronology(tmp.dat, time.thershold=time.thershold)


      if(j==1){out.dat=tmp.dat}
      if(j>1){out.dat=rbind(out.dat,tmp.dat)}

      #status <- (j/length(prop.list))*100

      #print(paste(round(status,0),"% completed"))
      setTxtProgressBar(pb, j)

    }#END Loop

    #----Post Process Results

    #Generate Take by property
    kill.by.prop<-dat.PropKill[dat.PropKill$CMP_NAME %in% trap.vec,]
    kill.by.prop<-unique(kill.by.prop[,c("ALWS_AGRPROP_ID","AGRP_PRP_ID","ST_NAME","ST_GSA_STATE_CD","CNTY_GSA_CNTY_CD","WTCM_QTY","CMP_NAME","WKR_QTY","WT_WORK_DATE")])
    kill.by.prop<-kill.by.prop[,c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE","CMP_NAME","WKR_QTY")]

    #Merge trap chronology and take data
    harvest.chronology <- merge(out.dat, kill.by.prop, by=c("AGRP_PRP_ID","ALWS_AGRPROP_ID","WT_WORK_DATE","CMP_NAME"),all.x = TRUE)
    colnames(harvest.chronology)[ncol(harvest.chronology)] <- "Take"

    #Ensure NAs in Take are 0
    tmp<-harvest.chronology$Take
    tmp[is.na(tmp)] <- 0
    harvest.chronology$Take <- tmp

    #Merge each chrnology by trap type
    if(k==1){out.harvest.chronology<-harvest.chronology}
    if(k>1){out.harvest.chronology<-rbind.data.frame(out.harvest.chronology, harvest.chronology)}
  } ##END Loop
  close(pb)

  #for some reason won't return proper dataset for some reason....

  return(out.harvest.chronology)
}#END Function




#--FNC - Set Start and End Dates

set.start.and.end.dates<-function(trap.harvest.chronology){

  date.lut <- calc.event.length(trap.harvest.chronology)

  trap.harvest.chronology$event.type <- "In Process"

  pb <- txtProgressBar(min = 0, max = nrow(date.lut), style = 3)

  for(i in 1:nrow(date.lut)){
    if(date.lut[i,"end.date"]!=date.lut[i,"start.date"]){
      trap.harvest.chronology[(trap.harvest.chronology$AGRP_PRP_ID==date.lut[i,"AGRP_PRP_ID"] &
                                 trap.harvest.chronology$event.id==date.lut[i,"event.id"] &
                                 trap.harvest.chronology$WT_WORK_DATE==date.lut[i,"start.date"]),"event.type"] <- "Event Start"

      trap.harvest.chronology[(trap.harvest.chronology$AGRP_PRP_ID==date.lut[i,"AGRP_PRP_ID"] &
                                 trap.harvest.chronology$event.id==date.lut[i,"event.id"] &
                                 trap.harvest.chronology$WT_WORK_DATE==date.lut[i,"end.date"]),"event.type"] <- "Event End"
    }#END

    setTxtProgressBar(pb, i)
  }#END Loop
  close(pb)

  return(trap.harvest.chronology)
}#END Function




#--FNC - Break up long events

break.up.long.events <- function(trap.harvest.chronology, long.event.thershold=5){

  date.lut <- calc.event.length(trap.harvest.chronology)
  long.events <- date.lut[date.lut$event.length>long.event.thershold,"unk.prp.event.id"]

  for(i in 1:length(long.events)){
    tmp<-trap.harvest.chronology[trap.harvest.chronology$unk.prp.event.id %in% long.events[i],]

    dvec <- tmp$WT_WORK_DATE
    new.id <- (as.numeric(dvec-dvec[1]) %/% 10)+1

    unk.prp.event.id <- paste0(tmp$unk.prp.event.id,".",new.id)
    event.id <- paste0(tmp$event.id,".",new.id)

    trap.harvest.chronology[trap.harvest.chronology$unk.prp.event.id %in% long.events[i],"unk.prp.event.id"] <-unk.prp.event.id
    #trap.harvest.chronology[trap.harvest.chronology$unk.prp.event.id %in% long.events[i],"event.id"] <-event.id
  }

  #Re-Calculate unk Event Ids
  trap.harvest.chronology<-trap.harvest.chronology[order(trap.harvest.chronology$AGRP_PRP_ID,trap.harvest.chronology$WT_WORK_DATE),,drop=FALSE]
  tmp.vec <- unique(trap.harvest.chronology$AGRP_PRP_ID)

  for(i in 1:length(tmp.vec)){
    new.event.id <- as.numeric(factor(trap.harvest.chronology[trap.harvest.chronology$AGRP_PRP_ID==tmp.vec[i],"unk.prp.event.id"]))
    trap.harvest.chronology[trap.harvest.chronology$AGRP_PRP_ID==tmp.vec[i],"event.id"] <- new.event.id
  }

  return(trap.harvest.chronology)

}#END Function



#--FNC - %Not In%

`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

#END Function





#--FNC - Generate Mean Property Size
generate.mean.property.size <- function(in.dat, area.col, area.thershold, adj.file="data/county_adjacency2010.csv"){

  #--County Adjacency File
  adj.file<-read.csv(adj.file,stringsAsFactors=FALSE)

  #Fix Fips
  adj.file[nchar(adj.file$FIPS)==4,"FIPS"] <- paste0("0",adj.file[nchar(adj.file$FIPS)==4,"FIPS"])
  adj.file[nchar(adj.file$FIPS.neighbor)==4,"FIPS.neighbor"] <- paste0("0",adj.file[nchar(adj.file$FIPS.neighbor)==4,"FIPS.neighbor"])

  adj.file$FIPS.ST <- substring(adj.file$FIPS, first=1, last=2)
  adj.file$FIPS.neighbor.ST <- substring(adj.file$FIPS.neighbor, first=1, last=2)

  #--Generate vectors for processing

  #Find NA values
  tmp<-unique(in.dat[is.na(in.dat[,area.col])==TRUE,"FIPS"])

  #Find those that are less that certain area
  tmp1<-unique(in.dat[in.dat[,area.col] <= area.thershold,"FIPS"])

  fips.vec<-unique(c(tmp,tmp1))

  fips.vec<-fips.vec[is.na(fips.vec)==FALSE]

  for(i in 1:length(fips.vec)){

    vec <- in.dat |>
      as_tibble() |>
      filter(FIPS == fips.vec[i],
             TOTAL.LAND > area.thershold) |>
      pull(TOTAL.LAND)

    if(length(vec)>0){
      mu <- mean(vec)
    }#END Logical

    #--Mean using adjacent counties
    if(length(vec)==0){

      fips.lut <- adj.file |>
        filter(FIPS == fips.vec[i]) |>
        pull(FIPS.neighbor)

      vec <- in.dat |>
        as_tibble() |>
        filter(FIPS %in% fips.lut,
               .data[[area.col]] > area.thershold) |>
        pull(all_of(area.col))

      #Use State if no county values
      if(length(vec)==0){
        st.fips <- substring(fips.vec[i],first=1,last=2)

        vec<-in.dat[in.dat$ST_GSA_STATE_CD %in% st.fips,"TOTAL.LAND"]
        vec<-vec[is.na(vec)==FALSE]
        vec<-vec[vec > area.thershold]
      }#END Logical

      mu <- mean(vec)
    }#END Logical

    #Assign Values
    if(is.nan(mu)){
      vals <- -1
    } else {
      vals<-rpois(n=1, lambda=mu)
    }

    in.dat[in.dat$FIPS == fips.vec[i] & in.dat[,area.col] <= area.thershold, area.col]<-vals

  }#END Loop

  return(in.dat)
}#END Function












#--FNC - Adjust firearm numbers
adjust.firearm.data <- function(in.dat,thershold=5){

  #--Add Ratio to find those reporting
  in.dat$ratio <- in.dat$WTCM_QTY / in.dat$WTM_QTY


  #--Assume those with Discharged as USET Name are shots fired and assign 1 firearm
  in.dat[in.dat$USET_NAME %in% c("DISCHARGED"),"WTCM_QTY"] <- 1


  #--Address those with zero firearm values.
  property.vec <- unique(in.dat[in.dat$WTCM_QTY==0,"AGRP_PRP_ID"])

  for(i in 1:length(property.vec)){
    tmp<-in.dat[in.dat$AGRP_PRP_ID==property.vec[i],"WTCM_QTY"]
    tmp<-tmp[tmp!=0]

    in.dat[in.dat$AGRP_PRP_ID==property.vec[i],"WTCM_QTY"] <- mean(tmp)
  }#END Loop

  tmp <- unique(tmp.dat$AGRP_PRP_ID)

  #--END zeros



  #--Address those with large values of WTCM_QTY which are likly discharged and not firearms
  property.vec <- unique(in.dat[in.dat$ratio > thershold,"AGRP_PRP_ID"])

  property.vec <- property.vec[property.vec %!in% tmp]

  in.dat[in.dat$AGRP_PRP_ID %in% property.vec & in.dat$WTCM_QTY>2,"WTCM_QTY"] <-1

  return(in.dat)
}#END Function

#----Simple Functions
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

# get all property info, try and recover properties with missing FIPS
check.all.properties <- function(tmp){

  no_area <- as_tibble(tmp[is.na(tmp$FIPS),])

  for_join <- no_area |>
    dplyr::select(AGRP_PRP_ID, ALWS_AGRPROP_ID) |>
    dplyr::distinct()

  propertyFIPS <- readr::read_csv("data/propertyFIPS.csv")

  with_area <- dplyr::left_join(for_join, propertyFIPS) |>
    dplyr::filter(!is.na(FIPS))

  recovered_areas <- dplyr::left_join(with_area, no_area) |>
    dplyr::select(-ST_ABBR)

  tmp <- dplyr::bind_rows(tmp, recovered_areas)
  return(tmp)
}


