### These functions processes htm files produced by Aquatroll units used by FCWC

### coded on a Macbook, so be wary if you try to run on Windows as some of the coding is different
# sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.2.3

# Last update:
# Sys.time()
# "2021-09-23 09:46:16 CDT"
# Author name: Brendan Turley
# Contact: brendan.turley@noaa.gov


###--------- R packages
require(gsw)
require(lubridate)
require(magrittr)
require(NISTunits)
require(readr)
require(rvest)
require(tools)


###--------- R version check
r <- R.version
if(as.numeric(r$major)<4 & as.numeric(r$minor)<1){
  warning(paste('\n\n Functions created for R version 4.1.0 \n\n 
                You are running an older version; some compatability issues may exist! \n\n'),
          immediate. = T)
}


###--------- data_extract_aquatroll
data_extract_aquatroll <- function(input # htm or csv file that contains the raw aquatroll output
                                   )
  {
  ### fields required for exporting raw data
  flds_req <- c("Date Time",
                "Salinity (ppt)",
                "Temperature (°C) AT",
                "Depth",
                "Pressure (psi)",
                "Actual Conductivity (µS/cm)",
                "Specific Conductivity (µS/cm)",
                "Total Dissolved Solids (ppt)",
                "Resistivity (Ω⋅cm)",
                "Density (g/cm³)",
                "Barometric Pressure (mm Hg)",
                "RDO Concentration (mg/L)",
                "RDO Saturation (%Sat)",
                "Oxygen Partial Pressure (Torr)",
                "Chlorophyll-a Fluorescence (RFU)",
                "Chlorophyll-a Concentration (µg/L)",
                "Battery Capacity (%)",
                "External Voltage (V)",
                "Barometric Pressure (mbar)",
                "Temperature (°C) HH",
                "Latitude (°)",
                "Longitude (°)",
                "Marked")
  
  if(file_ext(input)!='csv' & file_ext(input)!='htm'){
    warning(paste('\n\n File format needs to be csv or htm! \n\n'),
            immediate. = T)
  }
  
  if(file_ext(input)=='csv'){
    D <- readLines(input)
    # get header row of data
    ind <- grep('Date Time',D)
    # extract data
    D <- read_csv(input,skip=ind-1,col_types = cols())
    columns <- names(D)
    H <- read.csv(input,header=F,nrows=ind-1)
  }
  if(file_ext(input)=='htm'){
    H <- read_html(input) %>%
      html_table(fill = T) %>%
      .[[1]]
    # get header row of data
    row_d <- which(H[,1] == "Date Time")
    columns <- H[row_d,]
    # extract data
    D <- H[(row_d+1):nrow(H),]
    names(D) <- as.character(columns)
  }
  D <- as.data.frame(D)
  H <- as.data.frame(H)
  ### strip serial numbers
  names(D) <- gsub(" \\([0-9]+\\)", "", columns)
  ### Aquatroll serial number
  row_h <- which(H[,1] == "Device Model = Aqua TROLL 600 Vented")
  aquatroll_sn <- as.numeric(strsplit(as.character(H[row_h+1,1]),'=')[[1]][2])
  ### Handheld serial number
  row_sn <- grep('Device Model',H[,1])
  row_h2 <- setdiff(row_sn,row_h)
  handheld_sn <- ifelse(length(row_h2)>0,
                        as.numeric(strsplit(as.character(H[row_h2+1,1]),'=')[[1]][2]),
                        NA)
  ### rename temperaures
  ind_aquatroll <- grep(aquatroll_sn,columns)
  ### Aquatroll temperature
  ind_temp <- grep('Temperature',columns) %>%
    intersect(ind_aquatroll)
  names(D)[ind_temp] <- paste(names(D)[ind_temp],'AT')
  ### Handheld temperature
  ind_temp2 <- grep('Temperature',columns) %>%
    setdiff(ind_aquatroll)
  names(D)[ind_temp2] <- paste(names(D)[ind_temp2],'HH')
  ### what are depth units?
  depth_unit <- substr(names(D)[grep('Depth',names(D))],7,20)
  ### Strip depth units
  names(D)[grep('Depth',names(D))] <- 'Depth'
  ### missing fields
  flds_miss <- setdiff(flds_req, names(D))
  
  ### add missing columns
  if(length(flds_miss)>0){
    for(i in 1:length(flds_miss)){
      D$add_1 <- NA
      names(D)[ncol(D)] <- flds_miss[i]
    }
  }
  ### reorder data
  D <- D[,match(flds_req,names(D))]
  ### add aquatroll serial number
  D$aquatroll_sn <- aquatroll_sn
  ### add depth units
  D$depth_unit <- depth_unit
  ### output
  D[,c(2:22,24)] <- sapply(D[,c(2:22,24)],as.numeric)
  return(D)
}


###--------- summary_aquatroll
summary_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                              ignore.marked = T # TRUE will ignore the marked column from being exported in the output
)
{
  ### fields required for exporting raw data
  flds_req <- c("Date Time",
                "Salinity (ppt)",
                "Temperature (°C) AT",
                "Depth",
                "Pressure (psi)",
                "Actual Conductivity (µS/cm)",
                "Specific Conductivity (µS/cm)",
                "Total Dissolved Solids (ppt)",
                "Resistivity (Ω⋅cm)",
                "Density (g/cm³)",
                "Barometric Pressure (mm Hg)",
                "RDO Concentration (mg/L)",
                "RDO Saturation (%Sat)",
                "Oxygen Partial Pressure (Torr)",
                "Chlorophyll-a Fluorescence (RFU)",
                "Chlorophyll-a Concentration (µg/L)",
                "Battery Capacity (%)",
                "External Voltage (V)",
                "Barometric Pressure (mbar)",
                "Temperature (°C) HH",
                "Latitude (°)",
                "Longitude (°)",
                "Marked")
  
  if(file_ext(input)!='csv' & file_ext(input)!='htm'){
    warning(paste('\n\n File format needs to be csv or htm! \n\n'),
            immediate. = T)
  }
  if(file_ext(input)=='csv'){
    D <- readLines(input)
    # get header row of data
    ind <- grep('Date Time',D)
    # extract data
    D <- read_csv(input,skip=ind-1,col_types = cols())
    columns <- names(D)
    H <- read.csv(input,header=F,nrows=ind-1)
  }
  if(file_ext(input)=='htm'){
    H <- read_html(input) %>%
      html_table(fill = T) %>%
      .[[1]]
    # get header row of data
    row_d <- which(H[,1] == "Date Time")
    columns <- H[row_d,]
    # extract data
    D <- H[(row_d+1):nrow(H),]
    names(D) <- as.character(columns)
  }
  H <- data.frame(H)
  D <- data.frame(D)
  D[2:ncol(D)] <- lapply(D[2:ncol(D)],as.numeric)
  ### strip serial numbers
  names(D) <- gsub(" \\([0-9]+\\)", "", columns)
  ### Aquatroll serial number
  row_h <- which(H[,1] == "Device Model = Aqua TROLL 600 Vented")
  aquatroll_sn <- as.numeric(strsplit(as.character(H[row_h+1,1]),'=')[[1]][2])
  ### Handheld serial number
  row_sn <- grep('Device Model',H[,1])
  row_h2 <- setdiff(row_sn,row_h)
  handheld_sn <- ifelse(length(row_h2)>0,
                        as.numeric(strsplit(as.character(H[row_h2+1,1]),'=')[[1]][2]),
                        NA)
  ### rename temperaures
  ind_aquatroll <- grep(aquatroll_sn,columns)
  ### Aquatroll temperature
  ind_temp <- grep('Temperature',columns) %>%
    intersect(ind_aquatroll)
  names(D)[ind_temp] <- paste(names(D)[ind_temp],'AT')
  ### Handheld temperature
  ind_temp2 <- grep('Temperature',columns) %>%
    setdiff(ind_aquatroll)
  names(D)[ind_temp2] <- paste(names(D)[ind_temp2],'HH')
  ### what are depth units?
  depth_unit <- substr(names(D)[grep('Depth',names(D))],7,20)
  ### Strip depth units
  names(D)[grep('Depth',names(D))] <- 'Depth'
  ### missing fields
  flds_miss <- setdiff(flds_req, names(D))
  
  ### datetime
  D[,1] <- with_tz(force_tz(ymd_hms(D[,1]),tz='America/New_York'),'UTC')
  dtime <- D[1,1] ### start time
  # total duration
  duration_total <- as.numeric(D[nrow(D),1]-D[1,1],units='secs')+1
  ### lat/lon
  if(length(grep("latitude",columns,ignore.case = T))>0){
    lat_avg <- mean(D[,grep("latitude",columns,ignore.case = T)],na.rm=T)
    lon_avg <- mean(D[,grep("longitude",columns,ignore.case = T)],na.rm=T)
  } else {
    lat_avg <- lon_avg <- NA
  }
  ### max depth
  z_max <- max(D[,grep('depth',columns,ignore.case = T)],na.rm=T)
  ### is there data dropout
  if(ignore.marked==T){
    M <- D[,-grep('marked',columns,ignore.case = T)]
  }
  missingness <- unlist(lapply(M,function(x) sum(is.na(x))))
  columns <- names(D)
  dropout <- columns[which(missingness>0)]
  ### number of samples
  n_samp <- nrow(D)
  # order by time
  D <- D[order(D[,grep("date",columns,ignore.case = T)]),]
  # filter for downcast (not up)
  row_end <- which.max(D[,grep('depth',columns,ignore.case = T)])
  D <- D[1:row_end,]
  # down duration
  duration_down <- as.numeric(D[nrow(D),1]-D[1,1],units='secs')
  # mean descent rate
  dz_dt <- mean(abs(diff(D[,grep('depth',columns,ignore.case = T)])/
                      as.numeric(diff(D[,grep('date',columns,ignore.case = T)]))),na.rm=T)
  
  ### save output
  out <- data.frame(input=input,
                    time_utc=dtime,
                    aquatroll_sn=aquatroll_sn,
                    handheld_sn=handheld_sn,
                    n_samp=n_samp,
                    t_duration_sec=duration_total,
                    d_duration_sec=duration_down,
                    z_max_m=z_max,
                    dz_dt=dz_dt,
                    lon_dd=lon_avg,
                    lat_dd=lat_avg,
                    flds_miss=toString(flds_miss),
                    dropout=toString(dropout))
  return(out)
}


###--------- interp_aquatroll
interp_aquatroll <- function (input, # input file is the output data.frame from data_extract_aquatroll function
                              parms = c('temperature','salinity','chlorophyll','oxygen'), # parameters that you want to extract, smooth, and interpolate
                              z_min = 2, # depth cutoff to start interpolation, typically there is a soak period at the surface where readings are unreliable
                              resolution = 1, # resolution in meters of the interpolation
                              set_wd=NA, # set the working directory to save csv and plots; if NA, it reverts to the current working directory
                              plot=T # TRUE to plot interpolated data for visual inspection
)
{
  ### rename oxygen
  if(length(which(parms=='oxygen'))>0){
    parms[which(parms=='oxygen')] <- 'rdo'
  }
  
  ### test for valid parameters
  test <- sapply(parms,function(x) grep(x,names(input),ignore.case = T)) %>%
    sapply(length)
  if(any(test<1)){
    names(test)[which(test<1)]
    warning(paste('\n\n Invalid parameter: \n\n',
                  names(test)[which(test<1)],
                  '\n\n'),
            immediate. = T)
  }
  
  ### convert feet to meters
  if(unique(input[grep('unit',names(input))])=='(ft)'){
    input$Depth <- NISTftTOmeter(input$Depth)
  }
  columns <- names(input)
  
  ### chlorophyll dropout
  if(sum(is.na(input[,grep('chlorophyll',columns,ignore.case = T)[1]]))==nrow(input)){
    input[,ind[1]] <- input[,ind[2]]*10
  }
  
  ### salinity dropout
  if(sum(is.na(input[,grep('salinity',columns,ignore.case = T)]))==nrow(input)){
    lat <- ifelse(is.na(mean(input[,grep('latitude',columns,ignore.case = T)],na.rm=T)),27,mean(input[,grep('latitude',columns,ignore.case = T)],na.rm=T))
    lon <- ifelse(is.na(mean(input[,grep('longitude',columns,ignore.case = T)],na.rm=T)),-82,mean(input[,grep('longitude',columns,ignore.case = T)],na.rm=T))
    pressure_dbar <- gsw_p_from_z(-input$Depth,lat)
    SP <- gsw_SP_from_C(input[,grep('actual',columns,ignore.case = T)]/1000,
                        input[,grep('temperature',columns,ignore.case = T)[1]],
                        pressure_dbar)
    input[,grep('salinity',columns,ignore.case = T)] <- gsw_SA_from_SP(SP, pressure_dbar, lon, lat)
  }
  
  ### variables of interest
  ind_date <- grep('Date',columns)
  ind_lat <- grep('Latitude',columns)
  ind_lon <- grep('Longitude',columns)
  ind_depth <- grep('Depth',columns)
  ind_all <- c(1,ind_date,ind_lat,ind_lon,ind_depth)
  
  for(i in 1:length(parms)){
    ind <- grep(parms[i],names(input),ignore.case = T)
    if(parms[i]=='temperature'){
      ind <- ind[grep('AT',names(input)[ind])]
    }
    if(parms[i]=='chlorophyll' | parms[i]=='rdo'){
      ind <- ind[grep('Concentration',names(input)[ind])]
    }
    ind_all <- c(ind_all,ind)
  }
  ### keep only variables of interest
  input <- as.data.frame(input[,ind_all])
  columns <- names(input)
  
  # average lon & lat, before filtering
  lon_avg <- mean(input[,grep('Longitude',columns)], na.rm = T)
  lat_avg <- mean(input[,grep('Latitude',columns)], na.rm = T)
  
  # order by time
  input[,grep('Date',columns)]<- ymd_hms(input[,grep('Date',columns)])
  input <- input[order(input[,grep('Date',columns)]),]
  dtime <- input[1,grep('Date',columns)]
  timestamp <- paste(year(dtime),
                     sprintf("%02d", month(dtime)),
                     sprintf("%02d", day(dtime)),
                     sprintf("%02d", hour(dtime)),
                     sprintf("%02d", minute(dtime)),
                     sep='')
  
  # filter for downcast (not up)
  row_end <- which.max(input$Depth)
  input <- input[1:row_end,]
  
  # filter out surface entries (< 2 m), except row immediately before
  ind_lt2m <- which(input$Depth < z_min)
  if (length(ind_lt2m) > 0){
    row_beg <- max(ind_lt2m) - 1
    input <- input[row_beg:nrow(input),]
  }
  if(nrow(input)<3){
    stop('\n\n not enough data \n\n')
  }
  
  ### interpolate data to smooth
  breaks <- seq(0,ceiling(max(input$Depth)),resolution)
  z_cuts <- cut(input$Depth,breaks=breaks+.5)
  levels(z_cuts) <- breaks[2:length(breaks)]
  ### for plotting
  if(plot){
    if(is.na(set_wd)){
      setwd(paste(getwd()))
    } else {
      setwd(paste(set_wd))
    }
    png(paste(timestamp,'plots.png',sep='_'), height = 10, width = 7, units = 'in', res=300)
  }
  cols <- c(2,'purple',3,4)
  par(mfrow=c(2,2))
  
  ### empty data.frame to store output
  temp_out <- data.frame(matrix(NA,length(breaks),length(parms)+3))
  temp_out[,1] <- input[1,1]
  temp_out[,2] <- input[1,2]
  temp_out[,3] <- breaks
  for(i in 1:length(parms)){
    ind <- grep(parms[i],names(input),ignore.case = T)
    # temp_rm <- smooth.spline(input$Depth,input[,ind],df=nrow(input)/3)
    temp_rm <- smooth.spline(input$Depth,input[,ind],spar=.6)
    temp_agg <- aggregate(temp_rm$y,by=list(z_cuts),mean)
    # temp_agg <- aggregate(input[,ind],by=list(z_cuts),mean)
    names(temp_agg) <- c('depths','values')
    temp_agg$depths <- as.numeric(temp_agg$depths)
    temp_int <- approx(temp_agg$depths,temp_agg$values,xout=breaks,ties=mean)
    ### save output 
    temp_out[,i+3] <- temp_int$y
    ### plot
    plot(input[,ind],-input$Depth,col=cols[i],lwd=2,typ='l',las=1,xlab='',ylab='Depth (m)')
    mtext(names(input)[ind],1,line=2)
    points(temp_int$y,-temp_int$x,lwd=1.5)
    if(i==1){
      mtext(paste('Profile index:',input[1,1],sep=' '),adj=0)
      mtext(input$`Date Time`[1],adj=0,line=1)
    }
  }
  if(plot){
    dev.off()
  }
  names(temp_out) <- c('profile_ind','date_utc','depth_m',parms)
  temp_out <- na.omit(temp_out)
  return(temp_out)
}


###--------- process_aquatroll
### <this function is soon to be depreciated> ###
# The output is a list containing the raw data and a linear interpolation of the data to plot
process_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                              lat=NA, # supply a latitude if you know it is missing
                              lon=NA, # supply a longitude if you know it is missing
                              resolution=1, # resolution in meters of the linear interpolation
                              z_min=2, # depth cutoff to start interpolation, typically there is a soak period at the surface where readings are unreliable
                              span=5, # span to calculate centered moving average
                              write_csv=T, # TRUE to save output as two csv files: the interpolated data and the raw data 
                              set_wd=NA, # set the working directory to save csv and plots; if NA, it reverts to the current working directory
                              plot=T # TRUE to plot interpolated data for visual inspection
                              )
  {
  if(file_ext(input)!='csv' & file_ext(input)!='htm'){
    warning(paste('\n\n File format needs to be csv or htm! \n\n'),
            immediate. = T)
  }
  if(file_ext(input)=='csv'){
    D <- readLines(input)
    # get header row of data
    ind <- grep('Date Time',D)
    # extract data
    D <- read_csv(input,skip=ind-1,col_types = cols())
    columns <- names(D)
    H <- read.csv(input,header=F,nrows=ind-1)
  }
  if(file_ext(input)=='htm'){
    H <- read_html(input) %>%
      html_table(fill = T) %>%
      .[[1]]
    # get header row of data
    row_d <- which(H[,1] == "Date Time")
    columns <- H[row_d,]
    # extract data
    D <- H[(row_d+1):nrow(H),]
    names(D) <- as.character(columns)
  }
  ### strip serial numbers
  names(D) <- gsub(" \\([0-9]+\\)", "", columns)
  ### data check
  if(nrow(D)<5){
    # print(paste(input,input,sep='/'))
    warning(paste('\n\n not enough data \n\n',
                  paste(input,input,sep='/'),
                  '\n\n'),
            immediate. = T)
  }
  ### Aquatroll serial number
  row_h <- which(H[,1] == "Device Model = Aqua TROLL 600 Vented")
  aquatroll_sn <- as.numeric(strsplit(as.character(H[row_h+1,1]),'=')[[1]][2])
  ### bluetooth serial number
  row_h2 <- which(H[,1] == "Device Model = In-Situ Bluetooth Device")
  handheld_sn <- as.numeric(strsplit(as.character(H[row_h2+1,1]),'=')[[1]][2])
  ### variables of interest
  ind_aquatroll <- grep(aquatroll_sn,columns)
  ind_lat <- grep('Latitude',columns)
  ind_lon <- grep('Longitude',columns)
  ind_depth <- grep('Depth',columns)
  ind_sal <- grep('Salinity',columns)
  ind_temp <- grep('Temperature',columns) %>%
    intersect(ind_aquatroll)
  ind_chl <- grep('Chlorophyll-a Concentration',columns)
  ind_chlf <- grep('Chlorophyll-a Fluorescence',columns)
  ind_oxy <- grep('RDO Concentration',columns)
  ind_cond <- grep('Actual Conductivity',columns)
  ind_cond2 <- grep('Specific Conductivity',columns)
  ind_all <- c(1,ind_lat,ind_lon,ind_depth,ind_sal,ind_temp,ind_chl,ind_chlf,ind_oxy,ind_cond,ind_cond2)
  ### keep only variables of interest
  D <- data.frame(D[,ind_all])
  D[,2:ncol(D)] <- lapply(D[,2:ncol(D)],as.numeric) ### convert from character to numeric
  ### datetime forced to UTC
  D$Date.Time <- with_tz(force_tz(ymd_hms(D$Date.Time),tz='America/New_York'),'UTC')
  dtime <- D$Date.Time[1] ### start time
  timestamp <- paste(year(dtime),month(dtime),day(dtime),hour(dtime),minute(dtime),sep='')
  ### what are depth units?
  depth_unit <- substr(names(D)[grep('Depth',names(D))],8,9)
  ### convert to meter if needed
  if(depth_unit=='ft'){ 
    D$Depth..m. <- D$Depth..ft.*0.3048
    D <- D[,-which(names(D)=='Depth..ft.')]
  }
  ### calculate salinity from conductivity if needed
  if(length(ind_sal)==0){
    lat <- ifelse(is.na(mean(D$Latitude....,na.rm=T)),27,mean(D$Latitude....,na.rm=T))
    lon <- ifelse(is.na(mean(D$Longitude....,na.rm=T)),-82,mean(D$Longitude....,na.rm=T))
    pressure_dbar <- gsw_p_from_z(-D$Depth..m.,lat)
    SP <- gsw_SP_from_C(D$Specific.Conductivity..µS.cm./1000,
                        D$Temperature...C.,
                        pressure_dbar)
    D$Salinity..ppt. <- gsw_SA_from_SP(SP, pressure_dbar, lon, lat)
  }
  ### required fields
  flds_req <- c("Date.Time", "Latitude....", "Longitude....", "Depth..m.", "Salinity..ppt.", "Temperature...C.", "Chlorophyll.a.Concentration..µg.L.", "Chlorophyll.a.Fluorescence..RFU.", "RDO.Concentration..mg.L.",'Actual.Conductivity..µS.cm.','Specific.Conductivity..µS.cm.')
  re_names <- c('dtime_utc','lat_dd','lon_dd','depth_m','sal_ppt','temp_c','chl_ugl','chl_rfu','oxy_mgl','actual_conductivity_uScm','specfic_conductivity_uScm')
  ### reorder columns to be consistent
  D <- D[,order(names(D))]
  reorder <- order(flds_req)
  flds_req <- flds_req[reorder]
  re_names <- re_names[reorder]
  ### missing fields
  flds_miss <- setdiff(flds_req, names(D))
  ind_miss <- which(is.element(flds_req,flds_miss))
  if (length(flds_miss) > 0){
    warning(paste("\n\nMissing fields in csv: ",
                  paste(flds_miss, collapse=', '),
                  "\n\n",
                  paste(input,input,sep='/'),
                  "\n\n"),
            immediate. = F)
    names(D) <- re_names[-ind_miss]
  } else {
    names(D) <- re_names
    flds_miss <- NA
  }
  ### if you know lat/lon is missing
  if(!is.na(lat) & !is.na(lon)){
    warning(paste("\n\nUser supplied fields added: ",
                  paste(flds_miss, collapse=', '),
                  "\n\n",
                  paste(input,input,sep='/'),
                  "\n\n"))
    D$lat_dd <- lat
    D$lon_dd <- lon
  }
  ### if lat/lon is missing, interactively add them in
  flds_miss <- setdiff(re_names, names(D))
  # if (length(flds_miss) > 0){
  if (is.element('lon_dd',flds_miss) & is.element('lat_dd',flds_miss)){
    warning("\n\nAdding fields: Latitude, Longitude \n\n")
    lat_i <- readline(prompt = 'Enter latitude (decimal degrees): ')
    lon_i <- readline(prompt = 'Enter longitude (decimal degrees): ')
    D$lat_dd <- as.numeric(lat_i)
    D$lon_dd <- as.numeric(lon_i)
  }
  # average lon & lat, before filtering
  lon_avg <- mean(D$lon_dd, na.rm = T)
  lat_avg <- mean(D$lat_dd, na.rm = T)
  # order by time
  D <- D[order(D$dtime_utc),]
  # filter for downcast (not up)
  row_end <- which.max(D$depth_m)
  D <- D[1:row_end,]
  # filter out surface entries (< 2 m), except row immediately before
  ind_lt2m <- which(D$depth_m < z_min)
  if (length(ind_lt2m) > 0){
    row_beg <- max(ind_lt2m) - 1
    D <- D[row_beg:nrow(D),]
  }
  if(nrow(D)<3){
    stop('\n\n not enough data \n\n')
  }
  # average lon & lat, after filtering
  if (all(is.na(D$lon_dd) | is.na(D$lat_dd))){
    D$lon_dd <- lon_avg
    D$lat_dd <- lat_avg
  } else {
    D$lat_dd <- mean(D$lat_dd, na.rm = T)
    D$lon_dd <- mean(D$lon_dd, na.rm = T)
  }
  ### add input filename for reference
  D$input <- input
  ### interpolate data to smooth
  breaks <- seq(0,ceiling(max(D$depth_m)),resolution)
  z_cuts <- cut(D$depth_m,breaks=breaks+.5)
  levels(z_cuts) <- breaks[2:length(breaks)]
  # temperature
  if(!is.element('temp_c',flds_miss) & sum(!is.na(D$temp_c))==nrow(D)){
    temp_rm <- running(D$depth_m,D$temp_c,span,2)
    temp_agg <- aggregate(temp_rm$r.mean,by=list(z_cuts),mean)
    names(temp_agg) <- c('depths','values')
    temp_agg$depths <- as.numeric(temp_agg$depths)
    temp_int <- approx(temp_agg$depths,temp_agg$values,xout=breaks,ties=mean)
  } else {
    temp_int <- data.frame(x=-999,y=-999)
  }
  # salinity
  if(!is.element('sal_ppt',flds_miss) & sum(!is.na(D$sal_ppt))==nrow(D)){
    sal_rm <- running(D$depth_m,D$sal_ppt,span,2)
    sal_agg <- aggregate(sal_rm$r.mean,by=list(z_cuts),mean)
    names(sal_agg) <- c('depths','values')
    sal_agg$depths <- as.numeric(sal_agg$depths)
    sal_int <- approx(sal_agg$depths,sal_agg$values,xout=breaks,ties=mean)
  } else {
    sal_int <- data.frame(x=-999,y=-999)
  }
  # chlorophyll
  if(!is.element('chl_ugl',flds_miss) & sum(!is.na(D$chl_ugl))==nrow(D)){
    chl_rm <- running(D$depth_m,D$chl_ugl,span,2)
    chl_agg <- aggregate(chl_rm$r.mean,by=list(z_cuts),mean)
    names(chl_agg) <- c('depths','values')
    chl_agg$depths <- as.numeric(chl_agg$depths)
    chl_int <- approx(chl_agg$depths,chl_agg$values,xout=breaks,ties=mean)
  } else {
    chl_int <- data.frame(x=-999,y=-999)
  }
  # dissolved oxygen
  if(!is.element('oxy_mgl',flds_miss) & sum(!is.na(D$oxy_mgl))==nrow(D)){
    do_rm <- running(D$depth_m,D$oxy_mgl,span,2)
    do_agg <- aggregate(do_rm$r.mean,by=list(z_cuts),mean)
    names(do_agg) <- c('depths','values')
    do_agg$depths <- as.numeric(do_agg$depths)
    do_int <- approx(do_agg$depths,do_agg$values,xout=breaks,ties=mean)
  } else {
    do_int <- data.frame(x=-999,y=-999)
  }
  # save output
  data_interp <- data.frame(input=input,
                            dtime_utc=dtime,
                            lat_dd=D$lat_dd[1],
                            lon_dd=D$lon_dd[1],
                            depth_m=breaks,
                            temp_c=temp_int$y,
                            sal_ppt=sal_int$y,
                            chl_ugl=chl_int$y,
                            do_mgl=do_int$y)
  
  if(plot){
    if(is.na(set_wd)){
      setwd(paste(getwd()))
    } else {
      setwd(paste(set_wd))
    }
    png(paste(timestamp,aquatroll_sn,'plots.png',sep='_'), height = 10, width = 7, units = 'in', res=300)
    par(mfrow=c(2,2))
    plot(data_interp$temp_c, -data_interp$depth_m, typ='l', xlab='temperature (C)', ylab='Depth (m)',lwd=2)
    points(D$temp_c,-D$depth_m,col=2,typ='l',lwd=2,lty=2)
    mtext(input,adj=0,line=2)
    mtext(dtime,adj=0)
    
    plot(data_interp$sal_ppt, -data_interp$depth_m, typ='l', xlab='salinity (ppt)', ylab='Depth (m)',col=2,lwd=2)
    points(D$sal_ppt,-D$depth_m,col=1,typ='l',lwd=2,lty=2)
    
    plot(data_interp$chl_ugl, -data_interp$depth_m, typ='l', xlab='chlorophyll-a (ug/l)', ylab='Depth (m)',col=3,lwd=2)
    points(D$chl_ugl,-D$depth_m,col=2,typ='l',lwd=2,lty=2)
    
    plot(data_interp$do_mgl, -data_interp$depth_m, typ='l', xlab='dissolved oxygen (mg/l)', ylab='Depth (m)',col='purple',lwd=2)
    points(D$do_mgl,-D$depth_m,col=2,typ='l',lwd=2,lty=2)
    dev.off()
  }
  ### save output as csv
  if(write_csv){
    if(is.na(set_wd)){
      setwd(paste(getwd()))
    } else {
      setwd(paste(set_wd))
    }
    write.csv(D,paste(dtime,'_raw.csv',sep=''),row.names = F)
    write.csv(data_interp,paste(dtime,'_interp.csv',sep=''),row.names = F)
  }
  return(list(data_interp,D,aquatroll_sn))
}


###--------- bottom_finder
### finds the max depth per station for plotting bottom on section plots
### better method would be to find bathymetry from gridded bathymetry like ETOPO1 or CRM
bottom_finder <- function(longitudes, # vector of longitudes
                          depths # vector of depths corresponding to the longitudes
                          )
  {
  unique_lon <- unique(longitudes)
  bottom <- matrix(NA,length(unique_lon),2)
  for(i in 1:length(unique_lon)){
    indx <- which.min(depths[longitudes==unique_lon[i]])
    bottom[i,1] <- depths[longitudes==unique_lon[i]][indx]
    bottom[i,2] <- unique_lon[i]
  }
  return(bottom)
}


###--------- creates breaks for color palettes
breaks <- function(x, # vector of values to calculate breaks
                   int, # number: increment of the sequence
                   decimal = F # decimal is good for chlorophyll or other values logarithmic space
                   )
  {
  r_range <- range(x,na.rm=T)
  if(decimal){ ### decimal is good for chlorophyll or other values logarithmic space
    seqs <- seq(round(r_range[1],2),round(r_range[2],2),by=int)
  } else {
    seqs <- seq(floor(r_range[1]),ceiling(r_range[2]),by=int)  
  }
  return(seqs)
}


###--------- running linear slope and mean
running  <- function (x, # x vector used to calculate slope and mean
                      y, # y vector used to calculate slope and mean
                      r, # span to calculate the running parameter
                      b # indicate whether to calculate the running slope (1), mean (2), or both (3)
                      )
  {
  ind <- floor(r/2)
  y.t <- rep(NA,length(y))
  y.t4 <- rep(NA,length(y))
  if(any(is.na(y))){
    y.1 <- na.omit(y)
    y.2 <- na.action(y.1)
    x.1 <- x[-y.2]
  } else {
    y.1 <- y
    x.1 <- x
  }
  r_lm_b <- r_lm_b2 <- r_lm_b3 <- r_lm_b4 <- rep(NA,length(y.1))
  for(i in (ind):(length(y.1)-ind)){ # min = r*2+1
    if(b==1){
      r_lm_b[i] <- coefficients(lm(y.1[(i-ind+1):(i+ind)]~x.1[(i-ind+1):(i+ind)]))[2] 
    }
    if(b==2){
      r_lm_b4[i] <- mean(y[(i-ind+1):(i+ind)],na.rm=T)
    }
    if(b==3){
      r_lm_b[i] <- coefficients(lm(y.1[(i-ind+1):(i+ind)]~x.1[(i-ind+1):(i+ind)]))[2]
      r_lm_b4[i] <- mean(y.1[(i-ind+1):(i+ind)],na.rm=T)
    }
  }
  if(any(is.na(y))){
    y.t[-y.2] <- r_lm_b
    y.t4[-y.2] <- r_lm_b4
  } else {
    y.t <- r_lm_b
    y.t4 <- r_lm_b4
  }
  return(data.frame(slope=y.t,r.mean=y.t4))
}
