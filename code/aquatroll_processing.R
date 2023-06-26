### These functions processes htm files produced by Aquatroll units used by FCWC

### coded on a Macbook, so be wary if you try to run on Windows as some of the coding is different
# sessionInfo()
# R version 4.1.0 (2021-05-18)
# Platform: x86_64-apple-darwin17.0 (64-bit)
# Running under: macOS Big Sur 11.2.3

# Last update:
# Sys.time()
# "2022-06-17 10:43:48 CDT"
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
rm(r)



###--------- aquatroll_fields
### just a list of standard parameters reported in aquatroll htm files
aquatroll_fields_v1 <- function(){
  c("Date Time",
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
}

aquatroll_fields_v2 <- function(){ # newer sensors report salinity as PSU; updated 2022/11/18
  c("Date Time",
    "Salinity (PSU)",
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
}

###--------- check_headers
check_headers <- function(input # htm or csv file that contains the raw aquatroll output
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
  }
  if(file_ext(input)=='htm'){
    H <- read_html(input) %>%
      html_table(fill = T) %>%
      .[[1]]
    # get header row of data
    row_d <- which(H[,1] == "Date Time")
    columns <- H[row_d,]
  }
  gsub(" \\([0-9]+\\)", "", columns)
}


###--------- look_aquatroll
look_aquatroll <- function(input # htm or csv file that contains the raw aquatroll output
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
  H <- data.frame(H)
  D <- data.frame(D)
  D[2:(ncol(D)-1)] <- lapply(D[2:(ncol(D)-1)],as.numeric)
  names(D) <- gsub(" \\([0-9]+\\)", "", columns)
  ### return serial numbers for sensors
  serials <- as.numeric(gsub(".*?([0-9]+).*", "\\1", columns))
  
  out <- list(data = D,
              serials = serials)
  
  return(out)
}


###--------- summary_aquatroll
summary_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                              ignore.marked = T, # TRUE will ignore the marked column from being exported in the output
                              flds_req = aquatroll_fields_v2() # fields required for exporting raw data; default to newer set (2022/11/18)
)
{
  # ### fields required for exporting raw data 
  # flds_req <- c("Date Time",
  #               "Salinity (ppt)",
  #               "Temperature (°C) AT",
  #               "Depth",
  #               "Pressure (psi)",
  #               "Actual Conductivity (µS/cm)",
  #               "Specific Conductivity (µS/cm)",
  #               "Total Dissolved Solids (ppt)",
  #               "Resistivity (Ω⋅cm)",
  #               "Density (g/cm³)",
  #               "Barometric Pressure (mm Hg)",
  #               "RDO Concentration (mg/L)",
  #               "RDO Saturation (%Sat)",
  #               "Oxygen Partial Pressure (Torr)",
  #               "Chlorophyll-a Fluorescence (RFU)",
  #               "Chlorophyll-a Concentration (µg/L)",
  #               "Battery Capacity (%)",
  #               "External Voltage (V)",
  #               "Barometric Pressure (mbar)",
  #               "Temperature (°C) HH",
  #               "Latitude (°)",
  #               "Longitude (°)",
  #               "Marked")
  
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
  D[2:(ncol(D)-1)] <- lapply(D[2:(ncol(D)-1)],as.numeric)
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
  ### min oxygen
  do_min <- min(D[,grep('RDO Concentration',columns,ignore.case = T)],na.rm=T)
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
                    do_min=do_min,
                    dz_dt=dz_dt,
                    lon_dd=lon_avg,
                    lat_dd=lat_avg,
                    flds_miss=toString(flds_miss),
                    dropout=toString(dropout))
  return(out)
}


###--------- data_extract_aquatroll
data_extract_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                                   flds_req = aquatroll_fields_v2() # fields required for exporting raw data; default to newer set (2022/11/18)
)
{
  # ### fields required for exporting raw data
  # flds_req <- c("Date Time",
  #               "Salinity (ppt)",
  #               "Temperature (°C) AT",
  #               "Depth",
  #               "Pressure (psi)",
  #               "Actual Conductivity (µS/cm)",
  #               "Specific Conductivity (µS/cm)",
  #               "Total Dissolved Solids (ppt)",
  #               "Resistivity (Ω⋅cm)",
  #               "Density (g/cm³)",
  #               "Barometric Pressure (mm Hg)",
  #               "RDO Concentration (mg/L)",
  #               "RDO Saturation (%Sat)",
  #               "Oxygen Partial Pressure (Torr)",
  #               "Chlorophyll-a Fluorescence (RFU)",
  #               "Chlorophyll-a Concentration (µg/L)",
  #               "Battery Capacity (%)",
  #               "External Voltage (V)",
  #               "Barometric Pressure (mbar)",
  #               "Temperature (°C) HH",
  #               "Latitude (°)",
  #               "Longitude (°)",
  #               "Marked")
  
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
  # ### add input filename
  D$input <- paste(input)
  ### output
  D[,c(2:22,24)] <- sapply(D[,c(2:22,24)],as.numeric)
  return(D)
}


###--------- interp_aquatroll
interp_aquatroll <- function (input, # input file is the output data.frame from data_extract_aquatroll function
                              parms = c('temperature','salinity','chlorophyll','oxygen'), # parameters that you want to extract, smooth, and interpolate
                              shallow = F, # True if inshore or less than 3 meters of water; makes the interpolation more reasonable
                              downcast = F, # use downcast, if False, upcast is used
                              sal_calc = T, # calculate practical salinity from conductivity; consistent with Walton Smith data reported as practical salinity
                              z_min = 2, # depth cutoff to start interpolation, typically there is a soak period at the surface where readings are unreliable
                              spar = .6, # smoothing parameter for smooth.spline before interpolation; values 0:1 with higher values are more smooth
                              resolution = 1, # resolution in meters of the interpolation
                              set_wd = NA, # set the working directory to save csv and plots; if NA, it reverts to the current working directory
                              save_plot = T # TRUE to plot interpolated data for visual inspection
)
{
  ### save current working directory
  wd_now <- getwd()
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
  
  ### save serial number
  aquatroll_sn <- input[1,grep('aquatroll_sn',names(input))]
  
  ### convert feet to meters
  if(unique(input[grep('unit',names(input))])=='(ft)'){
    input$Depth <- NISTftTOmeter(input$Depth)
  }
  columns <- names(input)
  
  ### chlorophyll dropout
  if(sum(is.na(input[,grep('chlorophyll',columns,ignore.case = T)[2]]))==nrow(input)){
    ind <- grep('chlorophyll',columns,ignore.case = T)
    input[,ind[2]] <- input[,ind[1]]*10
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
  
  ### calculate salinity
  if(sal_calc==T){
    lat <- ifelse(is.na(mean(input[,grep('latitude',columns,ignore.case = T)],na.rm=T)),27,mean(input[,grep('latitude',columns,ignore.case = T)],na.rm=T))
    lon <- ifelse(is.na(mean(input[,grep('longitude',columns,ignore.case = T)],na.rm=T)),-82,mean(input[,grep('longitude',columns,ignore.case = T)],na.rm=T))
    pressure_dbar <- gsw_p_from_z(-input$Depth,lat)
    SP <- gsw_SP_from_C(input[,grep('actual',columns,ignore.case = T)]/1000,
                        input[,grep('temperature',columns,ignore.case = T)[1]],
                        pressure_dbar)
    input[,grep('salinity',columns,ignore.case = T)] <- SP
  }
  
  ### variables of interest
  ind_date <- grep('Date',columns)
  ind_lat <- grep('Latitude',columns)
  ind_lon <- grep('Longitude',columns)
  ind_depth <- grep('Depth',columns)
  ind_input <- grep('input',columns)
  ind_all <- c(ind_date,ind_lat,ind_lon,ind_depth,ind_input)
  
  for(i_par in 1:length(parms)){
    ind <- grep(parms[i_par],names(input),ignore.case = T)
    if(parms[i_par]=='temperature'){
      ind <- ind[grep('AT',names(input)[ind])]
    }
    if(parms[i_par]=='chlorophyll'){
      # ind <- ind[grep('Concentration',names(input)[ind])]
      ind <- ind[grep('Fluorescence',names(input)[ind])]
    }
    if(parms[i_par]=='rdo'){
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
  
  if(downcast==T){
    input <- input[1:row_end,]
    
    # filter out surface entries (< user set threshold), except row immediately before
    z_min <- ifelse(min(input$Depth, na.rm=T) > z_min, min(input$Depth, na.rm=T), z_min)
    ind_lt2m <- which(input$Depth < z_min)
    if (length(ind_lt2m) > 0){
      row_beg <- max(ind_lt2m)
      input <- input[row_beg:nrow(input),]
    }
  }
  
  if(downcast==F){
    input <- input[row_end:nrow(input),]
    
    # filter out surface entries (< 2 m), except row immediately before
    ind_lt2m <- which(input$Depth < 0)
    if (length(ind_lt2m) > 0){
      row_end <- max(ind_lt2m)
      input <- input[1:row_end,]
    }
  }
  
  ### smooth, bin, and plot
  if(nrow(input)<3 | max(input$Depth)<=0){
    warning('\n\n not enough data \n\n')
    return(NULL)
  } else {
    ### interpolate data to smooth
    if(shallow==T){ ### added 20220616 for shallow casts
      breaks <- seq(0,round(max(input$Depth),1)+resolution,resolution)
    } else {
      breaks <- seq(0,ceiling(max(input$Depth)),resolution) 
    }
    # z_cuts <- cut(input$Depth,breaks=breaks+.5)
    # levels(z_cuts) <- breaks[2:length(breaks)]
    
    ### for plotting
    if(save_plot){
      if(is.na(set_wd)){
        # setwd(paste(getwd()))
        setwd(paste(wd_now))
      } else {
        setwd(paste(set_wd))
      }
      png(paste(timestamp,'plots.png',sep='_'), height = 10, width = 7, units = 'in', res=300)
    }
    cols <- c(2,'purple',3,4)
    par(mfrow=c(2,2),mar=c(4,4,3,1),oma=c(1,1,2,1))
    
    ### empty data.frame to store output
    temp_out <- data.frame(matrix(NA,length(breaks),length(parms)+4))
    temp_out[,1] <- as.character(input[1,1])
    temp_out[,2] <- lon_avg
    temp_out[,3] <- lat_avg
    temp_out[,4] <- breaks
    for(i_par in 1:length(parms)){
      er <- F ### revert error code for ploting
      ind <- grep(parms[i_par],names(input),ignore.case = T)
      
      ### dealing with NAs because smooth.spline does not accept NAs
      if(all(is.na(input[,ind]))){ ### if all NA, then add -999 and error code for ploting
        temp_int <- data.frame(x=breaks,y=-999)
        input[,ind] <- -999
        er <- T
      } else if(any(is.na(input[,ind])) & !all(is.na(input[,ind]))){ ### if some NAs, attempt imputation
        na_impute <- approx(input$Depth,input[,ind],input$Depth[which(is.na(input[,ind]))])
        input[which(is.na(input[,ind])),ind] <- na_impute$y
      }
      if(all(!is.na(input[,ind]))){ ### if no NAs, smooth and bin data
        temp_rm <- smooth.spline(input$Depth,input[,ind],spar=spar)
        # z_cuts <- cut(temp_rm$x,breaks=breaks+.5)
        # z_cuts <- cut(temp_rm$x,breaks=breaks) # 20220616; modified for shallow waters, adding .5 unnecessary
        z_cuts <- cut(temp_rm$x,breaks=breaks+(resolution/2)) # add offset to center boxcar average
        levels(z_cuts) <- breaks[2:length(breaks)]
        temp_agg <- aggregate(temp_rm$y,by=list(z_cuts),mean)
        # temp_agg <- aggregate(input[,ind],by=list(z_cuts),mean) ### jsut bin instead of smooth; depreciated
        names(temp_agg) <- c('depths','values')
        # temp_agg$depths <- as.numeric(temp_agg$depths)
        temp_agg$depths <- as.numeric(paste(temp_agg$depths)) ### incorrectly converting factor into numeric; 2022/06/16
        temp_int <- approx(temp_agg$depths,temp_agg$values,xout=breaks,ties=mean)
      } else { ### if still NAs, then add -999 and error code for ploting
        temp_int <- data.frame(x=breaks,y=-999)
        er <- T
      }
      ### save output 
      temp_out[,i_par+4] <- temp_int$y
      ### plot
      plot(input[,ind],-input$Depth,col=cols[i_par],lwd=2,typ='l',las=1,xlab='',ylab='')
      mtext(names(input)[ind],1,line=2.5)
      mtext('Depth (m)',2,line=2.5)
      points(temp_int$y,-temp_int$x,lwd=1.5)
      if(i_par==1){
        mtext(paste('Date collected:',input[1,grep('Date',columns)]),adj=0)
      }
      if(i_par==2){
        legend('topright',c('Raw data','Interpolated'),
               lty=c(1,NA),pch=c(NA,1),col=c('purple',1),
               lwd=c(2,1.5),bty='n')
      }
      if(i_par==4){
        mtext(paste('Location: N ',sprintf('%.3f',round(lat_avg,3)),', W ',
                    sprintf('%.3f',round(abs(lon_avg),3)),sep=''),
              outer=T,side=3,at=.05,adj=0)
        mtext(paste('Input file:',input[1,grep('input',columns)]),
              outer=T,side=3,line=-1,at=.05,adj=0)
        mtext(paste('AquaTroll Serial number:',aquatroll_sn),
              outer=T,side=3,adj=1)
      }
      if(er){ ### plot if NAs error and no smoothing/binning
        mtext('NAs approx error',col='red')
      }
    }
    if(save_plot){
      dev.off()
    }
    ### return to original working directory
    setwd(paste(wd_now))
    ### rename columns for output
    names(temp_out) <- c('date_utc','lon_dd','lat_dd','depth_m',parms)
    temp_out <- temp_out[which(!is.na(temp_out$temperature)),]
    return(temp_out)
  }
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
    bottom[i,1] <- unique_lon[i]
    bottom[i,2] <- depths[longitudes==unique_lon[i]][indx]
  }
  bottom <- bottom[order(bottom[,1]),]
  bottom <- data.frame(lon=c(bottom[1,1]-1,bottom[,1],bottom[nrow(bottom),1]+1),
                       z=c(bottom[1,2]-100,bottom[,2],bottom[nrow(bottom),2]-100))
  return(bottom)
}


###--------- creates breaks for color palettes
breaks <- function(x, # vector of values to calculate breaks
                   int, # number: increment of the sequence
                   decimal = F, # decimal is good for chlorophyll or other values logarithmic space
                   round_digits = 2 # if decimal, what number of decimal places
)
{
  r_range <- range(x,na.rm=T)
  if(decimal){ ### decimal is good for chlorophyll or other values logarithmic space
    seqs <- seq(round(r_range[1],round_digits),round(r_range[2],round_digits),by=int)
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



###--------- which_xy
which_xy <- function(report,serial){
  report_out <- report[which(report$aquatroll_sn==serial),]
  lon_m <- mean(abs(diff(report_out$lon_dd)))
  lat_m <- mean(abs(diff(report_out$lat_dd)))
  res <- ifelse(lon_m>lat_m,'lon_dd','lat_dd')
  return(res)
}


###--------- check_neg
check_neg <- function(x){
  ifelse(x<0,0,x)
}


###--------- depth_neg
depth_neg <- function(depth){
  if(all(depth>0)){
    -depth
  } else {
    depth
  }
}

