###--------- process_aquatroll
### <this function is depreciated, but code remains here for reference> ###
# The output is a list containing the raw data and a linear interpolation of the data to plot
process_aquatroll <- function(input, # htm or csv file that contains the raw aquatroll output
                              lat = NA, # supply a latitude if you know it is missing
                              lon = NA, # supply a longitude if you know it is missing
                              resolution = 1, # resolution in meters of the linear interpolation
                              z_min = 2, # depth cutoff to start interpolation, typically there is a soak period at the surface where readings are unreliable
                              span = 5, # span to calculate centered moving average
                              write_csv = T, # TRUE to save output as two csv files: the interpolated data and the raw data 
                              set_wd = NA, # set the working directory to save csv and plots; if NA, it reverts to the current working directory
                              plot = T # TRUE to plot interpolated data for visual inspection
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