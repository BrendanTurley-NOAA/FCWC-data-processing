### aquatrol quality control ver. 2 (updated 2022/08/23)

### Recommended methods to comply with IOOS standards
# https://ioos.noaa.gov/project/qartod/

### flags
# 1 = Pass
# 2 = Not evaluated
# 3 = Suspect or Of High Interest
# 4 = Fail
# 9 = Missing data

### tests
# Group 1
# Required
# Test 1) Timing/Gap Test
# Test 2) Syntax Test
# Test 3) Location Test
# Test 4) Gross Range Test - find sensor ranges https://in-situ.com/pub/media/support/documents/AquaTROLL600_Spec-Sheet.pdf
# Test 5) Climatological Test

# Group 2
# Strongly Recommended
# Test 6) Spike Test
# Test 7) Rate of Change Test
# Test 8) Flat Line Test

# Group 3
# Suggested
# Test 9) Multi-Variate Test
# Test 10) Attenuated Signal Test
# Test 11) Neighbor Test
# Test 12) TS Curve/Space Test
# Test 13) Density Inversion Test

library(lubridate)

### find htm files
files_wd <- '~/Desktop/professional/projects/Postdoc_FL/data/FCWC/processed'
setwd(files_wd)
data_raw <- read.csv('aquatroll_raw_data_extracted.csv')


tmp <- data_raw[which(data_raw$profile.index==5),]


###--------- Test 1) Timing/Gap Test
data <- tmp$Date.Time
t_inc = 2

timing_gap_test <- function(data, t_inc = 2){
  flags <- rep(1,length(data))
  time_stamp <- ymd_hms(data)
  dt <- make_difftime(diff(time_stamp),units='seconds')
  n_flags <- which(abs(dt)>t_inc)
  if(length(n_flags)>0){
    flags[n_flags] <- 4
  }
  if(any(is.na(data))){
    flags[is.na(data)] <- 9
  }
  return(flags)
}

timing_gap_test(tmp$Date.Time)


###--------- Test 3) Location Test

lon <- tmp$Longitude....
lat <- tmp$Latitude....
threshold <- 20/111000 # approx 20 m

location_test <- function(longitude, latitude, threshold = 20/111000){
  flags <- matrix(1,length(longitude),2)
  ### unlikely displacement
  lon_diff <- abs(diff(lon))>threshold
  if(any(lon_diff==T,na.rm=T)){
    flags[which(lon_diff),2] <- 3
  }
  lat_diff <- abs(diff(lat))>threshold
  if(any(lat_diff==T,na.rm=T)){
    flags[which(lat_diff),2] <- 3
  }
  ### impossible locations; supercedes unlikely displacement
  lon_f2 <- which(abs(lon)>180)
  lat_f2 <- which(abs(lat)>90)
  if(length(lon_f2)>0){
    flags[lon_f2,1] <- 4
  }
  if(length(lat_f2)>0){
    flags[lat_f2,2] <- 4
  }
  if(any(is.na(longitude))){
    flags[is.na(longitude),1] <- 9
  }
  if(any(is.na(latitude))){
    flags[is.na(latitude),2] <- 9
  }
  return(flags)
}

location_test(lon,lat)




### qartod: in-situ temperature and salinity data


# Test 4) Gross Range Test


temp <- tmp$Temperature...C..AT
data <- temp

gross_range <- function(data, parameter = c('temperature','conductivity','salinity','dissolved_oxygen')){
  ### sensor ranges https://in-situ.com/pub/media/support/documents/AquaTROLL600_Spec-Sheet.pdf
  at_temp_C_sensor_range = c(-5, 50)
  at_cond_uScm_sensor_range = c(0, 350000)
  at_sal_psu_sensor_range = c(0, 350)
  at_do_mgl_sensor_range = c(0, 20)
  
  flags <- rep(1,length(data))
  if(parameter=='temperature'){
    ind <- (data<at_temp_C_sensor_range[1] | data>at_temp_C_sensor_range[2])
  }
  if(parameter=='conductivity'){
    ind <- (data<at_cond_uScm_sensor_range[1] | data>at_cond_uScm_sensor_range[2])
  }
  if(parameter=='salinity'){
    ind <- (data<at_sal_psu_sensor_range[1] | data>at_sal_psu_sensor_range[2])
  }
  if(parameter=='dissolved_oxygen'){ ### check QARTOD QA/QC manual on DO
    ind <- (data<at_do_mgl_sensor_range[1] | data>at_do_mgl_sensor_range[2])
  }
  if(any(ind==T)){
    flags[ind] <- 4
  }
  return(flags)
}

gross_range(temp,'temperature')
gross_range(temp)

