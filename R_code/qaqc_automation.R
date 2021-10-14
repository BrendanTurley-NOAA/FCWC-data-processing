### aquatrol quality control

### Recommended methods to comply with IOOS standards
# https://ioos.noaa.gov/project/qartod/

# ##flags
# Pass=1
# Not evaluated=2
# Suspect or Of High Interest=3
# Fail=4
# Missing data=9

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
  
### test data
library(lubridate)
library(ncdf4)
library(rgdal)

# source('~/Desktop/professional/projects/Postdoc_FL/scripts/FCWC/FCWC_process_htm.R')
source('~/Documents/R/Github/FCWC-data-processing/R_code/aquatroll_processing.R')

### find htm files
files_wd <- '~/Desktop/professional/projects/Postdoc_FL/data/FCWC/new/CaptivaPass'
setwd(files_wd)
files <- list.files()
ind <- grep('htm',files)

input1 <- files[2]
file_sum <- summary_aquatroll(input1)
input <- data_extract_aquatroll(input1)
 

flags <- data.frame(matrix(2,dim(input)[1],dim(input)[2]-4))
names(flags) <- names(input)[1:(ncol(input)-4)]
###--------- Test 1) Timing/Gap Test
ind <- grep('date',names(input),ignore.case=T)
flags[,ind] <- 1
time_stamp <- ymd_hms(input[,ind])
dt <- make_difftime(diff(time_stamp),units='seconds')
n_flags <- which(abs(dt)>1)
if(length(n_flags)>0){
  flags[n_flags,ind] <- 4
}

###--------- Test 3) Location Test
ind_lon <- grep('longitude',names(input),ignore.case=T)
ind_lat <- grep('latitude',names(input),ignore.case=T)
flags[,ind_lon] <- 1
flags[,ind_lat] <- 1
lon <- input[,ind_lon]
lat <- input[,ind_lat]
### missing data
lon_f1 <- which(is.na(lon))
lat_f1 <- which(is.na(lat))
if(length(lon_f1)>0){
  flags[lon_f1,ind_lon] <- 9
}
if(length(lat_f1)>0){
  flags[lat_f1,ind_lat] <- 9
}
### impossible locations
lon_f2 <- which(abs(lon)>180)
lat_f2 <- which(abs(lat)>90)
if(length(lon_f2)>0){
  flags[lon_f2,ind_lon] <- 4
}
if(length(lat_f2)>0){
  flags[lat_f2,ind_lat] <- 4
}


###--------- Test 4) Gross Range Test
# temperature
ind <- grep('temperature',names(input),ignore.case=T)
flags[,ind] <- 1
temp <- input[,ind]
### missing data
t1_f1 <- which(is.na(temp[,1]))
t2_f1 <- which(is.na(temp[,2]))
if(length(t1_f1)>0){
  flags[t1_f1,ind[1]] <- 9
}
if(length(t2_f1)>0){
  flags[t2_f1,ind[2]] <- 9
}
### out of range
t1_f2 <- which(temp[,1]>50 | temp[,1]<(-5))
t2_f2 <- which(temp[,2]>50 | temp[,2]<(-5))
if(length(t1_f2)>0){
  flags[t1_f2,ind[1]] <- 4
}
if(length(t2_f2)>0){
  flags[t2_f2,ind[2]] <- 4
}
# conductivity
ind <- grep('conductivity',names(input),ignore.case=T)
flags[,ind] <- 1
cond <- input[,ind]
### missing data
c1_f1 <- which(is.na(cond[,1]))
c2_f1 <- which(is.na(cond[,2]))
if(length(c1_f1)>0){
  flags[c1_f1,ind[1]] <- 9
}
if(length(c2_f1)>0){
  flags[c2_f1,ind[2]] <- 9
}
### out of range
c1_f2 <- which(cond[,1]>350000 | cond[,1]<0)
c2_f2 <- which(cond[,2]>350000 | cond[,2]<0)
if(length(c1_f2)>0){
  flags[c1_f2,ind[1]] <- 4
}
if(length(c2_f2)>0){
  flags[c2_f2,ind[2]] <- 4
}
# salinity
ind <- grep('salinity',names(input),ignore.case=T)
flags[,ind] <- 1
sal <- input[,ind]
### missing data
s_f1 <- which(is.na(sal))
if(length(s_f1)>0){
  flags[s_f1,ind] <- 9
}
### out of range
s_f2 <- which(sal>350 | sal<0)
if(length(s_f2)>0){
  flags[s_f2,ind] <- 4
}
# chlorophyll
ind <- grep('chlorophyll',names(input),ignore.case=T)
flags[,ind] <- 1
chl <- input[,ind]
### missing data
c1_f1 <- which(is.na(chl[,1]))
c2_f1 <- which(is.na(chl[,2]))
if(length(c1_f1)>0){
  flags[c1_f1,ind[1]] <- 9
}
if(length(c2_f1)>0){
  flags[c2_f1,ind[2]] <- 9
}
### out of range
c1_f2 <- which(chl[,1]>100 | chl[,1]<0)
c2_f2 <- which(chl[,2]>1000 | chl[,2]<0)
if(length(c1_f2)>0){
  flags[c1_f2,ind[1]] <- 4
}
if(length(c2_f2)>0){
  flags[c2_f2,ind[2]] <- 4
}
# oxygen
ind <- grep('rdo concentration',names(input),ignore.case=T)
flags[,ind] <- 1
rdo <- input[,ind]
### missing data
o_f1 <- which(is.na(rdo))
if(length(o_f1)>0){
  flags[o_f1,ind] <- 9
}
### out of range
o_f2 <- which(rdo>50 | rdo<0)
if(length(o_f2)>0){
  flags[o_f2,ind] <- 4
}



###--------- aquatrol quality control
qaqc_aquatroll <- function (input # input file is the output data.frame from data_extract_aquatroll function
)
{
  
}