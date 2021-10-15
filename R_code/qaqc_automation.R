### aquatrol quality control

### Recommended methods to comply with IOOS standards
# https://ioos.noaa.gov/project/qartod/

### flags
# 1 = Pass
# 2 = Not evaluated
# 3 = Suspect or Of High Interest
# 4 = Fail
# 5 = suspect spike test
# 6 = fail spike test
# 7 = suspect rate of change test
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
 

### initialize unchecked flag matrix
flags <- matrix(2,dim(input)[1],dim(input)[2]-4)
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
### these values are from the Aquatroll specification manual: https://in-situ.com/us/aqua-troll-600-multiparameter-sonde
###--------- Test 5) Climatological Test
### these values are from World Ocean Database 2018 User's Manual ver 0.10; appendix 11
###--------- Test 6) Spike Test
###--------- Test 7) Rate of Change Test
# temperature
ind <- grep('temperature',names(input),ignore.case=T)
flags[,ind] <- 1
temp <- input[,ind]
### out of range
t1_f2 <- which(temp[,1]>50 | temp[,1]<(-5))
t2_f2 <- which(temp[,2]>50 | temp[,2]<(-5))
if(length(t1_f2)>0){
  flags[t1_f2,ind[1]] <- 4
}
if(length(t2_f2)>0){
  flags[t2_f2,ind[2]] <- 4
}
### climatology only for water temperature
ind_dep <- grep('depth',names(input),ignore.case=T)
z <- input[,ind_dep[1]] ### depth used elsewhere for climatology
t1_f3 <- which(z<100 & temp[,1]>35 | z<100 & temp[,1]<(-2.10))
t1_f4 <- which(z>=100 & temp[,1]>30 | z>=100 & temp[,1]<(-2.10))
if(length(t1_f3)>0){
  flags[t1_f3,ind[1]] <- 3
}
if(length(t1_f4)>0){
  flags[t1_f4,ind[1]] <- 3
}
### spike test; 6 = fail; 5 = suspect
threshold_high <- 5*sd(temp[,1],na.rm=T)
threshold_low <- 3*sd(temp[,1],na.rm=T)
for(i in 2:(length(temp[,1])-1)){
  threshold <- mean(temp[c((i-1),(i+1)),1],na.rm=T)
  if(abs(temp[i,1]-threshold)>threshold_high &
     flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 6
  } else if (abs(temp[i,1]-threshold)>threshold_low &
             abs(temp[i,1]-threshold)<=threshold_high &
             flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 5
  }
}
### rate of change test; 7 = suspect
threshold_low <- 3*sd(temp[,1],na.rm=T)
for(i in 1:(length(temp[,1])-1)){
  if(abs(temp[i,1]-temp[(i+1),1])>threshold_low &
     flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 7
  }
}
# conductivity
ind <- grep('conductivity',names(input),ignore.case=T)
flags[,ind] <- 1
cond <- input[,ind]
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
### out of range
s_f2 <- which(sal>350 | sal<0)
if(length(s_f2)>0){
  flags[s_f2,ind] <- 4
}
### climatology
s_f3 <- which(z<50 & sal>40 | z<50 & sal<0)
s_f4 <- which(z>=50 & sal>40 | z>=50 & sal<20)
if(length(s_f3)>0){
  flags[s_f3,ind] <- 3
}
if(length(s_f4)>0){
  flags[s_f4,ind] <- 3
}
### spike test; 6 = fail; 5 = suspect
threshold_high <- 5*sd(sal,na.rm=T)
threshold_low <- 3*sd(sal,na.rm=T)
for(i in 2:(length(sal)-1)){
  threshold <- mean(sal[c((i-1),(i+1))],na.rm=T)
  if(abs(sal[i]-threshold)>threshold_high &
     flags[i,ind]==1){
    flags[i,ind] <- 6
  } else if (abs(sal[i]-threshold)>threshold_low &
             abs(sal[i]-threshold)<=threshold_high &
             flags[i,ind]==1){
    flags[i,ind] <- 5
  }
}
### rate of change test; 7 = suspect
threshold_low <- 3*sd(sal,na.rm=T)
for(i in 1:(length(sal)-1)){
  if(abs(sal[i]-sal[(i+1)])>threshold_low &
     flags[i,ind]==1){
    flags[i,ind] <- 7
  }
}
# chlorophyll
ind <- grep('chlorophyll',names(input),ignore.case=T)
flags[,ind] <- 1
chl <- input[,ind]
### out of range
c1_f2 <- which(chl[,1]>100 | chl[,1]<0)
c2_f2 <- which(chl[,2]>1000 | chl[,2]<0)
if(length(c1_f2)>0){
  flags[c1_f2,ind[1]] <- 4
}
if(length(c2_f2)>0){
  flags[c2_f2,ind[2]] <- 4
}
### spike test; 6 = fail; 5 = suspect
threshold_high <- 5*sd(chl[,1],na.rm=T)
threshold_low <- 3*sd(chl[,1],na.rm=T)
for(i in 2:(length(chl[,1])-1)){
  threshold <- mean(chl[c((i-1),(i+1)),1],na.rm=T)
  if(abs(chl[i,1]-threshold)>threshold_high &
     flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 6
  } else if (abs(chl[i,1]-threshold)>threshold_low &
             abs(chl[i,1]-threshold)<=threshold_high &
             flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 5
  }
}
threshold_high <- 5*sd(chl[,2],na.rm=T)
threshold_low <- 3*sd(chl[,2],na.rm=T)
for(i in 2:(length(chl[,2])-1)){
  threshold <- mean(chl[c((i-1),(i+1)),2],na.rm=T)
  if(abs(chl[i,2]-threshold)>threshold_high &
     flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 6
  } else if (abs(chl[i,2]-threshold)>threshold_low &
             abs(chl[i,2]-threshold)<=threshold_high &
             flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 5
  }
}
### rate of change test; 7 = suspect
threshold_low <- 3*sd(chl[,1],na.rm=T)
for(i in 1:(length(chl[,1])-1)){
  if(abs(chl[i,1]-chl[(i+1),1])>threshold_low &
     flags[i,ind[1]]==1){
    flags[i,ind[1]] <- 7
  }
}
threshold_low <- 3*sd(chl[,2],na.rm=T)
for(i in 1:(length(chl[,2])-1)){
  if(abs(chl[i,2]-chl[(i+1),2])>threshold_low &
     flags[i,ind[2]]==1){
    flags[i,ind[2]] <- 7
  }
}
# oxygen
ind <- grep('rdo concentration',names(input),ignore.case=T)
flags[,ind] <- 1
rdo <- input[,ind]
### out of range
o_f2 <- which(rdo>50 | rdo<0)
if(length(o_f2)>0){
  flags[o_f2,ind] <- 4
}
### climatology
o_f3 <- which(z<50 & rdo>436*1.025/31.26 | z<50 & rdo<0)
o_f4 <- which(z>=50 & rdo>392*1.025/31.26 | z>=50 & rdo<0)
if(length(o_f3)>0){
  flags[o_f3,ind] <- 3
}
if(length(o_f4)>0){
  flags[o_f4,ind] <- 3
}
### spike test; 6 = fail; 5 = suspect
threshold_high <- 5*sd(rdo,na.rm=T)
threshold_low <- 3*sd(rdo,na.rm=T)
for(i in 2:(length(rdo)-1)){
  threshold <- mean(rdo[c((i-1),(i+1))],na.rm=T)
  if(abs(rdo[i]-threshold)>threshold_high &
     flags[i,ind]==1){
    flags[i,ind] <- 6
  } else if (abs(rdo[i]-threshold)>threshold_low &
             abs(rdo[i]-threshold)<=threshold_high &
             flags[i,ind]==1){
    flags[i,ind] <- 5
  }
}
### rate of change test; 7 = suspect
threshold_low <- 3*sd(rdo,na.rm=T)
for(i in 1:(length(rdo)-1)){
  if(abs(rdo[i]-rdo[(i+1)])>threshold_low &
     flags[i,ind]==1){
    flags[i,ind] <- 7
  }
}
### global missing data test; if NA
na_flag <- which(is.na(input))
if(length(na_flag)>0){
  flags[na_flag] <- 9
}
### column names
flags <- data.frame(flags)
names(flags) <- names(input)[1:(ncol(input)-4)]

###--------- aquatroll quality control
qaqc_aquatroll <- function (input # input file is the output data.frame from data_extract_aquatroll function
)
{
  ### initialize unchecked flag matrix
  flags <- matrix(2,dim(input)[1],dim(input)[2]-4)
  
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
  ### these values are from the Aquatroll specification manual: https://in-situ.com/us/aqua-troll-600-multiparameter-sonde
  ###--------- Test 5) Climatological Test
  ### these values are from World Ocean Database 2018 User's Manual ver 0.10; appendix 11
  ###--------- Test 6) Spike Test
  ###--------- Test 7) Rate of Change Test
  # temperature
  ind <- grep('temperature',names(input),ignore.case=T)
  flags[,ind] <- 1
  temp <- input[,ind]
  ### out of range
  t1_f2 <- which(temp[,1]>50 | temp[,1]<(-5))
  t2_f2 <- which(temp[,2]>50 | temp[,2]<(-5))
  if(length(t1_f2)>0){
    flags[t1_f2,ind[1]] <- 4
  }
  if(length(t2_f2)>0){
    flags[t2_f2,ind[2]] <- 4
  }
  ### climatology only for water temperature
  ind_dep <- grep('depth',names(input),ignore.case=T)
  z <- input[,ind_dep[1]] ### depth used elsewhere for climatology
  t1_f3 <- which(z<100 & temp[,1]>35 | z<100 & temp[,1]<(-2.10))
  t1_f4 <- which(z>=100 & temp[,1]>30 | z>=100 & temp[,1]<(-2.10))
  if(length(t1_f3)>0){
    flags[t1_f3,ind[1]] <- 3
  }
  if(length(t1_f4)>0){
    flags[t1_f4,ind[1]] <- 3
  }
  ### spike test; 6 = fail; 5 = suspect
  threshold_high <- 5*sd(temp[,1],na.rm=T)
  threshold_low <- 3*sd(temp[,1],na.rm=T)
  for(i in 2:(length(temp[,1])-1)){
    threshold <- mean(temp[c((i-1),(i+1)),1],na.rm=T)
    if(abs(temp[i,1]-threshold)>threshold_high &
       flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 6
    } else if (abs(temp[i,1]-threshold)>threshold_low &
               abs(temp[i,1]-threshold)<=threshold_high &
               flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 5
    }
  }
  ### rate of change test; 7 = suspect
  threshold_low <- 3*sd(temp[,1],na.rm=T)
  for(i in 1:(length(temp[,1])-1)){
    if(abs(temp[i,1]-temp[(i+1),1])>threshold_low &
       flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 7
    }
  }
  # conductivity
  ind <- grep('conductivity',names(input),ignore.case=T)
  flags[,ind] <- 1
  cond <- input[,ind]
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
  ### out of range
  s_f2 <- which(sal>350 | sal<0)
  if(length(s_f2)>0){
    flags[s_f2,ind] <- 4
  }
  ### climatology
  s_f3 <- which(z<50 & sal>40 | z<50 & sal<0)
  s_f4 <- which(z>=50 & sal>40 | z>=50 & sal<20)
  if(length(s_f3)>0){
    flags[s_f3,ind] <- 3
  }
  if(length(s_f4)>0){
    flags[s_f4,ind] <- 3
  }
  ### spike test; 6 = fail; 5 = suspect
  threshold_high <- 5*sd(sal,na.rm=T)
  threshold_low <- 3*sd(sal,na.rm=T)
  for(i in 2:(length(sal)-1)){
    threshold <- mean(sal[c((i-1),(i+1))],na.rm=T)
    if(abs(sal[i]-threshold)>threshold_high &
       flags[i,ind]==1){
      flags[i,ind] <- 6
    } else if (abs(sal[i]-threshold)>threshold_low &
               abs(sal[i]-threshold)<=threshold_high &
               flags[i,ind]==1){
      flags[i,ind] <- 5
    }
  }
  ### rate of change test; 7 = suspect
  threshold_low <- 3*sd(sal,na.rm=T)
  for(i in 1:(length(sal)-1)){
    if(abs(sal[i]-sal[(i+1)])>threshold_low &
       flags[i,ind]==1){
      flags[i,ind] <- 7
    }
  }
  # chlorophyll
  ind <- grep('chlorophyll',names(input),ignore.case=T)
  flags[,ind] <- 1
  chl <- input[,ind]
  ### out of range
  c1_f2 <- which(chl[,1]>100 | chl[,1]<0)
  c2_f2 <- which(chl[,2]>1000 | chl[,2]<0)
  if(length(c1_f2)>0){
    flags[c1_f2,ind[1]] <- 4
  }
  if(length(c2_f2)>0){
    flags[c2_f2,ind[2]] <- 4
  }
  ### spike test; 6 = fail; 5 = suspect
  threshold_high <- 5*sd(chl[,1],na.rm=T)
  threshold_low <- 3*sd(chl[,1],na.rm=T)
  for(i in 2:(length(chl[,1])-1)){
    threshold <- mean(chl[c((i-1),(i+1)),1],na.rm=T)
    if(abs(chl[i,1]-threshold)>threshold_high &
       flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 6
    } else if (abs(chl[i,1]-threshold)>threshold_low &
               abs(chl[i,1]-threshold)<=threshold_high &
               flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 5
    }
  }
  threshold_high <- 5*sd(chl[,2],na.rm=T)
  threshold_low <- 3*sd(chl[,2],na.rm=T)
  for(i in 2:(length(chl[,2])-1)){
    threshold <- mean(chl[c((i-1),(i+1)),2],na.rm=T)
    if(abs(chl[i,2]-threshold)>threshold_high &
       flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 6
    } else if (abs(chl[i,2]-threshold)>threshold_low &
               abs(chl[i,2]-threshold)<=threshold_high &
               flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 5
    }
  }
  ### rate of change test; 7 = suspect
  threshold_low <- 3*sd(chl[,1],na.rm=T)
  for(i in 1:(length(chl[,1])-1)){
    if(abs(chl[i,1]-chl[(i+1),1])>threshold_low &
       flags[i,ind[1]]==1){
      flags[i,ind[1]] <- 7
    }
  }
  threshold_low <- 3*sd(chl[,2],na.rm=T)
  for(i in 1:(length(chl[,2])-1)){
    if(abs(chl[i,2]-chl[(i+1),2])>threshold_low &
       flags[i,ind[2]]==1){
      flags[i,ind[2]] <- 7
    }
  }
  # oxygen
  ind <- grep('rdo concentration',names(input),ignore.case=T)
  flags[,ind] <- 1
  rdo <- input[,ind]
  ### out of range
  o_f2 <- which(rdo>50 | rdo<0)
  if(length(o_f2)>0){
    flags[o_f2,ind] <- 4
  }
  ### climatology
  o_f3 <- which(z<50 & rdo>436*1.025/31.26 | z<50 & rdo<0)
  o_f4 <- which(z>=50 & rdo>392*1.025/31.26 | z>=50 & rdo<0)
  if(length(o_f3)>0){
    flags[o_f3,ind] <- 3
  }
  if(length(o_f4)>0){
    flags[o_f4,ind] <- 3
  }
  ### spike test; 6 = fail; 5 = suspect
  threshold_high <- 5*sd(rdo,na.rm=T)
  threshold_low <- 3*sd(rdo,na.rm=T)
  for(i in 2:(length(rdo)-1)){
    threshold <- mean(rdo[c((i-1),(i+1))],na.rm=T)
    if(abs(rdo[i]-threshold)>threshold_high &
       flags[i,ind]==1){
      flags[i,ind] <- 6
    } else if (abs(rdo[i]-threshold)>threshold_low &
               abs(rdo[i]-threshold)<=threshold_high &
               flags[i,ind]==1){
      flags[i,ind] <- 5
    }
  }
  ### rate of change test; 7 = suspect
  threshold_low <- 3*sd(rdo,na.rm=T)
  for(i in 1:(length(rdo)-1)){
    if(abs(rdo[i]-rdo[(i+1)])>threshold_low &
       flags[i,ind]==1){
      flags[i,ind] <- 7
    }
  }
  ### output
  ### global missing data test; if NA
  na_flag <- which(is.na(input))
  if(length(na_flag)>0){
    flags[na_flag] <- 9
  }
  ### column names
  flags <- data.frame(flags)
  names(flags) <- names(input)[1:(ncol(input)-4)]
  return(flags)
}

### test
qaqc_test <- qaqc_aquatroll(input)
hist(as.matrix(qaqc_test),breaks=seq(.5,9.5,1))
