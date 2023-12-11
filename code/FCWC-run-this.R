# library(akima)
library(fields)
library(interp)
library(lubridate)
library(magick)
library(ncdf4)
library(NISTunits)
library(terra)

# source('~/Documents/R/Github/FCWC-data-processing/code/aquatroll_processing.R')
source('~/R_projects/FCWC-data-processing/code/aquatroll_processing.R')

### loading bathymetry
# setwd("~/Desktop/professional/biblioteca/data")
setwd('~/data/shapefiles')
bathy <- nc_open('etopo1.nc')
topo <- ncvar_get(bathy, 'Band1')
topo_lat <- ncvar_get(bathy, 'lat')
topo_lon <- ncvar_get(bathy, 'lon')
nc_close(bathy)

### loading shape file for coastline
# setwd("~/Desktop/professional/biblioteca/data/shapefiles/ne_10m_admin_0_countries")
# world <- readOGR('ne_10m_admin_0_countries.shp')
# setwd("~/Desktop/professional/biblioteca/data/shapefiles/Florida_Shoreline__1_to_40%2C000_Scale_-shp")
setwd('C:/Users/brendan.turley/Documents/data/shapefiles/shapefiles/Florida_Shoreline__1_to_40_2C000_Scale_-shp')
FL <- vect('Florida_Shoreline__1_to_40_2C000_Scale_.shp')

setwd('~/R_projects/FCWC-data-processing/data/FCWC-data')
files <- list.files()

### find htm files
files_wd <- '~/R_projects/FCWC-data-processing/data/FCWC-data/154-20231206'
setwd(files_wd)
files <- list.files()
ind <- grep('htm',files)
dirs <- 'new'

i <- files[1]
check_headers(i)
look_aquatroll(i)
tmp_sum <- summary_aquatroll(i)
data_extract_aquatroll(i)

### loop through dirs to process htm files
data_extracted <- data.frame(matrix(NA,1000,28))
data_interp <- data.frame(matrix(NA,1000,11))
report <- data.frame(input=rep(NA,1000),
                     date_utc=rep(force_tz(Sys.time(),'UTC'),1000),
                     aquatroll_sn=rep(NA,1000),
                     handheld_sn=rep(NA,1000),
                     n_samp=rep(NA,1000),
                     t_duration_sec=rep(NA,1000),
                     d_duration_sec=rep(NA,1000),
                     z_max_m=rep(NA,1000),
                     do_min=rep(NA,1000),
                     dz_dt=rep(NA,1000),
                     lon_dd=rep(NA,1000),
                     lat_dd=rep(NA,1000),
                     flds_miss=rep(NA,1000),
                     dropout=rep(NA,1000),
                     dirs=rep(NA,1000),
                     profile_index=rep(NA,1000))
### counters to keep track of output
a <- 1
k <- 1
l <- 0
m <- 1
n <- 0
p_ind <- 1
### where to save output plots
plot_wd <- paste0('~/R_projects/FCWC-data-processing/figures/processed')
for(j in ind){
  # for(j in 26:30){
  setwd(files_wd)
  input <- files[j]
  ### process input file
  file_sum <- summary_aquatroll(input)
  raw_d <- data_extract_aquatroll(input)
  interp_d <- interp_aquatroll(raw_d, 
                               set_wd = plot_wd, save_plot = T, 
                               downcast = T, sal_calc = T, 
                               shallow = T, resolution = 1,
                               z_min = 1)
  ### add number of rows to counters to save data
  l <- l + nrow(raw_d)
  ### save data
  report[a,] <- cbind(file_sum,dirs,p_ind)
  data_extracted[k:l,] <- cbind(raw_d,dirs,p_ind)
  ### add one to counters to save data for next iteration
  a <- a + 1
  k <- l + 1
  ### interpolated data is different
  if(length(interp_d)>1){
    n <- n + nrow(interp_d)
    data_interp[m:n,] <- cbind(interp_d,dirs,p_ind,unique(raw_d$aquatroll_sn))
    m <- n + 1
  }
  ### profile index +1
  p_ind <- p_ind + 1
}
report <- report[!is.na(report$input),]
report <- report[,c(16,15,1:14)]
plot(report$lon_dd,report$lat_dd,asp=1)
names(data_extracted) <- c("Date Time",
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
                           "Marked",
                           'serial number',
                           'depth unit',
                           'input',
                           'folder',
                           'profile index')
data_extracted <- data_extracted[,c(28:26,24,1:23,25)]
data_extracted <- data_extracted[!is.na(data_extracted$folder),]
names(data_interp) <- c('dtime_utc',
                        'lon_dd',
                        'lat_dd',
                        'depth_m',
                        'temp_c',
                        'sal_psu',
                        # 'chl_ugl',
                        'chl_rfu',
                        'do_mgl',
                        'folder',
                        'profile',
                        'serial')
data_interp <- data_interp[,c(10:9,11,1:8)]
data_interp <- data_interp[!is.na(data_interp$profile),]

col_1 <- colorRampPalette(c('green4','gray70','mediumpurple4'))
for(i in ind){
  setwd(files_wd)
  input <- files[i]
  tmp_sum <- summary_aquatroll(input)
  ymd_hms(tmp_sum$time_utc)
  tmp_ex <- data_extract_aquatroll(input)
  ### plot raw
  setwd(plot_wd)
  png(paste(paste0(year(tmp_sum$time_utc[1]),
                   sprintf('%02d',month(tmp_sum$time_utc[1])),
                   sprintf('%02d',day(tmp_sum$time_utc[1])),
                   sprintf('%02d',hour(tmp_sum$time_utc[1])),
                   sprintf('%02d',minute(tmp_sum$time_utc[1]))),
            '_diagnostic_FCWC.png',sep=''),
      width = 8, height = 11, units = 'in', res = 300)
  par(mfrow=c(3,2))
  plot(tmp_ex$`Temperature (°C) AT`,-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,ylab='Depth (ft)')
  mtext(tmp_sum$time_utc)
  plot(tmp_ex$`Salinity (PSU)`,-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,ylab='Depth (ft)')
  plot(tmp_ex$`Chlorophyll-a Fluorescence (RFU)`,-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,ylab='Depth (ft)')
  plot(tmp_ex$`RDO Concentration (mg/L)`,-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,ylab='Depth (ft)')
  plot(tmp_ex$`RDO Saturation (%Sat)`,-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,ylab='Depth (ft)')
  plot(ymd_hms(tmp_ex$`Date Time`)-ymd_hms(tmp_ex$`Date Time`)[1],-tmp_ex$Depth,
       typ='o',col=col_1(nrow(tmp_ex)),pch=16,xlab='Duration (seconds)',ylab='Depth (ft)')
  dev.off()
}

plot(report$lon_dd,report$lat_dd,asp=1,
     xlim=c(min(report$lon_dd)-.2,max(report$lon_dd)+.2),
     ylim=c(min(report$lat_dd)-.2,max(report$lat_dd)+.2))
plot(FL,add=T)

dists <- (sqrt(abs(diff(report$lat_dd))^2+abs(diff(report$lon_dd))^2)*111)/1.609

library(sp)
dists <- spDists(cbind(report$lon_dd,report$lat_dd),longlat = T)
NISTkmTOmile(dists)


# ### comparison plots
# 
# compare_plot <- function(data_extracted, parameter, profiles = c(1,2)){
#   x_1 <- data_extracted[which(data_extracted$`profile index`==profiles[1]),parameter]
#   depth_1 <- -data_extracted[which(data_extracted$`profile index`==profiles[1]),'Depth']
#   sn_1 <- data_extracted[which(data_extracted$`profile index`==profiles[1]),'serial number'][1]
#   time_1 <- data_extracted[which(data_extracted$`profile index`==profiles[1]),'Date Time'][length(which(data_extracted$`profile index`==profiles[1]))]
#   col_1 <- colorRampPalette(c('mediumpurple1','mediumpurple4'))
#   
#   x_2 <- data_extracted[which(data_extracted$`profile index`==profiles[2]),parameter]
#   depth_2 <- -data_extracted[which(data_extracted$`profile index`==profiles[2]),'Depth']
#   sn_2 <- data_extracted[which(data_extracted$`profile index`==profiles[2]),'serial number'][1]
#   time_2 <- data_extracted[which(data_extracted$`profile index`==profiles[2]),'Date Time'][1]
#   col_2 <- colorRampPalette(c('green1','green4'))
#   
#   xlims <- c(min(c(x_1,x_2)),max(c(x_1,x_2)))
#   ylims <- c(min(c(depth_1,depth_2)),max(c(depth_1,depth_2)))
#   tdiff <- round(abs(ymd_hms(time_2)-ymd_hms(time_1)),2)
#   
#   plot(x_1,depth_1,col=col_1(length(x_1)),typ='o',pch=16,xlim=xlims,ylim=ylims,xlab=parameter,ylab='Depth')
#   points(x_2,depth_2,col=col_2(length(x_2)),typ='o',pch=16)
#   legend('topright',inset=c(0,-.1),legend=c(sn_1,sn_2),col=c('mediumpurple1','green1'),pch=16,horiz=T,bty='n',xpd=T)
#   mtext(paste(tdiff,units(tdiff)),adj=0)
# }
# 
# 
# png('compare_1-2.png',width=8,height=11,units='in',res=300)
# par(mfrow=c(2,2))
# compare_plot(data_extracted,'Temperature (°C) AT',c(1,2))
# compare_plot(data_extracted,'Salinity (PSU)',c(1,2))
# compare_plot(data_extracted,'Chlorophyll-a Fluorescence (RFU)',c(1,2))
# compare_plot(data_extracted,'RDO Concentration (mg/L)',c(1,2))
# dev.off()
# 
# png('compare_3-4.png',width=8,height=11,units='in',res=300)
# par(mfrow=c(2,2))
# compare_plot(data_extracted,'Temperature (°C) AT',c(3,4))
# compare_plot(data_extracted,'Salinity (PSU)',c(3,4))
# compare_plot(data_extracted,'Chlorophyll-a Fluorescence (RFU)',c(3,4))
# compare_plot(data_extracted,'RDO Concentration (mg/L)',c(3,4))
# dev.off()
# 
# png('compare_5-6.png',width=8,height=11,units='in',res=300)
# par(mfrow=c(2,2))
# compare_plot(data_extracted,'Temperature (°C) AT',c(5,6))
# compare_plot(data_extracted,'Salinity (PSU)',c(5,6))
# compare_plot(data_extracted,'Chlorophyll-a Fluorescence (RFU)',c(5,6))
# compare_plot(data_extracted,'RDO Concentration (mg/L)',c(5,6))
# dev.off()
# 
# png('compare_7-8.png',width=8,height=11,units='in',res=300)
# par(mfrow=c(2,2))
# compare_plot(data_extracted,'Temperature (°C) AT',c(7,8))
# compare_plot(data_extracted,'Salinity (PSU)',c(7,8))
# compare_plot(data_extracted,'Chlorophyll-a Fluorescence (RFU)',c(7,8))
# compare_plot(data_extracted,'RDO Concentration (mg/L)',c(7,8))
# dev.off()
# 
# png('compare_9-10.png',width=8,height=11,units='in',res=300)
# par(mfrow=c(2,2))
# compare_plot(data_extracted,'Temperature (°C) AT',c(9,10))
# compare_plot(data_extracted,'Salinity (PSU)',c(9,10))
# compare_plot(data_extracted,'Chlorophyll-a Fluorescence (RFU)',c(9,10))
# compare_plot(data_extracted,'RDO Concentration (mg/L)',c(9,10))
# dev.off()
# 
# 
# plot(diff(data_extracted[which(data_extracted$`profile index`==3),'Depth']),typ='l')
# abline(h=0,lty=2)
# plot(data_extracted[which(data_extracted$`profile index`==3),'Depth'][1:(nrow(data_extracted[which(data_extracted$`profile index`==3),])-1)],
#      diff(data_extracted[which(data_extracted$`profile index`==3),'Depth']),typ='l')


### -------------> interpolation 2021/10/16
sn <- unique(data_interp$serial)
output <- data_interp[which(data_interp$serial==sn),]
output$temp_f <- NISTdegCtOdegF(output$temp_c)
output$depth_m <- NISTmeterTOft(output$depth_m)

# dists <- (sqrt(abs(diff(output$lat_dd))^2+abs(diff(output$lon_dd))^2)*111)/1.609
dists <- cumsum(c(0,(sqrt(abs(diff(unique(output$lon_dd)))^2 + abs(diff(unique(output$lat_dd)))^2)*111)/1.609))
output$dists <- dists[as.numeric(as.factor(output$profile))]


### select xy dimension has the greatest extent
out <- which_xy(report,sn)
xlab <- c(expression(paste('Longitude (',degree,'W)')),
          expression(paste('Latitude (',degree,'N)')))

### makes dates
output$dtime_utc <- ymd_hms(output$dtime_utc)
### makes depth negative going downward; easier for visualization
output$depth_m <- depth_neg(output$depth_m)
### make location index for plotting
locs <- cbind(unique(output$lon_dd),
              unique(output$lat_dd),
              unique(output$profile))
locs_ind <- which(c('lon_dd','lat_dd')==out)

### define the bottom for plotting
bots <- bottom_finder(output[,out],output$depth_m)
### resolution for interpolation
z_res <- 100
x_res <- z_res*3
### interpolation
### temperature
# temp_int <- interp(output[,out],
#                    output$depth_m,
#                    output$temp_f,
#                    xo=seq(min(output[,out]),
#                           max(output[,out]),
#                           length=x_res),
#                    yo=seq(min(output$depth_m),
#                           max(output$depth_m),
#                           length=z_res))
### try in interp package, akima dependencies being deprecated

temp_int <- interp::interp(output[,out],
                           output$depth_m,
                           output$temp_f,
                           xo=seq(min(output[,out]),
                                  max(output[,out]),
                                  length=x_res),
                           yo=seq(min(output$depth_m),
                                  max(output$depth_m),
                                  length=z_res))
### salinity
sal_int <- interp::interp(output[,out],
                          output$depth_m,
                          output$sal_psu,
                          xo=seq(min(output[,out]),
                                 max(output[,out]),
                                 length=x_res),
                          yo=seq(min(output$depth_m),
                                 max(output$depth_m),
                                 length=z_res))
### chlorophyll
chl_int <- interp::interp(output[,out],
                          output$depth_m,
                          output$chl_rfu,
                          xo=seq(min(output[,out]),
                                 max(output[,out]),
                                 length=x_res),
                          yo=seq(min(output$depth_m),
                                 max(output$depth_m),
                                 length=z_res))
chl_int$z <- check_neg(chl_int$z)
### dissolved oxygen
do_int <- interp::interp(output[,out],
                         output$depth_m,
                         output$do_mgl,
                         xo=seq(min(output[,out]),
                                max(output[,out]),
                                length=x_res),
                         yo=seq(min(output$depth_m),
                                max(output$depth_m),
                                length=z_res))


### breaks and colors
t_col <- colorRampPalette(c('gray20','purple','darkorange','gold'))
s_col <- colorRampPalette(c('purple4','dodgerblue4','seagreen3','khaki1'))
c_col <- colorRampPalette(c('honeydew2','darkseagreen3','forestgreen','darkslategrey'))
ox.col1 <- colorRampPalette(c('gray20','firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('gray20','dodgerblue4','deepskyblue2','cadetblue1','azure'))

# t_breaks <- breaks(temp_int$z,.2)
t_breaks <- pretty(temp_int$z,15)
t_cols <- t_col(length(t_breaks)-1)
# s_breaks <- breaks(sal_int$z,.2)
s_breaks <- pretty(sal_int$z,15)
s_cols <- s_col(length(s_breaks)-1)
# c_breaks <- breaks(round(chl_int$z,3),.01,decimal = T, round_digits = 3)
c_breaks <- pretty(chl_int$z,15)
c_cols <- c_col(length(c_breaks)-1)
if(min(do_int$z,na.rm=T)<4){
  o_breaks <- seq(0,10,by=.5)
  o_cols <- c(ox.col1(length(o_breaks[o_breaks<2])),
              ox.col2(length(o_breaks[o_breaks>=2 & o_breaks<3.5])),
              ox.col3(length(o_breaks[o_breaks>=3.5])-1))
} else {
  o_breaks <- pretty(do_int$z,15)
  o_cols <- ox.col3(length(o_breaks)-1)
}


### plot it out
setwd(plot_wd)
png(paste(paste0(year(output$dtime_utc[1]),
                 sprintf('%02d',month(output$dtime_utc[1])),
                 sprintf('%02d',day(output$dtime_utc[1]))),
          '_FCWC.png',sep=''),
    width = 8, height = 11, units = 'in', res = 300)
# par(mfrow=c(3,2),mar=c(5,5,3,2),oma=c(1,1,4,1))
par(mfrow=c(3,2),mar=c(5,5,3,2),oma=c(5,1,4,1))
### temperature
imagePlot(temp_int$x,
          temp_int$y,
          temp_int$z,
          breaks=t_breaks,
          col=t_cols,
          ylim=c(min(temp_int$y),0),
          xlab='',ylab='',las=1)
text(locs[,locs_ind],rep(.3,nrow(locs)),locs[,3],xpd=T,col='gray50')
mtext(expression(paste('Temperature (',degree,'F)')),adj=1,line=1)
# mtext(expression(paste('Temperature (',degree,'C)')),adj=1,line=1)
# contour(temp_int$x,
#         temp_int$y,
#         temp_int$z,
#         levels=t_breaks,
#         add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output[,out],output$depth_m,pch=20,cex=.5,col='gray80')
mtext(xlab[locs_ind],1,line=3)
mtext('Depth (ft)',2,line=3)

### salinity
imagePlot(sal_int$x,
          sal_int$y,
          sal_int$z,
          breaks=s_breaks,
          col=s_cols,
          ylim=c(min(sal_int$y),0),
          xlab='',ylab='',las=1)
text(locs[,locs_ind],rep(.3,nrow(locs)),locs[,3],xpd=T,col='gray50')
mtext('Salinity (PSU)',adj=1,line=1)
# contour(sal_int$x,
#         sal_int$y,
#         sal_int$z,
#         levels=s_breaks,
#         add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output[,out],output$depth_m,pch=20,cex=.5,col='gray80')
mtext(xlab[locs_ind],1,line=3)
mtext('Depth (ft)',2,line=3)

### chlorophyll
imagePlot(chl_int$x,
          chl_int$y,
          chl_int$z,
          breaks=c_breaks,
          col=c_cols,
          ylim=c(min(chl_int$y),0),
          xlab='',ylab='',las=1)
text(locs[,locs_ind],rep(.3,nrow(locs)),locs[,3],xpd=T,col='gray50')
# mtext(expression(paste('Chlorophyll (',mu,'g l'^-1,')')),adj=1)
mtext('Chlorophyll Fluorescence (RFU)',adj=1,line=1)
# contour(chl_int$x,
#         chl_int$y,
#         chl_int$z,
#         levels=c_breaks,
#         add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output[,out],output$depth_m,pch=20,cex=.5,col='gray80')
mtext(xlab[locs_ind],1,line=3)
mtext('Depth (ft)',2,line=3)

### dissolved oxygen
imagePlot(do_int$x,
          do_int$y,
          do_int$z,
          breaks=o_breaks,
          col=o_cols,
          ylim=c(min(do_int$y),0),
          xlab='',ylab='',las=1)
text(locs[,locs_ind],rep(.3,nrow(locs)),locs[,3],xpd=T,col='gray50')
mtext(expression(paste('Dissolved Oxygen (mg l'^-1,')')),adj=1,line=1)
# contour(do_int$x,
#         do_int$y,
#         do_int$z,
#         levels=o_breaks,
#         add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output[,out],output$depth_m,pch=20,cex=.5,col='gray80')
mtext(xlab[locs_ind],1,line=3)
mtext('Depth (ft)',2,line=3)

### plot out locations to see where they are
zoom <- .2
plot(output$lon_dd[order(output$lon_dd)],
     output$lat_dd[order(output$lon_dd)],
     xlim=c(min(output$lon_dd)-zoom,max(output$lon_dd)+zoom),
     ylim=c(min(output$lat_dd)-zoom,max(output$lat_dd)+zoom),
     asp=1,las=1,typ='l',lwd=5,col='dodgerblue',
     xlab='',ylab='',xaxt='n',yaxt='n')
plot(FL, add=T, col='gray75')
points(output$lon_dd[order(output$lon_dd)],
       output$lat_dd[order(output$lon_dd)],
       bg='orange',pch=21,cex=1.5)
text(locs[,1],locs[,2],locs[,3],
     pos=2,xpd=T,col='gray50')
mtext(xlab[1],1,line=3)
mtext(xlab[2],2,line=3)
axis(1,seq(floor(min(output$lon_dd)-.1),ceiling(max(output$lon_dd)+.1),.1),las=1)
axis(2,seq(floor(min(output$lat_dd)-.1),ceiling(max(output$lat_dd)+.1),.1),las=1)
mtext(paste(year(output$dtime_utc[1]),
            month.abb[month(output$dtime_utc[1])],
            day(output$dtime_utc[1]),sep='-'),
      3,line=0.2,adj=1)
abline(v=seq(floor(min(output$lon_dd)-.1),ceiling(max(output$lon_dd)+.1),.1),
       h=seq(floor(min(output$lat_dd)-.1),ceiling(max(output$lat_dd)+.1),.1),
       lty=2,col='gray80')
### title
mtext('FCWC Water Quality Bulletin',
      outer=T,line=1,side=3,font=2,at=.05,adj=0,cex=1.25)
mtext(paste('Collected:',
            paste(day(output$dtime_utc[1]),
                  month.abb[month(output$dtime_utc[1])],
                  year(output$dtime_utc[1]),sep='-')),
      outer=T,line=-.1,side=3,at=.05,adj=0)
mtext(paste('Note: Data are early release and subject to further QA/QC, \nplease contact brendan.turley@noaa.gov for comments/concerns \nProcessed: ',as.Date(Sys.time())),
      outer=T,line=2,side=1,col='red',font=2,at=.01,adj=0,cex=.75)
dev.off()


files <- list.files(plot_wd)
ind <- grep('20230807',files)
ind2 <- grep('diagnostic',files)
image_write(image_read(files[c(setdiff(ind,ind2),ind2)]), format = "pdf", '20230807_154.pdf')
