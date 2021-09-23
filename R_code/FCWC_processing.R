library(akima)
library(gsw)
library(lubridate)
library(plotrix)
library(rgdal)

### read map data
setwd("~/Desktop/FCWC/shapefile")
world <- readOGR('ne_10m_admin_0_countries.shp')

### look for cruises
setwd("~/Desktop/FCWC/data")
cruises <- list.files()

cr <- 1 ### which cruise, there is only one in this example
setwd(paste("~/Desktop/FCWC/data/",cruises[cr],sep=''))
files <- list.files()
ind <- grep('csv',files)

### create empty data.frame to fill with data
new <- data.frame(depth=rep(NA,500),
                  temperature=rep(NA,500),
                  salinity=rep(NA,500),
                  chlorophyll=rep(NA,500),
                  oxygen=rep(NA,500),
                  latitude=rep(NA,500),
                  longitude=rep(NA,500))
bottom <- rep(NA,length(ind)-1)
lon <- rep(NA,length(ind)-1)
depth <- rep(NA,length(ind)-1)
lon2 <- rep(NA,length(ind)-1)
datetime <- rep(Sys.time(),length(ind)-1)
### these are for record keeping
m <- 1
n <- 0
o <- 1
p <- 0
for(i in ind){
  x <- readLines(files[i])
  sn.ind <- grep('Device Model = Aqua TROLL 600 Vented',x)
  serial_n <- substr(x[sn.ind+1],13,18)
  ### turn data into .csv
  start <- grep('Date Time',x)
  data <- read.csv(files[i],skip=(start-1))
  ### location data
  latitude <- mean(data$Latitude....,na.rm=T)
  longitude <- mean(data$Longitude....,na.rm=T)
  ### find column with sonde temperature
  at.ind <- grep(serial_n,names(data))
  aqua_troll <- names(data)[at.ind]
  t.ind <- grep('Temp',aqua_troll)
  temp.ind <- at.ind[t.ind]
  temp <- data[,temp.ind]
  ### find column with sonde depth
  depth.i <- grep('Depth',names(data))
  z <- -(data[,depth.i]*0.3048) ### convert to meters
  ### find column with sonde oxygen concentration
  oxy <- grep('RDO.Con',names(data))
  do <- data[,oxy]
  ### find column with sonde salinity concentration
  sal <- grep('Sal',names(data))
  ### If there is no salinity, calculate it with conductivity
  if(length(sal)<1){
    ac.ind <- grep('Actual.Conductivity',names(data))
    pressure.ind <- grep('Pressure..psi',names(data))
    p <- gsw_p_from_z(z,latitude)
    sp <- gsw_SP_from_C(data[,ac.ind]/1000,temp,p)
    psal <-  gsw_SA_from_SP(sp,p,longitude,latitude)
    cat(' No salinity data!','\n',files[i])
  } else {
    psal <- data[,sal]
  }
  ### find column with sonde chlorophyll fluorescence
  chl <- grep('Chlorophyll.a.F',names(data))
  chla <- data[,chl]
  ### time stamp
  time <- ymd_hms(data[,1],tz='US/Eastern')
  ### only the descent
  start.dec <- which(diff(data[,depth.i])>1)
  stop.dec <- which(diff(data[,depth.i])<(-1))
  
  ### binning the data and taking the median is better!!!
  breaks <- seq(floor(min(z[start.dec[1]:stop.dec[1]]))-.5,ceiling(max(z[start.dec[1]:stop.dec[1]]))+.5,1)
  bins <- .bincode(z[start.dec[1]:stop.dec[1]],temp[start.dec[1]:stop.dec[1]],breaks=breaks)
  temp.b <- aggregate(temp[start.dec[1]:stop.dec[1]],by=list(bins),median,na.rm=T)
  sal.b <- aggregate(psal[start.dec[1]:stop.dec[1]],by=list(bins),median,na.rm=T)
  chla.b <- aggregate(chla[start.dec[1]:stop.dec[1]],by=list(bins),median,na.rm=T)
  do.b <- aggregate(do[start.dec[1]:stop.dec[1]],by=list(bins),median,na.rm=T)
  ### store these for plotting on map
  p <- p + length(z[start.dec[1]:stop.dec[1]])
  depth[o:p] <- z[start.dec[1]:stop.dec[1]]
  lon2[o:p] <- rep(longitude,length(o:p))
  o <- p + 1
  datetime[i-ind[1]+1] <- time[1]
  ### store these for plotting bottom profile
  bottom[i-ind[1]+1] <- floor(min(z[start.dec[1]:stop.dec[1]]))-.5
  lon[i-ind[1]+1] <- longitude
  ### store these for interpolation to create profile images
  n <- n + nrow(temp.b)
  new$depth[m:n] <- breaks[temp.b$Group.1]+.5
  new$temperature[m:n] <- temp.b$x
  new$salinity[m:n] <- sal.b$x
  new$chlorophyll[m:n] <- chla.b$x
  new$oxygen[m:n] <- do.b$x
  new$latitude[m:n] <- rep(latitude,length(m:n))
  new$longitude[m:n] <- rep(longitude,length(m:n))
  m <- n + 1
}
### get rid of unused rows
new <- new[1:n,]
depth <- depth[1:p]
lon2 <- lon2[1:p]
### interpolate data for visualizing profiles
resolution <- 100
temp.in <- interp(new$depth,new$longitude,new$temperature,
                  xo=seq(min(new$depth,na.rm=T), max(new$depth,na.rm=T), length = resolution),
                  yo=seq(min(new$longitude,na.rm=T), max(new$longitude,na.rm=T), length = resolution*3))
sal.in <- interp(new$depth,new$longitude,new$salinity,
                 xo=seq(min(new$depth,na.rm=T), max(new$depth,na.rm=T), length = resolution),
                 yo=seq(min(new$longitude,na.rm=T), max(new$longitude,na.rm=T), length = resolution*3))
chl.in <- interp(new$depth,new$longitude,new$chlorophyll,
                 xo=seq(min(new$depth,na.rm=T), max(new$depth,na.rm=T), length = resolution),
                 yo=seq(min(new$longitude,na.rm=T), max(new$longitude,na.rm=T), length = resolution*3))
do.in <- interp(new$depth,new$longitude,new$oxygen,
                xo=seq(min(new$depth,na.rm=T), max(new$depth,na.rm=T), length = resolution),
                yo=seq(min(new$longitude,na.rm=T), max(new$longitude,na.rm=T), length = resolution*3))

### color palette for plotting
temp.col <- colorRampPalette(c(1,'purple','orange','gold'))
sal.col <- colorRampPalette(c('purple4','dodgerblue4','seagreen3','khaki1'))
chl.col <- colorRampPalette(c('honeydew2','darkseagreen3','forestgreen','darkslategrey'))
ox.col1 <- colorRampPalette(c(1,2))
ox.col2 <- colorRampPalette(c('gray20','gray40','gray90'))
ox.col3 <- colorRampPalette(c('yellow','gold'))

### save figure
setwd("~/Desktop/FCWC/figures")
png(paste("test_",cr,".png",sep=''), width = 7.5, height = 9.5, units = 'in', res = 300)

par(mfrow=c(3,2),mar=c(5,5,2,5))
image(temp.in$y,temp.in$x,t(temp.in$z),breaks=seq(min(temp.in$z,na.rm = T),max(temp.in$z,na.rm = T),length.out = 21),col=temp.col(20),las=1,xlab='',ylab='')
mtext(expression(paste('Temperature (',degree,'C)')),3,line=0.2,adj=1)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=2.4)
mtext('Depth (m)',2,line=2.4)
points(lon2,depth,pch=20,cex=.25)
polygon(c(min(lon)-1,sort(lon),max(lon)+1,max(lon)+1),c(min(bottom)-1,sort(bottom),max(bottom),min(bottom)-1),col='wheat4')
color.legend(max(new$longitude)+(mean(abs(diff(lon)))/10),min(new$depth),max(new$longitude+(mean(abs(diff(lon)))/10)*3),max(new$depth),seq(min(signif(temp.in$z,digits=2),na.rm = T),max(signif(temp.in$z,digits=2),na.rm = T),.5),temp.col(20),gradient ='y',align='rb',cex=.5)

image(sal.in$y,sal.in$x,t(sal.in$z),breaks=seq(min(sal.in$z,na.rm = T),max(sal.in$z,na.rm = T),length.out = 21),col=sal.col(20),las=1,xlab='',ylab='')
mtext('Salinity (PSU)',3,line=0.2,adj=1)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=2.4)
mtext('Depth (m)',2,line=2.4)
points(lon2,depth,pch=20,cex=.25)
polygon(c(min(lon)-1,sort(lon),max(lon)+1,max(lon)+1),c(min(bottom)-1,sort(bottom),max(bottom),min(bottom)-1),col='wheat4')
color.legend(max(new$longitude)+(mean(abs(diff(lon)))/10),min(new$depth),max(new$longitude+(mean(abs(diff(lon)))/10)*3),max(new$depth),seq(min(signif(sal.in$z,digits=2),na.rm = T),max(signif(sal.in$z,digits=2),na.rm = T),2),sal.col(20),gradient ='y',align='rb',cex=.5)

image(chl.in$y,chl.in$x,t(chl.in$z),breaks=seq(min(chl.in$z,na.rm = T),max(chl.in$z,na.rm = T),length.out = 21),col=chl.col(20),las=1,xlab='',ylab='')
mtext('Chlorophyll-a (RFU)',3,line=0.2,adj=1)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=2.4)
mtext('Depth (m)',2,line=2.4)
points(lon2,depth,pch=20,cex=.25)
polygon(c(min(lon)-1,sort(lon),max(lon)+1,max(lon)+1),c(min(bottom)-1,sort(bottom),max(bottom),min(bottom)-1),col='wheat4')
color.legend(max(new$longitude)+(mean(abs(diff(lon)))/10),min(new$depth),max(new$longitude+(mean(abs(diff(lon)))/10)*3),max(new$depth),seq(min(signif(chl.in$z,digits=1),na.rm = T),max(signif(chl.in$z,digits=1),na.rm = T),.1),chl.col(20),gradient ='y',align='rb',cex=.5)

image(do.in$y,do.in$x,t(do.in$z),breaks=seq(min(do.in$z,na.rm = T),max(do.in$z,na.rm = T),length.out = 21),col=ox.col2(20),las=1,xlab='',ylab='')
mtext('Dissolved Oxygen (mg/l)',3,line=0.2,adj=1)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=2.4)
mtext('Depth (m)',2,line=2.4)
points(lon2,depth,pch=20,cex=.25)
polygon(c(min(lon)-1,sort(lon),max(lon)+1,max(lon)+1),c(min(bottom)-1,sort(bottom),max(bottom),min(bottom)-1),col='wheat4')
color.legend(max(new$longitude)+(mean(abs(diff(lon)))/10),min(new$depth),max(new$longitude+(mean(abs(diff(lon)))/10)*3),max(new$depth),seq(min(signif(do.in$z,digits=1),na.rm = T),max(signif(do.in$z,digits=1),na.rm = T),.2),ox.col2(20),gradient ='y',align='rb',cex=.5)

plot(world,xlim=c(min(new$longitude)-.2,max(new$longitude)+.2),ylim=c(min(new$latitude)-.2,max(new$latitude)+.2),col='gray90',xlab='')
points(unique(new$longitude),unique(new$latitude),pch=21,bg='orange')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=2.4)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=2.6)
mtext(paste(year(datetime[1]),month.abb[month(datetime[1])],day(datetime[1]),sep='-'),3,line=0.2,adj=1)
axis(1,seq(floor(min(new$longitude)-.25),ceiling(max(new$longitude)+.25),.1))
axis(2,seq(floor(min(new$latitude)-.25),ceiling(max(new$latitude)+.25),.1),las=1)
box()
grid()
dev.off()


