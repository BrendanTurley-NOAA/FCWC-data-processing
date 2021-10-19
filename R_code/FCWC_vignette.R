### AquaTroll processing vignette

### load librarys for vizualisation
library(akima)
library(fields)
library(rgdal)

### define computer specific path to R project
path2proj <- '~/Documents/R/Github/'
### load necessary aquatroll processing functions
### this assumes you have a clone of the github repository: https://github.com/imaginaryfish/FCWC-data-processing.git
source(paste0(path2proj,'FCWC-data-processing/R_code/aquatroll_processing.R'))

### load shapefile for plotting
setwd(paste0(path2proj,'FCWC-data-processing/shapefile'))
world <- readOGR('ne_10m_admin_0_countries.shp')

### find and names files for input
setwd(paste0(path2proj,'FCWC-data-processing/data/2019-10-15'))
files <- list.files()

### let's open one file and look at it
data <- readLines(files[1])
head(data)
### hmmm, htm files are hard to interpret if you are a human

### ok, the basic file format has several lines of metadata at the top then the data is stored in tabular format below
### the data_extract_aquatroll function strips the metadata off and returns the raw data in a data.frame
data <- data_extract_aquatroll(files[1])
head(data)
### thats more like it

### we can process the raw data into interpolated bins and plot it out to see the results
### we are not going to save the plots for now
data_int <- interp_aquatroll(data,save_plot=F)
head(data_int)
### great, it all looks pretty good

### to see the metadata and some other data summary
data_summary <- summary_aquatroll(files[1])
data_summary
### there will only be one line per htm file


### now if we want to process multiple files we can call all this together in a for loop
### only call the htm files
ind <- grep('htm',files) 
files <- files[ind]

### empty dataframes for storing output
file_sum <- data.frame(matrix(NA,length(files),13))
out <- data.frame(matrix(NA,1000,25))
output <- data.frame(matrix(NA,1000,8))
### counters to keep track of output
j <- 1
k <- 0
m <- 1
n <- 0
### where to save output plots
plot_wd <- paste0(path2proj,'FCWC-data-processing/figures')
### progress bar
pb <- txtProgressBar(min = 0, max = length(files), style = 3)
for(i in 1:length(files)){
  input <- files[i]
  ### process input file
  file_sum[i,] <- summary_aquatroll(input)
  raw_d <- data_extract_aquatroll(input)
  interp_d <- interp_aquatroll(raw_d,set_wd=plot_wd)
  ### add number of rows to counters to save data
  k <- k + nrow(raw_d)
  n <- n + nrow(interp_d)
  ### save data
  out[j:k,] <- raw_d
  output[m:n,] <- interp_d
  ### add one to counters to save data for next iteration
  j <- k + 1
  m <- n + 1
  ### update progress bar
  setTxtProgressBar(pb, i)
}
### renames columns of output dataframes
names(out) <- names(raw_d)
names(output) <- names(interp_d)
### removes empty rows
out <- out[!is.na(out$`Date Time`),]
output <- output[!is.na(output$date_utc),]
### makes dates
output$date_utc <- ymd_hms(output$date_utc)
### makes depth negative going downward; easier for visualization
output$depth_m <- -(output$depth_m)


### define the bottom for plotting
bots <- bottom_finder(output$lon_dd,output$depth_m)
### resolution for interpolation
z_res <- 100
lon_res <- z_res*3
### interpolation
### temperature
temp_int <- interp(output$lon_dd,
                   output$depth_m,
                   output$temperature,
                   xo=seq(min(output$lon_dd),
                          max(output$lon_dd),
                          length=lon_res),
                   yo=seq(min(output$depth_m),
                          max(output$depth_m),
                          length=z_res))
### salinity
sal_int <- interp(output$lon_dd,
                  output$depth_m,
                  output$salinity,
                  xo=seq(min(output$lon_dd),
                         max(output$lon_dd),
                         length=lon_res),
                  yo=seq(min(output$depth_m),
                         max(output$depth_m),
                         length=z_res))
### chlorophyll
chl_int <- interp(output$lon_dd,
                  output$depth_m,
                  output$chlorophyll,
                  xo=seq(min(output$lon_dd),
                         max(output$lon_dd),
                         length=lon_res),
                  yo=seq(min(output$depth_m),
                         max(output$depth_m),
                         length=z_res))
### dissolved oxygen
do_int <- interp(output$lon_dd,
                 output$depth_m,
                 output$rdo,
                 xo=seq(min(output$lon_dd),
                        max(output$lon_dd),
                        length=lon_res),
                 yo=seq(min(output$depth_m),
                        max(output$depth_m),
                        length=z_res))


### breaks and colors
t_col <- colorRampPalette(c(1,'purple','darkorange','gold'))
s_col <- colorRampPalette(c('purple4','dodgerblue4','seagreen3','khaki1'))
c_col <- colorRampPalette(c('honeydew2','darkseagreen3','forestgreen','darkslategrey'))
ox.col1 <- colorRampPalette(c(1,'firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('dodgerblue4','deepskyblue2','cadetblue1'))

t_breaks <- breaks(temp_int$z,.1)
t_cols <- t_col(length(t_breaks)-1)
s_breaks <- breaks(sal_int$z,.5)
s_cols <- s_col(length(s_breaks)-1)
c_breaks <- breaks(round(chl_int$z,2),.2)
c_cols <- c_col(length(c_breaks)-1)
o_breaks <- seq(0,10,by=.25)
o_cols <- c(ox.col1(length(o_breaks[o_breaks<2])),
            ox.col2(length(o_breaks[o_breaks>=2 & o_breaks<3.5])),
            ox.col3(length(o_breaks[o_breaks>=3.5])-1))


### plot it out
setwd(plot_wd)
png(paste('FCWC_',paste(year(output$date_utc[1]),
                        month.abb[month(output$date_utc[1])],
                        day(output$date_utc[1]),sep='-'),
          '.png',sep=''),
    width = 7.5, height = 9.5, units = 'in', res = 300)
par(mfrow=c(3,2),mar=c(5,5,2,2))
### temperature
imagePlot(temp_int$x,
          temp_int$y,
          temp_int$z,
          breaks=t_breaks,
          col=t_cols,
          xlab='',ylab='',las=1)
mtext(expression(paste('Temperature (',degree,'C)')),adj=1)
contour(temp_int$x,
        temp_int$y,
        temp_int$z,
        levels=t_breaks,
        add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='gray80')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext('Depth (m)',2,line=3)

### salinity
imagePlot(sal_int$x,
          sal_int$y,
          sal_int$z,
          breaks=s_breaks,
          col=s_cols,
          xlab='',ylab='',las=1)
mtext('Salinity (ppt)',adj=1)
contour(sal_int$x,
        sal_int$y,
        sal_int$z,
        levels=s_breaks,
        add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='gray80')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext('Depth (m)',2,line=3)

### chlorophyll
imagePlot(chl_int$x,
          chl_int$y,
          chl_int$z,
          breaks=c_breaks,
          col=c_cols,
          xlab='',ylab='',las=1)
mtext(expression(paste('Chlorophyll (',mu,'g l'^-1,')')),adj=1)
contour(chl_int$x,
        chl_int$y,
        chl_int$z,
        levels=c_breaks,
        add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='gray80')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext('Depth (m)',2,line=3)

### dissolved oxygen
imagePlot(do_int$x,
          do_int$y,
          do_int$z,
          breaks=o_breaks,
          col=o_cols,
          xlab='',ylab='',las=1)
mtext(expression(paste('Dissolved Oxygen (mg l'^-1,')')),adj=1)
contour(do_int$x,
        do_int$y,
        do_int$z,
        levels=o_breaks,
        add=T)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='gray80')
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext('Depth (m)',2,line=3)

### plot out locations to see where they are
plot(output$lon_dd,output$lat_dd,
     xlim=c(min(output$lon_dd)-.5,max(output$lon_dd)+.5),
     ylim=c(min(output$lat_dd)-.5,max(output$lat_dd)+.5),
     asp=1,las=1,typ='l',lwd=5,col='dodgerblue',
     xlab='',ylab='',xaxt='n',yaxt='n')
plot(world,add=T,col='gray75')
points(output$lon_dd,output$lat_dd,
       bg='orange',pch=21,cex=1.5)
mtext(expression(paste('Longitude (',degree,'W)')),1,line=3)
mtext(expression(paste('Latitude (',degree,'N)')),2,line=3)
axis(1,seq(floor(min(output$lon_dd)-.5),ceiling(max(output$lon_dd)+.5),.5),las=1)
axis(2,seq(floor(min(output$lat_dd)-.5),ceiling(max(output$lat_dd)+.5),.5),las=1)
mtext(paste(year(output$date_utc[1]),
            month.abb[month(output$date_utc[1])],
            day(output$date_utc[1]),sep='-'),
      3,line=0.2,adj=1)
abline(v=seq(floor(min(output$lon_dd)-.5),ceiling(max(output$lon_dd)+.5),.5),
        h=seq(floor(min(output$lat_dd)-.5),ceiling(max(output$lat_dd)+.5),.5),
       lty=2,col='gray80')
dev.off()

