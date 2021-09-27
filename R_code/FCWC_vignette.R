library(akima)
library(fields)

### vignette
source('~/Documents/R/Github/FCWC-data-processing/R_code/aquatroll_processing.R')

setwd('~/Documents/R/Github/FCWC-data-processing/data/2019-10-15')
files <- list.files()
ind <- grep('htm',files)
files <- files[ind]

file_sum <- data.frame(matrix(NA,length(files),13))
out <- data.frame(matrix(NA,1000,25))
output <- data.frame(matrix(NA,1000,8))
j <- 1
k <- 0
m <- 1
n <- 0
for(i in 1:length(files)){
  input <- files[i]
  ### process input file
  file_sum[i,] <- summary_aquatroll(input)
  raw_d <- data_extract_aquatroll(input)
  interp_d <- interp_aquatroll(raw_d)
  ### add number of rows to counters to save data
  k <- k + nrow(raw_d)
  n <- n + nrow(interp_d)
  ### save data
  out[j:k,] <- raw_d
  output[m:n,] <- interp_d
  ### add one to counters to save data for next iteration
  j <- k + 1
  m <- n + 1
}
names(out) <- names(raw_d)
names(output) <- names(interp_d)
out <- out[!is.na(out$`Date Time`),]
output <- output[!is.na(output$date_utc),]
output$depth_m <- -(output$depth_m)


unique(output$date_utc)

plot(output$lon_dd,output$lat_dd,asp=1)

### finding bottom
bots <- bottom_finder(output$lon_dd,output$depth_m)
### resolution for interpolation
z_res <- 100
lon_res <- z_res*3
### interpolation
temp_int <- interp(output$lon_dd,output$depth_m,output$temperature,
                   xo=seq(min(output$lon_dd),max(output$lon_dd),length=lon_res),
                   yo=seq(min(output$depth_m),max(output$depth_m),length=z_res))
sal_int <- interp(output$lon_dd,output$depth_m,output$salinity,
                  xo=seq(min(output$lon_dd),max(output$lon_dd),length=lon_res),
                  yo=seq(min(output$depth_m),max(output$depth_m),length=z_res))
chl_int <- interp(output$lon_dd,output$depth_m,output$chlorophyll,
                  xo=seq(min(output$lon_dd),max(output$lon_dd),length=lon_res),
                  yo=seq(min(output$depth_m),max(output$depth_m),length=z_res))
do_int <- interp(output$lon_dd,output$depth_m,output$rdo,
                 xo=seq(min(output$lon_dd),max(output$lon_dd),length=lon_res),
                 yo=seq(min(output$depth_m),max(output$depth_m),length=z_res))


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
imagePlot(temp_int$x,
      temp_int$y,
      temp_int$z,
      breaks=t_breaks,
      col=t_cols)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='green')

imagePlot(sal_int$x,
          sal_int$y,
          sal_int$z,
          breaks=s_breaks,
          col=s_cols)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='green')

imagePlot(chl_int$x,
          chl_int$y,
          chl_int$z,
          breaks=c_breaks,
          col=c_cols)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='green')

imagePlot(do_int$x,
          do_int$y,
          do_int$z,
          breaks=o_breaks,
          col=o_cols)
polygon(bots$lon,
        bots$z,
        col='wheat4')
points(output$lon_dd,output$depth_m,pch=20,cex=.5,col='green')


