library(DT)
library(leaflet)
library(lubridate)
library(shiny)

files_wd <- '~/Desktop/professional/projects/Postdoc_FL/data/FCWC/processed'
setwd(files_wd)
# data <- read.csv('mertz2021.csv')
# data <- read.csv('all_report_shiny.csv')
# data <- read.csv('all_report_shiny2.csv')
data <- read.csv('all_report_shiny3.csv')
data$Date <- ymd_hms(data$Date)
min.Date <- as.Date(min(data$Date))
data <- data[-which(is.na(data$Longitude)),]
data$Bottom.Dissolved.Oxygen[which(data$Bottom.Dissolved.Oxygen<0)] <- NA
data <- data[-which(is.na(data$Bottom.Dissolved.Oxygen)),]
### there is no data for April or June right now
box_data <- data.frame(date=c(data$Date,
                              seq(as.Date('2020-01-01'),as.Date('2020-12-01'),'months')),
                       surf_t=c(data$Surface.Temperature,rep(NA,12)),
                       bot_t=c(data$Bottom.Temperature,rep(NA,12)),
                       surf_do=c(data$Surface.Dissolved.Oxygen,rep(NA,12)),
                       bot_do=c(data$Bottom.Dissolved.Oxygen,rep(NA,12)))

t_col <- colorRampPalette(c(1,'purple','darkorange','gold'))
t_breaks <- seq(18,38,by=.5)
t_cols <- t_col(length(t_breaks))
ox.col1 <- colorRampPalette(c(1,'firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('dodgerblue4','deepskyblue2','cadetblue1'))
o_breaks <- seq(0,11,by=.5)
o_cols <- c(ox.col1(length(o_breaks[o_breaks<2])),
            ox.col2(length(o_breaks[o_breaks>=2 & o_breaks<3.5])),
            ox.col3(length(o_breaks[o_breaks>=3.5])-1))

ui <- fluidPage(
  titlePanel("FCWC Data Explorer"),
  
  sidebarLayout( 
    sidebarPanel(
      dateRangeInput("daterange1", "Date range:",
                     start  = min.Date,
                     end    = NULL,
                     min    = min.Date,
                     max    = NULL,
                     format = "yyyy-mm-dd",
                     separator = " - "),
      selectInput('parameter', 'Parameter', names(data)[6:9], selected='Bottom.Dissolved.Oxygen'),
      selectInput('serial_num', 'Serial Number', c('all',sort(unique(data$aquatroll_sn))))
    ),
    mainPanel(
      h6('Click on any point on the map and the the data will pop up'),
      leafletOutput("map",height=600),
      hr(),
      h3('Time series plot'),
      h6('Click on any point below and the the data will be displayed below the plot'),
      plotOutput(outputId = "ts_plot", click = "plot_click"),
      verbatimTextOutput("info"),
      hr(),
      h3('Climatology plot'),
      h6('Click on any point below and the the data will be displayed below the plot'),
      plotOutput(outputId = "boxplot", click = "plot_click2"),
      verbatimTextOutput("info2"),
      hr(),
      h3('Data table'),
      tableOutput("table")
    )
  )
)