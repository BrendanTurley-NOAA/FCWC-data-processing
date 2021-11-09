### https://mastering-shiny.org/index.html
### https://rstudio.github.io/leaflet/shiny.html
### https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent

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
t_breaks <- seq(25,40,by=1)
ox.col1 <- colorRampPalette(c(1,'firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('dodgerblue4','deepskyblue2','cadetblue1'))
o_breaks <- seq(0,11,by=.5)
o_cols <- c(ox.col1(length(o_breaks[o_breaks<2])),
            ox.col2(length(o_breaks[o_breaks>=2 & o_breaks<3.5])),
            ox.col3(length(o_breaks[o_breaks>=3.5])-1))

ui <- fluidPage(
  sidebarLayout( 
    sidebarPanel(
      dateRangeInput("daterange1", "Date range:",
                     start  = "2018-01-01",
                     end    = NULL,
                     min    = "2018-01-01",
                     max    = "2021-12-31",
                     format = "yyyy-mm-dd",
                     separator = " - "),
      selectInput('parameter', 'Parameter', names(data)[6:9], selected='Bottom.Dissolved.Oxygen'),
      selectInput('serial_num', 'Serial Number', c('all',sort(unique(data$aquatroll_sn))))
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput(outputId = "ts_plot"),
      plotOutput(outputId = "boxplot"),
      tableOutput("table")
    )
  )
)



server <- function(input, output, session) {
  
  out <- reactive({
    if(input$serial_num=='all'){
      data[which(data$Date>=ymd(input$daterange1[1]) &
                   data$Date<=ymd(input$daterange1[2])),
           # which(names(data)==input$parameter)
      ]
    } else {
      data[which(data$Date>=ymd(input$daterange1[1]) &
                   data$Date<=ymd(input$daterange1[2]) & 
                   data$aquatroll_sn==input$serial_num),
           # which(names(data)==input$parameter)
      ]
    }
  })
  
  output$ts_plot <- renderPlot({
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      
      plot(out()$Date,out()$Bottom.Dissolved.Oxygen,
           xlab='Date',ylab='Bottom Dissolved Oxygen (mg/l)',
           las=2,bg=o_cols[o_i],pch=21,cex=1.5)
      # abline(h=c(3.5,2),col=c('gold4','red'),lty=2,lend=2)
    }
  })
  
  output$boxplot <- renderPlot({
    
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      
      boxplot(box_data$bot_do~month(box_data$date),na.action = na.pass,
              xlab='Month',ylab='Bottom Dissolved Oxygen (mg/l)',
              staplewex=0,outwex=0,outline=F,lty=1,lwd=1.5,names=month.abb[1:12],las=2)
      points(jitter(month(out()$Date),3,.3),out()$Bottom.Dissolved.Oxygen,
             bg=o_cols[o_i],pch=21,cex=1.5)
    }
  })
  
  output$map <- renderLeaflet({
    
    # basemap <- providers$Esri.NatGeoWorldMap
    basemap <- providers$Esri.OceanBasemap
    # basemap <- providers$Esri.WorldImagery
    # basemap <- providers$Esri.WorldTopoMap
    
    leaflet(data = out()) %>% 
      addProviderTiles(basemap) %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  observe({
    
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      
      leafletProxy("map", data = out()) %>%
        clearShapes() %>%
        addCircleMarkers(~Longitude, ~Latitude,
                         # radius = ~Bottom.Dissolved.Oxygen*2.5,
                         radius = out()$Bottom.Dissolved.Oxygen*2.5,
                         fillColor = o_cols[o_i],
                         stroke = T,
                         color='black',
                         weight=1,
                         fillOpacity = 0.4,
                         popup = paste('Date (UTC):',out()$Date,'<br>',
                                       'DO (mg/l):',round(out()$Bottom.Dissolved.Oxygen,2)))#,
      # clusterOptions = T)
    }
  })
  
  observe({
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      os <- colorBin(o_cols,o_breaks,bins=o_breaks[seq(1,23,2)])
      leafletProxy("map", data = out()) %>%
        addLegend(position = "topright",
                  pal = os, values = ~Bottom.Dissolved.Oxygen,
                  title = 'Oxygen (mg/l)')
    }
  })
  
  output$table <- renderTable({
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      out <- out()[order(out()$Date),]
      dat <- data.frame(Date=as.character(out$Date),DO=round(out$Bottom.Dissolved.Oxygen,2))
    }
  })
  
}


shinyApp(ui, server)
