### https://mastering-shiny.org/index.html
### https://rstudio.github.io/leaflet/shiny.html
### https://stackoverflow.com/questions/53016404/advantages-of-reactive-vs-observe-vs-observeevent
### https://www.r-bloggers.com/2016/03/r-shiny-leaflet-using-observers/
### https://shiny.rstudio.com/articles/layout-guide.html

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
    if(input$parameter=='Surface.Temperature'){
      t_i <- as.numeric(cut(out()$Surface.Temperature,t_breaks))
      bg <- t_cols[t_i]
      parm <- out()$Surface.Temperature
      ylab <- 'Surface Temperature (C)'
    }
    if(input$parameter=='Bottom.Temperature'){
      t_i <- as.numeric(cut(out()$Bottom.Temperature,t_breaks))
      bg <- t_cols[t_i]
      parm <- out()$Bottom.Temperature
      ylab <- 'Bottome Temperature (C)'
    }
    if(input$parameter=='Surface.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Surface.Dissolved.Oxygen,o_breaks))
      bg <- o_cols[o_i]
      parm <- out()$Surface.Dissolved.Oxygen
      ylab <- 'Surface Dissolved Oxygen (mg/l)'
    }
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      bg <- o_cols[o_i]
      parm <- out()$Bottom.Dissolved.Oxygen
      ylab <- 'Bottom Dissolved Oxygen (mg/l)'
    }
    
      plot(out()$Date,parm,
           xlab='Date',ylab=ylab,
           las=2,bg=bg,pch=21,cex=1.5)
      # abline(h=c(3.5,2),col=c('gold4','red'),lty=2,lend=2)
      
  })
  
  output$boxplot <- renderPlot({
    
    if(input$parameter=='Surface.Temperature'){
      t_i <- as.numeric(cut(out()$Surface.Temperature,t_breaks))
      bg <- t_cols[t_i]
      all_y <- box_data$surf_t
      ylab <- 'Surface Temperature (C)'
      select_y <- out()$Surface.Temperature
    }
    if(input$parameter=='Bottom.Temperature'){
      t_i <- as.numeric(cut(out()$Bottom.Temperature,t_breaks))
      bg <- t_cols[t_i]
      all_y <- box_data$bot_t
      ylab <- 'Bottom Temperature (C)'
      select_y <- out()$Bottom.Temperature
    }
    if(input$parameter=='Surface.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Surface.Dissolved.Oxygen,o_breaks))
      bg <- o_cols[o_i]
      all_y <- box_data$surf_do
      ylab <- 'Surface Dissolved Oxygen (mg/l)'
      select_y <- out()$Surface.Dissolved.Oxygen
    }
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      bg <- o_cols[o_i]
      all_y <- box_data$bot_do
      ylab <- 'Bottom Dissolved Oxygen (mg/l)'
      select_y <- out()$Bottom.Dissolved.Oxygen
    }
      
      boxplot(all_y~month(box_data$date),na.action = na.pass,
              xlab='Month',ylab=ylab,
              staplewex=0,outwex=0,outline=F,lty=1,lwd=1.5,names=month.abb[1:12],las=2)
      # mtext('Climatology Plot',cex=2,adj=0,font=2,line=1)
      points(jitter(month(out()$Date),3,.3),select_y,
             bg=bg,pch=21,cex=1.5)

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
   
    if(input$parameter=='Surface.Temperature'){
      t_i <- as.numeric(cut(out()$Surface.Temperature,t_breaks))
      cols <- t_cols[t_i]
      unit <- 'Temperature (C):'
      vals <- round(out()$Surface.Temperature,2)
      ext <- 1/2
    }
    if(input$parameter=='Bottom.Temperature'){
      t_i <- as.numeric(cut(out()$Bottom.Temperature,t_breaks))
      cols <- t_cols[t_i]
      unit <- 'Temperature (C):'
      vals <- round(out()$Bottom.Temperature,2)
      ext <- 1/2
    }
    if(input$parameter=='Surface.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Surface.Dissolved.Oxygen,o_breaks))
      cols <- o_cols[o_i]
      unit <- 'DO (mg/l):'
      vals <- round(out()$Surface.Dissolved.Oxygen,2)
      ext <- 2.5
    } 
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      o_i <- as.numeric(cut(out()$Bottom.Dissolved.Oxygen,o_breaks))
      cols <- o_cols[o_i]
      unit <- 'DO (mg/l):'
      vals <- round(out()$Bottom.Dissolved.Oxygen,2)
      ext <- 2.5
    }
    
    leafletProxy("map", data = out()) %>%
      clearMarkers() %>%
      addCircleMarkers(~Longitude, ~Latitude,
                       radius = vals*ext,
                       fillColor = cols,
                       stroke = T,
                       color='black',
                       weight=1,
                       fillOpacity = 0.4,
                       popup = paste('Date (UTC):',out()$Date,'<br>',
                                     unit,vals))#,
    # clusterOptions = T)
    # }

  })
  
  observe({
    if(input$parameter=='Surface.Temperature'){
      os <- colorBin(t_cols,t_breaks,bins=t_breaks[seq(1,16,2)])
      values <- out()$Surface.Temperature
      title <- 'Temperature (C)'
    }
    if(input$parameter=='Bottom.Temperature'){
      os <- colorBin(t_cols,t_breaks,bins=t_breaks[seq(1,16,2)])
      values <- out()$Bottom.Temperature
      title <- 'Temperature (C)'
    }
    if(input$parameter=='Surface.Dissolved.Oxygen'){
      os <- colorBin(o_cols,o_breaks,bins=o_breaks[seq(1,23,2)])
      values <- out()$Surface.Dissolved.Oxygen
      title <- 'Oxygen (mg/l)'
    }
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      os <- colorBin(o_cols,o_breaks,bins=o_breaks[seq(1,23,2)])
      values <- out()$Bottom.Dissolved.Oxygen
      title <- 'Oxygen (mg/l)'
    }
    leafletProxy("map", data = out()) %>%
      clearControls() %>%
      addLegend(position = "topright",
                pal = os,
                values = values,
                title = title)
  })
  
  output$table <- renderTable({
    out <- out()[order(out()$Date),]
    if(input$parameter=='Surface.Temperature'){
      dat <- data.frame('Date'=as.character(out$Date),
                        'Serial Number'=out$aquatroll_sn,
                        'Surface Temperature (C)'=round(out$Surface.Temperature,2))
    }
    if(input$parameter=='Bottom.Temperature'){
      dat <- data.frame('Date'=as.character(out$Date),
                        'Serial Number'=out$aquatroll_sn,
                        'Bottom Tmperature (C)'=round(out$Bottom.Temperature,2))
    }
    if(input$parameter=='Surface.Dissolved.Oxygen'){
      dat <- data.frame('Date'=as.character(out$Date),
                        'Serial Number'=out$aquatroll_sn,
                        'Surface Dissolved Oxygen (mg/l)'=round(out$Surface.Dissolved.Oxygen,2))
    }
    if(input$parameter=='Bottom.Dissolved.Oxygen'){
      dat <- data.frame('Date'=as.character(out$Date),
                        'Serial Number'=out$aquatroll_sn,
                        'Bottom Dissolved Oxygen (mg/l)'=round(out$Bottom.Dissolved.Oxygen,2))
    }
  })
  
  output$info <- renderText({
    paste0("Date (UTC): ", as.POSIXct(input$plot_click$x, origin = "1970-01-01"),
           "\nDissolved Oxygen (mg/l): ", round(as.numeric(input$plot_click$y),2))
  })
  
  output$info2 <- renderText({
    paste0("Date (UTC): ", as.POSIXct(input$plot_click2$x, origin = "1970-01-01"),
           "\nDissolved Oxygen (mg/l): ", round(as.numeric(input$plot_click2$y),2))
  })
  
}


shinyApp(ui, server)
