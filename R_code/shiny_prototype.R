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
data <- read.csv('all_report_shiny2.csv')
# data <- read.csv('all_report_shiny3.csv')
data$date_utc <- ymd_hms(data$date_utc)
data <- data[-which(is.na(data$lon_dd)),]
data <- data[-which(is.na(data$do_mgl)),]

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
      selectInput('Parameter', 'Parameter', names(data)),
      selectInput('serial_num', 'Serial Number', c('all',unique(data$aquatroll_sn)))
    ),
    mainPanel(
      leafletOutput("map"),
      plotOutput(outputId = "ts_plot"),
      tableOutput("table")
    )
  )
)



server <- function(input, output, session) {
  
    out <- reactive({
      if(input$serial_num=='all'){
      data[which(data$date_utc>=ymd(input$daterange1[1]) &
                   data$date_utc<=ymd(input$daterange1[2]) #&
                   # names(data)==input$Parameter &
      ),]
      } else {
        data[which(data$date_utc>=ymd(input$daterange1[1]) &
                     data$date_utc<=ymd(input$daterange1[2]) &
                     # names(data)==input$Parameter &
                     data$aquatroll_sn==input$serial_num),]
      }
    })
  
  output$ts_plot <- renderPlot({
    
    o_i <- as.numeric(cut(out()$do_mgl,o_breaks))
    
    plot(out()$date_utc,out()$do_mgl,
         xlab='Date',ylab='Bottom Dissolved Oxygen (mg/l)',
         las=2,bg=o_cols[o_i],pch=21,cex=1.5)
    abline(h=c(3.5,2),col=c('gold4','red'),lty=2,lend=2)
  })
  
  output$map <- renderLeaflet({
    
    o_i <- as.numeric(cut(out()$do_mgl,o_breaks))
    
    # basemap <- providers$Esri.NatGeoWorldMap
    basemap <- providers$Esri.OceanBasemap
    # basemap <- providers$Esri.WorldImagery
    # basemap <- providers$Esri.WorldTopoMap
    
    leaflet(data = out()) %>% 
      addProviderTiles(basemap) %>% 
      clearBounds() %>%
      addCircleMarkers(~lon_dd, ~lat_dd,
                       radius = ~do_mgl*2.5,
                       fillColor = o_cols[o_i],
                       stroke = T,
                       color='black',
                       weight=1,
                       fillOpacity = 0.4,
                       popup = paste('Date (UTC):',out()$date_utc,'<br>',
                                     'DO (mg/l):',round(out()$do_mgl,2)))#,
    # clusterOptions = T)
  })
  
  output$table <- renderTable({
    
    out <- out()[order(out()$date_utc),]
    dat <- data.frame(Date=as.character(out$date_utc),DO=round(out$do_mgl,2))
  })
  
}


shinyApp(ui, server)
