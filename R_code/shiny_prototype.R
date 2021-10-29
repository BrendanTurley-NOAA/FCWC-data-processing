### https://mastering-shiny.org/index.html
### https://rstudio.github.io/leaflet/shiny.html


library(leaflet)
library(lubridate)
library(shiny)

files_wd <- '~/Desktop/professional/projects/Postdoc_FL/data/FCWC/'
setwd(files_wd)
data <- read.csv('mertz2021.csv')
data$date_utc <- ymd_hms(data$date_utc)

ox.col1 <- colorRampPalette(c(1,'firebrick4','red'))
ox.col2 <- colorRampPalette(c('darkgoldenrod4','goldenrod2','gold'))
ox.col3 <- colorRampPalette(c('dodgerblue4','deepskyblue2','cadetblue1'))
o_breaks <- seq(0,10,by=.5)
o_cols <- c(ox.col1(length(o_breaks[o_breaks<2])),
            ox.col2(length(o_breaks[o_breaks>=2 & o_breaks<3.5])),
            ox.col3(length(o_breaks[o_breaks>=3.5])-1))

ui <- fluidPage(
  dateRangeInput("daterange1", "Date range:",
                 start  = "2018-01-01",
                 end    = NULL,
                 min    = "2018-01-01",
                 max    = "2021-12-31",
                 format = "yyyy-mm-dd",
                 separator = " - "),
  
  plotOutput(outputId = "ts_plot"),
  
  leafletOutput("map")
)


server <- function(input, output, session) {
  
  out <- reactive({
    data[which(data$date_utc>=ymd(input$daterange1[1]) &
                        data$date_utc<=ymd(input$daterange1[2])),]
  })
  
  output$ts_plot <- renderPlot({
    out <- data[which(data$date_utc>=ymd(input$daterange1[1]) &
                 data$date_utc<=ymd(input$daterange1[2])),]
    
    plot(out$date_utc,out$do_mgl,
         xlab='Date',ylab='Dissolved Oxygen')
  })
  
  output$map <- renderLeaflet({
        out <- data[which(data$date_utc>=ymd(input$daterange1[1]) &
                        data$date_utc<=ymd(input$daterange1[2])),]
  
    o_i <- as.numeric(cut(out$do_mgl,o_breaks))
  
  # basemap <- providers$Esri.NatGeoWorldMap
  basemap <- providers$Esri.OceanBasemap
  # basemap <- providers$Esri.WorldImagery
  # basemap <- providers$Esri.WorldTopoMap
  
  leaflet(data = out) %>% 
    addProviderTiles(basemap) %>% 
    setView(-82.3, 26.5, zoom = 8) %>%
    addCircleMarkers(~lon_dd, ~lat_dd,
                     radius = ~do_mgl*3,
                     color = o_cols[o_i],
                     stroke = FALSE,
                     fillOpacity = 0.5,
                     popup = paste('Date (UTC):',out$date_utc,'<br>',
                                   'DO (mg/l):',round(out$do_mgl,2)))
  })
  
}


shinyApp(ui, server)
