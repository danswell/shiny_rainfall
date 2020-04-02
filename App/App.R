# This code does the following:

#1 load libraries
#2. Read in static data
#3 extract/process data from data.act.gov.au
#4 extract/process site metadata from data.act.gov.au
#5 Create UI
#6 Create Server

#1 Load libraries

library(dplyr)
library(tidyr)
library(RSocrata)
library(soql)
library(lubridate)
library(ggplot2)
library(shiny)
library(leaflet)
library(plotly)

#2. Read in static data

Static_data <- read.csv("rainfall_data.csv", header = T)
Static_data$Value <- as.numeric(Static_data$Value)
Static_data$DatetimeAEST <- as.POSIXct(Static_data$DatetimeAEST, format = "%Y-%m-%dT%H:%M:%S")
Static_data$DatetimeAEST <- as_date(Static_data$DatetimeAEST)
Static_data$SiteID <- as.factor(Static_data$SiteID)


#3 Extract and process data from data.act.gov.au
# This code uses the RSocrate API. It can be used to extract both the data and the metadata
# The intent here is to pull data since 1 Jan 2020 - to reduce the load on the API.

Datecall <- paste0("DatetimeAEST between '2019-12-31T09:00.000' and ","'",Sys.Date(),"T09:00.000","'")

Query <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/yuhh-28ai.json") %>%
  soql_simple_filter("VariableName", "Rainfall") %>%
  soql_where(Datecall) %>% #dynamic date using sys.Date()
  soql_select("DatetimeAEST, Value, SiteID") %>%
  as.character()

Rainfall_data <- read.socrata(Query)
#write.csv(Rainfall_data, "rainfall_data.csv", row.names = FALSE)

#Prep data
Rainfall_data$Value <- as.numeric(Rainfall_data$Value)
Rainfall_data$DatetimeAEST <- as.POSIXct(Rainfall_data$DatetimeAEST, format = "%Y-%m-%dT%H:%M:%S")
Rainfall_data$DatetimeAEST <- as_date(Rainfall_data$DatetimeAEST)
Rainfall_data$SiteID <- as.factor(Rainfall_data$SiteID)

#merge static and new data, add month and years

Rainfall_data <- bind_rows(Static_data, Rainfall_data)
Rainfall_data$Month <- month(Rainfall_data$DatetimeAEST)
Rainfall_data$Year <- year(Rainfall_data$DatetimeAEST)

#4// Extract metadata from data.act.gov.au

Query2 <- soql() %>%
  soql_add_endpoint("https://www.data.act.gov.au/resource/tsq4-63ge.json") %>%
  soql_select("Siteid, siteName, latitude, longitude") %>%
  as.character()

My_meta <- read.socrata(Query2)

My_meta$Siteid <- as.factor(My_meta$Siteid)
My_meta$latitude <- as.numeric(My_meta$latitude)
My_meta$longitude <- as.numeric(My_meta$longitude)

#filter to relevant sites in the Rainfall dataset
My_meta <- My_meta[My_meta$Siteid %in% Rainfall_data$SiteID,]

My_meta$Name <- paste0(My_meta$Siteid, " - ", My_meta$siteName)

Intermediate <- My_meta %>%
  select(Siteid, Name)

Rainfall_data <- left_join(Rainfall_data, Intermediate, by = c("SiteID"="Siteid"))

##5// UI

ui <- fluidPage(titlePanel("ACT Rainfall Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select site to plot
                    selectInput(inputId = "site", label = strong("Rain Gauge"),
                                choices = unique(My_meta$Name),
                                selected = "410776 - Licking Hole Creek above Cotter Junction"),
                    
                    # Select date range to be plotted
                   # sliderInput("Date", strong("Date range"), min = min(Rainfall_data$DatetimeAEST), max = max(Rainfall_data$DatetimeAEST),
                  #              value = c(as.Date("1980-01-01"), as.Date("2016-12-31")),
                  #              timeFormat = "%Y-%m-%d"),
                  
                  radioButtons(inputId = "timeframe", "Time frame",
                               choices = c("last 7 days", "last 30 days", "last year", "all data", "custom"),
                                           selected = "all data"),
                  
                  conditionalPanel(condition = "input.timeframe == 'custom'",
                                   dateRangeInput("Date", strong("Date range (YYYY-mm-dd)"), min = min(Rainfall_data$DatetimeAEST), max = max(Rainfall_data$DatetimeAEST),
                                  start = min(Rainfall_data$DatetimeAEST), end = max(Rainfall_data$DatetimeAEST))), 
                                  #value = c(as.Date("1980-01-01"), as.Date("2016-12-31")),
                                  #timeFormat = "%Y-%m-%d"),
                  
                  radioButtons(inputId = "aggregator", "Data Aggregator", 
                               choices = c("daily", "monthly", "yearly"),
                               selected = "daily", inline = TRUE),
                  
                  # Add leaflet map
                  leafletOutput("my_map")),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotlyOutput(outputId = "lineplot", height = "400px"),
                    #textOutput(outputId = "cumplot", height = "400px"),
                    downloadButton("download", "Download data"),
                    p(),
                    tags$body("Clicking on pin on map can select a site, as can selecting from dropdown menu. Data provided by ACT Government,", a("ACT Government Open Data Portal", href= "https://www.data.act.gov.au/browse?q=ACT%20Daily%20Rainfall%20and%20Streamflow&sortBy=relevance"),". Questions and comments can be directed to
                    danswell(dot)starrs(at)act(dot)gov(dot)au. Data can be aggregated to monthly or calendar year. Aggregation method is summation. Note this is 
                    sensitive to the date picker input - partial months and years will be computed as selected on the date picker. So select whole months and years to compute meaningful statistics. 
                    Likewise, mean is computed based upon the time range selected. 
                    Code for this app can be found on", a("github", href="https://github.com/danswell/shiny_rainfall"))
                  )
    )
)
                


#6// Define server function
server <- function(input, output, session) {
  
  # Subset data by site
  selected_rain <- reactive({
    Rainfall_data %>%
      filter(
        Name == input$site
        )

  })
  
  #Update slider input to reflect site selected
  observeEvent(input$site, {
  updateDateRangeInput(session, "Date", start = min(selected_rain()$DatetimeAEST), end = max(selected_rain()$DatetimeAEST),
                       min = min(Rainfall_data$DatetimeAEST), max = max(Rainfall_data$DatetimeAEST))
})
  
  #Update selected site based on map click
  observeEvent(input$my_map_marker_click, {
    p <- input$my_map_marker_click
    
    #updateSelectInput(session, "site", selected = p$Siteid) 
    updateSelectInput(session, "site", "Update my site", selected = p$id)
    })


#rewriting this  
  
  #subset data by selected daterange
  selected_rain2 <- reactive({
  req(input$Date)
  validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
  validate(need(input$Date < input$Date[2], "Error: Start date should be earlier than end date."))
  selected_rain() %>%
    filter(DatetimeAEST > input$Date[1] & DatetimeAEST < input$Date[2]
    )
})  

  #####
  
  selected_rain2 <- reactive({
    switch(input$timeframe, 
           "last 7 days" = selected_rain() %>%
             filter(DatetimeAEST > max(DatetimeAEST-7))
           ,
           "last 30 days" = selected_rain() %>%
             filter(DatetimeAEST > max(DatetimeAEST-30))
           ,
           "last year" = selected_rain() %>%
             filter(DatetimeAEST > max(DatetimeAEST)-years(1))
           ,
           "all data" = selected_rain()
           ,
           "custom" =  selected_rain() %>%
             filter(DatetimeAEST > input$Date[1] & DatetimeAEST < input$Date[2]
             )
    )
  })
  
  
  output$my_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = My_meta, lng = ~longitude, lat = ~latitude, layerId = ~Name, popup = ~Name, label = ~Name) %>%
      setView(lng = 149.0, lat = -35.5, zoom = 9)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlotly({switch(input$aggregator, "yearly" = ggplotly(selected_rain2() %>%
                                                                                  group_by(Year) %>%
                                                                                  summarise(Yearly_rain = sum(Value, na.rm = T)) %>%
                                                                                  ggplot() + 
                                                                                  geom_col(mapping = aes(Year, Yearly_rain), color = "blue") +
                                                                                  geom_hline(aes(yintercept = mean(Yearly_rain)), color = "red", linetype = "dashed") +
                                                                                  geom_text(aes(min(Year),mean(Yearly_rain),label = paste0("Annual mean = ", round(mean(Yearly_rain),2)), vjust = -1, hjust = 0)) + 
                                                                                  labs(x = "Date", y = "Annual Rainfall (mm)", title = paste0("Rainfall at ", input$site)), tooltip = c("Year", "Yearly_rain"))
                                                            ,
                                                              "monthly" = ggplotly(selected_rain2() %>%
                                                                                     group_by(Year, Month) %>%
                                                                                     summarise(Monthly_rain = sum(Value, na.rm = T)) %>%
                                                                                     mutate(Date = as.Date(paste(sprintf("%d-%02d", Year, Month), "-01", sep=""))) %>%
                                                                                     ggplot() + 
                                                                                     geom_col(mapping = aes(Date, Monthly_rain), color = "blue") +
                                                                                     geom_hline(aes(yintercept = mean(Monthly_rain)), color = "red", linetype = "dashed") +
                                                                                     geom_text(aes(min(Date),mean(Monthly_rain),label = paste0("Monthly mean = ", round(mean(Monthly_rain),2)), vjust = -1, hjust = 0)) + 
                                                                                     labs(x = "Date", y = "Monthly Rainfall (mm)", title = paste0("Rainfall at ", input$site)), tooltip = c("Date", "Monthly_rain"))
                                                            ,
                                                              "daily" = ggplotly(selected_rain2() %>%
                                                                                   ggplot() +
                                                                                   geom_col(aes(DatetimeAEST, Value), color = "blue") +
                                                                                   geom_hline(aes(yintercept = mean(Value)), color = "red", linetype = "dashed") + 
                                                                                   geom_text(aes(min(DatetimeAEST),mean(Value),label = paste0(" Daily mean = ", round(mean(Value),2)), vjust = -1, hjust = "inward")) + 
                                                                                   labs(x = "Date", y = "Daily Rainfall (mm)", title = paste0("Rainfall at ", input$site)), tooltip = c("DatetimeAEST", "Value"))
  )
  }) 
  
  #Data download

  dfile <-reactive({switch(input$aggregator,
                           "yearly" = selected_rain2() %>%
                             group_by(Year) %>%
                             summarise(Yearly_rain_mm = sum(Value, na.rm = T)) %>%
                             select(Year, Yearly_rain_mm)
                           ,
                           "monthly" = selected_rain2() %>%
                             group_by(Year, Month) %>%
                             summarise(Monthly_rain_mm = sum(Value, na.rm = T)) %>%
                             mutate(Date = as.Date(paste(sprintf("%d-%02d", Year, Month), "-01", sep=""))) %>%
                             select(Date, Monthly_rain_mm, -Year) #this is misbehaving
                           ,
                           "daily" = selected_rain2() %>%
                             mutate(Rainfall_mm = Value) %>%
                             select(DatetimeAEST, Rainfall_mm)
  )
})


  output$download <- downloadHandler(
    filename = function() {
      paste(input$site, "data.csv", sep = "_")
    },
    content = function(file){
      write.csv(dfile(), file, row.names = F)
    }
  )
}

# Create Shiny object
shinyApp(ui = ui, server = server)
