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

#2. Read in static data

Static_data <- read.csv("Rainfall_data.csv", header = T)
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


##5// UI

ui <- fluidPage(titlePanel("ACT Rainfall Explorer"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select site to plot
                    selectInput(inputId = "site", label = strong("Rain Gauge"),
                                choices = unique(My_meta$Siteid),
                                selected = "410776"),
                    
                    # Select date range to be plotted
                    sliderInput("Date", strong("Date range"), min = as.Date("1980-01-01"), max = as.Date("2016-12-31"),
                                value = c(as.Date("1980-01-01"), as.Date("2016-12-31")),
                                timeFormat = "%Y-%m-%d"),
                    
                    # Data aggregator
                    # Select whether to aggregate data to monthly
                  checkboxInput(inputId = "monthly", label = strong("Monthly aggregator"), value = FALSE),
                  
                  # Select whether to aggregate data to annually
                  checkboxInput(inputId = "yearly", label = strong("Yearly aggregator"), value = FALSE),
                  
                  # Add leaflet map
                  leafletOutput("my_map")
                  ),

                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "400px"),
                    #textOutput(outputId = "cumplot", height = "400px"),
                    tags$body("Clicking on pin on map can select site, as can selecting from dropdown menu. Data provided by ACT Government (www.data.act.gov.au). Questions and comments can be directed to
                    danswell(dot)starrs(at)act(dot)gov(dot)au. Data can be aggregated to monthly or calendar year. Note this is 
                    sensitive to the time slider input - partial months and years will be computed based on time slider")
                    )
                    )
                  )


#6// Define server function
server <- function(input, output, session) {
  
  # Subset data by site
  selected_rain <- reactive({
    Rainfall_data %>%
      filter(
        SiteID == input$site
        )

  })
  
  #Update slider input to reflect site selected
  observeEvent(input$site, {
  updateSliderInput(session, "Date", value = c(min(selected_rain()$DatetimeAEST), max(selected_rain()$DatetimeAEST)),
                    min = min(selected_rain()$DatetimeAEST), max = max(selected_rain()$DatetimeAEST))
})
  
  #Update selected site based on map click
  observeEvent(input$my_map_marker_click, {
    p <- input$my_map_marker_click
    
    #updateSelectInput(session, "site", selected = p$Siteid) 
    updateSelectInput(session, "site", "Update my site", selected = p$id)
    })


  #subset data by selected daterange
  selected_rain2 <- reactive({
  req(input$Date)
  validate(need(!is.na(input$Date[1]) & !is.na(input$Date[2]), "Error: Please provide both a start and an end date."))
  validate(need(input$Date < input$Date[2], "Error: Start date should be earlier than end date."))
  selected_rain() %>%
    filter(DatetimeAEST > input$Date[1] & DatetimeAEST < input$Date[2]
    )
})  
  
#  https://stackoverflow.com/questions/48633984/pass-a-dynamic-date-date-range-to-daterangeinput-in-r-shiny

  
  output$my_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(data = My_meta, lng = ~longitude, lat = ~latitude, layerId = ~Siteid, popup = ~paste0(Siteid, " - ",siteName), label = ~paste0(Siteid, " - ",siteName)) %>%
      setView(lng = 149.0, lat = -35.5, zoom = 9)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({if(input$yearly == TRUE){
    selected_rain2() %>%
      group_by(Year) %>%
      summarise(Yearly_rain = sum(Value, na.rm = T)) %>%
      ggplot() + 
      geom_col(mapping = aes(Year, Yearly_rain), color = "blue") +
      labs(x = "Date", y = "Annual Rainfall (mm)", title = paste0("Rainfall at ", input$site))
  }
   else if(input$monthly == TRUE){
     selected_rain2() %>%
     group_by(Year, Month) %>%
     summarise(Monthly_rain = sum(Value, na.rm = T)) %>%
     mutate(My_date = as.Date(paste(sprintf("%d-%02d", Year, Month), "-01", sep=""))) %>%
     ggplot() + 
     geom_col(mapping = aes(My_date, Monthly_rain), color = "blue") +
     labs(x = "Date", y = "Monthly Rainfall (mm)", title = paste0("Rainfall at ", input$site))
    }
    else {selected_rain2() %>%
       ggplot() +
       geom_col(aes(DatetimeAEST, Value), color = "blue") +
       labs(x = "Date", y = "Daily Rainfall (mm)", title = paste0("Rainfall at ", input$site)) 
     }
  })
}

  #Create cumulative flow plot
  #output$cumplot <- renderPlot({
   # ggplot(selected_rain()) + 
    #  geom_line(aes(DatetimeAEST, cumsum(replace_na(Value,0)), color = "blue")) +
     # labs(x = "Date", y = "Cumulative flow (GL)", title = paste("Cumulative flow at", (input$site))) + 
      #scale_color_manual(values = c("blue", "red"), labels = c("Gauged cumulative flow", "Modelled cumulative flow"), name = "Legend")
#  })
#                              }

# Create Shiny object
shinyApp(ui = ui, server = server)




