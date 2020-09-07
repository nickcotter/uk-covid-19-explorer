library(shiny)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(readr)
library(lubridate)
library(R0)
library(zoo)


# load latest data
raw_data <- read_csv("https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv")

# pre-processing
preProcessedData <- raw_data %>%
    mutate(area = `Area name`) %>%
    mutate(code = `Area code`) %>%
    mutate(type = `Area type`) %>%
    mutate(date = ymd(`Specimen date`)) %>%
    mutate(cases = `Daily lab-confirmed cases`) %>%
    arrange(date) %>%
    dplyr::select(area, type, code, date, cases)

# lower tier local authority data
lowerTierData <- preProcessedData %>%
    filter(type == 'ltla')

areaNames <- unique(lowerTierData$area)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("UK COVID-19 Explorer"),
    
    sidebarLayout(
        
        # Sidebar with a slider input
        sidebarPanel(
            selectInput("lowerTierAreaName", "Area", areaNames)
        ),
        
        mainPanel(
            plotOutput("areaCases") 
        )
        
        #mainPanel(
        #    tabsetPanel(type="tabs",
#                        tabPanel("Area", plotOutput("areaCases")))    
 #       )
        
    ),
    

    
#    tabsetPanel(type = "tabs",
#                tabPanel("Top 10", plotOutput("top10") %>% withSpinner(color="#0dc5c1", type=4))
##                tabPanel("Map", leafletOutput("map", height = 800) %>% withSpinner(color="#0dc5c1", type=4)),
#    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    localData <- reactive({
        lowerTierData %>%
            filter(area==input$lowerTierAreaName)
    })
    
    output$areaCases <- renderPlot({
        ggplot(localData()) + aes(x=date, y=cases) + ylim(0, NA) + geom_line() + geom_smooth(method = "loess")
    })
    
    #output$map <- renderLeaflet({
    #    mappedLatestData <- latestData() %>%
    #        left_join(combinedGeoData)
    #    
    #    leaflet(mappedLatestData) %>%
    #        addTiles() %>%
    #        addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1, radius = ~TotalCases * 10, popup = ~paste(Area, ":", TotalCases)) %>%
    #        setView(lng = -2.89479, lat = 54.093409, zoom = 6)
    #})
}

# Run the application 
shinyApp(ui = ui, server = server)
