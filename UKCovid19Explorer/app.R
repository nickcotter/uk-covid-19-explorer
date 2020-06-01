library(shiny)
library(tidyverse)
library(leaflet)
library(shinycssloaders)

# load static data
englandNIData <- read.csv("https://extropy-datascience-public.s3-eu-west-1.amazonaws.com/data/ons/local-authorities/Local_Authority_Districts_(April_2019)_Boundaries_UK_BFE.csv") %>%
    mutate(AreaCode = LAD19CD) %>%
    mutate(Longitude = LONG) %>%
    mutate(Latitude = LAT) %>%
    select(c(AreaCode, Longitude, Latitude))

walesData <- read.csv("https://extropy-datascience-public.s3-eu-west-1.amazonaws.com/data/ons/local-authorities/Wales_Postcodes-120520.csv") %>%
    mutate(AreaCode=Primary.Care.Trust.Code) %>%
    select(c(AreaCode, Longitude, Latitude))

combinedGeoData <- bind_rows(englandNIData, walesData)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("UK COVID-19 Explorer"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Map", leafletOutput("map", height = 800) %>% withSpinner(color="#0dc5c1", type=4)),
                tabPanel("Top 10", plotOutput("top10") %>% withSpinner(color="#0dc5c1", type=4))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    caseData <- reactive({
        raw_data <- read.csv("https://raw.githubusercontent.com/tomwhite/covid-19-uk-data/master/data/covid-19-cases-uk.csv")
        raw_data %>%
            mutate(Date = as.Date(Date)) %>%
            mutate(TotalCases = as.integer(TotalCases)) %>%
            filter(TotalCases > 0)
    })
    
    latestData <- reactive({
        caseData() %>%
            group_by(AreaCode) %>% 
            slice(which.max(Date))
    })
    
    output$top10 <- renderPlot({
        top10 <- latestData() %>% ungroup() %>% top_n(10, TotalCases)
        ggplot(top10, aes(x=reorder(Area, TotalCases), y=TotalCases))  + geom_col() +
            xlab("Area") +
            coord_flip() +
            ggtitle("Top 10 Areas By Total Case Count")
    })
    
    output$map <- renderLeaflet({
        mappedLatestData <- latestData() %>%
            left_join(combinedGeoData)
        
        leaflet(mappedLatestData) %>%
            addTiles() %>%
            addCircles(lng = ~Longitude, lat = ~Latitude, weight = 10, radius = ~TotalCases * 10, popup = ~Area) %>%
            setView(lng = -2.89479, lat = 54.093409, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
