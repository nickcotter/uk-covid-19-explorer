library(shiny)
library(tidyverse)
library(leaflet)
library(shinycssloaders)
library(shinydashboard)
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
    filter(date < today()) %>%
    arrange(date) %>%
    dplyr::select(area, type, code, date, cases)
rm(raw_data)

#area types
areaTypes <- unique(preProcessedData$type)

# location data
englandNIData <- read.csv("https://extropy-datascience-public.s3-eu-west-1.amazonaws.com/data/ons/local-authorities/Local_Authority_Districts_(April_2019)_Boundaries_UK_BFE.csv") %>%
    mutate(code = LAD19CD) %>%
    mutate(longitude = LONG) %>%
    mutate(latitude = LAT) %>%
    dplyr::select(c(code, longitude, latitude))

walesData <- read.csv("https://extropy-datascience-public.s3-eu-west-1.amazonaws.com/data/ons/local-authorities/Wales_Postcodes-120520.csv") %>%
    mutate(code=Primary.Care.Trust.Code) %>%
    mutate(longitude=Longitude) %>%
    mutate(latitude=Latitude) %>%
    dplyr::select(c(code, longitude, longitude))

combinedGeoData <- bind_rows(englandNIData, walesData)

geoCodedData <- preProcessedData %>%
    left_join(combinedGeoData)

rm(englandNIData)
rm(walesData)
rm(combinedGeoData)


meanGenerationTime <- generation.time("gamma", c(5, 1.9))

ui <- dashboardPage(
    
    dashboardHeader(title = "UK COVID-19 Explorer"),
    
    dashboardSidebar(
        selectInput("areaType", "Area Type", c("Lower-Tier Local Authority" = "ltla",
                                               "Upper-Tier Local Authority" = "utla",
                                               "Region" = "region",
                                               "Nation" = "nation")),
        
        htmlOutput("areaSelector"),
        
        div(icon("arrow-up"), style="display:none"),
        
        fluidRow(align="center",
                 helpText("Latest R"),
                 h2(htmlOutput("latestR") %>% withSpinner(color="#0dc5c1", type=4)))
    ),
    
    dashboardBody(
        tabsetPanel(id = "tabSet",
            tabPanel("Area",value = "areaTab",
                             plotOutput("areaCases") %>% withSpinner(color="#0dc5c1", type=4), 
                             plotOutput("areaR") %>% withSpinner(color="#0dc5c1", type=4)),
        
            
            tabPanel("7 Day Map", leafletOutput("map", height = 800) %>% withSpinner(color="#0dc5c1", type=4))
        ),
        
    )
    
)

server <- function(input, output, session) {
    
    observeEvent(input$areaName, {
        updateTabsetPanel(session, "tabSet",
                          selected = "areaTab")
    })
    
    localData <- reactive({
        geoCodedData %>%
            filter(type == input$areaType) %>%
            filter(area==input$areaName)
    })
        
    effectiveR <- reactive({
        
        c <- localData() %>%
            mutate(avg=rollmean(cases, 7, fill=NA, align="right")) %>%
            filter(!is.na(avg))
        
        #l <- loess(cases~as.numeric(date), localData())
        #p <- predict(l)
        
        epid <- structure(c$avg, names=as.character(c$date))
        est <- estimate.R(epid=epid, begin=min(c$date), end=max(c$date), GT=meanGenerationTime, methods=c("TD"), nsim=1)
        r <- est$estimates$TD$R
        plottableR <- r[0:(length(r)-1)]
        data.frame(date=as.Date(names(plottableR)), r=plottableR) %>%
            slice(1:(n()-1))
        
    })
    
    output$areaSelector <- renderUI({
        
        a <- geoCodedData %>%
            filter(type == input$areaType) %>%
            dplyr::select(area)
        
        selectInput("areaName", "Area", sort(unique(a$area)))
    })
    
    output$areaCases <- renderPlot({
        ggplot(localData()) + aes(x=date, y=cases) + ylim(0, NA) + geom_line() + geom_smooth(method = "loess", span=0.5) + xlab("Date") + ylab("New Cases")
    })
    
    output$areaR <- renderPlot({
        
        tryCatch({
            
            r <- effectiveR()
            
            ggplot(r) + aes(x=date, y=r) + geom_line() + geom_hline(yintercept = 1, col="red") + geom_smooth(method = "loess", span=0.5) + xlab("Date") + ylab("Effective R")
            
            
        }, error=function(err) {
            print(err)
        })
        
    })
    
    output$latestR <- renderText({
        
        tryCatch({
            
            e <- effectiveR()
            
            
            l <- loess(r~as.numeric(date), e, span = 0.5)
            p <- predict(l)
            
            
            latestR <- last(p)

            col <- "black"
            if(latestR > 1) {
                col <- "red"
            } else if(latestR < 1) {
                col <- "green"
            }
            
            latestRoundedR <- round(latestR, digits=2)
            
            codedR <- paste("<font color='", col, "'>", latestRoundedR, "</font>")
            
            if(length(e > 2)) {
                penultimateR <- p[length(p)-2]
                if(penultimateR < latestR) {
                    paste(codedR, "<i class='fas fa-arrow-up'></i>")
                } else if(penultimateR > latestR) {
                    paste(codedR, "<i class='fas fa-arrow-down'></i>")
                } else {
                    codedR
                }
            } else {
                codedR
            }
        }, error=function(e) {
            "N/A"
        })
    })
    
    output$map <- renderLeaflet({
        
        data <- geoCodedData %>%
            filter(!is.na(latitude)) %>%
            filter(!is.na(longitude)) %>%
            filter(type=='ltla') %>%
            filter(date > today() - days(7)) %>%
            group_by(area, latitude, longitude) %>%  
            summarise(last7Days = sum(cases)) %>%
            filter(last7Days > 0)
        
        largest <- max(data$last7Days)
        
        leaflet(data, options = leafletOptions(preferCanvas = TRUE)) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~longitude, lat = ~latitude, radius =  ~100*last7Days/largest, popup = ~paste(area, ":", last7Days), clusterOptions = markerClusterOptions(maxClusterRadius=100)) %>%
            setView(lng = -2.89479, lat = 54.093409, zoom = 6)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
