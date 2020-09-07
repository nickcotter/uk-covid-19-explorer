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
rm(raw_data)

# lower tier local authority data
lowerTierData <- preProcessedData %>%
    filter(type == 'ltla')

areaNames <- sort(unique(lowerTierData$area))

meanGenerationTime <- generation.time("gamma", c(5, 1.9))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("UK COVID-19 Explorer"),
    
    sidebarLayout(
        
        # Sidebar with a slider input
        sidebarPanel(
            selectInput("lowerTierAreaName", "Area", areaNames),
            
            div(icon("arrow-up"), style="display:none"),
            
            fluidRow(align="center",
                     helpText("Latest R"),
                     h2(htmlOutput("latestR") %>% withSpinner(color="#0dc5c1", type=4))),
        ),
        
        mainPanel(
            plotOutput("areaCases") %>% withSpinner(color="#0dc5c1", type=4), 
            plotOutput("areaR") %>% withSpinner(color="#0dc5c1", type=4)
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

server <- function(input, output) {
    
    localData <- reactive({
        lowerTierData %>%
            filter(area==input$lowerTierAreaName)
    })
        
    effectiveR <- reactive({
        
        c <- localData() %>%
            mutate(avg=rollmean(cases, 7, fill=NA, align="right")) %>%
            filter(!is.na(avg))
        
        epid <- structure(c$avg, names=as.character(c$date))
        r <- estimate.R(epid=epid, end=max(c$date), GT=meanGenerationTime, methods=c("TD"), nsim=1)
        r$estimates$TD$R
    })
    
    output$areaCases <- renderPlot({
        ggplot(localData()) + aes(x=date, y=cases) + ylim(0, NA) + geom_line() + geom_smooth(method = "loess")
    })
    
    output$areaR <- renderPlot({
        
        tryCatch({
            
            r <- effectiveR()
            plottableR <- r[0:(length(r)-1)]
            
            plot(plottableR, type="l", ylab="Effective R", xlab="Day")
            abline(h=1)
            
        }, error=function(err) {
            print(err)
        })
        
    })
    
    output$latestR <- renderText({
        
        tryCatch({
            
            e <- effectiveR()
            
            latestR <- e[length(e)-1]
            
            col <- "black"
            if(latestR > 1) {
                col <- "red"
            } else if(latestR < 1) {
                col <- "green"
            }
            
            latestRoundedR <- round(latestR, digits=2)
            
            codedR <- paste("<font color='", col, "'>", latestRoundedR, "</font>")
            
            if(length(e > 2)) {
                penultimateR <- e[length(e)-2]
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
