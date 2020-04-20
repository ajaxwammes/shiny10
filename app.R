library(shiny)
library(reshape2)
library(shinythemes)
library(leaflet)
library(rworldmap)
library(sp)
library(dplyr)

options(shiny.sanitize.errors = FALSE)

data<-read.csv("Data_april_20.csv")
iso<-read.csv("ISO.csv")

data2 <- reshape2::dcast(
    dplyr::mutate(
        reshape2::melt(data,id.var="Traveling.from"),
        value=plyr::mapvalues(
            value, c("-1","0","1","3","5"),c("Selected country","Restricted to enter","Mandatory quarantine after arrival (14 days)",
                                             "Mandatory certificate of being COVID-19 free",
                                             "Entrance without restrictions"))
    ),Traveling.from~variable)

data3<-melt(data2, id.vars='Traveling.from',variable.name = 'Traveling.to')

pal <- colorFactor(palette = c("#129665","#f7f716","#ff8c00", "#e60000", "#ffffff"), domain=data3$value,reverse = FALSE)



ui <- fluidPage(theme = shinytheme("spacelab"),
                tabsetPanel(type = "tabs",
                            tabPanel("World Map",
                titlePanel(
                    title=img(src="covidtravel.png")),
                
                paste("Last Updated:",Sys.Date()),
                
                fluidRow(column(6,
                                sidebarLayout(
                                    sidebarPanel(
                                        selectInput(inputId = "location",label = "Select current location",
                                                    choices = unique(data3$Traveling.from), multiple = FALSE),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        br(),
                                        HTML("<b> Step 1:</b> Select your current location"),
                                        br(),
                                        br(),
                                        HTML("<b> Step 2:</b> See where you are allowed to travel, according to global COVID-19 travel restrictions"),
                                        br(),
                                        br(),
                                        HTML("<b> This travel map excludes: </b>"),
                                        br(),
                                        "> Availability of flights",
                                        br(),
                                        "> Standard visa regulations",
                                        br(),
                                        "> Essential movements (e.g. returing home, diplomacy)"),
                                    
                                    mainPanel(
                                        leafletOutput("mymap",width = 1235, height = 578)
                                    ))))),
                tabPanel("Contact",
                         titlePanel(title=img(src="covidtravel.png")),
                         paste("For feedback or commercial purposes: martvos10@outlook.com")
                         
                         
                         )))



server <- function(input, output) {
    output$mymap <- renderLeaflet({
        
        pfg4 <- data3[data3$Traveling.from == input$location,]
        pfg4 <- cbind(pfg4,iso)
        
        pfg5 <- pfg4[which(pfg4$value == "Selected country"),]
        
        map <- joinCountryData2Map(pfg4, joinCode = "ISO3",
                                   nameJoinColumn = "ISO")
        
        mytext<- paste(map$Traveling.to, " - ", map$value)  %>% 
            lapply(htmltools::HTML)
        
        print(map$value)
        
        leaflet(data=map) %>% addTiles() %>%
            setView(lng=pfg5$Longitude[1], lat=pfg5$Latitude[1], zoom=3) %>%
            addMarkers(lng=pfg5$Longitude[1], 
                       lat=pfg5$Latitude[1]) %>%
            addPolygons(data=map, fillColor = ~pal(map$value),
                        weight = 1.5,
                        opacity = 1,
                        color = "white",
                        dashArray = "3",
                        fillOpacity = 0.7,
                        highlight = highlightOptions(
                            weight = 3.5,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = .8,
                            bringToFront = TRUE),
                        label = ~mytext)  %>%
            addLegend(position = "bottomleft", pal = pal, values = map$value,opacity = 0.5, 
                      title = paste("Travel Restrictions:",Sys.Date())) %>% 
            addMiniMap()
        
    })
    
}

shinyApp(ui = ui, server = server)
