library(shiny)
library(reshape2)
library(shinythemes)
library(leaflet)
library(rworldmap)
library(sp)
library(dplyr)
library(shinycssloaders)

options(shiny.sanitize.errors = FALSE)

data<-read.csv("Data_may_21.csv")
iso<-read.csv("ISO.csv")
iso2<-read.csv("ISO2.csv")

visa<-read.csv("Visa.csv")

data2 <- reshape2::dcast(
dplyr::mutate(
reshape2::melt(data,id.var="Traveling.from"),
value=plyr::mapvalues(
    value, c("-1","0","1","3","5"),c("Selected country","Restricted to enter","Mandatory quarantine after arrival (14 days)",
                                     "Mandatory certificate of being COVID-19 free",
                                     "Entrance without restrictions"))
),Traveling.from~variable)

data3<-melt(data2, id.vars='Traveling.from',variable.name = 'Traveling.to')
data3$Traveling.to <- gsub("\\."," ",data3$Traveling.to)

visa2 <- reshape2::dcast(
dplyr::mutate(
reshape2::melt(visa, id.var="Passport"),
value=plyr::mapvalues(
    value, c("VR","ETA","VOA","7","14","15","21","28","30","31","42","60","90","120","180","240","360","VF","-1"),
    c("Visa Required","Visa on Arrival / eVisa","Requires ETA","Visa-Free 7 Days","Visa-Free 14 Days","Visa-Free 15 Days", 
      "Visa-Free 21 Days", "Visa-Free 28 Days", "Visa-Free 30 Days", "Visa-Free 31 Days","Visa-Free 42 Days", 
      "Visa-Free 60 Days", "Visa-Free 90 Days","Visa-Free 120 Days", "Visa-Free 180 Days",
      "Visa-Free 240 Days","Visa-Free 360 Days","Visa-Free","Own Country"))

),Passport~variable)

visa3<-melt(visa2, id.vars='Passport',variable.name = 'Traveling.to')
visa3$Traveling.to <- gsub("\\."," ",visa3$Traveling.to)
visa3$wiki <- visa3$Traveling.to
visa3$wiki <- gsub(" ","_",visa3$Traveling.to)


pal <- colorFactor(palette = c("#129665","#f7f716","#ff8c00", "#e60000", "#ffffff"), domain=data3$value,reverse = FALSE)
pal2 <- colorFactor(palette = c("#ff8c00","#ffffff","#ff8c00","#f7f716","#e60000","#129670","#129671","#129672",
                        "#129675","#129676","#129677","#129679","#129680","#129681","#129682","#129683","#129684",
                        "#129685","#129687","#129689"), domain = visa3$value, reverse = FALSE)


ui <- fluidPage(
    tags$head(includeHTML(("google-analytics.html"))),
    
    tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
'),
    
    theme = shinytheme("spacelab"),
        tabsetPanel(type = "tabs",
                    tabPanel("COVID Travel Restrictions (World Map)",
                             titlePanel("CovidTravel | Daily Updated",title=img(src="covidtravel.png")),
                             
                             paste("Last Updated:",Sys.Date()), HTML("<b>- Click on country to see the information source</b>"),
                             
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
                                                     HTML("<b> Step 1:</b> Wait for the map to load and select your current location"),
                                                     br(),
                                                     br(),
                                                     HTML("<b> Step 2:</b> See where you are allowed to travel, according to global COVID-19 travel restrictions"),
                                                     br(),
                                                     br(),
                                                     HTML("<b> This travel map excludes: </b>"),
                                                     br(),
                                                     tags$div(div(id="Foo2", tags$a("> General Visa Regulations"))),
                                                     tags$div(div(id="Foo3", tags$a("> Essential Movement"))),
                                                     HTML("> Availability of transport"),),

                                                 
                                                 mainPanel(
                                                     withSpinner(leafletOutput("mymap",width = 1235, height = 578))
                                                 ))))),
                    
                    tabPanel("General Visa Regulations (World Map)",
                             
                             titlePanel(
                                 "CovidTravel | Daily Updated",title=img(src="covidtravel.png")),
                             
                             HTML("<script>$('#Foo2').click(function() {
                                                tabs = $('.tabbable .nav.nav-tabs li')
                                                 tabs.each(function() {
                                                    $(this).removeClass('active')
                                                 })
                                                 $(tabs[1]).addClass('active')

                                                 tabsContents = $('.tabbable .tab-content .tab-pane')
                                                 tabsContents.each(function() {
                                                    $(this).removeClass('active')
                                                 })
                                                 $(tabsContents[1]).addClass('active')

                                                $('#Foo2').trigger('change').trigger('shown');

                                             })</script>"),

                             
                             paste("Last Updated:",Sys.Date()),HTML("<b>- Click on country to learn more </b>"),
                             
                             fluidRow(column(6,
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     selectInput(inputId = "passport",label = "Select your Passport",
                                                                 choices = unique(visa3$Passport), multiple = FALSE, selected = "Netherlands"),
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
                                                     br(),
                                                     HTML("<b> Step 1:</b> Select your passport"),
                                                     br(),
                                                     br(),
                                                     HTML("<b> Step 2:</b> See where you are allowed to travel, according to visa regulations"),
                                                     br(),
                                                     br(),
                                                     br(),
                                                     HTML("<b> Note: As a result of the COVID-19 outbreak, 
                                                     some visa on arrivals and visa-free have been suspended </b>"),
                                                     br()),
                                                 
                                                 
                                                 mainPanel(
                                                     withSpinner(leafletOutput("mymap2",width = 1235, height = 578))
                                                 ))))),
                    tabPanel("Essential Movement",
                             titlePanel(title=img(src="covidtravel.png")),
                             HTML("<script>$('#Foo3').click(function() {
                                                tabs = $('.tabbable .nav.nav-tabs li')
                                                 tabs.each(function() {
                                                    $(this).removeClass('active')
                                                 })
                                                 $(tabs[2]).addClass('active')

                                                 tabsContents = $('.tabbable .tab-content .tab-pane')
                                                 tabsContents.each(function() {
                                                    $(this).removeClass('active')
                                                 }) 
                                                 $(tabsContents[2]).addClass('active')

                                                $('#Foo3').trigger('change').trigger('shown');

                                             })</script>"),
                             HTML("<b> Always read the the source info before travelling (find the source information by clicking on the country)</b>"),
                             br(),
                             br(),
                             "Travel restrictions in CovidTravel do not include:",
                             br(),
                             HTML("<ol>
                           <li>General Visa Regulations</li>
                           <li>Availability of transportation (flights, trains, boats, etc.)</li>
                           <li>Essential movements such as diplomacy, humanitarian help, returning home, are permanent resident or transit</li>
                           <li>Depending on the country, business and education may also count as essential travel</li>
                           <li>In some cases only certain forms of transportation are allowed to enter a country 
                           (in certain cases a country cannot be entered by crossing land, but only by air</li>
                           <li>Specific regions within a country with deviating rules</li>
                           <li>Governmental recommendations</li>
                           <li>In some cases, health checks such as temperature checks are (in some cases randomly) executed at borders. 
                             When failing such a test, CovidTravel's travel restriction may not apply </li>
                           <li>CovidTravel has received information that in some cases despite of restricitons, travellers were stills allowed to enter. CovidTravel advises strongly to not travel when official sources state it is not allowed. </li>
                           </ol>")),
                             
                    tabPanel("About",
                 titlePanel(title=img(src="covidtravel.png")),
                 "CovidTravel provides the latest travel restrictions from governments around the world, as a result of the 
                 COVID-19 outbreak.", 
                 br(),
                 "It is designed to help businesses and individuals that rely on traveling, providing them a rare insight 
                into global travel restrictions, from the most reliable sources.",
                 br(),
                 br(),
                "CovidTravel is daily updated by data scraping from the most reliable official governmental websites and embassies 
                all around the world.", 
                 br(),
                "The source of data for a specific country can be found by clicking on the country.",
                br(),
                 "In addition to data-scraping, manual checks are performed in order provide data as accurate as possible.", 
                 br(),
                 br(),
                "CovidTravel is made and maintained by a team of data engineers from the National University of Singapore.", 
                br(),
                "CovidTravel and its intellectual property shall not be used without permission by any third 
                parties.",
                br(),
                br(),
                "For commercial interest, please find our contact information in the contact tab"),
                    
                    tabPanel("Contact",
                             titlePanel(title=img(src="covidtravel.png")),
                             paste("For questions, feedback and commercial purposes: "),
                             HTML("<a href=","mailto:mart@u.nus.edu", ">", "mart@u.nus.edu", "</a>"))
                    
                    ))


server <- function(input, output, session) {
  
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  output$geolocation <- renderPrint({
    input$geolocation
  })
  
  
  geolocation_lat <- reactive({
    as.character(input$lat)
  })
  
  geolocation_long <- reactive({
    as.character(input$long)
  })
  
  country_name <- reactive({
    my_url <- paste0("http://api.geonames.org/countryCodeJSON?lat=",
                     geolocation_lat(),
                     "&lng=",
                     geolocation_long(),
                     "&username=covidtravel")
    my_raw_result <- httr::GET(my_url)
    
    my_content <-httr::content(my_raw_result, as = "text")
    my_content_from_json <- jsonlite::fromJSON(my_content)
    return(my_content_from_json$countryName)
    
  })
  
  observe({
    
    updateSelectInput(session,
                      inputId = "location",
                      label = "Select current location",
                      choices = unique(data3$Traveling.from),
                      selected = country_name()
    )
    
  })
  
  output$geolocation <- renderPrint({
    country_name()
  })
  
    
    output$mymap <- renderLeaflet({

pfg4 <- data3[data3$Traveling.from == input$location,]
pfg4 <- cbind(pfg4,iso)

pfg5 <- pfg4[which(pfg4$value == "Selected country"),]

map <- joinCountryData2Map(pfg4, joinCode = "ISO3",
                           nameJoinColumn = "ISO")

mytext <- paste(map$Traveling.to, " - ", map$value, "(Click for more info)")  %>% 
  lapply(htmltools::HTML)

label <- paste("More info about ", map$Traveling.to,": ","<a href=",map$Source,"target=\"_blank\">",map$Source, "</a>")


print(map$value)

leaflet(data=map) %>% addTiles() %>%
  setView(lng=pfg5$Longitude[1], lat=pfg5$Latitude[1], zoom=2.5) %>%
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
                label = ~mytext, popup = ~label)  %>%
    addLegend(position = "bottomleft", pal = pal, values = map$value,opacity = 0.5, 
              title = paste("Travel Restrictions:",Sys.Date())) %>% 
    addMiniMap()

})

output$mymap2 <- renderLeaflet({

visa4 <- visa3[visa3$Passport == input$passport,]
visa4 <- cbind(visa4,iso2)

visa5 <- visa4[which(visa4$value == "Own Country"),]

map2 <- joinCountryData2Map(visa4, joinCode = "ISO3",
                            nameJoinColumn = "ISO")

mytext2<- paste(map2$Traveling.to, " - ", map2$value)  %>% 
    lapply(htmltools::HTML)

print(map2$value)

leaflet(data=map2) %>% addTiles() %>%
    setView(lng=visa5$Longitude[1], lat=visa5$Latitude[1], zoom=2.5) %>%
    addMarkers(lng=visa5$Longitude[1], 
               lat=visa5$Latitude[1]) %>%
    addPolygons(data=map2, fillColor = ~pal2(map2$value),
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
                    bringToFront = TRUE), popup = paste0("More info about ", map2$Traveling.to,": ", "<a href=", "https://en.wikipedia.org/wiki/",map2$wiki,
                                                         ">","https://en.wikipedia.org/wiki/",map2$wiki, "</a>"),
label = ~mytext2)  %>% addLegend(position = "bottomleft", pal = pal2, values = map2$value,opacity = 0.5, 
                                 title = paste("Visa Travel:",Sys.Date())) %>%
    addMiniMap()

})

}

shinyApp(ui = ui, server = server)