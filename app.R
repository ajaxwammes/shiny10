library(shiny)
library(reshape2)
library(shinythemes)
library(leaflet)
library(rworldmap)
library(sp)
library(dplyr)
library(shinycssloaders)

options(shiny.sanitize.errors = FALSE)

data <- read.csv("Data_aug_25.csv")
iso <- read.csv("ISO.csv")
iso2 <- read.csv("ISO2.csv")

visa <- read.csv("Visa.csv")

data2 <- reshape2::dcast(dplyr::mutate(
  reshape2::melt(data, id.var = "Traveling.from"),
  value = plyr::mapvalues(
    value,
    c("-1", "0", "1", "3", "5"),
    c(
      "Selected country",
      "Restricted to enter",
      "Mandatory quarantine after arrival",
      "Mandatory PCR test required",
      "Entrance without COVID-19 restrictions"
    )
  )
),
Traveling.from ~ variable)

data3 <-
  melt(data2, id.vars = 'Traveling.from', variable.name = 'Traveling.to')
data3$Traveling.to <- gsub("\\.", " ", data3$Traveling.to)

visa2 <- reshape2::dcast(dplyr::mutate(
  reshape2::melt(visa, id.var = "Passport"),
  value = plyr::mapvalues(
    value,
    c(
      "VR",
      "ETA",
      "VOA",
      "7",
      "14",
      "15",
      "21",
      "28",
      "30",
      "31",
      "42",
      "60",
      "90",
      "120",
      "180",
      "240",
      "360",
      "VF",
      "-1"
    ),
    c(
      "Visa Required",
      "Visa on Arrival / eVisa",
      "Requires ETA",
      "Visa-Free 7 Days",
      "Visa-Free 14 Days",
      "Visa-Free 15 Days",
      "Visa-Free 21 Days",
      "Visa-Free 28 Days",
      "Visa-Free 30 Days",
      "Visa-Free 31 Days",
      "Visa-Free 42 Days",
      "Visa-Free 60 Days",
      "Visa-Free 90 Days",
      "Visa-Free 120 Days",
      "Visa-Free 180 Days",
      "Visa-Free 240 Days",
      "Visa-Free 360 Days",
      "Visa-Free",
      "Own Country"
    )
  )
  
),
Passport ~ variable)

visa3 <-
  melt(visa2, id.vars = 'Passport', variable.name = 'Traveling.to')
visa3$Traveling.to <- gsub("\\.", " ", visa3$Traveling.to)
visa3$wiki <- visa3$Traveling.to
visa3$wiki <- gsub(" ", "_", visa3$Traveling.to)


pal <-
  colorFactor(
    palette = c("#129665", "#f7f716", "#ff8c00", "#e60000", "#ffffff"),
    domain = data3$value,
    reverse = FALSE
  )
pal2 <-
  colorFactor(
    palette = c(
      "#ff8c00",
      "#ffffff",
      "#ff8c00",
      "#f7f716",
      "#e60000",
      "#129670",
      "#129671",
      "#129672",
      "#129675",
      "#129676",
      "#129677",
      "#129679",
      "#129680",
      "#129681",
      "#129682",
      "#129683",
      "#129684",
      "#129685",
      "#129687",
      "#129689"
    ),
    domain = visa3$value,
    reverse = FALSE
  )


ui <- fluidPage(
  tags$head(includeHTML("google-analytics.html")),
  tags$head(
    HTML(
      '<script data-ad-client="ca-pub-8812626540932611" async src="https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"></script>'
    )
  ),
  HTML(
    '<title> Travel advice during COVID-19. Insight into global travel restrictions. </title>'
  ),
  theme = shinytheme("spacelab"),
  tabsetPanel(
    type = "tabs",
    tabPanel(
      "COVID Travel Restrictions (World Map)", value = "covid", 
      titlePanel("CovidTravel | Daily Updated", title =
                   img(src = "covidtravel.png")),
      
      paste("Last Updated:", Sys.Date()),
      HTML("<b>- Click on country to see the information source</b>"),
      
      fluidPage(fluidRow(column(
        12,
        fluidRow(column(
          2,
          wellPanel(
            selectInput(
              inputId = "location",
              label = "Select current location",
              choices = unique(data3$Traveling.from),
              multiple = FALSE,
              selected = "Netherlands"
            ),
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
            HTML("<b> Step 1:</b> Select your location"),
            br(),
            br(),
            HTML(
              "<b> Step 2:</b> See where you are allowed to travel, according to global COVID-19 travel restrictions"
            ),
            br(),
            br(),
            HTML("<b> This travel map excludes: </b>"),
            br(),
            tags$div(div(id = "Foo2", tags$a(
              "> General Visa Regulations"
            ))),
            tags$div(div(
              id = "Foo3",
              tags$a("> Essential Movement (e.g. business, family, returning home)")
            )),
            HTML("> Availability of transport"),
            br(),
            br(),
          )
        ),
        
        fluidRow(
          column(
            8,
            tags$style(type = "text/css", "#mymap {height: calc(100vh - 145px) !important;}"),
            withSpinner(leafletOutput("mymap"))
          ),
          
          column(
            1,
            a(img(src = "Anywhr.png"), href =
                "https://anywhr-affiliate.peachs.co/a/covidtravel", id = 'partner', target = "_blank")
          )
        ))
      )))
    ),
    
    tabPanel(
      "General Visa Regulations (World Map)", value = "visas", 
      
      titlePanel("CovidTravel | Daily Updated", title =
                   img(src = "covidtravel.png")),
      
      HTML(
        "<script>$('#Foo2').click(function() {
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

                                             })</script>"
      ),
      
      
      paste("Last Updated:", Sys.Date()),
      HTML("<b>- Click on country to learn more </b>"),
      
      fluidPage(fluidRow(column(
        12,
        fluidRow(column(
          2,
          wellPanel(
            selectInput(
              inputId = "passport",
              label = "Select your Passport",
              choices = unique(visa3$Passport),
              multiple = FALSE,
              selected = "Netherlands"
            ),
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
            HTML(
              "<b> Step 2:</b> See where you are allowed to travel, by standard visa regulations"
            ),
            br(),
            br(),
            br(),
            HTML(
              "<b> Note: As a result of COVID-19, some visa on arrivals and visa-free have been suspended </b>"
            ),
            br()
          )
        ),
        
        
        fluidRow(
          column(
            8,
            tags$style(type = "text/css", "#mymap2 {height: calc(100vh - 145px) !important;}"),
            withSpinner(leafletOutput("mymap2"))
          ),
          
          column(1,
                 a(
                   img(src = "Anywhr.png"), href = "https://anywhr.co", target = "_blank"
                 ))
        ))
      )))
    ),
    
    tabPanel(
      "Essential Movement", value = "movement",
      titlePanel(title = img(src = "covidtravel.png")),
      HTML(
        "<script>$('#Foo3').click(function() {
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

                                             })</script>"
      ),
      HTML("<b> > Essential Movement </b>"),
      br(),
      br(),
      HTML(
        "<b> Always read the the source info before travelling (find the source information by clicking on a country)</b>"
      ),
      br(),
      br(),
      "Travel restrictions in CovidTravel do not include:",
      br(),
      HTML(
        "<ol>
                           <li>General Visa Regulations</li>
                           <li>Availability of transportation (flights, trains, boats, etc.)</li>
                           <li>Essential movements such as diplomacy, humanitarian help, returning home, are permanent resident or transit</li>
                           <li>Depending on the country, business and education may also count as essential travel</li>
                           <li>In some cases only certain forms of transportation are allowed to enter a country
                           (in certain cases a country cannot be entered by crossing land, but only by air)</li>
                           <li>Specific regions within a country with deviating rules</li>
                           <li>Governmental recommendations</li>
                           <li>Some countries conduct health checks when entering the country (in some cases randomly).
                             When failing such a test, CovidTravel's travel restriction may not apply </li>
                           <li>CovidTravel has received information that in some cases despite of restricitons, travellers were stills allowed to enter. CovidTravel advises strongly to not travel when official sources state it is not allowed. </li>
                           </ol>"
      )
    ),
    
    tabPanel(
      "Receive Updates", value = "updates",
      titlePanel(title = img(src = "covidtravel.png")),
      HTML("<h2>Updates</h2>"),
      br(),
      HTML("<p>Click here to get notified of changes: </p>"),
      HTML(
        "<a href='https://forms.gle/6RmssgVwYGpMf3t77\' id='notifications' target='_blank'> -> Receive updates</a>"
      ),
      br(),
      br(),
      br(),
      HTML("<h2>Contact</h2>"),
      HTML("<p>For questions, feedback and commercial purposes: </p>"),
      HTML(
        "<a href='https://www.simplevisa.com/?utm_source=partners&utm_medium=banner&utm_campaign=covidtravel' id='simplevisa' target='_blank'>https://www.simplevisa.com</a>"
      ),
      br(),
      br(),
      br(),
      HTML("<h2>About</h2>"),
      HTML(
        "<ul>
                           <li>CovidTravel provides the latest travel restrictions from governments around the world, as a result of the
                 COVID-19 outbreak.</li>
                           <li>CovidTravel is designed to help individuals that want to travel in during the Covid-19 pandemic, providing them a rare insight
                into global travel restrictions, from the most reliable sources.</li>
                           <li>CovidTravel is daily updated by data scraping from 200+ official government and embassy websites.
                around the world. Clicking on a country will show the linked source.</li>
                           <li>CovidTravel and its intellectual property shall not be used without permission from CovidTravel by anyone.</li>
                           <li>CovidTravel is open for commercial collaborations. For this, please find our contact information in the contact tab</li>
                           </ul>"
      )
    ), id = "tabbar"
  )
)


server <- function(input, output) {
  output$mymap <- renderLeaflet({
    pfg4 <- data3[data3$Traveling.from == input$location, ]
    pfg4 <- cbind(pfg4, iso)
    
    pfg5 <- pfg4[which(pfg4$value == "Selected country"), ]
    
    map <- joinCountryData2Map(pfg4, joinCode = "ISO3",
                               nameJoinColumn = "ISO")
    
    mytext <-
      paste(map$Traveling.to, " - ", map$value, "(Click for more info)")  %>%
      lapply(htmltools::HTML)
    
    label <-
      paste(
        "More info about ",
        map$Traveling.to,
        ": ",
        "<a href=",
        map$Source,
        "target=\"_blank\">",
        map$Source,
        "</a>"
      )
    
    
    print(map$value)
    
    
    
    leaflet(data = map) %>% addTiles() %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(
        lng = pfg5$Longitude[1],
        lat = pfg5$Latitude[1],
        zoom = 2.5
      ) %>%
      addMarkers(lng = pfg5$Longitude[1],
                 lat = pfg5$Latitude[1]) %>%
      addPolygons(
        data = map,
        fillColor = ~ pal(map$value),
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
          bringToFront = TRUE
        ),
        label = ~ mytext,
        popup = ~ label
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = map$value,
        opacity = 0.5,
        title = paste("Travel Restrictions:", Sys.Date())
      )
    
  })
  
  output$mymap2 <- renderLeaflet({
    visa4 <- visa3[visa3$Passport == input$passport, ]
    visa4 <- cbind(visa4, iso2)
    
    visa5 <- visa4[which(visa4$value == "Own Country"), ]
    
    map2 <- joinCountryData2Map(visa4, joinCode = "ISO3",
                                nameJoinColumn = "ISO")
    
    mytext2 <- paste(map2$Traveling.to, " - ", map2$value)  %>%
      lapply(htmltools::HTML)
    
    wiki_label <- paste0("https://en.wikipedia.org/wiki/", map2$wiki)
    label2 <-
      paste(
        "More info about ",
        map2$Traveling.to,
        ": ",
        "<a href=",
        wiki_label,
        "target=\"_blank\">",
        wiki_label,
        "</a>"
      )
    
    
    
    print(map2$value)
    
    leaflet(data = map2) %>% addTiles() %>%
      addProviderTiles("CartoDB.Voyager") %>%
      setView(
        lng = visa5$Longitude[1],
        lat = visa5$Latitude[1],
        zoom = 2.5
      ) %>%
      addMarkers(lng = visa5$Longitude[1],
                 lat = visa5$Latitude[1]) %>%
      addPolygons(
        data = map2,
        fillColor = ~ pal2(map2$value),
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
          bringToFront = TRUE
        ),
        label = ~ mytext2,
        popup = ~ label2
      )  %>% addLegend(
        position = "bottomleft",
        pal = pal2,
        values = map2$value,
        opacity = 0.5,
        title = paste("Visa Travel:", Sys.Date())
      ) %>%
      addMiniMap()
    
  })
  
}

shinyApp(ui = ui, server = server)


