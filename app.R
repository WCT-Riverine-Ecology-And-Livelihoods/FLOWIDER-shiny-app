library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyalert)
library(plotly)
library(leaflet)
library(sf)
library(DT)
library(dplyr)
library(stringr)
library(anytime)
library(fst)

##loading data--------
fmis_data <- read.fst("Data/fmis_data.fst") 
station_loc <- read.fst("Data/fmis_station_coordinates.fst")
river_shp <- st_read("Data/river_shapefile_FLOWIDER.shp", stringsAsFactors = F)

##preparing fmis data--------
str(fmis_data)
addFormats("%d-%m-%Y")
fmis_data$date <- anydate(fmis_data$date)

##preparing station location data--------
new_cols <- c("species3", "species4", "species5", "species6")
station_loc[new_cols] <- str_split_fixed(station_loc$other_species, ",", 4)
station_loc$species1[station_loc$species1 == "Hardella"] <- "Turtle"
station_loc$species2[station_loc$species2 == "Turtles"] <- "Turtle"

for(i in 1:nrow(station_loc[new_cols])){
    station_loc[i,] <- stringr::str_trim(station_loc[i,], "left")
}

station_loc$species4[station_loc$species4 == "Redcrowned Roofed Turtle"] <- "Turtle"
station_loc$species4[station_loc$species4 == "Softshell Turtles"] <- "Turtle"
station_loc$species5[station_loc$species5 == "Narrowheaded Softshell Turtle"] <- "Turtle"
station_loc$latitude <- as.numeric(station_loc$latitude)
station_loc$longitude <- as.numeric(station_loc$longitude)

##creating active links for altimetry and discharge
station_loc$link_TheiaHydroweb <- ifelse(station_loc$altimetry_data_link_Theia.Hydroweb != "" | is.na(station_loc$altimetry_data_link_Theia.Hydroweb) == FALSE, 
                                         paste0("<a href = '",station_loc$altimetry_data_link_Theia.Hydroweb,"'target='_blank'>",station_loc$altimetry_data_link_Theia.Hydroweb,"</a>"), 
                                         NA)

station_loc$link_DAHITI <- ifelse(station_loc$altimetry_data_link_DAHITI != "", 
                                  paste0("<a href = '",station_loc$altimetry_data_link_DAHITI,"'target='_blank'>",station_loc$altimetry_data_link_DAHITI,"</a>"), 
                                  NA)

station_loc$link_RiverWatch4.5 <- ifelse(station_loc$link_RiverWatch != "" | is.na(station_loc$link_RiverWatch) == FALSE, 
                                         paste0("<a href = '",station_loc$link_RiverWatch,"'target='_blank'>",station_loc$link_RiverWatch,"</a>"), 
                                         NA)

station_location <- station_loc %>% select(-c(order, altimetry_data_link_Theia.Hydroweb, 
                                              altimetry_data_link_DAHITI, link_RiverWatch))
  
##preparing icons for map----
station_icon <- makeIcon(iconUrl = "www/pin_blue.png",
                         iconWidth = 24, iconHeight = 24,
                         iconAnchorX = 12, iconAnchorY = 24)
speciesstation_icon <- makeIcon(iconUrl = "www/pin_gold.png",
                                iconWidth = 24, iconHeight = 24,
                                iconAnchorX = 12, iconAnchorY = 24)
selectedstation_icon <- makeIcon(iconUrl = "www/pin_red.png",
                                 iconWidth = 24, iconHeight = 24,
                                 iconAnchorX = 12, iconAnchorY = 24)
riverstation_icon <- makeIcon(iconUrl = "www/pin_green.png",
                              iconWidth = 24, iconHeight = 24,
                              iconAnchorX = 12, iconAnchorY = 24)

##ui--------
ui <- tagList( 
        includeCSS("www/styles.css"), ##adding css element outside navbarPage to avoid ghost tabs from appearing
        tags$script(src = "https://kit.fontawesome.com/8bd76483c6.js"),
        navbarPage(
           title = div(
             div(
               id = "img-id",
               img(src = "WCTMainLogoWhite_edited.png", height = "57.5px", width = "auto")
             ),
             "FLOWIDER: River Flood Tracking, Gangetic Plains"
           ),
           id = "navbar",
           header = tags$head(
             tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                            padding-top:4px !important; 
                            padding-bottom:0 !important;
                            height: 60px;
                            font-family: Avenir;
                            font-size: 20px;
                            display: flex;
                            justify-content: center;
                            align-items: center;
                            }
                           .navbar {min-height:60px !important;}'))
           ),
           theme = shinytheme("united"),
           tabPanel(title = "Map", value = "tab1",
                    leafletOutput("mymap", width = "100%", height = "600px"),
                    absolutePanel(id = "controls", 
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",
                                  style = "font-family:Arial; font-size:14px",
                                  wellPanel(
                                  p("River flood dynamics are essential to monitor for the conservation of threatened riverine 
                                    wildlife and understanding socio-economic impacts."), ##p adds a new paragraph of text
                                  p("The", strong("FLOWIDER"), "application allows users to track flood water levels of 107 sites 
                                    across 24 Gangetic plains rivers, using data collected from state water resources department websites."),
                                  p(""),
                                  selectizeInput(inputId = "river", label = "Select river(s):", 
                                              choices = c(sort(unique(station_location$river)), ""),
                                              selected = "",
                                              multiple = T),
                                  p(""),
                                  p("After selecting the river above, please select a maximum of two stations on the map for viewing graph.", style = "color:blue"), 
                                  br(),
                                  textInput(inputId = "selectedstation", label = "Selected stations:", value = ""),
                                  actionButton("viewgraph", "View graph"),
                                  actionButton("clearstations", "Clear selection")
                                  )),
                    absolutePanel(id = "controls", 
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE, top = 200, left = 20, right = "auto", bottom = "auto",
                                  width = 200, height = "auto",
                                  style = "font-family:Arial; font-size:14px",
                                  wellPanel(
                                    checkboxGroupInput(inputId = "species", 
                                                       label = "Choose species of interest:", 
                                                       choiceNames = c("Ganges river dolphin", "Gharial", "Marsh crocodile (Mugger)",
                                                               "Freshwater turtles", "Smooth-coated otter", "Indian skimmer",
                                                               "Hilsa"),
                                                       choiceValues = c("GRD", "GHL", "MGR", "Turtle", "SCO", "Indian Skimmer", 
                                                                        "Hilsa"))))),
           tabPanel(title = "Graph", value = "tab2",
                    sidebarLayout(
                      sidebarPanel(
                        style = "font-family:Arial; font-size:12px",
                        radioButtons(inputId = "yearcondition", 
                                     label = "Select years:",
                                     choices = c("All years", "Specific date range"),
                                     selected = character(0)),
                        conditionalPanel(
                                     condition = "input.yearcondition == 'Specific date range'",
                                     dateRangeInput(inputId = "daterange",
                                                    label  = "Select date range:",
                                                    start  = NULL,
                                                    end    = NULL,
                                                    min    = NULL,
                                                    max    = NULL,
                                                    format = "yyyy-mm-dd",
                                                    separator = " to ")),
                        actionButton("go", "Go!")
                      ),
                      mainPanel(
                        shinycssloaders::withSpinner(plotlyOutput("plot"), type = 5, size = 2),
                        tags$style(type="text/css", ##to suppress error message, when changing input values
                                   ".shiny-output-error { visibility: hidden; }",
                                   ".shiny-output-error:before { visibility: hidden; }"
                        )
                      )
                    )),
           tabPanel(title = "Metadata", 
                    value = "tab3", 
                    style = "font-family:Arial; font-size:12px",
                    h3("Information table on stations", style = "font-family: Avenir"),
                    br(),
                    DT::dataTableOutput("mytable")),
           tabPanel(title = "About", 
                    value = "tab4", 
                    style = "font-family:Arial; font-size:14px",
                    h4(strong("Overview"), style = "font-family: Avenir"),
                    p("River flood dynamics are essential to monitor for the conservation of threatened riverine wildlife and understanding socio-economic impacts. The 
                      FLOWIDER: Flow and Water Level (FLOW) Integrated Datasets (ID) for Ecological studies of Rivers (ER) 
                      app provides a web-based interface to track flood water levels of 107 sites across 24  Gangetic plains rivers – in one place, for researchers and conservationists – who can select rivers and sites based on species of interest. 
                      The database is curated from publicly available data provided by state water resources department websites."), 
                    h4(strong("Description"), style = "font-family: Avenir"),
                    p(strong("FLOWIDER 1.0: Flow and Water Level (FLOW) Integrated Datasets (ID) for Ecological studies of Rivers (ER) - a web-based interface to track flood-season water levels of large rivers of India’s Gangetic plains")),
                    p("Accessing and analysing river flow water level data is crucial for scientists, students, and conservationists interested in hydrological monitoring for studies on river ecosystems, species ecology, and human livelihood dependence. In India’s Gangetic plains, hydrological data on river water levels during the flood-season (June to October) are made available in the public domain by government water resource departments on their web portals with the objective of flood disaster risk management and as a warning system. 
                      These data are, however, maintained as daily bulletins in scattered web sources that make it difficult to access time-series data in one place, and utilize, due to variable and non-translatable formats used."),
                    p("To overcome this issue, we present FLOWIDER 1.0, a web-based graphical and metadata interface developed using the", tags$i(class = "fab fa-r-project", title = "R Project"), "Shiny package, to observe and track flood-season water level data from 24 rivers and 107 gauging stations across six states within India’s Gangetic plains. 
                      FLOWIDER combines flood-season water level data from different information sources, presents a map view of gauging sites, and offers date-based graphical options to track the time-series of up to two stations at a time, for various applications. 
                      Some applications include:-"),
                    tags$ol(
                        tags$li("Assessment of flood level rise, recession, and seasonal to inter-annual variability in rivers"), 
                        tags$li("Planning surveys and designing studies to collect hydrological data from nearest available stations"), 
                        tags$li("Assessing the effects of the flood-pulse on variables of interest"),
                        tags$li("Assessing impacts of potential risks from dams or barrages, or extreme rainfall events, on flood-pulses")
                      ),
                    h4(strong("FLOWIDER Graphical Interface: Instructions on Use"), style = "font-family: Avenir"),
                    p("The Graph tool allows users to plot the flood water level data for their river gauging stations of interest, two stations at a time. Users are requested to note:-"),
                    tags$ol(
                      tags$li("For examining the flood water levels in a specific period, use the date ranges provided. Otherwise, select all years."), 
                      tags$li("When starting out, it is better to choose a single station of interest from the map interface. Two stations should be selected, ideally only when they are related to each other or influence each other in some way. 
                              For example, one station can be upstream or downstream of the other, or both could be located at the end points of a river (source and mouth), or one could be above a dam or barrage and the other below it. 
                              Another possibility is that both stations could be on different rivers but can be of relevance for a single species of interest, and hence the need to examine the flood water levels of both together could arise."), 
                      tags$li("The graph plots the two stations on two Y-axes. Reading two Y-axes can be a little confusing to some users, so users are specifically requested to note the endpoints of the two axes, which will be different for the two selected stations."),
                      tags$li("When the users move their cursor over the graph, they can read the flood water level value and date and assess the trend and variation.")
                    ),
                    h4(strong("Species Information"), style = "font-family: Avenir"),
                    tags$p("The distribution of river water level gauging stations in relation to species of interest for hydrological monitoring and habitat assessment relevant to their conservation, is based on the species' distributions known from the latest",
                    tags$a(href = "https://www.iucnredlist.org/", "IUCN Red List Assessments", target = "_blank"), 
                    "and other key literature. This literature is summarized below:-"),
                    tags$ol(
                      tags$li("BirdLife International. 2020. Rynchops albicollis. The IUCN Red List of Threatened Species 2020: e.T22694268A178970109.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2020-3.RLTS.T22694268A178970109.en", "https://dx.doi.org/10.2305/IUCN.UK.2020-3.RLTS.T22694268A178970109.en.", target = "_blank"))), 
                      tags$li("Choudhury, B.C. & de Silva, A. 2013. Crocodylus palustris. The IUCN Red List of Threatened Species 2013: e.T5667A3046723.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2013-2.RLTS.T5667A3046723.en", "https://dx.doi.org/10.2305/IUCN.UK.2013-2.RLTS.T5667A3046723.en.", target = "_blank"))), 
                      tags$li("Kelkar, N., Smith, B.D., Alom, M.Z., Dey, S., Paudel, S. & Braulik, G.T. 2022. Platanista gangetica. The IUCN Red List of Threatened Species 2022: e.T41756A50383346.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2022-1.RLTS.T41756A50383346.en", "https://dx.doi.org/10.2305/IUCN.UK.2022-1.RLTS.T41756A50383346.en.", target = "_blank"))), 
                      tags$li("Khoo, M., Basak, S., Sivasothi, N., de Silva, P.K. & Reza Lubis, I. 2021. Lutrogale perspicillata. The IUCN Red List of Threatened Species 2021: e.T12427A164579961.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2021-3.RLTS.T12427A164579961.en", "https://dx.doi.org/10.2305/IUCN.UK.2021-3.RLTS.T12427A164579961.en.", target = "_blank"))), 
                      tags$li("Lang, J., Chowfin, S. & Ross, J.P. 2019. Gavialis gangeticus. The IUCN Red List of Threatened Species 2019: e.T8966A149227430.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2019-1.RLTS.T8966A149227430.en", "https://dx.doi.org/10.2305/IUCN.UK.2019-1.RLTS.T8966A149227430.en.", target = "_blank"))), 
                      tags$li("Praschag, P., Ahmed, M.F., Das, I. & Singh, S. 2019. Batagur kachuga. The IUCN Red List of Threatened Species 2019: e.T10949A152043133.", 
                              tags$p(tags$a(href = "https://dx.doi.org/10.2305/IUCN.UK.2019-1.RLTS.T10949A152043133.en", "https://dx.doi.org/10.2305/IUCN.UK.2019-1.RLTS.T10949A152043133.en.", target = "_blank"))) 
                    ),
                    tags$p("A selection of 7 endangered or critically endangered taxa is used for the species-based station selection menu provided in the app. These include the Critically Endangered Gharial crocodile and Red-crowned roofed turtle, the Endangered Ganges river dolphin 
                    (India's National Aquatic Animal) and Indian Skimmer, and other vulnerable or near-threatened species such as Smooth-coated Otter, Marsh Crocodile, other species of freshwater softshell and hardshell turtles, and the Hilsa, a declining fish species."),
                    h4(strong("Metadata"), style = "font-family: Avenir"),
                    tags$p("The metadata provided for all stations includes details on the stations upstream and downstream of the selected reference stations, the zero reference level, the danger level (for flooding risks), and site location information. Attributes such as the presence of protected areas for wildlife adjoining these stations, 
                           order of the station from upstream to downstream along the river it is located at, and related details are also included. The metadata tab provides a search tool for the station of interest, and tabulates all the information about the station for the user to view at a glance."),
                    tags$p("In addition, the metadata tab provides links to multi-mission satellite altimetry measurements of river water levels for the virtual station nearest to each CWC station. The virtual stations are based on two data sources:-"),
                    tags$ol(
                       tags$li(tags$a(href = "https://hydroweb.theia-land.fr/", "Theia-HydroWeb", target = "_blank"), "provides time-series of water level data collected on 'Virtual Stations' for lakes and rivers around the world, based on satellite altimetry data"), 
                       tags$li(tags$a(href = "https://dahiti.dgfi.tum.de/en/", "DAHITI", target = "_blank"), " - Database for Hydrological Time Series of Inland Waters, was developed by the Deutsches Geodätisches Forschungsinstitut der Technischen Universität München (DGFI-TUM) in 2013 
                               to provide water level time-series of inland waters, including lakes, reservoirs, rivers, and wetlands derived from satellite data.")
                           ),
                    tags$p("Altimetry data from both sources are available free of charge for the user community after a short registration process, on both the above websites. All related documentation can also be viewed on the website."),
                    tags$p("Additionally, for a few selected sites, the app metadata also connects users to the Flood Observatory, Colorado's River and Reservoir Watch version 4.5 data repository. River and Reservoir Watch provides estimates of river discharge and runoff for a few selected 'reference pixels' of 25 km x 25 km, 
                           based on the calculation of microwave reflectance from the AMSR-E, AMSR-2, and other satellite passive microwave sensors, which is used to calculate discharge based on a global runoff model called WBM. Technical details can be found in the link below."),
                    tags$p("Brakenridge, G.R., Kettner, A.J., Paris, S., Cohen, S. & Nghiem, S.V. 2023. River and Reservoir Watch Version 4.5, Satellite-based river discharge and reservoir area measurements, DFO Flood Observatory, University of Colorado, USA.", 
                           tags$a(href = "https://floodobservatory.colorado.edu/SiteDisplays/20.htm", "https://floodobservatory.colorado.edu/SiteDisplays/20.htm", target = "_blank"), " (Accessed 20 February 2023)."),
                    h4(strong("Terms of Use of the App"), style = "font-family: Avenir"),
                    tags$p("River water level data are classified for all rivers of India’s Gangetic plains. However, for disaster management purposes, flood water level information is uploaded and made publicly accessible by state water resources departments and the",
                           tags$a(href = "https://ffs.india-water.gov.in/", "Central Water Commission, Government of India", target = "_blank"), ". Despite the availability of such vital information in the public domain, many water researchers working in the region are not aware and this limits the use and application of flood water level data in their work. 
                           Our painstakingly curated and designed app is an effort to help with this outreach by providing a viewing and exploration interface with a filter available based on threatened riverine wildlife species."),
                    h4(strong("Restrictions"), style = "font-family: Avenir"),
                    tags$ol(
                      tags$li("No data downloads are allowed in the app."),
                      tags$li("No classified information or data of any nature has been uploaded in the app."),
                      tags$li("Only publicly accessible information is provided with the working links to each data source in the metadata section of the app.")
                    ),
                    tags$hr(style = "border-color:#d3d3d3; background-color: #333; height: 2px"),
                    h4(strong("Data Sources"), style = "font-family: Avenir"),
                    tags$p("Flood Forecast - Central Water Commission, Government of India, is the official and primary repository of river water level data. All data are available on their", 
                           tags$a(href = "https://ffs.india-water.gov.in/", "website", target = "_blank"), "through a Search tool. The data presented in the FLOWIDER app have been compiled from the publicly available sources listed below:-"),
                    tags$ol(
                      tags$li(tags$a(href = "https://wrd.fmiscwrdbihar.gov.in/", "Flood Management Information System, Water Resources Department, Govt. of Bihar", target = "_blank")),
                      tags$li(tags$a(href = "https://idup.gov.in/en/page/flood-bulletin", "Flood Management Information System, Irrigation and Water Resources Department, Govt. of Uttar Pradesh", target = "_blank")),
                      tags$li(tags$a(href = "https://www.wbiwd.gov.in/index.php/applications/dailyreport ", "Irrigation and Waterways Department, Govt. of West Bengal", target = "_blank"))
                    ),
                    h4(strong("Acknowledgements"), style = "font-family: Avenir"),
                    p("We thank the Wildlife Conservation Trust for supporting this initiative. We gratefully acknowledge funding support from the BNP Paribas India Foundation."),
                    p(""),
                    h4(strong("Developed by"), style = "font-family: Avenir"),
                    p("Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust"),
                    h4(strong("Credits"), style = "font-family: Avenir"),
                    tags$ul(
                      tags$li(strong("Conceptualization:"), "Nachiket Kelkar & S.Ramya Roopa"),
                      tags$li(strong("App development:"), "S.Ramya Roopa"),
                      tags$li(strong("Data entry and management support:"), "Nachiket Kelkar, S.Ramya Roopa, Subhasis Dey, Akshay Kumar, Ravindra Kumar, Soumen Bakshi & Kanhaiya Kumar Das.")
                    ),
                    h4(strong("Citation"), style = "font-family: Avenir"),
                    tags$ul(
                      tags$li(strong("App :"), "Riverine Ecosystems and Livelihoods programme, Wildlife Conservation Trust (REAL-WCT). 2023. FLOWIDER 1.0: Flow and Water Level (FLOW) Integrated Datasets (ID) for Ecological studies of Rivers (ER) - a web-based interface to track flood-season water levels of large rivers of India’s Gangetic plains. R Shiny App version.", 
                              tags$a(href = "https://wct-riverine-ecology-and-livelihoods.shinyapps.io/FLOWIDER_app/", "https://wct-riverine-ecology-and-livelihoods.shinyapps.io/FLOWIDER_app/", target = "_blank")),
                      tags$li(strong("Paper :"), "Ramya Roopa, S. & Kelkar, N. (2023). FLOWIDER 1.0: a web-based interface to track flood-season water levels of large rivers of India’s Gangetic plains. Environmental Modelling & Software (Submitted).")
                    ),
                    h4(strong("Code Availability"), style = "font-family: Avenir"),
                    tags$p("The", tags$i(class = "fab fa-r-project", title = "R Project"), "code for this app is publicly available in",  tags$a(href = "https://github.com/WCT-Riverine-Ecology-And-Livelihoods/FLOWIDER-shiny-app/tree/main", "this", target = "_blank"), tags$i(class = "fa-brands fa-github", title="Github"), "repository.")
                    )
        )
)

##server--------
server <- function(input, output, session) {

##Output - Map
output$mymap <- renderLeaflet({
    leaflet() %>%
    addTiles() %>%
    setView(lng = 84.2, lat = 25.3,  zoom = 6) %>% 
    addPolylines(data = river_shp,
                 color = "blue",
                 weight = 2,
                 group = "Station",
                 label = ~paste("River:" , as.character(River_Name))) %>%
    addMarkers(data = station_location,
               lng = ~longitude, 
               lat  = ~latitude, 
               group = "Station",
               layerId = ~station,
               label = ~paste("Station:" , station,
                              "<br>River:", river) %>% lapply(htmltools::HTML),
               icon = station_icon) 
})

observeEvent(input$species, {
  if(is.null(input$species) == TRUE){
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(~longitude, ~latitude, 
                 data = station_location,
                 layerId = ~station,
                 label = ~paste("Station:" , station,
                                "<br>River:", river) %>% lapply(htmltools::HTML),
                 icon = station_icon)
      
  } else {
    selectedspecies_df <- reactive(station_location %>% 
                                     filter(species1 %in% input$species|species2 %in% input$species|species3 %in% input$species|
                                            species4 %in% input$species|species5 %in% input$species|species6 %in% input$species))
    leafletProxy("mymap") %>%
      clearMarkers() %>%
      addMarkers(~longitude, 
                 ~latitude, 
                 data = selectedspecies_df(), 
                 layerId = ~station,
                 label = ~paste("Station:" , station,
                                 "<br>River:", river) %>% lapply(htmltools::HTML),
                 icon = speciesstation_icon)
  }
}, ignoreNULL = FALSE) ##default behavior of observeEvent is to ignore null values, so this has to be changed

observeEvent(input$river, {
  selectedriver_df <- reactive(station_location %>%
                               filter(river %in% input$river))
  leafletProxy("mymap") %>%
  addMarkers(~longitude, 
             ~latitude, 
             data = selectedriver_df(), 
             group = "river_selection",
             layerId = ~station,
             label = ~paste("Station:" , station, "<br>River:", river) %>% lapply(htmltools::HTML),
             icon = riverstation_icon)
})

vals <- reactiveValues(station = NULL) ##reactiveValues creates a list-like object for storing reactive values

observeEvent(input$mymap_marker_click, { ##for updating selected stations in text box
  vals$station <- c(vals$station, input$mymap_marker_click$id)
  selectedstationloc_df <- reactive(station_location %>%
                                    filter(station %in% unlist(vals$station))
                                    )
  nonselectedstationloc_df <- reactive(station_location %>%
                                       filter(!(station %in% unlist(vals$station))))
  updateTextInput(inputId = "selectedstation", value = paste(unlist(vals$station), sep = ", "))
  leafletProxy("mymap") %>%
  addMarkers(~longitude, 
             ~latitude, 
             data = selectedstationloc_df(), 
             layerId = ~station,
             label = ~paste("Station:" , station,
                            "<br>River:", river) %>% lapply(htmltools::HTML),
             icon = selectedstation_icon)
})

observeEvent(input$clearstations, {
  vals$station <- NULL
  updateTextInput(inputId = "selectedstation", value = "")
  updateSelectizeInput(inputId = "river", selected = "")
  leafletProxy("mymap") %>%
  clearMarkers() %>%
  addMarkers(~longitude, 
             ~latitude, 
             data = station_location,
             layerId = ~station,
             label = ~paste("Station:" , station,
                            "<br>River:", river) %>% lapply(htmltools::HTML),
             icon = station_icon)
  updateCheckboxGroupInput(inputId = "species", 
                           label = "Choose species of interest:", 
                           choiceNames = c("Ganges river dolphin", "Gharial", "Marsh crocodile (Mugger)",
                                           "Freshwater turtles", "Smooth-coated otter", "Indian skimmer",
                                           "Hilsa"),
                           choiceValues = c("GRD", "GHL", "MGR", "Turtle", "SCO", 
                                            "Indian Skimmer", "Hilsa"))
  plot <- NULL
})

observeEvent(input$viewgraph, {
      if(length(unlist(vals$station)) <= 2){
        updateNavbarPage(session, inputId = "navbar", selected = "tab2")
      }
  else {
    shinyalert("Error", "Please select a maximum of two stations", type = "error")
  }
})

station_metadata_DT <- reactive({
  station_metadata_transposed <- station_location %>% filter(station %in% c(unlist(vals$station))) %>% t()
  station_metadata_df <- data.frame(cbind(rownames(station_metadata_transposed), station_metadata_transposed))
  if(length(unlist(vals$station)) == 2){
    colnames(station_metadata_df) <- c("Station", station_metadata_df[3,2], station_metadata_df[3,3])
    rownames(station_metadata_df) <- NULL
  } else if(length(unlist(vals$station)) == 1){
    colnames(station_metadata_df) <- c("Station", station_metadata_df[3,2])
    rownames(station_metadata_df) <- NULL
  }
  station_metadata_df 
})

selectedstation_df <- reactive(
  fmis_data %>% filter(station %in% unlist(vals$station))
)

df_with_values <- reactive(
  selectedstation_df() %>% filter(WL_m != "") 
)

observeEvent(input$yearcondition, {
  if(input$yearcondition == "Specific date range"){
    choices <- unique(df_with_values()$date)
    updateDateRangeInput(inputId = "daterange",
                         label  = "Select date range:",
                         start  = min(choices),
                         end    = max(choices),
                         min    = min(choices),
                         max    = max(choices))
  }
})

graph_df <- eventReactive(input$go, {
  if(length(unlist(vals$station)) == 1 & input$yearcondition == "All years"){
    selectedstation_df() %>% select(date,WL_m)
  }
  else if(length(unlist(vals$station)) == 1 & input$yearcondition == "Specific date range"){
    selectedstation_df() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      select(date,WL_m)
  }
  else if(length(unlist(vals$station)) == 2 & input$yearcondition == "All years"){
    selectedstation_df() %>%
      group_by(station) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = station, values_from = WL_m) %>%
      select(date,unlist(vals$station)[1], unlist(vals$station)[2])
  }
  else if(length(unlist(vals$station)) == 2 & input$yearcondition == "Specific date range"){
    selectedstation_df() %>%
      filter(date >= input$daterange[1] & date <= input$daterange[2]) %>%
      group_by(station) %>%
      mutate(row = row_number()) %>%
      tidyr::pivot_wider(names_from = station, values_from = WL_m) %>%
      select(date, unlist(vals$station)[1], unlist(vals$station)[2])
  }
})

##Output - Graph
output$plot <- renderPlotly({
  if(length(unlist(vals$station)) == 1){
    graphdf1 <- graph_df()
    graphdf1 %>%
      plot_ly(x = ~date, y = ~WL_m, type = "scatter", mode = "lines+markers",
              marker = list(color = "#DDAA33"),
              line = list(shape = "linear", color = "#DDAA33"),
              hovertemplate = paste('Date: %{x|%Y-%m-%d}',
                                    '<br>WL_m: %{y}<extra></extra>')
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Water Level (m)")
      )
  } else if(length(unlist(vals$station)) == 2){
    graphdf2 <- graph_df()
    p <- graphdf2 %>% plot_ly()
    p <- p %>%
      add_trace(x = ~date, 
                y = ~get(unlist(vals$station)[1]),
                name = paste(unlist(vals$station)[1]),
                type = "scatter",
                mode = "lines+markers",
                marker = list(color = "#DDAA33"),
                line = list(shape = "linear", color = "#DDAA33"),
                hovertemplate = paste('Date: %{x|%Y-%m-%d}',
                                      '<br>Water level: %{y}<extra></extra>')
      ) %>%
      add_trace(x = ~date, 
                y = ~get(unlist(vals$station)[2]),
                name = paste(unlist(vals$station)[2]),
                yaxis = "y2",
                type = "scatter",
                mode = "lines+markers",
                marker = list(color = "#BB5566"),
                line = list(shape = "linear", color = "#BB5566"),
                hovertemplate = paste('Date: %{x|%Y-%m-%d}',
                                      '<br>Water level: %{y}<extra></extra>')
      ) %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(
          tickfont = list(color = "#DDAA33"),
          title = list(text = paste(unlist(vals$station)[1], "Water Level (m)", sep = " "),
                       font = list(color = "#DDAA33"))),
        yaxis2 = list(
          tickfont = list(color = "#BB5566"),
          overlaying = "y",
          side = "right",
          title = list(text = paste(unlist(vals$station)[2], "Water Level (m)", sep = " "),
                       font = list(color = "#BB5566")))
      )
  }
})

##Output - Data table
output$mytable <- DT::renderDataTable({
  if(input$selectedstation != ""){
    station_metadata_DT() %>% filter(!(Station %in% c("station", "species1", "species2", "other_species", "species3", "species4", "species5", "species6")))
}
else {
  station_location %>% select(!c(species1, species2, other_species, species3, species4, species5, species6))
}
}, rownames = FALSE, escape = FALSE)

}

# Run the app ----
shinyApp(ui = ui, server = server)
