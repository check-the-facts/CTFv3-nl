#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#USE THIS IF PLOTLY DOESN'T WORK: devtools::install_github('ropensci/plotly')

library(ggplot2)
library(sp)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(inlmisc)
library(jsonlite)
library(nominatim)
library(tidygeocoder)
library(mdthemes)
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonio)
library(cld2)
library(magrittr)
library(htmlwidgets)
library(ggpubr)
library(plotly)
library(franc)
library(cbsodataR)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(DT)


#Pivot data to year column
#DUO data 
#######WO########
data4 <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/04-inschrijvingen-wo-2019.csv", sep= ',')
data4 <- pivot_longer(data4, cols = starts_with("X20"), values_to = "Registered", names_to = "Year", values_drop_na = TRUE) 

data4 %>% mutate_if(is.factor, as.character) -> data4

data4 <- mutate(data4, Year = as.integer(gsub("X", "", Year)))

data4$OPLEIDINGSNAAM.ACTUEEL <- as.character(data4$OPLEIDINGSNAAM.ACTUEEL)
data4$CROHO.ONDERDEEL <- as.character(data4$CROHO.ONDERDEEL)
data4$GEMEENTENAAM <- as.character(data4$GEMEENTENAAM)

data4$language <- detect_language(data4$OPLEIDINGSNAAM.ACTUEEL, plain_text = TRUE)
data4$language[is.na(data4$language) == TRUE] <- "en/nl"
data4$language[data4$language != 'en'] <- "en/nl"

#######HBO######
data4hbo <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/04-inschrijvingen-hbo-2019.csv", sep = ';')
data4hbo <- pivot_longer(data4hbo, cols = starts_with("X20"), values_to = "Registered", names_to = "Year", values_drop_na = TRUE) 

data4hbo %>% mutate_if(is.factor, as.character) -> data4hbo

data4hbo <- mutate(data4hbo, Year = as.integer(gsub("X", "", Year)))

data4hbo$OPLEIDINGSNAAM.ACTUEEL <- as.character(data4hbo$OPLEIDINGSNAAM.ACTUEEL)
data4hbo$CROHO.ONDERDEEL <- as.character(data4hbo$CROHO.ONDERDEEL)
data4hbo$GEMEENTENAAM <- as.character(data4hbo$GEMEENTENAAM)

data4hbo$language <- detect_language(data4hbo$OPLEIDINGSNAAM.ACTUEEL, plain_text = TRUE)
data4hbo$language[is.na(data4hbo$language) == TRUE] <- "en/nl"
data4hbo$language[data4hbo$language != 'en'] <- "en/nl"

######JOIN########
data4$HO.type <- 'WO'
data4hbo$HO.type <- 'HBO'

data4 <- rbind(data4, data4hbo)

#Query the provinces
bb_zh <- getbb('Zuid Holland')


#OSM all universities 
amenity_values <- c('university')


universities <- amenities$osm_polygons %>% 
  select(osm_id, name) %>% 
  drop_na(name)


universities %>% 
  write_sf('universities.shp', driver = 'ESRI Shapefile')

universities_from_shapefile <- read_sf('universities.shp')

MultiP <- amenities$osm_multipolygons %>%
  select(osm_id, name) %>%
  drop_na(name)

MultiP %>%
  write_sf('MultiP.shp', driver = 'ESRI Shapefile')

MultiP_shape <- read_sf('MultiP.shp')


#Adding official addresses
HOadr <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/01-instellingen-hbo-en-wo.csv", sep = ';')
HOadr$Full_Address<- paste(HOadr$STRAATNAAM, HOadr$HUISNUMMER.TOEVOEGING, HOadr$PLAATSNAAM, sep =" ")
HOadr$StreetN <- paste(HOadr$STRAATNAAM, HOadr$HUISNUMMER.TOEVOEGING, sep = " ")
HOadr$GEMEENTENAAM <- as.character(HOadr$GEMEENTENAAM)

HO_locations <- HOadr %>%
  geocode(street = StreetN, city = GEMEENTENAAM, country = 'Nederland', method = 'osm')

#WOZ, POP and Rent 2019

metadata <- cbs_get_meta("84583NED")
print(metadata$DataProperties$Key)

WBwaarde <- cbs_get_data("84583NED", 
                         select=c("WijkenEnBuurten","GemiddeldeWoningwaarde_35")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         WOZ = GemiddeldeWoningwaarde_35)


WBpop <-cbs_get_data("84583NED", 
                     select=c("WijkenEnBuurten","AantalInwoners_5")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         population = AantalInwoners_5)

WBrent <-cbs_get_data("84583NED", 
                      select=c("WijkenEnBuurten","HuurwoningenTotaal_41")) %>%
  mutate(WijkenEnBuurten = str_trim(WijkenEnBuurten),
         value = HuurwoningenTotaal_41)

# Retrieve data with municipal boundaries from PDOK
municipalBoundaries <- st_read("https://geodata.nationaalgeoregister.nl/cbsgebiedsindelingen/wfs?request=GetFeature&service=WFS&version=2.0.0&typeName=cbs_gemeente_2019_gegeneraliseerd&outputFormat=json")


# Link data from Statistics Netherlands to geodata
WBwaarde <- 
  municipalBoundaries %>%
  left_join(WBwaarde, by=c(statcode="WijkenEnBuurten"))

WBpop <- 
  municipalBoundaries %>%
  left_join(WBpop, by=c(statcode="WijkenEnBuurten"))

WBrent <- 
  municipalBoundaries %>%
  left_join(WBrent, by=c(statcode="WijkenEnBuurten"))

#convert because the format is wrong
WBwaarde <- st_transform(WBwaarde, "+proj=longlat +datum=WGS84")
WBpop <- st_transform(WBpop, "+proj=longlat +datum=WGS84")
WBrent <- st_transform(WBrent, "+proj=longlat +datum=WGS84")


# Choose a color palette and assign it to the values
colorbins1 <- colorBin("YlOrRd", domain = WBwaarde$WOZ, 5, pretty = TRUE)
colorbins2 <- colorBin("Blues", domain = WBpop$population, 9, pretty = TRUE)
colorbins3 <- colorBin("Greens", domain = WBrent$value, 5, pretty = TRUE)


# Create HTML labels for tooltip
tooltip1 <- sprintf("<strong>%s</strong><br/> Average housing value: %.1i"
                    ,WBwaarde$statnaam
                    ,WBwaarde$WOZ
) %>% lapply(htmltools::HTML)

tooltip2 <- sprintf("<strong>%s</strong><br/> Population: %.1i"
                    ,WBpop$statnaam
                    ,WBpop$population
) %>% lapply(htmltools::HTML)

tooltip3 <- sprintf("<strong>%s</strong><br/> Rental property percent: %.1f%%"
                    ,WBrent$statnaam
                    ,WBrent$value
) %>% lapply(htmltools::HTML)




#### interactive bars inschrijvingen/registrations ####




#plot
server <- shinyServer(function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      
      addPolygons(data = universities_from_shapefile, label = universities_from_shapefile$name, group = 'Universities') %>%
      addPolygons(data = MultiP_shape, label = MultiP_shape$name, group = 'Universities') %>%
      
      addPolygons(data = WBwaarde, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                  fillColor = ~colorbins1(WBwaarde$WOZ),
                  highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip1, group = 'WOZ') %>%
      
      addPolygons(data = WBpop, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                  fillColor = ~colorbins2(WBpop$population),
                  highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip2, group = 'Population') %>%
      
      addPolygons(data = WBrent, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                  fillColor = ~colorbins3(WBrent$value),
                  highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip3, group = 'Rental properties (%)') %>%
      
      addCircleMarkers(data=amenities$osm_points, group = 'Universities',
                       label=str_trunc(as.character(amenities$osm_points$name),100)) %>%
      
      addMarkers(labelOptions = labelOptions(noHide = F), lng = HO_locations$long, lat = HO_locations$lat,
                 clusterOptions = markerClusterOptions(maxClusterRadius = 9), label = HO_locations$INSTELLINGSNAAM, group="Official University address") %>%
      
      addLayersControl(overlayGroups = c("WOZ", "Population", "Rental properties (%)"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Population", "Rental properties (%)")) %>%
      AddSearchButton(group = 'Official University address',position = "topleft", zoom = 15)
    
    
  })
  
  
  output$table = DT::renderDataTable({
    data4 %>% 
      group_by(OPLEIDINGSNAAM.ACTUEEL, INSTELLINGSNAAM.ACTUEEL, HO.type, language, CROHO.ONDERDEEL, GEMEENTENAAM, TYPE.HOGER.ONDERWIJS) %>% 
      summarise_at(vars(Registered), funs(median)) %>%
      filter(HO.type %in% input$bar3) %>%
      filter(language %in% input$bar4) %>%
      filter(CROHO.ONDERDEEL %in% input$bar2) %>%
      filter(GEMEENTENAAM %in% input$bar5) %>%
      filter(TYPE.HOGER.ONDERWIJS %in% input$bar6)
  })
  output$bar <- renderPlotly({ 
    p <- data4 %>% 
      group_by(INSTELLINGSNAAM.ACTUEEL, Year, GESLACHT, HO.type, language, CROHO.ONDERDEEL, GEMEENTENAAM, TYPE.HOGER.ONDERWIJS) %>% 
      summarise_at(vars(Registered), funs(sum)) %>%
      filter(Year == input$bar1) %>%
      filter(HO.type %in% input$bar3) %>%
      filter(language %in% input$bar4) %>%
      filter(CROHO.ONDERDEEL %in% input$bar2) %>%
      filter(GEMEENTENAAM %in% input$bar5) %>%
      filter(TYPE.HOGER.ONDERWIJS %in% input$bar6) %>%
      ggplot() +
      geom_col(aes(INSTELLINGSNAAM.ACTUEEL, Registered, fill = GESLACHT), position = "dodge")
    ggplotly(p) 
    
    
  })
    
  
})


