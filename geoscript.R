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
library(dodgr)


##Example area##
BoundingBox <- getbb('Eindhoven')

#OSM all universities and public transport options

University <- opq(bbox = BoundingBox) %>%
  add_osm_feature(key ='amenity', value = 'university', value_exact = T) %>%
  osmdata_sf(quiet = FALSE) %>%
  unique_osmdata()

Library <- opq(bbox = BoundingBox) %>%
  add_osm_feature(key ='amenity', value = 'library', value_exact = T) %>%
  osmdata_sf(quiet = FALSE) %>%
  unique_osmdata()

Supermarket <- opq(bbox = BoundingBox) %>%
  add_osm_feature(key ='shop', value = 'supermarket', value_exact = T) %>%
  osmdata_sf(quiet = FALSE) %>%
  unique_osmdata()

Bus <- opq(bbox = BoundingBox) %>%
  add_osm_feature(key ='highway', value = 'bus_stop', value_exact = T) %>%
  osmdata_sf(quiet = FALSE) %>%
  unique_osmdata()


#####Combining all types of shapes#####
#####University#####

UniversityPolygons <- University$osm_polygons %>% 
  select(osm_id, name) %>% 
  drop_na(name)
UniversityPolygons %>% 
  write_sf('UniversityPolygons.shp', driver = 'ESRI Shapefile')
UniversityPolygonsShapefile <- read_sf('UniversityPolygons.shp')

####NULL in this case####
UniversityMultipolygons <- University$osm_multipolygons %>%
  select_(osm_id, name) %>%
  drop_na(name)
UniversityMultipolygons %>%
  write_sf('UniversityMultipolygons.shp', driver = 'ESRI Shapefile')
UniversityMultipolygonsShaoefile <- read_sf('UniversityMultipolygons.shp')
UniversityPoints <- University$osm_points %>%
  select(osm_id, name) %>%
  drop_na(name)


####Library####
LibraryPoints <- Library$osm_points %>%
  select(osm_id, name) %>%
  drop_na

###NULL in this case####
LibraryPolygons <- Library$osm_polygons %>% 
  select(osm_id, name) %>% 
  drop_na(name)
LibraryPolygons %>% 
  write_sf('LibraryPolygons.shp', driver = 'ESRI Shapefile')
LibraryPolygonsShapefile <- read_sf('LibraryPolygons.shp')
LibraryMultipolygons <- Library$osm_multipolygons %>%
  select_(osm_id, name) %>%
  drop_na(name)
LibraryMultipolygons %>%
  write_sf('LibraryMultipolygons.shp', driver = 'ESRI Shapefile')
LibraryMultipolygonsShaoefile <- read_sf('LibraryMultipolygons.shp')


######Supermarket######
SupermarketPoints <- Supermarket$osm_points %>%
  select(osm_id, name) %>%
  drop_na(name)

######NULL in this case#####
SupermarketPolygons <- Supermarket$osm_polygons %>% 
  select(osm_id, name) %>% 
  drop_na(name)
SuperMarketPolygons %>% 
  write_sf('SupermarketPolygons.shp', driver = 'ESRI Shapefile')
SupermarketPolygonsShapefile <- read_sf('SupermarketPolygons.shp')
SupermarketMultipolygons <- Supermarket$osm_multipolygons %>%
  select_(osm_id, name) %>%
  drop_na(name)
SupermarketMultipolygons %>%
  write_sf('SupermarketMultipolygons.shp', driver = 'ESRI Shapefile')
SupermarketMultipolygonsShaoefile <- read_sf('SupermarketMultipolygons.shp')


#####Bus#######
BusPoints <- Bus$osm_points %>%
  select(osm_id, name) %>%
  drop_na(name)

######NULL in this case####
BusPolygons <- Bus$osm_polygons %>% 
  select(osm_id, name) %>% 
  drop_na(name)
BusPolygons %>% 
  write_sf('BusPolygons.shp', driver = 'ESRI Shapefile')
BusPolygonsShapefile <- read_sf('BusPolygons.shp')
BusMultipolygons <- Bus$osm_multipolygons %>%
  select_(osm_id, name) %>%
  drop_na(name)
BusMultipolygons %>%
  write_sf('BusMultipolygons.shp', driver = 'ESRI Shapefile')
BusMultipolygonsShaoefile <- read_sf('BusMultipolygons.shp')



##Leaflet plot
r = 0.009
pal <- colorFactor(c("blue", "green", "orange", "red"), domain = c("University", "Library", "Supermarket", "Bus"))
leaflet() %>% 
  setView(lng = 5.476008, lat = 51.450782, zoom = 15) %>% 
  addTiles() %>%
  addProviderTiles("Esri.WorldGrayCanvas") %>%
  addPolygons(data = UniversityPolygonsShapefile, color= 'red',label = UniversityPolygonsShapefile$name, group = 'Universities') %>%
  addCircleMarkers(data=LibraryPoints, group = 'Libraries', color = 'green',label=str_trunc(as.character(LibraryPoints$name),100)) %>%
  addCircleMarkers(data=SupermarketPoints, group = 'Supermarkets',color = 'orange',label=str_trunc(as.character(SupermarketPoints$name),100)) %>%
  addCircleMarkers(data=BusPoints, group = 'Busstops',color = 'blue',label=str_trunc(as.character(BusPoints$name),100)) %>%
  addMarkers(5.476008, 51.450782) %>%
  addPolylines(lat=c(51.450782, 51.450782), lng=c(5.476008 - r, 5.476008 + r), color = 'black', opacity = T) %>%
  addLegend(pal = pal, values = c("Bus","Library", "Supermarket", "University"),opacity = 0.5)
  


####GGplot + plotly####
BusPoints <- BusPoints %>%
  mutate(lat = unlist(map(BusPoints$geometry,2)),
         long = unlist(map(BusPoints$geometry,1)))

LibraryPoints <- LibraryPoints %>%
  mutate(lat = unlist(map(LibraryPoints$geometry,2)),
         long = unlist(map(LibraryPoints$geometry,1)))

SupermarketPoints <- SupermarketPoints %>%
  mutate(lat = unlist(map(SupermarketPoints$geometry,2)),
         long = unlist(map(SupermarketPoints$geometry,1)))

UniversityPoints <- UniversityPolygons %>%
  mutate(lat = unlist(map(map(UniversityPolygons$geometry, 1),max)),
         long = unlist(map(map(UniversityPolygons$geometry, 1),min)))




    
p <- ggplot() +
  geom_point(data = BusPoints, aes(x=long, y=lat, color = 'Bus stop')) +
  geom_point(data = LibraryPoints, aes(x=long, y=lat, color = 'Library')) +
  geom_point(data = SupermarketPoints, aes(x=long, y=lat, color = 'Supermarket')) +
  geom_point(data = UniversityPoints, aes(x=long, y=lat, color = 'University')) +
  labs(title = "Bus stops in Eindhoven", x = "", y = "") +
  coord_quickmap()
ggplotly(p)



##Using OSMplotr instead?
BoundingBox <- getbb('Eindhoven')
xmin <- BoundingBox[1]
xmax <- BoundingBox[3]
ymin <- BoundingBox[2]
ymax <- BoundingBox[4]
BB <- c(xmin,ymin,xmax,ymax)
BB <- get_bbox(BB)


map <- osm_basemap(bbox = BB, bg = 'white')
map <- add_osm_objects(map,BusPoints, col = 'blue')
map <- add_osm_objects(map,UniversityPolygonsShapefile, col = 'red')
map <- add_osm_objects(map,SupermarketPoints, col = 'orange')
map <- add_osm_objects(map,LibraryPoints, col = 'green')
map <- add_axes (map)

print_osm_map(map)
