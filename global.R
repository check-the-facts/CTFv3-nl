# Install dependencies 

source('dependencies.R')



##############DATA TRANSFORMATION##############

data4 <- read.csv("data/04-inschrijvingen-wo-2019.csv", sep= ',')
data4hbo <- read.csv("data/04-inschrijvingen-hbo-2019.csv", sep = ';')
HOadr <- read.csv("data/01-instellingen-hbo-en-wo.csv", sep = ';')



temp <- c("Tilburg University", "Universiteit van Amsterdam", "Erasmus University Rotterdam","Eindhoven University of Technology", " Delft University of Technology", "Leiden University", "Wageningen University", "Maastricht University", "University of Groningen", "University of Twente")

data <- data.frame(
  year=rep(seq(2015,2019) , each=54),
  name=rep(temp , 27),
  value=sample( seq(200,400,1) , 270, replace = T)
)


#####GET CBS DATA + PDOK boundaries#######
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
# 

#########DUO WO########

data4 <- pivot_longer(data4, cols = starts_with("X20"), values_to = "Registered", names_to = "Year", values_drop_na = TRUE) 
data4 %>% mutate_if(is.factor, as.character) -> data4
data4 <- mutate(data4, Year = as.integer(gsub("X", "", Year)))
data4$OPLEIDINGSNAAM.ACTUEEL <- as.character(data4$OPLEIDINGSNAAM.ACTUEEL)
data4$CROHO.ONDERDEEL <- as.character(data4$CROHO.ONDERDEEL)
data4$GEMEENTENAAM <- as.character(data4$GEMEENTENAAM)
data4$language <- detect_language(data4$OPLEIDINGSNAAM.ACTUEEL, plain_text = TRUE)
data4$language[is.na(data4$language) == TRUE] <- "en/nl"
data4$language[data4$language != 'en'] <- "en/nl"


########DUO HBO#########

data4hbo <- pivot_longer(data4hbo, cols = starts_with("X20"), values_to = "Registered", names_to = "Year", values_drop_na = TRUE) 
data4hbo %>% mutate_if(is.factor, as.character) -> data4hbo
data4hbo <- mutate(data4hbo, Year = as.integer(gsub("X", "", Year)))
data4hbo$OPLEIDINGSNAAM.ACTUEEL <- as.character(data4hbo$OPLEIDINGSNAAM.ACTUEEL)
data4hbo$CROHO.ONDERDEEL <- as.character(data4hbo$CROHO.ONDERDEEL)
data4hbo$GEMEENTENAAM <- as.character(data4hbo$GEMEENTENAAM)
data4hbo$language <- detect_language(data4hbo$OPLEIDINGSNAAM.ACTUEEL, plain_text = TRUE)
data4hbo$language[is.na(data4hbo$language) == TRUE] <- "en/nl"
data4hbo$language[data4hbo$language != 'en'] <- "en/nl"


####DUO UNIVERSITY ADRESSES####

HOadr$Full_Address<- paste(HOadr$STRAATNAAM, HOadr$HUISNUMMER.TOEVOEGING, HOadr$PLAATSNAAM, sep =" ")
HOadr$StreetN <- paste(HOadr$STRAATNAAM, HOadr$HUISNUMMER.TOEVOEGING, sep = " ")
HOadr$GEMEENTENAAM <- as.character(HOadr$GEMEENTENAAM)
HO_locations <- HOadr %>%
  geocode(street = StreetN, city = GEMEENTENAAM, country = 'Nederland', method = 'osm')


#####JOIN######
data4$HO.type <- 'WO'
data4hbo$HO.type <- 'HBO'
data4 <- rbind(data4, data4hbo)

#Rename column INSTELLINGSNAAM to join data
colnames(data4)[which(names(data4) == "INSTELLINGSNAAM.ACTUEEL")] <- "INSTELLINGSNAAM"

#Rename Universiteit Maastricht to Maastricht University 
HO_locations$INSTELLINGSNAAM[HO_locations$INSTELLINGSNAAM == "Universiteit Maastricht"] <- "Maastricht University"

data4 <- merge(data4,HO_locations,by="INSTELLINGSNAAM")


labs <- lapply(seq(nrow(data4)), function(i) {
  paste0( '<p>', data4[i, "INSTELLINGSNAAM"], '<p></p>', 
          data4[i, "Full_Address"], ', ', 
          data4[i, "GEMEENTENAAM.x"],'</p><p>', 
          data4[i, "INTERNETADRES"], '</a>' ) 
})


profiles <- data.frame(c("Cultuur en Maatschappij","Economie en Maatschappij ", "Natuur en Gezondheid", "Natuur en Techniek"), fix.empty.names = F)



# HELP + sources Information ---------------------------------------------------------------

help <- read_csv2("help.csv")
sources <- read.csv("Sources.csv")




################################# Open Street Map Data ###############################

BoundingBox <- osmdata::getbb("Eindhoven")
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

# ####NULL in this case####
# UniversityMultipolygons <- University$osm_multipolygons %>%
#   select_(osm_id, name) %>%
#   drop_na(name)
# UniversityMultipolygons %>%
#   write_sf('UniversityMultipolygons.shp', driver = 'ESRI Shapefile')
# UniversityMultipolygonsShaoefile <- read_sf('UniversityMultipolygons.shp')
# UniversityPoints <- University$osm_points %>%
#   select(osm_id, name) %>%
#   drop_na(name)


####Library####
LibraryPoints <- Library$osm_points %>%
  select(osm_id, name) %>%
  drop_na

# ###NULL in this case####
# LibraryPolygons <- Library$osm_polygons %>% 
#   select(osm_id, name) %>% 
#   drop_na(name)
# LibraryPolygons %>% 
#   write_sf('LibraryPolygons.shp', driver = 'ESRI Shapefile')
# LibraryPolygonsShapefile <- read_sf('LibraryPolygons.shp')
# LibraryMultipolygons <- Library$osm_multipolygons %>%
#   select_(osm_id, name) %>%
#   drop_na(name)
# LibraryMultipolygons %>%
#   write_sf('LibraryMultipolygons.shp', driver = 'ESRI Shapefile')
# LibraryMultipolygonsShaoefile <- read_sf('LibraryMultipolygons.shp')


######Supermarket######
SupermarketPoints <- Supermarket$osm_points %>%
  select(osm_id, name) %>%
  drop_na(name)

# ######NULL in this case#####
# SupermarketPolygons <- Supermarket$osm_polygons %>% 
#   select(osm_id, name) %>% 
#   drop_na(name)
# SuperMarketPolygons %>% 
#   write_sf('SupermarketPolygons.shp', driver = 'ESRI Shapefile')
# SupermarketPolygonsShapefile <- read_sf('SupermarketPolygons.shp')
# SupermarketMultipolygons <- Supermarket$osm_multipolygons %>%
#   select_(osm_id, name) %>%
#   drop_na(name)
# SupermarketMultipolygons %>%
#   write_sf('SupermarketMultipolygons.shp', driver = 'ESRI Shapefile')
# SupermarketMultipolygonsShaoefile <- read_sf('SupermarketMultipolygons.shp')


#####Bus#######
BusPoints <- Bus$osm_points %>%
  select(osm_id, name) %>%
  drop_na(name)

# ######NULL in this case####
# BusPolygons <- Bus$osm_polygons %>% 
#   select(osm_id, name) %>% 
#   drop_na(name)
# BusPolygons %>% 
#   write_sf('BusPolygons.shp', driver = 'ESRI Shapefile')
# BusPolygonsShapefile <- read_sf('BusPolygons.shp')
# BusMultipolygons <- Bus$osm_multipolygons %>%
#   select_(osm_id, name) %>%
#   drop_na(name)
# BusMultipolygons %>%
#   write_sf('BusMultipolygons.shp', driver = 'ESRI Shapefile')
# BusMultipolygonsShaoefile <- read_sf('BusMultipolygons.shp')

r = 0.009
pal <- colorFactor(c("blue", "green", "orange", "red"), domain = c("University", "Library", "Supermarket", "Bus"))




