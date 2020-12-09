# Install dependencies 

source('dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

##############DATA TRANSFORMATION##############

data4 <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/04-inschrijvingen-wo-2019.csv", sep= ',')
data4hbo <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/04-inschrijvingen-hbo-2019.csv", sep = ';')
HOadr <- read.csv("C:/Users/jasmi/Desktop/BEP/Data/01-instellingen-hbo-en-wo.csv", sep = ';')


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

data4 <- merge(data4,HO_locations,by="INSTELLINGSNAAM")






# antimicrobial count for select input in ui.R

# ab <- antimicrobials %>%
#   filter(!is.na(ab_type)) %>%
#   group_by(ab_type) %>%
#   summarise(n = n()) %>%
#   arrange(desc(n)) %>%
#   filter(!is.na(ab_type)) %>%
#   distinct()
# 
# ab_groups <- antimicrobials %>%
#   filter(!is.na(ab_group)) %>%
#   select(ab_group) %>%
#   arrange(ab_group) %>%
#   distinct()
# 
# 
# update_ab <- antimicrobials %>%
#   select(ab_type, ab_group) %>%
#   distinct(.keep_all = TRUE)


# HELP Information ---------------------------------------------------------------

steps <- read_csv2("help.csv")

# FLUID DESIGN FUNCTION ---------------------------------------------------

fluid_design <- function(id, w, x, y, z) {
  fluidRow(
    div(
      id = id,
      column(
        width = 6,
        uiOutput(w),
        uiOutput(y)
      ),
      column(
        width = 6,
        uiOutput(x),
        uiOutput(z)
      )
    )
  )
}