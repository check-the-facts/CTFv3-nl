# Library
library(streamgraph)

# Create data:
data <- data.frame(
  year=rep(seq(1990,2016) , each=10),
  name=rep(letters[1:10] , 27),
  value=sample( seq(0,1,0.0001) , 270)
)

# Basic stream graph: just give the 3 arguments
pp <- streamgraph(data, key="name", value="value", date="year", height="300px", width="1000px")
pp 


data4 %>%
  group_by(INSTELLINGSNAAM, Year) %>% 
  streamgraph() %>%
  tally(vars(Registered), funs(sum)) %>%
  streamgraph(key = "INSTELLINGSNAAM", value = "Registered", date = "Year", offset="zero", interpolate="linear") %>%
  sg_legend(show=TRUE, label="Institutes: ")


streamgraphOutput(outputId, width = "100%", height = "400px")

plo <- data4 %>%
  tidyr::pivot_longer(-Year, names_to = ~INSTELLINGSNAAM, values_to = ~Registered)


temp <- c("Tilburg University", "Universiteit van Amsterdam", "Erasmus University Rotterdam","Eindhoven University of Technology", " Delft University of Technology", "Leiden University", "Wageningen University", "Maastricht University", "University of Groningen", "University of Twente")

data <- data.frame(
  year=rep(seq(2015,2019) , each=54),
  name=rep(temp , 27),
  value=sample( seq(200,400,1) , 270, replace = T)
)
data %>%
  group_by(name, year) %>%
  streamgraph(key="name", value="value", date="year", height="300px", width="1000px",interpolate="linear") %>%
  sg_legend(show=TRUE, label="Instelling ")
