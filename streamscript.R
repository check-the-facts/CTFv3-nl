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
  group_by(INSTELLINGSNAAM.ACTUEEL, Year, GESLACHT, HO.type, language, CROHO.ONDERDEEL, GEMEENTENAAM, TYPE.HOGER.ONDERWIJS) %>% 
  summarise_at(vars(Registered), funs(sum)) %>%
  filter(HO.type %in% input$bar3) %>%
  filter(language %in% input$bar4) %>%
  filter(CROHO.ONDERDEEL %in% input$bar2) %>%
  filter(GEMEENTENAAM %in% input$bar5) %>%
  filter(TYPE.HOGER.ONDERWIJS %in% input$bar6) %>%
  streamgraph()
  geom_col(aes(INSTELLINGSNAAM.ACTUEEL, Registered, fill = GESLACHT), position = "dodge")
  tally(wt=n) %>%
  streamgraph("name", "n", "year", offset="zero", interpolate="linear") %>%
  sg_legend(show=TRUE, label="DDSec names: ")


streamgraphOutput(outputId, width = "100%", height = "400px")