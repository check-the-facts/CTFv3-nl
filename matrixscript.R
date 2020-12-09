
library(shinyMatrix)
matrixInput("matrix1", class = "numeric")

## editable rownames
matrixInput("matrix",
            rows = list(
              names = TRUE,
              editableNames = TRUE),
            data = matrix(c(data4$OPLEIDINGSNAAM.ACTUEEL, data4$INSTELLINGSNAAM), 0)
)

 output$matrix = renderDataTable({
    data4 %>%
      group_by(OPLEIDINGSNAAM.ACTUEEL, INSTELLINGSNAAM.ACTUEEL, HO.type, language, CROHO.ONDERDEEL, GEMEENTENAAM, TYPE.HOGER.ONDERWIJS) %>%
      summarise_at(vars(Registered), funs(median)) %>%
      filter(HO.type %in% input$bar3) %>%
      filter(language %in% input$bar4) %>%
      filter(CROHO.ONDERDEEL %in% input$bar2) %>%
      filter(GEMEENTENAAM %in% input$bar5) %>%
      filter(TYPE.HOGER.ONDERWIJS %in% input$bar6)
  })

n = matrix(data= 0, nrow = length(data4$OPLEIDINGSNAAM.ACTUEEL), ncol = length(data4$OPLEIDINGSNAAM.ACTUEEL), dimnames = list(data4$OPLEIDINGSNAAM.ACTUEEL))

m <- matrix(0, length(), 2, dimnames = list())

ui <- fluidPage(
  titlePanel("shinyMatrix: Simple App"),
  mainPanel(
    tags$h4("Data"),
    matrixInput(
      "sample",
      value = c(data4$OPLEIDINGSNAAM.ACTUEEL, data4$INSTELLINGSNAAM),
      rows = list(
        extend = TRUE
      ),
      cols = list(
        names = TRUE
      )
    )
  ),
  mainPanel(
    width = 6,
    plotOutput("scatter")
  )
)

server <- function(input, output, session) {
  output$scatter <- renderPlot({
    
    plot(input$sample, col = "red", main = "Scatterplot")
  })
}

shinyApp(ui, server)
