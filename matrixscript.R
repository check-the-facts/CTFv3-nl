
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
     select(OPLEIDINGSNAAM.ACTUEEL) %>%
     filter(HO.type %in% input$wohboInput) %>%
     filter(language %in% input$languageInput) %>%
     filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
     filter(GEMEENTENAAM.x %in% input$locationInput) %>%
     filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
     filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
     filter(INSTELLINGSNAAM %in% input$instituteInput)
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
