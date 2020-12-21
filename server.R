

#Version 2



server <- function(input, output, session) {
  
  
  
  ####### UI - GENERAL #########
  
  
  #show intro modal
  observeEvent("", {
    showModal(modalDialog(
      includeHTML("intro_text.html"),
      easyClose = TRUE,
      footer = tagList(
        actionButton(inputId = "intro", label = "Close", icon = icon("window-close"))
      )
    ))
  })
  
  observeEvent(input$intro,{
    removeModal()
  })
  
  
  # use action buttons as tab selectors
  update_all <- function(x) {
    updateSelectInput(session, "tab",
                      choices = c("", "Study Programs", "Regional Stats", "Analytics", "Sources"),
                      label = "",
                      selected = x
    )
  }

  observeEvent(input$studyprograms, {
    update_all("Study Programs")
  })
  observeEvent(input$regionalstats, {
    update_all("Regional Stats")
  })
  observeEvent(input$analytics, {
    update_all("Analytics")
  })
  observeEvent(input$sources, {
    update_all("Sources")
  })

  
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    shinyjs::show("Studyprograms_panel")
    shinyjs::hide("RegionalStats_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  }, once = TRUE)
  
  observeEvent(input$studyprograms, {
    shinyjs::show("Studyprograms_panel")
    shinyjs::hide("RegionalStats_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$regionalstats, {
    shinyjs::show("RegionalStats_panel")
    shinyjs::hide("Studyprograms_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$analytics, {
    shinyjs::show("Analytics_panel")
    shinyjs::hide("Studyprograms_panel")
    shinyjs::hide("RegionalStats_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$sources, {
    shinyjs::show("Sources_panel")
    shinyjs::hide("Studyprograms_panel")
    shinyjs::hide("RegionalStats_panel")
    shinyjs::hide("Analytics_panel")
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "Study Programs", style = {
      if (x == "Study Programs") {
        paste("warning")
      } else {
        paste("primary")
      }
    })
    updateButton(session, "Regional Stats", style = {
      if (x == "Regional Stats") {
        paste("warning")
      } else {
        paste("primary")
      }
    })
    updateButton(session, "Analytics", style = {
      if (x == "Analytics") {
        paste("warning")
      } else {
        paste("primary")
      }
    })
    updateButton(session, "Sources", style = {
      if (x == "Sources") {
        paste("warning")
      } else {
        paste("primary")
      }
    })
  })
  
  

  
  # STUDY PROGRAMS  ----------------------------------------------------------

  
  output$studies_table = DT::renderDataTable({

      data4 %>%
        group_by(OPLEIDINGSNAAM.ACTUEEL, INSTELLINGSNAAM, HO.type, language, CROHO.ONDERDEEL, GEMEENTENAAM.x, TYPE.HOGER.ONDERWIJS, OPLEIDINGSVORM) %>%
        summarise_at(vars(Registered), list(mean = mean, median = median)) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(language %in% input$languageInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        datatable(options=list(lengthMenu = c(5, 30, 50), pageLength = 5, columnDefs = list(list(visible=FALSE, targets=9)), style = "font-size: 70%; width: 60%"))
  

  
      
    })
  
 
  # If conditions determining which plot should be used
  
  output$ins_stream <- renderStreamgraph({
    
    if(input$streamin == 'INSTELLINGSNAAM'){
      data4 %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(language %in% input$languageInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(INSTELLINGSNAAM, Year, Registered) %>%
        group_by(INSTELLINGSNAAM, Year) %>%
        summarise_all(funs(sum)) %>%
        streamgraph(key="INSTELLINGSNAAM", value="Registered", date="Year", height="300px", width="1000px") %>%
        sg_axis_x(1)
      
      } else if(input$streamin == 'CROHO.ONDERDEEL'){
        data4 %>%
          filter(HO.type %in% input$wohboInput) %>%
          filter(language %in% input$languageInput) %>%
          filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
          filter(GEMEENTENAAM.x %in% input$locationInput) %>%
          filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
          filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
          filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
          select(CROHO.ONDERDEEL, Year, Registered) %>%
          group_by(CROHO.ONDERDEEL, Year) %>%
          summarise_all(funs(sum)) %>%
          streamgraph(key="CROHO.ONDERDEEL", value="Registered", date="Year", height="300px", width="1000px") %>%
          sg_axis_x(1)
        
        } else if(input$streamin == 'GESLACHT'){
          data4 %>%
            filter(HO.type %in% input$wohboInput) %>%
            filter(language %in% input$languageInput) %>%
            filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
            filter(GEMEENTENAAM.x %in% input$locationInput) %>%
            filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
            filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
            filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
            select(GESLACHT, Year, Registered) %>%
            group_by(GESLACHT, Year) %>%
            summarise_all(funs(sum)) %>%
            streamgraph(key="GESLACHT", value="Registered", date="Year", height="300px", width="1000px") %>%
            sg_axis_x(1)
           
          }
    
  


  })


  
  output$bar_studies <- renderPlotly({ 
    if(input$streamin == 'INSTELLINGSNAAM'){
      p <- data4 %>% 
        filter(Year == input$yearInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(language %in% input$languageInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(INSTELLINGSNAAM, Registered, GESLACHT) %>%
        group_by(INSTELLINGSNAAM, GESLACHT) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(INSTELLINGSNAAM, Registered, fill = GESLACHT), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
      
    } else if(input$streamin == 'CROHO.ONDERDEEL'){
      p <- data4 %>% 
        filter(Year == input$yearInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(language %in% input$languageInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(CROHO.ONDERDEEL, Registered, GESLACHT) %>%
        group_by(CROHO.ONDERDEEL, GESLACHT) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(CROHO.ONDERDEEL, Registered, fill = GESLACHT), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
      
    } else if(input$streamin == "GESLACHT"){
      p <- data4 %>% 
        filter(Year == input$yearInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(language %in% input$languageInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(Registered, GESLACHT) %>%
        group_by(GESLACHT) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(GESLACHT, Registered), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
    }
    
    
    
  })

  output$map1 <- renderLeaflet({
    leaflet() %>%
          setView(mean(HO_locations$long), mean(HO_locations$lat), zoom = 7) %>%
          addTiles() %>%
          addProviderTiles("Esri.WorldGrayCanvas", options = providerTileOptions(noWrap = TRUE)) %>%
          enableMeasurePath() %>%
          addMarkers(labelOptions = labelOptions(noHide = F), lng = HO_locations$long, lat = HO_locations$lat, clusterOptions = markerClusterOptions(maxClusterRadius = 9), label = HO_locations$INSTELLINGSNAAM, group="Official University address") %>%
          AddSearchButton(group = 'Official University address',position = "topleft", zoom = 15) })
  

  
  ############ MAP 2 ###############
 
  
  output$map2 <- renderLeaflet({
    leaflet() %>% 
      setView(lng = 5.476008, lat = 51.450782, zoom = 14) %>%
      addTiles() %>%
      addProviderTiles("Esri.WorldGrayCanvas") %>%
      addPolygons(data = UniversityPolygonsShapefile, color= 'red',label = UniversityPolygonsShapefile$name, group = 'Universities') %>%
      addCircleMarkers(data=LibraryPoints, group = 'Libraries', color = 'green',label=str_trunc(as.character(LibraryPoints$name),100), radius = 3) %>%
      addCircleMarkers(data=SupermarketPoints, group = 'Supermarkets',color = 'orange',label=str_trunc(as.character(SupermarketPoints$name),100), radius = 3) %>%
      addCircleMarkers(data=BusPoints, group = 'Busstops',color = 'blue',label=str_trunc(as.character(BusPoints$name),100), radius = 3) %>%
      addLegend(pal = pal, values = c("Bus","Library", "Supermarket", "University"),opacity = 0.5)
   
    
    
  })
  
  
  # map1_data_react <- reactive({
  #  # shiny::validate(shiny::need(nrow(map1_data_react) > 0, "No data"))
  #   data4 %>%
  #     filter(HO.type %in% input$wohboInput) %>%
  #     filter(language %in% input$languageInput) %>%
  #     filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
  #     filter(GEMEENTENAAM.x %in% input$locationInput) %>%
  #     filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) 
  #     filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
  #     filter(INSTELLINGSNAAM %in% input$instituteInput) 
  # })
  # ## respond to the filtered data
  # observe({
  #   leafletProxy(mapId = "map1", data = map1_data_react()) %>%
  #     clearMarkers() %>%   ## clear previous markers
  #     addMarkers(label ='INSTELLINGSNAAM', popup = 'INTERNETADRES')
  # })
  # 
  
  
  
# react_matrix = reactive({
#   data4 %>%
#     select(OPLEIDINGSNAAM.ACTUEEL) %>%
#     filter(HO.type %in% input$wohboInput) %>%
#     filter(language %in% input$languageInput) %>%
#     filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
#     filter(GEMEENTENAAM.x %in% input$locationInput) %>%
#     filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
#     filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
#     filter(INSTELLINGSNAAM %in% input$instituteInput)
#   })


 # output$matrix <- renderDataTable({
 #   DT::datatable({})
#  #  
#   
# })
  


  
    
    
## lapply(labs, htmltools::HTML)
  




  # SOURCES -------------------------------------------------------------
  

  output$source_table <- renderTable(sources, escape=FALSE)

  
  # BOX REGIONAL STATS - box 1 -------------------------------------------------------------------
 output$mapwoz <- renderLeaflet({
   leaflet() %>%
     addTiles() %>%
     addProviderTiles("Esri.WorldGrayCanvas") %>%

     addPolygons(data = WBwaarde, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                 fillColor = ~colorbins1(WBwaarde$WOZ),
                 highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip1, group = 'WOZ')




 })
  

   
 output$mappop <- renderLeaflet({
   leaflet() %>%
     addTiles() %>%
     addProviderTiles("Esri.WorldGrayCanvas") %>%

     addPolygons(data = WBpop, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                 fillColor = ~colorbins2(WBpop$population),
                 highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip2, group = 'Population')


     

 })
 
  
 output$maprental <- renderLeaflet({
   leaflet() %>%
     addTiles() %>%
     addProviderTiles("Esri.WorldGrayCanvas") %>%

     addPolygons(data = WBrent, weight = 1, smoothFactor = 0.5, opacity = 0.6, fillOpacity = 0.6,
                 fillColor = ~colorbins3(WBrent$value),
                 highlightOptions = highlightOptions(weight = 2, color = "red", fillOpacity = 0.3), label = tooltip3, group = 'Rental properties (%)')

    


 })
  
  # BOX REGIONAL STATS - box2 --------------------------------------------------------------
  
  
  # BOX REGIONAL STATS - box 3 --------------------------------------------------------------
  
  
  
  # BOX REGIONAL STATS - box 4 --------------------------------------------------------------
  
  
  # BOX ANALYTICS - 1  ------------------------------------------------------------------
  


   
   

  
 
  
  # DOWNLOAD ----------------------------------------------------------------
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$filename, "_data4_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write_csv(data4, file)
    }
  )
  
 
  
  download_box <- function(exportname, plot) {
    downloadHandler(
      filename = function() {
        paste(exportname, Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        ggsave(file, plot = plot, device = "png", width = 8)
      }
    )
  }
  
  # output$down_studiestable <- download_box("studies_table", x)
  # output$down_studiesbar <- download_box("studies_bar", x)
  # output$down_studiesstream <- download_box("studies_stream", x)
  # output$down_map1 <- download_box("map1", x)
  # output$down_map2 <- download_box("map2", x)
  # output$down_map3 <- download_box("map3", x)
  # output$down_map4 <- download_box("map4", x)

}





