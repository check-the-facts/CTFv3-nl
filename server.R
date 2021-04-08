

#Version 3



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
                      choices = c("", "Opleidingen", "Arbeitsmarkt", "Analytics", "Sources"),
                      label = "",
                      selected = x
    )
  }

  observeEvent(input$Opleidingen, {
    update_all("Opleidingen")
  })
  observeEvent(input$arbeitsmarkt, {
    update_all("Arbeitsmarkt")
  })
  observeEvent(input$analytics, {
    update_all("Analytics")
  })
  observeEvent(input$sources, {
    update_all("Sources")
  })

  
  
  
  # DYNAMIC RENDER RULES ----------------------------------------------------
  
  observeEvent("", {
    shinyjs::show("Opleidingen_panel")
    shinyjs::hide("Arbeitsmarkt_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  }, once = TRUE)
  
  observeEvent(input$Opleidingen, {
    shinyjs::show("Opleidingen_panel")
    shinyjs::hide("Arbeitsmarkt_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$arbeitsmarkt, {
    shinyjs::show("Arbeitsmarkt_panel")
    shinyjs::hide("Opleidingen_panel")
    shinyjs::hide("Analytics_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$analytics, {
    shinyjs::show("Analytics_panel")
    shinyjs::hide("Opleidingen_panel")
    shinyjs::hide("Arbeitsmarkt_panel")
    shinyjs::hide("Sources_panel")
  })
  observeEvent(input$sources, {
    shinyjs::show("Sources_panel")
    shinyjs::hide("Opleidingen_panel")
    shinyjs::hide("Arbeitsmarkt_panel")
    shinyjs::hide("Analytics_panel")
  })
  
  
  # show active button with color
  
  observeEvent(input$tab, {
    x <- input$tab
    updateButton(session, "Opleidingen", style = {
      if (x == "Opleidingen") {
        paste("warning")
      } else {
        paste("primary")
      }
    })
    updateButton(session, "Arbeitsmarkt", style = {
      if (x == "Arbeitsmarkt") {
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
  
 

  
  # Opleidingen  ----------------------------------------------------------

  
  output$studies_table = DT::renderDataTable({

      data4 %>%
        group_by(OPLEIDINGSNAAM.ACTUEEL, INSTELLINGSNAAM, HO.type, Taal, CROHO.ONDERDEEL, GEMEENTENAAM.x, TYPE.HOGER.ONDERWIJS, OPLEIDINGSVORM) %>%
        summarise_at(vars(Registered), list(mean = mean, median = median)) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        datatable(extensions = c('Buttons','FixedColumns'), options=list(
          lengthMenu = c(5, 30, 50), 
          pageLength = 5, 
          columnDefs = list(list(visible=FALSE, targets=c(9,10))), 
          style = "font-size: 40%; width: 20%", 
          dom = 'Bfrtip', 
          scrollX = TRUE,
          fixedColumns = TRUE,
          buttons = c('csv', 'excel', 'pdf')
          ))
  
      
    })
  
 
  # If conditions determining which plot should be used
  
  output$ins_stream <- renderStreamgraph({
    
    if(input$streamin == 'INSTELLINGSNAAM'){
      data4 %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(INSTELLINGSNAAM, Jaar, Registered) %>%
        group_by(INSTELLINGSNAAM, Jaar) %>%
        summarise_all(funs(sum)) %>%
        streamgraph(key="INSTELLINGSNAAM", value="Registered", date="Jaar", height="300px", width="1000px" ) %>%
        sg_axis_x(1)
      
      } else if(input$streamin == 'CROHO.ONDERDEEL'){
        data4 %>%
          filter(HO.type %in% input$wohboInput) %>%
          filter(Taal %in% input$TaalInput) %>%
          filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
          filter(GEMEENTENAAM.x %in% input$locationInput) %>%
          filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
          filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
          filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
          select(CROHO.ONDERDEEL, Jaar, Registered) %>%
          group_by(CROHO.ONDERDEEL, Jaar) %>%
          summarise_all(funs(sum)) %>%
          streamgraph(key="CROHO.ONDERDEEL", value="Registered", date="Jaar", height="300px", width="1000px") %>%
          sg_axis_x(1)
        
      } else if(input$streamin == 'OPLEIDINGSNAAM.ACTUEEL'){
        data4 %>%
          filter(HO.type %in% input$wohboInput) %>%
          filter(Taal %in% input$TaalInput) %>%
          filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
          filter(GEMEENTENAAM.x %in% input$locationInput) %>%
          filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
          filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
          filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
          select(OPLEIDINGSNAAM.ACTUEEL, Jaar, Registered) %>%
          group_by(OPLEIDINGSNAAM.ACTUEEL, Jaar) %>%
          summarise_all(funs(sum)) %>%
          streamgraph(key="OPLEIDINGSNAAM.ACTUEEL", value="Registered", date="Jaar", height="300px", width="1000px") %>%
          sg_axis_x(1)
        
        } else if(input$streamin == 'GESLACHT'){
          data4 %>%
            filter(HO.type %in% input$wohboInput) %>%
            filter(Taal %in% input$TaalInput) %>%
            filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
            filter(GEMEENTENAAM.x %in% input$locationInput) %>%
            filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
            filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
            filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
            select(GESLACHT, Jaar, Registered) %>%
            group_by(GESLACHT, Jaar) %>%
            summarise_all(funs(sum)) %>%
            streamgraph(key="GESLACHT", value="Registered", date="Jaar", height="300px", width="1000px") %>%
            sg_axis_x(1) 
          
           
          }
    
  


  })
  
  output$selected_var_stream <- renderText({ 
    paste("Het aantal inschrijvingen van 2015 tot 2019 per ", input$streamin)
  })
  
  output$selected_var_bar <- renderText({
    paste("Het aantal inschrijvingen per",input$barin ,"in", input$JaarInput)
  })


  
  output$bar_studies <- renderPlotly({ 
    if(input$barin == 'INSTELLINGSNAAM'){
      p <- data4 %>% 
        filter(Jaar == input$JaarInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
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
      
    } else if(input$barin == 'CROHO.ONDERDEEL'){
      p <- data4 %>% 
        filter(Jaar == input$JaarInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
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
      
    } else if(input$barin == 'OPLEIDINGSNAAM.ACTUEEL'){
      p <- data4 %>% 
        filter(Jaar == input$JaarInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
        filter(CROHO.ONDERDEEL %in% input$crohoInput) %>%
        filter(GEMEENTENAAM.x %in% input$locationInput) %>%
        filter(TYPE.HOGER.ONDERWIJS %in% input$levelInput) %>%
        filter(OPLEIDINGSVORM %in% input$fullpartInput)  %>%
        filter(INSTELLINGSNAAM %in% input$instituteInput) %>%
        select(OPLEIDINGSNAAM.ACTUEEL, Registered, GESLACHT) %>%
        group_by(OPLEIDINGSNAAM.ACTUEEL, GESLACHT) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(OPLEIDINGSNAAM.ACTUEEL, Registered, fill = GESLACHT), position = "dodge") +
        coord_flip()
      
      
      ggplotly(p)
      
     
    } else if(input$barin == "GESLACHT"){
      p <- data4 %>% 
        filter(Jaar == input$JaarInput) %>%
        filter(HO.type %in% input$wohboInput) %>%
        filter(Taal %in% input$TaalInput) %>%
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
################# MAP 1 ####################
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
  

  
 
  # SOURCES -------------------------------------------------------------
  

  output$source_table <- renderTable(sources, escape=FALSE)
  

  
  # BOX Arbeitsmarkt -------------------------------------------------------------------
  
  output$selected_var_job <- renderText({
    paste("Het aantal uitstromers per",input$jobin ,"in", input$jobyearInput)
  })
  
  output$jobs1 <- renderPlotly({
    
    if(input$jobin == 'Perioden'){
      p <- L_market %>% 
        select(Perioden,UitstromersHo_1, Geslacht) %>%
        group_by(Perioden, Geslacht) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(Perioden, UitstromersHo_1, fill = Geslacht), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
      
    } else if(input$jobin == 'Studierichting'){
      p <- L_market %>% 
        filter(Perioden == input$jobyearInput) %>%
        select(Studierichting, UitstromersHo_1, Geslacht) %>%
        group_by(Studierichting, Geslacht) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(Studierichting, UitstromersHo_1, fill = Geslacht), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
      
    } else if(input$jobin == 'UitstromersHoMetEnZonderDiploma'){
      p <- L_market %>% 
        filter(Perioden == input$jobyearInput) %>%
        select(UitstromersHoMetEnZonderDiploma, Geslacht, UitstromersHo_1) %>%
        group_by(UitstromersHoMetEnZonderDiploma, Geslacht) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(UitstromersHoMetEnZonderDiploma, UitstromersHo_1, fill = Geslacht), position = "dodge") +
        coord_flip()
      
      
      ggplotly(p)
      
      
    } else if(input$jobin == "Arbeidsmarktpositie"){
      p <- L_market %>% 
        filter(Perioden == input$jobyearInput) %>%
        select(Arbeidsmarktpositie, Geslacht, UitstromersHo_1) %>%
        group_by(Arbeidsmarktpositie, Geslacht) %>% 
        summarise_all(funs(sum)) %>%
        ggplot() +
        geom_col(aes(Arbeidsmarktpositie, UitstromersHo_1, fill = Geslacht), position = "dodge") +
        coord_flip()
      
      ggplotly(p)
    }
    
    
  })

  
  
 
  
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
  


}





