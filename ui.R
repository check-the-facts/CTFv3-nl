

#Version 2.1



ui <- dashboardPage(
  skin = "black",
  title = "Check-the-Facts: Hoger Onderwijs",
  
  dashboardHeader(
    title = span(img(src = "check the facts logo.svg", height = 35), "Check-The-Facts: Hoger Onderwijs"),
    titleWidth = 600,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL,
      notificationItem(
        text = (help$text[1])),
      notificationItem(
        text = (help$text[2]))
    ),
    tags$li(
      a(
        strong("Meer Informatie over Check-the-Facts"),
        height = 40,
        href = 'https://forum.datacommunities.nl/t/prototype-onderwijsdata-dashboard-voor-studiekiezers-en-analisten-en/132',
        title = ""
       
      ),
      class = "dropdown"
    )),
  
  dashboardSidebar(
    width = 350,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    menuItem(
      "Download Dataset",
      tabName = "download",
      icon = icon("download"),
      textInput(
        inputId = "filename",
        placeholder = "Name download file",
        label = ""
      ),
      div(
        downloadButton(
          outputId = "downloadData",
          label = "dataset",
          icon = icon("download"),
          style = "color: black; margin-left: 15px; margin-bottom: 5px;"
        )
      )
   ),
    br(),
    selectizeInput("crohoInput", label = h5("Selecteer studierichting"),
                   choices = sort(unique(data4$CROHO.ONDERDEEL)), selected = sort(unique(data4$CROHO.ONDERDEEL)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("wohboInput", label = h5("Selecteer WO/HBO"),
                   br(),
                   choices = sort(unique(data4$HO.type)), selected = sort(unique(data4$HO.type)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("TaalInput", label = h5("Selecteer taal"),
                   choices = sort(unique(data4$Taal)), selected = sort(unique(data4$Taal)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    pickerInput("locationInput", h5("Selecteer locatie"), choices=sort(unique(data4$GEMEENTENAAM.x)), multiple = TRUE,
                selected = sort(unique(data4$GEMEENTENAAM.x)),
                options = list(`actions-box` = TRUE, size = 8, `live-search`= TRUE)),
    selectizeInput("levelInput", label = h5("Selecteer studieniveau"),
                   choices = sort(unique(data4$TYPE.HOGER.ONDERWIJS)), selected = sort(unique(data4$TYPE.HOGER.ONDERWIJS)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("fullpartInput", label = h5("Selecteer opleidingsvorm"),
                   choices = sort(unique(data4$OPLEIDINGSVORM)), selected = sort(unique(data4$OPLEIDINGSVORM)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    pickerInput("instituteInput", h5("Selecteer instelling"), choices=sort(unique(data4$INSTELLINGSNAAM)), multiple = TRUE,
                selected = sort(unique(data4$INSTELLINGSNAAM)),
                options = list(`actions-box` = TRUE, size = 8, `live-search`= TRUE)),
    
    br() ),
  
  dashboardBody(
   
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "CTF_style.css")
    ),

    useShinyjs(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
          bsButton("Opleidingen",
                   label = "Opleidingen",
                   icon = icon("user-graduate"),
                   style = "primary"),
          bsButton("regionalstats",
                   label = "Regionale Statistieken",
                   icon = icon("globe-europe"),
                   style = "primary"),
          bsButton("analytics",
                   label = "Analyse",
                   icon = icon("chart-line"),
                   style = "primary"),
          bsButton("sources",
                   label = "Databronnen",
                   icon = icon("table"),
                   style = "primary")
      )
    ), 
    br(),
    fluidRow(id = "Opleidingen_panel", 
             tabBox(title = "Opleiding inschrijvingen", id = "tabsetinsch", width = 6, height = 650,
                    tabPanel("Overzicht", textOutput("selected_var_stream"), streamgraphOutput("ins_stream")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ), 
                             selectInput("streamin",
                                         label = "Kies optie te tonen",
                                         choices = c("INSTELLINGSNAAM", "OPLEIDINGSNAAM.ACTUEEL","CROHO.ONDERDEEL", "GESLACHT"),
                                         selected = "INSTELLINGSNAAM",
                                         width = "300")),
                    tabPanel("Inschrijvingen per jaar", textOutput("selected_var_bar"), plotlyOutput("bar_studies")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ),
                             fluidRow(
                               column(width = 6, sliderInput(inputId = "JaarInput",label = "Selecteer Jaar", min = min(data4$Jaar),max =  max(data4$Jaar),value = max(data4$Jaar), step = 1, sep = ""
                             )),
                               column(width = 6, selectInput("barin",
                                                            label = "Kies optie te tonen",
                                                            choices = c("INSTELLINGSNAAM", "OPLEIDINGSNAAM.ACTUEEL","CROHO.ONDERDEEL", "GESLACHT"),
                                                            selected = "INSTELLINGSNAAM",
                                                            width = "300"))
                             
                             )),
                  
                    h6("Als de grafiek niet begrijpelijk is, probeer de gegevens dan te filteren met behulp van de filters in de zijbalk!")
                    
                    
             ), 
             tabBox(title = "Locaties", id = "tabsetmap", width = 6, height = 650,
                    tabPanel("Instellingen adressen", leafletOutput("map1", height = 550)%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("Locaties per stad", pickerInput("map2input", h5("Selecteer Locatie"), choices=sort(unique(data4$GEMEENTENAAM.x)), multiple = FALSE,
                                                             selected = "Eindhoven"),leafletOutput("map2", height = 450)%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ))
             ), br(),
             
             
             
             tabBox(title = "Opleidingen",id = "tabsetstud", width = 12,
                    tabPanel("Opleidingen tabel", DT::dataTableOutput("studies_table")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("Doorstroommatrix", "Gebruik de filters in de zijbalk om de matrix weer te geven", DT::dataTableOutput("matrix")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ))), br()
             
             
             
             
    ),
    
    fluidRow(id = "RegionalStats_panel", br(), 
             tabBox(title = "Huisvesting", id = "tabsethouse", width = 6,
                    tabPanel("WOZ", leafletOutput("mapwoz")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("bevolking", leafletOutput("mappop")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("percentage huurwoningen", leafletOutput("maprental")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ))),
             tabBox(title = "Arbeidsmarkt", id = "tabsetxyz", width = 6)),
    
    fluidRow(id = "Analytics_panel", br(),
             tabBox(title = "Wat was het effect van het leenstelsel dat in 2015 is ingevoerd op studenten?", id = "tabsetLoan2015", width = 6,
                    tabPanel("Deel 1", img(src = "2015inschrijv.jpeg", height = 300), br(),
                             "Uit deze plot zien we dat het aantal inschrijvingen van studenten sinds 2015 zelfs is toegenomen. Dit betekent echter niet noodzakelijk dat dit het gevolg is van de verandering in het leenstelsel!
                             Dit kan ook komen door een algemene trend in het onderwijs in Nederland. Kijk op het volgende tabblad om een ??????voortzetting van dit onderzoek te zien."),
                    tabPanel("Deel 2"))),
    
    fluidRow(id = "Sources_panel", br(),
               box(title = "Databronnen", "Deze tabel toont de originele gebruikte bronnen. Deze zijn allemaal openlijk beschikbaar (open data).",
                 br(), width = 12, tableOutput("source_table")
             ))
    
    ))


        
  

 




