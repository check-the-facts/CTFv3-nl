

#Version 2.1



ui <- dashboardPage(
  skin = "black",
  title = "Check-the-Facts: Higher Education",
  
  dashboardHeader(
    title = span(img(src = "check the facts logo.svg", height = 35), "Check-The-Facts: Higher Education"),
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
        strong("More about Check The Facts"),
        height = 40,
        href = "https://github.com/jasminkareem/CheckTheFacts",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )),
  
  dashboardSidebar(
    width = 350,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    menuItem(
      "Download Data",
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
          label = "Save data",
          icon = icon("download"),
          style = "color: black; margin-left: 15px; margin-bottom: 5px;"
        )
      )
   ),
    br(),
    sliderInput(
      inputId = "yearInput",
      label = "Select year",
      min = min(data4$Year),
      max =  max(data4$Year),
      value = max(data4$Year),
      step = 1,
      sep = ""
    ),
    selectizeInput("crohoInput", label = h5("Select area of study"),
                   choices = sort(unique(data4$CROHO.ONDERDEEL)), selected = sort(unique(data4$CROHO.ONDERDEEL)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("wohboInput", label = h5("Select WO/HBO"),
                   br(),
                   choices = sort(unique(data4$HO.type)), selected = sort(unique(data4$HO.type)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("languageInput", label = h5("Select language of study"),
                   choices = sort(unique(data4$language)), selected = sort(unique(data4$language)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    pickerInput("locationInput", h5("Select Location"), choices=sort(unique(data4$GEMEENTENAAM.x)), multiple = TRUE,
                selected = sort(unique(data4$GEMEENTENAAM.x)),
                options = list(`actions-box` = TRUE, size = 8, `live-search`= TRUE)),
    selectizeInput("levelInput", label = h5("Select level of study"),
                   choices = sort(unique(data4$TYPE.HOGER.ONDERWIJS)), selected = sort(unique(data4$TYPE.HOGER.ONDERWIJS)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    selectizeInput("fullpartInput", label = h5("Select Full/Part-time"),
                   choices = sort(unique(data4$OPLEIDINGSVORM)), selected = sort(unique(data4$OPLEIDINGSVORM)), multiple = TRUE, 
                   options = list(plugins= list('remove_button'))),
    pickerInput("instituteInput", h5("Select Institute"), choices=sort(unique(data4$INSTELLINGSNAAM)), multiple = TRUE,
                selected = "Tilburg University",
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
          bsButton("studyprograms",
                   label = "Study Programs",
                   icon = icon("user-graduate"),
                   style = "primary"),
          bsButton("regionalstats",
                   label = "Regional Stats",
                   icon = icon("globe-europe"),
                   style = "primary"),
          bsButton("analytics",
                   label = "Analytics",
                   icon = icon("chart-line"),
                   style = "primary"),
          bsButton("sources",
                   label = "Sources",
                   icon = icon("table"),
                   style = "primary")
      )
    ), 
    br(),
    
    fluidRow(id = "RegionalStats_panel", br(), 
             tabBox(title = "Housing", id = "tabsethouse", width = 6,
                    tabPanel("WOZ", leafletOutput("mapwoz")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("*Student* population", leafletOutput("mappop")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("*Student* Residential Rental Property (%)", leafletOutput("maprental")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ))),
             tabBox(title = "...", id = "tabsetxyz", width = 6)),
    
    fluidRow(id = "Analytics_panel", br(),
             tabBox(title = "What was the effect of the loan system that was introduced in 2015 on students?", id = "tabsetLoan2015", width = 6,
                    tabPanel("Plot 1", plotlyOutput("inschr_beforeafter2015")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ), br(),
                             "From this plot we see that student write-ins have actually increased since 2015. However, this does not necessarily mean that this is a result of the change in loan system! 
                             This could also be because of an overall trend in education in the Netherlands. Check the next tab to see a continuation of this investigation."),
                    tabPanel("..."))),
    
    fluidRow(id = "Sources_panel", br(),
               box(title = "Sources", "This table shows the original sources used. These are all openly available (open data).",
                 br(), width = 12, tableOutput("source_table")
             )),
    
    fluidRow(id = "Studyprograms_panel", 
             tabBox(title = "Inschrijvingen", id = "tabsetinsch", width = 6, height = 525,
                    tabPanel("Trend", streamgraphOutput("ins_stream")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("per group", plotlyOutput("bar_studies")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    selectInput("streamin",
                                label = "Choose a variable to display",
                                choices = c("INSTELLINGSNAAM", "OPLEIDINGSNAAM.ACTUEEL", "GESLACHT", "GEMEENTENAAM.x"),
                                selected = "INSTELLINGSNAAM")
                    
                    
             ), 
             tabBox(title = "Map", id = "tabsetmap", width = 6, height = 550,
                    tabPanel("HO locations"), leafletOutput("map1", height = 450)%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )), 
             br(),
             
             tabBox(title = "Studies",id = "tabsetstud", width = 12,
                    tabPanel("Study Program Table", DT::dataTableOutput("studies_table")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 )),
                    tabPanel("Matrix", "doorstroommatrix", DT::dataTableOutput("matrix")%>%shinycssloaders::withSpinner(type = 4,color = "#6db2f2", size = 0.7 ))), br()
             
          
          
             
             )))


        
  

 




