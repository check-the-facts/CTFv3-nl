

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
    
    fluid_design("RegionalStats_panel", "box1", "box2", "box3", "box4"),
    fluid_design("Analytics_panel", "box5", "box6", "box7", "box8"),
    fluidRow(id = "Sources_panel", br(),
               box(title = "Sources", "This table shows the original sources used. These are all openly available (open data).",
                 br(), width = 12, tableOutput("source_table")
             )),
    
    fluidRow(id = "Studyprograms_panel", br(),
             tabBox(title = "Studies",id = "tabsetstud", width =  12,
                    tabPanel("Study Program Table", DT::dataTableOutput("studies_table")),
                    tabPanel("Matrix", "a doorstroommatrix")), br(),
             tabBox(title = "Inschrijvingen", id = "tabsetinsch", width = 6,
                    tabPanel("Trend", streamgraphOutput("ins_stream")),
                    tabPanel("per group", plotlyOutput("bar_studies")),
                    selectInput("streamin",
                                label = "Choose a variable to display",
                                choices = c("INSTELLINGSNAAM", "OPLEIDINGSNAAM.ACTUEEL", "GESLACHT", "GEMEENTENAAM.x"),
                                selected = "INSTELLINGSNAAM")
                    
                    
                    ), br(),
             tabBox(title = "Map", id = "tabsetmap", width = 6,
                    tabPanel("HO locations"), leafletOutput("map1"))
          
          
             
             )))
        
  

 




# ###### OLD DASHBOARD
# 
# dashHeader <- dashboardHeader(title = "Check-the-Facts", titleWidth = 300)
# 
# dashSidebar <- dashboardSidebar(
#   
#   ,
#   hr(),
#   helpText(" All datasets are from openly available sources.  Initiative of CBS, KOOP, data.overheid and the Onderwijs community")
#   
# )
# 
# dashBody <- dashboardBody(
#   tabItems(
#     tabItem(tabName = "HomeTab", h2("Welcome! This is Check-the-Facts."), 
#             h4("A place where you can check the facts regarding higher education and hopefully be able to make better and data-driven decsions!"),
#             fluidPage(
#               fluidRow(leafletOutput(outputId = "map",height = "600px"),
#                        plotlyOutput('bar'),width=6, height=500),
#               DT::dataTableOutput("table"),width= 9, height=500)
#     ),
#     tabItem(tabName = "CompareTab", h2("Analysis of data (in depth)")),
#     tabItem(tabName = "ResearchTab", h2("Studies, Research and Reports")),
#     tabItem(tabName = "SourceTab", h2("text"))
#   ))
# 
# # Define UI
# ui <- dashboardPage(
#   header = dashHeader,
#   sidebar = dashSidebar,
#   body = dashBody,
#   title = 'Prototype 1'
# )