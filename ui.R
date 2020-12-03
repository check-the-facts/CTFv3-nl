

#Version 2.1


# library(ggplot2)
# library(sp)
# library(shiny)
# library(leaflet)
# library(dplyr)
# library(leaflet.extras)
# library(inlmisc)
# library(jsonlite)
# library(nominatim)
# library(tidygeocoder)
# library(mdthemes)
# library(tidyverse)
# library(osmdata)
# library(sf)
# library(geojsonio)
# library(cld2)
# library(magrittr)
# library(htmlwidgets)
# library(ggpubr)
# library(plotly)
# library(franc)
# library(cbsodataR)
# library(shinydashboard)
# library(shinythemes)
# library(shinyWidgets)
# library(readr)
# library(DT)
# library(dodgr)


###NEW DASHBOARD

ui <- dashboardPage(
  skin = "black",
  title = "Check-the-Facts: Higher Education",
  
  dashboardHeader(
    title = span(img(src = "check the facts.svg", height = 35), "Check-The-Facts: Higher Education"),
    titleWidth = 300,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("HELP"), 
      icon = icon("question"), 
      badgeStatus = NULL
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
    width = 300,
    div(class = "inlay", style = "height:15px;width:100%;background-color: #ecf0f5;"),
    menuItem(
      "DOWNLOAD SELECTION",
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
          label = "Save table data",
          icon = icon("download"),
          style = "color: black; margin-left: 15px; margin-bottom: 5px;"
        )
      ),
      div(
        downloadButton(
          outputId = "downloadMicroData",
          label = "Save graph Data",
          icon = icon("download"),
          style = "color: black; margin-left: 15px; margin-bottom: 5px;"
        )
      )
    ),
    br() ),
  
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "CTF_style.css")
    ),
    
    useShinyjs(),
    introjsUI(),
    
    # MAIN BODY ---------------------------------------------------------------
    
    fluidRow(
      column(
        width = 12,
        introBox(
          bsButton("Study Programs",
                   label = "Study Programs",
                   icon = icon("user-graduate"),
                   style = "primary"),
          bsButton("Regional Stats",
                   label = "Regional Stats",
                   icon = icon("globe-europe"),
                   style = "primary"),
          bsButton("Analytics",
                   label = "Analytics",
                   icon = icon("chart-line"),
                   style = "primary"),
          bsButton("Sources",
                   label = "Sources",
                   icon = icon("table"),
                   style = "primary"))
      )
    ),
    
    fluid_design("RegionalStats_panel", "box1", "box2", "box3", "box4"),
    fluid_design("Analytics_panel", "box5", "box6", "box7", "box8"),
    fluid_design("Sources_panel", "box_los1", "box_los2", "box_los3", NULL),
    
    fluidRow(
      div(
        id = "Studyprograms_panel",
        column(
          width = 12
        ),
        column(
          width = 6,
          uiOutput("box_pat2")
        ),
        column(
          width = 6,
          uiOutput("box_year"))))))
  

 




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