#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(ggplot2)
library(sp)
library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(inlmisc)
library(jsonlite)
library(nominatim)
library(tidygeocoder)
library(mdthemes)
library(tidyverse)
library(osmdata)
library(sf)
library(geojsonio)
library(cld2)
library(magrittr)
library(htmlwidgets)
library(ggpubr)
library(plotly)
library(franc)
library(cbsodataR)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(readr)
library(DT)





dashHeader <- dashboardHeader(title = "Check-the-Facts", titleWidth = 300)

dashSidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home",
             tabName = "HomeTab",
             icon=icon("dashboard")),
    menuItem('Analytics',
             tabName = 'CompareTab',
             icon = icon('bar-chart-o')),
    menuItem('Background and Sources',
             tabName = 'SourceTab',
             icon = icon('table'))
    
  ),
  sliderInput("bar1","Year:", min=min(data4$Year), max = max(data4$Year), value = 2019, step = 1, sep = ""),
  
  selectizeInput("bar2", label = h5("Select area of study"),
              choices = sort(unique(data4$CROHO.ONDERDEEL)), selected = NULL, multiple = TRUE, 
              options = list(plugins= list('remove_button'))),
  
  selectizeInput("bar3", label = h5("Select WO/HBO"),
              choices = sort(unique(data4$HO.type)), selected = NULL, multiple = TRUE, 
              options = list(plugins= list('remove_button'))),
  
  selectizeInput("bar4", label = h5("Select language of study"),
              choices = sort(unique(data4$language)), selected = NULL, multiple = TRUE, 
              options = list(plugins= list('remove_button'))),
  
  pickerInput("bar5", h5("Select Location"), choices=sort(unique(data4$GEMEENTENAAM)), multiple = TRUE,
                 options = list(`actions-box` = TRUE, size = 8)),
  
  selectizeInput("bar6", label = h5("Select Bachelor/Master"),
              choices = sort(unique(data4$TYPE.HOGER.ONDERWIJS)), selected = NULL, multiple = TRUE, 
              options = list(plugins= list('remove_button'))),
  hr(),
  helpText(" All datasets are from openly available sources.  Initiative of CBS, KOOP, data.overheid and the Onderwijs community")

)

dashBody <- dashboardBody(
  tabItems(
    tabItem(tabName = "HomeTab",
            fluidPage(
              fluidRow(leafletOutput(outputId = "map",height = "600px"),
                       plotlyOutput('bar'),width=6, height=500),
              DT::dataTableOutput("table"),width= 9, height=500)
            ),
    tabItem(tabName = "CompareTab", h2("Analysis of data (in depth)")),
    tabItem(tabName = "SourceTab", h2("text"))
  ))

# Define UI
ui <- dashboardPage(
  header = dashHeader,
  sidebar = dashSidebar,
  body = dashBody,
  title = 'Prototype 1'
)