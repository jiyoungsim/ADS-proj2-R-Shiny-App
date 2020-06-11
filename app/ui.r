library(leaflet)
library(shiny)
library(tidyverse)
library(dplyr)
library(sp)
library(sf)
library(RDSTK)
library(leaflet.extras)
library(shinydashboard)
library(shinycssloaders)
library(plyr)
library(readr)

childcare <-childcare <-read_csv(file = 'childcare_AppUsed.csv')
colnames(childcare) <- make.names(colnames(childcare), unique=TRUE)

dashboardPage(
  dashboardHeader(title = "FiCoH NYC",titleWidth=300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Manual", tabName = "Part0", icon = icon("info")),
      textInput("address1","Enter address (e.g. 2990 Broadway, New York, New York): ",value = NULL),
      numericInput("address2","Enter zipcode (e.g. 10027): ",value = NA),
      menuItem("At a Glance", tabName = "Part1", icon = icon("map")),
      menuItem("Explore More", icon = icon("layer-group"),
               menuItem("Safety", tabName = "Part2"),
               menuItem("Environment",  tabName = "Part3"), 
               menuItem("Childcare",
                        menuSubItem('childcare',
                                    tabName = 'Part4',
                                    icon = icon('line-chart')),
                        selectInput("childcare", "childcare", choices = c(unique(childcare$Child.Care.Type))),
                        radioButtons("variable", "Markers to show:",
                                     c("No Violation" = "noViolation",
                                       "Violation Rate Lower than Average"="lessThanAve"
                                       
                                     )),
                        checkboxInput("nearby", "Only Show the Childcare Center Nearby", value = FALSE, width = NULL)
                        
                        
                        
               )  
               
      )
      
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Part0",withSpinner(htmlOutput("userp"))),
      tabItem(tabName = "Part1",withSpinner(leafletOutput('plot1')), withSpinner(textOutput('text1')),withSpinner(tableOutput('table1a')), withSpinner(tableOutput('table1b')), withSpinner(tableOutput('table1c'))),
      ####reference
      tabItem(tabName = "Part2",withSpinner(leafletOutput('plot2')), withSpinner(plotOutput('boxplot2a')), withSpinner(plotOutput('boxplot2b'))),
      tabItem(tabName = "Part3",withSpinner(leafletOutput('plot3')), withSpinner(plotOutput('boxplot3a')), withSpinner(plotOutput('boxplot3b'))),
      tabItem(tabName = "Part4",withSpinner(leafletOutput('plot4')),withSpinner(tableOutput("table4")))
    )))