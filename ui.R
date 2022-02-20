### Author: AHz
### Date published: 1/18/2022


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lubridate)

# set up page structure 
shinyUI(navbarPage("PFAS in MA Drinking Water", 
                   tabPanel("What's in my water?", 
                            fluidPage(
                              #set theme
                              theme = shinytheme("cerulean"),
                              tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                              #add analytics
                              #tags$head(includeHTML(("html/google-analytics.html"))),
                              fluidRow(
                                column(5,
                                       #add panel with text on the left side of the page
                                       wellPanel(h3("What are PFAS?"),
                                                 htmlOutput("pfas_exp"),
                                                 h3("Health Effects"),
                                                 htmlOutput("health_effects"),
                                                 h3("Are there PFAS in my water?"),
                                                 htmlOutput("summary"),
                                                 h3("What can I do?"),
                                                 p("There are many steps you can take to reduce your exposure to PFAS."),
                                                 htmlOutput("treatment"),
                                                 htmlOutput("community"))
                                ),
                                #add input options 
                                column(7,
                                       h2("My PFAS Water Report"),
                                       uiOutput("instructions"),
                                       h4(""),
                                       fluidRow(column(6,uiOutput("town"),
                                                       uiOutput("chemicals")),
                                                column(2, uiOutput("year"),
                                                       #button does not currently work, requires Rmd file 
                                                       actionButton("download", 
                                                                    "Download Report", 
                                                                    icon = icon("download")))),
                                       #add graph and table content 
                                       tabsetPanel(
                                         tabPanel("Graphs",
                                                  htmlOutput("hint"),
                                                  plotlyOutput("dw")),
                                         tabPanel("Table", DT::dataTableOutput("dw_table")))
                                )
                              )
                            )
                            
                   ),
                   tabPanel(title = "FAQ",
                            htmlOutput("FAQ_text")),
                   tabPanel("About",
                            htmlOutput("about"))))
