### Author: AHz
### Date published: 1/18/2022


library(shiny)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(lubridate)
library(lemon)


shinyUI(navbarPage("PFAS in MA Drinking Water", 
                   tabPanel("What's in my water?", 
                            fluidPage(
                                theme = shinytheme("cerulean"),
                                tags$head(includeHTML(("html/google-analytics.html"))),
                                fluidRow(
                                    column(4,
                                           wellPanel(h3("What are PFAS?"),
                                                     htmlOutput("pfas_exp"),
                                                     h3("Health Effects"),
                                                     htmlOutput("health_effects"),
                                                     h3("Are there PFAS in my water?"),
                                                     htmlOutput("summary"),
                                                     h3("What can I do?"),
                                                     p("There are many steps you can take to reduce your exposure to PFAS."),
                                                     h4("Water Treatment"),
                                                     htmlOutput("treatment"),
                                                     h4("In Your Community"),
                                                     htmlOutput("community"))
                                    ),
                                    column(8,
                                           h2("My PFAS Water Report"),
                                           uiOutput("instructions"),
                                           h4(""),
                                           fluidRow(column(6,uiOutput("town"),
                                                           uiOutput("chemicals")),
                                                    column(2, uiOutput("year"),
                                                           actionButton("download", 
                                                                        "Download Report", 
                                                                        icon = icon("download")))),
                                           
                                           tabsetPanel(
                                             tabPanel("Graphs", 
                                                      p(""),
                                                      htmlOutput("hint"),
                                                      plotlyOutput("dw")),
                                             tabPanel("Table", DT::dataTableOutput("dw_table")))
                                    )
                                )
                            )
                            
                   ),
                   tabPanel(title = "FAQ",
                            id = "FAQ",
                            htmlOutput("FAQ_text")),
                   tabPanel("About",
                            h3("About this tool"),
                            htmlOutput("about"))))
