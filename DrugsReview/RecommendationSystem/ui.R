#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(plotly)
library(lubridate)

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        header <- dashboardHeader(title = "System rekomendacji leków"),
        
        sidebar <- dashboardSidebar(
            sidebarMenu(
                menuItem("Analiza eksploracyjna", tabName = "exploratory", icon = icon("dashboard"),
                         menuItem("Najważniejsze informacje", tabName = "summary"),
                         menuItem("Najlepsze leki", tabName = "BestDrugs"))
            ),
            sidebarMenu(
                menuItem("Analiza sentymentu", tabName = "sentiment", icon = icon("angry"))
            ),
            sidebarMenu(
                menuItem("Model klasyfikacji komentarzy", tabName = "model", icon = icon("book"))
            )
        ),
        body <- dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "summary",
                        h2("Analiza eksploracyjna - spis komentarzy i bazowe statystyki"),
                        fluidRow(selectInput("inputCateg", label = "Wybierz kategorię",
                                                                     choices = unique(drugsCom.train$condition)),
                        selectInput("inputDrug", label = "Wybierz lek", choices = unique(drugsCom.train$drugName))),
                        dataTableOutput("reviews"),
                        plotlyOutput("condition_piechart"),
                        plotlyOutput("drugName_piechart")
                ),
                tabItem(tabName = "BestDrugs",
                        h2("Najlepsze leki na poszczególne przypadłości"),
                        fluidRow(infoBoxOutput("AverageRatingInfoBox"),
                                 infoBoxOutput("CountOpinionsInfoBox"),
                                 infoBoxOutput("UsefulCountInfoBox")),
                        fluidRow(selectInput("inputCategSentBestDrugs", label = "Wybierz kategorię",
                                             choices = unique(drugsCom.train$condition))),
                        plotlyOutput("TopDrugPlot")),
                
                # Second tab content
                tabItem(tabName = "sentiment",
                        h2("Analiza sentymentu"),
                        fluidRow(selectInput("inputCategSent", label = "Wybierz kategorię",
                                             choices = unique(drugsCom.train$condition)),
                                 selectInput("inputDrugSent", label = "Wybierz lek", choices = unique(drugsCom.train$drugName))),
                        plotOutput("wordcloud"),
                        plotOutput("feelwords")
                        
                )
                
                
            )
        )
    )
)

