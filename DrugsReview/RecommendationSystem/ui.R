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

# Define UI for application that draws a histogram
shinyUI(
    dashboardPage(
        header <- dashboardHeader(title = "System rekomendacji leków"),
        
        sidebar <- dashboardSidebar(
            sidebarMenu(
                menuItem("Analiza eksploracyjna", tabName = "exploratory", icon = icon("dashboard"))
            ),
            sidebarMenu(
                menuItem("Analiza sentymentu", tabName = "sentiment", icon = icon("angry"))
            )
        ),
        body <- dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "exploratory",
                        h2("Analiza eksploracyjna - spis komentarzy i bazowe statystyki"),
                        fluidRow(selectInput("inputCateg", label = "Wybierz kategorię",
                                                                     choices = unique(drugsCom.train$condition)),
                        selectInput("inputDrug", label = "Wybierz lek", choices = unique(drugsCom.train$drugName))),
                        dataTableOutput("reviews"),
                        plotlyOutput("condition_piechart"),
                        plotlyOutput("drugName_piechart")
                ),
                
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

