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
library(DT)

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
                menuItem("Model", tabName = "model", icon = icon("book"),
                         menuItem("Tworzenie modelu", tabName = "modelCreation"),
                         menuItem("Podsumowanie modelu", tabName = "modelSummary"),
                         menuItem("Weryfikacja komentarza", tabName = "modelTesting"))
            )
        ),
        body <- dashboardBody(
            tabItems(
                # First tab content
                tabItem(tabName = "summary",
                        h2("Analiza eksploracyjna - spis komentarzy i bazowe statystyki"),
                        fluidRow(selectizeInput("inputCateg", label = "Wybierz kategorię",
                                                                     choices = unique(drugsCom.train$condition) %>% sort()),
                        selectizeInput("inputDrug", label = "Wybierz lek", choices = unique(drugsCom.train$drugName))),
                        DTOutput("reviews"),
                        plotlyOutput("rating_histogram")
                ),
                tabItem(tabName = "BestDrugs",
                        h2("Najlepsze leki na poszczególne przypadłości"),
                        fluidRow(infoBoxOutput("AverageRatingInfoBox"),
                                 infoBoxOutput("CountOpinionsInfoBox"),
                                 infoBoxOutput("UsefulCountInfoBox")),
                        fluidRow(selectizeInput("inputCategSentBestDrugs", label = "Wybierz kategorię",
                                             choices = unique(drugsCom.train$condition) %>% sort())),
                        plotlyOutput("TopDrugPlot"),
                        plotlyOutput("condition_piechart"),
                        plotlyOutput("drugName_piechart")),
                
                # Second tab content
                tabItem(tabName = "sentiment",
                        h2("Analiza sentymentu"),
                        fluidRow(selectizeInput("inputCategSent", label = "Wybierz kategorię",
                                             choices = unique(drugsCom.train$condition) %>% sort()),
                                 selectizeInput("inputDrugSent", label = "Wybierz lek", choices = unique(drugsCom.train$drugName))),
                        plotOutput("wordcloud"),
                        plotOutput("feelwords"),
                        plotOutput("notwords"),
                        plotOutput("tfIdf")
                        
                        
                ),
                tabItem(tabName = "modelCreation",
                        h2("Tworzenie modelu"),
                        h3("Wpisz hiperparametry sieci"),
                        fluidRow(numericInput("inputBatchSize", label = "Wpisz batch size", value = 64, width = "10%"),
                                 numericInput("inputEpoc", label = "Wpisz liczbę iteracji", value = 10, width = "10%"),
                                 numericInput("inputDenseUnits", label = "Wpisz liczbę neuronów w warstwie gęstej", value = 16, width = "10%"),
                                 numericInput("inputRate", label = "Wpisz współczynnik dropoutu", value = 0.5, width = "10%"),
                                 actionButton("inputModelButton", "Stwórz model", class = "btn-success")),
                        fluidRow(plotOutput("modelLossPlot"),
                                 plotOutput("modelROCPLOT"))),
                tabItem(tabName = "modelSummary",
                        h2("Podsumowanie modelu"),
                        fluidRow(infoBoxOutput("GeneralAccuracy")),
                        fluidRow(
                          plotOutput("modelPlot"),
                          plotOutput("rocPlot")
                        )),
                tabItem(tabName = "modelTesting",
                        h2("Rozpoznanie opinii"),
                        fluidRow(textInput("inputComment", label = "Wpisz komentarz",value = "",width = "75%"),
                                 infoBoxOutput("Response"),
                                 infoBoxOutput("Accuracy"),
                                 infoBoxOutput('Recommendation')),
                        fluidRow(selectizeInput("inputModelCateg", label = "Wybierz kategorię",
                                             choices = unique(drugsCom.test$condition)),
                                 selectizeInput("inputModelDrug", label = "Wybierz lek", choices = unique(drugsCom.test$drugName))),
                        DTOutput("testReviews")
                        
                        )
                
                
            )
        )
    )
)

