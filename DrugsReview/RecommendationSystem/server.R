#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  observe({
    
    
    tmp <- drugsCom.train %>% filter(condition == input$inputCateg)
    
    updateSelectInput(session = getDefaultReactiveDomain(), "inputDrug", label = "Wybierz lek", choices = unique(tmp$drugName))
    
    output$reviews <- renderDataTable(drugsCom.train %>% filter(condition == input$inputCateg & drugName == input$inputDrug) %>% select(uniqueID, review, usefulCount))
    output$condition_piechart <- renderPlotly(drugsCom.train %>% group_by(condition) %>% count(sort = T) %>%
                                      mutate(condition = ifelse(n < 2000, 'other', condition)) %>%
                                      plot_ly(labels = ~condition, values = ~n, type = 'pie') %>% layout(title = 'Jakie kategorie są najpopularniejsze'))
    output$drugName_piechart <- renderPlotly(drugsCom.train %>% filter(condition == input$inputCateg) %>% group_by(drugName) %>% count(sort = T) %>%
                                               mutate(condition = ifelse(n < 500, 'other', drugName)) %>% 
                                               plot_ly(labels = ~drugName, values = ~n, type = 'pie'))
                  
  })


  

})
