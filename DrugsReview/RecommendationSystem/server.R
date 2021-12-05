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
                      
  })


  

})
