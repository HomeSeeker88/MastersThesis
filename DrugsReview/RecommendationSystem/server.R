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
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), "inputDrug", label = "Wybierz lek", choices = unique(tmp$drugName))
    
    output$reviews <- renderDT(drugsCom.train %>% filter(condition == input$inputCateg & drugName == input$inputDrug) %>% select(uniqueID, review, usefulCount))
    output$condition_piechart <- renderPlotly(drugsCom.train %>% group_by(condition) %>% count(sort = T) %>%
                                      mutate(condition = ifelse(n < 2000, 'other', condition)) %>%
                                      plot_ly(labels = ~condition, values = ~n, type = 'pie') %>% layout(title = 'Jakie kategorie są najpopularniejsze'))
    output$drugName_piechart <- renderPlotly(drugsCom.train %>% filter(condition == input$inputCategSentBestDrugs) %>% group_by(drugName) %>% count(sort = T) %>%
                                               mutate(condition = ifelse(n < 500, 'other', drugName)) %>% 
                                               plot_ly(labels = ~drugName, values = ~n, type = 'pie') %>% layout(title = "Najczęściej oceniane leki"))
    
    output$rating_histogram <- renderPlotly({
      drugsCom.train %>%
        filter(drugName == input$inputDrug) %>% ggplot() +
        geom_histogram(aes(x = rating, fill = drugName), bins = 10, show.legend = F)
    }) 
    
    output$TopDrugPlot <-renderPlotly(drugsCom.train %>% filter(condition == input$inputCategSentBestDrugs) %>% 
                                      group_by(drugName) %>% summarise(AvgRating = mean(rating)) %>%
                                      arrange(desc(AvgRating)) %>% head(15) %>% ggplot(aes(x = fct_reorder(drugName, AvgRating),
                                                                                           y = AvgRating))+
                                      geom_bar(aes(y=AvgRating, fill = drugName), stat='identity', show.legend = F)+
                                      ylab("Średnia ocena") + xlab("Nazwa leku")+
                                      theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    
    tmpSent <- drugsCom.train %>% filter(condition == input$inputCategSent)
    updateSelectizeInput(session = getDefaultReactiveDomain(), "inputDrugSent", label = "Wybierz lek", choices = unique(tmpSent$drugName))
    
    output$wordcloud <- renderPlot(tidyComms %>% filter(condition == input$inputCategSent) %>%  inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = T) %>% 
                                     acast(formula = word ~ sentiment, value.var = "n", fill = 0) %>% 
                                     comparison.cloud(colors = c ("red", "green"),max.words = 100)
    )
    
    output$AverageRatingInfoBox <- renderInfoBox(
      infoBox("Średnia ocena w tej kategorii", drugsCom.train %>% filter(condition == input$inputCategSentBestDrugs) %>% 
                summarise(Avg = mean(rating)) %>% as.numeric() %>% round(digits=2),icon = icon("calculator"))
    )
    
    output$CountOpinionsInfoBox <- renderInfoBox(
      infoBox("Ilość komentarzy w tej kategorii", drugsCom.train %>% filter(condition == input$inputCategSentBestDrugs) %>% 
                count() %>% as.numeric())
    )
    
    output$UsefulCountInfoBox <- renderInfoBox(
      infoBox("Ilość polubień w tej kategorii", drugsCom.train %>% filter(condition == input$inputCategSentBestDrugs) %>% 
                summarise(Total = sum(usefulCount)) %>% as.numeric(), icon = icon("thumbs-up"))
    )
    
    output$feelwords <- renderPlot ({
      
      drugsCom.bigrams <- drugsCom.train %>% filter(drugName == input$inputDrugSent) %>% 
        mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition)) %>% unnest_tokens(bigram,
                                                                                                               review,
                                                                                                               token = "ngrams",
                                                                                                               n = 2)
      drugsCom.bigrams %>% count(bigram, sort = T)
      
      bigrams.separated <- drugsCom.bigrams %>% 
        separate(bigram, c("firstword", "secondword"), sep = " ")
      
      bigrams.separated <- bigrams.separated %>% 
        dplyr::filter(!(firstword %in% stop_words$word)) %>% 
        dplyr::filter(!secondword %in% stop_words$word)
      
      
      feel_words <- bigrams.separated %>% 
        dplyr::filter(str_detect(firstword, pattern = "feel")) %>% inner_join(AFINN, by = c(secondword = "word")) %>% 
        count(secondword, value, sort = T) %>% 
        ungroup()
      
      feel_words %>%  mutate(contribution = n * value) %>% arrange(desc(abs(contribution))) %>% head(20) %>% 
      mutate(secondword = reorder(secondword, contribution)) %>% ggplot(aes(secondword, n * value, fill = n * value > 0)) +
                                      geom_col(show.legend = F) + 
                                      xlab("Words preceded by \"feel\"") +
                                      ylab("Sentiment score * number of occurences") + 
                                      coord_flip()})
    
    output$tfIdf <- renderPlot({
      tidyComms <- drugsCom.train %>% filter(condition == input$inputDrugSent) %>% 
        unnest_tokens(word, review) %>% anti_join(stop_words)
      
      wordcounts <- tidyComms %>% group_by(drugName) %>% 
        summarise(words= n())
    })
    
    output$notwords <- renderPlot({
      drugsCom.bigrams <- drugsCom.train %>% filter(drugName == input$inputDrugSent) %>% 
        mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition)) %>% unnest_tokens(bigram,
                                                                                                               review,
                                                                                                               token = "ngrams",
                                                                                                               n = 2)
      drugsCom.bigrams %>% count(bigram, sort = T)
      
      bigrams.separated <- drugsCom.bigrams %>% 
        separate(bigram, c("firstword", "secondword"), sep = " ")
      
      bigrams.separated <- bigrams.separated %>% 
        dplyr::filter(!(firstword %in% stop_words$word)| firstword=='not' | firstword =='no') %>% 
        dplyr::filter(!secondword %in% stop_words$word)
      
      not <- bigrams.separated %>% 
        dplyr::filter(firstword == 'no' | firstword == 'not') %>% inner_join(AFINN, by = c(secondword = "word")) %>% 
        count(secondword, value, sort = T) %>% 
        ungroup()
      
      not %>% 
        mutate(contribution = n * value) %>% 
        arrange(desc(abs(contribution))) %>% 
        head(20) %>% 
        mutate(secondword = reorder(secondword, contribution)) %>% 
        ggplot(aes(secondword, n * value, fill = n * value < 0)) +
        geom_col(show.legend = F) + 
        xlab("Words preceded by \"no\" or \"not\"") +
        ylab("Sentiment score * number of occurences") + 
        coord_flip()})
    
    output$tfIdf <- renderPlot({
      tidyComms <- drugsCom.train %>% filter(drugName == input$inputDrugSent) %>% 
        unnest_tokens(word, review) %>% anti_join(stop_words)
      
      wordComms<- tidyComms %>% 
        count(drugName, word, sort= T)
      
      
      
      totalWords <- wordComms %>% group_by(drugName) %>% 
        summarize(total = sum(n))
      
      
      wordComms <- left_join(wordComms, totalWords)
      
      
      
      wordComms %>% ggplot(aes(n/total, fill = drugName))+
        geom_histogram(show.legend = F) + xlim(NA, 0.009)
      
    })
    
    output$GeneralAccuracy <- renderInfoBox({
      percentage <- drugsCom.test %>% filter(Correct == T) %>% 
        nrow()
      
      n <- drugsCom.test %>% nrow()
      
      infoBox("Procent dobrych odpowiedzi dla całego zbioru testowego", paste0(round(percentage/n * 100), "%"), icon = icon("dashboard"))
    })
    
    output$modelPlot <- renderPlot({
      plot(history)
    }, width = 600, height = 500)
    
    output$rocPlot <- renderPlot({
      ggplot(LR.DF,aes(x=specificity,y=sensitivity))+geom_path(size=0.5,color='blue')+scale_x_reverse()+
        geom_abline(intercept =1,lty=1,color="red")+theme_solarized()+
        ggtitle("Wykres krzywej ROC dla modelu sieci neuronowej")
    },width = 600, height = 500)
    
    output$Response <- renderInfoBox({

      res <- model %>% predict(input$inputComment)
      if (is.nan(res)){
        infoBox("Odpowiedź modelu ", "brak odpowiedzi", icon = icon("thumbs-up"))
      }
      else if (res > 0.75){
        infoBox("Odpowiedź modelu ", "Opinia pozytywna", icon = icon("thumbs-up"))
      }else{
        infoBox("Odpowiedź modelu ", "Opinia negatywna", icon = icon("thumbs-down"))
      }
      #infoBox("as",res, icon = icon("thumbs-up"))

      
    })
    
  
    
    tmpModel <- drugsCom.test %>% filter(condition == input$inputModelCateg)
    
    updateSelectizeInput(session = getDefaultReactiveDomain(), "inputModelDrug", label = "Wybierz lek", choices = unique(tmpModel$drugName))
    
    output$testReviews <- renderDT({
      dt <- datatable(drugsCom.test %>% filter(drugsCom.test$condition == input$inputModelCateg &
                                 drugsCom.test$drugName == input$inputModelDrug) %>% 
        select(uniqueID, review, rating, modelResponse, usefulCount, Correct))
      
      dt %>%  formatStyle(columns = "modelResponse", valueColumns = "modelResponse", target = "row",
                          backgroundColor =  styleEqual(levels = c("Opinia pozytywna", "Opinia negatywna"),
                                                           values = c("lightgreen", "red")))
        
    })
    
    output$Accuracy <- renderInfoBox({
      percentage <- drugsCom.test %>% filter(drugsCom.test$condition == input$inputModelCateg &
                                 drugsCom.test$drugName == input$inputModelDrug) %>% 
        filter(Correct == TRUE) %>% nrow() %>% as.numeric()
      
      n <- drugsCom.test %>% filter(drugsCom.test$condition == input$inputModelCateg &
                                 drugsCom.test$drugName == input$inputModelDrug) %>% nrow()
      
      
      infoBox("Procent dobrych odpowiedzi dla wybranego leku", value = paste0(round(percentage/n, 2)*100, "%"), icon = icon("dashboard"))
    }
      
    )
    output$Recommendation <- renderInfoBox({
      percentage <- drugsCom.test %>% filter(drugsCom.test$condition == input$inputModelCateg &
                                               drugsCom.test$drugName == input$inputModelDrug) %>%
        filter(modelResponse == "Opinia pozytywna") %>% nrow() %>% as.numeric()

      n <- drugsCom.test %>% filter(drugsCom.test$condition == input$inputModelCateg &
                                      drugsCom.test$drugName == input$inputModelDrug) %>% nrow()

      if(percentage/n >= 0.75){
        infoBox("Procent osób rekomendujących ten lek według modelu", value = paste0(round(percentage/n, 2)*100, "%"), icon = icon("smile"))
      }
      else if (percentage/n >0.5 & percentage/n <0.75){
        infoBox("Procent osób rekomendujących ten lek według modelu", value = paste0(round(percentage/n, 2)*100, "%"), icon = icon("meh"))
      }
      else{
        infoBox("Procent osób rekomendujących ten lek według modelu", value = paste0(round(percentage/n, 2)*100, "%"), icon = icon("frown"))
      }
      
      
    })


    
    

                  
  })


  

})
