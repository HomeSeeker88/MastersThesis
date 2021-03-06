library(tidyverse)
library(keras)
library(deepviz)
library(caret)
library(pROC)
library(ggthemes)

drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")
drugsCom.test <- read_tsv("Data/drugsComTest_raw.tsv")

drugsCom.train <- drugsCom.train %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))


drugsCom.test <- drugsCom.test %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))

X_train <- drugsCom.train$review
Y_train <- ifelse(drugsCom.train$rating >= 7, 1, 0)

X_test <- drugsCom.test$review
Y_test <- ifelse(drugsCom.test$rating>= 7, 1, 0)

#X_train
#Y_train

X_train %>% str_split(" ") %>% 
  sapply(length) %>% 
  summary()

num_words <- 10000
max_length <- 100

text_vectorization <- layer_text_vectorization(max_tokens = num_words,
                                               output_sequence_length = max_length)

text_vectorization %>% adapt(drugsCom.train$review)

get_vocabulary(text_vectorization)

text_vectorization(matrix(drugsCom.train$review[1], ncol = 1))

input <- layer_input(shape = c(1), dtype = "string")

output <- input %>% 
  text_vectorization() %>% 
  layer_embedding(input_dim = num_words + 1, output_dim = 128) %>%
  #layer_conv_1d(filters = 2, kernel_size = 3, activation = 'relu') %>% 
  layer_global_max_pooling_1d() %>% 
  layer_dense(units = 16, activation = 'relu') %>% 
  layer_dropout(0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid')

model <- keras_model(input, output)

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

history <- model %>% fit(
  X_train,
  Y_train,
  epochs = 10,
  batch_size = 512,
  validation_split = 0.2,
  verbose=2
)

results <- model %>% evaluate(X_test, Y_test, verbose = 0)
results

plot(history)

model %>% save_model_tf("MyModel")

model <- load_model_tf("MyModel")

model %>% predict(X_train[1])

model %>% plot_model()

results <- predict(model, X_test, type = "response")

rocLR<-roc(Y_test, results)

LR.DF<-data.frame(sensitivity=rocLR$sensitivities,specificity=rocLR$specificities)

auc(Y_test, results)

ggplot(LR.DF,aes(x=specificity,y=sensitivity))+geom_path(size=0.5,color='blue')+scale_x_reverse()+
  geom_abline(intercept =1,lty=1,color="red")+theme_solarized()+
  ggtitle("Wykres krzywej ROC dla modelu sieci neuronowej")

drugsCom.test <- drugsCom.test %>% 
  mutate (modelResponse = ifelse(model %>% predict(review, type = "response") >= 0.7, "Opinia pozytywna",
                                 "Opinia negatywna"))%>% 
  mutate(Correct = case_when(
    rating >= 7 & modelResponse == "Opinia pozytywna" ~ TRUE,
    rating >= 7 & modelResponse != "Opinia pozytywna" ~ FALSE,
    rating < 7 & modelResponse == "Opinia negatywna" ~ TRUE,
    TRUE~FALSE
  ))

m <- table(drugsCom.test$Correct, drugsCom.test$modelResponse) %>% as.data.frame()

trueP <- m[4,3]
trueN <- m[2,3]
trueP
trueN
falseP <- m[3,3]
falseN <- m[1,3]

spec <- round(trueP/(trueP+falseN), 2)
sens <- round(trueN/(trueN+falseP), 2)
prec <- round(trueP/(trueP + falseP), 2)
f1 <- round(2 * (prec*sens/(prec + sens)), 2)
spec
sens
#TODO: ZROBIĆ NA PECECIE