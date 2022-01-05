library(tidyverse)
library(keras)
drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")
drugsCom.test <- read_tsv("Data/drugsComTest_raw.tsv")

drugsCom.train <- drugsCom.train %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition), rating = ifelse(rating >= 7, 1, 0))


drugsCom.test <- drugsCom.test %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition), rating = ifelse(rating >= 7, 1, 0))

X_train <- drugsCom.train$review
Y_train <- drugsCom.train$rating

X_test <- drugsCom.test$review
Y_test <- drugsCom.test$rating

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

model %>% predict(X_train[1])

#TODO: ZROBIĆ NA PECECIE