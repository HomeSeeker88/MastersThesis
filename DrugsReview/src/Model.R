library(tidyverse)
library(keras)
drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")

drugsCom.train <- drugsCom.train %>% 
  mutate(condition = case_when(
    str_detect(condition, "Cance") ~ case_when(
      !str_detect(condition, "Cancer") ~ str_replace(condition, "Cance", "Cancer"),
      TRUE ~ condition
    ),
    TRUE ~ condition
  )) %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))


X_train <- drugsCom.train$review
Y_train <- drugsCom.train$rating/10

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

#TODO: ZROBIĆ NA PECECIE