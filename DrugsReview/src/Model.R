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

X_train
Y_train

X_train %>% str_split(" ") %>% 
  sapply(length) %>% 
  summary()

num_words <- 5000
max_length <- 100

#TODO: ZROBIĆ NA PECECIE