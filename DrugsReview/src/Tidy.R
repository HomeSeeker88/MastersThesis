library(tidymodels)
library(tidyverse)

drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")

drugsCom.train <- drugsCom.train %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))

drugsCom.train %>% group_by(condition) %>% count(sort = T)
