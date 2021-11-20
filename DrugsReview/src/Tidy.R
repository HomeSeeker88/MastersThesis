library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)

data("stop_words")

drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")

drugsCom.train <- drugsCom.train %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))

tidyComms <- drugsCom.train %>% unnest_tokens(word, review) %>% anti_join(stop_words)

tidyComms %>% count(word, sort = T)

tidyComms %>% count(word, sort = T) %>% 
  filter(n > 30000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word,n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

sentiments <- get_sentiments("nrc")
sentiments

tidyComms %>% inner_join(sentiments) %>% 
  count(uniqueID, word, sort= T)

tidyComms %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = T) %>% 
  acast(formula = word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(colors = c ("red", "green"),max.words = 100)

tidySentences <- drugsCom.train %>% unnest_tokens(sentence, review, token = "sentences")

negative <- get_sentiments("bing") %>% 
  filter(sentiment == "negative")

negative


wordcounts <- tidyComms %>% group_by(drugName) %>% 
  summarise(words= n())

tidyComms %>% semi_join(negative) %>% 
  group_by(drugName) %>% 
  summarise(negativewords = n()) %>% 
  left_join(wordcounts, by = "drugName") %>% 
  mutate(ratio = negativewords/words) %>% 
  arrange(ratio)

tidyComms %>% semi_join(negative) %>% 
  group_by(condition) %>% 
  summarise(negativewords = n())
