library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)

data("stop_words")

drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")

stop_words
drugsCom.bigrams <- drugsCom.train %>%
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

bigrams.counts <- bigrams.separated %>% count(firstword, secondword, sort = T)

bigrams.united <- bigrams.separated %>% unite(bigram, firstword, secondword, sep = " ")

bigram_tf_idf <- bigrams.united %>% 
  count(drugName, bigram) %>% 
  bind_tf_idf(bigram, drugName, n) %>% 
  arrange(desc(tf_idf))

bigram_tf_idf

bigrams.separated %>% dplyr::filter(firstword == "bad") %>% count(firstword, secondword, sort = T)

bigrams.separated %>% dplyr::filter(str_detect(firstword, pattern = "feel")) %>% count(firstword, secondword, sort = T)

AFINN <- get_sentiments("afinn")
AFINN

bad_word <- bigrams.separated %>% dplyr::filter(firstword == "bad") %>% 
  dplyr::filter(firstword == "bad") %>% inner_join(AFINN, by = c(secondword = "word")) %>% 
  count(secondword, value, sort = T) %>% 
  ungroup()

feel_words <- bigrams.separated %>% 
  dplyr::filter(str_detect(firstword, pattern = "feel")) %>% inner_join(AFINN, by = c(secondword = "word")) %>% 
  count(secondword, value, sort = T) %>% 
  ungroup()


feel_words %>% 
  mutate(contribution = n * value) %>% 
  arrange(desc(abs(contribution))) %>% 
  head(20) %>% 
  mutate(secondword = reorder(secondword, contribution)) %>% 
  ggplot(aes(secondword, n * value, fill = n * value > 0)) +
  geom_col(show.legend = F) + 
  xlab("Words preceded by \"feel\"") +
  ylab("Sentiment score * number of occurences") + 
  coord_flip()


library(igraph)

bigrams.graph <- bigrams.counts %>% 
  dplyr::filter(n > 700) %>% 
  graph_from_data_frame()

library(ggraph)

set.seed(2017)

ggraph(bigrams.graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)


set.seed(2016)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigrams.graph, layout = "fr") + 
  geom_edge_link(aes(edge_alpha = n), show.legend = F, arrow = a, end_cap = circle(.07, "inches")) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


drugsCom.train <- drugsCom.train %>%
  mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition))


