library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)

data("stop_words")

drugsCom.train <- read_tsv("Data/drugsComTrain_raw.tsv")

drugsCom.train <- drugsCom.train %>% mutate(condition = ifelse(str_detect(condition, "users found this"), NA, condition)) %>%  anti_join(stop_words)

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
  count(drugName, word, sort= T)

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


tidyComms %>% count(word, sort = T)

wordComms<- tidyComms %>% 
  count(drugName, word, sort= T)

totalWords <- wordComms %>% group_by(drugName) %>% 
  summarize(total = sum(n))


wordComms <- left_join(wordComms, totalWords)

top6<- wordComms %>%  arrange(desc(total)) %>% distinct(drugName, total) %>% top_n(6)

wordComms %>% inner_join(top6, by = c("drugName", "total")) %>% ggplot(aes(n/total, fill = drugName))+
  geom_histogram(show.legend = F) + xlim(NA, 0.009) + facet_wrap(~drugName, ncol=2, scales = "free_y")


freq_by_rank <- wordComms %>% group_by(drugName) %>% mutate(rank = row_number(), term.frequency= n/total)

freq_by_rank %>% inner_join(top6, by= c("drugName", "total")) %>% ggplot(aes(rank, term.frequency, color = drugName)) + 
  geom_line(size = 1.1, alpha = 0.8, show.legend = F)+
  scale_x_log10()+
  scale_y_log10()

rank_subset <- freq_by_rank %>% 
  filter(rank < 500,
         rank > 10)


lm(log10(term.frequency) ~ log10(rank), data=rank_subset)


freq_by_rank %>% inner_join(top6, by = c("drugName", "total")) %>% 
  ggplot(aes(rank, term.frequency, color = drugName)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

wordComms

wordComms <- wordComms %>% 
  bind_tf_idf(drugName, word, n)

wordComms

wordComms %>% select(-total) %>% 
  arrange(desc(tf_idf))


wordComms %>% arrange(desc(tf_idf)) %>% 
  inner_join(top6) %>% 
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(drugName) %>% 
  top_n(n = 2) %>% 
  ungroup() %>% 
  ggplot(aes(word, tf_idf, fill = drugName)) +
  geom_col(show.legend = F) +
  labs(x = NULL, y = "tf-idf") + 
  facet_wrap(~drugName, ncol = 2, scales = "free") +
  coord_flip()



tidyComms
