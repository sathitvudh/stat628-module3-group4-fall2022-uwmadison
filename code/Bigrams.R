library(tidyverse)
library(tm)
library(wordcloud)
library(SentimentAnalysis)
library(tidytext)
library(dplyr)
library(sqldf)
library(sentimentr)
library(textdata)
library(ggraph)
library(igraph)

tip = read.csv("./data/bars_tip_ca.csv")
tip



# Frequency for Bigrams
bigram_tips = tip %>% 
  select(business_id, stars, text) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  filter(!grepl('[0-9]', word)) %>%
  count(stars,word,business_id, sort = TRUE) 

# Calculate tf-idf (relavance)
bigram_tfidf <- bigram_tips %>%
  bind_tf_idf(word, stars, n) %>%
  arrange(desc(tf_idf))

# Plot top 30 relavent words
bigram_plot <- bigram_tfidf %>%
  group_by(stars) %>%
  slice(1:30) %>%
  ggplot()+
  geom_col(aes(tf_idf,reorder(word,tf_idf)))+
  facet_wrap(~stars, scales="free")
bigram_plot

# Keep only the rows that fall in 4, 4.5, or 5 stars
bigrams <- bigram_tfidf %>%
  filter(stars == c(4, 4.5, 5))

write.csv(bigrams, file = "bigrams.csv")

