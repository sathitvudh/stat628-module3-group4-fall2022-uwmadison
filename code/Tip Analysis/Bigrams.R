library(tidyverse)
library(tm)
library(tidytext)
library(dplyr)
library(sqldf)
library(sentimentr)
library(textdata)
library(ggraph)
library(igraph)
library(topicmodels)
library(vader)
library(tidygraph)
library(ggraph)

tip = read.csv("./data/bars_tip_ca.csv")
head(tip)


# Frequency for Bigrams
bigrams <- tip %>%
  select(business_id, stars, text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram,c(word1,word2), sep = " ") %>% 
  filter(!grepl('[0-9]', bigram)) %>%
  count(stars,bigram,business_id, sort = TRUE) 

bigrams_tfidf <- bigrams %>%
  bind_tf_idf(bigram, stars, n) %>%
  arrange(desc(tf_idf))

bigram_plot <- bigrams_tfidf %>%
  group_by(stars) %>%
  slice(1:30) %>%
  ggplot()+
  geom_col(aes(tf_idf,reorder(bigram,tf_idf)))+
  facet_wrap(~stars, scales="free")
bigram_plot



top_3 <- bigrams_tfidf %>%
  filter(stars == c(4, 4.5, 5)) %>%
  group_by(business_id)



write.csv(top_3, file = "./code/Shiny/bigrams.csv")




#Visualize Word Network
bigrams_separated <- tip %>%
  select(business_id, stars, text) %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!grepl('[0-9]', word1,word2)) %>%
  count(word1,word2, sort = TRUE) 

bigram_network <- bigrams_separated %>%
  filter(n>5) %>%
  as_tbl_graph()

ggraph(bigram_network, layout="fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name),vjust=1,hjust=1)





