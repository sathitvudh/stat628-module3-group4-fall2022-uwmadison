library(tidyverse)
library(tm)
library(wordcloud)
library(SentimentAnalysis)
library(tidytext)
library(dplyr)
library(SnowballC)
library(ggwordcloud)
library(sqldf)

tip = read.csv("bars_tip_ca.csv")
tip

#Made Tidy data
tidy_tips = tip %>%
  select(stars, text) %>%
  unnest_tokens("word", text)
head(tidy_tips)

#Remove Stop words
data("stop_words")
tidy_tips<-tidy_tips %>%
  anti_join(stop_words)

#Remove numbers
tidy_tips = tidy_tips[-grep("\\b\\d+\\b", tidy_tips$word)]

#Remove whitespace
tidy_tips$word <- gsub("\\s+","",tidy_tips$word)

#Stem words
tidy_tips<-tidy_tips %>%
  mutate_at("word", funs(wordStem((.), language="en")))

#Word Count
tips_count = tidy_tips %>% count(stars, word, sort=TRUE)
tips_count$stars = as.factor(tips_count$stars)


#Plot Hisograms of top words
tips_count %>%
  slice(1:15) %>%
  ggplot(mapping = aes(n,reorder(word,n)))+
  geom_col(stat="identity")


# Frequency for Happy Hour
tip %>% unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:15) %>% 
  ggplot() + geom_col(aes(n,reorder(word,n)), stat = "identity", fill = "#de5833") 

