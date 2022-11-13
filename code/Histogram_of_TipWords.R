library(tidyverse)
tips = read_csv("./data/Count_for_Tips.csv")
head(tips)

#pdf("Tip Word Histograms.pdf")
#par(mfrow=c(3,3))



tips %>%
  filter(word == "beer") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "#619CFF") +
  ggtitle("Beer") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

  
tips %>%
  filter(word == "wine") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "#619CFF") +
  ggtitle("Wine") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

tips %>%
  filter(word == "food") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "#619CFF") +
  ggtitle("Food") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))



tips %>%
  filter(word == "delici") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "lightcoral") +
  ggtitle("delici (Delicious)") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

tips %>%
  filter(word == "worst") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "lightcoral") +
  ggtitle("Worst") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

tips %>%
  filter(word == "rude") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "lightcoral") +
  ggtitle("Rude") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))



tips %>%
  filter(word == "servic") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "darkslategray4") +
  ggtitle("servic (Service)") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

tips %>%
  filter(word == "view") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "darkslategray4") +
  ggtitle("View") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))

tips %>%
  filter(word == "wait") %>%
  ggplot() +
  geom_col(aes(stars, n), fill = "darkslategray4") +
  ggtitle("wait") +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous(breaks=seq(1, 5, 0.5),limits = c(1, 5))



#dev.off()
