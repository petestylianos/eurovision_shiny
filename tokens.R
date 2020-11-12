## SENTIMENT 

library(rvest)
library(polite)
library(tidytext)


text2 <- scrape_lyrics("https://4lyrics.eu/esc/1999/the-mullans-when-you-need-me/")

text2 <- text2 %>% mutate(year = 1999, position = "first", country = "Sweeden", song = "Take Me to Your Heaven")

text2 <- text2 %>% mutate(year = 1999, position = "last", country = "Ireland", song = "When You Need Me")


all <- bind_rows(all, text2)

write.csv(all, file = "data/tokens.csv")

all <- read.csv("data/tokens.csv")

all %>% 
  select(-X)



# 2007 and 2017 different number 1



all %>% 
  filter(year == 1999) %>% 
  count(country)



stopwords_smart <- get_stopwords(source = "smart")

sentiments_bing <- get_sentiments("nrc")


all %>% 
  filter(year == 2018) %>% 
  anti_join(stopwords_smart) %>% 
  inner_join(sentiments_bing) %>% 
  count(sentiment, word, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(word, n), n, fill = sentiment)) + geom_col() +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free") + theme_minimal() +
  labs(
    title = "Sentiments in user reviews",
    x = "" ) 





sentiments <- all %>% 
  filter(year == 2018) %>% 
  anti_join(stopwords_smart) %>% 
  inner_join(sentiments_bing) %>% 
  count(sentiment, word, position, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  group_by(position, sentiment) %>%
  top_n(10) %>%
  ungroup() 

emotions <- sentiments %>%
  group_by(position, sentiment) %>%
  summarise(appearence = sum(n)) %>% 
  ungroup() %>% 
  mutate(emotion_index = appearence/sum(appearence)) %>% 
  arrange(desc(emotion_index))  


emotions  %>% 
  ggplot(aes(sentiment, appearence, fill = sentiment, label = paste(round(emotion_index,2),"%"))) +
  geom_col() +
  facet_wrap(~position) +
  geom_label(size =8, color = "black", label.size = 0.5) +
  scale_fill_discrete(type = c("orange", "purple", "darkred", "darkgreen",
                               "grey", "pink", "blue"))  +
  labs(
    title = "Emotions observed in user reviews",
    y = "Number of appearences"
  ) +
  theme(
    panel.background = element_rect(fill = "#bad2e3"),
    plot.background = element_rect(fill = "#bad2e3"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26, color = "navy",face = "bold", margin = unit(c(0, 0, 0.6, 0), "cm")),
    legend.background = element_rect(fill = "#bad2e3"),
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text( size = 16, face = "bold" ),
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 18, color = "#1f78b4"),
    plot.subtitle = element_text(size = 23, color = "#1f78b4", face = "italic", margin = unit(c(0, 0, 1, 0), "cm") ),
    axis.title.x   = element_blank(),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.line.x = element_line(linetype = "dashed", size = 2),
    axis.line.y = element_line(linetype = "dashed", size = 2),
    axis.text.x = element_text(size = 19, color = "black"),
    axis.text.y = element_text(size = 19, color = "black"),
    strip.text.x = element_text(
      size = 21, color = "navy", face = "bold.italic"),
    strip.background = element_rect(
      color="grey91", fill="#9fb6cd", size=1.5, linetype="solid"),
    legend.position = "none"
  ) 






sentiments <- all %>% 
  filter(year == 2018) %>% 
  anti_join(stopwords_smart) %>% 
  inner_join(sentiments_bing) %>% 
  count(position, sentiment, word, sort = TRUE) %>% 
  arrange(desc(n)) %>%
  group_by(sentiment) %>%
  ungroup() 

emotions <- sentiments %>%
  group_by(position, sentiment) %>%
  summarise(appearence = sum(n)) %>% 
  ungroup() %>% 
  mutate(emotion_index = appearence/sum(appearence)) %>% 
  arrange(desc(emotion_index))

encouraging <- emotions %>% 
  group_by(position) %>% 
  filter(sentiment %in% c("positive", "trust", "joy")) %>% 
  summarise(emotion = sum(emotion_index)) %>%
  pull(emotion)

anxious <- emotions %>% 
  group_by(position) %>% 
  filter(sentiment %in% c("surprise", "aticipation", "fear")) %>% 
  summarise(emotion = sum(emotion_index)) %>% 
  pull(emotion)

unpleasant <- emotions %>% 
  group_by(position) %>% 
  filter(sentiment %in% c("anger", "disgust", "sadness", "negative")) %>% 
  summarise(emotion = sum(emotion_index)) %>% 
  pull(emotion)


emotions_group <- tibble(encouraging,
                         anxious,
                         unpleasant) %>% 
  pivot_longer(cols = 1:3, names_to = "emotion", values_to = "value")

library(patchwork)


emotions_group[1:3,]  %>% 
  ggplot(aes(emotion, value, fill = emotion, label = paste(round(value,2),"%"))) +
  geom_col() +
  geom_label(size =12, color = "black", label.size = 0.5) +
  scale_fill_discrete(type = c("orange", "darkgreen", "darkred")) +
  labs(
    title = "Emotions observed in winning song",
    y = "Frequence of appearence"
  ) +
  theme(
    panel.background = element_rect(fill = "#bad2e3"),
    plot.background = element_rect(fill = "#bad2e3"),
    plot.title.position = "plot",
    text = element_text(size = 16),
  ) +
  theme(
    panel.background = element_rect(fill = "#bad2e3"),
    plot.background = element_rect(fill = "#bad2e3"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26, color = "navy",face = "bold", margin = unit(c(0, 0, 0.6, 0), "cm")),
    legend.background = element_rect(fill = "#bad2e3"),
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text( size = 16, face = "bold" ),
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 18, color = "#1f78b4"),
    plot.subtitle = element_text(size = 23, color = "#1f78b4", face = "italic", margin = unit(c(0, 0, 1, 0), "cm") ),
    axis.title.x   = element_blank(),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.line.x = element_line(linetype = "dashed", size = 2),
    axis.line.y = element_line(linetype = "dashed", size = 2),
    axis.text.x = element_text(size = 19, color = "black"),
    axis.text.y = element_text(size = 19, color = "black"),
    strip.text.x = element_text(
      size = 21, color = "navy", face = "bold.italic"),
    strip.background = element_rect(
      color="grey91", fill="#9fb6cd", size=1.5, linetype="solid"),
    legend.position = "none"
  ) +
  emotions_group[4:6,]  %>% 
  ggplot(aes(emotion, value, fill = emotion, label = paste(round(value,2),"%"))) +
  geom_col() +
  geom_label(size =12, color = "black", label.size = 0.5) +
  scale_fill_discrete(type = c("orange", "darkgreen", "darkred")) +
  labs(
    title = "Emotions observed in last placed song",
    y = "Frequence of appearence"
  ) +
  theme(
    panel.background = element_rect(fill = "#bad2e3"),
    plot.background = element_rect(fill = "#bad2e3"),
    plot.title.position = "plot",
    text = element_text(size = 16),
  ) +
  theme(
    panel.background = element_rect(fill = "#bad2e3"),
    plot.background = element_rect(fill = "#bad2e3"),
    plot.title.position = "plot",
    plot.title = element_text(size = 26, color = "navy",face = "bold", margin = unit(c(0, 0, 0.6, 0), "cm")),
    legend.background = element_rect(fill = "#bad2e3"),
    strip.text = element_text(size = 20),
    axis.text = element_text(size = 15),
    axis.title = element_text( size = 16, face = "bold" ),
    panel.spacing = unit(2, "lines"),
    plot.caption = element_text(size = 18, color = "#1f78b4"),
    plot.subtitle = element_text(size = 23, color = "#1f78b4", face = "italic", margin = unit(c(0, 0, 1, 0), "cm") ),
    axis.title.x   = element_blank(),
    axis.title.y = element_text(size = 20, color = "black"),
    axis.line.x = element_line(linetype = "dashed", size = 2),
    axis.line.y = element_line(linetype = "dashed", size = 2),
    axis.text.x = element_text(size = 19, color = "black"),
    axis.text.y = element_text(size = 19, color = "black"),
    strip.text.x = element_text(
      size = 21, color = "navy", face = "bold.italic"),
    strip.background = element_rect(
      color="grey91", fill="#9fb6cd", size=1.5, linetype="solid"),
    legend.position = "none"
  ) 







library(wordcloud2)


lyrics_freq <- all %>%
  filter(year == 2018 & position == "first") %>% 
  select(word, frequency) %>% 
  anti_join(stopwords_smart) 
  

wordcloud2(lyrics_freq, size=1.3, color='random-light', backgroundColor="black") 

  

