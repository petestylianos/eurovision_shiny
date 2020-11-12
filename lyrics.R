### Let's now explore the lyrics 

scrape_lyrics <- function(url) {
  # Check if the site is scrapable
  song <- bow(url) %>% scrape
  
  # Obtain the lyrics
  lyrics <- song %>% 
    html_nodes("p:nth-child(10) , #page_body :nth-child(9), p:nth-child(8), p:nth-child(7), #page_body p:nth-child(6), #page_body p:nth-child(5)") %>% 
    html_text()
  
  # In this format we will have a string that contains all words. 
  # Let's identify common special characters included in the lyrics such as: !, "", ?, \n, (, )
  # and remove them
  
  lyrics <- str_replace_all(lyrics, pattern = "\n", replacement = " ")
  lyrics <- str_replace_all(lyrics, pattern = "\r", replacement = "")
  lyrics <- str_replace_all(lyrics, pattern = "'", replacement = " ")
  lyrics <- str_replace_all(lyrics, pattern = "\\?", replacement = "")
  lyrics <- str_replace_all(lyrics, pattern = "\\(", replacement = "")
  lyrics <- str_replace_all(lyrics, pattern = "\\)", replacement = "")
  lyrics <- str_replace_all(lyrics, pattern = ",", replacement = "")
  
  # Now lets split our long string to get every word
  lyrics <- stringr::str_split(lyrics, pattern = " ")
  
  # Unlist the strings
  lyrics <- unlist(lyrics)
  
  # and remove whitespace
  lyrics <- str_trim(lyrics)
  
  # Finally let's remove common words from our list using the stop_words from 
  # the tidytext package
  
  # and create a frequency tibble 
  
  lyrics %>% 
    as_tibble() %>% 
    unnest_tokens(word, value) %>%
    anti_join(stop_words) %>% 
    count(word, name = "frequency") %>% 
    arrange(desc(frequency))
  
}

# Now we want to create a list that contains all winning songs url's, to pass them in 
# the lyrics_scrape function

# We have a table that contains the winning songs name's

win_songs <- readxl::read_xlsx(here::here("data/eurovision_data.xlsx")) %>% 
  clean_names() %>% 
  select(c(performer, song)) %>% 
  mutate(url = str_trim(paste("http://www.songlyrics.com/", performer, "/", song, "-lyrics/", sep = "" ),
  ))

# The urls we want to create should be of this format:
# "http://www.songlyrics.com/Performer-First-Last-Name/Song-Title-lyrics/"
# e.g "http://www.songlyrics.com/Emmelie-De-Forest/Only-Teardrops-lyrics/"

# Let's replicate the above url for all songs in our table


#song_url <- "http://www.songlyrics.com/Emmelie De Forest/Only Teardrops-lyrics/"

list_of_lyrics <- map(win_songs$url, possibly(scrape_lyrics, otherwise = NA_character_))

list_of_lyrics[[2]]


na.omit.list <- function(y) { return(y[!sapply(y, function(x) all(is.na(x)))]) }

list_of_lyrics <- na.omit.list(list_of_lyrics)


extractColumns = function(df){
  df %>% 
    select(word, frequency)
  
}

final_df <-  lapply(list_of_lyrics,extractColumns) %>% 
  bind_rows() %>% 
  mutate(word = as_factor(word)) %>% 
  group_by(word) %>% 
  mutate(occurence = sum(frequency)) %>% 
  distinct(word, occurence) %>% 
  arrange(desc(occurence))


final_df <- final_df %>% 
  ungroup() %>% 
  filter(str_length(final_df$word) > 3)
lyrics_freq <- read.csv("data/lyrics_freq.csv") %>% 
  as_tibble()
lyrics_freq %>% 
  arrange(desc(occurence)) %>% 
  head(10) %>% 
  ggplot(aes(reorder(word, occurence), occurence)) +
  geom_col(fill = "darkcyan") +
  coord_flip() +
  my_theme() +
  theme(
    axis.title.y = element_blank()
  ) +
  xlab("Occurence") +
  labs(
    title = "Popular words",
    subtitle = "Love is in the air",
    caption = "Between 1975 - 2019"
  )
wordcloud2(lyrics_freq, size=2.5, color='random-light', backgroundColor="black")





all 