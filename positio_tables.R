library(tidyverse)


# country_points_each_year <- data %>% 
#   filter(event == "f") %>% 
#   group_by(year, to_country, to_region, decade) %>% 
#   summarise(total_points = sum(points)) %>% 
#   ungroup() 
# 
# 
# position_table  <- country_points_each_year %>%
#   group_by(year) %>% 
#   arrange(desc(total_points)) %>% 
#   dplyr::mutate(position = row_number(),
#                 gap = total_points[1] - total_points) %>% 
#   ungroup()





data <- read.csv("data/data.csv") %>% 
  as_tibble()


unique(data$vote)


## Let's store in an object only the last three years where jury voting was introduced
## and keep only the votes of the finals,  maybe later we wiil also explore semifinals


jury_tele <-  data %>% 
  filter(year > 2015,
         event == "f")  

total_points_jury_tele <-  jury_tele %>% 
  group_by(year, vote, to_country) %>% 
  summarise(total_points = sum(points)) 

df <- total_points_jury_tele %>% 
  filter(year == "2016")
  

install.packages("tableHTML")
library(tableHTML)
library(dplyr)

a <- c('A', 'B', 'C', 'D', 'E')
b <- c(20, 25, 40, 55, 60)
c <- c(60, 30, 80, 50, 60)
min <- c(15, 20, 40, 55, 55)
max <- c(25, 30, 50, 65, 65)


df <- data.frame(a, b, c, min, max)



jury_position <- total_points_jury_tele %>% 
  filter(year == "2016") %>% 
  arrange(desc(total_points)) %>% 
  pivot_wider(names_from = c(vote),
              values_from = total_points) %>% 
  arrange(desc(jury)) %>% 
  ungroup() %>% 
  mutate(position = 1:n()) %>% 
  select(to_country, position)
  

tele_position <- total_points_jury_tele %>% 
  filter(year == "2016") %>% 
  arrange(desc(total_points)) %>% 
  pivot_wider(names_from = c(vote),
              values_from = total_points) %>% 
  arrange(desc(televoters)) %>% 
  ungroup() %>% 
  mutate(position = 1:n()) %>%
  select(to_country, position)


join_position <- jury_position %>% 
  left_join(tele_position, by = "to_country", suffix = c("jury", "tele"))


join_position %>% 
  mutate(to_country = ifelse(positionjury != positiontele ,
                    paste0('<span style="background-color:#ccccff">', to_country, '</span>'), 
                    ifelse( positionjury == positiontele, paste0('<span style="background-color:#ff9999">', to_country, '</span>'), 
                            b)),
         # c = ifelse(c < min , 
         #            paste0('<span style="background-color:#ccccff">', c, '</span>'), 
         #            ifelse( c > max,  paste0('<span style="background-color:#ff9999">', c, '</span>'), 
         #                    c))
         ) %>% 
  `[`(1:3) %>%
  tableHTML(escape = FALSE, rownames = FALSE, 
            widths = rep(50, 3))

