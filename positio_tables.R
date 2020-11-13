library(tidyverse)


country_points_each_year <- data %>%
  filter(event == "f") %>%
  group_by(year, to_country, to_region, decade) %>%
  summarise(total_points = sum(points)) %>%
  ungroup()


position_table  <- country_points_each_year %>%
  group_by(year) %>%
  arrange(desc(total_points)) %>%
  dplyr::mutate(position = row_number(),
                gap = total_points[1] - total_points) %>%
  ungroup()





data <- read.csv("data/data.csv") %>% 
  as_tibble()


unique(data$vote)


## Let's store in an object only the last three years where jury voting was introduced
## and keep only the votes of the finals,  maybe later we wiil also explore semifinals


jury_tele <-  data %>% 
  filter(year > 2015,
         event == "f")  

total_points_jury_tele <- data %>% 
  filter(year > 2015,
         event == "f")  %>% 
  group_by(year, vote, to_country) %>% 
  summarise(total_points = sum(points)) 

df <- total_points_jury_tele %>% 
  filter(year == "2016")
  





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
  left_join(tele_position, by = "to_country", suffix = c("jury", "tele")) %>% 
  left_join(filter(rankings, year == 2016)) %>% 
  select(c(1:3,6)) %>% 
  arrange(position)


 join_position %>% 
  mutate(to_country = ifelse(positionjury != positiontele ,
                    paste0('<span style="background-color:#ccccff">', to_country, '</span>'), 
                    ifelse(positionjury == positiontele, paste0('<span style="background-color:#ff9999">', to_country, '</span>'), 
                            positiontele)),
         # c = ifelse(c < min , 
         #            paste0('<span style="background-color:#ccccff">', c, '</span>'), 
         #            ifelse( c > max,  paste0('<span style="background-color:#ff9999">', c, '</span>'), 
         #                    c))
         ) %>% 
  `[`(1:4) %>%
  tableHTML(escape = FALSE, rownames = FALSE, 
            widths = rep(350, 4),
            headers = c("Country", "Jury Position", "Televoters Position", "Final"),
            caption = "asdasda",
            footer = "footer goes here",
            border = 4,
            collapse = "separate_shiny",
            spacing = '5px 4px'
            ) %>%
  add_css_header(css = list("text-align", "center"), 
                 headers = 1:4)  %>% 
  add_css_column(css = list("text-align", "center"), 
                 columns = 1:4)  
x





  total_points_jury_tele  %>% 
    filter(year == 2016) %>% 
    ggplot(aes(fct_reorder(to_country, -total_points), total_points, fill = vote)) +
    geom_col() +
    ggthemes::theme_solarized_2() +
    ggthemes::scale_fill_hc() +
    theme_classic() +
    theme(
      axis.text.x =  ggtext::element_markdown(angle = 90, face = "italic")
    ) +
    labs(
      x = "",
      y = "Total Points",
      fill = "Vote Origin"
    )
