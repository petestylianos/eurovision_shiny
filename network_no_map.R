library(tidyverse)
library(igraph)
library(ggraph)

data <- read_csv("data/data.csv")


data %>% 
  filter(year == 2010,
         event == FALSE,
  to_country == "Greece",
  points != 0) %>% 
  arrange(points) %>% 
  select(from_country, to_country, points, from_region) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = points , edge_width = points, 
                 edge_color = from_region)) +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.15, "lines")) +
  theme_void() +
  labs(
   edge_color = "" 
  )


