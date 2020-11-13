library(tidyverse)
library(igraph)
library(ggraph)

data <- read.csv("data/data.csv")


data %>% 
  mutate(from_region =
    str_replace_all(from_region, "eastern_europe", "Eastern Europe"),
    from_region =
      str_replace_all(from_region, "western_europe", "Western Europe"),
    from_region =
      str_replace_all(from_region, "northern_europe", "Northern Europe"),
    from_region =
      str_replace_all(from_region, "southern_europe", "Southern Europe"),
    from_region =
      str_replace_all(from_region, "former_countries", "Former Countries"),
    from_region =
      str_replace_all(from_region, "rest_of_the_world", "Rest of the World"),
  ) %>% 
  filter(event == "f",
         to_country == "Yugoslavia",
         vote == "televoters") %>% 
  group_by(from_country, from_region, to_country, to_region) %>% 
  summarise(total_points_received = sum(points, na.rm = T)) %>% 
  arrange(desc(total_points_received)) %>% 
  select(from_country, to_country, total_points_received, from_region, to_region) %>% 
  head(4) %>% 
  graph_from_data_frame() %>% 
  ggraph(layout = "kk") +
  geom_edge_link(aes(edge_width = total_points_received, 
                 edge_color = from_region,
                 label = total_points_received),
                 label_colour = "brown",
                 label_parse = T,
                 check_overlap = T,
                 
                 ) +
  geom_node_point(size = 5,  color = "black") +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.15, "lines")) +

  theme_void() + 
  guides(edge_color = guide_legend(override.aes = list(edge_width = 5)),
         edge_width = F) +
  theme(
    plot.background = element_rect(fill="plum")
    
  ) +
  labs(
   edge_color = "Region" 
  ) 


