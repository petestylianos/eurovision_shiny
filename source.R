library(tidyverse)
library(igraph)
library(ggraph)

data <- read.csv("data/data.csv")


loc <- read.csv("data/coordinates.csv") %>% 
  select(-c("X", "X.1"))

loc <- loc %>% 
  add_row(region = "Australia" , lon = 151.209900 , lat = -33.865143)


pacman::p_load(dplyr, colorfindr)

# Ensure reproducibility
set.seed(123)


pallete <-  get_colors("https://img.over-blog-kiwi.com/0/94/14/56/20170422/ob_d5632b_wsi-imageoptim-lights.jpg") %>% 
  make_palette(n = 10)

