library(igraph)
library(tidyverse)

## User will input year, country
## df need to be reactive

## after 2015 specify event == "f", vote == "televoters"


df <- data %>% 
  filter(year == "2018",
         to_country == "Australia",
         points != 0,
         vote == "televoters",
         event == "f") %>% 
  select(c(from_country, to_country, points))


# loc <- read.csv("data/coordinates.csv") %>% 
#   select(-c("X", "X.1"))
# 
# loc <- loc %>% 
#   add_row(region = "Australia" , lon = 151.209900 , lat = -33.865143)



## Locations in the map

meta <- df %>% 
  left_join(loc, by = c("from_country" = "region")) %>% 
  rename(name = from_country) %>% 
  select(-c(to_country, points)) %>% 
  add_row(name = "Australia", 
          lon = loc$lon[loc == "Australia"],
          lat = loc$lat[loc == "Australia"])


g <- graph.data.frame(df, 
                      vertices=meta)

## Wont work if NA's occur
lo <- layout.norm(as.matrix(meta[,2:3]))


library(sp)
gg <- get.data.frame(g, "both")

vert <- gg$vertices

#coordinates(vert) <- ~ lon + lat 

coords = data.frame(
  lon = vert$lon ,
  lat = vert$lat
)

dat=data.frame(name = vert$name)


vert2 = SpatialPointsDataFrame(coords, dat)


edges <- gg$edges

edges2 <- lapply(1:nrow(edges), function(i) {
  as(rbind(vert2[vert2$name == edges[i, "from"], ],
           vert2[vert2$name == edges[i, "to"], ]),
     "SpatialLines")
})


for (i in seq_along(edges2)) {
  edges2[[i]] <- spChFIDs(edges2[[i]], as.character(i))
}

edges3 <- do.call(rbind, edges2)

location_label <- function(loc){
  paste0(
    "",loc
  )
}

edge_label <- function(weight){
  paste0(
    "Points: ",weight
  )
}





library(leaflet)
leaflet(df) %>% 
  addProviderTiles("Esri") %>% 
  addMarkers(data = vert2, popup = ~location_label(name)) %>%
  addPolylines(data = edges3, weight = 2 * df$points, popup = edge_label(df$points),
               color = case_when(
                 df$from_region == "eastern_europe" ~ "purple",
                 df$from_region == "western_europe" ~ "blue",
                 df$from_region == "southern_europe" ~ "green",
                 df$from_region == "northern_europe" ~ "brown",
                 df$from_region == "rest_of_the_world" ~ "black",
                 df$from_region == "former_countries" ~ "grey",
               )
  ) %>% 
  addMarkers(data = filter(loc, region == "Australia" ), 
             icon = list(
               iconUrl = 'http://icons.iconarchive.com/icons/artua/star-wars/128/Master-Joda-icon.png',
               iconSize = c(75, 75)
             )) %>% 
  addLegend(colors = c("purple", "blue", "green", "brown", "black", "grey"),
            labels = c("eastern_europe", "western_europe", "southern_europe",
                       "northern_europe", "rest_of_the_world", "former_countries"))


