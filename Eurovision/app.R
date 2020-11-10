library(shiny)
library(sp)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Eurovision Song Contest"),
    
    shinyWidgets::setBackgroundColor(
      color = pallete[c(8:10)],
      gradient = "linear",
      direction = c("right", "bottom")
    ),
  
    
    tags$style(HTML(" 
        .navbar { background-color:#4045A0;}
        .navbar-default .navbar-nav > li > a {color:#C1A2DB;}
        ")
    ),
    

    
    navbarPage("", collapsible = T, footer = "FIT5147: Communicating with Data", 
               header = "",
               tabPanel("Votes & Alliances", icon = icon("globe"),
                        titlePanel("Impact of geographic location in point accumulation"),
                        
                        
                          fluidRow(
                            column(4, offset = 3, selectInput("year", "Select Year",  unique(data$year))),
                            column(3, uiOutput("secondSelection"))
                            ),
                          mainPanel(leafletOutput("votes", height = "800px", width = "1600px")
                                    )
                                    
                          
                        
                        
                        ),
               
               tabPanel(title = "Lyrics Exploration",
                        titlePanel("Common word in Song Lyrics")),
               tabPanel(title = "Jury vs Tele-voters",
                        titlePanel("Are Judges & Tele-voters voting cretiria simillar?"))
               
               )

  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$secondSelection <- renderUI({
    selectInput("country", "Select Country:", choices = sort(unique(data$to_country[data$year == input$year
                                                                                    & data$event == "f"])))
  })
  

  df <- reactive({
    data %>% 
    filter(year == input$year,
           to_country == input$country,
           points != 0,
           vote == "televoters",
           event == "f") %>% 
    select(c(from_country, to_country, points, from_region))
  })
  
  
  meta <- reactive({
    
    df() %>% 
    left_join(loc, by = c("from_country" = "region")) %>% 
    rename(name = from_country) %>% 
    select(-c(to_country, points)) %>% 
    add_row(name = input$country, 
            lon = loc$lon[loc == input$country],
            lat = loc$lat[loc == input$country])
  })
  
  
  
  g <- reactive({
    graph.data.frame(df(), 
                        vertices = meta() )
  })
  
  lo <- reactive({
    layout.norm(as.matrix(meta()[,2:3]))
  })
  
  
  gg <- reactive({
    get.data.frame(g(), "both")
  })
  
  
   vert <-  reactive({
     gg()$vertices 
      })
  
  #### coordinates

   coords <- reactive({
     data.frame(
     lon = vert()$lon ,
     lat = vert()$lat
   )
     })
   
   dat <-  reactive({
     data.frame(name = vert()$name)
   })
  
   
   
   vert2  <- reactive({
     SpatialPointsDataFrame(coords(), dat())
   })
   
   
   edges <- reactive({
     gg()$edges
   })
   
   edges2 <- reactive({
     lapply(1:nrow(edges()), function(i) {
     as(rbind(vert2()[vert2()$name == edges()[i, "from"], ],
              vert2()[vert2()$name == edges()[i, "to"], ]),
        "SpatialLines")
     })
   })
   
   reactive({
   for (i in seq_along(edges2())) {
     edges2()[[i]] <- spChFIDs(edges2()[[i]], as.character(i))
   }
   })
   
   edges3 <-  reactive({
     do.call(rbind, edges2())
   })
   
   
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
   
  
  
   
  output$votes <- renderLeaflet({
    
    leaflet(df()) %>% 
      addProviderTiles("Esri") %>% 
      addMarkers(data = filter(vert(), name != input$country),
                 label = ~location_label(name),
                 icon = list(
                   iconUrl = 'https://icons.iconarchive.com/icons/raindropmemory/in-spirited-we-love/128/Music-2-icon.png',
                   iconSize = c(50, 50)
                 )) %>%
      addPolylines(data = edges3(), weight = 2 * df()$points, popup = edge_label(df()$points),
                   color = case_when(
                     df()$from_region == "eastern_europe" ~ "#CC79A7",
                     df()$from_region == "western_europe" ~ "#56B4E9",
                     df()$from_region == "southern_europe" ~ "#009E73",
                     df()$from_region == "northern_europe" ~ "#D55E00",
                     df()$from_region == "rest_of_the_world" ~ "#F0E442",
                     df()$from_region == "former_countries" ~ "#947D88",
                   )
      ) %>% 
      addMarkers(data = filter(loc, region == input$country), 
                 icon = list(
                   iconUrl = 'https://icons.iconarchive.com/icons/mcdo-design/cats-2/128/Music-icon.png',
                   iconSize = c(60, 60)
                 ), label = ~location_label(region)) %>% 
      addLegend(position = "bottomleft" ,colors = c("#CC79A7", "#56B4E9", "#009E73", "#D55E00", "#F0E442", "#947D88"),
                labels = c("Eastern Europe", "Western Europe", "Southern Europe",
                           "Northern Europe", "Rest of the world", "Former Countries"))
       
    
  })
  
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
