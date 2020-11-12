# Define UI for application that draws a histogram


ui <- fluidPage(

    # Application title
  titlePanel(windowTitle = "Eurovision Song Contest",
             title = fluidRow(
               HTML('<span style="color:#CFE2F0;
               font-size: 40px;
               font-style: italic;
               text-shadow: 1px 1px 2px black, 0 0 25px blue, 0 0 5px darkblue;
               font-weight:bold;">Eurovision Song Contest<span>'), 
               align = "center")
  ),
    
    shinyWidgets::setBackgroundColor(
      color = c("#7178D2","#C1A2DB", "#4045A0"),
      gradient = "radial",
      direction = c("right", "bottom")
    ),

    tags$style(HTML(" 
        .navbar { background-color:#4045A0;}
        .navbar-default .navbar-nav > li > a {color:#C1A2DB;}
        ")
    ),
    
    tags$style(HTML(" 
a:link {
  color: #50C878;
  background-color: transparent;
  text-decoration: none;
}
                                                                  
a:visited {
  color: #9ACD32;
  background-color: transparent;
  text-decoration: none;
}

a:hover {
  color: red;
  background-color: transparent;
  text-decoration: wavy;
}
 ")),

    navbarPage("", collapsible = T, footer =  HTML('<span style="
                            font-size: 20px; 
                            font-weight:bold;">
                            <a href="https://www3.monash.edu/pubs/2019handbooks/units/FIT5147.html" target="_blank">
                            FIT5147: Communicating with Data</a><span>'),
               header = "",
               tabPanel("Introduction", icon = icon("home"),
                        fluidRow(
                          column(width = 6, 
                                 HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">
                                 Welcome to the <strong><span style="color:pink">Eurovision Song Contest !!</span></strong>, 
                                 or at least its <span style="font-style:italic;color:pink">Shiny App version</span>.
                                 <br><br>

                                 This Shiny app allows users to interactively explore data related to the
                                 <strong>Eurovision Song Contest,</strong> which is an annual international singing competition, 
                                 with participants representing primarily European countries. <br><br>
                                 The competition begun in 1956 with just 7 participating countries and since then it has evolved to 
                                 include 42 countries as of 2019, with Australia actively participating since 2016.<br><br>
                                 Nowadays, the competition consists of two semi-finals and a final. It is estimated that approxiamtely
                                 182 million viewers watched the last final held in Israel in 2019.<br><br>
                                 
                                 
                                 
                                 Unfortunately, 2020 was the first time since 1956 that the song contest was cancelled and 
                                 this app offers Eurovision fans the ability to recreate all previous voting procedures, while at the same time 
                                 exploring different ascpects of the competition and gaining valuable insight.
                                 
                                 <br><br>
                                 
                                 The app is structured arround answering the following questions:
                                 <br><br>
                                 <ul>
                                 <span style="font-style:bold;color:#4B0082;font-size:20px"><li>What role does the geographic location of a country play in its point accumulation?</li></span>
                                 <span style="font-style:bold;color:#4B0082"><li>What are the most common words used in song lyrics?</li></span>
                                 <span style="font-style:bold;color:#4B0082"><li>Are the judges and televoters voting criteria similar?</li></span>
                                 </ul>

                                 <br>
                                 Use the the navigation bar at the top to browse each section. 
                                 ')
                                 
                                 ),
                          column(width = 3, 
                        tags$image(src = "https://wallpapercave.com/wp/wp4346945.jpg", width="820", height="420")
                          )
                        ),
                        br(),
                        br(),
                        br(),
                        
                        HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">Fun Fact: In 2015 the Eurovision Song Contest was recognised by the Guinness Book of World Records as the Longest Running Annual TV Music Competition.
                        '),
                        br(),
                        br(),
                        HTML('
                             Source: <a href = "https://eurovision.tv/about/facts-and-figures">Eurovision Facts & Figures</a>
                             '),
                        br(),
                        br(),
                        ),
               tabPanel("Votes & Alliances", icon = icon("globe"),
                        titlePanel(
                          HTML('<span style="
                            font-size:28px; 
                            font-weight:bold;
                            padding-left: 350px;
                            color:pink">
                          What is the impact of geographic location in point accumulation?</span>
                          ')
                          ),
                        br(),
                        br(),
                        HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">
                        The following two visualizations will try to answer the above question.
                        The hypothesis is that countries located close to each other will tend to favorite their neighbors more 
                        than countries that are located furthest. This hypothesis was confirmed in the Exploration Project and the 
                        following figures illustrate this finding. <br><br>
                        The interactive leaflet map recreates all past voting procedures. By selecting your preferred year and country 
                        from the select input boxes the map updates to indicate from which countries the selected country received 
                        its points. <br><br>
                        </span>
                        '),
                        HTML('<span style="color:pink;font-size: 22px; font-weight:bold;">
                           How to interpret the visualization ?</span>
                        '),
                        br(),
                        HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">
                        The map groups countries by their location and attributes a color to each region. Then a line connects the 
                        voting country and the recepient country, with the width of the line being proportional to the amount of points 
                        given (1 to 12).<br><br>
                        By exploring different combinations of year and country it is revealed that the majority of
                        votes for a given country originates from votes given by neighbor countries. This phenomenon
                        can be particularly revealed in years where the chosen country received a low rank. Since, a
                        low rank can be justified from the fact that the song was not of the publics appeal, however 
                        it can be seen that in this circumnsatnce the country received most of its not many points from
                        neighbor countries.
                        </span>
                        '),
                        br(),
                        br(),
                        br(),
                          fluidRow(
                            column(4, offset = 3, selectInput("year", "Select Year",  unique(data$year))),
                            column(3, uiOutput("secondSelection"))
                            ),
                        fluidRow(
                          mainPanel(leafletOutput("votes", height = "700px", width = "1600px")
                                    )
                          ),
                        br(),
                        br(),
                        br(),
                        br(),
                        tags$style(".well {border: 2px solid;background-color:lightblue;}"),
                        tags$style(".option , #secondSelection .selectize-dropdown-content {color:#023D89;font-size:16px}"),
                        tags$style(".leaflet-control {border: 5px solid;background-color:lightblue;}"),
                        tags$style(".fa-fw {color:lightblue;}"),
                        
                        HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">
                           This network</span>
                        '),
                        
                        br(),
                        br(),
                        br(),
                        
                        
                        HTML('<span style="color:pink;font-size: 22px; font-weight:bold;">
                           How to interpret the visualization ?</span>
                        '),
                        br(),
                        br(),
                        br(),

                        sidebarLayout(
                          
                          
                        sidebarPanel(width = 3,
                          sliderInput("top", "Select Nubmer", min = 1, max = 12, value = 5, )
                         ),
                        mainPanel(
                          plotOutput("network", height = "600px", width = "1100px")
                          )
                        ),
                        br(),
                        br(),
                        HTML('<span style="color:#CFE2F0;font-size: 20px; font-weight:normal;">Fun Fact: Over 1,500 songs have taken part in the Eurovision Song Contest. 
                        If you would listen to all the songs without a break, you would be sitting up for nearly 72 hours.
                        '),
                        
                        br(),
                        br(),
                        ),
               
               tabPanel(title = "Lyrics Exploration", icon = icon("music"),
                        titlePanel("Common word in Song Lyrics"),
                        tags$image(src = "wordcloud.png", width="820", height="420"),
                        HTML('
                             <figure>
    <figcaption>Listen to the T-Rex:</figcaption>
    <audio
        controls
        src="sound.mp3">
            Your browser does not support the
            <code>audio</code> element.
    </audio>
</figure>'),
                        
                        
                        
                        sliderInput("lyric_year", "select year", min = min(all$year), max(all$year), value = 2005),
                        plotOutput("sent"),
                        fluidRow(
                          column(4, offset = 1, wordcloud2Output("first")),
                          column(4, offset = 2, wordcloud2Output("last"))
                        )
                        
                        
                        ),
               tabPanel(title = "Jury vs Tele-voters", icon = icon("envelope"),
                        titlePanel("Are Judges & Tele-voters voting criteria simillar?"),
                        
                        tags$div("In this final section of the app the voting criteria of the judges and the
                                 televoters are being compared.",
                                 style = "color:#CFE2F0;font-size: 20px;font-weight:normal;"
                        ),
                        br(),
                        
                        fluidRow(
                        selectInput("vote_year", "Select Year",  choices = c(2016:2019))
                        ),
                        plotly::plotlyOutput("fill"),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        htmlOutput("pos")
                        ),
               tabPanel(title = "About", icon = icon("info"),
                        titlePanel("Information"),
                        
                        
               )
               
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
      addPolylines(data = edges3(), weight = 2 * df()$points, label = edge_label(df()$points),
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
  
  netword_data <-  data %>% 
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
    ) 
  
  
  output$network <- renderPlot({
    
    netword_data %>% 
      filter(event == "f",
             to_country == input$country,
             vote == "televoters") %>% 
      group_by(from_country, from_region, to_country, to_region) %>% 
      summarise(total_points_received = sum(points, na.rm = T))%>% 
      arrange(desc(total_points_received)) %>% 
      select(from_country, to_country, total_points_received, from_region, to_region) %>% 
      head(input$top) %>% 
      graph_from_data_frame() %>% 
      ggraph(layout = "fr") +
      geom_edge_link(aes(edge_width = total_points_received, 
                         edge_color = from_region,
                         label = total_points_received),
                     label_colour = "brown",
                     label_parse = T,
                     check_overlap = T
      ) +
      geom_node_point(size = 5,  color = "black", shape = 18) +
      geom_node_text(aes(label = name), repel = TRUE, size = 13,
                     point.padding = unit(0.15, "lines")) +
      theme_void() + 
      theme(
        plot.background = element_rect(fill="#7178D2")
      ) +
      labs(
        edge_color = "Region" 
      ) 
    
  })
  
  stopwords_smart <- get_stopwords(source = "smart")
  
  sentiments_bing <- get_sentiments("nrc")
  
  
  sentiments <- reactive({
    all %>% 
    filter(year == input$lyric_year) %>% 
    anti_join(stopwords_smart) %>% 
    inner_join(sentiments_bing) %>% 
    count(position, sentiment, word, sort = TRUE) %>% 
    arrange(desc(n)) %>%
    group_by(sentiment) %>%
    ungroup() 
  })
  
  emotions <- reactive({
    sentiments() %>%
    group_by(position, sentiment) %>%
    summarise(appearence = sum(n)) %>% 
    ungroup() %>% 
    mutate(emotion_index = appearence/sum(appearence)) %>% 
    arrange(desc(emotion_index))
  })
  
  encouraging <-  reactive({
    emotions() %>% 
    group_by(position) %>% 
    filter(sentiment %in% c("positive", "trust", "joy")) %>% 
    summarise(emotion = sum(emotion_index)) %>%
    pull(emotion)
  })
  
  
  anxious <-  reactive({
    emotions() %>% 
    group_by(position) %>% 
    filter(sentiment %in% c("surprise", "aticipation", "fear")) %>% 
    summarise(emotion = sum(emotion_index)) %>% 
    pull(emotion)
  })
  
  unpleasant <-  reactive({
    emotions() %>% 
    group_by(position) %>% 
    filter(sentiment %in% c("anger", "disgust", "sadness", "negative")) %>% 
    summarise(emotion = sum(emotion_index)) %>% 
    pull(emotion)
  })
  
  emotions_group <-  reactive({
    tibble(encouraging(), 
           anxious(),
           unpleasant()) %>% 
    pivot_longer(cols = 1:3, names_to = "emotion", values_to = "value")
  })
  
  output$sent <- renderPlot({
    
    emotions_group()[1:3,]  %>% 
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
      emotions_group()[4:6,]  %>% 
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
  })
  
  lyrics_freq <- reactive({
    all %>%
    filter(year == input$lyric_year & position == "first") %>% 
    select(word, frequency) %>% 
    anti_join(stopwords_smart)
  })
  
  output$first <- renderWordcloud2({
    wordcloud2(lyrics_freq(), size=1.3, color='random-light', backgroundColor="black") 
    
  })
  
  
  lyrics_freq2 <- reactive({
    all %>%
      filter(year == input$lyric_year & position == "last") %>% 
      select(word, frequency) %>% 
      anti_join(stopwords_smart)
  })
  
  output$last <- renderWordcloud2({
    wordcloud2(lyrics_freq2(), size=1.3, color='random-light', backgroundColor="black") 
    
  })
  
  
  
  
  total_points_jury_tele <- data %>% 
    filter(year > 2015,
           event == "f")  %>% 
    group_by(year, vote, to_country) %>% 
    summarise(total_points = sum(points))  
  
  
  output$fill <- renderPlotly({
    
    ggplotly(
      total_points_jury_tele  %>% 
        filter(year == 2016) %>% 
        ggplot(aes(fct_reorder(to_country, -total_points), total_points, fill = vote)) +
        geom_col() +
        ggthemes::theme_solarized_2() +
        ggthemes::scale_fill_hc() +
        theme_classic() +
        theme(
          axis.text.x =  ggtext::element_markdown(angle = 90, face = "italic"),
          panel.background = element_rect(fill = "lightblue"),
          plot.background = element_rect(fill = "lightblue")
        ) +
        labs(
          x = "",
          y = "Total Points",
          fill = "Vote Origin"
        )
      , height = 500
    )
  })
  
  jury_position <- reactive({
    total_points_jury_tele %>% 
    filter(year == input$vote_year) %>% 
    arrange(desc(total_points)) %>% 
    pivot_wider(names_from = c(vote),
                values_from = total_points) %>% 
    arrange(desc(jury)) %>% 
    ungroup() %>% 
    mutate(position = 1:n()) %>% 
    select(to_country, position)
  })
  
  
  tele_position <- reactive({
    total_points_jury_tele %>% 
    filter(year == input$vote_year) %>%  
    arrange(desc(total_points)) %>% 
    pivot_wider(names_from = c(vote),
                values_from = total_points) %>% 
    arrange(desc(televoters)) %>% 
    ungroup() %>% 
    mutate(position = 1:n()) %>%
    select(to_country, position)
  })
  
  join_position <- reactive({
    jury_position() %>% 
    left_join(tele_position(), by = "to_country", suffix = c("jury", "tele"))
  })
  
  
  output$pos <- render_tableHTML({
    join_position() %>% 
      mutate(to_country = ifelse(positionjury != positiontele ,
                                 paste0('<span style="background-color:#ccccff">', to_country, '</span>'), 
                                 ifelse( positionjury == positiontele, paste0('<span style="background-color:#ff9999">', to_country, '</span>'), 
                                         positiontele)),
      ) %>% 
      `[`(1:3) %>%
      tableHTML(escape = FALSE, rownames = FALSE, 
                widths = rep(350, 3),
                headers = c("Country", "Jury Position", "Televoters Position"),
                caption = "asdasda",
                footer = "footer goes here",
                border = 4,
                collapse = "separate_shiny",
                spacing = '5px 4px'
      ) %>%
      add_css_header(css = list("text-align", "center"), 
                     headers = 1:3)  %>% 
      add_css_column(css = list("text-align", "center"), 
                     columns = 1:3)  
    
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
