library(tidyverse)
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(khroma)
#library(RColorBrewer)
library(scales)

movies <- read.csv("movies.csv")
movies <- movies %>%
  mutate(rating = ifelse(rating == "Not Rated", "Unrated", rating))


netflix_titles <- read.csv("netflix_titles.csv")

merged <- inner_join(movies, netflix_titles, by = c("name" = "title"))

merged <- merged %>%
  dplyr::select(-released, -writer, -show_id, -director.y, -cast, -country.y, 
                -date_added, -release_year, -rating.y, -duration, -listed_in) %>%
  
  mutate(rating.x = ifelse(rating.x == "Not Rated", "Unrated", rating.x)) %>%
  filter(rating.x != "NC-17", rating.x != "TV-14", rating.x != "TV-MA", rating.x != "TV-PG") %>%
  
  filter(genre != "Fantasy", genre != "Mystery", 
         genre != "Sci-Fi", genre != "Sport", genre != "Thriller") %>%
  
  mutate(genre = factor(genre)) %>%
  mutate(genre = fct_relevel(genre, levels = c("Horror", "Drama", "Crime", "Comedy", "Biography",
                                               "Animation", "Adventure", "Action")))

newData <- movies %>%
  mutate(name2 = str_trunc(name, 30, "right"), .after = name) %>%
  
  
  mutate(rating = factor(rating)) %>%
  filter(rating != "Approved", rating != "TV-14", rating != "TV-MA", rating != "TV-PG", 
         rating != "X") %>%
  
  mutate(rating = fct_relevel(rating, levels = c("G", "PG", "PG-13", "R", "NC-17", 
                                                 "Unrated"))) %>%
  
  mutate(genre = factor(genre)) %>%
  filter(genre != "Sport", 
         genre != "Western",
         genre != "Music", genre != "Musical")


### For Plot 3

new <- merged %>%
  group_by(company) %>%
  filter(n() >= 5)%>%
  
  filter(gross > 0) %>%
  filter(budget > 0) %>%
  mutate(profit = gross - budget) %>%
  group_by(year, company) %>%
  summarise(totalProfit = sum(profit))

new <- new %>%
  rename(Company = company, Year = year) %>%
  mutate(`Total Profit` = prettyNum(scales::dollar(totalProfit, style_negative = c("parens")),
                                    big.mark = ","))



#currently, sunset seems to work the best... berlin; smoothrainbow? ####last one tried is berlin
#broc is not bad but not great; lisbon
colors <- color("sunset")(30)

####### Selection Choices

rating <- sort(unique(newData$rating))
genre <- sort(unique(newData$genre))
year <- sort(unique(newData$year))

type <- sort(unique(merged$type))

company <- sort(unique(new$Company))


## For Plot 2

rating2 <- sort(unique(merged$rating.x))


######NOTE: POSSIBLY USE STRINGER_SUB or something like that to shorten the movie titles


ui <- navbarPage(theme = shinytheme("cerulean"),
                 "Movies And TV Shows App",
                 tabPanel("Movie Recommendations",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select Rating"),   #EVENTUALLY: ADD LIMITATIONS ON NUMBER OF SELECTIONS
                              selectizeInput(
                                inputId = "rating", 
                                label = NULL,
                                multiple = TRUE,
                                choices = rating,
                                selected = rating[1:2]),
                              
                              h3("Select Genre"),
                              selectizeInput(inputId="genre",
                                             label=NULL,
                                             multiple = TRUE,
                                             choices=genre,
                                             selected = genre[1]),
                              
                              
                              h3("Select Runtime"),
                              sliderInput(inputId = "runtime",
                                          label = "Select Range of Runtime (Minutes)",
                                          min = 55, max = 366, 
                                          value = c(55, 366),
                                          dragRange = TRUE),
                              
                              
                              
                              
                              h3("Select Release Year"),
                              sliderInput(inputId="year",
                                          label="Select Range of Release Years",
                                          min = 1980, max = 2020,
                                          step = 1,
                                          value = c(1995, 2005),
                                          sep = "",
                                          dragRange = TRUE),
                              
                            ),
                            mainPanel(
                              h2("Scores for Top-20 Highest Rated Movies"),
                              h4("Movie Titles"),
                              plotlyOutput("scoreplot")
                            )
                          )
                 ),
                 
                 tabPanel("Genre Comparison",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select Show Type"),
                              checkboxGroupInput(inputId="type", 
                                                 label= NULL, 
                                                 choices=type, selected = type[1:2]),
                              
                              h3("Select Rating"),
                              selectizeInput(
                                inputId = "rating2", 
                                label = NULL,
                                multiple = TRUE,
                                choices = rating2,
                                selected = rating2[c(1:3)]) #to show just G, PG
                            ),
                            mainPanel(
                              h2("Comparison of Scores between Genres"),
                              plotlyOutput("boxplot")
                            )
                          )
                 )  ,
                 
                 tabPanel("Company Profit",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Select Company"),
                              selectizeInput(inputId="company",
                                             label="Select Up to 5 Companies",
                                             multiple = TRUE,
                                             options = list(maxItems = 5), # limiting the number of user choices to 5
                                             choices=company,
                                             selected = company[2:4]),
                              
                              h3("Select Release Year"),
                              sliderInput(inputId="year3",
                                          label="Select Range of Release Years",
                                          min = 1980, max = 2020,
                                          step = 1,
                                          value = c(1980, 2020),
                                          sep = "",
                                          dragRange = TRUE),
                              
                            ),
                            mainPanel(
                              h2("Top Companies' Profit over Time"),
                              plotlyOutput("timeseries")
                            )
                          )
                 )
)




# Define server logic
server <- function(input, output) {
  
  
  reactdata <- reactive({
    newData |> 
      filter(rating == input$rating) %>%
      filter(genre == input$genre) %>%
      filter(runtime >= input$runtime[1], runtime <= input$runtime[2]) %>%
      filter(year >= input$year[1], year <= input$year[2])%>%
      slice(1:20)
  })
  
  # Plot for tab 1
  output$scoreplot <- renderPlotly({
    g1 <- reactdata() |>
      
      ggplot(aes(x = reorder(name2, score), y = score,
                 text = paste0("Title: ", name, "<br>",
                               "Writer: ", writer, "<br>",
                               "Director: ", director, "<br>",
                               "Star: ", star))) +
      
      geom_col(fill = "Light Blue")  +
      geom_text(aes(label = score),
                hjust =1, nudge_x = 0) +
      labs(x = NULL, y = NULL) +
      
      coord_flip() +
      
      ylim(0, 10) +
      
      theme_classic() +
      theme(
        
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())
    
    
    ggplotly(g1, tooltip = "text")
    
  })
  
  
  # Plot for tab 2
  output$boxplot <- renderPlotly({
    g2 <- merged |> 
      filter(type %in% input$type) %>%
      filter(rating.x %in% input$rating2) %>%  
      
      ggplot(aes(x = genre, y = score)) +
      geom_boxplot(fill = "lightskyblue1") +
      labs(x = "Movie Genre", y = "IMDb Score") +
      
      
      scale_y_continuous(breaks = seq(2, 10, 1), limits = c(2.48, 8.92)) +
      
      coord_flip() +
      theme_bw()
    ggplotly(g2)
    
  })
  
  
  # Plot for tab 3
  
  output$timeseries <- renderPlotly({
    g3 <- new %>%
      filter(Company %in% input$company) %>% #interactive aspect here
      filter(Year >= input$year3[1], Year <= input$year3[2]) %>%
      ggplot(aes(x = Year, y = totalProfit    #,
                 #text = paste0("Company: ", Company, "<br>",
                 #             "Total Profit: ", `Total Profit`)
      )) +
      geom_line(aes(color = Company), size = 1.125) + 
      labs(x = "Year", y = "Total Profit", main = "Company's Total Profit Over Time",
           color = "Company") +
      theme_bw() + 
      scale_y_continuous(labels = scales::comma, limits = c(-80604341, 1729684525 )) +
      #scale_color_brewer(palette = "Set2")
      
      scale_color_manual(values=c("Columbia Pictures" = colors[1], 
                                  "Paramount Pictures" = colors[2],
                                  "Warner Bros." = colors[3],
                                  "Universal Pictures" = colors[4],
                                  "New Line Cinema" = colors[5],
                                  "Dimension Films" = colors[6],
                                  "Dreamworks pictures" = colors[7],
                                  "Screen Gems" = colors[8],
                                  "Walt Disney Pictures" = colors[9],
                                  "Miramax" = colors[10],
                                  "Relativity Media" = colors[11],
                                  "Summit Entertainment" = colors[12],
                                  "The Weinstein Company" = colors[13],
                                  "Metro-Goldwyn-Mayer (MGM)" = colors[14],
                                  "Lionsgate" = colors[15],
                                  "Revolution Studios" = colors[16],
                                  "TriStar Pictures" = colors[17],
                                  "DreamWorks Animation" = colors[18],
                                  "Focus Features" = colors[19],
                                  "Paramount Vantage" = colors[20],
                                  
                                  "United Artists" = colors[21],
                                  "Alcon Entertainment" = colors[22],
                                  "CBS Films" = colors[23],
                                  "Golden Harvest Company" = colors[24],
                                  "Castle Rock Entertainment" = colors[25],
                                  "EuropaCorp" = colors[26],
                                  "Film4" = colors[27],
                                  "FilmDistrict" = colors[28],
                                  "StudioCanal" = colors[29],
                                  "Twentieth Century Fox" = colors[30]
                                  
      ))
    
    ggplotly(g3   #, tooltip = "text"
    )
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)



