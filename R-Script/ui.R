## ============================================================================
## SECTION 1: USER INTERFACE
## ============================================================================

library(shiny)

## List of movie genres
genre <- c("Adventure", "Animation", "Children", "Comedy", "Fantasy", "Romance", "Drama",
           "Action", "Crime", "Thriller", "Horror", "Mystery", "Sci-Fi", "IMAX",
           "War", "Musical", "Documentary", "Western", "Film-Noir")

## ============================================================================
## Main UI
## ============================================================================

shinyUI(fluidPage(style="background-color:#FF9900;", title = "Movie Recommender System: IESEG MBD - Fasih, Phan",
  titlePanel(h1("Movie Recommender Using Movie Lense Data (2016)", style="color:#FFFFFF;")),
  
  fluidRow(style="background-color:#F8F8F8;",
    
    ## Column 1: Ask for user information (e.g. Age, Genre)
    column(3, h4("Personal information:"),
           
           ## Asking for age
           numericInput("age", label=p("Age"), value=25, min=1, max=200),
           
           ## Asking for genre ratings
           lapply(1:length(genre), function(i) sliderInput(paste0("genre", i),
                                                           label = p(genre[i]),
                                                           min = 0, max = 2, value = 0))
          ),
    
    ## Column 2: Show some movies and ask user to rate them
    column(3, h4("Rate some movies:"),
           
           ## Show the slider
           lapply(1:5, function(i) sliderInput(paste0("slider", i),
                                               label = textOutput(paste0("movie", i)),
                                               min = 1, max = 5, value = 3)),
           
           
  
           ## Change new movies to rate
           actionButton("new_movies", "Change movies")
           
           ),
    
    ## Column 3: Final recommendation result
    column(6, h4("Recommendation movies:"),
           
           tableOutput("movie_recom")
           
           )
  )
))
