## ============================================================================
## SECTION 2: SERVER
## ============================================================================

## Remove everything in the beginning
# rm(list = ls())

library(shiny)
library(recommenderlab)

## ============================================================================
## Data preparation
## ============================================================================

## Data import
#setwd("C:/Users/Administrator/Desktop/Shiny")
setwd(getwd())
load("MovieLense2016.rda")
MovieLense <- MovieLense2016
MovieLenseMeta <- MovieLense2016Meta
MovieLenseMeta$title <- as.character(MovieLenseMeta$title)
MovieLenseTitle <- MovieLense2016Meta$title

## Initiating calculation
rates_matrix <- as.matrix(MovieLense@data)
avg_rates <- data.frame(title=colnames(rates_matrix),
                        count_rating=colSums(rates_matrix !=0))
avg_rates$title <- as.character(avg_rates$title)

avg_rates$count_rating_norm <- punif(avg_rates$count_rating,
                                     min(avg_rates$count_rating),
                                     max(avg_rates$count_rating))

avg_rates$avg_rates <- colSums(rates_matrix) / avg_rates$count_rating
avg_rates$avg_rates_norm <- punif(avg_rates$avg_rates,
                                  min(avg_rates$avg_rates),
                                  max(avg_rates$avg_rates))

## List of movie genres
genre <- c("Adventure", "Animation", "Children", "Comedy", "Fantasy", "Romance", "Drama",
           "Action", "Crime", "Thriller", "Horror", "Mystery", "Sci-Fi", "IMAX",
           "War", "Musical", "Documentary", "Western", "Film-Noir")

mv_pool <- 40

## ============================================================================
## Main Server
## ============================================================================

shinyServer(function(input, output) {
  if (file.exists("SavedRatings.rda")) load("SavedRatings.rda")
  
  ## Load recommenders
  if (file.exists("models.rda")) {load("models.rda")}
  else{
    rec_pop <- Recommender(MovieLense, "Popular")
    rec_svd <- Recommender(MovieLense, "SVD")
    rec_ub <-  Recommender(MovieLense, method = "UBCF", 
                           parameter = list(method = "Cosine", nn = 15))
    save(rec_pop, rec_ub, rec_svd, file = "models.rda")
      }
  
  ## Create a subste of movies list
  movies_subset <- reactive({
    
    ## Only select movies releasing 10 years after user birth year
    birth_year <- 2016 - input[["age"]]
    movies_subset <- MovieLenseMeta[MovieLenseMeta$year >= birth_year + 10,]
    movies_subset$year_norm <- punif(movies_subset$year,
                                     min(movies_subset$year,na.rm=T),
                                     max(movies_subset$year,na.rm=T))
    
    ## Merge with avg_rates
    movies_subset <- merge(movies_subset, avg_rates, by=c("title"), all.x=T)
    
    ## Calculate the score of each movie based on the genres ratings
    rates <- c()
    for (i in 1:length(genre))
      rates <- c(rates, input[[paste0("genre", i)]])
    rates <- c(rates, 0) # For unknown genre ratings
    
    movies_subset$genre_score <- rowSums(sweep(movies_subset[,3:22], MARGIN=2, rates, "*"), na.rm=T)
    movies_subset$genre_score_norm <- punif(movies_subset$genre_score,
                                            min(movies_subset$genre_score),
                                            max(movies_subset$genre_score))
    
    ## Calculate total score for ranking movies, the weight of each component is follow:
    ## - genre ratings: 50%
    ## - recent year: 10%
    ## - average rate: 10%
    ## - popular movies: 30%
    movies_subset$total_score <-
      movies_subset$genre_score_norm*0.5 +
      movies_subset$year_norm*0.1 +
      movies_subset$avg_rates_norm*0.1 +
      movies_subset$count_rating_norm*0.3
    
    movies_subset
  })
  
  ## Create the score table of all movies
  rate_movies <- reactive({
    
    mv_subet <- movies_subset()
    
    ## Create the topN movies list (N = 10)
    mv_sort <- head(mv_subet[with(mv_subet, order(-total_score)),], mv_pool)
    
    rate_movies <- data.frame('title' = mv_sort$title,
                         'genre_score' = mv_sort$genre_score,
                         'genre_score_norm' = mv_sort$genre_score_norm,
                         'year' = mv_sort$year,
                         'year_norm' = mv_sort$year_norm,
                         'avg_rates' = mv_sort$avg_rates,
                         'avg_rates_norm' = mv_sort$avg_rates_norm,
                         'count_rating' = mv_sort$count_rating,
                         'count_rating_norm' = mv_sort$count_rating_norm,
                         'total_score' = mv_sort$total_score)
  })
  
  
  
  movies_to_rate <- reactive({
    ignore <- input$new_movies  # listen to button
    
    mv_rates <- rate_movies()
    mv_rates$title <- as.character(mv_rates$title)
    #
    if (exists("SavedRatings")){
      load("SavedRatings.rda")
      mv_rates <- mv_rates[!mv_rates$title %in% SavedRatings[1,is.na(SavedRatings)==T] ,] } 
    
    #
    mv_list <- head(mv_rates$title[sample(1:mv_pool)], 5)
    
    output[[paste0("movie", 1)]] <- renderText(mv_list[1])
    output[[paste0("movie", 2)]] <- renderText(mv_list[2])
    output[[paste0("movie", 3)]] <- renderText(mv_list[3])
    output[[paste0("movie", 4)]] <- renderText(mv_list[4])
    output[[paste0("movie", 5)]] <- renderText(mv_list[5])
    
    mv_list
  })
  
  
  
  ## Make recommendations
  output$movie_recom <- renderTable({
    
    ## If ratings is already existed, load it back and update it
    if (file.exists("SavedRatings.rda")) load("SavedRatings.rda")
    
    ## Check if SavedRating variable is existed
    if (exists("SavedRatings")) ratings <- SavedRatings
    else {
      ratings <- matrix(NA, nrow = 1, ncol = ncol(MovieLense))
      colnames(ratings) <- colnames(MovieLense@data)
    }
    
  
        ## Read ratings
    for(i in 1:5)
      ratings[1, movies_to_rate()[i]] <- input[[paste0("slider", i)]]
    
    ## Updates the recent ratings of user
    SavedRatings <- ratings
    save(SavedRatings, file="SavedRatings.rda")
    
    ## create recommendations
    pred_pop <- predict(rec_pop, as(ratings, "realRatingMatrix"), type = "ratings")
    pred_svd <- predict(rec_svd, as(ratings, "realRatingMatrix"), type = "ratings")
    pred_ub <-  predict(rec_ub, as(ratings, "realRatingMatrix"), type = "ratings")
    
    
    #' Aggregate predictions - weigh by algorithm
    pred_hybrid <- rbind(0.1*as(pred_pop, "matrix"),0.5*as(pred_ub, "matrix"),0.4*as(pred_svd, "matrix"))
    pred_hybrid <- colSums(pred_hybrid, na.rm = T)  
    pred <- getTopNLists(as(rbind(pred_hybrid), "realRatingMatrix"))
    
    
    recommendationList <- getList(pred)[[1]]
    cbind('Movie Name' = recommendationList,
          'Predicted Rating' = sprintf("%1.1f", pred@ratings[[1]]))
  })
  
})
