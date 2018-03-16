#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
# Install, if not already
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("recommenderlab")

library(shiny)
library(dplyr)
library(recommenderlab)

load("RData/reviews.RData")
load("RData/beers.RData")
load("RData/breweries.RData")
load("RData/styles.RData")
load("RData/beer_rating_matrix.RData")
load("RData/beer_rec20.RData")

# Define server logic
shinyServer(function(input, output) {
  
### Retrieve recommendations for Beers  
  output$beer <- renderDataTable({
    
    ## Top Recommendations ##
    if (input$rec1 == "general"){
      beer1 <- reviews %>%
        group_by(beer_name) %>%
        summarize(n_obs = n(),
                  avg_rating = round(mean(review_overall), digits=2),
                  avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
        filter(n_obs >= input$reviews1) %>%
        filter(avg_abv >= input$abv1[1], avg_abv <= input$abv1[2]) %>%
        select(beer_name, avg_rating, avg_abv) %>%
        top_n(n = input$n1, wt = avg_rating) %>%
        arrange(desc(avg_rating))
      
      colnames(beer1) <- c("Beer Name", "Average Overall Rating", "Alcohol %")
      return(beer1)
    }
    else if (input$rec1 == "category"){
      if (input$cat == "brewery"){
        beer2 <- reviews %>%
          filter(brewery_name == input$brew) %>%
          group_by(beer_name) %>%
          summarize(n_obs = n(),
                    avg_rating = round(mean(review_overall), digits=2),
                    avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
          filter(n_obs >= input$reviews1) %>%
          filter(avg_abv >= input$abv1[1], avg_abv <= input$abv1[2]) %>%
          select(beer_name, avg_rating, avg_abv) %>%
          top_n(n = input$n1, wt = avg_rating) %>%
          arrange(desc(avg_rating))
        
        colnames(beer2) <- c("Beer Name", "Average Overall Rating", "Alcohol %")
        return(beer2)
      }
      else if (input$cat == "style"){
        beer3 <- reviews %>%
          filter(beer_style == input$sty) %>%
          group_by(beer_name) %>%
          summarize(n_obs = n(),
                    avg_rating = round(mean(review_overall), digits=2),
                    avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
          filter(n_obs >= input$reviews1) %>%
          filter(avg_abv >= input$abv1[1], avg_abv <= input$abv1[2]) %>%
          select(beer_name, avg_rating, avg_abv) %>%
          top_n(n = input$n1, wt = avg_rating) %>%
          arrange(desc(avg_rating))
        
        colnames(beer3) <- c("Beer Name", "Average Overall Rating", "Alcohol %")
        return(beer3)
      }
    }
    
    ## Item-Based Collaborative Filtering
    else if (input$rec1 == "individual"){
      # get user inputs for their beer ratings
      beers_to_rate <- reactive({
        rated_beers <- c(input$rated_beer1, input$rated_beer2, input$rated_beer3)
        rated_beers
        })
      beer_ratings <- reactive({ 
        ratings_of_beers <- c(input$beer_rating1, input$beer_rating2, input$beer_rating3)
        ratings_of_beers
        })

      # create rating matrix for current user
      ratings <- matrix(NA, nrow=1, ncol = ncol(r))
      colnames(ratings) <- colnames(r)
      for(i in 1:3)
        ratings[1, beers_to_rate()[i]] <- input[[paste0("beer_rating", i)]] #as.numeric(paste0(beer_ratings()[i]))

      # do prediction
      realRatings <- as(ratings, "realRatingMatrix")
      prediction <- predict(beer_rec20, realRatings, n = input$n1)

      df <- cbind("Beer Name" = getList(prediction)[[1]],
                  "predicted Rating" = sprintf("%1.1f", getRatings(prediction)[[1]]))      
      return(df)
    }
    
    
  }, 
      options = list(pageLength = 15,
                    lengthChange = FALSE))
  
### Retrieve top recommendations for Breweries ###  
  output$brewery <- renderDataTable({
    
    if (input$rec2 == "general"){
      brewery1 <- reviews %>%
        group_by(brewery_name) %>%
        summarize(n_obs = n(),
                  avg_rating = round(mean(review_overall), digits=2),
                  avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
        filter(n_obs >= input$reviews2) %>%
        filter(avg_abv >= input$abv2[1], avg_abv <= input$abv2[2]) %>%
        select(brewery_name, avg_rating, avg_abv) %>%
        top_n(n = input$n2, wt = avg_rating) %>%
        arrange(desc(avg_rating))
      
      colnames(brewery1) <- c("Brewery Name", "Average Overall Rating", "Alcohol %")
      return(brewery1)
    }
    else if (input$rec2 == "category"){
      brewery2 <- reviews %>%
        filter(beer_style == input$cat2) %>%
        group_by(brewery_name) %>%
        summarize(n_obs = n(),
                  avg_rating = round(mean(review_overall), digits=2),
                  avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
        filter(n_obs >= input$reviews2) %>%
        filter(avg_abv >= input$abv2[1], avg_abv <= input$abv2[2]) %>%
        select(brewery_name, avg_rating, avg_abv) %>%
        top_n(n = input$n2, wt = avg_rating) %>%
        arrange(desc(avg_rating))
      
      colnames(brewery2) <- c("Brewery Name", "Average Overall Rating", "Alcohol %")
      return(brewery2)
    }
  }, 
      options = list(pageLength = 15,
                lengthChange = FALSE)
  )
  
### Retrieve top recommendations for Beer Styles ###  
  output$style <- renderDataTable({
    ## General top beer styles
    if (input$rec3 == "general"){
      style1 <- reviews %>%
        group_by(beer_style) %>%
        summarize(n_obs = n(),
                  avg_rating = round(mean(review_overall), digits=2),
                  avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
        filter(n_obs >= input$reviews3) %>%
        filter(avg_abv >= input$abv3[1], avg_abv <= input$abv3[2]) %>%
        select(beer_style, avg_rating, avg_abv) %>%
        top_n(n = input$n3, wt = avg_rating) %>%
        arrange(desc(avg_rating))
      
      colnames(style1) <- c("Beer Style Name", "Average Overall Rating", "Alcohol %")
      return(style1)
    }
    ## top beer styles by brewery
    else if (input$rec3 == "category"){
      style2 <- reviews %>%
        filter(brewery_name == input$cat3) %>%
        group_by(beer_style) %>%
        summarize(n_obs = n(),
                  avg_rating = round(mean(review_overall), digits=2),
                  avg_abv = round(mean(beer_abv, na.rm = TRUE), digits=2)) %>%
        filter(n_obs >= input$reviews3) %>%
        filter(avg_abv >= input$abv3[1], avg_abv <= input$abv3[2]) %>%
        select(beer_style, avg_rating, avg_abv) %>%
        top_n(n = input$n3, wt = avg_rating) %>%
        arrange(desc(avg_rating))
      
      colnames(style2) <- c("Beer Style Name", "Average Overall Rating", "Alcohol %")
      return(style2)
    }
  }, 
  options = list(pageLength = 15,
                 lengthChange = FALSE))
  
  
  
})
