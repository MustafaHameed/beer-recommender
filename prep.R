#Install if not already
#install.packages("recommenderlab")
#install.packages("reshape2")
#install.packages("plyr")
#install.packages("dplyr")
#install.packages("tidyr")

library(recommenderlab)
library(reshape2)
library(plyr)
library(dplyr)
library(tidyr)

#setwd("~/your-working-directory")

# Load data
data <- read.csv("data/beer_reviews.csv", sep = ",", header = TRUE, encoding = "UTF-8")
# Exclude reviews without a reviewer profilename
reviews <- data[data$review_profilename!="", ]
# order reviews for reviewer per beer chronologically (latest review per beer will be first)
reviews <- reviews[with(reviews, order(review_profilename, beer_name, -review_time)), ]
# identify duplicate rows
dup <- !duplicated(reviews[, c(7,11)])
# removing duplicate rows from all reviews
reviews <- reviews[dup, ]

# build lists for breweries, styles and beers
breweries <- reviews %>% group_by(brewery_name) %>% summarize(n_obs = n()) %>% filter(n_obs > 100) %>% select(brewery_name)
styles <- reviews %>% group_by(beer_style) %>% summarize(n_obs = n()) %>% filter(n_obs > 100) %>% select(beer_style)
beers <- reviews %>% group_by(beer_name) %>% summarize(n_obs = n()) %>% filter(n_obs > 100) %>% select(beer_name)

# remove unnecessary columns for beer reviews
beer_reviews <- reviews[, c(7,11,4)]
# only use beers ratings with sufficient amount of reviews
beer_reviews <- beer_reviews[beer_reviews$beer_name %in% beers$beer_name, ]

# preparation for building recommender
g <- acast(beer_reviews, review_profilename ~ beer_name, fun.aggregate = mean, 
           value.var = "review_overall")
r <- as(g, "realRatingMatrix")
# train recommender model
beer_rec20 <- Recommender(r, method = "IBCF", param = list(
  method = "cosine", k = 20))

# Saving R Data elements 
save(beers, file = "RData/beers.RData")
save(breweries, file = "RData/breweries.RData")
save(styles, file = "RData/styles.RData")
save(reviews, file = "RData/reviews.RData")
save(r, file = "RData/beer_rating_matrix.RData")
save(beer_rec20, file = "RData/beer_rec20.RData")

