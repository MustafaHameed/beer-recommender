#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Define UI for application 
shinyUI(navbarPage("Beer Recommendation App", theme = "bootstrap.css",
                   tabPanel("Beer Recommendations",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("rec1", label = h4("Type of Recommendation:"),
                                             c("General Top Recommendations" = "general",
                                               "Categorical Top Recommendations" = "category",
                                               "Individual Recommendations" = "individual")),
                                conditionalPanel(
                                  condition = "input.rec1 == 'category'",
                                  radioButtons(inputId = "cat", label = h4("Top Beers from Category:"),
                                               c("Brewery" = "brewery",
                                                 "Beer Style" = "style")),
                                conditionalPanel(
                                  condition = "input.cat == 'brewery'",
                                  selectizeInput("brew", label = h4("Select your favorite brewery"), selected = NULL,
                                              multiple = FALSE, options = list(maxOptions = 2000, maxItems=1,
                                              placeholder = 'Select item'),
                                              choices = c("", breweries))),
                                conditionalPanel(
                                  condition = "input.cat == 'style'",
                                  selectizeInput("sty", label = h4("Select your favorite beer style"), selected = NULL,
                                              multiple = FALSE, options = list(maxOptions = 200, maxItems=1,
                                              placeholder = 'Select item'), choices = c("", styles)))),
                                conditionalPanel(
                                  condition = "input.rec1 == 'individual'",
                                  headerPanel(h4("Please rate 3 beers you liked:")),
                                  selectizeInput("rated_beer1",
                                              label = h5("My first rating:"), selected = 0,
                                              multiple = FALSE, options = list(maxItems=1, maxOptions=6000, 
                                              placeholder = 'Select item'), choices = c("", beers)),
                                  sliderInput("beer_rating1",
                                              label = NULL, min = 1, max = 5, value = 2.5, step = 0.5),
                                  selectizeInput("rated_beer2",
                                              label = h5("My second rating:"), selected = 0,
                                              multiple = FALSE, options = list(maxItems=1, maxOptions=6000,
                                              placeholder = 'Select item'), choices = c("", beers)),
                                  sliderInput("beer_rating2",
                                              label = NULL, min = 1, max = 5, value = 2.5, step = 0.5),
                                  selectizeInput("rated_beer3",
                                                label = h5("My third rating:"), selected = 0,
                                                multiple = FALSE, options = list(maxItems=1, maxOptions=6000,
                                                placeholder = 'Select item'), choices = c("", beers)),
                                  sliderInput("beer_rating3",
                                              label = NULL, min = 1, max = 5, value = 2.5, step = 0.5)),
                                
                                conditionalPanel(
                                  condition = "input.rec1 == 'general'",
                                  sliderInput("abv1",
                                              label = h4("Alcohol Percentage:"),
                                              min = 0.01,
                                              max = 57.7,
                                              value = c(4, 7),
                                              step = 0.01)),
                                conditionalPanel(
                                  condition = "input.rec1 == 'category'",
                                  conditionalPanel(
                                    condition = "input.cat == 'brewery'",
                                    sliderInput("abv1",
                                                label = h4("Alcohol Percentage:"),
                                                min = 0.01,
                                                max = 57.7,
                                                value = c(4, 7),
                                                step = 0.01)),
                                  conditionalPanel(
                                    condition = "input.cat == 'style'",
                                    sliderInput("abv1",
                                                label = h4("Alcohol Percentage:"),
                                                min = 0.01,
                                                max = 57.70,
                                                value = c(4, 7),
                                                step = 0.01))),
                                sliderInput("n1",
                                            label = h4("Number of Recommendations:"),
                                            min = 0,
                                            max = 50,
                                            value = 10,
                                            step = 5),
                                conditionalPanel(
                                  condition = "input.rec1 != 'individual'",
                                  sliderInput("reviews1",
                                              label = h4("Min. Number of Reviews:"),
                                              min = 100,
                                              max = 5000,
                                              value = 100,
                                              step = 100)
                                )
                                
                              ),
                              mainPanel(
                                fluidRow(
                                  column(10,
                                         dataTableOutput('beer')),
                                  tags$style(type="text/css", '#beer tfoot {display:none;}')
                                )
                              )
                            )
                          ),
                   tabPanel("Brewery Recommendations",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("rec2", label = h4("Type of Recommendation:"),
                                             c("General Top Recommendations" = "general",
                                               "Categorical Top Recommendations" = "category")),
                                conditionalPanel(
                                  condition = "input.rec2 == 'category'",
                                  selectizeInput("cat2", label = h4("Breweries based on your favorite beer style"), 
                                              selected = NULL, multiple = FALSE, options = list(maxOptions=200, 
                                              maxItems=1, placeholder = 'Select item'), choices = c("", styles))),
                                sliderInput("abv2",
                                            label = h4("Alcohol Percentage:"),
                                            min = 3,
                                            max = 13,
                                            value = c(4, 7),
                                            step = 0.01),
                                sliderInput("n2",
                                            label = h4("Number of Recommendations:"),
                                            min = 0,
                                            max = 50,
                                            value = 10,
                                            step = 5),
                                sliderInput("reviews2",
                                            label = h4("Min. Number of Reviews:"),
                                            min = 100,
                                            max = 10000,
                                            value = 100,
                                            step = 100)
                            
                              ),
                               mainPanel(
                                 fluidRow(
                                   column(10,
                                          dataTableOutput('brewery')),
                                   tags$style(type="text/css", '#brewery tfoot {display:none;}')
                              )))),
                   tabPanel("Beer Style Recommendations",
                            sidebarLayout(
                              sidebarPanel(
                                radioButtons("rec3", label = h4("Type of Recommendation:"),
                                             c("General Top Recommendations" = "general",
                                               "Categorical Top Recommendations" = "category")),
                                conditionalPanel(
                                  condition = "input.rec3 == 'category'",
                                  selectizeInput("cat3", label = h4("Beer styles from your favorite brewery"),
                                              selected = NULL, multiple = FALSE, options = list(maxItems=1, 
                                              maxOptions=2000, placeholder = 'Select item'), choices = c("", breweries))),
                                sliderInput("abv3",
                                            label = h4("Alcohol Percentage:"),
                                            min = 0.01,
                                            max = 12,
                                            value = c(4, 7),
                                            step = 0.01),
                                sliderInput("n3",
                                            label = h4("Number of Recommendations:"),
                                            min = 0,
                                            max = 50,
                                            value = 10,
                                            step = 5),
                                sliderInput("reviews3",
                                            label = h4("Min. Number of Reviews:"),
                                            min = 100,
                                            max = 10000,
                                            value = 100,
                                            step = 100)
                                
                              ),
                              mainPanel(
                                fluidRow(
                                  column(10,
                                         dataTableOutput('style')),
                                  tags$style(type="text/css", '#style tfoot {display:none;}')
                                ))))
                                
                              
          
  
))
