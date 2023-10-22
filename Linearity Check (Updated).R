
library(dplyr)
library(boot)
library(car)
library(ggplot2)


getwd()

setwd('C:/Users/zzhong13/Desktop/Multivariate')
data = read.csv('IMDB_data_Fall_2023.csv')



test_polynomial_relationship <- function(data, dependent_variable, predictor, max_degree = 5, num_folds = 5) {
  # Create a data frame to store results
  results <- list(degree = c(),MSE= c())
  
  # Fit a linear model
  fit <- lm(paste(dependent_variable, "~", predictor), data = data)
  
  # Create diagnostic plots to assess linearity
  res_plot = residualPlot(fit,main = predictor)
  
  
  #file_name <- paste(predictor, ".pdf")
  #pdf(file.path(getwd(), file_name))
  # Save the plot
  #ggsave(file.path(getwd(), file_name), plot = res_plot$residual, width = 6, height = 4)
  
  # Perform the Tukey test for linearity
  tukey_test <- ncvTest(fit)
  print(paste(predictor,tukey_test['p']))
  
  if (tukey_test['p'] > 0.10) {
    # If linearity is not violated, store results for the linear model
    residuals <- summary(fit)$residuals
    mse = mean(residuals^2)
    results$degree = c(results$degree,1) 
    results$MSE = c(results$MSE,mse)
    
    #print(results)
    } else {
    # Test polynomial degrees
    for (degree in 2:max_degree) {
      poly_predictor <- paste("poly(", predictor, ",", degree, ")", sep = "")
      
      # Fit a polynomial model
      poly_model <- glm(paste(dependent_variable, "~", poly_predictor), data = data)
      
      
      # Cross-validation using cv.glm for out-of-sample performance
      cv_results <- cv.glm(data, poly_model, K = num_folds)
      
      #print( cv_results)
      
      results$degree = c(results$degree,degree) 
      results$MSE = c(results$MSE,cv_results$delta[1])
    }
  }
  
  return(results)
}




numerical_features <- c("movie_budget", 
                        "duration", 
                        "nb_news_articles", 
                        "actor1_star_meter", 
                        "actor2_star_meter", 
                        "actor3_star_meter", 
                        "nb_faces", 
                        "action", 
                        #"adventure", # non linear but only 2 predicted values
                        #"scifi", #   non linear but only 2 predicted values
                        #"thriller",# non linear but only 2 predicted values
                        "musical", 
                        #"romance",# non linear but only 2 predicted values
                        "western", 
                        "sport", 
                        "horror",
                        #"drama", #non linear but only 2 predicted values
                        #"war",# 
                        "animation", 
                        #"crime", # non linear but only 2 predicted values
                        "movie_meter_IMDBpro")




numerical_features <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "action", "adventure", "scifi", "thriller", "musical", "romance", "western", "sport", "horror", "drama", "war", "animation", "crime", "movie_meter_IMDBpro")


target_variable <- "imdb_score"


max_degree <- 5  # Maximum polynomial degree to test
num_folds <- 5  # Number of cross-validation folds


linearity_check = list()

for (v in numerical_features) {
  tryCatch(
    {
      table <- test_polynomial_relationship(data, target_variable, v, max_degree, num_folds)
      if (inherits(table, "error")) {
        print(paste(v, 'non linear but only 2 predicted values'))
        next  # Skip to the next iteration
      } else {
        print(table)
      }
    },
    error = function(err) {
      print(paste(v, 'Error:', err$message))
    }
  )
}



data.frame(linearity_check)













