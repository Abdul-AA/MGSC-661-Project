

library(dplyr)
library(boot)
library(car)
library(ggplot2)
library(stargazer)


setwd('C:/Users/zzhong13/Desktop/Multivariate')


train = read.csv('../IMDB_data_Fall_2023.csv')
View(train)

test = read.csv('../test_data_IMDB_Fall_2023.csv')
View(test)

colnames(train)

train$Biography <- ifelse(grepl("Biography", train$genres), 1, 0)+ifelse(grepl("biography", train$plot_keywords), 1, 0)
train$Biography = ifelse(train$Biography >= 1,1,0)

train$Comedy <- ifelse(grepl("Comedy", train$genres), 1, 0)+ifelse(grepl("comedy", train$plot_keywords), 1, 0)
train$Comedy = ifelse(train$Comedy >= 1,1,0)

train$Animation <- ifelse(grepl("Animation", train$genres), 1, 0)+ifelse(grepl("animation", train$plot_keywords), 1, 0)
train$Animation = ifelse(train$Animation>=1, 1, 0)


train$Documentary1 <- ifelse(grepl("Documentary", train$genres), 1, 0)
train$Documentary2 <- ifelse(grepl("documentary", train$plot_keywords), 1, 0)
train$Documentary <- train$Documentary1+train$Documentary2
train$Documentary <- ifelse(train$Documentary, 1, 0)










month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

score_by_time = train %>%
  group_by(release_month) %>%
  summarize(mean_imdb_score = mean(imdb_score))

# Convert release_month to a factor with specified levels
score_by_time$release_month <- factor(score_by_time$release_month, levels = month_order)

score_by_time %>%
  ggplot(aes(x = release_month, y = mean_imdb_score)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean IMDb Score by Month", x = "Month", y = "Mean IMDb Score") +
  theme_minimal()
 

unique(test$release_year)
unique(test$release_month)
unique(train$release_year)




features = c('release_year','release_month')
target_variable = 'imdb_score'

# Function to run a simple linear regression for a given predictor and dependent variable
simple_linear_regression <- function(data, dependent_variable, predictor) {
  # Build the linear regression model
  model <- lm(paste(dependent_variable, "~", predictor), data = data)
  
  # Extract R-squared value
  r_squared <- summary(model)$r.squared
  
  # Extract p-value
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  
  return(data.frame(Predictor = predictor, R_squared = r_squared, P_value = p_value))
}


# Create an empty data frame to store results
results <- data.frame(Predictor = character(0), R_squared = numeric(0), P_value = numeric(0))

# Loop through the predictors and run simple linear regressions
for (predictor in features) {
  regression_result <- simple_linear_regression(train, target_variable, predictor)
  results <- rbind(results, regression_result)
}

# Print the results
print(results)



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
  
  print(paste(unlist(results[1])[which.min( unlist(results[2]))],min(unlist(results[2]))           ) )
  return(results)
}


max_degree <- 10  # Maximum polynomial degree to test
num_folds <- 5  # Number of cross-validation folds


Best_degree = list()

for (v in features) {
  tryCatch(
    {
      table <- test_polynomial_relationship(data, target_variable, v, max_degree, num_folds)
      if (inherits(table, "error")) {
        print(paste(v, 'non linear but only 2 predicted values'))
        Best_degree[v] = 0
        next  # Skip to the next iteration
      } else {
        Best_degree[v] = unlist(table[1])[which.min( unlist(table[2]))]
        print(table)
      }
    },
    error = function(err) {
      print(paste(v, 'Error:', err$message))
    }
  )
}

Best_degree



train$release_month = as.factor(train$release_month)
train$release_month = relevel(train$release_month , ref = 'Nov')




fit = lm(imdb_score~release_year + release_month, data = train)

summary(fit)


numerical_features <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "action", "adventure", "scifi", "thriller", "musical", "romance", "western", "sport", "horror", "drama", "war", "animation", "crime", "movie_meter_IMDBpro")


Cat_train <- train[, !names(train) %in% numerical_features]






# Create a new variable for the seasons
train$season <- case_when(
  train$release_month %in% c("Dec", "Jan", "Feb") ~ "Winter",
  train$release_month %in% c("Mar", "Apr", "May") ~ "Spring",
  train$release_month %in% c("Jun", "Jul", "Aug") ~ "Summer",
  train$release_month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
)

train$season = as.factor(train$season)
train$season = relevel(train$season , ref = 'Spring')

fit1 = lm(imdb_score~release_year + season, data = train)
summary(fit1)



Cat_train$aspect_ratio



Cat_train$is_IMAX <- case_when(
  Cat_train$aspect_ratio >=1.9 ~ 1,
  Cat_train$aspect_ratio < 1.9 ~ 0,
)




reg = lm(imdb_score~aspect_ratio+is_IMAX+aspect_ratio*is_IMAX, data = Cat_train)


residualPlot(reg, main = 'Aspect Ratio')

ncvTest(reg)



summary(reg)

Cat_train


write.csv(Cat_train, file = "processed_dataset.csv", row.names = FALSE)

getwd()

Cat_train$movie_title_length = lapply(Cat_train$movie_title ,nchar)
Cat_train$genres_length = lapply(Cat_train$genres,nchar)
Cat_train$plot_keywords_length = lapply(Cat_train$plot_keywords,nchar)
Cat_train$director_length = lapply(Cat_train$director,nchar)
Cat_train$cinematographer_length = lapply(Cat_train$cinematographer,nchar)
Cat_train$production_company_length = lapply(Cat_train$production_company,nchar)
Cat_train$distributor_length = lapply(Cat_train$distributor,nchar)
Cat_train$actor1_length = lapply(Cat_train$actor1,nchar)
Cat_train$actor2_length = lapply(Cat_train$actor2,nchar)
Cat_train$actor3_length = lapply(Cat_train$actor3,nchar)


Cat_train$movie_title_length = as.numeric(Cat_train$movie_title_length)
Cat_train$genres_length = as.numeric(Cat_train$genres_length)
Cat_train$plot_keywords_length = as.numeric(Cat_train$plot_keywords_length)
Cat_train$director_length = as.numeric(Cat_train$director_length)
Cat_train$cinematographer_length = as.numeric(Cat_train$cinematographer_length)
Cat_train$production_company_length = as.numeric(Cat_train$production_company_length)
Cat_train$distributor_length = as.numeric(Cat_train$distributor_length)
Cat_train$actor1_length = as.numeric(Cat_train$actor1_length)
Cat_train$actor2_length = as.numeric(Cat_train$actor2_length)
Cat_train$actor3_length = as.numeric(Cat_train$actor3_length)





reg = lm(imdb_score~movie_title_length+genres_length+
           plot_keywords_length+
           director_length+
           cinematographer_length+
           production_company_length+
           distributor_length+
           actor1_length+
           actor2_length+
           actor3_length
           ,data = Cat_train)

summary(reg)

View(Cat_train)

write.csv(Cat_train, file = "processed_dataset.csv", row.names = FALSE)




Cat_train$director = as.factor(Cat_train$director)
Cat_train$director = relevel(Cat_train$director, ref = 'Alexander Payne')


fit = lm(imdb_score~director,data = Cat_train)
summary_fit = summary(fit)




significant_directors <- summary_fit$coefficients[summary_fit$coefficients[, "Pr(>|t|)"] < 0.05, ]

row_names <- rownames(significant_directors)

# Remove the first 8 characters from each row name
new_row_names <- sub("^.{8}", "", row_names)

# Set the modified row names back to the data frame
rownames(significant_directors) <- new_row_names



write.csv(significant_directors,  file = "Significant Directors relative to Alexandre Payne.csv", row.names = TRUE)




reg = lm(imdb_score~release_day, data = Cat_train)


summary(reg)



colnames(Cat_train)



data = cbind(train[numerical_features],Cat_train[c('imdb_score',
                                                   "release_year",
                                                   #"is_IMAX",#
                                                   "aspect_ratio",
                                                   "colour_film" ,
                                                   "genres_length",
                                                   "distributor_length",
                                                   "actor1_length",
                                                   "Biography",
                                                   'Comedy',
                                                   'Animation',
                                                   'Documentary')])


model = glm(imdb_score~poly(movie_budget, 1,raw = TRUE) + 
              poly(duration, 2,raw = TRUE)+
              poly(nb_news_articles, 1,raw = TRUE)+
              poly(movie_meter_IMDBpro, 5)+
              release_year+
            #maturity_rating+
              aspect_ratio+
              colour_film+
              genres_length+
              distributor_length+
              actor1_length+
              Biography+
              Comedy+
              Animation+
              Documentary
            ,data = data)


summary(model)

res = cv.glm(data,model,K= 5)
res$delta

test = read.csv('../test_data_IMDB_Fall_2023.csv')


test$actor1_length = lapply(test$actor1,nchar)
test$genres_length = lapply(test$genres,nchar)
test$distributor_length = lapply(test$distributor,nchar)


test$actor1_length = as.numeric(test$actor1_length)
test$genres_length = as.numeric(test$genres_length)
test$distributor_length = as.numeric(test$distributor_length)



test$is_IMAX <- case_when(
  test$aspect_ratio >=1.9 ~ 1,
  test$aspect_ratio < 1.9 ~ 0,
)


test$Biography <- ifelse(grepl("Biography", test$genres), 1, 0)+ifelse(grepl("biography", test$plot_keywords), 1, 0)
test$Biography = ifelse(test$Biography>=1,1,0)

test$Comedy  <- ifelse(grepl("Comedy ", test$genres), 1, 0)+ifelse(grepl("comedy", test$plot_keywords), 1, 0)
test$Comedy  = ifelse(test$Comedy >=1,1,0)


test$Animation  <- ifelse(grepl("Animation", test$genres), 1, 0)+ifelse(grepl("animation", test$plot_keywords), 1, 0)
test$Animation = ifelse(test$Animation >=1,1,0)


test$Documentary  <- ifelse(grepl("Documentary", test$genres), 1, 0)+ifelse(grepl("documentary", test$plot_keywords), 1, 0)
test$Documentary = ifelse(test$Documentary >=1,1,0)






pred = predict(model,newdata = test)
pred








