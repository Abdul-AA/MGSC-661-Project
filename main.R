# libraries required
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(ggpubr)
library(e1071)
library(reshape2)
library(psych)
library(caret)
library(stargazer)
library(cvTools)
library(dplyr)
library(boot)
library(vcd)
library(tm)
library(SnowballC)
require(lmtest)
require(plm)

# load the data
imdb = read.csv('midterm-project/MGSC-661-Project/IMDB_data_Fall_2023.csv')
test = read.csv('midterm-project/MGSC-661-Project/test_data_IMDB_Fall_2023.csv')

####################
# Functions
####################

# model summary function
quick_fit = function(data){
  model_formula <- as.formula(paste("imdb_score ~ .", collapse = " + "))
  lm_model <- lm(formula = model_formula, data = data)

  return(summary(lm_model))

}

simple_linear_regression <- function(data, dependent_variable, predictor) {
  # Build the linear regression model
  model <- lm(paste(dependent_variable, "~", predictor), data = data)

  # Extract R-squared value
  r_squared <- summary(model)$r.squared

  # Extract p-value
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]

  return(data.frame(Predictor = predictor, R_squared = r_squared, P_value = p_value))
}

create_maturity = function(data,f){

  data[f] <- ifelse(grepl(f, data$maturity_rating), 1, 0)
  return(ifelse(data[f]>=1, 1, 0))
}

transform_data = function(df){

  df$Biography <- ifelse(grepl("Biography", df$genres), 1, 0)+ifelse(grepl("biography", df$plot_keywords), 1, 0)
  df$biography = ifelse(df$Biography>=1, 1, 0)

  df$Comedy  <- ifelse(grepl("Comedy ", df$genres), 1, 0)+ifelse(grepl("comedy", df$plot_keywords), 1, 0)
  df$comedy  = ifelse(df$Comedy >=1, 1, 0)


  df$Animation  <- ifelse(grepl("Animation", df$genres), 1, 0)+ifelse(grepl("animation", df$plot_keywords), 1, 0)
  df$animation = ifelse(df$Animation >=1, 1, 0)


  df$Documentary  <- ifelse(grepl("Documentary", df$genres), 1, 0)+ifelse(grepl("documentary", df$plot_keywords), 1, 0)
  df$documentary = ifelse(df$Documentary >=1, 1, 0)


  df$is_Miramax = ifelse(grepl('Miramax', df$production_company), 1, 0)
  df$is_Nov = ifelse(grepl('Nov', df$release_month), 1, 0)
  df$is_Dec = ifelse(grepl('Dec', df$release_month), 1, 0)

  df$is_color = ifelse(df$colour_film=="Color", 1, 0)


  maturity =c("R","PG-13",'Approved','TV-G','TV-14')

  for (r in maturity){
    df[r] = create_maturity(df, r)
  }
  df['TV.G'] = df['TV-G']
  df['TV.14'] = df['TV-14']

  df$actor1_length = lapply(df$actor1, nchar)
  df$genres_length = lapply(df$genres, nchar)
  df$distributor_length = lapply(df$distributor, nchar)
  df$movie_title_length = lapply(df$movie_title, nchar)

  df$actor1_length = as.numeric(df$actor1_length)
  df$genres_length = as.numeric(df$genres_length)
  df$distributor_length = as.numeric(df$distributor_length)
  df$movie_title_length = as.numeric(df$movie_title_length)
  df$comput  <- ifelse(grepl("comput", df$plot_keywords), 1, 0)
  df$holiday  <- ifelse(grepl("holiday", df$plot_keywords), 1, 0)
  df$riot  <- ifelse(grepl("riot", df$plot_keywords), 1, 0)
  df$terror  <- ifelse(grepl("terror", df$plot_keywords), 1, 0)
  df$wish  <- ifelse(grepl("wish", df$plot_keywords), 1, 0)
  df$love  <- ifelse(grepl("love", df$plot_keywords), 1, 0)


  return(df)
}

perform_chi_squared_test <- function(variable, imdb_score, variable_name) {
  contingency_table <- table(variable, imdb_score)
  chi_squared_result <- chisq.test(contingency_table)

  cat(paste("Chi-Squared Test for", variable_name, "\n"))
  print(chi_squared_result)
}

subset_numeric <- function(df) {
  return(df[, sapply(df, is.numeric)])
}

get_rmse <- function(degree, predictor) {
  formula <- as.formula(paste("imdb_score ~", paste0("poly(", predictor, ", ", degree, ")")))
  set.seed(123) # Setting seed for reproducibility
  model <- train(formula, data = p_df, method = "lm", trControl = train_control, metric = "RMSE")
  return(model$results$RMSE[1])
}

get_cv_mse = function(model,data,K){

  mse_distribution = c()

  for (i in 1:100){

    mse_distribution = c(mse_distribution,cv.glm(data,model,K = K)$delta[1])

  }

  print( mse_distribution)

  hist( mse_distribution)
}

create_logs <- function(df) {
  # Replace negative infinite values with 0 in specified columns
  df$log_nb_news_articles <- ifelse(is.infinite(df$log_nb_news_articles) & df$log_nb_news_articles < 0, 0, df$log_nb_news_articles)
  df$log_movie_meter_IMDBpro <- ifelse(is.infinite(df$log_movie_meter_IMDBpro) & df$log_movie_meter_IMDBpro < 0, 0, df$log_movie_meter_IMDBpro)
  df$log_movie_budget <- ifelse(is.infinite(df$log_movie_budget) & df$log_movie_budget < 0, 0, df$log_movie_budget)
  df$log_duration <- ifelse(is.infinite(df$log_duration) & df$log_duration < 0, 0, df$log_duration)

  # Compute log of release_year, handling potential negative or zero values
  df$log_release_year <- ifelse(df$release_year > 0, log(df$release_year), NA)

  return(df)
}

# Function to extract stem words
extract_stem_words <- function(text) {
  # lowercase the text
  text <- tolower(text)
  # Tokenize the text
  words <- unlist(strsplit(text, "\\|"))
  # Stem the words
  stemmed_words <- wordStem(words, language = "en")
  return(stemmed_words)
}

split_genres <- function(df) {
  return(df %>% separate_rows(genres, sep = "\\|"))
}

get_unique_genres <- function(df) {
  return(unique(df$genres))
}

# Function to create binary genre columns based on 'genres' and 'plot_keywords'
create_genre_dummies <- function(df, genres_list) {
  for (genre in genres_list) {
    genre_column_name <- tolower(genre)
    df[[genre_column_name]] <- ifelse(grepl(genre, df$genres) | grepl(genre_column_name, df$plot_keywords), 1, 0)
  }
  return(df)
}

# Function to print summary of linear models for genres
run_genre_regression <- function(df, genres_list) {
  for (genre in genres_list) {
    formula <- as.formula(paste("imdb_score ~", genre))
    lm_genre <- lm(formula, data = df)
    cat("Genre:", genre, "\n")
    print(summary(lm_genre))
  }
}

# Plotting
plot_categorical <- function(column_name, data, title) {
  ggplot(data, aes_string(x = column_name)) +
    geom_bar() +
    labs(title = title, x = column_name, y = "Frequency")
}

test_polynomial_relationship <- function(data, dependent_variable, predictor, max_degree = 5, num_folds = 5) {
  # Create a data frame to store results
  results <- list(degree = c(),MSE= c())

  # Fit a linear model
  fit <- lm(paste(dependent_variable, "~", predictor), data = data)

  # Create diagnostic plots to assess linearity
  res_plot = residualPlot(fit,main = predictor)

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


####################
# EDA: Genres
####################


# Genre-related operations
IMDB_test2 <- split_genres(test)
testset_genres <- get_unique_genres(IMDB_test2)
existing_genres <- c("Action", "Adventure", "Scifi", "Thriller", "Musical", "Romance", "Western", "Sport", "Horror", "Drama", "War", "Animation", "Crime")
all_genres <- union(testset_genres, existing_genres)

# Creating and populating new columns for genres
df <- create_genre_dummies(imdb, all_genres)

# Validate the transformations
genre_counts <- colSums(df[all_genres])
print(setNames(genre_counts, all_genres))

# Running regression models for each genre
run_genre_regression(df, all_genres)

# Writing output
write.csv(df, "../genres_dummied_z.csv", row.names = FALSE)

# Further data processing
useful_genres <- c("action", "adventure", "scifi", "thriller", "western", "sport", "horror", "drama", "war", "crime", "documentary", "biography", "fantasy", "comedy", "family")


####################
# EDA: Maturity Rating
####################

# Convert specified columns to factors
categorical_columns <- c("release_month", "language", "country", "maturity_rating",
                         "aspect_ratio", "distributor", "director", "actor1", "actor2",
                         "actor3", "colour_film", "cinematographer", "production_company")

imdb[categorical_columns] <- lapply(imdb[categorical_columns], factor)

# Run Chi-Squared tests
for (col in categorical_columns) {
  perform_chi_squared_test(col, imdb$imdb_score, col)
}

# Apply function to create plots
plots <- lapply(categorical_columns, function(col) plot_categorical(col, imdb, col))
lapply(plots, print)

# Creating dummy variables and performing linear regression
maturity_categories <- unique(imdb$maturity_rating)
lm_list <- list()

for (category in maturity_categories) {
  dummy_name <- paste0("dummy_", category)
  imdb[[dummy_name]] <- as.integer(imdb$maturity_rating == category)

  formula <- as.formula(paste("imdb_score ~", dummy_name))
  lm_model <- lm(formula, data = imdb)

  lm_list[[category]] <- list(
    category = category,
    model = lm_model,
    summary = summary(lm_model)
  )
}

# Printing regression summaries
for (category_info in lm_list) {
  cat("Category:", category_info$category, "\n")
  print(category_info$summary)
}

# Significant dummies based on regression analysis
significant_dummies <- c("dummy_R", "dummy_PG-13", "dummy_Approved", "dummy_TV-G", "dummy_TV-14")


####################
# EDA: Plot Keywords
####################

stemmed_plot_keywords <- sapply(test$plot_keywords, extract_stem_words)

# Split the plot_keywords column and check for the presence of each keyword
for (keyword in stemmed_plot_keywords) {
  imdb[[keyword]] <- ifelse(grepl(keyword, imdb$plot_keywords), 1, 0)
}

lm <- lm(imdb_score~., data = imdb[,c("imdb_score", stemmed_plot_keywords)])
summary_table <- summary(lm)
summary_table$coefficients
# out of the 60, 22 are undefinied because of singularities (i.e. not enough data to estimate the coefficients)
# can use "comput", "campus" (0.05), "holiday", "riot", "terror", "escap" (0.05), "wish", "love"


####################
# EDA: Numerical Features
####################

numerical_features <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "action", "musical", "western", "sport", "horror", "animation", "movie_meter_IMDBpro")
target_variable <- "imdb_score"
max_degree <- 5
num_folds <- 5

linearity_check <- list()

for (v in numerical_features) {
  result <- tryCatch({
    table <- test_polynomial_relationship(imdb, target_variable, v, max_degree, num_folds)
    list(variable = v, results = table)
  }, error = function(err) {
    message(paste("Error in processing", v, ":", err$message))
    NULL
  })

  if (!is.null(result)) {
    linearity_check[[length(linearity_check) + 1]] <- result
  }
}

print(linearity_check)

####################
# EDA: Seasonality
####################

# Assuming train dataset is already loaded and release_month is available
# Create a new variable for seasons
train$season <- case_when(
  train$release_month %in% c("Dec", "Jan", "Feb") ~ "Winter",
  train$release_month %in% c("Mar", "Apr", "May") ~ "Spring",
  train$release_month %in% c("Jun", "Jul", "Aug") ~ "Summer",
  train$release_month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
)

# Convert to factor and set Spring as reference
train$season <- factor(train$season, levels = c("Spring", "Summer", "Autumn", "Winter"))

# Linear model to evaluate the effect of seasons on IMDb score
season_model <- lm(imdb_score ~ season, data = train)
summary(season_model)

# Plotting
train %>%
  group_by(season) %>%
  summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = mean_imdb_score, fill = season)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean IMDb Score by Season", x = "Season", y = "Mean IMDb Score") +
  theme_minimal()


####################
# EDA: IMAX
####################

numerical_features <- c("movie_budget", "duration", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "action", "adventure", "scifi", "thriller", "musical", "romance", "western", "sport", "horror", "drama", "war", "animation", "crime", "movie_meter_IMDBpro")
Cat_train <- train[, !names(train) %in% numerical_features]

# Creating is_IMAX variable
Cat_train$is_IMAX <- ifelse(Cat_train$aspect_ratio >= 1.9, 1, 0)

# Linear model to evaluate the effect of IMAX on IMDb score
imax_model <- lm(imdb_score ~ is_IMAX, data = Cat_train)
summary(imax_model)

# Plotting
Cat_train %>%
  group_by(is_IMAX) %>%
  summarize(mean_imdb_score = mean(imdb_score, na.rm = TRUE)) %>%
  ggplot(aes(x = factor(is_IMAX), y = mean_imdb_score, fill = factor(is_IMAX))) +
  geom_bar(stat = "identity") +
  labs(title = "Mean IMDb Score by IMAX", x = "IMAX Format (0 = No, 1 = Yes)", y = "Mean IMDb Score") +
  theme_minimal()


####################
# Data Manipulation & Model Development
####################

# transform data as per EDA
df <- transform_data(imdb)
test = transform_data(test)


hist(df$nb_news_articles,breaks = 100)
hist(df$movie_meter_IMDBpro, breaks = 100)
hist(df$duration, breaks = 100)
hist(df$movie_budget, breaks = 100)
hist(df$actor1_star_meter, breaks = 100)
hist(log(df$actor1_star_meter), breaks = 100)

# converting the above columns into log data
df$log_movie_budget = log(df$movie_budget)
df$log_actor1_star_meter = log(df$actor1_star_meter)
df$log_actor1_star_meter <- ifelse(is.infinite(df$log_actor1_star_meter) & df$log_actor1_star_meter < 0, 0, df$log_actor1_star_meter)

df$log_actor3_star_meter = log(df$actor3_star_meter)
df$log_actor3_star_meter <- ifelse(is.infinite(df$log_actor3_star_meter) & df$log_actor3_star_meter < 0, 0, df$log_actor3_star_meter)

df$log_nb_faces = log(df$nb_faces)
df$log_nb_faces <- ifelse(is.infinite(df$log_nb_faces) & df$log_nb_faces < 0, 0, df$log_nb_faces)

test$log_actor1_star_meter = log(test$actor1_star_meter)
test$log_actor1_star_meter <- ifelse(is.infinite(test$log_actor1_star_meter) & test$log_actor1_star_meter < 0, 0, test$log_actor1_star_meter)

View(test)

hist(df$genres_length, breaks = 100)
hist(df$distributor_length, breaks = 100)
hist(df$actor3_star_meter, breaks = 10)
hist(df$nb_faces, breaks = 100)
hist(df$log_nb_news_articles, breaks = 100)
hist(df$log_movie_meter_IMDBpro, breaks = 100)
hist(log(df$duration), breaks = 100)
hist(log(df$movie_budget), breaks = 100)
hist(log(df$log_nb_faces),breaks = 100)


numerical_features = c("movie_title_length","aspect_ratio" , "genres_length" ,"distributor_length","actor1_length",
                       "movie_budget","duration" ,"nb_news_articles","movie_meter_IMDBpro" , "nb_faces" ,"actor3_star_meter" ,"actor1_star_meter",
                       "log_nb_news_articles" ,"log_movie_meter_IMDBpro", "log_movie_budget", "log_duration" ,"log_actor1_star_meter","log_actor3_star_meter")

results <- data.frame(Predictor = character(0), R_squared = numeric(0), P_value = numeric(0))


# Loop through the predictors and run simple linear regressions
for (predictor in numerical_features) {
  regression_result <- simple_linear_regression(df, "imdb_score", predictor)
  results <- rbind(results, regression_result)
}

print(results)

# filter a model based on p-values
fit1 = lm(imdb_score~movie_title_length+
          genres_length+
          distributor_length+
          actor1_star_meter+
          actor1_length+
          movie_budget+
          duration+
          nb_news_articles+
          movie_meter_IMDBpro+
          nb_faces, data = df)


residualPlots(fit1)

# adding the log data
fit2 = lm(imdb_score~movie_title_length+
          genres_length+
          distributor_length+
          log_actor1_star_meter+
          actor1_length+
          log_movie_budget+
          log_duration+
          log_nb_news_articles+
          log_movie_meter_IMDBpro+
          nb_faces, data = df)

residualPlots(fit2)

summary(fit2)

glm_model <- glm(imdb_score~poly(movie_budget, 1,raw = TRUE)+
                poly(duration, 1, raw = TRUE)+
                poly(nb_news_articles, 1,raw = TRUE)+
                poly(movie_meter_IMDBpro, 1,raw = TRUE)+
                nb_faces+
                release_year+
                movie_id+
                # movie_title_length+
                aspect_ratio+
                distributor_length+
                is_Miramax+
                is_Nov+
                is_Dec+
                comput+
                holiday+
                riot+
                # terror+
                # wish+
                #love+
                action+
                #sport+
                horror+
                drama+
                # crime+
                documentary+
                biography+
                comedy+
                animation+
                R+
                TV.G+
                TV.14+
                is_color
                ,data = df)

summary(glm_model)

significant_variables <- c("movie_budget",
                           "duration", "nb_news_articles",
                           "movie_meter_IMDBpro", "nb_faces",
                           "release_year", "movie_id", "aspect_ratio",
                           "distributor_length", "is_Miramax", "is_Nov", "is_Dec",
                           "comput", "holiday", "riot", "action", "comedy",
                           "horror", "drama", "documentary",
                           "biography", "animation", "R",
                           "TV.G", "TV.14", "is_color")

get_cv_mse(glm_model,df,5)


# glm with interaction terms
glm_model2 <- glm(imdb_score~movie_budget+
                  duration+
                  nb_news_articles+
                  movie_meter_IMDBpro+
                  nb_faces+
                  release_year+
                  aspect_ratio+
                  distributor_length+
                  is_Miramax+
                  is_Nov+
                  is_Dec+
                  comput+
                  holiday+
                  riot+
                  action+
                  horror+
                  drama+
                  documentary+
                  biography+
                  comedy+
                  animation+
                  R+
                  TV.G+
                  TV.14+
                  is_color+
                  duration*animation+
                  duration*documentary+
                  duration*biography+
                  duration*drama+
                  duration*animation+
                  duration*horror+
                  duration*action+
                  movie_budget*animation+
                  movie_budget*documentary+
                  movie_budget*biography+
                  movie_budget*drama+
                  movie_budget*animation+
                  movie_budget*horror+
                  movie_budget*action+
                  nb_news_articles*animation+
                  nb_news_articles*documentary+
                  nb_news_articles*biography+
                  nb_news_articles*drama+
                  nb_news_articles*animation+
                  nb_news_articles*horror+
                  nb_news_articles*action+
                  movie_meter_IMDBpro*animation+
                  movie_meter_IMDBpro*documentary+
                  movie_meter_IMDBpro*biography+
                  movie_meter_IMDBpro*drama+
                  movie_meter_IMDBpro*animation+
                  movie_meter_IMDBpro*horror+
                  movie_meter_IMDBpro*action+
                  duration*is_color+
                  movie_budget*is_color+
                  nb_news_articles*is_color+
                  movie_meter_IMDBpro*is_color
    ,data = cbind(df['imdb_score'],df[,significant_variables]))



summary(glm_model2)

get_cv_mse(glm_model2,df,5)


glm_model3 <- glm(imdb_score~movie_budget+
                  duration+
                  nb_news_articles+
                  movie_meter_IMDBpro+
                  release_year+
                  aspect_ratio+
                  distributor_length+
                  is_Miramax+
                  is_Nov+
                  comput+
                  holiday+
                  riot+
                  action+
                  horror+
                  drama+
                  documentary+
                  biography+
                  comedy+
                  animation+
                  R+
                  TV.G+
                  TV.14+
                  is_color+
                  duration*biography+
                  duration*drama+
                  duration*animation+
                  duration*action+
                  nb_news_articles*documentary+
                  nb_news_articles*animation+
                  nb_news_articles*horror+
                  nb_news_articles*action+
                  movie_meter_IMDBpro*animation+
                  movie_meter_IMDBpro*documentary+
                  movie_meter_IMDBpro*biography+
                  movie_meter_IMDBpro*drama+
                  movie_meter_IMDBpro*animation+
                  movie_meter_IMDBpro*horror+
                  movie_meter_IMDBpro*action+
                  duration*is_color+
                  movie_budget*is_color

  ,data = cbind(df['imdb_score'],df[,significant_variables]))

summary(glm_model3)

outlierTest(glm_model3)
vif(glm_model3)

get_cv_mse(glm_model3,cbind(df['imdb_score'],df[,significant_variables]),10)

downsized_df = cbind(df['imdb_score'],df[,significant_variables])


glm_model4 <- glm(imdb_score~poly(movie_budget, 1,raw = TRUE) +
                  poly(duration, 2,raw = TRUE)+
                  poly(nb_news_articles, 1,raw = TRUE)+
                  poly(movie_meter_IMDBpro, 2)+
                  nb_faces+
                  release_year+
                  movie_id+
                  aspect_ratio+
                  distributor_length+
                  is_Miramax+
                  is_Nov+
                  comput+
                  holiday+
                  riot+
                  action+
                  horror+
                  documentary+
                  animation +
                  TV.14+
                  is_color

  ,data = downsized_df)

score = c()

for (i in 1:100){

  score = c(score,cv.glm(df,glm_model4,K = 5)$delta[1])

}

score

hist(score)

get_cv_mse(glm_model4,df,5)


test$log_nb_news_articles = log(test$nb_news_articles)

test$log_movie_meter_IMDBpro = log(test$movie_meter_IMDBpro)
test$log_movie_budget = log(test$movie_budget)
test$log_duration = log(test$duration)


test$log_nb_news_articles <- ifelse(is.infinite(test$log_nb_news_articles) & test$log_nb_news_articles < 0, 0, test$log_nb_news_articles)
test$log_movie_meter_IMDBpro <- ifelse(is.infinite(test$log_movie_meter_IMDBpro) & test$log_movie_meter_IMDBpro < 0, 0, test$log_movie_meter_IMDBpro)
test$log_movie_budget <- ifelse(is.infinite(test$log_movie_budget) & test$log_movie_budget < 0, 0, test$log_movie_budget)
test$log_duration <- ifelse(is.infinite(test$log_duration) & test$log_duration < 0, 0, test$log_duration)


test$log_release_year = log(test$release_year)

glm_model5 <- glm(imdb_score~log_movie_budget +
                  log_duration+
                  poly(log_nb_news_articles, 2)+
                  poly(log_movie_meter_IMDBpro, 2)+
                  poly(log_actor1_star_meter,2)+
                  nb_faces+
                  release_year+
                  genres_length+
                  aspect_ratio+
                  distributor_length+
                  movie_title_length+
                  is_Miramax+
                  is_Nov+
                  is_Dec+
                  comput+
                  holiday+
                  riot+
                  terror+
                  wish+
                  love+
                  action+
                  sport+
                  horror+
                  drama+
                  crime+
                  documentary+
                  biography+
                  comedy +
                  animation +
                  R +
                  TV.G+
                  TV.14+
                  is_color, data = df)

outlierTest(glm_model5)


summary(lm_model)
vif(lm_model)
ncvTest(lm_model)
correct = coeftest(lm_model, vcov=vcovHC(lm_model, type="HC1"))


get_cv_mse(glm_model5,df,K=5)
residualPlots(glm_model5)


pred = predict(lm_model, newdata = test)
cbind(test$movie_title,as.vector(pred))

pred = predict(glm_model5, newdata = test)


cbind(test$movie_title,as.vector(pred))
residualPlots(lm_model)

get_cv_mse(glm_model,df,K=5)

pred = predict(glm_model, newdata = test)
cbind(test$movie_title,as.vector(pred))

summary(lm_model)


# Final LM Ordi. least sq. (R^2)
lm_final = lm(imdb_score~log_movie_budget +
              log_duration+
              poly(log_nb_news_articles, 1, raw = TRUE)+
              poly(log_movie_meter_IMDBpro, 4)+
              is_color+
              poly(genres_length,2)+
              biography+
              documentary+
              is_Miramax+
              horror+
              drama+
              documentary+
              biography+
              animation+
              R+
              TV.14+
              is_color
              ,data = df)

outlierTest(lm_final)
df_without_outliers = df[-c(1806,1581,191,395,1436,1255,989),]

# Final GLM Max Likelihood (for 5 fold cv)
glm_final = glm(imdb_score~log_movie_budget +
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                is_color+
                poly(genres_length,2)+
                biography+
                animation+
                documentary+
                is_Miramax+
                horror+
                drama+
                documentary+
                biography+
                animation+
                R+
                TV.14+
                is_color
                , data = df_without_outliers)

get_cv_mse(glm_final,df_without_outliers,5)

pred = predict(glm_final, newdata = test)
cbind(test$movie_title,as.vector(pred))