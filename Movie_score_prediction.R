df <- read.csv("~/Downloads/IMDB_data_Fall_2023.csv")
attach(df)
head(df)

# may need this for data manipulation;
library(dplyr)

# checking the shape of the df
dim(df)

#lets fetch the column names:
colnames(df)

#creating a function to subset numeric variables
subset_numeric <- function(df) {
  return(df[, sapply(df, is.numeric)])
}


numeric_df <- subset_numeric(df)

dim(numeric_df)

# 26 numeric features; however, some of this may be binary. Let us check


head(numeric_df)
numeric_df <- numeric_df[, sapply(numeric_df, function(col) length(unique(col)) > 2)]
colnames(numeric_df)
# let us check the new shape

dim(numeric_df)
# we have 13 numeric features now we can go ahead to perform multivariate and univariate analysis for these features

#let us explore the new df

head(numeric_df)

# fetch the numeric col names

colnames(numeric_df)
# Remove 'movie_id', aspect_ratio and 'imdb_score' from the list of predictors as they aren't meant for this kind of plot. Note that these are the numeric cols
predictors <- c("movie_budget", "release_day", "release_year", "duration", "nb_news_articles", "actor1_star_meter", 
                "actor2_star_meter", "actor3_star_meter", "nb_faces", 
                "movie_meter_IMDBpro")

# Univariate stats for each of the numeric feature:

# Checking the descriptive stats for numeric cols
summary(df[, predictors])
library(ggplot2)
# Lets check the distribution of each numeric col
h_plots=lapply(predictors, function(pred) {
  ggplot(df, aes_string(x = pred)) + 
    geom_histogram(fill = 'blue', color = 'black', bins = 30) +
    theme_minimal() +
    labs(title = paste("Distribution of", pred), x = pred, y='Frequency')
})
library(ggpubr)
# Arrange the plots in 2 matrices
h_matrix_1 <- ggarrange(plotlist = h_plots[1:6], ncol = 3, nrow = 2)
h_matrix_2 <- ggarrange(plotlist = h_plots[7:11], ncol = 3, nrow = 2)

# Display the matrices
h_matrix_1
h_matrix_2


# Lets use boxplots to check the distributuion and visually assess each numerical predictors for outliers

b_plots=lapply(predictors, function(pred) {
  ggplot(df, aes_string(y = pred)) + 
    geom_boxplot(fill = 'lightgreen', color = 'black') +
    theme_minimal() +
    labs(title = paste("Boxplot of", pred), y = pred)
})
# Arrange the plots in 2 matrices
b_matrix_1 <- ggarrange(plotlist = b_plots[1:6], ncol = 3, nrow = 2)
b_matrix_2 <- ggarrange(plotlist = b_plots[7:11], ncol = 3, nrow = 2)


# Display the matrices
b_matrix_1
b_matrix_2


#lets look at the skewness of each numeric var
install.packages('e1071')
library(e1071)

skewness_values <- sapply(df[, predictors], skewness)
kurtosis_values <- sapply(df[, predictors], kurtosis)

data.frame(Predictor = predictors, Skewness = skewness_values, Kurtosis = kurtosis_values)



# bivariate analysis of each predictor against the target variable: 


# Iterate over each predictor and create plots
s_plots <- lapply(predictors, function(pred) {
  ggplot(df, aes_string(x = pred, y = "imdb_score")) +
    geom_point(aes(color = imdb_score), alpha = 0.5) + 
    labs(title = paste("IMDB Score vs", pred), x = pred, y = "IMDB Score") +
    theme_minimal()
})


# Arrange the plots in 2 matrices
s_matrix_1 <- ggarrange(plotlist = s_plots[1:6], ncol = 3, nrow = 2)
s_matrix_2 <- ggarrange(plotlist = s_plots[7:11], ncol = 3, nrow = 2)

# Display the matrices
s_matrix_1
s_matrix_2

# Remove identifier cols
df <- df[, !names(df) %in% c("movie_title", "movie_id", "imdb_link")]
# 1. Identify Binary Columns

binary_cols <- names(df)[sapply(df, function(col) length(unique(col)) == 2)]

# 2. One-Hot Encoding

categorical_vars <- c("language", "country", 
                      "maturity_rating", "distributor", "director", "actor1", "actor2", 
                      "actor3", "genres","colour_film","aspect_ratio", "plot_keywords", "cinematographer", 
                      "production_company","action", "adventure", "scifi", "thriller", "musical", "romance", "western", "sport", 
                      "horror", "drama", "war", "animation", "crime")

#categorical_vars <- setdiff(categorical_vars, binary_cols)
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Remove the specified columns from the dataframe
df <- df[, !names(df) %in% c("movie_title", "movie_id", "imdb_link")]

large_level_vars <- categorical_vars[sapply(df[, categorical_vars], function(x) length(levels(x)) > 50)]
# removing the target col and columns with too many unique values
predictors_all <- setdiff(names(df), c("imdb_score", large_level_vars))

# List to store models
models_list <- list()

p_values <- numeric(length(predictors_all))
r_squared_values <- numeric(length(predictors_all))

for (i in 1:length(predictors_all)) {
  formula_str <- paste("imdb_score ~", predictors_all[i])
  model <- lm(formula_str, data = df)
  
  models_list[[predictors_all[i]]] <- model  # Storing model in the list
  
  p_values[i] <- ifelse(length(coef(model)) > 1, summary(model)$coefficients[2, 4], summary(model)$coefficients[1, 4])
  r_squared_values[i] <- summary(model)$r.squared
}

# Creating the results dataframe
result_df <- data.frame(
  Predictor = predictors_all,
  P_Value = p_values,
  R_Squared = r_squared_values
)

# Print the dataframe
print(result_df)

# this didnt work: Thought we might need this for our report
#library(stargazer)
#stargazer(models_list[1:38], type='html')

#multivariate analysis on the numerial features excluding the binary ones

install.packages('reshape2')
library(reshape2)  # for melt function

# Subset the numeric columns specified: Recreating numeric_df to exclude id
numeric_df <- df[c("movie_budget", "release_day", "release_year", "duration", 
                   "nb_news_articles", "actor1_star_meter", 
                   "actor2_star_meter", "actor3_star_meter", "nb_faces", 
                   "movie_meter_IMDBpro","imdb_score")]

# Compute the correlation matrix
cor_matrix <- cor(numeric_df)

# Melt the correlation matrix for ggplot
cor_melted <- melt(cor_matrix)

# Plot
ggplot(data = cor_melted, aes(x = Var1, y = Var2)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = round(value, 2)), size = 3) + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(min(cor_melted$value), max(cor_melted$value)), name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed()

# Iterate over each predictor and create plots with regression lines
s_plots <- lapply(predictors, function(pred) {
  ggplot(df, aes_string(x = pred, y = "imdb_score")) +
    geom_point(aes(color = imdb_score), alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, color = "red", aes(group = 1)) +  # Added this line for regression
    labs(title = paste("IMDB Score vs", pred), x = pred, y = "IMDB Score") +
    theme_minimal()
})

# Arrange the plots in 3 matrices
s_matrix_1 <- ggarrange(plotlist = s_plots[1:4], ncol = 2, nrow = 2)
s_matrix_2 <- ggarrange(plotlist = s_plots[6:9], ncol = 2, nrow = 2)
s_matrix_3 <- ggarrange(plotlist = s_plots[9:11], ncol = 2, nrow = 2)

# Display the matrices
s_matrix_1
s_matrix_2
s_matrix_3



# lets look for the categorical columns with large levels; these columns are likely useless and reduce model interpretability
large_level_vars <- categorical_vars[sapply(df[, categorical_vars], function(x) length(levels(x)) > 50)]
print(large_level_vars)
# Remove the variables with high levels from the predictor list
predictors_mlr <- setdiff(predictors_all, large_level_vars)

# Create the formula
formula <- as.formula(paste("imdb_score ~", paste(predictors_mlr, collapse = " + ")))

# Fit the model for the entire data 
model1 <- lm(formula, data = df)
summary(model1)

library('car')
#residualPlots(model1)

# lets create a linear model for the numeric data

# Create the formula using the numeric predictors
formula_numeric <- as.formula(paste("imdb_score ~", paste(predictors, collapse = " + ")))

# Fit the MLR model with only numeric predictors
model_numeric <- lm(formula_numeric, data = df)

# Display the summary of the model
summary(model_numeric)
# let us check for linearity
residualPlots(model_numeric)

# let us check for heteroskedasticity
ncvTest(model_numeric)


# let us correct hetero....

require(lmtest)
require(plm)

coeftest(model_numeric, vcov=vcovHC(model_numeric, type='HC1'))
# no colinearity as suggested by heatmap 
library(psych)
vif(model_numeric)

###We can create a model based on linearity and select predictors based on p-values. There is no collinearity in our dataset



library(caret)
# New  Processed data
p_df <- read.csv("~/Desktop/merged_df.csv")

# looking for the optimal polynomial combo for the nonlinear variables
#predictor=colnames(p_df)
nonlinear_vars <- c("movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro")

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Define a function to calculate the RMSE for a given degree of polynomial
get_rmse <- function(degree, predictor) {
  formula <- as.formula(paste("imdb_score ~", paste0("poly(", predictor, ", ", degree, ")")))
  set.seed(123) # Setting seed for reproducibility
  model <- train(formula, data = p_df, method = "lm", trControl = train_control, metric = "RMSE")
  return(model$results$RMSE[1])
}

# Identify the best polynomial degree for each non-linear predictor
best_degrees <- list()

for (var in nonlinear_vars) {
  rmse_values <- sapply(1:5, get_rmse, predictor = var)
  best_degrees[[var]] <- which.min(rmse_values)
}
best_degrees


predictors_mlr=setdiff(colnames(p_df),'imdb_score')

# looking for the optimal poly combo for the numeric vars
# Identify numeric non-binary columns
numeric_vars <- sapply(p_df, is.numeric)
binary_vars <- sapply(p_df, function(x) all(x %in% c(0,1)))
nonbinary_numeric_vars <- setdiff(names(p_df)[numeric_vars & !binary_vars], 'imdb_score')

# Set up cross-validation
train_control <- trainControl(method = "cv", number = 5)

# Define a function to calculate the RMSE for a given degree of polynomial
get_rmse <- function(degree, predictor) {
  formula <- as.formula(paste("imdb_score ~", paste0("poly(", predictor, ", ", degree, ")")))
  set.seed(123) # Setting seed for reproducibility
  model <- train(formula, data = p_df, method = "lm", trControl = train_control, metric = "RMSE")
  return(model$results$RMSE[1])
}

# Identify the best polynomial degree for each non-binary numeric predictor
best_degrees <- list()

for (var in nonbinary_numeric_vars) {
  rmse_values <- sapply(1:5, get_rmse, predictor = var)
  best_degrees[[var]] <- which.min(rmse_values)
}
best_degrees
which.min(rmse_values)

# applying the polynomial terms found from the gridsearch to create the final model
polynomial_terms <- sapply(names(best_degrees), function(var) {
  paste0("poly(", var, ", ", best_degrees[[var]], ")")
})

# setting the predictors up 
predictors_mlr=setdiff(colnames(p_df),'imdb_score')

linear_terms <- setdiff(predictors_mlr,  nonbinary_numeric_vars)
all_predictors <- c(polynomial_terms, linear_terms)

formula_final <- as.formula(paste("imdb_score ~", paste(all_predictors, collapse = " + ")))

# Fit the final model
final_model <- lm(formula_final, data = p_df)
summary(final_model)


# Setting up cross-validation to evaluate the performance of the final model
train_control <- trainControl(method = "cv", number = 5)

# Training the model using cross-validation with 5 folds
set.seed(123)  # Setting seed for reproducibility
cv_model <- train(formula_final, data = p_df, method = "lm", trControl = train_control, metric = "RMSE")

# Display the results
print(cv_model)



install.packages("cvTools")
library(cvTools)


# comparing the predicted scores to the actuals in a dataframe
set.seed(123)
cv_indices <- createFolds(p_df$imdb_score, k = 5)
predictions <- numeric()  # Vector to store predictions
actuals <- numeric()      # Vector to store actual values

for (k in 1:5) {
  train_data <- p_df[-cv_indices[[k]], ]
  valid_data <- p_df[cv_indices[[k]], ]
  
  model <- lm(formula_final, data = train_data)
  preds <- predict(model, newdata = valid_data)
  
  predictions <- c(predictions, preds)
  actuals <- c(actuals, valid_data$imdb_score)
}

results <- data.frame(Actual = actuals, Predicted = predictions)
print(results)
