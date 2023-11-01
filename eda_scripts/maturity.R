IMDB=read.csv('/midterm-project/MGSC-661-Project/IMDB_data_Fall_2023.csv')
attach(IMDB)

IMDB_test=read.csv('/midterm-project/MGSC-661-Project/test_data_IMDB_Fall_2023.csv')

categorical_columns = c("release_month", "language", "country", "maturity_rating", "aspect_ratio", "distributor", "director",
                        "actor1", "actor2", "actor3", "colour_film", "cinematographer", "production_company")

for (col in categorical_columns) {
  IMDB[[col]] <- factor(IMDB[[col]])
}

install.packages("vcd")
library(vcd)

perform_chi_squared_test <- function(variable, imdb_score, variable_name) {
  contingency_table <- table(variable, imdb_score)
  chi_squared_result <- chisq.test(contingency_table)
  
  cat(paste("Chi-Squared Test for", variable_name, "\n"))
  print(chi_squared_result)
}

for (col in categorical_columns) {
  perform_chi_squared_test(IMDB[[col]], imdb_score, variable_name = col)
}

#Variables that may not be statistically significant include:
#release_month, language, maturity_rating, actor3

#Variables that are statistically significant include:
#country, aspect_ratio, distributor, director, actor1, actor2, colour_film, cinematographer, production_company 

library(ggplot2)

rm_plot <- ggplot(IMDB, aes(x = release_month)) +
  geom_bar() +
  labs(title = "release_month", x = "release_month", y = "Frequency")
print(rm_plot)


language_plot <- ggplot(IMDB, aes(x = language)) +
  geom_bar() +
  labs(title = "Language", x = "Language", y = "Frequency")
print(language_plot)

country_plot <- ggplot(IMDB, aes(x = country)) +
  geom_bar() +
  labs(title = "country", x = "country", y = "Frequency")
print(country_plot)

mr_plot <- ggplot(IMDB, aes(x = maturity_rating)) +
  geom_bar() +
  labs(title = "maturity_rating", x = "maturity_rating", y = "Frequency")
print(mr_plot)

ar_plot <- ggplot(IMDB, aes(x = aspect_ratio)) +
  geom_bar() +
  labs(title = "aspect_ratio", x = "aspect_ratio", y = "Frequency")
print(ar_plot)

distributor_plot <- ggplot(IMDB, aes(x = distributor)) +
  geom_bar() +
  labs(title = "distributor", x = "distributor", y = "Frequency")
print(distributor_plot)

director_plot <- ggplot(IMDB, aes(x = director)) +
  geom_bar() +
  labs(title = "director", x = "director", y = "Frequency")
print(director_plot)

a1_plot <- ggplot(IMDB, aes(x = actor1)) +
  geom_bar() +
  labs(title = "actor1", x = "actor1", y = "Frequency")
print(a1_plot)

a2_plot <- ggplot(IMDB, aes(x = actor2)) +
  geom_bar() +
  labs(title = "actor2", x = "actor2", y = "Frequency")
print(a2_plot)

a3_plot <- ggplot(IMDB, aes(x = actor3)) +
  geom_bar() +
  labs(title = "actor3", x = "actor3", y = "Frequency")
print(a3_plot)

cf_plot <- ggplot(IMDB, aes(x = colour_film)) +
  geom_bar() +
  labs(title = "colour_film", x = "colour_film", y = "Frequency")
print(cf_plot)


#Creating dummies for maturity_rating from training set
# Load the required libraries
library(dplyr)

# Read the training data
IMDB=read.csv('../processing/IMDB_data_Fall_2023.csv')

# List of unique categories within "maturity_rating"
maturity_categories <- unique(IMDB$maturity_rating)

# Initialize a list to store the regression models and results
lm_list <- list()

# Loop through each category and perform linear regression
for (category in maturity_categories) {
  # Create a dummified variable for the current category
  IMDB[paste0("dummy_", category)] <- as.integer(IMDB$maturity_rating == category)


  # Fit a linear regression model for the current category
  # lm_model <- lm(imdb_score ~ dummy, data = IMDB_dummified)
  #
  # # Store the model and results in the list
  # lm_list[[category]] <- list(
  #   category = category,
  #   model = lm_model,
  #   summary = summary(lm_model)
  # )
}

# Access results for each category and print them
for (category_info in lm_list) {
  cat("Category:", category_info$category, "\n")
  print(category_info$summary)
}


###Significant dummies from maturity_level are: R, PG-13, Approved, TV-G, TV-14, 
# add the dummies to the processed dataset
processed_dataset <- read.csv('/midterm-project/MGSC-661-Project/Cat_featurs_except_genres_key_words.csv')

# insert column names dummy_R, dummy_PG-13, dummy_Approved, dummy_TV-G, dummy_TV-14 into processed_dataset, left join on movie_id
useful_dummies <- c("dummy_R", "dummy_PG-13", "dummy_Approved", "dummy_TV-G", "dummy_TV-14")
processed_dataset <- processed_dataset %>% left_join(IMDB %>% select(movie_id, all_of(useful_dummies)), by = "movie_id")

write.csv(processed_dataset, file = "/midterm-project/MGSC-661-Project/processed_imdb_dataset.csv")


# add "movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro, nb_faces, actor3_star_meter
useful_numericals <- c("movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro", "nb_faces", "actor3_star_meter")

processed_dataset <- processed_dataset %>% left_join(IMDB %>% select(movie_id, all_of(useful_numericals)), by = "movie_id")

write.csv(processed_dataset, file = "/midterm-project/MGSC-661-Project/processed_imdb_dataset.csv")