imdb <- read.csv('../processing/IMDB_data_Fall_2023.csv')

test <- read.csv('../processing/test_data_IMDB_Fall_2023.csv')

install.packages("tm")
install.packages("SnowballC")
library(tm)
library(SnowballC)

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

# Apply the function to your DataFrame
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

# save the coefficients to a csv file
write.csv(summary_table$coefficients, file = "../plot_keywords_coefficients.csv")

keywords_pred <- c("comput", "campus", "holiday", "riot", "terror", "escap", "wish", "love")

processed_dataset <- read.csv('/midterm-project/MGSC-661-Project/final_df.csv')

library(dplyr)

# join the keywords_pred columns to the processed dataset using cbind

processed_dataset <- cbind(processed_dataset, imdb[,c("movie_id", keywords_pred)])


# processed_dataset <- processed_dataset %>% left_join(imdb %>% select(movie_id, all_of(keywords_pred)), by = "movie_id")

# save the processed dataset to a csv file
write.csv(processed_dataset, file = "/midterm-project/MGSC-661-Project/final_df.csv")


# add imdb columns "movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro, nb_faces, actor3_star_meter to the processed dataset
processed_dataset <- read.csv('/midterm-project/MGSC-661-Project/final_df.csv')

# insert column names "movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro, nb_faces, actor3_star_meter into processed_dataset, cbind on movie_id
processed_dataset <- cbind(processed_dataset, imdb[,c("movie_id", "movie_budget", "duration", "nb_news_articles", "movie_meter_IMDBpro", "nb_faces", "actor3_star_meter")])

write.csv(processed_dataset, file = "/MGSC-661-Project/final_imdb_df.csv")