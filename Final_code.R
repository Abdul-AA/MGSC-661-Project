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


imdb <- read.csv('/midterm-project/MGSC-661-Project/IMDB_data_Fall_2023.csv')

test <- read.csv('/midterm-project/MGSC-661-Project/test_data_IMDB_Fall_2023.csv')

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

lm_keywords <- lm(imdb_score~., data = imdb[,c("imdb_score", stemmed_plot_keywords)])
summary_table <- summary(lm_keywords)
summary_table$coefficients

# out of the 60, 22 are undefinied because of singularities (i.e. not enough data to estimate the coefficients)
# Useful dummies above the significance level: "comput", "campus", "holiday", "riot", "terror", "escap", "wish", "love"
keywords_pred <- c("comput", "campus", "holiday", "riot", "terror", "escap", "wish", "love")

# processed_dataset <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/MGSC-661-Project/processed_dataset.csv')

library(dplyr)

processed_dataset <- processed_dataset %>% left_join(imdb %>% select(movie_id, all_of(keywords_pred)), by = "movie_id")

# save the processed dataset to a csv file
# write.csv(processed_dataset, file = "/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/MGSC-661-Project/processed_dataset.csv")

