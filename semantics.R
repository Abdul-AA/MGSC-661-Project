imdb <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/IMDB_data_Fall_2023.csv')

test <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/test_data_IMDB_Fall_2023.csv')

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
write.csv(summary_table$coefficients, file = "plot_keywords_coefficients.csv")
