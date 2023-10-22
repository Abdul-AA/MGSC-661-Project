# todo: discuss with group if we wanna do this

# Load the required libraries
library(tm)

install.packages("lsa")
library(lsa)

# Sample data
plot_keywords <- c("shower|stripper|tap dancing|underwear|undressing",
                   "retirement|spoof|terrorist|transsexual|undercover",
                   "reference to ben affleck|reference to brad pitt|reference to george clooney|reference to jack nicholson|website",
                   "race relations|racism|racist|social problem|stereotype",
                   "punk|punk rocker|repossession|televangelist|ufo",
                   "professor|research|sex|sexuality|student",
                   "prank gone wrong|scantily clad female|shotgun|sorority house|stabbed in the neck",
                   "police|revenge|sex|suburb|video camera",
                   "police officer killed|police officer shot in the chest|police officer shot in the forehead|police officer shot in the head|police shootout")

plot_keywords <- "police officer"
test_sentence <- "police"

# Preprocess the data
cleaned_data <- Corpus(VectorSource(plot_keywords))
cleaned_data <- tm_map(cleaned_data, content_transformer(tolower))
cleaned_data <- tm_map(cleaned_data, removePunctuation)
cleaned_data <- tm_map(cleaned_data, removeNumbers)
cleaned_data <- tm_map(cleaned_data, removeWords, stopwords("en"))
cleaned_data <- tm_map(cleaned_data, stripWhitespace)
keywords <- unlist(lapply(cleaned_data, FUN = as.character))

# Tokenize and preprocess the test sentence
test_sentence <- tolower(test_sentence)
test_sentence <- removePunctuation(test_sentence)
test_sentence <- removeNumbers(test_sentence)
test_sentence <- removeWords(test_sentence, stopwords("en"))
test_sentence <- stripWhitespace(test_sentence)

# Create a Document-Term Matrix (DTM) for both data and test sentence
dtm <- DocumentTermMatrix(Corpus(VectorSource(c(keywords, test_sentence))))
# Perform Latent Semantic Analysis (LSA)
lsa_space <- lsa(dtm)

# Calculate semantic similarity
similarity_scores <- cosine(lsa_space$tk[, 1], lsa_space$tk[, ncol(lsa_space$tk)])
print(similarity_scores)

# Combine the original data with similarity scores
data_with_similarity <- data.frame(Keywords = plot_keywords, Similarity = similarity_scores)

# Rank rows by similarity
data_with_similarity <- data_with_similarity[order(data_with_similarity$Similarity, decreasing = TRUE), ]

# Print the ranked data
print(data_with_similarity)
