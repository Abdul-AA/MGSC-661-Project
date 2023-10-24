IMDB=read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/IMDB_data_Fall_2023.csv')
View(IMDB)
attach(IMDB) 

IMDB_test = read.csv("/Users/aoluwolerotimi/Datasets/test_data_IMDB_Fall_2023.csv")

library(dplyr)
library(tidyr)
library(car)

# Split the pipe-delimited genres into separate rows
IMDB_test2 <- IMDB_test %>%
  separate_rows(genres, sep = "\\|")

# Get a list of unique genres
testset_genres <- unique(IMDB_test2$genres)
testset_genres
# Capitalized for union function to work as expected
existing_genres = c("Action",	"Adventure", "Scifi",	"Thriller",	"Musical", "Romance",	"Western", "Sport",	"Horror",	"Drama",	"War",	"Animation",	"Crime")

all_genres <- union(testset_genres, existing_genres)
all_genres
# Putting all in lower case for further steps
all_genres = c("action",	"adventure", "scifi",	"thriller",	"musical", "romance",	"western", "sport",	"horror",	"drama",	"war",
               "animation",	"crime", "documentary", "biography", "fantasy", "comedy", "mystery", "family")

# Creating modified dataset for modelling separate from original data
df = read.csv("/Users/aoluwolerotimi/Datasets/IMDB_data_Fall_2023.csv")


# Find genres that are not already represented
# Lowercase existing_genres for setdiff to work as expected 
existing_genres = c("action",	"adventure", "scifi",	"thriller",	"musical", "romance",	"western", "sport",	"horror",	"drama",	"war",	"animation",	"crime")
genres_to_add = setdiff(all_genres, existing_genres)
genres_to_add

# Create new columns for genres that are not already represented
for (genre in genres_to_add) {
  # Initialize the new column with 0s (or NA if you prefer)
  df[genre] = 0
}

View(df)
attach(df)

# sum counts to make sure the new transformation actually does something 
# Assuming you have created the binary genre columns in your data frame 'df'
# List of genre columns


# Calculate the sum of 1s in each genre column
genre_counts <- colSums(df[all_genres])

# Create a named vector with genre names as names
genre_counts_named <- setNames(genre_counts, all_genres)

# Print the counts with genre names
print(genre_counts_named)



# Complete dummifying 
# Check if each genre appears in the pipe-delimited "genres" column
df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0) 
df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)
df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)
df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)
df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)
df$family <- ifelse(grepl("Family", df$genres), 1, 0)
attach(df)

# sum counts to make sure the new transformation actually does something 

# Calculate the sum of 1s in each genre column
genre_counts <- colSums(df[all_genres])

# Create a named vector with genre names as names
genre_counts_named <- setNames(genre_counts, all_genres)

# Print the counts with genre names
print(genre_counts_named)

## again but also checking plot_keywords
df$action <- ifelse(grepl("Action", df$genres), 1, 0)+ifelse(grepl("action", df$plot_keywords), 1, 0)
df$action  = ifelse(df$action >= 1,1,0)

df$adventure <- ifelse(grepl("Adventure", df$genres), 1, 0)+ifelse(grepl("adventure", df$plot_keywords), 1, 0)
df$adventure  = ifelse(df$adventure >= 1,1,0)

df$scifi <- ifelse(grepl("Sci-Fi", df$genres), 1, 0)+ifelse(grepl("scifi", df$plot_keywords), 1, 0)
df$scifi = ifelse(df$scifi >= 1,1,0)

df$thriller <- ifelse(grepl("Thriller", df$genres), 1, 0)+ifelse(grepl("thriller", df$plot_keywords), 1, 0)
df$thriller  = ifelse(df$thriller >= 1,1,0)

df$musical <- ifelse(grepl("Music", df$genres), 1, 0)+ifelse(grepl("musical", df$plot_keywords), 1, 0)
df$musical  = ifelse(df$musical >= 1,1,0)

df$romance <- ifelse(grepl("Romance", df$genres), 1, 0)+ifelse(grepl("romance", df$plot_keywords), 1, 0)
df$romance  = ifelse(df$romance >= 1,1,0)

df$western <- ifelse(grepl("Western", df$genres), 1, 0)+ifelse(grepl("western", df$plot_keywords), 1, 0)
df$western  = ifelse(df$western >= 1,1,0)

df$sport <- ifelse(grepl("Sport", df$genres), 1, 0)+ifelse(grepl("sport", df$plot_keywords), 1, 0)
df$sport  = ifelse(df$sport >= 1,1,0)

df$horror <- ifelse(grepl("Horror", df$genres), 1, 0)+ifelse(grepl("horror", df$plot_keywords), 1, 0)
df$horror  = ifelse(df$horror >= 1,1,0)

df$drama <- ifelse(grepl("Drama", df$genres), 1, 0)+ifelse(grepl("drama", df$plot_keywords), 1, 0)
df$drama  = ifelse(df$drama >= 1,1,0)

df$war <- ifelse(grepl("War", df$genres), 1, 0)+ifelse(grepl("war", df$plot_keywords), 1, 0)
df$war  = ifelse(df$war >= 1,1,0)

df$animation <- ifelse(grepl("Animation", df$genres), 1, 0)+ifelse(grepl("animation", df$plot_keywords), 1, 0)
df$animation  = ifelse(df$animation >= 1,1,0)

df$crime <- ifelse(grepl("Crime", df$genres), 1, 0)+ifelse(grepl("crime", df$plot_keywords), 1, 0)
df$crime  = ifelse(df$crime >= 1,1,0)

df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0)+ifelse(grepl("documentary", df$plot_keywords), 1, 0)
df$documentary  = ifelse(df$documentary >= 1,1,0)

df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)+ifelse(grepl("biography", df$plot_keywords), 1, 0)
df$biography  = ifelse(df$biography >= 1,1,0)

df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)+ifelse(grepl("fantasy", df$plot_keywords), 1, 0)
df$fantasy  = ifelse(df$fantasy >= 1,1,0)

df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)+ifelse(grepl("comedy", df$plot_keywords), 1, 0)
df$comedy  = ifelse(df$comedy >= 1,1,0)

df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)+ifelse(grepl("mystery", df$plot_keywords), 1, 0)
df$mystery  = ifelse(df$mystery >= 1,1,0)

df$family <- ifelse(grepl("Family", df$genres), 1, 0)+ifelse(grepl("family", df$plot_keywords), 1, 0)
df$family  = ifelse(df$family >= 1,1,0)


#validate with this

attach(df)
genre_counts <- colSums(df[all_genres])
genre_counts_named <- setNames(genre_counts, all_genres)
print(genre_counts_named)


# Individual significance tests 
# Loop through the genres, fit linear regression models, and report on individudal variable significance
for (genre in all_genres) {
  formula <- as.formula(paste("imdb_score ~", genre))
  lm_genre <- lm(formula)
  cat("Genre:", genre, "\n")
  print(summary(lm_genre))
}

write.csv(df, "genres_dummied_z.csv", row.names = FALSE)

## list of the significant ones, at 0.05, single * 
# action
# adventure
# scifi
# thriller
# western
# sport
# horror
# drama
# war
# crime
# documentary
# biography
# fantasy
# comedy
# family

useful_genres <- c("action", "adventure", "scifi", "thriller", "western", "sport", "horror", "drama", "war",
                   "crime", "documentary", "biography", "fantasy", "comedy", "family")

genres_dummies <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/MGSC-661-Project/genres_dummied_z.csv')
processed_dataset <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/MGSC-661-Project/processed_imdb_dataset.csv')

# add genres_dummies[useful_genres] to processed_dataset
processed_dataset <- processed_dataset %>% left_join(genres_dummies %>% select(movie_id, all_of(useful_genres)), by = "movie_id")

write.csv(processed_dataset, file = "/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/MGSC-661-Project/processed_imdb_dataset.csv")



