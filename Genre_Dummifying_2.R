IMDB=read.csv("/Users/aoluwolerotimi/Datasets/IMDB_data_Fall_2023.csv")
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

# Complete dummifying 
# Check if each genre appears in the pipe-delimited "genres" column
df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0) 
df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)
df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)
df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)
df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)
df$family <- ifelse(grepl("Family", df$genres), 1, 0)
attach(df)


# Individual significance tests 
# Loop through the genres, fit linear regression models, and report on indivudal variable significance
for (genre in all_genres) {
  formula <- as.formula(paste("imdb_score ~", genre))
  lm_genre <- lm(formula)
  cat("Genre:", genre, "\n")
  print(summary(lm_genre))
}

# Retaining only those with p-value less than 0.001
sig_genres = c("documentary", "biography",	"comedy",	"family",	"action",	"scifi",	"thriller",	"horror",	"drama",	"war")
nonsig_genres = c("fantasy",	"mystery",	"adventure",	"musical",	"romance", "western",	"sport",	"animation",	"crime")

# Checking correlation for significant genres
reg = lm(imdb_score~documentary+biography+comedy+family+action+scifi+thriller+horror+drama+war)
vif(reg)
# no VIF greater than 4

# Removing non_significant genres
df <- df %>% select(-one_of(nonsig_genres))
attach(df)



