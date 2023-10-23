# TASK: CREATE DUMMY VARIABLES FOR THE GENRES IN TEST SET
# POPULATE THOSE COLUMNS FOR THE EXISTING DATA SET
# NEED TO DO THE SAME "IF IN" METHODOLOGY WHICH WE'LL HAVE TO APPLY FOR KEYWORDS


IMDB=read.csv("/Users/aoluwolerotimi/Datasets/IMDB_data_Fall_2023.csv")
attach(IMDB) 

IMDB_test = read.csv("/Users/aoluwolerotimi/Datasets/test_data_IMDB_Fall_2023.csv")

# tokenize all the values in genres column in test set 
#install.packages("tidyr")

library(dplyr)
library(tidyr)

# Split the pipe-delimited genres into separate rows
IMDB_test2 <- IMDB_test %>%
  separate_rows(genres, sep = "\\|")

# Get a list of unique genres
testset_genres <- unique(IMDB_test2$genres)
#View(IMDB_test2)

# Print the unique genres
print(testset_genres)

existing_genres = c("Action",	"Adventure", "Scifi",	"Thriller",	"Musical", "Romance",	"Western", "Sport",	"Horror",	"Drama",	"War",	"Animation",	"Crime")

all_genres <- union(testset_genres, existing_genres)
all_genres


df = read.csv("/Users/aoluwolerotimi/Datasets/IMDB_data_Fall_2023.csv")


# Find genres that are not already represented
genres_to_add = setdiff(all_genres, existing_genres)
genres_to_add

# Create new columns for genres that are not already represented
for (genre in genres_to_add) {
  # Initialize the new column with 0s (or NA if you prefer)
  df[genre] = 0
}

View(df)

# complete dummifying 

# Check if each genre is in the "genres" column
df$Documentary <- ifelse(grepl("Documentary", df$genres), 1, 0)
df$Biography <- ifelse(grepl("Biography", df$genres), 1, 0)
df$Fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)
df$Comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)
df$Mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)
df$Family <- ifelse(grepl("Family", df$genres), 1, 0)

attach(df)
# then do individual significance tests 
lmDocumentary = lm(imdb_score~Documentary)
summary(lmDocumentary)


# Loop through the genres and fit linear regression models
for (genre in genres_to_add) {
  # Create the formula dynamically
  formula <- as.formula(paste("imdb_score ~", genre))
  
  # Fit the linear regression model
  lm_genre <- lm(formula)
  
  # Print the name of the genre
  cat("Genre:", genre, "\n")
  
  # Print the summary of the linear regression model
  print(summary(lm_genre))
}

# won't be able to use this for all because the original dummies don't have capitalization
# going to try tolower only on the column title 

# Convert the column names to lowercase
colnames(df[, genres_to_add]) <- tolower(colnames(df[, genres_to_add]))
attach(df)
View(df)
# didn't work 

#df <- df %>% rename(new_name = old_name)
df <- df %>% rename(documentary = Documentary)
df <- df %>% rename(biography = Biography)
df <- df %>% rename(fantasy = Fantasy)
df <- df %>% rename(comedy = Comedy)
df <- df %>% rename(mystery = Mystery)
df <- df %>% rename(family = Family)
attach(df)

# okay this is working. finish off then loop through all genres


all_genres = c("action",	"adventure", "scifi",	"thriller",	"musical", "romance",	"western", "sport",	"horror",	"drama",	"war",
               "animation",	"crime", "documentary", "biography", "fantasy", "comedy", "mystery", "family")

all_genres

# Loop through the genres and fit linear regression models
for (genre in all_genres) {
  # Create the formula dynamically
  formula <- as.formula(paste("imdb_score ~", genre))
  
  # Fit the linear regression model
  lm_genre <- lm(formula)
  
  # Print the name of the genre
  cat("Genre:", genre, "\n")
  
  # Print the summary of the linear regression model
  print(summary(lm_genre))
}




# see if i need to do "as factor" when it's already a dummified variable --> seems like no 


## SCRAP CODE FROM WHEN WE WERE DOUBLE CHECKING THE COUNT OF CATEGORICAL VARIABLES
unique_cinematographers <- unique(IMDB$cinematographer)
num_unique_cinematographers <- length(unique_cinematographers)
num_unique_cinematographers

unique_list <- unique(IMDB$production_company)
unique_count <- length(unique_list)
unique_count

unique_list <- unique(IMDB$production_company)
unique_count <- length(unique_list)
unique_count

unique_list <- unique(IMDB$distributor)
unique_count <- length(unique_list)
unique_count

unique_list <- unique(IMDB$director)
unique_count <- length(unique_list)
unique_count




