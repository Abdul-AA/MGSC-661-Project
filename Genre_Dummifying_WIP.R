# TASK: CREATE DUMMY VARIABLES FOR THE GENRES IN TEST SET
# POPULATE THOSE COLUMNS FOR THE EXISTING DATA SET
# NEED TO DO THE SAME "IF IN" METHODOLOGY WHICH WE'LL HACVE TO APPLY FOR KEYWORDS


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


# then do individual significance tests 
# see if i need to do "as factor" when it's already a dummified variable 


# see if i can do "to lower" function to make it more fool-proof



## DOUBLE CHECKING THE COUNT OF CATEGORICAL VARIABLES
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




