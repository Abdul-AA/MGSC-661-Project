IMDB=read.csv("/Users/aoluwolerotimi/Datasets/IMDB_data_Fall_2023.csv")
attach(IMDB) 
names(IMDB)
View(IMDB)

# check for missing data in training set
missing_data <- is.na(IMDB)
missing_count <- colSums(missing_data)
missing_count

# check for missing data in test set
IMDB_test = read.csv("/Users/aoluwolerotimi/Datasets/test_data_IMDB_Fall_2023.csv")
m_data = is.na(IMDB_test)
m_count = colSums(m_data)
m_count
# no missing budget in this test set either
View(IMDB_test)

#checking if i missed any categorical in my search. i didn't. R actually captured fewer than there are 
categorical_columns <- names(IMDB)[sapply(IMDB, is.factor)]
categorical_columns <- c(categorical_columns, names(IMDB)[sapply(IMDB, is.character)])
categorical_columns

# check for distribution of categorical variables
# come back to "genres" and "plot_keywords", we need to make a call about that 
categorical_columns = c("release_month", "language", "country", "maturity_rating", "aspect_ratio", "distributor", "director",
                        "actor1", "actor2", "actor3", "colour_film")

# printing out the names of people (actors, directors) is probs not worth our time, esp when i remember Juan's hint. 
categorical_columns = c("release_month", "language", "country", "maturity_rating", "aspect_ratio", "distributor", "colour_film")

# okay even for distributors it's so many with 1 frequency count 
categorical_columns = c("release_month", "language", "country", "maturity_rating", "aspect_ratio", "colour_film")


for (col in categorical_columns) {
  cat(paste("Frequency table for column:", col, "\n"))
  print(table(IMDB[[col]]))
}
# okay, main thing coming out of this is that there are a handful of movies (21) with "Approved" as their rating which no longer exists as a rating
# there isn't a direct modern day mapping. so need to make a decision about that. it's all pre-1964 movies. 
# maybe we leave it in and see if the dummy is even significant? i'd guess not, so we'd probs be able to drop



# the genre dummies, i need to find a way to plot them in the same place
genre_columns = c("action",	"adventure", "scifi",	"thriller",	"musical", "romance",	"western", "sport",	"horror",	"drama",	"war",	"animation",	"crime")
# do we still need to do as.factor to something that is already a dummy? 

# Loop through the specified genre_columns and print the sum
for (col in genre_columns) {
  column_sum <- sum(IMDB[[col]])
  cat(paste("Sum of values in", col, ":", column_sum, "\n"))
}
