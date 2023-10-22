IMDB=read.csv("C:/Users/barab/OneDrive/Documents/McGill MMA/Courses/MGSC 661/IMDB_data_Fall_2023.csv")
attach(IMDB) 

categorical_columns = c("release_month", "language", "country", "maturity_rating", "aspect_ratio", "distributor", "director",
                        "actor1", "actor2", "actor3", "colour_film")

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
#release_month, language, maturity_rating, actor3, 

#Variables that are statistically significant include:
#country, aspect_ratio, distributor, director, actor1, actor2, colour_film

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
