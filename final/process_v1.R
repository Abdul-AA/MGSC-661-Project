

# libraries required
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)
library(ggpubr)
library(e1071)
library(reshape2)
library(psych)
library(caret)
library(stargazer)
library(cvTools)
library(dplyr)
library(boot)
library(vcd)
library(tm)
library(SnowballC)
require(lmtest)
require(plm)


setwd('C:/Users/zzhong13/Desktop/Multivariate')



log_test = read.csv('./logged test data.csv')
log_train = read.csv('./newest train.csv')

imdb = read.csv('./IMDB_data_Fall_2023.csv')
test = read.csv('./test_data_IMDB_Fall_2023.csv')


add_genres = function(df) {
  df$documentary <- ifelse(grepl("Documentary", df$genres), 1, 0)
  df$biography <- ifelse(grepl("Biography", df$genres), 1, 0)
  df$fantasy <- ifelse(grepl("Fantasy", df$genres), 1, 0)
  df$comedy <- ifelse(grepl("Comedy", df$genres), 1, 0)
  df$mystery <- ifelse(grepl("Mystery", df$genres), 1, 0)
  df$family <- ifelse(grepl("Family", df$genres), 1, 0)

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

  return(df)
}

add_maturities = function(df) {
  maturity_categories <- unique(df$maturity_rating)

  # Loop through each category and perform linear regression
  for (category in maturity_categories) {
    # Create a dummified variable for the current category
    df[category] <- as.numeric(df$maturity_rating == category)

  }
  
  if ("TV-G" %in% colnames(df)){
  df['TV.G'] = df["TV-G"]}
  if ('TV-14' %in% colnames(df)){
  df['TV.14'] = df["TV-14"]}
  if ('PG-13' %in% colnames(df)){
  df['PG.13'] = df['PG-13']}
  if ('NC-17' %in% colnames(df)){
  df['NC.17'] = df['NC-17']}
  return(df)
}


imdb <- add_genres(imdb)
imdb <- add_maturities(imdb)


add_maturity_test = function(test){
maturity =  unique(test$maturity_rating)

for (r in maturity){
  test[r] =  as.numeric(test$maturity_rating == r)
}

test['TV.G'] = 0
test['TV.14'] = 0
test['PG.13'] = test['PG-13']

return(test)
}



test <- add_genres(test)
test <- add_maturity_test(test)


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


add_logs = function(df) {
  df$log_nb_news_articles = log(df$nb_news_articles)
  df$log_movie_meter_IMDBpro = log(df$movie_meter_IMDBpro)
  df$log_movie_budget = log(df$movie_budget)
  df$log_duration = log(df$duration)
  df$log_actor1_star_meter = log(df$actor1_star_meter)
  
  df$log_nb_news_articles <- ifelse(is.infinite(df$log_nb_news_articles) & df$log_nb_news_articles < 0, 0, df$log_nb_news_articles)
  df$log_movie_meter_IMDBpro <- ifelse(is.infinite(df$log_movie_meter_IMDBpro) & df$log_movie_meter_IMDBpro < 0, 0, df$log_movie_meter_IMDBpro)
  df$log_movie_budget <- ifelse(is.infinite(df$log_movie_budget) & df$log_movie_budget < 0, 0, df$log_movie_budget)
  df$log_duration <- ifelse(is.infinite(df$log_duration) & df$log_duration < 0, 0, df$log_duration)
  
  df$log_actor1_star_meter <- ifelse(is.infinite(df$log_actor1_star_meter) & df$log_actor1_star_meter < 0, 0, df$log_actor1_star_meter)
  
  

  return(df)
}



imdb<- add_logs(imdb)
test<- add_logs(test)


add_lengths = function(test) {
  test$actor1_length = lapply(test$actor1,nchar)
  test$genres_length = lapply(test$genres,nchar)
  test$distributor_length = lapply(test$distributor,nchar)
  test$movie_title_length = lapply(test$movie_title,nchar)

  test$actor1_length = as.numeric(test$actor1_length)
  test$genres_length = as.numeric(test$genres_length)
  test$distributor_length = as.numeric(test$distributor_length)
  test$movie_title_length = as.numeric(test$movie_title_length)

  return(test)
}

imdb <- add_lengths(imdb)
test <- add_lengths(test)

add_others = function(test) {
  test$is_Miramax = ifelse(grepl('Miramax', test$production_company), 1,0)
  test$is_Nov = ifelse(grepl('Nov', test$release_month), 1,0)
  test$is_Dec = ifelse(grepl('Dec', test$release_month), 1,0)
  test$is_color = ifelse(test$colour_film=="Color",1,0)


  return(test)
}

imdb <- add_others(imdb)
test <- add_others(test)


diff <- setdiff(names(log_train), names(imdb))
print("Columns in log_train not in imdb:")
print(diff) # these variables are stored with dummy_ prefix so dont worry


lm_final = lm(imdb_score~log_movie_budget +
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                is_color+
                poly(genres_length,2)+
                biography+
                documentary+
                is_Miramax+
                horror+
                drama+
                documentary+
                biography+
                animation+
                R+
                TV.14+
                is_color
              ,data = imdb)





pred = predict(lm_final, newdata = test)

cbind(test$movie_title,as.vector(pred)) 
summary(lm_final )


lm_final = lm(imdb_score~log_movie_budget +
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                is_color+
                poly(genres_length,2)+
                biography+
                documentary+
                is_Miramax+
                horror+
                drama+
                documentary+
                biography+
                animation+
                R+
                TV.14+
                is_color
              ,data = imdb)

outlierTest(lm_final)

imdb_without_outliers = imdb[-c(1806,1581,191,395,1436,1255,989),]


glm_final = glm(imdb_score~log_movie_budget +
                  log_duration+
                  poly(log_nb_news_articles, 1, raw = TRUE)+
                  poly(log_movie_meter_IMDBpro, 4)+
                  is_color+
                  poly(genres_length,2)+
                  biography+
                  animation+
                  documentary+
                  is_Miramax+
                  horror+
                  drama+
                  documentary+
                  biography+
                  animation+
                  R+
                  TV.14+
                  is_color
                , data = imdb_without_outliers)

pred = predict(glm_final, newdata = test)

cbind(test$movie_title,as.vector(pred)) 
summary(lm_final )
