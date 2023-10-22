# coding file: main.R
imdb <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/IMDB_data_Fall_2023.csv')
attach(imdb)

install.packages("tidyverse")
library(tidyverse)

library(ggplot2)

ggplot(imdb, aes(x=imdb$imdb_score)) + geom_histogram(binwidth=0.5, color="black", fill="white") + labs(title="IMDB Rating Distribution", x="IMDB Rating", y="Count")

# keep only numerical variables
df_numerical <- imdb[, c("imdb_score","movie_budget", "duration", "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")]

# normalize the data
df_numerical_std <- scale(df_numerical)
df_numerical_std <- as.data.frame(df_numerical_std)

# make a boxplot of the data
boxplot(df_numerical_std, main="Boxplot of Numerical Variables", xlab="Variables", ylab="Standardized Values")


# make a scatterplot of each of the numerical variables with the IMDB score
plt_list <- list()
maturity_rating <- as.factor(imdb$maturity_rating)
for (i in 2:10) {
  # set y-max = 10
  plt <- ggscatter(df_numerical, x = colnames(df_numerical)[i], y = colnames(df_numerical)[1],
            xlab = names(df_numerical)[i], ylab = "IMDB Score", ylim = c(0,10))
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red")


  plt_list[[i-1]] <- plt
}

# create a 3x3 scatterplot matrix of the numerical variables vs the IMDB score
ggarrange(plotlist = plt_list, ncol = 3, nrow = 3)

