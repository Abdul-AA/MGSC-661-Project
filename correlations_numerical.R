
setwd('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/')

imdb <- read.csv('/Users/avimalhotra/Desktop/McGill MMA/Fall 23/MGSC661 Multivar Stats/midterm-project/IMDB_data_Fall_2023.csv')
attach(imdb)

# keep only numerical variables
df_numerical <- imdb[, c("imdb_score","movie_budget", "duration", "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")]

opt_poly_matrix <- list()

# movie budget vs imdb score
mb_reg1 <- lm(imdb_score~movie_budget)
mb_reg2 <- lm(imdb_score~poly(movie_budget,2))
mb_reg3 <- lm(imdb_score~poly(movie_budget,3))
mb_reg4 <- lm(imdb_score~poly(movie_budget,4))
mb_reg5 <- lm(imdb_score~poly(movie_budget,5))

anova(mb_reg1, mb_reg2, mb_reg3, mb_reg4, mb_reg5)
opt_poly_matrix[["movie_budget"]] <- mb_reg1


# duration vs imdb score
d_reg1 <- lm(imdb_score~duration)
d_reg2 <- lm(imdb_score~poly(duration,2))
d_reg3 <- lm(imdb_score~poly(duration,3))
d_reg4 <- lm(imdb_score~poly(duration,4))
d_reg5 <- lm(imdb_score~poly(duration,5))

anova(d_reg1, d_reg2, d_reg3, d_reg4, d_reg5)
anova(d_reg1, d_reg2, d_reg4, d_reg5)
opt_poly_matrix[["duration"]] <- d_reg5

# aspect ratio vs imdb score
ar_reg1 <- lm(imdb_score~aspect_ratio)
ar_reg2 <- lm(imdb_score~poly(aspect_ratio,2))
ar_reg3 <- lm(imdb_score~poly(aspect_ratio,3))
ar_reg4 <- lm(imdb_score~poly(aspect_ratio,4))
ar_reg5 <- lm(imdb_score~poly(aspect_ratio,5))

anova(ar_reg1, ar_reg2, ar_reg3, ar_reg4, ar_reg5)
opt_poly_matrix[["aspect_ratio"]] <- ar_reg2

# nb_news_articles vs imdb score
nna_reg1 <- lm(imdb_score~nb_news_articles)
nna_reg2 <- lm(imdb_score~poly(nb_news_articles,2))
nna_reg3 <- lm(imdb_score~poly(nb_news_articles,3))
nna_reg4 <- lm(imdb_score~poly(nb_news_articles,4))
nna_reg5 <- lm(imdb_score~poly(nb_news_articles,5))

anova(nna_reg1, nna_reg2, nna_reg3, nna_reg4, nna_reg5)
opt_poly_matrix[["nb_news_articles"]] <- nna_reg3

# actor1_star_meter vs imdb score
a1sm_reg1 <- lm(imdb_score~actor1_star_meter)
a1sm_reg2 <- lm(imdb_score~poly(actor1_star_meter,2))
a1sm_reg3 <- lm(imdb_score~poly(actor1_star_meter,3))
a1sm_reg4 <- lm(imdb_score~poly(actor1_star_meter,4))
a1sm_reg5 <- lm(imdb_score~poly(actor1_star_meter,5))

anova(a1sm_reg1, a1sm_reg2, a1sm_reg3, a1sm_reg4, a1sm_reg5)
opt_poly_matrix[["actor1_star_meter"]] <- a1sm_reg1

# actor2_star_meter vs imdb score
a2sm_reg1 <- lm(imdb_score~actor2_star_meter)
a2sm_reg2 <- lm(imdb_score~poly(actor2_star_meter,2))
a2sm_reg3 <- lm(imdb_score~poly(actor2_star_meter,3))
a2sm_reg4 <- lm(imdb_score~poly(actor2_star_meter,4))
a2sm_reg5 <- lm(imdb_score~poly(actor2_star_meter,5))

anova(a2sm_reg1, a2sm_reg2, a2sm_reg3, a2sm_reg4, a2sm_reg5)
anova(a2sm_reg1, a2sm_reg5)

opt_poly_matrix[["actor2_star_meter"]] <- a2sm_reg1

# actor3_star_meter vs imdb score
a3sm_reg1 <- lm(imdb_score~actor3_star_meter)
a3sm_reg2 <- lm(imdb_score~poly(actor3_star_meter,2))
a3sm_reg3 <- lm(imdb_score~poly(actor3_star_meter,3))
a3sm_reg4 <- lm(imdb_score~poly(actor3_star_meter,4))
a3sm_reg5 <- lm(imdb_score~poly(actor3_star_meter,5))

anova(a3sm_reg1, a3sm_reg2, a3sm_reg3, a3sm_reg4, a3sm_reg5)
anova(a3sm_reg1, a3sm_reg2, a3sm_reg4)

opt_poly_matrix[["actor3_star_meter"]] <- a3sm_reg4

# nb_faces vs imdb score
nf_reg1 <- lm(imdb_score~nb_faces)
nf_reg2 <- lm(imdb_score~poly(nb_faces,2))
nf_reg3 <- lm(imdb_score~poly(nb_faces,3))
nf_reg4 <- lm(imdb_score~poly(nb_faces,4))
nf_reg5 <- lm(imdb_score~poly(nb_faces,5))

anova(nf_reg1, nf_reg2, nf_reg3, nf_reg4, nf_reg5)
opt_poly_matrix[["nb_faces"]] <- nf_reg1

# movie_meter_IMDBpro vs imdb score
mm_reg1 <- lm(imdb_score~movie_meter_IMDBpro)
mm_reg2 <- lm(imdb_score~poly(movie_meter_IMDBpro,2))
mm_reg3 <- lm(imdb_score~poly(movie_meter_IMDBpro,3))
mm_reg4 <- lm(imdb_score~poly(movie_meter_IMDBpro,4))
mm_reg5 <- lm(imdb_score~poly(movie_meter_IMDBpro,5))

anova(mm_reg1, mm_reg2, mm_reg3, mm_reg4, mm_reg5)

opt_poly_matrix[["movie_meter_IMDBpro"]] <- mm_reg5

opt_model <- lm(imdb_score
                ~movie_budget
                +poly(duration,5)
                +poly(aspect_ratio,2)
                +poly(nb_news_articles,3)
                +actor1_star_meter
                +actor2_star_meter
                +poly(actor3_star_meter,4)
                +nb_faces
                +poly(movie_meter_IMDBpro,5)
              , data = df_numerical)

residualPlot(opt_model, quadratic=FALSE)

summary(opt_model)


plot(opt_model)

qqPlot(opt_model, envelope=list(style="none"))

outlierTest(opt_model)

# show outlier rows
df_numerical[c(191, 1581, 395, 1806, 316, 1592),]

# remove outliers
df_final <- df_numerical[-c(191, 1581, 395, 1806, 316, 1592),]

opt_model <- lm(imdb_score
                ~movie_budget
                +poly(duration,5)
                +poly(aspect_ratio,2)
                +poly(nb_news_articles,3)
                +actor1_star_meter
                +actor2_star_meter
                +poly(actor3_star_meter,4) #maybe remove?
                +nb_faces
                +poly(movie_meter_IMDBpro,5)
                , data = df_final)

model2 <- lm(imdb_score~ movie_budget + poly(duration, 5) + poly(nb_news_articles, 3) + nb_faces + poly(movie_meter_IMDBpro, 5), data = df_final)

summary(model2)

residualPlot(model2, quadratic=FALSE)

# check for colinearity: none found
vif(model2)

quantvars <- c("movie_budget", "duration", "aspect_ratio", "nb_news_articles", "actor1_star_meter", "actor2_star_meter", "actor3_star_meter", "nb_faces", "movie_meter_IMDBpro")
cor(df_final[,quantvars])

summary(model2)

# checking and correcting heteroskedasticity:
ncvTest(model2)

install.packages("lmtest")
install.packages("plm")
require(lmtest)
require(plm)

coeftest(opt_model, vcov=vcovHC(opt_model, type="HC1"))

# extract movie_id and plot_keywords
df_id_keywords <- imdb[, c("movie_id", "plot_keywords")]

install.packages("dplyr")
library(dplyr)

df_id_keywords[1,2]

s <- textSimilarity(x="relationship", y=df_id_keywords[1,2], method="cosine")

summary(model2)

test = read.csv('test_data_IMDB_Fall_2023.csv')

 # make a prediction on the test data

pred <- predict(model2, newdata = test[,c('movie_budget','duration','nb_news_articles','nb_faces','movie_meter_IMDBpro')])

pred

library(boot)
library(car)

glm_model2 <- glm(imdb_score~ movie_budget +
  poly(duration, 5) +
  poly(nb_news_articles, 3) +
  nb_faces +
  poly(movie_meter_IMDBpro, 5),
             data = df_final)

res = cv.glm(df_final, glm_model2, K=5)
res$delta

