
library(dplyr)
library(boot)
library(car)
library(ggplot2)
library(stargazer)
library(psych)

require(lmtest)
require(plm)

# setwd('C:/Users/zzhong13/Desktop/Multivariate')
df = read.csv('midterm-project/MGSC-661-Project/Cleaned Training Set.csv')


# df$log_nb_news_articles <- ifelse(is.infinite(df$log_nb_news_articles) & df$log_nb_news_articles < 0, 0, df$log_nb_news_articles)
# df$log_movie_meter_IMDBpro <- ifelse(is.infinite(df$log_movie_meter_IMDBpro) & df$log_movie_meter_IMDBpro < 0, 0, df$log_movie_meter_IMDBpro)
# df$log_movie_budget <- ifelse(is.infinite(df$log_movie_budget) & df$log_movie_budget < 0, 0, df$log_movie_budget)
# df$log_duration <- ifelse(is.infinite(df$log_duration) & df$log_duration < 0, 0, df$log_duration)



df = read.csv('./logged Train Set.csv ')

df$log_release_year = log(df$release_year)



quick_fit = function(data){
  model_formula <- as.formula(paste("imdb_score ~ .", collapse = " + "))
  lm_model <- lm(formula = model_formula, data = data)
  
  return(summary(lm_model))
  
}


colnames(df)


fit = lm(imdb_score~log_nb_news_articles+log_movie_meter_IMDBpro+log_movie_budget+log_duration, data = df)
summary(fit)

vif(fit)

outlierTest(fit)

fit_1 = glm(imdb_score~log_nb_news_articles+log_movie_meter_IMDBpro+log_movie_budget+log_duration, data = df[-c(191,1581,395,1806,481,1255),])


summary(fit_1)



ncvTest(fit_1)

correct = coeftest(fit_1, vcov=vcovHC(fit_1, type="HC1"))



get_cv_mse(fit_1,df[-c(191,1581,395,1806,481,1255),],10)


pred = predict(fit_1, newdata = test)


cbind(test$movie_title,as.vector(pred))



















model_formula <- as.formula(paste("imdb_score ~ .", collapse = " + "))
glm_model <- glm(formula = model_formula, data = df)

summary(glm_model)


significant_variables <- rownames(summary(glm_model)$coefficients[summary(glm_model)$coefficients[, "Pr(>|t|)"] < 0.1, ])
significant_variables = as.vector(significant_variables)
significant_variables = significant_variables[-1]


quick_fit( cbind(df['imdb_score'],df[,significant_variables]))



model_formula <- as.formula(paste("imdb_score ~ .", collapse = " + "))


glm_model <- glm(imdb_score~poly(movie_budget, 1,raw = TRUE)+ 
                   poly(duration, 1, raw = TRUE)+
                   poly(nb_news_articles, 1,raw = TRUE)+
                   poly(movie_meter_IMDBpro, 1,raw = TRUE)+
                   nb_faces+
                   release_year+
                   movie_id+
                  # movie_title_length+
                   aspect_ratio+
                   distributor_length+
                   is_Miramax+
                   is_Nov+
                   is_Dec+
                   comput+
                   holiday+
                   riot+
                  # terror+
                  # wish+
                   #love+
                   action+
                   #sport+
                   horror+
                   drama+
                  # crime+
                   documentary+
                   biography+
                   comedy+
                   animation+
                   R+
                   TV.G+
                   TV.14+
                   is_color
                   ,data = cbind(df['imdb_score'],df[,significant_variables]))

summary(glm_model)



df$log_nb_news_articles = log(df$nb_news_articles)

df$log_movie_meter_IMDBpro = log(df$movie_meter_IMDBpro)
df$log_movie_budget = log(df$movie_budget)
df$log_duration = log(df$duration)




lm_model <- lm(imdb_score~poly(movie_budget, 1,raw = TRUE)+ 
                   poly(duration, 1, raw = TRUE)+
                   poly(nb_news_articles, 1,raw = TRUE)+
                   poly(movie_meter_IMDBpro, 1,raw = TRUE)+
                   nb_faces+
                   release_year+
                   movie_id+
                   # movie_title_length+
                   aspect_ratio+
                   distributor_length+
                   is_Miramax+
                   is_Nov+
                   is_Dec+
                   comput+
                   holiday+
                   riot+
                   # terror+
                   # wish+
                   #love+
                   action+
                   #sport+
                   horror+
                   drama+
                   # crime+
                   documentary+
                   biography+
                   comedy+
                   animation+
                   R+
                   TV.G+
                   TV.14+
                   is_color
                 ,data = cbind(df['imdb_score'],df[,significant_variables]))




lm_model <- lm(imdb_score~poly(movie_budget, 1,raw = TRUE)+ 
                 poly(duration, 2, raw = TRUE)+
                 log_nb_news_articles+
                 log_movie_meter_IMDBpro+
                 nb_faces+
                 release_year+
                 movie_id+
                 # movie_title_length+
                 aspect_ratio+
                 distributor_length+
                 is_Miramax+
                 is_Nov+
                 is_Dec+
                 comput+
                 holiday+
                 riot+
                 # terror+
                 # wish+
                 #love+
                 action+
                 #sport+
                 horror+
                 drama+
                 # crime+
                 documentary+
                 biography+
                 comedy+
                 animation+
                 R+
                 TV.G+
                 TV.14+
                 is_color
               ,data = df)


write.csv(df,file = "logged Train Set.csv",row.names = FALSE)


cols_with_minus_inf <- sapply(df, function(col) any(is.infinite(col) & col == -Inf))

# Print the result
if (any(cols_with_minus_inf)) {
  cat("The following columns contain -Inf values:\n")
  cat(names(df)[cols_with_minus_inf], "\n")
} else {
  cat("No -Inf values found in any columns of the data frame.\n")
}



cols_with_minus_inf <- sapply(df, function(col) any(!is.numeric(col)))

# Print the result
if (any(cols_with_minus_inf)) {
  cat("The following columns contain -Inf values:\n")
  cat(names(df)[cols_with_minus_inf], "\n")
} else {
  cat("No -Inf values found in any columns of the data frame.\n")
}




par(mfrow = c(1, 2))

# Create the first histogram
hist(df$actor3_star_meter,breaks = 100)

# Create the second histogram
hist(df$log_actor3_star_meter,breaks = 100)



View(df)

hist(df$nb_news_articles,breaks = 100)

hist(df$movie_meter_IMDBpro, breaks = 100)

hist(df$duration, breaks = 100)

hist(df$movie_budget, breaks = 100)

hist(df$actor1_star_meter, breaks = 100)

hist(log(df$actor1_star_meter), breaks = 100)


df$log_actor1_star_meter = log(df$actor1_star_meter)
df$log_actor1_star_meter <- ifelse(is.infinite(df$log_actor1_star_meter) & df$log_actor1_star_meter < 0, 0, df$log_actor1_star_meter)

df$log_actor3_star_meter = log(df$actor3_star_meter)
df$log_actor3_star_meter <- ifelse(is.infinite(df$log_actor3_star_meter) & df$log_actor3_star_meter < 0, 0, df$log_actor3_star_meter)

df$log_nb_faces = log(df$nb_faces)
df$log_nb_faces <- ifelse(is.infinite(df$log_nb_faces) & df$log_nb_faces < 0, 0, df$log_nb_faces)



test$log_actor1_star_meter = log(test$actor1_star_meter)
test$log_actor1_star_meter <- ifelse(is.infinite(test$log_actor1_star_meter) & test$log_actor1_star_meter < 0, 0, test$log_actor1_star_meter)

View(test)

hist(df$genres_length, breaks = 100)

hist(df$distributor_length, breaks = 100)

hist(df$actor3_star_meter, breaks = 10)

hist(df$nb_faces, breaks = 100)

hist(df$log_nb_news_articles, breaks = 100)

hist(df$log_movie_meter_IMDBpro, breaks = 100)

hist(log(df$duration), breaks = 100)

hist(log(df$movie_budget), breaks = 100)

hist(log(df$log_nb_faces),breaks = 100)





ncvTest(lm_model)


residualPlots(lm_model)


simple_linear_regression <- function(data, dependent_variable, predictor) {
  # Build the linear regression model
  model <- lm(paste(dependent_variable, "~", predictor), data = data)
  
  # Extract R-squared value
  r_squared <- summary(model)$r.squared
  
  # Extract p-value
  p_value <- summary(model)$coefficients[2, "Pr(>|t|)"]
  
  return(data.frame(Predictor = predictor, R_squared = r_squared, P_value = p_value))
}

numerical_features = c("movie_title_length","aspect_ratio" , "genres_length" ,"distributor_length","actor1_length", 
"movie_budget","duration" ,"nb_news_articles","movie_meter_IMDBpro" , "nb_faces" ,"actor3_star_meter" ,"actor1_star_meter", 
"log_nb_news_articles" ,"log_movie_meter_IMDBpro", "log_movie_budget", "log_duration" ,"log_actor1_star_meter","log_actor3_star_meter")

results <- data.frame(Predictor = character(0), R_squared = numeric(0), P_value = numeric(0))

# Loop through the predictors and run simple linear regressions
for (predictor in numerical_features) {
  regression_result <- simple_linear_regression(df, "imdb_score", predictor)
  results <- rbind(results, regression_result)
}

# Print the results
print(results)

write.csv(results, file = "check_indiviudal pvalue.csv", row.names = FALSE)




fit = lm(imdb_score~movie_title_length+
           genres_length+
           distributor_length+
           actor1_star_meter+
           actor1_length+
           movie_budget+
           duration+
           nb_news_articles+
           movie_meter_IMDBpro+
           nb_faces, data = df)

residualPlots(fit)




fit2 = lm(imdb_score~movie_title_length+
           genres_length+
           distributor_length+
            log_actor1_star_meter+
            actor1_length+
            log_movie_budget+
            log_duration+
            log_nb_news_articles+
            log_movie_meter_IMDBpro+
           nb_faces, data = df)

residualPlots(fit2)

summary(fit2)






CV_MSE = c()

for (i in 1:100){
 
  score = c(score,cv.glm(cbind(df['imdb_score'],df[,significant_variables]),glm_model, K =5 )$delta[1])
  
}



hist(CV_MSE)


glm_model2 <- glm(imdb_score~movie_budget+ 
                    duration+
                    nb_news_articles+
                    movie_meter_IMDBpro+
                    nb_faces+
                    release_year+
                    #movie_id+
                    # movie_title_length+
                    aspect_ratio+
                    distributor_length+
                    is_Miramax+
                    is_Nov+
                    is_Dec+
                    comput+
                    holiday+
                    riot+
                    # terror+
                    # wish+
                    #love+
                    action+
                    #sport+
                    horror+
                    drama+
                    # crime+
                    documentary+
                    biography+
                    comedy+
                    animation+
                    R+
                    TV.G+
                    TV.14+
                    is_color+
                    duration*animation+
                    duration*documentary+
                    duration*biography+
                    duration*drama+
                    duration*animation+
                    duration*horror+
                    duration*action+
                    movie_budget*animation+
                    movie_budget*documentary+
                    movie_budget*biography+
                    movie_budget*drama+
                    movie_budget*animation+
                    movie_budget*horror+
                    movie_budget*action+
                    nb_news_articles*animation+
                    nb_news_articles*documentary+
                    nb_news_articles*biography+
                    nb_news_articles*drama+
                    nb_news_articles*animation+
                    nb_news_articles*horror+
                    nb_news_articles*action+
                    movie_meter_IMDBpro*animation+
                    movie_meter_IMDBpro*documentary+
                    movie_meter_IMDBpro*biography+
                    movie_meter_IMDBpro*drama+
                    movie_meter_IMDBpro*animation+
                    movie_meter_IMDBpro*horror+
                    movie_meter_IMDBpro*action+
                    duration*is_color+
                    movie_budget*is_color+
                    nb_news_articles*is_color+
                    movie_meter_IMDBpro*is_color+

  
                 ,data = cbind(df['imdb_score'],df[,significant_variables]))

summary(glm_model2)




sum(df$documentary)


sum(df$animation)
























test = read.csv('./test_data_IMDB_Fall_2023.csv')

create_maturity = function(data,f){
  
  data[f] <- ifelse(grepl(f, data$maturity_rating), 1, 0)
  return(ifelse(data[f]>=1, 1, 0))
}


transform_test = function(test){
  
  test$Biography <- ifelse(grepl("Biography", test$genres), 1, 0)+ifelse(grepl("biography", test$plot_keywords), 1, 0)
  test$biography = ifelse(test$Biography>=1,1,0)
  
  test$Comedy  <- ifelse(grepl("Comedy ", test$genres), 1, 0)+ifelse(grepl("comedy", test$plot_keywords), 1, 0)
  test$comedy  = ifelse(test$Comedy >=1,1,0)
  
  
  test$Animation  <- ifelse(grepl("Animation", test$genres), 1, 0)+ifelse(grepl("animation", test$plot_keywords), 1, 0)
  test$animation = ifelse(test$Animation >=1,1,0)
  
  
  test$Documentary  <- ifelse(grepl("Documentary", test$genres), 1, 0)+ifelse(grepl("documentary", test$plot_keywords), 1, 0)
  test$documentary = ifelse(test$Documentary >=1,1,0)
  
  
  test$is_Miramax = ifelse(grepl('Miramax', test$production_company), 1,0)
  test$is_Nov = ifelse(grepl('Nov', test$release_month), 1,0)
  test$is_Dec = ifelse(grepl('Dec', test$release_month), 1,0)
  
  test$is_color = ifelse(test$colour_film=="Color",1,0)
  
  maturity =c("R","PG-13",'Approved','TV-G','TV-14')
  
  for (r in maturity){
    test[r] = create_maturity(test,r)
  }
    test['TV.G'] = test['TV-G']
    test['TV.14'] = test['TV-14']
    
    test$actor1_length = lapply(test$actor1,nchar)
    test$genres_length = lapply(test$genres,nchar)
    test$distributor_length = lapply(test$distributor,nchar)
    test$movie_title_length = lapply(test$movie_title,nchar)
    
    test$actor1_length = as.numeric(test$actor1_length)
    test$genres_length = as.numeric(test$genres_length)
    test$distributor_length = as.numeric(test$distributor_length)
    test$movie_title_length = as.numeric(test$movie_title_length)
    test$comput  <- ifelse(grepl("comput", test$plot_keywords), 1, 0)
    test$holiday  <- ifelse(grepl("holiday", test$plot_keywords), 1, 0)
    test$riot  <- ifelse(grepl("riot", test$plot_keywords), 1, 0)
    test$terror  <- ifelse(grepl("terror", test$plot_keywords), 1, 0)
    test$wish  <- ifelse(grepl("wish", test$plot_keywords), 1, 0)
    test$love  <- ifelse(grepl("love", test$plot_keywords), 1, 0)
    
    
    return(test)
    }

  
  
  
  
  
test= transform_test(test)  
  
colnames(test)
  
write.csv(test,file = "Transformed Test Set.csv",row.names = FALSE) 
  
test = read.csv('./Transformed Test Set.csv')


test$log_nb_news_articles = log(test$nb_news_articles)

test$log_movie_meter_IMDBpro = log(test$movie_meter_IMDBpro)
test$log_movie_budget = log(test$movie_budget)
test$log_duration = log(test$duration)


test$log_nb_news_articles <- ifelse(is.infinite(test$log_nb_news_articles) & test$log_nb_news_articles < 0, 0, test$log_nb_news_articles)
test$log_movie_meter_IMDBpro <- ifelse(is.infinite(test$log_movie_meter_IMDBpro) & test$log_movie_meter_IMDBpro < 0, 0, test$log_movie_meter_IMDBpro)
test$log_movie_budget <- ifelse(is.infinite(test$log_movie_budget) & test$log_movie_budget < 0, 0, test$log_movie_budget)
test$log_duration <- ifelse(is.infinite(test$log_duration) & test$log_duration < 0, 0, test$log_duration)


test$log_release_year = log(test$release_year)


pred = predict(glm_model3, newdata = test)

colnames(test)


predict(lm_model, newdata = test)




summary(glm_model)


cbind(test$movie_title,as.list(pred))






get_cv_mse = function(model,data,K){
  
  mse_distribution = c()
  
  for (i in 1:100){
    
    mse_distribution = c(mse_distribution,cv.glm(data,model,K = K)$delta[1])
    
  }
  
  print( mse_distribution)
  
  hist( mse_distribution)
}





glm_model3 <- glm(imdb_score~movie_budget+ 
                    duration+
                    nb_news_articles+
                    movie_meter_IMDBpro+
                    #nb_faces+
                    release_year+
                    #movie_id+
                    # movie_title_length+
                    aspect_ratio+
                    distributor_length+
                    is_Miramax+
                    is_Nov+
                    #is_Dec+
                    comput+
                    holiday+
                    riot+
                    # terror+
                    # wish+
                    #love+
                    action+
                    #sport+
                    horror+
                    drama+
                    # crime+
                    documentary+
                    biography+
                    comedy+
                    animation+
                    R+
                    TV.G+
                    TV.14+
                    is_color+
                    # duration*animation+
                    # duration*documentary+
                    duration*biography+
                    duration*drama+
                    duration*animation+
                    # duration*horror+
                    duration*action+
                    # movie_budget*animation+
                    # movie_budget*documentary+
                    # movie_budget*biography+
                    # movie_budget*drama+
                    # movie_budget*animation+
                    # movie_budget*horror+
                    # movie_budget*action+
                    #nb_news_articles*animation+
                    nb_news_articles*documentary+
                    # nb_news_articles*biography+
                    # nb_news_articles*drama+
                    nb_news_articles*animation+
                    nb_news_articles*horror+
                    nb_news_articles*action+
                    movie_meter_IMDBpro*animation+
                    movie_meter_IMDBpro*documentary+
                    movie_meter_IMDBpro*biography+
                    movie_meter_IMDBpro*drama+
                    movie_meter_IMDBpro*animation+
                    movie_meter_IMDBpro*horror+
                    movie_meter_IMDBpro*action+
                    duration*is_color+
                    movie_budget*is_color+
                    #nb_news_articles*is_color+
                    #movie_meter_IMDBpro*is_color
                    
                    
                    ,data = cbind(df['imdb_score'],df[,significant_variables]))







summary(glm_model3)

outlierTest(glm_model3)
vif(glm_model3)




get_cv_mse(glm_model3,cbind(df['imdb_score'],df[,significant_variables]),10)




glm_model3 <- glm(imdb_score~poly(movie_budget, 1,raw = TRUE) + 
                    poly(duration, 2,raw = TRUE)+
                    poly(nb_news_articles, 1,raw = TRUE)+
                    poly(movie_meter_IMDBpro, 2)+
                    nb_faces+
                    release_year+
                    movie_id+
                    # movie_title_length+
                    aspect_ratio+
                    distributor_length+
                    is_Miramax+
                    is_Nov+
                    #is_Dec+
                    comput+
                    holiday+
                    riot+
                    # terror+
                    # wish+
                    #love+
                    action+
                    #sport+
                    horror+
                    #drama+
                    # crime+
                    documentary+
                    # biography+
                    # 
                    # comedy +
                    animation +
                    # R +
                    # TV.G+
                    TV.14+
                    is_color
                    # duration*animation+
                    # duration*documentary+
                    # duration*biography+
                    #duration*drama+
                    #duration*animation+
                    # duration*horror+
                    #duration*action+
                    # movie_budget*animation+
                    # movie_budget*documentary+
                    # movie_budget*biography+
                    # movie_budget*drama+
                    # movie_budget*animation+
                    # movie_budget*horror+
                    # movie_budget*action+
                    #nb_news_articles*animation+
                    # nb_news_articles*documentary+
                    # nb_news_articles*biography+
                    # nb_news_articles*drama+
                    #nb_news_articles*animation+
                    #nb_news_articles*horror+
                    #nb_news_articles*action
                    # movie_meter_IMDBpro*animation+
                    # movie_meter_IMDBpro*documentary+
                    # movie_meter_IMDBpro*biography+
                    #movie_meter_IMDBpro*drama+
                    #movie_meter_IMDBpro*animation+
                    #movie_meter_IMDBpro*horror+
                    #movie_meter_IMDBpro*action+
                    #duration*is_color+
                    # movie_budget*is_color+
                    # nb_news_articles*is_color+
                    #movie_meter_IMDBpro*is_color
                  
                  ,data = downsized_df)



score = c()

for (i in 1:100){
  
  score = c(score,cv.glm(downsized_df,glm_model3,K = 5)$delta[1])
  
}

score

hist(score)






downsized_df = cbind(df['imdb_score'],df[,significant_variables])






model = glm(imdb_score~poly(movie_budget, 1,raw = TRUE) + 
              poly(duration, 2,raw = TRUE)+
              poly(nb_news_articles, 1,raw = TRUE)+
              poly(movie_meter_IMDBpro, 3)+
              release_year+
              #maturity_rating+
              #poly(actor1_star_meter,2)+
              movie_title_length+
              is_color+
              genres_length+
              #distributor_length+
              actor1_length+
              biography+
              comedy+
              animation+
              documentary+
              is_Miramax+
              is_Nov
              #is_Dec
            ,data = df)

pred = predict(model, newdata = test)


cbind(test$movie_title,as.vector(pred))





summary(model)

vif(model)

outlierTest(model)


lm_model = lm(imdb_score~poly(movie_budget, 1,raw = TRUE) + 
              poly(duration, 2,raw = TRUE)+
              poly(nb_news_articles, 1,raw = TRUE)+
              poly(movie_meter_IMDBpro, 3)+
              release_year+
              #maturity_rating+
              #poly(actor1_star_meter,2)+
              #movie_title_length+
              is_color+
              genres_length+
              #distributor_length+
              #actor1_length+
              biography+
              comedy+
              animation+
              documentary+
              is_Miramax+
              is_Nov
            #is_Dec
            ,data = df)

summary(lm_model)

ncvTest(lm_model)




correct = coeftest(lm_model, vcov=vcovHC(lm_model, type="HC1"))




pred = predict(lm_model, newdata = test)


cbind(test$movie_title,as.vector(pred))




get_cv_mse(model, df,5)

cv.glm(df,model,K=5)$delta









glm_model = glm(imdb_score~poly(movie_budget, 1,raw = TRUE) + 
                poly(duration, 2,raw = TRUE)+
                poly(nb_news_articles, 1,raw = TRUE)+
                poly(movie_meter_IMDBpro, 3)+
                release_year+
                #maturity_rating+
                #poly(actor1_star_meter,2)+
                #movie_title_length+
                is_color+
                genres_length+
                #distributor_length+
                #actor1_length+
                biography+
                comedy+
                animation+
                documentary+
                is_Miramax+
                is_Nov
              #is_Dec
              ,data = df)




get_cv_mse(glm_model, df,5)



pred = predict(glm_model, newdata = test)


cbind(test$movie_title,as.vector(pred))


hist(df$distributor_length,breaks = 100)


hist(df$genres_length,breaks = 100)
hist(df$distributor_length,breaks = 100)
hist(log(df$genres_length),breaks = 100)
hist(log(df$distributor_length),breaks = 100)

df$log_genres_length = log(df$genres_length)

df$log_distributor_length = log(df$distributor_length)

test$log_genres_length = log(test$genres_length)

test$log_distributor_length = log(test$distributor_length)

write.csv(test, file = "logged test data.csv", row.names = FALSE)

























glm_model = glm(imdb_score~poly(log_movie_budget, 1,raw = TRUE) + 
                  log_duration, 3,raw = TRUE)+
                  log_nb_news_articles+
                  poly(log_movie_meter_IMDBpro, 3)+
                  #movie_id+
                  release_year+
                  #maturity_rating+
                  #poly(actor1_star_meter,2)+
                  #movie_title_length+
                  is_color+
                  genres_length+
                  distributor_length+
                  #poly(genres_length,2)+
                  #distributor_length+
                  #actor1_length+
                  biography+
                  comedy+
                  animation+
                  documentary+
                  is_Miramax+
                  is_Nov+
                  R
                #is_Dec
                ,data = df)


write.csv(df, file = "newest train.csv", row.names = FALSE)



get_cv_mse(glm_model,df,5)

residualPlots(glm_model)

vif(glm_model)

pred = predict(glm_model, newdata = test)


cbind(test$movie_title,as.vector(pred))




tail(colnames(test),10)



lm_model = lm(imdb_score~log_movie_budget + 
                  log_duration+
                  poly(log_nb_news_articles, 2)+
                  poly(log_movie_meter_IMDBpro, 3)+
                  release_year+
                  actor1_length+
                  #poly(log_actor1_star_meter,2)+
                  movie_title_length+
                  is_color+
                  genres_length+
                  #distributor_length+
                  genres_length +
                  #distributor_length+
                  #actor1_length+
                  biography+
                  comedy+
                  animation+
                  documentary+
                  is_Miramax+
                  is_Nov
                # comput+
                # holiday+
                # riot+
                # # terror+
                # # wish+
                # # love+
                # action+
                # sport+
                # horror+
                # drama+
                # crime+
                # documentary+
                # biography+
                # # comedy +
                # animation +
                # R +
                # TV.G+
                # TV.14+
                # is_color
                #is_Dec
                ,data = df)






glm_model <- glm(imdb_score~log_movie_budget + 
                    log_duration+
                    poly(log_nb_news_articles, 2)+
                    poly(log_movie_meter_IMDBpro, 2)+
                    poly(log_actor1_star_meter,2)+
                    nb_faces+
                    release_year+
                    genres_length+
                    aspect_ratio+
                    distributor_length+
                    movie_title_length+
                    is_Miramax+
                    is_Nov+
                    is_Dec+
                    comput+
                    holiday+
                    riot+
                    terror+
                    wish+
                    love+
                    action+
                    sport+
                    horror+
                    drama+
                    crime+
                    documentary+
                    biography+
                    comedy +
                    animation +
                    R +
                    TV.G+
                    TV.14+
                    is_color, data = df)




summary(glm_model)

vif(lm_model)

ncvTest(lm_model)

correct = coeftest(lm_model, vcov=vcovHC(lm_model, type="HC1"))


get_cv_mse(glm_model,df,K=5)

residualPlots(glm_model)





pred = predict(glm_model, newdata = test)


cbind(test$movie_title,as.vector(pred)) 



lm_model <- lm(imdb_score~log_movie_budget + 
                   poly(log_duration,2)+
                   poly(log_nb_news_articles, 1, raw = TRUE)+
                   poly(log_movie_meter_IMDBpro, 4)+
                   #poly(log_actor1_star_meter,2)+
                   nb_faces+
                   release_year+
                   genres_length+
                   aspect_ratio+
                   distributor_length+
                   #movie_title_length+
                   is_Miramax+
                   # is_Nov+
                   # is_Dec+
                   comput+
                   holiday+
                   riot+
                   # terror+
                   # wish+
                   # love+
                   action+
                   sport+
                   horror+
                   drama+
                   crime+
                   documentary+
                   biography+
                   # comedy +
                   animation +
                   R +
                   TV.G+
                   TV.14+
                   is_color, data = df)


residualPlot(lm_model)
residualPlots(lm_model)



summary(lm_model)



pred = predict(lm_model, newdata = test)


cbind(test$movie_title,as.vector(pred)) 



glm_model <- glm(imdb_score~log_movie_budget + 
                 log_duration+
                 poly(log_nb_news_articles, 2)+
                 poly(log_movie_meter_IMDBpro, 4)+
                 #poly(log_actor1_star_meter,2)+
                 nb_faces+
                 release_year+
                 #genres_length+
                 aspect_ratio+
                 distributor_length+
                 #movie_title_length+
                 is_Miramax+
                 # is_Nov+
                 # is_Dec+
                 comput+
                 holiday+
                 riot+
                 # terror+
                 # wish+
                 # love+
                 action+
                 sport+
                 horror+
                 drama+
                 crime+
                 documentary+
                 biography+
                 # comedy +
                 animation +
                 R +
                 TV.G+
                 TV.14+
                 is_color, data = df)


glm_model = glm(imdb_score~log_movie_budget + 
                log_duration+
                poly(log_nb_news_articles, 2)+
                poly(log_movie_meter_IMDBpro, 3)+
                release_year+
                actor1_length+
                #poly(log_actor1_star_meter,2)+
                movie_title_length+
                is_color+
                genres_length+
                #distributor_length+
                genres_length +
                #distributor_length+
                #actor1_length+
                biography+
                comedy+
                animation+
                documentary+
                is_Miramax+
                is_Nov+
                # comput+
                # holiday+
                # riot+
                # # terror+
                # # wish+
                # # love+
                # action+
                # sport+
                # horror+
                # drama+
                # crime+
                documentary+
                biography+
                # comedy +
                animation +
                # R +
                # TV.G+
                # TV.14+
                is_color
              #is_Dec
              ,data = df)


lm_model = lm(imdb_score~log_movie_budget + 
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                #release_year+
                #actor1_length+
                #poly(log_actor1_star_meter,1,raw = TRUE)+
                #movie_title_length+
                is_color+
                poly(genres_length,2)+
                #distributor_length+
                #genres_length +
                #distributor_length+
                #actor1_length+
                biography+
                #comedy+
                animation+
                documentary+
                is_Miramax+
                #is_Nov+
              # comput+
              # holiday+
              # riot+
              # # terror+
              # # wish+
              #love+
              # action+
              # sport+
              horror+
              drama+
              # crime+
              documentary+
              biography+
              # # comedy +
              animation+ 
              R+ 
              #TV.G
              TV.14+
              is_color
              #is_Dec
              ,data = df)




residualPlots(lm_model)



get_cv_mse(glm_model,df,K=5)




glm_model = glm(imdb_score~log_movie_budget + 
                log_duration+
                poly(log_nb_news_articles, 1, raw = TRUE)+
                poly(log_movie_meter_IMDBpro, 4)+
                #release_year+
                #actor1_length+
                #poly(log_actor1_star_meter,1,raw = TRUE)+
                #movie_title_length+
                is_color+
                poly(genres_length,2)+
                #distributor_length+
                #genres_length +
                #distributor_length+
                #actor1_length+
                biography+
                #comedy+
                animation+
                documentary+
                is_Miramax+
                #is_Nov+
                # comput+
                # holiday+
                # riot+
                # # terror+
                # # wish+
                #love+
                # action+
                # sport+
                horror+
                drama+
                # crime+
                documentary+
                biography+
                # # comedy +
                animation+ 
                R+ 
                #TV.G
                TV.14+
                is_color
              #is_Dec
              ,data = df)






pred = predict(glm_model, newdata = test)


cbind(test$movie_title,as.vector(pred)) 

summary(lm_model)



lm_model = lm(imdb_score~log_movie_budget + 
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
                ,data = df[-c(1806,1581,191,395,1436,1255,989),])

get_cv_mse(glm_model,df[-c(1806,1581,191,395,1436,1255,989),],K=5)




