
library(dplyr)
library(boot)
library(car)
library(ggplot2)
library(stargazer)
library(psych)


setwd('C:/Users/zzhong13/Desktop/Multivariate')
df = read.csv('./Cleaned Training Set.csv')


quick_fit = function(data){
  model_formula <- as.formula(paste("imdb_score ~ .", collapse = " + "))
  lm_model <- lm(formula = model_formula, data = data)
  
  return(summary(lm_model))
  
}

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





ncvTest(lm_model)


residualPlots(lm_model)



score = c()

for (i in 1:100){
 
  score = c(score,cv.glm(cbind(df['imdb_score'],df[,significant_variables]),glm_model2, K =5 )$delta[1])
  
}



hist(score)


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

pred = predict(glm_model3, newdata = test)


pred



summary(glm_model)


cbind(test$movie_title,as.list(pred))






get_cv_mse = function(model,data,K){
  
  score = c()
  
  for (i in 1:100){
    
    score = c(score,cv.glm(data,model,K = K)$delta[1])
    
  }
  
  print(score)
  
  hist(score)
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


require(lmtest)
require(plm)


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






