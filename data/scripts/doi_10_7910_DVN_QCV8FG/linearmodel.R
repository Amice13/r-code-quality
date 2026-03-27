library(tidyverse) 
library(gmodels) #for CrossTable
library(describedata) #for proc_means
library(ggplot2) #for heatmap
library(reshape2)
library(dplyr)
library(broom)
library(gmodels) #for CrossTable
library(describedata) #for proc_means

train_data = read.csv("train_data.csv",header=TRUE,sep= ",")
test_data = read.csv("test_data.csv",header=TRUE,sep= ",")
ls(df_data)

attach(df_data)

df_data$acrylic = as.integer(as.logical(df_data$acrylicMaterial_New))
df_data$oil = as.integer(as.logical(df_data$oilMaterial_New))
df_data$mixedmed = as.integer(as.logical(df_data$mixedmediaMaterial_New))
df_data$canvas = as.integer(as.logical(df_data$canvasMaterial_New))
df_data$framed = as.integer(as.logical(df_data$isFramed_New))
df_data$handmade = as.integer(as.logical(df_data$isHandmade_New))
df_data$northamerica = ifelse(df_data$location_clean=="North America", 1,0)
df_data$europe = ifelse(df_data$location_clean=="Europe", 1,0)

train_data$acrylic = as.integer(as.logical(train_data$acrylicMaterial_New))
train_data$oil = as.integer(as.logical(train_data$oilMaterial_New))
train_data$mixedmed = as.integer(as.logical(train_data$mixedmediaMaterial_New))
train_data$canvas = as.integer(as.logical(train_data$canvasMaterial_New))
train_data$framed = as.integer(as.logical(train_data$isFramed_New))
train_data$handmade = as.integer(as.logical(train_data$isHandmade_New))
train_data$northamerica = ifelse(train_data$location_clean=="North America", 1,0)
train_data$europe = ifelse(train_data$location_clean=="Europe", 1,0)

test_data$acrylic = as.integer(as.logical(test_data$acrylicMaterial_New))
test_data$oil = as.integer(as.logical(test_data$oilMaterial_New))
test_data$mixedmed = as.integer(as.logical(test_data$mixedmediaMaterial_New))
test_data$canvas = as.integer(as.logical(test_data$canvasMaterial_New))
test_data$framed = as.integer(as.logical(test_data$isFramed_New))
test_data$handmade = as.integer(as.logical(test_data$isHandmade_New))
test_data$northamerica = ifelse(test_data$location_clean=="North America", 1,0)
test_data$europe = ifelse(test_data$location_clean=="Europe", 1,0)

#Structured inputs + 2 interactions
model1 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ num_images*num_words_in_title, data = train_data)

summary(model1)
glance(model1)

#Model 2 = Model 1 + Painting title densities

model2 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ num_images*num_words_in_title +
              PT_topic0 + PT_topic1 + PT_topic2 , data = train_data)

summary(model2)
glance(model2)


#Model 2a = Model 1 + Painting title densities** removing topic1 and adding back topic3

model2a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PT_topic0 + PT_topic2 + PT_topic3  , data = train_data)

summary(model2a)
glance(model2a)


#Model 2b = Model 1 + Painting title densities** removing topic2 and adding back topic3

model2b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PT_topic0 + PT_topic1 + PT_topic3  , data = train_data)

summary(model2b)
glance(model2b)


#Model 3 = Model 1 + Painting Description densities

model3 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ num_images*num_words_in_title +
              PD_topic0 + PD_topic1 + PD_topic2 , data = train_data)

summary(model3)
glance(model3)

#Model 3a = Model 1 + Painting Description densities**removing topic1 and replacing with topic3

model3a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PD_topic0  + PD_topic2 + PD_topic3, data = train_data)

summary(model3a)
glance(model3a)

#Model 3b = Model 1 + Painting Description densities**removing topic2 and replacing with topic3

model3b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PD_topic0  + PD_topic1 + PD_topic3, data = train_data)

summary(model3b)
glance(model3b)


#Model 4 = Model 1 + Painting Title & Description densities

model4 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ num_images*num_words_in_title +
              PD_topic0  + PD_topic2 + PD_topic3+
              PD_topic0 + PD_topic1 + PD_topic2 , data = train_data)

summary(model4)
glance(model4)


#Model 4a = Model 1 + Painting Title & Description densities

model4a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PT_topic0 + PT_topic2 + PT_topic3+
               PD_topic0 + PD_topic2 + PD_topic3 , data = train_data)

summary(model4a)
glance(model4a)

#Model 4b = Model 1 + Painting Title & Description densities

model4b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ num_images*num_words_in_title +
               PT_topic0 + PT_topic1 + PT_topic3+
               PD_topic0 + PD_topic1 + PD_topic3 , data = train_data)

summary(model4b)
glance(model4b)



#Model 5 = Model 4 + interaction terms num_images * title topic densities

model5 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ 
              num_images*num_words_in_title +
              PT_topic0 + PT_topic1 + PT_topic2+
              num_images*PT_topic0 + num_images*PT_topic1 + num_images*PT_topic2+
              PD_topic0 + PD_topic1 + PD_topic2 , data = train_data)

summary(model5)
glance(model5)


#Model 5 = Model 4 + interaction terms num_images * title topic densities

model5a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic2 + PT_topic3+
               num_images*PT_topic0 + num_images*PT_topic2 + num_images*PT_topic3+
               PD_topic0 + PD_topic2 + PD_topic3 , data = train_data)

summary(model5a)
glance(model5a)

#Model 5 = Model 4 + interaction terms num_images * title topic densities

model5b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic1 + PT_topic3+
               num_images*PT_topic0 + num_images*PT_topic1 + num_images*PT_topic3+
               PD_topic0 + PD_topic1 + PD_topic3 , data = train_data)

summary(model5b)
glance(model5b)


#Model 6 = Model 4 + interaction terms num_images * description topic densities

model6 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ 
              num_images*num_words_in_title +
              PT_topic0 + PT_topic1 + PT_topic2+
              PD_topic0 + PD_topic1 + PD_topic2 + 
              num_images*PD_topic0 + num_images*PD_topic1 + num_images*PD_topic2 , data = train_data)

summary(model6)
glance(model6)

#Model 6 = Model 4 + interaction terms num_images * description topic densities

model6a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic2 + PT_topic3+
               PD_topic0 + PD_topic2 + PD_topic3 + 
               num_images*PD_topic0 + num_images*PD_topic2 + num_images*PD_topic3 , data = train_data)

summary(model6a)
glance(model6a)

#Model 6 = Model 4 + interaction terms num_images * description topic densities

model6b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic1 + PT_topic3+
               PD_topic0 + PD_topic1 + PD_topic3 + 
               num_images*PD_topic0 + num_images*PD_topic1 + num_images*PD_topic3 , data = train_data)

summary(model6b)
glance(model6b)


#Model 7 = Model 4 + interaction terms both 5 and 6

model7 = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
              admirers + seller_rating + seller_number_of_reviews + northamerica +
              europe + 
              #height_clean + width_clean + num_images + 
              height_clean*width_clean + painting_description_num_words + 
              num_words_in_title+ 
              num_images*num_words_in_title +
              PT_topic0 + PT_topic1 + PT_topic2+
              PD_topic0 + PD_topic1 + PD_topic2 + 
              num_images*PT_topic0 + num_images*PT_topic1 + num_images*PT_topic2+
              num_images*PD_topic0 + num_images*PD_topic1 + num_images*PD_topic2 , data = train_data)

summary(model7)
glance(model7)


#Model 7 = Model 4 + interaction terms both 5 and 6

model7a = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic2 + PT_topic3+
               PD_topic0 + PD_topic2 + PD_topic3 + 
               num_images*PT_topic0 + num_images*PT_topic2 + num_images*PT_topic3+
               num_images*PD_topic0 + num_images*PD_topic2 + num_images*PD_topic3 , data = train_data)

summary(model7a)
glance(model7a)


#Model 7 = Model 4 + interaction terms both 5 and 6

model7b = lm(log(price_clean) ~ acrylic + oil + mixedmed + canvas + framed + handmade +
               admirers + seller_rating + seller_number_of_reviews + northamerica +
               europe + 
               #height_clean + width_clean + num_images + 
               height_clean*width_clean + painting_description_num_words + 
               num_words_in_title+ 
               num_images*num_words_in_title +
               PT_topic0 + PT_topic1 + PT_topic3+
               PD_topic0 + PD_topic1 + PD_topic3 + 
               num_images*PT_topic0 + num_images*PT_topic1 + num_images*PT_topic3+
               num_images*PD_topic0 + num_images*PD_topic1 + num_images*PD_topic3 , data = train_data)

summary(model7b)
glance(model7b)

#predicting final model 7b on test data to get MSE

test_prediction = predict(model7b, test_data)



write.csv(cbind(test_prediction,log(test_data$price_clean)),file= 'prediction.csv')


#FITTING FINAL MODEL 7 ON STUDY 1 PAINTINGS TO PREDICT THEIR PRICES

pred_price = exp(predict(model7b, newdata = study1))





#nrow(df_data)
