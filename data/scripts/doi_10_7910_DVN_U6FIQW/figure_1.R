library("ggplot2")
library("dplyr")
library("ggpubr")
library('readxl')

####read the dataset

d <- read_excel("transcribing_tiktok_openai_with_final_dataset_0427_anonymized.xlsx") #read dataset

####A. Count videos with descriptions using anger, fear, and joy

dnew_emotion_desc <- d %>%
  select("anger_desc","fear_desc","joy_desc") #select variables of interest

desc_count <- dnew_emotion_desc %>%
  summarize(across(everything(), ~ sum(. > 0))) #sum values that are larger than 0

desc_count_t <- t(desc_count) #transpose the dataset

colnames(desc_count_t) <- "count" #rename column as "count"

desc_prop <- dnew_emotion_desc %>%
  summarize(across(everything(), ~ round(mean(. > 0) * 100, 2))) #calculate proportion 

desc_prop_t <- t(desc_prop) #transpose the dataset

colnames(desc_prop_t) <- "prop" #rename column as "prop"

d_desc <- as.data.frame(cbind(desc_count_t, desc_prop_t)) #merge two datasets 

d_desc #check d_desc

d_desc <- data.frame(
  emotion = c("anger", "fear", "joy"),
  count = c(918, 1337, 1274),
  prop = c(13.38, 19.48, 18.57)
) #make a new dataset for plotting

mod1 <- ggplot(data = d_desc, aes(x = emotion, y = count)) + geom_bar(stat = "identity") #create plots using count of videos

mod1 <- mod1 + coord_flip() #flip the plot

mod1 <- mod1 + theme_bw() + geom_text(aes(label = paste0(prop, "%")), hjust = 1.5, color = "white", size = 3.5) + xlab("number of videos(%)") + ylab("emotions in video descriptions") #organize the plot

mod1 #show plot

####B. Count videos with transcripts using anger, fear, and joy

dnew_emotion_trans <- d %>%
  select("anger_openai_trans","fear_openai_trans","joy_openai_trans") 

trans_count <- dnew_emotion_trans %>%
  summarize(across(everything(), ~ sum(. > 0))) #sum values that are larger than 0

trans_count_t <- t(trans_count) #transpose the dataset

colnames(trans_count_t) <- "count" #rename column as "count"

trans_count_t 

trans_prop <- dnew_emotion_trans %>%
  summarize(across(everything(), ~ round(mean(. > 0) * 100, 2))) #calculate proportion 

trans_prop_t <- t(trans_prop) #transpose the dataset

colnames(trans_prop_t) <- "prop" #rename column as "prop"

d_trans <- cbind(trans_count_t, trans_prop_t) #merge two datasets 

d_trans <- data.frame(
  emotion = c("anger", "fear", "joy"),
  count = c(3539, 4042, 3556),
  prop = c(51.57, 58.90, 51.82)
) #make a new dataset for plotting


mod2 <- ggplot(data = d_trans, aes(x = emotion, y = count)) +
  geom_bar(stat = "identity") #create plots using count of videos

mod2 <- mod2 + coord_flip() #flip the plot

mod2 <- mod2 + theme_bw() + geom_text(aes(label = paste0(prop, "%")), hjust = 1.5, color = "white", size = 3.5) + xlab("number of videos(%)") + ylab("emotions in video transcripts") #organize the plot

mod2 #show plot

####C. Count videos with deepface using anger, happy, and fear

dnew_deep_face <- as.data.frame(d %>%
  count(dominant_emotion_deep_face) %>%
  mutate(prop = (n / 6862)*100)) %>%
  mutate(prop = round(prop, 2)) #summarize dataset with n and proportion

dnew_deep_face_subset <- dnew_deep_face[c(4,3,1),] #subset using anger, happiness, and fear

dnew_deep_face_subset$dominant_emotion_deep_face[dnew_deep_face_subset$dominant_emotion_deep_face == "angry"] <- "anger"

dnew_deep_face_subset$dominant_emotion_deep_face[dnew_deep_face_subset$dominant_emotion_deep_face == "happy"] <- "happiness"

mod3 <- ggplot(data = dnew_deep_face_subset, aes(x = dominant_emotion_deep_face, y = n)) +
  geom_bar(stat = "identity") #create plots using count of videos

mod3 <- mod3 + coord_flip() #flip the plot

mod3 <- mod3 + theme_bw() + geom_text(aes(label = paste0(prop, "%")), hjust = 1.5, color = "white", size = 3.5) + xlab("number of videos(%)") + ylab("emotions in video thumbnails") #organize the plot

mod3 #show plot

figure <- ggarrange(mod1, mod2, mod3,
                    labels = c("A", "B", "C"),
                    ncol = 2, nrow = 2) #compile plots
figure #show finalized plot

                          
