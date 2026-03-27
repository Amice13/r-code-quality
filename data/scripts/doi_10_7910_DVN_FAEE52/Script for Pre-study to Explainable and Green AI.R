
# Script for pre-study to "Consumers Are Willing to Pay a Price for Explainable, But Not for Green AI. Evidence from a Choice-Based Conjoint Analysis"

# Version: 9 November 2021


library(conjoint)
library(grid)
library(gridExtra)
library(mlogit)
library(texreg)
library(skpr)
library(AlgDesign)
library(idefix)
library(ggplot2)
library(jtools)



#### Step 1: Create orthogonal design and full choice set matrix #####

experiment<-expand.grid(Nutzen =  c("A",
                                    "B",
                                    "C"),
                        Transparenz =  c("A",
                                         "B",
                                         "C"),
                        Kosten =  c("A",
                                    "B",
                                    "C"),
                        Energie =  c("A",
                                     "B",
                                     "C"))

# create orthogonal design
design1=caFactorialDesign(data=experiment, 
                          type = "orthogonal")
print(design1)
print(cor(caEncodedDesign(design1)))


# produce alternatives via shifting

design1 <- data.frame(lapply(design1, 
                             function(x) 
                               as.numeric(x)))

design2 <- design1
design2[design1 == 1] <- 2
design2[design1 == 2] <- 3
design2[design1 == 3] <- 1

design3 <- design2
design3[design2 == 1] <- 2
design3[design2 == 2] <- 3
design3[design2 == 3] <- 1



# combine the designs to get choice sets

fulldesign <- cbind(design1, design2, design3)


fulldesign_long <- data.frame()
for (i in 1: nrow(design1)){
  tmp <- data.frame(choice = i, 
                    stimulus = 1:4, 
                    rbind(design1[i, ], design2[i,], design3[i ,], rep(0, 5) ) # repeat 0 for no choice and 5 times for 5 variables
  )
  
  fulldesign_long <- rbind(fulldesign_long, tmp)
  
}



#### Step 2: Prepare downloaded data and merge with conjoint choice design ####


#### Read data ####

df.AI.pre <- read.csv("Dataset pre-study.csv")


#### prepare downloaded data ####

# transform choice variables

df.AI.pre$set1 <- 4
df.AI.pre$set1[df.AI.pre$CS01 == "Option A"] <- 1
df.AI.pre$set1[df.AI.pre$CS01 == "Option B"] <- 2
df.AI.pre$set1[df.AI.pre$CS01 == "Option C"] <- 3

df.AI.pre$set2 <- 4
df.AI.pre$set2[df.AI.pre$CS02 == "Option A"] <- 1
df.AI.pre$set2[df.AI.pre$CS02 == "Option B"] <- 2
df.AI.pre$set2[df.AI.pre$CS02 == "Option C"] <- 3

df.AI.pre$set3 <- 4
df.AI.pre$set3[df.AI.pre$CS03 == "Option A"] <- 1
df.AI.pre$set3[df.AI.pre$CS03 == "Option B"] <- 2
df.AI.pre$set3[df.AI.pre$CS03 == "Option C"] <- 3

df.AI.pre$set4 <- 4
df.AI.pre$set4[df.AI.pre$CS04 == "Option A"] <- 1
df.AI.pre$set4[df.AI.pre$CS04 == "Option B"] <- 2
df.AI.pre$set4[df.AI.pre$CS04 == "Option C"] <- 3

df.AI.pre$set5 <- 4
df.AI.pre$set5[df.AI.pre$CS05 == "Option A"] <- 1
df.AI.pre$set5[df.AI.pre$CS05 == "Option B"] <- 2
df.AI.pre$set5[df.AI.pre$CS05 == "Option C"] <- 3

df.AI.pre$set6 <- 4
df.AI.pre$set6[df.AI.pre$CS06 == "Option A"] <- 1
df.AI.pre$set6[df.AI.pre$CS06 == "Option B"] <- 2
df.AI.pre$set6[df.AI.pre$CS06 == "Option C"] <- 3

df.AI.pre$set7 <- 4
df.AI.pre$set7[df.AI.pre$CS07 == "Option A"] <- 1
df.AI.pre$set7[df.AI.pre$CS07 == "Option B"] <- 2
df.AI.pre$set7[df.AI.pre$CS07 == "Option C"] <- 3

df.AI.pre$set8 <- 4
df.AI.pre$set8[df.AI.pre$CS08 == "Option A"] <- 1
df.AI.pre$set8[df.AI.pre$CS08 == "Option B"] <- 2
df.AI.pre$set8[df.AI.pre$CS08 == "Option C"] <- 3

df.AI.pre$set9 <- 4
df.AI.pre$set9[df.AI.pre$CS09 == "Option A"] <- 1
df.AI.pre$set9[df.AI.pre$CS09 == "Option B"] <- 2
df.AI.pre$set9[df.AI.pre$CS09 == "Option C"] <- 3




#### Transform response data into required format ####


df.AI.pre$respondent <- 1:nrow(df.AI.pre)

test_frame <- df.AI.pre



r <- 9 # set number of choice sets
k <- 4 # set number of stimuli per choice set


mnl_frame <- data.frame() # define empty dataframe

for (i in 1:nrow(test_frame)){ # build datasets for every respondent and append them.
  
  tmp <- data.frame(respondent = test_frame$respondent[i],
                     choice = rep(1:r, each=k), 
                     stimulus = rep(1:k, r)
                     )
  
  tmp_choice <- rep(0, r*k)
  # get indices: which 
  get_indices <- c(test_frame$set1[i],
    test_frame$set2[i],
    test_frame$set3[i],
    test_frame$set4[i],
    test_frame$set5[i],
    test_frame$set6[i],
    test_frame$set7[i],
    test_frame$set8[i],
    test_frame$set9[i]
  ) + rep(k, r)*(0:(r-1)) # take into account that indices add up in steps of 3
  
  tmp_choice[get_indices] <- 1
  
  tmp <- data.frame(cbind(tmp, tmp_choice))
  mnl_frame <- rbind(mnl_frame, tmp)
  
}



#### generate dummy variables ####

# Nutzen - utility
fulldesign_long$Nutzen1 <- 0
fulldesign_long$Nutzen1[fulldesign_long$Nutzen == 1] <- 1

fulldesign_long$Nutzen2 <- 0
fulldesign_long$Nutzen2[fulldesign_long$Nutzen == 2] <- 1

fulldesign_long$Nutzen3 <- 0
fulldesign_long$Nutzen3[fulldesign_long$Nutzen == 3] <- 1


# Transparenz - transparency 

fulldesign_long$Transparenz1 <- 0
fulldesign_long$Transparenz1[fulldesign_long$Transparenz == 1] <- 1

fulldesign_long$Transparenz2 <- 0
fulldesign_long$Transparenz2[fulldesign_long$Transparenz == 2] <- 1

fulldesign_long$Transparenz3 <- 0
fulldesign_long$Transparenz3[fulldesign_long$Transparenz == 3] <- 1


# Kosten - costs

fulldesign_long$Kosten1 <- 0
fulldesign_long$Kosten1[fulldesign_long$Kosten == 1] <- 1

fulldesign_long$Kosten2 <- 0
fulldesign_long$Kosten2[fulldesign_long$Kosten == 2] <- 1

fulldesign_long$Kosten3 <- 0
fulldesign_long$Kosten3[fulldesign_long$Kosten == 3] <- 1


# Energie -energy efficiency

fulldesign_long$Energie1 <- 0
fulldesign_long$Energie1[fulldesign_long$Energie == 1] <- 1

fulldesign_long$Energie2 <- 0
fulldesign_long$Energie2[fulldesign_long$Energie == 2] <- 1

fulldesign_long$Energie3 <- 0
fulldesign_long$Energie3[fulldesign_long$Energie == 3] <- 1


# none
fulldesign_long$none <- 0
fulldesign_long$none[fulldesign_long$stimulus == 4] <- 1






# match data.frame with the designs
merged_data <- merge(mnl_frame, fulldesign_long, by = c("choice", "stimulus"))
merged_data <- merged_data[order(merged_data$respondent), ]


# convert data for mlogit
cbc <- mlogit.data(merged_data, choice="tmp_choice", shape="long", alt.var="stimulus", id.var = "respondent")




#### Step 3: Analysis ####

#### partworth model ####

ml1 <- mlogit(tmp_choice ~ Nutzen2 + Nutzen3 +
                Transparenz2 + Transparenz3 +
                Kosten2 + Kosten3 +
                Energie2 + Energie3 + 
                none | 0, cbc)
summary(ml1)



# wordreg(  ml1,   file = "Pretest 1.doc")





p1 <- plot_summs(ml1, scale = TRUE, 
                 ci_level = 0.95) + 
  theme( axis.text.y=element_text(hjust=0.95,vjust=0.2)) +
  scale_x_continuous(breaks = seq(-1.5, 1.5, by = 0.5)) +
  scale_y_discrete(labels = c('No-choice option',
                              'Energy 5h',
                              'Energy 3h',
                              '3.99 EUR',
                              '1.99 EUR',
                              'Transparency high',
                              'Transparency low',
                              '99% satisfied',
                              '94% satisfied'  )) +  theme(legend.position="bottom")


# png("Fig_pre_multinomlog.png", width=12, height=16,units="cm", res=400)

p1

# dev.off()





