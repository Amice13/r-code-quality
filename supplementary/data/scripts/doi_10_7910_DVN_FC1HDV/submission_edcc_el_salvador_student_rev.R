
rm (list = ls(all=TRUE))

#set your working directory

setwd("")

#install necessary packages

install.packages("ltm")
install.packages("irtoys")
install.packages("psych")
install.packages("plink")
install.packages("estimatr")
install.packages("ggplot2")
install.packages("mice")
install.packages("miceadd")
install.packages("mfx")
install.packages("texreg")
install.packages("dplyr")
install.packages("ICC")
install.packages("twosamples")
install.packages("car")

library(ltm)
library(irtoys)
library(psych)
library(plink)
library(estimatr)
library(ggplot2)
library(mice)
library(miceadd)
library(mfx)
library(texreg)
library(dplyr)
library(ICC)
library(twosamples)
library(car)

###Table3####

###store the csv files in your working directory.

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_baseline_t <- subset(student_test_2_baseline,
                                    treatment_2==1)

student_test_2_baseline_c <- subset(student_test_2_baseline,
                                    treatment_2==0)

#shift

mean(student_test_2_baseline_t$Shift*100, na.rm = T)
mean(student_test_2_baseline_c$Shift*100, na.rm = T)

Diff_shift <- mean(student_test_2_baseline_t$Shift*100, na.rm = T)-
  mean(student_test_2_baseline_c$Shift*100, na.rm = T)

reg_Shift_baseline <- lm_robust(Shift~
                                  treatment_2+
                                  department*urban.rural, 
                                
                                data=student_test_2_baseline, 
                                clusters = ID_IE, se_type = "stata")

#age

mean(student_test_2_baseline_t$Age_r, na.rm = T)
mean(student_test_2_baseline_c$Age_r, na.rm = T)

sd(student_test_2_baseline_t$Age_r, na.rm = T)
sd(student_test_2_baseline_c$Age_r, na.rm = T)

Diff_age <- mean(student_test_2_baseline_t$Age_r, na.rm = T)-
  mean(student_test_2_baseline_c$Age_r, na.rm = T)

reg_Age_baseline <- lm_robust(Age_r~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline, 
                              clusters = ID_IE, se_type = "stata")

#sex

mean(student_test_2_baseline_t$Sex*100, na.rm = T)
mean(student_test_2_baseline_c$Sex*100, na.rm = T)

Diff_sex <- mean(student_test_2_baseline_t$Sex*100, na.rm = T)-
  mean(student_test_2_baseline_c$Sex*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Sex~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline, 
                              clusters = ID_IE, se_type = "stata")

#N. of older siblings

mean(student_test_2_baseline_t$no_elder, na.rm = T)
mean(student_test_2_baseline_c$no_elder, na.rm = T)

sd(student_test_2_baseline_t$no_elder, na.rm = T)
sd(student_test_2_baseline_c$no_elder, na.rm = T)

Diff_no_elder <- mean(student_test_2_baseline_t$no_elder, na.rm = T)-
  mean(student_test_2_baseline_c$no_elder, na.rm = T)

reg_no_elder_baseline <- lm_robust(no_elder~
                                     treatment_2+
                                     department*urban.rural, 
                                   
                                   data=student_test_2_baseline, 
                                   clusters = ID_IE, se_type = "stata")

#younger brothers or/and sisters

mean(student_test_2_baseline_t$no_younger, na.rm = T)
mean(student_test_2_baseline_c$no_younger, na.rm = T)

sd(student_test_2_baseline_t$no_younger, na.rm = T)
sd(student_test_2_baseline_c$no_younger, na.rm = T)

Diff_no_younger <- mean(student_test_2_baseline_t$no_younger, na.rm = T)-
  mean(student_test_2_baseline_c$no_younger, na.rm = T)

reg_no_younger_baseline <- lm_robust(no_younger~
                                       treatment_2+
                                       department*urban.rural, 
                                     
                                     data=student_test_2_baseline, 
                                     clusters = ID_IE, se_type = "stata")

#test scores (20 items)

mean(student_test_2_baseline_t$total_score_2, na.rm = T)
mean(student_test_2_baseline_c$total_score_2, na.rm = T)

sd(student_test_2_baseline_t$total_score_2, na.rm = T)
sd(student_test_2_baseline_c$total_score_2, na.rm = T)

Diff_total_score_2 <- mean(student_test_2_baseline_t$total_score_2, na.rm = T)-
  mean(student_test_2_baseline_c$total_score_2, na.rm = T)

reg_total_score_baseline <- lm_robust(total_score_2~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (20 items) 性別で分ける。

student_test_2_baseline_female <- subset(student_test_2_baseline,
                                         Sex==0)

student_test_2_baseline_male <- subset(student_test_2_baseline,
                                       Sex==1)

student_test_2_baseline_female <- subset(student_test_2_baseline,
                                         Sex==0&
                                           treatment_2==0)

student_test_2_baseline_male <- subset(student_test_2_baseline,
                                       Sex==1&
                                         treatment_2==0)

student_test_2_baseline_c <- subset(student_test_2_baseline,
                                    treatment_2==0)

student_test_2_baseline_t <- subset(student_test_2_baseline,
                                    treatment_2==1)

student_test_2_baseline$Sex

mean(student_test_2_baseline_female$total_score_2, na.rm = T)
mean(student_test_2_baseline_male$total_score_2, na.rm = T)

sd(student_test_2_baseline_female$total_score_2, na.rm = T)
sd(student_test_2_baseline_male$total_score_2, na.rm = T)

reg_total_score_baseline <- lm_robust(ztest_score_baseline~
                                        Sex, 
                                      
                                      data=student_test_2_baseline_t, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (z scores of 20 items)

mean(student_test_2_baseline_t$ztest_score_baseline, na.rm = T)
mean(student_test_2_baseline_c$ztest_score_baseline, na.rm = T)

Diff_ztest_score_baseline <- mean(student_test_2_baseline_t$ztest_score_baseline, na.rm = T)-
  mean(student_test_2_baseline_c$ztest_score_baseline, na.rm = T)

reg_total_score_baseline <- lm_robust(ztest_score_baseline~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (18 items)

mean(student_test_2_baseline_t$total_score_baseline_exc2_4, na.rm = T)
mean(student_test_2_baseline_c$total_score_baseline_exc2_4, na.rm = T)

sd(student_test_2_baseline_t$total_score_baseline_exc2_4, na.rm = T)
sd(student_test_2_baseline_c$total_score_baseline_exc2_4, na.rm = T)

Diff_total_score_baseline_exc2_4 <- 
  mean(student_test_2_baseline_t$total_score_baseline_exc2_4, na.rm = T)-
  mean(student_test_2_baseline_c$total_score_baseline_exc2_4, na.rm = T)

reg_total_score_baseline_exc2_4 <- lm_robust(total_score_baseline_exc2_4~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline, 
                                             clusters = ID_IE, se_type = "stata")

#test scores (z scores of 18 items)

mean(student_test_2_baseline_t$ztest_score_baseline_exc_2_4, na.rm = T)
mean(student_test_2_baseline_c$ztest_score_baseline_exc_2_4, na.rm = T)

Diff_ztest_score_baseline_exc_2_4 <- 
  mean(student_test_2_baseline_t$ztest_score_baseline_exc_2_4, na.rm = T)-
  mean(student_test_2_baseline_c$ztest_score_baseline_exc_2_4, na.rm = T)

reg1_exc2_4 <- lm_robust(ztest_score_baseline_exc_2_4~
                           treatment_2+
                           department*urban.rural, 
                         data=student_test_2_baseline, 
                         clusters = ID_IE, se_type = "stata")

#textbook last year

mean(student_test_2_baseline_t$Math_textbook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_c$Math_textbook_LY_e*100, na.rm = T)

Diff_Math_textbook_LY_e_baseline <- 
  mean(student_test_2_baseline_t$Math_textbook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_c$Math_textbook_LY_e*100, na.rm = T)

reg_Math_textbook_LY_e_baseline <- lm_robust(Math_textbook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline, 
                                             clusters = ID_IE, se_type = "stata")

#notebook last year

mean(student_test_2_baseline_t$Math_Notebook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_c$Math_Notebook_LY_e*100, na.rm = T)

Diff_Math_Notebook_LY_e_baseline <- 
  mean(student_test_2_baseline_t$Math_Notebook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_c$Math_Notebook_LY_e*100, na.rm = T)

reg_Math_Notebook_LY_e_baseline <- lm_robust(Math_Notebook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline, 
                                             clusters = ID_IE, se_type = "stata")

#student own desk at home

mean(student_test_2_baseline_t$Student_own_desk_home*100, na.rm = T)
mean(student_test_2_baseline_c$Student_own_desk_home*100, na.rm = T)

Diff_Student_own_desk_home_baseline <- 
  mean(student_test_2_baseline_t$Student_own_desk_home*100, na.rm = T)-
  mean(student_test_2_baseline_c$Student_own_desk_home*100, na.rm = T)

reg_Student_own_desk_home_baseline <- lm_robust(Student_own_desk_home~
                                                  treatment_2+
                                                  department*urban.rural, 
                                                
                                                data=student_test_2_baseline, 
                                                clusters = ID_IE, se_type = "stata")

#smartphone

mean(student_test_2_baseline_t$Item_House__1*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_2_baseline_t$Item_House__1*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_2_baseline_t$Item_House__2*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_2_baseline_t$Item_House__2*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_2_baseline_t$Item_House__3*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_2_baseline_t$Item_House__3*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_2_baseline_t$Item_House__4*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_2_baseline_t$Item_House__4*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_2_baseline_t$Item_House__5*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_2_baseline_t$Item_House__5*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_2_baseline_t$Item_House__6*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_2_baseline_t$Item_House__6*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_2_baseline_t$Item_House__7*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_2_baseline_t$Item_House__7*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_2_baseline_t$Item_House__8*100, na.rm = T)
mean(student_test_2_baseline_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_2_baseline_t$Item_House__8*100, na.rm = T)-
  mean(student_test_2_baseline_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline, 
                               clusters = ID_IE, se_type = "stata")
#use fire for cooking

mean(student_test_2_baseline_t$Cooking_Equipment__1*100, na.rm = T)
mean(student_test_2_baseline_c$Cooking_Equipment__1*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_2_baseline_t$Cooking_Equipment__1*100, na.rm = T)-
  mean(student_test_2_baseline_c$Cooking_Equipment__1*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline, 
                                      clusters = ID_IE, se_type = "stata")

#balance of student household economic index

###compute composite index of household economic status

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_2_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_2_p <- cbind(student_test_2_baseline$Item_House__1,
                          student_test_2_baseline$Item_House__2,
                          student_test_2_baseline$Item_House__3,
                          student_test_2_baseline$Item_House__4,
                          student_test_2_baseline$Item_House__5,
                          student_test_2_baseline$Item_House__6,
                          student_test_2_baseline$Item_House__7,
                          student_test_2_baseline$Item_House__8,
                          student_test_2_baseline$Cooking_Equipment__1_r)

row.names(student_test_2_p) <- c(student_test_2_baseline$ID_IE_test)

student_test_2_p <- na.omit(student_test_2_p)

result <- prcomp(student_test_2_p, scale=T, na.rm=T, center=T)

summary(result)

student_test_2_p <- data.frame(student_test_2_p)

result2 <- round(result$rotation, 3)
result3 <- round(result$x, digits=3)

pc1 <- result$x[,1] # first principal component
pc2 <- result$x[,2] # second principal component
pc3 <- result$x[,3] # third principal component

student_id <- rownames(student_test_2_p)

student_test_2_p <- cbind(student_id, student_test_2_p, pc1, pc2, pc3)

student_test_2_p <- subset(student_test_2_p,
                           select=c("student_id",
                                    "pc1"))

student_test_2_baseline <- merge(student_test_2_baseline, student_test_2_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("student_id"), all=F)

write.csv(student_test_2_p,
          "student_test_2_p.csv",
          row.names = F)

#check balance of student household economic index

student_test_2_baseline_t <- subset(student_test_2_baseline,
                                    treatment_2==1)

student_test_2_baseline_c <- subset(student_test_2_baseline,
                                    treatment_2==0)

mean(student_test_2_baseline_t$pc1, na.rm = T)
mean(student_test_2_baseline_c$pc1, na.rm = T)

Diff_pc1 <- 
  mean(student_test_2_baseline_t$pc1, na.rm = T)-
  mean(student_test_2_baseline_c$pc1, na.rm = T)

reg_pc1 <- lm_robust(pc1~
                       treatment_2+
                       department*urban.rural, 
                     
                     data=student_test_2_baseline, 
                     clusters = ID_IE, se_type = "stata")

###Table A-2 in Appendix 2####

rm (list = ls(all=TRUE))

student_test_2_baseline_original <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_baseline_remaining <- subset(student_test_2_baseline_original,
                                            !(student_test_2_baseline_original$ID_IE==430323|
                                                student_test_2_baseline_original$ID_IE==430336|
                                                student_test_2_baseline_original$ID_IE==426534))

student_test_2_baseline_remaining_c <- subset(
  student_test_2_baseline_remaining,
  treatment_2==0)

student_test_2_baseline_remaining$treatment_2

student_test_2_baseline_original$original <- c(1)
student_test_2_baseline_remaining$original <- c(0)

student_test_2_baseline_check <- rbind.data.frame(student_test_2_baseline_original,
                                                  student_test_2_baseline_remaining)

#shift

mean(student_test_2_baseline_original$Shift*100, na.rm = T)
mean(student_test_2_baseline_remaining$Shift*100, na.rm = T)

Diff_shift <- mean(student_test_2_baseline_original$Shift*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Shift*100, na.rm = T)

reg_Shift_baseline <- lm_robust(Shift~
                                  original+
                                  department*urban.rural, 
                                
                                data=student_test_2_baseline_check, 
                                clusters = ID_IE, se_type = "stata")

#age

mean(student_test_2_baseline_original$Age_r, na.rm = T)
mean(student_test_2_baseline_remaining$Age_r, na.rm = T)

Diff_age <- mean(student_test_2_baseline_original$Age_r, na.rm = T)-
  mean(student_test_2_baseline_remaining$Age_r, na.rm = T)

reg_Age_baseline <- lm_robust(Age_r~
                                original+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_check, 
                              clusters = ID_IE, se_type = "stata")

#sex

mean(student_test_2_baseline_original$Sex*100, na.rm = T)
mean(student_test_2_baseline_remaining$Sex*100, na.rm = T)

Diff_sex <- mean(student_test_2_baseline_original$Sex*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Sex*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Sex~
                                original+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_check, 
                              clusters = ID_IE, se_type = "stata")

#number of elder siblings

mean(student_test_2_baseline_original$no_elder, na.rm = T)
mean(student_test_2_baseline_remaining$no_elder, na.rm = T)

Diff_no_elder <- mean(student_test_2_baseline_original$no_elder, na.rm = T)-
  mean(student_test_2_baseline_remaining$no_elder, na.rm = T)

reg_no_elder_baseline <- lm_robust(no_elder~
                                     original+
                                     department*urban.rural, 
                                   
                                   data=student_test_2_baseline_check, 
                                   clusters = ID_IE, se_type = "stata")

#number of younger siblings

mean(student_test_2_baseline_original$no_younger, na.rm = T)
mean(student_test_2_baseline_remaining$no_younger, na.rm = T)

Diff_no_younger <- mean(student_test_2_baseline_original$no_younger, na.rm = T)-
  mean(student_test_2_baseline_remaining$no_younger, na.rm = T)

reg_no_younger_baseline <- lm_robust(no_younger~
                                       original+
                                       department*urban.rural, 
                                     
                                     data=student_test_2_baseline_check, 
                                     clusters = ID_IE, se_type = "stata")

#test scores (20 items)

mean(student_test_2_baseline_original$total_score_2, na.rm = T)
mean(student_test_2_baseline_remaining$total_score_2, na.rm = T)

Diff_total_score_2 <- mean(student_test_2_baseline_original$total_score_2, na.rm = T)-
  mean(student_test_2_baseline_remaining$total_score_2, na.rm = T)

reg_total_score_baseline <- lm_robust(total_score_2~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_check, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (18 items)

mean(student_test_2_baseline_original$total_score_baseline_exc2_4, na.rm = T)
mean(student_test_2_baseline_remaining$total_score_baseline_exc2_4, na.rm = T)

Diff_total_score_baseline_exc2_4 <- 
  mean(student_test_2_baseline_original$total_score_baseline_exc2_4, na.rm = T)-
  mean(student_test_2_baseline_remaining$total_score_baseline_exc2_4, na.rm = T)

reg_total_score_baseline_exc2_4 <- lm_robust(total_score_baseline_exc2_4~
                                               original+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_check, 
                                             clusters = ID_IE, se_type = "stata")

#textbook last year

mean(student_test_2_baseline_original$Math_textbook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining$Math_textbook_LY_e*100, na.rm = T)

Diff_Math_textbook_LY_e_baseline <- 
  mean(student_test_2_baseline_original$Math_textbook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Math_textbook_LY_e*100, na.rm = T)

reg_Math_textbook_LY_e_baseline <- lm_robust(Math_textbook_LY_e~
                                               original+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_check, 
                                             clusters = ID_IE, se_type = "stata")

#notebook last year

mean(student_test_2_baseline_original$Math_Notebook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining$Math_Notebook_LY_e*100, na.rm = T)

Diff_Math_Notebook_LY_e_baseline <- 
  mean(student_test_2_baseline_original$Math_Notebook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Math_Notebook_LY_e*100, na.rm = T)

reg_Math_Notebook_LY_e_baseline <- lm_robust(Math_Notebook_LY_e~
                                               original+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_check, 
                                             clusters = ID_IE, se_type = "stata")

#student own desk at home

mean(student_test_2_baseline_original$Student_own_desk_home*100, na.rm = T)
mean(student_test_2_baseline_remaining$Student_own_desk_home*100, na.rm = T)

Diff_Student_own_desk_home_baseline <- 
  mean(student_test_2_baseline_original$Student_own_desk_home*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Student_own_desk_home*100, na.rm = T)

reg_Student_own_desk_home_baseline <- lm_robust(Student_own_desk_home~
                                                  original+
                                                  department*urban.rural, 
                                                
                                                data=student_test_2_baseline_check, 
                                                clusters = ID_IE, se_type = "stata")

#smartphone

mean(student_test_2_baseline_original$Item_House__1*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_2_baseline_original$Item_House__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_2_baseline_original$Item_House__2*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_2_baseline_original$Item_House__2*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_2_baseline_original$Item_House__3*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_2_baseline_original$Item_House__3*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_2_baseline_original$Item_House__4*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_2_baseline_original$Item_House__4*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_2_baseline_original$Item_House__5*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_2_baseline_original$Item_House__5*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_2_baseline_original$Item_House__6*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_2_baseline_original$Item_House__6*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_2_baseline_original$Item_House__7*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_2_baseline_original$Item_House__7*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_2_baseline_original$Item_House__8*100, na.rm = T)
mean(student_test_2_baseline_remaining$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_2_baseline_original$Item_House__8*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 original+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_check, 
                               clusters = ID_IE, se_type = "stata")
#use fire for cooking

mean(student_test_2_baseline_original$Cooking_Equipment__1*100, na.rm = T)
mean(student_test_2_baseline_remaining$Cooking_Equipment__1*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_2_baseline_original$Cooking_Equipment__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining$Cooking_Equipment__1*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1~
                                        original+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_check, 
                                      clusters = ID_IE, se_type = "stata")

###Table D-1 in Appendix 5#### 

#prepare dataframe for checking attrition at the endline

rm (list = ls(all=TRUE))

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_end <- 
  read.csv("student_test_2_endline.csv", header=T, stringsAsFactors = F)

student_test_2_remaining_e <- subset(student_test_2_end,
                                     select=c("ID_IE_test"))

student_test_2_remaining_e$attrition_2018 <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test"))

student_test_2_remaining_e_r <- 
  dplyr::left_join(student_test_2_baseline_id, 
                   student_test_2_remaining_e,
                   by=c("ID_IE_test"))

student_test_2_remaining_e_r$attrition_2018[
  is.na(student_test_2_remaining_e_r$attrition_2018)] <- 1

student_test_2_baseline <- merge(student_test_2_remaining_e_r, student_test_2_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

#Exclude a student that do not have shift information.

student_test_2_baseline <- subset(student_test_2_baseline,
                                  !(ID_IE_test==3147211))

#Exclude a student that do not have information surveyd in interview.

student_test_2_baseline <- subset(student_test_2_baseline,
                                  !(ID_IE_test==3135221))

#Column: End-line, OLS

reg_d1_end_ols <- lm_robust(attrition_2018~
                              
                              treatment_2+
                              ztest_score_baseline_exc_2_4+
                              department*urban.rural+
                              
                              Age_r+
                              Sex+
                              no_elder+
                              no_younger+
                              total_asset_num_baseline+
                              Shift,
                            
                            data=student_test_2_baseline, 
                            clusters = ID_IE, se_type = "stata")

summary(reg_d1_end_ols)


#Column: Endline, logit

formula_logit <-  attrition_2018 ~ 
  treatment_2+
  ztest_score_baseline_exc_2_4+
  department*urban.rural+
  
  Age_r+
  Sex+
  no_elder+
  no_younger+
  total_asset_num_baseline+
  Shift

reg_d1_end_logit <- logitmfx(formula_logit, data=student_test_2_baseline, 
                             atmean = TRUE, robust = TRUE, clustervar1 = "ID_IE", 
                             clustervar2 = NULL, start = NULL, control = list())

extract(reg_d1_end_logit)

#prepare dataframe for checking attrition at the follow-up

rm (list = ls(all=TRUE))

student_test_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

student_test_2_baseline <- read.csv("student_test_2_baseline.csv", 
                                    header=T, stringsAsFactors = F)

student_test_2_remaining_f <- subset(student_test_fus,
                                     select=c("ID_IE_test"))

student_test_2_remaining_f$attrition_2019 <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test"))

student_test_2_remaining_f_r <- 
  dplyr::left_join(student_test_2_baseline_id, 
                   student_test_2_remaining_f,
                   by=c("ID_IE_test"))

student_test_2_remaining_f_r$attrition_2019[
  is.na(student_test_2_remaining_f_r$attrition_2019)] <- 1

student_test_2_baseline <- merge(student_test_2_remaining_f_r, 
                                 student_test_2_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

#Exclude a student that do not have shift information.

student_test_2_baseline <- subset(student_test_2_baseline,
                                  !(ID_IE_test==3147211))

#Exclude a student that do not have information surveyd in interview.

student_test_2_baseline <- subset(student_test_2_baseline,
                                  !(ID_IE_test==3135221))

#Column: Follow-up, OLS
reg_d1_fus_ols <- lm_robust(attrition_2019~
                              treatment_2+
                              ztest_score_baseline_exc_2_4+
                              department*urban.rural+
                              
                              Age_r+
                              Sex+
                              no_elder+
                              no_younger+
                              total_asset_num_baseline+
                              Shift, 
                            
                            data=student_test_2_baseline, 
                            clusters = ID_IE, se_type = "stata")

summary(reg_d1_fus_ols)

#Column: follow-up, logit

formula_logit <-  attrition_2019 ~ 
  treatment_2+
  ztest_score_baseline_exc_2_4+
  department*urban.rural+
  
  Age_r+
  Sex+
  no_elder+
  no_younger+
  total_asset_num_baseline+
  Shift

reg_d1_fus_logit <- logitmfx(formula_logit, data=student_test_2_baseline, 
                             atmean = TRUE, robust = TRUE, clustervar1 = "ID_IE", 
                             clustervar2 = NULL, start = NULL, control = list())

extract(reg_d1_fus_logit)

###Table D-2 in Appendix 5####

rm (list = ls(all=TRUE))

#prepare dataframe for checking balance of samples remaining at the endline

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_end <- 
  read.csv("student_test_2_endline.csv", header=T, stringsAsFactors = F)

student_test_2_remaining_e <- subset(student_test_2_end,
                                     select=c("ID_IE_test"))

student_test_2_remaining_e$attrition_2018 <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test"))

student_test_2_remaining_e_r <- 
  dplyr::left_join(student_test_2_baseline_id, 
                   student_test_2_remaining_e,
                   by=c("ID_IE_test"))

student_test_2_remaining_e_r$attrition_2018[
  is.na(student_test_2_remaining_e_r$attrition_2018)] <- 1

student_test_2_baseline <- merge(student_test_2_remaining_e_r, student_test_2_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_2_baseline_remaining_end <- subset(student_test_2_baseline,
                                                attrition_2018==0)

student_test_2_baseline_remaining_end_t <-
  subset(student_test_2_baseline_remaining_end,
         treatment_2==1)

student_test_2_baseline_remaining_end_t <-
  subset(student_test_2_baseline_remaining_end_t,
         is.na(student_test_2_baseline_remaining_end_t$Item_House__1)==FALSE)

student_test_2_baseline_remaining_end_c <-
  subset(student_test_2_baseline_remaining_end,
         treatment_2==0)

#shift

mean(student_test_2_baseline_remaining_end_t$Shift*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Shift*100, na.rm = T)

Diff_shift <- mean(student_test_2_baseline_remaining_end_t$Shift*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Shift*100, na.rm = T)

reg_Shift_baseline <- lm_robust(Shift~
                                  treatment_2+
                                  department*urban.rural, 
                                
                                data=student_test_2_baseline_remaining_end, 
                                clusters = ID_IE, se_type = "stata")

#age

mean(student_test_2_baseline_remaining_end_t$Age_r, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Age_r, na.rm = T)

Diff_age <- mean(student_test_2_baseline_remaining_end_t$Age_r, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Age_r, na.rm = T)

reg_Age_baseline <- lm_robust(Age_r~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_remaining_end, 
                              clusters = ID_IE, se_type = "stata")

#sex

mean(student_test_2_baseline_remaining_end_t$Sex*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Sex*100, na.rm = T)

Diff_sex <- mean(student_test_2_baseline_remaining_end_t$Sex*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Sex*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Sex~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_remaining_end, 
                              clusters = ID_IE, se_type = "stata")

#N. of elder siblings

mean(student_test_2_baseline_remaining_end_t$no_elder, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$no_elder, na.rm = T)

Diff_no_elder <- mean(student_test_2_baseline_remaining_end_t$no_elder, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$no_elder, na.rm = T)

reg_no_elder_baseline <- lm_robust(no_elder~
                                     treatment_2+
                                     department*urban.rural, 
                                   
                                   data=student_test_2_baseline_remaining_end, 
                                   clusters = ID_IE, se_type = "stata")

#N. of younger siblings

mean(student_test_2_baseline_remaining_end_t$no_younger, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$no_younger, na.rm = T)

Diff_no_younger <- mean(student_test_2_baseline_remaining_end_t$no_younger, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$no_younger, na.rm = T)

reg_no_younger_baseline <- lm_robust(no_younger~
                                       treatment_2+
                                       department*urban.rural, 
                                     
                                     data=student_test_2_baseline_remaining_end, 
                                     clusters = ID_IE, se_type = "stata")

#test scores (20 items)

mean(student_test_2_baseline_remaining_end_t$total_score_2, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$total_score_2, na.rm = T)

sd(student_test_2_baseline_remaining_end_t$total_score_2, na.rm = T)
sd(student_test_2_baseline_remaining_end_c$total_score_2, na.rm = T)

Diff_total_score_2 <- mean(student_test_2_baseline_remaining_end_t$total_score_2, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$total_score_2, na.rm = T)

reg_total_score_baseline <- lm_robust(total_score_2~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_end, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (z scores of 20 items)

mean(student_test_2_baseline_remaining_end_t$ztest_score_baseline, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$ztest_score_baseline, na.rm = T)

Diff_ztest_score_baseline <- mean(student_test_2_baseline_remaining_end_t$ztest_score_baseline, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$ztest_score_baseline, na.rm = T)

reg_total_score_baseline <- lm_robust(ztest_score_baseline~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_end, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (18 items)

mean(student_test_2_baseline_remaining_end_t$total_score_baseline_exc2_4, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$total_score_baseline_exc2_4, na.rm = T)

sd(student_test_2_baseline_remaining_end_t$total_score_baseline_exc2_4, na.rm = T)
sd(student_test_2_baseline_remaining_end_c$total_score_baseline_exc2_4, na.rm = T)

Diff_total_score_baseline_exc2_4 <- 
  mean(student_test_2_baseline_remaining_end_t$total_score_baseline_exc2_4, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$total_score_baseline_exc2_4, na.rm = T)

reg_total_score_baseline_exc2_4 <- lm_robust(total_score_baseline_exc2_4~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_end, 
                                             clusters = ID_IE, se_type = "stata")

#test scores (z scores of 18 items)

mean(student_test_2_baseline_remaining_end_t$ztest_score_baseline_exc_2_4, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$ztest_score_baseline_exc_2_4, na.rm = T)

Diff_ztest_score_baseline_exc_2_4 <- 
  mean(student_test_2_baseline_remaining_end_t$ztest_score_baseline_exc_2_4, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$ztest_score_baseline_exc_2_4, na.rm = T)

reg1_exc2_4 <- lm_robust(ztest_score_baseline_exc_2_4~
                           treatment_2+
                           department*urban.rural, 
                         data=student_test_2_baseline_remaining_end, 
                         clusters = ID_IE, se_type = "stata")

#textbook last year

mean(student_test_2_baseline_remaining_end_t$Math_textbook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Math_textbook_LY_e*100, na.rm = T)

Diff_Math_textbook_LY_e_baseline <- 
  mean(student_test_2_baseline_remaining_end_t$Math_textbook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Math_textbook_LY_e*100, na.rm = T)

reg_Math_textbook_LY_e_baseline <- lm_robust(Math_textbook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_end, 
                                             clusters = ID_IE, se_type = "stata")

#notebook last year

mean(student_test_2_baseline_remaining_end_t$Math_Notebook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Math_Notebook_LY_e*100, na.rm = T)

Diff_Math_Notebook_LY_e_baseline <- 
  mean(student_test_2_baseline_remaining_end_t$Math_Notebook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Math_Notebook_LY_e*100, na.rm = T)

reg_Math_Notebook_LY_e_baseline <- lm_robust(Math_Notebook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_end, 
                                             clusters = ID_IE, se_type = "stata")

#student own desk at home

mean(student_test_2_baseline_remaining_end_t$Student_own_desk_home*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Student_own_desk_home*100, na.rm = T)

Diff_Student_own_desk_home_baseline <- 
  mean(student_test_2_baseline_remaining_end_t$Student_own_desk_home*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Student_own_desk_home*100, na.rm = T)

reg_Student_own_desk_home_baseline <- lm_robust(Student_own_desk_home~
                                                  treatment_2+
                                                  department*urban.rural, 
                                                
                                                data=student_test_2_baseline_remaining_end, 
                                                clusters = ID_IE, se_type = "stata")

#smartphone

mean(student_test_2_baseline_remaining_end_t$Item_House__1*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_2_baseline_remaining_end_t$Item_House__2*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__2*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_2_baseline_remaining_end_t$Item_House__3*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__3*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_2_baseline_remaining_end_t$Item_House__4*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__4*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_2_baseline_remaining_end_t$Item_House__5*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__5*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_2_baseline_remaining_end_t$Item_House__6*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__6*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_2_baseline_remaining_end_t$Item_House__7*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__7*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_2_baseline_remaining_end_t$Item_House__8*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_2_baseline_remaining_end_t$Item_House__8*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_end, 
                               clusters = ID_IE, se_type = "stata")
#use fire for cooking

mean(student_test_2_baseline_remaining_end_t$Cooking_Equipment__1*100, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$Cooking_Equipment__1*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_2_baseline_remaining_end_t$Cooking_Equipment__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$Cooking_Equipment__1*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_end, 
                                      clusters = ID_IE, se_type = "stata")

#student household economic index
#implement following code after computing household economic index
#refer to the section of Table 3 in this file

student_test_2_p <- 
  read.csv("student_test_2_p.csv", header=T, stringsAsFactors = F)

student_test_2_baseline_remaining_end <- merge(student_test_2_baseline_remaining_end, 
                                               student_test_2_p,
                                               by.x=c("ID_IE_test"), 
                                               by.y=c("student_id"), all=F)

student_test_2_baseline_remaining_end_t <-
  subset(student_test_2_baseline_remaining_end,
         treatment_2==1)

student_test_2_baseline_remaining_end_c <-
  subset(student_test_2_baseline_remaining_end,
         treatment_2==0)

mean(student_test_2_baseline_remaining_end_t$pc1, na.rm = T)
mean(student_test_2_baseline_remaining_end_c$pc1, na.rm = T)

Diff_pc1 <- 
  mean(student_test_2_baseline_remaining_end_t$pc1, na.rm = T)-
  mean(student_test_2_baseline_remaining_end_c$pc1, na.rm = T)

reg_pc1 <- lm_robust(pc1~
                       treatment_2+
                       department*urban.rural, 
                     
                     data=student_test_2_baseline_remaining_end, 
                     clusters = ID_IE, se_type = "stata")

###Table D-3 in Appendix 5####

#prepare dataframe for checking balance of samples remaining at the follow-up

rm (list = ls(all=TRUE))

student_test_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

student_test_2_baseline <- read.csv("student_test_2_baseline.csv", 
                                    header=T, stringsAsFactors = F)

student_test_2_remaining_f <- subset(student_test_fus,
                                     select=c("ID_IE_test"))

student_test_2_remaining_f$attrition_2019 <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test"))

student_test_2_remaining_f_r <- 
  dplyr::left_join(student_test_2_baseline_id, 
                   student_test_2_remaining_f,
                   by=c("ID_IE_test"))

student_test_2_remaining_f_r$attrition_2019[
  is.na(student_test_2_remaining_f_r$attrition_2019)] <- 1

student_test_2_baseline <- merge(student_test_2_remaining_f_r, 
                                 student_test_2_baseline,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("ID_IE_test"), all=F)

student_test_2_baseline_remaining_fus <- subset(student_test_2_baseline,
                                                attrition_2019==0)

student_test_2_baseline_remaining_fus_t <-
  subset(student_test_2_baseline_remaining_fus,
         treatment_2==1)

student_test_2_baseline_remaining_fus_c <-
  subset(student_test_2_baseline_remaining_fus,
         treatment_2==0)

#shift

mean(student_test_2_baseline_remaining_fus_t$Shift*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Shift*100, na.rm = T)

Diff_shift <- mean(student_test_2_baseline_remaining_fus_t$Shift*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Shift*100, na.rm = T)

reg_Shift_baseline <- lm_robust(Shift~
                                  treatment_2+
                                  department*urban.rural, 
                                
                                data=student_test_2_baseline_remaining_fus, 
                                clusters = ID_IE, se_type = "stata")

#age

mean(student_test_2_baseline_remaining_fus_t$Age_r, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Age_r, na.rm = T)

Diff_age <- mean(student_test_2_baseline_remaining_fus_t$Age_r, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Age_r, na.rm = T)

reg_Age_baseline <- lm_robust(Age_r~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_remaining_fus, 
                              clusters = ID_IE, se_type = "stata")

#sex

mean(student_test_2_baseline_remaining_fus_t$Sex*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Sex*100, na.rm = T)

Diff_sex <- mean(student_test_2_baseline_remaining_fus_t$Sex*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Sex*100, na.rm = T)

reg_Sex_baseline <- lm_robust(Sex~
                                treatment_2+
                                department*urban.rural, 
                              
                              data=student_test_2_baseline_remaining_fus, 
                              clusters = ID_IE, se_type = "stata")

#N. of elder siblings

mean(student_test_2_baseline_remaining_fus_t$no_elder, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$no_elder, na.rm = T)

Diff_no_elder <- mean(student_test_2_baseline_remaining_fus_t$no_elder, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$no_elder, na.rm = T)

reg_no_elder_baseline <- lm_robust(no_elder~
                                     treatment_2+
                                     department*urban.rural, 
                                   
                                   data=student_test_2_baseline_remaining_fus, 
                                   clusters = ID_IE, se_type = "stata")

#N. of younger siblings

mean(student_test_2_baseline_remaining_fus_t$no_younger, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$no_younger, na.rm = T)

Diff_no_younger <- mean(student_test_2_baseline_remaining_fus_t$no_younger, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$no_younger, na.rm = T)

reg_no_younger_baseline <- lm_robust(no_younger~
                                       treatment_2+
                                       department*urban.rural, 
                                     
                                     data=student_test_2_baseline_remaining_fus, 
                                     clusters = ID_IE, se_type = "stata")

#test scores (20 items)

mean(student_test_2_baseline_remaining_fus_t$total_score_2, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$total_score_2, na.rm = T)

sd(student_test_2_baseline_remaining_fus_t$total_score_2, na.rm = T)
sd(student_test_2_baseline_remaining_fus_c$total_score_2, na.rm = T)

Diff_total_score_2 <- mean(student_test_2_baseline_remaining_fus_t$total_score_2, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$total_score_2, na.rm = T)

reg_total_score_baseline <- lm_robust(total_score_2~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_fus, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (z scores of 20 items)

mean(student_test_2_baseline_remaining_fus_t$ztest_score_baseline, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$ztest_score_baseline, na.rm = T)

Diff_ztest_score_baseline <- mean(student_test_2_baseline_remaining_fus_t$ztest_score_baseline, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$ztest_score_baseline, na.rm = T)

reg_total_score_baseline <- lm_robust(ztest_score_baseline~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_fus, 
                                      clusters = ID_IE, se_type = "stata")

#test scores (18 items)

mean(student_test_2_baseline_remaining_fus_t$total_score_baseline_exc2_4, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$total_score_baseline_exc2_4, na.rm = T)

sd(student_test_2_baseline_remaining_fus_t$total_score_baseline_exc2_4, na.rm = T)
sd(student_test_2_baseline_remaining_fus_c$total_score_baseline_exc2_4, na.rm = T)

Diff_total_score_baseline_exc2_4 <- 
  mean(student_test_2_baseline_remaining_fus_t$total_score_baseline_exc2_4, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$total_score_baseline_exc2_4, na.rm = T)

reg_total_score_baseline_exc2_4 <- lm_robust(total_score_baseline_exc2_4~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_fus, 
                                             clusters = ID_IE, se_type = "stata")

#test scores (z scores of 18 items)

mean(student_test_2_baseline_remaining_fus_t$ztest_score_baseline_exc_2_4, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$ztest_score_baseline_exc_2_4, na.rm = T)

Diff_ztest_score_baseline_exc_2_4 <- 
  mean(student_test_2_baseline_remaining_fus_t$ztest_score_baseline_exc_2_4, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$ztest_score_baseline_exc_2_4, na.rm = T)

reg1_exc2_4 <- lm_robust(ztest_score_baseline_exc_2_4~
                           treatment_2+
                           department*urban.rural, 
                         data=student_test_2_baseline_remaining_fus, 
                         clusters = ID_IE, se_type = "stata")

#textbook last year

mean(student_test_2_baseline_remaining_fus_t$Math_textbook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Math_textbook_LY_e*100, na.rm = T)

Diff_Math_textbook_LY_e_baseline <- 
  mean(student_test_2_baseline_remaining_fus_t$Math_textbook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Math_textbook_LY_e*100, na.rm = T)

reg_Math_textbook_LY_e_baseline <- lm_robust(Math_textbook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_fus, 
                                             clusters = ID_IE, se_type = "stata")

#notebook last year

mean(student_test_2_baseline_remaining_fus_t$Math_Notebook_LY_e*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Math_Notebook_LY_e*100, na.rm = T)

Diff_Math_Notebook_LY_e_baseline <- 
  mean(student_test_2_baseline_remaining_fus_t$Math_Notebook_LY_e*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Math_Notebook_LY_e*100, na.rm = T)

reg_Math_Notebook_LY_e_baseline <- lm_robust(Math_Notebook_LY_e~
                                               treatment_2+
                                               department*urban.rural, 
                                             
                                             data=student_test_2_baseline_remaining_fus, 
                                             clusters = ID_IE, se_type = "stata")

#student own desk at home

mean(student_test_2_baseline_remaining_fus_t$Student_own_desk_home*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Student_own_desk_home*100, na.rm = T)

Diff_Student_own_desk_home_baseline <- 
  mean(student_test_2_baseline_remaining_fus_t$Student_own_desk_home*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Student_own_desk_home*100, na.rm = T)

reg_Student_own_desk_home_baseline <- lm_robust(Student_own_desk_home~
                                                  treatment_2+
                                                  department*urban.rural, 
                                                
                                                data=student_test_2_baseline_remaining_fus, 
                                                clusters = ID_IE, se_type = "stata")

#smartphone

mean(student_test_2_baseline_remaining_fus_t$Item_House__1*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__1*100, na.rm = T)

Diff_Item_House__1 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__1*100, na.rm = T)

reg_Item_House__1 <- lm_robust(Item_House__1~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")
#computer

mean(student_test_2_baseline_remaining_fus_t$Item_House__2*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__2*100, na.rm = T)

Diff_Item_House__2 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__2*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__2*100, na.rm = T)

reg_Item_House__2 <- lm_robust(Item_House__2~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")

#refrigerator

mean(student_test_2_baseline_remaining_fus_t$Item_House__3*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__3*100, na.rm = T)

Diff_Item_House__3 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__3*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__3*100, na.rm = T)

reg_Item_House__3 <- lm_robust(Item_House__3~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")

#car

mean(student_test_2_baseline_remaining_fus_t$Item_House__4*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__4*100, na.rm = T)

Diff_Item_House__4 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__4*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__4*100, na.rm = T)

reg_Item_House__4 <- lm_robust(Item_House__4~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")

#tv

mean(student_test_2_baseline_remaining_fus_t$Item_House__5*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__5*100, na.rm = T)

Diff_Item_House__5 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__5*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__5*100, na.rm = T)

reg_Item_House__5 <- lm_robust(Item_House__5~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")
#tap water

mean(student_test_2_baseline_remaining_fus_t$Item_House__6*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__6*100, na.rm = T)

Diff_Item_House__6 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__6*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__6*100, na.rm = T)

reg_Item_House__6 <- lm_robust(Item_House__6~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")

#electricity

mean(student_test_2_baseline_remaining_fus_t$Item_House__7*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__7*100, na.rm = T)

Diff_Item_House__7 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__7*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__7*100, na.rm = T)

reg_Item_House__7 <- lm_robust(Item_House__7~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")

#frush toilet

mean(student_test_2_baseline_remaining_fus_t$Item_House__8*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Item_House__8*100, na.rm = T)

Diff_Item_House__8 <- 
  mean(student_test_2_baseline_remaining_fus_t$Item_House__8*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Item_House__8*100, na.rm = T)

reg_Item_House__8 <- lm_robust(Item_House__8~
                                 treatment_2+
                                 department*urban.rural, 
                               
                               data=student_test_2_baseline_remaining_fus, 
                               clusters = ID_IE, se_type = "stata")
#use fire for cooking

mean(student_test_2_baseline_remaining_fus_t$Cooking_Equipment__1*100, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$Cooking_Equipment__1*100, na.rm = T)

Diff_Cooking_Equipment__1 <- 
  mean(student_test_2_baseline_remaining_fus_t$Cooking_Equipment__1*100, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$Cooking_Equipment__1*100, na.rm = T)

reg_Cooking_Equipment__1 <- lm_robust(Cooking_Equipment__1~
                                        treatment_2+
                                        department*urban.rural, 
                                      
                                      data=student_test_2_baseline_remaining_fus, 
                                      clusters = ID_IE, se_type = "stata")

#student household economic index
#implement following code after computing household economic index
#refer to the section of Table 3 in this file

student_test_2_p <- 
  read.csv("student_test_2_p.csv", header=T, stringsAsFactors = F)

student_test_2_baseline_remaining_fus <- merge(student_test_2_baseline_remaining_fus, 
                                               student_test_2_p,
                                               by.x=c("ID_IE_test"), 
                                               by.y=c("student_id"), all=F)

student_test_2_baseline_remaining_fus_t <-
  subset(student_test_2_baseline_remaining_fus,
         treatment_2==1)

student_test_2_baseline_remaining_fus_c <-
  subset(student_test_2_baseline_remaining_fus,
         treatment_2==0)

mean(student_test_2_baseline_remaining_fus_t$pc1, na.rm = T)
mean(student_test_2_baseline_remaining_fus_c$pc1, na.rm = T)

Diff_pc1 <- 
  mean(student_test_2_baseline_remaining_fus_t$pc1, na.rm = T)-
  mean(student_test_2_baseline_remaining_fus_c$pc1, na.rm = T)

reg_pc1 <- lm_robust(pc1~
                       treatment_2+
                       department*urban.rural, 
                     
                     data=student_test_2_baseline_remaining_fus, 
                     clusters = ID_IE, se_type = "stata")

###Table D-4 in Appendix 5####

rm (list = ls(all=TRUE))

student_test_2_end <- 
  read.csv("student_test_2_endline.csv", header=T, stringsAsFactors = F)

student_test_2_end <- 
  subset(student_test_2_end,
         is.na(student_test_2_end$Item_House__1)==F)

student_test_2_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

#Column I-1

regd4_end_1_1 <- lm_robust(treatment_2~
                             department*urban.rural+
                             ztest_score_baseline+
                             Age_r+Sex+
                             No_elder+
                             No_younger+
                             Shift+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8, 
                           
                           data=student_test_2_end,
                           clusters = ID_IE, se_type = "stata")

summary(regd4_end_1_1)

#Column I-2

regd4_end_1_2 <- lm_robust(treatment_2~
                             department*urban.rural+
                             ztest_score_baseline_exc_2_4+
                             Age_r+Sex+
                             No_elder+
                             No_younger+
                             Shift+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8, 
                           
                           data=student_test_2_end,
                           clusters = ID_IE, se_type = "stata")

summary(regd4_end_1_2)

#Column II-1

regd4_fus_2_1 <- lm_robust(treatment_2~
                             department*urban.rural+
                             ztest_score_baseline+
                             Age_r+Sex+
                             No_elder+
                             No_younger+
                             Shift_2018+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8, 
                           
                           data=student_test_2_fus,
                           clusters = ID_IE, se_type = "stata")

summary(regd4_fus_2_1)

#Column II-2

regd4_fus_2_2 <- lm_robust(treatment_2~
                             department*urban.rural+
                             ztest_score_baseline_exc_2_4+
                             Age_r+Sex+
                             No_elder+
                             No_younger+
                             Shift_2018+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8, 
                           
                           data=student_test_2_fus,
                           clusters = ID_IE, se_type = "stata")

summary(regd4_fus_2_2)

###Prepare dataframe to estimate impacts for students (Table 5, 6, 7, 8, 9, E-1, Figure 7)#####

rm (list = ls(all=TRUE))

###store the csv files in your working directory.
###read csv files

student_test_2 <- 
  read.csv("student_test_2_endline.csv", header=T, stringsAsFactors = F)

student_test_2_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

student_test_2_pc_data <- 
  read.csv("student_test_2_p.csv", header=T, stringsAsFactors = F)

###merge data frames

student_test_2 <- merge(student_test_2, student_test_2_pc_data,
                        by.x=c("ID_IE_test"), 
                        by.y=c("student_id"), all=F)

student_test_2_fus <- merge(student_test_2_fus, student_test_2_pc_data,
                            by.x=c("ID_IE_test"), 
                            by.y=c("student_id"), all=F)

#extracting student test data from the original data frame

test_data_end <- cbind.data.frame(student_test_2$Q1_Correct_or_Wrong_G2,
                                  student_test_2$Q2_Correct_or_Wrong_G2,
                                  student_test_2$Q3_Correct_or_Wrong_G2,
                                  student_test_2$Q4_Correct_or_Wrong_G2,
                                  student_test_2$Q5_Correct_or_Wrong_G2,
                                  student_test_2$Q6_Correct_or_Wrong_G2,
                                  student_test_2$Q7_Correct_or_Wrong_G2,
                                  student_test_2$Q8_Correct_or_Wrong_G2,
                                  student_test_2$Q9_Correct_or_Wrong_G2,
                                  student_test_2$Q10_Correct_or_Wrong_G2,
                                  student_test_2$Q11_Correct_or_Wrong_G2,
                                  student_test_2$Q12_Correct_or_Wrong_G2,
                                  student_test_2$Q13_Correct_or_Wrong_G2,
                                  student_test_2$Q14_Correct_or_Wrong_G2,
                                  student_test_2$Q15_Correct_or_Wrong_G2,
                                  student_test_2$Q16_Correct_or_Wrong_G2,
                                  student_test_2$Q17_Correct_or_Wrong_G2,
                                  student_test_2$Q18_Correct_or_Wrong_G2,
                                  student_test_2$Q19_Correct_or_Wrong_G2,
                                  student_test_2$Q20_Correct_or_Wrong_G2)

test_data_fus <- cbind.data.frame(student_test_2_fus$Q1_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q2_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q3_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q4_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q5_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q6_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q7_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q8_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q9_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q10_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q11_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q12_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q13_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q14_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q15_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q16_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q17_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q18_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q19_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q20_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q21_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q22_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q23_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q24_Correct_or_Wrong_G2,
                                  student_test_2_fus$Q25_Correct_or_Wrong_G2)

###read csv file of the parameters for IRT estimates

item_analysis_el_salvador <- 
  read.csv("item_analysis_el_salvador.csv", header=T, stringsAsFactors = F)

###estimate IRT scores

para.old <- item_analysis_el_salvador[1:20, 2:3]
para.new <- item_analysis_el_salvador[1:25, 4:5]
para.on <- list(para.old, para.new)

cat.old <- rep(x = 2, times = 20)
cat.new <- rep(x = 2, times = 25)
cat.on <- list(cat.old, cat.new)

pm.old <- as.poly.mod(n = 20)
pm.new <- as.poly.mod(n = 25)
pm.on <- list(pm.old, pm.new)

common.old<-c(9,10,11,13,19)
common.new<-c(1,2,3,4,5)
common.on<-cbind(common.old, common.new)

pars.on <- as.irt.pars(x = para.on, 
                       common = common.on, cat = cat.on, poly.mod = pm.on)

out.on <- plink(x = pars.on, rescale = "MS", symmetric = TRUE)

para.equ <- link.pars(x = out.on)

para.old$c_3 <- rep(0, 20)
para.end <- as.matrix(para.old)

theta.est_end <-mlebme(resp=test_data_end,ip=para.end,method="BM")
theta.est<-mlebme(resp=test_data_fus,ip=para.equ$group2,method="BM")

#standardized the IRT scores by the mean and sd of the IRT scores of 
#the control group at the end-line survey

student_test_2$irt_score_end <- theta.est_end[,1]
student_test_2_fus$irt_score <- theta.est[,1]

student_test_2_c <- subset(student_test_2,
                           treatment_2==0)

student_test_2_fus$irt_score_s <- c((student_test_2_fus$irt_score-
                                       mean(student_test_2_c$irt_score_end))/
                                      sd(student_test_2_c$irt_score_end))

student_test_2$irt_score_end_s <- c((student_test_2$irt_score_end-
                                       mean(student_test_2_c$irt_score_end))/
                                      sd(student_test_2_c$irt_score_end))

#r-binding the dataframes of end-line and follow-up data
#full samples

student_test_2_end_r <- subset(student_test_2,
                               select=c("irt_score_end_s",
                                        "ztest_score_2",
                                        "ztest_score_knowledge_2",
                                        "ztest_score_application_2",
                                        "ztest_score_reasoning_2",
                                        "treatment_2",
                                        "department",
                                        "urban.rural",
                                        "ztest_score_baseline_exc_2_4",
                                        "ztest_score_baseline",
                                        "Age_r",
                                        "Sex",
                                        "Shift",
                                        "No_elder",
                                        "No_younger",
                                        "Student_own_desk_home",
                                        "Math_textbook_LY_e",
                                        "Math_Notebook_LY_e",
                                        "Cooking_non_fire",
                                        "Item_House__1",
                                        "Item_House__2",
                                        "Item_House__3",
                                        "Item_House__4",
                                        "Item_House__5",
                                        "Item_House__6",
                                        "Item_House__7",
                                        "Item_House__8",
                                        "pc1",
                                        "Multigrade_G2",
                                        "Teacher_Sex",
                                        "Teacher_Age",
                                        "Highest_Degree_highschool",
                                        "Highest_Degree_professorate",
                                        "Highest_Degree_other",
                                        "Teachr_Qualification__2",
                                        "Teachr_Qualification__3",
                                        "Teachr_Qualification__6",
                                        "Teachr_Qualification__7",
                                        "Teachr_Apontmt_Post__1",
                                        "Teachr_Apontmt_Post__2",
                                        "Teachr_Apontmt_Post__3",
                                        "Teachr_Apontmt_Post__4",
                                        "Year_Start_teach_r",
                                        "HM_Sex",
                                        "HM_Age",
                                        "Year_HM_ttl",
                                        "Year_HM_this_school",
                                        "HM_Highest_Degree_highschool",
                                        "HM_Highest_Degree_master",
                                        "HM_Highest_Degree_professorate",
                                        "HM_Highest_Degree_other",
                                        "Number_total_Students",
                                        "School_Facility__2",
                                        "School_Facility__3",
                                        "School_Facility__4",
                                        "School_Facility__5",
                                        "School_Facility__6",
                                        "School_Facility__7",
                                        "School_Facility__8",
                                        "Donor_Project__2",
                                        "Donor_Project__3",
                                        "Donor_Project__4",
                                        "Donor_Project__5",
                                        "Donor_Project__6",
                                        "Donor_Project__7",
                                        "Donor_Project__8",
                                        "Donor_Project__9",
                                        "Donor_Project__10",
                                        "Donor_Project__11",
                                        "Donor_Project__12",
                                        "Donor_Project__13",
                                        "Donor_Project__14",
                                        "Donor_Project__15",
                                        "Donor_Project__16",
                                        "Donor_Project__17",
                                        "Donor_Project__18",
                                        "Donor_Project__19",
                                        "Donor_Project__20",
                                        "ID_IE",
                                        "ID_IE_test",
                                        
                                        "Time_Study_Home_G2",
                                        "Family_teach_study",
                                        
                                        "Number_G2_Students",
                                        "Work_Shift_G2_both",
                                        "Teaching_other_subject_G2",
                                        "Internet_Place__3",
                                        "Meal_Provision",
                                        "Supplement_Class",
                                        
                                        "Method_Home_Study__3",
                                        
                                        "read_textbook_e")) 

student_test_2_fus_r <- subset(student_test_2_fus,
                               select=c("irt_score_s",
                                        "ztest_score_2",
                                        "ztest_score_knowledge_2",
                                        "ztest_score_application_2",
                                        "ztest_score_reasoning_2",
                                        "treatment_2",
                                        "department",
                                        "urban.rural",
                                        "ztest_score_baseline_exc_2_4",
                                        "ztest_score_baseline",
                                        "Age_r",
                                        "Sex",
                                        "Shift_2019",
                                        "repeated_2019",
                                        "No_elder",
                                        "No_younger",
                                        "Student_own_desk_home",
                                        "Math_textbook_LY_e",
                                        "Math_Notebook_LY_e",
                                        "Cooking_non_fire",
                                        "Item_House__1",
                                        "Item_House__2",
                                        "Item_House__3",
                                        "Item_House__4",
                                        "Item_House__5",
                                        "Item_House__6",
                                        "Item_House__7",
                                        "Item_House__8",
                                        "pc1",
                                        "Multigrade_G2_2019",
                                        "Multigrade_G3_2019",
                                        "Teacher_Sex",
                                        "Teacher_Age",
                                        "Highest_Degree_highschool",
                                        "Highest_Degree_professorate",
                                        "Highest_Degree_other",
                                        "Teachr_Qualification__2",
                                        "Teachr_Qualification__3",
                                        "Teachr_Qualification__6",
                                        "Teachr_Qualification__7",
                                        "Teachr_Apontmt_Post__1",
                                        "Teachr_Apontmt_Post__2",
                                        "Teachr_Apontmt_Post__3",
                                        "Teachr_Apontmt_Post__4",
                                        "Year_Start_teach_r",
                                        "HM_Sex",
                                        "HM_Age",
                                        "Year_HM_ttl",
                                        "Year_HM_this_school",
                                        "HM_Highest_Degree_highschool",
                                        "HM_Highest_Degree_master",
                                        "HM_Highest_Degree_professorate",
                                        "HM_Highest_Degree_other",
                                        "Number_total_Students_2018",
                                        "School_Facility__2",
                                        "School_Facility__3",
                                        "School_Facility__4",
                                        "School_Facility__5",
                                        "School_Facility__6",
                                        "School_Facility__7",
                                        "School_Facility__8",
                                        "Donor_Project__2",
                                        "Donor_Project__3",
                                        "Donor_Project__4",
                                        "Donor_Project__5",
                                        "Donor_Project__6",
                                        "Donor_Project__7",
                                        "Donor_Project__8",
                                        "Donor_Project__9",
                                        "Donor_Project__10",
                                        "Donor_Project__11",
                                        "Donor_Project__12",
                                        "Donor_Project__13",
                                        "Donor_Project__14",
                                        "Donor_Project__15",
                                        "Donor_Project__16",
                                        "Donor_Project__17",
                                        "Donor_Project__18",
                                        "Donor_Project__19",
                                        "Donor_Project__20",
                                        "ID_IE",
                                        "ID_IE_test",
                                        
                                        "Time_Study_Home_G2",
                                        "Family_teach_study",
                                        
                                        "No_Students_Ttl_G2_b",
                                        "Teaching_other_subject_G2",
                                        
                                        "Math_textbook_e",
                                        
                                        "Method_Home_Study__3",
                                        
                                        "read_textbook_e",
                                        
                                        "Live_w_Parent_father",
                                        "Live_w_Parent_mother",
                                        "Live_w_Parent_none",
                                        
                                        "Shift_2018",
                                        
                                        "Internet_Place__3",
                                        "Meal_Provision",
                                        "Supplement_Class")) 

#Inserting NAs for the variables only used for either end-line or follow-up data analysis

student_test_2_end_r$Math_textbook_e <- c(NA)
student_test_2_end_r$No_Students_Ttl_G2_b <- c(NA)

student_test_2_end_r$Live_w_Parent_father <- c(NA)
student_test_2_end_r$Live_w_Parent_mother <- c(NA)
student_test_2_end_r$Live_w_Parent_none <- c(NA)
student_test_2_end_r$Shift_2018 <- c(NA)

student_test_2_fus_r$Work_Shift_G2_both <- c(NA)
student_test_2_fus_r$Number_G2_Students <- c(NA)

#

names(student_test_2_end_r)[which(names(student_test_2_end_r)==
                                    "irt_score_end_s" ) ] <- "irt_score_rev"

names(student_test_2_fus_r)[which(names(student_test_2_fus_r)==
                                    "irt_score_s" ) ] <- "irt_score_rev"

#

names(student_test_2_fus_r)[which(names(student_test_2_fus_r)==
                                    "Shift_2019" ) ] <- "Shift"

#

names(student_test_2_fus_r)[which(names(student_test_2_fus_r)==
                                    "Number_total_Students_2018" ) ] <- "Number_total_Students"

#

student_test_2_end_r$timing_fus <- c(0)
student_test_2_fus_r$timing_fus <- c(1)

student_test_2_end_r$timing_end <- c(1)
student_test_2_fus_r$timing_end <- c(0)

student_test_2_end_r$repeated_2019 <- c(0)

student_test_2_fus_r$Multigrade_G2_2019 <-
  c(student_test_2_fus_r$repeated_2019*student_test_2_fus_r$Multigrade_G2_2019)

student_test_2_fus_r$Multigrade_G3_2019 <- 
  replace(student_test_2_fus_r$Multigrade_G3_2019, 
          which(student_test_2_fus_r$repeated_2019==1&
                  student_test_2_fus_r$Multigrade_G3_2019==1), 0)

student_test_2_fus_r$Multigrade_G2 <- c(0)

student_test_2_end_r$Multigrade_G2_2019 <- c(0)
student_test_2_end_r$Multigrade_G3_2019 <- c(0)

#

student_test_2_rev <- rbind.data.frame(student_test_2_end_r,
                                       student_test_2_fus_r)

unique(student_test_2_end_r$Shift)

#creating variables for regressions

student_test_2_rev$treatment_2_2018 <- c(
  student_test_2_rev$treatment_2*student_test_2_rev$timing_end)

student_test_2_rev$treatment_2_2019 <- c(
  student_test_2_rev$treatment_2*student_test_2_rev$timing_fus)

student_test_2_rev$zscore_baseline_2018 <- c(
  student_test_2_rev$timing_end*student_test_2_rev$ztest_score_baseline)

student_test_2_rev$zscore_baseline_2019 <- c(
  student_test_2_rev$timing_fus*student_test_2_rev$ztest_score_baseline)

student_test_2_rev$zscore_baseline_2018_2 <- c(
  student_test_2_rev$timing_end*student_test_2_rev$ztest_score_baseline_exc_2_4)

student_test_2_rev$zscore_baseline_2019_2 <- c(
  student_test_2_rev$timing_fus*student_test_2_rev$ztest_score_baseline_exc_2_4)

student_test_2_rev$Method_Home_Study__3[
  is.na(student_test_2_rev$Method_Home_Study__3)] <- 0

###implement the following code for
#balanced samples

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

student_test_2_rev_end_id <- subset(student_test_2_rev_end,
                                    select=c("ID_IE_test"))

student_test_2_rev_fus_id <- subset(student_test_2_rev_fus,
                                    select=c("ID_IE_test"))

student_test_2_rev_id <- merge(student_test_2_rev_end_id, student_test_2_rev_fus_id,
                               by.x=c("ID_IE_test"), 
                               by.y=c("ID_IE_test"), all=F)

student_test_2_rev_end <- merge(student_test_2_rev_id, student_test_2_rev_end,
                                by.x=c("ID_IE_test"), 
                                by.y=c("ID_IE_test"), all=F)

student_test_2_rev_fus <- merge(student_test_2_rev_id, student_test_2_rev_fus,
                                by.x=c("ID_IE_test"), 
                                by.y=c("ID_IE_test"), all=F)

student_test_2_rev_panel <- rbind.data.frame(student_test_2_rev_end,
                                             student_test_2_rev_fus)

nrow(student_test_2_rev_panel)

###Table 5####
#column 1-1

reg5_1_1 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_1)

linearHypothesis(reg5_1_1, "treatment_2_2018 - treatment_2_2019 = 0")

#column 1-2

reg5_1_2 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_2)

linearHypothesis(reg5_1_2, "treatment_2_2018 - treatment_2_2019 = 0")

#column 1-3

reg5_1_3 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_1_3)

linearHypothesis(reg5_1_3, "treatment_2_2018 - treatment_2_2019 = 0")

#column 2-1

reg5_2_1 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural, 
                      
                      data=student_test_2_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_1)

linearHypothesis(reg5_2_1, "treatment_2_2018 - treatment_2_2019 = 0")

#column 2-2

reg5_2_2 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018+
                        zscore_baseline_2019+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_2)

linearHypothesis(reg5_2_2, "treatment_2_2018 - treatment_2_2019 = 0")

#column 2-3

reg5_2_3 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg5_2_3)

unique(student_test_2_rev_panel$repeated_2019)

linearHypothesis(reg5_2_3, "treatment_2_2018 - treatment_2_2019 = 0")

###Prepare dataframe for bound estimate (Kling and Liebman bound, Table 6 and Table E-1)####

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

#merge teacher and school characteristics variables in dataframe

teacher_2_baseline <- read.csv("teacher_2_baseline.csv", 
                               header=T, stringsAsFactors = F)

school_principal_2_baseline <- read.csv("school_principal_2_baseline.csv", 
                                        header=T, stringsAsFactors = F)

teacher_2_baseline <- subset(teacher_2_baseline,
                             select = c(ID_IE,
                                        Teacher_Sex,
                                        Teacher_Age,
                                        Highest_Degree_highschool,
                                        Highest_Degree_professorate,
                                        Highest_Degree_other,
                                        Teachr_Qualification__2,
                                        Teachr_Qualification__3,
                                        Teachr_Qualification__6,
                                        Teachr_Qualification__7,
                                        Teachr_Apontmt_Post__2,
                                        Teachr_Apontmt_Post__3,
                                        Teachr_Apontmt_Post__4,
                                        Year_Start_teach_r,
                                        Work_Shift_G2_both,
                                        Teaching_other_subject_G2,
                                        Multigrade_G2,
                                        Number_G2_Students))

school_principal_2_baseline <- subset(school_principal_2_baseline,
                                      select = c(ID_IE,
                                                 HM_Sex,
                                                 HM_Age,
                                                 Year_HM_ttl,
                                                 Year_HM_this_school,
                                                 HM_Highest_Degree_highschool,
                                                 HM_Highest_Degree_master,
                                                 HM_Highest_Degree_professorate,
                                                 HM_Highest_Degree_other,
                                                 School_Facility__2,
                                                 School_Facility__3,
                                                 School_Facility__4,
                                                 School_Facility__5,
                                                 School_Facility__6,
                                                 School_Facility__7,
                                                 School_Facility__8,
                                                 Internet_Place__3,
                                                 Meal_Provision,
                                                 Supplement_Class,
                                                 Donor_Project__2,
                                                 Donor_Project__3,
                                                 Donor_Project__4,
                                                 Donor_Project__5,
                                                 Donor_Project__6,
                                                 Donor_Project__7,
                                                 Donor_Project__8,
                                                 Donor_Project__9,
                                                 Donor_Project__10,
                                                 Donor_Project__11,
                                                 Donor_Project__12,
                                                 Donor_Project__13,
                                                 Donor_Project__14,
                                                 Donor_Project__15,
                                                 Donor_Project__16,
                                                 Donor_Project__17,
                                                 Donor_Project__18,
                                                 Donor_Project__19,
                                                 Donor_Project__20,
                                                 Number_total_Students))

student_test_2_baseline <- merge(student_test_2_baseline, 
                                 teacher_2_baseline,
                                 by.x=c("ID_IE"), 
                                 by.y=c("ID_IE"), all=F)

student_test_2_baseline <- merge(student_test_2_baseline, 
                                 school_principal_2_baseline,
                                 by.x=c("ID_IE"), 
                                 by.y=c("ID_IE"), all=F)

###prepare dataframe of Full sample
#add attrition dummy to dataframe of endline data

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

student_test_2_rev_end_remaining <- subset(student_test_2_rev_end,
                                           select=c("ID_IE_test"))

student_test_2_rev_end_remaining$attrition_d <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test",
                                              "treatment"))

student_test_2_rev_end_attrition_d <- dplyr::left_join(student_test_2_baseline_id, 
                                                       student_test_2_rev_end_remaining,
                                                       by=c("ID_IE_test"))

student_test_2_rev_end_attrition_d$attrition_d[
  is.na(student_test_2_rev_end_attrition_d$attrition_d)] <- 1

#add attrition dummy to dataframe of follow-up data

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

student_test_2_rev_fus_remaining <- subset(student_test_2_rev_fus,
                                           select=c("ID_IE_test"))

student_test_2_rev_fus_remaining$attrition_d <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test",
                                              "treatment"))

student_test_2_rev_fus_attrition_d <- dplyr::left_join(student_test_2_baseline_id, 
                                                       student_test_2_rev_fus_remaining,
                                                       by=c("ID_IE_test"))

student_test_2_rev_fus_attrition_d$attrition_d[
  is.na(student_test_2_rev_fus_attrition_d$attrition_d)] <- 1

#

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

student_test_2_rev_end_r <- subset(student_test_2_rev_end,
                                   select=c(irt_score_rev,
                                            ztest_score_knowledge_2,
                                            ztest_score_application_2,
                                            ztest_score_reasoning_2,
                                            treatment_2,
                                            treatment_2_2018,
                                            treatment_2_2019,
                                            timing_end,
                                            timing_fus,
                                            ID_IE_test))

student_test_2_rev_fus_r <- subset(student_test_2_rev_fus,
                                   select=c(irt_score_rev,
                                            ztest_score_knowledge_2,
                                            ztest_score_application_2,
                                            ztest_score_reasoning_2,
                                            treatment_2,
                                            treatment_2_2018,
                                            treatment_2_2019,
                                            timing_end,
                                            timing_fus,
                                            ID_IE_test))

student_test_2_rev_end_r <- dplyr::left_join(student_test_2_rev_end_attrition_d, 
                                             student_test_2_rev_end_r,
                                             by=c("ID_IE_test"))

###prepare dataframe of Balanced sample
#balanced samples

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

student_test_2_rev_end_id <- subset(student_test_2_rev_end,
                                    select=c("ID_IE_test"))

student_test_2_rev_fus_id <- subset(student_test_2_rev_fus,
                                    select=c("ID_IE_test"))

student_test_2_rev_id <- merge(student_test_2_rev_end_id, student_test_2_rev_fus_id,
                               by.x=c("ID_IE_test"), 
                               by.y=c("ID_IE_test"), all=F)

student_test_2_rev_end <- merge(student_test_2_rev_id, student_test_2_rev_end,
                                by.x=c("ID_IE_test"), 
                                by.y=c("ID_IE_test"), all=F)

student_test_2_rev_fus <- merge(student_test_2_rev_id, student_test_2_rev_fus,
                                by.x=c("ID_IE_test"), 
                                by.y=c("ID_IE_test"), all=F)

student_test_2_rev_panel <- rbind.data.frame(student_test_2_rev_end,
                                             student_test_2_rev_fus)

#add attrition dummy to dataframe of endline data (balanced sample)

student_test_2_rev_end <- subset(student_test_2_rev_panel,
                                 timing_end==1)

student_test_2_rev_end_remaining <- subset(student_test_2_rev_end,
                                           select=c("ID_IE_test"))

student_test_2_rev_end_remaining$attrition_d <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test",
                                              "treatment"))

student_test_2_rev_end_attrition_d <- dplyr::left_join(student_test_2_baseline_id, 
                                                       student_test_2_rev_end_remaining,
                                                       by=c("ID_IE_test"))

student_test_2_rev_end_attrition_d$attrition_d[
  is.na(student_test_2_rev_end_attrition_d$attrition_d)] <- 1

#add attrition dummy to dataframe of follow-up data (balanced sample)

student_test_2_rev_fus <- subset(student_test_2_rev_panel,
                                 timing_fus==1)

student_test_2_rev_fus_remaining <- subset(student_test_2_rev_fus,
                                           select=c("ID_IE_test"))

student_test_2_rev_fus_remaining$attrition_d <- c(0)

student_test_2_baseline_id <- subset(student_test_2_baseline,
                                     select=c("ID_IE_test",
                                              "treatment"))

student_test_2_rev_fus_attrition_d <- dplyr::left_join(student_test_2_baseline_id, 
                                                       student_test_2_rev_fus_remaining,
                                                       by=c("ID_IE_test"))

student_test_2_rev_fus_attrition_d$attrition_d[
  is.na(student_test_2_rev_fus_attrition_d$attrition_d)] <- 1

#rbind dataframe (balanced sample)

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

student_test_2_rev_end_r <- subset(student_test_2_rev_end,
                                   select=c(irt_score_rev,
                                            treatment_2,
                                            treatment_2_2018,
                                            treatment_2_2019,
                                            timing_end,
                                            timing_fus,
                                            ID_IE_test))

student_test_2_rev_fus_r <- subset(student_test_2_rev_fus,
                                   select=c(irt_score_rev,
                                            treatment_2,
                                            treatment_2_2018,
                                            treatment_2_2019,
                                            timing_end,
                                            timing_fus,
                                            ID_IE_test))

student_test_2_rev_end_r <- dplyr::left_join(student_test_2_rev_end_attrition_d, 
                                             student_test_2_rev_end_r,
                                             by=c("ID_IE_test"))

###Table 6#### 

###Kilng and Liebman bound
#attrition sample in the treatment group 

student_test_2_rev_end_r_ta <- subset(student_test_2_rev_end_r,
                                      treatment=="Treatment"&
                                        attrition_d==1)

student_test_2_rev_end_r_ta$treatment_2_2018 <-c(1)
student_test_2_rev_end_r_ta$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_ta$timing_end <- c(1)
student_test_2_rev_end_r_ta$timing_fus <- c(0)

#non-attrition sample in the treatment group

student_test_2_rev_end_r_tna <- subset(student_test_2_rev_end_r,
                                       treatment=="Treatment"&
                                         attrition_d==0)

student_test_2_rev_end_r_tna$treatment_2_2018 <-c(1)
student_test_2_rev_end_r_tna$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_tna$timing_end <- c(1)
student_test_2_rev_end_r_tna$timing_fus <- c(0)

student_test_2_rev_end_r_ca <- subset(student_test_2_rev_end_r,
                                      treatment=="Control"&
                                        attrition_d==1)

student_test_2_rev_end_r_ca$treatment_2_2018 <-c(0)
student_test_2_rev_end_r_ca$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_ca$timing_end <- c(1)
student_test_2_rev_end_r_ca$timing_fus <- c(0)

#non-attrition sample in control group

student_test_2_rev_end_r_cna <- subset(student_test_2_rev_end_r,
                                       treatment=="Control"&
                                         attrition_d==0)

student_test_2_rev_end_r_cna$treatment_2_2018 <-c(0)
student_test_2_rev_end_r_cna$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_cna$timing_end <- c(1)
student_test_2_rev_end_r_cna$timing_fus <- c(0)

###add attrition dummy to dataframe of follow-up data

student_test_2_rev_fus_r <- dplyr::left_join(student_test_2_rev_fus_attrition_d, 
                                             student_test_2_rev_fus_r,
                                             by=c("ID_IE_test"))

student_test_2_rev_fus_r_ta <- subset(student_test_2_rev_fus_r,
                                      treatment=="Treatment"&
                                        attrition_d==1)

student_test_2_rev_fus_r_tna <- subset(student_test_2_rev_fus_r,
                                       treatment=="Treatment"&
                                         attrition_d==0)

student_test_2_rev_fus_r_ta$treatment_2_2018 <-c(0)
student_test_2_rev_fus_r_ta$treatment_2_2019 <-c(1)
student_test_2_rev_fus_r_ta$timing_end <- c(0)
student_test_2_rev_fus_r_ta$timing_fus <- c(1)

#

student_test_2_rev_fus_r_ca <- subset(student_test_2_rev_fus_r,
                                      treatment=="Control"&
                                        attrition_d==1)

student_test_2_rev_fus_r_cna <- subset(student_test_2_rev_fus_r,
                                       treatment=="Control"&
                                         attrition_d==0)

student_test_2_rev_fus_r_ca$treatment_2_2018 <-c(0)
student_test_2_rev_fus_r_ca$treatment_2_2019 <-c(0)
student_test_2_rev_fus_r_ca$timing_end <- c(0)
student_test_2_rev_fus_r_ca$timing_fus <- c(1)

###Kling and Liebman (2004)

###implement code corresponding to the outcome and bandwidth

###Outcome: IRT scores

###treatment group,end-line

#Column 3-1 IRT scores
#adjust 0.15sd: lower bound, treatment group of end-line data

student_test_2_rev_end_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_tna$irt_score_rev)-
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.15)

#Column 4-1 IRT scores
#adjust 0.10sd: lower bound, treatment group of end-line data

student_test_2_rev_end_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_tna$irt_score_rev)-
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.10)

#Column 5-1 IRT scores
#adjust 0.10sd: higher bound, treatment group of end-line data

student_test_2_rev_end_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_tna$irt_score_rev)+
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.10)

#Column 6-1 IRT scores
#adjust 0.15sd: higher bound, treatment group of end-line data

student_test_2_rev_end_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_tna$irt_score_rev)+
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.15)

###Control group, end-line 
#Column 3-1 IRT scores
#adjust 0.15sd: lower bound, control group of end-line

student_test_2_rev_end_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_cna$irt_score_rev)+
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.15)

#Column 4-1 IRT scores
#adjust 0.10sd: lower bound, control group of end-line

student_test_2_rev_end_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_cna$irt_score_rev)+
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.1)

#Column 5-1 IRT scores
#adjust 0.10sd: higher bound, control group of end-line

student_test_2_rev_end_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_cna$irt_score_rev)-
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.1)

#Column 6-1 IRT scores
#adjust 0.15sd: higher bound, control group of end-line

student_test_2_rev_end_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_end_r_cna$irt_score_rev)-
      sd(student_test_2_rev_end_r_tna$irt_score_rev)*0.15)

###Treatment, follow-up
#Column 3-1 IRT scores
#adjust 0.15sd: lower bound, treatment group of follow-up data

student_test_2_rev_fus_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_tna$irt_score_rev)-
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.15)

#Column 4-1 IRT scores
#adjust 0.1sd: lower bound, treatment group of follow-up data

student_test_2_rev_fus_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_tna$irt_score_rev)-
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.1)

#Column 5-1 IRT scores
#adjust 0.10sd: higher bound, treatment group of follow-up data

student_test_2_rev_fus_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_tna$irt_score_rev)+
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.1)

#Column 6-1 IRT scores
#adjust 0.15sd: higher bound, treatment group of follow-up data

student_test_2_rev_fus_r_ta$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_tna$irt_score_rev)+
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.15)

###Control, follow-up
#Column 3-1 IRT scores
#adjust 0.15sd: lower bound, control group of follow-up data

student_test_2_rev_fus_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_cna$irt_score_rev)+
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.15)

#Column 4-1 IRT scores
#adjust 0.1sd: lower bound, control group of follow-up data

student_test_2_rev_fus_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_cna$irt_score_rev)+
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.1)

#Column 5-1 IRT scores
#adjust 0.10sd: higher bound, control group of follow-up data

student_test_2_rev_fus_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_cna$irt_score_rev)-
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.1)

#Column 6-1 IRT scores
#adjust 0.15sd: higher bound, control group of follow-up data

student_test_2_rev_fus_r_ca$irt_score_rev　<- 
  c(mean(student_test_2_rev_fus_r_cna$irt_score_rev)-
      sd(student_test_2_rev_fus_r_tna$irt_score_rev)*0.15)

###rbind dataframes (end-line and follow-up)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_end_r_ta,
                                         student_test_2_rev_end_r_tna,
                                         student_test_2_rev_end_r_ca,
                                         student_test_2_rev_end_r_cna,
                                         student_test_2_rev_fus_r_ta,
                                         student_test_2_rev_fus_r_tna,
                                         student_test_2_rev_fus_r_ca,
                                         student_test_2_rev_fus_r_cna)

student_test_2_rev_2_end <- subset(student_test_2_rev_2, 
                                   timing_end==1)

student_test_2_rev_2_fus <- subset(student_test_2_rev_2, 
                                   timing_fus==1)

student_test_2_rev_2_end <- merge(student_test_2_rev_2_end, student_test_2_baseline,
                                  by.x=c("ID_IE_test"), 
                                  by.y=c("ID_IE_test"), all=F)

student_test_2_rev_2_fus <- merge(student_test_2_rev_2_fus, student_test_2_baseline,
                                  by.x=c("ID_IE_test"), 
                                  by.y=c("ID_IE_test"), all=F)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_2_end,
                                         student_test_2_rev_2_fus)

#coding variables for regression analysis

student_test_2_rev_2$zscore_baseline_2018_2 <- c(
  student_test_2_rev_2$ztest_score_baseline_exc_2_4*
    student_test_2_rev_2$timing_end)

student_test_2_rev_2$zscore_baseline_2019_2 <- c(
  student_test_2_rev_2$ztest_score_baseline_exc_2_4*
    student_test_2_rev_2$timing_fus)

#run regression

#Column 3-1; 4-1; 5-1; 6-1
#Column 3-2; 4-2; 5-2; 6-2

reg6_3456_12 <- lm_robust(irt_score_rev~
                            treatment_2_2018+
                            treatment_2_2019+
                            timing_fus+
                            department*urban.rural+
                            zscore_baseline_2018_2+
                            zscore_baseline_2019_2+
                            Age_r+
                            Sex+
                            Shift+
                            no_elder+
                            no_younger+
                            Student_own_desk_home+
                            Math_textbook_LY_e+
                            Math_Notebook_LY_e+
                            Cooking_non_fire+
                            Item_House__1+
                            Item_House__2+
                            Item_House__3+
                            Item_House__4+
                            Item_House__5+
                            Item_House__6+
                            Item_House__7+
                            Item_House__8+
                            
                            Multigrade_G2+
                            Number_total_Students+
                            
                            Teacher_Sex+
                            Teacher_Age+
                            Highest_Degree_highschool+
                            Highest_Degree_professorate+
                            Highest_Degree_other+
                            
                            Teachr_Qualification__2+
                            Teachr_Qualification__3+
                            Teachr_Qualification__6+
                            Teachr_Qualification__7+
                            
                            Teachr_Apontmt_Post__2+
                            Teachr_Apontmt_Post__3+
                            Teachr_Apontmt_Post__4+
                            
                            Year_Start_teach_r+
                            
                            HM_Sex+
                            HM_Age+
                            Year_HM_ttl+
                            Year_HM_this_school+
                            
                            HM_Highest_Degree_highschool+
                            HM_Highest_Degree_master+
                            HM_Highest_Degree_professorate+
                            HM_Highest_Degree_other+
                            
                            School_Facility__2+
                            School_Facility__3+School_Facility__4+
                            School_Facility__5+School_Facility__6+
                            School_Facility__7+School_Facility__8+
                            
                            Donor_Project__2+
                            Donor_Project__3+
                            Donor_Project__4+
                            Donor_Project__5+
                            Donor_Project__6+
                            Donor_Project__7+
                            Donor_Project__8+
                            Donor_Project__9+
                            Donor_Project__10+
                            Donor_Project__11+
                            Donor_Project__12+
                            Donor_Project__13+
                            Donor_Project__14+
                            Donor_Project__15+
                            Donor_Project__16+
                            Donor_Project__17+
                            Donor_Project__18+
                            Donor_Project__19+
                            Donor_Project__20, 
                          
                          data=student_test_2_rev_2,
                          clusters = ID_IE, se_type = "stata")

summary(reg6_3456_12)

#Column 4-1 

reg6_4_1 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        no_elder+
                        no_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Multigrade_G2+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_2,
                      clusters = ID_IE, se_type = "stata")

#Column 4-2 

reg6_4_2 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        no_elder+
                        no_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Multigrade_G2+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_2,
                      clusters = ID_IE, se_type = "stata")

#Column 6-1 

reg6_6_1 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        no_elder+
                        no_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Multigrade_G2+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_2,
                      clusters = ID_IE, se_type = "stata")

#Column 6-2 

reg6_6_2 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        no_elder+
                        no_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Multigrade_G2+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_2,
                      clusters = ID_IE, se_type = "stata")

###Lee bound (lower bound)
#Prepare dataframe for estimating lower bound (full sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

student_test_2_rev$timing_end

student_test_2_rev_t_2019 <- subset(student_test_2_rev,
                                    treatment_2_2019==1&
                                      timing_fus==1)

quantile(student_test_2_rev_t_2018$irt_score_rev, 
         seq(0, 1, 0.03180))

quantile(student_test_2_rev_t_2019$irt_score_rev, 
         seq(0, 1, 0.02049))

student_test_2_rev_t_2018_tr <- subset(student_test_2_rev_t_2018,
                                       irt_score_rev<2.325)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2019_tr <- subset(student_test_2_rev_t_2019,
                                       irt_score_rev<3.34)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_t_2019_tr)

nrow(student_test_2_rev_t)

student_test_2_rev_c <- subset(student_test_2_rev,
                               treatment_2==0)

nrow(student_test_2_rev_c)


student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t,
                                         student_test_2_rev_c)

nrow(student_test_2_rev_2)

###Lee bound (higher bound)
#Prepare dataframe for estimating higher bound (full sample)

student_test_2_rev_t_2019 <- subset(student_test_2_rev,
                                    treatment_2_2019==1)

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1)

student_test_2_rev_t_2018_tr1 <- subset(student_test_2_rev_t_2018,
                                        irt_score_rev>(-1.67285))

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018_tr1,
         irt_score_rev==min(student_test_2_rev_t_2018_tr1$irt_score_rev))

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018_tr2,
         ztest_score_baseline_exc_2_4==
           max(student_test_2_rev_t_2018_tr2$ztest_score_baseline_exc_2_4))

nrow(student_test_2_rev_t_2018_tr1)
nrow(student_test_2_rev_t_2018_tr2)

student_test_2_rev_t_2018_tr2$ID_IE_test

student_test_2_rev_t_2018_tr <- subset(student_test_2_rev_t_2018_tr1,
                                       !(ID_IE_test==
                                           student_test_2_rev_t_2018_tr2$ID_IE_test))

nrow(student_test_2_rev_t_2018_tr)

#

student_test_2_rev_t_2019_tr1 <- subset(student_test_2_rev_t_2019,
                                        irt_score_rev>(-0.85))

student_test_2_rev_t_2019_tr2 <- subset(student_test_2_rev_t_2019,
                                        irt_score_rev<=(-0.85))

options(digits=10)

student_test_2_rev_t_2019_tr3 <- subset(student_test_2_rev_t_2019_tr2,
                                        ztest_score_baseline_exc_2_4>
                                          -1.108582806)

student_test_2_rev_t_2019_tr4 <- subset(student_test_2_rev_t_2019_tr2,
                                        ztest_score_baseline_exc_2_4<
                                          -1.1085828067&
                                          ztest_score_baseline_exc_2_4>
                                          -1.1085830000)

nrow(student_test_2_rev_t_2019_tr1)
nrow(student_test_2_rev_t_2019_tr2)
nrow(student_test_2_rev_t_2019_tr3)
nrow(student_test_2_rev_t_2019_tr4)

#draw first random number from random.org
set.seed(92897)

student_test_2_rev_t_2019_tr4 <- student_test_2_rev_t_2019_tr4[
  order(student_test_2_rev_t_2019_tr4$ID_IE_test),]
student_test_2_rev_t_2019_tr4$random <- runif(nrow(student_test_2_rev_t_2019_tr4))

student_test_2_rev_t_2019_tr4 <- student_test_2_rev_t_2019_tr4[
  order(student_test_2_rev_t_2019_tr4$random),]

student_test_2_rev_t_2019_tr4$number <- c(1:6)

student_test_2_rev_t_2019_tr5 <- subset(student_test_2_rev_t_2019_tr4,
                                        number <= 3)

student_test_2_rev_t_2019_tr5 <- 
  student_test_2_rev_t_2019_tr5[, colnames(student_test_2_rev_t_2019_tr5) != "number"]
student_test_2_rev_t_2019_tr5 <- 
  student_test_2_rev_t_2019_tr5[, colnames(student_test_2_rev_t_2019_tr5) != "random"]

student_test_2_rev_t_2019_tr <- rbind.data.frame(student_test_2_rev_t_2019_tr1,
                                                 student_test_2_rev_t_2019_tr3,
                                                 student_test_2_rev_t_2019_tr5)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t_2019_tr_check <- student_test_2_rev_t_2019_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

student_test_2_rev_c <- subset(student_test_2_rev,
                               treatment_2==0)

nrow(student_test_2_rev_c)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_t_2019_tr,
                                         student_test_2_rev_c)

nrow(student_test_2_rev_2)

#run regression

reg6_27_1 <- lm_robust(irt_score_rev~
                         treatment_2_2018+
                         treatment_2_2019+
                         timing_fus+
                         department*urban.rural+
                         zscore_baseline_2018_2+
                         zscore_baseline_2019_2+
                         Age_r+
                         Sex+
                         Shift+
                         No_elder+
                         No_younger+
                         Student_own_desk_home+
                         Math_textbook_LY_e+
                         Math_Notebook_LY_e+
                         Cooking_non_fire+
                         Item_House__1+
                         Item_House__2+
                         Item_House__3+
                         Item_House__4+
                         Item_House__5+
                         Item_House__6+
                         Item_House__7+
                         Item_House__8+
                         repeated_2019+
                         
                         Multigrade_G2+
                         Multigrade_G2_2019+
                         Multigrade_G3_2019+
                         Number_total_Students+
                         
                         Teacher_Sex+
                         Teacher_Age+
                         Highest_Degree_highschool+
                         Highest_Degree_professorate+
                         Highest_Degree_other+
                         Teachr_Qualification__2+
                         Teachr_Qualification__3+
                         Teachr_Qualification__6+
                         Teachr_Qualification__7+
                         Teachr_Apontmt_Post__2+
                         Teachr_Apontmt_Post__3+
                         Teachr_Apontmt_Post__4+
                         Year_Start_teach_r+
                         
                         HM_Sex+
                         HM_Age+
                         Year_HM_ttl+
                         Year_HM_this_school+
                         
                         HM_Highest_Degree_highschool+
                         HM_Highest_Degree_master+
                         HM_Highest_Degree_professorate+
                         HM_Highest_Degree_other+
                         
                         School_Facility__2+
                         School_Facility__3+School_Facility__4+
                         School_Facility__5+School_Facility__6+
                         School_Facility__7+School_Facility__8+
                         
                         Donor_Project__2+
                         Donor_Project__3+
                         Donor_Project__4+
                         Donor_Project__5+
                         Donor_Project__6+
                         Donor_Project__7+
                         Donor_Project__8+
                         Donor_Project__9+
                         Donor_Project__10+
                         Donor_Project__11+
                         Donor_Project__12+
                         Donor_Project__13+
                         Donor_Project__14+
                         Donor_Project__15+
                         Donor_Project__16+
                         Donor_Project__17+
                         Donor_Project__18+
                         Donor_Project__19+
                         Donor_Project__20, 
                       
                       data=student_test_2_rev_2,
                       clusters = ID_IE, se_type = "stata")

summary(reg6_27_1)

###Lee bound (lower bound)
#Prepare dataframe for estimating lower bound (balanced sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev_panel,
                                    treatment_2_2018==1&
                                      timing_end==1)

nrow(student_test_2_rev_t_2018)

student_test_2_rev_c_2018 <- subset(student_test_2_rev_panel,
                                    treatment_2_2018==0&
                                      timing_end==1)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_t_2019 <- subset(student_test_2_rev_panel,
                                    treatment_2_2019==1&
                                      timing_fus==1)

nrow(student_test_2_rev_t_2019)

student_test_2_rev_c_2019 <- subset(student_test_2_rev_panel,
                                    treatment_2_2019==0&
                                      timing_fus==1)

nrow(student_test_2_rev_c_2019)

quantile(student_test_2_rev_t_2019$irt_score_rev, 
         seq(0, 1, 0.02541))

student_test_2_rev_t_2019_tr <- subset(student_test_2_rev_t_2019,
                                       irt_score_rev<3.31)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t_2019_tr_id <- subset(student_test_2_rev_t_2019_tr,
                                          select=c("ID_IE_test"))

student_test_2_rev_t_2018_tr <- merge(student_test_2_rev_t_2019_tr_id, 
                                      student_test_2_rev_t_2018,
                                      by.x=c("ID_IE_test"), 
                                      by.y=c("ID_IE_test"), all=F)

student_test_2_rev_t <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_t_2019_tr)

nrow(student_test_2_rev_t)

student_test_2_rev_c <- subset(student_test_2_rev_panel,
                               treatment_2==0)

nrow(student_test_2_rev_c)


student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t,
                                         student_test_2_rev_c)

nrow(student_test_2_rev_2)

###Lee bound (higher bound)
#Prepare dataframe for estimating higher bound (balanced sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev_panel,
                                    treatment_2_2018==1&
                                      timing_end==1)

nrow(student_test_2_rev_t_2018)

student_test_2_rev_c_2018 <- subset(student_test_2_rev_panel,
                                    treatment_2_2018==0&
                                      timing_end==1)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_t_2019 <- subset(student_test_2_rev_panel,
                                    treatment_2_2019==1&
                                      timing_fus==1)

nrow(student_test_2_rev_t_2019)

student_test_2_rev_c_2019 <- subset(student_test_2_rev_panel,
                                    treatment_2_2019==0&
                                      timing_fus==1)

nrow(student_test_2_rev_c_2019)

quantile(student_test_2_rev_t_2019$irt_score_rev, 
         seq(0, 1, 0.02541))

student_test_2_rev_t_2019_tr1 <- subset(student_test_2_rev_t_2019,
                                        irt_score_rev>(-0.85))

student_test_2_rev_t_2019_tr2 <- subset(student_test_2_rev_t_2019,
                                        irt_score_rev<=(-0.85))

options(digits=10)

unique(student_test_2_rev_t_2019_tr2$ztest_score_baseline_exc_2_4)

student_test_2_rev_t_2019_tr3 <- subset(student_test_2_rev_t_2019_tr2,
                                        ztest_score_baseline_exc_2_4>
                                          -0.794325458)

student_test_2_rev_t_2019_tr4 <- subset(student_test_2_rev_t_2019_tr2,
                                        ztest_score_baseline_exc_2_4<
                                          -0.794325458&
                                          ztest_score_baseline_exc_2_4>
                                          -0.8)

nrow(student_test_2_rev_t_2019_tr1)
nrow(student_test_2_rev_t_2019_tr2)
nrow(student_test_2_rev_t_2019_tr3)
nrow(student_test_2_rev_t_2019_tr4)

#draw first random number from random.org
set.seed(952617)

student_test_2_rev_t_2019_tr4 <- student_test_2_rev_t_2019_tr4[
  order(student_test_2_rev_t_2019_tr4$ID_IE_test),]
student_test_2_rev_t_2019_tr4$random <- runif(nrow(student_test_2_rev_t_2019_tr4))

student_test_2_rev_t_2019_tr4 <- student_test_2_rev_t_2019_tr4[
  order(student_test_2_rev_t_2019_tr4$random),]

student_test_2_rev_t_2019_tr4$number <- c(1:13)

student_test_2_rev_t_2019_tr5 <- subset(student_test_2_rev_t_2019_tr4,
                                        number <= 3)

student_test_2_rev_t_2019_tr5 <- 
  student_test_2_rev_t_2019_tr5[, colnames(student_test_2_rev_t_2019_tr5) != "number"]
student_test_2_rev_t_2019_tr5 <- 
  student_test_2_rev_t_2019_tr5[, colnames(student_test_2_rev_t_2019_tr5) != "random"]

student_test_2_rev_t_2019_tr <- rbind.data.frame(student_test_2_rev_t_2019_tr1,
                                                 student_test_2_rev_t_2019_tr3,
                                                 student_test_2_rev_t_2019_tr5)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t_2019_tr_check <- student_test_2_rev_t_2019_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

student_test_2_rev_t_2019_tr_id <- subset(student_test_2_rev_t_2019_tr,
                                          select=c("ID_IE_test"))

student_test_2_rev_t_2018_tr <- merge(student_test_2_rev_t_2019_tr_id, 
                                      student_test_2_rev_t_2018,
                                      by.x=c("ID_IE_test"), 
                                      by.y=c("ID_IE_test"), all=F)

student_test_2_rev_t <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_t_2019_tr)

nrow(student_test_2_rev_t)

student_test_2_rev_c <- subset(student_test_2_rev_panel,
                               treatment_2==0)

nrow(student_test_2_rev_c)


student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t,
                                         student_test_2_rev_c)

nrow(student_test_2_rev_2)

#run regression

reg6_27_2 <- lm_robust(irt_score_rev~
                         treatment_2_2018+
                         treatment_2_2019+
                         timing_fus+
                         department*urban.rural+
                         zscore_baseline_2018_2+
                         zscore_baseline_2019_2+
                         Age_r+
                         Sex+
                         Shift+
                         No_elder+
                         No_younger+
                         Student_own_desk_home+
                         Math_textbook_LY_e+
                         Math_Notebook_LY_e+
                         Cooking_non_fire+
                         Item_House__1+
                         Item_House__2+
                         Item_House__3+
                         Item_House__4+
                         Item_House__5+
                         Item_House__6+
                         Item_House__7+
                         Item_House__8+
                         repeated_2019+
                         
                         Multigrade_G2+
                         Multigrade_G2_2019+
                         Multigrade_G3_2019+
                         Number_total_Students+
                         
                         Teacher_Sex+
                         Teacher_Age+
                         Highest_Degree_highschool+
                         Highest_Degree_professorate+
                         Highest_Degree_other+
                         Teachr_Qualification__2+
                         Teachr_Qualification__3+
                         Teachr_Qualification__6+
                         Teachr_Qualification__7+
                         Teachr_Apontmt_Post__2+
                         Teachr_Apontmt_Post__3+
                         Teachr_Apontmt_Post__4+
                         Year_Start_teach_r+
                         
                         HM_Sex+
                         HM_Age+
                         Year_HM_ttl+
                         Year_HM_this_school+
                         
                         HM_Highest_Degree_highschool+
                         HM_Highest_Degree_master+
                         HM_Highest_Degree_professorate+
                         HM_Highest_Degree_other+
                         
                         School_Facility__2+
                         School_Facility__3+School_Facility__4+
                         School_Facility__5+School_Facility__6+
                         School_Facility__7+School_Facility__8+
                         
                         Donor_Project__2+
                         Donor_Project__3+
                         Donor_Project__4+
                         Donor_Project__5+
                         Donor_Project__6+
                         Donor_Project__7+
                         Donor_Project__8+
                         Donor_Project__9+
                         Donor_Project__10+
                         Donor_Project__11+
                         Donor_Project__12+
                         Donor_Project__13+
                         Donor_Project__14+
                         Donor_Project__15+
                         Donor_Project__16+
                         Donor_Project__17+
                         Donor_Project__18+
                         Donor_Project__19+
                         Donor_Project__20, 
                       
                       data=student_test_2_rev_2,
                       clusters = ID_IE, se_type = "stata")

summary(reg6_27_2)

###Table E-1 in Apendix 6#### 

###Kling & Liebman bound
#attrition sample in the treatment group 

student_test_2_rev_end_r_ta <- subset(student_test_2_rev_end_r,
                                      treatment=="Treatment"&
                                        attrition_d==1)

student_test_2_rev_end_r_ta$treatment_2_2018 <-c(1)
student_test_2_rev_end_r_ta$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_ta$timing_end <- c(1)
student_test_2_rev_end_r_ta$timing_fus <- c(0)

#non-attrition sample in the treatment group

student_test_2_rev_end_r_tna <- subset(student_test_2_rev_end_r,
                                       treatment=="Treatment"&
                                         attrition_d==0)

student_test_2_rev_end_r_tna$treatment_2_2018 <-c(1)
student_test_2_rev_end_r_tna$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_tna$timing_end <- c(1)
student_test_2_rev_end_r_tna$timing_fus <- c(0)

student_test_2_rev_end_r_ca <- subset(student_test_2_rev_end_r,
                                      treatment=="Control"&
                                        attrition_d==1)

student_test_2_rev_end_r_ca$treatment_2_2018 <-c(0)
student_test_2_rev_end_r_ca$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_ca$timing_end <- c(1)
student_test_2_rev_end_r_ca$timing_fus <- c(0)

#non-attrition sample in control group

student_test_2_rev_end_r_cna <- subset(student_test_2_rev_end_r,
                                       treatment=="Control"&
                                         attrition_d==0)

student_test_2_rev_end_r_cna$treatment_2_2018 <-c(0)
student_test_2_rev_end_r_cna$treatment_2_2019 <-c(0)
student_test_2_rev_end_r_cna$timing_end <- c(1)
student_test_2_rev_end_r_cna$timing_fus <- c(0)

###add attrition dummy to dataframe of follow-up data

student_test_2_rev_fus_r <- dplyr::left_join(student_test_2_rev_fus_attrition_d, 
                                             student_test_2_rev_fus_r,
                                             by=c("ID_IE_test"))

student_test_2_rev_fus_r_ta <- subset(student_test_2_rev_fus_r,
                                      treatment=="Treatment"&
                                        attrition_d==1)

student_test_2_rev_fus_r_tna <- subset(student_test_2_rev_fus_r,
                                       treatment=="Treatment"&
                                         attrition_d==0)

student_test_2_rev_fus_r_ta$treatment_2_2018 <-c(0)
student_test_2_rev_fus_r_ta$treatment_2_2019 <-c(1)
student_test_2_rev_fus_r_ta$timing_end <- c(0)
student_test_2_rev_fus_r_ta$timing_fus <- c(1)

#

student_test_2_rev_fus_r_ca <- subset(student_test_2_rev_fus_r,
                                      treatment=="Control"&
                                        attrition_d==1)

student_test_2_rev_fus_r_cna <- subset(student_test_2_rev_fus_r,
                                       treatment=="Control"&
                                         attrition_d==0)

student_test_2_rev_fus_r_ca$treatment_2_2018 <-c(0)
student_test_2_rev_fus_r_ca$treatment_2_2019 <-c(0)
student_test_2_rev_fus_r_ca$timing_end <- c(0)
student_test_2_rev_fus_r_ca$timing_fus <- c(1)

###Kling and Liebman (2004)

###implement code corresponding to the outcome and bandwidth

###Outcome: Z scores

###Treatment group, end-line

###Column 3-1 Z scores
#adjust 0.15sd: lower bound, treatment group of end-line data

#Outcome: knowing
student_test_2_rev_end_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.15)

#Outcome: applying
student_test_2_rev_end_r_ta$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_application_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.15)

#Outcome: reasoning
student_test_2_rev_end_r_ta$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.15)

#Column 4-1 Z scores
#adjust 0.10sd: lower bound, treatment group of end-line data

#Outcome: knowing
student_test_2_rev_end_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.1)

#Outcome: applying
student_test_2_rev_end_r_ta$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_application_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.1)

#Outcome: reasoning
student_test_2_rev_end_r_ta$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.1)

###Column 5-1 Z scores
#adjust 0.10sd: higher bound, treatment group of end-line data

#Outcome: knowing
student_test_2_rev_end_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.1)

#Outcome: applying
student_test_2_rev_end_r_ta$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_application_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.1)

#Outcome: reasoning
student_test_2_rev_end_r_ta$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.1)

###Column 6-1 Z scores
#adjust 0.15sd: higher bound, treatment group of end-line data

#Outcome: knowing
student_test_2_rev_end_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.15)

#Outcome: applying
student_test_2_rev_end_r_ta$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_application_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.15)

#Outcome: reasoning
student_test_2_rev_end_r_ta$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.15)

###Control, end-line
###Column 3-1 Z scores
#adjust 0.15sd: lower bound, control group of end-line

#Outcome: knowing
student_test_2_rev_end_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.15)

#Outcome: applying
student_test_2_rev_end_r_ca$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_application_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.15)

#Outcome: reasoning
student_test_2_rev_end_r_ca$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_reasoning_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.15)

###Column 4-1 Z scores
#adjust 0.10sd: lower bound, control group of end-line

#Outcome: knowing
student_test_2_rev_end_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.1)

#Outcome: applying
student_test_2_rev_end_r_ca$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_application_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.1)

#Outcome: reasoning
student_test_2_rev_end_r_ca$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_reasoning_2)+
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.1)

###Column 5-1 Z scores
#adjust 0.10sd: higher bound, control group of end-line

#Outcome: knowing
student_test_2_rev_end_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.1)

#Outcome: applying
student_test_2_rev_end_r_ca$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_application_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.1)

#Outcome: reasoning
student_test_2_rev_end_r_ca$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_reasoning_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.1)

###Column 6-1 Z scores
#adjust 0.15sd: higher bound, control group of end-line

#Outcome: knowing
student_test_2_rev_end_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_knowledge_2)*0.15)

#Outcome: applying
student_test_2_rev_end_r_ca$ztest_score_application_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_application_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_application_2)*0.15)

#Outcome: reasoning
student_test_2_rev_end_r_ca$ztest_score_reasoning_2　<- 
  c(mean(student_test_2_rev_end_r_cna$ztest_score_reasoning_2)-
      sd(student_test_2_rev_end_r_tna$ztest_score_reasoning_2)*0.15)

###Treatment, follow-up
###Column 3-2 Z scores

#adjust 0.15sd: lower bound, treatment group of follow-up data

#Outcome: knowing
student_test_2_rev_fus_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.15)

###Column 4-2 Z scores
#adjust 0.10sd: lower bound, treatment group of follow-up data

#Outcome: knowing
student_test_2_rev_fus_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.1)

###Column 5-2 Z scores
#adjust 0.10sd: higher bound, treatment group of follow-up data

#Outcome: knowing
student_test_2_rev_fus_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.1)

###Column 6-2 Z scores
#adjust 0.15sd: higher bound, treatment group of follow-up data

#Outcome: knowing
student_test_2_rev_fus_r_ta$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.15)

###Control group, follow-up
#Column 3-2 Z scores
#adjust 0.15sd: lower bound, control group of follow-up data

#Outcome: knowing

student_test_2_rev_fus_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_cna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.15)

###Column 4-2 Z scores
#adjust 0.1sd: lower bound, control group of follow-up data

#Outcome: knowing

student_test_2_rev_fus_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_cna$ztest_score_knowledge_2)+
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.1)

###Column 5-2 Z scores
#adjust 0.10sd: higher bound, control group of follow-up data

#Outcome: knowing

student_test_2_rev_fus_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_cna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.1)

###Column 6-2 Z scores
#adjust 0.15sd: higher bound, control group of follow-up data

#Outcome: knowing

student_test_2_rev_fus_r_ca$ztest_score_knowledge_2　<- 
  c(mean(student_test_2_rev_fus_r_cna$ztest_score_knowledge_2)-
      sd(student_test_2_rev_fus_r_tna$ztest_score_knowledge_2)*0.15)

###rbind dataframes (only end-line)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_end_r_ta,
                                         student_test_2_rev_end_r_tna,
                                         student_test_2_rev_end_r_ca,
                                         student_test_2_rev_end_r_cna)

student_test_2_rev_2 <- merge(student_test_2_rev_2, student_test_2_baseline,
                              by.x=c("ID_IE_test"), 
                              by.y=c("ID_IE_test"), all=F)

nrow(student_test_2_rev_2)

unique(is.na(student_test_2_rev_2r$ztest_score_knowledge_2))

###rbind dataframes (only follow-up)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_fus_r_ta,
                                         student_test_2_rev_fus_r_tna,
                                         student_test_2_rev_fus_r_ca,
                                         student_test_2_rev_fus_r_cna)

student_test_2_rev_2 <- merge(student_test_2_rev_2, student_test_2_baseline,
                              by.x=c("ID_IE_test"), 
                              by.y=c("ID_IE_test"), all=F)

nrow(student_test_2_rev_2)

#run regression

###table E-1

student_test_2_rev_end_r <- subset(student_test_2_rev_2,
                                   timing_end==1)

#Column 3-1, 4-1, 5-1, 6-1: knowing, 

regE_3456_1_K <- lm_robust(ztest_score_knowledge_2~
                             
                             treatment_2_2018+
                             ztest_score_baseline_exc_2_4+
                             
                             department*urban.rural+
                             no_elder+
                             no_younger+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             
                             Age_r+Sex+
                             Shift+
                             
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8+
                             Number_total_Students+
                             Number_G2_Students+
                             Multigrade_G2+
                             
                             HM_Sex+
                             HM_Age+
                             Year_HM_ttl+
                             Year_HM_this_school+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_professorate+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Internet_Place__3+
                             Meal_Provision+
                             Supplement_Class+
                             Donor_Project__2+
                             Donor_Project__3+
                             Donor_Project__4+
                             Donor_Project__5+
                             Donor_Project__6+
                             Donor_Project__7+
                             Donor_Project__8+
                             Donor_Project__9+
                             Donor_Project__10+
                             Donor_Project__11+
                             Donor_Project__12+
                             Donor_Project__13+
                             Donor_Project__14+
                             Donor_Project__15+
                             Donor_Project__16+
                             Donor_Project__17+
                             Donor_Project__18+
                             Donor_Project__19+
                             Donor_Project__20+
                             
                             Teacher_Sex+Teacher_Age+
                             Highest_Degree_highschool+
                             Highest_Degree_professorate+
                             Highest_Degree_other+
                             Teachr_Qualification__2+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Year_Start_teach_r+
                             Work_Shift_G2_both+
                             Teaching_other_subject_G2, 
                           
                           data=student_test_2_rev_end_r,
                           clusters = ID_IE, se_type = "stata")

summary(regE_3456_1_K)

regE_4_1_K <- lm_robust(ztest_score_knowledge_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_4_1_K)

regE_5_1_K <- lm_robust(ztest_score_knowledge_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_5_1_K)

#Column 3-1. 4-1, 5-1, 6-1: applying

regE_3456_1_A <- lm_robust(ztest_score_application_2~
                             
                             treatment_2_2018+
                             ztest_score_baseline_exc_2_4+
                             
                             department*urban.rural+
                             no_elder+
                             no_younger+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             
                             Age_r+Sex+
                             Shift+
                             
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8+
                             Number_total_Students+
                             Number_G2_Students+
                             Multigrade_G2+
                             
                             HM_Sex+
                             HM_Age+
                             Year_HM_ttl+
                             Year_HM_this_school+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_professorate+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Internet_Place__3+
                             Meal_Provision+
                             Supplement_Class+
                             Donor_Project__2+
                             Donor_Project__3+
                             Donor_Project__4+
                             Donor_Project__5+
                             Donor_Project__6+
                             Donor_Project__7+
                             Donor_Project__8+
                             Donor_Project__9+
                             Donor_Project__10+
                             Donor_Project__11+
                             Donor_Project__12+
                             Donor_Project__13+
                             Donor_Project__14+
                             Donor_Project__15+
                             Donor_Project__16+
                             Donor_Project__17+
                             Donor_Project__18+
                             Donor_Project__19+
                             Donor_Project__20+
                             
                             Teacher_Sex+Teacher_Age+
                             Highest_Degree_highschool+
                             Highest_Degree_professorate+
                             Highest_Degree_other+
                             Teachr_Qualification__2+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Year_Start_teach_r+
                             Work_Shift_G2_both+
                             Teaching_other_subject_G2, 
                           
                           data=student_test_2_rev_end_r,
                           clusters = ID_IE, se_type = "stata")

summary(regE_3456_1_A)

regE_4_1_A <- lm_robust(ztest_score_application_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_4_1_A)

regE_5_1_A <- lm_robust(ztest_score_application_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_5_1_A)

#Column 3-1, 4-1, 5-1, 6-1: reasoning

regE_3456_1_R <- lm_robust(ztest_score_reasoning_2~
                             
                             treatment_2_2018+
                             ztest_score_baseline_exc_2_4+
                             
                             department*urban.rural+
                             no_elder+
                             no_younger+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             
                             Age_r+Sex+
                             Shift+
                             
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8+
                             Number_total_Students+
                             Number_G2_Students+
                             Multigrade_G2+
                             
                             HM_Sex+
                             HM_Age+
                             Year_HM_ttl+
                             Year_HM_this_school+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_professorate+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Internet_Place__3+
                             Meal_Provision+
                             Supplement_Class+
                             Donor_Project__2+
                             Donor_Project__3+
                             Donor_Project__4+
                             Donor_Project__5+
                             Donor_Project__6+
                             Donor_Project__7+
                             Donor_Project__8+
                             Donor_Project__9+
                             Donor_Project__10+
                             Donor_Project__11+
                             Donor_Project__12+
                             Donor_Project__13+
                             Donor_Project__14+
                             Donor_Project__15+
                             Donor_Project__16+
                             Donor_Project__17+
                             Donor_Project__18+
                             Donor_Project__19+
                             Donor_Project__20+
                             
                             Teacher_Sex+Teacher_Age+
                             Highest_Degree_highschool+
                             Highest_Degree_professorate+
                             Highest_Degree_other+
                             Teachr_Qualification__2+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Year_Start_teach_r+
                             Work_Shift_G2_both+
                             Teaching_other_subject_G2, 
                           
                           data=student_test_2_rev_end_r,
                           clusters = ID_IE, se_type = "stata")

summary(regE_3456_1_R)

regE_4_1_R <- lm_robust(ztest_score_reasoning_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_4_1_R)

regE_5_1_R <- lm_robust(ztest_score_reasoning_2~
                          
                          treatment_2_2018+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_end_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_5_1_R)

#Column 3-2, 4-2, 5-2, 6-2: knowing, 

student_test_2_rev_fus_r <- subset(student_test_2_rev_2,
                                   timing_fus==1)

regE_3456_2_K <- lm_robust(ztest_score_knowledge_2~
                             
                             treatment_2_2019+
                             ztest_score_baseline_exc_2_4+
                             
                             department*urban.rural+
                             no_elder+
                             no_younger+
                             Student_own_desk_home+
                             Math_textbook_LY_e+
                             Math_Notebook_LY_e+
                             Cooking_non_fire+
                             
                             Age_r+Sex+
                             Shift+
                             
                             Item_House__1+
                             Item_House__2+
                             Item_House__3+
                             Item_House__4+
                             Item_House__5+
                             Item_House__6+
                             Item_House__7+
                             Item_House__8+
                             Number_total_Students+
                             Number_G2_Students+
                             Multigrade_G2+
                             
                             HM_Sex+
                             HM_Age+
                             Year_HM_ttl+
                             Year_HM_this_school+
                             HM_Highest_Degree_highschool+
                             HM_Highest_Degree_master+
                             HM_Highest_Degree_professorate+
                             HM_Highest_Degree_other+
                             School_Facility__2+
                             School_Facility__3+
                             School_Facility__4+
                             School_Facility__5+
                             School_Facility__6+
                             School_Facility__7+
                             School_Facility__8+
                             Internet_Place__3+
                             Meal_Provision+
                             Supplement_Class+
                             Donor_Project__2+
                             Donor_Project__3+
                             Donor_Project__4+
                             Donor_Project__5+
                             Donor_Project__6+
                             Donor_Project__7+
                             Donor_Project__8+
                             Donor_Project__9+
                             Donor_Project__10+
                             Donor_Project__11+
                             Donor_Project__12+
                             Donor_Project__13+
                             Donor_Project__14+
                             Donor_Project__15+
                             Donor_Project__16+
                             Donor_Project__17+
                             Donor_Project__18+
                             Donor_Project__19+
                             Donor_Project__20+
                             
                             Teacher_Sex+Teacher_Age+
                             Highest_Degree_highschool+
                             Highest_Degree_professorate+
                             Highest_Degree_other+
                             Teachr_Qualification__2+
                             Teachr_Qualification__3+
                             Teachr_Qualification__6+
                             Teachr_Qualification__7+
                             Teachr_Apontmt_Post__2+
                             Teachr_Apontmt_Post__3+
                             Teachr_Apontmt_Post__4+
                             Year_Start_teach_r+
                             Work_Shift_G2_both+
                             Teaching_other_subject_G2, 
                           
                           data=student_test_2_rev_fus_r,
                           clusters = ID_IE, se_type = "stata")

summary(regE_3456_2_K)

regE_4_2_K <- lm_robust(ztest_score_knowledge_2~
                          
                          treatment_2_2019+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_fus_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_4_2_K)

regE_5_2_K <- lm_robust(ztest_score_knowledge_2~
                          
                          treatment_2_2019+
                          ztest_score_baseline_exc_2_4+
                          
                          department*urban.rural+
                          no_elder+
                          no_younger+
                          Student_own_desk_home+
                          Math_textbook_LY_e+
                          Math_Notebook_LY_e+
                          Cooking_non_fire+
                          
                          Age_r+Sex+
                          Shift+
                          
                          Item_House__1+
                          Item_House__2+
                          Item_House__3+
                          Item_House__4+
                          Item_House__5+
                          Item_House__6+
                          Item_House__7+
                          Item_House__8+
                          Number_total_Students+
                          Number_G2_Students+
                          Multigrade_G2+
                          
                          HM_Sex+
                          HM_Age+
                          Year_HM_ttl+
                          Year_HM_this_school+
                          HM_Highest_Degree_highschool+
                          HM_Highest_Degree_master+
                          HM_Highest_Degree_professorate+
                          HM_Highest_Degree_other+
                          School_Facility__2+
                          School_Facility__3+
                          School_Facility__4+
                          School_Facility__5+
                          School_Facility__6+
                          School_Facility__7+
                          School_Facility__8+
                          Internet_Place__3+
                          Meal_Provision+
                          Supplement_Class+
                          Donor_Project__2+
                          Donor_Project__3+
                          Donor_Project__4+
                          Donor_Project__5+
                          Donor_Project__6+
                          Donor_Project__7+
                          Donor_Project__8+
                          Donor_Project__9+
                          Donor_Project__10+
                          Donor_Project__11+
                          Donor_Project__12+
                          Donor_Project__13+
                          Donor_Project__14+
                          Donor_Project__15+
                          Donor_Project__16+
                          Donor_Project__17+
                          Donor_Project__18+
                          Donor_Project__19+
                          Donor_Project__20+
                          
                          Teacher_Sex+Teacher_Age+
                          Highest_Degree_highschool+
                          Highest_Degree_professorate+
                          Highest_Degree_other+
                          Teachr_Qualification__2+
                          Teachr_Qualification__3+
                          Teachr_Qualification__6+
                          Teachr_Qualification__7+
                          Teachr_Apontmt_Post__2+
                          Teachr_Apontmt_Post__3+
                          Teachr_Apontmt_Post__4+
                          Year_Start_teach_r+
                          Work_Shift_G2_both+
                          Teaching_other_subject_G2, 
                        
                        data=student_test_2_rev_fus_r,
                        clusters = ID_IE, se_type = "stata")

summary(regE_5_2_K)

###Lee bound (lower bound)  knowing
#Prepare dataframe for estimating lower bound (full sample)

#outcome (cognitive skill): knowing

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_knowledge_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_knowledge_2<=2.07)

nrow(student_test_2_rev_t_2018_tr1)

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_knowledge_2>2.07&
           ztest_score_knowledge_2<2.08&
           ztest_score_baseline_exc_2_4>(-0.40))

student_test_2_rev_t_2018_tr3 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_knowledge_2>2.07&
           ztest_score_knowledge_2<2.08&
           ztest_score_baseline_exc_2_4>(-0.7)&
           ztest_score_baseline_exc_2_4<(-0.4))

nrow(student_test_2_rev_t_2018_tr2)
nrow(student_test_2_rev_t_2018_tr3)

unique(student_test_2_rev_t_2018_tr3$ztest_score_knowledge_2)
unique(student_test_2_rev_t_2018_tr3$ztest_score_baseline_exc_2_4)

#draw first random number from random.org
set.seed(456620)

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$ID_IE_test),]
student_test_2_rev_t_2018_tr3$random <- runif(nrow(student_test_2_rev_t_2018_tr3))

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$random),]

student_test_2_rev_t_2018_tr3$number <- c(1:9)

student_test_2_rev_t_2018_tr4 <- subset(student_test_2_rev_t_2018_tr3,
                                        number <= 5)

student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "number"]
student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "random"]

#

student_test_2_rev_t_2018_tr <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                                 student_test_2_rev_t_2018_tr2,
                                                 student_test_2_rev_t_2018_tr4)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2018_tr_check <- student_test_2_rev_t_2018_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

#

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

#outcome (cognitive skill): applying
#lower bound

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_application_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_application_2<=2.4)

nrow(student_test_2_rev_t_2018_tr1)

#

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

#outcome (cognitive skill): reasoning
#lower bound

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_reasoning_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2<=3.401)

nrow(student_test_2_rev_t_2018_tr1)

max(student_test_2_rev_t_2018$ztest_score_reasoning_2)

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2>3.401&
           ztest_score_reasoning_2<3.410&
           ztest_score_baseline_exc_2_4>(0.8))

nrow(student_test_2_rev_t_2018_tr2)
unique(student_test_2_rev_t_2018_tr2$ztest_score_reasoning_2)
unique(student_test_2_rev_t_2018_tr2$ztest_score_baseline_exc_2_4)

student_test_2_rev_t_2018_tr3 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2>3.401&
           ztest_score_reasoning_2<3.410&
           ztest_score_baseline_exc_2_4<(0.8)&
           ztest_score_baseline_exc_2_4>(0.7))

nrow(student_test_2_rev_t_2018_tr2)
nrow(student_test_2_rev_t_2018_tr3)

unique(student_test_2_rev_t_2018_tr3$ztest_score_reasoning_2)
unique(student_test_2_rev_t_2018_tr3$ztest_score_baseline_exc_2_4)

#draw first random number from random.org
set.seed(399465)

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$ID_IE_test),]
student_test_2_rev_t_2018_tr3$random <- runif(nrow(student_test_2_rev_t_2018_tr3))

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$random),]

student_test_2_rev_t_2018_tr3$number <- c(1:6)

student_test_2_rev_t_2018_tr4 <- subset(student_test_2_rev_t_2018_tr3,
                                        number <= 1)

student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "number"]
student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "random"]

student_test_2_rev_t_2018_tr <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                                 student_test_2_rev_t_2018_tr2,
                                                 student_test_2_rev_t_2018_tr4)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2018_tr_check <- student_test_2_rev_t_2018_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

#

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

###Lee bound (higher bound)  knowing
#Prepare dataframe for estimating higher bound (full sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_knowledge_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_knowledge_2>(-1.5))

nrow(student_test_2_rev_t_2018_tr1)

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_knowledge_2<=(-1.5)&
           ztest_score_knowledge_2>(-1.83)&
           ztest_score_baseline_exc_2_4>(0.7))

nrow(student_test_2_rev_t_2018_tr2)

student_test_2_rev_t_2018_tr <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                                 student_test_2_rev_t_2018_tr2)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2018_tr_check <- student_test_2_rev_t_2018_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

###Lee bound (higher bound)  applying
#Prepare dataframe for estimating higher bound (full sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_application_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_application_2>(-0.836))

nrow(student_test_2_rev_t_2018_tr1)

unique(student_test_2_rev_t_2018_tr1$ztest_score_application_2)
unique(student_test_2_rev_t_2018_tr1$ztest_score_baseline_exc_2_4)

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_application_2<=(-0.836)&
           ztest_score_baseline_exc_2_4>(-0.8))

unique(student_test_2_rev_t_2018_tr2$ztest_score_application_2)
unique(student_test_2_rev_t_2018_tr2$ztest_score_baseline_exc_2_4)

nrow(student_test_2_rev_t_2018_tr2)

student_test_2_rev_t_2018_tr3 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_application_2<=(-0.836)&
           ztest_score_baseline_exc_2_4<=(-0.8))

nrow(student_test_2_rev_t_2018_tr3)

unique(student_test_2_rev_t_2018_tr3$ztest_score_application_2)
unique(student_test_2_rev_t_2018_tr3$ztest_score_baseline_exc_2_4)

#draw random number from random.org
set.seed(227880)

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$ID_IE_test),]
student_test_2_rev_t_2018_tr3$random <- runif(nrow(student_test_2_rev_t_2018_tr3))

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$random),]

student_test_2_rev_t_2018_tr3$number <- c(1:124)

student_test_2_rev_t_2018_tr4 <- subset(student_test_2_rev_t_2018_tr3,
                                        number <= 74)

student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "number"]
student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "random"]

#

student_test_2_rev_t_2018_tr <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                                 student_test_2_rev_t_2018_tr2,
                                                 student_test_2_rev_t_2018_tr4)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2018_tr_check <- student_test_2_rev_t_2018_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

###Lee bound (higher bound)  reasoning
#Prepare dataframe for estimating higher bound (full sample)

student_test_2_rev_t_2018 <- subset(student_test_2_rev,
                                    treatment_2_2018==1&
                                      timing_end==1)

quantile(student_test_2_rev_t_2018$ztest_score_reasoning_2, 
         seq(0, 1, 0.03180))

student_test_2_rev_t_2018_tr1 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2>(-0.459))

nrow(student_test_2_rev_t_2018_tr1)

unique(student_test_2_rev_t_2018$ztest_score_reasoning_2)
unique(student_test_2_rev_t_2018_tr1$ztest_score_baseline_exc_2_4)

student_test_2_rev_t_2018_tr2 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2<=(-0.459)&
           ztest_score_baseline_exc_2_4>(-0.8))

unique(student_test_2_rev_t_2018_tr2$ztest_score_reasoning_2)
unique(student_test_2_rev_t_2018_tr2$ztest_score_baseline_exc_2_4)

nrow(student_test_2_rev_t_2018_tr2)

student_test_2_rev_t_2018_tr3 <- 
  subset(student_test_2_rev_t_2018,
         ztest_score_reasoning_2<=(-0.459)&
           ztest_score_baseline_exc_2_4<=(-0.8))

nrow(student_test_2_rev_t_2018_tr3)

unique(student_test_2_rev_t_2018_tr3$ztest_score_application_2)
unique(student_test_2_rev_t_2018_tr3$ztest_score_baseline_exc_2_4)

#draw random number from random.org
set.seed(735121)

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$ID_IE_test),]
student_test_2_rev_t_2018_tr3$random <- runif(nrow(student_test_2_rev_t_2018_tr3))

student_test_2_rev_t_2018_tr3 <- student_test_2_rev_t_2018_tr3[
  order(student_test_2_rev_t_2018_tr3$random),]

student_test_2_rev_t_2018_tr3$number <- c(1:169)

student_test_2_rev_t_2018_tr4 <- subset(student_test_2_rev_t_2018_tr3,
                                        number <= 119)

student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "number"]
student_test_2_rev_t_2018_tr4 <- 
  student_test_2_rev_t_2018_tr4[, colnames(student_test_2_rev_t_2018_tr4) != "random"]

#

student_test_2_rev_t_2018_tr <- rbind.data.frame(student_test_2_rev_t_2018_tr1,
                                                 student_test_2_rev_t_2018_tr2,
                                                 student_test_2_rev_t_2018_tr4)

nrow(student_test_2_rev_t_2018_tr)

student_test_2_rev_t_2018_tr_check <- student_test_2_rev_t_2018_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

student_test_2_rev_c_2018 <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2018)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2018_tr,
                                         student_test_2_rev_c_2018)

nrow(student_test_2_rev_2)

#run regression

#Column (II)-1: knowing
#Column (VII)-1: knowing

regE_2_17_K <- lm_robust(ztest_score_knowledge_2~
                           
                           treatment_2+
                           ztest_score_baseline_exc_2_4+
                           
                           department*urban.rural+
                           Age_r+Sex+No_elder+
                           No_younger+
                           Shift+
                           Student_own_desk_home+
                           Math_textbook_LY_e+
                           Math_Notebook_LY_e+
                           Cooking_non_fire+
                           Item_House__1+
                           Item_House__2+
                           Item_House__3+
                           Item_House__4+
                           Item_House__5+
                           Item_House__6+
                           Item_House__7+
                           Item_House__8+
                           Number_total_Students+
                           Number_G2_Students+
                           Multigrade_G2+
                           
                           HM_Sex+
                           HM_Age+
                           Year_HM_ttl+
                           Year_HM_this_school+
                           HM_Highest_Degree_highschool+
                           HM_Highest_Degree_master+
                           HM_Highest_Degree_professorate+
                           HM_Highest_Degree_other+
                           School_Facility__2+
                           School_Facility__3+
                           School_Facility__4+
                           School_Facility__5+
                           School_Facility__6+
                           School_Facility__7+
                           School_Facility__8+
                           Internet_Place__3+
                           Meal_Provision+
                           Supplement_Class+
                           Donor_Project__2+
                           Donor_Project__3+
                           Donor_Project__4+
                           Donor_Project__5+
                           Donor_Project__6+
                           Donor_Project__7+
                           Donor_Project__8+
                           Donor_Project__9+
                           Donor_Project__10+
                           Donor_Project__11+
                           Donor_Project__12+
                           Donor_Project__13+
                           Donor_Project__14+
                           Donor_Project__15+
                           Donor_Project__16+
                           Donor_Project__17+
                           Donor_Project__18+
                           Donor_Project__19+
                           Donor_Project__20+
                           
                           Teacher_Sex+Teacher_Age+
                           Highest_Degree_highschool+
                           Highest_Degree_professorate+
                           Highest_Degree_other+
                           Teachr_Qualification__2+
                           Teachr_Qualification__3+
                           Teachr_Qualification__6+
                           Teachr_Qualification__7+
                           Teachr_Apontmt_Post__2+
                           Teachr_Apontmt_Post__3+
                           Teachr_Apontmt_Post__4+
                           Year_Start_teach_r+
                           Work_Shift_G2_both+
                           Teaching_other_subject_G2, 
                         
                         data=student_test_2_rev_2,
                         clusters = ID_IE, se_type = "stata")

summary(regE_2_17_K)

#Column (II)-1: applying
#Column (VII)-1: applying

regE_2_17_A <- lm_robust(ztest_score_application_2~
                           
                           treatment_2+
                           ztest_score_baseline_exc_2_4+
                           
                           department*urban.rural+
                           Age_r+Sex+No_elder+
                           No_younger+
                           Shift+
                           Student_own_desk_home+
                           Math_textbook_LY_e+
                           Math_Notebook_LY_e+
                           Cooking_non_fire+
                           Item_House__1+
                           Item_House__2+
                           Item_House__3+
                           Item_House__4+
                           Item_House__5+
                           Item_House__6+
                           Item_House__7+
                           Item_House__8+
                           Number_total_Students+
                           Number_G2_Students+
                           Multigrade_G2+
                           
                           HM_Sex+
                           HM_Age+
                           Year_HM_ttl+
                           Year_HM_this_school+
                           HM_Highest_Degree_highschool+
                           HM_Highest_Degree_master+
                           HM_Highest_Degree_professorate+
                           HM_Highest_Degree_other+
                           School_Facility__2+
                           School_Facility__3+
                           School_Facility__4+
                           School_Facility__5+
                           School_Facility__6+
                           School_Facility__7+
                           School_Facility__8+
                           Internet_Place__3+
                           Meal_Provision+
                           Supplement_Class+
                           Donor_Project__2+
                           Donor_Project__3+
                           Donor_Project__4+
                           Donor_Project__5+
                           Donor_Project__6+
                           Donor_Project__7+
                           Donor_Project__8+
                           Donor_Project__9+
                           Donor_Project__10+
                           Donor_Project__11+
                           Donor_Project__12+
                           Donor_Project__13+
                           Donor_Project__14+
                           Donor_Project__15+
                           Donor_Project__16+
                           Donor_Project__17+
                           Donor_Project__18+
                           Donor_Project__19+
                           Donor_Project__20+
                           
                           Teacher_Sex+Teacher_Age+
                           Highest_Degree_highschool+
                           Highest_Degree_professorate+
                           Highest_Degree_other+
                           Teachr_Qualification__2+
                           Teachr_Qualification__3+
                           Teachr_Qualification__6+
                           Teachr_Qualification__7+
                           Teachr_Apontmt_Post__2+
                           Teachr_Apontmt_Post__3+
                           Teachr_Apontmt_Post__4+
                           Year_Start_teach_r+
                           Work_Shift_G2_both+
                           Teaching_other_subject_G2, 
                         
                         data=student_test_2_rev_2,
                         clusters = ID_IE, se_type = "stata")

summary(regE_2_17_A)

student_test_2_rev_2$ztest_score_baseline_exc_2_4

#Column (II)-1: reasoning
#Column (VII)-1: reasoning

regE_2_17_R <- lm_robust(ztest_score_reasoning_2~
                           
                           treatment_2+
                           ztest_score_baseline_exc_2_4+
                           
                           department*urban.rural+
                           Age_r+Sex+No_elder+
                           No_younger+
                           Shift+
                           Student_own_desk_home+
                           Math_textbook_LY_e+
                           Math_Notebook_LY_e+
                           Cooking_non_fire+
                           Item_House__1+
                           Item_House__2+
                           Item_House__3+
                           Item_House__4+
                           Item_House__5+
                           Item_House__6+
                           Item_House__7+
                           Item_House__8+
                           Number_total_Students+
                           Number_G2_Students+
                           Multigrade_G2+
                           
                           HM_Sex+
                           HM_Age+
                           Year_HM_ttl+
                           Year_HM_this_school+
                           HM_Highest_Degree_highschool+
                           HM_Highest_Degree_master+
                           HM_Highest_Degree_professorate+
                           HM_Highest_Degree_other+
                           School_Facility__2+
                           School_Facility__3+
                           School_Facility__4+
                           School_Facility__5+
                           School_Facility__6+
                           School_Facility__7+
                           School_Facility__8+
                           Internet_Place__3+
                           Meal_Provision+
                           Supplement_Class+
                           Donor_Project__2+
                           Donor_Project__3+
                           Donor_Project__4+
                           Donor_Project__5+
                           Donor_Project__6+
                           Donor_Project__7+
                           Donor_Project__8+
                           Donor_Project__9+
                           Donor_Project__10+
                           Donor_Project__11+
                           Donor_Project__12+
                           Donor_Project__13+
                           Donor_Project__14+
                           Donor_Project__15+
                           Donor_Project__16+
                           Donor_Project__17+
                           Donor_Project__18+
                           Donor_Project__19+
                           Donor_Project__20+
                           
                           Teacher_Sex+Teacher_Age+
                           Highest_Degree_highschool+
                           Highest_Degree_professorate+
                           Highest_Degree_other+
                           Teachr_Qualification__2+
                           Teachr_Qualification__3+
                           Teachr_Qualification__6+
                           Teachr_Qualification__7+
                           Teachr_Apontmt_Post__2+
                           Teachr_Apontmt_Post__3+
                           Teachr_Apontmt_Post__4+
                           Year_Start_teach_r+
                           Work_Shift_G2_both+
                           Teaching_other_subject_G2, 
                         
                         data=student_test_2_rev_2,
                         clusters = ID_IE, se_type = "stata")

summary(regE_2_17_R)

###Lee bound (lower)
#follow-up

student_test_2_rev_t_2019 <- subset(student_test_2_rev,
                                    treatment_2_2019==1&
                                      timing_fus==1)

nrow(student_test_2_rev_t_2019)

quantile(student_test_2_rev_t_2019$ztest_score_knowledge_2, 
         seq(0, 1, 0.02049))

student_test_2_rev_t_2019_tr1 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2<(2.28))

nrow(student_test_2_rev_t_2019_tr1)

student_test_2_rev_t_2019_tr2 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2>=(2.28)&
           ztest_score_knowledge_2<(2.5)&
           ztest_score_baseline_exc_2_4>(0.7))

student_test_2_rev_t_2019_tr3 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2>=(2.28)&
           ztest_score_knowledge_2<(2.5)&
           ztest_score_baseline_exc_2_4<=(0.7)&
           ztest_score_baseline_exc_2_4>(0.4))

nrow(student_test_2_rev_t_2019_tr2)
nrow(student_test_2_rev_t_2019_tr3)

unique(student_test_2_rev_t_2019_tr2$ztest_score_knowledge_2)
unique(student_test_2_rev_t_2019_tr2$ztest_score_baseline_exc_2_4)

unique(student_test_2_rev_t_2019_tr3$ztest_score_knowledge_2)
unique(student_test_2_rev_t_2019_tr3$ztest_score_baseline_exc_2_4)

#draw first random number from random.org
set.seed(823542)

student_test_2_rev_t_2019_tr3 <- student_test_2_rev_t_2019_tr3[
  order(student_test_2_rev_t_2019_tr3$ID_IE_test),]
student_test_2_rev_t_2019_tr3$random <- runif(nrow(student_test_2_rev_t_2019_tr3))

student_test_2_rev_t_2019_tr3 <- student_test_2_rev_t_2019_tr3[
  order(student_test_2_rev_t_2019_tr3$random),]

student_test_2_rev_t_2019_tr3$number <- c(1:4)

student_test_2_rev_t_2019_tr4 <- subset(student_test_2_rev_t_2019_tr3,
                                        number <= 1)

student_test_2_rev_t_2019_tr4 <- 
  student_test_2_rev_t_2019_tr4[, colnames(student_test_2_rev_t_2019_tr4) != "number"]
student_test_2_rev_t_2019_tr4 <- 
  student_test_2_rev_t_2019_tr4[, colnames(student_test_2_rev_t_2019_tr4) != "random"]

#

student_test_2_rev_t_2019_tr <- rbind.data.frame(student_test_2_rev_t_2019_tr1,
                                                 student_test_2_rev_t_2019_tr2,
                                                 student_test_2_rev_t_2019_tr4)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t_2019_tr_check <- student_test_2_rev_t_2019_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

#

student_test_2_rev_c_2019 <- subset(student_test_2_rev,
                                    timing_fus==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2019)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2019_tr,
                                         student_test_2_rev_c_2019)

nrow(student_test_2_rev_2)

###Lee bound (higher)
#follow-up

student_test_2_rev_t_2019 <- subset(student_test_2_rev,
                                    treatment_2_2019==1&
                                      timing_fus==1)

nrow(student_test_2_rev_t_2019)

quantile(student_test_2_rev_t_2019$ztest_score_knowledge_2, 
         seq(0, 1, 0.02049))

student_test_2_rev_t_2019_tr1 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2>(-1.404199))

nrow(student_test_2_rev_t_2019_tr1)

student_test_2_rev_t_2019_tr2 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2<=(-1.404199)&
           ztest_score_baseline_exc_2_4<(-1))

student_test_2_rev_t_2019_tr3 <- 
  subset(student_test_2_rev_t_2019,
         ztest_score_knowledge_2<=(-1.404199)&
           ztest_score_baseline_exc_2_4>=(-1)&
           ztest_score_baseline_exc_2_4<(-0.7))

nrow(student_test_2_rev_t_2019_tr2)
nrow(student_test_2_rev_t_2019_tr3)

unique(student_test_2_rev_t_2019_tr2$ztest_score_knowledge_2)
unique(student_test_2_rev_t_2019_tr2$ztest_score_baseline_exc_2_4)

unique(student_test_2_rev_t_2019_tr3$ztest_score_knowledge_2)
unique(student_test_2_rev_t_2019_tr3$ztest_score_baseline_exc_2_4)

#draw first random number from random.org
set.seed(381626)

student_test_2_rev_t_2019_tr3 <- student_test_2_rev_t_2019_tr3[
  order(student_test_2_rev_t_2019_tr3$ID_IE_test),]
student_test_2_rev_t_2019_tr3$random <- runif(nrow(student_test_2_rev_t_2019_tr3))

student_test_2_rev_t_2019_tr3 <- student_test_2_rev_t_2019_tr3[
  order(student_test_2_rev_t_2019_tr3$random),]

student_test_2_rev_t_2019_tr3$number <- c(1:21)

student_test_2_rev_t_2019_tr4 <- subset(student_test_2_rev_t_2019_tr3,
                                        number <= 20)

student_test_2_rev_t_2019_tr4 <- 
  student_test_2_rev_t_2019_tr4[, colnames(student_test_2_rev_t_2019_tr4) != "number"]
student_test_2_rev_t_2019_tr4 <- 
  student_test_2_rev_t_2019_tr4[, colnames(student_test_2_rev_t_2019_tr4) != "random"]

#

student_test_2_rev_t_2019_tr <- rbind.data.frame(student_test_2_rev_t_2019_tr1,
                                                 student_test_2_rev_t_2019_tr2,
                                                 student_test_2_rev_t_2019_tr4)

nrow(student_test_2_rev_t_2019_tr)

student_test_2_rev_t_2019_tr_check <- student_test_2_rev_t_2019_tr %>%
  group_by(ID_IE_test) %>%
  filter(n()>1)

#

student_test_2_rev_c_2019 <- subset(student_test_2_rev,
                                    timing_fus==1&
                                      treatment_2==0)

nrow(student_test_2_rev_c_2019)

student_test_2_rev_2 <- rbind.data.frame(student_test_2_rev_t_2019_tr,
                                         student_test_2_rev_c_2019)

nrow(student_test_2_rev_2)

#run regression

regE_27_2 <- lm_robust(ztest_score_knowledge_2~
                         treatment_2+
                         ztest_score_baseline_exc_2_4+
                         
                         department*urban.rural+
                         Age_r+Sex+No_elder+
                         No_younger+
                         Live_w_Parent_father+
                         Live_w_Parent_mother+
                         Live_w_Parent_none+
                         Shift_2018+
                         repeated_2019+
                         Shift+
                         Student_own_desk_home+
                         Math_textbook_LY_e+
                         Math_Notebook_LY_e+
                         Cooking_non_fire+
                         Item_House__1+
                         Item_House__2+
                         Item_House__3+
                         Item_House__4+
                         Item_House__5+
                         Item_House__6+
                         Item_House__7+
                         Item_House__8+
                         
                         Number_total_Students+
                         Multigrade_G2_2019+
                         Multigrade_G3_2019+
                         
                         HM_Sex+
                         HM_Age+
                         Year_HM_ttl+
                         Year_HM_this_school+
                         HM_Highest_Degree_highschool+
                         HM_Highest_Degree_master+
                         HM_Highest_Degree_professorate+
                         HM_Highest_Degree_other+
                         School_Facility__2+
                         School_Facility__3+
                         School_Facility__4+
                         School_Facility__5+
                         School_Facility__6+
                         School_Facility__7+
                         School_Facility__8+
                         Internet_Place__3+
                         Meal_Provision+
                         Supplement_Class+
                         Donor_Project__2+
                         Donor_Project__3+
                         Donor_Project__4+
                         Donor_Project__5+
                         Donor_Project__6+
                         Donor_Project__7+
                         Donor_Project__8+
                         Donor_Project__9+
                         Donor_Project__10+
                         Donor_Project__11+
                         Donor_Project__12+
                         Donor_Project__13+
                         Donor_Project__14+
                         Donor_Project__15+
                         Donor_Project__16+
                         Donor_Project__17+
                         Donor_Project__18+
                         Donor_Project__19+
                         Donor_Project__20+
                         
                         Teacher_Sex+Teacher_Age+
                         Highest_Degree_highschool+
                         Highest_Degree_professorate+
                         Highest_Degree_other+
                         Teachr_Qualification__2+
                         Teachr_Qualification__3+
                         Teachr_Qualification__6+
                         Teachr_Qualification__7+
                         Teachr_Apontmt_Post__2+
                         Teachr_Apontmt_Post__3+
                         Teachr_Apontmt_Post__4+
                         Year_Start_teach_r, 
                       
                       data=student_test_2_rev_2,
                       clusters = ID_IE, se_type = "stata")

summary(regE_27_2)

###Table 7####

student_test_2_rev_end <- subset(student_test_2_rev,
                                 timing_end==1)

#Column 1-1

reg7_1_1 <- lm_robust(ztest_score_knowledge_2~
                        
                        treatment_2+
                        zscore_baseline_2018+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_1_1)

#Column 1-2

reg7_1_2 <- lm_robust(ztest_score_knowledge_2~
                        
                        treatment_2+
                        zscore_baseline_2018_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_1_2)

#Column 2-1

reg7_2_1 <- lm_robust(ztest_score_application_2~
                        
                        treatment_2+
                        zscore_baseline_2018+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_2_1)

#Column 2-2

reg7_2_2 <- lm_robust(ztest_score_application_2~
                        
                        treatment_2+
                        zscore_baseline_2018_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_2_2)

#Column 3-1

reg7_3_1 <- lm_robust(ztest_score_reasoning_2~
                        
                        treatment_2+
                        zscore_baseline_2018+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_3_1)

#Column 3-2

reg7_3_2 <- lm_robust(ztest_score_reasoning_2~
                        
                        treatment_2+
                        zscore_baseline_2018_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        Number_total_Students+
                        Number_G2_Students+
                        Multigrade_G2+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        Work_Shift_G2_both+
                        Teaching_other_subject_G2, 
                      
                      data=student_test_2_rev_end,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_3_2)

#Column4-1

student_test_2_rev_fus <- subset(student_test_2_rev,
                                 timing_fus==1)

reg7_4_1 <- lm_robust(ztest_score_knowledge_2~
                        
                        treatment_2+
                        zscore_baseline_2019+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_4_1)

#Column4-2

reg7_4_2 <- lm_robust(ztest_score_knowledge_2~
                        
                        treatment_2+
                        zscore_baseline_2019_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_4_2)

#Column5-1

reg7_5_1 <- lm_robust(ztest_score_application_2~
                        
                        treatment_2+
                        zscore_baseline_2019+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_5_1)

#Column5-2

reg7_5_2 <- lm_robust(ztest_score_application_2~
                        
                        treatment_2+
                        zscore_baseline_2019_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_5_2)

#Column6-1

reg7_6_1 <- lm_robust(ztest_score_reasoning_2~
                        
                        treatment_2+
                        zscore_baseline_2019+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_6_1)

#Column6-2

reg7_6_2 <- lm_robust(ztest_score_reasoning_2~
                        
                        treatment_2+
                        zscore_baseline_2019_2+
                        
                        department*urban.rural+
                        Age_r+Sex+No_elder+
                        No_younger+
                        Live_w_Parent_father+
                        Live_w_Parent_mother+
                        Live_w_Parent_none+
                        Shift_2018+
                        repeated_2019+
                        Shift+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        
                        Number_total_Students+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        School_Facility__2+
                        School_Facility__3+
                        School_Facility__4+
                        School_Facility__5+
                        School_Facility__6+
                        School_Facility__7+
                        School_Facility__8+
                        Internet_Place__3+
                        Meal_Provision+
                        Supplement_Class+
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20+
                        
                        Teacher_Sex+Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r,
                      
                      data=student_test_2_rev_fus,
                      clusters = ID_IE, se_type = "stata")

summary(reg7_6_2)

###Table 8####

#creating dummy variables for the heterogeneity analysis

student_test_2_rev$treatment_2_2018_pc1 <-
  c(student_test_2_rev$treatment_2_2018*
      student_test_2_rev$pc1)

student_test_2_rev$treatment_2_2019_pc1 <-
  c(student_test_2_rev$treatment_2_2019*
      student_test_2_rev$pc1)

#

student_test_2_rev$treatment_2_2018_zs <-
  c(student_test_2_rev$treatment_2_2018*
      student_test_2_rev$ztest_score_baseline)

student_test_2_rev$treatment_2_2019_zs <-
  c(student_test_2_rev$treatment_2_2019*
      student_test_2_rev$ztest_score_baseline)

#

student_test_2_rev$treatment_2_2018_zs_2 <-
  c(student_test_2_rev$treatment_2_2018*
      student_test_2_rev$ztest_score_baseline_exc_2_4)

student_test_2_rev$treatment_2_2019_zs_2 <-
  c(student_test_2_rev$treatment_2_2019*
      student_test_2_rev$ztest_score_baseline_exc_2_4)

#

student_test_2_rev$pc1_2018 <-
  c(student_test_2_rev$timing_end*
      student_test_2_rev$pc1)

student_test_2_rev$pc1_2019 <-
  c(student_test_2_rev$timing_fus*
      student_test_2_rev$pc1)

#

student_test_2_rev$treatment_2_2018_zs_2_pc <-
  c(student_test_2_rev$treatment_2_2018*
      student_test_2_rev$ztest_score_baseline_exc_2_4*
      student_test_2_rev$pc1)

student_test_2_rev$treatment_2_2019_zs_2_pc <-
  c(student_test_2_rev$treatment_2_2019*
      student_test_2_rev$ztest_score_baseline_exc_2_4*
      student_test_2_rev$pc1)

#

student_test_2_rev$pc1_2018_zs <-
  c(student_test_2_rev$timing_end*
      student_test_2_rev$pc1*
      student_test_2_rev$ztest_score_baseline_exc_2_4)

student_test_2_rev$pc1_2019_zs <-
  c(student_test_2_rev$timing_fus*
      student_test_2_rev$pc1*
      student_test_2_rev$ztest_score_baseline_exc_2_4)

#博士論文における依頼に対する追記（女子生徒に対する効果）

student_test_2_rev$female_d <-
  ifelse(student_test_2_rev$Sex==1,0,1)

student_test_2_rev$treatment_2_2018_sex <-
  c(student_test_2_rev$timing_end*
      student_test_2_rev$treatment_2*
      student_test_2_rev$female_d)

student_test_2_rev$treatment_2_2019_sex <-
  c(student_test_2_rev$timing_fus*
      student_test_2_rev$treatment_2*
      student_test_2_rev$female_d)

#

student_test_2_rev$sex_2018 <-
  c(student_test_2_rev$timing_end*
      student_test_2_rev$female_d)

student_test_2_rev$sex_2019 <-
  c(student_test_2_rev$timing_fus*
      student_test_2_rev$female_d)

#

student_test_2_rev$sex_2018 <-
  c(student_test_2_rev$timing_end*
      student_test_2_rev$Sex)

student_test_2_rev$sex_2019 <-
  c(student_test_2_rev$timing_fus*
      student_test_2_rev$Sex)

#Column 1-1

reg8_1_1 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        treatment_2_2018_pc1+
                        treatment_2_2019_pc1+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        repeated_2019+
                        
                        pc1_2018+
                        pc1_2019+
                        
                        Multigrade_G2+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg8_1_1)

#column 1-2

reg8_1_2 <- lm_robust(irt_score_rev~
                        treatment_2_2018+
                        treatment_2_2019+
                        treatment_2_2018_zs_2+
                        treatment_2_2019_zs_2+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        repeated_2019+
                        
                        pc1_2018+
                        pc1_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg8_1_2)

#Column 2

reg8_2 <- lm_robust(irt_score_rev~
                      treatment_2_2018+
                      treatment_2_2019+
                      treatment_2_2018_pc1+
                      treatment_2_2019_pc1+
                      treatment_2_2018_zs_2+
                      treatment_2_2019_zs_2+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018_2+
                      zscore_baseline_2019_2+
                      Age_r+
                      Sex+
                      Shift+
                      No_elder+
                      No_younger+
                      Student_own_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      repeated_2019+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      Multigrade_G2+
                      Multigrade_G2_2019+
                      Multigrade_G3_2019+
                      Number_total_Students+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      Highest_Degree_highschool+
                      Highest_Degree_professorate+
                      Highest_Degree_other+
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+School_Facility__4+
                      School_Facility__5+School_Facility__6+
                      School_Facility__7+School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_2_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg8_2)

#Column 3

reg8_3 <- lm_robust(irt_score_rev~
                      treatment_2_2018+
                      treatment_2_2019+
                      treatment_2_2018_pc1+
                      treatment_2_2019_pc1+
                      treatment_2_2018_zs_2+
                      treatment_2_2019_zs_2+
                      treatment_2_2018_zs_2_pc+
                      treatment_2_2019_zs_2_pc+
                      timing_fus+
                      department*urban.rural+
                      zscore_baseline_2018_2+
                      zscore_baseline_2019_2+
                      Age_r+
                      Sex+
                      Shift+
                      No_elder+
                      No_younger+
                      Student_own_desk_home+
                      Math_textbook_LY_e+
                      Math_Notebook_LY_e+
                      repeated_2019+
                      
                      pc1_2018+
                      pc1_2019+
                      
                      Multigrade_G2+
                      Multigrade_G2_2019+
                      Multigrade_G3_2019+
                      Number_total_Students+
                      
                      Teacher_Sex+
                      Teacher_Age+
                      Highest_Degree_highschool+
                      Highest_Degree_professorate+
                      Highest_Degree_other+
                      Teachr_Qualification__2+
                      Teachr_Qualification__3+
                      Teachr_Qualification__6+
                      Teachr_Qualification__7+
                      Teachr_Apontmt_Post__2+
                      Teachr_Apontmt_Post__3+
                      Teachr_Apontmt_Post__4+
                      Year_Start_teach_r+
                      
                      HM_Sex+
                      HM_Age+
                      Year_HM_ttl+
                      Year_HM_this_school+
                      
                      HM_Highest_Degree_highschool+
                      HM_Highest_Degree_master+
                      HM_Highest_Degree_professorate+
                      HM_Highest_Degree_other+
                      
                      School_Facility__2+
                      School_Facility__3+School_Facility__4+
                      School_Facility__5+School_Facility__6+
                      School_Facility__7+School_Facility__8+
                      
                      Donor_Project__2+
                      Donor_Project__3+
                      Donor_Project__4+
                      Donor_Project__5+
                      Donor_Project__6+
                      Donor_Project__7+
                      Donor_Project__8+
                      Donor_Project__9+
                      Donor_Project__10+
                      Donor_Project__11+
                      Donor_Project__12+
                      Donor_Project__13+
                      Donor_Project__14+
                      Donor_Project__15+
                      Donor_Project__16+
                      Donor_Project__17+
                      Donor_Project__18+
                      Donor_Project__19+
                      Donor_Project__20, 
                    
                    data=student_test_2_rev,
                    clusters = ID_IE, se_type = "stata")

summary(reg8_3)

###Table 9####

#column 3-1

student_test_2_rev$Time_Study_Home_G2

reg9_3_1 <- lm_robust(Time_Study_Home_G2~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

reg9_3_1
summary(reg9_3_1)

student_test_2_rev_end_c  <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2_2018==0)

mean(student_test_2_rev_end_c$Time_Study_Home_G2, na.rm=T)

linearHypothesis(reg9_3_1, "treatment_2_2018 - treatment_2_2019 = 0")

#column 3-2

reg9_3_2 <- lm_robust(Time_Study_Home_G2~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_panel,
                      clusters = ID_IE, se_type = "stata")

reg9_3_1
summary(reg9_3_2)

student_test_2_rev_end_c  <- subset(student_test_2_rev_panel,
                                    timing_end==1&
                                      treatment_2_2018==0)

mean(student_test_2_rev_end_c$Time_Study_Home_G2, na.rm=T)

linearHypothesis(reg9_3_2, "treatment_2_2018 - treatment_2_2019 = 0")

#column 4-1

student_test_2_rev$Method_Home_Study__3r <- 
  student_test_2_rev$Method_Home_Study__3*100

reg9_4_1 <- lm_robust(Method_Home_Study__3r~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev,
                      clusters = ID_IE, se_type = "stata")

summary(reg9_4_1)

student_test_2_rev_end_c  <- subset(student_test_2_rev,
                                    timing_end==1&
                                      treatment_2_2018==0)

mean(student_test_2_rev_end_c$Method_Home_Study__3)

linearHypothesis(reg9_4_1, "treatment_2_2018 - treatment_2_2019 = 0")

#column 4-2

student_test_2_rev_panel$Method_Home_Study__3r <- 
  student_test_2_rev_panel$Method_Home_Study__3*100

reg9_4_2 <- lm_robust(Method_Home_Study__3r~
                        treatment_2_2018+
                        treatment_2_2019+
                        timing_fus+
                        department*urban.rural+
                        zscore_baseline_2018_2+
                        zscore_baseline_2019_2+
                        Age_r+
                        Sex+
                        Shift+
                        No_elder+
                        No_younger+
                        Student_own_desk_home+
                        Math_textbook_LY_e+
                        Math_Notebook_LY_e+
                        Cooking_non_fire+
                        Item_House__1+
                        Item_House__2+
                        Item_House__3+
                        Item_House__4+
                        Item_House__5+
                        Item_House__6+
                        Item_House__7+
                        Item_House__8+
                        repeated_2019+
                        
                        Multigrade_G2+
                        Multigrade_G2_2019+
                        Multigrade_G3_2019+
                        Number_total_Students+
                        
                        Teacher_Sex+
                        Teacher_Age+
                        Highest_Degree_highschool+
                        Highest_Degree_professorate+
                        Highest_Degree_other+
                        Teachr_Qualification__2+
                        Teachr_Qualification__3+
                        Teachr_Qualification__6+
                        Teachr_Qualification__7+
                        Teachr_Apontmt_Post__2+
                        Teachr_Apontmt_Post__3+
                        Teachr_Apontmt_Post__4+
                        Year_Start_teach_r+
                        
                        HM_Sex+
                        HM_Age+
                        Year_HM_ttl+
                        Year_HM_this_school+
                        
                        HM_Highest_Degree_highschool+
                        HM_Highest_Degree_master+
                        HM_Highest_Degree_professorate+
                        HM_Highest_Degree_other+
                        
                        School_Facility__2+
                        School_Facility__3+School_Facility__4+
                        School_Facility__5+School_Facility__6+
                        School_Facility__7+School_Facility__8+
                        
                        Donor_Project__2+
                        Donor_Project__3+
                        Donor_Project__4+
                        Donor_Project__5+
                        Donor_Project__6+
                        Donor_Project__7+
                        Donor_Project__8+
                        Donor_Project__9+
                        Donor_Project__10+
                        Donor_Project__11+
                        Donor_Project__12+
                        Donor_Project__13+
                        Donor_Project__14+
                        Donor_Project__15+
                        Donor_Project__16+
                        Donor_Project__17+
                        Donor_Project__18+
                        Donor_Project__19+
                        Donor_Project__20, 
                      
                      data=student_test_2_rev_panel,
                      clusters = ID_IE, se_type = "stata")

summary(reg9_4_2)

student_test_2_rev_end_c  <- subset(student_test_2_rev_panel,
                                    timing_end==1&
                                      treatment_2_2018==0)

mean(student_test_2_rev_end_c$Method_Home_Study__3)

linearHypothesis(reg9_4_2, "treatment_2_2018 - treatment_2_2019 = 0")

###Figure1-1####

student_test_2_baseline$treatment_2r <- student_test_2_baseline$treatment_2

student_test_2_baseline$treatment_2r <- 
  replace(student_test_2_baseline$treatment_2r, 
          which(student_test_2_baseline$treatment_2r==0), "Control")

student_test_2_baseline$treatment_2r <- 
  replace(student_test_2_baseline$treatment_2r, 
          which(student_test_2_baseline$treatment_2r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

student_test_2_baseline$ztest_score_baseline

g <- ggplot(student_test_2_baseline)
g <- g + geom_line(aes(ztest_score_baseline, 
                       group=treatment_2r, color=treatment_2r,
                       linetype=treatment_2r), size=0.5, stat=("density"),
                   adjust=1.5)
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Z Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 8,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position=c(0.8, 0.9))
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "z_score_dens_baseline_g2.png", plot = g,
       dpi = 1000, width = 6, height = 4)

###Figure1-2####

g <- ggplot(student_test_2_baseline)
g <- g + geom_line(aes(ztest_score_baseline_exc_2_4, 
                       group=treatment_2r, color=treatment_2r,
                       linetype=treatment_2r), size=0.5, stat=("density"),
                   adjust=1.5)
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("Z Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position=c(0.8, 0.9))
g <- g + theme(legend.key.width = unit(2,"cm"))
plot(g)
ggsave(file = "Z_score_dens_baseline_g2_2.png", plot = g,
       dpi = 1000, width = 6, height = 4)

###Figure A in Appendix 4####
###compute composite index of household economic status

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_baseline$Cooking_Equipment__1_r <-
  ifelse(student_test_2_baseline$Cooking_Equipment__1==1,
         0,1)

student_test_2_p <- cbind(student_test_2_baseline$Item_House__1,
                          student_test_2_baseline$Item_House__2,
                          student_test_2_baseline$Item_House__3,
                          student_test_2_baseline$Item_House__4,
                          student_test_2_baseline$Item_House__5,
                          student_test_2_baseline$Item_House__6,
                          student_test_2_baseline$Item_House__7,
                          student_test_2_baseline$Item_House__8,
                          student_test_2_baseline$Cooking_Equipment__1_r)

row.names(student_test_2_p) <- c(student_test_2_baseline$ID_IE_test)

student_test_2_p <- na.omit(student_test_2_p)

result <- prcomp(student_test_2_p, scale=T, na.rm=T, center=T)

summary(result)

student_test_2_p <- data.frame(student_test_2_p)

result2 <- round(result$rotation, 3)
result3 <- round(result$x, digits=3)

pc1 <- result$x[,1] # 第一主成分得点
pc2 <- result$x[,2] # 第二主成分得点
pc3 <- result$x[,3] # 第二主成分得点

student_id <- rownames(student_test_2_p)
as.numeric(student_id)

student_test_2_p <- cbind(student_id, student_test_2_p, pc1, pc2, pc3)

row_name <- rbind("smartphone", "Computer", "Refrig", "Car", "TV", "Water", "Electricity",
                  "Toilet", "Wood")

cor_pc1 <- rbind(cor(student_test_2_p$pc1, student_test_2_p$X1),
                 cor(student_test_2_p$pc1, student_test_2_p$X2),
                 cor(student_test_2_p$pc1, student_test_2_p$X3),
                 cor(student_test_2_p$pc1, student_test_2_p$X4),
                 cor(student_test_2_p$pc1, student_test_2_p$X5),
                 cor(student_test_2_p$pc1, student_test_2_p$X6),
                 cor(student_test_2_p$pc1, student_test_2_p$X7),
                 cor(student_test_2_p$pc1, student_test_2_p$X8),
                 cor(student_test_2_p$pc1, student_test_2_p$X9))

cor_pc2 <- rbind(cor(student_test_2_p$pc2, student_test_2_p$X1),
                 cor(student_test_2_p$pc2, student_test_2_p$X2),
                 cor(student_test_2_p$pc2, student_test_2_p$X3),
                 cor(student_test_2_p$pc2, student_test_2_p$X4),
                 cor(student_test_2_p$pc2, student_test_2_p$X5),
                 cor(student_test_2_p$pc2, student_test_2_p$X6),
                 cor(student_test_2_p$pc2, student_test_2_p$X7),
                 cor(student_test_2_p$pc2, student_test_2_p$X8),
                 cor(student_test_2_p$pc2, student_test_2_p$X9))

cor_pc3 <- rbind(cor(student_test_2_p$pc3, student_test_2_p$X1),
                 cor(student_test_2_p$pc3, student_test_2_p$X2),
                 cor(student_test_2_p$pc3, student_test_2_p$X3),
                 cor(student_test_2_p$pc3, student_test_2_p$X4),
                 cor(student_test_2_p$pc3, student_test_2_p$X5),
                 cor(student_test_2_p$pc3, student_test_2_p$X6),
                 cor(student_test_2_p$pc3, student_test_2_p$X7),
                 cor(student_test_2_p$pc3, student_test_2_p$X8),
                 cor(student_test_2_p$pc3, student_test_2_p$X9))

cor_table <- cbind.data.frame(row_name, cor_pc1, cor_pc2, cor_pc3)

student_test_2_baseline <- merge(student_test_2_baseline, student_test_2_p,
                                 by.x=c("ID_IE_test"), 
                                 by.y=c("student_id"), all=F)

###Figure A in Appendix 4

student_test_2_baseline$treatment_2r <- student_test_2_baseline$treatment_2

student_test_2_baseline$treatment_2r <- 
  replace(student_test_2_baseline$treatment_2r, 
          which(student_test_2_baseline$treatment_2r==0), "Control")

student_test_2_baseline$treatment_2r <- 
  replace(student_test_2_baseline$treatment_2r, 
          which(student_test_2_baseline$treatment_2r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_test_2_baseline)
g <- g + geom_line(aes(pc1, 
                       group=treatment_2r, color=treatment_2r,
                       linetype=treatment_2r), size=0.5, stat="density")
g <- g + scale_colour_manual(values = cols)
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + xlab("First Principal Component") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.text = element_text(size=9))
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position=c(0.2, 0.9))
g <- g + theme(legend.position=c("bottom"))
g <- g + guides(color = guide_legend(override.aes = list(size = 0.5)))

plot(g)
ggsave(file = "pca_asset_g2.png", dpi = 1000, 
       width = 6, height = 4, plot = g)


###Figure2####

pc1_q1 <- quantile(student_test_2_baseline$pc1,
                   prob = c(0.2))

pc1_q2 <- quantile(student_test_2_baseline$pc1,
                   prob = c(0.4))

pc1_q3 <- quantile(student_test_2_baseline$pc1,
                   prob = c(0.6))

pc1_q4 <- quantile(student_test_2_baseline$pc1,
                   prob = c(0.8))

student_test_2_baseline$household_economic_st <- c(0)

student_test_2_baseline$household_economic_st <- 
  ifelse(student_test_2_baseline$pc1 <= pc1_q1,
         "Q1", student_test_2_baseline$household_economic_st)

student_test_2_baseline$household_economic_st <- 
  ifelse(student_test_2_baseline$pc1 > pc1_q1 &
           student_test_2_baseline$pc1 <= pc1_q2,
         "Q2", student_test_2_baseline$household_economic_st)

student_test_2_baseline$household_economic_st <- 
  ifelse(student_test_2_baseline$pc1 > pc1_q2 &
           student_test_2_baseline$pc1 <= pc1_q3,
         "Q3", student_test_2_baseline$household_economic_st)

student_test_2_baseline$household_economic_st <- 
  ifelse(student_test_2_baseline$pc1 > pc1_q3 &
           student_test_2_baseline$pc1 <= pc1_q4,
         "Q4", student_test_2_baseline$household_economic_st)

student_test_2_baseline$household_economic_st <- 
  ifelse(student_test_2_baseline$pc1 > pc1_q4,
         "Q5", student_test_2_baseline$household_economic_st)

g <- ggplot(aes(x=household_economic_st, y = total_score_2), 
            data=student_test_2_baseline)

g <- g + xlab("Student household economic status") 
g <- g + ylab("Total baseline score") 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(axis.title.x = element_text(size = 9))

g <- g + geom_boxplot()

plot(g)

ggsave(file = "figure_boxplot_es_g2.png", dpi = 1000, 
       width = 6, height = 4, plot = g)


###Figure3####

student_test_2_end <- subset(student_test_2_rev,
                             timing_end==1)

student_test_2_end$treatment_2r <- student_test_2_end$treatment_2

student_test_2_end$treatment_2r <- replace(student_test_2_end$treatment_2r, 
                                           which(student_test_2_end$treatment_2r==0), "Control")

student_test_2_end$treatment_2r <- replace(student_test_2_end$treatment_2r, 
                                           which(student_test_2_end$treatment_2r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_test_2_end)
g <- g + geom_line(aes(irt_score_rev, 
                       group=treatment_2r, color=treatment_2r,
                       linetype=treatment_2r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-2,4,by=1),limits=c(-2,4.5))
g <- g + xlab("IRT Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_plot_end.png", dpi = 1000, 
       width = 6, height = 4, plot = g)


###Figure4####

student_test_2_fus <- subset(student_test_2_rev,
                             timing_fus==1)

student_test_2_fus$treatment_2r <- student_test_2_fus$treatment_2

student_test_2_fus$treatment_2r <- replace(student_test_2_fus$treatment_2r, 
                                           which(student_test_2_fus$treatment_2r==0), "Control")

student_test_2_fus$treatment_2r <- replace(student_test_2_fus$treatment_2r, 
                                           which(student_test_2_fus$treatment_2r=="1"), "Treatment")

cols <- c("Treatment"="red", "Control"="blue") 

g <- ggplot(student_test_2_fus)
g <- g + geom_line(aes(irt_score_rev, 
                       group=treatment_2r, color=treatment_2r,
                       linetype=treatment_2r), size=0.5, stat=("density"))
g <- g + scale_linetype_manual(values=c("Treatment"="solid", 
                                        "Control"="dashed"))
g <- g + scale_colour_manual(values = cols)
g <- g + scale_x_continuous(breaks=seq(-2,4,by=1),limits=c(-2,4.5))
g <- g + xlab("IRT Score") 
g <- g + ylab("Density") 
g <- g + theme(axis.title.x = element_text(size = 9)) 
g <- g + theme(axis.title.y = element_text(angle = 0, 
                                           colour = "black",size = 9,
                                           vjust = 1)) 
g <- g + theme(legend.title = element_blank())
g <- g + theme(legend.position="bottom")
g <- g + theme(legend.key.width = unit(2,"cm"))
g <- g + theme(legend.text = element_text(size=8))
plot(g)
ggsave(file = "figure_plot_fus.png", dpi = 1000, 
       width = 6, height = 4, plot = g)

###Figure7####

student_test_2_rev_panel_end_t <-
  subset(student_test_2_rev_panel,
         timing_end==1&
           treatment_2==1)

student_test_2_rev_panel_fus_t <-
  subset(student_test_2_rev_panel,
         timing_fus==1&
           treatment_2==1)

student_test_2_rev_panel_end_t <-
  subset(student_test_2_rev_panel_end_t,
         select=c(ID_IE_test,
                  irt_score_rev))

student_test_2_rev_panel_fus_t <-
  subset(student_test_2_rev_panel_fus_t,
         select=c(ID_IE_test,
                  irt_score_rev))

names(student_test_2_rev_panel_end_t) <-
  c("ID_IE_test",
    "irt_score_rev_end")

names(student_test_2_rev_panel_fus_t) <-
  c("ID_IE_test",
    "irt_score_rev_fus")

student_test_2_rev_panel_t_r <- merge(student_test_2_rev_panel_end_t, 
                                      student_test_2_rev_panel_fus_t,
                                      by.x=c("ID_IE_test"), 
                                      by.y=c("ID_IE_test"), all=F)

student_test_2_rev_panel_t_r$diff_irt_score <- c(
  student_test_2_rev_panel_t_r$irt_score_rev_fus-
    student_test_2_rev_panel_t_r$irt_score_rev_end)

#draw figure 7

g <- ggplot(student_test_2_rev_panel_t_r, 
            aes(x = irt_score_rev_end, 
                y = diff_irt_score))

g <- g + geom_jitter(alpha=0.5, color="blue")
g <- g + coord_fixed()
g <- g + scale_x_continuous(breaks=seq(-2,3,by=1), 
                            limits=c(-2.5,3.5))
g <- g + scale_y_continuous(breaks=seq(-3,5,by=1), 
                            limits=c(-3.5,4))
g <- g + theme(axis.title.x = element_text(size=9))
g <- g + theme(axis.text.x = element_text(size=9))
g <- g + theme(axis.title.y = element_text(size=9))
g <- g + theme(axis.text.y = element_text(size=9))
g <- g + geom_vline(xintercept = 0, linetype = "dotted")
g <- g + geom_hline(yintercept = 0, linetype = "dotted")
g <- g + xlab("IRT scores (end-line)")
g <- g + ylab("Difference in IRT scores")
plot(g)
ggsave(file = "plot_irt_diff_t_r.png", dpi = 1000, 
       width = 4, height = 4, plot = g)

###Appendix 3-2#####

#Estimate item parameters

student_test_2_end <- 
  read.csv("student_test_2_endline.csv", header=T, stringsAsFactors = F)

student_test_2_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q1_Correct_or_Wrong_G2" ) ] <- "Q1_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q2_Correct_or_Wrong_G2" ) ] <- "Q2_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q3_Correct_or_Wrong_G2" ) ] <- "Q3_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q4_Correct_or_Wrong_G2" ) ] <- "Q4_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q5_Correct_or_Wrong_G2" ) ] <- "Q5_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q6_Correct_or_Wrong_G2" ) ] <- "Q6_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q7_Correct_or_Wrong_G2" ) ] <- "Q7_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q8_Correct_or_Wrong_G2" ) ] <- "Q8_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q9_Correct_or_Wrong_G2" ) ] <- "Q9_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q10_Correct_or_Wrong_G2" ) ] <- "Q10_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q11_Correct_or_Wrong_G2" ) ] <- "Q11_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q12_Correct_or_Wrong_G2" ) ] <- "Q12_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q13_Correct_or_Wrong_G2" ) ] <- "Q13_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q14_Correct_or_Wrong_G2" ) ] <- "Q14_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q15_Correct_or_Wrong_G2" ) ] <- "Q15_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q16_Correct_or_Wrong_G2" ) ] <- "Q16_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q17_Correct_or_Wrong_G2" ) ] <- "Q17_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q18_Correct_or_Wrong_G2" ) ] <- "Q18_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q19_Correct_or_Wrong_G2" ) ] <- "Q19_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q20_Correct_or_Wrong_G2" ) ] <- "Q20_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q21_Correct_or_Wrong_G2" ) ] <- "Q21_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q22_Correct_or_Wrong_G2" ) ] <- "Q22_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q23_Correct_or_Wrong_G2" ) ] <- "Q23_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q24_Correct_or_Wrong_G2" ) ] <- "Q24_Correct_or_Wrong_G2_fus"

names(student_test_2_fus)[which(names(student_test_2_fus)==
                                  "Q25_Correct_or_Wrong_G2" ) ] <- "Q25_Correct_or_Wrong_G2_fus"

student_test_2_end <- subset(student_test_2_end, 
                             select=c(ID_IE_test,
                                      Q1_Correct_or_Wrong_G2,
                                      Q2_Correct_or_Wrong_G2,
                                      Q3_Correct_or_Wrong_G2,
                                      Q4_Correct_or_Wrong_G2,
                                      Q5_Correct_or_Wrong_G2,
                                      Q6_Correct_or_Wrong_G2,
                                      Q7_Correct_or_Wrong_G2,
                                      Q8_Correct_or_Wrong_G2,
                                      Q9_Correct_or_Wrong_G2,
                                      Q10_Correct_or_Wrong_G2,
                                      Q11_Correct_or_Wrong_G2,
                                      Q12_Correct_or_Wrong_G2,
                                      Q13_Correct_or_Wrong_G2,
                                      Q14_Correct_or_Wrong_G2,
                                      Q15_Correct_or_Wrong_G2,
                                      Q16_Correct_or_Wrong_G2,
                                      Q17_Correct_or_Wrong_G2,
                                      Q18_Correct_or_Wrong_G2,
                                      Q19_Correct_or_Wrong_G2,
                                      Q20_Correct_or_Wrong_G2))

student_test_2_fus <- subset(student_test_2_fus, 
                             select=c(ID_IE_test,
                                      Q1_Correct_or_Wrong_G2_fus,
                                      Q2_Correct_or_Wrong_G2_fus,
                                      Q3_Correct_or_Wrong_G2_fus,
                                      Q4_Correct_or_Wrong_G2_fus,
                                      Q5_Correct_or_Wrong_G2_fus,
                                      Q6_Correct_or_Wrong_G2_fus,
                                      Q7_Correct_or_Wrong_G2_fus,
                                      Q8_Correct_or_Wrong_G2_fus,
                                      Q9_Correct_or_Wrong_G2_fus,
                                      Q10_Correct_or_Wrong_G2_fus,
                                      Q11_Correct_or_Wrong_G2_fus,
                                      Q12_Correct_or_Wrong_G2_fus,
                                      Q13_Correct_or_Wrong_G2_fus,
                                      Q14_Correct_or_Wrong_G2_fus,
                                      Q15_Correct_or_Wrong_G2_fus,
                                      Q16_Correct_or_Wrong_G2_fus,
                                      Q17_Correct_or_Wrong_G2_fus,
                                      Q18_Correct_or_Wrong_G2_fus,
                                      Q19_Correct_or_Wrong_G2_fus,
                                      Q20_Correct_or_Wrong_G2_fus,
                                      Q21_Correct_or_Wrong_G2_fus,
                                      Q22_Correct_or_Wrong_G2_fus,
                                      Q23_Correct_or_Wrong_G2_fus,
                                      Q24_Correct_or_Wrong_G2_fus,
                                      Q25_Correct_or_Wrong_G2_fus))

student_test_2_r <- merge(student_test_2_end, student_test_2_fus,
                          by.x=c("ID_IE_test"), 
                          by.y=c("ID_IE_test"), all=F)

test_data_end <- cbind.data.frame(student_test_2_r$Q1_Correct_or_Wrong_G2,
                                  student_test_2_r$Q2_Correct_or_Wrong_G2,
                                  student_test_2_r$Q3_Correct_or_Wrong_G2,
                                  student_test_2_r$Q4_Correct_or_Wrong_G2,
                                  student_test_2_r$Q5_Correct_or_Wrong_G2,
                                  student_test_2_r$Q6_Correct_or_Wrong_G2,
                                  student_test_2_r$Q7_Correct_or_Wrong_G2,
                                  student_test_2_r$Q8_Correct_or_Wrong_G2,
                                  student_test_2_r$Q9_Correct_or_Wrong_G2,
                                  student_test_2_r$Q10_Correct_or_Wrong_G2,
                                  student_test_2_r$Q11_Correct_or_Wrong_G2,
                                  student_test_2_r$Q12_Correct_or_Wrong_G2,
                                  student_test_2_r$Q13_Correct_or_Wrong_G2,
                                  student_test_2_r$Q14_Correct_or_Wrong_G2,
                                  student_test_2_r$Q15_Correct_or_Wrong_G2,
                                  student_test_2_r$Q16_Correct_or_Wrong_G2,
                                  student_test_2_r$Q17_Correct_or_Wrong_G2,
                                  student_test_2_r$Q18_Correct_or_Wrong_G2,
                                  student_test_2_r$Q19_Correct_or_Wrong_G2,
                                  student_test_2_r$Q20_Correct_or_Wrong_G2)

test_data_fus <- cbind.data.frame(student_test_2_r$Q1_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q2_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q3_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q4_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q5_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q6_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q7_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q8_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q9_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q10_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q11_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q12_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q13_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q14_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q15_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q16_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q17_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q18_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q19_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q20_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q21_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q22_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q23_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q24_Correct_or_Wrong_G2_fus,
                                  student_test_2_r$Q25_Correct_or_Wrong_G2_fus)

para.2PL_end <-est(resp=test_data_end,model="2PL",engine="ltm")
para.2PL_fus <-est(resp=test_data_fus,model="2PL",engine="ltm")

###end-line test items

#item discrimination
as.data.frame(para.2PL_end$est[,1])

#item difficulty
as.data.frame(para.2PL_end$est[,2])

###follow-up test items

#item discrimination
as.data.frame(para.2PL_fus$est[,1])

#item difficulty
as.data.frame(para.2PL_fus$est[,2])

###Footnotes####

###footnote 11

rm (list = ls(all=TRUE))

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

library(ICC)

ICC_esmate_impact_evaluation <- 
  ICCest(x = id_school, y = ztest_score_baseline_exc_2_4, data = student_test_2_baseline)

student_test_2_baseline$ztest_score_baseline_exc_2_4

###footnote 16

rm (list = ls(all=TRUE))

student_test_2_baseline <- 
  read.csv("student_test_2_baseline.csv", header=T, stringsAsFactors = F)

student_test_2_baseline_t <- subset(student_test_2_baseline, treatment_2==1)
student_test_2_baseline_c <- subset(student_test_2_baseline, treatment_2==0)

student_test_2_baseline_t$ztest_score_baseline

ad_test(student_test_2_baseline_t$ztest_score_baseline,
        student_test_2_baseline_c$ztest_score_baseline,
        nboots = 20000)

ad_test(student_test_2_baseline_t$ztest_score_baseline_exc_2_4,
        student_test_2_baseline_c$ztest_score_baseline_exc_2_4,
        nboots = 20000)

###footnotes 21 and 22

#implement code in the section of Prepare dataframe to estimate impacts for students

#footnote 21

student_test_2_rev_end_t <- subset(student_test_2_rev,
                                   treatment_2_2018==1&
                                     timing_end==1)

student_test_2_rev_end_c <- subset(student_test_2_rev,
                                   treatment_2_2018==0&
                                     timing_end==1)

ad_test(student_test_2_rev_end_t$irt_score_rev,
        student_test_2_rev_end_c$irt_score_rev,
        nboots = 20000)

#footnote 22

student_test_2_rev_fus_t <- subset(student_test_2_rev,
                                   treatment_2_2019==1&
                                     timing_fus==1)

student_test_2_rev_fus_c <- subset(student_test_2_rev,
                                   treatment_2_2019==0&
                                     timing_fus==1)

ad_test(student_test_2_rev_fus_t$irt_score_rev,
        student_test_2_rev_fus_c$irt_score_rev,
        nboots = 20000)

###footnote 31
#implement code related to Figure 7 before implementing the following code

student_test_2_fus <- 
  read.csv("student_test_2_fus.csv", header=T, stringsAsFactors = F)

student_test_2_fus_t <-
  subset(student_test_2_fus,
         treatment_2==1)

#

student_test_2_rev_panel_fus_t <- merge(student_test_2_fus_t, 
                                        student_test_2_rev_panel_t_r,
                                        by.x=c("ID_IE_test"), 
                                        by.y=c("ID_IE_test"), all=F)

student_test_2_rev_panel_fus_t$multi_assign_p <- 
  ifelse(student_test_2_rev_panel_fus_t$Teaching_Grade_f__3==1&
           (student_test_2_rev_panel_fus_t$Teaching_Grade_f__1==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__2==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__4==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__5==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__6==1),
         1, 0) 

student_test_2_rev_panel_fus_t$multi_assign_ls <- 
  ifelse(student_test_2_rev_panel_fus_t$Teaching_Grade_f__3==1&
           (student_test_2_rev_panel_fus_t$Teaching_Grade_f__7==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__8==1|
              student_test_2_rev_panel_fus_t$Teaching_Grade_f__9==1),
         1, 0) 

#

regf_30 <- lm_robust(diff_irt_score~
                       urban.rural*
                       department+
                       same_teacher_1819+
                       
                       ztest_score_baseline_exc_2_4+
                       Age_r+
                       Sex+
                       Shift+
                       No_elder+
                       No_younger+
                       Student_desk_home+
                       
                       Cooking_non_fire+
                       Item_House__1+
                       Item_House__2+
                       Item_House__3+
                       Item_House__4+
                       Item_House__5+
                       Item_House__6+
                       Item_House__7+
                       Item_House__8+
                       
                       Multigrade_G3_2019+
                       repeated_2019+
                       multi_assign_p+
                       multi_assign_ls+
                       
                       Teacher_Sex+
                       Teacher_Age+
                       Highest_Degree_highschool+
                       Highest_Degree_professorate+
                       Highest_Degree_other+
                       Teachr_Qualification__2+
                       Teachr_Qualification__3+
                       Teachr_Qualification__6+
                       Teachr_Qualification__7+
                       Teachr_Apontmt_Post__2+
                       Teachr_Apontmt_Post__3+
                       Teachr_Apontmt_Post__4+
                       Year_Start_teach_r+
                       
                       HM_Sex+
                       HM_Age+
                       Year_HM_ttl+
                       Year_HM_this_school+
                       
                       HM_Highest_Degree_highschool+
                       HM_Highest_Degree_master+
                       HM_Highest_Degree_professorate+
                       
                       School_Facility__2+
                       School_Facility__3+School_Facility__4+
                       School_Facility__5+School_Facility__6+
                       School_Facility__7+School_Facility__8+
                       
                       Donor_Project__2+
                       Donor_Project__3+
                       Donor_Project__4+
                       Donor_Project__5+
                       Donor_Project__6+
                       Donor_Project__7+
                       Donor_Project__8+
                       Donor_Project__9+
                       Donor_Project__10+
                       Donor_Project__11+
                       Donor_Project__12+
                       Donor_Project__13+
                       Donor_Project__14+
                       Donor_Project__15+
                       Donor_Project__16+
                       Donor_Project__17+
                       Donor_Project__18+
                       Donor_Project__19+
                       Donor_Project__20, 
                     
                     data=student_test_2_rev_panel_fus_t,
                     clusters = id_school, se_type = "stata")

summary(regf_30)

mean(student_test_2_rev_panel_fus_t$multi_assign_p)

###compute q-value####

#Table: IRT scores
reg5_1_1r <- tidy(reg5_1_1, conf.int = F)
reg5_1_1r_pval_1 <- reg5_1_1r[2,5]
reg5_1_1r_pval_2 <- reg5_1_1r[3,5]

reg5_1_2r <- tidy(reg5_1_2, conf.int = F)
reg5_1_2r_pval_1 <- reg5_1_2r[2,5]
reg5_1_2r_pval_2 <- reg5_1_2r[3,5]

reg5_1_3r <- tidy(reg5_1_3, conf.int = F)
reg5_1_3r_pval_1 <- reg5_1_3r[2,5]
reg5_1_3r_pval_2 <- reg5_1_3r[3,5]

reg5_2_1r <- tidy(reg5_2_1, conf.int = F)
reg5_2_1r_pval_1 <- reg5_2_1r[2,5]
reg5_2_1r_pval_2 <- reg5_2_1r[3,5]

reg5_2_2r <- tidy(reg5_2_2, conf.int = F)
reg5_2_2r_pval_1 <- reg5_2_2r[2,5]
reg5_2_2r_pval_2 <- reg5_2_2r[3,5]

reg5_2_3r <- tidy(reg5_2_3, conf.int = F)
reg5_2_3r_pval_1 <- reg5_2_3r[2,5]
reg5_2_3r_pval_2 <- reg5_2_3r[3,5]

#

reg6_4_1r <- tidy(reg6_4_1, conf.int = F)
reg6_4_1r_pval_1 <- reg6_4_1r[2,5]
reg6_4_1r_pval_2 <- reg6_4_1r[3,5]

reg6_4_2r <- tidy(reg6_4_2, conf.int = F)
reg6_4_2r_pval_1 <- reg6_4_2r[2,5]
reg6_4_2r_pval_2 <- reg6_4_2r[3,5]

reg6_6_1r <- tidy(reg6_6_1, conf.int = F)
reg6_6_1r_pval_1 <- reg6_6_1r[2,5]
reg6_6_1r_pval_2 <- reg6_6_1r[3,5]

reg6_6_2r <- tidy(reg6_6_2, conf.int = F)
reg6_6_2r_pval_1 <- reg6_6_2r[2,5]
reg6_6_2r_pval_2 <- reg6_6_2r[3,5]

#

reg8_3r <- tidy(reg8_3, conf.int = F)
reg8_3r_pval_1 <- reg8_3r[2,5]
reg8_3r_pval_2 <- reg8_3r[3,5]
reg8_3r_pval_3 <- reg8_3r[4,5]
reg8_3r_pval_4 <- reg8_3r[5,5]
reg8_3r_pval_5 <- reg8_3r[6,5]
reg8_3r_pval_6 <- reg8_3r[7,5]
reg8_3r_pval_7 <- reg8_3r[8,5]
reg8_3r_pval_8 <- reg8_3r[9,5]

#

pvalues <-c(reg5_1_1r_pval_1,
            reg5_1_1r_pval_2,
            
            reg5_1_2r_pval_1,
            reg5_1_2r_pval_2,
            
            reg5_1_3r_pval_1,
            reg5_1_3r_pval_2,
            
            reg5_2_1r_pval_1,
            reg5_2_1r_pval_2,
            
            reg5_2_2r_pval_1,
            reg5_2_2r_pval_2,
            
            reg5_2_3r_pval_1,
            reg5_2_3r_pval_2,
            
            reg6_4_1r_pval_1,
            reg6_4_1r_pval_2,
            
            reg6_6_1r_pval_1,
            reg6_6_1r_pval_2,
            
            reg6_4_2r_pval_1,
            reg6_4_2r_pval_2,
            
            reg6_6_2r_pval_1,
            reg6_6_2r_pval_2,
            
            reg8_3r_pval_1,
            reg8_3r_pval_2,
            reg8_3r_pval_3,
            reg8_3r_pval_4,
            reg8_3r_pval_5,
            reg8_3r_pval_6,
            reg8_3r_pval_7,
            reg8_3r_pval_8)

pvalue_name <- c("reg5_1_1r_2018",
                 "reg5_1_1r_2019",
                 
                 "reg5_1_2r_2018",
                 "reg5_1_2r_2019",
                 
                 "reg5_1_3r_2018",
                 "reg5_1_3r_2019",
                 
                 "reg5_2_1r_2018",
                 "reg5_2_1r_2019",
                 
                 "reg5_2_2r_2018",
                 "reg5_2_2r_2019",
                 
                 "reg5_2_3r_2018",
                 "reg5_2_3r_2019",
                 
                 "reg6_4_1r_2018",
                 "reg6_4_1r_2019",
                 
                 "reg6_6_1r_2018",
                 "reg6_6_1r_2019",
                 
                 "reg6_4_2r_2018",
                 "reg6_4_2r_2019",
                 
                 "reg6_6_2r_2018",
                 "reg6_6_2r_2019",
                 
                 "reg8_3r_2018",
                 "reg8_3r_2019",
                 
                 "reg8_3r_2018_pc1",
                 "reg8_3r_2019_pc1",
                 
                 "reg8_3r_2018_zs",
                 "reg8_3r_2019_zs",
                 
                 "reg8_3r_2018_zs_pc1",
                 "reg8_3r_2019_zs_pc1")

qvalue <- p.adjust(pvalues,method="fdr")

pvalues_df <- cbind.data.frame(pvalue_name,
                               pvalues,
                               qvalue)

write.csv(pvalues_df, "pvalues_df.csv", row.names = F)

#Table Z scores

reg7_1_1r <- tidy(reg7_1_1, conf.int = F)
reg7_1_1r_pval_1 <- reg7_1_1r[2,5]

reg7_1_2r <- tidy(reg7_1_2, conf.int = F)
reg7_1_2r_pval_1 <- reg7_1_2r[2,5]

reg7_2_1r <- tidy(reg7_2_1, conf.int = F)
reg7_2_1r_pval_1 <- reg7_2_1r[2,5]

reg7_2_2r <- tidy(reg7_2_2, conf.int = F)
reg7_2_2r_pval_1 <- reg7_2_2r[2,5]

reg7_3_1r <- tidy(reg7_3_1, conf.int = F)
reg7_3_1r_pval_1 <- reg7_3_1r[2,5]

reg7_3_2r <- tidy(reg7_3_2, conf.int = F)
reg7_3_2r_pval_1 <- reg7_3_2r[2,5]

reg7_4_1r <- tidy(reg7_4_1, conf.int = F)
reg7_4_1r_pval_1 <- reg7_4_1r[2,5]

reg7_4_2r <- tidy(reg7_4_2, conf.int = F)
reg7_4_2r_pval_1 <- reg7_4_2r[2,5]

reg7_5_1r <- tidy(reg7_5_1, conf.int = F)
reg7_5_1r_pval_1 <- reg7_5_1r[2,5]

reg7_5_2r <- tidy(reg7_5_2, conf.int = F)
reg7_5_2r_pval_1 <- reg7_5_2r[2,5]

reg7_6_1r <- tidy(reg7_6_1, conf.int = F)
reg7_6_1r_pval_1 <- reg7_6_1r[2,5]

reg7_6_2r <- tidy(reg7_6_2, conf.int = F)
reg7_6_2r_pval_1 <- reg7_6_2r[2,5]

regE_4_1_Kr <- tidy(regE_4_1_K, conf.int = F)
regE_4_1_Kr_pval_1 <- regE_4_1_Kr[2,5]

regE_4_1_Ar <- tidy(regE_4_1_A, conf.int = F)
regE_4_1_Ar_pval_1 <- regE_4_1_Ar[2,5]

regE_4_1_Rr <- tidy(regE_4_1_R, conf.int = F)
regE_4_1_Rr_pval_1 <- regE_4_1_Rr[2,5]

regE_5_1_Kr <- tidy(regE_5_1_K, conf.int = F)
regE_5_1_Kr_pval_1 <- regE_5_1_Kr[2,5]

regE_5_1_Ar <- tidy(regE_5_1_A, conf.int = F)
regE_5_1_Ar_pval_1 <- regE_5_1_Ar[2,5]

regE_5_1_Rr <- tidy(regE_5_1_R, conf.int = F)
regE_5_1_Rr_pval_1 <- regE_5_1_Rr[2,5]

regE_4_2_Kr <- tidy(regE_4_2_K, conf.int = F)
regE_4_2_Kr_pval_1 <- regE_4_2_Kr[2,5]

regE_5_2_Kr <- tidy(regE_5_2_K, conf.int = F)
regE_5_2_Kr_pval_1 <- regE_5_2_Kr[2,5]

#

pvalues_2 <-c(reg7_1_1r_pval_1,
              reg7_1_2r_pval_1,
              reg7_2_1r_pval_1,
              reg7_2_2r_pval_1,
              reg7_3_1r_pval_1,
              reg7_3_2r_pval_1,
              reg7_4_1r_pval_1,
              reg7_4_2r_pval_1,
              reg7_5_1r_pval_1,
              reg7_5_2r_pval_1,
              reg7_6_1r_pval_1,
              reg7_6_2r_pval_1,
              regE_4_1_Kr_pval_1,
              regE_4_1_Ar_pval_1,
              regE_4_1_Rr_pval_1,
              regE_5_1_Kr_pval_1,
              regE_5_1_Ar_pval_1,
              regE_5_1_Rr_pval_1,
              regE_4_2_Kr_pval_1,
              regE_5_2_Kr_pval_1)

pvalue_name_2 <- c("reg7_1_1r",
                   "reg7_1_2r",
                   "reg7_2_1r",
                   "reg7_2_2r",
                   "reg7_3_1r",
                   "reg7_3_2r",
                   "reg7_4_1r",
                   "reg7_4_2r",
                   "reg7_5_1r",
                   "reg7_5_2r",
                   "reg7_6_1r",
                   "reg7_6_2r",
                   "regE_4_1_Kr",
                   "regE_4_1_Ar",
                   "regE_4_1_Rr",
                   "regE_5_1_Kr",
                   "regE_5_1_Ar",
                   "regE_5_1_Rr",
                   "regE_4_2_Kr",
                   "regE_5_2_Kr")

qvalue_2 <- p.adjust(pvalues_2,method="fdr")

pvalues_df_2 <- cbind.data.frame(pvalue_name_2,
                               pvalues_2,
                               qvalue_2)

write.csv(pvalues_df_2, "pvalues_df_2.csv", row.names = F)
