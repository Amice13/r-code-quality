## Micro-foundation of Quest for Status: Testing StatusPerception and Multilateral Use of Force
## Yuji Masumura and Atsushi Tago
## 
## Code to translate the data 

# setting up ------------------------------------------------------------------

rm(list = ls())

library(tidyverse)




# Loading data ------------------------------------------------------------

data <- read_csv("data/Status_and_Multilateral_Use_of_Force.csv") 



# Translation -------------------------------------------------------------

data <- data %>% mutate(consent = recode(consent, "同意する"="Agree", "同意しない"="Do not agree"),
                        
                        interest = recode(interest,  "かなり注意を払っている" = "A lot of attention",
                                                     "やや注意を払っている" = "Some attention",
                                                     "あまり注意を払っていない" = "Limited attention",
                                                     "ほとんど注意を払っていない" = "No attention",
                                                     "わからない" = "Do not know"),
                        
                        leader_sup = recode(leader_sup , "とても支持している" = "Strongly Approve",
                                                         "どちらかと言えば支持している" = "Somewhat Approve",
                                                         "どちらかと言えば支持していない"= "Somewhat Disapprove",
                                                         "まったく支持していない" = "Strongly Disapprove",
                                                         "わからない" = "Do not know"),
                        
                        hawk_dove = recode(hawk_dove, "タカ派" = "Hawk",
                                                      "ハト派" = "Dove",
                                                      "わからない" = "Do not know"),
                        
                        priming_1 = recode(priming_1, "強く同意する" = "Strongly Agree",
                                                        "どちらかといえば同意する" = "Somewhat Agree",
                                                        "どちらかといえば同意しない" = "Somewhat Disagree",
                                                        "強く同意しない" = "Strongly Disagree",
                                                        "わからない" = "Do not know"),
                        
                        priming_2 = recode(priming_2, "強く同意する" = "Strongly Agree",
                                                            "どちらかといえば同意する" = "Somewhat Agree",
                                                            "どちらかといえば同意しない" = "Somewhat Disagree",
                                                           "強く同意しない" = "Strongly Disagree",
                                                           "わからない" = "Do not know"),
                        
                        priming_3 = recode(priming_3, "強く同意する" = "Strongly Agree",
                                            "どちらかといえば同意する" = "Somewhat Agree",
                                            "どちらかといえば同意しない" = "Somewhat Disagree",
                                            "強く同意しない" = "Strongly Disagree",
                                            "わからない" = "Do not know"),
                        
                        ##Group 1, UN Treatment Stay
                        DV1_G1 = recode(DV1_G1, "支持する" = "Support",   
                                                      "支持しない" = "Do not support",
                                                      "わからない" = "Do not know",
                                                      "答えたくない" = "Do not want to answer"),
                        
                        DV2_G1 = recode(DV2_G1, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                            "ステータスには影響はない" = "Do not have an impact",
                                            "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                    
                        DV3_G1 = recode(DV3_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G1 = recode(DV4_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G1 = recode(DV5_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G1 = recode(DV6_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G1 = recode(DV7_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G1 = recode(DV8_G1, "そう思う" =  "Yes, I think so",
                                                  "どちらでもない"= "Neither",
                                                  "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G1 = recode(DV9_G1, 
                                                 "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                                 "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                                 "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                                 "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                                 "国際社会の50パーセント以下" = "Below 50% of international society",
                                                 "わからない" = "Do not know"),
                        
                        DV10_G1 = recode(DV10_G1,  "支持する" = "Support",
                                             "支持しない" = "Do not support",
                                             "わからない" = "Do not know",
                                             "答えたくない" = "Do not want to answer"),
                        
                        DV11_G1 = recode(DV11_G1, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                         "ステータスには影響はない" = "Do not have an impact",
                                         "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV12_G1 = recode(DV12_G1, 
                                          "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                          "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                          "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                          "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                          "国際社会の50パーセント以下" = "Below 50% of international society",
                                          "わからない" = "Do not know"),
                        
                        
                        ##Group 2, US, Treatment Stay
                        DV1_G2 = recode(DV1_G2, "支持する" = "Support",   
                                           "支持しない" = "Do not support",
                                           "わからない" = "Do not know",
                                           "答えたくない" = "Do not want to answer"),
                        
                        DV2_G2 = recode(DV2_G2, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                      "ステータスには影響はない" = "Do not have an impact",
                                      "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV3_G2 = recode( DV3_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G2 = recode( DV4_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G2 = recode( DV5_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G2 = recode( DV6_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G2 = recode( DV7_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G2 = recode( DV8_G2, "そう思う" =  "Yes, I think so",
                                                 "どちらでもない"= "Neither",
                                                 "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G2 = recode(DV9_G2, 
                                        "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                        "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                        "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                        "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                        "国際社会の50パーセント以下" = "Below 50% of international society",
                                        "わからない" = "Do not know"),
                        
                        DV10_G2 = recode(DV10_G2,  "支持する" = "Support",
                                             "支持しない" = "Do not support",
                                             "わからない" = "Do not know",
                                             "答えたくない" = "Do not want to answer"),
                        
                        DV11_G2 = recode(DV11_G2,  "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                         "ステータスには影響はない" = "Do not have an impact",
                                         "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV12_G2 = recode(DV12_G2, 
                                      "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                      "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                      "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                      "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                      "国際社会の50パーセント以下" = "Below 50% of international society",
                                     "わからない" = "Do not know"),
                        
                        ##Group 3, UN, Treatment Withdraw
                        DV1_G3 = recode(DV1_G3, "支持する" = "Support",    
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV2_G3 = recode(DV2_G3, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                      "ステータスには影響はない" = "Do not have an impact",
                                      "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV3_G3 = recode( DV3_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G3 = recode( DV4_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G3 = recode( DV5_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G3 = recode( DV6_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G3 = recode( DV7_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G3 = recode( DV8_G3, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G3 = recode(DV9_G3, 
                                        "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                        "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                        "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                        "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                        "国際社会の50パーセント以下" = "Below 50% of international society",
                                        "わからない" = "Do not know"),
                        
                        DV10_G3 = recode(DV10_G3,  "支持する" = "Support",
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV11_G3 = recode(DV11_G3,  "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                         "ステータスには影響はない" = "Do not have an impact",
                                         "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV12_G3 = recode(DV12_G3, 
                                      "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                      "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                      "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                      "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                      "国際社会の50パーセント以下" = "Below 50% of international society",
                                      "わからない" = "Do not know"),
                        
                        ##Group 4, US, Treatment withdraw
                        DV1_G4 = recode(DV1_G4, "支持する" = "Support",    
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV2_G4 = recode(DV2_G4, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                      "ステータスには影響はない" = "Do not have an impact",
                                      "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV3_G4 = recode( DV3_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G4 = recode( DV4_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G4 = recode( DV5_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G4 = recode( DV6_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G4 = recode( DV7_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G4 = recode( DV8_G4, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G4 = recode(DV9_G4, 
                                        "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                        "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                        "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                        "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                        "国際社会の50パーセント以下" = "Below 50% of international society",
                                        "わからない" = "Do not know"),
                        
                        DV10_G4 = recode(DV10_G4,  "支持する" = "Support",
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV11_G4 = recode(DV11_G4,  "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                         "ステータスには影響はない" = "Do not have an impact",
                                         "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV12_G4 = recode(DV12_G4, 
                                      "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                      "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                      "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                      "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                      "国際社会の50パーセント以下" = "Below 50% of international society",
                                      "わからない" = "Do not know"),
                        
                        ##Group 5, UN, Control
                        DV1_G5 = recode(DV1_G5, "支持する" = "Support",   
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV2_G5 = recode(DV2_G5, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                      "ステータスには影響はない" = "Do not have an impact",
                                      "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV3_G5 = recode( DV3_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G5 = recode( DV4_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G5 = recode( DV5_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G5 = recode( DV6_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G5 = recode( DV7_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G5 = recode(DV8_G5, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G5 = recode(DV9_G5, 
                                      "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                      "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                      "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                      "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                      "国際社会の50パーセント以下" = "Below 50% of international society",
                                      "わからない" = "Do not know"),
                        
                        ##Group 6, US, Control
                        DV1_G6 = recode(DV1_G6, "支持する" = "Support",   
                                      "支持しない" = "Do not support",
                                      "わからない" = "Do not know",
                                      "答えたくない" = "Do not want to answer"),
                        
                        DV2_G6 = recode(DV2_G6, "ステータスを上げる影響をもつ" = "Have an impact of increasing status",
                                      "ステータスには影響はない" = "Do not have an impact",
                                      "ステータスを下げる影響がある" = "Have an impact of decreasing status"),
                        
                        DV3_G6 = recode( DV3_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV4_G6 = recode( DV4_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV5_G6 = recode( DV5_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV6_G6 = recode( DV6_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV7_G6 = recode( DV7_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV8_G6 = recode( DV8_G6, "そう思う" =  "Yes, I think so",
                                         "どちらでもない"= "Neither",
                                         "そう思わない"  =  "No, I do not think so"),
                        
                        DV9_G6 = recode(DV9_G6, 
                                      "国際社会の上位5パーセント" = "Among the top 5% of international society",
                                      "国際社会の上位6～15パーセント" = "Among the top 6 to 15% of international society",
                                      "国際社会の上位16～29パーセント" = "Among the top 16 to 29% of international society",
                                      "国際社会の上位30～49パーセント" = "Among the top 30 to 49% of international society",
                                      "国際社会の50パーセント以下" = "Below 50% of international society",
                                      "わからない" = "Do not know"),
                        
                        #SDO
                        
                        SDO1 = recode(SDO1, "１　全く同意しない／反対する" = "1",
                                            "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                            "７　完全に同意する／賛成する" = "7"),
                        
                        SDO2 = recode(SDO2, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO3 = recode(SDO3, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO4 = recode(SDO4, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO5 = recode(SDO5, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO6 = recode(SDO6, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO7 = recode(SDO7, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO8 = recode(SDO8, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO9 = recode(SDO9, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO10 = recode(SDO10, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO11 = recode(SDO11, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO12 = recode(SDO12, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO13 = recode(SDO13, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO14 = recode(SDO14, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO15 = recode(SDO15, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO16 = recode(SDO16, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        SDO16 = recode(SDO16, "１　全く同意しない／反対する" = "1",
                                      "２" = "2", "３" = "3", "４" = "4", "５" = "5", "６" = "6",
                                      "７　完全に同意する／賛成する" = "7"),
                        
                        #attention check
                        knowledge = recode(knowledge, "授権決議はあったはずだ" = "There must have been a resolution",
                                                      "授権決議はなかったはずだ" = "There must not have been a resolution",
                                                      "わからない" = "Do not know"),
                        
                        check1 = recode(check1, "中東のB国への自衛隊派遣" = "DF Troops deployment to state B in the Middle East",
                                                "アジアのC国への自衛隊派遣" ="SDF Troops deployment to state C in Asia",
                                                "自衛隊の早期撤退" =  "SDF early withdrawal",
                                                "自衛隊の派遣継続" =  "SDF continuing troop deployment",
                                                "どれでもない" = "None of the above",
                                        
                                        "自衛隊の早期撤退,自衛隊の派遣継続"  = 
                                          "SDF early withdrawal,SDF continuing troop deployment",
                                        "中東のB国への自衛隊派遣,自衛隊の早期撤退" = 
                                          "SDF Troops deployment to state B in the Middle East,SDF early withdrawal",
                                        "中東のB国への自衛隊派遣,自衛隊の派遣継続" = 
                                          "SDF Troops deployment to state B in the Middle East,SDF continuing troop deployment",
                                        "アジアのC国への自衛隊派遣,自衛隊の派遣継続" = 
                                          "SDF Troops deployment to state C in Asia,SDF continuing troop deployment",
                                        "中東のB国への自衛隊派遣,どれでもない" = 
                                          "SDF Troops deployment to state B in the Middle East,None of the above",
                                        "中東のB国への自衛隊派遣,アジアのC国への自衛隊派遣" =
                                          "SDF Troops deployment to state B in the Middle East,SDF Troops deployment to state C in Asia",
                                        "アジアのC国への自衛隊派遣,自衛隊の早期撤退" = 
                                          "SDF Troops deployment to state C in Asia,SDF early withdrawal",
                                        "自衛隊の早期撤退,どれでもない" = 
                                          "SDF early withdrawal,None of the above",
                                        "アジアのC国への自衛隊派遣,どれでもない" = 
                                          "SDF Troops deployment to state C in Asia,None of the above",
                                        "自衛隊の派遣継続,どれでもない" = 
                                          "SDF continuing troop deployment,None of the above",
                                        
                                        "中東のB国への自衛隊派遣,アジアのC国への自衛隊派遣,自衛隊の派遣継続" =
                                          "SDF Troops deployment to state B in the Middle East,SDF Troops deployment to state C in Asia,SDF continuing troop deployment",
                                        "中東のB国への自衛隊派遣,アジアのC国への自衛隊派遣,自衛隊の早期撤退" =
                                          "SDF Troops deployment to state B in the Middle East,SDF Troops deployment to state C in Asia,SDF early withdrawal",
                                        "中東のB国への自衛隊派遣,自衛隊の早期撤退,自衛隊の派遣継続" = 
                                          "SDF Troops deployment to state B in the Middle East,SDF early withdrawal,SDF continuing troop deployment",
                                        "アジアのC国への自衛隊派遣,自衛隊の早期撤退,自衛隊の派遣継続" = 
                                          "SDF Troops deployment to state C in Asia,SDF early withdrawal,SDF continuing troop deployment",
                                        "中東のB国への自衛隊派遣,自衛隊の派遣継続,どれでもない" =
                                          "SDF Troops deployment to state B in the Middle East,SDF continuing troop deployment,None of the above",
                                       
                                         "中東のB国への自衛隊派遣,アジアのC国への自衛隊派遣,自衛隊の早期撤退,自衛隊の派遣継続" = 
                                          "SDF Troops deployment to state B in the Middle East,SDF Troops deployment to state C in Asia,SDF early withdrawal,SDF continuing troop deployment",
                                        "中東のB国への自衛隊派遣,アジアのC国への自衛隊派遣,自衛隊の早期撤退,自衛隊の派遣継続,どれでもない"=
                                          "SDF Troops deployment to state B in the Middle East,SDF Troops deployment to state C in Asia,SDF early withdrawal,SDF continuing troop deployment,None of the above"),
                        
                        
                        check2 = recode(check2, "国連主導の平和維持活動" = "UN-led peacekeeping mission",
                                                "アメリカ主導の平和維持活動" =  "US-led peacekeeping mission",
                                                "どれでもない" = "Neither"),
                        
                        #demograohy
                        
                        liberal = recode(liberal,  "0 (革新)" = "0",
                                                   "5 (中間)" = "5",
                                                   "10 (保守)" = "10",
                                                   "答えたくない" = "Do not want to answer"),
                        
                        age = recode(age, "1945年以前" = "Before 1945",
                                          "1946年" = "1946",
                                          "1947年" = "1947",
                                          "1948年" = "1948",
                                          "1949年" = "1949",
                                          "1950年" = "1950",
                                          "1951年" = "1951",
                                          "1952年" = "1952",
                                          "1953年" = "1953",
                                          "1954年" = "1954",
                                          "1955年" = "1955",
                                          "1956年" = "1956",
                                          "1957年" = "1957",
                                          "1958年" = "1958",
                                          "1959年" = "1959",
                                          "1960年" = "1960",
                                          "1961年" = "1961",
                                          "1962年" = "1962",
                                          "1963年" = "1963",
                                          "1964年" = "1964",
                                          "1965年" = "1965",
                                          "1966年" = "1966",
                                          "1967年" = "1967",
                                          "1968年" = "1968",
                                          "1969年" = "1969",
                                          "1970年" = "1970",
                                          "1971年" = "1971",
                                          "1972年" = "1972",
                                          "1973年" = "1973",
                                          "1974年" = "1974",
                                          "1975年" = "1975",
                                          "1976年" = "1976",
                                          "1977年" = "1977",
                                          "1978年" = "1978",
                                          "1979年" = "1979",
                                          "1980年" = "1980",
                                          "1981年" = "1981",
                                          "1982年" = "1982",
                                          "1983年" = "1983",
                                          "1984年" = "1984",
                                          "1985年" = "1985",
                                          "1986年" = "1986",
                                          "1987年" = "1987",
                                          "1988年" = "1988",
                                          "1989年" = "1989",
                                          "1990年" = "1990",
                                          "1991年" = "1991",
                                          "1992年" = "1992",
                                          "1993年" = "1993",
                                          "1994年" = "1994",
                                          "1995年" = "1995",
                                          "1996年" = "1996",
                                          "1997年" = "1997",
                                          "1998年" = "1998",
                                          "1999年" = "1999",
                                          "2000年以降" = "After 2000"),
                        
                        gender = recode(gender,"女性" = "female",
                                        "男性" = "male",
                                        "答えたくない" = "Do not want to answer"),
                        
                        education = recode(education , "大学院卒業" = "Graduated from a graduate school",
                                           "大学院在学中・中退" = "Studying at a graduate school",
                                           "大学卒業" = "Graduated from a four-year college",
                                           "大学在学中・中退" = "Studying at a four-year college",
                                           "高校・高専・専門学校・短大卒業" = "Two-year college",
                                           "高校・高専・専門学校・短大在学中・中退" = "High school",
                                           "小学校・中学校卒業または高校在学中・中退" = "Elementary school or Junior high school" ) )





# Saving the data ---------------------------------------------------------
write.csv(data, "translated_data.csv", row.names=FALSE)
