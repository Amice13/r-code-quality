## Micro-foundation of Quest for Status: Testing StatusPerception and Multilateral Use of Force
## Yuji Masumura and Atsushi Tago
## 
## Code to preprocess the data 



# Setting Up --------------------------------------------------------------
rm(list = ls())


# Loading data ------------------------------------------------------------
data <- read_csv("translated_data.csv") %>% filter(consent == "Agree")


# Treatment status -----------------------------------------------------------



data <- data %>% mutate(priming = if_else(is.na(priming_1)==F, 1, 0),  ##whether recieved some priming questions or not
                        groups = case_when( 
                          is.na(DV1_G1) ==F ~ 1,  ## Group1: participate in a UN-led peacekeeping, then stay after casualties 
                          is.na(DV1_G2) ==F ~ 2,  ## Group2: participate in a US-led peacekeeping, then stay after casualties
                          is.na(DV1_G3) ==F ~ 3,  ## Group3: participate in a UN-led peacekeeping, then withdraw after casualties
                          is.na(DV1_G4) ==F ~ 4,  ## Group4: participate in a US-led peacekeeping, then withdraw after casualties
                          is.na(DV1_G5) ==F ~ 5,  ## Group5: did not participate in a UN-led peacekeeping
                          is.na(DV1_G6) ==F ~ 6,)) ## Group6: did not participate in a US-led peacekeeping



# Converting to long data -------------------------------------------------


#inserting 99 into the rows of the control group (Group 5 & 6) as a sign of the control group. 
#This sign will be used in pivot_longer(). Otherwise, drop_na() deletes too many observation.

data <- data %>% mutate(DV10_G1 = ifelse(is.na(DV1_G5)==F |is.na(DV1_G6)==F, "99", DV10_G1),
                        DV11_G1 = ifelse(is.na(DV1_G5)==F |is.na(DV1_G6)==F, "99", DV11_G1),
                        DV12_G1 = ifelse(is.na(DV1_G5)==F |is.na(DV1_G6)==F, "99", DV12_G1))



data <- data %>% pivot_longer(
  -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
     "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
     "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
     "liberal", "age", "gender", "education", "free_writing","groups",
     colnames(data)[str_detect(colnames(data),pattern="^fav")],
     colnames(data)[str_detect(colnames(data),pattern="^priming")],
     colnames(data)[str_detect(colnames(data),pattern="^DV2")],
     colnames(data)[str_detect(colnames(data),pattern="^DV3")],
     colnames(data)[str_detect(colnames(data),pattern="^DV4")],
     colnames(data)[str_detect(colnames(data),pattern="^DV5")],
     colnames(data)[str_detect(colnames(data),pattern="^DV6")],
     colnames(data)[str_detect(colnames(data),pattern="^DV7")],
     colnames(data)[str_detect(colnames(data),pattern="^DV8")],
     colnames(data)[str_detect(colnames(data),pattern="^DV9")],
     colnames(data)[str_detect(colnames(data),pattern="^DV10")],
     colnames(data)[str_detect(colnames(data),pattern="^DV11")],
     colnames(data)[str_detect(colnames(data),pattern="^DV12")],
     colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
  
     names_to = "DV1_origin", values_to = "DV1") %>% drop_na(DV1)
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV2_origin", values_to = "DV2") %>% drop_na(DV2)
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV3_origin", values_to = "DV3") %>% drop_na(DV3) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV4_origin", values_to = "DV4") %>% drop_na(DV4) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV5_origin", values_to = "DV5") %>% drop_na(DV5) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV6_origin", values_to = "DV6") %>% drop_na(DV6) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV7_origin", values_to = "DV7") %>% drop_na(DV7) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV8_origin", values_to = "DV8") %>% drop_na(DV8) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV1")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV9_origin", values_to = "DV9") %>% drop_na(DV9) 
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups", "DV1", "DV1_origin",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV10_origin", values_to = "DV10") %>% drop_na(DV10) %>% mutate(DV10=na_if(DV10, "99"))
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups", "DV1", "DV1_origin",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV12")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV11_origin", values_to = "DV11") %>% drop_na(DV11) %>% mutate(DV11=na_if(DV11, "99"))
  
  
data <- data %>%  pivot_longer(
    -c("StartDate", "EndDate", "Progress", "Duration (in seconds)", "Finished", "RecordedDate", 
       "LocationLatitude", "LocationLongitude", "UserLanguage", "consent",
       "interest",  "leader_sup", "hawk_dove", "check1", "check2", "knowledge",
       "liberal", "age", "gender", "education", "free_writing","groups", "DV1", "DV1_origin",
       colnames(data)[str_detect(colnames(data),pattern="^fav")],
       colnames(data)[str_detect(colnames(data),pattern="^priming")],
       colnames(data)[str_detect(colnames(data),pattern="^DV2")],
       colnames(data)[str_detect(colnames(data),pattern="^DV3")],
       colnames(data)[str_detect(colnames(data),pattern="^DV4")],
       colnames(data)[str_detect(colnames(data),pattern="^DV5")],
       colnames(data)[str_detect(colnames(data),pattern="^DV6")],
       colnames(data)[str_detect(colnames(data),pattern="^DV7")],
       colnames(data)[str_detect(colnames(data),pattern="^DV8")],
       colnames(data)[str_detect(colnames(data),pattern="^DV9")],
       colnames(data)[str_detect(colnames(data),pattern="^DV10")],
       colnames(data)[str_detect(colnames(data),pattern="^DV11")],
       colnames(data)[str_detect(colnames(data),pattern="^SDO")]),
    
    names_to = "DV12_origin", values_to = "DV12") %>% drop_na(DV12) %>% mutate(DV12=na_if(DV12, "99"))



# Variable transformation & creation -------------------------------------------------

#SDO
data$SDO1 <- as.numeric(data$SDO1)
data$SDO2 <- as.numeric(data$SDO2)
data$SDO3 <- as.numeric(data$SDO3)
data$SDO4 <- as.numeric(data$SDO4)
data$SDO5 <- as.numeric(data$SDO5)
data$SDO6 <- as.numeric(data$SDO6)
data$SDO7 <- as.numeric(data$SDO7)
data$SDO8 <- as.numeric(data$SDO8)
data$SDO9 <- as.numeric(data$SDO9)
data$SDO10 <- as.numeric(data$SDO10)
data$SDO11 <- as.numeric(data$SDO11)
data$SDO12 <- as.numeric(data$SDO12)
data$SDO13 <- as.numeric(data$SDO13)
data$SDO14 <- as.numeric(data$SDO14)
data$SDO15 <- as.numeric(data$SDO15)
data$SDO16 <- as.numeric(data$SDO16)

data$SDO_mean <- NA
for (i in 1:nrow(data)) {
  data$SDO_mean[i] <- mean(c(data$SDO1[i], data$SDO2[i],data$SDO3[i],data$SDO4[i],data$SDO5[i],
                             data$SDO6[i],data$SDO7[i],data$SDO8[i], 8-data$SDO9[i], 8-data$SDO10[i],
                             8-data$SDO11[i],8-data$SDO12[i],8-data$SDO13[i],8-data$SDO14[i],8-data$SDO15[i],8-data$SDO16[i]))
}


## liberal 
data <- data %>% mutate(liberal = na_if(liberal, "Do not want to answer"))
data$liberal <- as.numeric(data$liberal)


##age
data <- data %>%  mutate(age = 2021 - as.numeric( str_sub(data$age, start = -4,end = -1)))
data <- data %>% mutate(age_group = case_when(20<= age & age <= 29 ~ "20s or less",
                                              30<= age & age <= 39 ~ "30s",
                                              40<= age & age <= 49 ~ "40s",
                                              50<= age & age <= 59 ~ "50s",
                                              60<= age & age <= 69 ~ "60s",
                                              70<= age & age <= 79 ~ "70s or more"))

## Other Treatment Status
data <- data %>% mutate(treatment = if_else(groups == 5| groups==6, 0, if_else(groups == 1| groups==3,1,2)))
            #0 is assigned for the control group, 1 is assigned for the UN treatment group, and 2 is for the US treatment group


write.csv(data, "preprocessed_data.csv", row.names=FALSE)

