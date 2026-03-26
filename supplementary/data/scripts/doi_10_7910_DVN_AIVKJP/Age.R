rm(list=ls())

library(readxl)
library(tidyverse)
library(ggplot2)
library(maps)
library(kableExtra)

# set your working directory 

# define colorblind-friendly colors
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# load replication data 
load("data_replication.Rda")

# analyze age, plot age at time of submission. use DOBs to calculate age at time of submission # 

####calculate age #####

data$dob <- data$`DOB Victim`

# delete mulitples, will split by code later 
data <- data[which(data$`Case ID`!="64/2018"),]
data <- data[which(data$`Case ID`!="65/2018"),]
data <- data[which(data$`Case ID`!="67/2018"),]
data <- data[which(data$`Case ID`!="68/2018"),]
data <- data[which(data$`Case ID`!="79/2019"),]
data <- data[which(data$`Case ID`!="109/2019"),]
data <- data[which(data$`Case ID`!="116/2020"),]
data <- data[which(data$`Case ID`!="117/2020"),]
data <- data[which(data$`Case ID`!="118/2020"),]

myvars <- c("Case ID", "dob", "communication","Child is Author?")

dat <- data[myvars]

# extract all DOB strings (full date or year-only) from the 'dob' column
dat <- dat %>%
  mutate(dob_extracted = str_extract_all(
    dob,
    "\\b(?:\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}|\\d{4})\\b"
  ))

# each date gets its own row 
dat <- dat %>%
  unnest(dob_extracted)

# clean up extracted strings using trimws() and then convert to Date objects
dat <- dat %>%
  # Create a cleaned version
  mutate(dob_clean = trimws(dob_extracted)) %>%
  # Convert the cleaned strings to dates.
  mutate(dob_converted = case_when(
    str_detect(dob_clean, "^\\d{1,2}\\s+[A-Za-z]+\\s+\\d{4}$") ~ dmy(dob_clean),
    str_detect(dob_clean, "^\\d{4}$") ~ as.Date(paste0(dob_clean, "-12-31"), format = "%Y-%m-%d"),
    TRUE ~ NA_Date_
  ))

# calculate age by subtracting dob_converted from the communication date
dat <- dat %>%
  mutate(age = as.integer(interval(dob_converted, communication) / years(1)))

# calculate age by subtracting dob_converted from the communication date.
# age is computed as the whole number of years between the two dates.
dat <- dat %>%
  mutate(age = as.integer(interval(dob_converted, communication) / years(1)))

### age at time of submission
age <- data[myvars]


age <- age %>%
  rowwise() %>%
  mutate(extracted_ages = 
           if (!is.na(dob) && str_detect(dob, regex("age|time", ignore_case = TRUE))) {
             # Extract all sequences of digits
             list(as.numeric(str_extract_all(dob, "\\b\\d+\\b")[[1]]))
           } else {
             NA
           }
  ) %>%
  ungroup()
age_unnested <- age %>% unnest(extracted_ages)

# fix 
age_unnested$extracted_ages[age_unnested$dob=="At time of submission authors were all under the age of 18 years"] <- NA
age_unnested$extracted_ages[age_unnested$dob=="45 children, aged between 5 and 17"] <- NA
age_unnested$extracted_ages[age_unnested$dob=="Children aged between 1 and 11 years at time of submission"] <- NA
age_unnested$extracted_ages[age_unnested$dob=="Claimed to be under 18 at time of interception."] <- NA

# get full primary data set of ages 
age_unnested$age <- age_unnested$extracted_ages
vars <- c("Case ID","age","Child is Author?")
age_unnested <- age_unnested[vars]
age_unnested <- age_unnested[!is.na(age_unnested$age),]
dat <- dat[vars]
dat <- dat[!is.na(dat$age),]

ages <- rbind(age_unnested,dat)

adult <- ages[which(ages$age >=18),]
adult
sum(adult$`Child is Author?`)

# only children 
child <- ages[which(ages$age <18),]

# plot by child is author
child$child_author <- as.factor(child$`Child is Author?`)


# Figure 3: age at time of submission
age_plot <- ggplot(child, aes(x = age, fill = child_author)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(
    title = "Age at Time of Submission",
    x = "Age", y = "Frequency") +
  theme_minimal() +
  scale_fill_manual(values = cbPalette, labels = c("No","Yes"), name ="Child is Author?")
age_plot

# child is author 
data$childauthor <- as.numeric(data$`Child is Author?`)
sum(data$childauthor, na.rm = TRUE)/ nrow(data)

# code parents  
actors <- data[c("Case ID","State","Submitted by","Representation?","Alleged Victim(s)","Child is Author?","Actor Notes")] 
actors <- actors %>%
  mutate(parental = if_else(
    grepl("mother|father|daughter|son|child|parent", `Actor Notes`, ignore.case = TRUE) & 
      !grepl("grandmother", `Actor Notes`, ignore.case = TRUE), 
    1, 
    0
  ))

actors <- actors %>%
  mutate(mother = if_else(
    (grepl("mother", `Actor Notes`, ignore.case = TRUE) & 
       !grepl("grandmother", `Actor Notes`, ignore.case = TRUE)) |
      grepl("her\\s+.*?\\s*child", `Actor Notes`, ignore.case = TRUE) |
      grepl("her\\s+.*?\\s*children", `Actor Notes`, ignore.case = TRUE) |
      grepl("her\\s+.*?\\s*daughter", `Actor Notes`, ignore.case = TRUE) |
      grepl("her\\s+.*?\\s*son", `Actor Notes`, ignore.case = TRUE),
    1, 
    0
  ))

# where is child author 
sum(actors$`Child is Author?`,na.rm = T)
sum(actors$`Child is Author?`,na.rm = T)/nrow(actors)

childisnotauthor <- actors[which(actors$`Child is Author?`==0),]
sum(childisnotauthor$parental)
sum(childisnotauthor$parental)/nrow(childisnotauthor)
nrow(childisnotauthor[childisnotauthor$parental==0,])
sum(actors$parental)
sum(actors$parental)/nrow(actors)

sum(actors$mother)
sum(actors$mother)/nrow(actors)

