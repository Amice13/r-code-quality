# Title: Triple A Group Selection Tool###
# Author: Levi Orero
# Contact: l.orero@cgiar.org
# Script and data info: This script performs analyses on group response data.##
# Data consists of responses on group wellbeing and capacity indicators. 
# Data was collected in the Nyando river basin in 2014. 
# Data retrieved from R working directory

# Copyright statement: This script is the product of ICRAF Systems Dept

# Load required packages
library(kableExtra)
library(psych)
library(tibble)
library(tidyverse)
library(readr)

## Import Data from working directory ===========
# Group Interviews
original.sele <- read.csv("original_selection.csv")

#Key Informant Interviews
group.logist <- read_csv("logistic.selection.csv")


# Original Group Selection Tool =============
# Score group type based on assigned weights

original.tool <-
original.sele %>% rowwise()%>%
  mutate(wellbeing = sum (v_education,v_assets,v_income, v_land,
                          (0.5*v_landprep),(1.5*v_foodsec),
                          (2*v_partner)),
         capacity = sum(c_activeyears,(1.5*c_motivation),
                       (1.5*c_objectives),(1.5*c_leadership),
                       (0.5*c_membership),
                       (0.5*c_area), (1.5*c_agencydemo)))%>%
  as.data.frame()%>% 
  mutate(WB.scale = ifelse(wellbeing > 24,"High","Low"),
         AC.scale = ifelse(capacity > 24,"High","Low"))%>%
  mutate(group.score = paste0(WB.scale,AC.scale))%>%
  mutate(group.type = ifelse(group.score == "HighHigh","Type 1",
                             ifelse(group.score == "HighLow","Type 2",
                                    ifelse(group.score=="LowHigh",
                                    "Type 3","Type 4"))))

# Total Summary Distribution of groups per type
table(original.tool$group.type)

# Groups Selected
grp <- c(21,37,55,69,76,85,88,104,133,134,148,
         151,153,158,171,180,181,186,199,210,215,216,227,240)

triplea <- original.tool %>% filter (GRP %in% grp)

triplea <- triplea %>% arrange(-desc(GRP))

# Summary of groups selected

table(triplea$group.type)


# Shapiro Wilks test for Normality
shapiro.test(triplea$wellbeing)
shapiro.test(triplea$capacity)


# New Selection Tool =============

# Select data that we will score
scorer.or <- original.sele %>% 
  select(v_education:c_agencydemo,GRP,DIV)#trimmed frame

# Divide groups by Division
scorer.or.1 <- scorer.or %>% filter(DIV == 1)
scorer.or.2 <- scorer.or %>% filter(DIV == 2)
scorer.or.3 <- scorer.or %>% filter(DIV == 3)

#Create Scoring Keys
my.keys.list <- list(WB = c("v_education", "v_assets","v_income","v_land",
                              "v_landprep", "v_foodsec", "v_partner"),
                     AC = c("c_activeyears","c_motivation","c_objectives",
                              "c_leadership", #"c_membership", "c_area",
                              "c_agencydemo"))

#minus membership and area



my.scales <- scoreItems(my.keys.list, scorer.or)#Score Items

my.scales #show the output

# Print everything
print(my.scales, short.option = FALSE)

# Get More info from the scales output
# Cronbach's Alpha Values of reliability
my.scales$alpha

#Guttman Lamba 6 of reliability
my.scales$G6

#Correlation of the scales and then items
my.scales$cor

my.scales$item.cor

my.scales$item.corrected


# Then we assign scores to a data frame
my.scores <- my.scales$scores

new.tool <- data.frame(my.scores,GRP = scorer.or$GRP)

#Assign Group Type as per scores in WB and AC

new.tool <- new.tool %>% 
  mutate(WB.tag = ifelse(WB > median(WB),"High","Low"),
         AC.tag = ifelse(AC > median(AC),"High","Low"))%>%
  mutate(group.score = paste0(WB.tag,AC.tag))%>% 
  mutate(group.type = ifelse(group.score == "HighHigh","Type 1",
                             ifelse(group.score=="HighLow","Type 2",
                                    ifelse(group.score=="LowHigh",
                                           "Type 3","Type 4"))))
# Summary of group selection by new tool
table(new.tool$group.type)

#Compare old mapping to new mapping for selected groups ===========

triplea.new <- new.tool %>% filter(GRP %in% grp)

table(triplea$group.type)
table(triplea.new$group.type)

# Normality tests for new tool scores

shapiro.test(as.numeric(triplea.new$WB))
shapiro.test(as.numeric(triplea.new$AC))


##Merge with data from Key informant Interviews ======

#Final assessment	whether you would select the group again

# Exclude next chunk if you want all three categories of answers
# Including "no", "yes" and "very much/outstanding"

group.logist <- group.logist %>% # merging "yes" and "outstanding"
  mutate(final.asse = replace(final.asse, final.asse == 2, 1),
         soc.1 = replace(soc.1, soc.1 == 2, 1),
         outreach = replace(outreach, outreach == 2, 1),
         group = replace(group, group == 2, 1),
         finance.2 = replace(finance.2, finance.2 == 2, 1),
         finance.1 = replace(finance.1, finance.1 == 2, 1),
         soce.1 = replace(soce.1, soce.1 == 2, 1),
         individual = replace(individual, individual == 2, 1))

# Old selection tool 
triple.old.compare <- cbind(triplea[,c(1:4,19:20)],group.logist)

#Table groups that were "accepted" and type
table(triple.old.compare$final.asse, triple.old.compare$group.type)


# New selection tool 
triple.new.compare <- cbind(triplea.new,group.logist)

#Table groups that were "accepted" and type
table(triple.new.compare$final.asse, triple.new.compare$group.type)


## DEFINE SCORES VERSUS KEY INFORMANT VARIABLE SCORE =====
# Old Selection Tool====
#1. Social Cohesion (Internal) 1	- Joint activities with other members
triple.old.compare %>%
  group_by(soc.1)%>% summarise(mean.wellbeing = mean(wellbeing),
                               mean.capacity = mean(capacity),
                               median.wellbeing = median(wellbeing),
                               median.capacity = median(capacity))
#2. Social Cohesion (Internal) 2	- Joint investments with other members
triple.old.compare %>%
  group_by(soc.2)%>% summarise(mean.wellbeing = mean(wellbeing),
                               mean.capacity = mean(capacity),
                               median.wellbeing = median(wellbeing),
                               median.capacity = median(capacity))

#3. Social Cohesion (External) 1	- Joint activities with other groups
triple.old.compare %>%
  group_by(soce.1)%>% summarise(mean.wellbeing = mean(wellbeing),
                                mean.capacity = mean(capacity),
                                median.wellbeing = median(wellbeing),
                                median.capacity = median(capacity))

#4. Social Cohesion (External) 2	- Joint investments with other groups
triple.old.compare %>%
  group_by(soce.2)%>% summarise(mean.wellbeing = mean(wellbeing),
                                mean.capacity = mean(capacity),
                                median.wellbeing = median(wellbeing),
                                median.capacity = median(capacity))

#5. Social Cohesion (External) 3	- Exchange/Outreach
triple.old.compare %>%
  group_by(soce.3)%>% summarise(mean.wellbeing = mean(wellbeing),
                                mean.capacity = mean(capacity),
                                median.wellbeing = median(wellbeing),
                                median.capacity = median(capacity))

#6. Financial opportunities 1	- VSLA
triple.old.compare %>%
  group_by(finance.1)%>% summarise(mean.wellbeing = mean(wellbeing),
                                   mean.capacity = mean(capacity),
                                   median.wellbeing = median(wellbeing),
                                   median.capacity = median(capacity))

#7. Financial opportunities 2	- External connectors(political/other NGOs)
triple.old.compare %>%
  group_by(finance.2)%>% summarise(mean.wellbeing = mean(wellbeing),
                                   mean.capacity = mean(capacity),
                                   median.wellbeing = median(wellbeing),
                                   median.capacity = median(capacity))

#8. Individual	Performance of individual farmers
triple.old.compare %>%
  group_by(individual)%>% summarise(mean.wellbeing = mean(wellbeing),
                                    mean.capacity = mean(capacity),
                                    median.wellbeing = median(wellbeing),
                                    median.capacity = median(capacity))

#9. Overall Group Performance of the group
triple.old.compare %>%
  group_by(group)%>% summarise(mean.wellbeing = mean(wellbeing),
                               mean.capacity = mean(capacity),
                               median.wellbeing = median(wellbeing),
                               median.capacity = median(capacity))

#10. Outreach	Performance of the group in outreach
triple.old.compare %>%
  group_by(outreach)%>% summarise(mean.wellbeing = mean(wellbeing),
                                  mean.capacity = mean(capacity),
                                  median.wellbeing = median(wellbeing),
                                  median.capacity = median(capacity))

#11. Final Assesssment and project suitability
triple.old.compare %>%
  group_by(final.asse)%>% summarise(mean.wellbeing = mean(wellbeing),
                                    mean.capacity = mean(capacity),
                                    median.wellbeing = median(wellbeing),
                                    median.capacity = median(capacity))


## DEFINE SCORES VERSUS KEY INFORMANT VARIABLE SCORE =====
# New Selection Tool====
#1. Social Cohesion (Internal) 1	- Joint activities with other members
triple.new.compare %>%
  group_by(soc.1)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#2. Social Cohesion (Internal) 2	- Joint investments with other members
triple.new.compare %>%
  group_by(soc.2)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))

#3. Social Cohesion (External) 1	- Joint activities with other groups
triple.new.compare %>%
  group_by(soce.1)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))

#4. Social Cohesion (External) 2	- Joint investments with other groups
triple.new.compare %>%
  group_by(soce.2)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#5. Social Cohesion (External) 3	- Exchange/Outreach
triple.new.compare %>%
  group_by(soce.3)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#6. Financial opportunities 1	- VSLA
triple.new.compare %>%
  group_by(finance.1)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#7. Financial opportunities 2	- External connectors(political/other NGOs)
triple.new.compare %>%
  group_by(finance.2)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#8. Individual	Performance of individual farmers
triple.new.compare %>%
  group_by(individual)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#9. Overall Group Performance of the group
triple.new.compare %>%
  group_by(group)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))
#10. Outreach	Performance of the group in outreach
triple.new.compare %>%
  group_by(outreach)%>% summarise(mean.WB = mean(WB),
                               mean.AC = mean(AC),
                               median.WB = median(WB),
                               median.AC = median(AC))

#11. Final Assesssment and project suitability
triple.new.compare %>%
  group_by(final.asse)%>% summarise(mean.WB = mean(WB),
                                  mean.AC = mean(AC),
                                  median.WB = median(WB),
                                  median.AC = median(AC))

