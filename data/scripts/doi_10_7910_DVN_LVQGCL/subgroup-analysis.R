# this is the file creating the sub-group analysis for the main part of the paper figure 3

# clear workspace
#rm(list=ls(all=TRUE))

# load required packages
library(rio)
library(tidyverse)
library(list)

# load data

df <- import("data/main-data.csv")

# recode DK responses to NA
df$Income[df$Income == 5] <- NA
#df$Education[df$Education == 7] <- NA
df$Education[df$Education == 8] <- 3 # Lumps "Other", with "low education"
df$Children[df$Children == 6] <- NA
df$Gender[df$Gender == 3] <- NA
df$PartyMember[df$PartyMember == 3] <- NA

df$GovProcure <- ifelse(df$CommissionSurvey == 1,1,0) #create GovProcure dummy


#Create treatment status indicators
df$y_confidence <- ifelse(is.na(df$ConfidenceTreat),df$ConfidenceControl,df$ConfidenceTreat)
df$treat_confidence <- ifelse(df$Group == 1, 1, 0)  #treatment indicator

df$y_system <- ifelse(is.na(df$SystemTreat),df$SystemControl,df$SystemTreat) #single list variable
df$treat_system <- ifelse(df$Group == 2, 1, 0)  #treatment indicator

df$y_corruption <- ifelse(is.na(df$CorruptionTreat),df$CorruptionControl,df$CorruptionTreat)
df$treat_corruption <- ifelse(df$Group == 2, 1, 0)  #treatment indicator

df$y_censor <- ifelse(is.na(df$CensorTreat),df$CensorControl,df$CensorTreat)
df$treat_censor <- ifelse(df$Group == 1, 1, 0)  #treatment indicator


# in the main analysis we recode Direct Questions to binary (DK coded as NA)

df$ConfidenceDirect[df$ConfidenceDirect == 3] <- NA
df$ConfidenceDirect[df$ConfidenceDirect == 2] <- 0

df$SystemDirect[df$SystemDirect == 3] <- NA
df$SystemDirect[df$SystemDirect == 2] <- 0

df$CorruptionDirect[df$CorruptionDirect == 3] <- NA
df$CorruptionDirect[df$CorruptionDirect == 2] <- 0

df$CensorDirect[df$CensorDirect == 3] <- NA
df$CensorDirect[df$CensorDirect == 2] <- 0

##Create some subsets for analysis by group
##Create high and low income subsets
high.income <- df[ which(df$Income >= 3),]
low.income <- df[ which(df$Income < 3),]

##Create subsets based on belief in who commissioned survey
gov.commission <- df[ which(df$CommissionSurvey == 1),]
not.gov.commission <- df[ which(df$CommissionSurvey != 1),]

##Create subsets based on party membership
party.member <- df[ which(df$PartyMember == 1),]
not.party.member <- df[ which(df$PartyMember == 2),]

##Create subsets based on gender
men <- df[ which(df$Gender == 1),]
women <- df[ which(df$Gender == 2),]

##Create subsets by education
junior.college <- df[which(df$Education <= 5),]
uni <- df[which(df$Education > 5),]

##Create subsets by age
oldest <- df[which(df$YearBirth <= 1985),]
youngest <- df[which(df$YearBirth > 1985),]

##Create subsets by hukou
rural <- df[which(df$Hukou == 2),]
urban <- df[which(df$Hukou == 1),]



# and lets lets get the estimates for each subgroup 

source("code/subgroup-censor.R")
df_censor <- df 

source("code/subgroup-system.R")
df_system <- df 

source("code/subgroup-corruption.R")
df_corruption <- df 

#Fix high_income
source("code/subgroup-confidence.R")
df_confidence <- df

rm(list=ls()[! ls() %in% c("df_confidence","df_system", "df_corruption",  "df_censor")])

# prep for forest plot with faceting

group <- c("Income", "Income", "Age", "Age", "Education", "Education", "Gender","Gender", "CCP", "CCP", "Hukou", "Hukou", "Sponsor", "Sponsor")
subgroup <- c("High","Low", "Younger", "Older", "University", "No uni.", "Men", "Women",
              "Member", "No", "Rural", "Urban", "Government", "Independent")

df_confidence <- df_confidence %>% mutate(., item = c(rep("Confidence", 14)), 
                                          group = group,
                                          subgroup = subgroup
)

df_censor <- df_censor %>% mutate(., item = c(rep("Censorship", 14)), 
                                  group = group,
                                  subgroup = subgroup
)

df_system <- df_system %>% mutate(., item = c(rep("System", 14)), 
                                  group = group, 
                                  subgroup = subgroup
)

df_corruption <- df_corruption %>% mutate(., item = c(rep("Corruption", 14)), 
                                          group = group, 
                                          subgroup = subgroup
)

df <- bind_rows(df_confidence, df_corruption, df_system, df_censor)



# create unique label to facilitate forest plot
df <- mutate(df, testlab = ifelse(item=="Confidence", subgroup, 
                                  ifelse(item=="Corruption", paste(" ", subgroup), 
                                         ifelse(item=="System", paste("  ", subgroup),
                                                ifelse(item=="Censorship", paste("   ", subgroup), NA)
                                         ))))

#lets order them like we want them 


df$item <- factor(df$item, levels=c("Confidence", "Corruption", "System", "Censorship"))

# lets about facet by subgroup, and shape/lines by question item so consistent with main figure

ggplot(df, aes(y = forcats::fct_reorder(testlab, -as.numeric(item)), x = mean, xmin = lower, xmax = upper, shape=item, linetype= item, color = item)) +
  geom_point(size=4) +
  geom_errorbarh(height = .1) + 
  scale_x_continuous(limits=c(-.65,.25),breaks=c(-.5,-.4,-.3,-.2,-.1, 0, .1, .2)) +
  geom_vline(xintercept=0, color="grey60",linetype="dashed")+
  facet_grid(group ~ . , scales = "free", space = "free") +
  ylab("Subgroup") + xlab("Difference between Direct and Indirect Estimates (95% CI)") +
  theme_light(base_size = 14) +
  scale_shape_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                     values = c("Censorship" = 4, "Confidence" = 16, "Corruption" = 15, "System" = 17),
                     name = "Item:") + 
  scale_linetype_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                        values = c("Censorship" = "longdash", "Confidence" = "solid", "Corruption" = "F1", "System" = "dashed"), 
                        name = "Item:") +
  scale_color_manual(breaks = c("Confidence", "Corruption", "System", "Censorship"),
                     values = c("Censorship" = "#984ea3", "Confidence" = "#e41a1c", "Corruption" = "#4daf4a", "System" = "#377eb8"),
                     name = "Item:") +
  theme(legend.position = 'top',
        panel.grid.major.y = element_blank())

ggsave("output/fp_subgroup_inc_controls.pdf", width = 8.5, height = 11, units = c("in"))



