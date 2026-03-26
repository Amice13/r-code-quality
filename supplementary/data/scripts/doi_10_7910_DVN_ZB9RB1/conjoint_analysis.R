# Install packages

install.packages("dplyr")
install.packages("cregg")
install.packages("ggplot2")
install.packages("vtable")
install.packages("xtable")

# Load packages

library(dplyr)
library(cregg)
library(ggplot2)
library(vtable)
library(xtable)

# Set working directory

wd <- "[YOUR DIRECTORY]"
setwd(wd)

# Load functions for plotting

source("conjoint_plot_functions.R")

# Load data

df <- read.csv("data/conjoint_data.csv")

# Convert conjoint attributes to factors and set levels 

df$Sizeofproject <- factor(df$Sizeofproject, levels = c("$100 million", "$500 million"))
df$Projecttype <- factor(df$Projecttype, levels=c("Transportation infrastructure", "Tax collection capacity", "Civil society"))
df$Conditionalities <- factor(df$Conditionalities, levels = c("No conditions", "Democracy and human rights", "Economic policy", "Social policy"))
df$Procurement <- factor(df$Procurement, levels = c("Untied", "Tied"))
df$Regulationsduringimplementation <- factor(df$Regulationsduringimplementation, levels = c("No regulations", "Environment", "Corruption", "Labor"))
df$TermsofLending <- factor(df$TermsofLending, levels = c("Resource-backed commercial", "2% concessional", "8% concessional", "Grant"))
df$Reporting <- factor(df$Reporting, levels = c("Not publicly disclosed", "Publicly disclosed"))

# Convert covariates to factors

controls <- c("government_st", "china_st", "senior_binary_st", "country_binary_st", 
              "devpartner_st", "cce_above_median", "cce_above_25pctile", "cce_above_75pctile", "democracy", "ida_eligible","unsc_member_2020","china_aid_gdp_rec",
              "imp_peace_just_inst","imp_work_growth","imp_any_enviro","gov_open_agree","gov_role_inter_none","donor_respect")
df[,controls] <- lapply(df[,controls], factor)

# Merge in weights

weights <- read.csv('data/weights.csv')

df_w <- merge(df, weights, by='ResponseId') 

# Create separate dataframe for senior-level government respondents who are not development partners and who work in home country

df_home_senior_govt_not_devpartner <- subset(df_w, devpartner_st=="Not development partner" 
                                             & country_binary_st=="Home country" 
                                             & government_st=="Government"
                                             & senior_binary_st=="Senior-level")

# Cretae separate dataframes for respondents from countries that are dependent on natural resources

df_natres <- subset(df_w, natres_depend_above_median=="More dependent")
df_natres_25 <- subset(df_w, natres_depend_above_25pctile=="More dependent")
df_natres_75 <- subset(df_w, natres_depend_above_75pctile=="More dependent")
df_natres_consume <- subset(df_w, natres_consume_above_median=="More dependent")

#Create separate dataframes for ex-pat respondent and respondents working in home country

df_home <- subset(df_w, country_binary_st=="Home country")
df_expat <- subset(df_w, country_binary_st=="Expat")

# Create separate dataframe for non-development partner respondents

df_not_devpartner <- subset(df_w, devpartner_st=="Not development partner")

# Create separate dataframes for individual countries

df_w_Afghanistan <- subset(df_w, country_name_st=="Afghanistan")
df_w_Ghana <- subset(df_w, country_name_st=="Ghana")
df_w_Honduras <- subset(df_w, country_name_st=="Honduras")
df_w_Kosovo <- subset(df_w, country_name_st=="Kosovo")
df_w_Malawi <- subset(df_w, country_name_st=="Malawi")
df_w_Moldova <- subset(df_w, country_name_st=="Moldova")
df_w_Nepal <- subset(df_w, country_name_st=="Nepal")
df_w_Niger <- subset(df_w, country_name_st=="Niger")
df_w_Nigeria <- subset(df_w, country_name_st=="Nigeria")
df_w_Uganda <- subset(df_w, country_name_st=="Uganda")
df_w_Bosnia_and_Herzegovina <- subset(df_w, country_name_st=="Bosnia and Herzegovina")
df_w_Costa_Rica <- subset(df_w, country_name_st=="Costa Rica")
df_w_Dominican_Republic <- subset(df_w, country_name_st=="Dominican Republic")
df_w_El_Salvador <- subset(df_w, country_name_st=="El Salvador")

# Create separate dataframe for government officials and parliamentarians in Uganda

df_w_Uganda_gov <- subset(df_w_Uganda, government_st=="Government")

# Create separate dataframe of controls for descriptives

df_controls_only_collapse <- subset(df_w, select=c("ResponseId", "education_st", "experience_st", "china_st")) %>% distinct()

  df_controls_only_collapse[sapply(df_controls_only_collapse, is.character)] <- lapply(df_controls_only_collapse[sapply(df_controls_only_collapse, is.character)], 
                                                                                     as.factor)

  df_controls_only_collapse$education_st <- factor(df_controls_only_collapse$education_st,
                                                   levels=c("Primary","Secondary","Technical/Vocational","University/College","Postgraduate"),
                                                   labels=c("Primary","Secondary","Technical/vocational","University/college","Postgraduate"))
  
  df_controls_only_collapse$experience_st <- factor(df_controls_only_collapse$experience_st,
                                                    levels=c("0-5 years","6-10 years","11-15 years","16+ years"))




# Replicate bottom half of Table A2 (NB: Table A2 reports the number of respondents for which each variable is non-missing, not the number of respondents who take a particular value on each variable)

st(subset(df_controls_only_collapse, select = -ResponseId),
   labels = c("Education","Experience","Assistance from China"),
   title="\\textbf{Summary statistics}",fit.page = '.5\\textwidth',anchor="tab:summ_stats",
   out = 'latex',file="tables/tableA2_bottom.tex")


# Replicate Figure 3

plot1 <- cregg::cj(
  df_w, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p1

ggsave("figures/figure3.png", height = 7 , width = 6)



# Replicate Figure 4

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ government_st, estimate = "mm",
                    weights= ~non_res_w)

p1 <- plot_fnct_mm(plot1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p1

ggsave("figures/figure4.png", height = 7 , width = 6)




# Replicate Figure 5

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ government_st, estimate = "mm_diff",
                    weights= ~ non_res_w)

p1 <- plot_fnct_mmdiff(plot1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

p1

ggsave("figures/figure5.png", height = 7 , width = 6)




# Replicate Figure 6

plot1 <- cregg::cj(
  df_w, 
  y ~ Sizeofproject + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  by = ~ Projecttype,
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::facet_wrap(~BY, ncol = 3L) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p1

ggsave("figures/figure6.png", height = 7 , width = 9)




# Replicate Figure 7

plot1 <- cregg::cj(
  df_home_senior_govt_not_devpartner, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())
p1

ggsave("figures/figure7.png", height = 7 , width = 6)





# Replicate Figure 8

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm",
  by = ~china_st,
  weights= ~non_res_w
)

p1 <- plot_fnct_mm(plot1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

p1 

ggsave("figures/figure8.png", height = 7 , width = 6)



# Replicate Figure 9

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm_diff",
  by = ~china_st,
  weights = ~non_res_w
)

p1 <- plot_fnct_mmdiff(plot1)+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

p1 

ggsave("figures/figure9.png", height = 7 , width = 6)




# Replicate analyses in section "preventing abuse by recipient governments" (no tables or figures)

df_w$gov_role_inter_none_num <- ifelse(df_w$gov_role_inter_none == "International actors should defer", 1, 0)
  summary(df_w$gov_role_inter_none_num) # 8.1% of respondents believe international actors should avoid intervening across all priorities

df_w$imp_peace_just_inst_num <- ifelse(df_w$imp_peace_just_inst == "Institutional SDG among most important", 1, 0)
  summary(df_w$imp_peace_just_inst_num) # 56.1% of respondents believe "peace, justice, and strong institutions" is most important SDG
  
summary(df_w$imp_educ) # 61.5% of respondents believe "quality education" is most important SDG

df_w$imp_work_growth_num <- ifelse(df_w$imp_work_growth == "Labor SDG among most important", 1, 0)
  summary(df_w$imp_work_growth_num) # 53.1% of respondents believe "decent work and economic growth" is most important SDG

summary(df_w$gov_jobs_disagree) # 78.8% of respondents believe government does not generate enough jobs

df_w$gov_open_disagree_num <- ifelse(df_w$gov_open_disagree == "Government is not open or transparent", 1, 0)
  summary(df_w$gov_open_disagree_num) # 51.3% of respondents believe government is not open or transparent

summary(df_w$gov_open_role_inter_none) # 7.0% of respondents believe open and transparent government is domestic concern
  
summary(df_w$gov_open_role_inter_mob_int) # 52.4% of respondents believe international community should mobilize foreign actors to exert pressure

summary(df_w$gov_open_role_inter_mob_dom) # 42.9% of respondents believe international community should mobilize domestic actors to exert pressure

summary(df_w$gov_open_role_inter_aware) # 36.9% of respondents believe international community should raise awareness



# Replicate Figure A2

plot1 <- cregg::cj(
  df_natres, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())
p1

ggsave("figures/figureA2.png", height = 7 , width = 6)




# Replicate Figure A3

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ senior_binary_st, estimate = "mm",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mm(plot1)
p1

ggsave("figures/figureA3_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ senior_binary_st, estimate = "mm_diff",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mmdiff(plot1)
p1

ggsave("figures/figureA3_right.png", height = 7 , width = 6)




# Replicate Figure A4

plot1 <- cregg::cj(
  df_home, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())
p1

ggsave("figures/figureA4.png", height = 7 , width = 6)




# Replicate Figure A5

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm",
  by = ~country_binary_st,
  weights= ~non_res_w
)

p1 <- plot_fnct_mm(plot1)
p1 

ggsave("figures/figureA5_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm_diff",
  by = ~country_binary_st,
  weights = ~non_res_w
)

p1 <- plot_fnct_mmdiff(plot1)
p1 

ggsave("figures/figureA5_right.png", height = 7 , width = 6)




# Replicate Figure A6

plot1 <- cregg::cj(
  df_not_devpartner, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())
p1

ggsave("figures/figureA6.png", height = 7 , width = 6)






# Replicate Figure A7

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm",
  by = ~devpartner_st,
  weights= ~non_res_w
)

p1 <- plot_fnct_mm(plot1)
p1 

ggsave("figures/figureA7_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm_diff",
  by = ~devpartner_st,
  weights = ~non_res_w
)

p1 <- plot_fnct_mmdiff(plot1)
p1 

ggsave("figures/figureA7_right.png", height = 7 , width = 6)




# Replicate Figure A8

plot1 <- cregg::cj(
  df_w, 
  y ~ Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  by = ~ Sizeofproject,
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::facet_wrap(~BY, ncol = 3L) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())
p1

ggsave("figures/figureA8.png", height = 7 , width = 9)





# Replicate Figure A9

plot1 <- cregg::cj(
  df_w, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  by = ~ Procurement,
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1 <- plot(plot1,
           size= 1.5,
           vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::facet_wrap(~BY, ncol = 3L) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c( 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','plain','plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())
p1

ggsave("figures/figureA9.png", height = 7 , width = 9)




# Replicate Figure A10

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ cce_above_median, estimate = "mm",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mm(plot1)
p1

ggsave("figures/figureA10_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ cce_above_median, estimate = "mm_diff",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mmdiff(plot1)
p1

ggsave("figures/figureA10_right.png", height = 7 , width = 6)



# Replicate Figure A11
plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ democracy, estimate = "mm",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mm(plot1)
p1

ggsave("figures/figureA11_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ democracy, estimate = "mm_diff",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mmdiff(plot1)
p1

ggsave("figures/figureA11_right.png", height = 7 , width = 6)



# Replicate Figure A12

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm",
  by = ~ida_eligible,
  weights= ~non_res_w
)

p1 <- plot_fnct_mm(plot1)
p1 

ggsave("figures/figureA12_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm_diff",
  by = ~ida_eligible,
  weights = ~non_res_w
)

p1 <- plot_fnct_mmdiff(plot1)
p1 

ggsave("figures/figureA12_right.png", height = 7 , width = 6)





# Replicate Figure A13

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm",
  by = ~unsc_member_2020,
  weights= ~non_res_w
)

p1 <- plot_fnct_mm(plot1)
p1 

ggsave("figures/figureA13_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(
  df_w,
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ResponseId,
  estimate = "mm_diff",
  by = ~unsc_member_2020,
  weights = ~non_res_w
)

p1 <- plot_fnct_mmdiff(plot1)
p1 

ggsave("figures/figureA13_right.png", height = 7 , width = 6)




# Replicate Figure A14

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ china_aid_gdp_rec, estimate = "mm",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mm(plot1)
p1

ggsave("figures/figureA14_left.png", height = 7 , width = 6)

plot1 <- cregg::cj(df_w, 
                    y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                    feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                    id = ~ ResponseId, 
                    by = ~ china_aid_gdp_rec, estimate = "mm_diff",
                    weights = ~ non_res_w)

p1 <- plot_fnct_mmdiff(plot1)
p1

ggsave("figures/figureA14_right.png", height = 7 , width = 6)



# Replicate Figure A15

plot1_Afghanistan <- cregg::cj(
  df_w_Afghanistan, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Afghanistan <- plot(plot1_Afghanistan,
                       size= 1.5,
                       vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Afghanistan")
p1_Afghanistan

ggsave("figures/figureA15_Afghanistan.png", height = 7 , width = 6)




#plot AMCEs for Bosnia and Herzegovina

plot1_Bosnia_and_Herzegovina <- cregg::cj(
  df_w_Bosnia_and_Herzegovina, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Bosnia_and_Herzegovina <- plot(plot1_Bosnia_and_Herzegovina,
                                  size= 1.5,
                                  vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Bosnia and Herzegovina")
p1_Bosnia_and_Herzegovina


ggsave("figures/figureA15_Bosnia_and_Herzegovina.png", height = 7 , width = 6)





#plot AMCEs for Costa Rica

plot1_Costa_Rica <- cregg::cj(
  df_w_Costa_Rica, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Costa_Rica <- plot(plot1_Costa_Rica,
                      size= 1.5,
                      vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Costa Rica")
p1_Costa_Rica


ggsave("figures/figureA15_Costa_Rica.png", height = 7 , width = 6)




#plot AMCEs for Dominican Republic

plot1_Dominican_Republic <- cregg::cj(
  df_w_Dominican_Republic, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Dominican_Republic <- plot(plot1_Dominican_Republic,
                              size= 1.5,
                              vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Dominican Republic")
p1_Dominican_Republic


ggsave("figures/figureA15_Dominican_Republic.png", height = 7 , width = 6)




#plot AMCEs for El Salvador

plot1_El_Salvador <- cregg::cj(
  df_w_El_Salvador, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_El_Salvador <- plot(plot1_El_Salvador,
                       size= 1.5,
                       vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("El Salvador")
p1_El_Salvador


ggsave("figures/figureA15_El_Salvador.png", height = 7 , width = 6)






plot1_Ghana <- cregg::cj(
  df_w_Ghana, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Ghana <- plot(plot1_Ghana,
                 size= 1.5,
                 vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Ghana")
p1_Ghana

ggsave("figures/figureA15_Ghana.png", height = 7 , width = 6)




plot1_Honduras <- cregg::cj(
  df_w_Honduras, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Honduras <- plot(plot1_Honduras,
                    size= 1.5,
                    vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Honduras")
p1_Honduras

ggsave("figures/figureA15_Honduras.png", height = 7 , width = 6)



plot1_Kosovo <- cregg::cj(
  df_w_Kosovo, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Kosovo <- plot(plot1_Kosovo,
                  size= 1.5,
                  vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Kosovo")
p1_Kosovo

ggsave("figures/figureA15_Kosovo.png", height = 7 , width = 6)




plot1_Malawi <- cregg::cj(
  df_w_Malawi, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Malawi <- plot(plot1_Malawi,
                  size= 1.5,
                  vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Malawi")
p1_Malawi

ggsave("figures/figureA15_Malawi.png", height = 7 , width = 6)



plot1_Moldova <- cregg::cj(
  df_w_Moldova, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Moldova <- plot(plot1_Moldova,
                   size= 1.5,
                   vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Moldova")
p1_Moldova


ggsave("figures/figureA15_Moldova.png", height = 7 , width = 6)




plot1_Nepal <- cregg::cj(
  df_w_Nepal, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Nepal <- plot(plot1_Nepal,
                 size= 1.5,
                 vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Nepal")
p1_Nepal


ggsave("figures/figureA15_Nepal.png", height = 7 , width = 6)




plot1_Niger <- cregg::cj(
  df_w_Niger, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Niger <- plot(plot1_Niger,
                 size= 1.5,
                 vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Niger")
p1_Niger


ggsave("figures/figureA15_Niger.png", height = 7 , width = 6)



plot1_Nigeria <- cregg::cj(
  df_w_Nigeria, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Nigeria <- plot(plot1_Nigeria,
                   size= 1.5,
                   vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Nigeria")
p1_Nigeria


ggsave("figures/figureA15_Nigeria.png", height = 7 , width = 6)




plot1_Uganda <- cregg::cj(
  df_w_Uganda, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Uganda <- plot(plot1_Uganda,
                  size= 1.5,
                  vline = 0) + 
  
  ggplot2::scale_x_continuous(
  ) + 
  
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Uganda")
p1_Uganda


ggsave("figures/figureA15_Uganda.png", height = 7 , width = 6)





# Replicate Figure A16

plot1_Uganda <- cregg::cj(
  df_w_Uganda_gov, 
  y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
  feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
  id = ~ ResponseId,
  estimate = "amce",
  weights = ~ non_res_w
)

p1_Uganda <- plot(plot1_Uganda,
                  size= 1.5,
                  vline = 0) + 
  ggplot2::scale_x_continuous(
  ) + 
  ggplot2::geom_text(
    aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
    colour = "black", 
    size = 2.5,
    position = position_nudge(y = .5)
  ) + 
  theme(legend.position="none")+ 
  theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
  theme(panel.grid.major = element_line())+
  ggtitle("Uganda (government officials only)")
p1_Uganda

ggsave("figures/figureA16.png", height = 7 , width = 6)



# Replicate analyses in SI section C.4 (no figures)

  # Plot marginal means for respondents who did and did not select "peace, justice, and strong institutions” (SDG 16) as one of the “most important issues” for development
  
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~imp_peace_just_inst,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
  
  # Plot marginal means for respondents who did and did not select "decent work and economic growth” (SDG 8) as one of the “most important issues” for development
      
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~imp_work_growth,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
      
  # Plot marginal means for respondents who did and did not select any environmental SDG (SDG 13, 14, or 15) as one of the “most important issues” for development
      
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~imp_any_enviro,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
      
  # Plot marginal means for respondents who did and did not agree that their country has an "open and accountable government"
      
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~gov_open_agree,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
      
  # Plot marginal means for respondents who did and did not agree that international actors should avoid intervening in “domestic problems”
      
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~gov_role_inter_none,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
      

  # Plot marginal means for respondents who did and did not agree that donors gain influence by “respecting the government’s authority over final decisions”
      
      plot1 <- cregg::cj(
        df_w,
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ResponseId,
        estimate = "mm",
        by = ~donor_respect,
        weights= ~non_res_w
      )
      
      p1 <- plot_fnct_mm(plot1)
      p1 
      
      
      
# Replicate analyses in SI section D.2 (no figures)
    
    # Estimate AMCEs for respondents living in natural resource dependent countries, using 25th percentile as cutoff
      
      plot1 <- cregg::cj(
        df_natres_25, 
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ ResponseId,
        estimate = "amce",
        weights = ~ non_res_w
      )
      
      p1 <- plot(plot1,
                 size= 1.5,
                 vline = 0) + 
        ggplot2::scale_x_continuous(
        ) + 
        ggplot2::geom_text(
          aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
          colour = "black", 
          size = 2.5,
          position = position_nudge(y = .5)
        ) + 
        theme(legend.position="none")+ 
        theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
        theme(panel.grid.major = element_line())
      p1
      

  # Estimate AMCEs for respondents living in natural resource dependent countries, using 75th percentile as cutoff
      
      plot1 <- cregg::cj(
        df_natres_75, 
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ ResponseId,
        estimate = "amce",
        weights = ~ non_res_w
      )
      
      p1 <- plot(plot1,
                 size= 1.5,
                 vline = 0) + 
        ggplot2::scale_x_continuous(
        ) + 
        ggplot2::geom_text(
          aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
          colour = "black", 
          size = 2.5,
          position = position_nudge(y = .5)
        ) + 
        theme(legend.position="none")+ 
        theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
        theme(panel.grid.major = element_line())
      p1
      
      
  # Estimate AMCEs for respondents living in natural resource dependent countries, using consumption as proxy for dependence
      
      plot1 <- cregg::cj(
        df_natres_75, 
        y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting,
        feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
        id = ~ ResponseId,
        estimate = "amce",
        weights = ~ non_res_w
      )
      
      p1 <- plot(plot1,
                 size= 1.5,
                 vline = 0) + 
        ggplot2::scale_x_continuous(
        ) + 
        ggplot2::geom_text(
          aes(label = ifelse(is.na(std.error), "", sprintf("%0.2f (%0.2f)", estimate, std.error))),
          colour = "black", 
          size = 2.5,
          position = position_nudge(y = .5)
        ) + 
        theme(legend.position="none")+ 
        theme(axis.text.y = element_text(face = c('plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain','bold', 'plain', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'plain', 'bold', 'plain', 'plain', 'bold')))+
        theme(panel.grid.major = element_line())
      p1
      
      

      
      
# Replicate analyses in SI section D.8 (no figures)
      
    # Estimate marginal means for respondents living in more and less corrupt countries, using 25th percentile as cutoff
      
      plot1 <- cregg::cj(df_w, 
                         y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                         feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                         id = ~ ResponseId, 
                         by = ~ cce_above_25pctile, estimate = "mm",
                         weights = ~ non_res_w)
      
      p1 <- plot_fnct_mm(plot1)
      p1
      
      plot1 <- cregg::cj(df_w, 
                         y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                         feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                         id = ~ ResponseId, 
                         by = ~ cce_above_25pctile, estimate = "mm_diff",
                         weights = ~ non_res_w)
      
      p1 <- plot_fnct_mmdiff(plot1)
      p1
      
      
  # Estimate marginal means for respondents living in more and less corrupt countries, using 75th percentile as cutoff
      
      plot1 <- cregg::cj(df_w, 
                         y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                         feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                         id = ~ ResponseId, 
                         by = ~ cce_above_75pctile, estimate = "mm",
                         weights = ~ non_res_w)
      
      p1 <- plot_fnct_mm(plot1)
      p1
      
      plot1 <- cregg::cj(df_w, 
                         y ~ Sizeofproject + Projecttype + Conditionalities + Procurement + Regulationsduringimplementation + TermsofLending + Reporting, 
                         feature_labels = list(Sizeofproject = "Project size", Projecttype = "Project type", Conditionalities = "Conditionalities", Procurement = "Procurement", Regulationsduringimplementation = "Regulations during implementation", TermsofLending = "Terms of lending", Reporting = "Reporting"),
                         id = ~ ResponseId, 
                         by = ~ cce_above_75pctile, estimate = "mm_diff",
                         weights = ~ non_res_w)
      
      p1 <- plot_fnct_mmdiff(plot1)
      p1
      