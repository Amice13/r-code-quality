library(parameters)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)

data_demo <- read.csv("./data_demo.csv")
data_demo$Privacy.Broad <- data_demo$General.Reassurance
data_demo$Individual.Good <- data_demo$Individual.Good

# subset data to just those that have known age and gender
dataAG <- data_demo
dataAG$Age[dataAG$Age == 'Unknown'] <- NA
dataAG$Gender[dataAG$Gender == 'Unknown'] <- NA
dataAG <- dataAG[complete.cases(dataAG), ]

data_Geo <- read.csv("./data_geo.csv")
data_Geo$Privacy.Broad <- data_Geo$General.Reassurance
data_Geo$Individual.Good <- data_Geo$Individual.Good
data_Geo$Density <- as.factor(data_Geo$Density)
levels(data_Geo$Density) <- c("Rural", "Rural", "Urban")
data_Geo$Density <- relevel(data_Geo$Density, "Urban")

# total number of impressions (7010271)
nrow(data_demo)

# Average impressions per campaign (500733.6)
mean((data_demo %>% group_by(Ad) %>% summarise(Impressions = n() ))$Impressions)

# Total clicks (28026)
sum(data_demo$Clicks)

# Average (0.398%) and Std (0.100%) Click Through Rate of 14 ad campaigns
mean((data_demo %>% group_by(Ad) %>% summarise(CTR = 100 * sum(Clicks)/n() ))$CTR)
sqrt(var((data_demo %>% group_by(Ad) %>% summarise(CTR = 100 * sum(Clicks)/n() ))$CTR))

# number (6858820) and percentage (97.8%) of impressions with parish 
nrow(data_Geo)
nrow(data_Geo) / nrow(data_demo) * 100

# number (3920232) and percentage (55.9%) of impressions with age and gender 
nrow(dataAG)
nrow(dataAG) / nrow(data_demo) * 100



# CTR for Collective (0.4578044) vs Individual (0.3411595) good ads
Appeals <- data_demo %>% group_by(Individual.Good) %>% summarise(CTR = 100 * sum(Clicks)/n(), 
                                                                   Clicks = sum(Clicks), 
                                                                   Impr = n())
Appeals$Appeal[Appeals$Individual.Good == 0] <- "Collective\nGood"
Appeals$Appeal[Appeals$Individual.Good == 1] <- "Individual\nGood"
Appeals$CTR

# ------------------------------------------------
# Make Figure 2A
# ------------------------------------------------

neurips_theme = theme(panel.grid.major = element_line(size = 0.5, color = "grey"),
                      legend.position = c(0.9,0.9), text = element_text(size = 18), 
                      plot.title = element_text(hjust = 0.5))

model_cg <- glm(Clicks ~ Individual.Good, family='binomial', data=data_demo)
data_demo$Individual.Good <- data_demo$Individual.Good == 0
model_ig <- glm(Clicks ~ Individual.Good, family='binomial', data=data_demo)
data_demo$Individual.Good <- data_demo$Individual.Good == 0
data_demo$Individual.Good <- int(data_demo$Individual.Good)
df_cg <- model_parameters(model_cg, df_method="wald", exponentiate=TRUE)
df_ig <- model_parameters(model_ig, df_method="wald", exponentiate=TRUE)
Appeals$lower <- c(df_cg$CI_low[1],df_ig$CI_low[1])*100
Appeals$upper <- c(df_cg$CI_high[1],df_ig$CI_high[1])*100


ggplot(data=Appeals, aes(x=Appeal, y = CTR)) + geom_col(position='dodge', aes(fill=Appeal), show.legend = FALSE) + coord_flip() + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) + 
  theme_minimal() + neurips_theme + theme(text = element_text(size = 24)) + 
  labs(x = "") 

# Statistical difference of CTR for Individual- vs Collective-good
prop.test(c(Appeals$Clicks[1], Appeals$Clicks[2]), c(Appeals$Impr[1], Appeals$Impr[2]))


# ------------------------------------------------
# Make Table 2
# ------------------------------------------------

# modeling the four transparency statements with *all* the data
model_all = glm(Clicks ~
                        Individual.Good + Privacy.Broad + NonTech.Control + 
                        Technical.Control + Data.Transparency 

                      , family='binomial', data = data_demo)

par <- model_parameters(model_all, df_method="wald", exponentiate=TRUE)



# modeling the four transparency statements with *demo* data
model_all_ag = glm(Clicks ~
                  Individual.Good + Privacy.Broad + NonTech.Control + 
                  Technical.Control + Data.Transparency 
                
                , family='binomial', data = dataAG)

par2 <- model_parameters(model_all_ag, df_method="wald", exponentiate=TRUE)

# modeling the four transparency statements with *geo* data
model_all_geo = glm(Clicks ~
                     Individual.Good + Privacy.Broad + NonTech.Control + 
                     Technical.Control + Data.Transparency 
                   
                   , family='binomial', data = data_Geo)

par3 <- model_parameters(model_all_geo, df_method="wald", exponentiate=TRUE)

stargazer(model_all, model_all_ag, model_all_geo,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("All data", "Just Demographic", "Just Geographic"),
          title="Modeling the five statemenst",
          label="tbl:model_all")


# ------------------------------------------------
# Make Table 3
# ------------------------------------------------

# modeling the four transparency statements *and their interactions* with *all* the data
model_all_inter = glm(Clicks ~
                        Individual.Good + Privacy.Broad + NonTech.Control + 
                        Technical.Control + Data.Transparency 
                      + Individual.Good*(Data.Transparency+Privacy.Broad + NonTech.Control+Technical.Control)
                      
                      , family='binomial', data = data_demo)

par <- model_parameters(model_all_inter, df_method="wald", exponentiate=TRUE)

# modeling the four transparency statements *and their interactions* with *demo* data
model_all_inter_ag = glm(Clicks ~
                           Individual.Good + Privacy.Broad + NonTech.Control + 
                           Technical.Control + Data.Transparency 
                         + Individual.Good*(Data.Transparency+Privacy.Broad + NonTech.Control+Technical.Control)
                         
                         , family='binomial', data = dataAG)

par2 <- model_parameters(model_all_inter_ag, df_method="wald", exponentiate=TRUE)

# modeling the four transparency statements *and their interactions* with *geo* data
model_all_inter_geo = glm(Clicks ~
                           Individual.Good + Privacy.Broad + NonTech.Control + 
                           Technical.Control + Data.Transparency 
                         + Individual.Good*(Data.Transparency+Privacy.Broad + NonTech.Control+Technical.Control)
                         
                         , family='binomial', data = data_Geo)

par3 <- model_parameters(model_all_inter_geo, df_method="wald", exponentiate=TRUE)

stargazer(model_all_inter, model_all_inter_ag, model_all_inter_geo,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("All data", "Just Demographic", "Just Geographic"),
          title="Modeling the interaction of the appeal with the privacy and transparency statements",
          label="tbl:model_all_inter")

# ------------------------------------------------
# Make Table 4
# ------------------------------------------------


# modeling transparency statements for collective-good ads
model_all_cg = glm(Clicks ~
                     Privacy.Broad + NonTech.Control +
                     Technical.Control + Data.Transparency
                   
                   , family='binomial', data = data_demo %>% filter(Individual.Good %in% "0"))

par <- model_parameters(model_all_cg, df_method="wald", exponentiate=TRUE)

# modeling transparency statements for collective-good ads *demo* data
model_all_cg_demo = glm(Clicks ~
                        Privacy.Broad + NonTech.Control +
                        Technical.Control + Data.Transparency
                      
                      , family='binomial', data = dataAG %>% filter(Individual.Good %in% "0"))

par2 <- model_parameters(model_all_cg_ag, df_method="wald", exponentiate=TRUE)

# modeling transparency statements for collective-good ads *geo* data
model_all_cg_geo = glm(Clicks ~
                        Privacy.Broad + NonTech.Control +
                        Technical.Control + Data.Transparency
                      
                      , family='binomial', data = data_Geo %>% filter(Individual.Good %in% "0"))

par3 <- model_parameters(model_all_cg_geo, df_method="wald", exponentiate=TRUE)

stargazer(model_all_cg, model_all_cg_demo, model_all_cg_geo,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("All data", "Just Demographic", "Just Geographic"),
          title="Modeling the privacy and transparency statements for Collective-Good ads",
          label="tbl:model_all_cg")


# ------------------------------------------------
# Make Table 5
# ------------------------------------------------

# modeling transparency statements for individual-good ads
model_all_ig = glm(Clicks ~
                     Privacy.Broad + NonTech.Control +
                     Technical.Control + Data.Transparency
                   
                   , family='binomial', data = data_demo %>% filter(Individual.Good %in% "1"))

par <- model_parameters(model_all_ig, df_method="wald", exponentiate=TRUE)

# modeling transparency statements for individual-good ads *demo* data
model_all_ig_demo = glm(Clicks ~
                        Privacy.Broad + NonTech.Control +
                        Technical.Control + Data.Transparency
                      
                      , family='binomial', data = dataAG %>% filter(Individual.Good %in% "1"))

par2 <- model_parameters(model_all_ig_ag, df_method="wald", exponentiate=TRUE)

# modeling transparency statements for individual-good ads *geo* data
model_all_ig_geo = glm(Clicks ~
                        Privacy.Broad + NonTech.Control +
                        Technical.Control + Data.Transparency
                      
                      , family='binomial', data = data_Geo %>% filter(Individual.Good %in% "1"))

par3 <- model_parameters(model_all_ig_geo, df_method="wald", exponentiate=TRUE)

stargazer(model_all_ig, model_all_ig_demo, model_all_ig_geo,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("All data", "Just Demographic", "Just Geographic"),
          title="Modeling the privacy and transparency statements for Individual-Good ads",
          label="tbl:model_all_ig")


# ------------------------------------------------
# Make Table 6
# ------------------------------------------------

# modeling the four transparency statements with *demographic* the data
model_demo = glm(Clicks ~ Age + Gender +
                         Individual.Good + Privacy.Broad + NonTech.Control + 
                         Technical.Control + Data.Transparency 

                       , family='binomial', data = dataAG)

par <- model_parameters(model_demo, df_method="wald", exponentiate=TRUE)


# modeling the four transparency statements with *geographic* the data
model_geo =glm(Clicks ~ Density +
                       Individual.Good + Privacy.Broad + NonTech.Control + 
                       Technical.Control + Data.Transparency 
                     , family='binomial', data = data_Geo)


par2 <- model_parameters(model_geo, df_method = "wald",exponentiate=TRUE)

stargazer(model_demo, model_geo,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high)), 
          column.labels = c("Just Demographic", "Just Geographic"),
          title="Modeling demographics and geographics",
          label="tbl:model_demo_geo")

# ------------------------------------------------
# Make Table 7
# ------------------------------------------------


# For CG, no age difference for oldest and youngest, but middle ages are less likely
model_demo_CG_Age = glm(Clicks ~ Age + Gender
                        , family='binomial', data = dataAG %>% filter(Individual.Good==0))
par <- model_parameters(model_demo_CG_Age, df_method="wald", exponentiate=TRUE)

# cg females have no age difference
cg_f =glm(Clicks ~ Age 
          , family='binomial', data = dataAG %>% filter(Individual.Good == 0 & Gender == 'Female'))
par2 <- model_parameters(cg_f, df_method = "wald",exponentiate=TRUE)

# cg males have same top-line effect of cg ads
cg_m =glm(Clicks ~ Age 
          , family='binomial', data = dataAG %>% filter(Individual.Good == 0 & Gender == 'Male'))
par3 <- model_parameters(cg_m, df_method = "wald",exponentiate=TRUE)

stargazer(model_demo_CG_Age, cg_f, cg_m,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("Collective-Good", "Collective-Good\nFemale", "Collective-Good\nMale"),
          title="Modeling the Age and Gender differences for Collective-Good ads",
          label="tbl:model_demo_CG_Age")

# ------------------------------------------------
# Make Table 8
# ------------------------------------------------

# For IB, no age difference 18-54, and then 55-64 and 65+ have higher CTRs
model_demo_IB_Age = glm(Clicks ~ Age + Gender
                        , family='binomial', data = dataAG %>% filter(Individual.Good==1))

par <- model_parameters(model_demo_IB_Age, df_method="wald", exponentiate=TRUE)

#ib females have no diff 18-44 and then increasing
ib_f =glm(Clicks ~ Age
          , family='binomial', data = dataAG %>% filter(Individual.Good == 1 & Gender == 'Female'))
par2 <- model_parameters(ib_f, df_method = "wald",exponentiate=TRUE)

#ib males have no diff for 18-44 and 65+, and decreasing for 45-64
ib_m =glm(Clicks ~ Age 
          , family='binomial', data = dataAG %>% filter(Individual.Good == 1 & Gender == 'Male'))
par3 <- model_parameters(ib_m, df_method = "wald",exponentiate=TRUE)

stargazer(model_demo_IB_Age, ib_f, ib_m,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high)), 
          column.labels = c("Individual-Good", "Individual-Good\nFemale", "Individual-Good\nMale"),
          title="Modeling the Age and Gender differences for Individual-Good ads",
          label="tbl:model_demo_IG_Age")






cg_m_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
            Technical.Control + Data.Transparency
          , family='binomial', data = dataAG %>% filter(Individual.Good==0) %>% filter(Gender=='Male'))

par <- model_parameters(cg_m_all, df_method="wald", exponentiate=TRUE)

# no effect from gender with technical statement and collective-good  (Female)
cg_f_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
            Technical.Control + Data.Transparency
          , family='binomial', data = dataAG %>% filter(Individual.Good==0) %>% filter(Gender=='Female'))

par2 <- model_parameters(cg_f_all, df_method="wald", exponentiate=TRUE)


# O.R.= 0.51 effect from gender with technical statement and individual-good  (Male)
ig_m_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
            Technical.Control + Data.Transparency
          , family='binomial', data = dataAG %>% filter(Individual.Good==1) %>% filter(Gender=='Male'))

par3 <- model_parameters(ig_m_all, df_method="wald", exponentiate=TRUE)

# O.R.= 0.69 effect from gender with technical statement and individual-good (Male)
ig_f_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
            Technical.Control + Data.Transparency
          , family='binomial', data = dataAG %>% filter(Individual.Good==1) %>% filter(Gender=='Female'))

par4 <- model_parameters(ig_f_all, df_method="wald", exponentiate=TRUE)

stargazer(cg_m_all, cg_f_all, ig_m_all, ig_f_all,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high),
                           data.frame(low = par4$CI_low, high = par4$CI_high)), 
          column.labels = c("Collective-Good\nMale", "Collective-Good\nFemale", "Individual-Good\nMale", "Individual-Good\nFemale"),
          title="Modeling the statement differences by Appeal and Gender",
          label="tbl:appeal_gender")


# ------------------------------------------------
# Make Table 1
# ------------------------------------------------

cg_m_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
                 Technical.Control + Data.Transparency
               , family='binomial', data = dataAG %>% filter(Individual.Good==0) %>% filter(Gender=='Male'))

par <- model_parameters(cg_m_all, df_method="wald", exponentiate=TRUE)

# no effect from gender with technical statement and collective-good  (Female)
cg_f_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
                 Technical.Control + Data.Transparency
               , family='binomial', data = dataAG %>% filter(Individual.Good==0) %>% filter(Gender=='Female'))

par2 <- model_parameters(cg_f_all, df_method="wald", exponentiate=TRUE)


# O.R.= 0.51 effect from gender with technical statement and individual-good  (Male)
ig_m_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
                 Technical.Control + Data.Transparency
               , family='binomial', data = dataAG %>% filter(Individual.Good==1) %>% filter(Gender=='Male'))

par3 <- model_parameters(ig_m_all, df_method="wald", exponentiate=TRUE)

# O.R.= 0.69 effect from gender with technical statement and individual-good (Male)
ig_f_all = glm(Clicks ~ Privacy.Broad + NonTech.Control +
                 Technical.Control + Data.Transparency
               , family='binomial', data = dataAG %>% filter(Individual.Good==1) %>% filter(Gender=='Female'))

par4 <- model_parameters(ig_f_all, df_method="wald", exponentiate=TRUE)

stargazer(cg_m_all, cg_f_all, ig_m_all, ig_f_all,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par$CI_low, high = par$CI_high),
                           data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high),
                           data.frame(low = par4$CI_low, high = par4$CI_high)), 
          column.labels = c("Collective-Good\nMale", "Collective-Good\nFemale", "Individual-Good\nMale", "Individual-Good\nFemale"),
          title="Modeling the statement differences by Appeal and Gender",
          label="tbl:appeal_gender")

# ------------------------------------------------
# Make Table 9
# ------------------------------------------------

density_int = glm(Clicks ~ Density*(Individual.Good + Privacy.Broad + NonTech.Control +
                                      Technical.Control + Data.Transparency)
                  , family='binomial', data = data_Geo)
par <- model_parameters(density_int, df_method="wald", exponentiate=TRUE)

density_int_IG = glm(Clicks ~ Density*(Individual.Good)
                  , family='binomial', data = data_Geo)
par2 <- model_parameters(density_int_IG, df_method="wald", exponentiate=TRUE)

density_int_PB = glm(Clicks ~ Density*(Privacy.Broad)
                     , family='binomial', data = data_Geo)
par3 <- model_parameters(density_int_PB, df_method="wald", exponentiate=TRUE)

density_int_NT = glm(Clicks ~ Density*(NonTech.Control)
                     , family='binomial', data = data_Geo)
par4 <- model_parameters(density_int_NT, df_method="wald", exponentiate=TRUE)

density_int_T = glm(Clicks ~ Density*(Technical.Control)
                     , family='binomial', data = data_Geo)
par5 <- model_parameters(density_int_T, df_method="wald", exponentiate=TRUE)

density_int_DT = glm(Clicks ~ Density*(Data.Transparency)
                     , family='binomial', data = data_Geo)
par6 <- model_parameters(density_int_DT, df_method="wald", exponentiate=TRUE)



stargazer(density_int_IG, density_int_PB,density_int_NT,density_int_T, density_int_DT,
          apply.coef = exp,t.auto=F, p.auto=F, report = "vcsp*", header=FALSE, ci=TRUE,
          ci.custom = list(data.frame(low = par2$CI_low, high = par2$CI_high),
                           data.frame(low = par3$CI_low, high = par3$CI_high),
                           data.frame(low = par4$CI_low, high = par4$CI_high),
                           data.frame(low = par5$CI_low, high = par5$CI_high),
                           data.frame(low = par6$CI_low, high = par6$CI_high)), 
          column.labels = c("Appeal", "Privacy: Broad", "Non-Technical Control", "Technical Control", "Data Transparency"),
          title="Modeling the statement differences with an interaction for Density",
          label="tbl:density_inter")


# ------------------------------------------------
# Make Figure 2B
# ------------------------------------------------

fig2b <- rbind(tibble(model_parameters(model_all_cg, 
                                       df_method="wald", 
                                       exponentiate=TRUE)) %>% add_column(Appeal = 'Collective-Good'), 
               tibble(model_parameters(model_all_ig, 
                                       df_method="wald", 
                                       exponentiate=TRUE)) %>% add_column(Appeal = 'Individual-Good'))
fig2b$Coefficient <- fig2b$Coefficient-1
fig2b$CI_low <- fig2b$CI_low-1
fig2b$CI_high <- fig2b$CI_high-1

fig2b %>% 
  mutate(Parameter = factor(Parameter, levels=c("(Intercept)", "Data.Transparency","Technical.Control","NonTech.Control","Privacy.Broad"))) %>% filter(Parameter!='(Intercept)') %>%
ggplot( 
       aes(x=Parameter,
           y = Coefficient, 
           fill=factor(Appeal))) + 
  geom_col(position='dodge') + coord_flip() + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), position=position_dodge(0.9), width = 0.2) + 
  theme_minimal() + neurips_theme +
  theme(legend.position = c(.2, .92)) + scale_fill_discrete("") + theme(text = element_text(size = 24)) + theme(legend.text = element_text(size=14)) + 
  labs(x = "", y="Odds Ratio") +
  geom_hline(yintercept=0) + 
  guides(fill = guide_legend(reverse = TRUE)) + 
  scale_y_continuous(breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2),
                     labels=c(.6, .7,.8,.9,1, 1.1, 1.2)) + 
  scale_x_discrete(labels=c("Data\nTransparency", "Privacy:\nTechnical", "Privacy:\nNon-Technical", "Privacy:\nBroad"))

# ------------------------------------------------
# Make Figure 2C
# ------------------------------------------------

fig2c <- rbind(tibble(model_parameters(model_demo, 
                                       df_method="wald", 
                                       exponentiate=TRUE)) %>% add_column(Type = 'Demographic'),
               tibble(model_parameters(model_density, 
                                       df_method="wald", 
                                       exponentiate=TRUE)) %>% add_column(Type = 'Urban'))
fig2c$Coefficient <- fig2c$Coefficient-1
fig2c$CI_low <- fig2c$CI_low-1
fig2c$CI_high <- fig2c$CI_high-1

fig2c %>% 
  mutate(Parameter = factor(Parameter, levels=c("(Intercept)", "DensityRural","GenderMale","Age65+","Age55 - 64", "Age45 - 54","Age35 - 44", "Age25 - 34"))) %>% filter(Parameter!='(Intercept)') %>%
  ggplot( 
    aes(x=Parameter,
        y = Coefficient)) + 
  geom_col(position='dodge', fill='#C0C0C0') + coord_flip() + 
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), position=position_dodge(0.9), width = 0.2) + 
  theme_minimal() + neurips_theme +
  theme(legend.position = c(.2, .93)) + scale_fill_discrete("") + theme(text = element_text(size = 24)) + 
  labs(x = "", y="Odds Ratio") +
  geom_hline(yintercept=0) + 
  geom_vline(xintercept=0) + 
  scale_y_continuous(breaks=c(-.4, -.3, -.2, -.1, 0, .1, .2),
                     labels=c(.6, .7,.8,.9,1, 1.1, 1.2)) + 
  scale_x_discrete(labels=c("Rural", "Male", "Age [65+]", "Age [55-65]","Age [45-54]","Age [35-44]","Age [25-34]")) 

# ------------------------------------------------
# Make Figure 3
# ------------------------------------------------

df <- (dataAG %>% group_by(Individual.Good, Age, Gender) %>% summarise(CTR = 100 * sum(Clicks)/n(), 
                                                                         Clicks = sum(Clicks), 
                                                                         Impr = n()))
df$upper <- 0
df$lower <- 0
for (i in 1:nrow(df)){
  CI <- binom.test(df[i,]$Clicks,df[i,]$Impr)$conf
  df[i,]$lower <- CI[1]*100
  df[i,]$upper <- CI[2]*100
}
df$Appeal[df$Individual.Good == 0] <- "Collective-Good"
df$Appeal[df$Individual.Good == 1] <- "Individual-Good"
p <- ggplot(df, aes(x=Age, y=CTR, group=Gender)) + 
  geom_line(aes(colour=Appeal, linetype=Gender), size =1) + 
  geom_ribbon(aes(x=Age, y=CTR, ymax=upper, ymin=lower),alpha=0.2) + 
  facet_grid((.~Appeal)) +
  ggtitle(sprintf("CTR by Age, Gender, and Appeal")) + 
  theme_minimal() + neurips_theme + theme(legend.position = 'bottom') +
  labs(y="CTR (%)", x="Age")  + 
  guides(colour="none")
p

