##### Code for cross-national analysis in Section 3 and Appendices A and B. #####
#install.packages(c("readr","dplyr","margins","sjPlot")) # If packages not yet installed
# Load packages
library(readr)
library(dplyr)
library(margins)
library(sjPlot)

### Load protest event data (alter file path as needed)
repdat <- read_csv("cohesion_crossnat_replication_data.csv")

### View country frequencies in Table 1
table(repdat$country)

### Create factor variables for crowd size and country
repdat$size_factor <- factor(repdat$size, levels = c(1,2,3,4), labels=c("0-1,000","1,001-10,000","10,001-100,000","100,001-million"))
repdat$country_factor <- factor(repdat$country) # Levels are already labeled

### Normalize Word2Vec and LSA measures of crowd cohesion
repdat$word2vec_1_all_para_normalized <- (repdat$word2vec_1_all_para - min(repdat$word2vec_1_all_para))/(max(repdat$word2vec_1_all_para)-min(repdat$word2vec_1_all_para))
repdat$lsa_1_all_para_normalized <- (repdat$lsa_1_all_para - min(repdat$lsa_1_all_para))/(max(repdat$lsa_1_all_para)-min(repdat$lsa_1_all_para))

### Logit models using Word2Vec as cohesion measure
model1.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + country_factor, family = binomial, data = repdat)
model2.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + size_factor + country_factor, family = binomial, data = repdat)
model3.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + size_factor + violence + country_factor, family = binomial, data = repdat)
model4.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + size_factor + violence + intattention + country_factor, family = binomial, data = repdat)
model5.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + size_factor + violence + intattention + e_migdppcln + country_factor, family = binomial, data = repdat)
model6.glm <- glm(concession_delivered ~ word2vec_1_all_para_normalized + concession_promised + size_factor + violence + intattention + e_migdppcln + v2x_polyarchy + country_factor, family = binomial, data = repdat)

### Marginal effects in Fig. 1 (see LaTeX template, "latex_plots.tex", for recreating figure)
effects_mod1 <- margins(model1.glm)
summary(effects_mod1)
effects_mod2 <- margins(model2.glm)
summary(effects_mod2)
effects_mod3 <- margins(model3.glm)
summary(effects_mod3)
effects_mod4 <- margins(model4.glm)
summary(effects_mod4)
effects_mod5 <- margins(model5.glm)
summary(effects_mod5)
effects_mod6 <- margins(model6.glm)
summary(effects_mod6)

### Plotting predicted probability of concessions (Fig. 2)
plot_model(model1.glm, type = "pred",show.ci=TRUE,ci.lvl = .8,terms="word2vec_1_all_para_normalized",condition = c(country_factor = "Belgium"),axis.title = c("word2vec","Predicted Probability of Concession"),title="")

##### Code for Analyses in the Supplemental Materials #####

### Robustness test using LSA instead of Word2Vec as cohesion measure, for Appendix A
## Logit models using LSA
model7.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + country_factor, family = binomial, data = repdat)
model8.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + size_factor + country_factor, family = binomial, data = repdat)
model9.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + size_factor + violence + country_factor, family = binomial, data = repdat)
model10.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + size_factor + violence + intattention + country_factor, family = binomial(), data = repdat)
model11.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + size_factor + violence + intattention + e_migdppcln + country_factor, data = repdat)
model12.glm <- glm(concession_delivered ~ lsa_1_all_para_normalized + concession_promised + size_factor + violence + intattention + e_migdppcln + v2x_polyarchy + country_factor, family = binomial, data = repdat)

## Marginal effects in Fig. A.2 (see LaTeX template, "latex_plots.tex", for reproducing figure); Fig. A.1 simply copies Fig. 1 (see code in lines 28-40 above)
effects_mod7 <- margins(model7.glm)
summary(effects_mod7)
effects_mod8 <- margins(model8.glm)
summary(effects_mod8)
effects_mod9 <- margins(model9.glm)
summary(effects_mod9)
effects_mod10 <- margins(model10.glm)
summary(effects_mod10)
effects_mod11 <- margins(model11.glm)
summary(effects_mod11)
effects_mod12 <- margins(model12.glm)
summary(effects_mod12)

## Predicted outcomes in Fig. A.3
# Using Word2Vec (left panel):
plot_model(model1.glm, type = "pred",show.ci=TRUE,ci.lvl = .8,terms="word2vec_1_all_para_normalized",condition = c(country_factor = "Belgium"),axis.title = c("word2vec","Predicted Probability of Concession"),title="")
# Using LSA (right panel):
plot_model(model7.glm, type = "pred",show.ci=TRUE,ci.lvl = .8,terms="lsa_1_all_para_normalized",condition = c(country_factor = "Belgium"),axis.title = c("LSA","Predicted Probability of Concession"),title="")

### Calculate summary stats for Table B.1
summary(repdat$concession_delivered) # Variable labeled "Concession" in table
sd(repdat$concession_delivered)
summary(repdat$word2vec_1_all_para_normalized)
sd(repdat$word2vec_1_all_para_normalized)
summary(repdat$lsa_1_all_para_normalized)
sd(repdat$lsa_1_all_para_normalized)
summary(repdat$concession_promised)
sd(repdat$concession_promised)
summary(repdat$violence)
sd(repdat$violence)
summary(repdat$intattention)
sd(repdat$intattention, na.rm = TRUE)
summary(repdat$e_migdppcln)
sd(repdat$e_migdppcln)
summary(repdat$v2x_polyarchy) # Variable labeled "Electoral Democracy Index" in table
sd(repdat$v2x_polyarchy)
# Make dummy variables for each level of crowd size in order to summarize each level
# Crowd size = 0-1,000
repdat$size1 <- NA
repdat$size1[repdat$size == 1] <- 1
repdat$size1[repdat$size == 2] <- 0
repdat$size1[repdat$size == 3] <- 0
repdat$size1[repdat$size == 4] <- 0
summary(repdat$size1) 
sd(repdat$size1)
# Crowd size = 1,001-10,000
repdat$size2 <- NA
repdat$size2[repdat$size == 1] <- 0
repdat$size2[repdat$size == 2] <- 1
repdat$size2[repdat$size == 3] <- 0
repdat$size2[repdat$size == 4] <- 0
summary(repdat$size2)
sd(repdat$size2)
# Crowd size = 10,001-100,000
repdat$size3 <- NA
repdat$size3[repdat$size == 1] <- 0
repdat$size3[repdat$size == 2] <- 0
repdat$size3[repdat$size == 3] <- 1
repdat$size3[repdat$size == 4] <- 0
summary(repdat$size3)
sd(repdat$size3)
# Crowd size = 100,001-million
repdat$size4 <- NA
repdat$size4[repdat$size == 1] <- 0
repdat$size4[repdat$size == 2] <- 0
repdat$size4[repdat$size == 3] <- 0
repdat$size4[repdat$size == 4] <- 1
summary(repdat$size4)
sd(repdat$size4)
