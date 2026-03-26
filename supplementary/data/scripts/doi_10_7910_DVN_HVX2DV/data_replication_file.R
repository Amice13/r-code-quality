##################################
### Paper: Climate atttidues  ###
#################################
library(haven)
library(tidyverse)
library(sjmisc)
library(lavaan)
library(corrplot)
library(sjPlot)
library(ggplot2)
library(psych)
library(dplyr)
library(sjlabelled)
library(corrplot)
library(gtools)

#IMPORTING DATA WITH THE HERE PROJECT
df_1 <- haven::read_dta(here::here("L_WIV2018_wave4_4p.dta")) #Wif Referendum 2018, vierde meting
df_2 <- haven::read_dta(here::here("cv19k_EN_1.0p.dta")) #Politics and Values Wave 11 Data can be found here: https://www.dataarchive.lissdata.nl/study_units/view/807
  
#select variables 
df_1_select <- select(df_1, nomem_encr, maandnr, aRandom, v5a, v5b, v5c, v7a, v7b, v7c,
                      v9a, v9b, v9c, v9d, v10a, v10b, v10c,    
                      geslacht, gebjaar, leeftijd, oplcat, duur)

df_2_select <- select(df_2, nomem_encr, cv19k101, 
                      cv19k013, cv19k014, cv19k015, cv19k016, cv19k019,
                      cv19k021, cv19k025)

#joining these datasets 

df_join_select1 <- inner_join(df_1_select, df_2_select, by = "nomem_encr") 
glimpse(df_join_select1)

#make the name easy
df <- df_join_select1
df <- as_tibble(df)

glimpse(df)

#remove previous dataset to prevent confusion 
rm(df_1, df_1_select, df_2, df_2_select, df_join_select1)

#-----------------------------------------------------------------------------

#VARIABLES TO NUMERIC

df <- df %>%
  modify_if(is.numeric, unclass) # unclass numeric variables


df <- df %>%
  modify_at(c("geslacht", 
              "oplcat"), haven::as_factor) # convert these to factors 

######################
###DATA MANAGEMENT###
####################

#rename variables 
df <- df %>% 
  dplyr::rename(climate_a = v9a, #climate attitudes 
                climate_b = v9b, 
                climate_c = v9c, 
                climate_d = v9d, 
                pop1 = v7c, #populist attitudes 
                pop2 = v5b, 
                pop3 = v5a, 
                pop4 = v7a, 
                pop5 = v7b, 
                pop6 =v5c, 
                science = v10a, 
                pol_gain = v10b, 
                fin_gain = v10c, 
                ed_cat = oplcat,
                leftright = cv19k101, 
                gender = geslacht, 
                age = leeftijd, 
                trust_government = cv19k013, 
                trust_parliament = cv19k014,
                trust_legal = cv19k015,
                trust_EP = cv19k019,
                trust_media = cv19k021,
                trust_science = cv19k025)

#climate attitudes 
df <- df %>%
  dplyr::mutate(across(c(climate_a:climate_d), ~rec(.x, rec = "1=5; 2=4; 3=3; 4=2; 5=1; 6=NA; else=copy",
                                        val.labels = c("Helemaal eens", "Eens", "Niet mee eens, niet mee oneens", "oneens", "Helemaal oneens"))))

frq(df$climate_d)

#populist attitudes 
df <- df %>% 
  dplyr::mutate(across(c(pop1, 
                         pop2, 
                         pop3, 
                         pop4, 
                         pop5, 
                         pop6), ~rec(.x, rec = "6=NA; else=copy",
                                         val.labels = c("Helemaal oneens", "Oneens", "Niet mee eens, niet mee oneens", "Eens", "Helemaal eens"))))


#leftright orientation
df <- df %>% 
  dplyr::mutate(across(c(leftright), ~rec(.x, rec = "999=NA; else=copy",
                                          val.labels = c("0 left", "1", "2", "4", "3", "5", 
                                                         "6", "7", "8", "9", "10 Right"))))

#pathways pol_gain and fin_gain
df <- df %>% 
  dplyr::mutate(across(c(science, pol_gain, fin_gain), ~rec(.x, rec = "6=NA; else=copy",
                                          val.labels = c("Helemaal oneens", "Oneens", "Niet mee eens, niet mee oneens", "Eens", "Helemaal eens"))))

#education 
df$ed_cat <- plyr::revalue(df$ed_cat, c("1" = "Low", 
                                         "2" = "Low", 
                                         "3" = "Medium", 
                                         "4" = "Medium", 
                                         "5" = "High", 
                                         "6" = "High"))


#gender
frq(df$gender)

df$gender <- plyr::revalue(df$gender, c("1" = "Male",
                                          "2" = "Female"))
#trust 
df <- df %>% 
  dplyr::mutate(across(c(trust_government, 
                         trust_parliament,
                         trust_legal,
                         trust_EP,
                         trust_media,
                         trust_science), ~rec(.x, rec = "999=NA; else=copy",
                                              val.labels = c("0 no confidence at all", "1", "2", "3", "4", "5", 
                                                             "6", "7", "8", "9", "10 full confidence"))))
#listwise delte n=1133
df <- na.omit(df)

#############################################
### Appendix A: Descriptive statistics #####
############################################
library(vtable)

df <- df %>%
  modify_if(is.numeric, as.numeric)

descr.labels <- data.frame(nonsensename1_descr = c('climate_d' ,# this is the variable names
                                                   'leftright', 
                                                   'pop1',
                                                   'pop2',
                                                   'pop3',
                                                   'pop4',
                                                   'pop5',
                                                   'pop6',
                                                   'science',
                                                   'pol_gain',
                                                   'fin_gain',
                                                   'ed_cat', 
                                                   'gender', 
                                                   'age', 
                                                   'trust_government',
                                                   'trust_parliament',
                                                   'trust_legal',
                                                   'trust_EP',
                                                   'trust_media',
                                                   'trust_science'),
                           nonsensename2_descr = c('Opposition to climate policy' , # this are the new names 
                                                   'Left-right orientation', 
                                                   'Pop1',
                                                   'Pop2',
                                                   'Pop3',
                                                   'Pop4',
                                                   'Pop5',
                                                   'Pop6',
                                                   'Science unreliable', 
                                                   'Political gain', 
                                                   'Financial gain', 
                                                   'Educational level',
                                                   'Gender',
                                                   'Age', 
                                                   'Trust in government', 
                                                   'Trust in parliament', 
                                                   'Trust in legal system', 
                                                   'Trust in the European Parliament', 
                                                   'Trust in media', 
                                                   'Trust in science'))

#list the variables again and attach it to the descr.table 

descr.table  <- st(df, vars = c('climate_d',  # this is the variable names
                                'leftright',
                                'pop1',
                                'pop2',
                                'pop3',
                                'pop4',
                                'pop5',
                                'pop6',
                                'science',
                                'pol_gain',
                                'fin_gain',
                                'ed_cat', 
                                'gender', 
                                'age', 
                                'trust_government',
                                'trust_parliament',
                                'trust_legal',
                                'trust_EP',
                                'trust_media',
                                'trust_science'),
                   labels = descr.labels,
                   summ = c('notNA(x)',
                            'mean(x)',
                            'sd(x)',
                            'min(x)',
                            'max(x)'),
                   summ.names = c('N',
                                  'Mean',
                                  'Sd',
                                  'Min  ',
                                  'Max'),
                   title = "Descriptive statistics",
                   note = "",
                   anchor = "",
                   out = 'return')                                           

# this is to print out the table. 

descr.table %>%
  kbl('html', booktabs = TRUE,
      position = "H",
      caption = "Descriptives") %>%
  kable_styling(latex_options = "striped") %>%
  pack_rows("Dependent variable", 1, 2) %>%
  pack_rows("Independent variables", 2, 3) %>%
  pack_rows("Populist attitudes", 3, 7) %>%
  pack_rows("Mediating pathways", 9, 11) %>%
  pack_rows("Socio-economic controls", 11, 19) %>%
  pack_rows("Trust items", 20,25)


#############################################
### Appendix B: Correlation matrix #####
############################################

df_cormatrix <- df %>% select(pop1, pop2, pop3, pop4, pop5, pop6, science, pol_gain, fin_gain)

### calculating covariance matrix ###

cormatrix <- cor(df_cormatrix)
round(cormatrix,3)

#-----------------------------------------------------------------------------

#######################
###MEASUREMENT MODEL###
#######################

#CONFIRMATORY FACTOR ANALYSIS ON POPULIST ITEMS
measurement.pop <- 'populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6

populism~1
pop1 ~ 0*1' 


fit.measure.pop <-cfa(measurement.pop, data=df, meanstructure=T)
summary(fit.measure.pop, standardized=T, fit.measures=T, rsquare=T)

#modification 
mi <- modificationindices(fit.measure.pop, minimum.value=3.85, sort.=T)
mi

measurement.pop2 <- 'populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6

populism~1
pop1 ~ 0*1
pop2 ~~ pop3
pop1 ~~ pop2' 


fit.measure.pop2 <-cfa(measurement.pop2, data=df, meanstructure=T)
summary(fit.measure.pop2, standardized=T, fit.measures=T, rsquare=T)

#-----------------------------------------------------------------------------

#make gender and education numeric for analysis 
df$gender <- as.numeric(df$gender)
df$ed_cat <- as.numeric(df$ed_cat)

########################
### STRUCTURAL MODEL ###
########################

#direct effect model (Appendix C M1)
model.direct <- '

#measurement model
populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6

populism~1
pop1 ~ 0*1 

#residual covariance 
pop2 ~~ pop3
pop1 ~~ pop2

#regressions
climate_d ~ populism + leftright + ed_cat + gender + age'

fit.direct <- sem(model.direct, data=df, meanstructure =TRUE,  estimator="ML")

summary(fit.direct, standardized=T, fit.measures=TRUE, rsquare=TRUE)

#-----------------------------------------------------------------------------
#INDIRECT MODEL (Figure 1 / Appendix C M2)

model.indirect<-'

populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6

populism~1
pop1 ~ 0*1 
pop2 ~~ pop3
pop1 ~~ pop2

#regressions 
climate_d ~  a*populism + j*science + k*pol_gain + l*fin_gain  + ed_cat + gender + age +leftright
science ~ d*populism + ed_cat + gender + age +leftright
pol_gain ~ e*populism + ed_cat + gender + age +leftright
fin_gain ~ f*populism  + ed_cat + gender + age +leftright

#residual covariance 
science ~~ pol_gain  
pol_gain ~~ fin_gain
science ~~ fin_gain

#calculating specific indirect effects 
indirect1:= d*j #from populism on climate via science 
indirect2:= e*k #from populism on climate via pol_gain
indirect3:= f*l #from populism on climate via fin_gain

#calculating total indirect effects 
TIEPOP:= indirect1 + indirect2 + indirect3 + a'

fit.indirect <- sem(model.indirect, data=df, meanstructure =TRUE,  estimator="ML")
summary(fit.indirect, standardized=T, fit.measures=TRUE, rsquare=TRUE) 


#-----------------------------------------------------------------------------
#INTERACTION MODEL (Appendix D)
library(interplot)

#add latent variable of populism to data frame 
data_cfa<- df %>% dplyr::select(pop1,
                                pop2,
                                pop3,
                                pop4,
                                pop5,
                                pop6,
                                nomem_encr) %>% drop_na() %>% bind_cols(data.frame(lavaan::predict(fit.measure.pop2)))

df <- dplyr::left_join(df, data_cfa, by = "nomem_encr") 

df$pop_latent <- scales::rescale(df$populism, to = c(1, 5), 
                                 from = range(df$populism, na.rm = TRUE, finite = TRUE))

frq(df$pop_latent)

# REMANE FOR FUTURE USE

df <- df %>%
  dplyr::rename(pop1 = pop1.x, 
                pop2 = pop2.x,
                pop3 = pop3.x,
                pop4 = pop4.x,
                pop5 = pop5.x,
                pop6 = pop6.x)

#change gender and educational level to factor for OLS
df$gender <- as_factor(df$gender)
df$ed_cat <- as_factor(df$ed_cat)

#centering the data
df <- df %>% mutate_at(vars(pop_latent, leftright, pol_gain, fin_gain, science), .funs = list("scaled" = scale))

#science 
moderation1 <- lm(science ~ pop_latent + leftright + ed_cat + gender + age + pop_latent*leftright,data = df)
summary(moderation1)

#pol_gain
moderation2 <- lm(pol_gain ~ pop_latent + leftright + ed_cat + gender + age + pop_latent*leftright,data = df)
summary(moderation2)

#fin_gain
moderation3 <- lm(fin_gain ~ pop_latent + leftright + ed_cat + gender + age + pop_latent*leftright,data = df)
summary(moderation3)

#visualize estimated coefficient 
moderation1.plot <- interplot(moderation1, var1 = "pop_latent", var2 = "leftright") +
  xlab("Leftright") + 
  theme_bw() + 
  ggtitle("") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed")

moderation1.plot

moderation2.plot <- interplot(moderation2, var1 = "pop_latent", var2 = "leftright") +
  xlab("Leftright") + 
  theme_bw() + 
  ggtitle("") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed")

moderation2.plot

moderation3.plot <- interplot(moderation3, var1 = "pop_latent", var2 = "leftright") +
  xlab("Leftright") + 
  theme_bw() + 
  ggtitle("") +
  theme(plot.title = element_text(size = 12)) +
  geom_hline(yintercept = 0, linetype = "dashed")

moderation3.plot

#combine graphs (figure 2)
library(ggpubr)
graphcombined <- ggarrange(moderation1.plot, moderation2.plot, moderation3.plot,
                           labels = c("2a: Unreliable Science", "2b: Political Gain", "2c: Financial Gain"), 
                           ncol = 2, nrow=2, 
                           common.legend = TRUE, legend = "right")
graphcombined 

#-----------------------------------------------------------------------------
#MODERATED MEDIATION (Appendix E)

#science 
moderation_mediation_1 <- '
science ~ a1*pop_latent + ed_cat + gender + age
climate_d ~ b*science + ed_cat + gender + age
science ~ a2*leftright 
science ~ a3*pop_latent_x_leftright
climate_d ~ c*pop_latent 

# index of moderated mediation
imm := a3*b' 

fit.moderation_mediation_1  <- sem(moderation_mediation_1 , data=df, meanstructure =TRUE,  estimator="ML", fixed.x = FALSE)
summary(fit.moderation_mediation_1, standardized=T, fit.measures=TRUE, rsquare=TRUE) 

#political gain 
moderation_mediation_2 <- '
pol_gain ~ a1*pop_latent + ed_cat + gender + age
climate_d ~ b*pol_gain + ed_cat + gender + age
pol_gain ~ a2*leftright 
pol_gain ~ a3*pop_latent_x_leftright
climate_d ~ c*pop_latent 

# index of moderated mediation
imm := a3*b' 

fit.moderation_mediation_2  <- sem(moderation_mediation_2 , data=df, meanstructure =TRUE,  estimator="ML", fixed.x = FALSE)
summary(fit.moderation_mediation_2, standardized=T, fit.measures=TRUE, rsquare=TRUE) 

#financial gain 
moderation_mediation_3 <- '
fin_gain ~ a1*pop_latent + ed_cat + gender + age
climate_d ~ b*fin_gain + ed_cat + gender + age
fin_gain ~ a2*leftright 
fin_gain ~ a3*pop_latent_x_leftright
climate_d ~ c*pop_latent 

# index of moderated mediation
imm := a3*b' 

fit.moderation_mediation_3  <- sem(moderation_mediation_3 , data=df, meanstructure =TRUE,  estimator="ML", fixed.x = FALSE)
summary(fit.moderation_mediation_3, standardized=T, fit.measures=TRUE, rsquare=TRUE) 


#-----------------------------------------------------------------------------
#REPLICATION HUBER ET AL (2021) (Appendix F) 

#measurement model (appendix F.1)
measurement.full <- 'populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6
populism~1
pop1 ~ 0*1
pop1 ~~ pop2
pop2 ~~ pop3

trust =~ trust_government + trust_parliament + trust_legal + trust_EP + trust_media
trust~1
trust_government ~ 0*1
trust_government ~~ trust_parliament

trust ~~ populism'

fitcfa_full <-cfa(measurement.full, data=df, meanstructure=T)
summary(fitcfa_full, standardized=T, fit.measures=T, rsquare=T)

#measurement model populism and trust 
measurement.full <- 'populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6
populism~1
pop1 ~ 0*1
pop1 ~~ pop2
pop2 ~~ pop3

trust =~ trust_government + trust_parliament + trust_legal + trust_EP + trust_media
trust~1
trust_government ~ 0*1
trust_government ~~ trust_parliament

trust ~~ populism'

fitcfa_full <-cfa(measurement.full, data=df, meanstructure=T)
summary(fitcfa_full, standardized=T, fit.measures=T, rsquare=T)

#sem model (appendix f.2 + f.3)
model.rep <- '

#measurement model 
populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6
populism~1
pop1 ~ 0*1
pop1 ~~ pop2
pop2 ~~ pop3

trust =~ trust_government + trust_parliament + trust_legal + trust_EP + trust_media
trust~1
trust_government ~ 0*1
trust_government ~~ trust_parliament

#regressions 
climate_d ~ a*populism + b*trust + g*trust_science + e*leftright + ed_cat + gender + age
leftright ~ d*populism + ed_cat + gender + age
trust ~ c*populism  + ed_cat + gender + age
trust_science ~ f*populism + ed_cat + gender + age

indirect1: = c*b  #from populism on climate via inst trust
indirect2: = d*e #from populism on climate via l-r 
indirect3: = f*g #from populism on climate via trust in science 

#calculating total indirect effects 
TIEPOP:= indirect1 + indirect2 + indirect3 + a'

fit.rep <- sem(model.rep, data=df,meanstructure =TRUE,  estimator="ML")
summary(fit.rep, standardized=T, fit.measures=TRUE, rsquare=TRUE)

#sem model incl our pathways (appendix G)
model.indirect2 <-'

#measurement model 
populism =~ pop1 + pop2 + pop3 + pop4 + pop5 + pop6
populism~1
pop1 ~ 0*1
pop1 ~~ pop2
pop2 ~~ pop3

trust =~ trust_government + trust_parliament + trust_legal + trust_EP + trust_media
trust~1
trust_government ~ 0*1
trust_government ~~ trust_parliament

#regressions 
climate_d ~ a*populism + b*trust + g*trust_science + e*leftright + k*science + l*pol_gain + m*fin_gain + ed_cat + gender + age

science ~ h*populism + ed_cat + gender + age
pol_gain ~ i*populism + ed_cat + gender + age
fin_gain ~ j*populism + ed_cat + gender + age

trust ~ c*populism + ed_cat + gender + age
trust_science ~ f*populism  + ed_cat + gender + age
leftright ~ d*populism + ed_cat + gender +age 
#residual covariance 
science ~~ pol_gain 
pol_gain ~~ fin_gain
science ~~ fin_gain

#calculating specific indirect effects 
indirect1: = c*b  #from populism on climate via inst trust
indirect2: = d*e #from populism on climate via l-r 
indirect3: = h*k #from populism on climate via science 
indirect4: =i*l #from populism on climate via pol_gain 
indirect5: =j*m #from populism on climate via fin_gain 
indirect6: = f*g #from populism on climate via trust in science 

#calculating total indirect effects 
TIEPOP:= indirect1 + indirect2 + indirect3 + indirect4 + indirect5 + a'

fit.indirect2 <- sem(model.indirect2, data=df, meanstructure =TRUE,  estimator="ML")
summary(fit.indirect2, standardized=T, fit.measures=TRUE, rsquare=TRUE) 

#--------------- end script 

