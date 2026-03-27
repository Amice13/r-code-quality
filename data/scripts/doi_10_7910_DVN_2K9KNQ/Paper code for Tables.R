# ANES Analysis for immigration and rural


### rescale vars from zero to one
### correlation table of all the variables

library(dplyr)
library(haven)
library(srvyr)
library(survey)
library(tidyr)
library(ggplot2)
library(stargazer)
library(ltm)
library(ggpubr)


clean_ANES <- function(var){
  dplyr::recode(
    var, 
    `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
    `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN,
    `-9` = NaN, `99` = NaN, '97' = NaN,
    .default = var)
} 

clean_ANES7 <- function(var){
  dplyr::recode(
    var,
    '1' = 0, '7' = 1, '2' = 0.166667, '3' = 0.333333, '4' = 0.5, '5' = 0.666667, '6' = 0.833333, 
    `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
    `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN,
    `-9` = NaN, `99` = NaN, '97' = NaN,
    .default = var
  )
}

clean_ANES6 <- function(var){
  dplyr::recode(
    var,
    '1' = 0, '6' = 1, '2' = 0.2, '3' = 0.4, '4' = 0.6, '5' = 0.8,
    `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
    `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN,
    `-9` = NaN, `99` = NaN, '97' = NaN,
    .default = var
  )
}

clean_ANES5 <- function(var){
  dplyr::recode(
    var,
    '1' = 0, '5' = 1, '2' = 0.25, '3' = 0.5, '4' = 0.75,  
    `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
    `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN,
    `-9` = NaN, `99` = NaN, '97' = NaN,
    .default = var
  )
}

clean_FT <- function(var){
  dplyr::recode(
    var, 
    `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
    `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN,
    `-9` = NaN, `998` = NaN, `999` = NaN, '97' = NaN,
    .default = var)
} 

clean_ideo <- function(var){
  dplyr::recode(
    var,
    '1' = 0, '7' = 1, '2' = 0.166667, '3' = 0.333333, '4' = 0.5, '5' = 0.666667, '6' = 0.833333,
    '-7' = NaN, '6' = NaN,  '97' = NaN,
    .default = var)
}

recode_rurcons <- function(var){
  dplyr::recode(var, `2` = 0.833333, `3` = 0.666667, '4' = 0.5, `5` = 0.333333, `6` = 0.166667, '7' = 0, `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
                `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN, '97' = NaN,
                `-9` = NaN, `998` = NaN, `999` = NaN, .default = var )
}

recode_rr <- function(var){
  dplyr::recode(var, '2' = 0.75, '3' = 0.5, '4' = 0.25, '5' = 0, `-1` = NaN, `-2` = NaN, `-3` = NaN, `-4` = NaN,
                `-5` = NaN, `-6` = NaN, `-7` = NaN, `-8` = NaN, '97' = NaN,
                `-9` = NaN, `998` = NaN, `999` = NaN, .default = var)
}


# *****************************************************************
# LOAD DATA ####
# *****************************************************************

ANES_2019 <- read_dta(
  here::here("anes_pilot_2019.dta")
) %>% 
  zap_labels()


ANES_19_clean <- ANES_2019 %>% 
  mutate(
    residence       = clean_ANES(liveurban),
    resid_Place     = case_when(
      residence == 1 ~ "Rural Area",
      residence == 2 ~ "Small Town",
      residence == 3 ~ "Suburb",
      residence == 4 ~ "City"
    ),
    r_u_identity1    = clean_ANES(placeid1a),
    resid_identity1  = case_when(
      r_u_identity1 == 0 ~ "City Person",
      r_u_identity1 == 1 ~ "Suburb Person",
      r_u_identity1 == 2 ~ "Small Town Person",
      r_u_identity1 == 3 ~ "Country Person",
      r_u_identity1 == 4 ~ "Something else"
    ),
    resid_identity1 = factor(
      resid_identity1,
      levels = c("City Person", "Suburb Person", "Small Town Person", "Country Person", "Something Else")
    ),
    r_u_identity2    = clean_ANES(placeid1b),
    resid_identity2  = case_when(
      r_u_identity2 == 0 ~ "City Person",
      r_u_identity2 == 1 ~ "Suburb Person",
      r_u_identity2 == 2 ~ "Small Town Person",
      r_u_identity2 == 3 ~ "Country Person"
    ),
    resid_identity2 = factor(
      resid_identity2,
      levels = c("City Person", "Suburb Person", "Small Town Person", "Country Person")
    ),
    r_u_important   = clean_ANES(placeidimport),
    pid7x            = clean_ANES7(pid7x),
    PARTY           = case_when(
      pid7x %in% c(1:3) ~ "Democrat",
      pid7x %in%   4    ~ "Independent",
      pid7x %in% c(5:7) ~ "Republican",
      TRUE ~ NA_character_
    ),
    rural_hardtime1 = recode_rurcons(rural1alt1),
    rural_hardtime2 = recode_rurcons(rural1alt2),
    rural_get_more1  = clean_ANES7(rural2alt1),
    rural_get_more2 = clean_ANES7(rural2alt2),
    rural_influence1 = clean_ANES7(rural3alt1),
    rural_influence2 = clean_ANES7(rural3alt2),
    rural_respect1   = clean_ANES7(rural4alt1),
    rural_respect2 = clean_ANES7(rural4alt2),
    x_ft_illegal     = clean_FT(ftillegal),
    x_ft_immall = clean_FT(ftimmig1),
    x_ft_legal = clean_FT(ftimmig2),
    x_ft_muslim = clean_FT(ftmuslim),
    x_ft_hisp = clean_FT(fthisp),
    ax_immignum = clean_ANES7(immignum),
    ax_refugees = clean_ANES7(refugees),
    bx_pathway = clean_ANES5(pathway),
    bx_return = clean_ANES5(preturn),
    bx_open = clean_ANES5(popen),
    bx_release1 = clean_ANES5(release1),
    bx_release2 = clean_ANES5(release2),
    bx_famsep = clean_ANES5(famsep),
    ax_wall = clean_ANES7(wall7),
    # Demographics
    ideo           = clean_ideo(ideo5),
    gender          = clean_ANES(gender),
    FEMALE          = case_when(
      gender == 2 ~ TRUE,
      TRUE ~ FALSE
    ),
    edu            = clean_ANES6(educ),
    race            = clean_ANES(race),
    nonwhite        = case_when(
      race == 1 ~ FALSE,
      race != 1 ~ TRUE
    ),
    income          = clean_ANES(faminc_new),
    birthyear             = clean_ANES(birthyr),
    age             = case_when(
      birthyr %in% c(1995:2001) ~ "18 - 24",
      birthyr %in% c(1975:1994) ~ "25 - 44",
      birthyr %in% c(1974:1955) ~ "45 - 64",
      birthyr %in% c(1954:1900) ~ "65+"
    ),
    # Racial Resent
    RR1      = recode_rr(rr1),
    RR2      = clean_ANES5(rr2),
    RR3      = clean_ANES5(rr3),
    RR4      = recode_rr(rr4),
    # elites
    elite3   = recode_rr(elite3),
    elite1   = clean_ANES5(elite1),
    elite2   = clean_ANES5(elite2),
    elite4   = clean_ANES5(elite4),
    # pro-intellectualism/experts
    expert1  = clean_ANES5(experts),
    expert2  = clean_ANES5(science),
    expert3  = clean_ANES5(exphelp)
  ) 

ANES_19_clean <- ANES_19_clean %>% 
  mutate(ax_immignum = -ax_immignum+1,
         ax_refugees = -ax_refugees+1,
         bx_pathway = -bx_pathway+1,
         bx_open = -bx_open+1,
         bx_release2 = -bx_release2+1)

ANES_19_clean_weight <- subset(ANES_19_clean, !is.na( weight ) )

ANES_19_clean$bx_famsep
ANES_19_clean_weight$resid_Place

ANES_19_clean_weight <- ANES_19_clean_weight %>% 
  mutate(abortion2_new    = case_when(
    abortion2 == 7 ~ 1,
    abortion2 == 6 ~ 2,
    abortion2 == 5 ~ 3,
    abortion2 == 4 ~ 4,
    abortion2 == 3 ~ 5,
    abortion2 == 2 ~ 6,
    abortion2 == 1 ~ 1))

ANES_19_clean_weight <- ANES_19_clean_weight %>% 
  mutate(opideo = ((abortion2_new+hlthcare1+billtax+buyback))/11)

ANES_19_clean_weight <- ANES_19_clean_weight %>%
  mutate(polforce = case_when(
    excessive == 1 ~ 1,
    excessive == 2 ~ .75,
    excessive == 3 ~ .5,
    excessive == 4 ~ .25,
    excessive == 5 ~ 0
  ),
  elite = (elite1 + elite2 + elite3 + elite4)/4,
  income = (income-1)/15
  )

table(ANES_19_clean_weight$opideo)

############################################################
# Creating scales and correlations
############################################################


# Rural consciousness

rural_influence1 <- (ANES_19_clean_weight$rural_influence1)
rural_hardtime1 <- (ANES_19_clean_weight$rural_hardtime1)
rural_respect1 <- (ANES_19_clean_weight$rural_respect1)
rural_get_more1 <- (ANES_19_clean_weight$rural_get_more1)

rc1 <- data.frame(rural_influence1, rural_hardtime1, rural_respect1, rural_get_more1)
cor(rc1, use="complete.obs", method="kendall")

cronbach.alpha(rc1, na.rm = T)

rural_influence2 <- (ANES_19_clean_weight$rural_influence2)
rural_hardtime2 <- (ANES_19_clean_weight$rural_hardtime2)
rural_respect2 <- (ANES_19_clean_weight$rural_respect2)
rural_get_more2 <- (ANES_19_clean_weight$rural_get_more2)

rc2 <- data.frame(rural_influence2, rural_hardtime2, rural_respect2, rural_get_more2)
cor(rc2, use="complete.obs", method="kendall")

cronbach.alpha(rc2, na.rm = T)

rc1$rc1 <- (rural_influence1+rural_hardtime1+rural_respect1+rural_get_more1)/4
rc2$rc2 <- (rural_influence2+rural_hardtime2+rural_respect2+rural_get_more2)/4
rc <- data.frame(rc1$rc1, rc2$rc2)
rc
rc[is.na(rc)] = 0
rc$rc <- rowSums(rc)
rc$rc <- as.numeric(rc$rc)

ANES_19_clean_weight$rc <- rc$rc

rc <- ANES_19_clean_weight$rc

# Racial Resentment
RR1 <- (ANES_19_clean_weight$RR1)
RR2 <- (ANES_19_clean_weight$RR2)
RR3 <- (ANES_19_clean_weight$RR3)
RR4 <- (ANES_19_clean_weight$RR4)

RR <- data.frame(RR1, RR2, RR3, RR4)
cor(RR, use="complete.obs", method="kendall")
cronbach.alpha(RR, na.rm = T)
RR[is.na(RR)] = 0
RR$RR <- (RR1 + RR2 + RR3 + RR4)/4

unlist(RR$RR, use.names=FALSE)
ANES_19_clean_weight$RR <- RR$RR

unlist(ANES_19_clean_weight$RR, use.names=FALSE)

RR

# pro-expert scale and then anti-int scale
expert1 <- (ANES_19_clean_weight$expert1)
expert2 <- (ANES_19_clean_weight$expert2)
expert3 <- (ANES_19_clean_weight$expert3)
expert <- data.frame(expert1, expert2, expert3)
cor(expert, use="complete.obs", method="kendall")
cronbach.alpha(expert, na.rm = T)
expert[is.na(expert)] = 0
expert$expert <- (expert1 + expert2 + expert3)/3
unlist(expert$expert, use.names=FALSE)
ANES_19_clean_weight$expert <- expert$expert
unlist(ANES_19_clean_weight$expert, use.names=FALSE)
ANES_19_clean_weight$antiint <- (ANES_19_clean_weight$expert * -1)+6
ANES_19_clean_weight$antiint
unlist(ANES_19_clean_weight$antiint, use.names=FALSE)




populism <- data.frame(ANES_19_clean_weight$antiint, ANES_19_clean_weight$elite, ANES_19_clean_weight$pid7x)
cor(populism, use="complete.obs", method="pearson")

# pro- illegal imm policies
bx_pathway <- ANES_19_clean$bx_pathway
bx_return <- ANES_19_clean$bx_return
bx_open <- ANES_19_clean$bx_open
bx_release1 <- ANES_19_clean$bx_release1
bx_release2 <- ANES_19_clean$bx_release2
bx_famsep <- ANES_19_clean$bx_famsep
x_illimmpol <- data.frame(bx_pathway, bx_return,  bx_open, bx_release1, bx_release2, bx_famsep)
cor(x_illimmpol, use="complete.obs", method="kendall")
cronbach.alpha(x_illimmpol, na.rm=T)
x_illimmpol[is.na(x_illimmpol)] = 0
x_illimmpol$x_illimmpol <- (bx_pathway + bx_return + bx_open + bx_release1 + bx_release2 + bx_famsep)/6

unlist(x_illimmpol$x_illimmpol, use.names=FALSE)
ANES_19_clean$x_illimmpol <- x_illimmpol$x_illimmpol

ANES_19_clean_weight$abc_illimmpol <- (ANES_19_clean_weight$bx_pathway + ANES_19_clean_weight$bx_return + ANES_19_clean_weight$bx_open + ANES_19_clean_weight$bx_release1 + ANES_19_clean_weight$bx_release2 + ANES_19_clean_weight$bx_famsep)/6
ANES_19_clean_weight$abc_illimmpol

# pro- general imm policies
# pro- illegal imm policies
ax_immignum <- ANES_19_clean$ax_immignum
ax_refugees <- ANES_19_clean$ax_refugees
ax_wall <- ANES_19_clean$ax_wall
x_immpol <- data.frame(ax_wall, ax_immignum, ax_refugees)
x_immpol[is.na(x_immpol)] = 0
x_immpol$x_immpol <- (ax_wall + ax_immignum + ax_refugees)/3
unlist(x_immpol$x_immpol, use.names=FALSE)
ANES_19_clean$x_immpol <- x_immpol$x_immpol
cronbach.alpha(x_immpol, na.rm=T)


ANES_19_clean$abc_immpol <- x_immpol$x_immpol
ANES_19_clean$abc_immpol

ANES_19_clean_weight$abc_immpol <- (ANES_19_clean_weight$ax_wall + ANES_19_clean_weight$ax_immignum + ANES_19_clean_weight$ax_refugees)/3
ANES_19_clean_weight$abc_immpol

#########################################
# Transform vars
###########################################

# Income

ANES_19_clean_weight$income <- (ANES_19_clean_weight$income -1 )/15
ANES_19_clean_weight$income

##
#
# NEED AGE/BIRTH YR AND FTs!!!!!!!!!!!!!!!!!



#
ANES_19_Survey <- ANES_19_clean_weight %>% srvyr::as_survey(weight = weight)




##########################################
# Limited data frames
##########################################

ANES_19_whites  <- ANES_19_clean_weight %>% filter(nonwhite == "FALSE") 

ANES_19_whitesSurvey <- ANES_19_whites %>% srvyr::as_survey(weight=weight)

ANES_19_nonwhites <- ANES_19_clean_weight %>% filter(nonwhite == "TRUE") 

ANES_19_nonwhiteSurvey <- ANES_19_nonwhites %>% srvyr::as_survey(weight=weight)


# Rural and small town
ANES_19_ruralsmallonly <-  ANES_19_clean_weight %>% filter(residence < 3)  


# Rural and small town whites only
ANES_19_rswhites <- ANES_19_ruralsmallonly %>% filter(nonwhite == "FALSE")

ANES_19_rswhiteSurv <- ANES_19_ruralsmallonly %>% srvyr::as_survey(weight=weight)



# rural and small town non-whites only
ANES_19_rsnonwhites <- ANES_19_ruralsmallonly %>% filter(nonwhite == "TRUE")

ANES_19_rsnonwhiteSurv <- ANES_19_rsnonwhites %>% srvyr::as_survey(weight=weight)







## Correlation Plots


library(corrplot)

corr_matrix <- ANES_19_clean_weight[,c("rc", "residence", "elite", "antiint", "RR", "pid7x", "ideo", 
                                      "x_ft_illegal", "x_ft_immall", "x_ft_legal", "x_ft_muslim", "x_ft_hisp",
                                      "ax_immignum", "ax_refugees", "ax_wall", "bx_pathway", "bx_return",
                                      "bx_open", "bx_release1", "bx_release2", "bx_famsep")]
         corr_matrix             
         corr_matrix <- data.matrix(corr_matrix, rownames.force = NA)
         

         corr_use = cor(corr_matrix,
                        use = "pairwise.complete.obs", method = c("pearson"))

                  colnames(corr_use) <- c("Rural Consciousness", "Residence", "Anti-Elite", "Anti-Intellectualism", "Racial Resentment",
                          "Party ID", "Ideology", "Illegal Immigrant FT", "Immigrant FT", "Legal Immigrant FT", "Muslim FT",
                          "Hispanic FT", "Decrease Immigrants", "Oppose Refugees", "Oppose Border Wall", "Oppose Citizenship",
                          "Oppose Returning", "Oppose Open Borders", "Oppose Holding", "Oppose Releasing.", "Oppose Fam Separation")
                  rownames(corr_use) <- c("Rural Consciousness", "Residence", "Anti-Elite", "Anti-Intellectualism", "Racial Resentment",
                                          "Party ID", "Ideology", "Illegal Immigrant FT", "Immigrant FT", "Legal Immigrant FT", "Muslim FT",
                                          "Hispanic FT", "Decrease Immigrants", "Oppose Refugees", "Oppose Border Wall", "Oppose Citizenship",
                                          "Oppose Returning", "Oppose Open Borders", "Oppose Holding", "Oppose Releasing", "Oppose Fam Separation")
                  
         corrplot(corr_use, insig = "p-value")
         
         
 corr_matrix2 <- ANES_19_clean_weight[,c("rc", "residence", "elite", "antiint", "RR", "pid7x", "ideo", 
                                                "x_ft_illegal", "x_ft_immall", "x_ft_legal", "x_ft_muslim", "x_ft_hisp"
                                              )]
         corr_matrix2             
         corr_matrix2 <- data.matrix(corr_matrix2, rownames.force = NA)
         
         
         corr_use2 = cor(corr_matrix2,
                        use = "pairwise.complete.obs", method = c("pearson"))
         
         colnames(corr_use2) <- c("Rural Consciousness", "Residence", "Anti-Elite", "Anti-Intellectualism", "Racial Resentment",
                                 "Party ID", "Ideology", "Illegal Immigrant FT", "Immigrant FT", "Legal Immigrant FT", "Muslim FT",
                                 "Hispanic FT")
         rownames(corr_use2) <- c("Rural Consciousness", "Residence", "Anti-Elite", "Anti-Intellectualism", "Racial Resentment",
                                 "Party ID", "Ideology", "Illegal Immigrant FT", "Immigrant FT", "Legal Immigrant FT", "Muslim FT",
                                 "Hispanic FT")
         
         corrplot(corr_use2, insig = "p-value")
         


###########################################################
# Visualizing DV and rurality - all respondents
###########################################################



ANES_immiga <- ANES_19_Survey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("x_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "x_ft_illegal" ~ "Illegal Immigrants",
      variable == "x_ft_immall" ~ "Immigrants",
      variable == "x_ft_legal" ~ "Legal Immigrants",
      variable == "x_ft_muslim" ~ "Muslims",
      variable == "x_ft_hisp" ~ "Hispanics",
      variable == "x_illimmpol" ~ "Pro-Undocumented Immigrant Policies",
      variable == "x_immpol" ~ "General Pro-Immigrant Policies"))

f2a <- ggplot(ANES_immiga, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+ 
  ylab("Mean")+ 
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )


ANES_immigaz <- ANES_19_Survey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("abc_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "abc_illimmpol" ~ "Pro-Undocumented \n Immigrant Policies",
      variable == "abc_immpol" ~ "General Pro-\n Immigrant Policies"))


f2b <- ggplot(ANES_immigaz, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+ 
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )




ANES_immig12 <- ANES_19_Survey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("ax_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "ax_immignum" ~ "Decrease Immigrants",
      variable == "ax_refugees" ~ "Oppose more refugees",
      variable == "ax_wall" ~ "Oppose the Wall"))

af1b <- ggplot(ANES_immig12, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )


ANES_immig12 <- ANES_19_Survey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("ax_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "ax_immignum" ~ "Decrease Immigrants",
      variable == "ax_refugees" ~ "Oppose more refugees",
      variable == "ax_wall" ~ "Oppose the Wall"))

ggplot(ANES_immig12, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )


ANES_immig13 <- ANES_19_Survey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("bx_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "bx_pathway" ~ "Oppose Pathway \n to Citizenship",
      variable == "bx_return" ~ "Oppose Returning Them",
      variable == "bx_open" ~ "Oppose Decriminalize \n Unauthorized",
      variable == "bx_release1" ~ "Oppose Holding \n Unauthorized Immigrants",
      variable == "bx_release2" ~ "Oppose Releasing and \n Supervise Some",
      variable == "bx_famsep" ~ "Oppose Separating \n Children"))

af1a <- ggplot(ANES_immig13, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name="Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )



######################################################
# Visualizations for white respondents only
######################################################


ANES_immig1w <- ANES_19_whitesSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("x_ft_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "x_ft_illegal" ~ "Illegal Immigrants",
      variable == "x_ft_immall" ~ "Immigrants",
      variable == "x_ft_legal" ~ "Legal Immigrants",
      variable == "x_ft_muslim" ~ "Muslims",
      variable == "x_ft_hisp" ~ "Hispanics"))

f2c <- ggplot(ANES_immig1w, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "White Res.:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )


ANES_immigazw <- ANES_19_whitesSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("abc_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "abc_illimmpol" ~ "Pro-Undocumented \n Immigrant Policies",
      variable == "abc_immpol" ~ "General Pro-\n Immigrant Policies"))


f2d <- ggplot(ANES_immigazw, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "White Res.:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+ 
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )

ANES_immig1w2 <- ANES_19_whitesSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("ax_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "ax_immignum" ~ "Decrease Immigrants",
      variable == "ax_refugees" ~ "Oppose more refugees",
      variable == "ax_wall" ~ "Oppose the Wall"))

af1d <- ggplot(ANES_immig1w2, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "White Res.", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )


ANES_immig1w3 <- ANES_19_whitesSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("bx_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "bx_pathway" ~ "Oppose Pathway \n to Citizenship",
      variable == "bx_return" ~ "Oppose Returning Them",
      variable == "bx_open" ~ "Oppose Decriminalize \n Unauthorized",
      variable == "bx_release1" ~ "Oppose Holding \n Unauthorized Immigrants",
      variable == "bx_release2" ~ "Oppose Releasing and \n Supervise Some",
      variable == "bx_famsep" ~ "Oppose Separating \n Children"))

af1c <- ggplot(ANES_immig1w3, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "Greys", name = "White Res.", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    text=element_text(size=17),
    legend.position = 'bottom'
  )

ggarrange(f2a, f2c, f2b, f2d)

ggarrange(af1a, af1c, af1b, af1d)



######################################################
# Visualizations for nonwhites only
######################################################


ANES_immig1nw <- ANES_19_nonwhiteSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("x_ft_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "x_ft_illegal" ~ "Illegal Immigrants",
      variable == "x_ft_immall" ~ "Immigrants",
      variable == "x_ft_legal" ~ "Legal Immigrants",
      variable == "x_ft_muslim" ~ "Muslims",
      variable == "x_ft_hisp" ~ "Hispanics"))

ggplot(ANES_immig1nw, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "RdGy", name="Nonwhite Resp. Residency:")+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    legend.position = 'bottom'
  )


ANES_immigaznw <- ANES_19_nonwhiteSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("abc_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "abc_illimmpol" ~ "Pro-Undocumented \n Immigrant Policies",
      variable == "abc_immpol" ~ "General Pro-\n Immigrant Policies"))


ggplot(ANES_immigaznw, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "RdGy", name = "Non-White Resp. Residency:", limits=c("City", "Suburb", "Small Town", "Rural Area"))+ 
  theme_bw()+
  coord_flip()+
  xlab("")+ 
  ylab("Mean")+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    legend.position = 'bottom'
  )

ANES_immig1nw2 <- ANES_19_nonwhiteSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("ax_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "ax_immignum" ~ "Decrease Immigrants",
      variable == "ax_refugees" ~ "Oppose more refugees",
      variable == "ax_wall" ~ "Oppose the Wall"))

ggplot(ANES_immig1nw2, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "RdGy", name="Nonwhite Resp. Residency:")+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    legend.position = 'bottom', legend.text=element_text(size=8)
  )


ANES_immig1nw3 <- ANES_19_nonwhiteSurvey %>% 
  group_by(resid_Place) %>% 
  summarise_at(vars(starts_with("bx_")), survey_mean, na.rm = TRUE) %>% 
  reshape2::melt() %>%
  mutate(valtype = ifelse(grepl("_se", variable), "se","mean")) %>%
  mutate(variable = gsub("_se", "", variable)) %>%
  pivot_wider(
    names_from = "valtype",
    values_from = "value") %>%
  mutate(
    lwr = mean - 1.96*se,
    upr = mean + 1.96*se) %>% 
  filter(!(is.na(mean))) %>% 
  mutate(
    variable = case_when(
      variable == "bx_pathway" ~ "Oppose Pathway \n to Citizenship",
      variable == "bx_return" ~ "Oppose Returning Them",
      variable == "bx_open" ~ "Oppose Decriminalize \n Unauthorized",
      variable == "bx_release1" ~ "Oppose Holding \n Unauthorized Immigrants",
      variable == "bx_release2" ~ "Oppose Releasing and \n Supervise Some",
      variable == "bx_famsep" ~ "Oppose Separating \n Children"))

ggplot(ANES_immig1nw3, aes(x = variable, y = mean, fill = resid_Place))+
  #geom_pointrange(aes(ymin=lwr, ymax=upr), position = position_dodge(.5))+
  geom_bar(stat = "identity", position = position_dodge(), color = "black")+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=.2, position=position_dodge(.9))+
  #geom_text(mapping = aes(label = round(mean*100, 0), y=lwr), position = position_dodge(width = .9), vjust = 1.5, color = "white")+
  scale_fill_brewer(palette = "RdGy", name="Nonwhite Resp. Residency:")+ scale_color_hue(direction = -1, h.start=90) +
  theme_bw()+
  coord_flip()+
  xlab("")+
  ylab("Mean")+
  theme(
    title      = element_text(colour="black"),
    plot.title = element_text(size = 20, hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    legend.position = 'bottom', legend.text=element_text(size=8)
  )




###########################################
# Regression Models
#########################################


# RC
rill2 <- lm(x_ft_illegal ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(rill2)
rill5 <- lm(x_ft_illegal ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(rill5)

rill6 <- lm(x_ft_illegal ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill6)


library("margins")

summary(marg3 <- margins(rill6, variable = "rc"))
#x <- cplot(rill6, "rc", what="prediction", se.type = "shade", xlab = "Rural Consciousness", ylab = "Predicted Illegal Immigrant Value (Feeling Therm.)")
#x

rill7 <- lm(x_ft_illegal ~  rc + pid7x + ideo +
                 edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*factor(nonwhite),
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill7)


rill8 <- lm(x_ft_illegal ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill8)

rill9 <- lm(x_ft_illegal ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill9)

### TABLE 1
library(stargazer)
stargazer(list(rill2, rill5, rill6, rill7, rill8, rill9),
          title="Regression Results: Feelings Toward Illegal Immigrants",
          align=TRUE, dep.var.labels=c("Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Age 25-44", "Age 45-64",
                             "Age 65+", "Female", "Non-White", "Racial Resentment", "Anti-Intellectualism", "Anti-Elitism"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)





## Same but with policy ideology and law and order added

ANES_19_ruralsmallonly$elite <- as.numeric(ANES_19_ruralsmallonly$elite)

# RC

rill5x <- lm(x_ft_illegal ~ factor(resid_Place) + rc + pid7x + ideo +
              edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + billtax + hlthcare1 + polforce,
            data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(rill5x)

rill6x <- lm(x_ft_illegal ~ rc + pid7x + ideo +
              edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + billtax + hlthcare1 + polforce,
            data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill6x)


rill7x <- lm(x_ft_illegal ~  rc + pid7x + ideo +
              edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + billtax + hlthcare1 + polforce + rc*factor(nonwhite),
            data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill7x)


rill8x <- lm(x_ft_illegal ~ rc + pid7x + ideo +
              edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + billtax + hlthcare1 + polforce + rc*pid7x,
            data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill8x)

rill9x <- lm(x_ft_illegal ~ rc + pid7x + ideo +
              edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + billtax + hlthcare1 + polforce + rc*RR,
            data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(rill9x)

### Table A3
stargazer(list(rill5x, rill6x, rill7x, rill8x, rill9x),
          title="Regression Results: Feelings Toward Illegal Immigrants",
          align=TRUE, dep.var.labels=c("Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT", "Illegal Immigrants FT"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Age 25-44", "Age 45-64",
                             "Age 65+", "Female", "Non-White", "Racial Resentment", "Anti-Intellectualism", "Anti-Elitism"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)














reg_eff1 <- lm(x_ft_illegal ~ rc + pid7x + ideo + edu + income + FEMALE + age + RR + elite + antiint,
               data = ANES_19_rswhites, weights=(ANES_19_rswhites$weight))
summary(reg_eff1)


reg_ill6nw <- lm(x_ft_illegal ~ rc + pid7x + ideo +
                  edu + income + FEMALE +  age + RR + elite + antiint,
                data = ANES_19_rsnonwhites, weights=(ANES_19_rsnonwhites$weight))
summary(reg_ill6nw)


## TAble A4
stargazer(list(reg_eff1, reg_ill6nw),
          title="Regression Results: Feelings Toward Illegal Immigrants",
          align=TRUE, dep.var.labels=c("Illegal Immigrants FT"),
          covariate.labels=c("Rural Consciousness", "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)






!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
model2Frame <- data.frame(Variable = rownames(summary(reg_ill2)$coef),
                          Coefficient = summary(reg_ill2)$coef[, 1],
                          SE = summary(reg_ill2)$coef[, 2],
                          modelName = "All Respondents")
model3Frame <- data.frame(Variable = rownames(summary(reg_ill6)$coef),
                          Coefficient = summary(reg_ill6)$coef[, 1],
                          SE = summary(reg_ill6)$coef[, 2],
                          modelName = "Rural/Small Town Only - All")
model4Frame <- data.frame(Variable = rownames(summary(reg_ill61)$coef),
                          Coefficient = summary(reg_ill61)$coef[, 1],
                          SE = summary(reg_ill61)$coef[, 2],
                          modelName = "Urban/Suburban Only - All")
model5Frame <- data.frame(Variable = rownames(summary(reg_ill6w)$coef),
                          Coefficient = summary(reg_ill6w)$coef[, 1],
                          SE = summary(reg_ill6w)$coef[, 2],
                          modelName = "Rural/Small Town Only - Whites")
model6Frame <- data.frame(Variable = rownames(summary(reg_ill6nw)$coef),
                          Coefficient = summary(reg_ill6nw)$coef[, 1],
                          SE = summary(reg_ill6nw)$coef[, 2],
                          modelName = "Rural/Small Town Only - Non-Whites")

allModelFrame_ill <- data.frame(rbind(model2Frame, model3Frame, model4Frame, model5Frame, model6Frame))  
allModelFrame_ill 
allModelFrame_ill <- filter(allModelFrame_ill, Variable!="(Intercept)")
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier



zp1 <- ggplot(allModelFrame_ill, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21)
zp1 <- zp1 + coord_flip() + theme_bw() 
zp1 <- zp1 + scale_x_discrete(labels = c('Age: 25-44','Age: 45-64', "Age: 65+", "Anti-Intellectualism",
                                         "Education Level", "Anti-Elite", "Rural Resident", "Small Town Resident", 
                                         "Suburban Resident", "Female", "Ideology", "Income Level", "Nonwhite",
                                         "Party Identity", "Rural Consciousness", "Racial Resentment"))
zp1 <- zp1 + ggtitle("Feeling Thermometer of Illegal Immigrants")  + theme(legend.position = "right", legend.title = element_blank())
print(zp1)  # The trick to these is position_dodge().



!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




## OTHER DVs


# legal imm
reg_l1 <- lm(x_ft_legal ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_l1)
reg_l2 <- lm(x_ft_legal ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_l2)
reg_l3 <- lm(x_ft_legal ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_l3)


reg_l4 <- lm(x_ft_legal ~  rc + pid7x + ideo +
                         edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
                       data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_l4)

reg_l5 <- lm(x_ft_legal ~ rc + pid7x + ideo +
                       edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
                     data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_l5)
reg_l6 <- lm(x_ft_legal ~ rc + pid7x + ideo +
                      edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
                    data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_l6)


## Table A7
stargazer(list(reg_l1, reg_l2, reg_l3, reg_l4, reg_l5, reg_l6),
          title="Regression Results: Feelings Toward Legal Immigrants",
          align=TRUE, dep.var.labels=c("Legal Immigrants FT"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)





# hisp ft

reg_h1 <- lm(x_ft_hisp ~ factor(resid_Place) + rc,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_h1)
reg_h2 <- lm(x_ft_hisp ~ factor(resid_Place) + rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_h2)
reg_h3 <- lm(x_ft_hisp ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_h3)


reg_h4 <- lm(x_ft_hisp ~  rc + pid7x + ideo +
               edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_h4)

reg_h5 <- lm(x_ft_hisp ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_h5)
reg_h6 <- lm(x_ft_hisp ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_h6)


# Table A6
stargazer(list(reg_h1, reg_h2, reg_h3, reg_h4, reg_h5, reg_h6),
          title="Regression Results: Feelings Toward Hispanics",
          align=TRUE, dep.var.labels=c("Hispanics FT"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)






# muslim ft

reg_m1 <- lm(x_ft_muslim ~ factor(resid_Place) + rc,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_m1)
reg_m2 <- lm(x_ft_muslim ~ factor(resid_Place) + rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_m2)
reg_m3 <- lm(x_ft_muslim ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_m3)


reg_m4 <- lm(x_ft_muslim ~  rc + pid7x + ideo +
               edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_m4)

reg_m5 <- lm(x_ft_muslim ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_m5)
reg_m6 <- lm(x_ft_muslim ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_m6)


# tablee A5
stargazer(list(reg_m1, reg_m2, reg_m3, reg_m4, reg_m5, reg_m6),
          title="Regression Results: Feelings Toward Muslims",
          align=TRUE, dep.var.labels=c("Muslims FT"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)









# ax_immignum


reg_nu1 <- lm(ax_immignum ~ factor(resid_Place) + rc,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_nu1)
reg_nu2 <- lm(ax_immignum ~ factor(resid_Place) + rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_nu2)
reg_nu3 <- lm(ax_immignum ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_nu3)


reg_nu4 <- lm(ax_immignum ~  rc + pid7x + ideo +
               edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_nu4)

reg_nu5 <- lm(ax_immignum ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_nu5)
reg_nu6 <- lm(ax_immignum ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_nu6)


# table A9

stargazer(list(reg_nu1, reg_nu2, reg_nu3, reg_nu4, reg_nu5, reg_nu6),
          title="Regression Results: Decrease Number of Immigrants in US",
          align=TRUE, dep.var.labels=c("Decrease Immigrant Numbers (General Imm. Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)







# border wall

reg_w1 <- lm(ax_wall ~ factor(resid_Place) + rc,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_w1)
reg_w2 <- lm(ax_wall ~ factor(resid_Place) + rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_w2)
reg_w3 <- lm(ax_wall~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_w3)


reg_w4 <- lm(ax_wall ~  rc + pid7x + ideo +
                edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_w4)

reg_w5 <- lm(ax_wall ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_w5)
reg_w6 <- lm(ax_wall ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_w6)


# Table A10
stargazer(list(reg_w1, reg_w2, reg_w3, reg_w4, reg_w5, reg_w6),
          title="Regression Results: Oppose Border Wall",
          align=TRUE, dep.var.labels=c("Oppose Border Wall (General Imm. Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



# Oppose more refugees

reg_re1 <- lm(ax_refugees ~ factor(resid_Place) + rc,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_re1)
reg_re2 <- lm(ax_refugees ~ factor(resid_Place) + rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_re2)
reg_re3 <- lm(ax_refugees~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_re3)


reg_re4 <- lm(ax_refugees ~  rc + pid7x + ideo +
               edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_re4)

reg_re5 <- lm(ax_refugees ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_re5)
reg_re6 <- lm(ax_refugees ~ rc + pid7x + ideo +
               edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
             data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_re6)


# A11
stargazer(list(reg_re1, reg_re2, reg_re3, reg_re4, reg_re5, reg_re6),
          title="Regression Results: Oppose More Refugees",
          align=TRUE, dep.var.labels=c("Oppose More Refugees (General Imm. Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)




# Oppose pathway to citizenship

reg_pa1 <- lm(bx_pathway ~ factor(resid_Place) + rc,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_pa1)
reg_pa2 <- lm(bx_pathway ~ factor(resid_Place) + rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_pa2)
reg_pa3 <- lm(bx_pathway ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_pa3)


reg_pa4 <- lm(bx_pathway ~  rc + pid7x + ideo +
                edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_pa4)

reg_pa5 <- lm(bx_pathway ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_pa5)
reg_pa6 <- lm(bx_pathway ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_pa6)


# table A12
stargazer(list(reg_pa1, reg_pa2, reg_pa3, reg_pa4, reg_pa5, reg_pa6),
          title="Regression Results: Oppose Pathway to Citizenship for Undocumented Migrants (Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Pathway to Citizenship (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)





# Oppose returning migrants to home country

reg_ret1 <- lm(bx_return ~ factor(resid_Place) + rc,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ret1)
reg_ret2 <- lm(bx_return ~ factor(resid_Place) + rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ret2)
reg_ret3 <- lm(bx_return ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ret3)


reg_ret4 <- lm(bx_return ~  rc + pid7x + ideo +
                edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ret4)

reg_ret5 <- lm(bx_return ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ret5)
reg_ret6 <- lm(bx_return ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ret6)


# Table A13
stargazer(list(reg_ret1, reg_ret2, reg_ret3, reg_ret4, reg_ret5, reg_ret6),
          title="Regression Results: Oppose Returning Undocumented Migrants to Country of Origin(Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Returning Migrants (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



# Oppose Decriminalizing Unauth

reg_dec1 <- lm(bx_open ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_dec1)
reg_dec2 <- lm(bx_open ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_dec2)
reg_dec3 <- lm(bx_open ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_dec3)


reg_dec4 <- lm(bx_open ~  rc + pid7x + ideo +
                 edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_dec4)

reg_dec5 <- lm(bx_open ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_dec5)
reg_dec6 <- lm(bx_open ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_dec6)


# Table A14
stargazer(list(reg_dec1, reg_dec2, reg_dec3, reg_dec4, reg_dec5, reg_dec6),
          title="Regression Results: Oppose Decriminalizing Unauthorized Migrants (Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Decriminalization (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)





# Oppose Holding/Detaining Unauthorized Migrants

reg_ho1 <- lm(bx_release1 ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ho1)
reg_ho2 <- lm(bx_release1 ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ho2)
reg_ho3 <- lm(bx_release1 ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho3)


reg_ho4 <- lm(bx_release1 ~  rc + pid7x + ideo +
                 edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho4)

reg_ho5 <- lm(bx_release1 ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho5)
reg_ho6 <- lm(bx_release1 ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho6)


# Table A15
stargazer(list(reg_ho1, reg_ho2, reg_ho3, reg_ho4, reg_ho5, reg_ho6),
          title="Regression Results: Oppose Holding/Detaining Unauthorized Migrants (Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Holding Migrants (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)


# Oppose Releasing and Monitoring Some

reg_ho12 <- lm(bx_release2 ~ factor(resid_Place) + rc,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ho12)
reg_ho22 <- lm(bx_release2 ~ factor(resid_Place) + rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_ho22)
reg_ho32 <- lm(bx_release2 ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho32)


reg_ho42 <- lm(bx_release2 ~  rc + pid7x + ideo +
                edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho42)

reg_ho52 <- lm(bx_release2 ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho52)
reg_ho62 <- lm(bx_release2 ~ rc + pid7x + ideo +
                edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
              data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_ho62)


#Table A16

stargazer(list(reg_ho12, reg_ho22, reg_ho32, reg_ho42, reg_ho52, reg_ho62),
          title="Regression Results: Oppose Releasing and Surveilling Some Unauthorized Migrants (Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Release and Surveillance (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



# Oppose Separating Families of Unauthorized Migrants

reg_fs12 <- lm(bx_famsep ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_fs12)
reg_fs22 <- lm(bx_famsep ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(reg_fs22)
reg_fs32 <- lm(bx_famsep ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_fs32)


reg_fs42 <- lm(bx_famsep ~  rc + pid7x + ideo +
                 edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_fs42)

reg_fs52 <- lm(bx_famsep ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_fs52)
reg_fs62 <- lm(bx_famsep ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(reg_fs62)


# Table A17
stargazer(list(reg_fs12, reg_fs22, reg_fs32, reg_fs42, reg_fs52, reg_fs62),
          title="Regression Results: Oppose Separating Families of Unauthorized Migrants (Undoc. Policy Attitudes)",
          align=TRUE, dep.var.labels=c("Oppose Family Separation (Undoc. Policy Attitudes)"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)




## Overall undoc imm policy scale

runimm1 <- lm(abc_illimmpol ~ factor(resid_Place) + rc,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(runimm1)
runimm2 <- lm(abc_illimmpol ~ factor(resid_Place) + rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(runimm2)
runimm3 <- lm(abc_illimmpol ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(runimm3)


runimm4 <- lm(abc_illimmpol ~  rc + pid7x + ideo +
                 edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(runimm4)

runimm5 <- lm(abc_illimmpol ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(runimm5)
runimm6 <- lm(abc_illimmpol ~ rc + pid7x + ideo +
                 edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
               data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(runimm6)


#Table 2
stargazer(list(runimm1, runimm2, runimm3, runimm4, runimm5, runimm6),
          title="Regression Results: Support for Pro-Undocumented Immgrant Policies",
          align=TRUE, dep.var.labels=c("Pro-Undocumented Immigrant Policies"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)



## Overall gen imm policy scale

unimm1x <- lm(abc_immpol ~ factor(resid_Place) + rc,
                 data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(unimm1x)
unimm2x <- lm(abc_immpol ~ factor(resid_Place) + rc + pid7x + ideo +
                   edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
                 data = ANES_19_Survey, weights=(ANES_19_clean_weight$weight))
summary(unimm2x)
unimm3x <- lm(abc_immpol ~ rc + pid7x + ideo +
                   edu + income  + age + FEMALE + nonwhite + RR + antiint + elite,
                 data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(unimm3x)


unimm4x <- lm(abc_immpol ~  rc + pid7x + ideo +
                   edu + income  + age + FEMALE +  nonwhite + RR + antiint + elite + rc*nonwhite,
                 data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(unimm4x)

unimm5x <- lm(abc_immpol ~ rc + pid7x + ideo +
                   edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*pid7x,
                 data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(unimm5x)
unimm6x <- lm(abc_immpol ~ rc + pid7x + ideo +
                   edu + income  + age + FEMALE + nonwhite + RR + antiint + elite + rc*RR,
                 data = ANES_19_ruralsmallonly, weights=(ANES_19_ruralsmallonly$weight))
summary(unimm6x)


#Table A8

stargazer(list(unimm1x, unimm2x, unimm3x, unimm4x, unimm5x, unimm6x),
          title="Regression Results: Support for Pro-Undocumented Immgrant Policies",
          align=TRUE, dep.var.labels=c("Pro-Undocumented Immigrant Policies"),
          covariate.labels=c("Rural Respondents","Small Town Respondents", "Suburban Respondents", "Rural Consciousness",
                             "Party ID","Symbolic Ideology","Education","Income", "Female", "Age 25-44", "Age 45-64",
                             "Age 65+", "Non-White", "Racial Resentment", "Anti-Elitism", "Anti-Intellectualism", "RC X Non-White", "RC X Party ID", "RC X RR"),
          omit.stat=c("LL","ser","f"), no.space=TRUE)





alpha.scale(x, y)




