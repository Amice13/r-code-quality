
# Nottingham Replication Games: ----
#
# The Morning After:
# Cabinet Instability and the Purging of Ministers after Failed Coup Attempts in Autocracies
#
# Replicating the individual level analysis with the plm and fixest packages
#

setwd("~/Paper Projects/Replication_games/2023-notts-replication-games")

# Set-up

windowsFonts(Times=windowsFont("Times New Roman"))

pacman::p_load(tidyverse,texreg,haven,lfe,fixest,plm,lmtest,ggeffects,modelsummary,ggpubr)


# Individual level analysis ----


###
## Load data ----
###

df_aut <- read_rds("original-replication-material/morning_after_dataset_individuallevel.rds") %>% filter(democracy_lag == 0 & leader == 0 & !is.na(purged_y1) & !is.na(gdp_cap_pwt_ln) & !is.na(pop_pwt_ln))

##
## Get Number of coup attempts ----
##

df_aut_n <- df_aut %>% filter(!is.na(coupattempt_presentyear))

df_aut_names <- length(unique(df_aut_n$name))

n_countries <-  length(unique(df_aut_n$country_isocode))

table(df_aut_n$coupattempt_presentyear)

###
# Figure 3, 4, and 5 ----
###

## Responsibility ----

responsibility_felm <- felm(purged_y1 ~ minister_type/coupattempt_presentyear +
                              gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                              I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_aut)

## Affiliation ----

df_party <- df_aut %>% filter(party_group != "unknown") %>% mutate(party_group = as.factor(recode(party_group,"independent"="No party affiliation","leader_party"="From the leader's party","other_party"="From another party")))

affiliation_felm <- felm(purged_y1 ~ party_group/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                           I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_party)

## Importance ----

df_importance <- df_aut %>% filter(importance_factor != "0" & !is.na(importance_factor))

df_importance <- df_importance %>% mutate(importance_factor = fct_relevel(importance_factor,
                                                                          "Prime minister/President\n(not leader)",
                                                                          "Vice president,\ndeputy prime minister,\ntop minister",
                                                                          "Medium-ranking\nminister",
                                                                          "Low-ranking\nminister",
                                                                          "Junior minister\nor other low-\nranking post"
))

df_importance$importance_factor <- droplevels(df_importance$importance_factor)

importance_felm <- felm(purged_y1 ~ importance_factor/coupattempt_presentyear +
                          gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                          I(experience^3) | country_isocode+year | 0 | country_isocode , data=df_importance)

## Experience ----

df_experience <- df_aut %>% filter(importance_factor != "0.5")

quantiles_experience <- df_aut %>% summarise(x = quantile(experience, c(0.25, 0.5, 0.75)), experience = c(0.25, 0.5, 0.75))

experience_group <- felm(purged_y1 ~ as.factor(experience_group)/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode, data=df_experience)

## Combination (typology) ----

medianbyyear <- df_experience %>% group_by(country_isocode,year) %>% summarize(experience_median = median(experience,na.rm=TRUE))

df_combination <- df_experience %>% left_join(.,medianbyyear,by=c("country_isocode","year")) %>% filter(party_group != "unknown") %>%
  mutate(experience_bin = ifelse(experience >= experience_median, 1,0),
         important_bin = if_else(importance_numeric %in% c(3,4),1,0),
         loyal_bin = if_else(experience_bin == 1 & party_group == "leader_party",1,0),
         typology = case_when(important_bin == 0 & loyal_bin == 0 ~ 1,
                              important_bin == 1 & loyal_bin == 0 ~ 2,
                              important_bin == 0 & loyal_bin == 1 ~ 3,
                              important_bin == 1 & loyal_bin == 1 ~ 4
         )
  )


combination_felm <- felm(purged_y1 ~ as.factor(typology)/coupattempt_presentyear +
                           gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode , data=df_combination)


# Appendix K ----


## Responsibility ----

df_aut <- df_aut %>% mutate(minister_type = fct_relevel(minister_type,
                                                        "Minister of Defense",
                                                        "Minister of Foreign Affairs",
                                                        "Minister of Finance",
                                                        "Other type of minister",                                                      
                                                        "Minister of Natural Resources"
))


responsibility_felm_K <- felm(purged_y1 ~ minister_type*coupattempt_presentyear +
                                gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                                I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_aut)


## Party affiliation ----

df_party <- df_party %>% mutate(party_group = fct_relevel(party_group,
                                                          "From another party",
                                                          "No party affiliation",
                                                          "From the leader's party"
))

affiliation_felm_K <- felm(purged_y1 ~ party_group*coupattempt_presentyear +
                             gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                             I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_party)

## Importance ----

df_importance <- df_importance %>% mutate(importance_factor = fct_relevel(importance_factor,
                                                                          "Prime minister/President\n(not leader)",
                                                                          "Vice president,\ndeputy prime minister,\ntop minister",
                                                                          "Medium-ranking\nminister",
                                                                          "Low-ranking\nminister",
                                                                          "Junior minister\nor other low-\nranking post"
))
df_importance$importance_factor <- droplevels(df_importance$importance_factor)

importance_felm_K <- felm(purged_y1 ~ importance_factor*coupattempt_presentyear +
                            gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                            I(experience^3) | country_isocode+year | 0 | country_isocode, data=df_importance)

## Experience ----

experience_felm_K <- felm(purged_y1 ~ experience_group*coupattempt_presentyear +
                            gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode, data=df_experience)


## Combination ----

combination_felm_K <- felm(purged_y1 ~ relevel(as.factor(typology),2)*coupattempt_presentyear +
                             gdp_cap_pwt_ln + pop_pwt_ln | country_isocode+year | 0 | country_isocode , data=df_combination)


# PLM ----

## Responsibility ----

responsibility_plm <- plm(purged_y1 ~ minister_type/coupattempt_presentyear +
                            gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                            I(experience^3), index = c("country_isocode","year"), effect = "twoway", data = df_aut, model = "within",
                          vcov = function(x) vcovHC(x, cluster="group"))

## Affiliation ----

df_party <- df_aut %>% filter(party_group != "unknown") %>% mutate(party_group = as.factor(recode(party_group,"independent"="No party affiliation","leader_party"="From the leader's party","other_party"="From another party")))

affiliation_plm <- plm(purged_y1 ~ party_group/coupattempt_presentyear +
                         gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                         I(experience^3), index = c("country_isocode","year"), effect = "twoway", data = df_party, model = "within",
                       vcov = function(x) vcovHC(x, cluster="group"))

## Importance ----

#df_importance <- df_aut %>% filter(importance_factor != "0" & !is.na(importance_factor))
df_importance$importance_factor <- droplevels(df_importance$importance_factor)

importance_plm <- plm(purged_y1 ~ importance_factor/coupattempt_presentyear +
                        gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                        I(experience^3), index = c("country_isocode","year"), effect = "twoway", data = df_importance, model = "within",
                      vcov = function(x) vcovHC(x, cluster="group"))

## Experience ----

df_experience <- df_aut %>% filter(importance_factor != "0.5")

quantiles_experience <- df_aut %>% summarise(x = quantile(experience, c(0.25, 0.5, 0.75)), experience = c(0.25, 0.5, 0.75))

experience_group_plm <- plm(purged_y1 ~ as.factor(experience_group)/coupattempt_presentyear +
                              gdp_cap_pwt_ln + pop_pwt_ln, index = c("country_isocode","year"), effect = "twoway", data = df_experience, model = "within",
                            vcov = function(x) vcovHC(x, cluster="group"))

## Combination ----

medianbyyear <- df_experience %>% group_by(country_isocode,year) %>% summarize(experience_median = median(experience,na.rm=TRUE))

df_combination <- df_experience %>% left_join(.,medianbyyear,by=c("country_isocode","year")) %>% filter(party_group != "unknown") %>%
  mutate(experience_bin = ifelse(experience >= experience_median, 1,0),
         important_bin = if_else(importance_numeric %in% c(3,4),1,0),
         loyal_bin = if_else(experience_bin == 1 & party_group == "leader_party",1,0),
         typology = case_when(important_bin == 0 & loyal_bin == 0 ~ 1,
                              important_bin == 1 & loyal_bin == 0 ~ 2,
                              important_bin == 0 & loyal_bin == 1 ~ 3,
                              important_bin == 1 & loyal_bin == 1 ~ 4
         )
  )


combination_plm <- plm(purged_y1 ~ as.factor(typology)/coupattempt_presentyear +
                         gdp_cap_pwt_ln + pop_pwt_ln, index = c("country_isocode","year"), effect = "twoway", data = df_combination, model = "within",
                       vcov = function(x) vcovHC(x, cluster="group"))


# FIXEST ----

## Responsibility ----

responsibility_feols <- feols(purged_y1 ~ minister_type/coupattempt_presentyear +
                                gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                                I(experience^3) | country_isocode + year, vcov = ~country_isocode,
                              data = df_aut)

## Affiliation ----

affiliation_feols <- feols(purged_y1 ~ party_group/coupattempt_presentyear +
                             gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                             I(experience^3) | country_isocode + year, vcov = ~country_isocode,
                           data = df_party)

## Importance ----
df_importance$importance_factor <- droplevels(df_importance$importance_factor)

importance_feols <- feols(purged_y1 ~ importance_factor/coupattempt_presentyear +
                            gdp_cap_pwt_ln + pop_pwt_ln + experience + I(experience^2) +
                            I(experience^3) | country_isocode + year, vcov = ~country_isocode,
                          data = df_importance)

## Experience ----

experience_group_feols <- feols(purged_y1 ~ as.factor(experience_group)/coupattempt_presentyear +
                                  gdp_cap_pwt_ln + pop_pwt_ln | country_isocode + year, vcov = ~country_isocode,
                                data = df_experience)

## Combination ----

combination_feols <- feols(purged_y1 ~ as.factor(typology)/coupattempt_presentyear +
                             gdp_cap_pwt_ln + pop_pwt_ln | country_isocode + year, vcov = ~country_isocode,
                           data = df_combination)



# Tables ----


screenreg(list(responsibility_felm,responsibility_plm,responsibility_feols,responsibility_felm_K), digits = 6,
          custom.header = list("Responsibility"=1:4), custom.model.names = c("FELM","PLM","FEOLS","Appendix K"))

screenreg(list(affiliation_felm,affiliation_plm,affiliation_feols,affiliation_felm_K), digits = 6,
          custom.header = list("Affiliation"=1:4), custom.model.names = c("FELM","PLM","FEOLS","Appendix K"))

screenreg(list(importance_felm,importance_plm,importance_feols,importance_felm_K), digits = 6,
          custom.header = list("Importance"=1:4), custom.model.names = c("FELM","PLM","FEOLS","Appendix K"))

screenreg(list(experience_group,experience_group_plm,experience_group_feols,experience_felm_K), digits = 6,
          custom.header = list("Experience"=1:4), custom.model.names = c("FELM","PLM","FEOLS","Appendix K"))

screenreg(list(combination_felm,combination_plm,combination_feols,combination_felm_K), digits = 6,
          custom.header = list("Combination"=1:4), custom.model.names = c("FELM","PLM","FEOLS","Appendix K"))

# Tables LaTex ----


texreg(list(responsibility_felm,responsibility_plm,responsibility_feols), digits = 6,
       custom.header = list("Responsibility"=1:3), custom.model.names = c("FELM","PLM","FIXEST"), file = 'responsibility.tex')

texreg(list(affiliation_felm,affiliation_plm,affiliation_feols), digits = 6,
       custom.header = list("Affiliation"=1:3), custom.model.names = c("FELM","PLM","FIXEST"), file = 'affiliation.tex')

texreg(list(importance_felm,importance_plm,importance_feols), digits = 6,
       custom.header = list("Importance"=1:3), custom.model.names = c("FELM","PLM","FIXEST"), file = 'importance.tex')

texreg(list(experience_group,experience_group_plm,experience_group_feols), digits = 6,
       custom.header = list("Experience"=1:3), custom.model.names = c("FELM","PLM","FIXEST"), file = 'experience.tex')

texreg(list(combination_felm,combination_plm,combination_feols), digits = 6,
       custom.header = list("Combination"=1:3), custom.model.names = c("FELM","PLM","FIXEST"), file = 'combination.tex')


# Plots ----

####
## Set up template for graphs ----
####

pd <- position_dodge(0.75)

plot_canvas <- function(df=df,x=name,y=estimate,se=se){
  ggplot(data = df, aes(y = estimate, x = name)) + 
    geom_point(position = pd) + 
    geom_errorbar(aes(ymin = estimate-1.96*se, ymax = estimate+1.96*se),
                  width = 0,
                  position = pd,
                  size = 1) +
    coord_flip() +
    labs(x = "", 
         y = "Marginal Effects",
         color = "black") +
    geom_hline(yintercept = 0,
               linetype = "dashed") +
    theme_bw() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "",plot.title = element_text(size=14),
          axis.text.y = element_text(colour = "black",size=14),
          axis.text.x = element_text(colour = "black",size=14),
          text=element_text(family="Times",size=14,color="black"))
}

## Responsibility PLM ----
responsibility_plm_plot <- summary(responsibility_plm)


coef_responsibility <- tibble(name = c("Minister of Defense","Minister of Foreign Affairs","Minister of Finance",
                                       "Other type of minister","Minister of Natural Resources"),
                              estimate = c(responsibility_plm_plot$coefficients[10],
                                           responsibility_plm_plot$coefficients[11],
                                           responsibility_plm_plot$coefficients[12],
                                           responsibility_plm_plot$coefficients[13],
                                           responsibility_plm_plot$coefficients[14]),
                              se = c(responsibility_plm_plot$coefficients[10,2],
                                     responsibility_plm_plot$coefficients[11,2],
                                     responsibility_plm_plot$coefficients[12,2],
                                     responsibility_plm_plot$coefficients[13,2],
                                     responsibility_plm_plot$coefficients[14,2]
                              )) %>% mutate(name = fct_relevel(name,
                                                               "Minister of Natural Resources",
                                                               "Other type of minister",
                                                               "Minister of Finance",
                                                               "Minister of Foreign Affairs",
                                                               "Minister of Defense"
                              ))


plot_resp_plm <- plot_canvas(coef_responsibility) + ggtitle("H3a: Ministerial responsibility")

responsibility_feols_plot <- responsibility_feols

coef_responsibility <- tibble(name = c("Minister of Defense","Minister of Foreign Affairs","Minister of Finance",
                                       "Other type of minister","Minister of Natural Resources"),
                              estimate = c(responsibility_feols_plot$coefficients[10],
                                           responsibility_feols_plot$coefficients[11],
                                           responsibility_feols_plot$coefficients[12],
                                           responsibility_feols_plot$coefficients[13],
                                           responsibility_feols_plot$coefficients[14]),
                              se = c(responsibility_feols_plot$se[10],
                                     responsibility_feols_plot$se[11],
                                     responsibility_feols_plot$se[12],
                                     responsibility_feols_plot$se[13],
                                     responsibility_feols_plot$se[14]
                              )) %>% mutate(name = fct_relevel(name,
                                                               "Minister of Natural Resources",
                                                               "Other type of minister",
                                                               "Minister of Finance",
                                                               "Minister of Foreign Affairs",
                                                               "Minister of Defense"
                              ))


plot_resp_feols <- plot_canvas(coef_responsibility) + ggtitle("H3a: Ministerial responsibility")

ggarrange(plot_resp_plm, plot_resp_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

pdf(file = "plot_resp.pdf",   # The directory you want to save the file in
    width = 11.68, # The width of the plot in inches
    height = 8.26) # The height of the plot in inches


ggarrange(plot_resp_plm, plot_resp_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

dev.off()


## Affiliation PLM ----

affiliation_plm_plot <- summary(affiliation_plm)


coef_affiliation <- tibble(name = c("From another party","From the leader's party","No party affiliation"),
                           estimate = c(affiliation_plm_plot$coefficients[8],
                                        affiliation_plm_plot$coefficients[9],
                                        affiliation_plm_plot$coefficients[10]),
                           se = c(affiliation_plm_plot$coefficients[8,2],
                                  affiliation_plm_plot$coefficients[9,2],
                                  affiliation_plm_plot$coefficients[10,2]
                           )
) %>% mutate(name = fct_relevel(name,
                                "From the leader's party",
                                "No party affiliation",
                                "From another party"))


plot_affil_plm<- plot_canvas(coef_affiliation) + ggtitle("H3a: Party affiliation")

affiliation_feols_plot <- affiliation_feols

coef_affiliation <- tibble(name = c("From the leader's party",
                                    "No party affiliation",
                                    "From another party"),
                           estimate = c(affiliation_feols_plot$coefficients[8],
                                        affiliation_feols_plot$coefficients[9],
                                        affiliation_feols_plot$coefficients[10]),
                           se = c(affiliation_feols_plot$se[8],
                                  affiliation_feols_plot$se[9],
                                  affiliation_feols_plot$se[10]
                           )) %>% mutate(name = fct_relevel(name,
                                                            "From the leader's party",
                                                            "No party affiliation",
                                                            "From another party"
                           ))


plot_affil_feols <- plot_canvas(coef_affiliation) + ggtitle("H3a: Party affiliation")

ggarrange(plot_affil_plm, plot_affil_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

pdf(file = "plot_affil.pdf",   # The directory you want to save the file in
    width = 11.68, # The width of the plot in inches
    height = 8.26) # The height of the plot in inches


ggarrange(plot_affil_plm, plot_affil_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

dev.off()


## Importance PLM ----

importance_plm_plot <- summary(importance_plm)

coef_importance <- tibble(name = c("Prime minister/President\n(not leader)","Vice president,\ndeputy prime minister,\ntop minister",
                                   "Medium-ranking\nminister","Low-ranking\nminister","Junior minister\nor other low-\nranking post"),
                          estimate = c(importance_plm_plot$coefficients[10],
                                       importance_plm_plot$coefficients[11],
                                       importance_plm_plot$coefficients[12],
                                       importance_plm_plot$coefficients[13],
                                       importance_plm_plot$coefficients[14]),
                          se = c(importance_plm_plot$coefficients[10,2],
                                 importance_plm_plot$coefficients[11,2],
                                 importance_plm_plot$coefficients[12,2],
                                 importance_plm_plot$coefficients[13,2],
                                 importance_plm_plot$coefficients[14,2]
                          )
) %>% mutate(name = fct_relevel(name,"Junior minister\nor other low-\nranking post","Low-ranking\nminister",
                                "Medium-ranking\nminister","Vice president,\ndeputy prime minister,\ntop minister",
                                "Prime minister/President\n(not leader)"
                                
))


plot_importance_plm <- plot_canvas(coef_importance) + ggtitle("H3b: Importance in cabinet")

importance_feols_plot <- importance_feols

coef_importance <- tibble(name = c("Prime minister/President\n(not leader)","Vice president,\ndeputy prime minister,\ntop minister",
                                   "Medium-ranking\nminister","Low-ranking\nminister","Junior minister\nor other low-\nranking post"),
                          estimate = c(importance_feols_plot$coefficients[10],
                                       importance_feols_plot$coefficients[11],
                                       importance_feols_plot$coefficients[12],
                                       importance_feols_plot$coefficients[13],
                                       importance_feols_plot$coefficients[14]),
                          se = c(importance_feols_plot$se[10],
                                 importance_feols_plot$se[11],
                                 importance_feols_plot$se[12],
                                 importance_feols_plot$se[13],
                                 importance_feols_plot$se[14]
                          )
) %>% mutate(name = fct_relevel(name,
                                "Junior minister\nor other low-\nranking post","Low-ranking\nminister",
                                "Medium-ranking\nminister","Vice president,\ndeputy prime minister,\ntop minister",
                                "Prime minister/President\n(not leader)"))


plot_importance_feols <- plot_canvas(coef_importance) + ggtitle("H3b: Importance in cabinet")

ggarrange(plot_importance_plm, plot_importance_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

pdf(file = "plot_importance.pdf",   # The directory you want to save the file in
    width = 11.68, # The width of the plot in inches
    height = 8.26) # The height of the plot in inches


ggarrange(plot_importance_plm, plot_importance_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

dev.off()


## Experience PLM ----

experience_group_plm_plot <- summary(experience_group_plm)

coef_experience_group <- tibble(name = c("Less than 2 years","2-3 years",
                                         "4-6 years","Over 6 years"),
                                estimate = c(experience_group_plm_plot$coefficients[6],
                                             experience_group_plm_plot$coefficients[7],
                                             experience_group_plm_plot$coefficients[8],
                                             experience_group_plm_plot$coefficients[9]),
                                se = c(experience_group_plm_plot$coefficients[6,2],
                                       experience_group_plm_plot$coefficients[7,2],
                                       experience_group_plm_plot$coefficients[8,2],
                                       experience_group_plm_plot$coefficients[9,2]
                                )) %>% mutate(name = fct_relevel(name,
                                                                 "Over 6 years","4-6 years",
                                                                 "2-3 years",
                                                                 "Less than 2 years"
                                                                 
                                ))


plot_expgroup_plm <- plot_canvas(coef_experience_group) + ggtitle("H2a: Experience")

experience_group_feols_plot <- experience_group_feols

coef_experience_group <- tibble(name = c("Less than 2 years","2-3 years",
                                         "4-6 years","Over 6 years"),
                                estimate = c(experience_group_feols_plot$coefficients[6],
                                             experience_group_feols_plot$coefficients[7],
                                             experience_group_feols_plot$coefficients[8],
                                             experience_group_feols_plot$coefficients[9]),
                                se = c(experience_group_feols_plot$se[6],
                                       experience_group_feols_plot$se[7],
                                       experience_group_feols_plot$se[8],
                                       experience_group_feols_plot$se[9]
                                )) %>% mutate(name = fct_relevel(name,
                                                                 "Over 6 years","4-6 years",
                                                                 "2-3 years",
                                                                 "Less than 2 years"
                                                                 
                                ))


plot_expgroup_feols <- plot_canvas(coef_experience_group) + ggtitle("H2a: Experience")

ggarrange(plot_expgroup_plm, plot_expgroup_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

pdf(file = "plot_expgroup.pdf",   # The directory you want to save the file in
    width = 11.68, # The width of the plot in inches
    height = 8.26) # The height of the plot in inches


ggarrange(plot_expgroup_plm, plot_expgroup_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

dev.off()


## Combination PLM ----

combination_plm_plot <- summary(combination_plm)

coef_combination <- tibble(name = c("Low responsibility and\n weak signs of loyalty","High responsibility and\n weak signs of loyalty",
                                    "Low responsibility and\n strong signs of loyalty","High responsibility and\n strong signs of loyalty"),
                           estimate = c(combination_plm_plot$coefficients[6],
                                        combination_plm_plot$coefficients[7],
                                        combination_plm_plot$coefficients[8],
                                        combination_plm_plot$coefficients[9]),
                           se = c(combination_plm_plot$coefficients[6,2],
                                  combination_plm_plot$coefficients[7,2],
                                  combination_plm_plot$coefficients[8,2],
                                  combination_plm_plot$coefficients[9,2]
                           )) %>% mutate(name = fct_relevel(name,
                                                            "High responsibility and\n strong signs of loyalty","Low responsibility and\n strong signs of loyalty",
                                                            "High responsibility and\n weak signs of loyalty","Low responsibility and\n weak signs of loyalty"
                                                            
                           ))


plot_combination_plm <- plot_canvas(coef_combination) + ggtitle("H4: Combinations of traits")

combination_feols_plot <- combination_feols

coef_combination <- tibble(name = c("Low responsibility and\n weak signs of loyalty","High responsibility and\n weak signs of loyalty",
                                    "Low responsibility and\n strong signs of loyalty","High responsibility and\n strong signs of loyalty"),
                           estimate = c(combination_feols_plot$coefficients[6],
                                        combination_feols_plot$coefficients[7],
                                        combination_feols_plot$coefficients[8],
                                        combination_feols_plot$coefficients[9]),
                           se = c(combination_feols_plot$se[6],
                                  combination_feols_plot$se[7],
                                  combination_feols_plot$se[8],
                                  combination_feols_plot$se[9]
                           )) %>% mutate(name = fct_relevel(name,
                                                            "High responsibility and\n strong signs of loyalty","Low responsibility and\n strong signs of loyalty",
                                                            "High responsibility and\n weak signs of loyalty","Low responsibility and\n weak signs of loyalty"
                                                            
                           ))


plot_combination_feols <- plot_canvas(coef_combination) + ggtitle("H4: Combinations of traits")

ggarrange(plot_combination_plm, plot_combination_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

pdf(file = "plot_combination.pdf",   # The directory you want to save the file in
    width = 11.68, # The width of the plot in inches
    height = 8.26) # The height of the plot in inches


ggarrange(plot_combination_plm, plot_combination_feols, 
          labels = c("PLM", "FIXEST"),
          ncol = 2, nrow = 1)

dev.off()
