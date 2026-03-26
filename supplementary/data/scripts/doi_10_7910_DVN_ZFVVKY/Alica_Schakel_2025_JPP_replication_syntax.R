
### The paper is based on NCP wave 19, download here:
# https://doi.org/10.18712/NSD-NSD2940-V6
# This is the replication R script which can be used to reproduce tables/figures
# in the main text in Alica, Berkay and Schakel, Arjan H. (2025)
# 'Does multilevel government increase legitimacy? Citizens' preferences for
# subnational authority and acceptance of governmental decisions,'
# Journal of Public Policy, forthcoming.###

### Please consult "Alica_Schakel_JPP_2025_dataset_creation.R" to see how the
# variables from NCP19 have been recoded and operationalised to produce replication dataset. ###

### The replication dataset includes 17 variables which all come from NCP19.###

### Install and open all the necessary packages for the analysis and data visualisation:
my_packages <- c("foreign", "car", "MASS", "stargazer", "ggplot2", "ggeffects",
                 "glm.predict", "haven", "dplyr", "viridis", "brant", "fixest",
                 "DescTools", "devtools", "coreSim", "separationplot",
                 "reshape2", "modmarg", "ggthemes", "gridExtra", "scales",
                 "repmis", "FactoMineR", "corrplot", "psych", "rms", "ordinal",
                 "effects", "erer", "arm", "lme4", "sjPlot", "jtools", "expss",
                 "Hmisc", "gmodels", "pollster", "installr", 'tidyr',
                 'marginaleffects', 'ggpubr', 'patchwork', 'modelsummary',
                 'extrafont', 'systemfonts')

my_installed_packages <- my_packages %in% rownames(installed.packages())
if (any(my_installed_packages == FALSE)) {
  install.packages(my_packages[!my_installed_packages])
}

invisible(lapply(my_packages, library, character.only = TRUE))

font_import() # This is to import the font "Sabon". JPP uses the font "Sabon".
loadfonts() # This is to import the font "Sabon". JPP uses the font "Sabon".

### Open the replication dataset: Alica_Schakel_2025_JPP_replication.rds ###

setwd("/Users/uibmacbookpro/Desktop/WtA")

dataset <- readRDS("Alica_Schakel_2025_JPP_replication.rds")
View(dataset)


####PREFERENCES FOR SUBNATIONAL AUTHORITY####

# After receiving this intro. The survey question was: "Norway has a system of
# governance comprising of three levels – the national, the regional (counties)
# and the municipal (municipality) level of governance.
# Please state if you think each of these is a desirable feature, or an
# undesirable feature of having different levels of government."
# The question asked for the following six items. Respondents could indicate for
# each item whether the statement was "very desirable", "somewhat desirable",
# "somewhat undesirable", "very undesirable".

dataset$sf1 # Having power divided up between different levels of government 
dataset$sf2 # Allowing different laws in response to varying needs and conditions in different parts of Norway 
dataset$sf3 # Different levels of government having power to hold each other to account for problems

dataset$sh1 # Allowing the governments of different parts of Norway to get involved in decision-making on national issues
dataset$sh2 # Different governments arguing over the best way to solve a particular problem 
dataset$sh3 # Different levels of government being forced to respect each other’s roles and responsibilities when dealing with a problem 


# WILLINGNESS TO ACCEPT####

### NOTE that the treatment depends on tier value. See Figure 1. 

dataset$first_answer_original #How willing are you to accept this decision?

dataset$decision_maker # The tier that made the decision.

dataset$second_answer #How willing are you to accept this decision?

table(dataset$treatment) # 6 values 


# TABLE 2: Survey items used to tap preferences for subnational authority.####

survey_items <- c('sf1', 'sf2', 'sf3', 'sh1', 'sh2', 'sh3')
pca_items <- dataset[, survey_items]

pca_result <- pca(pca_items) # Principal component analysis to obtain factor loadings.


# FIGURE 2: Share of respondents who are (not) willing to accept a decision.####

table(dataset$first_answer, dataset$decision_maker) #Syntax that produces a 3x5 table with estimates to produce the figure. 


# FIGURE 3: The impact of preferences for subnational authority on the willingness to accept a decision taken a municipality, county, or the national government. ####


####First run three models, one per tier (M, C, N) and save the model results (assign them to an object)

#### Fixed effects Model First Answer

m1.psa.natbase <- feols(first_answer ~ psa * tier_n | responseid,
                  data = dataset)

summary(m1.psa.natbase) # See coefficients of the model


####Second, produce estimates for 84% and 95% confidence intervals#

#### Preference for Subnational Authority (PSA) * Tier (N as baseline category)

preds.psalow.natbase <- plot_predictions(m1.psa.natbase,
                                         condition = list(tier_n = list("M", "C"),
                                                          psa = 0),
                                         conf_level = 0.84,
                                         gray = T,
                                         draw = F) # Get the estimates & CIs when psa low

preds.high.natbase <- plot_predictions(m1.psa.natbase,
                                       condition = list(tier_n = list("M", "C"),
                                                        psa = 1),
                                       conf_level = 0.84,
                                       gray = T,
                                       draw = F) # Get the estimates & CIs when psa high


###### Figure 3A ####

plot1.m1.natbase <- plot_predictions(m1.psa.natbase,
                                     condition = list(tier_n = list("M", "C"),
                                                      psa = "minmax"),
                                     conf_level = 0.95,
                                     gray = T)

plot1.m1.natbase <-  plot1.m1.natbase + geom_hline(yintercept = 2.096457 , #n.bas
                                                   linetype = 1) +
  labs(x="Decision taken by",
       y="Willingness to accept decision (4-point merged scale)",
       shape="Preference for\nSubnational Authority",
       title="A") +
  scale_x_discrete(labels = c("Municipality",
                              "County"))  +
  scale_shape_discrete(labels = c("Min (= 0)", "Max (= 1)"))

plot1.m1.natbase <- plot1.m1.natbase + geom_linerange(data=preds.psalow.natbase,
                                                      aes(y = estimate,
                                                          x = tier_n,
                                                          ymin = conf.low,
                                                          ymax = conf.high),
                                                      position = position_nudge(x = -0.037),
                                                      linewidth = 1.2) +
  geom_linerange(data = preds.high.natbase,
                 aes(y = estimate,
                     x = tier_n,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.037),
                 linewidth = 1.2) + ylim(1.25, 2.50)

plot1.m1.natbase <- plot1.m1.natbase + theme_classic() +
  theme(legend.position = c(0.25, 0.9),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, vjust = 1.5,
                                  face = "bold", size = 20),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        text = element_text(family = "serif"))

plot1.m1.natbase$layers[[1]]$geom_params$fatten = 8

ggsave("plot1_left_natbase.pdf", plot1.m1.natbase, width = 8, height = 6)


##### Figure 3B####

##### Line graph with N as baseline 

# 84% CIs

plot2.m1.natbase.84ci <- plot_predictions(m1.psa.natbase,
                                          condition = list("psa", "tier_n"),
                                          conf_level = 0.84,
                                          gray = T)

plot2.m1.natbase.84ci <- plot2.m1.natbase.84ci +
  labs(x = "Preference for subnational authority",
       y = "",
       linetype = "Decision taken by",
       title="B")

plot2.m1.natbase.84ci <- plot2.m1.natbase.84ci +
  scale_linetype_manual(labels = c("National government",
                                   "Municipality",
                                   "County"),
                        values = c("solid", "dashed", "dotted")) +
  theme_classic() + theme(legend.position = c(0.8, 0.2),
                          axis.text = element_text(size = 16),
                          axis.title = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5, vjust = 1.5,
                                                    face = "bold", size = 20),
                          legend.title = element_text(size = 18),
                          legend.text = element_text(size = 18),
                          text = element_text(family = "serif")
  ) + ylim(1.25, 2.50)


ggsave("plot1_right_natbase_84CI.pdf", plot2.m1.natbase.84ci, width = 11,
       height = 7)

#### Put Figure 3A and Figure 3B in one combined plot:

combined.plots.1 <- ggarrange(plot1.m1.natbase, plot2.m1.natbase.84ci,
                              nrow = 1, ncol = 2)

# Save Figure 3 as a .tiff file:
ggsave(combined.plots.1, filename = 'Figure_3.tiff',
         width = 15, height = 7, dpi = 600, device = "tiff")


# FIGURE 4: Share of respondents who change their willingness to accept a decision after they learn that another government supports a decision.#####

table(dataset$chng_A_ord) # Q2: Municipality closes a kindergarten.
table(dataset$chng_B_ord) # Q4: County closes a upper secondary school.
table(dataset$chng_C_ord) # Q6: National gov. closes a university/college department



### FIGURE 5 ####


# The second question was asked after receiving a treatment.
# Three tiers who took decisions, three treatments:
# M decides: county supports (5A), nat supports (5B), C&N support (5C)
# C decides: M supports (5D), N supports (5E), M&N support (5F)
# N decides: M supports (5G), C supports (5H), M&C support (5I)

# For each plot first calculate estimates, then plot. 

##### Figures 5A-C: Municipality decides to close kindergarten.####

m2.supportM_merged <- feols(second_answer ~ first_answer_factor * psa * support_m + tier_m | responseid,
                            data = dataset)
summary(m2.supportM_merged)
modelsummary(m2.supportM_merged, stars=T, output = "Q2_mun_merged.docx")

###### Figure 5A: Municipality decides to close kindergarten and county supports #####

# Produces estimates for 4xWtA-scores for respondent with low preference for
# subnat auth (0.44; mean minus 1 stdev) and for a respondent with a high
# preference subnat auth (0.88; mean plus 1 stdev)

preds.psalow.mundec <- plot_predictions(m2.supportM_merged,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .44,
                                                           support_m = c("Respondent's County Supports"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.mundec <- plot_predictions(m2.supportM_merged,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa = .88,
                                                         support_m = c("Respondent's County Supports"),
                                                         tier_m = c("M")),
                                      by = c("support_m", "psa", "first_answer_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

# Plot estimates

plot2.supportM_cou <- plot_predictions(m2.supportM_merged,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          support_m = c("Respondent's County Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_m_c <- ggplot(plot2.supportM_cou,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_c <- p2_m_c +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_c <- p2_M_c + geom_linerange(data=preds.psalow.mundec,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.1 <- p2_M_c  + labs(tag = "A:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.14, 0.97)) + annotate("text", x=3, y=3.35, label="*", size=8)

###### Figure 5B: M decides; N supports #####

preds.low.mundec.2 <- plot_predictions(m2.supportM_merged,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          support_m = c("National Government Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.mundec.2 <- plot_predictions(m2.supportM_merged,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           support_m = c("National Government Supports"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportM_nat <- plot_predictions(m2.supportM_merged,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          support_m = c("National Government Supports"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_m_n <- ggplot(plot2.supportM_nat,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_n <- p2_m_n +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_n <- p2_M_n + geom_linerange(data=preds.low.mundec.2,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)


my.plot.mun.2 <- p2_M_n + labs(tag = "B:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.14, 0.97)) 

###### Figure 5C: M decides; C&N supports #####

preds.low.mundec.3 <- plot_predictions(m2.supportM_merged,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          support_m = c("County & National Government Support"),
                                                          tier_m = c("M")),
                                       by = c("support_m", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.mundec.3 <- plot_predictions(m2.supportM_merged,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           support_m = c("County & National Government Support"),
                                                           tier_m = c("M")),
                                        by = c("support_m", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportM_counat <- plot_predictions(m2.supportM_merged,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa = c(.44, .88),
                                                             support_m = c("County & National Government Support"),
                                                             tier_m = c("M")),
                                          by = c("support_m", "psa", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_m_cn <- ggplot(plot2.supportM_counat,
                  aes(x = first_answer_factor,
                      y = estimate,
                      group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~support_m) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_M_cn <- p2_m_cn +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_M_cn <- p2_M_cn + geom_linerange(data=preds.low.mundec.3,
                                    aes(y = estimate,
                                        x = first_answer_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.mundec.3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.mun.3 <- p2_M_cn  + labs(tag = "C:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.13, 0.97))

#Merge Figures 5A-5C
p1 <- ggarrange(my.plot.mun.1, my.plot.mun.2, my.plot.mun.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "none") + plot_annotation(title="Municipality decides to close kindergarten") &
  theme(plot.title = element_text(hjust = .5, size = 18), legend.text = element_blank())


##### Figures 5D-F: County decides to close upper secondary school.####
m2.supportC <- feols(second_answer~ first_answer_factor * psa * Support_c + tier_c | responseid,
                     data = dataset)
summary(m2.supportC)

modelsummary::modelsummary(m2.supportC, stars = T,
                           output = "county_merged.docx", shape = term ~ model + statistic)

###### Figure 5D: County decides to close upper secondary school and municipality supports ####

preds.psalow.coudec <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .44,
                                                           Support_c = c("Respondent's Municipality Supports"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.coudec <- plot_predictions(m2.supportC,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa = .88,
                                                         Support_c = c("Respondent's Municipality Supports"),
                                                         tier_c = c("C")),
                                      by = c("Support_c", "psa", "first_answer_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

plot2.supportC_mun <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          Support_c = c("Respondent's Municipality Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)



p2_c_m <- ggplot(plot2.supportC_mun,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_m <- p2_c_m +
  labs(x="",
       y="Willingness to accept decision\nwhen support is provided by another tier",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5),
                           axis.title.y = element_text(size = 14, vjust = 3.5))


p2_C_m <- p2_C_m + geom_linerange(data=preds.psalow.coudec,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.1 <- p2_C_m + labs(tag = "D:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 17),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.19, 0.97)) + annotate("text", x=3, y=3.5, label="**", size=8)


###### Figure 5E: County decides to close upper secondary school and national government supports ####

preds.low.coudec.2 <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          Support_c = c("National Government Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.coudec.2 <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           Support_c = c("National Government Supports"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportC_nat <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          Support_c = c("National Government Supports"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_c_n <- ggplot(plot2.supportC_nat,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_n <- p2_c_n +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_C_n <- p2_C_n + geom_linerange(data=preds.low.coudec.2,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.2 <- p2_C_n  + labs(tag = "E:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.14, 0.97)) + annotate("text", x=1, y=2.1, label="*", size=8)


###### Figure 5F: County decides to close upper secondary school and municipality and national government support #####


preds.low.coudec.3 <- plot_predictions(m2.supportC,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          Support_c = c("Municipality & National Government Support"),
                                                          tier_c = c("C")),
                                       by = c("Support_c", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.coudec.3 <- plot_predictions(m2.supportC,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           Support_c = c("Municipality & National Government Support"),
                                                           tier_c = c("C")),
                                        by = c("Support_c", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportC_munnat <- plot_predictions(m2.supportC,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa = c(.44, .88),
                                                             Support_c = c("Municipality & National Government Support"),
                                                             tier_c = c("C")),
                                          by = c("Support_c", "psa", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_c_mn <- ggplot(plot2.supportC_munnat,
                  aes(x = first_answer_factor,
                      y = estimate,
                      group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support_c) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_C_mn <- p2_c_mn +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5)) +
  ylim(1,4)


p2_C_mn <- p2_C_mn + geom_linerange(data=preds.low.coudec.3,
                                    aes(y = estimate,
                                        x = first_answer_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.coudec.3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.cou.3 <- p2_C_mn + labs(tag = "F:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.12, 0.97))

#Merge Figures 5D-5F into one plot:
p2 <- ggarrange(my.plot.cou.1, my.plot.cou.2, my.plot.cou.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "none") 

p2 <- p2 + plot_annotation(title = "\nCounty decides to close upper secondary school") &
  theme(plot.title = element_text(hjust = .5, size = 18))

##### Figures 5G-I: National government decides to close a department of a university (college). ####

m2.supportN <- feols(second_answer ~ first_answer_factor * psa * Support + tier_n | responseid,
                     data = dataset)
summary(m2.supportN)

modelsummary::modelsummary(m2.supportN, stars = T,
                           output = "nat_merged.docx", shape = term ~ model + statistic)

###### Figure 5G: National government decides to close a department of a university (college) and municipality supports #####

preds.psalow.natdec <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .44,
                                                           Support = c("Respondent's Municipality Supports"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa low

preds.high.natdec <- plot_predictions(m2.supportN,
                                      newdata = datagrid(first_answer_factor = 1:4,
                                                         psa = .88,
                                                         Support = c("Respondent's Municipality Supports"),
                                                         tier_n = c("N")),
                                      by = c("Support", "psa", "first_answer_factor"),
                                      gray = T,
                                      conf_level = 0.84,
                                      draw = F) # Get the estimates & CIs when psa high

plot2.supportN_mun <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          Support = c("Respondent's Municipality Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)



p2_n_m <- ggplot(plot2.supportN_mun,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_m <- p2_n_m +
  labs(x="",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic()  + theme(legend.position = "bottom",
                           plot.title = element_text(hjust = 0.5, vjust = 1.5))


p2_N_m <- p2_N_m + geom_linerange(data=preds.psalow.natdec,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.1 <- p2_N_m  + labs(tag = "G:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.138, 0.961)) + annotate("text", x=c(2,3,4),
                                                        y=c(2.9,3.6,4.1),
                                                        label=c("**", "**", "*"), size=8)

###### Figure 5H: National govenrment decides to close a department of a university (college) and county supports #####

preds.low.natdec.2 <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          Support = c("Respondent's County Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.natdec.2 <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           Support = c("Respondent's County Supports"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportN_cou <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = c(.44, .88),
                                                          Support = c("Respondent's County Supports"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.95,
                                       draw = F)


p2_n_c <- ggplot(plot2.supportN_cou,
                 aes(x = first_answer_factor,
                     y = estimate,
                     group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_c <- p2_n_c +
  labs(x="Willingness to accept decision (Q1-Q3-Q5)",
       y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5),
                          axis.title.x = element_text(vjust = -0.25))


p2_N_c <- p2_N_c + geom_linerange(data=preds.low.natdec.2,
                                  aes(y = estimate,
                                      x = first_answer_factor,
                                      ymin = conf.low,
                                      ymax = conf.high),
                                  position = position_nudge(x = -0.05),
                                  linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.2,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.2 <- p2_N_c  + labs(tag = "H:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.138, 0.961)) + annotate("text",
                                                        x=c(1,2,3,4),
                                                        y=c(2.025,2.7,3.45,4.1),
                                                        label=c("*", "**", "**", "*"),
                                                        size=8)


###### Figure 5I: National govenrment decides to close a department of a university (college) and municipality and county supports #####

preds.low.natdec.3 <- plot_predictions(m2.supportN,
                                       newdata = datagrid(first_answer_factor = 1:4,
                                                          psa = .44,
                                                          Support = c("Municipality & County Support"),
                                                          tier_n = c("N")),
                                       by = c("Support", "psa", "first_answer_factor"),
                                       gray = T,
                                       conf_level = 0.84,
                                       draw = F) # Get the estimates & CIs when psa low

preds.high.natdec.3 <- plot_predictions(m2.supportN,
                                        newdata = datagrid(first_answer_factor = 1:4,
                                                           psa = .88,
                                                           Support = c("Municipality & County Support"),
                                                           tier_n = c("N")),
                                        by = c("Support", "psa", "first_answer_factor"),
                                        gray = T,
                                        conf_level = 0.84,
                                        draw = F) # Get the estimates & CIs when psa high

plot2.supportN_muncou <- plot_predictions(m2.supportN,
                                          newdata = datagrid(first_answer_factor = 1:4,
                                                             psa = c(.44, .88),
                                                             Support = c("Municipality & County Support"),
                                                             tier_n = c("N")),
                                          by = c("Support", "psa", "first_answer_factor"),
                                          gray = T,
                                          conf_level = 0.95,
                                          draw = F)


p2_n_mc <- ggplot(plot2.supportN_muncou,
                  aes(x = first_answer_factor,
                      y = estimate,
                      group = first_answer_factor, shape = psa)) +
  geom_hline(yintercept = c(1, 2, 3, 4), color='gray') + 
  facet_grid(.~Support) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high, group = psa),
                  position = position_dodge(width = 0.2), fatten = 7)

p2_N_mc <- p2_n_mc +
  labs(x="", y="",
       shape="Preference for Subnational Authority") +
  scale_shape_discrete(labels= c("Mean - 1SD (0.44)", "Mean + 1SD (0.88)")) +
  theme_classic() + theme(legend.position = "bottom",
                          plot.title = element_text(hjust = 0.5, vjust = 1.5)) 


p2_N_mc <- p2_N_mc + geom_linerange(data=preds.low.natdec.3,
                                    aes(y = estimate,
                                        x = first_answer_factor,
                                        ymin = conf.low,
                                        ymax = conf.high),
                                    position = position_nudge(x = -0.05),
                                    linewidth = 1.2) +
  geom_linerange(data = preds.high.natdec.3,
                 aes(y = estimate,
                     x = first_answer_factor,
                     ymin = conf.low,
                     ymax = conf.high),
                 position = position_nudge(x = 0.05),
                 linewidth = 1.2)



my.plot.nat.3 <- p2_N_mc + labs(tag = "I:") +
  theme(text = element_text(family = "serif"),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, vjust = 1.5, size = 16),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        strip.text.x = element_text(size = 12.5),
        plot.tag = element_text(face="bold"),
        plot.tag.position = c(0.138, 0.961)) + annotate("text",
                                                        x=c(1,2,3,4),
                                                        y=c(2.2,2.9,3.6,4.2),
                                                        label=c("*", "**", "**", "*"),
                                                        size=8)

#Combine Figures 5G-5I into one plot:
p3 <- ggarrange(my.plot.nat.1, my.plot.nat.2, my.plot.nat.3, nrow = 1, ncol = 3,
                common.legend = T, legend = "bottom") + plot_annotation(title = "\nNational government decides to close\na department of a university (college)") &
  theme(plot.title = element_text(hjust = .5, size = 18)) 

#Combine all into one plot that is Figure 5:
my.plot.all <- ggarrange(p1, p2, p3, nrow = 3, ncol = 1)

#Save Figure 5 as a .tiff file:
ggsave("Figure_5.tiff",
       width = 13, height = 16, dpi = 600, device = "tiff")

#### END ####