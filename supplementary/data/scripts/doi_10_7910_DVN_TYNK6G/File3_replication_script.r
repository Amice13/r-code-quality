
rm(list = ls())

# Load required packages
required_packages <- c(
  "sandwich", "lmtest", "doBy", "psych", 
  "dplyr", "broom", "broom.helpers", "ggeffects", "fixest",
  "texreg", "msm", "pscl", "alpaca", "endogeneity",
  "rnaturalearth", "countrycode", "viridis",
  "ggplot2", "stargazer", "xtable", "kableExtra")

# Check and install
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  }
}

install_if_missing(required_packages)

# Load all packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Set path to working directory
if(Sys.info()['user']=='lik86'){path_to_data<-"/Users/lik86/Dropbox/OHCHR_RIO/"} #set filepath for replication data folder
setwd(path_to_data)
# Set path to save files
savedfilepath <- '/Users/lik86/Dropbox/OHCHR_RIO/Figures/' #set filepath for objects to save

#####################################################
# Load main dataset
#####################################################
MergedData <- readRDS("File2_maindata.rds")

MergedData <- as.data.frame(MergedData)

names(MergedData)

# Define themes
theme1 <-   theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(), 
  axis.line = element_line(colour = "black"),
  element_text(color = "black", size=14),
  axis.text = element_text(color = "black", size=14),
  axis.title = element_text(color = "black", size = 14)
)

theme2 <-   theme(
  panel.grid.major = element_blank(), 
  panel.grid.minor = element_blank(),
  panel.background = element_blank(), 
  axis.line = element_line(colour = "black"),
  element_text(color = "black", size=14),
  axis.text = element_text(color = "black", size=12),
  axis.title = element_text(color = "black", size = 13)
)

########################################
### Figure 1: OHCHR visits over time
########################################

# use visits dataset
UNHC <- read.csv("File1_OHCHR_visits.csv")

sumvisits <- UNHC %>%
  group_by(Start.Year, Position) %>%
  summarise(total_visits = sum(Officials, na.rm = TRUE), .groups = 'drop') %>%  
  filter(Start.Year >= 1998 & Start.Year <= 2019) 

fig1 <- ggplot(sumvisits, aes(x = Start.Year, y = total_visits, 
                   fill = factor(Position))) +
  geom_bar(stat="identity", alpha=0.7) +
  labs(title = "", x = "", y = "Frequency", fill = "Position") +
  theme2 +
  scale_x_continuous(breaks = seq(1998, 2019, by = 1)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_viridis_d(option = "D", direction = -1)

print(fig1)

# # save figure
# ggsave(filename = "figure1.jpg",
#        plot = fig1,
#        width = 9, 
#        height = 4,
#        dpi = 600,
#        path = savedfilepath)

#######################################################
### Figure 2: Geographic distribution of OHCHR visits
#######################################################

# read world data
world <- ne_countries(scale = "medium", returnclass = "sf")

# COW codes for world data
world$ccode <- countrycode(world$sovereignt, "country.name", "cown")
world$ccode[world$sovereignt == "Republic of Serbia"] <- 345

# COW codes for UNHC data
UNHC$ccode <- countrycode(UNHC$Country.Visited, "country.name", "cown")
UNHC$ccode[UNHC$Country.Visited == "Yugoslavia"] <- 891
UNHC$ccode[UNHC$Country.Visited == "Lichtenstein"] <- 438

# collapse by country and merge with world data
UNHC.countrycollapse <- summaryBy(cbind(Officials, Conference.Only) ~ ccode, 
                                  FUN = sum, data = UNHC)
UNHC.world <- merge(world, UNHC.countrycollapse, by = 'ccode', all = TRUE)
UNHC.world$Officials.sum[is.na(UNHC.world$Officials.sum)] <- 0
UNHC.world$Conference.Only.sum[is.na(UNHC.world$Conference.Only.sum)] <- 0

# set theme
theme_set(theme_bw())

# Map 1: Visits 1998-2008
names(UNHC)[names(UNHC) == "Start.Year"] <- "year"
UNHC.early <- subset(UNHC, year < 2009)
UNHC.early.countrycollapse <- summaryBy(cbind(Officials, Conference.Only) ~ ccode, 
                                        FUN = sum, data = UNHC.early)
UNHC.early.world <- merge(world, UNHC.early.countrycollapse, by = 'ccode', all = TRUE)
UNHC.early.world$Officials.sum[is.na(UNHC.early.world$Officials.sum)] <- 0
UNHC.early.world$Conference.Only.sum[is.na(UNHC.early.world$Conference.Only.sum)] <- 0

early <- ggplot(data = UNHC.early.world) +
  geom_sf(aes(fill = Officials.sum)) +
  scale_fill_viridis_c(option = "D", direction = -1,
                       limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  ggtitle("Official Visits 1998-2008") +
  theme(plot.title = element_text(hjust = 0.5)) + 
  labs(fill = "Count")

print(early)

# Map 2: Visits 2009-2019
UNHC.late <- subset(UNHC, year >= 2009)
UNHC.late.countrycollapse <- summaryBy(cbind(Officials, Conference.Only) ~ ccode, 
                                       FUN = sum, data = UNHC.late)
UNHC.late.world <- merge(world, UNHC.late.countrycollapse, by = 'ccode', all = TRUE)
UNHC.late.world$Officials.sum[is.na(UNHC.late.world$Officials.sum)] <- 0
UNHC.late.world$Conference.Only.sum[is.na(UNHC.late.world$Conference.Only.sum)] <- 0

late <- ggplot(data = UNHC.late.world) +
  geom_sf(aes(fill = Officials.sum)) +
  scale_fill_viridis_c(option = "D", direction = -1,
                       limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  ggtitle("Official Visits 2009-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Count")

print(late)

# Map 3: Conference visits only 1998-2019
confs <- ggplot(data = UNHC.world) +
  geom_sf(aes(fill = Conference.Only.sum)) +
  scale_fill_viridis_c(option = "D", direction = -1,
                       limits = c(0, 8), breaks = seq(0, 8, by = 2)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  ggtitle("Conference Visits 1998-2019") +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(fill = "Count")

print(confs)

# save figures
# ggsave(filename = "figure2a_early.jpg",
#        plot = early,
#        width = 9, 
#        height = 4,
#        dpi = 600,
#        path = savedfilepath)

# ggsave(filename = "figure2b_late.jpg",
#        plot = late,
#        width = 9, 
#        height = 4,
#        dpi = 600,
#        path = savedfilepath)

# ggsave(filename = "figure2c_confs.jpg",
#        plot = confs,
#        width = 9, 
#        height = 4,
#        dpi = 600,
#        path = savedfilepath)


#################################
### Main models and figures
#################################

#################################
# BASELINE MODEL
#################################
m1 <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
            Polyarchy + Population + GDP, 
          family = binomial(link = "probit"), 
          data = MergedData)

# create identifier for model sample charachteristics
esample <- nobs(m1)
esample <- rownames(as.matrix(resid(m1)))
prediction.sample <- MergedData[esample, ]

# how many clusters
ccode.sample <- data.frame(unique(prediction.sample$ccode))
n_clusters <- nrow(ccode.sample)

# estimate model with clustered ses
m1_c <- coeftest(m1,
                 vcov = vcovCL, 
                 type = "HC1", 
                 cluster = ~ ccode, 
                 df = n_clusters - 1) 
nobs(m1_c)
logLik(m1_c)


# Tidy model, get confidence intervals
coef_data <- tidy(m1_c, conf.int = TRUE)

# Filter out intercept, reorder variables
coef_data <- coef_data %>%
  filter(term != "(Intercept)") %>%  
  mutate(term = factor(term, levels = c("GDP", "Population", "Polyarchy", 
                                        "Physical_Integrity_Rights")))

# Figure 3: Baseline Model
# Plot results
basemod <- ggplot(coef_data, aes(x = estimate, y = term)) +
  geom_point() +  
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1, size = 0.5) +  
  geom_vline(xintercept = 0, linetype = "dashed") +  
  scale_y_discrete(
    labels = c(
      "Physical_Integrity_Rights" = "Physical Integrity Rights"
    )  # Customize variable labels
  ) +
  xlim(c(-0.4,0.6)) +
  labs(y = NULL, x = "Estimate") +  # Customize axis labels
  theme1 
print(basemod)

# save image
# ggsave(filename="figure3.jpg", 
#        plot=basemod, 
#        width=7, height=4,
#        dpi=600,
#        path=savedfilepath)

##############################
# FULL MODEL
##############################
m2 <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
            Democratic_Episode + Autocratic_Episode + 
            Peacekeepers + 
            Polyarchy + Population + GDP, 
          family = binomial(link = "probit"), 
          data = MergedData)

m2_c <- coeftest(m2, 
                 vcov = vcovCL, 
                 type = "HC1", 
                 cluster = ~ccode, 
                 df = n_clusters - 1)

# Tidy model, get confidence intervals
coef_data2 <- tidy(m2_c, conf.int = TRUE) 

# Filter out intercept, reorder variables
coef_data2 <- coef_data2 %>%
  filter(term != "(Intercept)") %>%  
  mutate(term = factor(term, 
                       levels = c("GDP", "Population", "Polyarchy", 
                                  "Physical_Integrity_Rights",
                                  "Peacekeepers",
                                  "Autocratic_Episode", 
                                  "Democratic_Episode")))  

# Figure 4: Full Model
# Plot results
fullmod <- ggplot(coef_data2, aes(x = estimate, y = term)) +
  geom_point() +  
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1, size = 0.5) +  
  geom_vline(xintercept = 0, linetype = "dashed") +  
  scale_y_discrete(
    labels = c(
      "Physical_Integrity_Rights" = "Physical Integrity Rights",
      "Autocratic_Episode" = "Autocratic Episode",
      "Democratic_Episode" = "Democratic Episode")
  ) +
  xlim(c(-0.4,0.6)) +
  labs(y = NULL, x = "Estimate") +  # Customize axis labels
  theme1 

print(fullmod)

# save image
# ggsave(filename="figure4.jpg", 
#        plot=fullmod, 
#        width=7, height=4,
#        dpi=600,
#        path=savedfilepath)

####################################################################
# Full results for baseline and full models (Appendix B, Table A3)
####################################################################
covlabs = c("Physical Integrity Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP")

stargazer(m1_c, m2_c, 
          type = "text", #comment out for latex
          title = "Full Results for Figures 2 & 3", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534', '3534'), c('Log Likelihood', '-859.1071', '-849.8656'))
)       

##########################################################################################
# Figure 5: Probability of a visit by physical integrity rights and peacekeeping troops 
##########################################################################################

# physical integrity rights 
predict.Physint <- ggpredict(m2, 
                             terms = "Physical_Integrity_Rights[n=12]",
                             condition = c(Democratic_Episode = 0, Autocratic_Episode = 0), 
                             vcov_fun = "vcovCL", 
                             vcov_type = "HC1",
                             vcov.args = list(cluster = MergedData$ccode))

predict_df <- as.data.frame(predict.Physint)

plot_physint <- ggplot(predict_df, aes(x = x, y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  labs(title = "", y = "Probability of Official Visit", 
       x = "Physical Integrity Rights") + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), 
                     limits = c(0, 0.3)) +  # convert y to percentage
  theme1 +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.25), 
        panel.grid.minor = element_line(color = "grey90", size = 0.25)) 

print(plot_physint)

# peacekeeping troops
predict.pko <- ggpredict(m2, 
                         terms = "Peacekeepers[n=12]",
                         condition = c(Democratic_Episode = 0, Autocratic_Episode = 0), 
                         vcov_fun = "vcovCL", 
                         vcov_type = "HC1",
                         vcov.args = list(cluster = MergedData$ccode))

predict_df2 <- as.data.frame(predict.pko)
plot_pko <- ggplot(predict_df2, aes(x = x, y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  labs(title = "", y = "Probability of Official Visit", 
       x = "Peacekeeping Troops") + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_x_continuous(n.breaks = 6) +
  theme1 +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.25), 
        panel.grid.minor = element_line(color = "grey90", size = 0.25)) 

print(plot_pko)

# save images
# ggsave(filename="figure5a_physint.jpg", 
#        plot=plot_physint, 
#        width=5, height=4,
#        dpi=600,
#        path=savedfilepath)
# 
# ggsave(filename="figure5b_pko.jpg", 
#        plot=plot_pko, 
#        width=5, height=4,
#        dpi=600,
#        path=savedfilepath)

############################################################
# Figure 6: Probability of a visit by democratic episode
############################################################
predict.dem <- ggpredict(m2, 
                         terms = "Democratic_Episode[n=2]",
                         condition = c(Autocratic_Episode = 0), 
                         vcov_fun = "vcovCL", 
                         vcov_type = "HC1",
                         vcov.args = list(cluster = MergedData$ccode))
summary(predict.dem)

predict_df3 <- as.data.frame(predict.dem)
predict_df3 <- predict_df3[predict_df3$x %in% c(0, 1), ]

plot_dem <- ggplot(predict_df3, aes(x = as.factor(x), y = predicted)) +  
  geom_bar(stat = "identity", fill = "darkgray", 
           alpha = 0.6, width=0.5, position = "dodge") + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +  
  labs(title = "", y = "Probability of Official Visit", 
       x = "Democratic Episode") + 
  scale_y_continuous(labels = scales::label_percent(accuracy = .5),
                     breaks = seq(0, 0.125, by = 0.025), 
                     limits = c(0, 0.125)) +  
  theme1

print(plot_dem)

# save image
# ggsave(filename="figure6.jpg", 
#        plot=plot_dem, 
#        width=5, height=4,
#        dpi=600,
#        path=savedfilepath)


######################################
# CONFERENCE VISITS ANALYSIS
######################################

# Appendix B, Table A4: 
m3 <- glm(Conference_di_lead ~  Physical_Integrity_Rights + 
            Democratic_Episode + Autocratic_Episode + Peacekeepers +
            Polyarchy + Population + GDP, 
          family = binomial(link = "probit"), data = MergedData)
#estimating models with clustered ses
m3_c <- coeftest(m3,
                 vcov = vcovCL, 
                 type = "HC1", 
                 cluster = ~ ccode, 
                 df = n_clusters -1) 
nobs(m3_c)
logLik(m3_c)

stargazer(m3_c, 
          type = "text", #comment out for latex
          title = "Full Results for Figure 4", style = "default", 
          covariate.labels = covlabs,
          dep.var.caption = NULL,
          dep.var.labels = c("Conference Visit"),
          add.lines=list(c('Observations','3534'), c('Log Likelihood', ' -308.183' ))
)       

# Figure 7: Conference visits
coef_data3 <- tidy(m3_c, conf.int = TRUE)

coef_data3 <- coef_data3 %>%
  filter(term != "(Intercept)") %>%
  mutate(term = factor(term, 
                       levels = c("GDP", "Population", "Polyarchy", 
                                  "Physical_Integrity_Rights",
                                  "Peacekeepers",
                                  "Autocratic_Episode", 
                                  "Democratic_Episode")))

plot_confs <- ggplot(coef_data3, aes(x = estimate, y = term)) +
  geom_point() +  
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), 
                width = 0.1, size = 0.5) +  
  geom_vline(xintercept = 0, linetype = "dashed") +  
  scale_y_discrete(
    labels = c(
      "Physical_Integrity_Rights" = "Physical Integrity Rights",
      "Autocratic_Episode" = "Autocratic Episode",
      "Democratic_Episode" = "Democratic Episode")
  ) +
  xlim(c(-1, 1)) +
  labs(y = NULL, x = "Estimate") +
  theme1 

print(plot_confs)

# save image
# ggsave(filename="figure7.jpg", 
#        plot=plot_confs, 
#        width=7, height=4,
#        dpi=600,
#        path=savedfilepath)


########################################
### OTHER APPENDIX TABLES AND FIGURES
########################################

# Table A1: Summary Statistics
sumvars <- c("Official_di_lead", 
             "Physical_Integrity_Rights",
             "Democratic_Episode", "Autocratic_Episode",
             "Peacekeepers",
             "Polyarchy", "Population", "GDP")

sumvars_names <- c("Official Visit", 
                   "Physical Integrity Rights", 
                   "Democratic Episode", 
                   "Autocratic Episode", 
                   "Peacekeepers", 
                   "Polyarchy", 
                   "Population", 
                   "GDP")

desc_stats <- describe(prediction.sample[, sumvars])
summary_stats <- desc_stats[, c("n", "mean", "sd", "min", "max")]
rownames(summary_stats) <- sumvars_names

print(summary_stats)


# Table A2: Total Visits and Mean Visits Per Year by OHCHR Official
# use visits dataset
UNHC <- read.csv("File1_OHCHR_visits.csv")

visit_summary_total <- UNHC %>%
  group_by(Official, Position) %>%
  summarise(
    Total_Visits = n(),
    Mean_Visits_Per_Year = Total_Visits / n_distinct(Start.Year),
    .groups = 'drop'
  ) 

visit_summary_total <- visit_summary_total %>%
  mutate(Mean_Visits_Per_Year = round(Mean_Visits_Per_Year, 3))

print(visit_summary_total)
# 
# kable(visit_summary_total, 
#       format = "latex", booktabs = TRUE, 
#       caption = "Total Visits and Mean Visits Per Year by OHCHR Official",
#       col.names = c("Official", "Position", 
#                     "Total Visits", "Mean Visits per Year"))


# Figure A1: Visits by OHCHR officials over time
summary_UNHC <- UNHC %>%
  group_by(Start.Year,Official) %>%
  summarise(total_visits = sum(Officials, na.rm = TRUE)) %>%  
  filter(Start.Year >= 1998 & Start.Year <= 2019)

timetrend_UNHC <- ggplot(summary_UNHC, aes(x = Start.Year, y = total_visits, fill = factor(Official))) +
  geom_bar(stat="identity", alpha=0.8) +
  labs(title = "", x = "", y = "Frequency", fill = "Individual") +
  theme2 +
  scale_x_continuous(breaks = seq(1998, 2019, by = 1)) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  + 
  scale_fill_viridis_d(option = "C", direction = -1)

print(timetrend_UNHC)

# save image
# ggsave(filename="figureA1.jpg",
#        plot=timetrend_UNHC,
#        width=9, height=4,
#        dpi=300,
#        path=savedfilepath)


# Table A5: Top 3 Visit Sites (Including Ties) by OHCHR Official
visit_summary <- UNHC %>%
  group_by(Official, Country.Visited) %>%
  summarise(
    Visits = n(),
    Position = first(Position),  # Capture Position for each official
    .groups = 'drop'
  )

visit_summary_rank <- visit_summary %>%
  group_by(Official) %>%
  slice_max(order_by = Visits, n = 3, with_ties = TRUE) %>%
  arrange(Official, desc(Visits))

print(visit_summary_rank)
# kable(visit_summary_rank[c("Official", "Position", "Country.Visited", "Visits")],  
#       format = "latex",
#       booktabs = TRUE, 
#       caption = "Top 3 Visit Sites (Including Ties) by OHCHR Official", 
#       col.names = c("Official", "Position", "Country Visited", "Visits"),
#       longtable = TRUE) %>%
#   kable_styling(latex_options = c("hold_position"))


# Table A6: High Commissioner-Fixed Effects Probit with Bias Correction
# model 1
hcfe <- feglm(Official_di_lead ~ Physical_Integrity_Rights +
                Polyarchy + Population + GDP  |  hc.id, 
              MergedData, family = binomial(probit)) 
summary(hcfe, type = "clustered", cluster = ~hc.id)
hcfe_bc <- biasCorr(object = hcfe)
summary(hcfe_bc, type = "clustered", cluster = ~hc.id)

# model 2 with fixed effects
hcfe2 <- feglm(Official_di_lead ~ Physical_Integrity_Rights +
                Democratic_Episode + Autocratic_Episode + 
                Peacekeepers + 
                Polyarchy + Population + GDP  |  hc.id, 
              MergedData, family = binomial(probit)) 
summary(hcfe2, type = "clustered", cluster = ~hc.id)
hcfe2_bc <- biasCorr(object = hcfe2)

summary(hcfe2_bc, type = "clustered", cluster = ~hc.id)

## run to export results
# custom_names <- c("Physical Integrity Rights", 
#                   "Polyarchy", 
#                   "Population", 
#                   "GDP", 
#                   "Democratic Episode", 
#                   "Autocratic Episode", 
#                   "Peacekeepers")
# 
# texreg(l = list(hcfe_bc, hcfe2_bc), file = "feprobit.tex", 
#        inline.css = FALSE, doctype = TRUE, 
#        head.tag = TRUE, body.tag = TRUE, caption.above = TRUE,  
#        caption = "High Commissioner-Fixed Effects Probit with Bias Correction",        
#        stars = c(0.01, 0.05, 0.1),
#        digits = 3,
#        custom.coef.names = custom_names)


# Table A7: Physical Integrity Rights Squared & 
# Figure A2: Predicted Probability: Physical integrity Rights Squared
pir_min <- abs(min(MergedData$Physical_Integrity_Rights, na.rm=T))
MergedData$physint_rescale <- MergedData$Physical_Integrity_Rights + pir_min

m_sq <- glm(Official_di_lead ~ physint_rescale + I(physint_rescale^2) +
              Democratic_Episode + Autocratic_Episode + Peacekeepers +
              Polyarchy + Population + GDP, 
            family = binomial(link = "probit"), 
            data = MergedData)
m_sq_c <- coeftest(m_sq, 
                   vcov = vcovCL, 
                   type = "HC1", 
                   cluster = ~ccode, 
                   df = n_clusters-1) 

nobs(m_sq_c)
logLik(m_sq_c)

stargazer(m_sq_c, 
          type = "text",
          title = "Physical Integrity Rights Squared", style = "default", 
          covariate.labels = c("Physical Integrity Rights", "Physical Integrity Squared",
                               "Democratic Episode", "Autocratic Episode", "Peacekeepers",
                               "Polyarchy", "Population", "GDP"), 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534'), c('Log Likelihood', '-847.9657'))
)     

predict.Physint2 <- ggpredict(m_sq, 
                              terms = "physint_rescale[n=12]",
                              condition = c(Democratic_Episode = 0, Autocratic_Episode = 0), 
                              vcov_fun = "vcovCL", 
                              vcov_type = "HC1",
                              vcov.args = list(cluster = MergedData$ccode))

pred_df <- as.data.frame(predict.Physint2)

# create plot 
plot_physint2 <- ggplot(pred_df, aes(x = x, y = predicted)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) + 
  labs(title = "", y = "Probability of Official Visit", 
       x = "Physical Integrity Rights") + 
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +  
  theme1 +
  theme(panel.grid.major = element_line(color = "grey90", size = 0.25), 
        panel.grid.minor = element_line(color = "grey90", size = 0.25)) 
plot_physint2

# save image
# ggsave(filename="figureA2.jpg", 
#        plot=plot_physint2, 
#        width=5, height=4,
#        dpi=200,
#        path=savedfilepath)


# Table A8: Zero Inflated Poisson
summary(zipmod <- zeroinfl(Officials_count_lead ~ Physical_Integrity_Rights + 
                          Polyarchy + Population + GDP + 
                          Democratic_Episode + Autocratic_Episode + 
                          Peacekeepers 
                        |Physical_Integrity_Rights , 
                        data = MergedData))

# # export results
# texreg(l = list(zipmod), file = "zip.tex", 
#        inline.css = FALSE, doctype = TRUE, 
#        head.tag = TRUE, body.tag = TRUE, caption.above = TRUE,  
#        caption = "Zero Inflated Poisson",        
#        stars = c(0.01, 0.05, 0.1),
#        digits = 3,
# )


# Table A9: Split Sample
# split sample and only estimate model on cases below median on physint 
# (using median from sample estimation)

physint.below.med <- subset(MergedData, Physical_Integrity_Rights < 0.4176)

splitmod1 <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Polyarchy + Population + GDP, 
           family = binomial(link = "probit"), 
           data = physint.below.med)


splitmod1_c <- coeftest(splitmod1, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 112) #the number of clusters-1

nobs(splitmod1_c)
logLik(splitmod1_c)

#adding democratization, autocratization, peacekeepers
splitmod2 <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP, 
           family = binomial(link = "probit"), 
           data = physint.below.med)

splitmod2_c <- coeftest(splitmod2, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 112) #the number of clusters-1

nobs(splitmod2_c)
logLik(splitmod2_c)

covlabs = c("Physical Integrity Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP")

stargazer(splitmod1_c, splitmod2_c, 
          type = "text", #comment out for latex
          title = "Split Sample", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','1766', '1766'), c('Log Likelihood', '-565.7777', '-558.8671'))
)          


# Table A10: Substituting Civilian Killings
civmod <- glm(Official_di_lead ~ log_Civilian_Killings + 
            Democratic_Episode + Autocratic_Episode + Peacekeepers +
            Polyarchy + Population + GDP,
          family = binomial(link = "probit"), data = MergedData)
civmod_c <- coeftest(civmod, 
                 vcov = vcovCL, 
                 type = "HC1", 
                 cluster = ~ccode, 
                 df = 170) #the number of clusters-1

nobs(civmod_c)
logLik(civmod_c)

stargazer(civmod_c, 
          type = "text",
          title = "Substituting Civilian Killings", style = "default", 
          covariate.labels = c("Civilian Killings", "Democratic Episode",
                               "Autocratic Episode", "Peacekeepers",
                               "Polyarchy", "Population", "GDP"), 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534'), c('Log Likelihood', '-852.6012'))
)     


# Table A11: Alternative Human Rights Measures
cirimod<- glm(Official_di_lead ~ Physical.Integrity.Rights.Index + Civil.and.Political.Rights.Index +
             Women.s.Political.Rights + Democratic_Episode + Autocratic_Episode +
             Peacekeepers +  Polyarchy + Population + GDP, 
           family = binomial(link = "probit"), 
           data = MergedData)

cirimod_c <- coeftest(cirimod, 
                   vcov = vcovCL, 
                   type = "HC1", 
                   cluster = ~ccode, 
                   df = 170) #the number of clusters-1

nobs(cirimod_c)
logLik(cirimod_c)

covlabs = c("Physical Integrity Rights", "Civil and Political Rights", "Women's Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP")

stargazer(cirimod_c, 
          type = "text", 
          title = "Alternate Human Rights Measures", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534', '3534'), c('Log Likelihood', '-859.1071', '-849.8656'))
)      


# Table A12: Alternative Regime Transition
# higher threshold for democratic transition
dem_hi <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
              Peacekeepers + polity2 + Population + GDP + dem.trans.maj + neg.trans , 
            family = binomial(link = "probit"), 
            data = MergedData)

dem_hi_c <- coeftest(dem_hi, 
                   vcov = vcovCL, 
                   type = "HC1", 
                   cluster = ~ccode, 
                   df = 170) #the number of clusters-1

nobs(dem_hi_c)
logLik(dem_hi_c)

# lower threshold for democratic transition
dem_lo <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
              Peacekeepers  + polity2 + Population + GDP + dem.trans.min + neg.trans , 
            family = binomial(link = "probit"), 
            data = MergedData)

dem_lo_c <- coeftest(dem_lo, 
                   vcov = vcovCL, 
                   type = "HC1", 
                   cluster = ~ccode, 
                   df = 170) #the number of clusters-1

nobs(dem_lo_c)
logLik(dem_lo_c)


covlabs = c("Physical Integrity Rights",
            "Peacekeepers","Polyarchy", "Population", "GDP",
            "Major Democratic Transition", "Minor Democratic Transition", "Negative Regime Change")

stargazer(dem_hi_c, dem_lo_c, 
          type = "text", #comment out for latex
          title = "Alternate Regime Transition", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3172', '3172'), c('Log Likelihood', '-767.3811', '-767.1183'))
)          


# Table A13: Additional Controls
# model 1 - with conflict
add_conflict <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP + intensity_ordered, 
           family = binomial(link = "probit"), 
           data = MergedData)

add_conflict_c <- coeftest(add_conflict, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(add_conflict_c)
logLik(add_conflict_c)

# model 2 - with spatial lag 
add_sp <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
              Democratic_Episode + Autocratic_Episode + 
              Peacekeepers + 
              Polyarchy + Population + GDP + W_official_dum_con400_lead , 
            family = binomial(link = "probit"), 
            data = MergedData)

add_sp_c <- coeftest(add_sp, 
                   vcov = vcovCL, 
                   type = "HC1", 
                   cluster = ~ccode, 
                   df = 170) #the number of clusters-1

nobs(add_sp_c)
logLik(add_sp_c)

covlabs = c("Physical Integrity Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP", "Conflict", "Spatial Lag")

stargazer(add_conflict_c, add_sp_c, 
          type = "text", #comment out for latex
          title = "Additional Controls", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534', '3489'), c('Log Likelihood', '-848.6288', '-810.5375'))
)  


# Table A14: Biviarate Probit
biprob <- biprobit(Official_di_lead ~ Physical_Integrity_Rights +  Democratic_Episode
                 + Autocratic_Episode + Peacekeepers + Polyarchy + Population + GDP
                 , Conference_di_lead ~ 
                   Physical_Integrity_Rights + Democratic_Episode + 
                   Autocratic_Episode + Peacekeepers + Polyarchy + Population + GDP, 
                 data = prediction.sample)

print(biprob$estimates, digits=3)

# export results
# texreg(l = list(rr14), file = "biprob.tex", 
#        inline.css = FALSE, doctype = TRUE, 
#        head.tag = TRUE, body.tag = TRUE, caption.above = TRUE,  
#        caption = "Bivariate Provit",        
#        stars = c(0.01, 0.05, 0.1),
#        digits = 3,
# )


# Table A15: Only High Commissioner Visits
hconly <- glm(HC_only_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP, 
           family = binomial(link = "probit"), 
           data = MergedData)

hconly_c <- coeftest(hconly, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(hconly_c)
logLik(hconly_c)

covlabs = c("Physical Integrity Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP")

stargazer(hconly_c, 
          type = "text", #comment out for latex
          title = "Only High Commissioner Visits", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534'), c('Log Likelihood' , '-609.0722'))
)          


# Table A16: Interaction Results & 
# Figure A3: Interaction Term Marginal Effects

# *PKO
pi_pko <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP +  PKO.physint, 
           family = binomial(link = "probit"), 
           data = MergedData)

pi_pko_c <- coeftest(pi_pko, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(pi_pko_c)
logLik(pi_pko_c)

pi_pko$coef
at.Physical_Integrity_Rights <- seq(-2.5582, 5.3362 , .05 )
slopes <-pi_pko$coef[5] + pi_pko$coef[9]*at.Physical_Integrity_Rights
# slopes
estmean <- coef(pi_pko)
var <-vcov(pi_pko)
SEs <- rep(NA, length(at.Physical_Integrity_Rights))
for (i in 1:length(at.Physical_Integrity_Rights)){
  j <- at.Physical_Integrity_Rights[i]
  SEs[i] <- deltamethod (~ (x5) + (x9)*j, estmean, var)
}
upper <- slopes + 1.96*SEs
lower <- slopes -1.96*SEs
cbind(at.Physical_Integrity_Rights, slopes, upper, lower)

plot(at.Physical_Integrity_Rights, slopes, type = "l", lty = 1, ylim = c(-.1, .2), xlim = c(-2.5, 5.3), xlab = "Physical Integrity Rights", ylab = "Marginal Effect of PKO") 
  points(at.Physical_Integrity_Rights, upper, type = "l", lty = 2) 
  points(at.Physical_Integrity_Rights, lower, type = "l", lty = 2) 
  points(at.Physical_Integrity_Rights, rep(0, length(at.Physical_Integrity_Rights)), type = "l", col = "gray")

# *democratic episode
pi_dem <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Autocratic_Episode + Peacekeepers + Democratic_Episode +
             Polyarchy + Population + GDP + demep.physint, 
           family = binomial(link = "probit"), 
           data = MergedData)

pi_dem_c <- coeftest(pi_dem, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(pi_dem_c)
logLik(pi_dem_c)

pi_dem$coef
at.Physical_Integrity_Rights <- seq(-2.5582, 5.3362 , .5 )
slopes <-pi_dem$coef[5] + pi_dem$coef[9]*at.Physical_Integrity_Rights
# slopes

estmean <- coef(pi_dem)
var <-vcov(pi_dem)
SEs <- rep(NA, length(at.Physical_Integrity_Rights))
for (i in 1:length(at.Physical_Integrity_Rights)){
  j <- at.Physical_Integrity_Rights[i]
  SEs[i] <- deltamethod (~ (x5) + (x9)*j, estmean, var)
}
upper <- slopes + 1.96*SEs
lower <- slopes -1.96*SEs
cbind(at.Physical_Integrity_Rights, slopes, upper, lower)

plot(at.Physical_Integrity_Rights, slopes, type = "l", lty = 1, ylim = c(-1, 1), xlim = c(-2.5, 5.3), xlab = "Physical Integrity Rights", ylab = "Marginal Effect of Democratic Episode")
points(at.Physical_Integrity_Rights, upper, type = "l", lty = 2)
points(at.Physical_Integrity_Rights, lower, type = "l", lty = 2)
points(at.Physical_Integrity_Rights, rep(0, length(at.Physical_Integrity_Rights)), type = "l", col = "gray")

# Table A16: Interaction Results
covlabs = c("Physical Integrity Rights",
"Democratic Episode", "Autocratic Episode", "Peacekeepers",
"Polyarchy", "Population", "GDP", "PKOxPIR", "PKOxDemEp")

stargazer(pi_pko_c, pi_dem_c, 
          type = "text", #comment out for latex
          title = "Interaction Results", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3534', '3534'), c('Log Likelihood', '-849.6778', '-849.7976'))
)      


# Table A17: Other Opportunity Measures
# model 1 - with TJ 
tjmod <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP + fmax.domestic.trial + fmax.truth, 
           family = binomial(link = "probit"), 
           data = MergedData)

tjmod_c <- coeftest(tjmod, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(tjmod_c)
logLik(tjmod_c)


# model 2 - with NHRI
nhrimod <- glm(Official_di_lead ~ Physical_Integrity_Rights + 
             Democratic_Episode + Autocratic_Episode + 
             Peacekeepers + 
             Polyarchy + Population + GDP + independent, 
           family = binomial(link = "probit"), 
           data = MergedData)

nhrimod_c <- coeftest(nhrimod, 
                  vcov = vcovCL, 
                  type = "HC1", 
                  cluster = ~ccode, 
                  df = 170) #the number of clusters-1

nobs(nhrimod_c)
logLik(nhrimod_c)


covlabs = c("Physical Integrity Rights",
            "Democratic Episode", "Autocratic Episode", "Peacekeepers",
            "Polyarchy", "Population", "GDP", "Trial", "Truth", "NHRI")

stargazer(tjmod_c, nhrimod_c, 
          type = "text", #comment out for latex
          title = "Other Opportunity Measures", 
          style = "default", 
          covariate.labels = covlabs, 
          dep.var.caption = NULL,
          dep.var.labels = c("Official Visit"),
          add.lines=list(c('Observations','3028', '3534'), c('Log Likelihood', '-728.5469', '-845.9704'))
)          
