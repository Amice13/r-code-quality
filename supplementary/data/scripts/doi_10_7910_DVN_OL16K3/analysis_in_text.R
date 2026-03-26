rm(list = ls())

##########################################################################################
###### Setup and Necessary Functions #####################################################
##########################################################################################

# Set working directory

# Install and load packages
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <-
  c("stargazer",
    "geosphere",
    "ggplot2",
    "dplyr",
    "sp",
    "spdep",
    "fields",
    "SpatialEpi",
    "tidyr")
ipak(pkg = packages)

# Ensure seed replicability across R versions
RNGkind(sample.kind = "Rounding")

# Function for obtaining Block Bootstrap SEs and 95 percentile CIs
block_bootstrap <- function(data, model, cluster, seed, iterations, treatment){
  
  # Get model matrix
  model_data <- model.matrix(model)
  
  # Subset original data to observations in model matrix
  new_data <- data[row.names(data) %in% row.names(model_data),]
  
  # Split data by cluster
  lookup <- split(1:nrow(new_data), new_data[cluster])
  
  # Get cluster names
  cluster_names <- names(lookup)
  
  # Set Seed
  set.seed(seed)
  
  # Store results for coefficient of interest
  coefficients <- c()
  
  # For each bootstrap iteration
  for(i in 1:iterations){
    
    # Sample clusters with replacement
    new_clusters <- sample(cluster_names, size = length(cluster_names), replace = TRUE)
    
    # Collect data with these clusters
    boot_data <- new_data[unlist(lookup[new_clusters]),]
    
    # Run regression
    formula <- gsub("()", '', model$call[2])
    boot_reg <-  lm(data = boot_data, formula)
    
    # Collect coefficient of interest
    coefficients[i] <- coef(boot_reg)[treatment]
  }
  
  # Return equal-tailed p-value from across iterations
  p <- 2*min(sum(coefficients>0), sum(coefficients<0))/iterations
  names(p) <- treatment
  
  # Return standard error
  se <- sd(coefficients)
  names(se) <- treatment
  
  # Return 95 percentile CI
  percentile_ci <- matrix(c(quantile(coefficients, 0.025), 
                            quantile(coefficients, 0.975)), ncol = 2, nrow = 1)
  row.names(percentile_ci) <- treatment
  
  results <- list(p, se, percentile_ci)
  names(results) <- c("p", "se", "percentile_ci")
  return(results)
}

# Load Data
pulwama_full <- read.csv("analysis_dataset.csv")

# Restrict to villages with a polling booth (73% of sample)
pulwama <- pulwama_full[!is.na(pulwama_full$AC),]

# Remove villages with booth-level reporting error (<1% of sample)
pulwama <- pulwama[pulwama$reporting_error==0 & 
                     !is.na(pulwama$reporting_error),]

##########################################################################################
###### In-Text Tables and Figures ########################################################
##########################################################################################

#################### Figure 1 ####################

# Load Data
bjp_support <- read.csv("Figure 1.csv")

# Reshape Data
bjp_support <- gather(bjp_support, popularity, approval, BJP:Modi, factor_key=TRUE)
bjp_support$Date <- as.Date(bjp_support$Date, "%m/%d/%y")

# Plot Figure
ggplot(data = bjp_support, aes(Date, approval, group = popularity)) +
  geom_line(aes(lty = popularity)) +
  ylim(20,80) + ylab("Net Satisfaction (%)") +
  geom_vline(xintercept = as.numeric(bjp_support$Date[58])) +
  geom_vline(xintercept = as.numeric(bjp_support$Date[46])) +
  geom_hline(yintercept = bjp_support$approval[59], lty=3, col="black") +
  theme_bw() +
  geom_label(x = as.Date("2019-02-02"), y = 60,
             label = "Pulwama") + 
  geom_label(x = as.Date("2019-03-08"), y = 70,
             label = "Balakot") +
  theme(legend.title = element_blank())

ggsave("Figure_1.png", height = 8, width = 14, units = "cm")

#################### Figure 2 ####################

# Load data
issues <- read.csv("Figure 2.csv")

# Code Factors
issues$Issue <- factor(issues$Issue, levels = 
                         rev(c("Economic Welfare", "Rising Prices",
                               "Communal Tension", 'Women Security', 
                               'Family Income/Poverty', 'Electricity/Road/Water',
                               'Unemployment','Corruption','Educational System',
                               'Terror Attacks','Total')))

# Plot Figure
ggplot(issues, aes(x=Issue, y=Preference)) + 
  geom_bar(stat = "identity") +
  xlab("Self-Reported Most Important Issue") +
  ylab("Percentage Preferring Modi Over Gandhi") +
  guides(fill=FALSE) +
  coord_flip() +scale_fill_brewer(palette="Dark2") +
  theme_bw()

ggsave("Figure_2.png", height = 8, width = 14, units = "cm")

#################### Table 1 ####################
table_1_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                    bjp_diff ~ proximity)

table_1_column_1_CIs <- block_bootstrap(pulwama, table_1_column_1,
                                      "PC", 1, 1000, "proximity")

table_1_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                 bjp_diff ~ proximity + as.factor(AC))

table_1_column_2_CIs <- block_bootstrap(pulwama, table_1_column_2,
                                        "PC", 1, 1000, "proximity")

table_1_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
              bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town + log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved + proportion_male + proportion_sc +
                proportion_literate + proportion_working + proportion_cultivators +
                proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_1_column_3_CIs <- block_bootstrap(pulwama, table_1_column_3, "PC", 1, 1000, "proximity")

table_1_column_4 <- lm(data = pulwama[pulwama$Incumbent_Party=='SP',],
             bjp_diff ~ proximity + bjp_share_14 +opp_share_14 + 
               as.factor(AC) + Town  + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)

table_1_column_4_CIs <- block_bootstrap(pulwama, table_1_column_4, "PC", 1, 1000, "proximity")

table_1_column_5 <- lm(data = pulwama, bjp_diff ~ proximity + bjp_share_14 +
                         opp_share_14 + as.factor(AC) + Town + log_towndistance100k + 
                         log_towndistance50k + log_towndistance10k + log_towndistance500k +
                         log_population + village_nonelec + village_nonpaved +
                         proportion_male + proportion_sc + proportion_literate +
                         proportion_working + proportion_cultivators + proportion_aglaborers +
                         proportion_marginalworkers + women_percent_14 +
                         muslim_percent_14 + age_avg_14)
table_1_column_5_CIs <- block_bootstrap(pulwama, table_1_column_5, "PC", 1, 1000, "proximity")

stargazer(table_1_column_1, table_1_column_2, table_1_column_3,
                  table_1_column_4, table_1_column_5,
                  ci.custom = list(table_1_column_1_CIs$percentile_ci,
                                   table_1_column_2_CIs$percentile_ci,
                                   table_1_column_3_CIs$percentile_ci,
                                   table_1_column_4_CIs$percentile_ci,
                                   table_1_column_5_CIs$percentile_ci), 
                  ci.level = 0.95, 
                  p = list(table_1_column_1_CIs$p,
                           table_1_column_2_CIs$p,
                           table_1_column_3_CIs$p,
                           table_1_column_4_CIs$p,
                           table_1_column_5_CIs$p),
                  omit=c("bjp_share_14", "AC" , "opp_share_14",
                         "Town", "log_towndistance50k", "log_towndistance10k",
                         "log_towndistance100k", "log_towndistance500k", "log_population",
                         "village_nonelec", "village_nonpaved",
                         "proportion_male", "proportion_sc", "proportion_literate", 
                         "proportion_working", "proportion_cultivators", 
                         "proportion_aglaborers", "proportion_marginalworkers",
                         "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant"),
                  ci.separator = ",",
                  omit.stat=c('adj.rsq','ser','f'),
                  dep.var.labels  = "Change in BJP Vote Share",
                  covariate.labels = "Proximity",
                  add.lines=list(c("AC Dummies", "No", "Yes","Yes", "Yes", "Yes"),
                                 c("Controls", "No", "No", "Yes", "Yes", "Yes")),
                  column.labels= c("BJP Seats", "BJP Seats", "BJP Seats", "SP Seats", "All Seats"),
                  star.cutoffs = c(.05, .01),
                  star.char = c("*", "**"),
                  notes=c("Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                          "Full list of controls provided in Appendix C. *p<0.05, **p<0.01"),
                  notes.append=F,
                  title = "Main Results",
                  out='table1.tex')

#################### Table 2 ####################

table_2_column_1 <-  lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
                        bjp_diff_09 ~ proximity)

table_2_column_1_CIs <- block_bootstrap(pulwama, table_2_column_1,
                                        "PC", 1, 1000, "proximity")

table_2_column_2 <-  lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
                        bjp_diff_09 ~ proximity + as.factor(AC))

table_2_column_2_CIs <- block_bootstrap(pulwama, table_2_column_2,
                                        "PC", 1, 1000, "proximity")

table_2_column_3 <-  lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
                        bjp_diff_09 ~ proximity + as.factor(AC) + bjp_share_09 + Town +
                          log_towndistance100k + log_towndistance50k + log_towndistance10k +
                          log_towndistance500k + log_population + village_nonelec +
                          village_nonpaved + proportion_male + proportion_sc + 
                          proportion_literate + proportion_working + proportion_cultivators +
                          proportion_aglaborers + proportion_marginalworkers + women_percent_14 +
                          muslim_percent_14 + age_avg_14)

table_2_column_3_CIs <- block_bootstrap(pulwama, table_2_column_3,
                                        "PC", 1, 1000, "proximity")

# Note: opp_share_14 omitted to preserve the same sample as the placebo '09 regressions
table_2_column_4 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' &
                                        !is.na(pulwama$bjp_diff_09),],
                       bjp_diff ~ proximity + bjp_share_14 + 
                         as.factor(AC) + Town + log_towndistance100k + log_towndistance50k +
                         log_towndistance10k + log_towndistance500k + log_population + 
                         village_nonelec + village_nonpaved + proportion_male + proportion_sc +
                         proportion_literate + proportion_working + proportion_cultivators +
                         proportion_aglaborers + proportion_marginalworkers + 
                         women_percent_14 + muslim_percent_14 + age_avg_14)
table_2_column_4_CIs <- block_bootstrap(pulwama, table_2_column_4, "PC", 1, 1000, "proximity")


stargazer(table_2_column_1, table_2_column_2, table_2_column_3,
          table_2_column_4,
          ci.custom = list(table_2_column_1_CIs$percentile_ci,
                           table_2_column_2_CIs$percentile_ci,
                           table_2_column_3_CIs$percentile_ci,
                           table_2_column_4_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_2_column_1_CIs$p,
                   table_2_column_2_CIs$p,
                   table_2_column_3_CIs$p,
                   table_2_column_4_CIs$p),
          omit=c("bjp_share_14", "AC" , "opp_share_14", "bjp_share_09",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant"),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = c("BJP Change 09-14", "BJP Change 14-19"),
          covariate.labels = "Proximity",
          add.lines=list(c("AC Dummies", "No", "Yes","Yes", "Yes"),
                         c("Controls", "No", "No", "Yes", "Yes")),
          star.cutoffs = c(.05, .01),
          star.char = c("*", "**"),
          notes=c("Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                  "Full list of controls provided in Appendix C. *p<0.05, **p<0.01"),
          notes.append=F,
          title = "2009 Placebo Test",
          out='table2.tex')

#################### Table 3 ####################
# Generate Distances to Placebo Jawans' Hometowns
pulwama <- pulwama[order(pulwama$id),]
coordinate_matrix <- as.data.frame(cbind(pulwama$long, pulwama$lat))

placebo_coordinates <- pulwama[pulwama$village_code %in% 
                            c(207367,126071,141794),c("long","lat")]

placebo_distances <- apply(placebo_coordinates, 1,
                           function(x) distHaversine(coordinate_matrix, x))

jawan_distances <- paste("jawan_distance", 1:3, sep="")
pulwama[jawan_distances] <- NA
pulwama[jawan_distances] <- placebo_distances
pulwama <- transform(pulwama, placebo_mindist = pmin(jawan_distance1,
                                               jawan_distance2,
                                               jawan_distance3)/1000)
pulwama$placebo_proximity <- log(pulwama$placebo_mindist+1) * -1

jawan_placebo <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' &
                                     pulwama$placebo_mindist<=20,],
                    bjp_diff ~ placebo_proximity + bjp_share_14 + opp_share_14 +
                      as.factor(AC) + Town  + 
                      log_towndistance100k + log_towndistance50k +
                      log_towndistance10k + log_towndistance500k + log_population + 
                      village_nonelec + village_nonpaved +
                      proportion_male + proportion_sc + proportion_literate + proportion_working +
                      proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                      women_percent_14 + muslim_percent_14 + age_avg_14)

stargazer(jawan_placebo,
                  omit=c("bjp_share_14", "AC" , "opp_share_14", "bjp_share_09",
                         "Town", "log_towndistance50k", "log_towndistance10k",
                         "log_towndistance100k", "log_towndistance500k", "log_population",
                         "village_nonelec", "village_nonpaved",
                         "proportion_male", "proportion_sc", "proportion_literate", 
                         "proportion_working", "proportion_cultivators", 
                         "proportion_aglaborers", "proportion_marginalworkers",
                         "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant"),
                  omit.stat=c('adj.rsq','ser','f'),
                  dep.var.labels  = c("Change in BJP Vote Share"),
                  covariate.labels = "Proximity",
                  add.lines=list(c("AC Dummies", "Yes"),
                                 c("Controls", "Yes")),
                  column.labels= c(""),
                  notes=c("*p<0.05, **p<0.01",
                          "Unadjusted (non-robust) SEs in parentheses.",
                          "Full list of controls provided in Appendix C."),
                  title = "Proximity to Non-Pulwama Jawan Deaths",
                  notes.append=F, out='table3.tex')

#################### Table 4 ####################

median_support <- median(pulwama$bjp_share_14, na.rm=T)

table_4_column_1 <- lm(data = pulwama[pulwama$incumbent_share_14 >= median_support &
                                pulwama$Incumbent_Party=="BJP",], 
               bjp_diff ~ proximity + bjp_share_14 +opp_share_14 + 
                 as.factor(AC) + Town  + 
                 log_towndistance100k + log_towndistance50k +
                 log_towndistance10k + log_towndistance500k + log_population + 
                 village_nonelec + village_nonpaved +
                 proportion_male + proportion_sc + proportion_literate + proportion_working +
                 proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                 women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_1_CIs <- block_bootstrap(pulwama, table_4_column_1, "PC", 1, 1000, "proximity")


table_4_column_2 <- lm(data = pulwama[pulwama$incumbent_share_14 < median_support &
                               pulwama$Incumbent_Party=="BJP",], 
              bjp_diff ~ proximity + bjp_share_14 +opp_share_14 + 
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_2_CIs <- block_bootstrap(pulwama, table_4_column_2, "PC", 1, 1000, "proximity")

table_4_column_3 <- lm(data = pulwama[pulwama$bjp_share_14 >= median_support &
                                        pulwama$Incumbent_Party=="BJP",], 
                       turnout_19 ~ proximity + tr_14 + bjp_share_14 + opp_share_14 + 
                         as.factor(AC) + Town  + 
                         log_towndistance100k + log_towndistance50k +
                         log_towndistance10k + log_towndistance500k + log_population + 
                         village_nonelec + village_nonpaved +
                         proportion_male + proportion_sc + proportion_literate + proportion_working +
                         proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                         women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_3_CIs <- block_bootstrap(pulwama, table_4_column_3, "PC", 1, 1000, "proximity")

table_4_column_4 <- lm(data = pulwama[pulwama$bjp_share_14 < median_support &
                                       pulwama$Incumbent_Party=="BJP",], 
                      turnout_19 ~ proximity + tr_14 + bjp_share_14 + opp_share_14 + 
                        as.factor(AC) + Town  + 
                        log_towndistance100k + log_towndistance50k +
                        log_towndistance10k + log_towndistance500k + log_population + 
                        village_nonelec + village_nonpaved +
                        proportion_male + proportion_sc + proportion_literate + proportion_working +
                        proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                        women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_4_CIs <- block_bootstrap(pulwama, table_4_column_4, "PC", 1, 1000, "proximity")

table_4_column_5 <- lm(data = pulwama[pulwama$bjp_share_14 >= median_support &
                                      pulwama$Incumbent_Party=="BJP",], 
                     log_votes_19 ~ proximity + log_votes_14 + log_votes_14_opp + log_turnout +
                       as.factor(AC) + Town  + 
                       log_towndistance100k + log_towndistance50k +
                       log_towndistance10k + log_towndistance500k + log_population + 
                       village_nonelec + village_nonpaved +
                       proportion_male + proportion_sc + proportion_literate + proportion_working +
                       proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                       women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_5_CIs <- block_bootstrap(pulwama, table_4_column_5, "PC", 1, 1000, "proximity")

table_4_column_6 <- lm(data = pulwama[pulwama$bjp_share_14 < median_support &
                                     pulwama$Incumbent_Party=="BJP",], 
                    log_votes_19 ~ proximity + log_votes_14 + log_votes_14_opp + log_turnout+ 
                      as.factor(AC) + Town  + 
                      log_towndistance100k + log_towndistance50k +
                      log_towndistance10k + log_towndistance500k + log_population + 
                      village_nonelec + village_nonpaved +
                      proportion_male + proportion_sc + proportion_literate + proportion_working +
                      proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                      women_percent_14 + muslim_percent_14 + age_avg_14)
table_4_column_6_CIs <- block_bootstrap(pulwama, table_4_column_6, "PC", 1, 1000, "proximity")

stargazer(table_4_column_1, table_4_column_2, table_4_column_3,
          table_4_column_4, table_4_column_5, table_4_column_6,
          ci.custom = list(table_4_column_1_CIs$percentile_ci,
                           table_4_column_2_CIs$percentile_ci,
                           table_4_column_3_CIs$percentile_ci,
                           table_4_column_4_CIs$percentile_ci,
                           table_4_column_5_CIs$percentile_ci,
                           table_4_column_6_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_4_column_1_CIs$p,
                   table_4_column_2_CIs$p,
                   table_4_column_3_CIs$p,
                   table_4_column_4_CIs$p,
                   table_4_column_5_CIs$p,
                   table_4_column_6_CIs$p),
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant",
                 "tr_14", "log_votes_14", "log_votes_14_opp", "log_turnout"),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = c("BJP Vote Share", "Turnout", "Log BJP Votes"),
          covariate.labels = "Proximity",
          add.lines=list(c("AC Dummies", "Yes", "Yes","Yes", "Yes", "Yes", "Yes"),
                         c("Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          column.labels= c("Pro BJP", "Anti BJP", "Pro BJP", "Anti BJP", "Pro BJP", "Anti BJP"),
          star.cutoffs = c(.05, .01),
          star.char = c("*", "**"),
          notes=c("Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                  "Full list of controls provided in Appendix C. *p<0.05, **p<0.01"),
          notes.append=F,
          title = "Heterogeneous Effects by Initial BJP Support",
          out='table4.tex')

#################### Table 5 ####################
# Note: opp_share_14 omitted from BSP/SP regressions as this is redundant with (b)sp_share_14

table_5_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$INC_Contests==1,],
              inc_diff ~ proximity + inc_share_14 + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_5_column_1_CIs <- block_bootstrap(pulwama, table_5_column_1, "PC", 1, 1000, "proximity")

table_5_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$BSP_Contests==1,],
              bsp_diff ~ proximity + bsp_share_14 + bjp_share_14 + 
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_5_column_2_CIs <- block_bootstrap(pulwama, table_5_column_2, "PC", 1, 1000, "proximity")

table_5_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$SP_Contests==1,],
             sp_diff ~ proximity + sp_share_14 + bjp_share_14 + 
               as.factor(AC) + Town  + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
table_5_column_3_CIs <- block_bootstrap(pulwama, table_5_column_3, "PC", 1, 1000, "proximity")

stargazer(table_5_column_1, table_5_column_2, table_5_column_3,
          ci.custom = list(table_5_column_1_CIs$percentile_ci,
                           table_5_column_2_CIs$percentile_ci,
                           table_5_column_3_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_5_column_1_CIs$p,
                   table_5_column_2_CIs$p,
                   table_5_column_3_CIs$p),
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant",
                 "bsp_share_14", "sp_share_14", "inc_share_14"),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = c("", "Change in Vote Share", ""),
          covariate.labels = "Proximity",
          add.lines=list(c("AC Dummies", "Yes", "Yes","Yes"),
                         c("Controls", "Yes", "Yes", "Yes")),
          column.labels= c("INC", "BSP", "SP"),
          star.cutoffs = c(.05, .01),
          star.char = c("*", "**"),
          notes=c("*p<0.05, **p<0.01",
                  "Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                  "Full list of controls provided in Appendix C."),
          notes.append=F,
          title = "Effects on Opposition Parties",
          out='table5.tex')

#################### Figure 4 ####################
# Select Placebo Villages, One from each Radius
radius <- paste("radius", 1:12, sep="")

distances <- paste("distance", 1:12, sep="")

pulwama[radius] <- as.numeric(pulwama[distances]<=20000)

set.seed(1)

placebo_villages <- matrix(nrow=10000, ncol=12, NA)

pulwama$row <- 1:nrow(pulwama)

for(i in 1:12){
  
  placebo_villages[,i] <- sample(pulwama$row[pulwama[radius[i]]==1 & !is.na(pulwama[radius[i]])],
                                 10000, replace=T)
}

# Get sets of placebo distances

placebo_dists <- apply(coordinate_matrix, 1, function(x) distHaversine(coordinate_matrix, x))


# Run regression for each set of placebo distances

placebo_result <- c()

for(j in 1:1000){
  
  placebo_set <- as.numeric(placebo_villages[j,])
  
  distance_set <- placebo_dists[,c(placebo_set)]
  
  p_distances <- paste("p_distance", 1:12, sep="")
  
  pulwama[p_distances] <- NA
  
  pulwama[p_distances] <- distance_set
  
  pulwama <- transform(pulwama, p_mindist = pmin(p_distance1, p_distance2, p_distance3, p_distance4,
                                                 p_distance5, p_distance6, p_distance7, p_distance8,
                                                 p_distance9, p_distance10, p_distance11, p_distance12)/1000)
  
  pulwama$placebo_prox <- log(pulwama$p_mindist+1) * -1
  
  placebo_reg <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",], bjp_diff ~ placebo_prox + as.factor(AC) +
                      bjp_share_14 + opp_share_14 + log_towndistance10k + log_towndistance100k +
                      Town  + log_towndistance50k + log_towndistance500k + log_population +
                      village_nonelec + village_nonpaved +
                      proportion_male + proportion_sc + proportion_literate + proportion_working +
                      proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                      women_percent_14 + muslim_percent_14 + age_avg_14)
  
  placebo_result[j] <- placebo_reg$coef[2]
  
}

# p -value
sum(abs(placebo_result)>abs(coef(table_1_column_3)["proximity"]))/1000

# Plot Figure
placebo_result <- as.data.frame(placebo_result)

ggplot(data = placebo_result, aes(placebo_result)) +
  geom_histogram(col='black',alpha=0, bins=100) + 
  xlab("Placebo Result") + ylab("Number of Simulations") +
  geom_vline(xintercept = coef(table_1_column_3)["proximity"], col='black') +
  geom_vline(xintercept = -coef(table_1_column_3)["proximity"], col='black') +
  theme_bw()

ggsave("Figure_4.png", height = 8, width = 14, units = "cm")

#################### Figure 5 ####################

## Morgan Kelly's estimation/SE code
## Code slightly modified to estimate spatial structure of DV, not regression residuals
## Find code at https://github.com/morganwkelly/persistence

Spatial_HAC_dv <- function(equation,
                           study_file,
                           range_search,
                           dv,
                           Smoothness = 0.5,
                           residual_upper = 1000,
                           residual_lower = -1000,
                           subset_obs = 1:nrow(study_file),
                           spatial_fit=F,
                           opt_method = "REML") {
  #
  # This is a simple function to calculate HAC standard errors using a Matern kernel as outlined in
  # Understanding Persistence. It is basically a wrapper around the Krig command in fields library.
  #
  # range_search is a sequence of values where the MLE of structure is calculated: the function chooses
  # the maximum likelihood values.
  #
  # Smoothness gives the smoothness of the Matern function, set to exponential (0.5) by default.
  #
  # residual_lower and upper allow you to set values at which to truncate outlying residuals that
  # can interfere with calculating spatial parameters.
  #
  # subset_obs allows you to choose a subset of residuals for calculating spatial params.
  #
  # spatial_fit tells whether the MLE values from Krig should be returned: useful for diagnostic plots but can be v large.
  #
  # opt_method sets the optimization method to be used by Krig function.
  
  
  fm_ols = as.formula(equation)
  
  ols = lm(fm_ols, data = study_file)
  
  Coords = study_file %>% select(X, Y) %>% as.matrix()
  if (anyDuplicated(Coords)>0) stop("Duplicated coordinates: you should omit duplicates or jitter locations.")
  
  Residuals = ols$residuals
  Residuals[-subset_obs] = NA
  
  ##Truncate high values which can mess up spatial parameter ests
  Residuals = ifelse(Residuals > residual_upper, residual_upper, Residuals)
  Residuals = ifelse(Residuals < residual_lower, residual_lower, Residuals)
  
  ###########compute moran stat for spatial autocorrelation
  study_spatial = SpatialPoints(coords = Coords)
  proj4string(study_spatial) = CRS("+proj=longlat +datum=WGS84")
  
  nearest = knn2nb(knearneigh(study_spatial, k = 5, longlat = T))   #k nearest neighbours
  nearest = nb2listw(nearest, style = "W")
  Moran_z = as.vector(lm.morantest(ols, listw = nearest)$statistic)
  
  ###scale residuals to mean=0 and sd=1 to get correlation matrix
  kappa = Smoothness
  hold_search = data.frame(range_search, lambda = NA, loglik = NA)
  
  for (j in 1:length(range_search)) {
    fit_search = Krig(
      x = Coords,
      Y = dv,
      Distance = "rdist.earth",
      Covariance = "Matern",
      smoothness = kappa,
      theta = range_search[j],
      give.warnings = FALSE,
      method = opt_method
    )
    # print(j)
    hold_search[j, 2:3] = fit_search$lambda.est[6, c(1, 5)]
  }
  
  cov_par = hold_search %>% arrange(loglik) %>% slice(1) %>% as.numeric()
  
  #If MLE range is at end of search range print warning
  
  if (cov_par[1] == max(range_search))
    warning("Your range search values are too low: try higher ones.")
  if (cov_par[1] == min(range_search))
    warning("Your range search values are too high: try lower ones.")
  
  
  ############MLE estimates of spatial parameters
  Range = as.numeric(cov_par[1])
  Effective_Range_km = 1.6 * sqrt(8 * kappa) * Range  #in kilometres: fields uses miles
  Structure = as.numeric(1 / (1 + cov_par[2]))  #this is weight rho between systematic correlation and spatial noise
  loglik = -as.numeric(cov_par[3])
  
  spatial_parameters = data.frame(Range,
                                  Effective_Range_km,
                                  Structure,
                                  Moran_z,
                                  Smoothness = kappa,
                                  loglik)
  
  #######weighting kernel is here
  KL = Structure * fields::Matern(rdist.earth(x1 = Coords),
                                  range = Range,
                                  smoothness = kappa) +
    diag(nrow(Coords)) * (1 - Structure)
  
  ##Calculate HAC standard errors
  X = qr.X(ols$qr)
  N = nrow(X)
  k = ncol(X)
  U = ols$res
  V = t(X) %*% (U * KL * U) %*% X / N
  sv_X=svd(X)   #invert X'X using SVD
  v=sv_X$v
  d_2=diag(sv_X$d^(-2))
  xx.inv = N *(v %*% d_2 %*% t(v))
  Cov = xx.inv %*% V %*% xx.inv / N
  hac.se = sqrt(diag(Cov))
  hac.t = summary(ols)$coef[, 1] / hac.se
  hac.p = 2 * (1 - pnorm(abs(hac.t)))
  hac = cbind.data.frame(coef = ols$coef, hac.se, hac.t, hac.p)
  
  ###############spatial kriging fit at MLE parameters
  fit= ifelse(spatial_fit,
              Krig(
                x = Coords,
                Y = scale(Residuals),
                Distance = "rdist.earth",
                Covariance = "Matern",
                smoothness = 0.5,
                theta = Range,
                give.warnings = FALSE
              ),
              "Krig output not returned"
  )
  
  output = list(
    HAC = hac,
    Spatial_Parameters = spatial_parameters,
    fit = fit,
    OLS = ols,
    Residuals = Residuals
  )
  return(output)
}

###### Estimate Spatial Structure of Standardized DV

# Remove observations with missingness
no_nas <- pulwama[row.names(pulwama) %in%
                    row.names(model.matrix(table_1_column_3)),]

# label x,y coordinates
no_nas$X <- no_nas$long
no_nas$Y <- no_nas$lat

# Standardize DV
no_nas$bjp_norm <- (no_nas$bjp_diff - mean(no_nas$bjp_diff)) / 
  sd(no_nas$bjp_diff)

# Run regression
equation_check <- "bjp_norm ~ proximity +
bjp_share_14 + opp_share_14 + as.factor(AC) + Town + 
log_towndistance100k + log_towndistance50k +
log_towndistance10k + log_towndistance500k + log_population + 
village_nonelec + village_nonpaved +
proportion_male + proportion_sc + proportion_literate + proportion_working +
proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
women_percent_14 + muslim_percent_14 + age_avg_14"

normalized_reg <- lm(data = no_nas, equation_check)

# Estimate DV Spatial Correlation
# Setting range from 12.2 to 12.4 because the solution is 12.3
# Can expand this range and obtain the same result, will run longer
hs_check=Spatial_HAC_dv(equation=equation_check,
                        study_file=no_nas,
                        dv=no_nas$bjp_norm,
                        range_search = seq(from=12.2, 12.4, by=0.1),
                        Smoothness = 0.5)

# Obtain MLE parameters
round(hs_check$Spatial_Parameters,1)

###### Construct 1,000 Simulated DVs
# NOTE: This is a computationally intensive procedure and can take hours.
# Load in the "spatial_simulation_DVs" data below to skip this procedure.
#############################################
#long <- no_nas$long
#lat <- no_nas$lat

#grid <- latlong2grid(cbind(long, lat))
#obj1 <- matern.image.cov(grid=grid, theta = 12.3, smoothness = 0.5, setup=T)
#set.seed(1)

#start <- Sys.time()
#sim <- replicate(n = 1000, diag(sim.rf(obj1)))
#end <- Sys.time()

#write.csv(sim, "spatial_simulation_DVs.csv")
#############################################

# Load in 1,000 Simulated DVs
sim <- read.csv("spatial_simulation_DVs.csv")

# Estimate 1,000 simulated effects
result <- c()

for(i in 1:1000){
  
  no_nas$sim <- sim[,i]
  
  simulated_reg <- lm(data = no_nas,
             sim ~ proximity + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  
  result[i] <- coef(simulated_reg)["proximity"]
  
}

# p-value
sum(abs(result)>abs(coef(normalized_reg)["proximity"]))/1000

# Plot Figure
sim_results <- as.data.frame(result)

ggplot(data = sim_results, aes(result)) +
  geom_histogram(col='black',alpha=0, bins=100) + 
  xlab("Placebo Result") +
  ylab("Number of Simulations") +
  geom_vline(xintercept = coef(normalized_reg)["proximity"], col='black') +
  geom_vline(xintercept = -coef(normalized_reg)["proximity"], col='black') +
  theme_bw()

ggsave("Figure_5.png", height = 8, width = 14, units = "cm")
