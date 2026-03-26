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
    "ggplot2",
    "dplyr",
    "mgcv",
    "spdep",
    "multiwayvcov",
    "lmtest",
    "geosphere")
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
###### Supplementary Tables and Figures ##################################################
##########################################################################################

#################### Table A.1 ####################

covs <- c("bjp_share_14", "opp_share_14",
          "women_percent_14", "muslim_percent_14", "age_avg_14",
          "Town", "log_towndistance50k", "log_towndistance10k",
          "log_towndistance100k", "log_towndistance500k", "log_population",
          "village_nonelec", "village_nonpaved",
          "proportion_male", "proportion_sc", "proportion_literate", 
          "proportion_working", "proportion_cultivators", 
          "proportion_aglaborers", "proportion_marginalworkers")

stargazer(pulwama[covs],
          out='tableA1.tex')

#################### Table A.5 ####################

pulwama$attended <- as.numeric(pulwama$radius3==1 |
                               pulwama$radius5==1 | 
                               pulwama$radius6==1 |
                               pulwama$radius7==1)

pulwama$unnao <- as.numeric(pulwama$radius6==1)

table_A5_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
                        bjp_diff ~ proximity +
                          bjp_share_14 +opp_share_14 +
                          as.factor(AC) + Town  + 
                          log_towndistance100k + log_towndistance50k +
                          log_towndistance10k + log_towndistance500k + log_population + 
                          village_nonelec + village_nonpaved +
                          proportion_male + proportion_sc + proportion_literate + proportion_working +
                          proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                          women_percent_14 + muslim_percent_14 + age_avg_14)

table_A5_column_1_CIs <- block_bootstrap(pulwama, table_A5_column_1,
                                         "PC", 1, 1000, "proximity")

table_A5_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP" &
                                         pulwama$attended==0,],
                 bjp_diff ~ proximity +
                   bjp_share_14 +opp_share_14 +
                   as.factor(AC) + Town  + 
                   log_towndistance100k + log_towndistance50k +
                   log_towndistance10k + log_towndistance500k + log_population + 
                   village_nonelec + village_nonpaved +
                   proportion_male + proportion_sc + proportion_literate + proportion_working +
                   proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                   women_percent_14 + muslim_percent_14 + age_avg_14)

table_A5_column_2_CIs <- block_bootstrap(pulwama, table_A5_column_2,
                                         "PC", 1, 1000, "proximity")

table_A5_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP" &
                                         pulwama$unnao==0,],
                bjp_diff ~ proximity +
                  bjp_share_14 +opp_share_14 +
                  as.factor(AC) + Town  + 
                  log_towndistance100k + log_towndistance50k +
                  log_towndistance10k + log_towndistance500k + log_population + 
                  village_nonelec + village_nonpaved +
                  proportion_male + proportion_sc + proportion_literate + proportion_working +
                  proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                  women_percent_14 + muslim_percent_14 + age_avg_14)

table_A5_column_3_CIs <- block_bootstrap(pulwama, table_A5_column_3,
                                         "PC", 1, 1000, "proximity")

stargazer(table_A5_column_1, table_A5_column_2, table_A5_column_3,
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant"),
          ci.custom = list(table_A5_column_1_CIs$percentile_ci,
                           table_A5_column_2_CIs$percentile_ci,
                           table_A5_column_3_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_A5_column_1_CIs$p,
                   table_A5_column_2_CIs$p,
                   table_A5_column_3_CIs$p),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          column.labels= c("Full Sample", "Excluding Most-Attended", "Excluding Unnao"),
          add.lines=list(c("AC Dummies", "Yes", "Yes", "Yes"),
                         c("Controls","Yes", "Yes", "Yes")),
          notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
          title = "Heterogeneity by BJP Attendance",
          out='tableA5.tex')

#################### Table A.6 ####################

table_A6_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
         bjp_diff ~ proximity + as.factor(PC))

table_A6_column_1_CIs <- block_bootstrap(pulwama, table_A6_column_1,
                                         "PC", 1, 1000, "proximity")

table_A6_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                        bjp_diff ~ proximity + as.factor(DISTRICT))

table_A6_column_2_CIs <- block_bootstrap(pulwama, table_A6_column_2,
                                         "PC", 1, 1000, "proximity")

# (Note: Radius 8 excluded because there are no BJP Incumbents)
table_A6_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                        bjp_diff ~ proximity + radius1 + radius2 + radius3 +
                          radius4 + radius5 + radius6 + radius7 +
                          radius9 + radius10 + radius11 + radius12)

table_A6_column_3_CIs <- block_bootstrap(pulwama, table_A6_column_3,
                                         "PC", 1, 1000, "proximity")

stargazer(table_A6_column_1, table_A6_column_2, table_A6_column_3, 
          ci.custom = list(table_A6_column_1_CIs$percentile_ci,
                           table_A6_column_2_CIs$percentile_ci,
                           table_A6_column_3_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_A6_column_1_CIs$p,
                   table_A6_column_2_CIs$p,
                   table_A6_column_3_CIs$p),
          omit=c("PC", "DISTRICT", "radius1", "radius2", "radius3",
                 "radius4", "radius5", "radius6", "radius7",
                 "radius9", "radius10", "radius11", "radius12", "Constant"),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          add.lines=list(c("PC Dummies", "Yes", "No","No"),
                         c("District Dummies", "No", "Yes","No"),
                         c("Site Dummies", "No", "No","Yes"),
                         c("Controls", "No", "No", "No")),
          column.labels= c("BJP Seats", "BJP Seats", "BJP Seats", "SP Seats", "All Seats"),
          notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
          title = "Additional Minimalist Specifications",
          out='tableA6.tex')

#################### Figure A.1 ####################

no_nas <- pulwama[row.names(pulwama) %in% row.names(model.matrix(table_A5_column_1)),]

AC_aggregation <- no_nas %>%
  group_by(AC) %>%
  summarise(ac_mean = mean(bjp_diff))

# Get S.D. of AC-level mean vote change
sd(AC_aggregation$ac_mean)

# Simulate bivariate effects with observed AC-level variation

AC_aggregation$sim_effect <- NA

set.seed(1)

result <- c()

# effects taken from Table 1, Columns 1 and 3

observed_effect <- - 0.03

bivariate_effect <- - 0.015

for(i in 1:1000){
  
  AC_aggregation$sim_effect <- rnorm(nrow(AC_aggregation),
                                     sd = sd(AC_aggregation$ac_mean))
  
  no_nas <- merge(no_nas, AC_aggregation[,c("AC", "sim_effect")],
                 by="AC", all.x=T)
  
  no_nas$sim_dv <- mean(no_nas$bjp_diff) + 
    no_nas$sim_effect + observed_effect*no_nas$proximity
  
  no_nas$sim_effect <- NULL
  
  result[i] <- coef(lm(data = no_nas, sim_dv ~ proximity))[2]

  }

result <- as.data.frame(result)

ggplot(data = result, aes(result)) +
  geom_histogram(col='black',alpha=0, bins=100) + 
  xlab("Placebo Result") +
  ylab("Number of Simulations") +
  geom_vline(xintercept = observed_effect + bivariate_effect, col='red') +
  geom_vline(xintercept = observed_effect - bivariate_effect, col='red') +
  geom_vline(xintercept = observed_effect, col='blue') +
  theme_bw()

ggsave("Figure_A1.png", height = 8, width = 14, units = "cm")

table(result >= (observed_effect - bivariate_effect) | 
        result <= (observed_effect + bivariate_effect))

#################### Figure A.2 ####################

gam_bjp <- gam(data = pulwama[pulwama$Incumbent_Party=='BJP',],
               bjp_diff ~ s(mindist) + bjp_share_14 +opp_share_14 + 
                 as.factor(AC) + Town + log_towndistance100k + log_towndistance50k +
                 log_towndistance10k + log_towndistance500k + log_population + 
                 village_nonelec + village_nonpaved + proportion_male + 
                 proportion_sc + proportion_literate + proportion_working +
                 proportion_cultivators + proportion_aglaborers +
                 proportion_marginalworkers + women_percent_14 + age_avg_14)
png("Figure_A2.png", width = 3600, height=2400, res = 600)
plot(gam_bjp, xlab="Distance to Home Village",ylab="f(Distance)")
dev.off() 

#################### Table A.7 ####################

pulwama$inverse_distance <- 1/(pulwama$mindist+1)

table_A7_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
              bjp_diff ~ inverse_distance + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)

table_A7_column_1_CIs <- block_bootstrap(pulwama, table_A7_column_1, 
                                         "PC", 1, 1000, "inverse_distance")

pulwama$neg_distance <- -1 * pulwama$mindist

table_A7_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                        bjp_diff ~ neg_distance + bjp_share_14 + opp_share_14 + 
                          as.factor(AC) + Town + 
                          log_towndistance100k + log_towndistance50k +
                          log_towndistance10k + log_towndistance500k + log_population + 
                          village_nonelec + village_nonpaved +
                          proportion_male + proportion_sc + proportion_literate + proportion_working +
                          proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                          women_percent_14 + muslim_percent_14 + age_avg_14)

table_A7_column_2_CIs <- block_bootstrap(pulwama, table_A7_column_2, 
                                         "PC", 1, 1000, "neg_distance")

# make table
  stargazer(table_A7_column_1, table_A7_column_2,
                   ci.custom = list(table_A7_column_1_CIs$percentile_ci,
                                    table_A7_column_2_CIs$percentile_ci), 
                   ci.level = 0.95, 
                   p = list(table_A7_column_1_CIs$p,
                            table_A7_column_2_CIs$p),
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
                   dep.var.labels  = c("BJP Change 14-19"),
                   covariate.labels = c("Inverse Distance", "Negative Linear Distance"),
                   add.lines=list(c("State Constituency Dummies", "Yes", "Yes"),
                                  c("Electoral + Sociodemographic Controls", "Yes", "Yes")),
                   column.labels= c(""),
                   notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                   title="Alternate Functional Form Specifications",
                   out='tableA7.tex')

#################### Figure A.3 ####################

  # Run regression at each binary cutoff
  
  pulwama$close <- as.numeric(pulwama$mindist<= 5)
  
  km5 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
            bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)
  
  km5_CIs <- block_bootstrap(pulwama, km5, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 6)
  km6 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
            bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)
  km6_CIs <- block_bootstrap(pulwama, km6, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 7)
  km7 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
            bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)
  km7_CIs <- block_bootstrap(pulwama, km7, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 8)
  km8 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
            bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)
  km8_CIs <- block_bootstrap(pulwama, km8, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 9)
  km9 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
            bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)
  km9_CIs <- block_bootstrap(pulwama, km9, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 10)
  km10 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km10_CIs <- block_bootstrap(pulwama, km10, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 11)
  km11 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km11_CIs <- block_bootstrap(pulwama, km11, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 12)
  km12 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km12_CIs <- block_bootstrap(pulwama, km12, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 13)
  km13 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km13_CIs <- block_bootstrap(pulwama, km13, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 14)
  km14 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km14_CIs <- block_bootstrap(pulwama, km14, "PC", 1, 1000, "close")
  
  pulwama$close <- as.numeric(pulwama$mindist<= 15)
  km15 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",],
             bjp_diff ~ close + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
  km15_CIs <- block_bootstrap(pulwama, km15, "PC", 1, 1000, "close")
  
  # Plot Results
  
  ci <-  matrix(NA, ncol = 2, nrow = 11)
  
  ci <- rbind(km5_CIs$percentile_ci,
              km6_CIs$percentile_ci,
              km7_CIs$percentile_ci,
              km8_CIs$percentile_ci,
              km9_CIs$percentile_ci,
              km10_CIs$percentile_ci,
              km11_CIs$percentile_ci,
              km12_CIs$percentile_ci,
              km13_CIs$percentile_ci,
              km14_CIs$percentile_ci,
              km15_CIs$percentile_ci)
  
  coefs <- c(coef(km5)["close"],
             coef(km6)["close"],
             coef(km7)["close"],
             coef(km8)["close"],
             coef(km9)["close"],
             coef(km10)["close"],
             coef(km11)["close"],
             coef(km12)["close"],
             coef(km13)["close"],
             coef(km14)["close"],
             coef(km15)["close"])
  
  bandwidth_data <- as.data.frame(cbind(c(5:15),
                                        ci,coefs))
  
  colnames(bandwidth_data) <- c("Model", "lower", "upper", "coef")
  
  bandwidth_data$lower <- as.numeric(as.character(bandwidth_data$lower))
  
  bandwidth_data$upper <- as.numeric(as.character(bandwidth_data$upper))
  
  bandwidth_data$coef <- as.numeric(as.character(bandwidth_data$coef))
  
  ggplot(bandwidth_data, aes(x=as.factor(Model), y=coef)) + 
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                  position=position_dodge(.9)) +
    ylim(-0.05,0.01) + ylab("Effect of Proximity (Binary Cutoff)") +
    geom_hline(yintercept = 0, lty=2, col='red') +
    xlab("Bandwidth (km)")
  
  ggsave("Figure_A3.png", height = 8, width = 14, units = "cm")
  
#################### Table A.8 ####################
  
table_A8_column_1_CIs <- block_bootstrap(pulwama, table_A5_column_1,
                             "AC", 1, 1000, "proximity")
  
table_A8_column_2_CIs <- block_bootstrap(pulwama, table_A5_column_1,
                             "SUB_DIST", 1, 1000, "proximity")

table_A8_column_3_CIs <- block_bootstrap(pulwama, table_A5_column_1,
                             "DISTRICT", 1, 1000, "proximity")  

# Get Closest Home Village
pulwama$radius <- NA

distances <- paste("distance", 1:12, sep="")

  for (i in 1:nrow(pulwama)){
    
    pulwama$radius[i] <- names(which.min(pulwama[i,distances]))
    
  }

table_A8_column_4_CIs <- block_bootstrap(pulwama, table_A5_column_1,
                                         "radius", 1, 1000, "proximity")    

stargazer(table_A5_column_1, table_A5_column_1, 
          table_A5_column_1, table_A5_column_1,
          ci.custom = list(table_A8_column_1_CIs$percentile_ci,
                          table_A8_column_2_CIs$percentile_ci,
                          table_A8_column_3_CIs$percentile_ci,
                          table_A8_column_4_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_A8_column_1_CIs$p,
                      table_A8_column_2_CIs$p,
                      table_A8_column_3_CIs$p,
                      table_A8_column_4_CIs$p),
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
          dep.var.labels  = c("BJP Change 14-19"),
          covariate.labels = c("Log Distance"),
          add.lines=list(c("State Constituency Dummies", "Yes", "Yes", "Yes", "Yes"),
                        c("Electoral + Sociodemographic Controls", "Yes", "Yes", "Yes", "Yes")),
          column.labels= c(""),
          notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
          title="Alternate Cluster Bootstrap Confidence Intervals", out='tableA8.tex')
  
#################### Figure A.4 ####################

# Conley function taken from https://static1.squarespace.com/static/558eff8ce4b023b6b855320a/
# t/573bd63745bf21da74c080a8/1463539276997/ARE_212_Section_10.pdf
# tol = 1e-26 added to code

olsConley <- function(data, y, X, lat, lon, cutoff) {
  n <- nrow(data)
  ydata <- as.vector(y)
  xdata <- as.matrix(cbind(1,X))
  k <- ncol(xdata)
  betahat <- solve(t(xdata) %*% xdata, tol = 1e-26) %*% t(xdata) %*% ydata
  e <- ydata - xdata %*% betahat
  # grab latitude & longitude
  latdata <- as.vector(lat)
  londata <- as.vector(lon)
  # loop over all of the spatial units (aka observations)
  meatWeight <- lapply(1:n, function(i) {
    # turn longitude & latitude into KMs. 1 deg lat = 111 km, 1 deg lon = 111 km* cos(lat)
    lonscale <- cos(latdata[i]*pi / 180) * 111
    latscale <- 111
    # distance --> use pythagorean theorem! who knew that was useful?
    dist <- as.numeric(sqrt((latscale*(latdata[i] - latdata))^2
                            + (lonscale*(londata[i] - londata))^2))
    # set a window var = 1 iff observation j is within cutoff dist of obs i
    window <- as.numeric(dist <= cutoff)
    # this next part is where the magic happens. this thing makes:
    # sum_j(X_iX_j'e_ie_j K(d_{ij})), and we make n of them - one for each i.
    # double transpose here is because R is bad at dealing with 1 x something stuff.
    # we want x_i'; this is an easy way to get it. Now for some dimensions
    # (we want k x k at the end):
    XeeXh <- ((t(t(xdata[i, ])) %*% matrix(1, 1, n) * e[i]) *
                # k x 1 1 x n 1 x 1
                (matrix(1, k, 1) %*% (t(e) * t(window)))) %*% xdata
    # k x 1 1 x n n x k
    return(XeeXh)
  })
  # phew! Now let's make our sandwich. First, the meat = sum_i what we just made
  meat <- (Reduce("+", meatWeight)) / n
  # and the usual bread
  bread <- solve(t(xdata) %*% xdata, tol = 1e-26)
  # mmmm, delicious sandwich
  sandwich <- n* (t(bread) %*% meat %*% bread)
  # se as per usual
  se <- sqrt(diag(sandwich))
  output <- list(betahat, se)
  names(output) <- c("betahat", "conleySE")
  output
  return(output)
}

# Apply Conley function

model <- model.matrix(table_A5_column_1)

ci <- matrix(NA, ncol = 2, nrow = 10)

for(i in 1:10){
  
  ci[i,] <- coef(table_A5_column_1)[2] + c(-1,1) * 
    olsConley(model, pulwama$bjp_diff[row.names(pulwama) %in% row.names(model)],
              model[,2:60],
              pulwama$lat[row.names(pulwama) %in% row.names(model)],
              pulwama$long[row.names(pulwama) %in% row.names(model)], 
              i)$conleySE[2]*1.96

  }

# Plot Conley SE Results

conley_data <- as.data.frame(cbind(c(1:10), ci, coef(table_A5_column_1)[2]))

colnames(conley_data) <- c("Radius (km)", "lower", "upper", "coef")

conley_data$`Radius (km)` <- as.factor(conley_data$`Radius (km)`)

ggplot(conley_data, aes(x=`Radius (km)`, y=coef)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  ylim(-0.05,0) + ylab("Effect of Proximity") +
  geom_hline(yintercept = 0, lty=2, col='red')

ggsave("Figure_A4.png", height = 8, width = 14, units = "cm")

#################### Wild Bootstrap t-test ####################

# Wild bootstrap function with an equal-tail p-value, using Rademacher weights

wild_p <- function(dv, sample_var, seed, base_formula){
  
  # Subset data as appropriate
  data <- pulwama[which(sample_var==T),]
  
  # Run regression
  formula <- paste(dv, "~", "lndist +", base_formula, sep=" ")
  
  base_regression <-  lm(data = data, formula)
  
  # Get vcov matrix
  cov <- cluster.vcov(base_regression,data$PC)
  
  # Get original t-value
  base_wald <- coeftest(base_regression,cov)[2,3]
  
  model <- model.matrix(base_regression)
  
  # Get dataset and DV without NAs
  no_nas <- data[row.names(data) %in% row.names(model),]
  
  no_nas$new_dv <- unlist(no_nas[dv])
  
  # Run restricted regression (without treatment variable)
  restricted_formula <- paste("new_dv ~", base_formula, sep=" ")
  
  restricted_regression <-  lm(data = no_nas, restricted_formula)
  
  # Generate predictions, residuals
  predictions <- predict(restricted_regression)
  
  residuals <- resid(restricted_regression)
  
  # Generate clusters by PC
  lookup <- split(1:nrow(no_nas), no_nas$PC)
  
  gnames <- names(lookup)
  
  group <- no_nas$PC
  
  clusters <- sapply(group, function(x) which(gnames==x))
  
  # Write formula for simulated regressions (with resampled residuals)
  sim_formula <- paste("sim_dv ~", "lndist + ", base_formula, sep=" ")
  
  t <- c()
  
  set.seed(seed)
  
  # Run 1,000 regressions with residual sign resampling
  for(i in 1:1000){
    
    # Resample sign of residuals by cluster
    signs <- sample(c(-1,1),length(gnames), replace=T)
    
    altered_residuals <- residuals * signs[clusters]
    
    # Generate simulated DVs by adding new resiudals to fitted values
    no_nas$sim_dv <- predictions + altered_residuals
    
    # Run simulated regression
    sim_reg <- lm(data = no_nas, sim_formula)
    
    # Get new t-value
    cov <- cluster.vcov(sim_reg,no_nas$PC)
    
    t[i] <- (coeftest(sim_reg,cov)[2,1])/coeftest(sim_reg,cov)[2,2]
    
  }
  
  # Get p-value based on how many of the simulated t-values exceed the observed one
  p <- 2*min(sum(round(t,digits=6)<base_wald), sum(round(t,digits=6)>base_wald))/1000
  
  return(p)
  
}

form <- "bjp_share_14 + opp_share_14 + 
  as.factor(AC) + Town + 
  log_towndistance100k + log_towndistance50k +
  log_towndistance10k + log_towndistance500k + log_population + 
  village_nonelec + village_nonpaved +
  proportion_male + proportion_sc + proportion_literate + proportion_working +
  proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
  women_percent_14 + muslim_percent_14 + age_avg_14"

wild_p("bjp_diff", pulwama$Incumbent_Party=="BJP", 1, form)

#################### Table A.9 ####################

table_A9_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='SP',],
         sp_diff ~ proximity + bjp_share_14 + sp_share_14 + 
           as.factor(AC) + Town  + 
           log_towndistance100k + log_towndistance50k +
           log_towndistance10k + log_towndistance500k + log_population + 
           village_nonelec + village_nonpaved +
           proportion_male + proportion_sc + proportion_literate + proportion_working +
           proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
           women_percent_14 + muslim_percent_14 + age_avg_14)

table_A9_column_1_CIs <- block_bootstrap(pulwama, table_A9_column_1, "PC", 1, 1000, "proximity")

table_A9_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' | 
                                         pulwama$Incumbent_Party=='AD',],
          nda_diff ~ proximity + nda_share_14 + opp_share_14 + 
            as.factor(AC) + Town  + 
            log_towndistance100k + log_towndistance50k +
            log_towndistance10k + log_towndistance500k + log_population + 
            village_nonelec + village_nonpaved +
            proportion_male + proportion_sc + proportion_literate + proportion_working +
            proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
            women_percent_14 + muslim_percent_14 + age_avg_14)

table_A9_column_2_CIs <- block_bootstrap(pulwama, table_A9_column_2, "PC", 1, 1000, "proximity")

# Including reporting errors
full_data <- pulwama_full[!is.na(pulwama_full$AC),]
full_data$proximity <- full_data$lndist * -1

table_A9_column_3 <- lm(data = full_data[full_data$Incumbent_Party=='BJP',],
           bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
             as.factor(AC) + Town + 
             log_towndistance100k + log_towndistance50k +
             log_towndistance10k + log_towndistance500k + log_population + 
             village_nonelec + village_nonpaved +
             proportion_male + proportion_sc + proportion_literate + proportion_working +
             proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
             women_percent_14 + muslim_percent_14 + age_avg_14)

table_A9_column_3_CIs <- block_bootstrap(full_data, table_A9_column_3, "PC", 1, 1000, "proximity")

table_A9_column_4 <- lm(data = pulwama,
             incumbent_diff ~ proximity + incumbent_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)

table_A9_column_4_CIs <- block_bootstrap(pulwama, table_A9_column_4, "PC", 1, 1000, "proximity")

stargazer(table_A9_column_1, table_A9_column_2, table_A9_column_3, table_A9_column_4,
                   ci.custom = list(table_A9_column_1_CIs$percentile_ci,
                                    table_A9_column_2_CIs$percentile_ci,
                                    table_A9_column_3_CIs$percentile_ci,
                                    table_A9_column_4_CIs$percentile_ci), 
                   ci.level = 0.95, 
                   p = list(table_A9_column_1_CIs$p,
                            table_A9_column_2_CIs$p,
                            table_A9_column_3_CIs$p,
                            table_A9_column_4_CIs$p),
                   omit=c("bjp_share_14", "AC" , "opp_share_14",
                          "Town", "log_towndistance50k", "log_towndistance10k",
                          "log_towndistance100k", "log_towndistance500k", "log_population",
                          "village_nonelec", "village_nonpaved",
                          "proportion_male", "proportion_sc", "proportion_literate", 
                          "proportion_working", "proportion_cultivators", 
                          "proportion_aglaborers", "proportion_marginalworkers",
                          "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant",
                          "sp_share_14", "nda_share_14", "incumbent_share_14"),
                   ci.separator = ",",
                   omit.stat=c('adj.rsq','ser','f'),
                   dep.var.labels  = c("SP Change", "NDA Change", "BJP Change", "Incumb. Change"),
                   covariate.labels = c("Proximity"),
                   add.lines=list(c("State Constituency Dummies", "Yes", "Yes", "Yes", "Yes"),
                                  c("Controls", "Yes", "Yes", "Yes", "Yes")),
                   column.labels= c(""),
                   notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                   title="Additional Electoral Results",
                   out='tableA9.tex')

#################### Table A.10 ####################

table_A10_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",], 
              turnout_19 ~ proximity + tr_14 + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_A10_column_1_CIs <- block_bootstrap(pulwama, table_A10_column_1, "PC", 1, 1000, "proximity")

table_A10_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=="BJP",], 
                log_votes_19 ~ proximity + log_votes_14 + log_votes_14_opp + log_turnout +
                  as.factor(AC) + Town  + 
                  log_towndistance100k + log_towndistance50k +
                  log_towndistance10k + log_towndistance500k + log_population + 
                  village_nonelec + village_nonpaved +
                  proportion_male + proportion_sc + proportion_literate + proportion_working +
                  proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                  women_percent_14 + muslim_percent_14 + age_avg_14)
table_A10_column_2_CIs <- block_bootstrap(pulwama, table_A10_column_2, "PC", 1, 1000, "proximity")

stargazer(table_A10_column_1, table_A10_column_2,
                   ci.custom = list(table_A10_column_1_CIs$percentile_ci,
                                    table_A10_column_2_CIs$percentile_ci), 
                   ci.level = 0.95, 
                   p = list(table_A10_column_1_CIs$p,
                            table_A10_column_2_CIs$p),
                   omit=c("bjp_share_14", "AC" , "opp_share_14",
                          "Town", "log_towndistance50k", "log_towndistance10k",
                          "log_towndistance100k", "log_towndistance500k", "log_population",
                          "village_nonelec", "village_nonpaved",
                          "proportion_male", "proportion_sc", "proportion_literate", 
                          "proportion_working", "proportion_cultivators", 
                          "proportion_aglaborers", "proportion_marginalworkers",
                          "women_percent_14", "muslim_percent_14", "age_avg_14", "Constant",
                          "tr_14", "log_votes_14", "log_turnout", "log_votes_14_opp"),
                   ci.separator = ",",
                   omit.stat=c('adj.rsq','ser','f'),
                   dep.var.labels  = c("Change in Turnout 14-19", "Change in Log BJP Votes, 14-19"),
                   covariate.labels = c("Proximity"),
                   add.lines=list(c("State Constituency Dummies", "Yes", "Yes"),
                                  c("Electoral + Sociodemographic Controls", "Yes", "Yes")),
                   column.labels= c(""),
                   notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                   title="Additional Electoral Results",
                   out='tableA10.tex')

#################### Table A.11 ####################

pulwama$early_phase <- pulwama$Phase<=4

table_A11_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$early_phase==1,],
            bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)

table_A11_column_1_CIs <- block_bootstrap(pulwama, table_A11_column_1,
                                          "PC", 1, 1000, "proximity")

table_A11_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$early_phase==0,],
           bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
             as.factor(AC) + Town + 
             log_towndistance100k + log_towndistance50k +
             log_towndistance10k + log_towndistance500k + log_population + 
             village_nonelec + village_nonpaved +
             proportion_male + proportion_sc + proportion_literate + proportion_working +
             proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
             women_percent_14 + muslim_percent_14 + age_avg_14)

table_A11_column_2_CIs <- block_bootstrap(pulwama, table_A11_column_2,
                                          "PC", 1, 1000, "proximity")

table_A11_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$Incumbent_Reruns==1,],
            bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)

table_A11_column_3_CIs <- block_bootstrap(pulwama, table_A11_column_3, "PC", 1, 1000, "proximity")

table_A11_column_4 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP' & pulwama$Incumbent_Reruns==0,],
                bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                  as.factor(AC) + Town + 
                  log_towndistance100k + log_towndistance50k +
                  log_towndistance10k + log_towndistance500k + log_population + 
                  village_nonelec + village_nonpaved +
                  proportion_male + proportion_sc + proportion_literate + proportion_working +
                  proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                  women_percent_14 + muslim_percent_14 + age_avg_14)

table_A11_column_4_CIs <- block_bootstrap(pulwama, table_A11_column_4,
                                  "PC", 1, 1000, "proximity")

stargazer(table_A11_column_1, table_A11_column_2, table_A11_column_3, table_A11_column_4,
                   ci.custom = list(table_A11_column_1_CIs$percentile_ci,
                                    table_A11_column_2_CIs$percentile_ci,
                                    table_A11_column_3_CIs$percentile_ci,
                                    table_A11_column_4_CIs$percentile_ci), 
                   ci.level = 0.95, 
                   p = list(table_A11_column_1_CIs$p,
                            table_A11_column_2_CIs$p, 
                            table_A11_column_3_CIs$p,
                            table_A11_column_4_CIs$p),
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
                   dep.var.labels  = c("Change in BJP Share 14-19"),
                   covariate.labels = c("Proximity"),
                   add.lines=list(c("State Constituency Dummies", "Yes", "Yes", "Yes", "Yes"),
                                  c("Controls", "Yes", "Yes", "Yes", "Yes")),
                   column.labels= c(""),
                   notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                   title="Heterogeneous Effects",
                   out='tableA11.tex')

#################### Table A.12 ####################

table_A12_column_1 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
            bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + log_nightlight_intensity + 
              as.factor(AC) + Town + 
              log_towndistance100k + log_towndistance50k +
              log_towndistance10k + log_towndistance500k + log_population + 
              village_nonelec + village_nonpaved +
              proportion_male + proportion_sc + proportion_literate + proportion_working +
              proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
              women_percent_14 + muslim_percent_14 + age_avg_14)

table_A12_column_1_CIs <- block_bootstrap(pulwama, table_A12_column_1, "PC", 1, 1000, "proximity")

table_A12_column_2 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                         bjp_diff ~ proximity  + 
                           as.factor(AC) + Town + 
                           log_towndistance100k + log_towndistance50k +
                           log_towndistance10k + log_towndistance500k + log_population + 
                           village_nonelec + village_nonpaved +
                           proportion_male + proportion_sc + proportion_literate + proportion_working +
                           proportion_cultivators + proportion_aglaborers + proportion_marginalworkers)

table_A12_column_2_CIs <- block_bootstrap(pulwama, table_A12_column_2, "PC", 1, 1000, "proximity")

table_A12_column_3 <- lm(data = pulwama[pulwama$Incumbent_Party=='BJP',],
                bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                  as.factor(AC) + women_percent_14 + muslim_percent_14 + age_avg_14)

table_A12_column_3_CIs <- block_bootstrap(pulwama, table_A12_column_3, "PC", 1, 1000, "proximity")

stargazer(table_A12_column_1, table_A12_column_2, table_A12_column_3,
                   ci.custom = list(table_A12_column_1_CIs$percentile_ci,
                                    table_A12_column_2_CIs$percentile_ci,
                                    table_A12_column_3_CIs$percentile_ci), 
                   ci.level = 0.95, 
                   p = list(table_A12_column_1_CIs$p,
                            table_A12_column_2_CIs$p,
                            table_A12_column_3_CIs$p),
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
                   dep.var.labels  = c("BJP Change 14-19"),
                   covariate.labels = c("Proximity", "Log Nightlight Intensity"),
                   add.lines=list(c("State Constituency Dummies", "Yes", "Yes", "Yes"),
                                  c("Booth-Level Controls", "Yes", "No", "Yes"),
                                  c("Village-Level Controls", "Yes", "Yes", "No")),
                   column.labels= c(""),
                   notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
                   title="Alternate Covariate Specifications",
                   out='tableA12.tex')

#################### Figure A.5 ####################

# Load in Spatial Simulations of the DV--see in-text replication file for code to make this
sim <- read.csv("spatial_simulation_DVs.csv")

# Get bivariate effect on standardized DV

no_nas <- pulwama[row.names(pulwama) %in% row.names(model.matrix(table_A5_column_1)),]

# Ensure correct order for merging in simulated DVs
no_nas <- no_nas[order(no_nas$id),]

no_nas$bjp_norm <- (no_nas$bjp_diff - mean(no_nas$bjp_diff)) / sd(no_nas$bjp_diff)

standardized_reg <- lm(data = no_nas, bjp_norm ~ proximity)

# Run regressions on simulated DVs

result <- c()

for(i in 1:1000){
  
  no_nas$sim <- sim[,i]
  
  sim_reg <- lm(data = no_nas,
             sim ~ proximity)
  
  
  result[i] <- coef(sim_reg)["proximity"]
  
}

sim_results <- as.data.frame(result)

ggplot(data = sim_results, aes(result)) +
  geom_histogram(col='black',alpha=0, bins=100) + 
  xlab("Placebo Effects: Kelly Noise Simulations") +
  ylab("Number of Simulations") +
  geom_vline(xintercept = coef(standardized_reg)["proximity"], col='red') +
  geom_vline(xintercept = -coef(standardized_reg)["proximity"], col='red') +
  theme_bw()

ggsave("Figure_A5.png", height = 8, width = 14, units = "cm")

sum(abs(result)>abs(coef(standardized_reg)["proximity"]))/1000

## Get spatial correlation structure of the residuals for our main regression
## Code written by Morgan Kelly, available at https://github.com/morganwkelly/persistence

Spatial_HAC_resid <- function(equation,
                              study_file,
                              range_search,
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
      Y = scale(Residuals),
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

## Apply code to main regression

equation_check <- "bjp_diff ~ proximity +
bjp_share_14 + opp_share_14 + as.factor(AC) + Town + 
log_towndistance100k + log_towndistance50k +
log_towndistance10k + log_towndistance500k + log_population + 
village_nonelec + village_nonpaved +
proportion_male + proportion_sc + proportion_literate + proportion_working +
proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
women_percent_14 + muslim_percent_14 + age_avg_14"

no_nas$X <- no_nas$long
no_nas$Y <- no_nas$lat

# Setting range from 1.3 to 1.5 because the solution is 1.4
# Can expand this range and obtain the same result, will run longer

kelly_se=Spatial_HAC_resid(equation=equation_check,
                           study_file=no_nas,
                           range_search = seq(from=1.2, 1.5, by=0.1),
                           Smoothness = 0.5)

round(kelly_se$Spatial_Parameters,1)
round(kelly_se$HAC[,1:3],3)

#################### Figure A.6 ####################

# Apply 2 additional robustness checks to Kelly SEs
# (Remove outlying region + add quadratic in lat/long)

equation_check <- "bjp_diff ~ proximity +
bjp_share_14 + opp_share_14 + as.factor(AC) + Town + 
log_towndistance100k + log_towndistance50k +
log_towndistance10k + log_towndistance500k + log_population + 
village_nonelec + village_nonpaved +
proportion_male + proportion_sc + proportion_literate + proportion_working +
proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
women_percent_14 + muslim_percent_14 + age_avg_14 +
lat + long + I(lat^2) + I(long^2) + I(lat*long)"

# Setting range from 1.2 to 1.4 because the solution is 1.3
# Can expand this range and obtain the same result, will run longer

kelly_se_checks=Spatial_HAC_resid(equation=equation_check,
                                  study_file=no_nas[no_nas$radius2==0 &
                                                      no_nas$radius3==0,],
                                  range_search = seq(from=1.2, 1.4, by=0.1),
                                  Smoothness = 0.5)

round(kelly_se_checks$HAC[,1:3],3)

ci <-  matrix(NA, ncol = 2, nrow = 2)

ci[1,] <- kelly_se$HAC["proximity", "coef"] + 
  c(-1, 1) * 1.96 *kelly_se$HAC["proximity", "hac.se"] 

ci[2,] <- kelly_se_checks$HAC["proximity", "coef"] + 
  c(-1, 1) * 1.96 *kelly_se$HAC["proximity", "hac.se"] 

coefs <- c(kelly_se$HAC["proximity", "coef"],
           kelly_se_checks$HAC["proximity", "coef"])

kelly_se_data <- as.data.frame(cbind(c("Kelly SEs", "Kelly SEs + Robustness Checks"),
                                     ci,coefs))

colnames(kelly_se_data) <- c("Model", "lower", "upper", "coef")

kelly_se_data$lower <- as.numeric(as.character(kelly_se_data$lower))

kelly_se_data$upper <- as.numeric(as.character(kelly_se_data$upper))

kelly_se_data$coef <- as.numeric(as.character(kelly_se_data$coef))

ggplot(kelly_se_data, aes(x=Model, y=coef)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  ylim(-0.05,0) + ylab("Effect of (Negative) Log Distance") +
  geom_hline(yintercept = 0, lty=2, col='red')

ggsave("Figure_A6.png", height = 8, width = 14, units = "cm")

#################### Figure A.7 ####################

# Make quadratic lat/long variables

no_nas$x2 <- no_nas$X^2
no_nas$y2 <- no_nas$Y^2
no_nas$y3 <- no_nas$Y^3
no_nas$x3 <- no_nas$X^3
no_nas$x2y <- no_nas$X^2 * no_nas$Y
no_nas$y2x <- no_nas$Y^2 * no_nas$X
no_nas$xy <- no_nas$X * no_nas$Y

# run regression 

latlong <- lm(data = no_nas,
              bjp_diff ~  proximity +
                bjp_share_14 +opp_share_14 +
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14 + X + Y +
                xy + y2 + x2 + x3 + y3 + x2y + y2x)

# Apply Conley SE function (code above)

model <- model.matrix(latlong)

ci <- matrix(NA, ncol = 2, nrow = 10)

for(i in 1:10){
  
  ci[i,] <- coef(latlong)[2] + c(-1,1) * 
    olsConley(no_nas,
              no_nas$bjp_diff,
              model[,2:ncol(model)],
              no_nas$lat,
              no_nas$lon, 
              i)$conleySE[2]*1.96
  
}

conley_data <- as.data.frame(cbind(c(1:10), ci, coef(latlong)[2]))

colnames(conley_data) <- c("Radius (km)", "lower", "upper", "coef")

conley_data$`Radius (km)` <- as.factor(conley_data$`Radius (km)`)

ggplot(conley_data, aes(x=`Radius (km)`, y=coef)) + 
  geom_point() +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                position=position_dodge(.9)) +
  ylim(-0.05,0) + ylab("Effect of Log Distance") +
  geom_hline(yintercept = 0, lty=2, col='red')

ggsave("Figure_A7.png", height = 8, width = 14, units = "cm")

#################### Table A.13 ####################

# Get neighbors (defined as within 4.5km)
nb4.5 <- dnearneigh(as.matrix(no_nas[,c("long", "lat")]), 
                    0, 4.5, row.names = NULL, longlat = TRUE)


form<- bjp_diff ~ proximity +
  bjp_share_14 +opp_share_14 +
  as.factor(AC) + Town  + 
  log_towndistance100k + log_towndistance50k +
  log_towndistance10k + log_towndistance500k + log_population + 
  village_nonelec + village_nonpaved +
  proportion_male + proportion_sc + proportion_literate + proportion_working +
  proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
  women_percent_14 + muslim_percent_14 + age_avg_14

table_A13_column_1 <- errorsarlm(form, data=no_nas,
                                 nb2listw(nb4.5), tol.solve=1.0e-30)

table_A13_column_2 <- lagsarlm(form, data=no_nas, nb2listw(nb4.5), tol.solve=1.0e-30)

# Get average of neighbors' DV
dists <- apply(no_nas[,c("long", "lat")], 1,
               function(x) distHaversine(no_nas[,c("long", "lat")], x))

no_nas$neighbors <- c()

for(i in 1:nrow(no_nas)){
  
  new_dists <- dists[,i]
  
  names(new_dists) <- 1:2514
  
  no_nas$neighbors[i] <- mean(no_nas$bjp_diff[as.numeric(names(sort(new_dists)[2:6])) ])
  
}

table_A13_column_3 <- lm(data = no_nas,
          bjp_diff ~ proximity +
            bjp_share_14 +opp_share_14 +
            as.factor(AC) + Town  + 
            log_towndistance100k + log_towndistance50k +
            log_towndistance10k + log_towndistance500k + log_population + 
            village_nonelec + village_nonpaved +
            proportion_male + proportion_sc + proportion_literate + proportion_working +
            proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
            women_percent_14 + muslim_percent_14 + age_avg_14 + neighbors)
table_A13_column_3_CIs <- block_bootstrap(no_nas, table_A13_column_3,
                                          "PC", 1, 1000, "proximity")

stargazer(table_A13_column_1, table_A13_column_2, table_A13_column_3,
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14",
                 "age_avg_14", "Constant", "neighbors"),
          se = list(sqrt(diag(table_A13_column_1$resvar)),
                    sqrt(diag(table_A13_column_2$resvar)),
                    table_A13_column_3_CIs$se),
          p.auto = F,
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          add.lines=list(c("State Constituency Dummies", "Yes", "Yes", "Yes"),
                         c("Electoral + Sociodemographic Controls","Yes", "Yes", "Yes")),
          title = "Spatial Lag and Error Models + Controlling for Neighbors",
          out='tableA13.tex')

#################### Table A.14 ####################

set.seed(1)
nySFE <- SpatialFiltering(bjp_diff ~ proximity + bjp_share_14 +opp_share_14 +
                              as.factor(AC) + Town+ log_towndistance100k + log_towndistance50k +
                              log_towndistance10k + log_towndistance500k + log_population + 
                              village_nonelec + village_nonpaved + proportion_male +
                              proportion_sc + proportion_literate + proportion_working +
                              proportion_cultivators + proportion_aglaborers +
                              proportion_marginalworkers + women_percent_14 +
                              muslim_percent_14 + age_avg_14, data=no_nas,
                              nb=nb4.5, style="W", verbose=FALSE)

dat <- as.data.frame(nySFE$dataset)
colnames(dat) <- colnames(nySFE$dataset)

no_nas$no <- 1:nrow(no_nas)
dat$no <- 1:nrow(dat)

no_nas <- merge(no_nas, dat, by="no", all.x=T)

nylmSFE <- lm(data = no_nas, bjp_diff ~  proximity +
                bjp_share_14 +opp_share_14 +
                as.factor(AC) + Town  + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14 + vec1 + vec2 +                     
                vec4  + vec39 + vec33 + vec9 + vec12 + vec77 + vec14 + vec72 + vec42 +
                vec22 + vec15 + vec50 + vec80 + vec32 + vec98 + vec91 + vec68 + vec191 +
                vec61 + vec92 + vec5 + vec194 + vec10 + vec27 + vec45 + vec76)

moran_CIs <- block_bootstrap(no_nas, nylmSFE, "PC", 1, 1000, "proximity")

stargazer(nylmSFE,
          ci.custom = list(moran_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(nylmSFE$p),
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14",
                 "age_avg_14", "Constant",
                 colnames(no_nas)[which(grepl("vec",colnames(no_nas))==T)]),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          add.lines=list(c("State Constituency Dummies", "Yes"),
                         c("Electoral + Sociodemographic Controls","Yes")),
          column.labels= c("Moran Eigenvectors"),
          notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
          title = "Including Moran Eigenvectors as Covariates",
          out='tableA14.tex')

#################### Table A.15 ####################

table_A15_column_1 <- lm(data = no_nas[no_nas$MoV_Last < median(no_nas$MoV_Last, na.rm=T),],
              bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_1_CIs <- block_bootstrap(no_nas, table_A15_column_1,
                                          "PC", 1, 1000, "proximity")

table_A15_column_2 <- lm(data = no_nas[no_nas$MoV_Last >= median(no_nas$MoV_Last, na.rm=T),],
               bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                 as.factor(AC) + Town + 
                 log_towndistance100k + log_towndistance50k +
                 log_towndistance10k + log_towndistance500k + log_population + 
                 village_nonelec + village_nonpaved +
                 proportion_male + proportion_sc + proportion_literate + proportion_working +
                 proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                 women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_2_CIs <- block_bootstrap(no_nas, table_A15_column_2,
                                          "PC", 1, 1000, "proximity")

table_A15_column_3 <- lm(data = no_nas[no_nas$muslim_percent_14 < 
                                 median(no_nas$muslim_percent_14, na.rm=T),],
                 bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                   as.factor(AC) + Town + 
                   log_towndistance100k + log_towndistance50k +
                   log_towndistance10k + log_towndistance500k + log_population + 
                   village_nonelec + village_nonpaved +
                   proportion_male + proportion_sc + proportion_literate + proportion_working +
                   proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                   women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_3_CIs <- block_bootstrap(no_nas, table_A15_column_3,
                                          "PC", 1, 1000, "proximity")

table_A15_column_4 <- lm(data = no_nas[no_nas$muslim_percent_14 >=
                                         median(no_nas$muslim_percent_14, na.rm=T),],
                  bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                    as.factor(AC) + Town + 
                    log_towndistance100k + log_towndistance50k +
                    log_towndistance10k + log_towndistance500k + log_population + 
                    village_nonelec + village_nonpaved +
                    proportion_male + proportion_sc + proportion_literate + proportion_working +
                    proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                    women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_4_CIs <- block_bootstrap(no_nas, table_A15_column_4,
                                      "PC", 1, 1000, "proximity")

table_A15_column_5 <- lm(data = no_nas[no_nas$proportion_sc < 
                                         median(no_nas$proportion_sc, na.rm=T),],
             bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
               as.factor(AC) + Town + 
               log_towndistance100k + log_towndistance50k +
               log_towndistance10k + log_towndistance500k + log_population + 
               village_nonelec + village_nonpaved +
               proportion_male + proportion_sc + proportion_literate + proportion_working +
               proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
               women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_5_CIs <- block_bootstrap(no_nas, table_A15_column_5, "PC", 1, 1000, "proximity")

table_A15_column_6 <- lm(data = no_nas[no_nas$proportion_sc >= 
                                         median(no_nas$proportion_sc, na.rm=T),],
              bjp_diff ~ proximity + bjp_share_14 + opp_share_14 + 
                as.factor(AC) + Town + 
                log_towndistance100k + log_towndistance50k +
                log_towndistance10k + log_towndistance500k + log_population + 
                village_nonelec + village_nonpaved +
                proportion_male + proportion_sc + proportion_literate + proportion_working +
                proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
                women_percent_14 + muslim_percent_14 + age_avg_14)
table_A15_column_6_CIs <- block_bootstrap(no_nas, table_A15_column_6,
                                          "PC", 1, 1000, "proximity")

stargazer(table_A15_column_1, table_A15_column_2, table_A15_column_3,
          table_A15_column_4, table_A15_column_5, table_A15_column_6,
          ci.custom = list(table_A15_column_1_CIs$percentile_ci,
                           table_A15_column_2_CIs$percentile_ci,
                           table_A15_column_3_CIs$percentile_ci,
                           table_A15_column_4_CIs$percentile_ci,
                           table_A15_column_5_CIs$percentile_ci,
                           table_A15_column_6_CIs$percentile_ci), 
          ci.level = 0.95, 
          p = list(table_A15_column_1_CIs$p,
                   table_A15_column_2_CIs$p,
                   table_A15_column_3_CIs$p,
                   table_A15_column_4_CIs$p,
                   table_A15_column_5_CIs$p,
                   table_A15_column_6_CIs$p),
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14",
                 "age_avg_14", "Constant"),
          ci.separator = ",",
          omit.stat=c('adj.rsq','ser','f'),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          add.lines=list(c("AC Dummies", "Yes", "Yes", "Yes","Yes", "Yes", "Yes"),
                         c("Controls", "Yes", "Yes", "Yes", "Yes", "Yes", "Yes")),
          column.labels= c("Close Seats", "Safe Seats", "Fewer Muslims",
                           "More Muslims", "Fewer SCs", "More SCs"),
          notes="Cluster (block) bootstrap 95 percentile confidence intervals in parentheses.",
          title = "Caste, Religion, and Constituency Heterogeneous Effects",
          out='tableA15.tex')

#################### Table A.16 ####################

sc_jawan <- no_nas[no_nas$radius5==1 | no_nas$radius6==1 | no_nas$radius8==1,]

table_A16_column_1 <- lm(data = sc_jawan[sc_jawan$proportion_sc < 
                             median(sc_jawan$proportion_sc, na.rm=T),],
           bjp_diff ~ proximity +
             bjp_share_14 + opp_share_14 + as.factor(AC) + 
             log_towndistance100k + log_towndistance50k + Town +
             log_towndistance10k + log_towndistance500k + log_population + 
             village_nonelec + village_nonpaved +
             proportion_male + proportion_sc + proportion_literate + proportion_working +
             proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
             women_percent_14 + muslim_percent_14 + age_avg_14)

table_A16_column_2 <- lm(data = sc_jawan[sc_jawan$proportion_sc >= 
                             median(sc_jawan$proportion_sc, na.rm=T),],
           bjp_diff ~ proximity +
             bjp_share_14 + opp_share_14 + as.factor(AC) + 
             log_towndistance100k + log_towndistance50k + Town +
             log_towndistance10k + log_towndistance500k + log_population + 
             village_nonelec + village_nonpaved +
             proportion_male + proportion_sc + proportion_literate + proportion_working +
             proportion_cultivators + proportion_aglaborers + proportion_marginalworkers + 
             women_percent_14 + muslim_percent_14 + age_avg_14)

stargazer(table_A16_column_1, table_A16_column_2,
          omit=c("bjp_share_14", "AC" , "opp_share_14",
                 "Town", "log_towndistance50k", "log_towndistance10k",
                 "log_towndistance100k", "log_towndistance500k", "log_population",
                 "village_nonelec", "village_nonpaved",
                 "proportion_male", "proportion_sc", "proportion_literate", 
                 "proportion_working", "proportion_cultivators", 
                 "proportion_aglaborers", "proportion_marginalworkers",
                 "women_percent_14", "muslim_percent_14",
                 "age_avg_14", "Constant"),
          column.labels = c("Fewer SCs", "More SCs"),
          dep.var.labels  = "Change in BJP Vote Share",
          covariate.labels = "Proximity",
          omit.stat=c('adj.rsq','ser','f'),
          notes="Unadjusted standard errors in parentheses.",
          add.lines=list(c("AC Dummies", "Yes", "Yes"),
                         c("Controls","Yes", "Yes")),
          title = "SC Heterogeneity for SC Deaths",
          out='tableA16.tex')

#################### Figure A.8 ####################

# Get 1 sd below/above mean proportion to make graph bounds

muslim_lower_bound <- mean(pulwama$muslim_percent_14[pulwama$Incumbent_Party=='BJP'], na.rm=T) -
  sd(pulwama$muslim_percent_14[pulwama$Incumbent_Party=='BJP'], na.rm=T)

muslim_upper_bound <- mean(pulwama$muslim_percent_14[pulwama$Incumbent_Party=='BJP'], na.rm=T) +
  sd(pulwama$muslim_percent_14[pulwama$Incumbent_Party=='BJP'], na.rm=T)

sc_lower_bound <- mean(pulwama$proportion_sc[pulwama$Incumbent_Party=='BJP'], na.rm=T) -
  sd(pulwama$proportion_sc[pulwama$Incumbent_Party=='BJP'], na.rm=T)

sc_upper_bound <- mean(pulwama$proportion_sc[pulwama$Incumbent_Party=='BJP'], na.rm=T) +
  sd(pulwama$proportion_sc[pulwama$Incumbent_Party=='BJP'], na.rm=T)

# Make Plots

ggplot(data = pulwama[pulwama$Incumbent_Party=='BJP',],
       aes(mindist, muslim_percent_14))+
       geom_smooth(method="loess") + 
       ylab("Percent Muslim") + xlab("Procession Distance") +
       ylim(muslim_lower_bound, muslim_upper_bound)

ggsave("Figure_A8_1.png", width = 10, height=7, units = "cm")

ggplot(data = pulwama[pulwama$Incumbent_Party=='BJP',],
       aes(mindist, proportion_sc*100))+
       geom_smooth(method="loess") +
       ylab("Percent SC") + xlab("Procession Distance") +
       ylim(sc_lower_bound*100, sc_upper_bound*100)

ggsave("Figure_A8_2.png", width = 10, height=7, units = "cm")

#################### Appendix R: Back-of-the-Envelope Calculations ####################

## Get point estimate for # votes lost

sim_data <- no_nas
sim_data$proximity <- -1 * log(21)

sim_data$new_prediction_mid <- predict(table_A5_column_1, newdata = sim_data)

no_nas$new_prediction_mid <- predict(table_A5_column_1, newdata = no_nas)

sim_data$sim_vote_shares <- sim_data$new_prediction_mid + sim_data$bjp_share_14
no_nas$sim_vote_shares <- no_nas$new_prediction_mid + no_nas$bjp_share_14

# Best estimate
sum((sim_data$sim_vote_shares)*sim_data$Votes_19) - 
  sum((no_nas$sim_vote_shares)*no_nas$Votes_19)

## Get 95% CI upper bound

table_A5_column_1$coefficients['proximity'] <- table_A5_column_1_CIs$percentile_ci[1]

sim_data <- no_nas
sim_data$proximity <- -1 * log(21)

sim_data$new_prediction_high <- predict(table_A5_column_1, newdata = sim_data)

no_nas$new_prediction_high <- predict(table_A5_column_1, newdata = no_nas)

sim_data$sim_vote_shares <- sim_data$new_prediction_high + sim_data$bjp_share_14
no_nas$sim_vote_shares <- no_nas$new_prediction_high + no_nas$bjp_share_14

# Upper bound estimate
sum((sim_data$sim_vote_shares)*sim_data$Votes_19) - 
  sum((no_nas$sim_vote_shares)*no_nas$Votes_19)

## Get 95% CI lower bound

table_A5_column_1$coefficients['proximity'] <- table_A5_column_1_CIs$percentile_ci[2]

sim_data <- no_nas
sim_data$proximity <- -1 * log(21)

sim_data$new_prediction_low <- predict(table_A5_column_1, newdata = sim_data)

no_nas$new_prediction_low <- predict(table_A5_column_1, newdata = no_nas)

sim_data$sim_vote_shares <- sim_data$new_prediction_low + sim_data$bjp_share_14
no_nas$sim_vote_shares <- no_nas$new_prediction_low + no_nas$bjp_share_14

# Upper bound estimate
sum((sim_data$sim_vote_shares)*sim_data$Votes_19) - 
  sum((no_nas$sim_vote_shares)*no_nas$Votes_19)
