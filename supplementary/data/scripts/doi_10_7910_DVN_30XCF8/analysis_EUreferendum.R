################################################################################
# Immigrants, elections, and turnout
# Author: Linuz Aggeborn, Henrik Andersson, Sirus Dehdari, Karl-Oskar Lindgren
# Description: Create figures for individual-level turnout in the 2002 local
# elections and the 2003 euro referendum.
################################################################################

# Saving local paths for input and output:
in_output <- "E://ProjData//Concurrent elections//finaldata//"

out_figure <- "C://Userdata//Shared//Output//Concurrent elections//img//" 
out_table <- "C://Userdata//Shared//Output//Concurrent elections//tex//" 

out_figure_mydp <- "//tsclient//C//Users//HP ZBook//Dropbox//Projects//Medborgarskapsprojektet//figures//"

# Loading packages
library(readstata13) # To read Stata13 dta-files
library(xtable)
library(multiwayvcov)
library(sandwich)
library(lmtest)
library(ggplot2)
library(plm)
library(lfe) # Command "felm" for fixed effects models
library(Jmisc) # Command "demean"

### Importing individual-level data: ----
turnout_02_03 <-  read.dta13(paste(in_output,"turnout_2002_2003.dta", sep =""))


### ESTIMATION:----
# We estimate the turnout rates for each category of year between immigration
# year and election year. We keep only observations for year categories between
# 3 and 40

# Subsetting data for immigrants from non-nordic countries, that only immigrated
# once:
tempreg_df <- subset(turnout_02_03, nordic == 0 & immig_once == 1)

# Estimating turnout rates in the 2003 euro referendum for each year category:
reg_eu <- felm(EUvote ~ as.factor(dist_year_eu) -1 | 0 | 0 | 0,
                        data = tempreg_df[tempreg_df$dist_year_eu %in% c(3:40),])
coeff_eu <- summary(reg_eu)

# Estimating turnout rates in the 2002 local election for each year category:
reg_local <- felm(local_vote ~ as.factor(dist_year_local) -1 | 0 | 0 | 0,
               data = tempreg_df[tempreg_df$dist_year_local %in% c(3:40),])
coeff_local <- summary(reg_local)


# We construct a data frame for these estimates:
reg_coef <- data.frame(Years = rep(3:40), 
            Estimate = c(coeff_eu$coefficients[,1],coeff_local$coefficients[,1]), 
            se = c(coeff_eu$coefficients[,2],coeff_local$coefficients[,2]), 
            Election = c(rep("Referendum",length(3:40)), rep("Local", length(3:40))))
                         


# Setting critical value for CI:
CI_int <- 0.05
CI_t <- qnorm(CI_int/2, lower.tail = FALSE)

# Computing lower and upper limits for the confidence intervals:

reg_coef$Min <- reg_coef$Estimate-reg_coef$se*CI_t
reg_coef$Max <- reg_coef$Estimate+reg_coef$se*CI_t 

# We can restrict the upper bound to 1:
reg_coef$Max[reg_coef$Max>1] <- 1


# Set up ggplot figure:
temp_gg <- ggplot(reg_coef, aes(x = Years, y = Estimate))


temp_gg + geom_point(aes(color = Election, shape = Election), size = 3) +
  geom_errorbar(aes(x = Years, ymin = Min, ymax = Max,color = Election, linetype = Election), width = 0.3) +
  theme_bw(base_size = 20) +
  labs(y= "Turnout in election/referendum",x = "Years since immigration") + 
  scale_color_manual(values = c("black","black")) +
  theme(legend.position="bottom")

# Saving to file:
ggsave(paste(out_figure,"turnout02_03.pdf", sep = ""))
ggsave(paste(out_figure_mydp,"turnout02_03.pdf", sep = ""))
###



### Subsetting, based on origin of immigrants:----

# Next, we restrict the sample to immigrants from other EU countries:
tempreg_df <- subset(turnout_02_03, nordic == 0 & immig_once == 1 & eu_country == 1)

reg_eu <- felm(EUvote ~ as.factor(dist_year_eu) -1 | 0 | 0 | 0,
               data = tempreg_df[tempreg_df$dist_year_eu %in% c(3:30),])
coeff_eu <- summary(reg_eu)

reg_local <- felm(local_vote ~ as.factor(dist_year_local) -1 | 0 | 0 | 0,
                  data = tempreg_df[tempreg_df$dist_year_local %in% c(3:30),])
coeff_local <- summary(reg_local)


# We construct a data frame for these estimates:
reg_coef <- data.frame(Years = rep(3:30), 
                       Estimate = c(coeff_eu$coefficients[,1],coeff_local$coefficients[,1]), 
                       se = c(coeff_eu$coefficients[,2],coeff_local$coefficients[,2]), 
                       Election = c(rep("Referendum",length(3:30)), rep("Local", length(3:30))))



# Setting critical value for CI:
CI_int <- 0.05
CI_t <- qnorm(CI_int/2, lower.tail = FALSE)

# Computing lower and upper limits for the confidence intervals:

reg_coef$Min <- reg_coef$Estimate-reg_coef$se*CI_t
reg_coef$Max <- reg_coef$Estimate+reg_coef$se*CI_t 

# We can restrict the upper bound to 1:
reg_coef$Max[reg_coef$Max>1] <- 1


# Set up ggplot figure:
temp_gg <- ggplot(reg_coef, aes(x = Years, y = Estimate))

temp_gg + geom_point(aes(color = Election, shape = Election), size = 3) +
  geom_errorbar(aes(x = Years, ymin = Min, ymax = Max,color = Election, linetype = Election), width = 0.3) +
  theme_bw(base_size = 20) +
  labs(y= "Turnout in election/referendum",x = "Years since immigration") + 
  scale_color_manual(values = c("black","black")) +
  theme(legend.position="bottom")

# Saving to file
ggsave(paste(out_figure,"turnout02_03_onlyEU.pdf", sep = ""))
ggsave(paste(out_figure_mydp,"turnout02_03_onlyEU.pdf", sep = ""))
###  



# Next, we restrict the sample to immigrants from non EU coutries:
tempreg_df <- subset(turnout_02_03, nordic == 0 & immig_once == 1 & eu_country == 0)

reg_eu <- felm(EUvote ~ as.factor(dist_year_eu) -1 | 0 | 0 | 0,
               data = tempreg_df[tempreg_df$dist_year_eu %in% c(3:30),])
coeff_eu <- summary(reg_eu)

reg_local <- felm(local_vote ~ as.factor(dist_year_local) -1 | 0 | 0 | 0,
                  data = tempreg_df[tempreg_df$dist_year_local %in% c(3:30),])
coeff_local <- summary(reg_local)


# We construct a data frame for these estimates:
reg_coef <- data.frame(Years = rep(3:30), 
                       Estimate = c(coeff_eu$coefficients[,1],coeff_local$coefficients[,1]), 
                       se = c(coeff_eu$coefficients[,2],coeff_local$coefficients[,2]), 
                       Election = c(rep("Referendum",length(3:30)), rep("Local", length(3:30))))



# Setting critical value for CI:
CI_int <- 0.05
CI_t <- qnorm(CI_int/2, lower.tail = FALSE)

# Computing lower and upper limits for the confidence intervals:
reg_coef$Min <- reg_coef$Estimate-reg_coef$se*CI_t
reg_coef$Max <- reg_coef$Estimate+reg_coef$se*CI_t 

# We can restrict the upper bound to 1:
reg_coef$Max[reg_coef$Max>1] <- 1


# Set up ggplot figure:
temp_gg <- ggplot(reg_coef, aes(x = Years, y = Estimate))


temp_gg + geom_point(aes(color = Election, shape = Election), size = 3) +
  geom_errorbar(aes(x = Years, ymin = Min, ymax = Max,color = Election,  linetype = Election), width = 0.3) +
  theme_bw(base_size = 20) +
  labs(y= "Turnout in election/referendum",x = "Years since immigration") + 
  scale_color_manual(values = c("black","black")) +
  theme(legend.position="bottom")

# Save to file:
ggsave(paste(out_figure,"turnout02_03_nonEU.pdf", sep = ""))
ggsave(paste(out_figure_mydp,"turnout02_03_nonEU.pdf", sep = ""))
###  






