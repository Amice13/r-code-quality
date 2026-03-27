# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure D.3

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("ggplot2", "dplyr", "tidyr", "plm", "lmtest")
library(ggplot2)
library(dplyr)
library(tidyr)
library(plm)
library(lmtest)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/soep.rds")


# ----------------------------------------------------------------------------
# DATA PREPARATION
# ----------------------------------------------------------------------------

# ensure data is sorted correctly
dat <- dat %>%
  arrange(pid, syear)

# create a variable for the first year each pid appears
dat <- dat %>%
  group_by(pid) %>%
  mutate(first_year = min(syear, na.rm = TRUE),
         year_since_first = syear - first_year + 1) %>%
  ungroup()

# create indicator for Greens preference
dat <- dat %>%
  mutate(greens_pref = ifelse(partypref == "Greens", 1, 0))

# for each pid, keep only the first 4 years since joining
dat_limited <- dat %>%
  filter(year_since_first %in% 1:4)

# create a wide format table with greens_pref for each year_since_first
greens_wide <- dat_limited %>%
  select(pid, year_since_first, greens_pref) %>%
  pivot_wider(names_from = year_since_first,
              values_from = greens_pref,
              names_prefix = "y") %>%
  replace(is.na(.), 0)  # Replace NAs with 0

# Step 6: Create binary indicators for 1–4 consecutive years
greens_streaks <- greens_wide %>%
  rowwise() %>%
  mutate(
    streak_1 = ifelse(y1 == 1, 1, 0),
    streak_2 = ifelse(all(c(y1, y2) == 1), 1, 0),
    streak_3 = ifelse(all(c(y1, y2, y3) == 1), 1, 0),
    streak_4 = ifelse(all(c(y1, y2, y3, y4) == 1), 1, 0),
  ) %>%
  ungroup() %>%
  select(pid, starts_with("streak_"))

# merge with initial data set
dat = merge(dat, greens_streaks, by="pid")

# create binary outcome (party preference for CDU)
dat$partypref_cdu = dat$partypref=="CDU/CSU"


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

# create panel data set
pdat = plm::pdata.frame(dat, index = c("pid", "syear"))

# two-way fixed effects models estimating the effect of crime concern on CDU/CSU preference among different samples of Green supporters, without and with controls
# extracting the coefficient and the standard error for the crime concern variable
longterm_green_ests = rbind.data.frame(
  
 coeftest(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_1==1 & pdat$year_since_first>1,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_1==1 & pdat$year_since_first>1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
 coeftest(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_1==1 & pdat$year_since_first>1,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_1==1 & pdat$year_since_first>1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],

 coeftest(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_2==1 & pdat$year_since_first>2,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_2==1 & pdat$year_since_first>2,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
 coeftest(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_2==1 & pdat$year_since_first>2,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_2==1 & pdat$year_since_first>2,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],

 coeftest(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_3==1 & pdat$year_since_first>3,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_3==1 & pdat$year_since_first>3,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
 coeftest(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_3==1 & pdat$year_since_first>3,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_3==1 & pdat$year_since_first>3,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],

 coeftest(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_4==1 & pdat$year_since_first>4,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern, data=pdat[pdat$streak_4==1 & pdat$year_since_first>4,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
 coeftest(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_4==1 & pdat$year_since_first>4,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(partypref_cdu ~ crime_concern+econ_general+econ_own+environment+peace+job_sec, data=pdat[pdat$streak_4==1 & pdat$year_since_first>4,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2]
)


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# prepare the data frame with the coefficient estimates for plotting
names(longterm_green_ests)[1:2] = c("coef", "se")
longterm_green_ests$model = rep(c("w/o controls", "w/ controls"), 4)
longterm_green_ests$green_vote_pref = c("First year", "First year", "First two years", "First two years", "First three years", "First three years", "First four years", "First four years")
longterm_green_ests$green_vote_pref = factor(longterm_green_ests$green_vote_pref, levels = rev(c("First year", "First two years", "First three years", "First four years")))

# create Figure D.3
ggplot(data=longterm_green_ests, aes(x=green_vote_pref, y=coef, group=model, color=model)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(position=position_dodge(width=.5), size=3) + 
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-1.645*se, ymax=coef+1.645*se), width=0, linewidth=1.2, position=position_dodge(width=.5)) +
  coord_flip() +
  theme_bw() +
  xlab("Party preference for Greens") + ylab("Coefficient") +
  theme(legend.position = "bottom", text=element_text(size=20), axis.text.x = element_text(angle = 90, vjust=.5, hjust=1)) +
  scale_color_manual(breaks=c("w/o controls", "w/ controls"), values=c("gray50", "gray0"), name="")

# save Figure D.3
ggsave("figures/figure_d3.pdf", width = 7, height = 5)