# ----------------------------------------------------------------------------
# INFO  
# ----------------------------------------------------------------------------

# Article: The Electoral Politics of Immigration and Crime
# Author(s): Jeyhun Alizade
# Code to replicate Figure 4

# ----------------------------------------------------------------------------
# SETUP
# ----------------------------------------------------------------------------

# clear environment
rm(list=ls())

# install and load necessary packages
# install.packages("plm", "ggplot2", "lmtest")
library(plm)
library(ggplot2)
library(lmtest)

# set working directory (change working directory to where the replication folder is saved on your computer)
setwd("/Users/jeyhunalizade/Dropbox/immigration crime project/ajps_replication/replication_files/")

# load data set
dat = readRDS("data/panel_survey.rds")

# declare as panel data set
dat = pdata.frame(dat, index=c("pseudonym", "wave")) 


# ----------------------------------------------------------------------------
# MODELS
# ----------------------------------------------------------------------------

## Effect of fear of refugee crime on vote intention for right-wing parties and their own party among left-wing voters

# two-way FE without controls among left-wing voters
# extract coefficient and standard error for perceived effect of refugees on local crime
vote_ests_twofe <- rbind.data.frame(
  coeftest(plm(fdp_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(left_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(left_now~ref_loc_crime, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  
  coeftest(plm(fdp_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(greens_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(greens_now~ref_loc_crime, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  
  coeftest(plm(fdp_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(spd_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(spd_now~ref_loc_crime, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2]
)
names(vote_ests_twofe) <- c("coef", "se")

# two-way FE with controls among left-wing voters
# extract coefficient and standard error for perceived effect of refugees on local crime
vote_ests_twofe_cov <- rbind.data.frame(
  coeftest(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(left_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(left_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$left_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  
  coeftest(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(greens_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(greens_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  
  coeftest(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(fdp_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(afd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2],
  coeftest(plm(spd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),
           vcov=vcovHC(plm(spd_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$spd_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))[1,1:2]
)
names(vote_ests_twofe_cov) <- c("coef", "se")


# ----------------------------------------------------------------------------
# PLOTTING
# ----------------------------------------------------------------------------

# combine estimates in one data frame
vote_ests <- rbind.data.frame(vote_ests_twofe, vote_ests_twofe_cov)

# create variable for party respondent voted for
vote_ests$party <- rep(rep(c("Linke voter", "Green voter", "SPD voter"), each=4), 2)
vote_ests$party <- factor(vote_ests$party, levels=c("Linke voter", "Green voter", "SPD voter"))

# create variable for outcome (vote intention for a given right-wing party)
vote_ests$outcome <- rep(c("FDP", "CDU/CSU", "AfD", "Own party", "FDP", "CDU/CSU", "AfD", "Own party", "FDP", "CDU/CSU", "AfD", "Own party"), 2)
vote_ests$outcome <- factor(vote_ests$outcome, levels=rev(c("CDU/CSU", "FDP", "AfD", "Own party")))

# create variable indicating the model (without or with controls)
vote_ests$model <- rep(c("no controls", "controls"), each=12)
vote_ests$model <- factor(vote_ests$model, levels=rev(c("no controls", "controls")))

# create Figure 4
ggplot(data=vote_ests, aes(x=outcome, y=coef, group=model, color=model)) + 
  geom_hline(yintercept=0, linetype="dashed") +
  geom_point(position=position_dodge(width=.5), size=3) + 
  geom_errorbar(aes(ymin=coef-1.96*se, ymax=coef+1.96*se), width=0, position=position_dodge(width=.5)) +
  geom_errorbar(aes(ymin=coef-1.645*se, ymax=coef+1.645*se), width=0, linewidth=1.2, position=position_dodge(width=.5)) +
  facet_wrap(~party, nrow=1) + 
  coord_flip() +
  theme_bw() +
  xlab("Vote intention") + ylab("Coefficient") +
  theme(legend.position = "bottom", text=element_text(size=18),
        panel.grid = element_blank(), axis.text = element_text(color="black"),
        axis.title.x = element_text(margin = margin(t = 10)),  # Adjust top margin of x-axis title
        axis.title.y = element_text(margin = margin(r = 10)),
        axis.text.x = element_text(size = 13)) +
  scale_color_manual(breaks=c("no controls", "controls"), values=c("gray50", "gray0"), name="") +
  scale_y_continuous(limits=c(-.12, .12), breaks = seq(-.10, .10, .10))

# save Figure 4
ggsave("figures/figure_4.pdf", width=7, height=5)


# ----------------------------------------------------------------------------
# NUMBERS REPORTED ON PAGE 18, FIRST PARAGRAPH
# ----------------------------------------------------------------------------

# save model with CDU/CSU vote intention as DV, Green voters subsample
mod_cdu_green_controls = coeftest(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),
         vcov=vcovHC(plm(cdu_now~ref_loc_crime+ref_loc_culture+ref_loc_economy+ref_loc_services+ref_loc_islam+ref_loc_jobs+ref_loc_schools+ref_loc_housing+ref_loc_wayoflife+ref_crime+ref_terror+ref_integrating+ref_citizenship+ref_reduce+ref_moredone+ref_cultgiveup+ref_economy+ref_intown+distance_refcenter_cat+cont_publicplaces_cat+cont_stores_cat+cont_center_cat+cont_school_cat+cont_work_cat+cont_spoken_cat+fearcrime+crime_ok+satis_locpolice+tr_police+nbtrust+lrscale+genderscale+welf_index, dat[dat$green_any==1,], model="within", effect = "twoways"),type="HC0", cluster="group"))

# Effect magnitude of concern about local refugee crime on CDU/CSU vote intention among Green voters (model with controls)
print(mod_cdu_green_controls["ref_loc_crime",1]*100)

# P-value of concern about local refugee crime on CDU/CSU vote intention among Green voters (model with controls)
print(mod_cdu_green_controls["ref_loc_crime",4]) # p-value below alpha=0.05


# Overall probability of indicating vote intention for CDU among Green voters (discussed on p. 18)
print(mean(dat$cdu_now[dat$green_any==1], na.rm=T)*100)

