########################################################
## This R-file produces Figure 14 in SI
########################################################

## Packages
rm(list = ls())

install.packages(c("tidyverse", "haven", "ggplot2"))
install.packages('interflex', type = "source", repos = 'http://cran.us.r-project.org') 

library(tidyverse);library(haven);library(ggplot2);library(interflex)

##
set.seed(123)
## set working directory as root = ""

dataRaw = read_dta("10_Data_analysis_final.dta") %>% 
  filter(!(is.na(win_again_noind)),
         party_11 != "INDEPENDENT")

dataLP = dataRaw %>% filter(persig14 == 0) %>% mutate(
  master_mandate_sd = scale(master_mandate),
  coun_IND_sd = scale(coun_IND),
  coun_NRM_sd = scale(coun_NRM), 
  coun_age_sd = scale(coun_age), 
  coun_asst_motor_sd = scale(coun_asst_motor), 
  coun_terms_sd = scale(coun_terms), 
  coun_speaker_sd = scale(coun_speaker), 
  log_pop_ea_sd = scale(log_pop_ea), 
  literacy_share_ea_sd = scale(literacy_share_ea), 
  elf_ea_sd = scale(elf_ea), 
  poverty_census_ea_sd = scale(poverty_census_ea), 
  agr_share_ea_sd = scale(agr_share_ea)
) %>%
  mutate(master_mandate_sdint = master_mandate_sd * id,
         coun_IND_sdint = coun_IND_sd * id,
         coun_NRM_sdint = coun_NRM_sd *id, 
         coun_age_sdint = coun_age_sd * id, 
         coun_asst_motor_sdint = coun_asst_motor_sd * id, 
         coun_terms_sdint = coun_terms_sd * id, 
         coun_speaker_sdint = coun_speaker_sd * id, 
         log_pop_ea_sdint = log_pop_ea_sd * id, 
         literacy_share_ea_sdint = literacy_share_ea_sd * id, 
         elf_ea_sdint = elf_ea_sd * id, 
         poverty_census_ea_sdint = poverty_census_ea_sd * id, 
         agr_share_ea_sdint = agr_share_ea_sd * id)

controls = c("master_mandate_sd",'coun_NRM_sd','coun_age_sd', 'coun_asst_motor_sd', 'coun_terms_sd', 'coun_speaker_sd', 
             'log_pop_ea_sd', 'literacy_share_ea_sd', 'elf_ea_sd', 'poverty_census_ea_sd', 'agr_share_ea_sd',
             "master_mandate_sdint",'coun_NRM_sdint','coun_age_sdint', 'coun_asst_motor_sdint', 'coun_terms_sdint', 'coun_speaker_sdint', 
             'log_pop_ea_sdint', 'literacy_share_ea_sdint', 'elf_ea_sdint', 'poverty_census_ea_sdint', 'agr_share_ea_sdint')

fig14 = interflex(Y = "win_again_noind", D = "id", X = "PA3_11", Z = controls, 
                 data = as.data.frame(dataLP),
                 estimator = "binning", FE = "master_district", 
                 cl = "master_district", weights = "wid",
                 Xlabel="Party advantage", Ylabel="Win Probability", 
                 Dlabel = "Treatment", nbins = 2)


ggsave("Figure-SI14.pdf", 
       plot(fig14, theme.bw = TRUE, show.grid = FALSE,
            xlim = c(-1,1), ylim = c(-3, 1),cex.axis = 0.8, 
            cex.lab = 0.8, cex.main = 0.8, bin.labs = FALSE, 
            main = "Low Performance"),
       height = 6, width = 8)










