##############################################################################
#
#                             Replication scripts
#               Table A.5 Column 3 Bootstrapped Standard Errors
#         Battaglini M., Leone Sciabolazza V., Patacchini, E. (2019)
#                     "Effectiveness of connected legislators"
#                      American Journal of Political Science
#
##############################################################################

# Note 1:
# This script should be run after completing scripts beginning with "01_"

# Note 2:
# To speed up replication time:
# 04_01_2_main_estimates_tab_2_col_4_boot_st_err
# 04_01_1_main_estimates_tab_2_col_3_boot_st_err
# 04_02_1_heterogeneity_tab_3_col_1_boot_st_err
# 04_02_2_heterogeneity_tab_3_col_2_boot_st_err
# 04_02_3_heterogeneity_tab_3_col_3_boot_st_err
# 04_02_4_heterogeneity_tab_3_col_4_boot_st_err
# 04_02_5_heterogeneity_tab_3_col_5_boot_st_err
# 04_02_6_heterogeneity_tab_3_col_6_boot_st_err
# 04_03_1_robustness_checks_tab_A2_col_1_boot_st_err
# 04_03_2_robustness_checks_tab_A2_col_2_boot_st_err
# 04_03_3_robustness_checks_tab_A2_col_3_boot_st_err
# 04_03_4_robustness_checks_tab_A2_col_4_boot_st_err
# 04_03_5_robustness_checks_tab_A2_col_5_boot_st_err
# 04_04_1_heterogeneity_appendix_tab_A5_col_1_boot_st_err
# 04_04_2_heterogeneity_appendix_tab_A5_col_2_boot_st_err
# 04_04_4_heterogeneity_appendix_tab_A5_col_4_boot_st_err
# 04_04_5_heterogeneity_appendix_tab_A5_col_5_boot_st_err
# 04_04_6_heterogeneity_appendix_tab_A5_col_6_boot_st_err
# 04_04_7_heterogeneity_appendix_tab_A5_col_7_boot_st_err
# 04_04_8_heterogeneity_appendix_tab_A5_col_8_boot_st_err
# 04_04_9_heterogeneity_appendix_tab_A5_col_9_boot_st_err
# 04_04_10_heterogeneity_appendix_tab_A5_col_10_boot_st_err
# 04_05_1_different_stages_tab_A7_col_1_boot_st_err
# 04_05_2_different_stages_tab_A7_col_2_boot_st_err
# 04_05_3_different_stages_tab_A7_col_3_boot_st_err
# 04_06_horse_race_tab_A3_col_9_boot_st_err
# 04_07_1_party_effects_ties_effects_timing_tab_A6_col_1_boot_st_err
# 04_07_2_party_effects_ties_effects_timing_tab_A6_col_2_boot_st_err
# 04_07_3_party_effects_ties_effects_timing_tab_A6_col_3_boot_st_err
# 04_07_4_party_effects_ties_effects_timing_tab_A6_col_4_boot_st_err
# by using 31 additional instances of R (i.e., one for each script)

# load data
load("replication_data.RData")
load("het_estimates_appendix.rda")

# load libraries
library(econet)

# Bootstrap standard errors
# WARNING: This may take a very long time to run (i.e. more than a month).
boot_het_estimates <- boot(object = het_estimates[[3]], 
                           hypothesis = "het_l", 
                           group = db$id_2, 
                           niter = 500, weights = TRUE)

#-----------------------------------------------------------------------------
# Print results table A.5
#-----------------------------------------------------------------------------

# print results column 3 table A.5
boot_het_estimates
