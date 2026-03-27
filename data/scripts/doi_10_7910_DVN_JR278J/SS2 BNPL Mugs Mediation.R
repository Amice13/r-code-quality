library(readxl)
BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN <- read_excel("C:/Users/Dionysius Ang/Desktop/DionDropbox/Dropbox/Appriss retail/Experiments/Stijn - Mugs/BNPL pay in 7, 14, 21 vs pay in 21_October 16, 2023_06.03-CLEAN.xlsx", 
                                                                           sheet = "Sheet1")
View(BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN)

#Parallel Mediation - BNPL >> PC + BC >> FC#

process(data = BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN, y="FC",x="BNPL",m=c("PC","BC"),boot = 10000, model = 4,describe=1,contrast=1,seed = 617624, listmiss=0)

#Reverse Causality - BNPL >> FC >> PC#

process(data = BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN, y="PC",x="BNPL",m=c("FC"),boot = 10000, model = 4,describe=1,contrast=1,seed = 617624, listmiss=0)


#Reverse Causality - BNPL >> FC >> BC#
process(data = BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN, y="BC",x="BNPL",m=c("FC"),boot = 10000, model = 4,describe=1,contrast=1,seed = 617624, listmiss=0)

str(BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN)





