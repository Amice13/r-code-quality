library(readxl)
BNPL_pay_in_7_14_21_vs_pay_in_21_October_16_2023_06_03_CLEAN <- read_excel("library(readxl)
BNPL_Coffee <- read_excel("C:/Users/Dionysius Ang/Desktop/DionDropbox/Dropbox/Appriss retail/Experiments/Dion - Coffee/BNPL - Coffee.xlsx")
View(BNPL_Coffee)")
)

BNPL_Coffee$SuggestC <- as.numeric(BNPL_Coffee$SuggestC)
BNPL_Coffee$PayC <- as.numeric(BNPL_Coffee$PayC)

#Parallel Mediation - PayC x SuggestC >> PC + FC >> PI#

process(data = BNPL_Coffee, y="PI",x="PayC",w ="SuggestC", m=c("PC","FC"),boot = 10000, model = 8,describe=1,contrast=1,seed = 617624, listmiss=0)

#Parallel Mediation - PayC >> PC + FC >> PI#

process(data = BNPL_Coffee, y="PI",x="PayC",m=c("PC","FC"),boot = 10000, model = 4,describe=1,contrast=1,seed = 617624, listmiss=0)
