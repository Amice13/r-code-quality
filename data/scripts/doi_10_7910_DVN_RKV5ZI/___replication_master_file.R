#############Territorial autonomy and the trade-off between civil and communal violence#############
#############Andreas Juon#############
#############American Political Science Review (APSR)#############
#############Replication Master File#############


##############1. Preliminaries##############

########1.1 Working directory########
rm(list=ls())
#please specify path to replication folder: setwd("PATH TO REPLICATION FOLDER")
#In RStudio, just run the following line:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

########1.2 Libraries and functions########
source("__libraries_functions.R")

########1.3 Variable definitions########
source("__variable_definitions.R")


##############2. Group/dyad-administrative unit-year analyses##############

########2.1 Import data########
main_group <- read.csv("../replication_data/group_unit_year.csv")
main_dyad <- read.csv("../replication_data/dyad_unit_year.csv")

########2.2 Main analyses########
#figure 1: constructed in yEd (see figure1.graphml)
#figure 2: constructed in yEd (see figure2.graphml)
source("_main_figure3_figures7.R")#figure 3 / figure S7
source("_main_table1_tablea5.R")#table 1 / table A5
source("_main_figure4.R")#figure 4

########2.3 Appendix 1########
source("appendix1_tablea1.R")#table A1
source("appendix1_tablea2.R")#table A2
source("appendix1_tablea3.R")#table A3
source("appendix1_tablea4.R")#table A4

########2.4 Appendix 3########
source("appendix3.1.R")#figures A1-A3 / tables X1-X3
source("appendix3.2.R")#figures A4-A5 / tables X4-X5
source("appendix3.3.R")#figure A6 / table X6
source("appendix3.4.R")#figure A7 / tables X7-X8
source("appendix3.5.R")#figure A8 / table X9
source("appendix3.7.1a.R")#figure A10 / tables X11-X12
source("appendix3.7.2a.R")#figure A13 / tables X15-X19
source("appendix3.7.2b.R")#figure A14 / tables X20-X23
source("appendix3.7.3a.R")#figure A15 / tables X24-X27
source("appendix3.7.3b.R")#figure A16 / table X28
source("appendix3.7.3c.R")#figure A17 / tables X29-X31
source("appendix3.7.3d.R")#figure A18 / tables X32-X34
source("appendix3.7.4.R")#figure A19 / tables X35-X36
source("appendix3.7.5.R")#figure A20 / tables X37-X38

########2.5 Appendix 5########
#figure A24: constructed in yEd (see figurea24.graphml)
source("appendix5.1.R")#figure 6 (panel a) / table A8
source("appendix5.5.R")#figure A26

########2.6 Supplement 2########
source("supplement2.5.R")#figures S8-S13
source("supplement2.6.R")#figures S14, S15, and S16 / table S5

########2.7 Supplement 3########
source("supplement3.R")#figures S17-S18 / table S7


##############3. Administrative unit-year analyses##############

########3.1 Import data########
main_unit <- read.csv("../replication_data/unit_year.csv")

########3.2 Main analyses########
source("appendix3.6.R")#figure A9 / table X10

########3.3 Supplement 1########
source("supplement1.R")#figures S1-S4 / tables S1-S2

########3.4 Supplement 2########
source("supplement2.1_2.4.R")#figures S5-S6 / tables S3-S4
source("supplement2.7.R")#table S6


##############4. Group/dyad-grid cell-year analyses##############

########4.1 Import data########
grid_group <- read.csv("../replication_data/group_grid_year.csv")
grid_dyad <- read.csv("../replication_data/dyad_grid_year.csv")

########4.2 Appendix 3.7 (b)########
source("appendix3.7.1b.R")#figure A11 / table X13

########4.3 Appendix 4########
#appendix4a.do#Stata script that produces IV model estimates and tables A6-A7
source("appendix4b.R")#figures 5, A21, A23 (based on Stata script referenced above)


##############5. Group/dyad-year analyses##############

########5.1 Import data########
country_group <- read.csv("../replication_data/group_year.csv")
country_dyad <- read.csv("../replication_data/dyad_year.csv")

########5.2 Appendix 3.7 (c)########
source("appendix3.7.1c.R")#figure A12 / table X14


##############6. Directed dyad-administrative unit-year analyses##############

########6.1 Import data########
main_ddyad <- read.csv("../replication_data/dyad_directed_unit_year.csv")

########6.2 Appendix 5.2########
source("appendix5.2.R")#figure 6 (panel b) / table A9


##############7. Individual-level analyses##############

########7.1 Import data########
main_individual <- read.csv("../replication_data/individual.csv")

########7.2 Appendix 5.3########
#table A10 manually produced, directly in Word
source("appendix5.3.R")#figure 6 (panel c) / table A11


##############8. Conflict-level analyses##############

########8.1 Import data########
conf_int <- read.csv("../replication_data/conflict_intervention.csv")
conf_pp <- read.csv("../replication_data/conflict_peace_processes.csv")

########8.2 Appendix 5.4########
source("appendix5.4.R")#figure A25 / table A12