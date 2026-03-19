##################
# Replication material:
# Unity in Diversity? The Development of Political Parties in the Parliament of Canada (1867 - 2011).
# Jean-François Godbout and Bjørn Høyland
# British Journal of Political Science
#######################################
# 1) Datasets:
#     a) loyaltyLib.csv - Members' voting loyalty Liberal Party 
#     b) loyaltyCon.csv - Members' voting loyalty Conservative Party
#     c) libs.csv - Party Unity Liberal Party
#     d) cons.csv - Party Unity Conservative Party
##############
# 2) Main analysis:
#     a) Loyal.r - Analysis of MPs voting loyalty (pooled over votes)
#     b) Unity.r - Analysis of party unity (pooled over MPs)
# set to working library
rm(list=ls())
source("Figure-1.r")
source("Loyal.r") 
source("Unity.r") 
save.image("estimates.RData") 
# These files produces the main regression tables and store the resulting estimates in an .RData file
##########
# 3) On the basis of the .RData file, we make the following Figures
source("substantiveEffects.R")
source("substantiveVotes.R") 
source("substantiveBackground.R")
##########
# 4) Supplementary analysis
source("loyalGov.r") 
source("UnityAbstentions.r") 
save.image("estimatesGov.RData")
# 5) Other Figures in the Appendix
source("figuresAppendix.r") 
source("substativeVotesCon.r") 
source("loyaltyplot.r") 

