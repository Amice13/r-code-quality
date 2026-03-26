######################
# MONTE CARLO EXPERIMENTS
#
# ALL EXPERIMENTS
#
# ### DO NOT RUN THIS FILE - IT NEEDS "DEEP" INPUT DATA NOT INCLUDED IN THE 
# ### REPLICATION DATAVERVSE - FINAL RESULTS ARE SAVED UNDER data/monte_carlo_res/*
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/replication.R
#
#############################



# MONTE CARLO SIMULATIONS ###

# Burnin
print(paste(Sys.time(), "MC: BURNIN", " ###################"))
try(source("scripts/monte_carlos/mc_burnin.R"))

# Size and Instances
print(paste(Sys.time(), "MC: SIZE & INSTANCES", " ###################"))
try(source("scripts/monte_carlos/mc_size.R"))

# Size and Instances
print(paste(Sys.time(), "MC: BOOTSTRAPPED SEs", " ###################"))
try(source("scripts/monte_carlos/mc_bootse.R"))




