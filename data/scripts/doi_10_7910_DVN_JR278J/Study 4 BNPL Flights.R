
#Serial Mediation BNPL >> PC BC >> FC >> PI#
process(data = BNPL_Flights_Study_4_, y="PI",x="BNPL",m=c("PC","BC","FC"),boot = 10000, model = 80,describe=1,contrast=1,seed = 617624, listmiss=0)

