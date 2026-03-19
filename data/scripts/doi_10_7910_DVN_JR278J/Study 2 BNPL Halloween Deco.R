
#Mediation BNPL >> FC >> Items Model 4.
process(data = BNPL_Halloween_Deco_Study_2_, y="Items",x="BNPL",m=c("FC"),boot = 10000, model = 4,describe=1, stand=1,contrast=1,seed = 617624, listmiss=0)


#Mediation BNPL >> FC >> Spend Amt Model 4.
process(data = BNPL_Halloween_Deco_Study_2_, y="SAmt",x="BNPL",m=c("FC"),boot = 10000, model = 4,describe=1, stand=1,contrast=1,seed = 617624, listmiss=0)
