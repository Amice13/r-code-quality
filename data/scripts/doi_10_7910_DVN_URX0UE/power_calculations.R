
library(powerAnalysis)
# Note on power.t from documentation:
# n is Number of observations (in the SMALLEST group if two groups)
# ratio is The ratio n2/n1 between the larger group and the smaller group. 
# Should be a value equal to or greater than 1 since n2 is the larger group. Defaults to 1 (equal group sizes). 

# Power for Control VS Anytreat
#20000 was target, then numbers are sample size used for different outcomes after attrition
N_target = 20000
N_knowledge = 18762
N_other = 18223
N_followup = 6217



#COVID control (no COVID info, as in Esther's email) has N*4k/20k, rest has N*16k/20k -> ratio = 1/4
power.t(es = NULL, n = N_target , power = 0.85, sig.level = 0.05,
        ratio = 1/4, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = N_knowledge , power = 0.85, sig.level = 0.05,
        ratio = 1/4, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = N_other , power = 0.85, sig.level = 0.05,
        ratio = 1/4, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = N_followup , power = 0.85, sig.level = 0.05,
        ratio = 1/4, type = "unequal",
        alternative = "two.sided")

# Power for any variation VS the rest within COVID treatment

#AMA race, info race, racism statement and heterogeneous impacts: split 8 VS 8 cells (1k per cell), thus N*8k/20k each -> type = "two", N = N * 16k/20k * 1/2 (N is per group for balanced samples)
power.t(es = NULL, n = 8 * N_target / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")
power.t(es = NULL, n = 8 * N_knowledge / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")
power.t(es = NULL, n = 8 * N_other / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")
power.t(es = NULL, n = 8 * N_followup / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")



###### THIS IS ADDITIONAL, MAIN SPECIFICATIONS FOR POWER ARE ABOVE

#Pure control (placebo AMA and no COVID info) has N*2k/20k, anytreat has N*18k/20k -> ratio = 1/9
power.t(es = NULL, n = N_target , power = 0.85, sig.level = 0.05,
        ratio = 1/9, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = N_knowledge , power = 0.85, sig.level = 0.05,
        ratio = 1/9, type = "unequal",
        alternative = "two.sided")

# Power for any variation VS the rest within any treatment

#A AMA race and info race: split 9k VS 9k, thus N*9k/18k each -> type = "two", N = N * 18k/20k * 1/2 (N is per group for balanced samples)
power.t(es = NULL, n = 9 * N_target / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")
power.t(es = NULL, n = 9 * N_knowledge / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1, type = "two",
        alternative = "two.sided")

#B AMA racism statement: split 10k VS 8k, thus N*10k/18k treatment and N*8k/18k control -> type = "two", Nt = N * 18k/20k * 5/9, Nc = N * 18k/20k * 4/9 (Nt is treatment size and Nc is control size)
power.t(es = NULL, n = 8 * N_target / 20 , power = 0.85, sig.level = 0.05,
        ratio = 4/5, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = 8 * N_knowledge / 20 , power = 0.85, sig.level = 0.05,
        ratio = 4/5, type = "unequal",
        alternative = "two.sided")

#C COVID info: split 16k VS 2k, thus N*16k/18k treatment and N*2k/18k control -> type = "two", Nt = N * 18k/20k * 8/9, Nc = N * 18k/20k * 1/9 (Nt is treatment size and Nc is control size)
power.t(es = NULL, n = 2 * N_target / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1/8, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = 2 * N_knowledge / 20 , power = 0.85, sig.level = 0.05,
        ratio = 1/8, type = "unequal",
        alternative = "two.sided")

#D heterogeneous impacts: split 8k VS 10k, thus N*8k/18k treatment and N*10k/18k control -> type = "two", Nt = N * 18k/20k * 4/9, Nc = N * 18k/20k * 5/9 (Nt is treatment size and Nc is control size)
power.t(es = NULL, n = 8 * N_target / 20 , power = 0.85, sig.level = 0.05,
        ratio = 4/5, type = "unequal",
        alternative = "two.sided")
power.t(es = NULL, n = 8 * N_knowledge / 20 , power = 0.85, sig.level = 0.05,
        ratio = 4/5, type = "unequal",
        alternative = "two.sided")
