### META-ANALYSES SPRAYS FOR DRY MOUTH ###

# 1. XEROSTOMIA MEASURED WITH DMQ #


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES 

library(readxl)
pair_xerostomia_dmq <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/xerostomia DMQ/pair_xerostomia_dmq.xlsx")
View(pair_xerostomia_dmq)


# LOADING PAIRED ANALYSIS PACKAGES

library(metafor)

library(meta)


# ANALYSING PAIRS

table(pair_xerostomia_dmq$par)


# DEFINING PAIRS

pair_CA <- subset(pair_xerostomia_dmq, par == "CA")

pair_CA


meta_CA <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_CA,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_CA

forest(meta_CA,
       fontfamily="Times", fontsize=12, xlim=c(-3, 3), 
       comb.fixed=FALSE, col.square="darkblue", col.diamond = "black",
       lab.e="1% malic acid spray", lab.c="Placebo",
       label.left="Placebo", label.right="1% malic acid spray")


# 2. XEROSTOMIA MEASURED WITH 10 CM VAS #


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES

library(readxl)
pair_xerostomia_vas <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/xerostomia VAS/pair_xerostomia_vas.xlsx")
View(pair_xerostomia_vas)

# ANALYSING PAIRS

table(pair_xerostomia_vas$par)

# DEFINING PAIRS

pair_IB <- subset(pair_xerostomia_vas, par == "IB")

pair_IB


meta_IB <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_IB,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_IB

forest(meta_IB,
       fontfamily="Times", fontsize=12, xlim=c(-6, 6), 
       comb.fixed=FALSE, col.square="darkblue", col.diamond = "black",
       lab.e="OGT", lab.c="Saliveze",
       label.left="OGT", label.right="Saliveze")


######################################################################################################
  
  
# 3. STIMULATED SALIVARY FLOW #


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES
  
library(readxl)
pair_stimulated_flow <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/stimulated salivary flow/pair_stimulated_flow.xlsx")
View(pair_stimulated_flow)


# ANALYSING PAIRS

table(pair_stimulated_flow$par)


# DEFINING PAIRS

pair_CA <- subset(pair_stimulated_flow, par == "CA")

pair_CA


# PAIRED META-ANALYSIS

meta_CA <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_CA,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_CA

forest(meta_CA,
       fontfamily="Times", fontsize=12, xlim=c(-0.6, 0.6), 
       comb.fixed=FALSE, col.square="darkblue", col.diamond = "black",
       lab.e="1% malic acid spray", lab.c="Placebo",
       label.left="Placebo", label.right="1% malic acid spray")


############################################################################################################################


# 4. ANALYSIS OF UNSTIMULATED SALIVARY FLOW #


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES

library(readxl)
mtc_unstimulated_flow <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/unstimulated salivary flow/mtc_unstimulated_flow.xlsx")
View(mtc_unstimulated_flow)


# LOADING PACKAGES FOR MTC

library(coda)

library(gemtc)

library(rjags)


# BUILDING DATAFRAME

data <- as.data.frame(mtc_unstimulated_flow)

data

treatments <- read.table(textConnection('
                                         id  description
                                         A   "Placebo"
                                         C   "1% malic acid spray"
                                         E   "P.emblica spray"
                                         F   "10% trehalose spray"'), header=TRUE)


# VISUALIZING NETWORK OF STUDIES PER TREATMENT


network <- mtc.network(data, description="sprays", treatments=treatments)

network

summary(network)

plot(network)


# IMPORTING DATASET FOR PAIRED ANALYSIS


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES

library(readxl)
pair_unstimulated_flow <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/unstimulated salivary flow/pair_unstimulated_flow.xlsx")
View(pair_unstimulated_flow)


# LOADING PACKAGES FOR PAIRED META-ANALYSIS

library(metafor)

library(meta)


# ANALYSING PAIRS

table(pair_unstimulated_flow$par)

summary(network)


# DEFINING PAIRS

pair_CA <- subset(pair_unstimulated_flow, par == "CA")

pair_CA


pair_EA <- subset(pair_unstimulated_flow, par == "EA")

pair_EA


pair_FA <- subset(pair_unstimulated_flow, par == "FA")

pair_FA


# PAIRED META-ANALYSIS


meta_CA <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_CA,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_CA

forest(meta_CA,
       fontfamily="Times", fontsize=12, xlim=c(-0.2, 0.2), 
       comb.fixed=FALSE, col.square="darkblue",
       lab.e="1% malic acid spray", lab.c="Placebo",
       label.left="Placebo", label.right="1% malic acid spray")




meta_EA <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_EA,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_EA

forest(meta_EA,
       fontfamily="Times", fontsize=12, xlim=c(-0.2, 0.2), 
       comb.fixed=FALSE, col.square="darkblue",
       lab.e="P. emblica spray", lab.c="Placebo",
       label.left="Placebo", label.right="P. emblica spray")




meta_FA <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, data=pair_FA,
                    method.tau="DL", sm="MD", subgroup=par, common=FALSE,
                    studlab=Author, backtransf=TRUE) 
meta_FA

forest(meta_FA,
       fontfamily="Times", fontsize=12, xlim=c(-0.4, 0.4), 
       comb.fixed=FALSE, col.square="darkblue",
       lab.e="10% trehalose spray", lab.c="Placebo",
       label.left="Placebo", label.right="10% trehalose spray")



# COMBINED PAIRED META-ANALYSIS

meta_pairs <- metacont(t_n, t_media, t_dp, c_n, c_media, c_dp, 
                       data=pair_unstimulated_flow,
                       method.tau="DL", sm="MD", subgroup=par, common=FALSE, 
                       studlab=Author, backtransf = TRUE)

meta_pairs

forest(meta_pairs,
       overall=FALSE, fontfamily="Times", fontsize=12, xlim=c(-0.3, 0.3),
       test.effect.subgroup = TRUE, common=FALSE, col.square="darkblue", col.diamond = "black",
       lab.e="Treatment 1", lab.c="Treatment 2",
       label.left="Favours 2", label.right="Favours 1")



#### Save as PDF > 15 x 25 > forest-meta_pairs > format Portrait



# CALCULATING HETEROGENEITY - WITH TAU´S CI

# TAU FOR 'EA' AND 'FA' COMPARISONS WAS NOT CALCULATED


tau_CA<-rma(m1i=t_media, m2i=c_media, sd1i=t_dp, sd2i=c_dp, n1i=t_n, n2i=c_n,
            data=par_CA, measure="MD")
tau_CA

predict(tau_CA, transf=exp, addx = TRUE)
confint(tau_CA)


########################################################################################


# PUBLICATION BIAS ANALYSIS BY FUNNEL PLOT WAS NOT ASSESSED DUE TO THE NUMBER OF STUDIES


########################################################################################


# DEFINING NETWORK MODEL AND STATISTICAL PARAMETERS


# LOADING PACKAGES FOR MTC

library(coda)

library(gemtc)

library(rjags)


# Setting seed for a random number

set.seed(1234)


# Effect model

modelo_R=mtc.model(network,likelihood="normal",link="identity",type="consistency",
                   linearModel="random",n.chain=4)

resultR <- mtc.run(modelo_R, n.adapt=2000, n.iter=20000, thin=50)

test=mcmc.list(resultR$samples)

autocorr.plot(test)

par(mfrow=c(3,3))

traceplot(test)


# Convergency test

gelman.plot(test)

gelman.diag(test)

gelman.diag(resultR)$mpsrf

#### Overall Potential Scale Reduction Factor must be < 1.05



######################################################################################



# Setting seed for a fixed number

set.seed(1234)


# Effect model

model_F=mtc.model(network,likelihood="normal",link="identity",type="consistency",
                  linearModel="fixed", n.chain=4)


# Fixed test

resultF <- mtc.run(model_F, n.adapt=2000, n.iter=20000, thin=50)

test=mcmc.list(resultF$samples)

autocorr.plot(test)


# Convergency by traceplots

par(mfrow=c(3,3))

traceplot(test)


# Avaliar converg?ncia

gelman.plot(test)

gelman.diag(test)

gelman.diag(resultF)$mpsrf 

#### Overall Potential Scale Reduction Factor must be < 1.05

#### Error in plot.new() : figure margins too large


# Summary - DIC analysis (RANDOM and FIXED)

summary(resultR)

summary(resultF)


# Inconsistency analysis

### Nodesplit plots

mtc.nodesplit.comparisons(network)

set.seed(1234)


split=mtc.nodesplit(network, comparisons=mtc.nodesplit.comparisons(network),
                    linearModel="fixed",n.adapt = 2000, n.iter=20000, thin=50) 

#### ERROR (Session - Restart R - Packages - Restart Node-Split)
# If there´s no error, go to > names(split)
# Ctrl + Shift + F10


# LOADING PACKAGES FOR MTC

library(coda)

library(gemtc)

library(rjags)


set.seed(1234)

split <- mtc.nodesplit(network, comparisons=mtc.nodesplit.comparisons(network),
                       linearModel="fixed",n.adapt=2000, n.iter=20000, thin=50)

names(split)

summary.ns <- summary(split)

print(summary.ns)

plot(summary.ns)


# NO INCONSISTENCY WAS FOUND IN THE ANALYSIS


#### Forest plot

par(mfrow=c(1,1))

forest(relative.effect(resultadoF, "A"))

#### ERROR (Session - Restart R - Packages - Restart Forest)
# If there´s no error, go to > forest(relative.effect(resultadoR, "B"))
# Ctrl + Shift + F10

library (readxl)

library(gemtc)

library(rjags)


# SEARCH FOR THE FILE WHERE YOU PASTED IT IN YOUR COMPUTER. PASTE THE PATH FOR FUTURE ACCESSES

library(readxl)
pair_unstimulated_flow <- read_excel("C:/Users/Sinval JR/OneDrive/Área de Trabalho/SUBMISSION BDJ/SUBMISSION/DATA - 02-12-2025/unstimulated salivary flow/pair_unstimulated_flow.xlsx")
View(pair_unstimulated_flow)

modelo_R=mtc.model(network,likelihood="normal",link="identity",type="consistency",
                   linearModel="random", n.chain=4)

resultR <- mtc.run(modelo_R, n.adapt=2000, n.iter=20000, thin=50)


par(mfrow=c(1,1))

forest(relative.effect(resultR, "A"))

forest(relative.effect(resultR, "C"))

forest(relative.effect(resultR, "E"))

forest(relative.effect(resultR, "F"))


######################################################################################
#### Table of relative effects

table=(relative.effect.table(resultR))

table=round(table,digits=3)

table



###################################################################################### 
#### Ranking of treatments

#### > Positive outcome: 1 > Negative outcome > -1


rank=rank.probability(resultF, preferredDirection=1)

rank


######################################################################################
#### SUCRA

a=4

Sucra=rep(0,a)

Sucra

acumulados=matrix(rep(0,a*(a-1)),ncol=(a-1))

for (j in 1:a) {
  aux=0
  for (b in 1:(a-1)) {
    aux=aux+rank[j,b]
    acumulados[j,b]=aux
  }
  
  Sucra[j]=sum(acumulados[j,])
}

Sucra=Sucra/(a-1)

Sucra

######################################################################################################