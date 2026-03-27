# R script for:
# Ingo Rohlfing: "Mind the gap: A review of simulation designs for QCA"
# doi: 10.1177/2053168015623562
# Typescript
#
# Machine: Lenovo T430
# OS: Windows 7
# R: 3.2.2
# R Studio: 0.99.473

# install.packages("QCA",dependencies=T) # Install QCA package if necessary
library(QCA)
setwd("e:/data/qca/replication") # setwd("") Specify your working directory

##############
# Manuscript #
##############

### Table 1
# Table 1 is a manual combination of two truth tables with different minimum inclusion thresholds and assignment of values to Y
koenig <- read.csv("koenig_2004.csv", sep=";",header=T,as.is=T) # Loading original data
koenigTT <- truthTable(koenig,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
              incl.cut1=1,sort.by=c("incl","n"),complete=T) # Original truth table
koenigTT_max74 <- truthTable(koenig,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
                    incl.cut1=1,incl.cut0=0.74,sort.by=c("incl","n"),complete=T) # Modified truth table

### Table 2
inclusion <- matrix(nrow=3,ncol=6) # Matrix for summary of counts
dimnames(inclusion) <- list(c("n=1","n=2","n=3-7"),
                            c("Ahn/Lee (>0.74)","Ahn/Lee (all)","Samford (>0.74)","Samford (all)","Koenig-Archibugi (>0.74)","Koenig-Archibugi (all)"))
### Ahn/Lee
# Model 2, n cut-offs 1 to 7
ahnlee_freq <- read.csv("model_2_sim.csv", as.is=T)
ahnlee_freq$Configurations <- gsub("[\n ]+", "", ahnlee_freq$Configurations) # Cleaning line breaks
inclusion[1,1] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 1"& ahnlee_freq$incl.cut1.val>=0.75]))
inclusion[1,2] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 1"]))
inclusion[2,1] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 2"& ahnlee_freq$incl.cut1.val>=0.75]))
inclusion[2,2] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 2"]))
inclusion[3,1] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 3"& ahnlee_freq$incl.cut1.val>=0.75]))
inclusion[3,2] <- dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 3"]))
# Numbers not inserted in table because no difference to frequency threshold of 3
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 4"]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 4"& ahnlee_freq$incl.cut1.val>=0.75]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 5"]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 5"& ahnlee_freq$incl.cut1.val>=0.75]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 6"]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 6"& ahnlee_freq$incl.cut1.val>=0.75]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 7"]))
dim(table(ahnlee_freq$Configurations[ahnlee_freq$n.cut=="Frequency Threshold = 7"& ahnlee_freq$incl.cut1.val>=0.75]))

### Samford
# n cut-offs 1 to 2. Higher cut-offs not analyzed because inclusion scores barely > 0.75 (n=3) or below it (n=4, n=5)
samford_freq <- read.csv("model_sim.csv",as.is=T)
samford_freq$Configurations <- gsub("[\n ]+", "", samford_freq$Configurations) # Cleaning line breaks
inclusion[1,3] <- dim(table(samford_freq$Configurations[samford_freq$n.cut=="Frequency Threshold = 1"& samford_freq$incl.cut1.val>=0.75]))
inclusion[1,4] <- dim(table(samford_freq$Configurations[samford_freq$n.cut=="Frequency Threshold = 1"]))
inclusion[2,3] <- dim(table(samford_freq$Configurations[samford_freq$n.cut=="Frequency Threshold = 2"& samford_freq$incl.cut1.val>=0.75]))
inclusion[2,4] <- dim(table(samford_freq$Configurations[samford_freq$n.cut=="Frequency Threshold = 2"]))

### Koenig-Archibugi
# Although unprocessed data does not reproduce Koenig-Archibugi's solution (see appendix), I use this dataset for reasons of comparability
# n cut-offs 1 to 2
koenig_freq <- read.csv("ka_model_1_sim.csv",as.is=T)
koenig_freq$Configurations <- gsub("[\n ]+", "", koenig_freq$Configurations) # Cleaning line breaks
inclusion[1,5] <- dim(table(koenig_freq$Configurations[koenig_freq$n.cut=="Frequency Threshold = 1"& koenig_freq$incl.cut1.val>=0.75]))
inclusion[1,6] <- dim(table(koenig_freq$Configurations[koenig_freq$n.cut=="Frequency Threshold = 1"]))
inclusion[2,5] <- dim(table(koenig_freq$Configurations[koenig_freq$n.cut=="Frequency Threshold = 2"& koenig_freq$incl.cut1.val>=0.75]))
inclusion[2,6] <- dim(table(koenig_freq$Configurations[koenig_freq$n.cut=="Frequency Threshold = 2"]))

inclusion # Reproducing table 2

### Table 3
reprodsucc <- matrix(nrow=3,ncol=3) # Matrix for summary of reproduction successes
dimnames(reprodsucc) <- list(c("Success model 1 (%)","Success model 2 (%)","Success model 3 (%)"),c("Ahn/Lee","Samford","Koenig-Archibugi"))

### Ahn, Sang-Hoon and Sophia Seung-yoon Lee (2012): Explaining Korean Welfare State Development with New Empirical Data and Methods. Asian Social Work and Policy Review 6 (2): 67-85.
# Reproducing original analysis
ahnlee <- read.csv("e:/data/qca/replication/CWS_data_extended_KOR.csv", sep=",") # Importing full CWS data
ahnlee <- ahnlee[which(ahnlee$ID=="KOR" & ahnlee$YEAR>=1964),] # Extracting Korea data
ahnlee <- subset(ahnlee,select=c(SOCX,CGDP,STUNEMR,PO65NEW,POPNEW)) # Extracting relevant variables
ahnlee$SOCX <- calibrate(ahnlee$SOCX,type="fuzzy",thresholds=c(1,3,7),logistic=T,idm=0.95)
ahnlee$CGDP <- calibrate(ahnlee$CGDP,type="fuzzy",thresholds=c(2104,9560,20000),logistic=T,idm=0.95)
ahnlee$STUNEMR <- calibrate(ahnlee$STUNEMR,type="fuzzy",thresholds=c(2.4,3.75,5),logistic=T,idm=0.95)
ahnlee$ELDERLY <- 100*ahnlee$PO65NEW/ahnlee$POPNEW
ahnlee$ELDERLY <- calibrate(ahnlee$ELDERLY,type="fuzzy",thresholds=c(3.39,5.34,9.5),logistic=T,idm=0.95)
ahnlee <- na.omit(ahnlee) # Dropping missings because QCA package requires complete data
ahnleeTT <- truthTable(ahnlee,outcome="SOCX",conditions=c("CGDP","STUNEMR","ELDERLY"),n.cut=1,
              incl.cut1=0.85,sort.by=c("incl","n"))
ahnleePS <- eqmcc(ahnleeTT,outcome="OUT",include="?",explain="1",details=T)
# Original result cannot be replicated. Personal correspondence with Lee: KCP dataset is too short on the temporal dimension

# Simulating inclusion of irrelevant condition
models_ahnlee <- list() # Capturing models
modelsno_ahnlee <- vector() # Capturing number of models
modelsnone <- c("None") # Entry for models_ahnlee if no truth table row with Y=1
modelszero <- 0 # Entry for modelszero if no truth table row with Y=1
termlist_ahnlee <- list() # Capturing terms of models
set.seed(111) # set.seed for reproducibility
for (i in 1:1000){
  ahnlee$c <- runif(n=nrow(ahnlee), min=0, max=1) # Generating random condition
  ahnleeTT <- truthTable(ahnlee,outcome="socx",conditions=c("cgdp","stunemr","elderly","c"),n.cut=1,
                incl.cut1=0.85,sort.by=c("incl","n"))
  models <- list()
  if(max(as.numeric(ahnleeTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_ahnlee <- c(models_ahnlee,modelsnone)
    modelsno_ahnlee <- c(modelsno_ahnlee,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    ahnleePS <- eqmcc(ahnleeTT,outcome="OUT",include="?",explain="1") # Generating solution
    modelsno_ahnlee <- c(modelsno_ahnlee,length(ahnleePS$solution))
    for (m in 1:length(ahnleePS$solution)) { # Loop over all models in solution
      if (length(ahnleePS$solution[[m]])==1) { # Model without equifinality
        models_ahnlee <- c(models_ahnlee,ahnleePS$solution[[m]])
      }
      if (length(ahnleePS$solution[[m]])>1) { # Model with equifinality
        models <- ahnleePS$solution[[c(m,1)]] # Extracting first term of model
        for (p in 2:length(ahnleePS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,ahnleePS$solution[[c(m,p)]],sep="+") # Reconstructing model with equifinality
        }
      }
      models_ahnlee <- c(models_ahnlee,models) # Concatenating models
    }
    termlist_ahnlee <- c(termlist_ahnlee, ahnleePS$solution)
  }
}

summodel_ahnlee <- sum(modelsno_ahnlee) # Total number of models
reprodmodel_ahnlee <- 100*round(length(which(models_ahnlee=="CGDP"))/summodel_ahnlee, digits=4) # Share of models identical to original models
reprodsucc[1,1] <- reprodmodel_ahnlee

### Samford, Steven (2010): Averting "Disruption and Reversal": Reassessing the Logic of Rapid Trade Reform in Latin America. Politics & Society 38 (3): 373-407.
# Reproducing original analysis
samford <- read.csv("Samford2010set.csv", sep=",",header=T) # Loading Samford data
samfordTT <- truthTable(samford,outcome="raplib",conditions=c("execunco","switcher","devalu","grostron","hyperinf","groweak","manufac"),
                        incl.cut1=0.75,n.cut=2,sort.by=c("incl","n"))
(samfordPS <- eqmcc(samfordTT,outcome="OUT",explain="1",include="?",details=T)) # Parsimonious solution

# Simulating inclusion of irrelevant condition
models_samford <- list() # Capturing models
modelsno_samford <- vector() # Capturing number of models
modelsnone <- c("None") # Entry for models_samford if no truth table row with Y=1
modelszero <- 0 # Entry for modelszero if no truth table row with Y=1
termlist_samford <- list() # Capturing terms of models
set.seed(222) # set.seed for reproducibility
for (i in 1:1000){
  samford$c <- runif(n=nrow(samford), min=0, max=1) # Generating random condition
  samfordTT <- truthTable(samford,outcome="raplib",conditions=c("execunco","switcher","devalu","grostron","hyperinf","groweak","manufac","c"),
                          incl.cut1=0.75,n.cut=2,sort.by=c("incl","n"))
  models <- list()
  if(max(as.numeric(samfordTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_samford <- c(models_samford,modelsnone)
    modelsno_samford <- c(modelsno_samford,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    samfordPS <- eqmcc(samfordTT,outcome="OUT",explain="1",include="?") # Generating solution
    modelsno_samford <- c(modelsno_samford,length(samfordPS$solution))
    for (m in 1:length(samfordPS$solution)) { # Loop over all models in solution
      if (length(samfordPS$solution[[m]])==1) { # Model without equifinality
        models_samford <- c(models_samford,samfordPS$solution[[m]])
      }
      if (length(samfordPS$solution[[m]])>1) { # Model with equifinality
        models <- samfordPS$solution[[c(m,1)]] # Extracting first term of model
        for (p in 2:length(samfordPS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,samfordPS$solution[[c(m,p)]],sep="+") # Reconstructing model with equifinality
        }
      }
      models_samford <- c(models_samford,models) # Concatenating models
    }
    termlist_samford <- c(termlist_samford, samfordPS$solution)
  }
}

summodel_samford <- sum(modelsno_samford) # Total number of models
reprodmodel_samford_1 <- 100*round(length(which(models_samford=="EXECUNCO*DEVALU"))/summodel_samford, digits=4) # Share of models identical to original models
reprodmodel_samford_2 <- 100*round(length(which(models_samford=="EXECUNCO*HYPERINF"))/summodel_samford, digits=4) # Share of models identical to original models
reprodmodel_samford_3 <- 100*round(length(which(models_samford=="EXECUNCO*grostron*MANUFAC"))/summodel_samford, digits=4) # Share of models identical to original models
reprodsucc[1,2] <- reprodmodel_samford_1
reprodsucc[2,2] <- reprodmodel_samford_2
reprodsucc[3,2] <- reprodmodel_samford_3

### Koenig-Archibugi, Mathias (2004): Explaining Government Preferences for Institutional Change in Eu Foreign and Security Policy. International Organization 58 (1): 137-174.
# Reproducing original analysis using modified data (see appendix)
koenigsa <- read.csv("koenig_2004sa.csv", sep=";",header=T) # Loading manually modified data (see appendix)
koenigsaTT <- truthTable(koenigsa,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
                incl.cut1=1,sort.by=c("incl","n"))
(koenigsaCS <- eqmcc(koenigsaTT, include="1", explain="1")) # Conservative solution

# Demonstration that original data does not lead to Koenig-Archibugi's solution
koenig <- read.csv("koenig_2004.csv", sep=";",header=T) # Loading original data
koenigTT <- truthTable(koenig,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
              incl.cut1=1,sort.by=c("incl","n"),complete=T)
(koenigCS <- eqmcc(koenigTT, include="1", explain="1")) # Conservative solution
(koenigPS <- eqmcc(koenigTT, include="?", explain="1")) # Parsimonious solution

# Simulating inclusion of irrelevant condition
models_koenigsa <- list() # Captures models
modelsno_koenigsa <- vector() # Captures number of models per simulated condition
modelsnone <- c("None") # Entry for models_koenigsa if no truth table row with Y=1
modelszero <- 0 # Entry for models_koenigsa if no truth table row with Y=1
termlist_koenigsa <- list() # Captures terms of models
set.seed(333) # set.seed for reproducibility
for (i in 1:1000) { # Runs 1000 simulations
  koenigsa$c <- runif(n=nrow(koenigsa), min=0, max=1) # Generates random condition
  koenigsaTT <- truthTable(koenigsa,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities","c"),
                           incl.cut1=1,sort.by=c("incl","n"))
  models <- list() # Placeholder for internal purposes
  if(max(as.numeric(koenigsaTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_koenigsa <- c(models_koenigsa,modelsnone)
    modelsno_koenigsa <- c(modelsno_koenigsa,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    koenigsaCS <- eqmcc(koenigsaTT,outcome="OUT",explain="1",include="1") # Generates solution
    modelsno_koenigsa <- c(modelsno_koenigsa,length(koenigsaCS$solution)) # Assigns number of models
    for (m in 1:length(koenigsaCS$solution)) { # Loop over all models in solution
      if (length(koenigsaCS$solution[[m]])==1) { # Model without equifinality
        models_koenigsa <- c(models_koenigsa,koenigsaCS$solution[[m]]) # Assigns model
      }
      if (length(koenigsaCS$solution[[m]])>1) { # Model with equifinality
        models <- koenigsaCS$solution[[c(m,1)]] # Extracts first term of model
        for (p in 2:length(koenigsaCS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,koenigsaCS$solution[[c(m,p)]],sep="+") # Extracts model(s) with equifinality
        }
      }
      models_koenigsa <- c(models_koenigsa,models) # Assigns model(s)
    }
    termlist_koenigsa <- c(termlist_koenigsa, koenigsaCS$solution) # Assigns individual term(s)
  }
}

summodel_koenigsa <- sum(modelsno_koenigsa) # Total number of models
reprodmodel_koenigsa <- 100*round(length(which(models_koenigsa=="CONFORMITY*REGIONALISM+IDMASS*REGIONALISM*capabilities"))/summodel_koenigsa, digits=4) # Share of models identical to original models
reprodsucc[1,3] <- reprodmodel_koenigsa
reprodsucc # Producing table 3

### Optional: Reproduction success per sufficient term
#termunlist_koenigsa <- unlist(termlist_koenigsa)
#sumterms_koenigsa <- length(termunlist_koenigsa)
#reprodterm1_koenigsa <- 100*round(length(which(termunlist_koenigsa=="CONFORMITY*REGIONALISM"))/sumterms_koenigsa,digits=4)
#reprodterm2_koenigsa <- 100*round(length(which(termunlist_koenigsa=="IDMASS*REGIONALISM*capabilities"))/sumterms_koenigsa, digits=4)

############
# Appendix #
############

### Table 1
koenig50TT <- truthTable(koenig,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
                incl.cut1=0.5,sort.by=c("incl","n"),complete=F)
(koenig50CS <- eqmcc(koenig50TT, include="1", explain="1")) # Solution with model ambiguity
koenig50CS$PIchart # Reproducing table 1

# Histograms representing mdoel ambiguity
hist(modelsno_ahnlee,xlab="",xaxt="n",xlim=c(min(modelsno_ahnlee),max(modelsno_ahnlee)),freq=F,main="") # Ahn/Lee
axis(1,at=seq(min(modelsno_ahnlee),max(modelsno_ahnlee)),labels=seq(min(modelsno_ahnlee),max(modelsno_ahnlee),1)) # Axis labels for Ahn/Lee
hist(modelsno_samford,xlab="",xaxt="n",xlim=c(min(modelsno_samford),max(modelsno_samford)),freq=F,main="") # Samford
axis(1,at=seq(min(modelsno_samford),max(modelsno_samford),1),labels=seq(min(modelsno_samford),max(modelsno_samford),1)) # Axis labels for Samford
hist(modelsno_koenigsa,xlab="",xaxt="n",xlim=c(min(modelsno_koenigsa),max(modelsno_koenigsa)),freq=F,main="") # Koenig-Archibugi
axis(1,at=seq(min(modelsno_koenigsa),max(modelsno_koenigsa),1),labels=seq(min(modelsno_koenigsa),max(modelsno_koenigsa),1)) # Axis labels for Koenig-Archibugi

### Table 2
koenigTT # Truth table for Koenig-Archibugi
(koenigCS <- eqmcc(koenigTT, include="1", explain="1")) # Conservative solution for original Koenig-Archibugi data
(koenigPS <- eqmcc(koenigTT, include="?", explain="1")) # Parsimonious solution for original Koenig-Archibugi data
koenigPS$SA$M1 # Simplifying assumptions made for parsominious solution. Koenig-Archibugi only makes simplifying assumption about row 11 (2004: 162).

### Table 3
reprodsucc <- matrix(nrow=3,ncol=3) # Matrix for summary of reproduction successes
dimnames(reprodsucc) <- list(c("Success model 1 (%)","Success model 2 (%)","Success model 3 (%)"),c("Ahn/Lee","Samford","Koenig-Archibugi"))

### Ahn, Sang-Hoon and Sophia Seung-yoon Lee (2012): Explaining Korean Welfare State Development with New Empirical Data and Methods. Asian Social Work and Policy Review 6 (2): 67-85.
# Reproducing original analysis
ahnlee <- read.csv("e:/data/qca/replication/CWS_data_extended_KOR.csv", sep=",") # Importing full CWS data
ahnlee <- ahnlee[which(ahnlee$ID=="KOR" & ahnlee$YEAR>=1964),] # Extracting Korea data
ahnlee <- subset(ahnlee,select=c(SOCX,CGDP,STUNEMR,PO65NEW,POPNEW)) # Extracting relevant variables
ahnlee$SOCX <- calibrate(ahnlee$SOCX,type="fuzzy",thresholds=c(1,3,7),logistic=T,idm=0.95)
ahnlee$CGDP <- calibrate(ahnlee$CGDP,type="fuzzy",thresholds=c(2104,9560,20000),logistic=T,idm=0.95)
ahnlee$STUNEMR <- calibrate(ahnlee$STUNEMR,type="fuzzy",thresholds=c(2.4,3.75,5),logistic=T,idm=0.95)
ahnlee$ELDERLY <- 100*ahnlee$PO65NEW/ahnlee$POPNEW
ahnlee$ELDERLY <- calibrate(ahnlee$ELDERLY,type="fuzzy",thresholds=c(3.39,5.34,9.5),logistic=T,idm=0.95)
ahnlee <- na.omit(ahnlee) # Dropping missings because QCA package requires complete data
ahnleeTT <- truthTable(ahnlee,outcome="SOCX",conditions=c("CGDP","STUNEMR","ELDERLY"),n.cut=1,
                       incl.cut1=0.85,sort.by=c("incl","n"))
ahnleePS <- eqmcc(ahnleeTT,outcome="OUT",include="?",explain="1",details=T)
# Original result cannot be replicated. Personal correspondence with Lee: KCP dataset is too short on the temporal dimension

# Simulating inclusion of irrelevant condition
models_ahnlee <- list() # Capturing models
modelsno_ahnlee <- vector() # Capturing number of models
modelsnone <- c("None") # Entry for models_ahnlee if no truth table row with Y=1
modelszero <- 0 # Entry for modelszero if no truth table row with Y=1
termlist_ahnlee <- list() # Capturing terms of models
set.seed(111) # set.seed for reproducibility
for (i in 1:1000){
  ahnlee$c <- runif(n=nrow(ahnlee), min=0, max=1) # Generating random condition
  ahnleeTT <- truthTable(ahnlee,outcome="socx",conditions=c("cgdp","stunemr","elderly","c"),n.cut=1,
                         incl.cut1=0.85,sort.by=c("incl","n"))
  models <- list()
  if(max(as.numeric(ahnleeTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_ahnlee <- c(models_ahnlee,modelsnone)
    modelsno_ahnlee <- c(modelsno_ahnlee,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    ahnleePS <- eqmcc(ahnleeTT,outcome="OUT",include="?",explain="1",min.dis=F) # Generating solution
    modelsno_ahnlee <- c(modelsno_ahnlee,length(ahnleePS$solution))
    for (m in 1:length(ahnleePS$solution)) { # Loop over all models in solution
      if (length(ahnleePS$solution[[m]])==1) { # Model without equifinality
        models_ahnlee <- c(models_ahnlee,ahnleePS$solution[[m]])
      }
      if (length(ahnleePS$solution[[m]])>1) { # Model with equifinality
        models <- ahnleePS$solution[[c(m,1)]] # Extracting first term of model
        for (p in 2:length(ahnleePS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,ahnleePS$solution[[c(m,p)]],sep="+") # Reconstructing model with equifinality
        }
      }
      models_ahnlee <- c(models_ahnlee,models) # Concatenating models
    }
    termlist_ahnlee <- c(termlist_ahnlee, ahnleePS$solution)
  }
}

summodel_ahnlee <- sum(modelsno_ahnlee) # Total number of models
reprodmodel_ahnlee <- 100*round(length(which(models_ahnlee=="CGDP"))/summodel_ahnlee, digits=4) # Share of models identical to original models
reprodsucc[1,1] <- reprodmodel_ahnlee

### Samford, Steven (2010): Averting "Disruption and Reversal": Reassessing the Logic of Rapid Trade Reform in Latin America. Politics & Society 38 (3): 373-407.
# Reproducing original analysis
samford <- read.csv("Samford2010set.csv", sep=",",header=T) # Loading Samford data
samfordTT <- truthTable(samford,outcome="raplib",conditions=c("execunco","switcher","devalu","grostron","hyperinf","groweak","manufac"),
                        incl.cut1=0.75,n.cut=2,sort.by=c("incl","n"))
(samfordPS <- eqmcc(samfordTT,outcome="OUT",explain="1",include="?",details=T)) # Parsimonious solution

# Simulating inclusion of irrelevant condition
models_samford <- list() # Capturing models
modelsno_samford <- vector() # Capturing number of models
modelsnone <- c("None") # Entry for models_samford if no truth table row with Y=1
modelszero <- 0 # Entry for modelszero if no truth table row with Y=1
termlist_samford <- list() # Capturing terms of models
set.seed(222) # set.seed for reproducibility
for (i in 1:1000){
  samford$c <- runif(n=nrow(samford), min=0, max=1) # Generating random condition
  samfordTT <- truthTable(samford,outcome="raplib",conditions=c("execunco","switcher","devalu","grostron","hyperinf","groweak","manufac","c"),
                          incl.cut1=0.75,n.cut=2,sort.by=c("incl","n"))
  models <- list()
  if(max(as.numeric(samfordTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_samford <- c(models_samford,modelsnone)
    modelsno_samford <- c(modelsno_samford,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    samfordPS <- eqmcc(samfordTT,outcome="OUT",explain="1",include="?",min.dis=F) # Generating solution
    modelsno_samford <- c(modelsno_samford,length(samfordPS$solution))
    for (m in 1:length(samfordPS$solution)) { # Loop over all models in solution
      if (length(samfordPS$solution[[m]])==1) { # Model without equifinality
        models_samford <- c(models_samford,samfordPS$solution[[m]])
      }
      if (length(samfordPS$solution[[m]])>1) { # Model with equifinality
        models <- samfordPS$solution[[c(m,1)]] # Extracting first term of model
        for (p in 2:length(samfordPS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,samfordPS$solution[[c(m,p)]],sep="+") # Reconstructing model with equifinality
        }
      }
      models_samford <- c(models_samford,models) # Concatenating models
    }
    termlist_samford <- c(termlist_samford, samfordPS$solution)
  }
}

summodel_samford <- sum(modelsno_samford) # Total number of models
reprodmodel_samford_1 <- 100*round(length(which(models_samford=="EXECUNCO*DEVALU"))/summodel_samford, digits=4) # Share of models identical to original models
reprodmodel_samford_2 <- 100*round(length(which(models_samford=="EXECUNCO*HYPERINF"))/summodel_samford, digits=4) # Share of models identical to original models
reprodmodel_samford_3 <- 100*round(length(which(models_samford=="EXECUNCO*grostron*MANUFAC"))/summodel_samford, digits=4) # Share of models identical to original models
reprodsucc[1,2] <- reprodmodel_samford_1
reprodsucc[2,2] <- reprodmodel_samford_2
reprodsucc[3,2] <- reprodmodel_samford_3

### Koenig-Archibugi, Mathias (2004): Explaining Government Preferences for Institutional Change in Eu Foreign and Security Policy. International Organization 58 (1): 137-174.
# Reproducing original analysis using modified data (see appendix)
koenigsa <- read.csv("koenig_2004sa.csv", sep=";",header=T) # Loading manually modified data (see appendix)
koenigsaTT <- truthTable(koenigsa,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
                         incl.cut1=1,sort.by=c("incl","n"))
(koenigsaCS <- eqmcc(koenigsaTT, include="1", explain="1")) # Conservative solution

# Demonstration that original data does not lead to Koenig-Archibugi's solution
koenig <- read.csv("koenig_2004.csv", sep=";",header=T) # Loading original data
koenigTT <- truthTable(koenig,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities"),
                       incl.cut1=1,sort.by=c("incl","n"),complete=T)
(koenigCS <- eqmcc(koenigTT, include="1", explain="1")) # Conservative solution
(koenigPS <- eqmcc(koenigTT, include="?", explain="1")) # Parsimonious solution

# Simulating inclusion of irrelevant condition
models_koenigsa <- list() # Captures models
modelsno_koenigsa <- vector() # Captures number of models per simulated condition
modelsnone <- c("None") # Entry for models_koenigsa if no truth table row with Y=1
modelszero <- 0 # Entry for models_koenigsa if no truth table row with Y=1
termlist_koenigsa <- list() # Captures terms of models
set.seed(333) # set.seed for reproducibility
for (i in 1:1000) { # Runs 1000 simulations
  koenigsa$c <- runif(n=nrow(koenigsa), min=0, max=1) # Generates random condition
  koenigsaTT <- truthTable(koenigsa,outcome=c("supranationalism"),conditions=c("idmass","conformity","regionalism","capabilities","c"),
                           incl.cut1=1,sort.by=c("incl","n"))
  models <- list() # Placeholder for internal purposes
  if(max(as.numeric(koenigsaTT$tt$OUT), na.rm = T)==0) { # 1st loop for truth tables without any row being "1"
    models_koenigsa <- c(models_koenigsa,modelsnone)
    modelsno_koenigsa <- c(modelsno_koenigsa,modelszero)
  }
  else { # 2nd loop for truth tables with at least one row being "1"
    koenigsaCS <- eqmcc(koenigsaTT,outcome="OUT",explain="1",include="1",min.dis=F) # Generates solution
    modelsno_koenigsa <- c(modelsno_koenigsa,length(koenigsaCS$solution)) # Assigns number of models
    for (m in 1:length(koenigsaCS$solution)) { # Loop over all models in solution
      if (length(koenigsaCS$solution[[m]])==1) { # Model without equifinality
        models_koenigsa <- c(models_koenigsa,koenigsaCS$solution[[m]]) # Assigns model
      }
      if (length(koenigsaCS$solution[[m]])>1) { # Model with equifinality
        models <- koenigsaCS$solution[[c(m,1)]] # Extracts first term of model
        for (p in 2:length(koenigsaCS$solution[[m]])) { # Loop for extracting remaining terms of model
          models <- paste(models,koenigsaCS$solution[[c(m,p)]],sep="+") # Extracts model(s) with equifinality
        }
      }
      models_koenigsa <- c(models_koenigsa,models) # Assigns model(s)
    }
    termlist_koenigsa <- c(termlist_koenigsa, koenigsaCS$solution) # Assigns individual term(s)
  }
}

summodel_koenigsa <- sum(modelsno_koenigsa) # Total number of models
reprodmodel_koenigsa <- 100*round(length(which(models_koenigsa=="CONFORMITY*REGIONALISM+IDMASS*REGIONALISM*capabilities"))/summodel_koenigsa, digits=4) # Share of models identical to original models
reprodsucc[1,3] <- reprodmodel_koenigsa
reprodsucc # Producing table 3
