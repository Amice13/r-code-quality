###############################################################################
### Description: 	This document provides code for reproducing select 	      ###
###					      models and a figure from the appendix of the paper,  	    ###
###					      "Elections Activate Partisanship Across Countries,”  	    ###
###					      which is authored by Shane P. Singh and Judd R.  		      ###
###					      Thornton and appears in the American Political Science 		###
###					      Review. 		                                              ###
###############################################################################

##############
##############
#Install Any Needed Packages                                                                                                                                  
############## 
##############
install.packages('readstata13') #Install the readstata13 package to open Stata data," authored by Jan Marvin Garbuszus and Sebastian Jeworutzki
install.packages('lme4') #Install the lme4 package for fitting linear and generalized linear mixed-effects models, authored by Douglas Bates, Martin Maechler, Ben Bolker, Steven Walker, Rune Haubo Bojesen Christensen, Henrik Singmann, Bin Dai, Fabian Scheipl, Gabor Grothendieck, and Peter Green
install.packages('ordinal') #Install the ordinal package for fitting ordered regression models, authored by Rune Haubo Bojesen Christensen


##############
##############
#Set Working Folder to Where the Data Are Stored                                                                                                                        
############## 
##############
setwd("/Users/singh/Desktop/singh_thornton_APSR_replication")  #edit this to align with your directory structure


##############
##############
#Load the CSES data                                                                                                                       
############## 
##############
library(readstata13)
CSES_data <- read.dta13("singh_thornton_APSR_replication.dta")
CSES_data_restricted <- subset(CSES_data, election_samp==1) #Only use surveys that included each outcome variable. Also exclude some Finnish respondents (see note 1 of the text). 




##############
##############
#Far-right Column of Table C.1                                                                                                                      
############## 
##############
library(lme4)
mod.1<-glmer(partyID~ln_time_since_election+
               (ln_time_since_election|cntryyear),
             family=binomial(link="logit"), data=CSES_data_restricted, verbose=TRUE, )
summary(mod.1)



##############
##############
#Far-right Column of Table C.2                                                                                                                      
############## 
##############
library(ordinal)
mod.2<-clmm(as.ordered(partyID_strength)~ln_time_since_election+
                          (ln_time_since_election|cntryyear),
                        link="logit", threshold = "flexible", data=CSES_data_restricted)
summary(mod.2)



##############
##############
#Far-right Column of Table C.3                                                                                                                     
############## 
##############
library(lme4)
mod.3<-lmer(eval_inc~partyID_inc*ln_time_since_election +
                         (partyID_inc+ln_time_since_election|cntryyear),
                        , data=CSES_data_restricted, verbose=TRUE, REML = 'FALSE')
summary(mod.3)



##############
##############
#Figure G.1                                                                                                                    
############## 
##############
library(foreign)
library(lattice)

time_til_elec_data <- read.dta13("days_until_next_election.dta")
set.seed(123) #Setting the seed will ensure that the jitter in the stripplot replicates exactly.
my.theme <- list( 
  axis.components = list(left = list(tck = 1, pad1 = 1, pad2  =
                                       0), top = list(tck = 0, pad1 = 1, pad2 = 0), right = list(tck = 0, 
                                                                                                 pad1 = 1, pad2 = 0), bottom = list(tck = 1, pad1 = 1, pad2 = 0))) 
trellis.par.set(theme = my.theme)
dev.new(width=4,height=4)
dev.size() 

densityplot(~ var1, data = time_til_elec_data, col="black"
            ,aspect=1,
            auto.key=list(space = "bottom", cex=.8),
            par.settings = list(superpose.line = list(col =
                                                        c("Black", "Black"), lty=c(2,1))),xlab="Days from Survey to Subsequent Election", scales=list(y=list(rot=90)))


