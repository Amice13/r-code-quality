
#######################################################################################################################################################################################

# THIS PROGRAM PERFORMS REGRESSIONS IN FINTECH, INVESTOR SOPHISTICATION AND FINANCIAL PORTFOLIO CHOICES ((Tables 1,2 and 3 and Table B2 in Appendix B);
# Results in Table 1 can be fully replicated while those in Table 2, 3, and Table B2 in Appendix B are not replicable as regressions contains the randomly generated variables among covariates;
# INPUT DATA: fintech_data_rcfs.csv
# For a description of the variables see the document "data and programs description"	

#######################################################################################################################################################################################


rm(list=ls())


library(car)
library(AER)
library(MASS)
library(sjPlot) 
library(modelsummary) 
library(aod) 
library(ggplot2)
library(cowplot)
library(betareg)
library(sandwich)
library(miceadds)
library(rsq)
library(stargazer)


# Import DATA

data <- read.csv('fintech_data_rcfs.csv')

# data preparation

data$ACOM4C <- factor(data$ACOM4C) 
data$STUDIO5 <- factor(data$STUDIO5) 
data$STRAN <- factor(data$STRAN) 
data$ANNO <- factor(data$ANNO)
data$ETA5 <- factor(data$ETA5) 
data$QUALP3 <- factor(data$QUALP3) 
data$CLYEQ2 <- factor(data$CLYEQ2) 
data$RISFIN <- factor(data$RISFIN) 
data$TIPOFAM <- factor(data$TIPOFAM) 
data$HHFINTOT <- factor(data$HHFINTOT)
data$LLFINTOT<-factor(data$LLFINTOT)
data$COLDIS <- factor(data$COLDIS) 
data$SESSO <- factor(data$SESSO)
data$IREG <- factor(data$IREG)


#######################################################################################################################################################################################
# SELECTION TABLE B2; RESULTS IN THIS TABLE CANNOT BE REPLICATED BECAUSE THE VARIABLE HDIGF HAS BEEN REPLACED WITH PSEUDO-DATA;
#######################################################################################################################################################################################

sel_HDIGF_index <- with(data,glm((HDIGF) ~ SESSO+STUDIO5+STRAN+ACOM4C+ANNO+ETA5+IREG+QUALP3+CLYEQ2+RISFIN+TIPOFAM+
                                      HHFINTOT, family = quasibinomial(link='logit'), weights = PESO))

summary(sel_HDIGF_index)

#CLUSTERED ERRORS

sel_HDIGF_index_cl <-glm.cluster(data=data, formula=HDIGF ~ SESSO+STUDIO5+STRAN+ACOM4C+ANNO+ETA5+IREG+QUALP3+CLYEQ2+RISFIN+TIPOFAM+
                                   HHFINTOT, cluster=data$IREG, family ="gaussian", weights = data$PESO)
summary(sel_HDIGF_index_cl)

###############################################################
# PRINT selection TABLE B2
require("stargazer")
# print stargazer output with robust standard errors
stargazer(sel_HDIGF_index, #type = "text", 
          se = list(summary(sel_HDIGF_index_cl)[,2]),single.row = TRUE)
rsq(sel_HDIGF_index, type='kl')
###############################################################


#######################################################################################################################################################################################
# REGRESSIONS TABLE 1
#######################################################################################################################################################################################


# REGRESSION 1 
qbiYCF_IREG <- with(data,glm((ROR) ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                              COLDIS+HHFINTOT, family = gaussian(link='identity'), weights = PESO))
summary(qbiYCF_IREG)


# REGRESSION 1 + REG cluster
qbiYCF_IREG_cl <-glm.cluster(data=data, formula=ROR ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                               COLDIS+ HHFINTOT, cluster=data$IREG, family ="gaussian", weights = data$PESO)

#REGRESSION 2 
qbiQHR_IREG<- with(data,glm((QHRISK) ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                              COLDIS+ HHFINTOT, family = gaussian(link='identity'), weights = PESO))
summary(qbiQHR_IREG)

# REGRESSION 2 + REG cluster
qbiQHR_IREG_cl <-glm.cluster(data=data, formula=QHRISK ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                               COLDIS+  HHFINTOT, cluster=data$IREG, family ="gaussian", weights = data$PESO)
summary(qbiQHR_IREG_cl)


###############################################################
# PRINT TABLE 1
require("stargazer")

stargazer( qbiYCF_IREG, qbiQHR_IREG,# type = "text", 
                se = list(summary(qbiYCF_IREG_cl)[,2]
                          ,summary(qbiQHR_IREG_cl)[,2]),
                     single.row = TRUE)

# pseudo rsquare;
rsq(qbiYCF_IREG,type='kl')
rsq(qbiQHR_IREG,type='kl')
##############################################################




#######################################################################################################################################################################################
# REGRESSIONS TABLE2: RESULTS IN THIS TABLE CANNOT BE REPLICATED BECAUSE THE VARIABLES HDIGF AND SOPHHDIGF HAVE BEEN REPLACED WITH PSEUDO-DATA;
#######################################################################################################################################################################################


 qbiYCF_SHDIGF_IREG <- with(data,glm((ROR) ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                          +COLDIS+HHFINTOT+HDIGF+SOPHHDIGF, family = gaussian(link='identity'), weights = PESO))
 
 summary(qbiYCF_SHDIGF_IREG)

 
 qbiYCF_HHLL_IREG <- with(data,glm((ROR) ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                       +COLDIS+HHFINTOT+LLFINTOT, family = gaussian(link='identity'), weights = PESO))
 
 summary(qbiYCF_HHLL_IREG)
 
 qbiQHR_SHDIGFL_IREG <- with(data,glm((QHRISK) ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                        COLDIS+HHFINTOT+LLFINTOT+ HDIGF+SOPHHDIGF+UNSHDIGF, family = gaussian(link='identity'), weights = PESO))
 summary(qbiQHR_SHDIGFL_IREG)
 
  
 # clustered errors
   
   
 qbiYCF_SHDIGF_IREG_cl <-glm.cluster(data=data, formula=ROR ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                       +COLDIS+HHFINTOT+HDIGF+SOPHHDIGF, cluster=data$IREG, family ="gaussian", weights = data$PESO)
 
 summary(qbiYCF_SHDIGF_IREG_cl)

 qbiYCF_HHLL_IREG_cl<- glm.cluster(data=data, formula=ROR ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                     +COLDIS+HHFINTOT+LLFINTOT, cluster=data$IREG, family ="gaussian", weights = data$PESO)
 
 summary(qbiYCF_HHLL_IREG_cl)
 
  
 qbiQHR_SHDIGFL_IREG_cl <- glm.cluster(data=data, formula=QHRISK ~ STRAN+ACOM4C+IREG+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                        COLDIS+HHFINTOT+LLFINTOT+ HDIGF+SOPHHDIGF+UNSHDIGF, cluster=data$IREG, family ="gaussian", weights = data$PESO)
 summary(qbiQHR_SHDIGFL_IREG_cl)
 
#################################################################################
  # PRINT TABLE 2 WITH CLUSTERED ERRORS (RESULTS IN THIS TABLE CANNOT BE REPLICATED BECAUSE THE VARIABLES HDIGF AND SOPHHDIGF HAVE BEEN REPLACED WITH PSEUDO-DATA);
 
 stargazer( qbiYCF_SHDIGF_IREG , qbiYCF_HHLL_IREG , qbiQHR_SHDIGFL_IREG,# type = "text", 
            se = list(summary(qbiYCF_SHDIGF_IREG_cl)[,2]
                      ,summary(qbiYCF_HHLL_IREG_cl)[,2], summary(qbiQHR_SHDIGFL_IREG_cl)[,2]),
            single.row = TRUE)
 
 rsq(qbiYCF_SHDIGF_IREG,type='kl')
 rsq(qbiYCF_HHLL_IREG,type='kl')
 rsq(qbiQHR_SHDIGFL_IREG,type='kl')
 ##################################################################################
 
 
 
#######################################################################################################################################################################################
   # REGRESSIONS  TABLE 3: RESULTS IN THIS TABLE CANNOT BE REPLICATED BECAUSE THE VARIABLES HDIGF, UNSHDIGF, DIGFHATREG, UNSREGDIGFHAT, AND UNSREGDIGFHAT2 HAVE BEEN REPLACED WITH PSEUDO-DATA;
#######################################################################################################################################################################################
 
   qbiQHR_UNSREG<- with(data,glm((QHRISK) ~ STRAN+ACOM4C+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                    COLDIS+LLFINTOT+DIGFHATREG+UNSREGDIGFHAT+UNSREGDIGFHAT2, family = gaussian(link='identity'), weights = PESO))
   summary(qbiQHR_UNSREG)
   

   
   qbiQHR_UNSHREG<- with(data,glm((QHRISK) ~ STRAN+ACOM4C+TIPOFAM+CLYEQ2+ANNO+SESSO+STUDIO5+ETA5+QUALP3+RISFIN+
                                    COLDIS+LLFINTOT+HDIGF+UNSHDIGF, family = gaussian(link='identity'), weights = PESO))
   summary(qbiQHR_UNSHREG)
  
   
   
   ######################################################################
   # PRINT TABLE 3 RESULTS IN THIS TABLE CANNOT BE REPLICATED BECAUSE THE VARIABLES HDIGF, UNSHDIGF, DIGFHATREG, UNSREGDIGFHAT, AND UNSREGDIGFHAT2 HAVE BEEN REPLACED WITH PSEUDO-DATA
  stargazer( qbiQHR_UNSHREG, qbiQHR_UNSREG,single.row = TRUE)
  
   rsq(qbiQHR_UNSHREG,type='kl')
   rsq(qbiQHR_UNSREG,type='kl')
   ######################################################################