#######################################################################
#######################################################################
## This script contains the code to reproduce Figures 8-12          ##
## as well as the individual panels of Figures SI2-17  in the paper ##
## "Trading votes:What drives MEP support for                       ##
##	trade liberalization?" by Robert Basedow and Julian Hoerner     ##
##  which appears in the Journal of European Public Policy.         ##
##	Please note that the code to reproduce the main models and      ##
## additional figures can be found in a separate Stata dofile.      ##
## The individual panels of Figures SI2-17 can be combined          ##
## to produce the figures using two .txt files, as explained below. ##
## Please consult the readme file for further information           ##                                
######################################################################



setwd("C:/Users/julia/Dropbox/EP Trade Paper/Drafts/3rd submission (final)/Replication 24.07.2023")

##Please replace with your own directory as appropriate

#Install required packages 

install.packages('interflex')
install.packages('ggplot2')
install.packages('sandwich')
install.packages('Lmoments')
install.packages('lfe')
install.packages('gridExtra')
install.packages('grid')
install.packages('ggplotify')
install.packages('RColorBrewer')
install.packages('pcse')
install.packages('gtable')
install.packages('MASS')
install.packages('mvtnorm')
install.packages('pROC')
install.packages('ModelMetrics')
install.packages('Rcpp')
install.packages('lmtest')
install.packages('AER')
install.packages('future')
install.packages('read_dta')
install.packages("tibble")
install.packages("plyr")                          
install.packages("dplyr")    
install.packages ("ggpubr")

##Interflex:

#Hainmueller, J., Mummolo, J., Xu, Y., 2019. How Much Should We Trust Estimates from Multiplicative Interaction Models? Simple Tools to Improve Empirical Practice. Political Analysis 27, 163-192. https://doi.org/10.1017/pan.2018.46

#R Package: https://cran.r-project.org/web/packages/interflex/interflex.pdf

#Call required packages 

library('interflex')
library('ggplot2')
library ('sandwich')
library ('Lmoments')
library ('lfe')
library ('gridExtra')
library ('grid')
library ('ggplotify')
library ('RColorBrewer')
library ('pcse')
library ('gtable')
library ('MASS')
library ('mvtnorm')
library ('pROC')
library ('ModelMetrics')
library ('Rcpp')
library ('lmtest')
library ('AER')
library ('future')
library ("tibble")
library("plyr") 
library("dplyr") 
library('interflex')
library('tidyverse')
library('foreign')
library('ggpubr')


#Load Dataset

df<-read_csv("C:/Users/julia/Dropbox/EP Trade Paper/Drafts/3rd submission (final)/Final Replication Files/EP_Trade_Dataset_Final_24072023.csv")

##Please edit path with your own directory name as appropriate

#Required edits of variable names 

df<-as.data.frame(df)
names(df)[names(df)=="for"]<-"vote_for"
names(df)[names(df)=="year"]<-"year_date"

#Subset data for different models 

df_lib<-df[which(df$dum_lib=="1"),]
df_lib_cons<-df[which(df$dum_lib=="1" & df$procedure_new=="2"),]
df_lib_no_cons<-df[which(df$dum_lib=="1" & df$procedure_new!="2"),]




################################################################################
################################################################################
####Main Models
################################################################################
################################################################################


##Unemployment

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "agri_interact_std", "EU_interact_std", "rile_interact_std", "prot_interact_std")

#Model 4 - All (Figure 8)

unemp_all <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z = controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "All - Unemployment",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "Figure8_unemp_all.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 5 - Trade Agreements 

unemp_TA <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "Trade Agreements - Unemployment",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "unemp_TA.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)


#Model 6 - Regulations, Directives, Resolution

unemp_RDR <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "RDR - Unemployment",
          Ylabel = " Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "unemp_RDR.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)




##Agriculture


controls<-c( "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate","std_eu_unemp_1574t", "prot_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std" )

#Model 4 - All (Figure 9)

agri_all <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All - Empl. in Agriculture",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "Figure9_agri_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



#Model 5 - Trade Agreements

agri_TA <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements - Agriculture",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


#Model 6 - Regulations, Directives, Resolution

agri_RDR <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "RDR - Agriculture",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


        
##EU Attitude

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_eu_unemp_1574t",  "std_protectionism_nat_ipolate","agri_interact_std", "rile_interact_std", "prot_interact_std", "unemp_interact_std")

#Model 4 - All (Figure 10)

EU_all <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "All - EU",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "All", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "Figure10_EU_all.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)


#Model 5 - Trade Agreements 

EU_TA <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "Trade Agreements - EU",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "EU_TA.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 6 - Regulations, Directives, Resolution

EU_RDR <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "RDR - EU",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "EU_RDR.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)


        
##Protectionism

#Model 4 - All (Figure 11)


controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std")

prot_all <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "All - Protectionism",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "All", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "Figure11_prot_all.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 5 - Trade Agreements 

prot_TA <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "Trade Agreements - Protectionism",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "prot_TA.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 6 - Regulations, Directives, Resolution

prot_RDR <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "RDR - Protectionism",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "prot_RDR.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)


        


##Left-Right (Figure 12)

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020",  "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "prot_interact_std", "EU_interact_std", "unemp_interact_std")

#Model 4 - All

rile_all <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "All - Left/Right",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "All", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "Figure12_rile_all.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 5 - Trade Agreements

rile_TA <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main = "Trade Agreements - Left/Right",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "rile_TA.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)



#Model 6 -Regulations, Directives, Resolution

rile_RDR <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
          base = NULL, Z=controls, IV = NULL, FE =NULL,
          full.moderate = FALSE,
          weights = NULL, na.rm = TRUE, Xunif = FALSE,
          CI = TRUE, neval = 50, X.eval = NULL,
       method = "logit", vartype = "delta",
       vcov.type = "cluster",time=NULL,
          pairwise = TRUE,nboots = 200, nsimu = 1000,
          parallel =TRUE,cores = 4, cl = "MEP_ID",
          Z.ref = NULL, D.ref = NULL,
          nbins = 3, cutoffs = NULL, wald = TRUE,
          bw = NULL, kfold = 10, grid = 30,
          metric = NULL, figure = TRUE, bin.labs = TRUE,
          order = NULL, subtitles = NULL, show.subtitles = NULL,
          Xdistr = "histogram", main ="RDR - Left/Right",
          Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
          xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
          theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
          cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
          interval = NULL, file = "rile_RDR.png", ncols = NULL,
          pool = FALSE, color = NULL, show.all = FALSE,
          legend.title = NULL, diff.values = NULL,
          percentile = FALSE, gam.k = 10,
        angle=c(30, 100,-30,-120),
        span=NULL, scale = 1.1, height = 7, width = 10)

################################################################################
#NB: Figure8_unemp_all, Figure9_agri_all, Figure10_EU_all, Figure11_prot_all and Figure12_rile_all appear as Figures 8-12 in the main manuscript.
#Figure SI2 was compiled from individual panels containing the remaining graphs produce above  (use "EP_Trade_Figure_SI2.txt")
#see "EP_Treade_ReadMe.txt" for further guidance
################################################################################
      

################################################################################
################################################################################
#Robustness Checks
################################################################################
################################################################################

################################################################################
#Linear Models with Fixed Effects
################################################################################


##Unemployment

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "agri_interact_std", "EU_interact_std", "rile_interact_std", "prot_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z = controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_all_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_TA_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment" , Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_RDR_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


##Agriculture


controls<-c( "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate","std_eu_unemp_1574t", "prot_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std" )

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_all_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_TA_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_RDR_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



##EU Attitude

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_eu_unemp_1574t",  "std_protectionism_nat_ipolate","agri_interact_std", "rile_interact_std", "prot_interact_std", "unemp_interact_std")

#Model 4 - All


model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_all_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_TA_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - "Regulations, Directives, Resolutions"

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_RDR_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

##Protectionism

#Model 4 - All

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std")
model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_all_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_TA_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_RDR_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


##Left-Right

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020",  "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "prot_interact_std", "EU_interact_std", "unemp_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_all_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_TA_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE ="year_date",
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main ="Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_RDR_ln_fx.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


################################################################################
###Linear models (no fixed effects)
################################################################################


##Unemployment

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "agri_interact_std", "EU_interact_std", "rile_interact_std", "prot_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z = controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_all_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Trade Agreements", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_TA_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolution",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "unemp_RDR_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

##Agriculture


controls<-c( "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate","std_eu_unemp_1574t", "prot_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std" )

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_all_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_TA_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "agri_RDR_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



##EU Attitude

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_eu_unemp_1574t",  "std_protectionism_nat_ipolate","agri_interact_std", "rile_interact_std", "prot_interact_std", "unemp_interact_std")

#Model 4 - All


model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff.on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_all_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_TA_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_pro_anti_EU_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "EU_RDR_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

##Protectionism

#Model 4 - All

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_rile_nat_ipolate", "std_pro_anti_EU_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "rile_interact_std", "EU_interact_std", "unemp_interact_std")
model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_all_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_TA_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "prot_RDR_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



##Left-Right

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020",  "std_pro_anti_EU_nat_ipolate", "std_protectionism_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "prot_interact_std", "EU_interact_std", "unemp_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_all_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_TA_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Liberal & No Cons

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_rile_nat_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "linear", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main ="Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "rile_RDR_ln.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



###############################################################################
### CHES Data for L/R and EU Position 
###############################################################################

#Unemployment

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_lrgen_ipolate", "std_eu_position_ipolate" , "std_protectionism_nat_ipolate", "agri_interact_std", "ches_interact_std", "eu_ches_interact_std", "prot_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z = controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_unemp_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Unemployment", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_unemp_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_unemp_1574t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Unemployment",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Regulations, Directives, Resolutions", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_unemp_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


##Agriculture


controls<-c( "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_lrgen_ipolate", "std_eu_position_ipolate", "std_protectionism_nat_ipolate","std_eu_unemp_1574t", "prot_interact_std", "ches_interact_std", "eu_ches_interact_std", "unemp_interact_std" )

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_agri_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_agri_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_emtk_ab_t", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Empl. in Agriculture", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_agri_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


##EU Attitude

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020",  "std_eu_unemp_1574t", "std_lrgen_ipolate", "std_protectionism_nat_ipolate","agri_interact_std", "ches_interact_std", "prot_interact_std", "unemp_interact_std")

#Model 4 - All


model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_eu_position_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_EU_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_eu_position_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_EU_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_eu_position_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "EU Attitude", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_EU_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

##Protectionism

#Model 4 - All

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020", "std_lrgen_ipolate", "std_eu_position_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "ches_interact_std", "eu_ches_interact_std", "unemp_interact_std")
model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D ="std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_prot_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements 

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_prot_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 - Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_protectionism_nat_ipolate", X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Protectionism", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_prot_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)



##Left-Right

controls<-c("std_eu_emtk_ab_t", "std_eu_emtk_htc_t", "std_eu_gdp_pps_hab_eu27_2020",  "std_eu_position_ipolate", "std_protectionism_nat_ipolate", "std_eu_unemp_1574t", "agri_interact_std", "prot_interact_std", "eu_ches_interact_std", "unemp_interact_std")

#Model 4 - All

model <- interflex(estimator='binning', data=df_lib, Y = "vote_for", D = "std_lrgen_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "All",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_lr_all.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 5 - Trade Agreements

model <- interflex(estimator='binning', data=df_lib_cons, Y = "vote_for", D = "std_lrgen_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main = "Trade Agreements",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_lr_TA.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)

#Model 6 -Regulations, Directives, Resolution

model <- interflex(estimator='binning', data=df_lib_no_cons, Y = "vote_for", D = "std_lrgen_ipolate",  X = "std_sq_salience_norm_10", treat.type = NULL,
                   base = NULL, Z=controls, IV = NULL, FE =NULL,
                   full.moderate = FALSE,
                   weights = NULL, na.rm = TRUE, Xunif = FALSE,
                   CI = TRUE, neval = 50, X.eval = NULL,
                   method = "logit", vartype = "delta",
                   vcov.type = "cluster",time=NULL,
                   pairwise = TRUE,nboots = 200, nsimu = 1000,
                   parallel =TRUE,cores = 4, cl = "MEP_ID",
                   Z.ref = NULL, D.ref = NULL,
                   nbins = 3, cutoffs = NULL, wald = TRUE,
                   bw = NULL, kfold = 10, grid = 30,
                   metric = NULL, figure = TRUE, bin.labs = TRUE,
                   order = NULL, subtitles = NULL, show.subtitles = NULL,
                   Xdistr = "histogram", main ="Regulations, Directives, Resolutions",
                   Ylabel = "Eff. on Likl. Vote For", Dlabel = "Left-Right", Xlabel ="Politicization",
                   xlab = "Politicization", ylab = "Eff. on Likl. Vote For", xlim = c(-1,4), ylim = NULL,
                   theme.bw = FALSE, show.grid = TRUE, cex.main = NULL,
                   cex.sub = NULL, cex.lab = NULL, cex.axis = NULL,
                   interval = NULL, file = "ches_lr_RDR.png", ncols = NULL,
                   pool = FALSE, color = NULL, show.all = FALSE,
                   legend.title = NULL, diff.values = NULL,
                   percentile = FALSE, gam.k = 10,
                   angle=c(30, 100,-30,-120),
                   span=NULL, scale = 1.1, height = 7, width = 10)


################################################################################
#NB:Figures SI3-SI17 were compiled from the panels above using LaTeX (use "EP_Trade_Figure_SI3_SI17.txt")
#see "EP_Treade_ReadMe.txt" for further guidance
################################################################################
''