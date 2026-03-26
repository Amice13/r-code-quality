# Replication code for "Who did What for Whom": the zero-inflated regression model.

# set working directory (Please change accordingly.)

# You may have to install these packages first.
#install.packages("pscl")
#install.packages("stargazer")
# Call the libraries.
library(pscl)
library(stargazer)

sessionInfo() # the results were created using these versions, which you may have to install to get the exact same results
#R version 3.5.2 (2018-12-20)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6

#Matrix products: default
#BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] stats     graphics  grDevices utils     datasets  methods   base     

#other attached packages:
#  [1] pscl_1.5.2      ggplot2_3.0.0   Cairo_1.5-9     psych_1.8.4     dplyr_0.7.6    

#loaded via a namespace (and not attached):
#  [1] Rcpp_0.12.18         plyr_1.8.4           pillar_1.3.0         compiler_3.5.2      
#[5] DEoptimR_1.0-8       bindr_0.1.1          tools_3.5.2          gtable_0.2.0        
#[9] tibble_1.4.2         nlme_3.1-137         lattice_0.20-38      pkgconfig_2.0.2     
#[13] rlang_0.2.2          Matrix_1.2-15        rstudioapi_0.9.0     yaml_2.2.0          
#[17] parallel_3.5.2       ergm_3.9.4           bindrcpp_0.2.2       coda_0.19-1         
#[21] withr_2.1.2          knitr_1.20           trust_0.1-7          grid_3.5.2          
#[25] tidyselect_0.2.5     robustbase_0.93-2    glue_1.3.0           R6_2.2.2            
#[29] foreign_0.8-71       purrr_0.2.5          magrittr_1.5         scales_1.0.0        
#[33] MASS_7.3-51.1        assertthat_0.2.0     mnormt_1.5-5         colorspace_1.3-2    
#[37] lpSolve_5.6.13       labeling_0.3         network_1.13.0.1     lazyeval_0.2.1      
#[41] munsell_0.5.0        


 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# LOAD AND PREPARE DATA FOR RUNNING MODELS:
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# load reference data
LA_CAT <- read.csv(file = "/Users/bizhao/R/CAT analysis/LA_CAT_reference.csv",header = TRUE,sep = ",") #Change the directory accordingly!
# call the country-year rows with UAs on torture (remove the ones without UAs on torture)
LA_cat_tor <- LA_CAT[LA_CAT$year>=1987&LA_CAT$tor>0&LA_CAT$cat_rat==1,]


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Zero-inflated models:
# We used both Poisson and negative binomail distribution to fit the models.
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#Variables:
#Dependent variable: CAT; count of UAs that mentioned CAT by country-year
#Independent variable 1 (for count part of the model): year
#Independent varialbe 2 (for logit part of the model) : tor; count of UAs on torture by country-year
#Independent variable 3 (for logit part of the model): ua; count of all UAs by country-year

#scale year
LA_cat_tor$year.scale <- scale(LA_cat_tor$year,center = TRUE,scale = TRUE) 

# Fit zero-inflated models with Poisson distribution. Use "year" variable to predict the count model coefficients, use the variables of "number of UAs on torture" and "total number of UAs" for the logit part to explain excess zeros.
zinb.year.1 <- zeroinfl(CAT ~ year.scale|tor,data = LA_cat_tor)
summary(zinb.year.1)
zinb.year.2 <- zeroinfl(CAT ~ year.scale|tor+ua,data = LA_cat_tor)
summary(zinb.year.2)


# Fit zero-inflated models with negative binomial distribution and same specification for the count model and the logit model.
zinb.year.3 <- zeroinfl(CAT ~ year.scale|tor,data = LA_cat_tor,dist = "negbin",EM=TRUE)
summary(zinb.year.3)
zinb.year.4 <- zeroinfl(CAT ~ year.scale|tor+ua,data = LA_cat_tor,dist = "negbin",EM=TRUE)
summary(zinb.year.4)

# Create table
stargazer(zinb.year.1, zinb.year.2,zinb.year.3,zinb.year.4, type = "text",header = TRUE)