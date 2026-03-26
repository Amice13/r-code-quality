rm(list = ls());gc()

library(dplyr)
library(ggplot2)
library(lme4)
library(tidyr)
library(margins)
library(rootSolve)
library(nlshrink)
library(Matrix)

sessionInfo()

# R version 4.3.2 (2023-10-31)
# Platform: x86_64-pc-linux-gnu (64-bit)
# Running under: Ubuntu 22.04.4 LTS
# 
# Matrix products: default
# BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.10.0 
# LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.10.0
# 
# locale:
#   [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=de_CH.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=de_CH.UTF-8   
# [6] LC_MESSAGES=en_US.UTF-8    LC_PAPER=de_CH.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
# [11] LC_MEASUREMENT=de_CH.UTF-8 LC_IDENTIFICATION=C       
# 
# time zone: Europe/Zurich
# tzcode source: system (glibc)
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] nlshrink_1.0.1    rootSolve_1.8.2.4 margins_0.3.26    tidyr_1.3.1       lme4_1.1-35.1     Matrix_1.6-5      ggplot2_3.4.4    
# [8] dplyr_1.1.4      
# 
# loaded via a namespace (and not attached):
#   [1] gtable_0.3.4       compiler_4.3.2     tidyselect_1.2.0   Rcpp_1.0.11        splines_4.3.2      scales_1.3.0       boot_1.3-28       
# [8] lattice_0.22-5     R6_2.5.1           generics_0.1.3     MASS_7.3-60        forcats_1.0.0      ggrepel_0.9.4      tibble_3.2.1      
# [15] nloptr_2.0.3       munsell_0.5.0      lubridate_1.9.3    minqa_1.2.6        prediction_0.3.14  pillar_1.9.0       rlang_1.1.2       
# [22] utf8_1.2.4         timechange_0.2.0   cli_3.6.1          withr_2.5.2        magrittr_2.0.3     grid_4.3.2         rstudioapi_0.15.0 
# [29] texreg_1.39.3      lifecycle_1.0.4    nlme_3.1-163       vctrs_0.6.5        data.table_1.14.10 glue_1.6.2         fansi_1.0.6       
# [36] colorspace_2.1-0   purrr_1.0.2        httr_1.4.7         tools_4.3.2        pkgconfig_2.0.3   
set.seed(2023)

source("estimate.ED.R")

source("01_gen_measures.R")

source("02_gen_output.R")

rm(list = ls());gc()