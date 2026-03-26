# make sure to first install all libraries listed below
library(here)
here()

library(lfe)
library(fixest)
library(dplyr)
library(car)
library(ggplot2)
library(patchwork)
library(stargazer)
library(Hmisc)
library(interflex)
library(countrycode)


library(spatstat)
library(sp)
library(rgdal)
library(rgeos)
library(sf)
library(raster)

# load parallelization
library(foreach)
library(doParallel)
library(parallel)
ncores <- detectCores()-1

## load analysis data, define outcome and control variables
source(here("scripts","define_vars.R"))

## define functions and paths for regressions, tables, plots
source(here("scripts","output_functions.R"))

## runs models and writes out Table 1 as base_7columns.tex
source(here("scripts","tab1_baseline.R"))

## prepares appendix table 44 and saves as base_cont_pred.tex
source(here("scripts","tabA1_cont_pred.R"))

## prepares appendix table A5 and saves as base_int_margin_dv.tex
source(here("scripts","tab_im_dv.R"))

## prepares appendix table A6 and saves as base_int_margin_pred.tex
source(here("scripts","tab_im_pred.R"))

## writes out Table 2 as suit_rf_benchmark.tex, writes out Figure 5 as suit_rf_horserace.pdf
# also writes Table A7 as suit_placebo.tex
source(here("scripts","tab_suit_rf.R"))

## make mechanism analysis: path dependence vs. serial correlation
# saves Table A8 as suit_mediators.tex
# saves Table 3 as mechanisms_suit.tex
# saves Table A9 as mechanisms_suit_both.tex
# saves Table A10 as mechanisms_suit_acde.tex
# saves Table A11 as mechanisms_suit_acde_both.tex
source(here("scripts","tab_suit_mech.R"))

## make spatial randomization inference
# Figure 3: RI_balance_crops_kppm_vargamma.pdf
# Figure 4: RI_outcomes_crops_kppm_vargamma
# Figure A2: right-hand panel: RI_ann_kppm.pdf
source(here("scripts","spatial_RI_kppm.R"))

## same for alternative but inappropriate standard point process model
# save RI_balance_crops_ppm.pdf
# save RI_outcomes_crops_ppm.pdf
# save Figure A2: left-hand panel as RI_ann_ppm.pdf
source(here("scripts","spatial_RI.R"))

## produce counterfactual maps
# Figure 2 as Map_CF_crops_kppm_both.pdf
# Figure A1 as Map_CF_crops_ppm_both.pdf
source(here("scripts","cf_maps.R"))

## Heterogeneity Analyses for appendix
# Figure A3: het_crop_by_crop.pdf
# Figure A4: countries_all.pdf
# Figure A5: het_empires_crops.pdf
# Table A12: het_amin.tex
# Table A13: het_settlers.tex
# Table A14: het_colonial_duration.tex
# Figure A6: het_ia_crops.pdf
# Figure A7: spillovers_minerals_new.pdf
source(here("scripts","heterogeneity.R"))

## Gloabl comparisons for Appendix
source(here("scripts","beyond_africa.R"))


