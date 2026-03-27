#####
## Paper:    Legislature Size and Welfare: Evidence from Brazil
## Authors:  Umberto Mignozzetti, Gabriel Cepaluni, and Danilo Freire
## Contact:  umbertomig(at)ucsd.edu
## Obs:      Analysis: all results
#####

## Starting
rm(list=ls())
set.seed(32473) # From random.org

# Needed packages
pkgs <- c('foreign', 'readxl', 'remotes', 'ggmap',
          'rdd', 'tidyverse', 'sp', 'sandwich','coefplot',
          'gridExtra', 'rdrobust','reshape2', 'stringi', 
          'grid', 'tidytext', 'caret', 'tm', 'scales',
          'SnowballC','triangle','survey', 'rddensity', 
          'RTextTools', 'haven', 'sets', 'magrittr',
          'ggpubr')

# Install if not already installed
installIfNot <- function(x) {
  if(x %in% rownames(installed.packages()) == FALSE) 
    install.packages(x, dependencies = T)
} 
lapply(pkgs, installIfNot)

# ## R Text Tools: warning: this package is hard to install
# if(sum(c('tree','maxent', 'RTextTools') %in% rownames(installed.packages()))<3) {
#   library(remotes)
#   install_version("tree", "1.0-37")
#   install_version("maxent", "1.3.3.1")
#   install_version("RTextTools", "1.4.2")
# }

# Load scripts
lapply(pkgs, require, character.only = T)
rm(pkgs, installIfNot)

## Warning: If "stargazer' not working, here is a patch by Alex Knorre: 
#  (https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53)
currentdir <- getwd()
setwd(tempdir())
remove.packages("stargazer")
download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz",
              destfile = "stargazer_5.2.3.tar.gz")
untar("stargazer_5.2.3.tar.gz")
stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
stargazer_src[1990] <- stargazer_src[1995]
stargazer_src[1995] <- ""
writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
install.packages("stargazer", repos = NULL, type="source")
library(stargazer)
unlink('stargazer_5.2.3.tar.gz')
unlink('stargazer', recursive = TRUE)
setwd(currentdir)
rm(list=ls())

## From this point on, the script assumes that the files are
## in a Desktop folder called "final".

# Load dataset
load('~/Desktop/final/datasets/datasets.RData') # Change 1
source('~/Desktop/final/code/auxFunctions.R')   # Change 2

# Save log screen
if (dir.exists('~/Desktop/final/results'))        # Optional 1
  unlink('~/Desktop/final/results', recursive = T)
dir.create('~/Desktop/final/results')             # Optional 1
sink("~/Desktop/final/results/logAnalyzing.txt",  # Optional 2
     append=F, split=TRUE)

#####
## Pre-Processing and Useful Functions
#####

# Saving containers
svtabs <- "~/Desktop/final/results/tables/" # Optional 3
svplots <- "~/Desktop/final/results/figs/"  # Optional 4
dir.create(svtabs)
dir.create(svplots)

# Control variables vector
controls <- c('pop2003', 'gdp', 'nseats2000', 'northeast', 'year')
controls_cs <- c('pop2003', 'gdp', 'nseats2000', 'northeast')

vhealth <- c('infmort', 'pnmort')
lhealth <- c('Infant Mortality', 'Postneonatal Mortality')

veduc <- c('avgstudent_elemclass', 'ideb_iy')
leduc <- c('Avg. Enroll. Elem. Educ.', 'Elem. Educ. Quality Index')

vmech1 <- c('npol_coamayor2004', 'totnfunc_app', 
            'totnfunc_career', 'nassist_counc')
lmech1 <- c('Mayoral Coalition Size', 'Num. Appointed Bureaucrats', 
            'Num. Career Bureaucrats', 'Num. Councilor Assistants')

vmech2 <- c('numfem_cc2004', 'n_nonwhite_councilor',
            'compbyseat', 'prop_apr_cc')
lmech2 <- c('Num. Female Councilors', 'Num. Non-white Councilors',
            'Competitors by Seat', 'Prop. Approv. Legislation')

# Notes Variables
nts <- c('\\footnotesize{Local linear RD Estimates using CCT Optimal Bandwidth Selection and Triangular Kernel. Quadratic Robust Standard Errors in Parentheses.}',
         '\\footnotesize{Controls: population, GDP per capita, number of seats in 2000, year, and dummy for northeast region.}')

#####
## Tables
#####

## Saving Tables
setwd(svtabs)

## Summary Tables
stargazer(dat %>% select(nseats2000_only2000,
                         pop2000, 
                         gdp_census2000_only2000,
                         prop_poverty2000, 
                         nseats2004,
                         infmort,
                         pnmort,
                         avgstudent_elemclass, 
                         ideb_iy, 
                         npol_coamayor2004, 
                         totnfunc_app,
                         totnfunc_career, 
                         nassist_counc,
                         numfem_cc2004, 
                         n_nonwhite_councilor,
                         compbyseat, 
                         prop_apr_cc),
          title='Summary Statistics', digits=2,
          covariate.labels = c('Number of Seats 2000', 
                               'Population 2000',
                               'GDP Census 2000', 
                               'Proportion of Poverty Census 2000',
                               'Number of Seats 2004', 
                               'Infant Mortality 2005-2008', 
                               'Postneonatal Mortality 2005-2008',
                               'Enrollment Elementary School 2005-2008', 
                               'Quality of Elementary School Index 2005-2008',
                               'Mayoral Pre-electoral Coalition Size 2004', 
                               'Number of Appointed Bureaucrats 2005-2008',
                               'Num. Career Bureaucrats', 
                               'Num. Councilor Assistants',
                               'Number Female Elected 2004',
                               'Number Non-White Elected 2004',
                               'Competition per Seat 2004',
                               'Proportion Approved Legislation 2005'),
          style='aer',
          summary.stat = c('n', 'mean', 'sd', 'min', 'max'),
          label = 'tab_descr_vars_paper',
          out='sumstat.tex',
          align=T)

stargazer(munLeg %>% select(PublicGoods, 
                            Oversight, 
                            Others, 
                            HealthEduc),
          title='Summary Statistics', digits=2,
          covariate.labels = c("Legislation -- Public Goods",
                               "Legislation -- Oversight",
                               "Legislation -- Others",
                               "Legislation -- Education and Health"),
          style='aer',
          label = 'tab_descr_vars_paper2',
          summary.stat = c('n', 'mean', 'sd', 'min', 'max'),
          out='sumstatleg.tex',
          align=T)

stargazer(surveyCC %>% select(supp_jobCouncilors, 
                              supp_demandCouncilors, 
                              supp_personalRequestsCouncilor, 
                              supp_lawCouncilors, 
                              supp_constrCouncilors),
          title='Summary Statistics', digits=2,
          summary.stat = c('n', 'mean', 'sd', 'min', 'max'),
          covariate.labels = c("Coalition Support -- Councilors Job Appointments",
                               "Coalition Support -- Councilors Demands",
                               "Coalition Support -- Councilors Personal Requests",
                               "Coalition Support -- Councilors Legislation",
                               "Coalition Support -- Councilors Constructions"),
          style='aer',
          label = 'tab_descr_vars_paper',
          out='sumstatsurv.tex',
          align=T)

#####
## Validity Check Research Design
#####

## Cattaneo et al. (2017a) manipulation test

# Running variable
# Local Linear
summary(rddensity(X = dat$dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=1, massPoints = TRUE))

# Quadratic
summary(rddensity(X = dat$dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=2, massPoints = TRUE))

# Cubic
summary(rddensity(X = dat$dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=3, massPoints = TRUE))

# Quartic
summary(rddensity(X = dat$dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=4, massPoints = TRUE))

# Placebo running variable
# Local Linear
summary(rddensity(X = dat$fake_dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=1, massPoints = TRUE))

# Quadratic
summary(rddensity(X = dat$fake_dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=2, massPoints = TRUE))

# Cubic
summary(rddensity(X = dat$fake_dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=3, massPoints = TRUE))

# Quartic
summary(rddensity(X = dat$fake_dist2005_2008_by1000[dat$year==2005],
                  vce = 'jackknife', p=4, massPoints = TRUE))

## Validity check 1
mod <- list()

# 1 - Additional Legislator - No control
mod[[1]] <- gen_rdrob(dat$nseats2004, dat$dist2005_2008_by1000, ctrls = NULL)

# 2 - Fake Cutoff Additional Legislator - No control
mod[[2]] <- gen_rdrob(dat$nseats2004, dat$fake_dist2005_2008_by1000, ctrls = NULL)

# 3 - Additional Legislator - Controls
mod[[3]] <- gen_rdrob(dat$nseats2004, dat$dist2005_2008_by1000, dat[,controls_cs])

# 4 - Fake Cutoff Additional Legislator - Controls
mod[[4]] <- gen_rdrob(dat$nseats2004, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# Print RD Validity Check
stargazer(mod,
          title = 'Research Design Validity Check',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c('Additional Number of Seats 2004 without Controls',
                            'Placebo Additional Number of Seats 2004 without Controls',
                            'Additional Number of Seats 2004 with Controls',
                            'Placebo Additional Number of Seats 2004 with Controls'),
          dep.var.labels.include=F,
          label='validitycheck',
          out='validity1.tex',
          add.lines = ls_mod(mod),
          notes=nts,
          omit.stat=c('all'), 
          model.numbers = F, 
          align=T,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          digits=2,
          se = ls_semod(mod))

## Validity check 2
mod <- list()

# 1 - Number of Seats Previous to Law
mod[[1]] <- gen_rdrob(dat$nseats2000_only2000, dat$dist2005_2008_by1000, dat[,'pop2003'])

# 1 - Population 2000 Census
mod[[2]] <- gen_rdrob(dat$pop2000, dat$dist2005_2008_by1000, dat[,'pop2003'])

# 2 - GDP 2000 Census
mod[[3]] <- gen_rdrob(dat$gdp_census2000_only2000, dat$dist2005_2008_by1000, dat[,'pop2003'])

# 3 - Poverty 2000 Census
mod[[4]] <- gen_rdrob(dat$prop_pov2000_only2000, dat$dist2005_2008_by1000, dat[,'pop2003'])

# Print RD Validity Check
stargazer(mod,
          title = 'Research Design Validity Check',
          keep='iv', 
          covariate.labels = 'LATE',
          column.labels = c('Number of Seats 2000',
                            'Population 2000 Census',
                            'GDP 2000 Census',
                            'Proportion Poverty 2000 Census'),
          dep.var.labels.include=F, 
          label='validitycheck2',
          out='validity2.tex',
          add.lines = ls_mod(mod),
          notes=nts,
          omit.stat=c('all'), 
          style='ajps', 
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          model.numbers = F, 
          align=T, 
          digits=2,
          se = ls_semod(mod))

#####
## Legislature Size and Welfare
#####

mod <- list()
modfake <- list()

# 1 - Infant Mortality
mod[[1]] <- gen_rdrob(dat$infmort, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[1]] <- gen_rdrob(dat$infmort, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# 2 - Postneonatal Infant Mortality
mod[[2]] <- gen_rdrob(dat$pnmort, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[2]] <- gen_rdrob(dat$pnmort, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# 3 - Enrollment Elementary Education
mod[[3]] <- gen_rdrob(dat$avgstudent_elemclass, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[3]] <- gen_rdrob(dat$avgstudent_elemclass, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# 4 - Elementary Education Quality
mod[[4]] <- gen_rdrob(dat$ideb_iy, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[4]] <- gen_rdrob(dat$ideb_iy, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)


# Print RD Robust Legislature Size and Welfare
stargazer(mod,
          title = 'Legislature Size and Welfare', 
          out='welfareoutc.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c(lhealth, leduc),
          dep.var.labels.include=F, label='welfoutc',
          add.lines = ls_mod(mod),
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          notes=nts,
          omit.stat=c('all'), style='ajps', 
          model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Legislature Size and Welfare -- Placebo', 
          out='placwelfareoutc.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c(lhealth, leduc),
          dep.var.labels.include=F, label='placwelfoutc',
          add.lines = ls_mod(modfake),
          notes=nts,
          omit.stat=c('all'), style='ajps',
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

### Mechanism 1
mod <- list()
modfake <- list()

## Access to Resources and Patronage
# 1 - Mayoral coalition
mod[[1]] <- gen_rdrob(dat$npol_coamayor, dat$dist2005_2008_by1000, dat[,controls_cs])
modfake[[1]] <- gen_rdrob(dat$npol_coamayor, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# 2 - Number of Politically Appointed Workers
mod[[2]] <- gen_rdrob(dat$totnfunc_app, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[2]] <- gen_rdrob(dat$totnfunc_app, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# 3 - Career Bureaucrats
mod[[3]] <- gen_rdrob(dat$totnfunc_career, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[3]] <- gen_rdrob(dat$totnfunc_career, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# 4 - Councilor Assistants
mod[[4]] <- gen_rdrob(dat$nassist_counc, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[4]] <- gen_rdrob(dat$nassist_counc, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# Print Results
stargazer(mod,
          title = 'Bargaining, Coalition, and Public Employment', 
          out='mechreg1.tex',
          keep='iv', 
          covariate.labels = 'LATE',
          column.labels = lmech1,
          dep.var.labels.include=F, 
          label='mechreg1',
          add.lines = ls_mod(mod),
          notes=nts,
          omit.stat=c('all'), style='ajps',
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Mechanism Regressions -- Placebo', 
          out='placmechreg1.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = lmech1,
          dep.var.labels.include=F, label='placmechreg1',
          add.lines = ls_mod(modfake),
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          notes=nts,
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

### Mechanism Alternative
mod <- list()
modfake <- list()

## Legislative Composition
# 1 - Female Elected
mod[[1]] <- gen_rdrob(dat$numfem_cc2004, dat$dist2005_2008_by1000, dat[,controls_cs])
modfake[[1]] <- gen_rdrob(dat$numfem_cc2004, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# 2 - Non-white
mod[[2]] <- gen_rdrob(dat$n_nonwhite_councilor, dat$dist2005_2008_by1000, dat[,controls_cs])
modfake[[2]] <- na_fill(mod[[2]])

## Electoral Incentives and Legislative Productivity
# 3 - Candidates per Seat
mod[[3]] <- gen_rdrob(dat$compbyseat, dat$dist2005_2008_by1000, dat[,controls_cs])
modfake[[3]] <- gen_rdrob(dat$compbyseat, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# 4 - Proportion Laws Approved City Council
mod[[4]] <- gen_rdrob(dat$prop_apr_cc, dat$dist2005_2008_by1000, dat[,controls_cs])
modfake[[4]] <- gen_rdrob(dat$prop_apr_cc, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# Print Results
stargazer(mod,
          title = 'Representation, Competitiveness, and Legislation Approval', 
          out='mechreg2.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = lmech2,
          dep.var.labels.include=F, label='mechreg2',
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          add.lines = ls_mod(mod),
          notes=nts,
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Representation, Competitiveness, and Legislation Approval -- Placebo',
          out='placmechreg2.tex',
          keep='iv',
          covariate.labels = 'LATE',
          column.labels = lmech2,
          dep.var.labels.include=F,
          label='placmechreg2',
          add.lines = ls_mod(modfake),
          notes=nts,
          omit.stat=c('all'), style='ajps',
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

## Appendix: further analysis
# Mayoral parties
mod <- list()
mod[[1]] <- gen_rdrob(dat$partyMayorPT, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[2]] <- gen_rdrob(dat$partyMayorPSDB, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[3]] <- gen_rdrob(dat$partyMayorPMDB, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[4]] <- gen_rdrob(dat$partyMayorPFL, dat$dist2005_2008_by1000, dat[,controls_cs])

modfake <- list()
modfake[[1]] <- gen_rdrob(dat$partyMayorPT, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[2]] <- gen_rdrob(dat$partyMayorPSDB, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[3]] <- gen_rdrob(dat$partyMayorPMDB, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[4]] <- gen_rdrob(dat$partyMayorPFL, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# Print Results
stargazer(mod,
          title = 'Mayors from Major Parties', 
          out='appPartyMayors.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c('Mayor from PT', 'Mayor from PSDB', 
                            'Mayor from PMDB', 'Mayor from PFL (DEM)'),
          dep.var.labels.include=F, label='appmayparty',
          add.lines = ls_mod(mod),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Mayors from Major Parties -- Placebo',
          out='placAppPartyMayors.tex',
          keep='iv',
          covariate.labels = 'LATE',
          column.labels = c('Mayor from PT', 'Mayor from PSDB', 
                            'Mayor from PMDB', 'Mayor from PFL (DEM)'),
          dep.var.labels.include=F,
          label='placappmayparty',
          add.lines = ls_mod(modfake),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps',
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

## Mayoral characteristics
mod <- list()
mod[[1]] <- gen_rdrob(dat$femaleMayor, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[2]] <- gen_rdrob(dat$collegeDegreeMayor, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[3]] <- gen_rdrob(dat$reelectedMayor2004, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[4]] <- gen_rdrob(dat$reelectedMayor2008, dat$dist2005_2008_by1000, dat[,controls_cs])

modfake <- list()
modfake[[1]] <- gen_rdrob(dat$femaleMayor, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[2]] <- gen_rdrob(dat$collegeDegreeMayor, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[3]] <- gen_rdrob(dat$reelectedMayor2004, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[4]] <- gen_rdrob(dat$reelectedMayor2008, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# Print Results
stargazer(mod,
          title = 'Mayoral Characteristics', 
          out='appMayorsChar.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c('Female Mayor', 'Mayor w. College Degree', 
                            'Reelected Mayor 2004', 'Reelected Mayor 2008'),
          dep.var.labels.include=F, label='appmaychar',
          add.lines = ls_mod(mod),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Mayoral characteristics -- Placebo',
          out='placAppMayorsChar.tex',
          keep='iv',
          covariate.labels = 'LATE',
          column.labels = c('Female Mayor', 'Mayor w. College Degree', 
                            'Reelected Mayor 2004', 'Reelected Mayor 2008'),
          dep.var.labels.include=F,
          label='placappmaychar',
          add.lines = ls_mod(modfake),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps',
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

## City-council major parties composition
mod <- list()
mod[[1]] <- gen_rdrob(dat$propCCPT, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[2]] <- gen_rdrob(dat$propCCPSDB, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[3]] <- gen_rdrob(dat$propCCPMDB, dat$dist2005_2008_by1000, dat[,controls_cs])
mod[[4]] <- gen_rdrob(dat$propCCPFL, dat$dist2005_2008_by1000, dat[,controls_cs])

modfake <- list()
modfake[[1]] <- gen_rdrob(dat$propCCPT, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[2]] <- gen_rdrob(dat$propCCPSDB, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[3]] <- gen_rdrob(dat$propCCPMDB, dat$fake_dist2005_2008_by1000, dat[,controls_cs])
modfake[[4]] <- gen_rdrob(dat$propCCPFL, dat$fake_dist2005_2008_by1000, dat[,controls_cs])

# Print Results
stargazer(mod,
          title = 'Councilors from Major Parties', 
          out='appPartyCouncil.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c('Councilors from PT', 'Councilors from PSDB', 
                            'Councilors from PMDB', 'Councilors from PFL (DEM)'),
          dep.var.labels.include=F, label='appcouncilparty',
          add.lines = ls_mod(mod),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Councilors from Major Parties -- Placebo',
          out='placAppPartyCouncil.tex',
          keep='iv',
          covariate.labels = 'LATE',
          column.labels = c('Councilors from PT', 'Councilors from PSDB', 
                            'Councilors from PMDB', 'Councilors from PFL (DEM)'),
          dep.var.labels.include=F,
          label='placappcouncilparty',
          add.lines = ls_mod(modfake),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps',
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))

## Transfers per capita
mod <- list()
mod[[1]] <- gen_rdrob(dat$log_total_transf_pc, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
mod[[2]] <- gen_rdrob(dat$log_transf_fpm_pc, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
mod[[3]] <- gen_rdrob(dat$log_transf_fundeb_pc, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
mod[[4]] <- gen_rdrob(dat$log_revenue_total_pc, dat$dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

modfake <- list()
modfake[[1]] <- gen_rdrob(dat$log_total_transf_pc, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[2]] <- gen_rdrob(dat$log_transf_fpm_pc, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[3]] <- gen_rdrob(dat$log_transf_fundeb_pc, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)
modfake[[4]] <- gen_rdrob(dat$log_revenue_total_pc, dat$fake_dist2005_2008_by1000, dat[,controls], clust = dat$ibge_code)

# Print Results
stargazer(mod,
          title = 'Transfers and Revenue', 
          out='appTransfRev.tex',
          keep='iv', covariate.labels = 'LATE',
          column.labels = c('Total Transfers', 'FPM Transfers', 
                            'Education Transfers', 'Total Revenue'),
          dep.var.labels.include=F, label='apptrev',
          add.lines = ls_mod(mod),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps', model.numbers = F, align=T, digits=2,
          se = ls_semod(mod))

stargazer(modfake,
          title = 'Transfers and Revenue -- Placebo',
          out='placTransfRev.tex',
          keep='iv',
          covariate.labels = 'LATE',
          column.labels = c('Total Transfers', 'FPM Transfers', 
                            'Education Transfers', 'Total Revenue'),
          dep.var.labels.include=F,
          label='plactrev',
          add.lines = ls_mod(modfake),
          notes=nts,
          star.cutoffs = c(.1, .05, .01),
          star.char = c("+", "*", "**"),
          omit.stat=c('all'), style='ajps',
          model.numbers = F, align=T, digits=2,
          se = ls_semod(modfake))


## SVM Accuracy for text data
## SVM fitting removed from materials because it takes too long to run
## Results available upon request at umbertomig (at) ucsd (dot) edu
tab7 <- data.frame(Variable = c('Local Public Goods', 
                                'Oversight', 
                                'Education and Healthcare', 
                                'Others'),
                   Accuracy = c(93.8, 94.9, 92.5, 93.5))

stargazer(tab7, 
          summary = F, 
          rownames = F, 
          table.placement = 'htb!',
          title = 'Accuracy of SVM Classifier (Tested in 20% of the Data)',
          out='tab7app.tex',
          label='tab7app')
rm(tab7)

## Survey Design Tables (Appendix Table 8)
dat.rake <- svydesign(ids=~1, weights=~weights, pcf=~pcf, data=surveyCC)
agetab <- prop.table(table(surveyCC$less39))
agetabrake <- prop.table(svytable(~less39, design = dat.rake))
femtab <- prop.table(table(surveyCC$female))
femtabrake <- prop.table(svytable(~female, design = dat.rake))
nstab <- prop.table(table(surveyCC$nseats))
nstabrake <- prop.table(svytable(~nseats, design = dat.rake))
regiontab <- prop.table(table(surveyCC$region))
regiontabrake <- prop.table(svytable(~region, design = dat.rake))

tab8 <- data.frame(
  Variables = c('Age lower than 39',
                'Female',
                'Number of Seats = 9',
                'Number of Seats = 10',
                'Number of Seats = 11',
                'Number of Seats = 12',
                'Number of Seats = 13',
                'Number of Seats = 14',
                'Number of Seats = 15 or more',
                'Region = Central-West',
                'Region = Northeast',
                'Region = North',
                'Region = Southeast',
                'Region = South'),
  SProp = c(agetab[2],
            femtab[2],
            nstab[7],
            as.numeric(nstab[1:6]),
            as.numeric(regiontab)),
  PProp = c(0.34, 0.12, 0.90, 0.06, 
            0.02, 0.01, 0.005, 0.004, 
            0.009, 0.08, 0.32, 0.08, 
            0.30, 0.21),
  WProp = c(agetabrake[2],
            femtabrake[2],
            nstabrake[7],
            as.numeric(nstabrake[1:6]),
            as.numeric(regiontabrake))
)
row.names(tab8) <- tab8$Variables
tab8$Variables <- NULL
names(tab8) <- c('Sample Proportions', 'Population Proportions', 'Weighted Proportions')

stargazer(tab8, 
          summary = F,
          digits = 2,
          table.placement = 'htb!',
          title = 'Proportions for Each Bin Used in the Weighting Process',
          out='tab8app.tex',
          label='tab8app')

rm(tab8, agetab, femtab, nstab, regiontab, dat.rake)

#####
## Graphs
#####

### Saving Graphs
setwd(svplots)

# ## Map Brazilian Municipalities
download.file("http://biogeo.ucdavis.edu/data/gadm2.8/rds/BRA_adm2.rds", 
              'map.rds', mode = 'wb')
gadm <- readRDS('map.rds')
unlink('map.rds')

state_name = data.frame(NAME_1 = sort(names(table(as.character(gadm$NAME_1)))),
                        state = c('AC', 'AL', 'AP', 'AM', 'BA', 'CE',
                                  'DF', 'ES', 'GO', 'MA', 'MT', 'MS',
                                  'MG', 'PA', 'PB', 'PR', 'PE', 'PI',
                                  'RJ', 'RN', 'RS', 'RO', 'RR', 'SC',
                                  'SP', 'SE', 'TO'))

mun <- data.frame(municipality = as.character(gadm$NAME_2),
                  NAME_1 = as.character(gadm$NAME_1))
for(i in 1:dim(state_name)[2]) state_name[,i] <- as.character(state_name[,i])
for(i in 1:dim(mun)[2]) mun[,i] <- as.character(mun[,i])
mun <- left_join(mun,state_name)
mun$municipality = as.character(mun$municipality)
mun$state = as.character(mun$state)
rm(state_name)

#Fix some names
mun$municipality[mun$municipality=='Lajedao'&mun$state=='BA']='Lajedão'
mun$municipality[mun$municipality=='Ipu'&mun$state=='CE']='Ipú'
mun$municipality[mun$municipality=='Goianira'&mun$state=='GO']='Goiania'
mun$municipality[mun$municipality=='Pirauba'&mun$state=='MG']='Piraúba'
mun$municipality[mun$municipality=='Rio doce'&mun$state=='MG']='Rio Doce'
mun$municipality[mun$municipality=='Chale'&mun$state=='MG']='Chalé'
mun$municipality[mun$municipality=='Inhacor'&mun$state=='RS']='Inhacorá'
mun$municipality[mun$municipality=='Marau'&mun$state=='RS']='Maraú'
mun$municipality[mun$municipality=='Portao'&mun$state=='RS']='Portão'
mun$municipality[mun$municipality=='urea'&mun$state=='RS']='Áurea'
mun$municipality[mun$municipality=='Ponta Alta'&mun$state=='SC']='Ponte Alta'
mun$municipality[mun$municipality=='Aguai'&mun$state=='SP']='Aguaí'
mun$municipality[mun$municipality=='Paranaparema'&mun$state=='SP']='Paranapanema'
mun$municipality[mun$municipality=='Pirajui'&mun$state=='SP']='Pirajuí'

mun$NAME_1 <- NULL
aux <- as_tibble(unique(dat[,c('municipality','ibge_code',
                     'state','dist2005_2008_by1000')]))
row.names(mun) <- NULL
mun <- left_join(mun, aux)

gadm$treatment <- abs(mun$dist2005_2008_by1000)
gadm$treatment[is.na(gadm$treatment)] = 46.811

pdf(file = 'sampplot2019.pdf', width = 8, height = 6)
spplot(gadm, "treatment", col.regions = gray.colors(50),
       main="Sample Selection Status of Brazilian Municipality",
       lty=3, lwd=0.1, par.settings = list(axis.line = list(col =  'transparent')),
       col='transparent',
       xlab = 'Municipalities Plotted by Population Thresholds Proximity')
dev.off()
rm(aux,mun,mun_aux,i,gadm)

## Plot Running Variable
cuts <- c(47.619, 95.238, 142.857, 190.476, 238.095, 285.714, 333.333,
          380.952, 428.571, 476.19, 523.809, 571.428)
fakecuts <- seq(from = 47619/2,to = 47619*13, 47619)/1000
df <- data.frame(Type = 'Cutoffs', cuts = cuts)
#df <- rbind(df,data.frame(Type = 'Placebo', cuts = fakecuts))
mpl <- ggplot() +
  geom_jitter(data = dat,
              aes(x = pop2003, y = nseats2004,
                  colour = -1*abs(dist2005_2008_by1000)),
              shape = 4, show.legend = F, inherit.aes = F,
              alpha = .3)+
  theme_bw() +
  scale_colour_gradient(low = "gray90", high = 'gray30', guide = "none") +
  geom_point(data = dat, aes(x = pop2003, y = nseats2004)) +
  scale_y_continuous(breaks=c(9:21)) + theme_bw(base_size = 16) +
  theme(axis.text.x  = element_text(angle=90, hjust=1, vjust=0.5, size=16)) +
  scale_x_continuous(breaks=cuts) +
  xlab("Population Cutoffs (2003)") +
  ylab("Number of Seats (2004 Election)") +
  # geom_vline(data=df, aes(xintercept = cuts,
  #                         group = as.factor(Type)),
  #            linetype = "dotted",  alpha = .5) +
  theme(plot.title = element_text(hjust = 0.5), legend.position = 'none', 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(
    axis.title.y = element_text(vjust = +2, size = 13),
    axis.title.x = element_text(vjust = -2, size = 13)
  ); mpl

ggsave(mpl, file="distrseats.pdf", width=10, height=7)
rm(mpl, cuts)

### Sensitivity to BW Choices
g1 <- bw_robust(vars = c(vhealth, veduc), nams = c(lhealth, leduc),
                dat = dat, vi = 'dist2005_2008_by1000', cs = rep(F,4),
                clust = dat$ibge_code, title = 'Legislature Size and Welfare', 
                regular = T,
                yl = 'Estimates')

g2 <- bw_robust(vars = vmech1, nams = lmech1,
                dat = dat, vi = 'dist2005_2008_by1000', 
                cs = c(T,F,F,T),
                clust = dat$ibge_code, 
                title = 'Bargaining, Coalition, and Public Employment', 
                regular = T,
                yl = 'Estimates')

g3 <- bw_robust(vars = vmech2, nams = lmech2,
                dat = dat, vi = 'dist2005_2008_by1000', cs = rep(T,4),
                clust = dat$ibge_code, title = 'Representation, Competitiveness, and\nLegislation Approval', regular = T,
                yl = 'Estimates')

res <- rbind(g1[[2]], g3[[2]], g2[[2]])
res$Variable <- factor(res$Variable, levels = unique(res$Variable))

pdf(file = 'bwsens.pdf', width = 10, height = 12)
ggplot(res, aes(x = bw, y = est)) +
  geom_line(size = 2) +
  geom_hline(yintercept=0, size=.5, linetype=3)+
  geom_vline(xintercept=1, size=.5, linetype=2, color=2)+
  theme_bw() + labs(title = 'Bandwidth Sensitivity') +
  xlab('') + ylab('') +
  geom_line(aes(x = bw, y = cilo), size = .7, linetype = 2) +
  geom_line(aes(x = bw, y = cihi), size = .7, linetype = 2) +
  facet_wrap(~Variable, ncol = 4)+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

## Placebo
g1 <- bw_robust(vars = c(vhealth, veduc), nams = c(lhealth, leduc),
                dat = dat, vi = 'fake_dist2005_2008_by1000', cs = rep(F,4),
                clust = dat$ibge_code, title = 'Legislature Size and Welfare - Placebo Cutoffs', regular = T,
                yl = 'Estimates')

g2 <- bw_robust(vars = vmech1, nams = lmech1,
                dat = dat, vi = 'fake_dist2005_2008_by1000', cs = c(T,F,F,T),
                clust = dat$ibge_code, title = 'Bargaining, Coalition, and Public Employment - Placebo Cutoffs', regular = T,
                yl = 'Estimates')

g3 <- bw_robust(vars = vmech2[-2], nams = lmech2[-2],
                dat = dat, vi = 'fake_dist2005_2008_by1000', cs = rep(T,3),
                clust = dat$ibge_code, title = 'Representation, Competitiveness, and \nLegislation Approval - Placebo Cutoffs',
                regular = T,
                yl = 'Estimates')

res <- rbind(g1[[2]], g3[[2]], g2[[2]])
res$Variable <- factor(res$Variable, levels = unique(res$Variable))

pdf(file = 'placbwsens.pdf', width = 10, height = 12)
ggplot(res, aes(x = bw, y = est)) +
  geom_line(size = 2) +
  geom_hline(yintercept=0, size=.5, linetype=3)+
  geom_vline(xintercept=1, size=.5, linetype=2, color=2)+
  theme_bw() + labs(title = 'Bandwidth Sensitivity -- Placebo Regressions') +
  xlab('') + ylab('') +
  geom_line(aes(x = bw, y = cilo), size = .7, linetype = 2) +
  geom_line(aes(x = bw, y = cihi), size = .7, linetype = 2) +
  facet_wrap(~Variable, ncol = 4)+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

### Polynomial degree
g1 <- rd_sensfform(vds = c(vhealth, veduc), 
                   label = c(lhealth, leduc),
                   dfr = dat, iv = 'dist2005_2008_by1000', 
                   cs = rep(F,4),
                   clust = dat$ibge_code, 
                   title = 'Legislature Size and Welfare')

g2 <- rd_sensfform(vds = vmech1, 
                   label = lmech1,
                   dfr = dat, iv = 'dist2005_2008_by1000', 
                   cs = c(T,F,F,F),
                   clust = dat$ibge_code, 
                   title = 'Bargaining, Coalition, and Public Employment')

g3 <- rd_sensfform(vds = vmech2, label = lmech2,
                   dfr = dat, iv = 'dist2005_2008_by1000', 
                   cs = rep(T,4),
                   clust = dat$ibge_code, 
                   title = 'Representation, Competitiveness, and Legislation Approval')

res <- rbind(g1[[2]], g3[[2]], g2[[2]])
res$label <- factor(res$label, levels = unique(res$label))

pdf(file = 'ffsens.pdf', width = 10, height = 12)
ggplot(data = res, aes(x=polyorder, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=coef - 1.645*SE, ymax=coef + 1.645*SE), size = .7,
                width = 0.5)+ xlab("") +
  ylab('') +
  ggtitle('Sensitivity to Local Polynomial Degree Order') +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() + facet_wrap(~label, ncol = 4)+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

# Placebo
g1 <- rd_sensfform(vds = c(vhealth, veduc), 
                   label = c(lhealth, leduc),
                   dfr = dat, iv = 'fake_dist2005_2008_by1000', cs = rep(F,4),
                   clust = dat$ibge_code, title = 'Legislature Size and Welfare -- Placebo Cutoffs')

g2 <- rd_sensfform(vds = vmech1, label = lmech1,
                   dfr = dat, iv = 'fake_dist2005_2008_by1000', cs = c(T,F,F,F),
                   clust = dat$ibge_code, title = 'Bargaining, Coalition, and Public Employment -- Placebo Cutoffs')

g3 <- rd_sensfform(vds = vmech2[-2], label = lmech2[-2],
                   dfr = dat, iv = 'fake_dist2005_2008_by1000', cs = rep(T,3),
                   clust = dat$ibge_code, title = 'Representation, Competitiveness, and Legislation Approval -- Placebo Cutoffs')

res <- rbind(g1[[2]], g3[[2]], g2[[2]])
res$label <- factor(res$label, levels = unique(res$label))

pdf(file = 'placffsens.pdf', width = 10, height = 14)
ggplot(data = res, aes(x=polyorder, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=coef - 1.645*SE, ymax=coef + 1.645*SE), size = .7,
                width = 0.5)+ xlab("") +
  ylab('') +
  ggtitle('Sensitivity to Local Polynomial Degree Order -- Placebo Cutoffs') +
  geom_hline(yintercept=0, linetype="dotted") +
  theme_bw() + facet_wrap(~label, ncol = 4)+
  theme(plot.title = element_text(hjust = 0.5))
dev.off()

### Dropping States
g1 <- rd_dropping(vars = vhealth, 
                  todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, 
                  cs = rep(F,4),
                  clust = dat$ibge_code, 
                  labs = lhealth,
                  title = 'Legislature Size and Welfare -- Healthcare')

g2 <- rd_dropping(vars = veduc, 
                  todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, 
                  cs = rep(F,4),
                  clust = dat$ibge_code, 
                  labs = leduc,
                  title = 'Legislature Size and Welfare -- Education')

pdf(file = 'dropstatesens1.pdf', width = 10, height = 14)
grid.arrange(g1[[1]], g2[[1]], ncol=1)
dev.off()

g3 <- rd_dropping(vars = vmech1[1:2], todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, cs = c(T,F),
                  clust = dat$ibge_code, labs = lmech1[1:2],
                  title = 'Bargaining, Coalition, and Public Employment')


g4 <- rd_dropping(vars = vmech1[3:4], todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, cs = c(F,F),
                  clust = dat$ibge_code, labs = lmech1[3:4],
                  title = 'Bargaining, Coalition, and Public Employment')

g5 <- rd_dropping(vars = vmech2[1:2], todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, cs = rep(T,4),
                  clust = dat$ibge_code, labs = lmech2[1:2],
                  title = 'Representation')

g6 <- rd_dropping(vars = vmech2[3:4], todrop = dat$state,
                  iv = dat$dist2005_2008_by1000, cs = rep(T,4),
                  clust = dat$ibge_code, labs = lmech2[3:4],
                  title = 'Competitiveness and Legislation Approval')

pdf(file = 'dropstatesens2.pdf', width = 10, height = 14)
grid.arrange(g3[[1]], g4[[1]], g5[[1]], g6[[1]], ncol=1)
dev.off()

## Placebo
g1 <- rd_dropping(vars = vhealth, 
                  todrop = dat$state,
                  iv = dat$fake_dist2005_2008_by1000, 
                  cs = rep(F,4),
                  clust = dat$ibge_code, 
                  labs = lhealth,
                  title = 'Legislature Size and Welfare -- Healthcare -- Placebo Cutoffs')

g2 <- rd_dropping(vars = veduc, 
                  todrop = dat$state,
                  iv = dat$fake_dist2005_2008_by1000, 
                  cs = rep(F,4),
                  clust = dat$ibge_code, 
                  labs = leduc,
                  title = 'Legislature Size and Welfare -- Education -- Placebo Cutoffs')

pdf(file = 'placdropstatesens1.pdf', width = 10, height = 14)
grid.arrange(g1[[1]], g2[[1]], ncol=1)
dev.off()

g3 <- rd_dropping(vars = vmech1, todrop = dat$state,
                  iv = dat$fake_dist2005_2008_by1000, cs = c(T,F,F,F),
                  clust = dat$ibge_code, labs = lmech1,
                  title = 'Bargaining, Coalition, and Public Employment -- Placebo Cutoffs')

g4 <- rd_dropping(vars = vmech2[-2], todrop = dat$state,
                  iv = dat$fake_dist2005_2008_by1000, cs = rep(T,3),
                  clust = dat$ibge_code, labs = lmech2[-2],
                  title = 'Representation, Competitiveness, and Legislation Approval -- Placebo Cutoffs')

pdf(file = 'placdropstatesens2.pdf', 
    width = 10, height = 14)
grid.arrange(g3[[1]], g4[[1]], ncol=1)
dev.off()

### Covariate Sensitivity
mod <- rd_senscontrol(vars = vmech1, labs = lmech1, 
                      cs = c(T, rep(F,2), T),
                      iv = dat$dist2005_2008_by1000,
                      clust = dat$ibge_code, dfr = dat)

mod <- rbind(mod, rd_senscontrol(
  vars = vmech2, labs = lmech2, cs = rep(T,4),
  iv = dat$dist2005_2008_by1000,
  clust = dat$ibge_code, dfr = dat))

mod$control <- gsub(' ', '', mod$control)

mod$control <- 
  factor(mod$control,
         levels = unique(mod$control[order(nchar(mod$control))]))

g1 <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5)+ facet_wrap(~label, nrow = 1) +
  coord_flip() + theme_bw() + xlab('') +
  ylab('Estimates (with 90% CI)') + geom_hline(yintercept=0, linetype="dotted") +
  ggtitle('Sensitivity to Control Variables - Political Outcomes')+
  theme(plot.title = element_text(hjust = 0.5))

g1

ggsave(g1, file="senscov1.pdf", width=14, height=8.5)

mod <- rd_senscontrol(vars = c(veduc, vhealth), 
                      labs = c(leduc, lhealth), 
                      cs = rep(F,4),
                      iv = dat$dist2005_2008_by1000,
                      clust = dat$ibge_code, dfr = dat)

mod$control <- gsub(' ', '', mod$control)

mod$control <- factor(mod$control,
                      levels = unique(mod$control[order(nchar(mod$control))]))

g2 <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5)+ facet_wrap(~label, nrow = 1) +
  coord_flip() + theme_bw() + xlab('') +
  ylab('Estimates (with 90% CI)') + geom_hline(yintercept=0, linetype="dotted") +
  ggtitle('Sensitivity to Control Variables - Welfare Outcomes')+
  theme(plot.title = element_text(hjust = 0.5))

g2

ggsave(g2, file="senscov2.pdf", width=14, height=8.5)

## Placebo outcomes
# Legislature Size and Welfare
mod <- rd_senscontrol(vars = c(vhealth, veduc), labs = c(lhealth, leduc),
                      cs = rep(F,4),
                      iv = dat$fake_dist2005_2008_by1000,
                      clust = dat$ibge_code, dfr = dat)
g1 <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5)+ facet_wrap(~label, nrow = 1) +
  coord_flip() + theme_bw() + xlab('RD Covariate Variables') +
  ylab('Estimates') + geom_hline(yintercept=0, linetype="dotted") +
  ggtitle('Sensitivity to Control Variables\nLegislature Size and Welfare -- Placebo Cutoffs')+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file = 'placsens1cov.pdf', width = 10, height = 16)
grid.draw(rbind(ggplotGrob(g1), size="first"))
dev.off()

# Mechanism Outcomes
mod <- rd_senscontrol(vars = vmech1, labs = lmech1, 
                      cs = c(T, rep(F,2),T),
                      iv = dat$fake_dist2005_2008_by1000,
                      clust = dat$ibge_code, dfr = dat)
g1 <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5)+ facet_wrap(~label, nrow = 1) +
  coord_flip() + theme_bw() + xlab('RD Covariate Variables') +
  ylab('Estimates') + geom_hline(yintercept=0, linetype="dotted") +
  ggtitle('Sensitivity to Control Variables\nBargaining, Coalition, and Public Employment -- Placebo Cutoffs')+
  theme(plot.title = element_text(hjust = 0.5))

mod <- rd_senscontrol(vars = vmech2[-2],
                      labs = lmech2[-2],
                      cs = rep(T,3),
                      iv = dat$fake_dist2005_2008_by1000,
                      clust = dat$ibge_code, dfr = dat)

g2 <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5) + facet_wrap(~label, nrow = 1) +
  coord_flip() + theme_bw() + xlab('RD Covariate Variables') +
  ylab('Estimates') + geom_hline(yintercept=0, linetype="dotted") +
  ggtitle('Sensitivity to Control Variables\nRepresentation, Competitiveness, and Legislation Approval Mechanisms -- Placebo Cutoffs')+
  theme(plot.title = element_text(hjust = 0.5))

pdf(file = 'placsens2cov.pdf', width = 10, height = 16)
grid.arrange(g1, g2, ncol=1, nrow=2)
dev.off()

### Adding cutoffs regression
g1 <- rd_addcuts(vars = c(vhealth, veduc),
                 iv = dat$dist2005_2008_by1000, cs = rep(F,4),
                 clust = dat$ibge_code, labs = c(lhealth, leduc),
                 title = 'Legislature Size and Welfare')

pdf(file = 'rdaddcuts1.pdf', width = 10, height = 14)
grid.draw(rbind(ggplotGrob(g1[[1]]),size="first"))
dev.off()

g3 <- rd_addcuts(vars = vmech1,
                 iv = dat$dist2005_2008_by1000, cs = c(T,F,F,T),
                 clust = dat$ibge_code, labs = lmech1,
                 title = 'Bargaining, Coalition, and Public Employment')

g4 <- rd_addcuts(vars = vmech2,
                 iv = dat$dist2005_2008_by1000, cs = rep(T,4),
                 clust = dat$ibge_code, labs = lmech2,
                 title = 'Representation, Competitiveness, and Legislation Approval')

pdf(file = 'rdaddcuts2.pdf', width = 10, height = 14)
grid.draw(rbind(ggplotGrob(g3[[1]]),ggplotGrob(g4[[1]]),
                size="first"))
dev.off()

## Placebo
g1 <- rd_addcuts(vars = c(vhealth, veduc), addcuts = seq(from = 47.619*2, to = 47.619*12, 47.619),
                 iv = dat$fake_dist2005_2008_by1000, cs = rep(F,4),
                 clust = dat$ibge_code, labs = c(lhealth, leduc),
                 title = 'Legislature Size and Welfare -- Placebo Cutoffs')

pdf(file = 'placrdaddcuts1.pdf', width = 10, height = 14)
grid.draw(rbind(ggplotGrob(g1[[1]]), size="first"))
dev.off()

g3 <- rd_addcuts(vars = vmech1, addcuts = seq(from = 47.619*2, to = 47.619*12, 47.619),
                 iv = dat$fake_dist2005_2008_by1000, cs = c(T,F,F,T),
                 clust = dat$ibge_code, labs = lmech1,
                 title = 'Bargaining, Coalition, and Public Employment -- Placebo Cutoffs')

g4 <- rd_addcuts(vars = vmech2[-2],
                 labs = lmech2[-2],
                 addcuts = seq(from = 47.619*2, to = 47.619*12, 47.619),
                 iv = dat$fake_dist2005_2008_by1000, cs = rep(T,4),
                 clust = dat$ibge_code,
                 title = 'Representation, Competitiveness, and Legislation Approval -- Placebo Cutoffs')

pdf(file = 'placrdaddcuts2.pdf', width = 10, height = 14)
grid.arrange(g3[[1]], g4[[1]], ncol=1, nrow=2)
dev.off()

#####
## Legislation Approval
#####

munLeg$ibge_code <- as.numeric(munLeg$ibge_code)

aux <- munLeg %>%
  mutate(treat = ifelse(dist2005_2008_by1000 < 0, 
                        'Control', 'Treatment')) %>%
  left_join(dat[,c('ibge_code', 'pop2003')] %>% na.omit() %>% unique()) %>%
  group_by(ibge_code) %>%
  summarize(treat = first(treat),
            PublicGoods = sum(PublicGoods),
            Oversight = sum(Oversight),
            Others = sum(Others),
            HealthEduc = sum(HealthEduc),
            pop2003 = mean(pop2003)) %>%
  na.omit() %>%
  mutate(PublicGoods = PublicGoods/pop2003,
         Oversight = Oversight/pop2003,
         Others = Others/pop2003,
         HealthEduc = HealthEduc/pop2003) %>%
  group_by(treat) %>%
  summarize(`PG Provision` = mean(PublicGoods),
            `Oversight` = mean(Oversight),
            `Health & Education` = mean(HealthEduc)) %>%
  gather(vars, val, `PG Provision`:`Health & Education`) %>%
  rename(`Treatment Status` = treat,
         Variables = vars,
         Values = val) %>%
  mutate(Variables = factor(Variables, levels = c('PG Provision',
                                                  'Oversight',
                                                  'Health & Education')))

p1 <- ggplot(aux, 
       aes(x = Variables, y = Values, 
           group = `Treatment Status`, 
           fill = `Treatment Status`)) +
  scale_fill_grey() +
  geom_bar(position = 'dodge', stat = 'identity') + 
  theme_bw() +
  xlab("") + ylab("") + 
  theme(legend.position = 'bottom') +
  facet_wrap(~'Legislation Approved (per capita / 1000)') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

aux2 <- munLeg %>%
  filter(HealthEduc == 1) %>%
  mutate(treat = ifelse(dist2005_2008_by1000<0, 
                        'Control', 'Treatment')) %>%
  left_join(dat[,c('ibge_code', 'pop2003')]) %>%
  group_by(ibge_code) %>%
  summarize(treat = first(treat),
            PublicGoods = sum(PublicGoods),
            Oversight = sum(Oversight),
            pop2003 = mean(pop2003)) %>%
  na.omit() %>%
  mutate(PublicGoods = PublicGoods/pop2003,
         Oversight = Oversight/pop2003) %>%
  group_by(treat) %>%
  summarize(`PG Provision` = mean(PublicGoods),
            `Oversight` = mean(Oversight)) %>%
  gather(vars, val, `PG Provision`:`Oversight`) %>%
  rename(`Treatment Status` = treat,
         Variables = vars,
         Values = val) %>%
  mutate(Variables = factor(Variables, levels = c('PG Provision',
                                                  'Oversight')))

p2 <- ggplot(aux2, 
             aes(x = Variables, y = Values, 
                 group = `Treatment Status`, 
                 fill = `Treatment Status`)) +
  scale_fill_grey() +
  geom_bar(position = 'dodge', stat = 'identity') + 
  theme_bw() +
  xlab("") + ylab("") + 
  theme(legend.position = 'bottom') +
  facet_wrap(~'Health and Education Leg. Approved (per capita / 1000)') + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggsave(plot = ggarrange(p1, p2, nrow=2, 
                        common.legend = TRUE, 
                        legend = "bottom"), 
       filename = 'laws.pdf', width = 8, height = 10)

#####
## Survey analysis
#####
dat.rake <- svydesign(ids=~1, weights=~weights, pcf=~pcf, data=surveyCC)

### Support Mayor -- Jobs
# Q: Which strategies do mayors use to secure city council
#    support?

# Appointments
res <- data.frame(svytable(~supp_jobCouncilors, dat.rake))
names(res) <- c('value', 'count_w')
res$pct_w <- res$count_w/sum(res$count_w)
res$Vars <- 'Job\nAppointments'

# Demands from voters
aux <- data.frame(svytable(~supp_demandCouncilors, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Grant Voters\nDemands'
res <- rbind(res,aux)

# Personal requests
aux <- data.frame(svytable(~supp_personalRequestsCouncilor, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Grant Councilor\'s\nPersonal Requests'
res <- rbind(res,aux)

# Lawmaking
aux <- data.frame(svytable(~supp_lawCouncilors, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Lawmaking\nSupport'
res <- rbind(res,aux)

# Constructions
aux <- data.frame(svytable(~supp_constrCouncilors, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Constructions that\nFavors Councilor'
res <- rbind(res,aux)

res <- filter(res, value==1)

# Plot 
g1 <- ggplot(res, aes(x=fct_reorder(Vars, pct_w, .desc = T), y=pct_w)) +
  geom_bar(stat="identity") +
  geom_text(data=res, aes(label=paste0(round(pct_w*100,1),"%")), 
            vjust= -0.5, color = 'black', size=3.5, fontface = 2) +
  scale_x_discrete(
  #  guide = guide_axis(n.dodge = 2)
    ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=10),
        legend.position = 'none', 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ylim(0,0.7) +
  xlab('') + ylab(''); g1

ggsave(g1, file="strategySupportMayors.pdf", width=8, height=5); rm(g1)

### Common activities
# Constructions
res <- data.frame(svytable(~common_grantHospitalAccess, dat.rake))
names(res) <- c('value', 'count_w')
res$pct_w <- res$count_w/sum(res$count_w)
res$Vars <- 'Help hospital\n admission'

# Fix potholes
aux <- data.frame(svytable(~common_fixPotHoles, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Fix street potholes'
res <- rbind(res,aux)

# Discuss legislation
aux <- data.frame(svytable(~common_discussLegislation, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Legislative duties'
res <- rbind(res,aux)

### Get medication
aux <- data.frame(svytable(~common_getMedication, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Help get medication'
res <- rbind(res,aux)

aux <- data.frame(svytable(~common_overseeConstruction, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Oversee construction'
res <- rbind(res,aux)

aux <- data.frame(svytable(~common_getLocalPublicGoods, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Other local public goods'
res <- rbind(res,aux)

aux <- data.frame(svytable(~common_grantSchoolAccess, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Help school admission'
res <- rbind(res,aux)

aux <- data.frame(svytable(~common_visitSchools, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Oversee school quality'
res <- rbind(res,aux)

aux <- data.frame(svytable(~common_lawCelebration, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Honors legislation'
res <- rbind(res,aux)

res$Vars <- factor(res$Vars, 
                   levels = c('Help hospital\n admission',
                              'Help school admission',
                              'Help get medication',
                              'Fix street potholes',
                              'Other local public goods',
                              'Legislative duties',
                              'Honors legislation',
                              'Oversee school quality',
                              'Oversee construction'))
res <- filter(res, value==1)

# Plot 
g1 <- ggplot(res, aes(x=fct_reorder(Vars, pct_w, .desc = T), y=pct_w)) +
  geom_bar(stat="identity") +
  geom_text(data=res, aes(label=paste0(round(pct_w*100,1),"%")), 
            vjust= 1, color = 'gray90', size=4, fontface = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))+
  xlab('How common in the councilor\'s practice are the following services?') +
  ylab('')

g1
ggsave(g1, file="commonJobsCouncilors.pdf", width=8, height=5); rm(g1)

### Electoral Yield activities
# Constructions
res <- data.frame(svytable(~getvot_grantHospitalAccess, dat.rake))
names(res) <- c('value', 'count_w')
res$pct_w <- res$count_w/sum(res$count_w)
res$Vars <- 'Help hospital\n admission'

# Fix potholes
aux <- data.frame(svytable(~getvot_fixPotHoles, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Fix street potholes'
res <- rbind(res,aux)

# Discuss legislation
aux <- data.frame(svytable(~getvot_discussLegislation, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Legislative duties'
res <- rbind(res,aux)

### Get medication
aux <- data.frame(svytable(~getvot_getMedication, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Help get medication'
res <- rbind(res,aux)

aux <- data.frame(svytable(~getvot_overseeConstruction, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Oversee construction'
res <- rbind(res,aux)

aux <- data.frame(svytable(~getvot_getLocalPublicGoods, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Other local public goods'
res <- rbind(res,aux)

aux <- data.frame(svytable(~getvot_grantSchoolAccess, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Help school admission'
res <- rbind(res,aux)

aux <- data.frame(svytable(~getvot_visitSchools, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Oversee school quality'
res <- rbind(res,aux)

aux <- data.frame(svytable(~getvot_lawCelebration, dat.rake))
names(aux) <- c('value', 'count_w')
aux$pct_w <- aux$count_w/sum(aux$count_w)
aux$Vars <- 'Honors legislation'
res <- rbind(res,aux)

res$Vars <- factor(res$Vars, 
                   levels = c('Help hospital\n admission',
                              'Help school admission',
                              'Help get medication',
                              'Fix street potholes',
                              'Other local public goods',
                              'Legislative duties',
                              'Honors legislation',
                              'Oversee school quality',
                              'Oversee construction'))
res <- filter(res, value==1)

# Plot 
g1 <- ggplot(res, aes(x=fct_reorder(Vars, pct_w, .desc = T), y=pct_w)) +
  geom_bar(stat="identity") +
  geom_text(data=res, aes(label=paste0(round(pct_w*100,1),"%")), 
            vjust= 1, color = 'gray90', size=4, fontface = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))+
  xlab('Does the service gives many/some votes? (versus few/none)') +
  ylab('')
g1

ggsave(g1, file="elecYieldJobsCouncilors.pdf", width=8, height=5); rm(g1)

# Plot Survey Respondents Geolocated
p <- ggmap(BRMapSurveyCC)
p <- p + geom_point(data = surveyCClatlon, 
                    aes(x = lon, y = lat),
                    color = "black", 
                    pch = 18, 
                    size=3) +
  ggtitle('Distribution of Responses -- 2016 City Counselors Survey') +
  xlab('Longitude') + ylab('Latitude')
p
ggsave(p, file="distrrespsurvey2016.pdf", width = 10, height = 10)

#####
## Simulation RD Multiple Cutoffs
#####
set.seed(123)

## Few data generating processes
pdf(file = 'simdistrRDD.pdf', width = 8, height = 8)
par(mfrow = c(2,2))
# Uniform: Beta(1,1)
curve(dbeta(x, 1, 1), from = 0, to = 1, lwd = 2,
      main = 'Uniform Distribution -- Beta(1,1)',
      ylim = c(0,3), ylab = '', xlab = '')
for (i in seq(0.1,0.9,0.1)) abline(v=i, lwd = 1, lty = 3)
# Triangular: Beta(1,2)
curve(dbeta(x, 1, 2), from = 0, to = 1, lwd = 2,
      main = 'Triangular Distribution -- Beta(1,2)',
      ylim = c(0,3), ylab = '', xlab = '')
for (i in seq(0.1,0.9,0.1)) abline(v=i, lwd = 1, lty = 3)
# U-shaped: Beta(0.5, 0.5)
curve(dbeta(x, 0.5, 0.5), from = 0, to = 1, lwd = 2,
      main = 'U-Shaped Distribution -- Beta(0.5,0.5)',
      ylim = c(0,3), ylab = '', xlab = '')
for (i in seq(0.1,0.9,0.1)) abline(v=i, lwd = 1, lty = 3)
# Inverted U-shaped: Beta(2,2)
curve(dbeta(x, 2, 2), from = 0, to = 1, lwd = 2,
      main = 'Inverted U-Shaped Distribution -- Beta(2,2)',
      ylim = c(0,3), ylab = '', xlab = '')
for (i in seq(0.1,0.9,0.1)) abline(v=i, lwd = 1, lty = 3)
dev.off()

## Runing simulations
e1 <- seq(1,10, 1)
e2 <- c(1, 2, 2, 1, 2, 2, 1, 2, 2, 1)
e3 <- c(1, 1+0.9, 1+0.9+0.8, 1+0.9+0.8+0.7,
        1+0.9+0.8+0.7+0.6, 1+0.9+0.8+0.7+0.6+0.5,
        1+0.9+0.8+0.7+0.6+0.5+0.4,
        1+0.9+0.8+0.7+0.6+0.5+0.4+0.3,
        1+0.9+0.8+0.7+0.6+0.5+0.4+0.3+0.2,
        1+0.9+0.8+0.7+0.6+0.5+0.4+0.3+0.2+0.1)

modelsN <- list()
modelsC <- list()

modelsN[[1]] <- funcsims(jumps=e1, controlled = F, addnoise = T)
modelsN[[2]] <- funcsims(jumps=e2, controlled = F, addnoise = T)
modelsN[[3]] <- funcsims(jumps=e3, controlled = F, addnoise = T)
modelsN[[4]] <- funcsims(jumps=e1, controlled = F, shape1 = 1, shape2 = 2, addnoise = T)
modelsN[[5]] <- funcsims(jumps=e2, controlled = F, shape1 = 1, shape2 = 2, addnoise = T)
modelsN[[6]] <- funcsims(jumps=e3, controlled = F, shape1 = 1, shape2 = 2, addnoise = T)
modelsN[[7]] <- funcsims(jumps=e1, controlled = F, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsN[[8]] <- funcsims(jumps=e2, controlled = F, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsN[[9]] <- funcsims(jumps=e3, controlled = F, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsN[[10]] <- funcsims(jumps=e1, controlled = F, shape1 = 2, shape2 = 2, addnoise = T)
modelsN[[11]] <- funcsims(jumps=e2, controlled = F, shape1 = 2, shape2 = 2, addnoise = T)
modelsN[[12]] <- funcsims(jumps=e3, controlled = F, shape1 = 2, shape2 = 2, addnoise = T)

modelsC[[1]] <- funcsims(jumps=e1, controlled = T, addnoise = T)
modelsC[[2]] <- funcsims(jumps=e2, controlled = T, addnoise = T)
modelsC[[3]] <- funcsims(jumps=e3, controlled = T, addnoise = T)
modelsC[[4]] <- funcsims(jumps=e1, controlled = T, shape1 = 1, shape2 = 2, addnoise = T)
modelsC[[5]] <- funcsims(jumps=e2, controlled = T, shape1 = 1, shape2 = 2, addnoise = T)
modelsC[[6]] <- funcsims(jumps=e3, controlled = T, shape1 = 1, shape2 = 2, addnoise = T)
modelsC[[7]] <- funcsims(jumps=e1, controlled = T, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsC[[8]] <- funcsims(jumps=e2, controlled = T, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsC[[9]] <- funcsims(jumps=e3, controlled = T, shape1 = 0.5, shape2 = 0.5, addnoise = T)
modelsC[[10]] <- funcsims(jumps=e1, controlled = T, shape1 = 2, shape2 = 2, addnoise = T)
modelsC[[11]] <- funcsims(jumps=e2, controlled = T, shape1 = 2, shape2 = 2, addnoise = T)
modelsC[[12]] <- funcsims(jumps=e3, controlled = T, shape1 = 2, shape2 = 2, addnoise = T)

mod <- data.frame(label = c(rep('Uniform', 3),
                            rep('Triangular', 3),
                            rep('U-Shaped', 3),
                            rep('Inverted-U Shaped', 3)),
                  control = 'No Controls',
                  effect = rep(c('1.0', '0.5', '0.0'), 4),
                  coef = as.numeric(lapply(modelsN, mean)),
                  SE = as.numeric(lapply(modelsN, sd)))

mod <- rbind(mod, data.frame(label = c(rep('Uniform', 3),
                                       rep('Triangular', 3),
                                       rep('U-Shaped', 3),
                                       rep('Inverted-U Shaped', 3)),
                             control = 'Controls',
                             effect = rep(c('1.0', '0.0', '0.5'), 4),
                             coef = as.numeric(lapply(modelsC, mean)),
                             SE = as.numeric(lapply(modelsC, sd))))

g <- mod %>%
  mutate(conf.low = coef-1.645*SE,
         conf.high = coef+1.645*SE) %>%
  group_by(control) %>%
  ggplot(aes(x=control, y=coef)) + geom_point() +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), size = .7,
                width = 0.5) + facet_wrap(~label+effect, nrow = 4) +
  theme_bw() + xlab('RD Estimates (90% CI)') +
  ylab('Estimates') + geom_hline(data = mod,
                                 aes(yintercept = as.numeric(
                                   as.character(effect))),
                                 linetype="dotted")+
  ggtitle('Simulations Multiple Threshold Regressions')+
  theme(plot.title = element_text(hjust = 0.5))

ggsave(g, file="estimsRDD.pdf", width=8, height=14)

rm(g, mod, modelsC, modelsN, e1, e2, e3)

#####
## End of Processing
#####
sessionInfo()
date()

## End of file
sink()

## Info
# > sessionInfo()
# R version 4.3.1 (2023-06-16)
# Platform: aarch64-apple-darwin20 (64-bit)
# Running under: macOS Ventura 13.4.1
# 
# Matrix products: default
# BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.3-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.11.0
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# time zone: America/Los_Angeles
# tzcode source: internal
# 
# attached base packages:
#   [1] grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] stargazer_5.2.3  ggpubr_0.6.0     magrittr_2.0.3   sets_1.0-24      haven_2.5.3     
# [6] RTextTools_1.4.3 SparseM_1.81     rddensity_2.4    survey_4.2-1     Matrix_1.6-0    
# [11] triangle_1.0     SnowballC_0.7.1  scales_1.2.1     tm_0.7-11        NLP_0.2-1       
# [16] caret_6.0-94     lattice_0.21-8   tidytext_0.4.1   stringi_1.7.12   reshape2_1.4.4  
# [21] rdrobust_2.1.1   gridExtra_2.3    coefplot_1.2.8   sp_2.0-0         lubridate_1.9.2 
# [26] forcats_1.0.0    stringr_1.5.0    dplyr_1.1.2      purrr_1.0.1      readr_2.1.4     
# [31] tidyr_1.3.0      tibble_3.2.1     tidyverse_2.0.0  rdd_0.57         Formula_1.2-5   
# [36] AER_1.2-10       survival_3.5-5   car_3.1-2        carData_3.0-5    lmtest_0.9-40   
# [41] zoo_1.8-12       sandwich_3.0-2   ggmap_3.0.2      ggplot2_3.4.2    remotes_2.4.2.1 
# [46] readxl_1.4.3     foreign_0.8-84  
# 
# loaded via a namespace (and not attached):
#   [1] rstudioapi_0.15.0    shape_1.4.6          farver_2.1.1         rmarkdown_2.23      
# [5] ragg_1.2.5           vctrs_0.6.3          tau_0.0-24           rstatix_0.7.2       
# [9] htmltools_0.5.5      broom_1.0.5          janeaustenr_1.0.0    cellranger_1.1.0    
# [13] pROC_1.18.4          parallelly_1.36.0    tokenizers_0.3.0     plyr_1.8.8          
# [17] lifecycle_1.0.3      iterators_1.0.14     pkgconfig_2.0.3      R6_2.5.1            
# [21] fastmap_1.1.1        future_1.33.0        digest_0.6.33        useful_1.2.6        
# [25] colorspace_2.1-0     textshaping_0.3.6    labeling_0.4.2       randomForest_4.7-1.1
# [29] fansi_1.0.4          timechange_0.2.0     httr_1.4.6           abind_1.4-5         
# [33] compiler_4.3.1       proxy_0.4-27         withr_2.5.0          backports_1.4.1     
# [37] DBI_1.1.3            RgoogleMaps_1.4.5.3  ggsignif_0.6.4       MASS_7.3-60         
# [41] lava_1.7.2.1         tree_1.0-43          caTools_1.18.2       ModelMetrics_1.2.2.2
# [45] tools_4.3.1          future.apply_1.11.0  nnet_7.3-19          glue_1.6.2          
# [49] nlme_3.1-162         generics_0.1.3       recipes_1.0.6        gtable_0.3.3        
# [53] tzdb_0.4.0           class_7.3-22         data.table_1.14.8    hms_1.1.3           
# [57] xml2_1.3.5           utf8_1.2.3           foreach_1.5.2        pillar_1.9.0        
# [61] lpdensity_2.4        mitools_2.4          splines_4.3.1        tidyselect_1.2.0    
# [65] knitr_1.43           stats4_4.3.1         xfun_0.39            hardhat_1.3.0       
# [69] timeDate_4022.108    yaml_2.3.7           evaluate_0.21        codetools_0.2-19    
# [73] cli_3.6.1            rpart_4.1.19         systemfonts_1.0.4    munsell_0.5.0       
# [77] Rcpp_1.0.11          globals_0.16.2       png_0.1-8            parallel_4.3.1      
# [81] gower_1.0.1          assertthat_0.2.1     jpeg_0.1-10          bitops_1.0-7        
# [85] listenv_0.9.0        glmnet_4.1-7         slam_0.1-50          ipred_0.9-14        
# [89] prodlim_2023.03.31   e1071_1.7-13         crayon_1.5.2         rlang_1.1.1         
# [93] cowplot_1.1.1       
# > date()
# [1] "Sun Aug  6 21:13:05 2023"
