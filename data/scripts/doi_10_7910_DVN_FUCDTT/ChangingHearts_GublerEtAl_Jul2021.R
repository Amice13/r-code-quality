
## The final version of this script was edited in 7 July 2021

## To run it, you need to change the setwd() command on line 305 of
## the script to reference the replication folder on your local machine

################################################################################
##### Preparation
################################################################################

########################################
#### Packages
########################################

## Load packages (install them first if they are not yet installed)
library(ggplot2)
library(scales)
library(reshape2)
library(data.table)
library(car)
library(psych)
library(apsrtable)
library(foreign)
library(haven)
library(cowplot)
library(lmtest)
library(ggforce)
library(RItools)
library(interflex)
library(stargazer)
library(GGally)

########################################
#### Settings
########################################

## Set R options
options(digits = 2, width = 80, dev = "pdf", scipen = 8)

## Set ggplot options
theme_new <- theme_set(theme_bw(base_family = "serif", base_size = 10))
theme_new <- theme_update(
    axis.title.x = element_text(vjust = -0.5),
    axis.title.y = element_text(vjust = 1.25, angle = 90)
)

########################################
#### Session Info
########################################

## This is a printout from the sessionInfo() command, which notes the system,
## version of R, and version of the packages used to generate our results:

## sessionInfo()

## R version 4.1.0 (2021-05-18)
## Platform: x86_64-apple-darwin17.0 (64-bit)
## Running under: macOS Big Sur 10.16

## Matrix products: default
## BLAS:   /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRblas.dylib
## LAPACK: /Library/Frameworks/R.framework/Versions/4.1/Resources/lib/libRlapack.dylib

## locale:
## [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base

## other attached packages:
##  [1] GGally_2.1.2      stargazer_5.2.2   interflex_1.2.6   RItools_0.1-17
##  [5] SparseM_1.81      ggforce_0.3.3     lmtest_0.9-38     zoo_1.8-9
##  [9] cowplot_1.1.1     haven_2.4.1       foreign_0.8-81    apsrtable_0.8-8
## [13] psych_2.1.6       car_3.0-11        carData_3.0-4     data.table_1.14.0
## [17] reshape2_1.4.4    scales_1.1.1      ggplot2_3.3.5

## loaded via a namespace (and not attached):
##  [1] nlme_3.1-152         svd_0.5              doParallel_1.0.16
##  [4] RColorBrewer_1.1-2   tools_4.1.0          utf8_1.2.1
##  [7] R6_2.5.0             DBI_1.1.1            mgcv_1.8-36
## [10] colorspace_2.0-2     withr_2.4.2          tidyselect_1.1.1
## [13] gridExtra_2.3        mnormt_2.0.2         curl_4.3.2
## [16] compiler_4.1.0       sandwich_3.0-1       mvtnorm_1.1-2
## [19] Lmoments_1.3-1       stringr_1.4.0        digest_0.6.27
## [22] rio_0.5.27           pkgconfig_2.0.3      parallelly_1.26.1
## [25] rlang_0.4.11         readxl_1.3.1         gridGraphics_0.5-1
## [28] farver_2.1.0         generics_0.1.0       dplyr_1.0.7
## [31] ModelMetrics_1.2.2.2 zip_2.2.0            magrittr_2.0.1
## [34] ggplotify_0.0.7      Formula_1.2-4        Matrix_1.3-4
## [37] Rcpp_1.0.6           munsell_0.5.0        fansi_0.5.0
## [40] abind_1.4-5          lifecycle_1.0.0      stringi_1.6.2
## [43] pROC_1.17.0.1        MASS_7.3-54          plyr_1.8.6
## [46] grid_4.1.0           parallel_4.1.0       listenv_0.8.0
## [49] forcats_0.5.1        crayon_1.4.1         lattice_0.20-44
## [52] splines_4.1.0        hms_1.1.0            tmvnsim_1.0-2
## [55] pillar_1.6.1         pcse_1.9.1.1         codetools_0.2-18
## [58] glue_1.4.2           BiocManager_1.30.16  AER_1.2-9
## [61] vctrs_0.3.8          tweenr_1.0.2         foreach_1.5.1
## [64] cellranger_1.1.0     gtable_0.3.0         purrr_0.3.4
## [67] polyclip_1.10-0      reshape_0.8.8        future_1.21.0
## [70] assertthat_0.2.1     openxlsx_4.2.4       xtable_1.8-4
## [73] lfe_2.8-6            survival_3.2-11      tibble_3.1.2
## [76] iterators_1.0.13     rvcheck_0.1.8        globals_0.14.0
## [79] ellipsis_0.3.2

########################################
#### Functions
########################################

## A function to standardize variables on a 0 to 1 scale
zero_to_one <- function(x, na.rm = T)
{
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x,na.rm = T))
}

## Two functions to format numbers
format_int <- function(x)
{
    formatC(x, digits = 0, format = "f", big.mark = ",")
}
format_dec <- function(x)
{
    formatC(x, digits = 2, format = "f", big.mark = ",")
}

## Function to calculate Cronbach's Alpha values for the first study
cbfunc <- function(data){
    require(psych)
    attach(data)
    cbvars <- list(
        possec = data.frame(i_admire, i_love),
        negsec = data.frame(i_resent, i_shame),
        pospri = data.frame(i_excite, i_plea),
        negpri = data.frame(i_fear, i_anger),
        icb_pre = data.frame(m_less, m_learn, m_suffer),
        diss = data.frame(d_uncom, d_uneasy, d_bother, d_tense, d_concern),
        icb_post = data.frame(gji_violence, gji_lazy, gji_work, gji_honest,
            gji_victim),
        emp = data.frame(e_sym, e_moved, e_com, e_warm, e_soft, e_tender),
        law = data.frame(law_english, law_tuition, law_welfare, law_hire),
        bills = data.frame(st8_hb116, st8_hb469, st8_hb466),
        harm = data.frame(law_english, law_tuition, law_welfare, law_hire,
            immig_opinion_reverse, arizona_law, st8_hb497),
        help = data.frame(st8_hb116, st8_hb469, st8_hb466)
        )
    detach(data)
    cbvalues <- sapply(
        names(cbvars),
        function(x){
            psych::alpha(cbvars[[x]])$total$raw_alpha
        })
    names(cbvalues) <- names(cbvars)
    return(cbvalues)
}

## Marginal Effects function
TwowayME.f <- function(M, X, Z, level)
{
    ## A function to generate 2-way marginal effects plots in R.
    ## Written by Joshua Gubler ~  http://scholar.byu.edu/jgubler; originally
    ## based on Stata code from Joel Selway and Brambor, Clark, and Golder.
    ## Last modified: 12 October 2014 (Move to ggplot2 and reduce output to
    ## values rather than graph - David Romney)
    ## Variables must be in the following order: y = x z (control variables
    ## here) xz. The model can include as many control variables as you need.
    ## M = an object of type "lm," "glm," or other estimation -- i.e. the object
    ## that contains the regression estimation you seek to plot.
    ## X = the variable whose effect on Y you seek to plot
    ## Z = the moderating variable (will be positioned on the X-axis of the
    ## plot)
    ## xlab = label for x-axis (in quotes)
    ## ylab = label for y-axis (in quotes)
    ## level = to set the confidence level. Two options (don't put these in
    ## quotes): 95, 90.  If you do not specify either option, the confidence
    ## intervals will not be correct.
    ## Example: TwowayME.f(estimation.lm, ses, edu, "Education levels",
    ## "Effect of SES on Civil War", 90)

    S <- summary(M)
    N <- c(1:20)

    ## 20 equally-spaced values for the moderating variable
    zmin <- rep(min(Z, na.rm = TRUE), 20)
    zmax <- rep(max(Z, na.rm = TRUE), 20)
    Znew <- (((N - 1) / (20 - 1)) * (zmax - zmin)) + zmin

    ## Grab elements of coefficient and vcov matrix
    H <- head(S$coefficients, 3)
    T <- tail(S$coefficients, 1)
    b <- rbind(H, T)
    Vcov <- vcov(M)
    Vcov <- as.data.frame(Vcov)
    Vcov1 <- Vcov[, c(1:3)]
    Vcov2 <- Vcov[, -c(0:0 - length(Vcov))]
    Vcov <- cbind(Vcov1, Vcov2)
    Vh <- head(Vcov, 3)
    Vt <- tail(Vcov, 1)
    V <- rbind(Vh, Vt)
    b1 <- b[2, 1]
    b2 <- b[3, 1]
    b3 <- b[4, 1]
    varb1 <- V[2, 2]
    varb2 <- V[3, 3]
    varb3 <- V[4, 4]
    covb1b3 <- V[4, 2]
    covb2b3 <- V[4, 3]

    ## Calculate ME values
    conb <- b1 + b3 * Znew

    ## Calculate standard errors when W = 0, W = 1, W = 2, W = 3, and when W = 4
    conse <- sqrt(varb1 + varb3 * (Znew^2) + 2 * covb1b3 * Znew)

    ## Upper and lower CIs
    ci <- NA
    ci[level == 95] <- qnorm(0.975)
    ci[level == 90] <- qnorm(0.95)
    a = ci * conse
    upper = conb + a
    lower = conb - a

    ## Return values
    return(data.frame(Znew, conb, upper, lower))
}


################################################################################
##### Load and clean the study 1 data
################################################################################

## Some initial pre-processing of the data was done to remove all identifying
## variables and rename some other variables - the code used to do this can be
## found in this section (commented out so that it doesn't run as this has already
## been done to the included data file)

## Note that a few of the variable names/values used in the code below contain
## acronyms that would be identifying, these have been marked out with
## asterisks

## ## Clear workspace
## rm(list = ls())

## ## Packages
## library(foreign)
## library(data.table)

## ## Load data
## my_file <- "original_data.dta"
## stud01 <- data.table(read.dta(my_file))

## ## Remove variables containing personal information
## remove <- c(
##     ## Deleted here are the response id, response set, name, email, ip address,
##     ## status, start date, and end date
##     "V1", "V2", "V3", "V5", "V6", "V7", "V8", "V9",

##     ## Next we delete a bunch of variables that match respondents to specific
##     ## addresses, precincts, or other identifying information (many of these are
##     ## variables related to geocoding the respondents, information from the ***
##     ## database, or information on elected officials), as well as some
##     ## variables that we do not use in our analysis
##     "county", "precinct", "cd_new", "ld_new", "sd_new", "cdold", "ld_old",
##     "sd_old", "phone", "address", "city", "zip", "address_id", "std_priadr",
##     "std_fullstreetaddr", "std_city", "std_state", "std_postalcode", "std_zip",
##     "stdzip4", "std_crt", "std_dpbc", "std_lot", "std_lotord", "std_achkdi",
##     "std_errstt", "std_rectyp", "std_dpvftn", "std_dpvstt", "std_county",
##     "matchtype", "longitude", "latitude", "geocodequalitytype", "matchscore",
##     "attended", "county_alt", "state_alte", "state_dele", "precinct_c",
##     "secretary", "election_j", "county_del", "vice_chair", "treasurer",
##     "caucus", "dup", "rand", "sr_id", "std_secadr", "***accesscode", "***",
##     "website", "called", "termend", "electoaccessc", "title", "orgphone",
##     "population", "class", "electedofficial", "lacking", "former", "comments",
##     "religion", "rel_activity", "***_*******", "other_lang", "*******_lang",
##     "*******_area", "DO_Q_Q5", "DO_Q_Q18", "DO_Q_Q16", "DO_Q_Q12",
##     "DO_Q_Q20", "DO_Q_Q22", "DO_Q_Q14", "DO_BR_FL_20", "respondent_comments"
## )
## stud01[, (remove) := NULL]

## ## Rename variables (uses a .csv with the old and new names saved in it)
## var_names <- "var_names.csv"
## var_names <- fread(var_names)
## setnames(stud01, old = var_names$old_names, new = var_names$new_names)

## ## Remove a respondent who was not part of the three main samples
## stud01 <- stud01[sample != "****", ]
## ## Was 5,812 respondents, now 5,811

## ## Additionally, the names of some variables/values were altered
## setnames(stud01,
##          c("****_hb497", "****_hb116", "****_hb469", "****_hb466", "****_news"),
##          c("st8_hb497", "st8_hb116", "st8_hb469", "st8_hb466", "st8_news"))
## stud01[, sample := plyr::mapvalues(sample, "***", "Voter")]

## ## Save data
## setwd("Data")
## save(stud01, file = "stud01_deID.RData")
## write.csv(stud01, file = "stud01_deID.csv", row.names = FALSE)
## setwd("..")


######################
##Load and clean Study 1 data ##
######################

## Change this to your working directory
#setwd("~/Dropbox/GKMR/PapersPresentations/2020Submission/Replication")
setwd("/Users/jrg27/Dropbox/Karpowitz_Monson_Project/CognitiveDissonance/PapersPresentations/2020Submission/Replication")
##Load data
load("Data/stud01_deID.RData")

## Add sample dummy variables
old <- c("Caucus", "Delegate", "Voter", "Elected Official")
new <- c("caucus_dummy", "delegate_dummy", "voter_dummy", "elect_dummy")
stud01[, (new) := lapply(old, function(x) {as.numeric(stud01$sample == x)})]
stud01[, activist_dummy := as.numeric(caucus_dummy == 1
                          | delegate_dummy == 1)]
rm(old, new)

## Fix treatment variables
vars <- c("treatment1", "treatment2", "treatment3", "treatment4")
stud01[, (vars) := lapply(1:4,
                  function(x) {as.numeric(stud01$treatment == x)})]
rm(vars)

## Creating new numeric variables for some of the variables
stud01[, partyidnum := as.numeric(partyid)]
stud01[partyid == "Other" | partyid == "Don't know", partyidnum := NA] # Those
                                        # who said "Other" or "Don't know" are
                                        # marked as NAs
stud01[, ideologynum := as.numeric(ideology)]
stud01[ideology == "Don't know", ideologynum := 3] # Those who said "Don't know"
                                        # are marked as "Neither, middle of the
                                        # road"
stud01[, year_born := as.numeric(as.character(year_born))]
stud01[, age := 2012 - year_born]
stud01[, gendernum := ifelse(gender == "Male", 1, 0)]
stud01[, incomenum := as.numeric(income)]
stud01[, educationnum := as.numeric(education)]

## Changing some existing variables to numeric
var <- c("m_less", "m_learn", "m_suffer", "gji_violence", "gji_lazy",
         "gji_work", "gji_honest", "gji_victim", "gji_opport", "gji_right",
         "law_english", "law_tuition", "law_welfare", "law_hire",
         "immig_opinion", "arizona_law", "st8_hb497", "st8_hb116",
         "st8_hb469", "st8_hb466")
stud01[, (var) := lapply(stud01[, var, with = FALSE], as.numeric)]
rm(var)

## Reverse-coding some variables
stud01[, m_learn := abs(m_learn - 8)]
stud01[, immig_opinion_reverse := abs(immig_opinion - 5)]
stud01[, st8_hb116_reverse := abs(st8_hb116 - 6)]
stud01[, st8_hb469_reverse := abs(st8_hb469 - 6)]
stud01[, st8_hb466_reverse := abs(st8_hb466 - 6)]
stud01[, ideologynum := abs(ideologynum - 6)]

## Standardizing variables
var <- c("partyidnum", "ideologynum", "i_admire", "i_love",
         "i_resent", "i_shame", "i_excite", "i_plea", "i_fear", "i_anger",
         "m_less", "m_learn", "m_suffer", "d_uncom", "d_uneasy", "d_bother",
         "d_tense", "d_concern", "gji_violence", "gji_lazy", "gji_work",
         "gji_honest", "gji_victim", "gji_opport", "gji_right", "e_sym",
         "e_moved", "e_com", "e_warm", "e_soft", "e_tender", "law_english",
         "law_tuition", "law_welfare", "law_hire", "immig_opinion",
         "arizona_law", "st8_hb497", "st8_hb116", "st8_hb469", "st8_hb466",
         "immig_opinion_reverse", "st8_hb116_reverse", "st8_hb469_reverse",
         "st8_hb466_reverse")
## Remove zeros that aren't supposed to be there
stud01[, (var) := lapply(stud01[, var, with = FALSE], function(x) {
    x[x == 0] <- NA
    return(x)
})]
## Apply standardizing function
stud01[, (var) := lapply(stud01[, var, with = FALSE], zero_to_one)]
rm(var)

## Index variables
stud01[, possec := rowMeans(.SD, na.rm = TRUE), .SDcols = c("i_admire", "i_love")]
stud01[, negsec := rowMeans(.SD, na.rm = TRUE), .SDcols = c("i_resent", "i_shame")]
stud01[, pospri := rowMeans(.SD, na.rm = TRUE), .SDcols = c("i_excite", "i_plea")]
stud01[, negpri := rowMeans(.SD, na.rm = TRUE), .SDcols = c("i_fear", "i_anger")]
stud01[, icb_pre := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("m_less", "m_learn", "m_suffer")]
stud01[, diss := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("d_uncom", "d_uneasy", "d_bother", "d_tense", "d_concern")]
stud01[, icb_post := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("gji_violence", "gji_lazy", "gji_work", "gji_honest",
                   "gji_victim")]
stud01[, emp := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("e_sym", "e_moved", "e_com", "e_warm", "e_soft", "e_tender")]
stud01[, law := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("law_english", "law_tuition", "law_welfare", "law_hire")]
stud01[, bills := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("st8_hb116", "st8_hb469", "st8_hb466")]
stud01[, harm := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("law_english", "law_tuition", "law_welfare", "law_hire",
                   "immig_opinion_reverse", "arizona_law", "st8_hb497")]
stud01[, help := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("st8_hb116", "st8_hb469", "st8_hb466")]

## Add two dichotomous variables
stud01[emp <= 0.5, emp_d := 0]
stud01[emp > 0.5, emp_d := 1]
stud01[icb_pre <= 0.5, icb_pre_d := 0]
stud01[icb_pre > 0.5, icb_pre_d := 1]

########################################
#### Clean out certain respondents (record n-size at each step)
########################################

## Save copy of original data and size
dat_1 <- copy(stud01)
n_1 <- nrow(dat_1)

## Remove duplicates
dat_2 <- stud01 <- stud01[!duplicated(identifier), ]
n_2 <- nrow(dat_2)

## Remove non-whites
values <- names(table(stud01$ethnicity))
drops <- values[!(values == "White / Caucasian")]
dat_3 <- stud01 <- stud01[!(ethnicity %in% drops), ]
n_3 <- nrow(dat_3)
rm(values, drops)

## Remove those who responded "No" or didn't respond to the manipulation
## question
dat_4 <- stud01 <- stud01[!(is.na(stud01$vidscreen)), ]
dat_4 <- stud01 <- stud01[stud01$vidscreen == "Yes", ]
n_4 <- nrow(dat_4)

## Remove those who didn't finish the survey
dat_5 <- stud01 <- stud01[finished == 1, ]
n_5 <- nrow(dat_5)

## Summary of those dropped
dropped <- data.table(
    " " = c("Full dataset", "Duplicates", "Non-whites",
    "Didn't pass manipulation check",
    "Didn't finish"),
    "Removed" = c(0, n_1 - n_2, n_2 - n_3, n_3 - n_4, n_4 - n_5),
    "Subjects Remaining" = c(n_1, n_2, n_3, n_4, n_5)
)

## Other summaries
stud01[, .N, by = sample]
stud01[, .N, by = treatment]
with(stud01, table(sample, treatment))

## Calculate Cronbach's alpha values
Exp1CB <- list(
    all = cbfunc(stud01),
    caucus = cbfunc(stud01[caucus_dummy == 1, ]),
    delegate = cbfunc(stud01[delegate_dummy == 1, ]),
    voter = cbfunc(stud01[voter_dummy == 1, ]),
    elect = cbfunc(stud01[elect_dummy == 1, ])
    )

## Treatment names
stud01$treat.names <- NA
stud01$treat.names[stud01$treatment==1] <- "Humanization"
stud01$treat.names[stud01$treatment==2] <- "Information"
stud01$treat.names[stud01$treatment==3] <- "Combined"
stud01$treat.names[stud01$treatment==4] <- "Control"

###Code for samples:

## Voters numbers
invited <- 5513
completed <- nrow(dat_2[voter_dummy == 1 & finished == 1, ])
responserate <- format_dec(completed / invited * 100)
invited <- format_int(invited)
completed <- format_int(completed)
analysis <- format_int(nrow(stud01[voter_dummy == 1, ]))

## Activist numbers
invited <- 2517 + 25711
completed <- nrow(dat_2[activist_dummy == 1 & finished == 1, ])
responserate <- format_dec(completed / invited * 100)
invited <- format_int(invited)
completed <- format_int(completed)
analysis <- format_int(nrow(stud01[activist_dummy == 1, ]))

## Elected officials numbers
invited <- 1714
completed <- nrow(dat_2[elect_dummy == 1 & finished == 1, ])
responserate <- format_dec(completed / invited * 100)
invited <- format_int(invited)
completed <- format_int(completed)
analysis <- format_int(nrow(stud01[elect_dummy == 1, ]))

## Total number
total <- format_int(nrow(stud01))



################################################################################
##### To load and clean Study 2 data
################################################################################

## As in Study 1, we begin with code (commented out) to remove identifying or unused
## variables from our dataset. This has already been done prior to this replication file.

## ## Clear workspace
## rm(list = ls())

## ## Packages
## library(foreign)
## library(data.table)

## ## Read in the data
## stud02 <- read_dta("Data/stud02_deID.dta")
## stud02 <- as.data.table(stud02)

## ## Remove variables
## remove <- c(
##     "ResponseID", "ResponseSet", "IPAddress", "StartDate", "EndDate",
##     "RecipientLastName", "RecipientFirstName", "RecipientEmail",
##     "ExternalDataReference", "Status", "Voter_ID", "order", "quartile",
##     "random", "Q1", "Q2", "Q3", "Q4", "Q5_1", "Q6_1", "Q7_1", "Q8_1", "Q9_1",
##     "Q10_1", "Q11_1", "Q12_1", "Q13_1", "Q14_1", "Q15_1", "Q16_1", "Q17_1",
##     "Q18_1", "Q19_1", "Q20_1", "Q21_1", "Q22_1", "Q23_1", "Q24_1", "Q26",
##     "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q37", "Q39",
##     "Q45", "RO_BL_Hispanic_Pictures", "RO_BL_Positive_Hispanic",
##     "RO_BL_Dissonance", "RO_BL_Policy_Attitudes", "DO_Q_Q5", "DO_Q_Q6",
##     "DO_Q_Q7", "DO_Q_Q8", "DO_Q_Q9", "DO_Q_Q10", "DO_Q_Q11", "DO_Q_Q12",
##     "DO_Q_Q13", "DO_Q_Q14", "DO_Q_Q15", "DO_Q_Q16", "DO_Q_Q17", "DO_Q_Q25",
##     "DO_Q_Q30", "DO_Q_Q31", "DO_Q_Q32", "DO_Q_Q33", "DO_Q_Q35", "DO_Q_Q36",
##     "DO_Q_Q38", "DO_Q_Q40", "DO_Q_Q41", "DO_Q_Q43", "DO_Q_Q44",
##     "LocationLatitude", "LocationLongitude", "LocationAccuracy", "V3", "V4",
##     "Email", "V6", "V7", "V8", "V9", "V10", "Middle_Name", "PartyReg",
##     "County_ID", "House_Number", "Direction_Prefix", "Street",
##     "Direction_Suffix", "City", "Zip", "Street_Type", "Phone", "Unit_Type",
##     "Unit_Number", "Name_Suffix", "House_Number_Suffix", "w1_1", "w1_2",
##     "w1_3", "w1_5", "w1_7", "w1_8_1", "w1_8_2", "w1_8_3", "w1_8_4", "w1_9",
##     "w1_10_1", "w1_10_2", "w1_10_3", "w1_10_4", "w1_11", "w1_12_1", "w1_12_2",
##     "w1_12_3", "w1_12_4", "w1_13", "w1_14_1", "w1_14_2", "w1_14_3", "w1_14_4",
##     "w1_15", "w1_16_1", "w1_16_2", "w1_16_3", "w1_16_4", "w1_17", "w1_18_1",
##     "w1_18_2", "w1_18_3", "w1_18_4", "w1_19", "w1_20_1", "w1_20_2", "w1_20_3",
##     "w1_20_4", "w1_21", "w1_22_1", "w1_22_2", "w1_22_3", "w1_22_4", "w1_23",
##     "w1_24_1", "w1_24_2", "w1_24_3", "w1_24_4", "w1_25", "w1_26_1", "w1_26_2",
##     "w1_26_3", "w1_26_4", "w1_27", "w1_28_1", "w1_28_2", "w1_28_3", "w1_28_4",
##     "w1_29", "w1_30_1", "w1_30_2", "w1_30_3", "w1_30_4", "w1_31", "w1_32_1",
##     "w1_32_2", "w1_32_3", "w1_32_4", "w1_33", "w1_34_1", "w1_34_2", "w1_34_3",
##     "w1_34_4", "w1_35", "w1_36_1", "w1_36_2", "w1_36_3", "w1_36_4", "w1_37",
##     "w1_38_1", "w1_38_2", "w1_38_3", "w1_38_4", "w1_39", "w1_40_1", "w1_40_2",
##     "w1_40_3", "w1_40_4", "w1_41", "w1_42_1", "w1_42_2", "w1_42_3", "w1_42_4",
##     "w1_43", "w1_44_1", "w1_44_2", "w1_44_3", "w1_44_4", "w1_45", "w1_46",
##     "w1_49", "w1_50_1", "w1_50_2", "w1_50_3", "w1_50_4", "w1_50_5", "w1_51_1",
##     "w1_51_2", "w1_51_3", "w1_51_4", "w1_52_1", "w1_52_2", "w1_52_3",
##     "w1_52_4", "w1_53_1", "w1_53_2", "w1_53_3", "w1_53_4", "w1_54_1",
##     "w1_54_2", "w1_54_3", "w1_54_4", "w1_55", "w1_59", "w1_60", "w1_61",
##     "w1_62", "LastName", "FirstName", "_merge"
## )
## stud02[, (remove) := NULL]

## ## Save data
## setwd("Data")
## save(stud02, file = "stud02_deID.RData")
## write.csv(stud02, file = "stud02_deID.csv", row.names = FALSE)
## setwd("..")

############
## Load Data
############

load("Data/stud02_deID.RData")

## Treatment conditions
myvars <- c("RO_BR_FL_238", "RO_BR_FL_268", "RO_BR_FL_265", "RO_BR_FL_262")
stud02[apply(stud02[, ..myvars], 1, function(x) any(x == "Positive Legal", na.rm = TRUE)),
       condition := 0]
stud02[apply(stud02[, ..myvars], 1, function(x) any(x == "Positive Illegal", na.rm = TRUE)),
       condition := 1]

## Humanization Measures and Index
myvars <- c("post_hum1", "post_hum2")
setnames(stud02, paste0("Q25_", 1:8), paste0("post_hum", 1:8))
setnames(stud02, grep("^hum", names(stud02), value = TRUE),
         paste0("pre_", grep("^hum", names(stud02), value = TRUE)))
tmpcb <- psych::alpha(stud02[, ..myvars])
stud02[, post_hum_measure := tmpcb$scores]
stud02[, post_hum_measure := (post_hum_measure - 1) / (7 - 1)]
## Alpha
tmpcb$total$raw_alpha

## Dissonance Measures and Index
setnames(stud02, c("Q35_1", "Q35_4", "Q35_5", "Q35_7", "Q35_8", "Q36_3",
                   "Q36_4", "Q36_5", "Q36_8", "Q36_9"),
         paste0("diss", 1:10))
myvars <- c("diss1", "diss2", "diss5", "diss6", "diss7")
tmpcb <- psych::alpha(stud02[, ..myvars])
stud02[, diss_measure := tmpcb$scores]
stud02[, diss_measure := (diss_measure - 1) / (7 - 1)]
my_med <- median(stud02$diss_measure, na.rm = TRUE)
stud02[, diss_hi := as.integer(!(diss_measure <= my_med))]
stud02[is.na(diss_measure), diss_hi := NA]
my_med <- median(stud01$diss[stud01$treatment == 4], na.rm = TRUE)
stud02[, diss_hi_alt := as.integer(!(diss_measure <= my_med))]
stud02[is.na(diss_measure), diss_hi_alt := NA]
## Alpha
tmpcb$total$raw_alpha

## Empathy Measures and Index
setnames(stud02, paste0("Q38_", 1:6), paste0("emp", 1:6))
tmpcb <- psych::alpha(stud02[, paste0("emp", 1:6), with = FALSE])
stud02[, emp_index := tmpcb$scores]
stud02[, emp_index01 := (emp_index - 1) / (7 - 1)]
## Alpha
tmpcb$total$raw_alpha

## Policy Measures and Index
oldvars <- c("Q40_1", "Q40_2", "Q41_14", "Q41_21", "Q41_22", "Q41_16", "Q41_20",
             "Q42", "Q43_6", "Q43_7", "Q44_1", "Q44_2", "Q44_3", "Q44_4")
newvars <- c("pol1a", "pol1b", "pol2a", "pol2b", "pol2c", "pol2d", "pol2e",
             "pol3", "pol4a", "pol4b", "pol5a", "pol5b", "pol5c", "pol5d")
setnames(stud02, oldvars, newvars)
stud02[, pol1b_rev := abs(pol1b - 6)]
stud02[, pol3_rev := abs(pol3 - 5)]
myvars <- c("pol1b_rev", "pol3_rev", "pol4a", "pol4b", "pol5a", "pol5b", "pol5c", "pol5d")
stud02[, pol3_gohome := 0]
stud02[pol3 == 1 | pol3 == 2, pol3_gohome := 1]
stud02[is.na(pol3), pol3_gohome := NA]
tmpcb <- psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)
stud02[, policy_harm := tmpcb$scores]
stud02[, policy_harm := (policy_harm - 1) / (6.4 - 1)]
## Alpha
tmpcb$total$raw_alpha

## Antipathy Measures and Index
stud02[, icb8_rev := abs(icb8 - 8)]
myvars <- paste0("icb", c(1:6, 8:10))
myvars[7] <- "icb8_rev"
tmpcb <- psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)
stud02[, icb_measure := tmpcb$scores]
## Alpha
tmpcb$total$raw_alpha

## Fix the hi_icb measure
stud02[, hi_icb := as.integer(!(icb_measure < 4))]
stud02[is.na(icb_measure), hi_icb := NA]

## Standardize some measures
stud02[, icb_measure := zero_to_one(icb_measure)]
stud02[, partyid := zero_to_one(partyid)]

## Change gender measure to dichotomous
stud02[, gender := gender - 1]

## Remove non-whites
stud02 <- stud02[!(stud02$ethnicity %in% c(1:4, 6:8)), ]

## Remove people who never received a treatment assignment in wave 2
stud02 <- stud02[!is.na(condition)]

################################################################################
##### To load and clean Pilot Data (presented in the appendix)
################################################################################

## ## As the previous studies, this code, now commented out, keeps only the necessary variables from the
## ## pretest dataset

## ## Clear workspace
## rm(list = ls())

## ## Packages
## library(haven)

## ## Load data
## predf <- read_dta("../R&RFiles/All_Variables_2011_***_Student_Cognitive_Dissonance.dta")
## predf <- subset(predf, vidscreen==1)
## predf <- as.data.table(predf)

## ## Remove variables containing personal information
## remove <- c(
##     "responseid", "responseset", "name", "identifier", "email", "ipaddress",
##     "status", "startdate", "enddate", "finished", "treatment", "Home_State",
##     "Home_Country", "Utah_or_not_", "Q70", "welcome", "govapprove",
##     "legapprove", "t_illegal", "t_poly", "t_black", "t_mormons", "t_catholics",
##     "t_gays", "treatment1", "treatment2", "treatment3", "treatment4",
##     "treatment5", "treatment6", "treatment7", "treatment8", "treatment0",
##     "vid_firstclick", "vid_lastclick", "vid_pagesubmit", "vid_clickcount",
##     "vidscreen", "e_sym", "e_moved", "e_com", "e_warm", "e_soft", "e_tender",
##     "i_admire", "i_love", "i_resent", "i_shame", "i_excite", "i_plea",
##     "i_fear", "i_anger", "i_admire_0", "i_love_0", "i_resent_0", "i_shame_0",
##     "i_excite_0", "i_plea_0", "i_fear_0", "i_anger_0", "infra_firstclick",
##     "infra_lastclick", "infra_pagesubmit", "infra_clickcount", "d_uncom",
##     "d_angry", "d_shame", "d_uneasy", "d_friend", "d_disgust", "d_emba",
##     "d_bother", "d_firstclick", "d_lastclick", "d_pagesubmit", "d_clickcount",
##     "d_opti", "d_annoy", "d_tense", "d_disa", "d_happy", "d_ener", "d_concern",
##     "d_good", "d2_firstclick", "d2_lastclick", "d2_pagesubmit",
##     "d2_clickcount", "gji_right", "gji_firstclick", "gji_lastclick",
##     "gji_pagesubmit", "gji_clickcount", "law_english", "law_tuition",
##     "law_welfare", "law_hire", "law_firstclick", "law_lastclick",
##     "law_pagesubmit", "law_clickcount", "immig_opinion", "arizona_law",
##     "utah_hb497", "utah_hb116", "utah_hb469", "utah_hb466", "utah_news",
##     "mani_check", "Q38", "internet", "gender", "year_born", "partyid",
##     "ideology", "grad_year", "religion", "rel_activity", "lds_mission",
##     "other_lang", "mission_lang", "mission_area", "employ", "ethnicity",
##     "marital", "income", "rate_survey", "Q57"
## )
## predf[, (remove) := NULL]

## ## Save data
## setwd("Data")
## save(predf, file = "pretest.RData")
## write.csv(predf, file = "pretest.csv", row.names = FALSE)
## setwd("..")

############
## Load data
############
load("Data/pretest.RData")

# Outgroup Antipathy Measure:
# Changing to numeric
predf$m_less <- as.numeric(predf$m_less)
predf$m_learn <- as.numeric(predf$m_learn)
# Reverse coding one of the variables
predf$m_learn <- abs(predf$m_learn - 8)
predf$m_suffer <- as.numeric(predf$m_suffer)
predf$gji_violence <- as.numeric(predf$gji_violence)
predf$gji_lazy <- as.numeric(predf$gji_lazy)
predf$gji_work <- as.numeric(predf$gji_work)
predf$gji_honest <- as.numeric(predf$gji_honest)
predf$gji_victim <- as.numeric(predf$gji_victim)
predf$gji_opport <- as.numeric(predf$gji_opport)
# Generating index:
preantipathy.df <- data.frame(predf$m_less,predf$m_learn,predf$m_suffer,predf$gji_violence,predf$gji_lazy,predf$gji_work,predf$gji_honest,predf$gji_victim,predf$gji_opport)
## cronbach(preantipathy.df)
predf$antipathy <- (predf$m_less + predf$m_learn + predf$m_suffer + predf$gji_violence + predf$gji_lazy + predf$gji_work + predf$gji_honest + predf$gji_victim + predf$gji_opport)/9

# AUTHORITARIANISM
auth.df <- data.frame(as.numeric(predf$v_indep), as.numeric(predf$v_obed), as.numeric(predf$v_curi), as.numeric(predf$v_well))
predf$auth <- (as.numeric(predf$v_indep) + (3-as.numeric(predf$v_obed)) + as.numeric(predf$v_curi) + (3-as.numeric(predf$v_well)))/4

# SOCIAL DOMINANCE ORIENTATION
# Changing to numeric
predf$sdo_eQualize <- as.numeric(predf$sdo_eQualize)
predf$sdo_inferior <- as.numeric(predf$sdo_inferior)
predf$sdo_desirable <- as.numeric(predf$sdo_desirable)
predf$sdo_chance <- as.numeric(predf$sdo_chance)
predf$sdo_problems <- as.numeric(predf$sdo_problems)
predf$sdo_step <- as.numeric(predf$sdo_step)
predf$sdo_ideal <- as.numeric(predf$sdo_ideal)
predf$sdo_stay <- as.numeric(predf$sdo_stay)
# Reverse coding
predf$sdo_eQualize <- abs(predf$sdo_eQualize - 8)
predf$sdo_desirable <- abs(predf$sdo_desirable - 8)
predf$sdo_problems <- abs(predf$sdo_problems - 8)
predf$sdo_ideal <- abs(predf$sdo_ideal - 8)
# Generating the index variable
pre.sdo.df <- subset(predf, select=c("sdo_eQualize","sdo_inferior","sdo_desirable","sdo_chance","sdo_problems","sdo_step","sdo_ideal","sdo_stay"))
predf$sdo <- (predf$sdo_eQualize + predf$sdo_inferior + predf$sdo_desirable + predf$sdo_chance + predf$sdo_problems + predf$sdo_step + predf$sdo_ideal + predf$sdo_stay)/ncol(pre.sdo.df)

# FEELING THERM AND TRADITIONAL ETHNO MEASURE (coded now on a 7-point scale)
predf$whitetherm <- as.numeric(predf$t_white)*.07
predf$hisptherm <- as.numeric(predf$t_hispanics)*.07
predf$ethno <- (4 + ((predf$whitetherm)*(3/7) - (predf$hisptherm)*(3/7)))



################################################################################
##### Results from the Body of the Paper
################################################################################

########################################
#### Section: Research Design
########################################

## Alpha for study 1 antipathy
Exp1CB$all["icb_pre"]

## Treatment condition numbers for study 1
table(stud01$treatment)
## 1 = Humanization
## 2 = Information
## 3 = Combined
## 4 = Control

## Alpha for infrahumanization (positive secondary emotions) from first study
Exp1CB$all["possec"]

## Alpha for empathy from first study
Exp1CB$all["emp"]

## Alpha for the policy harm index
Exp1CB$all["harm"]

## Alpha for antipathy from second study
myvars <- paste0("icb", c(1:6, 8:10))
myvars[7] <- "icb8_rev"
psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)$total$raw_alpha

## Study 2 above/below midpoint
table(stud02$hi_icb)

## Alpha for dissonance from second study
myvars <- c("diss1", "diss2", "diss5", "diss6", "diss7")
psych::alpha(stud02[, ..myvars])$total$raw_alpha

## Alpha for empathy from second study
psych::alpha(stud02[, paste0("emp", 1:6), with = FALSE])$total$raw_alpha

## Alpha for policies from second study
myvars <- c("pol1b_rev", "pol3_rev", "pol4a", "pol4b", "pol5a", "pol5b", "pol5c", "pol5d")
psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)$total$raw_alpha



########################################
#### Section: Changing Hearts, Study 1
########################################

## Main study 1 regression model for humanization ~ treatments * antipathy
r_s1_hum_antdint <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                          treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                          treatment3 * icb_pre_d,
                      data = stud01)

## To produce the figure, use the predict function to calculate subgroup means
## and confidence intervals
p_data <- data.frame(
    treatment = c(1, 2, 3, 4, 1, 2, 3, 4),
    treatment1 = c(1, 0, 0, 0, 1, 0, 0, 0),
    treatment2 = c(0, 1, 0, 0, 0, 1, 0, 0),
    treatment3 = c(0, 0, 1, 0, 0, 0, 1, 0),
    icb_pre_d = c(0, 0, 0, 0, 1, 1, 1, 1)
)
p_data <- cbind(p_data,
                predict(r_s1_hum_antdint, newdata = p_data, interval = "confidence"))
p_data <- as.data.table(p_data)
labs01 <- c("Low", "High")
labs02 <- c("Control", "Information", "Humanization",
            "Combined")
p_data[, icb_pre_d := plyr::mapvalues(icb_pre_d, 0:1, labs01)]
p_data[, icb_pre_d := factor(icb_pre_d, labs01, labs01)]
p_data[, treatment := plyr::mapvalues(treatment, c(4, 2, 1, 3), labs02)]
p_data[, treatment := factor(treatment, labs02, labs02)]

## Plot of study 1 humanization by level of antipathy and treatment group
p <- ggplot(p_data, aes(y = fit, x = treatment, ymin = lwr, ymax = upr,
                        shape = icb_pre_d,
                   group = icb_pre_d)) +
    geom_errorbar(colour = "black", width = 0.1) +
    geom_line() +
    geom_point(size = 3, fill = "white") +
    scale_shape_manual("Outgroup Antipathy", values = 21:22) +
    labs(x = "Treatment", y = "Humanization Level",
         linetype = "Outgroup Antipathy")
ggsave("FiguresTables/hum01_antipathy.png", p, width = 6.25, height = 3)

## Numbers quoted in the paragraph from study 1
p_data[icb_pre_d == "High" & treatment == "Control"]
p_data[icb_pre_d == "High" & treatment == "Humanization"]
p_data[icb_pre_d == "Low" & treatment == "Control"]
p_data[icb_pre_d == "Low" & treatment == "Humanization"]

## Statistical test of difference between conditions
## Comparing high antipathy in control to high antipathy in humanization treatment
hypo_01 <- "0*treatment1 + 1*icb_pre_d + 0*treatment1:icb_pre_d = 1*treatment1 + 1*icb_pre_d + 1*treatment1:icb_pre_d"
linearHypothesis(r_s1_hum_antdint, hypo_01)
## Above is equivalent to joint test of the coefficients on treatment1 and
## treatment1:icb_pre_d
## Comparing low antipathy in control to low antipathy in humanization treatment
hypo_02 <- "0*treatment1 + 0*icb_pre_d + 0*treatment1:icb_pre_d = 1*treatment1 + 0*icb_pre_d + 0*treatment1:icb_pre_d"
linearHypothesis(r_s1_hum_antdint, hypo_02)
## Above is equivalent to a test of the coefficient on treatment 1

## Standard deviation of the outcome
sd(stud01$possec, na.rm = TRUE)
0.18 / 0.27
0.09 / 0.27

## Comparison to combined treatment
p_data[icb_pre_d == "High" & treatment == "Combined"]
p_data[icb_pre_d == "Low" & treatment == "Combined"]

## Similar results with statistical test using the combined treatment
hypo_01 <- "0*treatment3 + 1*icb_pre_d + 0*treatment3:icb_pre_d = 1*treatment3 + 1*icb_pre_d + 1*treatment3:icb_pre_d"
linearHypothesis(r_s1_hum_antdint, hypo_01)
## Comparing low antipathy in control to low antipathy in humanization treatment
hypo_02 <- "0*treatment3 + 0*icb_pre_d + 0*treatment3:icb_pre_d = 1*treatment3 + 0*icb_pre_d + 0*treatment3:icb_pre_d"
linearHypothesis(r_s1_hum_antdint, hypo_02)

## Standard deviation of the outcome
0.15 / 0.27
0.10 / 0.27

## Preparing data for the study 1 marginal effects plots
# Regression models
reg_med_me1 <- lm(emp ~ treatment1 + icb_pre + treatment2 + treatment3
                  + treatment2 * icb_pre + treatment3 * icb_pre
                  + treatment1 * icb_pre,
                  stud01)
reg_med_me2 <- lm(emp ~ treatment2 + icb_pre + treatment1 + treatment3
                  + treatment1 * icb_pre + treatment3 * icb_pre
                  + treatment2 * icb_pre,
                  stud01)
reg_med_me3 <- lm(emp ~ treatment3 + icb_pre + treatment2 + treatment1
                  + treatment2 * icb_pre + treatment1 * icb_pre
                  + treatment3 * icb_pre,
                  stud01)
## ME numbers
dat1 <- TwowayME.f(reg_med_me1, stud01$treatment1, stud01$icb_pre, 95)
dat2 <- TwowayME.f(reg_med_me2, stud01$treatment2, stud01$icb_pre, 95)
dat3 <- TwowayME.f(reg_med_me3, stud01$treatment3, stud01$icb_pre, 95)
dat1 <- cbind(treatment = "Humanization", dat1)
dat2 <- cbind(treatment = "Information", dat2)
dat3 <- cbind(treatment = "Combined", dat3)
plot_data1 <- rbind(dat1, dat2, dat3)
## Rug plot numbers
dat1 <- cbind(treatment = "Humanization",
              icb_pre = as.numeric(reg_med_me1$model$icb_pre),
              conb = as.numeric(0))
dat2 <- cbind(treatment = "Information",
              icb_pre = as.numeric(reg_med_me2$model$icb_pre),
              conb = as.numeric(0))
dat3 <- cbind(treatment = "Combined",
              icb_pre = as.numeric(reg_med_me3$model$icb_pre),
              conb = as.numeric(0))
plot_data2 <- data.frame(rbind(dat1, dat2, dat3))
plot_data2$icb_pre <- as.numeric(as.character(plot_data2$icb_pre))
plot_data2$conb <- as.numeric(as.character(plot_data2$conb))

## Plot of study 1 marginal effects
set.seed(33333)                         #for rug plot
p <- ggplot(plot_data1, aes(x = Znew)) +
    geom_line(aes(y = conb)) +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_rug(data = plot_data2, aes(x = icb_pre, y = conb), sides = "b",
             position = position_jitter(width = 0.05, height = 0.001),
             alpha = 0.05) +
    xlab("Outgroup Antipathy") +
    ylab("Marginal Effects of Treatment\non Empathic Concern") +
    facet_wrap(~ treatment)
ggsave("FiguresTables/MEplots.png", p, width = 6, height = 3)

## Main study 1 regression model for empathy ~ treatments * antipathy
r_s1_emp_antdint <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                          treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                          treatment3 * icb_pre_d,
                      data = stud01)
## This corresponds to the difference in means tests below

## Preparing data for the empathy gap plot
labs01 <- c("Low", "High")
labs02 <- c("Control", "Information", "Humanization",
            "Combined")
p_data <- stud01[, list(emp = mean(emp, na.rm = TRUE),
                          se = sd(emp, na.rm = TRUE) /
                              sqrt(sum(!is.na(emp)))),
                   by = list(icb_pre_d, treatment)]
p_data <- p_data[!is.na(icb_pre_d)]
p_data[, icb_pre_d := plyr::mapvalues(icb_pre_d, 0:1, labs01)]
p_data[, icb_pre_d := factor(icb_pre_d, labs01, labs01)]
p_data[, treatment := plyr::mapvalues(treatment, c(4, 2, 1, 3), labs02)]
p_data[, treatment := factor(treatment, labs02, labs02)]
my_q <- qnorm(.975)
p_data_diff <- cbind(
  "Condition" = c("Control", "Information", "Humanization", "Combined"),
  "Diff" = c(p_data[1,3]-p_data[4,3],
             p_data[7,3]-p_data[8,3],
             p_data[2,3]-p_data[6,3],
             p_data[3,3]-p_data[5,3]),
  "seDiff" = c(sqrt(sd(stud01$emp[stud01$treatment==4 & stud01$icb_pre_d==0],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==4 & stud01$icb_pre_d==0])) + sd(stud01$emp[stud01$treatment==4 & stud01$icb_pre_d==1],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==4 & stud01$icb_pre_d==1]))),
               sqrt(sd(stud01$emp[stud01$treatment==2 & stud01$icb_pre_d==0],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==2 & stud01$icb_pre_d==0])) + sd(stud01$emp[stud01$treatment==2 & stud01$icb_pre_d==1],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==2 & stud01$icb_pre_d==1]))),
               sqrt(sd(stud01$emp[stud01$treatment==1 & stud01$icb_pre_d==0],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==1 & stud01$icb_pre_d==0])) + sd(stud01$emp[stud01$treatment==1 & stud01$icb_pre_d==1],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==1 & stud01$icb_pre_d==1]))),
               sqrt(sd(stud01$emp[stud01$treatment==3 & stud01$icb_pre_d==0],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==3 & stud01$icb_pre_d==0])) + sd(stud01$emp[stud01$treatment==3 & stud01$icb_pre_d==1],na.rm=T)^2/sum(!is.na(stud01$emp[stud01$treatment==3 & stud01$icb_pre_d==1])))
  )
)
p_data_diff.df <- as.data.frame(p_data_diff)
p_data_diff.df <- transform(p_data_diff.df, Condition = unlist(Condition))
p_data_diff.df$Condition <- as.factor(p_data_diff.df$Condition)
p_data_diff.df$Condition <- factor(p_data_diff.df$Condition, as.character(p_data_diff.df$Condition))
p_data_diff.df$Diff <- as.numeric(p_data_diff.df$Diff)
p_data_diff.df$seDiff <- as.numeric(p_data_diff.df$seDiff)
my_q <- qnorm(.975)

## Plotting the empathy gap for study 1
p <- ggplot(p_data_diff.df, aes(y = Diff, x = Condition, ymin = Diff - seDiff * my_q, ymax = Diff + seDiff * my_q)) +
  geom_errorbar(colour = "black", width = 0.1) +
  geom_line(aes(group=1)) +
  geom_point(size = 3, fill = "white") +
  labs(x = "Treatment", y = "Empathy Gap")
ggsave("FiguresTables/empdiff01.png", p, width = 5.25, height = 3)

## Standard deviation of empathy difference
sd(stud01$emp, na.rm = TRUE)
## 0.28
0.55 / 0.28
0.18 / 0.28

########################################
#### Section: Changing Hearts, Study 2
########################################

## Preparing data for pre-post photo humanization results
labs01 <- c("Low", "High")
labs02 <- c("Pre-Photo", "Post-Photo")
p_data <- stud02[, list(hum01 = mean(pre_hum_measure, na.rm = TRUE),
                        se01 = sd(pre_hum_measure, na.rm = TRUE) /
                            sqrt(sum(!is.na(pre_hum_measure))),
                        hum02 = mean(post_hum_measure, na.rm = TRUE),
                        se02 = sd(post_hum_measure, na.rm = TRUE) /
                            sqrt(sum(!is.na(post_hum_measure)))),
                 by = list(hi_icb)]
p_data <- p_data[!is.na(hi_icb)]
p_data <- melt(p_data, id.vars = "hi_icb")
p_data <- as.data.table(p_data)
p_data[, wave := gsub(".+0(\\d)", "\\1", variable)]
p_data[, variable := gsub("\\d+", "", variable)]
p_data <- as.data.table(dcast(p_data, hi_icb + wave ~ variable))
p_data[, hi_icb := plyr::mapvalues(hi_icb, 0:1, labs01)]
p_data[, hi_icb := factor(hi_icb, labs01, labs01)]
p_data[, wave := plyr::mapvalues(wave, 1:2, labs02)]
p_data[, wave := factor(wave, labs02, labs02)]
my_q <- qnorm(.975)

## Plotting the humanization pre/post photo
p <- ggplot(p_data, aes(y = hum, x = wave, ymin = hum - se * my_q,
                   ymax = hum + se * my_q, shape = hi_icb, group = hi_icb)) +
    geom_errorbar(colour = "black", width = 0.1) +
    geom_line() +
    geom_point(size = 3, fill = "white") +
    scale_shape_manual("Outgroup Antipathy", values = 21:22) +
    labs(x = "Wave", y = "Humanization Level", linetype = "Outgroup Antipathy")
tab_s02_hum <- p_data
ggsave("FiguresTables/hum02.png", p, width = 5.5, height = 3)

## Statistical test for p-value for responding to images
with(stud02[hi_icb == 0],
     t.test(pre_hum_measure, post_hum_measure, paired = TRUE))
with(stud02[hi_icb == 1],
     t.test(pre_hum_measure, post_hum_measure, paired = TRUE))

## Preparing data for figure showing empathy by condition and antipathy
labs01 <- c("Low", "High")
labs02 <- c("Legal Immigrants", "Illegal Immigrants")
p_data <- stud02[, list(emp = mean(emp_index01, na.rm = TRUE),
                        se = sd(emp_index01, na.rm = TRUE) /
                            sqrt(sum(!is.na(emp_index01)))),
                 by = list(hi_icb, condition)]
p_data <- p_data[!is.na(hi_icb)]
p_data <- p_data[!is.na(condition)]
p_data[, hi_icb := plyr::mapvalues(hi_icb, 0:1, labs01)]
p_data[, hi_icb := factor(hi_icb, labs01, labs01)]
p_data[, condition := plyr::mapvalues(condition, 0:1, labs02)]
p_data[, condition := factor(condition, labs02, labs02)]
my_q <- qnorm(.975)

## Plotting figure of empathy by condition and antipathy
p1 <- ggplot(p_data, aes(y = emp, x = condition, ymin = emp - se * my_q,
                   ymax = emp + se * my_q, shape = hi_icb, group = hi_icb)) +
    geom_errorbar(colour = "black", width = 0.1) +
    geom_line() +
    geom_point(size = 3, fill = "white") +
    scale_shape_manual("Outgroup\nAntipathy", values = 21:22) +
    labs(x = "", y = "Self-reported Empathic Concern",
         linetype = "Outgroup Antipathy")
tab_s02_emp <- p_data

## Preparing data for study 2 empathy gap figure
p_data_diff <- cbind(
  "Condition" = c("Legal Immigrants", "Illegal Immigrants"),
  "Diff" = c(p_data[hi_icb == "Low" & condition == "Legal Immigrants", 3] - p_data[hi_icb == "High" & condition == "Legal Immigrants", 3],
             p_data[hi_icb == "Low" & condition == "Illegal Immigrants", 3] - p_data[hi_icb == "High" & condition == "Illegal Immigrants", 3]
             ),
  "seDiff" = c(sqrt(sd(stud02$emp_index01[stud02$condition==0 & stud02$hi_icb==0],na.rm=T)^2/sum(!is.na(stud02$emp_index01[stud02$condition==0 & stud02$hi_icb==0])) + sd(stud02$emp_index01[stud02$condition==0 & stud02$hi_icb==1],na.rm=T)^2/sum(!is.na(stud02$emp_index01[stud02$condition==0 & stud02$hi_icb==1]))),
               sqrt(sd(stud02$emp_index01[stud02$condition==1 & stud02$hi_icb==0],na.rm=T)^2/sum(!is.na(stud02$emp_index01[stud02$condition==1 & stud02$hi_icb==0])) + sd(stud02$emp_index01[stud02$condition==1 & stud02$hi_icb==1],na.rm=T)^2/sum(!is.na(stud02$emp_index01[stud02$condition==1 & stud02$hi_icb==1])))
  )
)
p_data_diff.df <- as.data.frame(p_data_diff)
p_data_diff.df <- transform(p_data_diff.df, Condition = unlist(Condition))
p_data_diff.df$Condition <- as.factor(p_data_diff.df$Condition)
p_data_diff.df$Condition <- factor(p_data_diff.df$Condition, as.character(p_data_diff.df$Condition))
p_data_diff.df$Diff <- as.numeric(p_data_diff.df$Diff)
p_data_diff.df$seDiff <- as.numeric(p_data_diff.df$seDiff)
my_q <- qnorm(.975)

## Plotting empathy gap figure for study 2
p2 <- ggplot(p_data_diff.df, aes(y = Diff, x = Condition, ymin = Diff - seDiff * my_q, ymax = Diff + seDiff * my_q)) +
  geom_errorbar(colour = "black", width = 0.1) +
  geom_line(aes(group=1)) +
  geom_point(size = 3, fill = "white") +
  labs(x = "Treatment", y = "Empathy Gap"
  )
tab_s02_emp <- p_data_diff.df

## Creating a figure with both the empathy by condition/antipathy and empathy
## gap figures are on the same plot
pFinal <- plot_grid(p1, p2, align = "v", axis = "lr", ncol = 1)
ggsave("FiguresTables/s02_empplusdiff.png", pFinal, height = 6, width = 5.5)

## Statistical test for difference in differences test
r_s2_emp_antdint <- lm(emp_index01 ~ condition * hi_icb, stud02)
summary(r_s2_emp_antdint)

## Coefficient of interest is on condition:hi_icb, which is equal to the
## difference between the point estimates in the empathy gap figure

########################################
#### Section: Dissonance as a Mechanism
########################################

## Preparing data for the plot of study 2 dissonance by antipathy/condition
labs01 <- c("Low", "High")
labs02 <- c("Legal Immigrants", "Illegal Immigrants")

p_data <- stud02[, list(diss = mean(diss_measure, na.rm = TRUE),
                        se = sd(diss_measure, na.rm = TRUE) /
                            sqrt(sum(!is.na(diss_measure)))),
                 by = list(hi_icb, condition)]
p_data <- p_data[!is.na(hi_icb)]
p_data[, hi_icb := plyr::mapvalues(hi_icb, 0:1, labs01)]
p_data[, hi_icb := factor(hi_icb, labs01, labs01)]
p_data[, condition := plyr::mapvalues(condition, 0:1, labs02)]
p_data[, condition := factor(condition, labs02, labs02)]
my_q <- qnorm(.975)
p_data <- p_data[!is.na(condition)]

## Plot of study 2 dissonance by antipathy/condition
ggplot(p_data, aes(y = diss, x = condition, ymin = diss - se * my_q,
                   ymax = diss + se * my_q, shape = hi_icb, group = hi_icb)) +
    geom_errorbar(colour = "black", width = 0.1) +
    geom_line() +
    geom_point(size = 3, fill = "white") +
    scale_shape_manual("Outgroup Antipathy", values = 21:22) +
    labs(x = "Experimental Condition", y = "Self-reported Dissonance",
         linetype = "Outgroup Antipathy")
ggsave("FiguresTables/s02_diss.png", height = 3, width = 5.5)
tab_s02_diss <- p_data

## Regression model for statistical tests
r_s2_diss_antdint <- lm(diss_measure ~ condition * hi_icb, stud02)
summary(r_s2_diss_antdint)

## For first statistical test mentioned, of high antipathy vs low antipathy in
## the legal treatment, see the coefficient on "hi_icb"

## For the second statistical test mentioned, dissonance change between
## treatments for low antipathy, see the coefficient on "condition"

## For the third statistical test, we are testing the hypothesis that the
## increase in dissonance between treatments is larger for those with high
## antipathy, which corresponds to the coefficient on the interaction term,
## i.e. "condition:hi_icb"

########################################
#### Section: Changing Minds about Policy
########################################

## Study 1
r_s1_harm <- lm(harm ~ treatment1 + treatment2 + treatment3, stud01)
r_s1_harm_antcint <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre +
                        treatment1:icb_pre + treatment2:icb_pre +
                        treatment3:icb_pre,
                        stud01)
sink("FiguresTables/r_s1_harm_antcint.tex")
apsrtable(r_s1_harm, r_s1_harm_antcint,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Standard deviation of the outcome
sd(stud01$harm, na.rm = TRUE)
## 0.22
0.01 / 0.22
0.03 / 0.22

## Study 2
r_s2_harm <- lm(policy_harm ~ condition, stud02)
r_s2_harm_antcint <- lm(policy_harm ~ condition * icb_measure, stud02)
sink("FiguresTables/r_s2_harm_antcint.tex")
apsrtable(r_s2_harm, r_s2_harm_antcint,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Standard deviation of the outcome
sd(stud02$policy_harm, na.rm = TRUE)
## 0.23

################################################################################
##### Appendix
################################################################################

########################################
#### Survey Administration and Sampling Details
########################################

## Voters numbers
invited <- 5513
completed <- nrow(dat_2[voter_dummy == 1 & finished == 1, ])
completed
responserate <- format_dec(completed / invited * 100)
responserate
analysis <- format_int(nrow(stud01[voter_dummy == 1, ]))
analysis

## Activist numbers
invited <- 2517 + 25711
invited
completed <- nrow(dat_2[activist_dummy == 1 & finished == 1, ])
completed
responserate <- format_dec(completed / invited * 100)
responserate
analysis <- format_int(nrow(stud01[activist_dummy == 1, ]))
analysis

## Elected officials numbers
invited <- 1714
completed <- nrow(dat_2[elect_dummy == 1 & finished == 1, ])
completed
responserate <- format_dec(completed / invited * 100)
responserate
analysis <- format_int(nrow(stud01[elect_dummy == 1, ]))
analysis

## Total number
total <- format_int(nrow(stud01))

########################################
#### Survey Measures - Measure of Outgroup Antipathy
########################################

## Getting plot data ready for the figures
p_data_1 <- stud01[, "icb_pre", with = FALSE]
p_data_1[, dataset := "Study 1"]
p_data_2 <- as.data.table(stud02[, "icb_measure"])
p_data_2[, icb_measure := zero_to_one(as.numeric(icb_measure))]
p_data_2[, dataset := "Study 2"]
p_data <- rbindlist(list(p_data_1, p_data_2))

## Plot for outgroup antipathy for studies 1 and 2
p <- ggplot(p_data, aes(x = icb_pre)) +
    geom_density(fill = "light gray", adjust = 0.75) +
    facet_wrap(~ dataset) +
    labs(x = "Outgroup Antipathy", y = "Density")
ggsave("FiguresTables/density_icbs.png", p, width = 6.25, height = 3)

## N-sizes quoted in the figure for studies 1 and 2
table(is.na(p_data[dataset == "Study 1"]$icb_pre))
table(is.na(p_data[dataset == "Study 2"]$icb_pre))

########################################
#### Validation of Outgroup Antipathy Measure
########################################

## Uses the pilot data

## Histograms
hist(predf$antipathy)
hist(predf$hisptherm)
hist(predf$ethno)
hist(predf$sdo)
hist(predf$auth)

##To check for correlations:
cor(predf$antipathy,predf$hisptherm,use="complete.obs")
cor(predf$antipathy,predf$ethno,use="complete.obs")
cor(predf$antipathy,predf$sdo,use="complete.obs")
cor(predf$antipathy,predf$auth,use="complete.obs")

## Correlation Matrix
var_names <- c("antipathy", "hisptherm", "ethno", "sdo", "auth")
var_labs <- c("Outgroup Antipathy", "Latino Feeling Therm.", "Ethnocentrism",
              "SDO", "Authoritarianism")
predf <- as.data.frame(predf)
mymat <- cor(predf[, var_names], use = "complete.obs")
colnames(mymat) <- rownames(mymat) <- var_labs
stargazer(mymat, type = "latex", digits = 2, float = FALSE,
          out = "FiguresTables/corrmat_pretest.tex")

var_labs <- c("Outgroup\nAntipathy", "Latino Feeling\nThermometer",
              "Ethnocentrism", "SDO", "Authoritarianism")
myplot_hex <- function (data, mapping, ...) {
    p <- ggplot(data = data, mapping = mapping) + stat_binhex(..., bins = 15) +
        scale_fill_gradientn(colours = c("light gray", "black"))
    p
}
myplot_2dhist <- function (data, mapping, ...) {
    p <- ggplot(data = data, mapping = mapping) + geom_bin2d(...) +
        scale_fill_gradient(low = "light gray", high = "black")
    p
}
ggpairs(predf[, var_names], columnLabels = var_labs,
        lower = list(continuous = myplot_2dhist))
ggsave("FiguresTables/corrmat_pretest.png", width = 8.5, height = 5.5)

########################################
#### Factor Analysis
########################################

## Study 1 factor analysis
tmp <- na.omit(subset(stud01,
                      select = c("law_english", "law_tuition", "law_welfare",
                          "law_hire", "immig_opinion_reverse", "arizona_law",
                          "st8_hb497", "st8_hb116", "st8_hb466",
                          "st8_hb469")))
princomp_fit <- princomp(tmp, cor = 2)
explore_fit <- factanal(tmp, 2, rotation = "varimax")
explore_load <- explore_fit$loadings[, 1:2]
plot_fa <- data.table(
    "Variables" = c("Law (English)", "Law (Tuition)", "Law (Welfare)",
    "Law (Hire)", "Immigration Opinion", "Arizona Law", "State Bill Harm", "State Bill Help 1",
    "State Bill Help 2", "State Bill Help 3"),
    "Factor1" = as.data.table(explore_load)$Factor1,
    "Factor2" = as.data.table(explore_load)$Factor2
)
plot_pc <- data.table(
    "Labels" = ordered(names(princomp_fit$sdev),
    levels = names(princomp_fit$sdev)),
    "var_expl" = (princomp_fit$sdev)^2 / sum((princomp_fit$sdev)^2)
)

## Plot 1
my_size <- 2.9
p1 <- ggplot(plot_fa, aes(x = Factor1, y = Factor2, label = Variables,
                          family = "serif")) +
    geom_point() +
    geom_text(data = plot_fa[c(1, 9)], size = my_size, hjust = 1.1,
              vjust = -0.1) +
    geom_text(data = plot_fa[c(2, 5, 8)], size = my_size, hjust = -0.1,
              vjust = 1.1) +
    geom_text(data = plot_fa[c(4)], size = my_size, hjust = 1.1, vjust = 1.1) +
    geom_text(data = plot_fa[c(3, 6:7, 10)], size = my_size, hjust = -0.1,
              vjust = -0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_circle(aes(x0 = 0.7, y0 = -0.24, r = 0.28), size = 0.25,
                inherit.aes = FALSE) +
    xlab("Factor 1") +
    ylab("Factor 2") +
    xlim(-0.33, 0.98) +
    ylim(-0.55, 0.75)
plot(p1)

## Plot 2
p2 <- ggplot(plot_pc, aes(x = Labels, y = var_expl, group = 1)) +
    geom_line() +
    geom_point() +
    xlab("Components") +
    ylab("Variance Explained") +
    scale_x_discrete(labels = abbreviate)
plot(p2)

## Final Plot
pFinal <- plot_grid(p1, p2, align = "v", axis = "lr", ncol = 1)
ggsave("FiguresTables/s1_factoranalysis.png", width = 6.25, height = 8)

## Study 2 factor analysis
tmp <- na.omit(subset(stud02,
                      select = c("pol1a", "pol1b", "pol2a", "pol2b", "pol2c",
                                 "pol2d", "pol2e", "pol3", "pol4a", "pol4b",
                                 "pol5a", "pol5b", "pol5c", "pol5d")))
tmp[, pol1b := abs(pol1b - 6)]
tmp[, pol3 := abs(pol3 - 5)]
princomp_fit <- princomp(tmp, cor = 2)
explore_fit <- factanal(tmp, 2, rotation = "varimax")
explore_load <- explore_fit$loadings[, 1:2]
plot_fa <- data.table(
    "Variables" = c("Aid Legal", "Aid Illegal", "Don't Give Back",
                    "See Themselves American", "Not Bothered To Learn",
                    "Assimilate Well", "Should Try Harder", "Immigration Opinion",
                    "Taking Resources", "Should Deny Rights",
                    "Law (English)", "Law (Tuition)", "Law (Welfare)",
                    "Law (Hire)"),
    "Factor1" = as.data.table(explore_load)$Factor1,
    "Factor2" = as.data.table(explore_load)$Factor2
)
plot_pc <- data.table(
    "Labels" = ordered(names(princomp_fit$sdev),
    levels = names(princomp_fit$sdev)),
    "var_expl" = (princomp_fit$sdev)^2 / sum((princomp_fit$sdev)^2)
)

## Plot 1
my_size <- 2.9
p1 <- ggplot(plot_fa, aes(x = Factor1, y = Factor2, label = Variables,
                          family = "serif")) +
    geom_point() +
    geom_text(data = plot_fa[c(1, 5, 7, 10, 14)], size = my_size, hjust = 1.1,
              vjust = -0.1) +
    geom_text(data = plot_fa[c(6, 9, 13)], size = my_size, hjust = -0.1,
              vjust = 1.1) +
    geom_text(data = plot_fa[c(2, 8)], size = my_size, hjust = 1.1, vjust = 1.1) +
    geom_text(data = plot_fa[c(12, 3:4, 11)], size = my_size, hjust = -0.1,
              vjust = -0.1) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_circle(aes(x0 = 0.625, y0 = 0.3, r = 0.32), size = 0.25,
                inherit.aes = FALSE) +
    xlab("Factor 1") +
    ylab("Factor 2") +
    xlim(-0.25, 0.95) +
    ylim(-0.55, 0.75)
plot(p1)

## Plot 2
p2 <- ggplot(plot_pc, aes(x = Labels, y = var_expl, group = 1)) +
    geom_line() +
    geom_point() +
    xlab("Components") +
    ylab("Variance Explained") +
    scale_x_discrete(labels = abbreviate)
plot(p2)

## Final Plot
pFinal <- plot_grid(p1, p2, align = "v", axis = "lr", ncol = 1)
ggsave("FiguresTables/s2_factoranalysis.png", width = 6.25, height = 8)

########################################
#### Balance
########################################

## A few additional simplified variables
stud01[, student := as.integer(employ == "Student")]
stud01[is.na(employ), student := NA]
stud01[, married := as.integer(marital == "Married")]
stud01[is.na(marital), married := NA]

myvars1 <- c("treatment", "gendernum", "age", "student", "partyidnum",
             "incomenum", "educationnum", "married")

tmpdat <- stud01[treatment %in% c(1, 4), ..myvars1]
tmpdat[, treatment := as.logical(treatment == 1)]
baltest_1a <- xBalance(as.formula(paste("treatment ~ ", paste0(myvars1[-1], collapse = " + "))),
                       data = tmpdat,
                       report = c("chisquare.test", "std.diffs"))

tmpdat <- stud01[treatment %in% c(2, 4), ..myvars1]
tmpdat[, treatment := as.logical(treatment == 2)]
baltest_1b <- xBalance(as.formula(paste("treatment ~ ", paste0(myvars1[-1], collapse = " + "))),
                       data = tmpdat,
                       report = c("chisquare.test", "std.diffs"))

tmpdat <- stud01[treatment %in% c(3, 4), ..myvars1]
tmpdat[, treatment := as.logical(treatment == 3)]
baltest_1c <- xBalance(as.formula(paste("treatment ~ ", paste0(myvars1[-1], collapse = " + "))),
                       data = tmpdat,
                       report = c("chisquare.test", "std.diffs"))

myvars2 <- c("condition", "gender", "age", "w1_4", "partyid")

tmpdat <- stud02[, ..myvars2]
tmpdat[, condition := as.logical(condition == 1)]
tmpdat <- tmpdat[!is.na(condition)]
baltest_2a <- xBalance(as.formula(paste("condition ~ ", paste0(myvars2[-1], collapse = " + "))),
                       data = tmpdat,
                       report = c("chisquare.test", "std.diffs"))

myvars1_labs <- c("Gender", "Age", "Student", "Party ID", "Income", "Education",
                  "Married")
myvars1_labs <- c(myvars1_labs, paste(myvars1_labs, "(NA)"))

baldat_1 <- data.table(
    var = rep(myvars1_labs, 3),
    treat = rep(c("Humanization", "Information", "Combined"),
                each = length(myvars1_labs)),
    balstat = c(baltest_1a$results[1:14, 1, ],
                baltest_1b$results[1:14, 1, ],
                baltest_1c$results[1:14, 1, ])
)
baldat_1[, treat := factor(treat, c("Humanization", "Information", "Combined"))]
baldat_1[, var := factor(var, rev(myvars1_labs))]

## Overall statistics (reported in the table)
baltest_1a$overall
baltest_1b$overall
baltest_1c$overall

myvars2_labs <- c("Gender", "Age", "Student", "Party ID", "Gender (NA)",
                  "Age (NA)", "Student (NA)", "Party ID (NA)")

baldat_2 <- data.table(
    var = myvars2_labs,
    treat = rep("Illegal Condition", length(myvars2_labs)),
    balstat = baltest_2a$results[1:8, 1, ]
)
baldat_2[, var := factor(var, rev(myvars2_labs))]

## Overall statistics (reported in the table)
baltest_2a$overall

balplot_1 <- ggplot(baldat_1, aes(x = balstat, y = var, color = treat)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.5) +
    geom_point() +
    coord_cartesian(xlim = c(-0.125, 0.1)) +
    labs(y = "Covariate", x = "Standardized Difference", colour = "Treatment",
         title = "Study 1")

balplot_2 <- ggplot(baldat_2, aes(x = balstat, y = var)) +
    geom_vline(xintercept = 0, linetype = 2, size = 0.5) +
    geom_point() +
    coord_cartesian(xlim = c(-0.125, 0.1)) +
    labs(y = "Covariate", x = "Standardized Difference",
         title = "Study 2")

balplot_final <- plot_grid(balplot_1, balplot_2, ncol = 1,
                           rel_heights = c(1.6, 1), align = "v", axis = "lr")
ggsave("FiguresTables/balance.png", balplot_final, width = 5, height = 7)

########################################
#### Descriptive Statistics
########################################

## Use this function to unstandardize some measures
rescale_func <- function(x, mymin, mymax) {
    x * (mymax - mymin) + mymin
}

## N-size
dim(stud01)
dim(stud02)

## Gender
table(stud01$gender, useNA = "ifany")
18/3498
table(stud02$gender, useNA = "ifany")
table(stud02$gender, useNA = "ifany") / 1982
5/1982

## Age
table(stud01$age, useNA = "ifany")
mean(stud01$age, na.rm = TRUE)
59/3498
table(stud02$age, useNA = "ifany")
mean(stud02$age, na.rm = TRUE)
7/1982

## Student
table(stud01$employ, useNA = "ifany")
table(stud01$employ, useNA = "ifany") / 3498
table(stud02$w1_4, useNA = "ifany")
table(stud02$w1_4, useNA = "ifany") / 1982

## Party ID
table(rescale_func(stud01$partyidnum, 1, 7), useNA = "ifany")
mean(rescale_func(stud01$partyidnum, 1, 7), na.rm = TRUE)
173/3498
table(rescale_func(stud02$partyid, 1, 7), useNA = "ifany")
mean(rescale_func(stud02$partyid, 1, 7), na.rm = TRUE)
14/1982

## Education
table(stud01$education, useNA = "ifany")
table(stud01$education, useNA = "ifany") / 3498
## No data in study 2

## Income
table(stud01$income, useNA = "ifany")
table(stud01$income, useNA = "ifany") / 3498
## No data in study 2

## Marital Status
table(stud01$marital, useNA = "ifany")
table(stud01$marital, useNA = "ifany") / 3498
## No data on marital status in study 2

########################################
#### Supporting Tables, Changing Hearts, Study 1
########################################

## Study 1 humanization, supporting table for figure 2
r_s1_hum_treat <- lm(possec ~ treatment1 + treatment2 + treatment3,
                     data = stud01)
r_s1_hum_antdint <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01)
r_s1_hum_antdint_cont <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                                icb_pre_d + treatment1:icb_pre_d +
                                treatment2:icb_pre_d + treatment3:icb_pre_d +
                                gendernum + age + partyidnum,
                            data = stud01)
sink("FiguresTables/r_s1_hum_antdint_cont.tex")
apsrtable(r_s1_hum_treat, r_s1_hum_antdint, r_s1_hum_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()


## Study 1 empathy, supporting table for figure 3
r_s1_emp_treat <- lm(emp ~ treatment1 + treatment2 + treatment3,
                     stud01)
r_s1_emp_antcint <- lm(emp ~ treatment1 + treatment2 + treatment3 + icb_pre +
                           treatment1:icb_pre + treatment2:icb_pre +
                           treatment3:icb_pre,
                       stud01)
r_s1_emp_antcint_cont <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                                icb_pre + treatment1:icb_pre +
                                treatment2:icb_pre + treatment3:icb_pre +
                                gendernum + age + partyidnum,
                            stud01)
sink("FiguresTables/r_s1_emp_antcint_cont.tex")
apsrtable(r_s1_emp_treat, r_s1_emp_antcint, r_s1_emp_antcint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Main study 1 regression model for empathy ~ treatments * antipathy
r_s1_emp_antdint <- lm(emp ~ treatment1 + treatment2 + treatment3 + icb_pre_d +
                           treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       data = stud01)
r_s1_emp_antdint_cont <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                                icb_pre_d + treatment1:icb_pre_d +
                                treatment2:icb_pre_d + treatment3:icb_pre_d +
                                gendernum + age + partyidnum,
                            data = stud01)
sink("FiguresTables/r_s1_emp_antdint_cont.tex")
apsrtable(r_s1_emp_treat, r_s1_emp_antdint, r_s1_emp_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Supporting Tables, Changing Hearts, Study 2
########################################

r_s2_emp_treat <- lm(emp_index01 ~ condition, stud02)
r_s2_emp_antdint <- lm(emp_index01 ~ condition * hi_icb, stud02)
r_s2_emp_antdint_cont <- lm(emp_index01 ~ condition * hi_icb + gender + age +
                                partyid,
                            data = stud02)
sink("FiguresTables/r_s2_emp_antdint_cont.tex")
apsrtable(r_s2_emp_treat, r_s2_emp_antdint, r_s2_emp_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Supporting Tables, Dissonance as a Mechanism
########################################

r_s2_diss_treat <- lm(diss_measure ~ condition, stud02)
r_s2_diss_antdint <- lm(diss_measure ~ condition * hi_icb, stud02)
r_s2_diss_antdint_cont <- lm(diss_measure ~ condition * hi_icb + gender +
                                 age + partyid,
                             stud02)
sink("FiguresTables/r_s2_diss_antdint_cont.tex")
apsrtable(r_s2_diss_treat, r_s2_diss_antdint, r_s2_diss_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Supporting Tables, Changing Minds about Policy
########################################

## Study 1
r_s1_harm <- lm(harm ~ treatment1 + treatment2 + treatment3, stud01)
r_s1_harm_antdint <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre_d +
                        treatment1:icb_pre_d + treatment2:icb_pre_d +
                        treatment3:icb_pre_d,
                        stud01)
r_s1_harm_antdint_cont <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre_d +
                                 treatment1:icb_pre_d + treatment2:icb_pre_d +
                                 treatment3:icb_pre_d + gendernum + age +
                                 partyidnum,
                        stud01)
sink("FiguresTables/r_s1_harm_antdint_cont.tex")
apsrtable(r_s1_harm, r_s1_harm_antdint, r_s1_harm_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Study 2
r_s2_harm <- lm(policy_harm ~ condition, stud02)
r_s2_harm_antdint <- lm(policy_harm ~ condition * hi_icb, stud02)
r_s2_harm_antdint_cont <- lm(policy_harm ~ condition * hi_icb + gender + age +
                                 partyid,
                           stud02)
sink("FiguresTables/r_s2_harm_antdint_cont.tex")
apsrtable(r_s2_harm, r_s2_harm_antdint, r_s2_harm_antdint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Continuous

## Study 1
r_s1_harm <- lm(harm ~ treatment1 + treatment2 + treatment3, stud01)
r_s1_harm_antcint <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre +
                            treatment1:icb_pre + treatment2:icb_pre +
                            treatment3:icb_pre,
                        stud01)
r_s1_harm_antcint_cont <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre +
                                 treatment1:icb_pre + treatment2:icb_pre +
                                 treatment3:icb_pre + gendernum + age + partyidnum,
                             stud01)
sink("FiguresTables/r_s1_harm_antcint_cont.tex")
apsrtable(r_s1_harm, r_s1_harm_antcint, r_s1_harm_antcint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Study 2
r_s2_harm <- lm(policy_harm ~ condition, stud02)
r_s2_harm_antcint <- lm(policy_harm ~ condition * icb_measure, stud02)
r_s2_harm_antcint_cont <- lm(policy_harm ~ condition * icb_measure + gender +
                                 age + partyid,
                             stud02)
sink("FiguresTables/r_s2_harm_antcint_cont.tex")
apsrtable(r_s2_harm, r_s2_harm_antcint, r_s2_harm_antcint_cont,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Additional Results: Marginal Effects by Antipathy, Study 2
########################################

## ME regressions
r_s2_me1 <- lm(emp_index01 ~ condition, stud02)
r_s2_me2 <- lm(emp_index01 ~ condition * icb_measure, stud02)
r_s2_me3 <- lm(emp_index01 ~ condition * icb_measure + gender + age + partyid,
               stud02)
sink("FiguresTables/r_s2_mereg.tex")
apsrtable(r_s2_me1, r_s2_me2, r_s2_me3,
          digits = 2,
          align = "c",
          model.names = c("(1)", "(2)", "(3)"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy",
                         "Gender (1 = Female)", "Age", "Party ID (0--1)"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## ME fig
pdat1 <- TwowayME.f(r_s2_me2, stud02$condition, stud02$icb_measure, 95)
pdat2 <- data.frame(icb_measure = stud02$icb_measure,
                    conb = 0)
p <- ggplot(pdat1, aes(x = Znew)) +
    geom_line(aes(y = conb)) +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_rug(data = pdat2, aes(x = icb_measure, y = conb), sides = "b",
             position = position_jitter(width = 0.05, height = 0.001),
             alpha = 0.05) +
    labs(x = "Outgroup Antipathy", y = "Marginal Effects of Treatment\non Empathy")
ggsave("FiguresTables/s2_me.png", p, width = 3, height = 3)

########################################
#### Additional Results, by Sub-Population
########################################

## Humanization
r_s1_hum_antdint <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01)
r_s1_hum_subpop1 <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       voter_dummy == 1)
r_s1_hum_subpop2 <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       delegate_dummy == 1 | caucus_dummy == 1)
r_s1_hum_subpop3 <- lm(possec ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       elect_dummy == 1)
sink("FiguresTables/r_hum_subpop.tex")
apsrtable(r_s1_hum_antdint, r_s1_hum_subpop1, r_s1_hum_subpop2, r_s1_hum_subpop3,
          digits = 2,
          align = "c",
          model.names = c("Everyone", "Voters", "Activists", "Elected Officials"),
          coef.names = c("Intercept", "Humanization", "Information",
              "Combined", "Outgroup Antipathy", "Antipathy $\\times$ Humanization",
              "Antipathy $\\times$ Information", "Antipathy $\\times$ Combined"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note,
                       "Variables are on a 0--1 scale"))
sink()

## Empathy
r_s1_emp_antdint <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01)
r_s1_emp_subpop1 <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       voter_dummy == 1)
r_s1_emp_subpop2 <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       delegate_dummy == 1 | caucus_dummy == 1)
r_s1_emp_subpop3 <- lm(emp ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       elect_dummy == 1)
sink("FiguresTables/r_emp_subpop.tex")
apsrtable(r_s1_emp_antdint, r_s1_emp_subpop1, r_s1_emp_subpop2, r_s1_emp_subpop3,
          digits = 2,
          align = "c",
          model.names = c("Everyone", "Voters", "Activists", "Elected Officials"),
          coef.names = c("Intercept", "Humanization", "Information",
              "Combined", "Outgroup Antipathy", "Antipathy $\\times$ Humanization",
              "Antipathy $\\times$ Information", "Antipathy $\\times$ Combined"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note,
                       "Variables are on a 0--1 scale"))
sink()

## Empathy
r_s1_harm_antdint <- lm(harm ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01)
r_s1_harm_subpop1 <- lm(harm ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       voter_dummy == 1)
r_s1_harm_subpop2 <- lm(harm ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       delegate_dummy == 1 | caucus_dummy == 1)
r_s1_harm_subpop3 <- lm(harm ~ treatment1 + treatment2 + treatment3 +
                           treatment1 * icb_pre_d + treatment2 * icb_pre_d +
                           treatment3 * icb_pre_d,
                       data = stud01,
                       elect_dummy == 1)
sink("FiguresTables/r_harm_subpop.tex")
apsrtable(r_s1_harm_antdint, r_s1_harm_subpop1, r_s1_harm_subpop2, r_s1_harm_subpop3,
          digits = 2,
          align = "c",
          model.names = c("Everyone", "Voters", "Activists", "Elected Officials"),
          coef.names = c("Intercept", "Humanization", "Information",
              "Combined", "Outgroup Antipathy", "Antipathy $\\times$ Humanization",
              "Antipathy $\\times$ Information", "Antipathy $\\times$ Combined"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note,
                       "Variables are on a 0--1 scale"))
sink()

########################################
#### Additional Results, Interaction with Political Ideology or Party ID
########################################

## Study 1 Interaction, ideology (both empathy and policy outcomes)
reg_med_me1 <- lm(emp ~ treatment1 + ideologynum + treatment2 + treatment3
                  + treatment2 * ideologynum + treatment3 * ideologynum
                  + treatment1 * ideologynum,
                  stud01)
reg_med_me2 <- lm(emp ~ treatment2 + ideologynum + treatment1 + treatment3
                  + treatment1 * ideologynum + treatment3 * ideologynum
                  + treatment2 * ideologynum,
                  stud01)
reg_med_me3 <- lm(emp ~ treatment3 + ideologynum + treatment2 + treatment1
                  + treatment2 * ideologynum + treatment1 * ideologynum
                  + treatment3 * ideologynum,
                  stud01)
reg_med_me1harm <- lm(harm ~ treatment1 + ideologynum + treatment2 + treatment3
                      + treatment2 * ideologynum + treatment3 * ideologynum
                      + treatment1 * ideologynum,
                      stud01)
reg_med_me2harm <- lm(harm ~ treatment2 + ideologynum + treatment1 + treatment3
                      + treatment1 * ideologynum + treatment3 * ideologynum
                      + treatment2 * ideologynum,
                      stud01)
reg_med_me3harm <- lm(harm ~ treatment3 + ideologynum + treatment2 + treatment1
                      + treatment2 * ideologynum + treatment1 * ideologynum
                      + treatment3 * ideologynum,
                      stud01)
dat1 <- TwowayME.f(reg_med_me1, stud01$treatment1, stud01$ideologynum, 95)
dat2 <- TwowayME.f(reg_med_me2, stud01$treatment2, stud01$ideologynum, 95)
dat3 <- TwowayME.f(reg_med_me3, stud01$treatment3, stud01$ideologynum, 95)
dat1harm <- TwowayME.f(reg_med_me1harm, stud01$treatment1, stud01$ideologynum, 95)
dat2harm <- TwowayME.f(reg_med_me2harm, stud01$treatment1, stud01$ideologynum, 95)
dat3harm <- TwowayME.f(reg_med_me3harm, stud01$treatment1, stud01$ideologynum, 95)
dat1 <- cbind(outcome = "Empathic Concern", treatment = "Humanization", dat1)
dat2 <- cbind(outcome = "Empathic Concern", treatment = "Information", dat2)
dat3 <- cbind(outcome = "Empathic Concern", treatment = "Combined", dat3)
dat1harm <- cbind(outcome = "Policy Harm", treatment = "Humanization", dat1harm)
dat2harm <- cbind(outcome = "Policy Harm", treatment = "Information", dat2harm)
dat3harm <- cbind(outcome = "Policy Harm", treatment = "Combined", dat3harm)
plot_data1 <- rbind(dat1, dat2, dat3, dat1harm, dat2harm, dat3harm)
dat1 <- cbind(outcome = "Empathic Concern", treatment = "Humanization",
              ideologynum = as.numeric(reg_med_me1$model$ideologynum),
              conb = as.numeric(0))
dat2 <- cbind(outcome = "Empathic Concern", treatment = "Information",
              ideologynum = as.numeric(reg_med_me2$model$ideologynum),
              conb = as.numeric(0))
dat3 <- cbind(outcome = "Empathic Concern", treatment = "Combined",
              ideologynum = as.numeric(reg_med_me3$model$ideologynum),
              conb = as.numeric(0))
dat1harm <- cbind(outcome = "Policy Harm", treatment = "Humanization",
              ideologynum = as.numeric(reg_med_me1harm$model$ideologynum),
              conb = as.numeric(0))
dat2harm <- cbind(outcome = "Policy Harm", treatment = "Information",
              ideologynum = as.numeric(reg_med_me2harm$model$ideologynum),
              conb = as.numeric(0))
dat3harm <- cbind(outcome = "Policy Harm", treatment = "Combined",
              ideologynum = as.numeric(reg_med_me3harm$model$ideologynum),
              conb = as.numeric(0))
plot_data2 <- data.frame(rbind(dat1, dat2, dat3, dat1harm, dat2harm, dat3harm))
plot_data2$ideologynum <- as.numeric(as.character(plot_data2$ideologynum))
plot_data2$conb <- as.numeric(as.character(plot_data2$conb))
p <- ggplot(plot_data1, aes(x = Znew)) +
    geom_line(aes(y = conb)) +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_rug(data = plot_data2, aes(x = ideologynum, y = conb), sides = "b",
             position = position_jitter(width = 0.05, height = 0.001),
             alpha = 0.05) +
    xlab("Political Ideology") +
    ylab("Marginal Effects of Treatment\non Empathic Concern and Policy Harm") +
    facet_grid(outcome ~ treatment)
ggsave("FiguresTables/s1_me_ideology.png", p, width = 6, height = 5)

## Study 1 Interaction, party id, with empathy and policy as outcomes
reg_med_me1 <- lm(emp ~ treatment1 + partyidnum + treatment2 + treatment3
                  + treatment2 * partyidnum + treatment3 * partyidnum
                  + treatment1 * partyidnum,
                  stud01)
reg_med_me2 <- lm(emp ~ treatment2 + partyidnum + treatment1 + treatment3
                  + treatment1 * partyidnum + treatment3 * partyidnum
                  + treatment2 * partyidnum,
                  stud01)
reg_med_me3 <- lm(emp ~ treatment3 + partyidnum + treatment2 + treatment1
                  + treatment2 * partyidnum + treatment1 * partyidnum
                  + treatment3 * partyidnum,
                  stud01)
reg_med_me1harm <- lm(harm ~ treatment1 + partyidnum + treatment2 + treatment3
                      + treatment2 * partyidnum + treatment3 * partyidnum
                      + treatment1 * partyidnum,
                      stud01)
reg_med_me2harm <- lm(harm ~ treatment2 + partyidnum + treatment1 + treatment3
                      + treatment1 * partyidnum + treatment3 * partyidnum
                      + treatment2 * partyidnum,
                      stud01)
reg_med_me3harm <- lm(harm ~ treatment3 + partyidnum + treatment2 + treatment1
                      + treatment2 * partyidnum + treatment1 * partyidnum
                      + treatment3 * partyidnum,
                      stud01)
dat1 <- TwowayME.f(reg_med_me1, stud01$treatment1, stud01$partyidnum, 95)
dat2 <- TwowayME.f(reg_med_me2, stud01$treatment2, stud01$partyidnum, 95)
dat3 <- TwowayME.f(reg_med_me3, stud01$treatment3, stud01$partyidnum, 95)
dat1harm <- TwowayME.f(reg_med_me1harm, stud01$treatment1, stud01$partyidnum, 95)
dat2harm <- TwowayME.f(reg_med_me2harm, stud01$treatment1, stud01$partyidnum, 95)
dat3harm <- TwowayME.f(reg_med_me3harm, stud01$treatment1, stud01$partyidnum, 95)
dat1 <- cbind(outcome = "Empathic Concern", treatment = "Humanization", dat1)
dat2 <- cbind(outcome = "Empathic Concern", treatment = "Information", dat2)
dat3 <- cbind(outcome = "Empathic Concern", treatment = "Combined", dat3)
dat1harm <- cbind(outcome = "Policy Harm", treatment = "Humanization", dat1harm)
dat2harm <- cbind(outcome = "Policy Harm", treatment = "Information", dat2harm)
dat3harm <- cbind(outcome = "Policy Harm", treatment = "Combined", dat3harm)
plot_data1 <- rbind(dat1, dat2, dat3, dat1harm, dat2harm, dat3harm)
dat1 <- cbind(outcome = "Empathic Concern", treatment = "Humanization",
              partyidnum = as.numeric(reg_med_me1$model$partyidnum),
              conb = as.numeric(0))
dat2 <- cbind(outcome = "Empathic Concern", treatment = "Information",
              partyidnum = as.numeric(reg_med_me2$model$partyidnum),
              conb = as.numeric(0))
dat3 <- cbind(outcome = "Empathic Concern", treatment = "Combined",
              partyidnum = as.numeric(reg_med_me3$model$partyidnum),
              conb = as.numeric(0))
dat1harm <- cbind(outcome = "Policy Harm", treatment = "Humanization",
              partyidnum = as.numeric(reg_med_me1harm$model$partyidnum),
              conb = as.numeric(0))
dat2harm <- cbind(outcome = "Policy Harm", treatment = "Information",
              partyidnum = as.numeric(reg_med_me2harm$model$partyidnum),
              conb = as.numeric(0))
dat3harm <- cbind(outcome = "Policy Harm", treatment = "Combined",
              partyidnum = as.numeric(reg_med_me3harm$model$partyidnum),
              conb = as.numeric(0))
plot_data2 <- data.frame(rbind(dat1, dat2, dat3, dat1harm, dat2harm, dat3harm))
plot_data2$partyidnum <- as.numeric(as.character(plot_data2$partyidnum))
plot_data2$conb <- as.numeric(as.character(plot_data2$conb))
p <- ggplot(plot_data1, aes(x = Znew)) +
    geom_line(aes(y = conb)) +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_rug(data = plot_data2, aes(x = partyidnum, y = conb), sides = "b",
             position = position_jitter(width = 0.05, height = 0.001),
             alpha = 0.05) +
    xlab("Party ID") +
    ylab("Marginal Effects of Treatment\non Empathic Concern and Policy Harm") +
    facet_grid(outcome ~ treatment)
ggsave("FiguresTables/s1_me_partyid.png", p, width = 6, height = 5)

## Study 2, with empathy and policy outcomes
r_s2_meparty <- lm(emp_index01 ~ condition * partyid, stud02)
r_s2_mepartyharm <- lm(policy_harm ~ condition * partyid, stud02)
pdat1a <- TwowayME.f(r_s2_meparty, stud02$condition, stud02$partyid, 95)
pdat1b <- TwowayME.f(r_s2_mepartyharm, stud02$condition, stud02$partyid, 95)
pdat1 <- rbind(cbind(outcome = "Empathic Concern", pdat1a),
               cbind(outcome = "Policy Harm", pdat1b))
pdat2a <- data.frame(outcome = "Empathic Concern",
                     partyid = stud02$partyid,
                     conb = 0)
pdat2b <- data.frame(outcome = "Policy Harm",
                     partyid = stud02$partyid,
                     conb = 0)
pdat2 <- data.frame(rbind(pdat2a, pdat2b))
p <- ggplot(pdat1, aes(x = Znew)) +
    geom_line(aes(y = conb)) +
    geom_line(aes(y = upper), linetype = "dashed") +
    geom_line(aes(y = lower), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed", size = 0.25) +
    geom_rug(data = pdat2, aes(x = partyid, y = conb), sides = "b",
             position = position_jitter(width = 0.05, height = 0.001),
             alpha = 0.05) +
    labs(x = "Party ID", y = "Marginal Effects of Treatment\non Empathic Concern and Policy Harm") +
    facet_wrap(~ outcome, ncol = 1)
ggsave("FiguresTables/s2_meparty.png", p, width = 3, height = 5)

########################################
#### Additional Results, Study 2 3-item antipathy measure
########################################

## New antipathy measure
myvars <- paste0("icb", 8:10)
stud02[, icb_alt := psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)$scores]
stud02[, hi_icb_alt := as.integer(!(icb_alt < 4))]
stud02[is.na(icb_alt), hi_icb_alt := NA]

myvars <- c("condition", "hi_icb_alt", "emp_index01", "diss_measure",
            "policy_harm")
tmpdat <- stud02[, ..myvars]
setnames(tmpdat, "hi_icb_alt", "hi_icb")

## Study 2 humanization t-test
with(stud02[hi_icb_alt == 0],
     t.test(pre_hum_measure, post_hum_measure, paired = TRUE))
with(stud02[hi_icb_alt == 1],
     t.test(pre_hum_measure, post_hum_measure, paired = TRUE))

## Study 2 tables
r_s2icbalt_1 <- lm(emp_index01 ~ condition * hi_icb, tmpdat)
r_s2_emp_antdint <- lm(emp_index01 ~ condition * hi_icb, stud02)
sink("FiguresTables/r_s2icbalt_1.tex")
apsrtable(r_s2icbalt_1, r_s2_emp_antdint,
          digits = 2,
          align = "c",
          model.names = c("3-Item", "9-Item"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()
r_s2icbalt_2 <- lm(diss_measure ~ condition * hi_icb, tmpdat)
r_s2_diss_antdint <- lm(diss_measure ~ condition * hi_icb, stud02)
sink("FiguresTables/r_s2icbalt_2.tex")
apsrtable(r_s2icbalt_2, r_s2_diss_antdint,
          digits = 2,
          align = "c",
          model.names = c("3-Item", "9-Item"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()
r_s2icbalt_3 <- lm(policy_harm ~ condition * hi_icb, tmpdat)
r_s2_harm_antdint <- lm(policy_harm ~ condition * hi_icb, stud02)
sink("FiguresTables/r_s2icbalt_3.tex")
apsrtable(r_s2icbalt_3, r_s2_harm_antdint,
          digits = 2,
          align = "c",
          model.names = c("3-Item", "9-Item"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Additional Results, Separate Laws
########################################

## Study 1, examining the policy outcomes separately
r_s1_law1_antint <- lm(law_english ~ treatment1 + treatment2 + treatment3 +
                           icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       stud01)
r_s1_law2_antint <- lm(law_tuition ~ treatment1 + treatment2 + treatment3 +
                           icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       stud01)
r_s1_law3_antint <- lm(law_welfare ~ treatment1 + treatment2 + treatment3 +
                           icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       stud01)
r_s1_law4_antint <- lm(law_hire ~ treatment1 + treatment2 + treatment3 +
                           icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       stud01)
r_s1_immop_antint <- lm(immig_opinion_reverse ~ treatment1 + treatment2 + treatment3 +
                            icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                            treatment3:icb_pre_d,
                        stud01)
r_s1_ariz_antint <- lm(arizona_law ~ treatment1 + treatment2 + treatment3 +
                           icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                           treatment3:icb_pre_d,
                       stud01)
r_s1_arizlike_antint <- lm(st8_hb497 ~ treatment1 + treatment2 + treatment3 +
                               icb_pre_d + treatment1:icb_pre_d + treatment2:icb_pre_d +
                               treatment3:icb_pre_d,
                           stud01)
sink("FiguresTables/r_s1_polsep.tex")
apsrtable(r_s1_law1_antint, r_s1_law2_antint, r_s1_law3_antint,
          r_s1_law4_antint, r_s1_immop_antint, r_s1_ariz_antint,
          r_s1_arizlike_antint,
          digits = 2,
          align = "c",
          model.names = c("Law (English)", "Law (Tuition)", "Law (Welfare)",
                          "Law (Hire)", "Imm. Opinion", "AZ Law",
                          "State Bill Harm"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy",
                         "Antipathy $\\times$ Humanization",
                         "Antipathy $\\times$ Information",
                         "Antipathy $\\times$ Combined"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Study 2, examining the policy outcomes separately
myvars <- c("condition", "hi_icb", "pol1b", "pol3", "pol4a", "pol4b", "pol5a",
            "pol5b", "pol5c", "pol5d")
tmpdat <- stud02[, ..myvars]
tmpdat[, pol1b := abs((pol1b - 1) / (5 - 1) - 1)]
tmpdat[, pol3 := abs((pol3 - 1) / (4 - 1) - 1)]
tmpdat[, pol4a := (pol4a - 1) / (8 - 1)]
tmpdat[, pol4b := (pol4b - 1) / (8 - 1)]
tmpdat[, pol5a := (pol5a - 1) / (8 - 1)]
tmpdat[, pol5b := (pol5b - 1) / (8 - 1)]
tmpdat[, pol5c := (pol5c - 1) / (8 - 1)]
tmpdat[, pol5d := (pol5d - 1) / (8 - 1)]
r_s2_aidillegal <- lm(pol1b ~ condition * hi_icb, tmpdat)
r_s2_immop <- lm(pol3 ~ condition * hi_icb, tmpdat)
r_s2_takeresource <- lm(pol4a ~ condition * hi_icb, tmpdat)
r_s2_denyrights <- lm(pol4b ~ condition * hi_icb, tmpdat)
r_s2_law1 <- lm(pol5a ~ condition * hi_icb, tmpdat)
r_s2_law2 <- lm(pol5b ~ condition * hi_icb, tmpdat)
r_s2_law3 <- lm(pol5c ~ condition * hi_icb, tmpdat)
r_s2_law4 <- lm(pol5d ~ condition * hi_icb, tmpdat)
sink("FiguresTables/r_s2_polsep.tex")
apsrtable(r_s2_law1, r_s2_law2, r_s2_law3, r_s2_law4, r_s2_immop,
          r_s2_aidillegal, r_s2_takeresource, r_s2_denyrights,
          digits = 2,
          align = "c",
          model.names = c("Law (English)", "Law (Tuition)", "Law (Welfare)",
                          "Law (Hire)", "Imm. Opinion", "Aid Illegal",
                          "Take Resources", "Deny Rights"),
          coef.names = c("Intercept", "Illegal", "Antipathy",
                         "Illegal $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Additional Results, Common Policy Outcomes
########################################

stud01[, harm_alt := rowMeans(.SD, na.rm = TRUE),
       .SDcols = c("law_english", "law_tuition", "law_welfare", "law_hire",
                   "immig_opinion_reverse")]

myvars <- c("pol3_rev", "pol5a", "pol5b", "pol5c", "pol5d")
tmpcb <- psych::alpha(as.data.frame(stud02[, ..myvars]), check.keys = TRUE)
stud02[, harm_alt := tmpcb$scores]
stud02[, harm_alt := (harm_alt - 1) / (6.4 - 1)]

r_s1_harm_antdint <- lm(harm ~ treatment1 + treatment2 + treatment3 + icb_pre_d +
                        treatment1:icb_pre_d + treatment2:icb_pre_d +
                        treatment3:icb_pre_d,
                        stud01)
r_s1_harm_alt <- lm(harm_alt ~ treatment1 + treatment2 + treatment3 + icb_pre_d +
                        treatment1:icb_pre_d + treatment2:icb_pre_d +
                        treatment3:icb_pre_d,
                        stud01)
sink("FiguresTables/r_s1_harm_alt.tex")
apsrtable(r_s1_harm_alt, r_s1_harm_antdint,
          digits = 2,
          align = "c",
          model.names = c("Common Policy Items", "Full Policy Scale"),
          coef.names = c("Intercept", "Humanization", "Information",
                         "Combined", "Outgroup Antipathy", "Humanization $\\times$ Antipathy",
                         "Information $\\times$ Antipathy", "Combined $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

## Study 2
r_s2_harm_antdint <- lm(policy_harm ~ condition * hi_icb, stud02)
r_s2_harm_alt <- lm(harm_alt ~ condition * hi_icb, stud02)
sink("FiguresTables/r_s2_harm_alt.tex")
apsrtable(r_s2_harm_alt, r_s2_harm_antdint,
          digits = 2,
          align = "c",
          model.names = c("Common Policy Items", "Full Policy Scale"),
          coef.names = c("Intercept", "Illegal Condition", "Outgroup Antipathy",
                         "Illegal Condition $\\times$ Antipathy"),
          order = "lr",
          coef.rows = 2,
          Sweave = TRUE,
          stars = "default",
          notes = list(se.note, stars.note))
sink()

########################################
#### Additional Results, Empathy and Policy
########################################

## Hexagon plot of relationship between empathy and harmful policies in study 1
p_data <- stud01[, c("treatment", "emp", "harm", "icb_pre_d"), with = FALSE]
p_data[, icb_pre_d := factor(icb_pre_d)]
p_data[, icb_pre_d := plyr::mapvalues(icb_pre_d, 0:1, c("Low", "High"))]
mylabs <- c("Control", "Information", "Humanization",
            "Combined")
p_data[, treatment := plyr::mapvalues(treatment, c(4, 1:3), mylabs)]
p_data[, treatment := factor(treatment, mylabs)]
p_data <- na.omit(p_data)
ggplot(p_data, aes(x = emp, y = harm)) +
    stat_binhex(bins = 15) +
    scale_fill_gradientn(colours = c("light gray", "black")) +
    geom_smooth(aes(linetype = icb_pre_d), method = "lm", colour = "black",
                se = FALSE, size = 0.75) +
    labs(x = "Empathic Concern", y = "Support for Harmful Policies",
         linetype = "Outgroup\nAntipathy", fill = "Count") +
    facet_wrap(~ treatment, nrow = 1)
ggsave("FiguresTables/s1_hexplot_empANDharm_BYantipathy.png", height = 2.75,
       width = 6.25)

## Hexagon plot for study 2
p_data <- stud02[, c("condition", "emp_index01", "policy_harm", "hi_icb"),
                 with = FALSE]
p_data[, hi_icb := factor(hi_icb)]
p_data[, hi_icb := plyr::mapvalues(hi_icb, 0:1, c("Low", "High"))]
mylabs <- c("Legal Immigrants", "Illegal Immigrants")
p_data[, condition := plyr::mapvalues(condition, 0:1, mylabs)]
p_data[, condition := factor(condition, mylabs)]
p_data <- na.omit(p_data)
ggplot(p_data, aes(x = emp_index01, y = policy_harm)) +
    stat_binhex(bins = 15) +
    scale_fill_gradientn(colours = c("light gray", "black")) +
    geom_smooth(aes(linetype = hi_icb), method = "lm", colour = "black",
                se = FALSE, size = 0.75) +
    labs(x = "Empathic Concern", y = "Support for Harmful Policies",
         linetype = "Outgroup\nAntipathy", fill = "Count") +
    facet_wrap(~ condition, nrow = 1)
ggsave("FiguresTables/s2_hexplot_empANDharm_BYantipathy.png", height = 2.75,
       width = 3.75)

########################################
#### Additional Results, flexible interaction effects
########################################

## Study 1, Empathy, Humanization
set.seed(33333)
s1_intchk_emp_1 <- interflex(estimator = "kernel", data = stud01,
                             Y = "emp", D = "treatment", X = "icb_pre",
                             treat.type = "discrete", base = "4", na.rm = TRUE,
                             main = "Kernel", ylim = c(-0.25, 0.6), ncols = 1,
                             xlab = "", ylab = "Marginal Effect of Treatment on Empathy",
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
s1_intchk_emp_2 <- interflex(estimator = "binning", data = stud01,
                             Y = "emp", D = "treatment", X = "icb_pre",
                             treat.type = "discrete", base = "4", na.rm = TRUE,
                             nbins = 2, main = "Two Bins (Paper)", ylab = "",
                             ylim = c(-0.25, 0.6), ncols = 1,
                             xlab = "Outgroup Antipathy (Moderator)",
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5,
                             bin.labs = FALSE)
s1_intchk_emp_3 <- interflex(estimator = "binning", data = stud01,
                             Y = "emp", D = "treatment", X = "icb_pre",
                             treat.type = "discrete", base = "4", na.rm = TRUE,
                             nbins = 3, main = "Three Bins", ylab = "", xlab = "",
                             ylim = c(-0.25, 0.6), ncols = 1,
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5,
                             bin.labs = FALSE)
pFinal <- plot_grid(s1_intchk_emp_1$figure, s1_intchk_emp_2$figure, s1_intchk_emp_3$figure,
                    ncol = 3, nrow = 1, rel_widths = c(1, 1, 1))
ggsave("FiguresTables/s1_intchk_emp_new.png", pFinal, width = 6, height = 6.5)

## Study 2, Empathy
set.seed(33333)
s2_intchk_emp_1 <- interflex(estimator = "kernel", data = stud02,
                             Y = "emp_index01", D = "condition", X = "icb_measure",
                             treat.type = "discrete", base = "0", na.rm = TRUE,
                             main = "Kernel", ylim = c(-0.34, 0.052),
                             xlab = "", ylab = "Marginal Effect of Treatment on Empathy",
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
s2_intchk_emp_2 <- interflex(estimator = "binning", data = stud02, nbins = 2,
                             cutoffs = 0.5, Y = "emp_index01", D = "condition",
                             X = "icb_measure", treat.type = "discrete",
                             base = "0", na.rm = TRUE, bin.labs = FALSE,
                             main = "Two Bins (Paper)", ylim = c(-0.34, 0.052),
                             xlab = "Outgroup Antipathy", ylab = "",
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
s2_intchk_emp_3 <- interflex(estimator = "binning", data = stud02, nbins = 3,
                             Y = "emp_index01", D = "condition", X = "icb_measure",
                             treat.type = "discrete", base = "0", na.rm = TRUE,
                             main = "Three Bins", ylim = c(-0.34, 0.052),
                             xlab = "", ylab = "", bin.labs = FALSE,
                             cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
pFinal <- plot_grid(s2_intchk_emp_1$figure, s2_intchk_emp_2$figure, s2_intchk_emp_3$figure,
                    nrow = 1, rel_widths = c(1, 1, 1))
ggsave("FiguresTables/s2_intchk_emp_new.png", width = 6, height = 2.5)

## Study 2, Dissonance
set.seed(33333)
s2_intchk_diss_1 <- interflex(estimator = "kernel", data = stud02,
                              Y = "diss_measure", D = "condition", X = "icb_measure",
                              treat.type = "discrete", base = "0", na.rm = TRUE,
                              main = "Kernel", ylim = c(-0.125, 0.2),
                              xlab = "", ylab = "Marginal Effect of Treatment on Dissonance",
                              cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
s2_intchk_diss_2 <- interflex(estimator = "binning", data = stud02, nbins = 2,
                              cutoffs = 0.5, Y = "diss_measure", D = "condition",
                              X = "icb_measure", treat.type = "discrete",
                              base = "0", na.rm = TRUE, bin.labs = FALSE,
                              main = "Two Bins (Paper)", ylim = c(-0.125, 0.2),
                              xlab = "Outgroup Antipathy", ylab = "",
                              cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
s2_intchk_diss_3 <- interflex(estimator = "binning", data = stud02, nbins = 3,
                              Y = "diss_measure", D = "condition", X = "icb_measure",
                              treat.type = "discrete", base = "0", na.rm = TRUE,
                              main = "Three Bins", ylim = c(-0.125, 0.2),
                              xlab = "", ylab = "", bin.labs = FALSE,
                              cex.main = 0.5, cex.sub = 0.5, cex.lab = 0.5, cex.axis = 0.5)
pFinal <- plot_grid(s2_intchk_diss_1$figure, s2_intchk_diss_2$figure, s2_intchk_diss_3$figure,
                    nrow = 1, rel_widths = c(1, 1, 1))
ggsave("FiguresTables/s2_intchk_diss_new.png", width = 6, height = 2.5)
