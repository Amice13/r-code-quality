### File    : syria-selfid-replication.r
##  Author  : Daniel Corstange (daniel.corstange@gmail.com)
##  Created : Fri 12 Apr 2019 --- 09:58 AM
##  Notes   : replication script for "National and Subnational
##            Identification in the Syrian Civil War"

### read/process data
#### preliminaries (load some libraries)

## load various libraries
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(xtable)
library(texreg)
library(stargazer)
library(lmtest)
library(car)
library(pscl)                           # for vuong test
library(psy)                            # for cronbach
library(psych)                          # for tetrachoric

#### read data

Syr15 <- read_spss("corstange-syria-jop-replication.sav")

#### process background covariates

BG <- 
    transmute(
        Syr15,
        ## metadata
        rid    = 150000 + 1:nrow(Syr15),
        ## basic demographics
        female = as.logical(Q5),
        age    = Q7,
        age_bi = ifelse(age >= median(age, na.rm = TRUE),
                        TRUE, FALSE),
        province_syr =
            factor(as_factor(Q15)),
        ## room density (and binary versions)
        syr_room_density = Q10 / (Q11 + 1),
        leb_room_density = Q12 / (Q13 + 1),
        srd_bi =
            ifelse(syr_room_density >=
                   median(syr_room_density,
                          na.rm = TRUE),
                   TRUE, FALSE),
        lrd_bi =
            ifelse(leb_room_density >=
                   median(leb_room_density,
                          na.rm = TRUE),
                   TRUE, FALSE),
        lsrd_bi =
            ifelse((leb_room_density / syr_room_density) >=
                   median((leb_room_density / syr_room_density),
                          na.rm = TRUE),
                   TRUE, FALSE),
        ## education
        educ =
            factor(ifelse(as_factor(Q8) == "Illiterate", 1,
                   ifelse(as_factor(Q8) == "Finished primary school", 2,
                   ifelse(as_factor(Q8) == "Finished middle school", 3,
                          4))),
                   labels = c("Illiterate",
                              "Primary",
                              "Middle",
                              "Secondary")),
        educ_03 =
            as.numeric(educ) - 1,
        ## finished high school
        ## watch out for the backquote (`) and the leading space!
        educ_hs = ifelse(as_factor(Q8)
                         %in% c("Finished secondary school",
                                "Some university",
                                "Bachelor`s degree",
                                "Master`s degree or higher",
                                "Vocational/Technical training"),
                         TRUE, FALSE),
        ## knowledge
        know = ((as_factor(Q27a) == "Walid al-Mouallem") +
                (as_factor(Q27c) == "7 Years") +
                (as_factor(Q27d) == "250") +
                (as_factor(Q27e) == "Tunisia")),
        know_bi =
            ifelse(know > median(know, na.rm = TRUE),
                   TRUE, FALSE),
        ## somewhat/very interested in politics
        interest_bi =
            ifelse(as_factor(Q25)
                   %in% c("Very interested",
                          "Somewhat interested"), TRUE,
            ifelse(as_factor(Q25)
                   %in% c("A little interested",
                          "Not interested"), FALSE,
                   NA)),
        ## understand politics (middle category counted as "understand")
        understand_bi =
            ifelse(Q26 <= -98, NA,
            ifelse(Q26 >= 3, TRUE,
                   FALSE)),
        ## engagement
        engaged = know_bi + interest_bi + understand_bi,
        engaged_bi =              # >= median: 918, 1082; > median: 1267, 733
            ifelse(engaged >= median(engaged, na.rm = TRUE),
                   TRUE, FALSE),
        engaged_3cat =
            factor(ifelse(engaged == 0, 1,
                   ifelse(engaged == 3, 3,
                          2)),
                   labels = c("Low", "Mid", "High")),
        ## community
        sunni =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q21),
                         ignore.case = TRUE), NA,
            ifelse(as_factor(Q21) == "Sunni", TRUE,
                   FALSE)),
        alawi =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q21),
                         ignore.case = TRUE), NA,
            ifelse(as_factor(Q21) == "Alawi", TRUE,
                   FALSE)),
        kurdish_speak =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q19b),
                         ignore.case = TRUE), NA,
            ifelse(as_factor(Q19b) == "Comfortably", TRUE,
                   FALSE)),
        minority = ifelse(!sunni | kurdish_speak, TRUE, FALSE),
        sect_3cat =
            factor(ifelse(!sunni, 3,
                   ifelse(kurdish_speak, 2,
                   ifelse(sunni, 1, NA))),
                   labels = c("Sunni",
                              "Kurd",
                              "Heterodox")),
        sect = as_factor(Q21),
        sect =
            factor(ifelse(sect == "Sunni" & !kurdish_speak, 1,
                   ifelse(kurdish_speak, 2,
                   ifelse(sect %in% c("Shia",
                                      "Alawi",
                                      "Ismaili"), 3,
                   ifelse(grepl("Christian",
                                sect,
                                ignore.case = TRUE), 4,
                   ifelse(sect == "Druze", 5,
                   ifelse(sect == "Other", 6,
                          NA)))))),
                   labels = c("Sunni Arab",
                              "Kurd",
                              "Alawi/Shia",
                              "Christian",
                              "Druze",
                              "Other")),
        ## religiosity
        pray_daily =
            ifelse(as_factor(Q22) %in% c("No Answer",
                                         "Don`t Know",
                                         "Not Applicable"), NA,
            ifelse(as_factor(Q22) %in% c("Once or twice a day",
                                         "Several times a day"), TRUE,
                   FALSE)),
        pray_multiple =
            ifelse(as_factor(Q22) %in% c("No Answer",
                                         "Don`t Know",
                                         "Not Applicable"), NA,
            ifelse(as_factor(Q22) %in% c("Several times a day"), TRUE,
                   FALSE)),
        quran =
            factor(as_factor(Q23)),
        quran_04 =
            as.numeric(quran) - 1,
        quran_daily =
            ifelse(as_factor(Q23) %in% c("No Answer",
                                         "Don`t Know",
                                         "Not Applicable"), NA,
            ifelse(as_factor(Q23) %in% c("Once or twice a day",
                                         "Several times a day"), TRUE,
                   FALSE)),
        quran_weekly =
            ifelse(as_factor(Q23) %in% c("No Answer",
                                         "Don`t Know",
                                         "Not Applicable"), NA,
            ifelse(as_factor(Q23) %in% c("Once a week",
                                         "Once or twice a day",
                                         "Several times a day"), TRUE,
                   FALSE)),
        ## religion and politics
        rel_econ =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q24a),
                         ignore.case = TRUE), NA,
                   Q24a),
        rel_pol =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q24b),
                         ignore.case = TRUE), NA,
                   Q24b),
        rel_crime =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q24c),
                         ignore.case = TRUE), NA,
                   Q24c),
        rel_family =
            ifelse(grepl("no answer|don[`']t know|not applicable",
                         as_factor(Q24d),
                         ignore.case = TRUE), NA,
                   Q24d),
        ## factional support
        faction_none =            # no faction mentioned
            ifelse(is.na(Q28a) &
                   is.na(Q28b) &
                   is.na(Q28c) &
                   is.na(Q28d) &
                   is.na(Q28e) &
                   is.na(Q28f),
                   TRUE, FALSE),
        faction_govt =            # if govt 1st or 2nd (most Kurds say govt 2nd)
            ifelse(is.na(Q28b) | Q28b == 3,
                   FALSE, TRUE),
        faction_fsa =             # fsa 1st, NOT mention Islamists
            ifelse(is.na(Q28a)  |
                   Q28a != 1    |
                   !is.na(Q28c) |
                   !is.na(Q28d),
                   FALSE, TRUE),
        faction_islamist =          # any Islamists mentioned
            ifelse(is.na(Q28c) & is.na(Q28d), FALSE, TRUE),
        faction_islamist_domestic = # domestic mentioned, foreign NOT mentioned
            ifelse(!is.na(Q28c) & is.na(Q28d), TRUE, FALSE),
        faction_islamist_foreign =  # foreign mentioned at all
            ifelse(is.na(Q28d), FALSE, TRUE),
        faction_oppn =
            ifelse(faction_govt & faction_none, TRUE, FALSE),
        faction_all =
            factor(ifelse(faction_none, 1,
                   ifelse(faction_islamist_foreign, 2,
                   ifelse(faction_islamist_domestic, 3,
                   ifelse(faction_fsa, 4,
                   ifelse(faction_govt, 5,
                          NA))))),
                   labels = c("No Preference",
                              "Foreign Islamists",
                              "Domestic Islamists",
                              "Nationalists",
                              "Government"))
    )

#### process experiment

FR <-
    transmute(
        Syr15,
        ## metadata
        rid    = 150000 + 1:nrow(Syr15),
        ## framing 
        fr_group =
            factor(Q30,
                   labels = c("ctrl",
                              "sect-many",
                              "sect-few",
                              "dem",
                              "rel",
                              "for",
                              "sect-dem",
                              "sect-rel",
                              "sect-for",
                              "dem-sect",
                              "rel-sect",
                              "for-sect")),
        ## pool many/few (for robustness check)
        fr_group_pooled =
            factor(ifelse(fr_group == "ctrl", "ctrl",
                   ifelse(fr_group %in% c("sect-many", "sect-few"), "sect",
                   ifelse(fr_group == "dem", "dem",
                   ifelse(fr_group == "rel", "rel",
                   ifelse(fr_group == "for", "for",
                   ifelse(fr_group %in% c("sect-dem", "dem-sect"), "sect-dem",
                   ifelse(fr_group %in% c("sect-rel", "rel-sect"), "sect-rel",
                   ifelse(fr_group %in% c("sect-for", "for-sect"), "sect-for",
                          NA)))))))),
                   levels = c("ctrl",
                              "sect",
                              "dem",
                              "rel",
                              "for",
                              "sect-dem",
                              "sect-rel",
                              "sect-for")),
        ## pool many/few
        fr_ctrl = grepl("ctrl", fr_group),
        fr_sect = grepl("sect", fr_group),
        fr_dem  = grepl("dem", fr_group),
        fr_rel  = grepl("rel", fr_group),
        fr_for  = grepl("for", fr_group),
        ## "m" many, "f" few (for robustness check)
        fr_sectm =
            ifelse(fr_group %in% c("sect-many",
                                   "sect-dem",
                                   "sect-rel",
                                   "sect-for"),
                   TRUE, FALSE),
        fr_sectf =
            ifelse(fr_group %in% c("sect-few",
                                   "dem-sect",
                                   "rel-sect",
                                   "for-sect"),
                   TRUE, FALSE),
        fr_demm =
            ifelse(fr_group %in% c("dem",
                                   "dem-sect"),
                   TRUE, FALSE),
        fr_demf =
            ifelse(fr_group %in% c("sect-dem"),
                   TRUE, FALSE),
        fr_relm =
            ifelse(fr_group %in% c("rel",
                                   "rel-sect"),
                   TRUE, FALSE),
        fr_relf =
            ifelse(fr_group %in% c("sect-rel"),
                   TRUE, FALSE),
        fr_form =
            ifelse(fr_group %in% c("for",
                                   "for-sect"),
                   TRUE, FALSE),
        fr_forf =
            ifelse(fr_group %in% c("sect-for"),
                   TRUE, FALSE),
        ## self-id outcome
        id_categories =
            factor(as_factor(Q32b)),
        id_syrian =
            ifelse(id_categories == "Only Syrian", TRUE, FALSE),
        id_3cat =
            factor(ifelse(id_categories == "Only Syrian", 1,
                   ifelse(grepl("Race|Religion|Sect|Tribe",
                                id_categories), 2,
                   ifelse(grepl("Class|Job|Other",
                                id_categories), 3,
                          NA))),
                   labels = c("Syrian",
                              "Communal",
                              "Class")),
        id_4cat =
            factor(ifelse(id_categories == "Only Syrian", 1,
                   ifelse(grepl("Religion|Sect",
                                id_categories), 2,
                   ifelse(grepl("Race|Tribe",
                                id_categories), 3,
                          4))),
                   labels = c("Syrian",
                              "Communal",
                              "Religion",
                              "Class")),
        )

#### combine background and experiment data

ID <- left_join(BG, FR, by = "rid")

### estimate models
#### define the equations

## equations to estimate for:
##   - the core specifications reported in the article
##     + eq_min (bivariate relationship with minority/majority)
##     + eq_treat (average treatment effect)
##     + eq_treatmin (conditional average treatment effect)
##   - various other specifications to confirm robustness
##     + (everything else)

eq_min <-
    formula(id_syrian
            ~ minority)

eq_treat <-
    formula(id_syrian
            ~ fr_sect
            + fr_dem
            + fr_rel
            + fr_for)

eq_treatmin <-
    formula(id_syrian
            ~ minority * fr_sect
            + minority * fr_dem
            + minority * fr_rel
            + minority * fr_for)

eq_base <-
    formula(id_syrian
            ~ age
            + female
            + quran_04
            + educ_03
            + log(syr_room_density)
            + log(leb_room_density)
            + faction_govt
            + minority)

eq_full <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_sect
                   + minority * fr_dem
                   + minority * fr_rel
                   + minority * fr_for)

eq_treat2 <-
    formula(id_syrian
            ~ fr_sect * fr_dem
            + fr_sect * fr_rel
            + fr_sect * fr_for)

eq_treat2min <-
    formula(id_syrian
            ~ minority * fr_sect * fr_dem
            + minority * fr_sect * fr_rel
            + minority * fr_sect * fr_for)

eq_base_treat <-
    update.formula(eq_base,
                   . ~ .
                   + fr_sect
                   + fr_dem
                   + fr_rel
                   + fr_for)

eq_base_treat2 <-
    update.formula(eq_base,
                   . ~ .
                   + fr_sect * fr_dem
                   + fr_sect * fr_rel
                   + fr_sect * fr_for)

eq_base_treatmin <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_sect
                   + minority * fr_dem
                   + minority * fr_rel
                   + minority * fr_for)

eq_base_treat2min <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_sect * fr_dem
                   + minority * fr_sect * fr_rel
                   + minority * fr_sect * fr_for)

#### estimate the models

## we'll do OLS for simplicity and binary probit to
## confirm robustness to functional form assumptions
mods_id_ols <- mods_id_probit <- list()

mods_names <-
    c("eq_base",
      "eq_min",
      "eq_treat",
      "eq_treat2",
      "eq_treatmin",
      "eq_treat2min",
      "eq_base_treat",
      "eq_base_treat2",
      "eq_base_treatmin",
      "eq_base_treat2min",
      "eq_full")

## NB: drop the 9 NAs from majority/minority
for (name in mods_names) {
    mods_id_ols[[name]] <-
        glm(eval(parse(text = name)),
            family = gaussian,
            subset = !is.na(minority),
            data   = ID)
    mods_id_probit[[name]] <-
        glm(eval(parse(text = name)),
            family = binomial(link = "probit"),
            subset = !is.na(minority),
            data   = ID)
}

### self-id summary statistics (article: Table 1)

## full sample distibution (full; majority; minority)
selfid_full <-
    rbind(with(ID,
               prop.table(table(id_4cat))),
          with(filter(ID, !minority),
               prop.table(table(id_4cat))),
          with(filter(ID, minority),
               prop.table(table(id_4cat))))
selfid_full <-
    substr(format(round(selfid_full, 3),
                  nsmall = 3),
           2, 10)

## set the row and column names
dimnames(selfid_full) <-
    list(c("Full Sample",
           "Majority",
           "Minority"),
         c("Syrian Only",
           "Ethnicity/Tribe",
           "Religion/Sect",
           "Class/Other"))

## simple table with all conditions
xtable(selfid_full,
       align   = "lcccc",
       label   = "tab:dv",
       caption = paste("Self-identification by category,",
                       "proportions pooled across",
                       "all experimental conditions"))

### main results table (article: Table 2)

## which models should we use?
tex_mods <-
    with(mods_id_ols,
         list(eq_min,
              eq_treat,
              eq_treatmin))

## what should we call the models?
tex_cols <-
    c("Baseline",
      "Average Effects",
      "Conditional Effects")

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Minority",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame",
      "Sectarian Frame $\\times$ Minority",
      "Democracy Frame $\\times$ Minority",
      "Secularism Frame $\\times$ Minority",
      "Foreigners Frame $\\times$ Minority")

## reorder coefficients
tex_cord <- c(1:2, 3, 7, 4, 8, 5, 9, 6, 10)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       single.row         = TRUE,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

### main results figure (article: Figure 1)
#### helper function (predicted probabilities, probit)

## calculate marginal effects for dummies in a GLM
## arguments:
##  - model estimated via glm
##  - string for the first treatment variable 
##  - string for the conditioning variable 
##  - regexp of variables to set to 0
## examples:
##  - pp_glm(mod, "fr_sect", "minority", "fr_(dem|rel|for)")
##  - pp_glm(mod, "fr_rel", "minority")

pp_glm <- function(mod, x, z,
                   re_to0 = NULL,
                   probs  = c(.025, .050, .500, .950, .975),
                   reps   = 1000,
                   digits = 3) {
    ## basic testing for unmatched variable names
    for (i in c(x, z))
        if (length(grep(i, names(mod$coefficients))) == 0)
            stop(paste("No match for ", "\"", i, "\"", sep = ""))
    ## gather coefficient names
    bx  <- names(mod$coefficients)[grep(x,  names(mod$coefficients))]
    bz  <- names(mod$coefficients)[grep(z,  names(mod$coefficients))]
    bx0 <- bx[-grep(":", bx)]           # constituent term
    bz0 <- bz[-grep(":", bz)]           # constituent term
    bxz <- bx[ grep(":", bx)]           # interaction term
    bz_ <- bz[ grep(":", bz)]           # misc other terms
    ## link function
    link <- if (is.null(mod$family)) {
                identity
            } else if (mod$family$link == "probit") {
                pnorm
            } else if (mod$family$link == "logit") {
                plogis
            } else {
                NULL
            }
    ## simulate the coefficients
    require(MASS)
    B <- mvrnorm(n     = reps,
                 mu    = coef(mod),
                 Sigma = vcov(mod))
    ## get model matrix
    X <- model.matrix(mod)
    ## if there are variables to set to 0 by fiat, do that
    if (!is.null(re_to0)) {
        X[,names(mod$coefficients)[grepl(re_to0, names(mod$coefficients))]] <- 0
    }
    ## set up simulation bucket
    sim <-
        array(data     = 0,
              dim      = c(nrow(X), reps, 4),
              dimnames = list(1:nrow(X),
                              1:reps,
                              c("x0z0",
                                "x0z1",
                                "x1z0",
                                "x1z1")))
    j <- 1
    for (xi in 0:1) {
        X0       <- X
        X0[,bx0] <- xi
        for (zi in 0:1) {
            X0[,bz0] <- zi
            X0[,bxz] <- xi * zi
            X0[,bz_] <- X0[,bz_] * zi
            sim[,,j] <- link(X0 %*% t(B))
            j <- j + 1
        }
    }
    pp <-
        matrix(data     = 0,
               nrow     = 4,
               ncol     = 5,
               dimnames = list(c("x0z0",
                                 "x0z1",
                                 "x1z0",
                                 "x1z1"),
                               names(quantile(0, probs = probs))))
    for (j in 1:4) {
        means  <- apply(sim[,,j], 2, mean)
        pp[j,] <- quantile(means, probs = probs)
    }
    ## return
    return(list(pp     = pp,
                raw    = sim))
}

#### calculate predicted probabilities for interactive models

## only select the TREAT * MINORITY interactive models
these_mods <- names(mods_id_probit)[grepl("treatmin", names(mods_id_probit))]
margs_sect <- list()

## for each treatment type, calculate the PPs
for (mod in these_mods) {
    for(mtype in c("fr_sect", "fr_rel", "fr_dem", "fr_for")) {
        margs_sect[[mtype]][[mod]] <-
            pp_glm(mods_id_probit[[mod]], mtype, "minority")
    }
}

#### plot it
##### plot probability difference for all frames
###### gather data

Diff_treat <- 
    rbind(with(margs_sect$fr_sect$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z0"] - # majority: treatment effect
                                    raw[,,"x0z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)),
                     quantile(apply(raw[,,"x1z1"] - # minority: treatment effect
                                    raw[,,"x0z1"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_rel$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z0"] - # majority: treatment effect
                                    raw[,,"x0z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)),
                     quantile(apply(raw[,,"x1z1"] - # minority: treatment effect
                                    raw[,,"x0z1"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_dem$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z0"] - # majority: treatment effect
                                    raw[,,"x0z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)),
                     quantile(apply(raw[,,"x1z1"] - # minority: treatment effect
                                    raw[,,"x0z1"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_for$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z0"] - # majority: treatment effect
                                    raw[,,"x0z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)),
                     quantile(apply(raw[,,"x1z1"] - # minority: treatment effect
                                    raw[,,"x0z1"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))))
Diff_treat <-
    setNames(as_tibble(Diff_treat),
             c("lo95", "lo90", "pe", "hi90", "hi95"))
Diff_treat$treat <-
    rep(factor(4:1,
               labels = c("Sect",
                          "Religion",
                          "Democracy",
                          "Foreigners")),
        each = 2)
Diff_treat$minority <- c(FALSE, TRUE)
Diff_treat$sig <-
    with(Diff_treat,
         sign(lo95) == sign(hi95))

###### plot it

plot_treat_diff <-
    ggplot(Diff_treat,
           aes(x = ifelse(minority,
                          as.numeric(treat) + 0.10,
                          as.numeric(treat) - 0.10),
               y = pe,
               color = sig,
               shape = minority,
               group = minority)) +
    ## vertical line
    geom_hline(yintercept = 0,
               lty        = 2) +
    ## error bars
    geom_errorbar(aes(ymin = lo90,
                      ymax = hi90),
                  width = 0.05) +
    geom_errorbar(aes(ymin = lo95,
                      ymax = hi95),
                  width = 0.00) +
    ## points
    geom_point(size = 2.50,
               fill = "white") +
    ## manual
    scale_shape_manual(values = c(19, 15),
                       labels = c("Majority", "Minority"),
                       guide  = guide_legend(reverse = TRUE)) +
    scale_color_manual(values = c("grey50", "black"),
                       guide  = FALSE) +
    ## scales
    ## scale_x_continuous(name         = "Treatments",
    scale_x_continuous(name         = NULL,
                       labels       = c("Sectarian",
                                        "Secularism",
                                        "Democracy",
                                        "Foreigners"),
                       breaks       = 4:1,
                       minor_breaks = NULL,
                       limits       = c(0.50, 4.50)) +
    scale_y_continuous(name         = "Probability Difference",
                       breaks       = seq(-.60, .60, .20),
                       labels       = round(seq(-.60, .60, .20), 1),
                       limits       = c(-.42, .42)) +
    ## rest
    coord_flip() +
    theme_bw() +
    theme(legend.position = c(0.84, 0.17), # bottom-right inside plot
          legend.text     = element_text(size = 6),
          ## legend.position = "bottom",
          ## axis.text.y     = element_text(color = c("black", rep("grey50", 3))),
          axis.text.y     = element_text(color = c("black",
                                                   rep("grey33", 3)),
                                         face  = c("bold.italic",
                                                   rep("plain", 3))),
          legend.title    = element_blank())
plot_treat_diff

##### plot predicted probabilities for sect frame
###### gather data

Dat <-
    setNames(as_tibble(margs_sect$fr_sect$eq_treatmin$pp),
             c("lo95", "lo90", "pe", "hi90", "hi95"))
Dat$treat    <- rep(c(FALSE, TRUE), each = 2)
Dat$minority <- rep(c(FALSE, TRUE), times = 2)

###### plot it
plot_sect_pp <-
    ggplot(Dat,
           aes(x = ifelse(minority,
                          as.numeric(treat) - 0.05,
                          as.numeric(treat) + 0.05),
               y = pe,
               group = minority)) +
    ## error bars
    geom_errorbar(aes(ymin = lo90,
                      ymax = hi90),
                  color = "grey50",
                  width = 0.05) +
    geom_errorbar(aes(ymin = lo95,
                      ymax = hi95),
                  color = "grey50",
                  width = 0.00) +
    ## points and lines
    geom_line(color = "grey50") +
    geom_point(size = 3.00) +
    ## label lines
    annotate(geom  = "text",
             x     = c(.33, .67),
             y     = c(.73, .57),
             ## y     = c(.75, .57),
             size  = 2.50,
             label = c("Majority", "Minority")) +
    ## scales
    scale_x_continuous(name         = "Sectarian Frame",
                       labels       = c("No", "Yes"),
                       breaks       = 0:1,
                       minor_breaks = NULL,
                       limits       = c(-0.25, 1.25)) +
    scale_y_continuous(name         = "Predicted Probability",
                       breaks       = seq(.1, .9, .2),
                       limits       = c(0.28, 0.92)) +
    theme_bw()
plot_sect_pp

##### plot majority-minority gap
###### gather data

Diff_minority <- 
    rbind(with(margs_sect$fr_sect$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z1"] -
                                    raw[,,"x1z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_rel$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z1"] -
                                    raw[,,"x1z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_dem$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z1"] -
                                    raw[,,"x1z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))),
          with(margs_sect$fr_for$eq_treatmin,
               rbind(quantile(apply(raw[,,"x1z1"] -
                                    raw[,,"x1z0"],
                                    2, mean),
                              probs = c(.025, .050, .500, .950, .975)))))
Diff_minority <-
    setNames(as_tibble(Diff_minority),
             c("lo95", "lo90", "pe", "hi90", "hi95"))
Diff_minority$treat <-
    rep(factor(4:1,
               labels = rev(c("Sectarian",
                              "Secularism",
                              "Democracy",
                              "Foreigners"))))
Diff_minority$sig <-
    with(Diff_minority,
         sign(lo95) == sign(hi95))

###### plot it

plot_minority_diff <-
    ggplot(Diff_minority,
           aes(x = treat,
               y = pe,
               color = sig)) +
    ## vertical line
    geom_hline(yintercept = 0,
               lty        = 2) +
    ## error bars
    geom_errorbar(aes(ymin = lo90,
                      ymax = hi90),
                  width = 0.05) +
    geom_errorbar(aes(ymin = lo95,
                      ymax = hi95),
                  width = 0.00) +
    ## points
    geom_point(size = 2.50,
               fill = "white") +
    ## scales
    ## xlab("Treatments") +
    xlab(NULL) +
    scale_color_manual(values = c("black", "grey50"),
                       guide  = FALSE) +
    scale_y_continuous(name         = "Probability Difference",
                       breaks       = seq(-.60, .60, .20),
                       labels       = round(seq(-.60, .60, .20), 1),
                       limits       = c(-.50, .50)) +
    ## rest
    coord_flip() +
    theme_bw() +
    theme(axis.text.y = element_text(color = c(rep("grey33", 3),
                                               "black"),
                                     face  = c(rep("plain", 3),
                                               "bold.italic")))
plot_minority_diff 

### appendix A (Sample Composition and Benchmark Comparisons)
#### demographic benchmarks
##### sources
###### male/female in Lebanon

## UNHCR data source
## https://data2.unhcr.org/en/situations/syria/location/71
## https://data2.unhcr.org/api/population/get/demography?widget_id=71639&geo_id=71&sv_id=4&population_collection=22

## second link is the raw data in JSON format
## it shows that:
##   males:   469150
##   females: 517792
## so the percent male is:
##   469150 / (469150 + 517792) # => .475

###### male/female in Syria

## World Bank data source
## https://datacatalog.worldbank.org/dataset/gender-statistics
## http://databank.worldbank.org/data/reports.aspx?source=gender-statistics

## indicator is:
## Sex ratio at birth (male births per female births)

## available data
## 2002: 1.1
## 2007: 1.1
## 2014: 1.1

###### literacy rates in syria

## World Development Indicators data source

## indicators are
## literacy rate, adult female (% of females ages 15 and above)
## literacy rate, adult male (% of males ages 15 and above)
## literacy rate, adult total (% of people ages 15 and above)

## available data
## 2002: 74.2, 91.0, 82.9
## 2004: 73.6, 87.8, 80.8

## (note the other data source in the AJPS appendix does not
## show later literacy rate data as Erin wrote)

## CIA World Factbook

## indicators are
## literacy, age 15 and over can read and write

## 2015 estimate
## female: 81
## male: 91.7
## total: 86.4

###### educational attainment in Syria

## World Development Indicators data source

## college education
## Educational attainment, at least completed post-secondary, population 25+, female (%) (cumulative)
## Educational attainment, at least completed post-secondary, population 25+, male (%) (cumulative)
## Educational attainment, at least completed post-secondary, population 25+, total (%) (cumulative)

## 2009 estimate
## 10.3, 14.5, 12.4

## HS or better
## Educational attainment, at least completed upper secondary, population 25+, female (%) (cumulative)
## Educational attainment, at least completed upper secondary, population 25+, male (%) (cumulative)
## Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative)

## 2009 estimate
## 18.9, 24.8, 21.9


## middle school or better
## Educational attainment, at least completed lower secondary, population 25+, female (%) (cumulative)
## Educational attainment, at least completed lower secondary, population 25+, male (%) (cumulative)
## Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)

## 2009 estimate
## 29.0, 38.9, 34.1

###### educational attainment in Lebanon

## source
## ASSESSMENT OF THE IMPACT OF SYRIAN REFUGEES IN LEBANON AND THEIR
## EMPLOYMENT PROFILE 2013 International Labour Organization Regional
## Office for the Arab States
## p. 20

###### household size in Syria

## source
## Food security in Syria: Preliminary results based on the
## 2006/07 expenditure survey
## p. 7

###### household size in Lebanon

## source
## ASSESSMENT OF THE IMPACT OF SYRIAN REFUGEES IN LEBANON AND THEIR
## EMPLOYMENT PROFILE 2013 International Labour Organization Regional
## Office for the Arab States
## p. 19

###### ethnic/religious

## CIA World Factbook
## Arab 90.3; Kurdish, Armenian, other 9.7

## Sunni 74
## Alawi, Ismaili, and Shia 13
## Christian 10
## Druze 3

###### poverty

## World Factbook

## population below poverty line (2014 estimate)
## 82.5

##### gather data

Benchmarks <-
    tribble(
        ~ stat,               ~ syr07, ~ syr09,  ~ syr15, ~ leb, ~ sample,
        "male",               NA,      .52,      NA,      .48,   .60,
        "literacy, female",   NA,      NA,       .81,     .85,   .81,
        "literacy, male",     NA,      NA,       .92,     .87,   .91,
        "literacy, total",    NA,      NA,       .86,     .86,   .87,
        "HS, female",         NA,      .19,      NA,      .10,   .18,
        "HS, male",           NA,      .25,      NA,      .13,   .21,
        "HS, total",          NA,      .22,      NA,      .12,   .20,
        "college, female",    NA,      .10,      NA,      .03,   .02,
        "college, male",      NA,      .15,      NA,      .04,   .03,
        "college, total",     NA,      .12,      NA,      .04,   .02,
        "family size syr",    5.8,     NA,       NA,      NA,    6.4,
        "family size leb",    NA,      NA,       NA,      5,     5.5
    )                          

Benchmarks[,"stat"] <-
    c("Proportion Male",
      "Literacy, Female",
      "Literacy, Male",
      "Literacy, Total",
      "Secondary School Completion, Female",
      "Secondary School Completion, Male",
      "Secondary School Completion, Total",
      "College Completion, Female",
      "College Completion, Male",
      "College Completion, Total",
      "Family Size, Pre-War",
      "Family Size, Post-War")

names(Benchmarks) <-
    c("Statistic",
      "2007",                           # Syria 2007
      "2009",                           # Syria 2009
      "2015",                           # Syria 2015
      "Population",                     # Lebanon population
      "Sample")                         # Lebanon sample

##### table

print(xtable(Benchmarks,
             align   = "llccccc",
             label   = "tab:benchmarks",
             caption = "Basic Demographic Benchmarks"),
      include.rownames = FALSE)

#### age distributions
##### process data
###### gather data from UN-sponsored Vulnerability Assessment

## UN figures compiled from:
##   UNICEF et al. (2017), Vulnerability Assessment of Syrian
##   Refugees in Lebanon, 2016, produced jointly by UNICEF,
##   UNHCR, and the UN World Food Programme.
##     authors sent us the data file for the age distribution.
##     see <unicef-etal-vulnerability-assessment-age.xlsx>

UN_age <-
    data.frame(bins   = factor(1:15,
                               labels = c("0-4",
                                          "5-9",
                                          "10-14",
                                          "15-19",
                                          "20-24",
                                          "25-29",
                                          "30-34",
                                          "35-39",
                                          "40-44",
                                          "45-49",
                                          "50-54",
                                          "55-59",
                                          "60-64",
                                          "65-69",
                                          "70+")),
               male   = c(20, 18, 13, 8, 5,
                          6,  9,  7,  4, 3,
                          2,  2,  1,  1, 1) / 100,
               female = c(17, 16, 12, 9, 9,
                          10, 8,  6,  4, 3,
                          2,  2,  1,  1, 1) / 100)
UN_age <-
    with(UN_age,
         data.frame(bins = bins,
                    age  = c(male, female),
                    sex  = rep(factor(1:2,
                                      labels = c("Male",
                                                 "Female")),
                               each = nrow(UN_age))))

###### gather survey data

## bin survey data
ID$agebins5 <-
    with(ID,
         cut(age,
             c(-Inf, seq(24, 64, 5), Inf),
             labels = c("20-24",
                        "25-29",
                        "30-34",
                        "35-39",
                        "40-44",
                        "45-49",
                        "50-54",
                        "55-59",
                        "60-64",
                        "65-69")))

## bin survey data to match the UN data
ID$agebins5 <-
ID$agebins5_alt <-
    with(ID,
         cut(age,
             c(-Inf, seq(4, 69, 5), Inf),
             labels = c("0-4",
                        "5-9",
                        "10-14",
                        "15-19",
                        "20-24",
                        "25-29",
                        "30-34",
                        "35-39",
                        "40-44",
                        "45-49",
                        "50-54",
                        "55-59",
                        "60-64",
                        "65-69",
                        "70+")))

## get the survey data into dataframes
Age <-
    data.frame(bins = levels(ID$agebins5),
               age  = c(with(subset(ID, female == 0),
                             prop.table(table(agebins5))),
                       with(subset(ID, female == 1),
                            prop.table(table(agebins5)))),
               sex  = rep(factor(1:2,
                                 labels = c("Male", "Female")),
                          each = length(levels(ID$agebins5))))
Age_alt <-
    data.frame(bins = ordered(1:length(levels(ID$agebins5_alt)),
                              labels = levels(ID$agebins5_alt)),
               age  = c(with(subset(ID, female == 0),
                             prop.table(table(agebins5_alt))),
                       with(subset(ID, female == 1),
                            prop.table(table(agebins5_alt)))),
               sex  = rep(factor(1:2,
                                 labels = c("Male", "Female")),
                          each = length(levels(ID$agebins5_alt))))

###### put UN and survey data together (adults only)

## UN data adults only
UN_adult <-
    subset(UN_age,
           !(bins %in% c("0-4",
                         "5-9",
                         "10-14",
                         "15-19")))
UN_adult$age <- 
    c(with(subset(UN_adult, sex == "Male"),
           age / sum(age)),
      with(subset(UN_adult, sex == "Female"),
           age / sum(age)))

## survey data adults only
Survey_adult <- 
    subset(Age_alt,
           !(bins %in% c("0-4",
                         "5-9",
                         "10-14",
                         "15-19")))

## combine UN and survey data
Adult_combo <- rbind(UN_adult, Survey_adult)
Adult_combo$src <-
    rep(factor(1:2,
               labels = c("UN Vulnerability Assessment", "Survey")),
        each = nrow(Adult_combo) / 2)
Adult_combo$age <-
    with(Adult_combo,
         ifelse(sex == "Male", age, -age))

##### KS test of equality of distributions across data sources

## equality of male subsample across survey and UN data (p = .99)
ks.test(filter(Adult_combo,
               sex == "Male",
               src == "Survey")$age,
        filter(Adult_combo,
               sex == "Male",
               src != "Survey")$age)

## equality of female subsample across survey and UN data (p = .99)
ks.test(filter(Adult_combo,
               sex == "Female",
               src == "Survey")$age,
        filter(Adult_combo,
               sex == "Female",
               src != "Survey")$age)

##### plot UN and survey

plot_age_both <-
    ggplot(data = Adult_combo,
           aes(x    = bins,
               y    = age,
               fill = sex)) +
    geom_hline(yintercept = 0) +
    geom_bar(stat = "identity",
             data = subset(Adult_combo, sex == "Male"),
             alpha = 0.50) +
    geom_bar(stat = "identity",
             data = subset(Adult_combo, sex == "Female"),
             alpha = 0.50) +
    annotate(geom  = "rect",
             xmin  = (nrow(Adult_combo) / 4) - 0.33,
             xmax  = (nrow(Adult_combo) / 4) + 0.33,
             ymin  = 0.125 * c(-1, 1) - 0.05,
             ymax  = 0.125 * c(-1, 1) + 0.05,
             color = "grey50",
             fill  = "white") +
    annotate(geom     = "text",
             x        = nrow(Adult_combo) / 4,
             y        = -0.125,
             fontface = "italic",
             color    = "grey50",
             label    = "Female") +
    annotate(geom     = "text",
             x        = nrow(Adult_combo) / 4,
             y        = 0.125,
             fontface = "italic",
             color    = "grey50",
             label    = "Male") +
    scale_fill_manual(values = c("darkred", "darkblue")) +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.3, 0.3, 0.1),
                       labels = abs(round(seq(-0.3, 0.3, 0.1), 1))) +
    labs(x    = "Age",
         y    = "Proportion") +
    coord_flip() +
    facet_wrap(~ src) +
    theme_bw() +
    theme(legend.position = "none")
plot_age_both 

#### province of origin
##### gather data

## UNHCR figures compiled from:
##   UNHCR Lebanon - Beirut Country Office. Syria Refugee Response
##   Lebanon: Places of Origin of Syrian Refugees Registered in
##   Lebanon. 31 March 2015.

## figures on province population in Syria come from:
##   OCHA (2014), "Syrian Arab Republic Governorates Profile (June 2014)"
##   https://reliefweb.int/sites/reliefweb.int/files/resources/Syria%20governorate%20profiles%206%20August%202014.pdf

Province <-
    data.frame(province = levels(ID$province_syr),
               syria_n  = c("Daraa"        = 1027000,
                            "Quneitra"     = 90000,
                            "Al-Suwayda"   = 370000,
                            "Dimashq"      = 1754000,
                            "Rif Dimashq"  = 2835900,
                            "Homs"         = 1803000,
                            "Hama"         = 1628000,
                            "Tartus"       = 797000,
                            "Latakia"      = 1008000,
                            "Idlib"        = 1501000,
                            "Halab"        = 4867991,
                            "Al-Raqqa"     = 944000,
                            "Deir al-Zour" = 1239005,
                            "Al-Hasaka"    = 1512000),
               survey_n = as.vector(table(ID$province_syr)),
               unhcr_n  = c("Daraa"        = 23386 + 30613 + 28354,
                            "Quneitra"     = 10512 + 31,
                            "Al-Suwayda"   = 251 + 666 + 124,
                            "Dimashq"      = 55105,
                            "Rif Dimashq"  = 19175 + 62755 + 14502 + 25305 + 12524 + 2679 + 11440 + 2326,
                            "Homs"         = 1089 + 3072 + 151189 + 15150 + 55705 + 19940,
                            "Hama"         = 16048 + 50585 + 7681 + 11229 + 956,
                            "Tartus"       = 1749 + 25 + 49 + 1181 + 345,
                            "Latakia"      = 3464 + 919 + 40 + 520,
                            "Idlib"        = 4412 + 67666 +10462 + 24061 + 45247,
                            "Halab"        = 4618 + 23882 + 26423 + 28898 + 11140 + 10608 + 133659 + 3094,
                            "Al-Raqqa"     = 5678 + 43411 + 15850,
                            "Deir al-Zour" = 17169 + 3395 + 5094,
                            "Al-Hasaka"    = 2711 + 15233 + 12351 + 618))
## convert to proportions
Province$syria_prop <-
    with(Province,
         syria_n / sum(syria_n))
Province$survey_prop <-
    with(Province,
         survey_n / sum(survey_n))
Province$unhcr_prop <-
    with(Province,
         unhcr_n / sum(unhcr_n))
## difference between survey proportions and UNHCR proportions
Province$survey_vs_unhcr <-
    with(Province,
         survey_prop - unhcr_prop)
## fancy province labels: province name + proportion of Syrian pop
levels(Province$province) <-
    with(Province,
         sort(paste(as.character(province),
                    paste0("(",
                           substr(format(round(Province$syria_prop, 2)), 2, 10),
                           ")"))))
## reorder province levels by survey proportion
Province$province <-
    with(Province,
         reorder(province, -survey_prop))

##### KS test of equality of distributions across data sources

## equality across survey and UN data (p = .90)
with(Province,
     ks.test(survey_prop, unhcr_prop))

##### plot

plot_province <-
    ggplot(data = Province,
           aes(x = province,
               y = survey_prop)) +
    geom_bar(stat = "identity",
             aes(x = province,
                 y = survey_prop),
             fill  = "darkblue",
             alpha = .50) +
    geom_bar(stat = "identity",
             aes(x = province,
                 y = -unhcr_prop),
             fill  = "darkred",
             alpha = .50) +
    geom_hline(yintercept = 0) +
    geom_line(aes(x = as.numeric(province),
                  y = survey_vs_unhcr),
              color = "darkorchid4") +
    geom_point(aes(x = as.numeric(province),
                   y = survey_vs_unhcr),
               color = "darkorchid4") +
    geom_label(x        = length(Province$province),
               y        = -.125,
               size     = 3,
               fontface = "italic",
               color    = "grey50",
               label    = "UNHCR Estimate") +
    geom_label(x        = length(Province$province),
               y        = .125,
               size     = 3,
               fontface = "italic",
               color    = "grey50",
               label    = "Survey Sample") +
    scale_y_continuous(limits = c(-0.25, 0.25),
                       breaks = seq(-0.3, 0.3, 0.1),
                       labels = abs(round(seq(-0.3, 0.3, 0.1), 1))) +
    labs(x    = "Province",
         y    = "Proportion") +
    coord_flip() +
    theme_bw()
plot_province
#### sect

Sect <-
    tribble(
        ~ group,      ~ hinnebusch, ~ gulf2000, ~ cia,
        "Sunni Arab", 61,           59,         65,
        "Kurd"      , 8,            9,          9,
        "Alawi/Shia", 13.5,         11.8,       13,
        "Christian",  14.5,         9.3,        10,
        "Druze",      3,            3.2,        3
    )
Sect$survey <- as.numeric(round(prop.table(table(ID$sect))[-6] * 100, 1))

## prettified the table by hand, but here are the data
Sect

### appendix B (Summary Statistics from Survey Sample)
#### summary statistics table

Sumstats <-
    as.data.frame(dplyr::select(ID,
                                minority,
                                age,
                                female,
                                quran_04,
                                educ_03,
                                syr_room_density,
                                leb_room_density,
                                faction_govt))

stargazer(Sumstats,
          align  = TRUE,
          digits = 2,
          label  = "tab:sumstats:iv",
          title  = "Summary Statistics",
          covariate.labels = c("Minority",
                               "Age",
                               "Female",
                               "Quran",
                               "Education",
                               "Room Density (in Syria)",
                               "Room Density (in Lebanon)",
                               "Government Supporter"),
          summary.stat     = c("mean", "sd", "min", "max"))

### appendix C (Pooling Treatments)
#### lrtest version
##### helper function to do lrtests on a list of models

## x is a list of models
lrtest_mods <- function(x) {
    require(car)
    nmods <- length(x)
    mat   <- matrix(data     = 0,
                    nrow     = nmods,
                    ncol     = nmods,
                    dimnames = list(names(x),
                                    names(x)))
    ## do lrtests for each pair of models
    for (i in 1:2) {
        for (j in (i+1):3) {
            mat[i,j] <-
                lrtest(x[[i]],
                       x[[j]])["Pr(>Chisq)"][2,]

        }
    }
    return(t(mat))
}

##### define equations

eq_base <-
    formula(id_syrian
            ~ age
            + female
            + quran_04
            + educ_03
            + log(syr_room_density)
            + log(leb_room_density)
            + faction_govt
            + minority)

eq_treat_raw <-
    formula(id_syrian
            ~ minority * fr_group)

eq_treat_pooled <-
    formula(id_syrian
            ~ minority * fr_group_pooled)

eq_treat_main <-
    formula(id_syrian
            ~ minority * fr_sect
            + minority * fr_dem
            + minority * fr_rel
            + minority * fr_for)

eq_full_raw <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_group)

eq_full_pooled <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_group_pooled)

eq_full_main <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_sect
                   + minority * fr_dem
                   + minority * fr_rel
                   + minority * fr_for)

##### models

mods_robust_ols <- mods_robust <- list()

mods_names <-
    c("eq_treat_raw",
      "eq_treat_pooled",
      "eq_treat_main",
      "eq_full_raw",
      "eq_full_pooled",
      "eq_full_main")

## make sure to drop NAs from majority/minority
for (name in mods_names) {
    mods_robust[[name]] <-
        glm(eval(parse(text = name)),
            family = binomial(link = "probit"),
            subset = !is.na(minority),
            data   = ID)
    mods_robust_ols[[name]] <-
        glm(eval(parse(text = name)),
            family = gaussian,
            subset = !is.na(minority),
            data   = ID)
}

##### lrtests

## model labels
mod_names <-
    c("Raw",
      "Pooled",
      "Main")

## container matrices
lr_out_ols <- lr_out <- 
    matrix(data     = "",
           nrow     = 3,
           ncol     = 3,
           dimnames = list(mod_names,
                           mod_names))

## ols version
p_base_ols <-
    format(round(lrtest_mods(mods_robust_ols[1:3]), 2),
           digits = 2)
p_full_ols <-
    format(round(lrtest_mods(mods_robust_ols[4:6]), 2),
           digits = 2)

## probit version
p_base <-
    format(round(lrtest_mods(mods_robust[1:3]), 2),
           digits = 2)
p_full <-
    format(round(lrtest_mods(mods_robust[4:6]), 2),
           digits = 2)


## put models with/without covariates on upper/lower triangle
lr_out_ols[which(lower.tri(lr_out_ols))] <-
    p_base_ols[which(lower.tri(p_base_ols))]
lr_out_ols[which(upper.tri(lr_out_ols))] <-
    p_full_ols[which(lower.tri(p_full_ols))]

lr_out[which(lower.tri(lr_out))] <-
    p_base[which(lower.tri(p_base))]
lr_out[which(upper.tri(lr_out))] <-
    p_full[which(lower.tri(p_full))]

##### latex table

xtable(cbind(lr_out_ols,
             lr_out),
       align   = c("lcccccc"),
       caption = paste("Likelihood ratio test $p$-values",
                       "of treatment group operationalizations",
                       "(lower triangle: no covariates,",
                       "upper triangle: with covariates)"),
       label   = "tab:lrtest")

#### Vuong version
##### define equations

eq_base <-
    formula(id_syrian
            ~ age
            + female
            + quran_04
            + educ_03
            + log(syr_room_density)
            + log(leb_room_density)
            + faction_govt
            + minority)

eq_treat_raw <-
    formula(id_syrian
            ~ minority * fr_group)

eq_treat_pooled <-
    formula(id_syrian
            ~ minority * fr_group_pooled)

eq_treat_main <-
    formula(id_syrian
            ~ minority * fr_sect
            + minority * fr_dem
            + minority * fr_rel
            + minority * fr_for)

eq_full_raw <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_group)

eq_full_pooled <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_group_pooled)

eq_full_main <-
    update.formula(eq_base,
                   . ~ .
                   + minority * fr_sect
                   + minority * fr_dem
                   + minority * fr_rel
                   + minority * fr_for)

##### models

## NOTE: the vuong function spits up with OLS, so use the probit version
mods_robust_ols <- mods_robust <- list()

mods_names <-
    c("eq_treat_raw",
      "eq_treat_pooled",
      "eq_treat_main",
      "eq_full_raw",
      "eq_full_pooled",
      "eq_full_main")

## make sure to drop NAs from majority/minority
for (name in mods_names) {
    mods_robust[[name]] <-
        glm(eval(parse(text = name)),
            family = binomial(link = "probit"),
            subset = !is.na(minority),
            data   = ID)
    mods_robust_ols[[name]] <-
        glm(eval(parse(text = name)),
            family = gaussian,
            subset = !is.na(minority),
            data   = ID)
}

##### vuong tests

## NOTE: the vuong function doesn't return values,
##       but just prints them out
##       looks like I need to enter everything by hand
##       (grumble grumble)

## raw vs pooled (no covariates)
## Raw: m1 > m2 p = .093; z = 1.32
## AIC: m2 > m1 p = .040; z = -1.75
## BIC: m2 > m1 p = .001; z = -10.34
with(mods_robust,
     vuong(eq_treat_raw,
           eq_treat_pooled))

## raw vs main (no covariates)
## Raw: m1 > m2 p = .049; z = 1.65
## AIC: m2 > m1 p = .004; z = -2.64
## BIC: m2 > m1 p = .001; z = -14.65
with(mods_robust,
     vuong(eq_treat_raw,
           eq_treat_main))

## pooled vs main (no covariates)
## Raw: m1 > m2 p = .162; z = 0.99
## AIC: m2 > m1 p = .020; z = -2.06
## BIC: m2 > m1 p = .001; z = -10.57
with(mods_robust,
     vuong(eq_treat_pooled,
           eq_treat_main))

## raw vs pooled (with covariates)
## Raw: m1 > m2 p = .067; z = 1.50
## AIC: m2 > m1 p = .094; z = -1.32
## BIC: m2 > m1 p = .001; z = -9.21
with(mods_robust,
     vuong(eq_full_raw,
           eq_full_pooled))

## raw vs main (with covariates)
## Raw: m1 > m2 p = .038; z = 1.77
## AIC: m2 > m1 p = .009; z = -2.35
## BIC: m2 > m1 p = .001; z = -13.88
with(mods_robust,
     vuong(eq_full_raw,
           eq_full_main))

## pooled vs main (with covariates)
## Raw: m1 > m2 p = .173; z = 0.94
## AIC: m2 > m1 p = .012; z = -2.27
## BIC: m2 > m1 p = .001; z = -11.26
with(mods_robust,
     vuong(eq_full_pooled,
           eq_full_main))

## gather z statistics from output (no covariates)
vrawz_base <- c(1.32, 1.65, 0.99)
vaicz_base <- c(-1.75, -2.64, -2.06)
vbicz_base <- c(-10.34, -14.65, -10.57)

## gather z statistics from output (with covariates)
vrawz_full <- c(1.50, 1.77, 0.94)
vaicz_full <- c(-1.32, -2.35, -2.27)
vbicz_full <- c(-9.21, -13.88, -11.26)

## gather 1-tailed p values from output (no covariates)
vrawp_base <- c(.093, .049, .162)
vaicp_base <- c(.040, .004, .020)
vbicp_base <- c(.001, .001, .001)

## gather 1-tailed p values from output (with covariates)
vrawp_full <- c(.067, .038, .173)
vaicp_full <- c(.094, .009, .012)
vbicp_full <- c(.001, .001, .001)

##### put it all in matrices

## model labels
mod_names <-
    c("Raw",
      "Pooled",
      "Main")

## container matrices
vmatz_raw <- vmatz_aic <- vmatz_bic <- 
    matrix(data     = NA,
           nrow     = 3,
           ncol     = 3,
           dimnames = list(mod_names,
                           mod_names))

## raw data
vmatz_raw[c(which(lower.tri(vmatz_raw)),
            which(upper.tri(vmatz_raw)))] <-
    c(vrawz_base, vrawz_full)

## aic corrected
vmatz_aic[c(which(lower.tri(vmatz_aic)),
            which(upper.tri(vmatz_aic)))] <-
    c(vaicz_base, vaicz_full)

## bic corrected
vmatz_bic[c(which(lower.tri(vmatz_bic)),
            which(upper.tri(vmatz_bic)))] <-
    c(vbicz_base, vbicz_full)

## combine across columns
vmatz_all <-
    cbind(vmatz_raw,
          vmatz_aic,
          vmatz_bic)

## create a text version and get rid of the NAs in the diagonals
vmatz_text <-
    format(round(vmatz_all, 2), nsmall = 2)
vmatz_text[which(grepl("NA", vmatz_text))] <- ""

## add the stars for p < .05, p < .01
vmatz_text[which(abs(vmatz_all) > 1.96)] <-
    paste(format(round(vmatz_all[which(abs(vmatz_all) >= 1.96)], 2),
                 nsmall = 2),
          "{*}",
          sep = "^")
vmatz_text[which(abs(vmatz_all) > 2.58)] <-
    paste(format(round(vmatz_all[which(abs(vmatz_all) >= 2.58)], 2),
                 nsmall = 2),
          "{**}",
          sep = "^")

##### latex table

print(xtable(vmatz_text,
             align   = c("lccccccccc"),
             caption = "caption",
             label   = "tab:vuong"),
      type = "latex",
      sanitize.text.function = function(x){x})

## NOTE: you're going to have to do some of this manually
## example below:

## \begin{table}[htbp]
## \centering
## % \begin{tabular}{lccccccccc}
## \begin{tabular}{lD{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}D{.}{.}{3.3}}
##   \hline
##   & \multicolumn{3}{c}{Uncorrected}
##   & \multicolumn{3}{c}{AIC Corrected}
##   & \multicolumn{3}{c}{BIC Corrected}
##   \\
##   \cmidrule(lr){2-4}
##   \cmidrule(lr){5-7}
##   \cmidrule(lr){8-10}
##   & \multicolumn{1}{c}{Raw}
##   & \multicolumn{1}{c}{Pooled}
##   & \multicolumn{1}{c}{Main}
##   & \multicolumn{1}{c}{Raw}
##   & \multicolumn{1}{c}{Pooled}
##   & \multicolumn{1}{c}{Main}
##   & \multicolumn{1}{c}{Raw}
##   & \multicolumn{1}{c}{Pooled}
##   & \multicolumn{1}{c}{Main}
##   \\
##  % & Raw & Pooled & Main & Raw & Pooled & Main & Raw & Pooled & Main \\ 
##   \hline
## Raw &  &   1.50 &   1.77 &  &  -1.32 &  -2.35^{*} &  &  -9.21^{**} & -13.88^{**} \\ 
##   Pooled &   1.32 &  &   0.94 &  -1.75 &  &  -2.27^{*} & -10.34^{**} &  & -11.26^{**} \\ 
##   Main &   1.65 &   0.99 &  &  -2.64^{**} &  -2.06^{*} &  & -14.65^{**} & -10.57^{**} &  \\ 
##    \hline
##       \multicolumn{10}{l}{\scriptsize{$^{**}p<0.01$, $^*p<0.05$}}
## \end{tabular}
## \caption{Vuong test $z$ statistics to compare models using
##   nonnested treatment group operationalizations (lower triangles:
##   no covariates, upper triangles: with covariates).  $z > 0$
##   prefer the column operationalization, $z < 0$ prefer the row
##   operationalization.} 
## \label{tab:vuong}
## \end{table}

### appendix D (Balance Checks)
#### process the quantifier-pooled groups

## conditions pooling across "many" and "a few"
ID$fr_group_pooled <-
    with(ID,
         factor(ifelse(fr_group == "ctrl", "ctrl",
                ifelse(fr_group %in% c("sect-many", "sect-few"), "sect",
                ifelse(fr_group == "dem", "dem",
                ifelse(fr_group == "rel", "rel",
                ifelse(fr_group == "for", "for",
                ifelse(fr_group %in% c("sect-dem", "dem-sect"), "sect-dem",
                ifelse(fr_group %in% c("sect-rel", "rel-sect"), "sect-rel",
                ifelse(fr_group %in% c("sect-for", "for-sect"), "sect-for",
                       NA)))))))),
                levels = c("ctrl",
                           "sect",
                           "dem",
                           "rel",
                           "for",
                           "sect-dem",
                           "sect-rel",
                           "sect-for")))

#### gather the relevant IVs

## background covariates
IVs <-
    c("minority",
      "age",
      "female",
      "quran_04",
      "educ_03",
      "log(syr_room_density)",
      "log(leb_room_density)",
      "faction_govt",
      "know_bi",
      "interest_bi",
      "understand_bi",
      "engaged_bi")

#### loop through the IVs and different types of treatment groups

## equations
EQs <-
    c(null   = formula(~ 1),
      raw    = formula(~ fr_group),
      pooled = formula(~ fr_group_pooled),
      main   = formula(~ fr_sect + fr_dem + fr_rel + fr_for))

## set up containers
Mods <-
    list()
balance_iv <-
    matrix(0,
           nrow     = length(IVs),
           ncol     = 3,
           dimnames = list(IVs,
                           c("raw",
                             "pooled",
                             "main")))

## loop through all the covariates for all of the equations
for (iv in IVs) {
    for (eq in names(EQs)) {
        Mods[[eq]] <-
            glm(update.formula(EQs[[eq]], eval(parse(text = iv)) ~ .),
                data   = ID,
                family = if (with(ID,
                                  length(unique(eval(parse(text = iv)))) == 2)) {
                             binomial(link = "probit")
                         } else {
                             gaussian
                         })
    }
    for (eq in names(EQs[-1])) {
        balance_iv[iv, eq] <-
            lrtest(Mods[["null"]],
                   Mods[[eq]])["Pr(>Chisq)"][2,]
    }
}

#### tabulate the p-values

## gather values and summaries
balance_tab <-
    rbind(balance_iv,
          apply(balance_iv,
                2,
                quantile,
                probs = c(0, .25, .50, .75, 1)))

## label the rows
rownames(balance_tab) <-
    c("Minority",
      "Age",
      "Female",
      "Quran",
      "Education",
      "Logged Room Density (in Syria)",
      "Logged Room Density (in Lebanon)",
      "Government Supporter",
      "Knowledgeable about Politics",
      "Interested in Politics",
      "Understand Politics",
      "Engaged in Politics (Index)",
      "Minimum",
      "1st Quarter",
      "Median",
      "3rd Quarter",
      "Maximum")

## label the columns
colnames(balance_tab) <-
    c("Raw",                            # Raw Groups
      "Quantifiers",                    # Pooled Quantifiers
      "Frames")                         # Pooled Single/Dual Frames

## round to 2 decimals, replace 1 with .99
balance_tab <- round(balance_tab, 2)
balance_tab[which(balance_tab == 1)] <- .99

#### create the latex table

## create latex table
xtable(balance_tab,
       digits  = 2,
       align   = "lccc",
       label   = "tab:balance",
       caption = paste("Balance Checks,",
                       "Treatment Conditions",
                       "versus Background Covariates",
                       "($p$-values derived from likelihood ratio",
                       "tests against null model,",
                       "uncorrected for multiple comparisons)"))

#### multiple testing corrections

apply(balance_iv,
      2,
      p.adjust,
      method = "bonferroni")

apply(balance_iv,
      2,
      p.adjust,
      method = "fdr")

### appendix E (OLS and Probit Yield Analogous Inferences)

tex_mods <-
    list(mods_id_ols$eq_treatmin,
         mods_id_ols$eq_full,
         mods_id_probit$eq_treatmin,
         mods_id_probit$eq_full)

tex_cols <-
    rep(c("No Covariates",
          "With Covariates"), 2)

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Minority",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame",
      "Sectarian Frame $\\times$ Minority",
      "Democracy Frame $\\times$ Minority",
      "Secularism Frame $\\times$ Minority",
      "Foreigners Frame $\\times$ Minority",
      "Age",
      "Female",
      "Quran",
      "Education",
      "Logged Room Density (in Syria)",
      "Logged Room Density (in Lebanon)",
      "Government Supporter")

## reorder coefficients
tex_cord <- c(1:2, 3, 7, 4, 8, 5, 9, 6, 10, 11:17)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       single.row         = TRUE,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

### appendix F (Multiple Comparisons)
#### gather p-values

## gather pvals from main model (OLS)
pvals_ols <-
    pnorm(-abs(coef(mods_id_ols$eq_treatmin) /
               sqrt(diag(vcov(mods_id_ols$eq_treatmin))))) * 2 

## gather pvals from main model (probit)
pvals_probit <-
    pnorm(-abs(coef(mods_id_probit$eq_treatmin) /
               sqrt(diag(vcov(mods_id_probit$eq_treatmin))))) * 2 

## gather raw and corrected p-values
pvals <-
    cbind(raw = pvals_ols,
          fdr = p.adjust(pvals_ols, method = "fdr"),
          bon = p.adjust(pvals_ols, method = "bonferroni"))

#### prettify p-values

## turn them into text for prettifying
pvals_text <-
    format(round(pvals, 2), nsmall = 2)

## add stars and fix 1.00 and 0.00 rounding errors 
pvals_text[which(pvals < .05)] <-
    paste(format(round(pvals[which(pvals < .05)], 2),
                 nsmall = 2),
          "{*}",
          sep = "^")
pvals_text[which(pvals < .01)] <- "< 0.01^{**}"
pvals_text[which(pvals > .99)] <- "> 0.99"

#### prettify output

## use the coef names from the main table
rownames(pvals_text) <- 
    c("(Intercept)",
      "Minority",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame",
      "Sectarian Frame $\\times$ Minority",
      "Democracy Frame $\\times$ Minority",
      "Secularism Frame $\\times$ Minority",
      "Foreigners Frame $\\times$ Minority")

## get the row orders as in the main table
pvals_out <- pvals_text[c(1, 2, 3, 7, 4, 8, 5, 9, 6, 10),]

#### latex table

## stars
print(xtable(pvals_out,
             align   = c("lccc"),
             caption = "caption",
             label   = "tab:multiple"),
      type = "latex",
      sanitize.text.function = function(x){x})


## \begin{table}[htbp]
## \centering
## % \begin{tabular}{lccc}
## \begin{tabular}{l D{.}{.}{3.3} D{.}{.}{3.3} D{.}{.}{3.3}}
##   \hline
##   & \multicolumn{1}{c}{Uncorrected}
##   & \multicolumn{2}{c}{Corrected}
##   \\
##   \cmidrule(lr){3-4}
##   & & \multicolumn{1}{c}{False Discovery Rate} & \multicolumn{1}{c}{Bonferroni} \\
##  % & raw & fdr & bon \\ 
##   \hline
##   (Intercept) & < 0.01^{**} & < 0.01^{**} & < 0.01^{**} \\ 
##       \rowcolor{HI}
##   Minority & < 0.01^{**} & < 0.01^{**} & < 0.01^{**} \\ 
##       \rowcolor{HI}
##   Sectarian Frame & 0.86 & 0.87 & > 0.99 \\ 
##       \rowcolor{HI}
##   Sectarian Frame $\times$ Minority & < 0.01^{**} & < 0.01^{**} & 0.02^{*} \\ 
##   Democracy Frame & 0.85 & 0.87 & > 0.99 \\ 
##   Democracy Frame $\times$ Minority & 0.60 & 0.87 & > 0.99 \\ 
##   Secularism Frame & 0.11 & 0.27 & > 0.99 \\ 
##   Secularism Frame $\times$ Minority & 0.76 & 0.87 & > 0.99 \\ 
##   Foreigners Frame & 0.59 & 0.87 & > 0.99 \\ 
##   Foreigners Frame $\times$ Minority & 0.87 & 0.87 & > 0.99 \\ 
##   \hline
##   \multicolumn{4}{l}{\scriptsize{$^{**}p<0.01$, $^*p<0.05$}}
## \end{tabular}
## \caption{Robustness of Inferences to Corrections for Multiple
##   Comparisons (uncorrected $p$-values on coefficient estimates
##   from the main model and corrected $p$-values after controlling
##   the false discovery rate and using the Bonferroni adjustment)}
## \label{tab:multiple}
## \end{table}

### appendix G (Additional Specifications)
#### G.1 (Null Effects Among Sunni Arabs)
##### gather models

## storage container
mods_maj <- list()

mods_maj$govt <-
    glm(eq_treat,
        subset = !minority & faction_govt,
        data   = ID)

mods_maj$oppn <-
    glm(eq_treat,
        subset = !minority & !faction_govt & !faction_none,
        data   = ID)

mods_maj$fsa <-
    glm(eq_treat,
        subset = !minority & faction_fsa,
        data   = ID)

mods_maj$islamist <-
    glm(eq_treat,
        subset = !minority & faction_islamist,
        data   = ID)

mods_maj$islamist_d <-
    glm(eq_treat,
        subset = !minority & faction_islamist_domestic,
        data   = ID)

mods_maj$islamist_f <-
    glm(eq_treat,
        subset = !minority & faction_islamist_foreign,
        data   = ID)

##### table

tex_mods <- mods_maj

tex_cols <-
    c("Government",
      "Pooled",                         # Pooled Opposition
      "FSA",
      "All",                            # Pooled Islamists
      "Domestic",                       # Domestic Islamists
      "Foreign")                        # Foreign Islamists

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame")

## reorder coefficients
tex_cord <- c(1:5)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

#### G.2 (Similar Effects Among Different Minorities)
##### gather models

## storage container
mods_min <- list()

mods_min$pooled <-
    glm(eq_treat,
        subset = sect_3cat %in% c("Kurd", "Heterodox"),
        data   = ID)

mods_min$pooled_noalawi <-
    glm(eq_treat,
        subset = sect_3cat %in% c("Kurd", "Heterodox") & !alawi,
        data   = ID)

mods_min$nonsunni <-
    glm(eq_treat,
        subset = sect_3cat == "Heterodox",
        data   = ID)

mods_min$nonsunni_noalawi <-
    glm(eq_treat,
        subset = sect_3cat == "Heterodox" & !alawi,
        data   = ID)

mods_min$kurd <-
    glm(eq_treat,
        subset = sect_3cat == "Kurd",
        data   = ID)

mods_min$min_inter <- 
    glm(update.formula(eq_treat,
                       . ~ kurdish_speak * .),
        subset = sect_3cat %in% c("Kurd", "Heterodox"),
        data   = ID)

mods_min$all_inter <- 
    glm(update.formula(eq_treat,
                       . ~ sect_3cat * .),
        data   = ID)

##### table

## don't use the interactive models
tex_mods <-
    mods_min[names(mods_min) %in% c("pooled",
                                    "pooled_noalawi",
                                    "nonsunni",
                                    "nonsunni_noalawi",
                                    "kurd")]

tex_cols <-
    c("Pooled",
      "Pooled, No Alawis",
      "Non-Sunni Arabs",
      "Non-Sunni, No Alawis",
      "Kurds")

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame")

## reorder coefficients
tex_cord <- c(1:5)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

##### interactive

mod <-
    glm(eq_treat,
        subset = sect_3cat == "Heterodox" & !alawi,
        data = ID)
summary(mod)

## coef of interest: kurd * sect
## p = .64
summary(mods_min$min_inter)

## coefs of interest: kurd * sect and heterodox * sect
## b: .19 and .14
summary(mods_min$all_inter)
## but diff between the two is p = .65
with(mods_min,
     linearHypothesis(all_inter,
                      paste("sect_3catKurd:fr_sectTRUE",
                            "sect_3catHeterodox:fr_sectTRUE",
                            sep = " = ")))

##### minorities and factional affiliations

## NB: NA refers to Kurdish groups
with(filter(ID, sect_3cat %in% c("Kurd", "Heterodox")),
     cbind(table(faction_all, useNA = "always"),
           prop.table(table(faction_all, useNA = "always"))))

mods_min_faction <- list()

mods_min_faction$pooled <- 
    glm(eq_treat,
        subset = sect_3cat %in% c("Kurd", "Heterodox"),
        data   = ID)

mods_min_faction$govt <- 
    glm(eq_treat,
        subset = (sect_3cat %in% c("Kurd", "Heterodox") &
                  faction_govt),
        data   = ID)

mods_min_faction$govtnokurd <- 
    glm(eq_treat,
        subset = (sect_3cat %in% c("Kurd", "Heterodox") &
                  faction_govt &
                  !kurdish_speak),
        data   = ID)

mods_min_faction$nongovt <- 
    glm(eq_treat,
        subset = (sect_3cat %in% c("Kurd", "Heterodox") &
                  !faction_govt),
        data   = ID)

## get relevant models
## drop the no Kurds model, which tells the same story,
## but is covered elsewhere in the appendices
tex_mods <- mods_min_faction
tex_mods[["govtnokurd"]] <- NULL

tex_cols <-
    c("Pooled",
      "Supporter",
      "Non-Supporter")

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame")

## reorder coefficients
tex_cord <- c(1:5)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

#### G.3 (Treatment Effects Do Not Vary by Political Engagement)
##### engagement index

## Cronbach's alpha = .80
with(ID,
     cronbach(cbind(interest_bi,
                    understand_bi,
                    know_bi)))

## sample proportions on diag, tetrachoric correlations in lower triangle
engaged_mat <-
    with(ID,
         tetrachoric(cbind(know_bi,
                           interest_bi,
                           understand_bi))$rho)
diag(engaged_mat) <-
    apply(dplyr::select(ID,
                        know_bi,
                        interest_bi,
                        understand_bi),
          2,
          mean)
engaged_mat[upper.tri(engaged_mat)] <- NA

## sample proportions on diag,
## tetrachoric correlations in lower triangle
engaged_mat

## sample proportion of index
with(ID, mean(engaged_bi, na.rm = TRUE))

##### engagement models

mods_engaged_minority <- mods_engaged_majority <- list()

IVs <-
    c("know_bi",
      "interest_bi",
      "understand_bi",
      "engaged_bi")

## make sure to drop NAs from majority/minority
for (iv in IVs) {
    mods_engaged_minority[[iv]] <-
        glm(update.formula(eq_treat,
                           . ~ eval(parse(text = iv)) * .),
            family = gaussian,
            subset = minority,
            data   = ID)
    mods_engaged_majority[[iv]] <-
        glm(update.formula(eq_treat,
                           . ~ eval(parse(text = iv)) * .),
            family = gaussian,
            subset = !minority,
            data   = ID)
}

##### minorities table

tex_mods <-
    mods_engaged_minority

tex_cols <-
    c("Knowledge",
      "Interest",
      "Understanding",
      "Index")

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Engaged",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame",
      "Sectarian Frame $\\times$ Engaged",
      "Democracy Frame $\\times$ Engaged",
      "Secularism Frame $\\times$ Engaged",
      "Foreigners Frame $\\times$ Engaged")

## reorder coefficients
tex_cord <- c(1:2, 3, 7, 4, 8, 5, 9, 6, 10)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

##### majorities table

tex_mods <-
    mods_engaged_majority

tex_cols <-
    c("Knowledge",
      "Interest",
      "Understanding",
      "Index")

## variable names (original order)
tex_vars <-
    c("(Intercept)",
      "Engaged",
      "Sectarian Frame",
      "Democracy Frame",
      "Secularism Frame",
      "Foreigners Frame",
      "Sectarian Frame $\\times$ Engaged",
      "Democracy Frame $\\times$ Engaged",
      "Secularism Frame $\\times$ Engaged",
      "Foreigners Frame $\\times$ Engaged")

## reorder coefficients
tex_cord <- c(1:2, 3, 7, 4, 8, 5, 9, 6, 10)

## texreg it to screen
texreg(l                  = tex_mods,
       custom.coef.names  = tex_vars,
       custom.model.names = tex_cols,
       reorder.coef       = tex_cord,
       leading.zero       = TRUE,
       booktabs           = TRUE,
       dcolumn            = TRUE,
       stars              = c(.01, .05))

### (Emacs local variables)

## If you're using Emacs, allow it to evaluate these forms so
## that you can use outline mode for code folding.

## Local Variables:
## eval: (outline-minor-mode 1)
## eval: (font-lock-mode 1)
## outline-regexp: "^\###+ "
## outline-level: (lambda ()
##                  (cond
##                   ((looking-at "^### ")     1)
##                   ((looking-at "^#### ")    2)
##                   ((looking-at "^##### ")   3)
##                   ((looking-at "^###### ")  4)
##                   ((looking-at "^####### ") 5)
##                   (t 1000)))
## eval: (font-lock-add-keywords
##        nil
##        '(("^### .*" 0 (aref outline-font-lock-faces 0) t)
##          ("^#### .*" 0 (aref outline-font-lock-faces 1) t)
##          ("^##### .*" 0 (aref outline-font-lock-faces 2) t)
##          ("^###### .*" 0 (aref outline-font-lock-faces 3) t)
##          ("^####### .*" 0 (aref outline-font-lock-faces 4) t)))
## eval: (hide-sublevels 1)
## End:
