################################################################################
# Replication: Who Leaves and Who Enters? Electoral Bans and Political Corruption
################################################################################

# Loading packages -------------------------------------------------------------
if(require(tidyverse) == F) {install.packages("tidyverse"); require(tidyverse)}
if(require(ggplot2) == F) {install.packages("ggplot2"); require(ggplot2)}
if(require(scales) == F) {install.packages("scales"); require(scales)}
if(require(here) == F) {install.packages("here"); require(here)}
if(require(lubridate) == F) {install.packages("lubridate"); require(lubridate)}
if(require(modelsummary) == F) {install.packages("modelsummary"); require(modelsummary)}
if(require(gt) == F) {install.packages("gt"); require(gt)}
if(require(did) == F) {install.packages("did"); require(did)}
if(require(ggeffects) == F) {install.packages("ggeffects"); require(ggeffects)}
if(require(scales) == F) {install.packages("scales"); require(scales)}
if(require(patchwork) == F) {install.packages("patchwork"); require(patchwork)}
if(require(HonestDiD) == F) {install.packages("HonestDiD"); require(HonestDiD)}

################################################################################
##### Data

### Loading data ---------------------------------------------------------------
setwd("~") # Set the working directory to the folder containing 'corr.ban.v2.RData'
load("corr.ban.v2.RData")

### Set output directory --------------------------------------------------------
outfolder <- '~/output' # Directory for saving outputs (figures/tables)

################################################################################
##### Descriptive analysis
# Bans by election year --------------------------------------------------------
# Aggregate bans at the municipal level by election year; keep post-CRL years (>= 2012).
# Convert year to character so ggplot treats the x-axis as discrete categories.
tab.bans <- corr.mun %>% 
  group_by(elec.year) %>% 
  summarise(t1.ban.gov = sum(t1.ban.gov)) %>% 
  filter(elec.year >= 2012) %>% 
  mutate(elec.year = as.character(elec.year)) %>% 
  rename(Bans = t1.ban.gov)

# Bar chart of bans by election year; labels above bars and minimalist axes.
tab.bans %>% 
  ggplot(., aes(x = elec.year, y = Bans)) +
  geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
  ylab("Number of bans") + xlab("Political Term of Banned Mayor") + 
  geom_text(aes(label = tab.bans$Bans), 
            position = position_dodge(.9), 
            size = 3.3, vjust = -0.4, colour = "black") +
  expand_limits(y = c(0, 550)) + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()) 

# Save figures (PNG and EPS). EPS uses Cairo to embed fonts for publication quality.
ggsave(file.path(outfolder, 'desc.bans.png'), width = 5, height = 3)
ggsave(file.path(outfolder, 'desc.bans.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

### Summary statistics of municipal data ---------------------------------------

# Preparing the appropriate data:
# - Flag treated units at the municipality level (id) if they were ever treated across periods.
# - Add a post-treatment indicator and select outcomes/covariates for the table.
desc.sum <- corr.mun %>% 
  group_by(id) %>% 
  mutate_at(vars('t1.ban.gov'), ~ max(.)) %>% ungroup() %>%
  mutate(post = ifelse(elec.year >= 2012, 1, 0)) %>%  # Post-treatment period: 1; otherwise 0
  # Select treatment and time indicators, outcomes, and covariates:
  select('post', 't1.ban.gov', 
         "fut.ban.t1", 
         'is.mayor.fresh', 'is.counc.fresh',
         "mayor.vote.frag", "nep", 
         "incumb.gender", "incumb.college", "incumb.local") 

# Variable labels:
new_names <- c("Post Treatment Period", "Banned Mayors", 'Corruption Charges', 
               "Outsider Mayors", "Outsider Councilors", 
               'Mayoral Vote Fragmentation', 
               'Number of Effective Parties', 
               'Female Incumbents', 'Incumbents with College Degree', 'Local Incubents')

# Generate final table:
desc.sum %>% 
  # Rename variables with the new labels:
  rename_with(~set_names(new_names)) %>% 
  # Compute summary statistics with the modelsummary package:
  datasummary_skim(., fmt = 3, output = 'gt', histogram = F) %>% 
  # Title and footnote:
  tab_header(title = md("Summary Statistics of Municipal Variables")) %>% 
  tab_footnote(footnote = "Notes: For each variable, the table displays the mean, 
  standard deviation, minimum, median, and maximum. 
               Each observation corresponds to a specific municipality.") %>% 
  # Improve table appearance:
  tab_options(heading.title.font.size=16) %>% 
  opt_vertical_padding(scale = 0) %>% 
  cols_hide(columns = c(2,3)) %>% 
  # Save to the output folder:
  gtsave(filename = file.path(outfolder, "tab.summary.mun.tex"))


### Summary statistics of party data -------------------------------------------

# Preparing the appropriate data:
# - Flag treated units at the party–municipality level (id) if ever treated across periods.
# - Add a post-treatment indicator and select outcomes/covariates for the table.
desc.sum <- corr.ban %>% 
  group_by(id) %>% 
  mutate_at(vars('t1.ban.gov'),
            ~ max(.)) %>% ungroup() %>% 
  mutate(post=ifelse(elec.year>=2012, 1, 0)) %>%  # Post-treatment indicator: 1; otherwise 0
  # Select treatment/time indicators, outcomes, and covariates:
  select('post', 't1.ban.gov',
         "mayor.elec", "counc.elec", 
         "incumb.gender", "incumb.college", "incumb.local") 

# Variable labels:
new_names <- c("Post Treatment Period", "Banned Mayors", 
               'Elected Mayors', "Elected City Councilors", 
               'Female Incumbents', 'Incumbents with College Degree', 
               'Local Incubents')

# Generate final table:
desc.sum %>% 
  # Rename variables with the new labels:
  rename_with(~set_names(new_names)) %>% 
  # Compute summary statistics with the modelsummary package:
  datasummary_skim(., fmt = 3, output = 'gt', histogram = F) %>% 
  # Title and footnote:
  tab_header(title = md("Summary Statistics of Party Variables")) %>% 
  tab_footnote(footnote = "Notes: For each variable, the table displays the mean, 
  standard deviation, minimum, median, and maximum. 
  Each observations represents a political party within a specific municipality.") %>% 
  # Improve the appearance of the table:
  tab_options(heading.title.font.size=16) %>% 
  opt_vertical_padding(scale = 0) %>% 
  cols_hide(columns = c(2,3)) %>% 
  # Save to the output folder:
  gtsave(filename = file.path(outfolder, "tab.summary.part.tex"))

################################################################################
##### Tests 
# DID helper function ----------------------------------------------------------
get.did.mod <- 
  function(data, treatment, outvars, covars = c(), b_period = "varying", 
           control = "nevertreated"){
    
    if(length(covars) == 0){ covar.form <- as.formula("~ 1") }
    if(length(covars) > 0){  covar.form <- paste(covars, collapse = "+") }
    if(length(covars) > 0){  covar.form <- paste0("~", covar.form) %>% as.formula() }
    
    did.list <- list()
    for(i in 1:length(outvars)){
      
      data$treatment <- data[, treatment] %>% unlist()
      data$outcome   <- data[, outvars[i]] %>% unlist()
      
      # Convert a binary treatment to treatment timing (first treated election year):
      # - If treated in a given period, set 'treatment' to that period's election year; otherwise use a sentinel.
      # - Collapse to the earliest treated year within id (via max over a 0/sentinel recode).
      # - Replace the sentinel with 0 to encode 'never treated'.
      data <- data %>%
        mutate(treatment = ifelse(treatment == 1, elec.year, 1000)) %>% 
        group_by(id) %>% 
        mutate(treatment = max(treatment)) %>% ungroup() %>% 
        mutate(treatment = ifelse(treatment == 1000, 0, treatment)) 
      
      # Estimate ATT(g,t) using did::att_gt with the specified base period and control group.
      # 'xformla' supplies the (optional) covariates for doubly robust estimation.
      did.list[[i]] <- data %>%
        att_gt(yname = "outcome", gname = "treatment", idname = "id", tname = "elec.year", 
               xformla = covar.form, data = ., base_period = b_period, control_group = control) 
      names(did.list)[[i]] <- outvars[i]
    }
    
    return(did.list)
  }

# Main results -----------------------------------------------------------------

# With covariates (doubly robust specification) --------------------------------
covars = c("pib.def.log", "incumb.gender", "incumb.college", "incumb.local")

mod.corr.1.cov <- get.did.mod(data=corr.mun, treatment = 't1.ban.gov',
                              outvars = c('mayor.improb', "counc.improb",
                                          'is.mayor.fresh', 'is.counc.fresh'),
                              covars=covars)

mod.reel.1.cov <- get.did.mod(data=corr.ban, treatment = 't1.ban.gov',
                              outvars = c('mayor.elec', "counc.elec"),
                              covars=covars)

# Quick inspection of effects:
# - 'dynamic' = event-study path; 'simple' = average treatment effect across post periods.
aggte(mod.corr.1.cov$mayor.improb, type = "dynamic") %>% ggdid()
aggte(mod.corr.1.cov$mayor.improb, type = "simple") 
aggte(mod.corr.1.cov$counc.improb, type = "dynamic") %>% ggdid()
aggte(mod.corr.1.cov$counc.improb, type = "simple") 
aggte(mod.corr.1.cov$is.mayor.fresh, type = "dynamic") %>% ggdid()
aggte(mod.corr.1.cov$is.mayor.fresh, type = "simple") 
aggte(mod.corr.1.cov$is.counc.fresh, type = "dynamic") %>% ggdid()
aggte(mod.corr.1.cov$is.counc.fresh, type = "simple") 

aggte(mod.reel.1.cov$mayor.elec, type = "dynamic") %>% ggdid()
aggte(mod.reel.1.cov$mayor.elec, type = "simple") 
aggte(mod.reel.1.cov$counc.elec, type = "dynamic") %>% ggdid()
aggte(mod.reel.1.cov$counc.elec, type = "simple") 


# Without covariates (baseline specification) ----------------------------------

mod.corr.1 <- get.did.mod(data = corr.mun, treatment = 't1.ban.gov', 
                          outvars = c('mayor.improb', "counc.improb",
                                      'is.mayor.fresh', 'is.counc.fresh'))

mod.reel.1 <- get.did.mod(data = corr.ban, treatment = 't1.ban.gov', 
                          outvars = c('mayor.elec', "counc.elec"))

# Quick view of results:
# - 'dynamic' = event-study path across leads/lags; 'simple' = average post-treatment effect.
aggte(mod.corr.1$mayor.improb, type = "dynamic") %>% ggdid()
aggte(mod.corr.1$mayor.improb, type = "simple") 
aggte(mod.corr.1$counc.improb, type = "dynamic") %>% ggdid()
aggte(mod.corr.1$counc.improb, type = "simple") 
aggte(mod.corr.1$is.mayor.fresh, type = "dynamic") %>% ggdid()
aggte(mod.corr.1$is.mayor.fresh, type = "simple") 
aggte(mod.corr.1$is.counc.fresh, type = "dynamic") %>% ggdid()
aggte(mod.corr.1$is.counc.fresh, type = "simple") 

aggte(mod.reel.1$mayor.elec, type = "dynamic") %>% ggdid()
aggte(mod.reel.1$mayor.elec, type = "simple") 
aggte(mod.reel.1$counc.elec, type = "dynamic") %>% ggdid()
aggte(mod.reel.1$counc.elec, type = "simple") 

################################################################################
### Tables 
# Function to build results table ----------------------------------------------
# `get.did.tab` extracts the main results from a list of DID models
get.did.tab <- function(mod.list){
  # `mod.list` is a list of did::att_gt objects
  
  for(i in 1:length(mod.list)){
    # Getting aggregated results:
    x <- mod.list[[i]] %>% aggte(type = 'simple')
    # Option "simple" computes a weighted average of all group–time ATTs,
    # with weights proportional to group size.
    
    out.tab <- data.frame(ATT = x$overall.att,
                          Std.Error = x$overall.se,
                          conf.low  = x$overall.att - (1.96 * x$overall.se),
                          conf.high = x$overall.att + (1.96 * x$overall.se),
                          N         = x$DIDparams$n,
                          conf.low.01  = x$overall.att - (2.576 * x$overall.se),
                          conf.high.01 = x$overall.att + (2.576 * x$overall.se),
                          conf.low.001  = x$overall.att - (3.291 * x$overall.se),
                          conf.high.001 = x$overall.att + (3.291 * x$overall.se)
    )
    # Note: Confidence intervals are computed manually as ATT ± z * SE,
    # because values from `broom::tidy()` do not match the CIs from `aggte()`
    # (did package). To verify, compare:
    # aggte(mod.gen.cand, type = 'simple')
    # tidy(mod.gen.cand)
    
    out.tab <- out.tab %>% 
      # Significance stars based on two-sided CIs not crossing zero:
      # *   : 95% CI excludes 0
      # **  : 99% CI excludes 0
      # *** : 99.9% CI excludes 0
      mutate(sig = ifelse(ATT > 0 & conf.low > 0 & conf.high > 0, "*", ""),
             sig = ifelse(ATT < 0 & conf.low < 0 & conf.high < 0, "*", sig),
             sig = ifelse(ATT > 0 & conf.low.01 > 0 & conf.high.01 > 0, "**", sig),
             sig = ifelse(ATT < 0 & conf.low.01 < 0 & conf.high.01 < 0, "**", sig),
             sig = ifelse(ATT > 0 & conf.low.001 > 0 & conf.high.001 > 0, "***", sig),
             sig = ifelse(ATT < 0 & conf.low.001 < 0 & conf.high.001 < 0, "***", sig))  %>% 
      # Rounding and formatting:
      mutate(ATT = round(ATT, 3), 
             ATT = paste0(ATT, sig),
             Std.Error = round(Std.Error, 3), 
             Std.Error = paste0("(", Std.Error, ")")) %>% 
      # Outcome variable labels:
      mutate(outvar = names(mod.list)[[i]]) %>% 
      select(outvar, ATT, Std.Error, N)
    
    if(i == 1){ did.out.tab <- out.tab } else { did.out.tab <- rbind(did.out.tab, out.tab) }
  }
  
  return(did.out.tab)
}

################################################################################
### Tables 
### Future corruption (table) --------------------------------------------------
list.corr <-list(mod.corr.1$mayor.improb)
names(list.corr) <- c("")

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble()   # Transpose and convert to tibble for shaping

# Selecting outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0 = c('Electoral ban', "", "N")) %>%  # Set display labels for rows (coef, SE line, sample size)
  select(V0, V1)

# Replace column labels
names(tab) <- c("X", "Corruption")

# Build LaTeX table with gt
tab <- tab %>% 
  as.tbl() %>%  # Note: as.tbl() is deprecated; consider as_tibble() in future revisions
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = 'The Effect of Banning Corrupt Mayors on Corruption in the Next Tenure') %>% 
  # Notes:
  tab_footnote(footnote = "This table presents estimates of the average effect of 
  banning incumbent mayors on the likelihood of corruption charges filed by 
  prosecutors in the next political term. 
  The second column represents corruption charges in the subsequent government. 
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0) 

tab %>% gtsave(filename = file.path(outfolder, "tab.corr.term1.tex"))

# Preview the gt table
tab

### Party Success --------------------------------------------------------------
list.corr <- list(mod.reel.1$mayor.elec, mod.reel.1$counc.elec)
names(list.corr) <- c("Elected Mayors", "Elected Councilors")

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble() 

# Select outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0 = c('Electoral ban', "", "N")) %>%  # Set display labels for rows (coef, SE line, sample size)
  select(V0, V1, V2)

# Replace outcome labels
names(tab) <- c("X", "Elected Mayors", "Elected Councilors")

# Build LaTeX table
tab <- tab %>% 
  as.tbl() %>% 
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = "The Effect of Electoral Bans on the Performance of the Banned Mayor's Party") %>% 
  # Notes:
  tab_footnote(footnote = "This table presents estimates of the average effect of banning incumbent 
  mayors on their party's electoral performance. 
  The columns represent the probability of the banned mayor's party electing 
  the next mayor and the proportion of elected councilors, respectively. 
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0)

tab %>% gtsave(filename = file.path(outfolder, "tab.perf.term1.tex"))

# Preview the table
tab

### Outsiders ------------------------------------------------------------------
list.corr <-list(mod.corr.1$is.mayor.fresh, mod.corr.1$is.counc.fresh)
names(list.corr) <- c("Ousider Elected Mayors", "Ousider Elected Councilors")  # Note: "Ousider" appears to be a typo; consider "Outsider"

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble() 

# Select outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0 = c('Electoral ban', "", "N")) %>%  # Set display labels for rows (coef, SE line, sample size)
  select(V0, V1, V2)

# Replace outcome labels
names(tab) <- c("X", "Ousider Elected Mayors", "Ousider Elected Councilors")  # Keep labels as defined above

# Build LaTeX table
tab <- tab %>% 
  as.tbl() %>% 
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = "The Effect of Electoral Bans on the Electoral Success of Outsiders") %>% 
  # Notes:
  tab_footnote(footnote = "This table presents estimates of the average effect 
  of banning incumbent mayors on the probability of electing outsiders. 
  The columns represent the probability of electing outsider mayors and the 
  proportion of outsider councilors elected, respectively.
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0) 

tab %>% gtsave(filename = file.path(outfolder, "tab.outsiders.1.tex"))

# Preview the table
tab


################################################################################
### Tables with Covariates 
### Future corruption ----------------------------------------------------------
list.corr <-list(mod.corr.1.cov$mayor.improb)
names(list.corr) <- c("")

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble() 

# Select outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0=c('Electoral ban', "", "N")) %>% # Set row labels (coefficient, SE line, sample size)
  select(V0, V1)

# Replace column labels
names(tab) <- c("X", "Corruption Charges")

# Add covariate disclosure rows (DR specification uses time-varying covariates)
adlines <- data.frame(a=c("GDP (log)", 
                          "Incumbent's gender", 
                          "Incumbent's level of education",
                          "Incumbent locally born"),
                      b=c("Yes", "Yes", "Yes", "Yes"))
names(adlines) <- names(tab)
tab <- rbind(tab, adlines) %>% data.frame() 

# Build LaTeX table
tab <- tab %>% 
  as.tbl() %>% 
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = 'The Effect of Banning Corrupt Mayors on the Likelihood of Account Disapproval for Subsequent Governments with Time-Variant Covariates') %>% 
  # Notes (method and interpretation)
  tab_footnote(footnote = "This table presents estimates of the average effect of 
  banning incumbent mayors on the likelihood of corruption charges filed by 
  prosecutors in the next political term with time-variant covariates. 
  The tests use a doubly robust estimation approach. 
  The covariates include, for each electoral period, the log of municial GDP, as well as 
  the incumbents' party affiliation, gender, level of education, and whether 
  their birthplace is within the same municipality. 
  
  The second column represents corruption charges in the subsequent government. 
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0) 

tab %>% gtsave(filename = file.path(outfolder, "tab.corr.term1.cov.tex"))

# Preview the table
tab

### Party Success --------------------------------------------------------------
list.corr <-list(mod.reel.1.cov$mayor.elec, mod.reel.1.cov$counc.elec)
names(list.corr) <- c("Elected Mayors", "Elected Councilors")

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble() 

# Select outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0=c('Electoral ban', "", "N")) %>% # Set row labels (coefficient, SE line, sample size)
  select(V0, V1, V2)

# Replace column labels
names(tab) <- c("X", "Elected Mayors", "Elected Councilors")

# Add covariate disclosure rows
adlines <- data.frame(a=c("GDP (log)", 
                          "Incumbent's gender", 
                          "Incumbent's level of education",
                          "Incumbent locally born"),
                      b=c("Yes", "Yes", "Yes", "Yes"),
                      c=c("Yes", "Yes", "Yes", "Yes"))
names(adlines) <- names(tab)
tab <- rbind(tab, adlines) %>% data.frame() 

# Build LaTeX table
tab <- tab %>% 
  as.tbl() %>% 
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = "The Effect of Electoral Bans on the Performance of the Banned Mayor's Party with Time-Variant Covariates") %>% 
  # Notes (method and interpretation)
  tab_footnote(footnote = "This table presents estimates of the average effect of banning incumbent 
  mayors on their party's electoral performance with time-variant covariates. 
  The tests use a doubly robust estimation approach. 
  The covariates include, for each electoral period, the log of municial GDP, as well as 
  the incumbents' party affiliation, gender, level of education, and whether 
  their birthplace is within the same municipality. 
  The columns represent the probability of the banned mayor's party electing 
  the next mayor and the proportion of elected councilors, respectively. 
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0) 

tab %>% gtsave(filename = file.path(outfolder, "tab.perf.term1.cov.tex"))

# Preview the table
tab

### Outsiders ------------------------------------------------------------------
list.corr <-list(mod.corr.1.cov$is.mayor.fresh, mod.corr.1.cov$is.counc.fresh)
names(list.corr) <- c("Ousider Elected Mayors", "Ousider Elected Councilors") # Note: "Ousider" seems to be a typo; consider "Outsider"

tab <- get.did.tab(list.corr)
tab <- tab %>% t() %>% as_tibble() 

# Select outcomes of interest
tab <- tab[-1, ] %>% 
  mutate(V0=c('Electoral ban', "", "N")) %>% # Set row labels (coefficient, SE line, sample size)
  select(V0, V1, V2)

# Replace column labels
names(tab) <- c("X", "Ousider Elected Mayors", "Ousider Elected Councilors") # Kept as in code; adjust strings if needed in outputs

# Build LaTeX table
tab <- tab %>% 
  as.tbl() %>% 
  rename_with(~ str_replace_all(., "\\.|X", " ")) %>% 
  gt() %>%  
  tab_header(title = "The Effect of Electoral Bans on the Electoral Success of Outsiders  with Time-Variant Covariates") %>% 
  # Notes (method and interpretation)
  tab_footnote(footnote = "This table presents estimates of the average effect 
  of banning incumbent mayors on the probability of electing outsiders with time-variant covariates. 
  The tests use a doubly robust estimation approach. 
  The covariates include, for each electoral period, the log of municial GDP, as well as 
  the incumbents' party affiliation, gender, level of education, and whether 
  their birthplace is within the same municipality. 
  The columns represent the probability of electing outsider mayors and the 
  proportion of outsider councilors elected, respectively.
  The coefficients represent a weighted average of all group-time average 
  treatment effects, with weights proportional to group size. 
  These coefficients were estimated using the did R package by Callaway and 
  Sant’Anna (2021). 
  Standard errors are reported in parentheses. 
  At the bottom of the table, the number of observations is also provided. 
  Significance levels are denoted as follows: * p < 0.05, ** p < 0.01, *** p < 0.001.") %>% 
  tab_options(heading.title.font.size = 16) %>% 
  opt_vertical_padding(scale = 0) 

tab %>% gtsave(filename = file.path(outfolder, "tab.outsiders.1.cov.tex"))

# Preview the table
tab
################################################################################
### Graphs 

# Graph function ---------------------------------------------------------------
# Helper to build event-study plots (ATT by relative time to treatment)
# - 'model' is a did::att_gt object
# - 'ylabel' sets the y-axis label
# - 'treat.title' sets the plot title
get.did.graph <- function(model, ylabel, treat.title){
  
  model <- model %>% 
    # Compute event-study effects (dynamic ATT over exposure length)
    aggte(type = 'dynamic') %>% 
    tidy() 
  
  did.graph <- model %>% 
    ggplot(aes(x = event.time, y = estimate)) +
    # Color post-treatment periods differently
    geom_point(aes(color = as.numeric(event.time) >= 0), size = 2) +  
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high,
                      color = as.numeric(event.time) >= 0),
                  width = 0.2) + 
    scale_color_manual(values = c("TRUE" = "brown4", "FALSE" = "gray30"),
                       labels = c("Pre", "Post")) + 
    theme_minimal() +
    labs(x = "Years between elections", y = ylabel) +
    ggtitle(treat.title) +
    scale_x_continuous(breaks = seq(-12, 8, 4)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
    # Add percentage labels above points
    geom_text(aes(label = paste0(round(model$estimate * 100, 1), "%")), 
              position = position_dodge(1.4), 
              size = 3.3, vjust = -0.2, hjust = -0.05, colour = "black") +
    theme(axis.title = element_text(face = 'plain', colour = "black", size = 10),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major.y = element_blank(),
          legend.position = 'bottom',
          legend.title = element_blank())
  
  # Edge case: if only one event time is present, show a single "Post" label
  if(length(model$estimate) == 1){
    did.graph <- did.graph +
      scale_color_manual(values = c("TRUE" = "brown4", "FALSE" = "gray30"),
                         labels = c("Post"))
  }
  
  return(did.graph)
}


# Graphs -----------------------------------------------------------------------

# Corruption charges (no covariates)
g1.fut.ban.t1 <- get.did.graph(mod.corr.1$mayor.improb, "Corruption Charges", "")
g1.fut.ban.t1
ggsave(file.path(outfolder, 'g1.corr.png'), width = 5.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.corr.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

# Party performance (no covariates)
g1.mayor.elec <- get.did.graph(mod.reel.1$mayor.elec, "Elected Mayor", "")
g1.counc.elec <- get.did.graph(mod.reel.1$counc.elec, "Elected Councilors", "")
g1.mayor.elec + g1.counc.elec
ggsave(file.path(outfolder, 'g1.reel.png'), width = 7.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.reel.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

# Outsiders (no covariates)
g1.mayor.fresh <- get.did.graph(mod.corr.1$is.mayor.fresh, "mayor.fresh", "")
g1.counc.fresh <- get.did.graph(mod.corr.1$is.counc.fresh, "counc.fresh", "")
g1.mayor.fresh + g1.counc.fresh
ggsave(file.path(outfolder, 'g1.fresh.png'), width = 7.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.fresh.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)


# With covariates
g1.fut.ban.t1.cov <- get.did.graph(mod.corr.1.cov$mayor.improb, "Corruption Charges", "")
g1.fut.ban.t1.cov
ggsave(file.path(outfolder, 'g1.corr.cov.png'), width = 5.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.corr.cov.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

g1.mayor.elec.cov <- get.did.graph(mod.reel.1.cov$mayor.elec, "Elected Mayor", "")
g1.counc.elec.cov <- get.did.graph(mod.reel.1.cov$counc.elec, "Elected Councilors", "")
g1.mayor.elec.cov + g1.counc.elec.cov
ggsave(file.path(outfolder, 'g1.reel.cov.png'), width = 7.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.reel.cov.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

g1.mayor.fresh.cov <- get.did.graph(mod.corr.1.cov$is.mayor.fresh, "mayor.fresh", "")
g1.counc.fresh.cov <- get.did.graph(mod.corr.1.cov$is.counc.fresh, "counc.fresh", "")
g1.mayor.fresh.cov + g1.counc.fresh.cov
ggsave(file.path(outfolder, 'g1.fresh.cov.png'), width = 7.5, height = 3.5)
ggsave(file.path(outfolder, 'g1.fresh.cov.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)

################################################################################
# Sensitivity analyses -------------------------------------------------------------------

for(i in seq(0, 1.2, .05)){
  print(i)
  
  tempmod <- corr.mun %>%
    ungroup() %>% 
    mutate(ban.year = ifelse(t1.ban.gov == 1, elec.year, 100000)) %>% 
    # Find the first treatment year per municipality (ban.year):
    group_by(mun.cod) %>% 
    mutate(ban.year = min(ban.year)) %>% 
    ungroup() %>% 
    # Define post-treatment group relative to ban.year:
    mutate(post.treat.group = ifelse(elec.year >= ban.year, 1, 0)) %>%
    # Compute the outcome mean by election year and treatment group:
    group_by(elec.year, post.treat.group) %>% 
    mutate(mayor.improb.mean = mean(mayor.improb, na.rm = TRUE)) %>% 
    ungroup() %>%
    # Inflate the treated group's outcome by i times its group-specific mean:
    mutate(mayor.improb = ifelse(post.treat.group == 1, 
                                 mayor.improb + (mayor.improb.mean * i), mayor.improb)) %>% 
    # Run the DID model on the modified outcome:
    get.did.mod(data = ., treatment = 't1.ban.gov', outvars = c('mayor.improb'))
  
  list.corr.temp <- list(tempmod$mayor.improb)  
  names(list.corr.temp) <- c("mayor.improb")
  
  tab <- 
    get.did.tab(list.corr.temp) %>% 
    mutate(Std.Error = gsub("\\(|\\)", "", Std.Error),
           ATT       = gsub("\\*\\*\\*|\\*\\*|\\*", "", ATT),
           Std.Error = as.numeric(Std.Error),
           ATT       = as.numeric(ATT),
           cmin      = ATT - (1.96 * Std.Error), 
           cmax      = ATT + (1.96 * Std.Error),
           share     = i)   # Multiplier applied to the treated group's mean
  
  if(i == 0){ sim.tab <- tab } else { sim.tab <- rbind(sim.tab, tab) }
}

# Plot the estimated ATT as the multiplier (share) increases; error bars show 95% CIs.
sim.tab %>%
  ggplot(aes(x = share, y = ATT, ymin = cmin, ymax = cmax)) + 
  geom_errorbar(width = 0.002) +
  geom_point(size = 1.5) +
  theme_minimal() +
  labs(x = "Number of Times", y = "ATT") +
  scale_x_continuous(breaks = seq(0, 1.2, .2)) +
  scale_y_continuous(breaks = seq(-.10, .02, .02)) +
  expand_limits(y = c(-.10, .02)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  theme(axis.ticks = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave(file.path(outfolder, 'simul.png'), width = 7.5, height = 3.5)
ggsave(file.path(outfolder, 'simul.eps'), width = 5, height = 3,
       units = "in", device = cairo_ps)


################################################################################
# End






