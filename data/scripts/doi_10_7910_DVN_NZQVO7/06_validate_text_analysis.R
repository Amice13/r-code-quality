####################################################################
####################################################################
## Replication Material
##
## Shaun Bowler, Gail McElroy, Stefan Müller:
## Voter Expectations of Government Formation in Coalition Systems: 
## The Importance of the Information Context.
## European Journal of Political Research

## File: 06_validate_text_analysis.R

## See 000_description_replication_material.pdf for a detailed 
## overview of the required data and the outputs of this file.

####################################################################
####################################################################

## validate the automated detection of coalition signals using
## the manual coding of pre-electoral coalitions identified by Best (2015)

## load packages
library(dplyr) # A Grammar of Data Manipulation, CRAN v1.0.2
library(texreg) # Conversion of R Regression Output to LaTeX or HTML Tables, CRAN v1.37.5
library(stringr) # Simple, Consistent Wrappers for Common String Operations, CRAN v1.4.0


## load full dataset
dta_combined <- readRDS("data_reg_full.rds") 

## remove missing responses (important)
dta_select_reg <- dta_combined %>% 
    filter(!is.na(predicted_coalition_stand))

## load coding of coalitions options from the Appendix in Best (2015)
dat_coalitions_manual <- read.csv("data_coalition_options_coded.csv")

## get unique values for "references" to coalitions ("ref_...")
## "unique" results in one observation per government-election option
dat_coalitions_text <- dta_select_reg %>% 
    dplyr::select(year, election_id, coalition_option_stand, 
                  ref_perc_of_all_coalitions) %>% 
    unique() %>% 
    left_join(dat_coalitions_manual)


## remove single-party governments (no coalitions)
dat_coalitions_text <- dat_coalitions_text %>% 
    filter(str_detect(coalition_option_stand, "_"))


dat_coalitions_text_valid <- filter(dat_coalitions_text, !is.na(preelectoral_coalition_best))

dat_coalitions_text_valid$preelectoral_coalition_best <- factor(dat_coalitions_text_valid$preelectoral_coalition_best, 
                                                                levels = c("Negative", "Neutral", "Alternative",
                                                                           "Desired"))

lm_text <- lm(ref_perc_of_all_coalitions ~ 
                  preelectoral_coalition_best -1, ## remove intercept
              data = dat_coalitions_text_valid)

## save regression table
htmlreg(list(lm_text), custom.coef.names = c("Coalition signal: Negative coalition signal", 
                                             "Coalition signal: Neutral coalition signal",
                                             "Coalition signal: Alternative pre-electoral coalition",
                                             "Coalition signal: Desired pre-electoral coalition"),
        caption = "",
        single.row = TRUE,
        file = "tab_02.htm")

