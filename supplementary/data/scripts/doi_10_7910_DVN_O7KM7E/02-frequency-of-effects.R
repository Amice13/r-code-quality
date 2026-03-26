##### Setup -----
## If you haven't installed the included "bapvar" package,
## you can do so via the following:
# devtools::install("bapvar")
## Load required packages
library(bapvar)   ## For BaP-VAR sampling and analysis functions
library(dplyr)    ## For data wrangling
library(tidyr)    ## For data wrangling
## Source auxiliary functions
source("code/00-util.R") ## The functions are documented (roxygen-style) there
## Ensure needed subdirectories exist
prepare_directories()


##### Data prep -----
## Read in signs of coefficients
coef_signs <- read.csv("data/coef-signs.csv")
## Change the case citations for convenience with output filenames
coef_signs$cited_case <- fix_citation(coef_signs$cited_case)
## Get filenames containing replicated posterior samples
file_names <- list.files(
    path = "output", full.names = TRUE,
    pattern = "[0-9]+-US-LEXIS-[0-9]+-lag[0-9].rds"
)
## And citations for precedents being replicated
cases_sampled <- unique(gsub(
    pattern = "output/(.+)-lag[0-9].rds",
    replacement = "\\1",
    x = file_names
))


##### Ensure replicated results match stored results -----
courts <- c("SC", "AC", "DC")
vars   <- c(outer(courts, courts, paste, sep = "1_"))
## For every case you generated replication samples for,
for ( i in seq_along(cases_sampled) ) {
    ## Update progress
    cat("\r", i, "/", length(cases_sampled))
    ## Determine which set of lags to use
    case  <- cases_sampled[i]
    lags  <- coef_signs$optimal_lags[coef_signs$cited_case == case][1]
    ## Load the replication samples drawn and summarise output
    draws <- readRDS(paste0("output/", case, "-lag", lags, ".rds"))
    qs    <- summary(draws$samples)$quantiles
    signs <- sapply(vars, function(v) {
        return(ifelse(qs[v, "2.5%"] > 0, 1, ifelse(qs[v, "97.5%"] < 0, -1, 0)))
    })
    ## Check if that matches the reported results
    reported_result <- unlist(coef_signs[coef_signs$cited_case == case, vars])
    ## If not, let's see what the discrepancy is
    ## (we shouldn't trip this branch, but it is potentially possible
    ## given system and software version differences since we're sampling here)
    if ( !isTRUE(all.equal(signs, reported_result)) ) {
        cat("\nFor ", case, ",\n", sep = "")
        ## Check each of the lag coefficients
        for ( v in vars ) {
            ## And see what the replication CI is relative to "reported sign"
            if ( signs[v] != reported_result[v] ) {
                if ( reported_result[v] > 0 ) {
                    cat("\t", v, " coefficient was reported reliably positive,",
                        "\n\twhile the replication 2.5% quantile was ",
                        qs[v, "2.5%"], "\n", sep = "")
                } else if (reported_result[v] < 0 ) {
                    cat("\t", v, " coefficient was reported reliably negative,",
                        "\n\twhile the replication 97.5% quantile was ",
                        qs[v, "97.5%"], "\n", sep = "")
                } else {
                    cat("\t", v, " coefficient was reported not reliably",
                        "positive or negative,",
                        "\n\twhile the replication 95% CI was [",
                        paste(qs[v, c("2.5%", "97.5%")], sep = ", "), "]\n",
                        sep = "")
                }
            }
        }
    }
}


##### Reproduce Table  1 from stored results -----
## Since overwhelmingly the optimal number of lags is one,
## we'll look at the coefficients for the first lag;
## we also reshape the data a bit here so that each precedent has 3 rows
## instead of 1---each row will be the sign of a lag variable in each
## equation for that precedent
coef_signs <- coef_signs %>%
    select(cited_case:DC1_DC) %>%
    pivot_longer(
        cols = contains("1_"),
        names_to = c("var", ".value"),
        names_sep = "1_"
    )
tab1 <- coef_signs %>%         ## Start w effect sign results
    select(-cited_case) %>%    ## Remove the precedent citation column
    select(-optimal_lags) %>%  ## Remove the column giving optimal # of lags
    group_by(var) %>%          ## Group by lag coefficient
    summarise_all(sum_abs)     ## And count the # diff to 0 for each lag coef
## Now there will be four columns, one identifying the lag variable and one
## for each equation, and three rows, one for each lag variable.
## We view the table, ordering the rows as in the paper
tab1[match(c("SC", "AC", "DC"), tab1$var), ]


##### Reproduce Figure 1 from stored results -----
## First we determine for each precedent which variables Granger cause others
## (this takes a minute or two)
granger_causation <- coef_signs %>%
    group_by(cited_case) %>%
    group_modify(~ {
        tibble(
            SC_causes_AC = granger_cause(.x, "SC", "AC"),
            SC_causes_DC = granger_cause(.x, "SC", "DC"),
            AC_causes_SC = granger_cause(.x, "AC", "SC"),
            AC_causes_DC = granger_cause(.x, "AC", "DC"),
            DC_causes_SC = granger_cause(.x, "DC", "SC"),
            DC_causes_AC = granger_cause(.x, "DC", "AC")
        )
    })
fig1_data <- granger_causation %>%
    ungroup() %>%
    select(-cited_case) %>%
    summarise_all(sum) %>%
    pivot_longer(
        everything(),
        names_sep = "_causes_",
        names_to = c("V1", "V2"),
        values_to = "count"
    )
## You can compare the numbers generated with Figure 1
fig1_data
## But there's not R replication code for the plot itself since we used TikZ


##### Reproduce Table  2 from stored results -----
tab2 <- granger_causation %>%
    ungroup() %>%
    summarise(
        SC = sum(SC_causes_AC & SC_causes_DC),
        AC = sum(AC_causes_SC & AC_causes_DC),
        DC = sum(DC_causes_SC & DC_causes_AC)
    ) %>%
    pivot_longer(cols = everything()) %>%
    rbind(
        coef_signs %>%
            group_by(cited_case) %>%
            group_modify(~ {
                tibble(
                    Top_down = has_pattern(.x, "top-down"),
                    Bottom_up = has_pattern(.x, "bottom-up")
                )
            }) %>%
            ungroup() %>%
            select(-cited_case) %>%
            summarise_all(sum) %>%
            pivot_longer(cols = everything())
    )
tab2
