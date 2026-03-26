# ====
# Figure 2 Replication:
# PanelMatch effect plot
# input: intensive_margin_paneldata.RData
# saved results: Estimated_results_allexp_aug.RData; Estimated_results_allimp_aug.RData
# R version 4.4.2 (2024-10-31)
# ====

################################################################################
## setup -----
################################################################################
# clean slate
rm(list = ls())
date()


library(tidyverse)
library(latex2exp)
library(kableExtra)
library(modelsummary)
library(PanelMatch)
`%!in%` <- Negate(`%in%`)

# set directory
REPLICATION_DATA_DIR <- "data/"
REPLICATION_FIG_DIR <-  "output/"


################################################################################
## 0. load data  ------
################################################################################
load(file = paste0(REPLICATION_DATA_DIR, "intensive_margin_paneldata.RData"))
data.panel.pm  %>% glimpse()


# #################################################################
##  1. Run Analyses ----------------------------
# #################################################################

# omit character variables to run PM
data.panel.pm <- data.panel.pm %>%
  mutate(HSsection_num = as.numeric(as.roman(HSsection)))  %>%
  dplyr::select(-HS_6d, -HSsection)

table(data.panel.pm$HSsection_num, useNA = "always")

data.panel.pm$`Commodity.Code` <- as.integer(data.panel.pm$`Commodity.Code`)

data.panel.pm <- as.data.frame(data.panel.pm)
data.panel.pm %>% glimpse

# Rum PM for current and placebo outcomes each
# RUN_PM <- FALSE # set to false to use saved results
RUN_PM <- TRUE

## With covariates: loop over placebo and current outcomes ----------------------------
for (VERSION in c("allexp_aug", "allimp_aug")){
  if (RUN_PM){
    # results to be stored
    PMresults <- list()
    PEresults <- list()
    Balances <- list()

    for (i in 1:3){
      if(VERSION == "allimp_aug"){

        OUTCOME.NONREF <- "tradevol_imp_aug.ln"
        TREAT <- "fdi_count_dm_imp.once"

        OUTCOME_list <- data.frame(outcome_label = c("allimp_aug",
                                                    "allimp_aug_placebo1",
                                                    "allimp_aug_placebo2"),
                                  outcomes = c("tradevol_imp_aug.ln",
                                                "tradevol_imp_aug.ln_lag",
                                                "tradevol_imp_aug.ln_lag2"))

      MISSINGNESS_index <- "tradevol_imp_aug_missing" # trade missingness index to be used as matching covariates


      }else if(VERSION == "allexp_aug"){
        OUTCOME.NONREF <- "tradevol_exp_aug.ln"
        TREAT <- "fdi_count_dm_exp.once"

        OUTCOME_list <- data.frame(outcome_label = c("allexp_aug",
                                                      "allexp_aug_placebo1",
                                                      "allexp_aug_placebo2"),
                                  outcomes = c("tradevol_exp_aug.ln",
                                                "tradevol_exp_aug.ln_lag",
                                                "tradevol_exp_aug.ln_lag2")) #list of placebo and current outcomes

        MISSINGNESS_index <- "tradevol_exp_aug_missing"
    }

      # DV for the iteration
      OUTCOMEname  <- OUTCOME_list$outcome_label[i] 
      OUTCOME <- OUTCOME_list$outcomes[i]
      
      print(OUTCOME)

      # Covariates
      covslist_pm <- c("row_exp.ln", "row_imp.ln",
                      "expmean.ln", "impmean.ln",
                      "export_country", "import_country",
                      "intermediate", "gvc_ui", "gvc_di",
                      "rauch_n", "rauch_w",
                      "vnm_avgMFN",
                      MISSINGNESS_index)


      # Covariates to be used
      covs <- paste0("I(lag(", covslist_pm, ", 1:4))", collapse = " + ")
      print(covs)


      # Panel Match without lagged outcome variable and with all covariates----
     data.panel.pmproc <- PanelMatch::PanelData(data.panel.pm,
                                    time.id = "year",
                                    unit.id = "Commodity.Code",
                                    treatment = TREAT,
                                    outcome = OUTCOME)

      set.seed(08542)

      PM.result <- PanelMatch(panel.data = data.panel.pmproc,
                              lag = 4,
                              refinement.method = "CBPS.weight",
                              covs.formula = as.formula(paste0("~ ", covs)),
                              exact.match.variables = "HSsection_num",
                              lead = 0:4,
                              verbose = TRUE,
                              qoi = "att",
                              forbid.treatment.reversal = TRUE)


      PMresults[[i]] <- PM.result # store PM result

      print(paste(i, "th PM done", VERSION))

      # Estimate Effects -----
      PE.estimate <- PanelEstimate(sets = PMresults[[i]], panel.data = data.panel.pmproc)
      PEresults[[i]] <- PE.estimate
      print(summary(PE.estimate))

      print(paste0(i, "th estimate done", VERSION))

      # Balance -----
      covslist_balance <- c("row_exp.ln", "row_imp.ln",
                            "expmean.ln", "impmean.ln",
                            "export_country", "import_country",
                            "intermediate", "gvc_ui", "gvc_di",
                            "rauch_n", "rauch_w", "vnm_avgMFN",
                            MISSINGNESS_index)

      Balances[[i]] <- get_covariate_balance(PMresults[[i]],
                                            panel.data = data.panel.pmproc,
                                            covariates = c(OUTCOME, # add outcome variable
                                                            covslist_balance))
      print(Balances[[i]])

      print(paste(i, "th balance done", VERSION))

    } # end loop over placebos and current outcomes

    # save the list of Balances and PM
    save(Balances, file = paste0(REPLICATION_DATA_DIR, "PM_balance_", VERSION, ".RData"))
    save(PMresults, file = paste0(REPLICATION_DATA_DIR, "PM_results_", VERSION, ".RData"))
    save(PEresults, file = paste0(REPLICATION_DATA_DIR, "PE_results_", VERSION, ".RData"))


    # combine placebo and current PE estimates -----
    PLACEBO <- c(0, 1, 2) # not placebo, placebo1, placebo2
    Est.results.comb <- list() # store results

    for (k in 1:3){
      placebo <- PLACEBO[k] # version name

      PE.result <- PEresults[[k]]

      Est.results <- as.data.frame(summary(PE.result))
      Est.results$lag <- placebo #specify version

      if (placebo == 0){
        Est.results$time <- rownames(Est.results)
      }else if (placebo == 1){
        Est.results$time <- c("t-1", "t+0", "t+1", "t+2", "t+3")[1:nrow(Est.results)]
      }else if (placebo == 2){
        Est.results$time <- c("t-2", "t-1", "t+0", "t+1", "t+2")[1:nrow(Est.results)]
      }

      Est.results$time <- ordered(Est.results$time,
      levels = c("t-2", "t-1", "t+0", "t+1", "t+2", "t+3", "t+4"))

      Est.results.comb[[k]] <- Est.results
    }
    
    Est.results.all <- do.call(rbind, Est.results.comb)
    Est.results.all$time <- dplyr::recode(Est.results.all$time, "t+0" = "t")
    Est.results.all$time  <- ordered(Est.results.all$time, levels = c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"))

    # effect sizes in percentage
    print(Est.results.all %>% filter(lag == 0) %>%
          mutate(effectsize_percentage = (exp(.$estimate) - 1) * 100))

    # save results
    save(Est.results.all, file = paste0(REPLICATION_DATA_DIR, "Estimated_results_", VERSION, ".RData"))

  }
}


# ############################
## 2. Plot effects --------------------------------------------------------------------
# ############################

# plot each
plots.list <- list()

for (VERSION in c("allexp_aug", "allimp_aug")){

  if (VERSION == "allexp_aug"){
    YLAB <- "Estimated effect of FDI on trade volume"
    axis_text_setting <- element_text(size = 24, color = "black")
  
  }else if (VERSION == "allimp_aug"){
    YLAB <- " "
    axis_text_setting <- element_blank()
  } # to remove axis text for the second plot

  # load estimates
  load(file = paste0(REPLICATION_DATA_DIR, "Estimated_results_", VERSION, ".RData"))
  print(Est.results.all)

  # select results for plotting
  Est.results.plot <- Est.results.all %>%
    filter((time == "t-1" & lag == 2) | (time == "t-1" & lag == 1) |
           (time %in% c("t", "t+1", "t+2", "t+3", "t+4") & lag == 0)) %>%
    mutate(time = ifelse(lag == 2, "t-2", as.character(time))) %>% # placebo 2: code the t-3 vs t-1 comparison as "t-2"
    mutate(time = factor(time,  levels = c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"))) 

  plot.est <- ggplot(aes(x = time, y = estimate), data = Est.results.plot) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red")  +
    geom_point(aes(color = as.factor(time)),
               position = position_dodge(width = 0.5), size = 4)  +
    geom_linerange(aes(ymin = `2.5%`, ymax = `97.5%`, color = as.factor(time)),
                   position = position_dodge(width = 0.5)) +
    ylim(-0.25, 1.05) +
    scale_x_discrete(breaks = c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"),
                     labels = unname(TeX(paste0("$\\textit{", c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"), "}")))) +
    scale_color_manual(breaks = c("t-2", "t-1", "t", "t+1", "t+2", "t+3", "t+4"),
                       values=c("black", "black", "black", "black", "black", "black", "black")) + 
    geom_rect(aes(xmin = -Inf, xmax = 2.5, ymin = -Inf, ymax = Inf),
                  fill = "gray", alpha = 0.05) +
    ggplot2::annotate("segment", x = 1.25, xend = 0.45, y = 0.6, yend = 0.6,
                      arrow = arrow(length = unit(0.25, "cm")), lwd = 0.5) + # adding arrows 
    ggplot2::annotate("segment", x = 1.25, xend = 2.45, y = 0.6, yend = 0.6,
                      arrow = arrow(length = unit(0.25, "cm")), lwd = 0.5) + 
    ggplot2::annotate("text", x = 1.45, y = 0.7, label = "Placebo test", size = 8) +
    xlab("") +
    ylab(YLAB) +
    theme_bw() +
    theme(legend.position = "none",
          axis.title.y = element_text(size = 24,
                                      margin = margin(0, 20, 0, 0)),
          axis.title.x = element_text(size = 24,
                                      margin = margin(20, 0, 0, 0)),
          axis.text.y = axis_text_setting,
          axis.text.x = element_text(size = 24, color = "black"),
          strip.background = element_blank(),
          strip.text = element_text(size = 24,
                                    face = "bold",
                                    margin = margin(20, 0, 20, 0)),
          panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey20", fill = NA, linewidth = 1.25), 
          panel.spacing.x = unit(2, "lines"),
          plot.margin = margin(0.5, 0.2, 0.2, 0.2, "cm")
          )

  plots.list[[VERSION]] <- plot.est

}

# adjust the width of the panels
g.exp <- ggplotGrob(plots.list[["allexp_aug"]])
g.imp <- ggplotGrob(plots.list[["allimp_aug"]])

g.imp$widths[2:3] <- g.exp$widths[2:3]  # Ensure y-axis label space is equal
g.imp$widths[6] <- g.exp$widths[6] # Force panel width to match

plots.list[["allexp_aug"]] <- g.exp
plots.list[["allimp_aug"]] <- g.imp

# save each
for (VERSION in c("allexp_aug", "allimp_aug")){
  if (VERSION == "allexp_aug"){
    PLOTTITLE <- "Export"
    }else if (VERSION == "allimp_aug"){
    PLOTTITLE <- "Import"
    }

  ggsave(plot = plots.list[[VERSION]],
       file = paste0(REPLICATION_FIG_DIR, "F2_Intensive_VNM_PanelE_placebo_", PLOTTITLE, ".pdf"),
       width = 9.5,
       height = 7)
}
