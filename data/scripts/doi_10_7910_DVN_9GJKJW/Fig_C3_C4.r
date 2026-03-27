# ====
# Figure C3 and C4 Replication:
# PanelMatch balace plot for export and import
# input: intensive_margin_paneldata.RData,
# The results with covariates are saved from Fig_2.r
# saved results: PM_balance_allexp_aug.RData, PM_balance_allimp_aug.RData,
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

## 1.1 Without refinement at time t --------------------------------

# Rum PM for current and placebo outcomes each
# RUN_PM <- FALSE # set to false to use saved results
RUN_PM <- TRUE

if(RUN_PM){
  for (VERSION in c("allexp_aug", "allimp_aug")){
    if(VERSION == "allimp_aug"){

      OUTCOME.NONREF <- "tradevol_imp_aug.ln"
      TREAT <- "fdi_count_dm_imp.once"

      MISSINGNESS_index <- "tradevol_imp_aug_missing" 


    }else if(VERSION == "allexp_aug"){

      OUTCOME.NONREF <- "tradevol_exp_aug.ln"
      TREAT <- "fdi_count_dm_exp.once"

      MISSINGNESS_index <- "tradevol_exp_aug_missing"
    }

    # Panel Match
    data.panel.pmproc <- PanelData(data.panel.pm,
                                      time.id = "year",
                                      unit.id = "Commodity.Code",
                                      treatment = TREAT,
                                      outcome = OUTCOME.NONREF)
    set.seed(08542)

    PM.nonref <- PanelMatch(panel.data = data.panel.pmproc,
                            lag = 4,
                            refinement.method = "none",
                            exact.match.variables = "HSsection_num",
                            lead = 0:4,
                            verbose = TRUE,
                            qoi = "att",
                            forbid.treatment.reversal = TRUE)


    # Covariate balance
    covslist_balance_nonref <- c("row_exp.ln", "row_imp.ln",
                                "expmean.ln", "impmean.ln",
                                "export_country", "import_country",
                                "intermediate", "gvc_ui", "gvc_di",
                                "rauch_n", "rauch_w", "vnm_avgMFN",
                            　   MISSINGNESS_index)

    balance.nonref <- get_covariate_balance(PM.nonref,
                                            panel.data = data.panel.pmproc,
                                            covariates = c(OUTCOME.NONREF, covslist_balance_nonref))
    print(balance.nonref)

    balance.nonref.save <- balance.nonref

    # save the list of Balances and PM
    save(balance.nonref, file = paste0(REPLICATION_DATA_DIR, "PM_balance_NONREF_", VERSION, ".RData"))
    save(PM.nonref, file = paste0(REPLICATION_DATA_DIR, "PM_results_NONREF_", VERSION, ".RData"))

  }
}



# #################################################################
##  2. Plot balance ----------------------------
# #################################################################
for (VERSION in c("allexp_aug", "allimp_aug")){

  if(VERSION == "allimp_aug"){
      OUTCOME.NONREF <- "tradevol_imp_aug.ln"
      MISSINGNESS_index <- "tradevol_imp_aug_missing"
    }else if(VERSION == "allexp_aug"){
      OUTCOME.NONREF <- "tradevol_exp_aug.ln"
      MISSINGNESS_index <- "tradevol_exp_aug_missing"
    }

  # load balance --------------------------------
  # without refinements
  load(file = paste0(REPLICATION_DATA_DIR, "PM_balance_NONREF_", VERSION, ".RData"))
  #with refinements
  load(file = paste0(REPLICATION_DATA_DIR, "PM_balance_", VERSION, ".RData"))

  # combined data for non-refined and refined models
  Balancedat_before <- as.data.frame(balance.nonref)
  Balancedat_before <- tidyr::gather(data.frame(time = rownames(Balancedat_before),
                                                Balancedat_before),
                                    key = var, value = SD, -time)

  Balancedat_before$refinement <- "Before refinement"
  Balancedat_before$var <- gsub("att\\.", "", Balancedat_before$var)
  Balancedat_before

  Balancedat_after <- as.data.frame(Balances[[1]])
  Balancedat_after <- tidyr::gather(data.frame(time = rownames(Balancedat_after),
                                              Balancedat_after),
                                    key = var, value = SD, -time)

  Balancedat_after$refinement <- "After refinement"
  Balancedat_after$var <- gsub("att\\.", "", Balancedat_after$var)
  Balancedat_after

  Balancedat_comb <- rbind(Balancedat_before, Balancedat_after)
  setdiff(unique(Balancedat_before$var), unique(Balancedat_after$var))
  setdiff(unique(Balancedat_after$var), unique(Balancedat_before$var)) #make sure they match


  covslist_balance <- c("row_exp.ln", "row_imp.ln",
                        "expmean.ln", "impmean.ln",
                        "export_country", "import_country",
                        "intermediate", "gvc_ui", "gvc_di",
                        "rauch_n", "rauch_w", "vnm_avgMFN"
                        ) 

  Balancedat_comb <- Balancedat_comb %>% filter(var %in% c(OUTCOME.NONREF, covslist_balance))


  # prepare variables for plot
  Balancedat_comb$refinement <- ordered(Balancedat_comb$refinement,
                                        levels = c("Before refinement", "After refinement"))

  Balancedat_comb$time <-  dplyr::recode(Balancedat_comb$time,
                                        "t_4" = "t-4",
                                        "t_3" = "t-3",
                                        "t_2" = "t-2",
                                        "t_1" = "t-1")

  Balancedat_comb$time <- ordered(Balancedat_comb$time,
                                  levels = c("t-4", "t-3", "t-2", "t-1"))

  Balancedat_comb <- Balancedat_comb %>%  filter(time %in% c("t-4", "t-3", "t-2", "t-1"))

  # plot theme
  axis.title.size <- 20
  bplot_basetheme <-  theme_bw() +
      theme(legend.position = "none",
            plot.title = element_text(size = axis.title.size,
                                      face = "bold",
                                      hjust = 0.5,
                                      margin = margin(0, 0, 5, 0)),
        plot.margin = margin(0.1, 0.5, 0.1, 0.1, "cm"),
        axis.title.y = element_text(size = axis.title.size,
                                    margin = margin(0, 20, 0, 0)),
        axis.title.x = element_text(size = axis.title.size,
                                    margin = margin(20, 0, 0, 0)),
        axis.text.x = element_text(size = axis.title.size, color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(size = axis.title.size,
                                  margin = margin(20, 0, 20, 0)),
        panel.border = element_rect(color = "black", fill = NA, size = 2),
        panel.grid = element_blank())

    # color DV as red and other covariates as grey
    colsset <- eval(parse(text = paste0("c('", OUTCOME.NONREF, "' = 'red', ", paste(paste0("'", covslist_balance, "'=", "'grey50'"), collapse = ","), ")")))


  # plot before
  plot.before <- Balancedat_comb %>%
    filter(refinement == "Before refinement") %>%
    ggplot(aes(time, SD, group = var)) +
    geom_line(aes(color = var)) +
    scale_color_manual(values = colsset) +
    scale_x_discrete(breaks = c("t-4", "t-3", "t-2", "t-1")) +
    scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                       labels = seq(-0.2, 1, 0.2),
                       limits = c(-0.4, 1)) +
      xlab("") +
      ylab("Standardized difference") +
      ggtitle("(a) Before refinement") +
      bplot_basetheme +
      theme(axis.text.y = element_text(size = axis.title.size, color = "black"))

  # plot after
  plot.after <- Balancedat_comb %>%
    filter(refinement == "After refinement") %>%
    ggplot(aes(time, SD, group = var)) +
    geom_line(aes(color = var)) +
    scale_color_manual(values = colsset) +
      scale_x_discrete(breaks = c("t-4", "t-3", "t-2", "t-1")) +
      scale_y_continuous(breaks = seq(-0.2, 1, 0.2),
                        labels = seq(-0.2, 1, 0.2),
                        limits = c(-0.4, 1)) +
      xlab("") +
      ylab(" ") +
      ggtitle("(b) After refinement") +
      bplot_basetheme +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank())

  # adjust the width of the panels
  plot.before <- ggplotGrob(plot.before)
  plot.after <- ggplotGrob(plot.after)

  plot.after$widths[6] <- plot.before$widths[6] # Force panel width to match

  ggsave(plot.before,
        file = paste0(REPLICATION_FIG_DIR, "FC34_Intensive_VNM_Balance_", VERSION, "_before.pdf"),
        width = 6.5, height = 6)

  ggsave(plot.after,
        file = paste0(REPLICATION_FIG_DIR, "FC34_Intensive_VNM_Balance_", VERSION, "_after.pdf"),
        width = 6.5, height = 6)

}