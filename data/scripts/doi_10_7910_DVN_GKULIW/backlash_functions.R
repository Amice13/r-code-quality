#######################################################################################################
# Ethnic accommodation and the backlash from dominant groups #
# Libraries and functions required for running analyses #
# Andreas Juon #
# Journal of Conflict Resolution #
# April 2025 #
#######################################################################################################

# Import libraries
library(stargazer)
library(car)
library(marginaleffects)
library(ggplot2)
library(texreg)
library(plyr)
library(dplyr)
library(lme4)
library(MASS)
library(sensemakr)
library(fect)
library(margins)
library(grid)
library(gridExtra)
library(tidyr)
library(reshape2)
options(max.print = 99999999)

# Define standard sets of variables
controls_c <- c("ld10_bdead_allc","mnm_party_exists","mnm_partygov","vdem_libdem","lsize_abs","lgdppc","gdppc_change")
controls_c_lmnm_partyno <- c("ld10_bdead_allc","lmnm_partyno","mnm_partygov","vdem_libdem","lsize_abs","lgdppc","gdppc_change")
shocks3 <- c("ltt_nextelec","month_protest2_nsc_forward3_close3","month_viol_civil_nsc_forward3_close3")
backlash_months <- c("nobacklash_months_l1","I(nobacklash_months_l1^2)","I(nobacklash_months_l1^3)","lunreg_backlash_no")
re_fe <- c("cowcodef","yearf","surveywave", "1", "(1|cowcode)", "(1|cowcode_year)")
controls_i <- c("age","gender","educ_high","interest_pol_d","rightwing","authoritarian")

# Define order in which variables will be reported #
vars.order <- c("conc_close","conc_symbolic_close","conc_nonsymbolic_close","conch_symbolic_close","concv_symbolic_close","conch_nonsymbolic_close","concv_nonsymbolic_close","conci_close","concc_close","concv_close","conci_symbolic_close","concc_symbolic_close","concv_symbolic_close","conci_nonsymbolic_close","concc_nonsymbolic_close","concv_nonsymbolic_close",
                 "conc_sum_close","conc_sum_symbolic_close","conc_sum_nonsymbolic_close","conch_sum_symbolic_close","concv_sum_symbolic_close","conch_sum_nonsymbolic_close","concv_sum_nonsymbolic_close","conci_sum_sumclose","concc_close","concv_sum_close","conci_sum_sumsymbolic_close","concc_symbolic_close","concv_sum_symbolic_close","conci_sum_sumnonsymbolic_close","concc_nonsymbolic_close","concv_sum_nonsymbolic_close"
                 ,"mnm_party_exists:","mnm_party_exists","mnm_party_exists_l5:","mnm_party_exists_l5","mnm_party_exists_avg:","mnm_party_exists_avg","mnm_partygov")

# Define functions

## Confidence intervals
get_confint<-function(model, vcovCL){
  t<-qt(.975, model$df.residual)
  ct<-coeftest(model, vcovCL)
  est<-cbind(ct[,1], ct[,1]-t*ct[,2], ct[,1]+t*ct[,2])
  colnames(est)<-c("Estimate","LowerCI","UpperCI")
  return(est)
}
cluster.se<-function(model, cluster)
{
  require(multiwayvcov)
  require(lmtest)
  vcovCL<-cluster.vcov(model, cluster)
  coef<-coeftest(model, vcovCL)
  return(coef)
}

## Default labels
labels_conc_default = c("Concession number","Concession number x DN party","Concession number (group-based)","Concession number (group-based) x DN party","Concession number (group-blind)","Concession number (group-blind) x DN party")
labels_dnp_default = c("DN party","DN party in government")
labels_controls_default = c("Months to next election (log)","Recent subordinate group protest","Recent civil violence","Battle deaths (last 10y, log)","Democracy level","Abs. size (log)","GDP p.c. (log)","GDP growth","Regional DG mobilization events (log)")

## Add stars to printout
get_stars <- function(x) {
  if (x < 0.001) {
    paste("***", sep ="")
  }
  else if (x < 0.01) {
    paste("**", sep ="")
  }
  else if (x < 0.05) {
    paste("*", sep ="")
  }
  else if (x < 0.1) {
    paste("†", sep ="")
  }
  else {
    paste("")
  }
}

add_stars <- function(x) {
  if (x < 0.001) {
    paste(round(x, 3), "***", sep ="")
  }
  else if (x < 0.01) {
    paste(round(x, 3), "**", sep ="")
  }
  else if (x < 0.05) {
    paste(round(x, 3), "*", sep ="")
  }
  else if (x < 0.1) {
    paste(round(x, 3), "†", sep ="")
  }
  else {
    paste(round(x, 3))
  }
}

## Analysis script
run_analysis <- function(term_conc = "conc_sum", time_window = "_close3", moderator = "mnm_party_exists",
                         term_controls = controls_c, term_shocks = shocks3, term_time = backlash_months, term_controls_additional = c(),
                         DV = "backlash_no",
                         fes = c("cowcodef","yearf"),
                         model = "negbin",
                         model_label = "main", model_name = "",
                         labels_conc = labels_conc_default, labels_dnp = labels_dnp_default, labels_controls = labels_controls_default,
                         analysis_data = subset(backlash_data, state_control == 1 & protest_sample == 1)) {
  
  suppressWarnings({#suppresses glm.fit warnings arising from use of fixed effects; avoids console getting cluttered; remove here and remove closing brackets }) below to show warnings
    
  # 0. Define main analysis terms from inputs
  term_m1 <- paste(term_conc, time_window, sep="")
  term_m2  <- paste(moderator, " * ", term_conc, time_window, sep="")
  term_m3  <- paste(term_conc, "_symbolic", time_window, " + ", term_conc, "_nonsymbolic", time_window, sep="")
  term_m4  <- paste(moderator, " * ", term_conc, "_symbolic", time_window, " + ", moderator, " * ", term_conc, "_nonsymbolic", time_window, sep="")
  
  
  # 1. Main models #
  
  ## estimation
  print("Running main analyses ...")
  
  if (model == "negbin") {
    print("... using negative binomial model ...")
    m1a_3 <- glm.nb(as.formula(paste(DV, paste(c(term_m1, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = analysis_data)
    m1b_3 <- glm.nb(as.formula(paste(DV, paste(c(term_m2, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = analysis_data)
    m2a_3 <- glm.nb(as.formula(paste(DV, paste(c(term_m3, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = analysis_data)
    m2b_3 <- glm.nb(as.formula(paste(DV, paste(c(term_m4, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), control=glm.control(maxit=1000), data = analysis_data)
  }
  
  if (model == "poisson") {
    print("... using poisson model ...")
    m1a_3 <- glm(as.formula(paste(DV, paste(c(term_m1, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "poisson", data = analysis_data)
    m1b_3 <- glm(as.formula(paste(DV, paste(c(term_m2, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "poisson", data = analysis_data)
    m2a_3 <- glm(as.formula(paste(DV, paste(c(term_m3, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "poisson", data = analysis_data)
    m2b_3 <- glm(as.formula(paste(DV, paste(c(term_m4, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "poisson", data = analysis_data)
  }
  
  if (model == "ols") {
    print("... using ols model ...")
    m1a_3 <- glm(as.formula(paste(DV, paste(c(term_m1, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "gaussian", data = analysis_data)
    m1b_3 <- glm(as.formula(paste(DV, paste(c(term_m2, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "gaussian", data = analysis_data)
    m2a_3 <- glm(as.formula(paste(DV, paste(c(term_m3, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "gaussian", data = analysis_data)
    m2b_3 <- glm(as.formula(paste(DV, paste(c(term_m4, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "gaussian", data = analysis_data)
  }
  
  if (model == "logit") {
    print("... using logit model ...")
    m1a_3 <- glm(as.formula(paste(DV, paste(c(term_m1, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "binomial", data = analysis_data)
    m1b_3 <- glm(as.formula(paste(DV, paste(c(term_m2, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "binomial", data = analysis_data)
    m2a_3 <- glm(as.formula(paste(DV, paste(c(term_m3, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "binomial", data = analysis_data)
    m2b_3 <- glm(as.formula(paste(DV, paste(c(term_m4, term_shocks, term_controls, term_controls_additional, term_time, fes), collapse = " + "), sep = " ~ ")), family = "binomial", data = analysis_data)
  }
  
  cl_m1a_3 <- data.frame(cluster.se(m1a_3, as.integer(analysis_data$cowcode))[, 2])
  cl_m1b_3 <- data.frame(cluster.se(m1b_3, as.integer(analysis_data$cowcode))[, 2])
  cl_m2a_3 <- data.frame(cluster.se(m2a_3, as.integer(analysis_data$cowcode))[, 2])
  cl_m2b_3 <- data.frame(cluster.se(m2b_3, as.integer(analysis_data$cowcode))[, 2])

  ## wald tests
  print("Estimating Wald tests ...")
  m1b_3_jointsig <- linearHypothesis(m1b_3, paste(term_conc,time_window, " + mnm_party_exists:", term_conc,time_window, " = 0", sep = ""), test = "Chisq", vcov = cluster.vcov(m1b_3, analysis_data$cowcode))
  m2a_3_equality <- linearHypothesis(m2a_3, paste(term_conc, "_symbolic",time_window, " = ", term_conc, "_nonsymbolic",time_window, sep = ""), test = "Chisq", vcov = cluster.vcov(m2a_3, analysis_data$cowcode))
  m2b_3_jointsig1 <- linearHypothesis(m2b_3, paste(term_conc, "_symbolic",time_window, " + mnm_party_exists:", term_conc, "_symbolic",time_window, " = 0", sep = ""), test = "Chisq", vcov = cluster.vcov(m2b_3, analysis_data$cowcode))
  m2b_3_jointsig2 <- linearHypothesis(m2b_3, paste(term_conc, "_nonsymbolic",time_window, " + mnm_party_exists:", term_conc, "_nonsymbolic",time_window, " = 0", sep = ""), test = "Chisq", vcov = cluster.vcov(m2b_3, analysis_data$cowcode))
  m2b_3_equality <- linearHypothesis(m2b_3, paste(term_conc, "_symbolic",time_window, " + mnm_party_exists:", term_conc, "_symbolic", time_window, " = ", term_conc, "_nonsymbolic",time_window, " + mnm_party_exists:", term_conc, "_nonsymbolic",time_window, sep = ""), test = "Chisq", vcov = cluster.vcov(m2b_3, analysis_data$cowcode))

  ## reporting results
  print("Exporting results ...")
  stargazer(m1a_3, m1b_3, m2a_3, m2b_3,  se = c(cl_m1a_3, cl_m1b_3, cl_m2a_3, cl_m2b_3),
            type="text", dep.var.labels.include = F,  order=vars.order,  model.names = F, model.numbers = F,
            column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6"),
            title=paste("Ethnic accommodation and the number of mobilization events involving the dominant group", model_name, ".", sep =""),
            omit=c("cowcodef", "yearf","nobacklash_months_l1","nobacklash_violent_months_l1"), style = "ajps",
            star.cutoffs = c(.1, .05, .01, .001), star.char = c("†", "*", "**","***"),
            add.lines = list(c("Country-FE","yes","yes","yes","yes"),
                             c("Year-FE","yes","yes","yes","yes"),
                             c("Wald-Test Chisq","","","",""),
                             c("Joint sig. int. concession","",add_stars(round(m1b_3_jointsig[4][2,],3)),"",""),
                             c("Joint sig. int. concession (group-based)","","","",add_stars(m2b_3_jointsig1[4][2,])),
                             c("Joint sig. int. concession (group-blind)","","","",add_stars(m2b_3_jointsig2[4][2,]))
            ),
            covariate.labels = c(labels_conc, labels_dnp, labels_controls),
            notes = c("† p<0.1; * p<0.05; ** p<0.01; *** p<0.001; country-clustered SE's in parentheses; cubic terms for group-wise months without mobilization included but not reported."), notes.append=FALSE,
            out = c(paste("./tables/results/",model_label,".html", sep = ""), paste("./tables/results/",model_label,".tex", sep = ""))
  )

  ## Calculating marginal effect
  print("Calculating marginal effects ...")
        
  m1a_3_me <- avg_comparisons(m1a_3, variables = setNames(list(c(0,1)), paste(term_conc,time_window, sep ="")), vcov = cluster.vcov(m1a_3, as.integer(analysis_data$cowcode)))
  m1a_3_me$term <- "concession number"
  m1a_3_me$mnm_partyno <- 0
  m1b_3_me <- avg_comparisons(m1b_3, variables = setNames(list(c(0,1)), paste(term_conc,time_window, sep ="")), by = "mnm_party_exists", vcov = cluster.vcov(m1b_3, as.integer(analysis_data$cowcode)))
  m1b_3_me$term <- "concession number"
  m1b_3_me$mnm_partyno <- seq(-1, -2)
  m2a_3_me <- avg_comparisons(m2a_3, variables = setNames(list(c(0,1), c(0,1)), c(paste(term_conc, "_symbolic",time_window, sep =""), paste(term_conc, "_nonsymbolic",time_window, sep =""))), vcov = cluster.vcov(m2a_3, as.integer(analysis_data$cowcode)))
  m2a_3_me$term <- c("concession number (group-blind)", "concession number (group-based)")
  m2a_3_me$mnm_partyno <- 0
  m2b_3_me <- avg_comparisons(m2b_3, variables = setNames(list(c(0,1), c(0,1)), c(paste(term_conc, "_symbolic",time_window, sep =""), paste(term_conc, "_nonsymbolic",time_window, sep =""))), by = "mnm_party_exists", vcov = cluster.vcov(m2b_3, as.integer(analysis_data$cowcode)))
  m2b_3_me$term <- rep(c("concession number (group-blind)", "concession number (group-based)"),each=2)
  m2b_3_me$mnm_partyno <- rep(seq(-1,-2),2)

  ## Wald test that marginal effect differs depending on presence of DNP
  m1b_3_me_int_diff <- avg_comparisons(m1b_3, variables = setNames(list(c(0,1)), paste(term_conc,time_window, sep ="")), by = "mnm_party_exists", vcov = cluster.vcov(m1b_3, as.integer(analysis_data$cowcode)), hypothesis = "pairwise", joint = TRUE)
  m2b_3_me_int_diff1 <- avg_comparisons(m2b_3, variables = setNames(list(c(0,1)), paste(term_conc, "_symbolic",time_window, sep ="")), by = "mnm_party_exists", vcov = cluster.vcov(m2b_3, as.integer(analysis_data$cowcode)), hypothesis = "pairwise", joint = TRUE)
  m2b_3_me_int_diff2 <- avg_comparisons(m2b_3, variables = setNames(list(c(0,1)), paste(term_conc, "_nonsymbolic",time_window, sep ="")), by = "mnm_party_exists", vcov = cluster.vcov(m2b_3, as.integer(analysis_data$cowcode)), hypothesis = "pairwise")

  ## Marginal effects graph
  m_me <- rbind.fill(m1a_3_me, m2a_3_me, m1b_3_me, m2b_3_me)
  m_me$mnm_partyno <- ifelse(m_me$term == "concession number", m_me$mnm_partyno + 0.2, m_me$mnm_partyno)
  m_me$mnm_partyno <- ifelse(m_me$term == "concession number (group-blind)", m_me$mnm_partyno - 0.2, m_me$mnm_partyno)
  m_me$upper = m_me$estimate + 1.645 * m_me$std.error
  m_me$lower = m_me$estimate - 1.645 * m_me$std.error
  m_me$AME = m_me$estimate
  m_me_plot <- ggplot(data = subset(m_me, mnm_partyno <= 5.2))+
    geom_errorbar(mapping=aes(y=mnm_partyno, xmin=upper, xmax=lower, group = factor(term), colour = factor(term)), width=0.15, size=0.8) +
    geom_point(mapping=aes(y=mnm_partyno, x=AME, group = factor(term), colour = factor(term), fill = factor(term), shape = factor(term)), size=2) +
    theme_bw() + theme(plot.title = element_text(size=12), axis.title=element_text(size=8)) +
    geom_vline(xintercept = 0, colour = "grey60", linetype = 2) +
    theme(text=element_text(family="Times"), plot.title =element_text(size=12), legend.position = "bottom",
          plot.caption=element_text(hjust = 0.5, size = 7)) +
    ylab("dominant nationalist party (DNP)") + xlab("change in predicted number of dominant group mobilization events") +
    scale_colour_manual(values = c("#8856a7","#e34a33","#2b8cbe"))+
    scale_fill_manual(values = c("#8856a7","#e34a33","#2b8cbe")) +
    guides(colour = guide_legend(reverse = F, title=NULL), fill = guide_legend(reverse = F, title=NULL), shape = guide_legend(reverse = F, title=NULL) ) +
    scale_y_continuous(breaks=c(0, -1, -2), labels=c("unconditional\n(models 1, 3)", "no\n(models 2, 4)", "yes\n(models 2, 4)")) +
    labs(caption = paste("Wald test for difference in AME between DNP = 1 vs. DNP = 0:\n",
                         "concession number: ", add_stars(m1b_3_me_int_diff[5][1,]),
                         "; concession number (group-based): ", add_stars(m2b_3_me_int_diff1[5][1,]),
                         "; concession number (group-blind): ", add_stars(m2b_3_me_int_diff2[5][1,]),
                         sep=""
    ))
  ggsave(m_me_plot, file=paste("./figures/margins/",model_label,".pdf", sep=""), width = 17, height = 7, units="cm",dpi=600)
  ggsave(m_me_plot, file=paste("./figures/margins/",model_label,".png", sep=""), width = 17, height = 7, units="cm",dpi=600)
  
  print(paste("Done. Results tables and figures in 'figures_margins' and 'tables/results' folders, prefixed by '", model_label, "'.", sep=""))
  
  list("model_results" = list(m1a_3, m1b_3, m2a_3, m2b_3),
       "model_ses" = c(cl_m1a_3, cl_m1b_3, cl_m2a_3, cl_m2b_3),
       "wald_tests_coefs" = c(m1b_3_jointsig, m2a_3_equality, m2b_3_jointsig1, m2b_3_jointsig2, m2b_3_equality),
       "margins" = m_me,
       "wald_tests_margins" = c(m1b_3_me_int_diff, m2b_3_me_int_diff1, m2b_3_me_int_diff2),
       "margins_plot" = m_me_plot
  )
  
  
  })
}


get_texreg <- function(mod, data = NULL) {
  
  # Recode
  
  if (class(mod) == "fixest") {
    
    if (is.null(data)) {
      stop("Data used for model fitting needed in order to compute n. treated!\n")
    }
    
    # Extract treatment variable
    treatment <- as.character(mod$fml)[3]
    treatment <- gsub("0 \\+ ", "", treatment)
    
    # Extract selection of obs. used in model
    if (is.null(unlist(mod$obs_selection))) {
      
      vec_selection <- rep(TRUE, nrow(data))
      
    } else {
      
      vec_selection <- unlist(mod$obs_selection)
    }
    
    # Compute n. treated units
    if (length(na.omit(unique(pull(data[, treatment])))) == 2){
      
      n_treated <- data[vec_selection, ][data[vec_selection, treatment] == 1, mod$fixef_vars[1]] %>% 
        distinct() %>% 
        nrow()
      
    } else {
      
      n_treated <- NA
      
    }
    
    # Extract object
    tx_obj <- texreg:::createTexreg(
      coef = mod$coefficients[which(str_detect(names(mod$coefficients), treatment))],
      coef.names =  names(mod$coefficients)[which(str_detect(names(mod$coefficients), treatment))],
      se = mod$se[which(str_detect(names(mod$coefficients), treatment))],
      pvalues = mod$coeftable[which(str_detect(names(mod$coefficients), treatment)), 4],
      gof.names = c("N. obs.", "N. treated"),
      gof.decimal = c(FALSE, FALSE),
      gof = c(
        mod$nobs,
        n_treated
      )
    )
    
  } else if (class(mod) == "fect") {
    
    
    # Extract object
    tx_obj <- texreg:::createTexreg(
      coef = mod$est.avg[1],
      coef.names = mod$D,
      se = mod$est.avg[2],
      pvalues = mod$est.avg[5],
      gof.names = c("Total units", "Treated units", "Years"),
      gof.decimal = c(FALSE, FALSE, FALSE),
      gof = c(
        mod$N,
        mod$Ntr,
        mod$T
      )
    )
    
  }
  
  # return texreg object
  return(tx_obj)
  
}


grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + 
                    theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x +
                 theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl), 
                                            legend,ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend, ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  invisible(combined)
}
