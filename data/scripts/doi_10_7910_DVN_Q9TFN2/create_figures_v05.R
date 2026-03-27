# Clear
rm(list = ls(all.names = TRUE)) # will clear all (including hidden) objects.
invisible(gc()) # free up memory
# Packages used for plotting (if not already installed, install them first)
libplt <- c("formattable", "ggplot2", "latex2exp", "reshape", "stringr")
lapply(libplt, require, character.only = TRUE)

# Load workspace of main simulation results
load("simulations/simulations_results_probit_A2_C3_N3_Rho5_R2000_B1000_K3_interceptTRUE_standardizeTRUE_postcv_included.Rdata")

# Renaming coefficient patterns to align better with literature
dimnames(ell2_errs)[["pattern"]] <- c("exactly sparse", "intermediate", "approximately sparse")
dimnames(bhats_stud)[["pattern"]] <- c("exactly sparse", "intermediate", "approximately sparse")

# Plotting results

# Set up black "palette" (for JPE)
blackPalette <- c("#000000", "#000000", "#000000")

# # Set up grayscale color palette
# gsPalette <- c("#000000", "#999999", "#CCCCCC")

# Color-blind friendly color palette (for supplement)
cbPalette <- c("#E69F00", "#56B4E9", "#000000", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# (0) TABULATING AND PLOTTING MISSING VALUES
# Produces numbers reported in Section 6.3.1 of the paper

# Recoding clear cases of non-convergence as NA
ell2_errs[ell2_errs > 1000000] <- NA
bhats_stud[ell2_errs > 1000000] <- NA

# (0.a) Missing ell2 errors (=> numbers in Section 6.3.1)
# By method, pooling all configurations
df_na_ell2_errs <- melt(is.na(ell2_errs))
mytab <- table(df_na_ell2_errs$value, df_na_ell2_errs$method)
percent(signif(proportions(mytab, 2), digits = 4))[2, ]
# Fraction of missing post-CV cases by configuration
margin <- c("rho", "n", "pattern")
mean_na_ell2_postcv <- apply(is.na(ell2_errs[, , , , , , "Post-CV"]), margin, mean)
max(mean_na_ell2_postcv) # worst configuration wrt. NAs (a fraction)

# (0.b) Missing studentized beta estimates (not reported but similar)
# By method, pooling all configurations
df_na_bhats_stud <- melt(is.na(bhats_stud))
mytab <- table(df_na_bhats_stud$value, df_na_bhats_stud$method)
percent(signif(proportions(mytab, 2), digits = 4))[2, ]
# Fraction of missing post-CV cases by configuration
margin <- c("rho", "n", "pattern")
mean_na_bhats_stud_postcv <- apply(is.na(bhats_stud[, , , , , , "Post-CV"]), margin, mean)
max(mean_na_bhats_stud_postcv) # worst configuration wrt. NAs

# Dropping post-CV from further consideration
ell2_errs <- ell2_errs[, , , , , , c("BCV", "Post-BCV", "CV")]
bhats_stud <- bhats_stud[, , , , , , c("BCV", "Post-BCV", "CV")]

# (1) ELL2 ESTIMATION ERROR PLOTS

# Calculate mean and median (ell2) estimation errors
mean_ell2_errs <- colMeans(ell2_errs, na.rm = TRUE)
margin <- seq(2, length(dim(ell2_errs))) # holding all but MC dim fixed
median_ell2_errs <- apply(ell2_errs, margin, median, na.rm = TRUE)
# note: missings (NAs) are dropped here and counts reported in figure notes.

# Create labels (via functions) for plots
nplab <- function(string) {
  sprintf("n, p = %s", string) # note: labeller expects a string - not integer
}
rholab <- function(string) {
  sprintf("rho = %s", string)
}
capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}
percent_fctn <- function(x, digits = 1, format = "g", ...) {
  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
}

# (1.1) COMPARING MEAN ESTIMATION ERRORS ACROSS METHODS FOR GIVEN (c0, alpha(n)) PAIRS
# Produces FIGURE 1 in the paper
# ...  and FIGURE H.2.1 in the supplementary material
do_save <- TRUE
metlist <- c("BCV", "Post-BCV", "CV")
g <- guide_legend(title = "Method:")
scale_linetype <- setNames(c("dashed", "dotdash", "solid"), metlist)
for (thisrule in 1:numrules) { # varying alpha(n)
  switch(alpha_rules[thisrule],
    "bcch" = {
      palette <- blackPalette
      device <- "eps" # for JPE copy editing
    },
    "adhoc" = {
      palette <- cbPalette
      device <- "pdf" # easier to inspect
    }
  )
  for (thisc0 in numc0:numc0) { # fixed c0
    mean_ell2_errs_temp <- mean_ell2_errs[, , , thisc0, thisrule, ]
    df <- melt(mean_ell2_errs_temp)
    ggplot(data = df, aes(x = rho, y = value, group = method)) +
      geom_line(aes(linetype = method, color = method), linewidth = 0.5) +
      geom_point(aes(shape = method, color = method), size = 2) +
      scale_shape(solid = FALSE) +
      facet_grid(pattern ~ n,
                 labeller = labeller(n = nplab, pattern = capitalize)) +
      geom_hline(yintercept = sqrt(2), linetype = "dotted",
                 color = "black", linewidth = 0.5) +
      scale_x_continuous(breaks = seq(rhovec[1], rhovec[numrho], by = .2)) +
      scale_y_continuous(limits = c(0, 2), expand = c(0, 0)) +
      labs(
        y = TeX("$E[\\|\\widehat{\\theta}-\\theta_{0}\\|_{2}]$"),
        x = TeX("$\\rho$")#,
        # caption = paste0("Notes: The dotted black lines correspond to the (constant) error of the all-zeros estimator.",
        #   ifelse(anyNA(ell2_errs[, , , , thisc0, thisrule, "BCV"]),
        #          sprintf("\nBCV cases dropped due to nonconvergence: %d (%s). ",
        #                  sum(is.na(ell2_errs[, , , , thisc0, thisrule, "BCV"])),
        #                  percent_fctn(sum(is.na(ell2_errs[, , , , thisc0, thisrule, "BCV"]))
        #                               / length(ell2_errs[, , , , thisc0, thisrule, "BCV"]))), ""),
        #   ifelse(anyNA(ell2_errs[, , , , thisc0, thisrule, "Post-BCV"]),
        #          sprintf("\nPost-BCV cases dropped due to nonconvergence: %d (%s). ",
        #                  sum(is.na(ell2_errs[, , , , thisc0, thisrule, "Post-BCV"])),
        #                  percent_fctn(sum(is.na(ell2_errs[, , , , thisc0, thisrule, "Post-BCV"]))
        #                               / length(ell2_errs[, , , , thisc0, thisrule, "Post-BCV"]))), ""),
        #   ifelse(anyNA(ell2_errs[, , , , thisc0, thisrule, "CV"]),
        #          sprintf("\nCV cases dropped due to nonconvergence: %d (%s)",
        #                  sum(is.na(ell2_errs[, , , , thisc0, thisrule, "CV"])),
        #                  percent_fctn(sum(is.na(ell2_errs[, , , , thisc0, thisrule, "CV"]))
        #                               / length(ell2_errs[, , , , thisc0, thisrule, "CV"]))), "")
        # )
      ) +
      theme_bw() +
      theme(
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size = rel(0.75), hjust = 0.5),
        legend.text = element_text(size = rel(0.75)),
        legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
        legend.spacing = unit(0, "mm"),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.spacing.y = unit(3, "mm"),
        # plot.caption = element_text(hjust = 0, size = rel(0.75))
      ) +
      guides(linetype = g, color = g, shape = g) +
      scale_colour_manual(values = palette) +
      scale_linetype_manual(values = scale_linetype)
    if (do_save == TRUE) {
      switch(alpha_rules[thisrule],
        "bcch" = {
          fig_no <- "1"
          device <- "eps" # for JPE copy editing
        },
        "adhoc" = {
          fig_no <- "H_2_1"
          device <- "pdf" # easier to inspect
        }
      )
      filename <- sprintf("simulations/img/Figure_%s_%s_mean_ell2_err_BCV_PostBCV_CV_R%d_B%d_K%d_c1dot%s_alpha%s_intercept%s_standardize%s.%s",
                          fig_no, link, nummc, b, k,
                          str_replace(as.character(c0vec[thisc0] - floor(c0vec[thisc0])), "0.", ""),
                          alpha_rules[thisrule], intercept, standardize, device)
      ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
             device = device, units = "cm", dpi = 320)
    }
  }
}

# (1.2) BCV AND POST-BCV MEAN ESTIMATION ERRORS FOR VARYING (c0, alpha(n)) PAIRS
# Produces FIGUREs 2 and 3 in the paper
# ...  and FIGUREs H.2.2 and H.2.3 in the supplementary material
do_save <- TRUE
g <- guide_legend(title = TeX("$c_0 =$"))
scale_linetype <- setNames(c("dashed", "dotdash", "solid"), c0vec)
for (method in c("BCV", "Post-BCV")) { # c0 only enters BCV and Post-BCV
  switch(method,
    "BCV" = {
      y_lim <- c(0.6, 1.6)
    },
    "Post-BCV" = {
      y_lim <- c(0, 2)
    }
  )
  for (thisrule in 1:numrules) { # varying alpha(n)
    switch(alpha_rules[thisrule],
      "bcch" = {
        palette <- blackPalette
      },
      "adhoc" = {
        palette <- cbPalette
      }
    )
    df <- melt(mean_ell2_errs[, , , , thisrule, method])
    ggplot(data = df, aes(x = rho, y = value, group = as.factor(c0))) +
      geom_line(aes(linetype = as.factor(c0), color = as.factor(c0)), linewidth = 0.5) +
      geom_point(aes(shape = as.factor(c0), color = as.factor(c0)), size = 2) +
      scale_shape(solid = FALSE) +
      facet_grid(pattern ~ n,
                 labeller = labeller(n = nplab, pattern = capitalize)) +
      geom_hline(yintercept = sqrt(2), linetype = "dotted",
                 color = "black", linewidth = 0.5) +
      scale_x_continuous(breaks = seq(rhovec[1], rhovec[numrho], by = .2)) +
      scale_y_continuous(limits = y_lim, expand = c(0, 0)) +
      labs(
        y = TeX("$E[\\|\\widehat{\\theta}-\\theta_{0}\\|_{2}]$"),
        x = TeX("$\\rho$")#,
        # caption = paste("Notes: The dotted black lines correspond to the (constant) error of the all-zeros estimator.",
        #   ifelse(method == "BCV" & anyNA(ell2_errs[, , , , , thisrule, "BCV"]),
        #          sprintf("BCV cases dropped due to nonconvergence: %d (%s).",
        #                  sum(is.na(ell2_errs[, , , , , thisrule, "BCV"])),
        #                  percent_fctn(sum(is.na(ell2_errs[, , , , , thisrule, "Post-BCV"]))
        #                               / length(ell2_errs[, , , , , thisrule, "Post-BCV"]))), ""),
        #   ifelse(method == "Post-BCV" & anyNA(ell2_errs[, , , , , thisrule, "Post-BCV"]),
        #          sprintf("Post-BCV cases dropped due to nonconvergence: %d (%s).",
        #                  sum(is.na(ell2_errs[, , , , , thisrule, "Post-BCV"])),
        #                  percent_fctn(sum(is.na(ell2_errs[, , , , , thisrule, "Post-BCV"]))
        #                               / length(ell2_errs[, , , , , thisrule, "Post-BCV"]))), "")
        # )
      ) +
      theme_bw() +
      theme(
        legend.justification = c(1, 1),
        legend.position = c(1, 1),
        legend.key = element_rect(colour = "transparent", fill = NA),
        legend.box.background = element_rect(colour = "black"),
        legend.title = element_text(size = rel(0.75), hjust = 0.5),
        legend.text = element_text(size = rel(0.75)),
        legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
        legend.spacing = unit(0, "mm"),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        panel.spacing.y = unit(3, "mm"),
        plot.caption = element_text(hjust = 0, size = rel(0.5))
      ) +
      guides(linetype = g, color = g, shape = g) +
      scale_colour_manual(values = palette) +
      scale_linetype_manual(values = scale_linetype)
    if (do_save == TRUE) {
      switch(method,
        "BCV" = {
          switch(alpha_rules[thisrule],
            "bcch" = {
              fig_no <- "2"
              device <- "eps" # for JPE copy editing
            },
            "adhoc" = {
              fig_no <- "H_2_2"
              device <- "pdf" # easier to inspect
            }
          )
        },
        "Post-BCV" = {
          switch(alpha_rules[thisrule],
            "bcch" = {
              fig_no <- "3"
              device <- "eps" # for JPE copy editing
            },
            "adhoc" = {
              fig_no <- "H_2_3"
              device <- "pdf" # easier to inspect
            }
          )
        }
      )
      filename <- sprintf("simulations/img/Figure_%s_%s_mean_ell2_%s_err_for_different_c0_R%d_B%d_K%d_alpha%s_intercept%s_standardize%s.%s",
                          fig_no, link, method, nummc, b, k,
                          alpha_rules[thisrule], intercept, standardize, device)
      ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
             device = device, units = "cm")
    }
  }
}

# (2) NORMAL APPROXIMATIONS: KERNEL DENSITY PLOTS

# (2.1) COMPARING APPROXIMATIONS ACROSS SAMPLE/PROBLEM SIZE FOR GIVEN (c0, alpha(n)) PAIRS
# Produces FIGURE 4 in the paper
# ...  and FIGURE H.2.4 in the supplementary material
do_save <- TRUE
metlist <- c("BCV", "Post-BCV", "CV")
g <- guide_legend(title = TeX("$n,p =$"))
scale_linetype <- setNames(c("dashed", "dotdash", "solid"), as.character(nvec))
for (thisrule in 1:numrules) { # varying alpha(n)
  switch(alpha_rules[thisrule],
    "bcch" = {
      palette <- blackPalette
    },
    "adhoc" = {
      palette <- cbPalette
    }
  )
  for (thisc0 in numc0:numc0) { # fixed c0
    for (thisrho in 1:1) {
      df <- melt(bhats_stud[, thisrho, , , thisc0, thisrule, ])
      ggplot(df, aes(x = value, color = as.factor(n))) +
        stat_density(aes(linetype = as.factor(n)),
                     kernel = "gaussian", bw = "nrd0",
                     geom = "line", position = "identity") +
        facet_grid(pattern ~ method, labeller = labeller(pattern = capitalize)) +
        labs(y = "Density") +
        theme_bw() +
        theme(
          legend.justification = c(1, 1), 
          legend.position = c(1, 1),
          legend.key = element_rect(colour = "transparent", fill = NA),
          legend.box.background = element_rect(colour = "black"),
          legend.title = element_text(size = rel(0.75), hjust = 0.5),
          legend.text = element_text(size = rel(0.75)),
          legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
          legend.spacing = unit(0, "mm"),
          legend.spacing.x = unit(0, "mm"),
          legend.spacing.y = unit(0, "mm"),
          panel.spacing.y = unit(3, "mm"),
          plot.title = element_text(size = rel(0.5)),
          plot.caption = element_text(hjust = 0, size = rel(0.5)),
          axis.title.x = element_blank()
        ) +
        scale_x_continuous(limits = c(min(min(df$value), -5), max(max(df$value), 5))) +
        coord_cartesian(xlim = c(-5, 5), ylim = c(0, 0.5)) +
        guides(linetype = g, color = g) +
        scale_colour_manual(values = palette) +
        scale_linetype_manual(values = scale_linetype) +
        stat_function(fun = dnorm, linetype = "dotted", color = "black")
      if (do_save == TRUE) {
        switch(alpha_rules[thisrule],
          "bcch" = {
            fig_no <- "4"
            device <- "eps" # for JPE copy editing
          },
          "adhoc" = {
            fig_no <- "H_2_4"
            device <- "pdf" # easier to inspect
          }
        )
        filename <-
          sprintf("simulations/img/Figure_%s_%s_normal_approx_by_n_each_method_and_pattern_R%d_B%d_K%d_c1dot%s_alpha%s_rhodot%s_intercept%s_standardize%s.%s",
                  fig_no, link, nummc, b, k,
                  str_replace(as.character(c0vec[thisc0] - floor(c0vec[thisc0])), "0.", ""),
                  alpha_rules[thisrule], str_replace(as.character(rhovec[thisrho]), "0.", ""),
                  intercept, standardize, device)
        ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
               device = device, units = "cm")
      }
    }
  }
}

# (2.2) COMPARING POST-BCV AND CV APPROXIMATIONS FOR DIFFERENT rho LEVELS
# Produces FIGURE 5 in the paper
# ...  and FIGURE H.2.5 in the supplementary material
do_save <- TRUE
g <- guide_legend(title = "Method:")
metlist <- c("Post-BCV", "CV")
gsPalette <- c("#000000", "#999999")
scale_linetype <- setNames(c("solid", "dashed"), metlist)
for (thisrule in 1:numrules) { # varying alpha(n)
  switch(alpha_rules[thisrule],
    "bcch" = {
      palette <- blackPalette
    },
    "adhoc" = {
      palette <- cbPalette
    }
  )
  for (thisc0 in numc0:numc0) {
    for (thispat in numpat:numpat) {
      for (thisn in numn:numn) {
        df <- melt(bhats_stud[, -1, thisn, thispat, thisc0, thisrule, metlist])
        ggplot(df, aes(x = value)) +
          stat_density(aes(linetype = method, color = method),
                       kernel = "gaussian", bw = "nrd0",
                       geom = "line", position = "identity") +
          facet_wrap(~ rho, nrow = 3, labeller = labeller(rho = rholab)) +
          labs(y = "Density") +
          theme_bw() +
          theme(
            legend.justification = c(1, 1),
            legend.position = c(1, 1),
            legend.key = element_rect(colour = "transparent", fill = NA),
            legend.box.background = element_rect(colour = "black"),
            legend.title = element_text(size = rel(0.75), hjust = 0.5),
            legend.text = element_text(size = rel(0.75)),
            legend.margin = margin(c(0, 0, 0, 0), unit = "mm"),
            legend.spacing = unit(0, "mm"),
            legend.spacing.x = unit(0, "mm"),
            legend.spacing.y = unit(0, "mm"),
            panel.spacing.y = unit(3, "mm"),
            plot.title = element_text(size = rel(0.5)),
            plot.caption = element_text(hjust = 0, size = rel(0.5)),
            axis.title.x = element_blank()
          ) +
          scale_x_continuous(limits = c(min(min(df$value), -5), max(max(df$value), 5))) +
          coord_cartesian(xlim = c(-5, 5), ylim = c(0, 0.5)) +
          guides(linetype = g, color = g) +
          scale_colour_manual(values = palette) +
          scale_linetype_manual(values = scale_linetype) +
          stat_function(fun = dnorm, linetype = "dotted", color = "black")
        if (do_save == TRUE) {
          switch(alpha_rules[thisrule],
            "bcch" = {
              fig_no <- "5"
              device <- "eps" # for JPE copy editing
            },
            "adhoc" = {
              fig_no <- "H_2_5"
              device <- "pdf" # easier to inspect
            }
          )
          filename <-
            sprintf("simulations/img/Figure_%s_%s_normal_approx_Post-BCV_vs_CV_for_different_rho_R%d_B%d_K%d_%s_np%d_c1dot%s_alpha%s_intercept%s_standardize%s.%s",
                    fig_no, link, nummc, b, k, patlist[thispat], nvec[thisn],
                    str_replace(as.character(c0vec[thisc0] - floor(c0vec[thisc0])), "0.", ""),
                    alpha_rules[thisrule], intercept, standardize, device)
          ggsave(filename, height = (2 / 3) * 22.85, width = 16.5,
                 device = device, units = "cm")
        }
      }
    }
  }
}
