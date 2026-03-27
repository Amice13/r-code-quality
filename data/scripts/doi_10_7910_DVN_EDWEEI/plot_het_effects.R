#' Plot Treatment Effect Heterogeneity by Moderator Variables
#'
#' Creates faceted plots showing how treatment effects vary across
#' different moderator variables and vulnerability levels.
#'
#' @param ame_in Matrix. Average marginal effects data with moderator values
#' @param het_lab Character. Label for heterogeneity moderator variable
#' @param file_name Character. Base name for output file
#' @param newdata Logical. Whether using newdata prediction approach (default: TRUE)
#' @param fig_label Character. Figure label
#' @param ylb Numeric. Lower y-axis limit (default: -0.03)
#' @param yub Numeric. Upper y-axis limit (default: 0.03)
#' @param breakfact Numeric. Factor for y-axis break spacing (default: 1)
#' @param figheight Numeric. Figure height in inches (default: 4.5)
#' @param N Integer. Sample size for caption
#' @param zones Integer. Number of zones for caption
#' @param countries Integer. Number of countries for caption
#' @param moderator_note Character. Additional note about moderator (default: "")
#'
#' @return Invisible. Function called to save plot
#'
#' @details
#' Function handles multiple moderator types: democracy, education, cropland.
#' Requires global quantile objects: dem_qtl, crop_qtl, info_qtl, elec_qtl, assoc_qtl.
#' Saves plots to Output/figures/ with prefix "fig_wrp_het_".
#'
#' @note Requires global variables: fwidth, fscale, fdpi, lwidth, ptsize, lab_*
plot_het_effects <- function(ame_in, het_lab, file_name, fig_label, newdata = TRUE, ylb = -.03, yub = 0.03,
                             breakfact = 1, figheight = 4.5,
                             N, zones, countries, moderator_note = "") {
  
  ame_df <- lapply(seq_len(ncol(ame_in)), function(i) data.frame(ame_in[, i]))
  ame_df <- do.call(rbind, ame_df)
  ame_df <- ame_df %>%
    dplyr::mutate(
      reg_loser_50 = ifelse(reg_loser_50 == 1, "Severe damage", "Limited damage"),
      term = case_when(
        term == "best_tanom_7d_z" ~ paste0("(c) ", lab_best_7d),
        term == "best_tanom_2sd_7d_z" ~ paste0("(a) ", lab_best_2sd),
        term == "modis_burnanomp_mu_6m_w1_z" ~ paste0("(b) ", lab_fire),
        term == "noaa_cpc_tdev_7d_z" ~ paste0("(d) ", lab_noaa_7d)
      )
    )
  if ("v2x_polyarchy" %in% names(ame_df)) {
    ame_df <- ame_df %>%
      dplyr::mutate(
        v2x_polyarchy = factor(
          case_when(
            v2x_polyarchy == dem_qtl[1] ~ "Q1",
            v2x_polyarchy == dem_qtl[2] ~ "Q2",
            v2x_polyarchy == dem_qtl[3] ~ "Q3",
            v2x_polyarchy == dem_qtl[4] ~ "Q4"
          )
        )
      )
  }
  if ("v2x_freexp_altinf" %in% names(ame_df)) {
    ame_df <- ame_df %>%
      dplyr::mutate(
        v2x_freexp_altinf = factor(
          case_when(
            v2x_freexp_altinf == info_qtl[1] ~ "Q1",
            v2x_freexp_altinf == info_qtl[2] ~ "Q2",
            v2x_freexp_altinf == info_qtl[3] ~ "Q3",
            v2x_freexp_altinf == info_qtl[4] ~ "Q4"
          )
        )
      )
  }
  if ("v2xel_frefair" %in% names(ame_df)) {
    ame_df <- ame_df %>%
      dplyr::mutate(
        v2xel_frefair = factor(
          case_when(
            v2xel_frefair == elec_qtl[1] ~ "Q1",
            v2xel_frefair == elec_qtl[2] ~ "Q2",
            v2xel_frefair == elec_qtl[3] ~ "Q3",
            v2xel_frefair == elec_qtl[4] ~ "Q4"
          )
        )
      )
  }
  if ("v2x_frassoc_thick" %in%names(ame_df)) {
    ame_df <- ame_df %>%
      dplyr::mutate(
        v2x_frassoc_thick = factor(
          case_when(
            v2x_frassoc_thick == assoc_qtl[1] ~ "Q1",
            v2x_frassoc_thick == assoc_qtl[2] ~ "Q2",
            v2x_frassoc_thick == assoc_qtl[3] ~ "Q3",
            v2x_frassoc_thick == assoc_qtl[4] ~ "Q4"
          )
        )
      )
  }
  if (het_lab == "Cropland") {
    ame_df <- ame_df %>%
      dplyr::mutate(
        crop = factor(
          case_when(
            crop == crop_qtl[1] ~ "Q1",
            crop == crop_qtl[2] ~ "Q2",
            crop == crop_qtl[3] ~ "Q3",
            crop == crop_qtl[4] ~ "Q4"
          )
        )
      )
  }
  if (het_lab == "Education") {
    ame_df <- ame_df %>%
      filter(edu != "Other")
  }
  if (newdata) {
    names(ame_df)[5] <- "modvar"
  } else {
    names(ame_df)[4] <- "modvar"
  }
  p_het <- ame_df %>%
    ggplot(aes(x = modvar, y = estimate, ymin = conf.low, ymax = conf.high, color = reg_loser_50)) +
    geom_hline(yintercept = 0, color = "grey") +
    geom_pointrange(position = position_dodge(.25), linewidth = lwidth, size = ptsize) +
    theme_classic(base_size = 14) +
    scale_color_manual(values = c("blue", "black"), breaks = c("Limited damage", "Severe damage")) +
    scale_y_continuous(limits = c(ylb, yub), breaks = seq(ylb, yub, .01 * breakfact)) +
    facet_wrap(~term) +
    labs(
      color = "",
      x = paste0(
        "Average effect of +1SD climate shock on climate risk perceptions\n",
        "moderated by ",
        het_lab
      ),
      y = "Average effect",
      caption = str_wrap(
        paste(
          "Notes: Bars denote 95% confidence intervals with robust standard errors clustered by administrative zone.",
          "The outcome indicates whether respondents identify climate change as a top or major risk in their daily lives.",
          moderator_note,
          N, "respondents across", zones, "zones in", countries, "countries.",
          sep = " "
        ),
        138
      )
    ) +
    theme(
      panel.grid = element_blank(),
      legend.background = element_rect(fill = "transparent"),
      legend.position = "bottom",
      axis.ticks = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 14, margin = margin(b = 10)),
      axis.text = element_text(color = "black"),
      axis.title.x = element_text(margin = margin(t = 15)),
      axis.title.y = element_text(margin = margin(r = 15)),
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "plot",
      strip.placement = "outside",
      panel.spacing.y = unit(3, "lines")
    )
  ggsave(
    p_het,
    filename = here("Output", "figures", paste0("fig_", fig_label, "_wrp_het_", file_name, ".pdf")),
    width = fwidth,
    height = figheight,
    scale = fscale
  )
}
