# Replication package for 
# "Explaining How Human Rights Protections Change After Internal Armed Conflicts"
# Johannes Karreth, Patricia L. Sullivan, Ghazal Dezfuli
# November 30, 2019
# jkarreth@ursinus.edu 
# 
# This file: PC-HRP_Figures.R
# Purpose: Create figures
# Also see: PC_HRP_Analyses.do for analyses
  
library("tidyverse")

# read in the data - note that these are the exact same cases as in the analyses (688 obs total)
d <- read_csv("PC-HRP.csv")

d_cases_years <- select(d, conflictid, yrs_post, styr, endyr, year, conflictcountry, ccode, civtarget_both, govcomp4, hrpchange)
d_cases <- summarize(group_by(d_cases_years, conflictid), ccode = first(ccode), conflictcountry = first(conflictcountry), civtarget_both = first(civtarget_both), hrpchange = first(hrpchange), styr = first(styr), endyr = first(endyr))

# fill up the time series so that I get 10 years before and after conflict

library("xts")
library("zoo")

hrp <- rio::import("Source/HRP/HumanRightsProtectionScores_v3.01.csv")
hrp$V1 <- NULL

hrp[hrp$COW == 316, ]$COW <- 315
hrp[hrp$COW == 530, ]$COW <- 529
hrp344 <- hrp[hrp$COW == 345 & hrp$YEAR < 1991, ]
hrp344$COW <- 344
hrp346 <- hrp[hrp$COW == 345 & hrp$YEAR < 1992, ]
hrp346$COW <- 346
hrp <- rbind(hrp, hrp344, hrp346)
hrp <- arrange(hrp, COW, YEAR)

d_start <- d_cases
d_start$start <- 1

d_end <- d_cases
d_end$end <- 1


viz <- left_join(x = hrp, y = d_start, by = c("COW" = "ccode", "YEAR" = "styr"))
viz <- left_join(x = viz, y = select(d_end, ccode, endyr, end), by = c("COW" = "ccode", "YEAR" = "endyr"))

conflictid_running <- unique(d_cases$conflictid)
vizdat <- list()

for(i in 1:length(conflictid_running)){
vizdat[[i]] <- left_join(x = select(hrp, COW, YEAR, theta_mean, theta_sd), y = filter(d_cases, conflictid == conflictid_running[i]), by = c("COW" = "ccode"))
vizdat[[i]]$viz <- ifelse(vizdat[[i]]$YEAR >= vizdat[[i]]$styr - 10 & vizdat[[i]]$YEAR <= vizdat[[i]]$endyr + 10, 1, 0)
vizdat[[i]]$conflict <- ifelse(vizdat[[i]]$YEAR >= vizdat[[i]]$styr & vizdat[[i]]$YEAR <= vizdat[[i]]$endyr, 1, 0)
vizdat[[i]] <- filter(vizdat[[i]], viz == 1)
}

vizdat_df <- do.call(rbind, vizdat)

vizdat_df[vizdat_df$conflictcountry == "Burundi" & (vizdat_df$YEAR == 1988 | vizdat_df$YEAR == 1991 | vizdat_df$YEAR == 1992 | vizdat_df$YEAR == 1993 | vizdat_df$YEAR == 1997), ]$conflict <- 1

vizdat_df_conflict <- filter(vizdat_df, conflict == 1)

# labels for printing
vizdat_df$civtarg_label <- ifelse(vizdat_df$civtarget_both == 0, "No deliberate targeting",
                                  ifelse(vizdat_df$civtarget_both == 1, "Targeting by gov't",
                                         "Targeting by both gov't and rebels"))

# fix Myanmar for file name
vizdat_df[vizdat_df$conflictcountry == "Burma/Myanmar", ]$conflictcountry <- "Myanmar"
  
# Figures

plots <- list()
for(i in 1:length(conflictid_running)){
  
plots[[i]] <- ggplot(data = vizdat_df[vizdat_df$conflictid == conflictid_running[i], ], aes(x = YEAR, y = theta_mean)) + 
  # geom_ribbon(aes(ymin = theta_mean - theta_sd, ymax = theta_mean + theta_sd), alpha = 0.25) +
  annotate(geom = "rect", xmin = min(vizdat_df_conflict[vizdat_df_conflict$conflictid == conflictid_running[i], ]$YEAR - 0.5), xmax = max(vizdat_df_conflict[vizdat_df_conflict$conflictid == conflictid_running[i], ]$YEAR + 0.5), ymin = -Inf, ymax = Inf, fill = "lightgrey") + 
  geom_line() + 
  geom_line(aes(y = theta_mean - theta_sd), linetype = "dashed", size = 0.25) + 
  geom_line(aes(y = theta_mean + theta_sd), linetype = "dashed", size = 0.25) + 
  # geom_rect(aes(xmin = YEAR, xmax = YEAR, ymin = -Inf, ymax = Inf, fill = factor(conflict)), width = 5) + 
  # facet_wrap(~ conflictid, scales = "free_x") + 
  # geom_segment(aes(y = -Inf, yend = Inf, x = YEAR, xend = YEAR, alpha = factor(conflict)), inherit.aes = FALSE, color = "black", size = 5) + 
  # scale_alpha_discrete(range = c(0,0.3)) + 
  ylim(-4, 2.5) +
  xlab("") + 
  ylab("Human rights protection score") + 
  labs(title = paste(vizdat_df[vizdat_df$conflictid == conflictid_running[i], ]$conflictcountry[1], ": ", vizdat_df[vizdat_df$conflictid == conflictid_running[i], ]$civtarg_label[1], sep = "")) + 
  theme_minimal()

ggsave(plot = plots[[i]], filename = paste("tsplots/ts_", vizdat_df[vizdat_df$conflictid == conflictid_running[i], ]$conflictcountry[1], vizdat_df[vizdat_df$conflictid == conflictid_running[i], ]$endyr[1], ".pdf", sep = ""), device = "pdf", width = 6, height = 4)
}

# Iraq example

# see figure ts_Iraq1988_annot.pdf, annotated by hand

## Figure 1

library("countrycode")

d$continent <- countrycode(sourcevar = d$ccode, origin = "cown", destination = "continent")
d[d$ccode == 315, ]$continent <- "Europe" 
d[d$ccode == 345, ]$continent <- "Europe"
d[d$ccode == 265, ]$continent <- "Europe"
d[d$ccode == 529, ]$continent <- "Africa"
d[d$ccode == 678, ]$continent <- "Asia"
d[d$ccode == 817, ]$continent <- "Asia"
d$lregion <- d$continent

library("ggrepel")

d <- mutate(group_by(d, year, yrs_post), 
            case_year = n())
table(d[d$yrs_post == 1, ]$case_year)

d <- arrange(d, yrs_post, hrpchange)
d$fig1plot_topdrop <- 1:nrow(d)
d <- arrange(d, yrs_post, -hrpchange)
d$fig1plot_toprise <- 1:nrow(d)
d$fig1plot <- ifelse(d$fig1plot_topdrop <= 7 | d$fig1plot_toprise <= 5, 1, 0)

d$civtarg_label <- ifelse(d$civtarget_both == 0, "No deliberate targeting",
                                  ifelse(d$civtarget_both == 1, "Targeting by gov't",
                                         "Targeting by both"))

p_pre5 <- ggplot(data = d[d$yrs_post == 1, ], aes(x = hrp_pre5, y = hrpchange)) + geom_hline(yintercept = 0) + geom_segment(aes(x = hrp_pre5, xend = hrp_pre5, y = 0, yend = hrpchange), alpha = 0.3) + geom_point(shape = 20) + geom_text_repel(data = d[d$fig1plot == 1, ], aes(label = paste(conflictcountry, year))) + theme_minimal() + xlab("HRP before conflict (5-year average)") + ylab("Change in HRP")

ggsave(p_pre5, file = "hrpchange_pre5conflictHRP_year1.pdf", width = 9, height = 6)

p_year <- ggplot(data = d[d$yrs_post == 1, ], aes(x = year, y = hrpchange)) + 
  annotate(geom = "text", x = 1960, y = 1.4, label = "HRP improved after conflict", hjust = 0, size = 8, fontface = "bold", color = "gray") + 
  annotate(geom = "text", x = 1960, y = -2.1, label = "HRP worsened after conflict", hjust = 0, size = 8, fontface = "bold", color = "gray") + 
  geom_hline(yintercept = 0) + 
  geom_segment(aes(x = year, xend = year, y = 0, yend = hrpchange), alpha = 0.3) + 
  geom_point(shape = 20) + 
  geom_text_repel(data = d[d$fig1plot == 1, ], aes(label = paste(conflictcountry, year))) + theme_minimal() + 
  xlab("") + ylab("Change in HRP") + 
  scale_x_continuous(breaks = seq(1960, 2005, by = 10), limits = c(1960, 2005)) 

ggsave(p_year, file = "hrpchange_year_year1.pdf", width = 9, height = 6)

# Table 3 insert

# based on the following commands in Stata: 
# quietly xtreg hrpchange i.civtarget_gov##i.govcomp4 govcas_high yrs_post hrp_pre5 if dupdrop==0 & sample ==1, cl(ccode)
# contrast r.civtarget_gov@govcomp4, mcompare(noadjust) effects post level(90)

fig <- data.frame(contrast = c(-.6744075, -.2126753, -.6789292 ,-.4104277), 
                  outcome = c("... gov't control is unclear, or failed state", "... gov't remains in power", "... power is shared", "... rebels assume power"))

ptab3 <- ggplot(data = fig, aes(x = contrast, y = outcome)) + 
  geom_segment(aes(x = 0, xend = contrast, yend = outcome), arrow = arrow(length = unit(0.1, "in"))) + 
  annotate(geom = "text", x = fig$contrast[1] - 0.01, y = fig$outcome[1], label = "*", size = 10) +
  annotate(geom = "text", x = fig$contrast[3] - 0.01, y = fig$outcome[3], label = "*", size = 10) +
  annotate(geom = "text", x = fig$contrast[4] - 0.01, y = fig$outcome[4], label = "*", size = 10) +
  labs(title = "Difference in HRP change compared to conflicts\nwhere government did NOT target civilians",
       caption = "Based on estimates in Table 3.\nAsterisks indicate a difference significant at p < 0.05, one-tailed tests,\nderived from standard errors clustered by conflict countries.") + 
  xlab("Lower values mean human rights protection dropped after conflicts where governments targeted civilians.            0 = no difference") + 
  ylab("If gov't targeted civilians and ...") + 
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0), axis.text.y = element_text(color = "black"))

ggsave(ptab3, file = "table3.pdf", width = 10, height = 3)

# Same, but with conflictoutcome instead of govcomp

fig <- data.frame(contrast = c(.1302752, -.5742337, -.8467299), 
                  outcome = c("government victory", "settlement", "rebel victory"))

ptab3_memo <- ggplot(data = fig, aes(x = contrast, y = outcome)) + 
  geom_segment(aes(x = 0, xend = contrast, yend = outcome), arrow = arrow(length = unit(0.1, "in"))) + 
  annotate(geom = "text", x = fig$contrast[2] - 0.01, y = fig$outcome[2], label = "*", size = 10) +
  annotate(geom = "text", x = fig$contrast[3] - 0.01, y = fig$outcome[3], label = "*", size = 10) +
  labs(title = "Difference in HRP change compared to conflicts\nwhere government did NOT target civilians",
       caption = "Based on alternative specification of Model (1) in Table 2.\nAsterisks indicate a difference significant at p < 0.05, one-tailed tests,\nderived from standard errors clustered by conflict countries.") + 
  xlab("Lower values mean human rights protection dropped after conflicts where governments targeted civilians.            0 = no difference") + 
  ylab("If gov't targeted civilians and conflict ends with ...") + 
  theme_minimal() +
  theme(axis.title.y = element_text(angle = 0), axis.text.y = element_text(color = "black"))

ggsave(ptab3_memo, file = "table3_memo.pdf", width = 10, height = 3)

# Figure 4 (robustness test summary)

tab45 <- read_csv("tab45_summary.csv")

p_tab45 <- ggplot(data = tab45, aes(x = Coef, y = factor(ypos))) + 
  geom_segment(aes(x = Lower, xend = Upper, y = factor(ypos), yend = factor(ypos))) + 
  geom_point() + 
  scale_y_discrete(labels = c("5   |   5", 
                              "5   |   6", 
                              "5   |   7", 
                              "5   |   8", 
                              "5   |   9", 
                              "5   |   10", 
                              "6   |   10", 
                              "7   |   10", 
                              "8   |   10", 
                              "9   |   10", 
                              "10  |  10" )) + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  xlim(-0.7, 0.1) + 
  ylab("") + xlab("") + 
  # labs(title = "Government targeting of civilians reduces change in HRP scores after conflict by ...") + 
  theme_minimal() + 
  theme(axis.text.y = element_text(color = "black", hjust = 0.5))

ggsave(p_tab45, file = "table4_insert.pdf", width = 6, height = 2)

sessionInfo()

# R version 3.6.1 (2019-07-05)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS Catalina 10.15.2

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#  [1] xts_0.11-2      zoo_1.8-7       forcats_0.4.0   stringr_1.4.0   dplyr_0.8.3     purrr_0.3.3     readr_1.3.1    
#  [8] tidyr_1.0.0     tibble_2.1.3    ggplot2_3.2.1   tidyverse_1.3.0

# loaded via a namespace (and not attached):
#  [1] Rcpp_1.0.3       cellranger_1.1.0 pillar_1.4.3     compiler_3.6.1   dbplyr_1.4.2     tools_3.6.1      zeallot_0.1.0   
#  [8] jsonlite_1.6     lubridate_1.7.4  lifecycle_0.1.0  nlme_3.1-143     gtable_0.3.0     lattice_0.20-38  pkgconfig_2.0.3 
# [15] rlang_0.4.2      reprex_0.3.0     cli_2.0.1        rstudioapi_0.10  DBI_1.1.0        haven_2.2.0      withr_2.1.2     
# [22] xml2_1.2.2       httr_1.4.1       fs_1.3.1         generics_0.0.2   vctrs_0.2.1      hms_0.5.3        grid_3.6.1      
# [29] tidyselect_0.2.5 glue_1.3.1       R6_2.4.1         fansi_0.4.1      readxl_1.3.1     modelr_0.1.5     magrittr_1.5    
# [36] backports_1.1.5  scales_1.1.0     rvest_0.3.5      assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.5    lazyeval_0.2.2  
# [43] munsell_0.5.0    broom_0.5.3      crayon_1.3.4   