#############################
# PLOTTING OF MONTE-CARLO RESULTS: RE UNCERTAINTY
# 
# 
# Part of:
# Shaping States into Nations: The Effects of Ethnic Geography on State Borders
# Müller-Crepon, Schvitz, Cederman
# Replication files
#
# Called from scripts/analysis/analysis_all.R
#
#############################



# UNCERTAINTY: BOOTSTRAPPED SEs ##


## Load BOOTSE data
mc_bootse_path <- c(file.path("data/monte_carlo_res/mc_bootse"))

## Read parameters and results
mc_param_tb <- do.call(rbind, lapply(mc_bootse_path, function(p){
  as_tibble(readRDS(file.path(p, "parameters.rds")))
}))
mc_results_tb <- do.call(rbind, lapply(mc_bootse_path, function(p){
  as_tibble(readRDS(file.path(p, "results.rds")))
}))

## Check 
stopifnot(nrow(mc_param_tb) == nrow(mc_results_tb))


# Compute bias + 95% CI hits
mc_eval_tb <- bind_cols(mc_param_tb, mc_results_tb)
mc_eval_tb <- mc_eval_tb %>%
  mutate(b0_se_hit = covered(beta0, b0, b0.se), 
         b1_se_hit = covered(beta1, b, b.se),
         b0_basic_hit = covered_bs(beta0, b0.bootci.basic.low, b0.bootci.basic.high), 
         b1_basic_hit = covered_bs(beta1, b1.bootci.basic.low, b1.bootci.basic.high),
         b0_perc_hit = covered_bs(beta0, b0.bootci.perc.low, b0.bootci.perc.high), 
         b1_perc_hit = covered_bs(beta1, b1.bootci.perc.low, b1.bootci.perc.high),
         b0_err = b0 - beta0,
         b1_err = b - beta1, 
         b0_se_valid = is.finite(b0.se), 
         b1_se_valid = is.finite(b.se))

# Bootstrap Hits wide->long
mc_b0_long <- mc_eval_tb %>%
  select(c(seed, beta0, beta1, num_instances, b0_basic_hit, b0_perc_hit, b0_se_hit, b0, b)) %>% 
  mutate(standard = b0_se_hit, basic = b0_basic_hit, perc = b0_perc_hit) %>%
  select(-c(b0_se_hit, b0_basic_hit, b0_perc_hit)) %>%
  tidyr::gather(ci_method, b0_hit, basic, perc, standard)

mc_b1_long <- mc_eval_tb %>%
  select(seed, beta0, beta1, num_instances, b1_basic_hit, b1_perc_hit, b1_se_hit, b0, b) %>% 
  mutate(standard = b1_se_hit, basic = b1_basic_hit, perc = b1_perc_hit) %>%
  select(-c(b1_se_hit, b1_basic_hit, b1_perc_hit)) %>%
  tidyr::gather(ci_method, b1_hit, basic, perc, standard)

mc_long_tb <- mc_b0_long %>% left_join(mc_b1_long)

## Only keep standard and bootstrapped percentile based 
mc_long_tb <- mc_long_tb[mc_long_tb$ci_method %in% c("standard", "perc"),]
mc_long_tb$ci_label <- ifelse(mc_long_tb$ci_method == "standard", 
                              "Max. Comp. Lik. CI", "Parametric Bootstrap CI")
mc_long_tb$ci_label <- factor(mc_long_tb$ci_label, 
                              levels = c("Max. Comp. Lik. CI", "Parametric Bootstrap CI"), 
                              ordered = T)

# Prepare Plot
## Beta 0 coverage
scale <- .25
agg_tb0 <- mc_long_tb %>%
  group_by(beta0, beta1, ci_label) %>%
  summarize(cvg_diff = mean(b0_hit - 0.95, na.rm = TRUE)) %>% 
  ungroup()
agg_tb0$parameter <- "CI of Beta 0"

## Beta 1 coverage
agg_tb1 <- mc_long_tb %>%
  group_by(beta0, beta1, ci_label) %>%
  summarize(cvg_diff = mean(b1_hit - 0.95, na.rm = TRUE)) %>% ungroup()
agg_tb1$parameter <- "CI of Beta 1"

## Combined coverage plot -- bar chart
agg_tb <- rbind(agg_tb0, agg_tb1)
agg_tb$beta1 <- factor(agg_tb$beta1, levels = c(2,1,0), ordered = T)

## Drop native CI
agg_tb <- agg_tb[agg_tb$ci_label != "Max. Comp. Lik. CI",]

## Add 95% confidence interval
set.seed(0)
N <- 100
agg_tb$pval <- sapply(N*(.95 + agg_tb$cvg_diff),
                      function(v){
                        binom.test(v, N, p= .95,alternative = "two.sided")$p.value
                      })
agg_tb$upval <- sapply(N*(.95 + agg_tb$cvg_diff),
                      function(v){
                        binom.test(v, N, p= .95,alternative = "two.sided")$conf.int[2]
                      })
agg_tb$loval <- sapply(N*(.95 + agg_tb$cvg_diff),
                      function(v){
                        binom.test(v, N, p= .95,alternative = "two.sided")$conf.int[1]
                      })


# Plot 

## Beta 1 and 2
g_ls <- lapply(c(0,1), function(b){
  if(b == 0){
    agg_tb <- agg_tb[agg_tb$parameter == "CI of Beta 0", ]
  } else {
    agg_tb <- agg_tb[agg_tb$parameter == "CI of Beta 1", ]
  }
  
  g <- ggplot(agg_tb, aes(ci_label, y = .95,
                          group = ci_label)) + 
    geom_hline(yintercept = 0.95, lty = 2, color = "darkgrey") +
    geom_segment(aes(xend = ci_label, yend = .95 + cvg_diff), 
                 color = "black",
                 lwd = 5) +
    geom_segment(aes(xend = ci_label, y = upval, yend = loval), 
                 color = "grey",
                 lwd = 1) +
    geom_point(aes(y = .95 + cvg_diff), col = "grey") +
    theme_minimal() +
    facet_grid(beta1 ~ parameter) +
    ylab("Coverage")  +
    ylim(c(.75, 1.00)) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          legend.title = element_blank(),
          legend.position = "none",
          panel.spacing = unit(.8, "lines")) +
    NULL
  if(b == 1){
    g <- g  +
      ggtitle("Beta 1 CI Coverage")+
      facet_grid(beta1 ~ beta0, labeller = facet_labs) +
      theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      NULL
  } else {
    g <- g  +
      ylab(paste0("CI Coverage")) +
      ggtitle("Beta 0  CI Coverage")+
      facet_grid(beta1 ~ beta0, 
                 labeller = function(x){facet_labs(x, dict = c(beta0 = "Beta 0", beta1 = NA))}) +
      NULL
  }
  g
})


# Save
png(file.path(fig.path, "inference_se_cov.png"), width = 6, height = fig.height,
    res = 400, units = "in")
par(mar = c(0,0,0,0), mfrow = c(3,1))
grid.arrange(g_ls[[1]], g_ls[[2]], nrow = 1, widths = c(.55,.45))
dev.off()
