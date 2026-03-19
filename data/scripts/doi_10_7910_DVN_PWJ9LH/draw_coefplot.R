## file to draw coefplots and run MH tests
## NOTE: you will need to install the below packages using install.packages()
### if you have not already done so
library(dplyr)
library(ggplot2)
library(foreach)
library(ggthemes)
wd <- "C:/Users/Chase Eck/Dropbox/research/hrsprojects/replication_package"
setwd(wd)
# read in estimates
est <- haven::read_dta("output/event_study_regsave/coefplot_estimates.dta")

create_label_vars <- function(df) {

    df <- df %>% 
    mutate(t_lab = ifelse(
        t == 0, 
        "Event Wave", 
        "One Wave After"
    ), 
    event_lab = case_when(
        event == "any_disab" ~ "Disability onset", 
        event == "any_health3to5" ~ "Poor health", 
        event == "any_rhosp" ~ "Any hosp", 
        event == "any_rlost_job" ~ "Job exit", 
        event == "fem_rwidowed" ~ "Female widowed", 
        event == "male_rwidowed" ~ "Male widowed", 
        event == "wneg2" ~ "Wealth loss"
    ), 
    lhs_lab = case_when(
        lhs == "any_helpr" ~ "Panel C: Any Child Helps with ADLs", 
        lhs == "at_kp" ~ "Panel B: Any Child-to-Parent Transfer", 
        lhs == "at_pk" ~ "Panel A: Any Parent-to-Child Transfer"
    ),
    event_lab = forcats::fct_relevel(as.factor(event_lab), # reorder to match table ordering
        "Wealth loss", 
        "Job exit", 
        "Male widowed", 
        "Female widowed", 
        "Any hosp", 
        "Disability onset", 
        "Poor health")
    )

    return(df)

}

# Need to calculate p-values for Multiple Hypothesis Testing
est <- est %>% 
    mutate(tstat = coef/stderr, 
            tstat_gt196 = abs(tstat) > 1.96,
            pval = 2*(1- pt(abs(tstat), df = .2*N))) %>%  # approximate Residual Degrees of Freedom
    group_by(lhs) %>%
    mutate(adj_pval = p.adjust(pval, method = "holm"),   # p.adjust returns a vector of adjusted pvalues
            sig = pval <= .05, 
            adj_sig = adj_pval <= .05, 
            adj_sig_star = case_when(
                adj_pval < .001 ~ "***", 
                adj_pval < .01 ~ "**", 
                adj_pval < .05 ~ "*", 
                TRUE ~ ""
            )) 
est <- est %>% filter(t == 0 | t == 1) 
est <- create_label_vars(est)


foreach(p = unique(est$lhs)) %do% {

ggplot(est %>% filter(lhs == p), aes(x = event_lab, y = coef, label = adj_sig_star, group = t_lab, col = t_lab)) + 
    geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
        position = position_dodge(width = .5)) + 
    geom_text(position = position_dodge(width = 1), color = "black", vjust = -.5, size = 5, 
              fontface = "bold", show.legend = FALSE) + 
    xlab("") + 
    ylab("Coefficient") + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    scale_x_discrete(labels = scales::wrap_format(10)) +
    theme_stata() + 
    theme(
        plot.background = element_rect(fill = "white"), 
        legend.title = element_blank(), 
        strip.background =element_rect(fill="white"), 
        legend.background = element_blank(), 
        axis.text.y = element_text(angle = 0, vjust = .5, hjust=1)) + 
    ggsave(stringr::str_glue("output/coefplot/table3_coefplot_{p}.pdf"), 
        height = .25*11, 
        width = .9*8.5)

}

## the one by wealth 
west <- haven::read_dta("output/event_study_regsave/coefplot_estimates_bywealth.dta")
west <- west %>% filter(t == 0 | t == 1) 
west <- create_label_vars(west) %>%
    mutate(samp_lab = ifelse(samp == "hw", "High Wealth HH", "Low Wealth HH"))

foreach(p = unique(west$lhs)) %do% {

    ggplot(west %>% filter(lhs == p), aes(x = event_lab, 
        y = coef,
        col = t_lab,
        fill = t_lab, 
        shape = samp_lab)) + 
        geom_pointrange(aes(ymin = ci_lower, ymax = ci_upper), 
            position = position_dodge(width = .5)) + 
        xlab("") + 
        ylab("Coefficient") + 
        scale_shape_manual(values = c(0,1)) +
        scale_x_discrete(labels = scales::wrap_format(10)) +
        geom_hline(yintercept = 0, linetype = "dashed") + 
        theme_stata() + 
        theme(plot.background = element_rect(fill = "white"), 
            legend.title = element_blank(), 
            legend.position = "bottom",
            legend.margin = margin(0,0,0,0),
            legend.spacing.y = unit(0, "mm"),
            strip.background = element_rect(fill="white"), 
            legend.background = element_blank(), 
            axis.text.y = element_text(angle = 0, vjust = .5, hjust=1)) + 
        ggsave(stringr::str_glue("output/coefplot/table3_coefplot_byw_{p}.pdf"), 
        height = .25*11, 
        width = .9*8.5)
}
