resp_blocks = filter(nbhds_d, type == "resp") %>%
    mutate(reock = area / (4*pi*radius^2)) %>%
    select(id, blocks, reock, test_only) %>%
    ungroup()

REGENERATE=T
# Precision and Recall -------

# calculate F1 scores for set of predictions, grouped by id, city, and ...
calc_f1 = function(d, ...) {
    d %>%
        ungroup() %>%
        mutate(.i = row_number()) %>%
        inner_join(resp_blocks, by="id", suffix=c("", "_resp")) %>%
        mutate(recall = pct_in * blocks / blocks_resp,
               precision = pct_in) %>%
        pivot_longer(precision:recall, names_to="metric", values_to="value") %>%
        group_by(id, city, metric, blocks_resp, ...) %>%
        dplyr::summarize(value = median(value)) %>%
        pivot_wider(names_from=metric) %>%
        mutate(f1 = 2/(1/precision + 1/recall)) %>%
        ungroup()
}


## COMPARE TO TRACTS --------
acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip")),
                  mutate(nbhds_tract, type="tract")) %>%
    calc_f1(type, test_only) %>%
    group_by(id) %>%
    mutate(test_only = c("fit", "test")[test_only[1]+1L]) %>%
    ungroup()


#acc_d
p = acc_d %>%
    pivot_longer(precision:f1, names_to="stat") %>%
    pivot_wider(names_from=type) %>%
    mutate(diff_base = post_base - tract,
           diff_full = post_full - tract) %>%
    select(-post_base:-tract) %>%
    pivot_longer(diff_base:diff_full, names_to="type") %>%
    pivot_wider(names_from=stat) %>%
    mutate(test_only = c(fit="In-sample", test="Out-of-sample")[test_only],
           type = c(diff_base="Baseline model", diff_full="Full model")[type]) %>%
ggplot(aes(type, f1, fill=lbl_city(city))) +
    facet_grid(~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY)) +
    labs(x=NULL, y="F1 score for full model vs. tract",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())

if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_tract.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}

p2=p
#, Pooled="#808080"
## COMPARE TO ZCTAS --------
acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip")),
                  mutate(nbhds_zcta, type="zcta")) %>%
    calc_f1(type, test_only) %>%
    group_by(id) %>%
    mutate(test_only = c("fit", "test")[test_only[1]+1L]) %>%
    ungroup()

p = acc_d %>%
    pivot_longer(precision:f1, names_to="stat") %>%
    pivot_wider(names_from=type) %>%
    mutate(diff_base = post_base - zcta,
           diff_full = post_full - zcta) %>%
    select(-post_base:-zcta) %>%
    pivot_longer(diff_base:diff_full, names_to="type") %>%
    pivot_wider(names_from=stat) %>%
    mutate(test_only = c(fit="In-sample", test="Out-of-sample")[test_only],
           type = c(diff_base="Baseline model", diff_full="Full model")[type]) %>%
ggplot(aes(type, f1, fill=lbl_city(city))) +
    facet_grid(~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY)) +
    labs(x=NULL, y="F1 score for full model vs. ZCTA",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())


if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_zcta.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}


## CALCULATE ACCURACY STATS --------

ggplot(acc_d, aes(type, f1, fill=city)) +
    # facet_grid(city ~ test_only) +
    facet_grid(~ test_only) +
    geom_boxplot()

acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip")),
          nbhds_circle) %>%
    calc_f1(type, test_only) %>%
    mutate(test_only = c("fit", "test")[test_only+1L])
acc_d_sum = acc_d %>%
    filter(!str_detect(type, "circle")) %>%
    mutate(type = str_c(type, "_", str_to_lower(test_only))) %>%
    group_by(type) %>%
    dplyr::summarize(across(precision:f1, median))
if (!file.exists(path <- here("paper/tables/model_acc.rds")) || REGENERATE) {
    as.list(acc_d_sum[, -1]) %>%
        map(~ number(., 0.01)) %>%
        map(~ `names<-`(., acc_d_sum$type)) %>%
    write_rds(path, compress="gz")
}

## COMPARE TO PAIRED CIRCLES --------

acc_paired = left_join(
    calc_f1(filter(nbhds_d, type %in% c("post_full", "post_base")), .i, type, test_only),
    select(calc_f1(nbhds_d_paired, type, test_only, .i), id, .i, type, precision:f1),
    by=c("id", "type"), suffix=c("", "_circ")
)

acc_paired %>%
    group_by(type, test_only) %>%
    dplyr::summarize(improvement = median(f1 - f1_circ))

plot_d_paired = acc_paired %>%
    group_by(id, city, type, test_only) %>%
    dplyr::summarize(improvement = median(f1 - f1_circ)) %>%#,
    mutate(test_only = c("In-sample", "Out-of-sample")[test_only+1L],
           type = c(post_base="Baseline model", post_full="Full model")[type])
p = ggplot(plot_d_paired, aes(type, improvement, fill=lbl_city(city))) +
    facet_grid(. ~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY)) +
    labs(x=NULL, y="F1 score for full model vs. circle",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())
if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_circle.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}
p1=p


if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_combined.pdf")) || REGENERATE) {
    ggsave(figpath, plot=(p1/p2)+plot_layout(guides="collect"), width=6.5, height=5)
}

# Demographic stats ------
if (FALSE) {
post_demg_d = nbhds_d %>%
    filter(type %in% c("post_full", "post_base")) %>%
    group_by(type, id, city, test_only) %>%
    mutate(white = pop_white / pop, .after=blocks) %>%
    dplyr::summarize(across(pct_dem:reock, median, na.rm=T)) %>%
    inner_join(select(filter(nbhds_d, type == "resp"), id, pct_dem:reock),
               by="id", suffix=c("", "_resp")) %>%
    mutate(test_only = c("In-sample", "Out-of-sample")[test_only+1L],
           type = c(post_base="Base model", post_full="Full model")[type]) %>%
    print()
ggplot(post_demg_d, aes(type, white - pop_white_resp/pop_resp, fill=lbl_city(city))) +
    facet_grid(. ~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY, Pooled="#808080")) +
    labs(x=NULL, y="F1 score for full model vs. circle (higher is better)",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())
}
