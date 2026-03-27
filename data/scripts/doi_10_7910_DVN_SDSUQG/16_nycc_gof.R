resp_blocks = filter(nbhds_d, type == "resp") %>%
    mutate(reock = area / (4*pi*radius^2)) %>%
    select(id, blocks, reock, test_only) %>%
    ungroup()
REGENERATE = T

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
        summarize(value = median(value)) %>%
        pivot_wider(names_from=metric) %>%
        mutate(f1 = 2/(1/precision + 1/recall)) %>%
        ungroup()
}

acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip")),
          nbhds_circle) %>%
    calc_f1(type, test_only) %>%
    mutate(test_only = c("fit", "test")[test_only+1L])
acc_d_sum = acc_d %>%
    filter(!str_detect(type, "circle")) %>%
    mutate(type = str_c(type, "_", str_to_lower(test_only))) %>%
    group_by(type) %>%
    summarize(across(precision:f1, median))
if (!file.exists(path <- here("paper/tables/model_acc_nycc.rds")) || REGENERATE) {
    as.list(acc_d_sum[, -1]) %>%
        map(~ number(., 0.01)) %>%
        map(~ `names<-`(., acc_d_sum$type)) %>%
    write_rds(path, compress="gz")
}

acc_paired = left_join(
    calc_f1(filter(nbhds_d, type %in% c("post_full", "post_base")), .i, type, test_only),
    select(calc_f1(nbhds_d_paired, type, test_only, .i), id, .i, type, precision:f1),
    by=c("id", "type"), suffix=c("", "_circ")
)

plot_d_paired = bind_rows(
    group_by(acc_paired, id, city, type, test_only) %>%
        summarize(improvement = median(f1 - f1_circ)),
    group_by(acc_paired, id, type, test_only) %>%
        summarize(improvement = median(f1 - f1_circ)) %>%
        mutate(city="pooled")
) %>%
    mutate(test_only = c("In-sample", "Out-of-sample")[test_only+1L],
           type = c(post_base="Baseline model", post_full="Full model")[type])
p = ggplot(plot_d_paired%>%filter(city=='new-york'), aes(type, improvement, fill=lbl_city(city))) +
    facet_grid(. ~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, width=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY, Pooled="#808080")) +
    labs(x=NULL, y="F1 score for full model vs. circle",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())+
    guides(fill=F)
if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_circle_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}
p1=p

# make tract plot


acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip")),
                  mutate(nbhds_tract, type="tract")) %>%
    calc_f1(type, test_only) %>%
    group_by(id) %>%
    mutate(test_only = c("fit", "test")[test_only[1]+1L]) %>%
    ungroup()

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
    geom_boxplot(size=0.4, width=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY, Pooled="#808080")) +
    labs(x=NULL, y="F1 score for full model vs. tract",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())+
    guides(fill=F)

if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_tract_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}
p2=p


if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_combined_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, plot=(p1/p2)+plot_layout(guides="collect"), width=6.5, height=5)
}


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
    labs(x=NULL, y="F1 score for full model vs. ZCTA",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())


if (!file.exists(figpath <- here("paper/figures/nbhd_gof_vs_zcta_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, plot=p, width=6.5, height=3)
}




# Try w/ AUC
if (FALSE) {
library(yardstick)

block_d = mutate(block_d, across(med_inc:pct_homeown, ~ coalesce(., median(., na.rm=T))))

split_block_d = split(block_d, ~ city)
resps = semi_join(resp_d, model_d, by="id")

post_auc = function(resp, id, fit_ids, split_block_d, adj_gr, n_sim=200, pb=NULL) {
    if (!is.null(pb)) cli::cli_progress_update(id=pb)
    resp_city = resp$city[1]
    resp_id = if (resp$id %in% fit_ids) resp$id else NULL
    city_d = split_block_d[[resp_city]]

    start_fips = resp$neighborhood[[1]][1]
    start_idx = match(start_fips, adj_gr[[resp_city]]$blocks)
    max_ring = max(raw_model_d$ring[raw_model_d$id == resp$id]) + 2L
    relevant = nbhdmodel:::get_within_ring(max_ring, start_idx, adj_gr[[resp_city]]$graph)
    dists = as.numeric(s2_distance_matrix(city_d$centroid[start_idx],
                                          city_d$centroid[relevant$idx]))
    incl = city_d$fips[relevant$idx] %in% resp$neighborhood[[1]]

    post_full = post_incl(m_full[[resp$city]], resp, resp_id, city_d[relevant$idx, ], preproc)
    post_base = post_incl(m_baseline[[resp$city]], resp, resp_id, city_d[relevant$idx, ], preproc)

    #nbhds_full = simulate_neighborhood(m_full[[resp$city]], resp, n_sim, resp_id,
    #                              city_d, adj_gr[[resp$city]], max_ring,
    #                              preproc)
    #post_tbl = table(unlist(nbhds_full), useNA="always") / length(nbhds_full)
    #post_incl_full = coalesce(as.numeric(post_tbl[as.character(relevant$idx)]), 0)

    #nbhds_base = simulate_neighborhood(m_baseline[[resp$city]], resp, n_sim, resp_id,
    #                              city_d, adj_gr[[resp$city]], max_ring,
    #                              preproc)
    #post_tbl = table(unlist(nbhds_base), useNA="always") / length(nbhds_base)
    #post_incl_base = coalesce(as.numeric(post_tbl[as.character(relevant$idx)]), 0)

    #d_roc = tibble(incl=as.factor(incl), dist=-dists,
    #               full=post_full, base=post_base)
    #d_roc = bind_rows(dist=roc_curve(d_roc, incl, dist),
    #                  base=roc_curve(d_roc, incl, base),
    #                  full=roc_curve(d_roc, incl, full),
    #                  .id="predictor")
    idx = sample.int(length(incl), min(500, length(incl)), replace=F)

    tibble(sample = if_else(!is.null(resp_id), "In-sample", "Out-of-sample"),
           incl = as.factor(1 - incl[idx]),
           dist = -dists[idx],
           full = post_full[idx],
           base = post_base[idx])
    #tibble(auc_dist = pr_auc(d_roc, incl, dist)$.estimate,
    #       auc_base = pr_auc(d_roc, incl, base)$.estimate,
    #       auc_full = pr_auc(d_roc, incl, full)$.estimate)
    #tibble(auc_dist =  fastAUC(-dists, incl),
    #       auc_base = fastAUC(post_incl_base, incl),
    #       auc_full = fastAUC(post_incl_full, incl),
    #       roc = list(d_roc))
}

pb = cli::cli_progress_bar(str_glue("Nbhd. AUC "), total=nrow(resps))
nbhds_auc2 = resps %>%
    #slice_sample(n=10) %>%
    group_by(id) %>%
    group_modify(post_auc,fit_ids, split_block_d, adj_gr, pb=pb, .keep=TRUE)

nbhds_auc %>%
    select(-roc) %>%
    mutate(diff_base = auc_base - auc_dist,
           diff_full = auc_full - auc_dist) %>%
    ungroup() %>%
    mutate(city = resps$city,
           sample = if_else(id %in% fit_d$id, "In-sample", "Out-of-sample")) %>%
    pivot_longer(starts_with("diff_"), names_to="model", names_prefix="auc_",
                 values_to="auc_diff") %>%
ggplot(aes(model, auc_diff, fill=city)) +
    facet_wrap(~ sample) +
    geom_boxplot()

nbhds_auc2 %>%
    mutate(diff_base = auc_base - auc_dist,
           diff_full = auc_full - auc_dist) %>%
    ungroup() %>%
    mutate(city = resps$city,
           sample = if_else(id %in% fit_d$id, "In-sample", "Out-of-sample")) %>%
    pivot_longer(starts_with("diff_"), names_to="model", names_prefix="auc_",
                 values_to="auc_diff") %>%
    ggplot(aes(model, auc_diff, fill=city)) +
    facet_wrap(~ sample) +
    geom_boxplot()

nbhds_auc %>%
    mutate(diff_base = auc_base - auc_dist,
           diff_full = auc_full - auc_dist) %>%
    filter(diff_full <= -0.02) %>%
    head(1) %>%
    pull(roc) %>%
    `[[`(1) %>%
ggplot(aes(specificity, 1-sensitivity, color=predictor)) +
    geom_path()
}


# Demographic stats ------
if (FALSE) {
post_demg_d = nbhds_d %>%
    filter(type %in% c("post_full", "post_base")) %>%
    group_by(type, id, city, test_only) %>%
    mutate(white = pop_white / pop, .after=blocks) %>%
    summarize(across(pct_dem:reock, median, na.rm=T)) %>%
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
