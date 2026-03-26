# setup --------
resp_id = 1497L
resp = filter(resp_d, id == resp_id)
resp_home = resp$neighborhood[[1]][1]

city_d = filter(block_d, city == resp$city)
geom_d = read_rds(here("data/block_geometry.rds")) %>%
    filter(city == resp$city)

# resp. demographics
t(select(resp, party_combined:marital_status))

focus_d = nbhdmodel:::local_area(resp_home, city_d, geom_d, 2.10) %>%
    mutate(incl = fips %in% resp$neighborhood[[1]]) %>%
    select(-city)
resp_nbhd = dplyr::summarize(filter(focus_d, incl)) %>%
    suppressMessages()

plot_map = function(qty, shade=TRUE, plot_nbhd=TRUE, ...) {
    ggplot(focus_d, aes(fill={{ qty }}, ..., alpha=shade*pop+1)) +
        geom_sf(size=0) +
        #geom_sf(aes(geometry=centroid), inherit.aes=F,
        #        data=filter(focus_d, fips==resp_home)) +
        { if (plot_nbhd) geom_sf(data=resp_nbhd, inherit.aes=F, color="black",
                size=0.7, fill="transparent") } +
        { if (shade) scale_alpha_continuous(trans="log10", oob=squish,
                                            limits=c(1, quantile(focus_d$pop, 0.95)),
                                            range=c(0.2, 0.9)) } +
        { if (!shade) scale_alpha_continuous(range=c(0.8, 1)) } +
        theme_void() +
        theme(legend.position="bottom",
              legend.key.width=unit(1, "cm")) +
        guides(alpha="none")
}

# resp. maps ------

plot_map(pop_white/pop) +
    scale_fill_wa_c("sea_star", labels=percent, name="Pct. white")
ggsave(here("slides/resp_race.png"), width=5, height=5, dpi=600, bg=F)
plot_map(pct_college) +
    scale_fill_wa_c("ferries", labels=percent, name="Pct. college-educated")
ggsave(here("slides/resp_educ.png"), width=5, height=5, dpi=600, bg=F)

filter(model_d, id == resp$id) %>%
    left_join(focus_d, by=c("block" = "fips")) %>%
    filter(ring <= 5) %>%
ggplot(aes(fill=as.factor(ring), geometry=geometry)) +
    geom_sf(size=0.2) +
    scale_fill_wa_d("sound_sunset", guide="none", reverse=TRUE) +
    theme_void()
ggsave(here("slides/rings.png"), width=5, height=5, dpi=400, bg=F)


# nbhd predictions -----
nbhds_full = simulate_neighborhood(m_full$miami, resp, 1000, resp_id, city_d,
                                   adj_gr[[resp$city]], proc_fn=preproc)
nbhds_base = simulate_neighborhood(m_baseline$miami, resp, 1000, resp_id, city_d,
                                   adj_gr[[resp$city]], proc_fn=preproc)

calc_post = function(nbhds) {
    incl_table = table(unlist(nbhds)) / length(nbhds)
    incl_df = tibble(fips=city_d$fips[as.integer(names(incl_table))],
                     post = as.numeric(incl_table))
    focus_d %>%
        left_join(incl_df, by="fips") %>%
        pull(post) %>%
        coalesce(0)
}

focus_d$post_full = calc_post(nbhds_full)
focus_d$post_base = calc_post(nbhds_base)


p_race = plot_map(pop_white/pop) +
    scale_fill_wa_c("sea_star", labels=percent, name="Pct. white")
p_diff = plot_map(post_full - post_base, shade=F) +
    scale_fill_wa_c("vantage", midpoint=0, reverse=TRUE,
                    name="Full - baseline\ninclusion prob.\n", labels=percent) +
    theme(legend.position="bottom")
p = p_race + p_diff
if (!file.exists(figpath <- here("paper/figures/indiv_nbhd_race_diff.pdf")))
    ggsave(figpath, width=7, height=4)



post_d = bind_rows(Full=nbhd_demg(nbhds_full, city_d),
                   Baseline=nbhd_demg(nbhds_base, city_d),
                   .id="model")
post_d %>%
    ggplot(aes(model, pop_white/pop)) +
    geom_violin(fill="#e4e4e4", size=0.5, adjust=0.66,
                draw_quantiles=0.5, width=0.6) +
    geom_hline(aes(yintercept=pop_white/pop),
               color=wacolors$sea_star[15], lty="dashed", size=1.25,
               data=filter(nbhds_d, id == resp_id, type == "resp")) +
    scale_y_continuous("Pct. white", labels=percent) +
    labs(x=NULL) +
    theme_minimal() +
    theme(text=element_text(color="#505050"))

# walkthrough for presentation -------

focus_d = nbhdmodel:::local_area(resp_home, city_d, geom_d, 1.00) %>%
    mutate(incl = fips %in% resp$neighborhood[[1]]) %>%
    select(-city) %>%
    st_transform(4326)
resp_model = filter(model_d, id == resp$id)

bg = png::readPNG(here("slides/indiv_bg.png"))
home_pt = st_point(c(-80.149528, 26.1543144)) %>%
    st_sfc() %>%
    st_set_crs(st_crs(focus_d))

p_base = ggplot() +
    annotation_custom(grid::rasterGrob(bg, x = unit(0.5, "npc"),
                                       width = unit(1, "npc"),
                                       height = unit(1, "npc")),
                      -Inf, Inf, -Inf, Inf) +
    scale_fill_manual(values=c(d="#00000080", t="transparent", b="#cadaff7c", y="#fff080a0"), guide="none") +
    scale_color_manual(values=c(d="black", t="transparent", b="transparent", y="white"), guide="none") +
    theme_void()
plot_partial_map = function(i) {
    b_blue = intersect(resp$neighborhood[[1]], resp_model$block[1:(i-1)])
    b_black = setdiff(resp_model$block[1:(i-1)], resp$neighborhood[[1]])
    b_yellow = resp_model$block[i]
    plot_d = focus_d %>%
        mutate(x = factor(case_when(
            fips %in% b_blue ~ "b",
            fips %in% b_yellow ~ "y",
            fips %in% b_black ~ "d",
            TRUE ~ "t")))
    p_base +
        geom_sf(aes(fill=x, color=x), data=plot_d, size=0.5) +
        geom_sf(size=3.5, shape=15, color="black", data=home_pt, inherit.aes=F) +
        geom_sf(size=2.5, shape=15, color="white", data=home_pt, inherit.aes=F) +
        coord_sf(xlim=c(-80.163275, -80.137739), ylim=c(26.149640, 26.161977), expand=F)
}

plot_i = c(1:10, 20, 30, 40, 200)
walk(plot_i, function(i) {
    p = plot_partial_map(i)
    path = str_glue("slides/steps/nbhd_step_{formatC(i, width=2, flag='0')}.png")
    ggsave(here(path), plot=p, width=ncol(bg), height=nrow(bg), units="px")
})


# GOF --------
resp_blocks = filter(nbhds_d, type == "resp") %>%
    mutate(reock = area / (4*pi*radius^2)) %>%
    select(id, blocks, reock, test_only) %>%
    ungroup()

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

acc_d = bind_rows(filter(nbhds_d, !type %in% c("resp", "post_flip", "post_base")),
                  nbhds_circle) %>%
    calc_f1(type, test_only) %>%
    mutate(test_only = c("Fit", "Test")[test_only+1L])
acc_d_sum = acc_d %>%
    filter(!str_detect(type, "circle")) %>%
    mutate(type = str_c(type, "_", str_to_lower(test_only))) %>%
    group_by(type) %>%
    dplyr::summarize(across(precision:f1, median))

acc_paired = left_join(
    calc_f1(filter(nbhds_d, type == "post_full"), .i, type, test_only),
    select(calc_f1(nbhds_d_paired, type, test_only, .i), id, .i, type, precision:f1),
    by=c("id", "type"), suffix=c("", "_circ")
)

plot_d_paired = bind_rows(
    group_by(acc_paired, id, city, type, test_only) %>%
        dplyr::summarize(improvement = median(f1 - f1_circ)),
    group_by(acc_paired, id, type, test_only) %>%
        dplyr::summarize(improvement = median(f1 - f1_circ)) %>%
        mutate(city="pooled")
) %>%
    mutate(test_only = c("Fit", "Test")[test_only+1L])
p = ggplot(plot_d_paired, aes(y=improvement, fill=lbl_city(city))) +
    facet_grid(. ~ test_only) +
    geom_hline(yintercept=0, lty="dashed", color="#666666") +
    geom_boxplot(size=0.4, outlier.size=0.4) +
    scale_fill_manual(values=c(PAL_CITY, Pooled="#808080")) +
    labs(x=NULL, y="F1 score for full model vs. circle (higher is better)",
         fill="City") +
    theme_paper() +
    theme(panel.grid.major.x=element_blank())
ggsave(here("slides/post_gof.png"), plot=p, width=10, height=5, dpi=600, bg=F)
