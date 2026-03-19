d_comp_resp = filter(nbhds_d, type=="resp") %>%
    transmute(id=id, dem=pct_dem, college=pct_college, homeown=pct_homeown,
              white=pop_white/pop)
d_comp_circ = nbhds_circle %>%
    filter(nbhd_r == 1.0) %>%
    transmute(id=id, dem=pct_dem, college=pct_college, homeown=pct_homeown,
              white=pop_white/pop)

d_comp = as_tibble(d_comp_resp - d_comp_circ) %>%
    mutate(id = d_comp_resp$id,
           gerry = abs(dem) + abs(college) + abs(homeown) + abs(white))


d_diff = nbhds_d %>%
    filter(type %in% c("post_base", "post_full")) %>%
    transmute(type=type, city=city, race=race, party=party_combined,
              id=id, dem=pct_dem, college=pct_college,
              homeown=pct_homeown, white=pop_white/pop) %>%
    group_by(type, city, race, party, id) %>%
    summarize(across(where(is.numeric), mean)) %>%
    pivot_longer(dem:white) %>%
    pivot_wider(names_from=type, values_from=value) %>%
    mutate(diff = post_full - post_base) %>%
    select(-post_full, -post_base) %>%
    pivot_wider(names_from=name, values_from=diff) %>%
    left_join(select(nbhds_circle, id, nbhd_r, sd_white:herf_own), by="id") %>%
    mutate(seg_opp_race = sd_white + herf_race/3)
    #print()
    #left_join(d_comp_circ, by="id", suffix=c("_mod", "_circ"))

terc = quantile(d_diff$sd_dem, 1:2/3, na.rm=T)
labs = tibble(l=1:3, y=0.08,
              x=c(0.5*terc[1], mean(terc), max(d_diff$sd_dem, na.rm=T)*0.5 + 0.5*terc[2]))
p1 = ggplot(d_diff, aes(sd_dem, y=after_stat(count/sum(count)))) +
    facet_wrap(~ nbhd_r, labeller=\(x) list(str_glue("Radius: {number(x$nbhd_r, 0.1)} miles"))) +
    geom_histogram(bins=30) +
    geom_vline(xintercept=terc, color="#759C44", size=1) +
    geom_text(aes(x, y, label=l), size=5, data=labs, inherit.aes=F,
              family="Times", fontface="bold") +
    scale_y_continuous("Fraction of respondents", expand=expansion(0),
                       labels=percent, limits=c(0, 0.1)) +
    scale_x_continuous("Standard deviation in pct. Democratic", labels=percent) +
    theme_paper()
terc = quantile(d_diff$sd_white, 1:2/3, na.rm=T)
labs = tibble(l=1:3, y=0.08,
              x=c(0.5*terc[1], mean(terc), max(d_diff$sd_dem, na.rm=T)*0.5 + 0.5*terc[2]))
p2 = ggplot(d_diff, aes(sd_white, y=after_stat(count/sum(count)))) +
    facet_wrap(~ nbhd_r, labeller=\(x) list(str_glue("Radius: {number(x$nbhd_r, 0.1)} miles"))) +
    geom_histogram(bins=30) +
    geom_vline(xintercept=terc, color="#759C44", size=1) +
    geom_text(aes(x, y, label=l), size=5, data=labs, inherit.aes=F,
              family="Times", fontface="bold") +
    scale_y_continuous("Fraction of respondents", expand=expansion(0),
                       labels=percent, limits=c(0, 0.1)) +
    scale_x_continuous("Standard deviation in pct. white", labels=percent) +
    theme_paper()
p = p1 + p2 + plot_layout(nrow=2)
if (!file.exists(figpath <- here("paper/figures/local_diversity.pdf")))
    ggsave(figpath, plot=p, width=6.5, height=4.5)

p = d_diff %>%
    ungroup() %>%
    mutate(opp = as.factor(ntile(sd_dem, 3))) %>%
    drop_na(party, opp, dem) %>%
    #filter(nbhd_r == 1.0) %>%
ggplot(aes(opp, dem, fill=party)) +
    facet_wrap(~ nbhd_r, labeller=\(x) list(str_glue("Radius: {number(x$nbhd_r, 0.1)} miles"))) +
    geom_hline(yintercept=0.0, color="#888888") +
    geom_boxplot(outlier.size=0.1) +
    scale_fill_manual(values=GOP_DEM[c(14, 11, 8, 5, 2)], name=NULL,
                      labels=c("Strong Dem.", "Lean Dem.", "Independent",
                               "Lean Rep.", "Strong Rep.")) +
    labs(x="Tercile of local area diversity",
         y="Change in neighborhood pct. Democratic,\nfull - baseline model") +
    theme(legend.position="top") +
    theme_paper()
if (!file.exists(figpath <- here("paper/figures/nbhd_opp_diff_party.pdf")))
    ggsave(figpath, plot=p, width=6.5, height=3)

d_diff %>%
    ungroup() %>%
    filter(nbhd_r == 1.0) %>%
    mutate(opp = as.factor(ntile(sd_dem, 3))) %>%
    drop_na(party, opp, dem) %>%
    group_by(party, opp) %>%
    summarize(diff = median(dem))


p = d_diff %>%
    ungroup() %>%
    mutate(opp = as.factor(ntile(sd_white, 3)),
           race = fct_lump(race, 2)) %>%
    drop_na(race, herf_race, sd_white, white) %>%
    #filter(nbhd_r == 1.0) %>%
ggplot(aes(opp, white, fill=race)) +
    facet_wrap(~ nbhd_r, labeller=\(x) list(str_glue("Radius: {number(x$nbhd_r, 0.1)} miles"))) +
    geom_hline(yintercept=0.0, color="#888888") +
    geom_boxplot(outlier.size=0.1) +
    scale_fill_wa_d("sea_star", name=NULL,
                    labels=c("Hispanic", "White", "Black, Asian,\nand Other")) +
    labs(x="Tercile of local area diversity",
         y="Change in neighborhood pct. white,\nfull - baseline model") +
    theme(legend.position="top") +
    theme_paper()
if (!file.exists(figpath <- here("paper/figures/nbhd_opp_diff_race.pdf")))
    ggsave(figpath, plot=p, width=6.5, height=3)


d_diff %>%
    ungroup() %>%
    mutate(opp = as.factor(ntile(sd_white, 3)),
           race = fct_lump(race, 1)) %>%
    group_by(opp, race) %>%
    drop_na(opp, race) %>%
    summarize(diff = median(white, na.rm=T))
