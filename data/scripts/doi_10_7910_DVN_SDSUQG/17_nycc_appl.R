# fit full model but with non-Census covariates removed
model_d = raw_model_d %>%
    preproc() %>%
    drop_na(city, gender, race, minority, educ_grp, homeowner, party) %>%
    select(-income_est, -income_dist) %>%
    group_by(ring) %>%
    mutate(dist = if_else(dist == 0 & ring > 0, dist + 0.5*mean(dist), dist)) %>%
    group_by(id)
form_nycc = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school) +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) +
    pct_race + pct_race:minority +
    pct_party + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    educ_grp + party + minority + homeowner
m_nycc <- neighborhood_model(form_nycc, data=model_d)


nbhd_stats_path = here("data/nhbd_stats_full_nycc.rds")
nbhds_resps = read_rds(nbhd_stats_path)

nbhds_resps %>%
    # transmute(id=id, diversity=1-rowMeans(pick(starts_with("herf_")))) %>%
    filter(blocks >= 50) %>%
    transmute(id=id, diversity=1-herf_race) %>%
    filter(diversity %in% range(diversity)) # get min and max

id_highdiv = "email_425"
id_lowdiv = "meta_741"

resp_high = filter(resp_d, id == id_highdiv)
resp_high_home = resp_high$neighborhood[[1]][1]
resp_low = filter(resp_d, id == id_lowdiv)
resp_low_home = resp_low$neighborhood[[1]][1]

# Simulate `N_sim` neighborhoods for each of `N_cf` synthetic residents on `resp`'s block
sim_cf <- function(resp, N_cf=100, N_sim=20) {
    resp_home = resp$neighborhood[[1]][1]
    # Build table of `N_cf` possible block residents (assume covariate independence)
    samp_bin <- function(levels, col, N, home) {
        p <- block_d[[col]][block_d$fips == home]
        c(1-p, p)
        sample(levels, N, replace=TRUE, prob=c(1-p, p))
    }
    tbl_party = transmute(nbhds_resps,
                          dem_lean=democrats/registrants,
                          rep_lean=republicans/registrants,
                          independent=1-dem_lean-rep_lean)
    tbl_race = transmute(nbhds_resps,
                         white=pop_white/pop,
                         black=pop_black/pop,
                         hisp=pop_hisp/pop,
                         multi=1-white-black-hisp)
    samp_tbl <- function(tbl, N, id) {
        pr = unlist(tbl[nbhds_resps$id == resp$id, ])
        names(pr)[sample(length(pr), N, replace=TRUE, prob=pr)]
    }

    resp_cf = tibble(
        homeowner = samp_bin(c("Renter", "Homeowner"), "pct_homeown", N_cf, resp_home),
        education =  samp_bin(c("hs", "grad_4yr"), "pct_college", N_cf, resp_home),
        party = samp_tbl(tbl_party, N_cf, resp$id),
        race = samp_tbl(tbl_race, N_cf, resp$id)
    )
    resp_cf = bind_cols(resp_cf, select(resp, -any_of(colnames(resp_cf))))

    # simulate `N_sim` neighborhoods for each
    sims = rowwise(resp_cf) %>%
        group_map(function(rr, ...) {
            cat(".")
            simulate_neighborhood(m_nycc, rr, N_sim, NULL, block_d,
                                  adj_gr$`new-york`, proc_fn=preproc)
        })
    cat("\n")

    sims
}

N_sim = 20
N_cf = 100
if (!file.exists(path <- here("data/nycc_sim_cf.rda"))) {
    sims_high = sim_cf(resp_high, N_cf=N_cf, N_sim=N_sim)
    sims_low = sim_cf(resp_low, N_cf=N_cf, N_sim=N_sim)

    save(sims_low, sims_high, file=path, compress="xz")
} else {
    load(path)
}

# count how often each block shows up in each neighborhood
m_sims_high = do.call(cbind, map(sims_high, function(nbhds) {
    tabulate(unlist(nbhds), nrow(block_d)) / N_sim
}))
m_sims_low = do.call(cbind, map(sims_low, function(nbhds) {
    tabulate(unlist(nbhds), nrow(block_d)) / N_sim
}))
rm(sims_high, sims_low)

# build curves showing how the size of the consensus COI changes based on thresholds
agree_curves <- function(m_sims) {
    expand_grid(thresh_post = seq(0.05, 1, 0.05),
                thresh_ppl = seq(0.5*N_cf, N_cf, 10)) %>%
        rowwise() %>%
        mutate(size = sum(rowSums(m_sims >= thresh_post) >= thresh_ppl))
        # mutate(pop = sum(block_d$pop[rowSums(m_sims >= thresh_post) >= thresh_ppl]))
}

agree_high = agree_curves(m_sims_high)
agree_low = agree_curves(m_sims_low)

p1 <- bind_rows(high = agree_high,
          low = agree_low,
          .id="div") %>%
    filter(thresh_post == 0.5) %>%
    mutate(lbl = str_c(str_to_title(div), "-diversity area")) %>%
ggplot(aes(thresh_ppl/N_cf, size, z=size, group=div, label=lbl, color=div)) +
    geomtextpath::geom_textline(linewidth=0.8, hjust=0.1, family="Times", size=3) +
    scale_x_continuous("Fraction of block residents\nagreeing on community area") +
    labs(y="Blocks in community area") +
    coord_cartesian(expand=FALSE) +
    guides(color="none") +
    theme_paper()

geom_blocks = tigris::blocks("NY", county=c("Queens"), year=2020) %>%
    select(fips=GEOID20, geometry)

dists = as.numeric(s2_distance_matrix(block_d$centroid[block_d$fips == resp_high_home],
                                      block_d$centroid))
p2 <- block_d %>%
    mutate(x = rowSums(m_sims_high >= 0.5) / N_cf) %>%
    filter(dists <= 1.42 * M_PER_MI) %>%
    left_join(geom_blocks, by="fips") %>%
    select(geometry, x) %>%
ggplot(aes(fill=x, geometry=geometry)) +
    geom_sf(linewidth=0, color="transparent") +
    geom_sf(aes(geometry=centroid), data=filter(block_d, fips==resp_high_home),
            color="white", shape=8, size=1.1, inherit.aes=FALSE) +
    coord_sf(crs=4326, expand=FALSE) +
    scale_fill_wa_c("volcano", reverse=TRUE, labels=percent,
                    name="Share of residents\nwho include block\nin community") +
    theme_void(base_family="Times", base_size=10) +
    theme(legend.position=c(0.5, -0.1),
          legend.key.width=unit(0.8, "cm"),
          legend.key.height=unit(0.6, "cm"),
          legend.title.align=0.5,
          legend.direction="horizontal")


p2 + p1 + plot_layout(widths=c(5.5, 4.5))
ggsave(here("paper/figures/nycc_coi_sum_pred.pdf"), width=7.5, height=3.75)
