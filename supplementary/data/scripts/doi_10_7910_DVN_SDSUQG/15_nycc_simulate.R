sim_paths = c(full=here("data/sim_full_nycc.rds"),
              base=here("data/sim_base_nycc.rds"),
              flip=here("data/sim_flip_nycc.rds"),
              circle=here("data/sim_circle_nycc.rds"),
              tract=here("data/sim_tract_nycc.rds"),
              zcta=here("data/sim_zcta_nycc.rds"),
              paired_full=here("data/sim_paired_full_nycc.rds"),
              paired_base=here("data/sim_paired_base_nycc.rds"))


nbhd_stats_path = here("data/nhbd_stats_full_nycc.rds")
#resp = resp_d

nbhds_resps = read_rds(nbhd_stats_path)

if (all(file.exists(sim_paths))) {
    nbhds_post_full = read_rds(sim_paths["full"])
    nbhds_post_base = read_rds(sim_paths["base"])
    nbhds_post_flip = read_rds(sim_paths["flip"])
    nbhds_circle = read_rds(sim_paths["circle"])
    nbhds_tract = read_rds(sim_paths["tract"])
    nbhds_circle_paired_full = read_rds(sim_paths["paired_full"])
    nbhds_circle_paired_base = read_rds(sim_paths["paired_base"])
    nbhds_zcta = read_rds(sim_paths["zcta"])
} else {
    # fill NAs for prediction
    block_d = mutate(block_d, across(med_inc:pct_homeown, ~ dplyr::coalesce(., median(., na.rm=T))))

    split_block_d = split(block_d, ~ city)
    resps = semi_join(resp_d, model_d, by="id")

    # helper function to calculate neighborhood demographics for each simulation
    post_nbhds = function(resp, id, m, fit_ids, split_block_d, adj_gr, n_sim=1, pb=NULL) {
        if (!is.null(pb)) cli::cli_progress_update(id=pb)
        cat(resp$id, " ")

        city_d = split_block_d[['new-york']]
        resp_id = if (resp$id %in% fit_ids) resp$id else NULL
        nbhds = simulate_neighborhood(m, resp, n_sim, resp_id,
                                      city_d, adj_gr[['new-york']], proc_fn=preproc)
        nbhd_demg(nbhds, city_d, resp$neighborhood[[1]]) %>%
            bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                             map_clicks, map_time)) %>%
            mutate(test_only = is.null(resp_id))
    }

    # model posteriors ----
    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (full)"), total=nrow(resps))
    nbhds_post_full = group_by(resps, id) %>%
        group_modify(post_nbhds, m_full, fit_ids, split_block_d, adj_gr,
                     pb=pb, .keep=TRUE)
    write_rds(nbhds_post_full, sim_paths["full"], compress="xz")

    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (baseline)"), total=nrow(resps))
    nbhds_post_base = group_by(resps, id) %>%
        group_modify(post_nbhds, m_baseline, fit_ids, split_block_d, adj_gr,
                     pb=pb, .keep=TRUE)
    write_rds(nbhds_post_base, sim_paths["base"], compress="xz")

    # Counterfactual race -----
    resps_flip_race = semi_join(resp_d, model_d, by="id") %>%
        mutate(race = if_else(race == "white", "hisp", "white"))

    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (flip)"), total=nrow(resps))
    nbhds_post_flip = group_by(resps_flip_race, id) %>%
        group_modify(post_nbhds, m_full, fit_ids, split_block_d, adj_gr,
                     pb=pb, .keep=TRUE)
    write_rds(nbhds_post_flip, sim_paths["flip"], compress="xz")

    # circle of fixed radius -----
    post_circle = function(resp, id, split_block_d, adj_gr, radius=resp$radius/M_PER_MI, pb=NULL) {
        if (!is.null(pb)) cli::cli_progress_update(id=pb)
        resp_city = resp$city[1]
        city_d = split_block_d[[resp_city]]

        start_fips = resp$neighborhood[[1]][1]
        start_idx = match(start_fips, adj_gr[[resp_city]]$blocks)
        max_ring = ceiling(10 * sqrt(max(radius)))
        relevant = nbhdmodel:::get_within_ring(max_ring, start_idx, adj_gr[[resp_city]]$graph)
        dists = as.numeric(s2_distance_matrix(city_d$centroid[start_idx],
                                              city_d$centroid[relevant$idx]))

        nbhds = map(radius, ~ city_d$fips[relevant$idx[dists / M_PER_MI <= .]])

        nbhd_demg(nbhds, city_d, resp$neighborhood[[1]]) %>%
            bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                             map_clicks, map_time))
    }

    calc_nbhd_circle = function(r) {
        pb = cli::cli_progress_bar(str_glue("Nbhd. circle stats (r = {sprintf('%.1f', r)})"),
                                   total=nrow(resps))
        group_by(resps, id) %>%
            group_modify(post_circle, split_block_d, adj_gr, radius=r, pb=pb, .keep=TRUE) %>%
            mutate(type=str_glue("circle_{sprintf('%.1f', r)}"),
                   nbhd_r = r,
                   .before=id)
    }
    nbhds_circle = map_dfr(c(0.5, 1.0, 2.0), calc_nbhd_circle)
    write_rds(nbhds_circle, sim_paths["circle"], compress="xz")

    # paired compairsons --------
    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (paired, full)"), total=nrow(resps))
    nbhds_circle_paired_full = resps %>%
        left_join(select(nbhds_post_full, id, radius), by="id") %>%
        group_by(id) %>%
        group_modify(post_circle, split_block_d, adj_gr, pb=pb, .keep=TRUE)
    write_rds(nbhds_circle_paired_full, sim_paths["paired_full"], compress="xz")

    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (paired, base)"), total=nrow(resps))
    nbhds_circle_paired_base = resps %>%
        left_join(select(nbhds_post_base, id, radius), by="id") %>%
        group_by(id) %>%
        group_modify(post_circle, split_block_d, adj_gr, pb=pb, .keep=TRUE)
    write_rds(nbhds_circle_paired_base, sim_paths["paired_base"], compress="xz")

    # tract that the resp. lives in -----
    post_tract = function(resp, id, split_block_d, pb=NULL) {
        if (!is.null(pb)) cli::cli_progress_update(id=pb)
        resp_city = resp$city[1]
        city_d = split_block_d[[resp_city]]

        start_fips = resp$neighborhood[[1]][1]
        start_tract = fixed(str_sub(start_fips, 1, 11))
        nbhds = list(city_d$fips[str_starts(city_d$fips, start_tract)])

        nbhd_demg(nbhds, city_d, resp$neighborhood[[1]]) %>%
            bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                             map_clicks, map_time))
    }

    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (tracts)"), total=nrow(resps))
    nbhds_tract = group_by(resps, id) %>%
        group_modify(post_tract, split_block_d, pb=pb, .keep=TRUE)
    write_rds(nbhds_tract, sim_paths["tract"], compress="xz")





    # ZCTA that the resp. lives in -----
    d_zctas = tigris::zctas(cb=TRUE, year=2020, starts_with=c("1", "2", "3", "86", "85")) %>%
        st_transform(4326) %>%
        select(zip=ZCTA5CE20, geometry)
    match_zctas = st_join(st_as_sf(block_d["centroid"]), d_zctas,
                          join=st_intersects)
    post_zcta = function(resp, id, split_block_d, pb=NULL) {
        if (!is.null(pb)) cli::cli_progress_update(id=pb)
        resp_city = resp$city[1]
        city_d = split_block_d[[resp_city]]

        start_fips = resp$neighborhood[[1]][1]
        start_zcta = match_zctas$zip[match(start_fips, block_d$fips)]
        if (is.na(start_zcta)) return(data.frame())
        nbhds = list(block_d$fips[which(match_zctas$zip == start_zcta)])

        nbhd_demg(nbhds, city_d, resp$neighborhood[[1]]) %>%
            bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                             map_clicks, map_time))
    }

    pb = cli::cli_progress_bar(str_glue("Nbhd. stats (ZCTAs)"), total=nrow(resps))
    nbhds_zcta = group_by(resps, id) %>%
        group_modify(post_zcta, split_block_d, pb=pb, .keep=TRUE)
    write_rds(nbhds_zcta, sim_paths["zcta"], compress="xz")


    }

nbhds_d = bind_rows(resp=nbhds_resps,
                    post_full=nbhds_post_full,
                    post_base=nbhds_post_base,
                    post_flip=nbhds_post_flip,
                    .id="type")
nbhds_d_paired = bind_rows(post_full=nbhds_circle_paired_full,
                           post_base=nbhds_circle_paired_base,
                           .id="type")
rm(nbhds_resps, nbhds_post_full, nbhds_post_base, nbhds_post_flip,
   nbhds_circle_paired_full, nbhds_circle_paired_base)

