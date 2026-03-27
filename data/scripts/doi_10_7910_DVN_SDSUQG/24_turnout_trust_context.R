rm(list=ls())
gc()
source('replication/00_setup_load.R')
library(modelsummary)
library(stargazer)
# library(Hmisc)
REGENERATE = T

# Calculate neighborhood summary statistics ----

nbhd_stats_path = here("data/nhbd_stats.rds")

if (file.exists(nbhd_stats_path)) {
    nbhds_resps = read_rds(nbhd_stats_path)
} else {
    # fill in gaps in block data
    block_d = mutate(block_d, across(med_inc:pct_homeown, ~ coalesce(., median(., na.rm=T))))

    split_block_d = split(block_d, ~ city)
    resps = semi_join(resp_d, model_d, by="id")

    # helper function to calculate neighborhood demographics for each respondent
    resp_nbhd = function(resp, id, fit_ids, split_block_d) {
        city_d = split_block_d[[resp$city]]
        nbhd_demg(resp$neighborhood, city_d) %>%
            bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                             map_clicks, map_time)) %>%
            mutate(test_only = !(resp$id %in% fit_ids))
    }

    nbhds_resps = group_by(resps, id) %>%
        group_modify(resp_nbhd, fit_ids, split_block_d, .keep=TRUE)

    write_rds(nbhds_resps, nbhd_stats_path, compress="xz")
}


nbhds_resps = nbhds_resps %>%
    left_join(raw_model_d %>%
                  filter(group == 'C')%>%
                  group_by(id)%>%
                  summarize(trust_1 = trust_1[1],
                            trust_2 = trust_2[1],
                            trust_3 = trust_3[1],

                            vote_2020 = vote_2020[1],
                            president = president[1]),by = 'id')%>%
    mutate(trust_tot = (trust_1+trust_2+trust_3)/10,
           p_black = pop_black/pop,
           p_white = pop_white/pop,
           party = recode(party_combined , `rep_strong` = "republican", `rep_lean` = "republican", `dem_strong`='democrat', `dem_lean`="democrat"),
           biden = president == 'biden',
           college = education %in% c('grad_4yr', 'grad_2yr', 'postgrad'),
           married = marital_status %in% c('Married', ' Legal Domestic Partnership'),
           children_home = children_home=='Yes',
           homeowner = homeowner=='Yes',
           white = race=='white')

m1 = lm(trust_tot ~ p_white*white + pct_dem*party + pct_college+ college + gender + age + married + children_home + homeowner , data = nbhds_resps)




stargazer(m1, type = 'text', out = 'tables/trust_context.tex')
