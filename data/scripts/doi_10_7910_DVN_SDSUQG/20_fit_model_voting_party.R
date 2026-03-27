REGENERATE=T
 form_vote = ~ is_church + log(dist_church) + is_park +
     is_school*children_home + log(dist_school)*children_home +
     same_bg + same_tract * same_rr +
     sqrt(pop/1e4) + sqrt(area) +
     pct_race + pct_race:vote_2020 + pct_race:minority + pct_race:minority:vote_2020 +
     pct_party + pct_party:vote_2020 + pct_party:party:vote_2020 +
     pct_own + pct_own:homeowner +
     pct_educ + pct_educ:educ_grp +
     log(med_inc) + log(med_inc):educ_grp +
     age + educ_grp + retired + sqrt(tenure) +
     party + minority + homeowner + children_home + vote_2020


form_party = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) + pct_race +
    pct_race:party + pct_race:minority +
    pct_party + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home


# Prepare data -----
model_d = raw_model_d %>%
    filter(group == "C") %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist, -housing, -starts_with("trust"))

# fit/test split
set.seed(5118)
fit_ids = sample(unique(model_d$id), 400)
test_ids = setdiff(unique(model_d$id), fit_ids)
fit_d = filter(model_d, id %in% fit_ids)
test_d = filter(model_d, id %in% test_ids)

# Fit models -----
cities = unique(fit_d$city)
names(cities) = cities
m_party = map(cities, function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    m = neighborhood_model(form_party, filter(fit_d, city == x))
    cli::cli_process_done()
    m
})
# m_vote = map(cities, function(x) {
#     cli::cli_process_start("Fitting baseline model for {x}")
#     m = neighborhood_model(form_vote, filter(fit_d, city == x))
#     cli::cli_process_done()
#     m
# })

# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
 #   `Block population` = get_coef(m_party, "sqrt(pop/10000)"),
  #  `Block area` = get_coef(m_party, "sqrt(area)"),
    #same_tract = get_coef(m_party, "same_tractTRUE"),
    #same_rr = get_coef(m_party, "same_rrTRUE"),
    #is_park = get_coef(m_party, "is_parkTRUE"),
  #  Church = get_coef(m_party, "is_churchTRUE"),
  #  `Distance to church` = get_coef(m_party, "log(dist_church)"),
    #`med_inc` = get_coef(m_party, "log(med_inc)"),
    `Fraction same\nrace (white, Dem.)` = get_coef(m_party, "pct_race"),
    `Fraction same\nrace (white, Rep.)` = map2(get_coef(m_party, "pct_race"),
                                               get_coef(m_party, "pct_race:partyrep"),
                                               `+`),
    `Fraction same\nrace (minority)` = map2(get_coef(m_party, "pct_race"),
                                            get_coef(m_party, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nparty (Dem.)` = get_coef(m_party, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_party, "pct_party"),
    #                         get_coef(m_party, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Rep.)` = map2(get_coef(m_party, "pct_party"),
                                        get_coef(m_party, "pct_party:partyrep"), `+`)
 #   `Fraction same\neducation (college)` = get_coef(m_party, "pct_educ"),
  #  `Fraction same\neducation (no college)` = map2(get_coef(m_party, "pct_educ"),
                                                   # get_coef(m_party, "pct_educ:educ_grpno_coll"),
                                                   # `+`)
)

coef_d = make_coef_d(coefs)

make_coef_table(m_party) %>%
    write_csv(here("paper/dataverse_fits/full_party_interaction.csv"))


dodger = position_dodge(width=0.7)
p = coef_d %>%
    mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2) * (city == "Phoenix")) %>%
ggplot(aes(q50, coef, color=city, group=city)) +
    geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)), color=NA) +
    geom_vline(xintercept=0, color="#00000044") +
    geom_linerange(aes(xmin=q5, xmax=q95), size=1, position=dodger) +
    geom_linerange(aes(xmin=q25, xmax=q75), size=1.8, position=dodger) +
    geom_point(position=dodger, size=2.6) +
    geom_text(aes(x=q95+0.005, label=number(100*q50, 0.1, suffix="pp")),
               position=dodger, hjust=0, size=2.5, show.legend=FALSE,
               family="Times", color="#444444") +
    scale_x_continuous(labels=function(x) number(100*x, 1, suffix="pp")) +
    scale_y_discrete(expand=expansion(mult=0)) +
    coord_cartesian(xlim=c(-0.5, 0.5)) +
    scale_fill_manual(values=c("#00000000", "#0000000C"), guide="none") +
    labs(x="Percentage point change in probability of inclusion at boundary",
         y=NULL, color=NULL) +
    theme_paper() +
    theme(legend.position=c(0.92, 0.7),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected_party.pdf"))||REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=5)


p1=p
# nycc

source(here("replication/12_nycc_prep.R"))
REGENERATE=T
form_vote = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) +
    pct_race + pct_race:vote_2021 + pct_race:minority + pct_race:minority:vote_2021 +
    pct_party + pct_party:party + pct_party:vote_2021 + pct_party:party:vote_2021 +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home + vote_2021


form_party = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) + pct_race +
    pct_race:party + pct_race:minority +
    pct_party + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home


model_d = raw_model_d %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist) %>%
    group_by(ring) %>%
    mutate(dist = if_else(dist == 0 & ring > 0, dist + 0.5*mean(dist), dist)) %>%
    group_by(id)

#m_full = neighborhood_model(form_full, data=model_d)


# fit/test split
set.seed(1313)
fit_ids = sample(unique(model_d$id), 500)
test_ids = setdiff(unique(model_d$id), fit_ids)
fit_d = filter(model_d, id %in% fit_ids)
test_d = filter(model_d, id %in% test_ids)


# Fit models -----
cities = unique(fit_d$city)
names(cities) = cities
m_party = map(cities, function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    m = neighborhood_model(form_party, filter(fit_d, city == x))
    cli::cli_process_done()
    m
})
m_vote = map(cities, function(x) {
    cli::cli_process_start("Fitting baseline model for {x}")
    m = neighborhood_model(form_vote, filter(fit_d, city == x))
    cli::cli_process_done()
    m
})

# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Fraction same\nrace (white, Dem.)` = get_coef(m_party, "pct_race"),
    `Fraction same\nrace (white, Rep.)` = map2(get_coef(m_party, "pct_race"),
                                               get_coef(m_party, "pct_race:partyrep"),
                                               `+`),
    `Fraction same\nrace (minority)` = map2(get_coef(m_party, "pct_race"),
                                            get_coef(m_party, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nparty (Dem.)` = get_coef(m_party, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_party, "pct_party"),
    #                         get_coef(m_party, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Rep.)` = map2(get_coef(m_party, "pct_party"),
                                         get_coef(m_party, "pct_party:partyrep"), `+`),
    `Fraction same\neducation (college)` = get_coef(m_party, "pct_educ"),
    `Fraction same\neducation (no college)` = map2(get_coef(m_party, "pct_educ"),
                                                   get_coef(m_party, "pct_educ:educ_grpno_coll"),
                                                   `+`)
)

coef_d = make_coef_d(coefs)


dodger = position_dodge(width=0.7)
p = coef_d %>%
    mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2) * (city == "Phoenix")) %>%
    ggplot(aes(q50, coef, color=city, group=city)) +
    geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)), color=NA) +
    geom_vline(xintercept=0, color="#00000044") +
    geom_linerange(aes(xmin=q5, xmax=q95), size=1, position=dodger) +
    geom_linerange(aes(xmin=q25, xmax=q75), size=1.8, position=dodger) +
    geom_point(position=dodger, size=2.6) +
    geom_text(aes(x=q95+0.005, label=number(100*q50, 0.1, suffix="pp")),
              position=dodger, hjust=0, size=2.5, show.legend=FALSE,
              family="Times", color="#444444") +
    scale_x_continuous(labels=function(x) number(100*x, 1, suffix="pp")) +
    scale_y_discrete(expand=expansion(mult=0)) +
    coord_cartesian(xlim=c(-0.5, 0.5)) +
    scale_fill_manual(values=c("#00000000", "#0000000C"), guide="none") +
    labs(x="Percentage point change in probability of inclusion at boundary",
         y=NULL, color=NULL) +
    theme_paper() +
    theme(legend.position=c(0.92, 0.7),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected_party_nycc.pdf"))||REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=5)

make_coef_table(m_party) %>%
    write_csv(here("paper/dataverse_fits/full_nycc_party_interaction.csv"))

p2=p




get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Fraction same\nrace (white, no vote)` = get_coef(m_vote, "pct_race"),
    `Fraction same\nrace (white, vote)` = map2(get_coef(m_vote, "pct_race"),
                                               get_coef(m_vote, "pct_race:vote_2021TRUE"),
                                               `+`),
    `Fraction same\nrace (minority, no vote)` = map2(get_coef(m_vote, "pct_race"),
                                            get_coef(m_vote, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nrace (minority, vote)` = pmap(list(get_coef(m_vote, "pct_race"),
                                                     get_coef(m_vote, "pct_race:minorityTRUE"),
                                                     get_coef(m_vote, "pct_race:vote_2021TRUE"),
                                                     get_coef(m_vote, "pct_race:minorityTRUE:vote_2021TRUE")),
                                                     function(a,b,c,d) a+b+c+d),
    `Fraction same\nparty (Dem., no vote)` = get_coef(m_vote, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_vote, "pct_party"),
    #                         get_coef(m_vote, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Dem., vote)` = map2(get_coef(m_vote, "pct_party"),
                                         get_coef(m_vote, "pct_party:vote_2021TRUE"), `+`),
    `Fraction same\nparty (Rep., no vote)` = map2(get_coef(m_vote, "pct_party"),
                                         get_coef(m_vote, "pct_party:partyrep"), `+`),
    `Fraction same\nparty (Rep., vote)` = pmap(list(get_coef(m_vote, "pct_party"),
                                                  get_coef(m_vote, "pct_party:partyrep"),
                                                  get_coef(m_vote, "pct_party:vote_2021TRUE"),
                                                  get_coef(m_vote, "pct_party:partyrep:vote_2021TRUE")),  function(a,b,c,d) a+b+c+d)
)

coef_d = make_coef_d(coefs)


dodger = position_dodge(width=0.7)
p = coef_d %>%
    mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2) * (city == "Phoenix")) %>%
    ggplot(aes(q50, coef, color=city, group=city)) +
    geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)), color=NA) +
    geom_vline(xintercept=0, color="#00000044") +
    geom_linerange(aes(xmin=q5, xmax=q95), size=1, position=dodger) +
    geom_linerange(aes(xmin=q25, xmax=q75), size=1.8, position=dodger) +
    geom_point(position=dodger, size=2.6) +
    geom_text(aes(x=q95+0.005, label=number(100*q50, 0.1, suffix="pp")),
              position=dodger, hjust=0, size=2.5, show.legend=FALSE,
              family="Times", color="#444444") +
    scale_x_continuous(labels=function(x) number(100*x, 1, suffix="pp")) +
    scale_y_discrete(expand=expansion(mult=0)) +
    coord_cartesian(xlim=c(-0.5, 0.5)) +
    scale_fill_manual(values=c("#00000000", "#0000000C"), guide="none") +
    labs(x="Percentage point change in probability of inclusion at boundary",
         y=NULL, color=NULL) +
    theme_paper() +
    theme(legend.position=c(0.92, 0.7),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected_vote_nycc.pdf"))||REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=5)

make_coef_table(m_party) %>%
    write_csv(here("paper/dataverse_fits/full_nycc_turnout_interaction.csv"))


p3=p


if (!file.exists(figpath <- here("paper/figures/full_coef_selected_party_combined.pdf")) || REGENERATE) {
    ggsave(figpath, plot=(p1+p2)+plot_layout(guides="collect"), width=11, height=5)
}
