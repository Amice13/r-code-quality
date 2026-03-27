# Get home price data and integrate it --------
REGENERATE=T
st_cty = model_d %>%
    ungroup() %>%
    distinct(tract) %>%
    separate(tract, c("state", "county", NA), sep=c(2, 5)) %>%
    distinct() %>%
    group_by(state)

d_homes = group_modify(st_cty, function(d_cty, key) {
    tidycensus::get_acs("block group", table="B25077", cache_table=TRUE,
                        year=2019, state=key$state, county=d_cty$county)
}) %>%
    ungroup() %>%
    select(block_group = GEOID, med_home=estimate)


model_d = model_d %>%
    select(-any_of(c("med_home", "lhomeprc"))) %>%
    left_join(d_homes, by="block_group") %>%
    mutate(lhomeprc = coalesce(log(med_home), log(median(med_home, na.rm=TRUE)))) %>%
    ungroup() %>%
    mutate(lhomeprc = coalesce(lhomeprc, log(median(med_home, na.rm=TRUE)))) %>%
    group_by(id) %>%
    select(-med_home)

form_full_homeprc =  ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr +
    sqrt(pop/1e4) + sqrt(area) +
    pct_race*lhomeprc + pct_race:minority +
    pct_party*lhomeprc + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    educ_grp:lhomeprc +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home

# Fit models -----
cities = unique(model_d$city)
names(cities) = cities
m_full = map(cities, function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    m = neighborhood_model(form_full_homeprc, filter(model_d, city == x))
    cli::cli_process_done()
    m
})

get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Home price direct effect` = get_coef(m_full, "lhomeprc"),
    `Home price-same race interaction` = get_coef(m_full, "pct_race:lhomeprc"),
    `Home price-same party interaction` = get_coef(m_full, "lhomeprc:pct_party"),
    `Home price-noncollege interaction` = get_coef(m_full, "lhomeprc:educ_grpno_coll")
)

coef_d = make_coef_d(coefs)

dodger = position_dodge(width=0.7)
p = coef_d %>%
    # mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2) * (city == "Phoenix")) %>%
    ggplot(aes(q50, coef, color=city, group=city)) +
    # geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)), color=NA) +
    geom_vline(xintercept=0, color="#00000044") +
    geom_linerange(aes(xmin=q5, xmax=q95), linewidth=1, position=dodger) +
    geom_linerange(aes(xmin=q25, xmax=q75), linewidth=1.8, position=dodger) +
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
    theme(legend.position=c(0.92, 0.65),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected_homeprc.pdf"))||REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=2.75)


make_coef_table(m_full) %>%
    write_csv(here("paper/dataverse_fits/full_home_prices.csv"))
