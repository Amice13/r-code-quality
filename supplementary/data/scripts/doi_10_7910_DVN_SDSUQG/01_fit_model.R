# Prepare data -----
REGENERATE = T

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
m_full = map(cities, function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    m = neighborhood_model(form_full, filter(fit_d, city == x))
    cli::cli_process_done()
    m
})
m_baseline = map(cities, function(x) {
    cli::cli_process_start("Fitting baseline model for {x}")
    m = neighborhood_model(form_baseline_nodemg, filter(fit_d, city == x))
    cli::cli_process_done()
    m
})

# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Block population` = get_coef(m_full, "sqrt(pop/10000)"),
    `Block area` = get_coef(m_full, "sqrt(area)"),
    #same_tract = get_coef(m_full, "same_tractTRUE"),
    #same_rr = get_coef(m_full, "same_rrTRUE"),
    #is_park = get_coef(m_full, "is_parkTRUE"),
    Church = get_coef(m_full, "is_churchTRUE"),
    `Distance to church` = get_coef(m_full, "log(dist_church)"),
    #`med_inc` = get_coef(m_full, "log(med_inc)"),
    `Fraction same\nrace (white)` = get_coef(m_full, "pct_race"),
    `Fraction same\nrace (minority)` = map2(get_coef(m_full, "pct_race"),
                                            get_coef(m_full, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nparty (Dem.)` = get_coef(m_full, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_full, "pct_party"),
    #                         get_coef(m_full, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Rep.)` = map2(get_coef(m_full, "pct_party"),
                                        get_coef(m_full, "pct_party:partyrep"), `+`),
    `Fraction same\neducation (college)` = get_coef(m_full, "pct_educ"),
    `Fraction same\neducation (no college)` = map2(get_coef(m_full, "pct_educ"),
                                                   get_coef(m_full, "pct_educ:educ_grpno_coll"),
                                                   `+`)
)

coef_d = make_coef_d(coefs)

dodger = position_dodge(width=0.7)
p = coef_d %>%
    mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2) * (city == "Phoenix")) %>%
ggplot(aes(q50, coef, color=city, group=city)) +
    geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)), color=NA) +
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
    theme(legend.position=c(0.92, 0.7),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected.pdf"))|REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=5)

make_coef_table(m_full) %>%
    fmt_coef_table("Full model estimates.") %>%
    write_file(here("paper/tables/model_coef_full.tex"))
make_coef_table(m_baseline) %>%
    fmt_coef_table("Baseline model estimates.") %>%
    write_file(here("paper/tables/model_coef_base.tex"))
