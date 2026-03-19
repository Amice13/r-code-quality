REGENERATE = T


model_d = raw_model_d %>%
    filter(group == "C") %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist) %>%
    group_by(city) %>%
    mutate(trust_tot = trust_1 + trust_2 + trust_3,
           trusting = trust_tot > median(trust_tot, na.rm=TRUE)) %>%
    group_by(id)
# Fit models -----
cities = unique(model_d$city)
names(cities) = cities
# Fit with housing / trust questions
form_housing = ~ is_church + log(dist_church) + is_park + is_school * children_home +
    log(dist_school) * children_home + same_bg + same_tract *
    same_rr + sqrt(pop/10000) + sqrt(area) + pct_race*housing*homeowner +
    pct_party*housing + pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    age + educ_grp + retired + sqrt(tenure) + party + minority +
    homeowner + children_home
m_housing = map(cities, function(x) {
    cli::cli_process_start("Fitting housing model for {x}")
    m = neighborhood_model(form_housing, filter(model_d, city == x, !is.na(housing)))
    cli::cli_process_done()
    m
})

# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Fraction same\nrace (renter, no ban)` = get_coef(m_housing, "pct_race"),
    `Fraction same\nrace (renter, supp. ban)` = map2(get_coef(m_housing, "pct_race"),
                                            get_coef(m_housing, "pct_race:housingTRUE"),
                                            `+`),
    `Fraction same\nrace (homeowner, no ban)` = map2(get_coef(m_housing, "pct_race"),
                                                     get_coef(m_housing, "pct_race:homeownerYes"),
                                            `+`),
    `Fraction same\nrace (homeowner, supp. ban)` = pmap(
        list(get_coef(m_housing, "pct_race"),
             get_coef(m_housing, "pct_race:housingTRUE"),
             get_coef(m_housing, "pct_race:homeownerYes"),
             get_coef(m_housing, "pct_race:housingTRUE:homeownerYes")),
        function(a, b, c, d) a+b+c+d
        ),
    `Fraction same\nparty (no ban)` = get_coef(m_housing, "pct_party"),
    `Fraction same\nparty (supp. ban)` = map2(get_coef(m_housing, "pct_party"),
                                            get_coef(m_housing, "housingTRUE:pct_party"),
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
    theme(legend.position=c(0.91, 0.5),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/housing_coef_selected.pdf"))||REGENERATE )
    ggsave(figpath, plot=p, width=6.5, height=5)
p1=p


make_coef_table(m_housing) %>%
    fmt_coef_table("Full model estimates.") %>%
    write_file(here("paper/tables/model_coef_full_housing.tex"))
make_coef_table(m_housing) %>%
    write_csv(here("paper/dataverse_fits/full_housing.csv"))





#### trust

form_trust = ~ is_church + log(dist_church) + is_park + is_school * children_home +
    log(dist_school) * children_home + same_bg + same_tract *
    same_rr + sqrt(pop/10000) + sqrt(area) + pct_race*trusting +
    pct_party*trusting + pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    age + educ_grp + retired + sqrt(tenure) + party + minority +
    homeowner + children_home
m_trust = map(cities, function(x) {
    cli::cli_process_start("Fitting trust model for {x}")
    m = neighborhood_model(form_trust, filter(model_d%>%select(-housing), city == x, !is.na(trusting)))
    cli::cli_process_done()
    m
})

# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)

coefs = list(
    `Fraction same\nrace (not trusting)` = get_coef(m_trust, "pct_race"),
    `Fraction same\nrace (trusting)` = map2(get_coef(m_trust, "pct_race"),
                                          get_coef(m_trust, "pct_race:trustingTRUE"),
                                          `+`),
    `Fraction same\nparty (not trusting)` = get_coef(m_trust, "pct_party"),
    `Fraction same\nparty (trusting)` = map2(get_coef(m_trust, "pct_party"),
                                                   get_coef(m_trust, "trustingTRUE:pct_party"),
                                             `+`)
)

coef_d = make_coef_d(coefs)
make_coef_table(m_trust) %>%
    write_csv(here("paper/dataverse_fits/full_trust.csv"))


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
    theme(legend.position=c(0.91, 0.5),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/trust_coef_selected.pdf"))||REGENERATE )
    ggsave(figpath, plot=p, width=6.5, height=5)

p2=p



if (!file.exists(figpath <- here("paper/figures/housing_trust_coef_selected.pdf")) || REGENERATE) {
    ggsave(figpath, plot=(p1+p2)+plot_layout(guides="collect"), width=11, height=5)
}



