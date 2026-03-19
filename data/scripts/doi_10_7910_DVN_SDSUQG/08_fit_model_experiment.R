# Prepare data -----
REGENERATE=T
model_d = raw_model_d %>%
    # filter(group == "C") %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist, -trust_1, -trust_2, -trust_3, -housing)

# fit/test split
# set.seed(5118)
fit_d = filter(model_d, !is.na(group))

# Fit models -----
cities = unique(fit_d$city)
names(cities) = cities
groups = unique(fit_d$group)
names(groups) = groups


m_full = map(cities[cities!='phoenix'], function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    d = filter(fit_d, city == x)
    m = neighborhood_model(form_full_treat, d, verbose=T, tol_rel_grad=1e4)
    cli::cli_process_done()
    m
})


# FIT PHOENIX APPROXIMATELY, BY HAND
d = filter(fit_d, city == "phoenix")
fit_d_ph = filter(d, ring > 0) %>%
    ungroup() %>%
    mutate(across(where(is.factor), droplevels),
           id = as.factor(id))
fit_form = update.formula(form_full_treat, I(1-incl) ~ log(dist) + . + s(id, bs="re"))
m_bam = mgcv::bam(fit_form, data=fit_d_ph, family=binomial(link="cloglog"),
                  discrete=T)

# format so we can plot
draws = mvtnorm::rmvnorm(1000, m_bam$coefficients, m_bam$Vc)
idx_z = which(str_detect(colnames(draws), "s\\(id\\)"))
idx_alpha = which(str_detect(colnames(draws), "log\\(dist\\)"))
colnames(draws) = str_glue("coefs[{colnames(draws)}]")
colnames(draws)[idx_z] = str_glue("id_ranef[{str_sub(colnames(draws)[idx_z], 7)}]")
colnames(draws)[idx_alpha] = "alpha"
out = list(post = posterior::as_draws_rvars(draws))
names(out$post$id_ranef) = levels(fit_d_ph$id)
out$post$coefs = out$post$coefs / out$post$alpha
m_full$phoenix = out


# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)


coefs = list(
    `Fraction same\nrace (Subset: white -- Treatment: Race - Control)` = map2(get_coef(m_full, "pct_race:groupR"),get_coef(m_full, "pct_race:groupC"),`-`),
    `Fraction same\nrace (Subset: white -- Treatment: Race - Race Placebo)` = map2(get_coef(m_full, "pct_race:groupR"),get_coef(m_full, "pct_race:groupRH"),`-`),


    `Fraction same\nrace (Subset: minority -- Treatment: Race - Control)` = map2(map2(get_coef(m_full, "pct_race:groupR"),
                                                                                      get_coef(m_full, "minorityTRUE:pct_race:groupR"),
                                                                                      `+`),map2(get_coef(m_full, "pct_race:groupC"),
                                                                                                get_coef(m_full, "minorityTRUE:pct_race:groupC"),
                                                                                                `+`),`-`),
    `Fraction same\nrace (Subset: minority -- Treatment: Race - Race Placebo)` = map2(map2(get_coef(m_full, "pct_race:groupR"),
                                                                                           get_coef(m_full, "minorityTRUE:pct_race:groupR"),
                                                                                           `+`),map2(get_coef(m_full, "pct_race:groupRH"),
                                                                                                     get_coef(m_full, "minorityTRUE:pct_race:groupRH"),
                                                                                                     `+`),`-`),

    `Fraction same\nparty (Subset: Dem. -- Treatment: Party - Control)` = map2(get_coef(m_full, "groupP:pct_party"),get_coef(m_full, "groupC:pct_party"),`-`),

    `Fraction same\nparty (Subset: Dem. -- Treatment: Party - Party Placebo)` = map2(get_coef(m_full, "groupP:pct_party"),get_coef(m_full, "groupPH:pct_party"),`-`),


    `Fraction same\nparty (Subset: Rep. -- Treatment: Party - Control)` = map2(map2(get_coef(m_full, "groupP:pct_party"),
                                                                                    get_coef(m_full, "partyrep:groupP:pct_party"), `+`),map2(get_coef(m_full, "groupC:pct_party"),
                                                                                                                                             get_coef(m_full, "partyrep:groupC:pct_party"), `+`),`-`),

    `Fraction same\nparty (Subset: Rep. -- Treatment: Party - Party Placebo)` = map2(map2(get_coef(m_full, "groupP:pct_party"),
                                                                                          get_coef(m_full, "partyrep:groupP:pct_party"), `+`),map2(get_coef(m_full, "groupPH:pct_party"),
                                                                                                                                                   get_coef(m_full, "partyrep:groupPH:pct_party"), `+`),`-`)
)


coef_d = make_coef_d(coefs)


dodger = position_dodge(width=0.7)
p1 = coef_d %>%
    mutate(n=1:n(),
           group = case_when(grepl('white', coef)|grepl('Dem.',coef) ~ '1',
                             grepl('minority', coef)|grepl('Rep.',coef)~ '2'))%>%
    mutate(stripe = group) %>%
ggplot(aes(q50, coef, color=city, group=city)) +
    geom_tile(aes(x=0, height=1.0, width=1.20, fill=as.factor(stripe)),
    color=NA) +
    geom_vline(xintercept=0, color="#00000044") +
    geom_linerange(aes(xmin=q5, xmax=q95), linewidth=1, position=dodger) +
    geom_linerange(aes(xmin=q25, xmax=q75), linewidth=1.8, position=dodger) +
    geom_point(position=dodger, size=2.6) +
    scale_color_wa_d() +
    scale_fill_manual(values=c("#00000000", "#0000000C"), guide="none") +
    geom_text(aes(x=q95+0.005, label=percent(q50, accuracy=0.1)),
              position=dodger, hjust=0, size=2.5, show.legend=FALSE,
              family="Times", color="#444444") +
    scale_x_continuous(labels=percent) +
    scale_y_discrete(expand=expansion(mult=0)) +
    coord_cartesian(xlim=c(-0.4, 0.4)) +

    labs(x="Percentage point change in probability of inclusion at boundary",
         y=NULL, color=NULL) +
    theme_paper() +
    theme(legend.position='bottom',
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/experiment_coef_selected_diff.pdf"))||REGENERATE)
    ggsave(figpath, plot=p1, width=6.5, height=8)



# Table output

proc_coef_names = function(x) {
    y = x %>%
        str_remove_all("TRUE") %>%
        str_remove_all("Yes") %>%
        str_replace_all(":", " * ") %>%
        str_replace("partyind", "party = IND") %>%
        str_replace("partyrep", "party = REP") %>%
        str_replace("educ_grpno_coll", "educ = No College") %>%
        str_replace("educ", "education") %>%
        str_replace("pct_", "Fraction same ") %>%
        str_replace("is_", "") %>%
        str_replace("_bg", " block group") %>%
        str_replace("_tract", " tract") %>%
        str_replace("dist_", "distance to") %>%
        str_replace("_rr", " road region") %>%
        str_replace("children_home", "children") %>%
        str_replace("pop", "population") %>%
        str_replace("med_inc", "income") %>%
        str_replace(" own", " ownership") %>%
        str_replace("group([A-Z]+)", "\\1 group") %>%
        str_replace_all("(log|sqrt)\\((\\w+).*\\)", "\\2")
    names(y) = x
    y
}


sum_phoenix = summary(m_bam)

m_summ = list(`new-york` = summary(m_full$`new-york`),
              miami = summary(m_full$miami))

m_summ$phoenix = tibble(
    variable = str_replace(names(sum_phoenix$p.coeff), "log\\(dist\\)", "alpha"),
    mean = sum_phoenix$p.coeff,
    median = sum_phoenix$p.coeff,
    sd = sum_phoenix$p.table[, 2],
    mad = NA
) |>
    mutate(q5 = mean + qnorm(0.05) * sd,
           q95 = mean + qnorm(0.95) * sd)

bind_rows(m_summ, .id="city") %>%
    select(coefficient=variable, city, mean, `std. dev.`=sd, q5, median, q95) %>%
    rename_with(str_to_title) %>%
    mutate(Coefficient = fct_inorder(proc_coef_names(Coefficient)),
           City = lbl_city(City)) %>%
    arrange(Coefficient, City) %>%
    knitr::kable(format="latex", digits=2, longtable=TRUE, booktabs=TRUE,
                 caption="Full experimental sample model estimates.") %>%
    kableExtra::kable_styling(font_size=9, latex_options=c("striped"),
                              stripe_index=which(rep(rep(c(T, F), 100), each=3))) %>%
    write_file(here("paper/tables/model_coef_experiment.tex"))

