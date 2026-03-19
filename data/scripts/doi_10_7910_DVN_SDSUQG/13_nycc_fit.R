# reload original preproc function

REGENERATE = T

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

m_full = neighborhood_model(form_full, filter(fit_d))

m_baseline =  neighborhood_model(form_baseline_nodemg, filter(fit_d))



model_d_meta = raw_model_d %>%
    filter(survey=='meta')%>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist) %>%
    group_by(ring) %>%
    mutate(dist = if_else(dist == 0 & ring > 0, dist + 0.5*mean(dist), dist)) %>%
    group_by(id)

m_meta = neighborhood_model(form_full, data=model_d_meta)

model_d_email = raw_model_d %>%
    filter(survey=='email')%>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist) %>%
    group_by(ring) %>%
    mutate(dist = if_else(dist == 0 & ring > 0, dist + 0.5*mean(dist), dist)) %>%
    group_by(id)
m_email = neighborhood_model(form_full, data=model_d_email)




# Coefficient figures -----
get_coef = function(m, name) list(`new-york`=m_full$post$coefs[name] * m_full$post$alpha)
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
    mutate(stripe = (floor(as.integer(coef)/2+0.5) %% 2)) %>%
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
    scale_color_manual(values=PAL_CITY["NYC"], guide="none") +
    scale_fill_manual(values=c("#00000000", "#0000000C"), guide="none") +
    labs(x="Percentage point change in probability of inclusion at boundary",
         y=NULL, color=NULL) +
    theme_paper() +
    theme(legend.position=c(0.92, 0.7),
          legend.background=element_blank())


if (!file.exists(figpath <- here("paper/figures/full_coef_selected_nycc.pdf")) || REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=3.5)

### facebook survey only
get_coef = function(m, name) list(`new-york`=m_meta$post$coefs[name] * m_meta$post$alpha)

coefs_meta = list(
    `Block population` = get_coef(m_meta, "sqrt(pop/10000)"),
    `Block area` = get_coef(m_meta, "sqrt(area)"),
    #same_tract = get_coef(m_meta, "same_tractTRUE"),
    #same_rr = get_coef(m_meta, "same_rrTRUE"),
    #is_park = get_coef(m_meta, "is_parkTRUE"),
    Church = get_coef(m_meta, "is_churchTRUE"),
    `Distance to church` = get_coef(m_meta, "log(dist_church)"),
    #`med_inc` = get_coef(m_meta, "log(med_inc)"),
    `Fraction same\nrace (white)` = get_coef(m_meta, "pct_race"),
    `Fraction same\nrace (minority)` = map2(get_coef(m_meta, "pct_race"),
                                            get_coef(m_meta, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nparty (Dem.)` = get_coef(m_meta, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_meta, "pct_party"),
    #                         get_coef(m_meta, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Rep.)` = map2(get_coef(m_meta, "pct_party"),
                                         get_coef(m_meta, "pct_party:partyrep"), `+`),
    `Fraction same\neducation (college)` = get_coef(m_meta, "pct_educ"),
    `Fraction same\neducation (no college)` = map2(get_coef(m_meta, "pct_educ"),
                                                   get_coef(m_meta, "pct_educ:educ_grpno_coll"),
                                                   `+`)
)


coef_d_meta = make_coef_d(coefs_meta)

make_coef_table(list(`new-york`=m_meta)) %>%
    write_csv(here("paper/dataverse_fits/nycc_meta_only.csv"))


dodger = position_dodge(width=0.7)
p_meta = coef_d_meta %>%
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
          legend.background=element_blank())+
    guides(color = 'none')


if (!file.exists(figpath <- here("paper/figures/full_coef_selected_nycc_meta.pdf"))|| REGENERATE)
    ggsave(figpath, plot=p_meta, width=6.5, height=5)
# ### email survey only

get_coef = function(m, name) list(`new-york`=m_email$post$coefs[name] * m_email$post$alpha)

coefs_email = list(
    `Block population` = get_coef(m_email, "sqrt(pop/10000)"),
    `Block area` = get_coef(m_email, "sqrt(area)"),
    #same_tract = get_coef(m_email, "same_tractTRUE"),
    #same_rr = get_coef(m_email, "same_rrTRUE"),
    #is_park = get_coef(m_email, "is_parkTRUE"),
    Church = get_coef(m_email, "is_churchTRUE"),
    `Distance to church` = get_coef(m_email, "log(dist_church)"),
    #`med_inc` = get_coef(m_email, "log(med_inc)"),
    `Fraction same\nrace (white)` = get_coef(m_email, "pct_race"),
    `Fraction same\nrace (minority)` = map2(get_coef(m_email, "pct_race"),
                                            get_coef(m_email, "pct_race:minorityTRUE"),
                                            `+`),
    `Fraction same\nparty (Dem.)` = get_coef(m_email, "pct_party"),
    #`pct_party (IND)` = map2(get_coef(m_email, "pct_party"),
    #                         get_coef(m_email, "pct_party:partyind"), `+`),
    `Fraction same\nparty (Rep.)` = map2(get_coef(m_email, "pct_party"),
                                         get_coef(m_email, "pct_party:partyrep"), `+`),
    `Fraction same\neducation (college)` = get_coef(m_email, "pct_educ"),
    `Fraction same\neducation (no college)` = map2(get_coef(m_email, "pct_educ"),
                                                   get_coef(m_email, "pct_educ:educ_grpno_coll"),
                                                   `+`)
)


coef_d_email = make_coef_d(coefs_email)

make_coef_table(list(`new-york`=m_email)) %>%
    write_csv(here("paper/dataverse_fits/nycc_email_only.csv"))


dodger = position_dodge(width=0.7)
p_email = coef_d_email %>%
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
          legend.background=element_blank())+
    guides(color = 'none')


if (!file.exists(figpath <- here("paper/figures/full_coef_selected_nycc_email.pdf"))|| REGENERATE)
    ggsave(figpath, plot=p_email, width=6.5, height=5)


if (!file.exists(figpath <- here("paper/figures/full_coef_selected_combined_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, plot=(p_meta+ggtitle('Meta survey') + p_email+ggtitle('Email survey'))+plot_layout(guides="collect"), width=11, height=5)
}

if (!file.exists(figpath <- here("paper/figures/full_coef_selected_combined_nycc.png")) || REGENERATE) {
    ggsave(figpath, plot=(p_meta+ggtitle('Meta survey') + p_email+ggtitle('Email survey'))+plot_layout(guides="collect"), width=11, height=5)
}


## tables

make_coef_table(list(m_full)) %>%
    fmt_coef_table("Full model estimates.") %>%
    write_file(here("paper/tables/model_coef_full_nycc.tex"))

make_coef_table(list(m_baseline)) %>%
    fmt_coef_table("Baseline model estimates.") %>%
    write_file(here("paper/tables/model_coef_base_nycc.tex"))



