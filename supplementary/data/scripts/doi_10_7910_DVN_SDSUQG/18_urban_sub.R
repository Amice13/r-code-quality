# calculate urban/suburban
REGENERATE = T

if (!file.exists(path <- here("data/city_boundaries.rds"))) {
    path_raw <- here("data-raw/500Cities_City_11082016/CityBoundaries.shp")
    city_shp <- read_sf(path_raw) %>%
        filter(NAME %in% c("New York", "Phoenix", "Miami")) %>%
        transmute(city = str_replace(str_to_lower(NAME), " ", "-")) %>%
        st_transform(4326) %>%
        st_make_valid()
    write_rds(city_shp, path, compress="xz")
} else {
    city_shp <- read_rds(path)
}

res = st_within(block_d$centroid, city_shp$geometry, sparse=FALSE)

block_d$city_proper = rep(NA_integer_, nrow(block_d))
block_d$city_proper[res[, 1]] = 1L
block_d$city_proper[res[, 2]] = 2L
block_d$city_proper[res[, 3]] = 3L
levels(block_d$city_proper) = city_shp$city
class(block_d$city_proper) = "factor"
rm(res)


# rerun model

raw_model_d = make_model_frame(resp_d, block_d, adj_gr)

# Define model formulae ---------
form_full_urbsub = ~ is_church + log(dist_church) + is_park +
    is_school*children_home + log(dist_school)*children_home +
    same_bg + same_tract * same_rr + same_tract * urban +
    sqrt(pop/1e4) + sqrt(area) +
    pct_race + pct_race:minority + pct_race:urban +
    pct_party + pct_party:party +
    pct_own + pct_own:homeowner +
    pct_educ + pct_educ:educ_grp +
    log(med_inc) + log(med_inc):educ_grp +
    age + educ_grp + retired + sqrt(tenure) +
    party + minority + homeowner + children_home

preproc = function(d) {
    d %>%
        mutate(block = fips,
               block_group = str_sub(block, 1, 12),
               tract = str_sub(block, 1, 11),
               is_school = coalesce(type == "school", FALSE),
               is_church = coalesce(type == "church", FALSE),
               is_park = coalesce(type == "park", FALSE),
               area = area / M_PER_MI^2,
               urban = !is.na(city_proper),
               pop_race = case_when(race == "white" ~ pop_white,
                                    race == "black" ~ pop_black,
                                    race == "hisp" ~ pop_hisp,
                                    T ~ pop - pop_white - pop_black - pop_hisp),
               pop_race = pmax(0, pop_race),
               race = if_else(race %in% c("white", "black", "hisp"), race, "other"),
               party = str_sub(party_combined, 1, 3),
               party_strong = party_combined %in% c("dem_strong", "rep_strong"),
               reg_party = case_when(
                   party_combined %in% c("dem_strong", "dem_lean") ~ democrats,
                   party_combined %in% c("rep_strong", "rep_lean") ~ republicans,
                   T ~ registrants - democrats - republicans),
               pct_race = pop_race / (pop + 0.001),
               pct_party = reg_party / (registrants + 0.001),
               minority = !(race == "white"),
               income_est  = case_when(income == "Less than $30,000" ~ 15e3,
                                       income == "$30,000 – 39,999" ~ 35e3,
                                       income == "$40,000 – 49,999" ~ 45e3,
                                       income == "$50,000 – 59,999" ~ 55e3,
                                       income == "$60,000 – 69,999" ~ 65e3,
                                       income == "$70,000 – 79,999" ~ 75e3,
                                       income == "$80,000 – 89,999" ~ 85e3,
                                       income == "$90,000 – 99,999" ~ 95e3,
                                       income == "$100,000 -119,999" ~ 110e3,
                                       income == "$120,000  or More" ~ 160e3),
               income_dist = abs(log(income_est) - log(med_inc)) /
                   mean(log(med_inc), na.rm=T),
               educ_grp = case_when(education %in% c("no_hs", "some_hs",
                                                     "hs", "some_coll") ~ "no_coll",
                                    education %in% c("grad_2yr", "grad_4yr", "postgrad") ~ "coll"),
               pct_educ = if_else(educ_grp == "coll", pct_college, 1 - pct_college),
               pct_own = if_else(homeowner == "Yes", pct_homeown, 1 - pct_homeown),
               age_grp = case_when(age <= 40 ~ "00_40",
                                   age > 40 & age <= 55 ~ "41_55",
                                   age > 55 & age <= 65 ~ "56_65",
                                   age > 65 & age <= 75 ~ "66_75",
                                   age > 75 ~ "76_99")) %>%
        select(city, block, block_group, tract, rr_id,
               is_school, is_church, is_park, dist_school, dist_church,
               id, incl, urban, pop, race, pop_race,
               reg=registrants, reg_party, pct_race,
               pct_party, party, party_combined, party_strong,
               gender, minority, age, age_grp, educ_grp, pct_educ,
               med_inc, income_est, income_dist,
               tenure=housing_tenure, homeowner, retired,
               children_home, pct_own, pct_homeown, pct_college,
               area, any_of("ring"), dist,
               trust_1, trust_2, trust_3, housing) %>%
        group_by(id) %>%
        mutate(same_bg = block_group == block_group[1],
               same_tract = tract == tract[1],
               same_rr = rr_id == rr_id[1])
}

model_d = raw_model_d %>%
    filter(group == "C") %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist, -housing, -starts_with("trust"))

# Fit models -----
cities = unique(model_d$city)
names(cities) = cities
m_full = map(cities, function(x) {
    cli::cli_process_start("Fitting full model for {x}")
    m = neighborhood_model(form_full_urbsub, filter(model_d, city == x))
    cli::cli_process_done()
    m
})


# Coefficient figures -----
get_coef = function(m, name) map(m, ~ .$post$coefs[name] * .$post$alpha)
coefs = list(
    `Urban direct effect` = get_coef(m_full, "urbanTRUE"),
    `Urban-same tract interaction` = get_coef(m_full, "same_tractTRUE:urbanTRUE"),
    `Urban-same race interaction` = get_coef(m_full, "urbanTRUE:pct_race")
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
    theme(legend.position=c(0.92, 0.45),
          legend.background=element_blank())
if (!file.exists(figpath <- here("paper/figures/full_coef_selected_urban.pdf"))||REGENERATE)
    ggsave(figpath, plot=p, width=6.5, height=2.25)


make_coef_table(m_full) %>%
    write_csv(here("paper/dataverse_fits/full_urban_ind.csv"))
