library(modelsummary)
library(Hmisc)
REGENERATE = T

# Calculate neighborhood summary statistics ----

nbhd_stats_path = here("data/nhbd_stats_full.rds")
#resp = resp_d
if (file.exists(nbhd_stats_path)) {
    nbhds_resps = read_rds(nbhd_stats_path)
} else {
    resp_d = resp_d %>% filter(id %in% model_d$id)
    # fill in gaps in block data
   # block_d = mutate(block_d, across(med_inc:pct_homeown, ~ coalesce(., median(., na.rm=T))))

    split_block_d = split(block_d, ~ city)
    resps = semi_join(resp_d, model_d, by="id")

    # helper function to calculate neighborhood demographics for each respondent
    resp_nbhd = function(resp, id, split_block_d) {
        city_d = split_block_d[[resp$city]]
        nbhd_demg(resp$neighborhood, city_d) %>%
        bind_cols(select(resp, age:retired, race:marital_status, party_combined,
                         map_clicks, map_time))# %>%
          #  mutate(test_only = !(resp$id %in% fit_ids))
    }

    nbhds_resps = group_by(resps, id) %>%
        group_modify(resp_nbhd,  split_block_d, .keep=TRUE)

    write_rds(nbhds_resps, nbhd_stats_path, compress="xz")
}


# Tables of summary statistics ----

surv = resp_d %>%
   # filter(id %in% nbhds_resps$id) %>% # limit to respondents in analysis sample
    mutate(dem = as.numeric(substr(party_combined,1,3)=='dem'),
           rep = as.numeric(substr(party_combined,1,3)=='rep'),
           white = as.numeric(race=='white'),
           income_num = recode(income, `$100,000 -119,999` = 110000,
                               `$120,000  or More` = 160000,
                               `$30,000 – 39,999` = 35000,
                               `$40,000 – 49,999` = 45000,
                               `$50,000 – 59,999` = 55000,
                               `$60,000 – 69,999` = 65000,
                               `$70,000 – 79,999` = 75000,
                               `$80,000 – 89,999` = 85000,
                               `$90,000 – 99,999` = 95000,
                               `Less than $30,000` = 20000),
           female = as.numeric(gender=='female'),
           biden = as.numeric(president=='biden'),
           homeowner_num = as.numeric(homeowner=='Yes'),
           married = as.numeric(marital_status=='Married'),
           children_home_num = as.numeric(children_home=='Yes'),
           college = as.numeric(education %in% c('grad_2yr','grad_4yr', 'postgrad')),
           miami = as.numeric(city=='miami'),
           nyc = as.numeric(city=='new-york'),
           phoenix = as.numeric(city=='phoenix'))

## make summary statistics of the sample

resp_stat_path = here("paper/tables/survey-summary-full-final.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(resp_stat_path)|| REGENERATE) {
  datasummary(data = surv,
              formula = dem + rep + biden + age + female + white +
                income_num  + college + housing_tenure + homeowner_num +
                married + children_home_num ~
                city * (Mean + SD) + Mean + SD,
              output='latex') %>%
    write_file(file=resp_stat_path)
}

# Make summary statistics of the neighborhoods
  # reformat neighborhood dataframe

d = nbhds_resps %>%
  group_by(id) %>%
  summarize(democrats = sum(democrats, na.rm =T),
            registrants = sum(registrants, na.rm = T),
            republicans = sum(republicans, na.rm = T),
            pop = sum(pop,na.rm=T),
            pop_white = sum(pop_white,na.rm=T),
            pop_black = sum(pop_black,na.rm=T),
            area = sum(area,na.rm=T),
            city = city[1])%>%
  mutate(dem_p = democrats/registrants,
         rep_p = republicans/registrants,
         white_p = pop_white/pop,
         black_p = pop_black/pop,
         sq.miles = area / M_PER_MI^2)


Mean=function(x){mean(x,na.rm=T)}
SD=function(x){sd(x,na.rm=T)}

nbhd_table_path = here("paper/tables/nbh-summary-full.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)|| REGENERATE) {
    datasummary(data = d,
                formula = pop + sq.miles + dem_p + rep_p + white_p + black_p ~
                    city*(Mean + Median + Min + Max) +
                    Mean + Median + Min + Max,
                output = 'latex') %>%
        write_file(file=nbhd_table_path)
}



# Plots of summary statistics ----

d_pooled = d %>%
    select(city, pop, sq.miles, dem_p, rep_p, white_p, black_p) %>%
    mutate(city='Pooled') %>%
    pivot_longer(cols = c('pop', 'sq.miles', 'dem_p', 'rep_p', 'white_p', 'black_p'))

d_plot = d %>%
    select(city, pop, sq.miles, dem_p, rep_p, white_p, black_p) %>%
    pivot_longer(cols = c('pop', 'sq.miles', 'dem_p', 'rep_p', 'white_p', 'black_p')) %>%
    bind_rows(d_pooled) %>%
    mutate(city = recode(city, `miami`='Miami', `new-york`='NYC',
                         `phoenix`='Phoenix', `Pooled`='Pooled'),
           name = recode(name, `pop`='Population', `sq.miles`='Area (Square miles)',
                         `dem_p`='Prop. Dem.', `rep_p`='Prop. Rep.',
                         `white_p`='Prop. White', `black_p`='Prop. Black')) %>%
    mutate(city = factor(city, levels = c('Miami', 'NYC', 'Phoenix', 'Pooled')),
           name = factor(name, levels = c('Population', 'Area (Square miles)',
                                          'Prop. Dem.', 'Prop. Rep.',
                                          'Prop. White', 'Prop. Black')))

make_sum_plot = function(var, xlab="City", ...) {
    p = d_plot %>%
        filter(name == var) %>%
    ggplot(aes(x=city, y=value, fill=city)) +
        geom_boxplot(outlier.size=0.8, ...) +
        scale_fill_manual(values=PAL_CITY, guide="none") +
        labs(y=NULL, x=xlab, title=var) +
        theme_paper() +
        theme(panel.grid.major.x=element_blank())
    if (var %in% c("Population", "Area (Square miles)")) {
        p = p + scale_y_continuous(labels=comma, trans="log10")
    } else if (str_starts(var, "Prop.")) {
        p = p + scale_y_continuous(labels=percent)
    }
    p
}
p = make_sum_plot("Population", xlab=NULL) +
    make_sum_plot("Prop. Dem.", xlab=NULL) +
    make_sum_plot("Prop. White", xlab=NULL) +
    make_sum_plot("Area (Square miles)") +
    make_sum_plot("Prop. Rep.") +
    make_sum_plot("Prop. Black")
if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_full.pdf")) || REGENERATE) {
    ggsave(figpath, p, width=6.5, height=4.5) %>%
        suppressWarnings()
}





###### Make comparisons across respondent party and race

d_race = nbhds_resps %>%
  mutate(white = ifelse(race=='white','white', 'non-white'))%>%
  group_by(id, white) %>%
  summarize(democrats = sum(democrats, na.rm =T),
            registrants = sum(registrants, na.rm = T),
            republicans = sum(republicans, na.rm = T),
            pop = sum(pop,na.rm=T),
            pop_white = sum(pop_white,na.rm=T),
            pop_black = sum(pop_black,na.rm=T),
            area = sum(area,na.rm=T),
            city = city[1])%>%
  mutate(dem_p = democrats/registrants,
         rep_p = republicans/registrants,
         white_p = pop_white/pop,
         black_p = pop_black/pop,
         sq.miles = area / M_PER_MI^2)

nbhd_table_path = here("paper/tables/nbh-summary-race-full.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)||REGENERATE) {
  datasummary(data = d_race,
              formula =  white_p +black_p  ~
                city*(white*(Mean )) +
                white*(Mean),
              output = 'latex') %>%
    write_file(file=nbhd_table_path)
}


d_pooled = d_race %>%
  select(city, white, white_p, black_p) %>%
  mutate(city='Pooled') %>%
  pivot_longer(cols = c( 'white_p', 'black_p'))

d_plot = d_race %>%
  select(city,  white, white_p, black_p) %>%
  pivot_longer(cols = c( 'white_p', 'black_p')) %>%
  bind_rows(d_pooled) %>%
  mutate(city = recode(city, `miami`='Miami', `new-york`='NYC',
                       `phoenix`='Phoenix', `Pooled`='Pooled'),
         name = recode(name,
                       `white_p`='Prop. White', `black_p`='Prop. Black')) %>%
  mutate(city = factor(city, levels = c('Miami', 'NYC', 'Phoenix', 'Pooled')),
         name = factor(name, levels = c(
           'Prop. White', 'Prop. Black')),
         white = capitalize(white))


make_sum_plot_by_race = function(var, xlab="City", ...) {
  p = d_plot %>%
    filter(name == var & !is.na(white)) %>%
    ggplot(aes(x=city, y=value, fill=white)) +
    geom_boxplot(outlier.size=0.8, position=position_dodge(.8),...) +
    # scale_fill_manual(values=PAL_CITY, guide="none") +
    scale_fill_wa_d("sound_sunset", which=c(13,8)) +
    labs(y=NULL, x=xlab, title=var) +
    theme_paper() +
    theme(panel.grid.major.x=element_blank(),legend.position='bottom', legend.title=element_blank())
  if (var %in% c("Population", "Area (Sq. Mi.)")) {
    p = p + scale_y_continuous(labels=comma, trans="log10")
  } else if (str_starts(var, "Prop.")) {
    p = p + scale_y_continuous(labels=percent)
  }
  p
}
p = make_sum_plot_by_race("Prop. White") +
  make_sum_plot_by_race("Prop. Black")

if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_by_race_full.pdf")) || REGENERATE) {
  ggsave(figpath, p, width=7, height=3.5) %>%
    suppressWarnings()
}



d_party = nbhds_resps %>%
  mutate(party3 = case_when(grepl('dem_',party_combined) ~'Democrat',
                            grepl('rep_',party_combined)~'Republican',
         T ~ 'Independent')) %>%
  group_by(id, party3) %>%
  summarize(democrats = sum(democrats, na.rm =T),
            registrants = sum(registrants, na.rm = T),
            republicans = sum(republicans, na.rm = T),
            pop = sum(pop,na.rm=T),
            pop_white = sum(pop_white,na.rm=T),
            pop_black = sum(pop_black,na.rm=T),
            area = sum(area,na.rm=T),
            city = city[1])%>%
  mutate(dem_p = democrats/registrants,
         rep_p = republicans/registrants,
         white_p = pop_white/pop,
         black_p = pop_black/pop,
         sq.miles = area / M_PER_MI^2)

nbhd_table_path = here("paper/tables/nbh-summary-party-full.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)||REGENERATE) {
  datasummary(data = d_party,
              formula =  dem_p+ rep_p  ~
                city*(party3*(Mean )) +
                party3*(Mean),
              output = 'latex') %>%
    write_file(file=nbhd_table_path)
}


d_pooled = d_party %>%
  select(city, party=party3, dem_p, rep_p) %>%
  mutate(city='Pooled') %>%
  pivot_longer(cols = c( 'dem_p', 'rep_p'))

d_plot = d_party %>%
  select(city,  party=party3, dem_p, rep_p) %>%
  pivot_longer(cols = c( 'dem_p', 'rep_p')) %>%
  bind_rows(d_pooled) %>%
  mutate(city = recode(city, `miami`='Miami', `new-york`='NYC',
                       `phoenix`='Phoenix', `Pooled`='Pooled'),
         name = recode(name,
                       `dem_p`='Prop. Democrat', `rep_p`='Prop. Republican')) %>%
  mutate(city = factor(city, levels = c('Miami', 'NYC', 'Phoenix', 'Pooled')),
         name = factor(name, levels = c(
           'Prop. Democrat', 'Prop. Republican')))


make_sum_plot_by_party = function(var, xlab="City", ...) {
  p = d_plot %>%
    filter(name == var & !is.na(party)) %>%
    ggplot(aes(x=city, y=value, fill=party)) +
    geom_boxplot(outlier.size=0.8, position=position_dodge(.85),...) +
    scale_fill_manual(values=GOP_DEM[c(14, 8, 2)], name=NULL,
                      labels=c("Democrat", "Independent", "Republican")) +
    labs(y=NULL, x=xlab, title=var) +
    theme_paper() +
    theme(panel.grid.major.x=element_blank(),legend.position='bottom', legend.title=element_blank())
  if (var %in% c("Population", "Area (Sq. Mi.)")) {
    p = p + scale_y_continuous(labels=comma, trans="log10")
  } else if (str_starts(var, "Prop.")) {
    p = p + scale_y_continuous(labels=percent)
  }
  p
}
p = make_sum_plot_by_party("Prop. Democrat") +
  make_sum_plot_by_party("Prop. Republican")

if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_by_party_full.pdf")) || REGENERATE) {
  ggsave(figpath, p, width=7, height=4) %>%
    suppressWarnings()
}


####
library(xtable)

block_d %>%
    group_by(city)%>%
    summarize(
              tot_pop = sum(pop, na.rm=T),
              tot_reg = sum(registrants, na.rm=T),
              tot_pop_white = sum(pop_white,na.rm=T),
              tot_pop_black = sum(pop_black,na.rm=T),
              tot_pop_hisp = sum(pop_hisp,na.rm=T),
              tot_dem = sum(democrats,na.rm=T),
              tot_rep = sum(republicans,na.rm=T),
              pct_college = round(weighted.mean(pct_college, w = pop),3),
              pct_homeown = round(weighted.mean(pct_homeown, w = pop),3),
              med_inc = round(weighted.mean(med_inc, w = pop),0)

              )%>%
    mutate(
        pct_reg = round(tot_reg/tot_pop,3),
        pct_white = round(tot_pop_white/tot_pop,3),
        pct_black = round(tot_pop_black/tot_pop,3),
        pct_hisp = round(tot_pop_hisp/tot_pop,3),
        pct_dem = round(tot_dem/tot_reg,3),
        pct_rep = round(tot_rep/tot_reg,3) )%>%
    select(city,pct_college:pct_rep)%>%
    t()%>%
    xtable(digits=3) %>%
    print(file=here('paper/tables/city-pop-summary-final.tex'))








