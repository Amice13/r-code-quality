library(dplyr)
library(xtable)
library(BBmisc)
library(data.table)
library(plotrix)
library(estimatr)
library(stargazer)

model_d = raw_model_d %>%
    # filter(group == "C") %>%
    preproc() %>%
    drop_na(city, gender, race, minority, age_grp, educ_grp, tenure, retired,
            homeowner, children_home, party) %>%
    select(-income_est, -income_dist)

### Number of usable neighborhoods
length(unique(model_d$id))


## Get counts of usable neighborhood respondents in each survey group
raw_model_d %>%
    group_by(id,  survey)%>%
    dplyr::summarize(n=n())%>%
    group_by( survey)%>%
    dplyr::summarize(n())




#### Make table A1


x = resp_d %>%
   # mutate(housing_tenure = ifelse(housing_tenure > 1900, 2021-housing_tenure,
    #                               ifelse(housing_tenure >90, NA,housing_tenure)))%>%
    filter(id%in%model_d$id)%>%
    group_by(survey)%>%
    dplyr::summarize(N=n(),
              Democrat_mean=mean(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),
              Democrat_sd=sd(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),

                Repubican_mean = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),
              Repubican_sd = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),

                VoteEricAdams_mean = mean(mayor=='Eric Adams',na.rm=T),
              VoteEricAdams_sd = sd(mayor=='Eric Adams',na.rm=T),

              Age_mean = mean(age,na.rm=T),
              Age_sd = sd(age,na.rm=T),

              Female_mean = mean(gender=='female',na.rm=T),
              Female_sd = sd(gender=='female',na.rm=T),

              Income_mean = mean(dplyr::recode(income,
                                               `$120,000  or More`= 160 ,
                                               `$75,000 – 119,999`= 97.5,
                                               `$50,000 – 74,999`= 62.5,
                                               `$25,000 – 49,999`= 37.5,
                                               `Less than $25,000 `= 20), na.rm=T),
              Income_sd = sd(dplyr::recode(income,
                                              `$120,000  or More`= 160 ,
                                              `$75,000 – 119,999`= 97.5,
                                              `$50,000 – 74,999`= 62.5,
                                              `$25,000 – 49,999`= 37.5,
                                              `Less than $25,000 `= 20), na.rm=T),

                Education_mean = mean(education %in% c('grad_2yr','grad_4yr','postgrad')),
              Education_sd = sd(education %in% c('grad_2yr','grad_4yr','postgrad')),

              YearsResidence_mean = mean(housing_tenure, na.rm=T),
              YearsResidence_sd = sd(housing_tenure, na.rm=T),

              Homeowner_mean = mean(homeowner=='Homeowner', na.rm=T),
              Homeowner_sd = sd(homeowner=='No', na.rm=T),

              Married_mean = mean(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),
              Married_sd = sd(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),

                ChildrenInHome_mean = mean(children_home=='Yes', na.rm=T),
              ChildrenInHome_sd = sd(children_home=='Yes', na.rm=T)


              )


x = x %>%
    pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
    filter(grepl('_mean', name))%>%
    select(survey, name, mean=value)%>%
    mutate(name = str_replace(name,'_mean', ''))%>%
    left_join(x %>%
                  pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
                  filter(grepl('_sd', name))%>%
                  select(survey, name, sd=value)%>%
                  mutate(name = str_replace(name,'_sd', '')),by=c('survey','name'))%>%
    mutate(mean=round(mean,2),
           sd = round(sd, 2))


x = x %>%
    filter(survey=='meta')%>%
    left_join(x %>%
                  filter(survey=='email'),by='name')





pooled = resp_d %>%
    filter(id%in%model_d$id)%>%
    dplyr::summarize(N=n(),
              Democrat_mean=mean(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),
              Democrat_sd=sd(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),

              Repubican_mean = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),
              Repubican_sd = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),

                VoteEricAdams_mean = mean(mayor=='Eric Adams',na.rm=T),
              VoteEricAdams_sd = sd(mayor=='Eric Adams',na.rm=T),

              Age_mean = mean(age,na.rm=T),
              Age_sd = sd(age,na.rm=T),

              Female_mean = mean(gender=='female',na.rm=T),
              Female_sd = sd(gender=='female',na.rm=T),

              Income_mean = mean(dplyr::recode(income,
                                               `$120,000  or More`= 160 ,
                                               `$75,000 – 119,999`= 97.5,
                                               `$50,000 – 74,999`= 62.5,
                                               `$25,000 – 49,999`= 37.5,
                                               `Less than $25,000 `= 20), na.rm=T),
              Income_sd = sd(dplyr::recode(income,
                                           `$120,000  or More`= 160 ,
                                           `$75,000 – 119,999`= 97.5,
                                           `$50,000 – 74,999`= 62.5,
                                           `$25,000 – 49,999`= 37.5,
                                           `Less than $25,000 `= 20), na.rm=T),

              Education_mean = mean(education %in% c('grad_2yr','grad_4yr','postgrad')),
              Education_sd = sd(education %in% c('grad_2yr','grad_4yr','postgrad')),

              YearsResidence_mean = mean(housing_tenure, na.rm=T),
              YearsResidence_sd = sd(housing_tenure, na.rm=T),

              Homeowner_mean = mean(homeowner=='Homeowner', na.rm=T),
              Homeowner_sd = sd(homeowner=='No', na.rm=T),

              Married_mean = mean(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),
              Married_sd = sd(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),

              ChildrenInHome_mean = mean(children_home=='Yes', na.rm=T),
              ChildrenInHome_sd = sd(children_home=='Yes', na.rm=T)


    )

pooled = pooled %>%
    mutate(survey='pooled')%>%
    pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
    filter(grepl('_mean', name))%>%
    select(survey, name, mean=value)%>%
    mutate(name = str_replace(name,'_mean', ''))%>%
    left_join(pooled %>%
                  mutate(survey='pooled')%>%
                  pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
                  filter(grepl('_sd', name))%>%
                  select(survey, name, sd=value)%>%
                  mutate(name = str_replace(name,'_sd', '')),by=c('survey','name'))%>%
    mutate(mean=round(mean,2),
           sd = round(sd, 2))

x %>%
    left_join(pooled, by='name')%>%
    select(name,
           mean_meta=mean.x, sd_meta=sd.x,
           mean_email=mean.y, sd_email=sd.y,
           mean_pooled=mean, sd_pooled=sd
           )%>%
    xtable







##### Map attrition

    bal = resp_d %>%
        mutate(usable.neighborhood = as.numeric(id%in%model_d$id))%>%
        mutate(
            Democrat = as.numeric(party_combined %in% c('dem_strong','dem_lean')),
            Republican = as.numeric(party_combined %in% c('rep_strong','rep_lean')),
            `Vote Eric Adams` = as.numeric(mayor=='Eric Adams'),
            Age = age,
            Female = as.numeric(gender=='female'),
            Income = dplyr::recode(income,
                                   `$120,000  or More`= 160 ,
                                   `$75,000 – 119,999`= 97.5,
                                   `$50,000 – 74,999`= 62.5,
                                   `$25,000 – 49,999`= 37.5,
                                   `Less than $25,000 `= 20),
            College = as.numeric(education %in% c('grad_2yr','grad_4yr','postgrad')),
            Homeowner = as.numeric(homeowner=='Homeowner'),
            Married = as.numeric(marital_status %in% c('Married', 'Domestic Partnership')),
            `Years Residence` = ifelse(housing_tenure > 1900, 2021-housing_tenure,
                                       ifelse(housing_tenure >90, NA,housing_tenure)),

            ChildrenInHome = as.numeric(children_home=='Yes'))%>%
        select(survey,group,Democrat:`Years Residence`,usable.neighborhood)



m = lm(usable.neighborhood ~ survey  , data = bal )









m2 =  lm(usable.neighborhood ~ survey + Age + College + Democrat + Female + Homeowner + Income + Married + Republican + `Vote Eric Adams` + `Years Residence`, data = bal)





stargazer(m,m2)


summary(m2)




##############

library(modelsummary)
library(Hmisc)
REGENERATE = T

# Calculate neighborhood summary statistics ----

nbhd_stats_path = here("data/nhbd_stats_full_nycc.rds")
#resp = resp_d
if (file.exists(nbhd_stats_path)) {
    nbhds_resps = read_rds(nbhd_stats_path)
} else {
    # resp_d = resp_d %>% filter(id %in% model_d$id)
    # fill in gaps in block data
    # block_d = mutate(block_d, across(med_inc:pct_homeown, ~ coalesce(., median(., na.rm=T))))

    resps = semi_join(resp_d, model_d, by="id")

    nbhds_resps = nbhd_demg(resps$neighborhood, block_d) %>%
        bind_cols(select(resps, id, age:retired, race:marital_status, party_combined,
                         map_clicks, map_time, survey))

    write_rds(nbhds_resps, nbhd_stats_path, compress="xz")
}


# Tables of summary statistics ----

surv = resp_d %>%
   #  filter(id %in% nbhds_resps$id) %>% # limit to respondents in analysis sample
    mutate(dem = as.numeric(substr(party_combined,1,3)=='dem'),
           rep = as.numeric(substr(party_combined,1,3)=='rep'),
           white = as.numeric(race=='white'),
           income_num = as.numeric(case_when( income == "Less than $25,000" ~ 20e3,
                                   income == "$25,000 – 49,999" ~ 37.5e3,
                                   income == "$50,000 – 74,999" ~ 62.5e3,
                                   income == "$75,000 – 119,999" ~ 97.5e3,
                                   income == "$120,000  or More" ~ 160e3)),
           female = as.numeric(gender=='female'),
           ericadams = as.numeric(mayor=='Eric Adams'),
           homeowner_num = as.numeric(homeowner=='Homeowner'),
           married = as.numeric(marital_status=='Married'),
           children_home_num = as.numeric(children_home=='Yes'),
           college = as.numeric(education %in% c('grad_2yr','grad_4yr', 'postgrad')),
           survey_meta = as.numeric(survey=='meta'),
           survey_email = as.numeric(survey=='email'))

## make summary statistics of the sample

Mean=function(x){mean(x,na.rm=T)}
SD=function(x){sd(x,na.rm=T)}

resp_stat_path = here("paper/tables/survey-summary-full-sample-nycc-final.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(resp_stat_path)|| REGENERATE) {
    datasummary(data = surv,
                formula = dem + rep + ericadams + age + female + white +
                    income_num  + college + housing_tenure + homeowner_num +
                    married + children_home_num ~
                    survey * (Mean + SD) + Mean + SD,
                output='latex') %>%
        write_file(file=resp_stat_path)
}

# Make summary statistics of the neighborhoods
# reformat neighborhood dataframe

d = nbhds_resps %>%
    group_by(id) %>%
    dplyr::summarize(democrats = sum(democrats, na.rm =T),
              registrants = sum(registrants, na.rm = T),
              republicans = sum(republicans, na.rm = T),
              pop = sum(pop,na.rm=T),
              pop_white = sum(pop_white,na.rm=T),
              pop_black = sum(pop_black,na.rm=T),
              area = sum(area,na.rm=T),
              survey = survey[1])%>%
    mutate(dem_p = democrats/registrants,
           rep_p = republicans/registrants,
           white_p = pop_white/pop,
           black_p = pop_black/pop,
           sq.miles = area / M_PER_MI^2)

nbhd_table_path = here("paper/tables/nbh-summary-full-nycc.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)|| REGENERATE) {
    datasummary(data = d,
                formula = pop + sq.miles + dem_p + rep_p + white_p + black_p ~
                    survey*(Mean + Median + Min + Max) +
                    Mean + Median + Min + Max,
                output = 'latex') %>%
        write_file(file=nbhd_table_path)
}



# Plots of summary statistics ----
PAL_SURV = c(Meta = "#465177", Email ="#E4C22B", Pooled = "#965127")
d_pooled = d %>%
    select(survey, pop, sq.miles, dem_p, rep_p, white_p, black_p) %>%
    mutate(survey='Pooled') %>%
    pivot_longer(cols = c('pop', 'sq.miles', 'dem_p', 'rep_p', 'white_p', 'black_p'))

d_plot = d %>%
    select(survey, pop, sq.miles, dem_p, rep_p, white_p, black_p) %>%
    pivot_longer(cols = c('pop', 'sq.miles', 'dem_p', 'rep_p', 'white_p', 'black_p')) %>%
    bind_rows(d_pooled) %>%
    mutate(survey = Hmisc::capitalize(survey),
           name = recode(name, `pop`='Population', `sq.miles`='Area (Square miles)',
                         `dem_p`='Prop. Dem.', `rep_p`='Prop. Rep.',
                         `white_p`='Prop. White', `black_p`='Prop. Black')) %>%
    mutate(
           name = factor(name, levels = c('Population', 'Area (Square miles)',
                                          'Prop. Dem.', 'Prop. Rep.',
                                          'Prop. White', 'Prop. Black')))

make_sum_plot = function(var, xlab="Survey", ...) {
    p = d_plot %>%
        filter(name == var) %>%
        ggplot(aes(x=survey, y=value, fill=survey)) +
        geom_boxplot(outlier.size=0.8, ...) +
        scale_fill_manual(values=PAL_SURV, guide="none") +
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
if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_full_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, p, width=6.5, height=4.5) %>%
        suppressWarnings()
}





###### Make comparisons across respondent party and race

d_race = nbhds_resps %>%
    mutate(white = ifelse(race=='white','white', 'non-white'))%>%
    group_by(id, white) %>%
    dplyr::summarize(democrats = sum(democrats, na.rm =T),
              registrants = sum(registrants, na.rm = T),
              republicans = sum(republicans, na.rm = T),
              pop = sum(pop,na.rm=T),
              pop_white = sum(pop_white,na.rm=T),
              pop_black = sum(pop_black,na.rm=T),
              area = sum(area,na.rm=T),
              survey = survey[1])%>%
    mutate(dem_p = democrats/registrants,
           rep_p = republicans/registrants,
           white_p = pop_white/pop,
           black_p = pop_black/pop,
           sq.miles = area / M_PER_MI^2)

nbhd_table_path = here("paper/tables/nbh-summary-race-full-nycc.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)) {
    datasummary(data = d_race,
                formula =  white_p +black_p  ~
                    survey*(white*(Mean )) +
                    white*(Mean),
                output = 'latex') %>%
        write_file(file=nbhd_table_path)
}


d_pooled = d_race %>%
    select(survey, white, white_p, black_p) %>%
    mutate(survey='Pooled') %>%
    pivot_longer(cols = c( 'white_p', 'black_p'))

d_plot = d_race %>%
    select(survey,  white, white_p, black_p) %>%
    pivot_longer(cols = c( 'white_p', 'black_p')) %>%
    bind_rows(d_pooled) %>%
    mutate(survey = Hmisc::capitalize(survey),
           name = recode(name,
                         `white_p`='Prop. White', `black_p`='Prop. Black')) %>%
    mutate(
           name = factor(name, levels = c(
               'Prop. White', 'Prop. Black')),
           white = capitalize(white))


make_sum_plot_by_race = function(var, xlab="Survey", ...) {
    p = d_plot %>%
        filter(name == var & !is.na(white)) %>%
        ggplot(aes(x=survey, y=value, fill=white)) +
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

if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_by_race_full_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, p, width=7, height=3.5) %>%
        suppressWarnings()
}



d_party = nbhds_resps %>%
    mutate(party3 = case_when(grepl('dem_',party_combined) ~'Democrat',
                              grepl('rep_',party_combined)~'Republican',
                              T ~ 'Independent')) %>%
    group_by(id, party3) %>%
    dplyr::summarize(democrats = sum(democrats, na.rm =T),
              registrants = sum(registrants, na.rm = T),
              republicans = sum(republicans, na.rm = T),
              pop = sum(pop,na.rm=T),
              pop_white = sum(pop_white,na.rm=T),
              pop_black = sum(pop_black,na.rm=T),
              area = sum(area,na.rm=T),
              survey = survey[1])%>%
    mutate(dem_p = democrats/registrants,
           rep_p = republicans/registrants,
           white_p = pop_white/pop,
           black_p = pop_black/pop,
           sq.miles = area / M_PER_MI^2)

nbhd_table_path = here("paper/tables/nbh-summary-party-party-nycc.tex")
# manual editing required for paper to tidy up labels
if (!file.exists(nbhd_table_path)) {
    datasummary(data = d_party,
                formula =  dem_p+ rep_p  ~
                    survey*(party3*(Mean )) +
                    party3*(Mean),
                output = 'latex') %>%
        write_file(file=nbhd_table_path)
}


d_pooled = d_party %>%
    select(survey, party=party3, dem_p, rep_p) %>%
    mutate(survey='Pooled') %>%
    pivot_longer(cols = c( 'dem_p', 'rep_p'))

d_plot = d_party %>%
    select(survey,  party=party3, dem_p, rep_p) %>%
    pivot_longer(cols = c( 'dem_p', 'rep_p')) %>%
    bind_rows(d_pooled) %>%
    mutate(survey = Hmisc::capitalize(survey),
           name = recode(name,
                         `dem_p`='Prop. Democrat', `rep_p`='Prop. Republican')) %>%
    mutate(
           name = factor(name, levels = c(
               'Prop. Democrat', 'Prop. Republican')))


make_sum_plot_by_party = function(var, xlab="Survey", ...) {
    p = d_plot %>%
        filter(name == var & !is.na(party)) %>%
        ggplot(aes(x=survey, y=value, fill=party)) +
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

if (!file.exists(figpath <- here("paper/figures/nbhd_stats_sum_by_party_full_nycc.pdf")) || REGENERATE) {
    ggsave(figpath, p, width=7, height=4) %>%
        suppressWarnings()
}

