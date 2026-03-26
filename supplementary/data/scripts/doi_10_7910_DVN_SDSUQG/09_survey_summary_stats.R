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


## Get counts of usable neighborhood respondents in each treatment group
model_d %>%
    group_by(id,  group)%>%
    summarize(n=n())%>%
    group_by( group)%>%
    summarize(n())

## Get counts of usable neighborhood respondents in each city group
model_d %>%
    group_by(id,  city)%>%
    summarize(n=n())%>%
    group_by( city)%>%
    summarize(N=n(),
              )



# Make table A1 #########


x = resp_d %>%
    mutate(housing_tenure = ifelse(housing_tenure > 1900, 2021-housing_tenure,
                                   ifelse(housing_tenure >90, NA,housing_tenure)))%>%
    filter(id%in%model_d$id)%>%
    group_by(city)%>%
    summarize(N=n(),
              Democrat_mean=mean(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),
              Democrat_sd=sd(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),

                Repubican_mean = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),
              Repubican_sd = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),

                VoteBiden_mean = mean(president=='biden',na.rm=T),
              VoteBiden_sd = sd(president=='biden',na.rm=T),

              Age_mean = mean(age,na.rm=T),
              Age_sd = sd(age,na.rm=T),

              Female_mean = mean(gender=='female',na.rm=T),
              Female_sd = sd(gender=='female',na.rm=T),

              Income_mean = mean(dplyr::recode(income,
                                              `$120,000  or More`= 160 ,
                                              `$100,000 -119,999`= 110,
                                              `$90,000 – 99,999`= 95,
                                              `$80,000 – 89,999`= 85,
                                              `$70,000 – 79,999`= 75,
                                              `$60,000 – 69,999` = 65,
                                              `$50,000 – 59,999`= 55,
                                              `$40,000 – 49,999`= 45,
                                              `$30,000 – 39,999`= 35,
                                              `Less than $30,000`= 25), na.rm=T),
              Income_sd = sd(dplyr::recode(income,
                                              `$120,000  or More`= 160 ,
                                              `$100,000 -119,999`= 110,
                                              `$90,000 – 99,999`= 95,
                                              `$80,000 – 89,999`= 85,
                                              `$70,000 – 79,999`= 75,
                                              `$60,000 – 69,999` = 65,
                                              `$50,000 – 59,999`= 55,
                                              `$40,000 – 49,999`= 45,
                                              `$30,000 – 39,999`= 35,
                                              `Less than $30,000`= 25), na.rm=T),

                Education_mean = mean(education %in% c('grad_2yr','grad_4yr','postgrad')),
              Education_sd = sd(education %in% c('grad_2yr','grad_4yr','postgrad')),

              YearsResidence_mean = mean(housing_tenure, na.rm=T),
              YearsResidence_sd = sd(housing_tenure, na.rm=T),

              Homeowner_mean = mean(homeowner=='Yes', na.rm=T),
              Homeowner_sd = sd(homeowner=='No', na.rm=T),

              Married_mean = mean(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),
              Married_sd = sd(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),

                ChildrenInHome_mean = mean(children_home=='Yes', na.rm=T),
              ChildrenInHome_sd = sd(children_home=='Yes', na.rm=T)


              )


x = x %>%
    pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
    filter(grepl('_mean', name))%>%
    select(city, name, mean=value)%>%
    mutate(name = str_replace(name,'_mean', ''))%>%
    left_join(x %>%
                  pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
                  filter(grepl('_sd', name))%>%
                  select(city, name, sd=value)%>%
                  mutate(name = str_replace(name,'_sd', '')),by=c('city','name'))%>%
    mutate(mean=round(mean,2),
           sd = round(sd, 2))


x = x %>%
    filter(city=='miami')%>%
    left_join(x %>%
                  filter(city=='new-york'),by='name')%>%
    left_join(x %>%
                  filter(city=='phoenix'),by='name')





pooled = resp_d %>%
    mutate(housing_tenure = ifelse(housing_tenure > 1900, 2021-housing_tenure,
                                   ifelse(housing_tenure >90, NA,housing_tenure)))%>%
    filter(id%in%model_d$id)%>%
    summarize(N=n(),
              Democrat_mean=mean(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),
              Democrat_sd=sd(party_combined %in% c('dem_strong','dem_lean'),na.rm=T),

              Repubican_mean = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),
              Repubican_sd = mean(party_combined %in%c('rep_strong','rep_lean'),na.rm=T),

              VoteBiden_mean = mean(president=='biden',na.rm=T),
              VoteBiden_sd = sd(president=='biden',na.rm=T),

              Age_mean = mean(age,na.rm=T),
              Age_sd = sd(age,na.rm=T),

              Female_mean = mean(gender=='female',na.rm=T),
              Female_sd = sd(gender=='female',na.rm=T),

              Income_mean = mean(dplyr::recode(income,
                                               `$120,000  or More`= 160 ,
                                               `$100,000 -119,999`= 110,
                                               `$90,000 – 99,999`= 95,
                                               `$80,000 – 89,999`= 85,
                                               `$70,000 – 79,999`= 75,
                                               `$60,000 – 69,999` = 65,
                                               `$50,000 – 59,999`= 55,
                                               `$40,000 – 49,999`= 45,
                                               `$30,000 – 39,999`= 35,
                                               `Less than $30,000`= 25), na.rm=T),
              Income_sd = sd(dplyr::recode(income,
                                           `$120,000  or More`= 160 ,
                                           `$100,000 -119,999`= 110,
                                           `$90,000 – 99,999`= 95,
                                           `$80,000 – 89,999`= 85,
                                           `$70,000 – 79,999`= 75,
                                           `$60,000 – 69,999` = 65,
                                           `$50,000 – 59,999`= 55,
                                           `$40,000 – 49,999`= 45,
                                           `$30,000 – 39,999`= 35,
                                           `Less than $30,000`= 25), na.rm=T),

              Education_mean = mean(education %in% c('grad_2yr','grad_4yr','postgrad')),
              Education_sd = sd(education %in% c('grad_2yr','grad_4yr','postgrad')),

              YearsResidence_mean = mean(housing_tenure, na.rm=T),
              YearsResidence_sd = sd(housing_tenure, na.rm=T),

              Homeowner_mean = mean(homeowner=='Yes', na.rm=T),
              Homeowner_sd = sd(homeowner=='No', na.rm=T),

              Married_mean = mean(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),
              Married_sd = sd(marital_status %in% c('Married', 'Domestic Partnership'), na.rm = T),

              ChildrenInHome_mean = mean(children_home=='Yes', na.rm=T),
              ChildrenInHome_sd = sd(children_home=='Yes', na.rm=T)


    )

pooled = pooled %>%
    mutate(city='pooled')%>%
    pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
    filter(grepl('_mean', name))%>%
    select(city, name, mean=value)%>%
    mutate(name = str_replace(name,'_mean', ''))%>%
    left_join(pooled %>%
                  mutate(city='pooled')%>%
                  pivot_longer(Democrat_mean:ChildrenInHome_sd)%>%
                  filter(grepl('_sd', name))%>%
                  select(city, name, sd=value)%>%
                  mutate(name = str_replace(name,'_sd', '')),by=c('city','name'))%>%
    mutate(mean=round(mean,2),
           sd = round(sd, 2))

x %>%
    left_join(pooled, by='name')%>%
    select(name,
           mean_miami=mean.x, sd_miami=sd.x,
           mean_nyc=mean.y, sd_nyc=sd.y,
           mean_phoenix=mean.x.x, sd_phoenix=sd.x.x,
           mean_pooled=mean.y.y, sd_pooled=sd.y.y
           )%>%
    xtable


# BALANCE PLOT #############
bal = resp_d %>%
    filter(id%in%model_d$id)%>%
    mutate(
        Democrat = as.numeric(party_combined %in% c('dem_strong','dem_lean'))%>%normalize(method='range',range=c(0,1)),
        Republican = as.numeric(party_combined %in% c('rep_strong','rep_lean'))%>%normalize(method='range',range=c(0,1)),
        `Vote Biden` = as.numeric(president=='biden')%>%normalize(method='range',range=c(0,1)),
        Age = age%>%normalize(method='range',range=c(0,1)),
        Female = as.numeric(gender=='female')%>%normalize(method='range',range=c(0,1)),
        Income = dplyr::recode(income,
                               `$120,000  or More`= 160 ,
                               `$100,000 -119,999`= 110,
                               `$90,000 – 99,999`= 95,
                               `$80,000 – 89,999`= 85,
                               `$70,000 – 79,999`= 75,
                               `$60,000 – 69,999` = 65,
                               `$50,000 – 59,999`= 55,
                               `$40,000 – 49,999`= 45,
                               `$30,000 – 39,999`= 35,
                               `Less than $30,000`= 25)%>%normalize(method='range',range=c(0,1)),
        College = as.numeric(education %in% c('grad_2yr','grad_4yr','postgrad'))%>%normalize(method='range',range=c(0,1)),
        Homeowner = as.numeric(homeowner=='Yes')%>%normalize(method='range',range=c(0,1)),
        Married = as.numeric(marital_status %in% c('Married', 'Domestic Partnership'))%>%normalize(method='range',range=c(0,1)),
        `Years Residence` = ifelse(housing_tenure > 1900, 2021-housing_tenure,
                                   ifelse(housing_tenure >90, NA,housing_tenure))%>%normalize(method='range',range=c(0,1)),

        ChildrenInHome = as.numeric(children_home=='Yes')%>%normalize(method='range',range=c(0,1)))%>%
    select(city,group,Democrat:`Years Residence`)%>%
    pivot_longer(Democrat:`Years Residence`)


bal %>%
    group_by(name,group)%>%
    summarize(mean = mean(value,na.rm=T),
              se = std.error(value,na.rm=T))%>%
    mutate(city='pooled')%>%
    bind_rows(bal %>%
                  group_by(name, city, group)%>%
                  summarize(mean = mean(value,na.rm=T),
                            se = std.error(value,na.rm=T))%>%
                  filter(city=='miami'))%>%
    bind_rows(bal %>%
                  group_by(name, city,group)%>%
                  summarize(mean = mean(value,na.rm=T),
                            se = std.error(value,na.rm=T))%>%
                  filter(city=='new-york'))%>%
    bind_rows(bal %>%
                  group_by(name, city,group)%>%
                  summarize(mean = mean(value,na.rm=T),
                            se = std.error(value,na.rm=T))%>%
                  filter(city=='phoenix'))%>%
    mutate(city=recode(city,`miami`='Miami', `new-york`='New York City', `phoenix`='Phoenix', `pooled`='Pooled'),
           Group=recode(group, `P`='Party', `PH`='Party Placebo', `R`='Race', `RH`='Race Placebo', `C`='Control'),
           )%>%
ggplot(aes(x = mean, y =  name, color = Group))+
    geom_point(position=position_dodge(0.7))+
    geom_errorbar(aes(xmin=mean-1.96*se,xmax=mean+1.96*se),width=0, position=position_dodge(0.7))+
    scale_color_wa_d("coast", which=c(5, 2, 1, 3, 4)) +
    theme_paper() +
    facet_grid(.~city)+
    xlab('Standardized Mean')+
    ylab('Variable')+
    theme(legend.position='bottom')

ggsave(filename='paper/figures/balance_plot.pdf', width=7, height=5)




# Secondary experimental outcomes ###########

resp_d$trust = rowMeans(resp_d[,c('trust_1','trust_2','trust_3')]/10, na.rm=T)

out = lapply(unique(resp_d$city), FUN = function(ct){
 p1 =   difference_in_means(trust ~ group,
                            condition1='PH', condition2 = 'P',
                            data = resp_d %>% filter(id %in% model_d$id & city == ct))
p2 =   difference_in_means(housing ~ group,
                           condition1='PH', condition2 = 'P',
                           data = resp_d %>% filter(id %in% model_d$id & city == ct))


r1 =   difference_in_means(trust ~ group,
                           condition1='RH', condition2 = 'R',
                           data = resp_d %>% filter(id %in% model_d$id & city == ct))
r2 =   difference_in_means(housing ~ group,
                           condition1='RH', condition2 = 'R',
                           data = resp_d %>% filter(id %in% model_d$id & city == ct))

l = list(p1,p2,r1,r2)
names(l)=c('Party - Trust', 'Party - Housing', 'Race - Trust', 'Race - Housing')
return(l)
}
)

p1 =   difference_in_means(trust ~ group,
                           condition1='PH', condition2 = 'P',
                           data = resp_d %>% filter(id %in% model_d$id ))
p2 =   difference_in_means(housing ~ group,
                           condition1='PH', condition2 = 'P',
                           data = resp_d %>% filter(id %in% model_d$id ))


r1 =   difference_in_means(trust ~ group,
                           condition1='RH', condition2 = 'R',
                           data = resp_d %>% filter(id %in% model_d$id ))
r2 =   difference_in_means(housing ~ group,
                           condition1='RH', condition2 = 'R',
                           data = resp_d %>% filter(id %in% model_d$id ))

l = list(p1,p2,r1,r2)
names(l)=c('Party - Trust', 'Party - Housing', 'Race - Trust', 'Race - Housing')

out[[4]] = l

names(out)=c(unique(resp_d$city), 'pooled')



g = tibble(
    City = c('pooled', 'pooled', 'pooled', 'pooled',
             'miami', 'miami', 'miami', 'miami',
             'new-york', 'new-york', 'new-york', 'new-york',
             'phoenix', 'phoenix', 'phoenix', 'phoenix'),
    Type = c('Race', 'Race', 'Party', 'Party',
             'Race', 'Race', 'Party', 'Party',
             'Race', 'Race', 'Party', 'Party',
             'Race', 'Race', 'Party', 'Party'),
    Outcome = c('Trust', 'Housing', 'Trust', 'Housing',
                'Trust', 'Housing', 'Trust', 'Housing',
                'Trust', 'Housing', 'Trust', 'Housing',
                'Trust', 'Housing', 'Trust', 'Housing'),
    Estimate = c(
        out$pooled$`Race - Trust`$coefficients,
        out$pooled$`Race - Housing`$coefficients,
        out$pooled$`Party - Trust`$coefficients,
        out$pooled$`Party - Housing`$coefficients,

        out$miami$`Race - Trust`$coefficients,
        out$miami$`Race - Housing`$coefficients,
        out$miami$`Party - Trust`$coefficients,
        out$miami$`Party - Housing`$coefficients,

        out$`new-york`$`Race - Trust`$coefficients,
        out$`new-york`$`Race - Housing`$coefficients,
        out$`new-york`$`Party - Trust`$coefficients,
        out$`new-york`$`Party - Housing`$coefficients,

        out$phoenix$`Race - Trust`$coefficients,
        out$phoenix$`Race - Housing`$coefficients,
        out$phoenix$`Party - Trust`$coefficients,
        out$phoenix$`Party - Housing`$coefficients

    ),
    CI.l = c(
        out$pooled$`Race - Trust`$conf.low,
        out$pooled$`Race - Housing`$conf.low,
        out$pooled$`Party - Trust`$conf.low,
        out$pooled$`Party - Housing`$conf.low,

        out$miami$`Race - Trust`$conf.low,
        out$miami$`Race - Housing`$conf.low,
        out$miami$`Party - Trust`$conf.low,
        out$miami$`Party - Housing`$conf.low,

        out$`new-york`$`Race - Trust`$conf.low,
        out$`new-york`$`Race - Housing`$conf.low,
        out$`new-york`$`Party - Trust`$conf.low,
        out$`new-york`$`Party - Housing`$conf.low,

        out$phoenix$`Race - Trust`$conf.low,
        out$phoenix$`Race - Housing`$conf.low,
        out$phoenix$`Party - Trust`$conf.low,
        out$phoenix$`Party - Housing`$conf.low
    ),
    CI.u = c(
        out$pooled$`Race - Trust`$conf.high,
        out$pooled$`Race - Housing`$conf.high,
        out$pooled$`Party - Trust`$conf.high,
        out$pooled$`Party - Housing`$conf.high,

        out$miami$`Race - Trust`$conf.high,
        out$miami$`Race - Housing`$conf.high,
        out$miami$`Party - Trust`$conf.high,
        out$miami$`Party - Housing`$conf.high,

        out$`new-york`$`Race - Trust`$conf.high,
        out$`new-york`$`Race - Housing`$conf.high,
        out$`new-york`$`Party - Trust`$conf.high,
        out$`new-york`$`Party - Housing`$conf.high,

        out$phoenix$`Race - Trust`$conf.high,
        out$phoenix$`Race - Housing`$conf.high,
        out$phoenix$`Party - Trust`$conf.high,
        out$phoenix$`Party - Housing`$conf.high
    )
) %>%
    ggplot(aes(x = Estimate, y =  Type, color = Type))+
    geom_point(size=2)+
    geom_errorbar(aes(xmin=CI.l,xmax=CI.u),width=0)+
    theme_paper()+
    facet_grid(City~Outcome)+
    xlab('Treatment Effect')+
    ylab('')+
    geom_vline(xintercept=0,linetype='dashed')+
    theme(legend.position='bottom')


ggsave(plot=g, filename='paper/figures/experiment_results_housing_trust.pdf', width=7, height=4.5)




# Map attrition ############

bal = resp_d %>%
    mutate(usable.neighborhood = as.numeric(id%in%model_d$id))%>%
    mutate(
        Democrat = as.numeric(party_combined %in% c('dem_strong','dem_lean')),
        Republican = as.numeric(party_combined %in% c('rep_strong','rep_lean')),
        `Vote Biden` = as.numeric(president=='biden'),
        Age = age,
        Female = as.numeric(gender=='female'),
        Income = dplyr::recode(income,
                               `$120,000  or More`= 160 ,
                               `$100,000 -119,999`= 110,
                               `$90,000 – 99,999`= 95,
                               `$80,000 – 89,999`= 85,
                               `$70,000 – 79,999`= 75,
                               `$60,000 – 69,999` = 65,
                               `$50,000 – 59,999`= 55,
                               `$40,000 – 49,999`= 45,
                               `$30,000 – 39,999`= 35,
                               `Less than $30,000`= 25),
        College = as.numeric(education %in% c('grad_2yr','grad_4yr','postgrad')),
        Homeowner = as.numeric(homeowner=='Yes'),
        Married = as.numeric(marital_status %in% c('Married', 'Domestic Partnership')),
        `Years Residence` = ifelse(housing_tenure > 1900, 2021-housing_tenure,
                                   ifelse(housing_tenure >90, NA,housing_tenure)),

        ChildrenInHome = as.numeric(children_home=='Yes'))%>%
    select(city,group,Democrat:`Years Residence`,usable.neighborhood)




m2 = lapply(unique(bal$city), FUN=function(ct){
    lm(usable.neighborhood ~ group + Age + College + Democrat + Female + Homeowner + Income + Married + Republican + `Vote Biden` + `Years Residence`, data = bal %>%
           filter(city==ct))
})

stargazer(m2[[1]],m2[[2]],m2[[3]],  type='text', out = "tables/map_attrition.tex")




