#####Merging#####


#Drop cases outside of Europe

election <- country_main[!is.na(country_main$countryname),]


panel_b <- ggplot(election, aes(x = diff)) + 
  geom_histogram(fill = 'black', color = 'grey') +
  theme_bw() +
  xlim(-1,0.25) +
  labs(x = "",
       y = "",
       title = "B. Country-Election")

#By country#

country_scatter <- ggplot(election, aes(x = year, y = diff)) +
  geom_point() +
  geom_smooth(se = F, color = 'black') +
  theme_bw() +
  labs(x = 'Election Date', y = 'Issue Salience Divegence') +
  facet_wrap(~countryname) +
  ylim(-1,0) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#ggsave("Figures/country_scatter.png", plot = country_scatter, dpi = 700, width = 11, height = 8.5, units = "in")

election <- election %>%
  group_by(countryname) %>%
  mutate(med = median(diff, na.rm =T)) %>%
  ungroup()


country_election_box <- ggplot(election, aes(x = reorder(countryname, med), y = diff)) + 
  geom_boxplot() +
  labs(x = "Country", 
       y = "Issue Salience Divergence") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, vjust =1, hjust=1))
#ggsave("Figures/country_election_box.png", plot = country_election_box, dpi = 700, width = 11, height = 8.5, units = "in")

election <- election %>%
  mutate(decade = case_when(
    year >= 1944 & year <= 1949 ~ "40s",
    year >= 1950 & year <= 1959 ~ "50s",
    year >= 1960 & year <= 1969 ~ "60s",
    year >= 1970 & year <= 1979 ~ "70s",
    year >= 1980 & year <= 1989 ~ "80s",
    year >= 1990 & year <= 1999 ~ "90s",
    year >= 2000 & year <= 2009 ~ "00s",
    year >= 2010 ~ "10s"
  ))

election$decade <- factor(election$decade, levels = c("40s", 
                                                      "50s",
                                                      "60s",
                                                      "70s",
                                                      "80s",
                                                      "90s",
                                                      "00s",
                                                      "10s"))

decade_election_box <- ggplot(election, aes(x = decade, y = diff)) + 
  geom_boxplot() + 
  labs(x = 'Decade',
       y = 'Issue Salience Divergence') +
  theme_bw()

#ggsave("Figures/decade_election_box.png", plot = decade_election_box, dpi = 700, width = 11, height = 8.5, units = "in")

#Load continuous disproportionality measure#

Data <- repmis::source_data(url = "http://bit.ly/Ss6zDO") %>%
  filter(country %in% sample) %>%
  rename(countryname = country,
         gal_dis = disproportionality) %>%
  mutate(gal_dis_scale = scale(gal_dis)) 


#Start with election/country level factors

election <- dplyr::left_join(country_main, des, by = c('countryname', 'year', 'month'))


election <- dplyr::left_join(election, country_pol, by = c('countryname', 'edate')) %>%
  filter(!is.na(country))

#lag values within group and calculate difference#

election <- election %>%
  dplyr::arrange(countryname, year, month) %>%
  group_by(countryname) %>%
  mutate(diff_lag = dplyr::lag(diff, n=1, default =NA),
         diff_delta = diff - diff_lag,
         diff_delta_abs = abs(diff-diff_lag),
         first_elec = min(year, na.rm = T),
         age = year - first_elec,
         age2 =age * age)




#####country level differences#####

country <- election %>%
  dplyr::group_by(country) %>%
  dplyr::summarize(diff_mean = mean(diff, na.rm = T),
            diff_sd = sd(diff, na.rm =T),
            econ_pol = mean(econ_pol, na.rm =T),
            nonecon_pol = mean(nonecon_pol, na.rm =T),
            enep = mean(enep, na.rm =T),
            m = mean(tier1_avemag, na.rm =T))


#####models#####

#####Using PLM package#####

#add numericly ascending values as time indicator#

election <- election %>%
  group_by(country)%>%
  arrange(country, edate)%>%
  mutate(orders = row_number())

election$diff_scale <- scale(election$diff)
election$age_10 <- election$age/10
election$enep_scale <- scale(election$enep)
election$econ_pol_scale <- scale(election$econ_pol)
election$nonecon_pol_scale <- scale(election$nonecon_pol)
election$year_10 <- election$year/10

election <- election %>%
  arrange(countryname, orders) %>%
  group_by(country) %>%
  mutate(diff_lag = dplyr::lag(diff_scale))

#Make pdata.frame

electionp <- pdata.frame(election, index = c('countryname', 'orders'))



p1 <- plm(diff_scale ~  age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale +
            year_10 +
            region +
            exec +
            type,
          data = electionp,
          model = 'pooling')
mean(p1$residuals^2)

p2 <- plm(diff_scale ~ 
            age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale ,
          data = electionp,
          model = 'within')
mean(p2$residuals^2)

p3 <- plm(diff_scale ~ age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale ,
          data = electionp,
          model = 'fd')
mean(p3$residuals^2)


stargazer(p1, p2, p3,
          title = 'OLS Models Explaining Variation in Issue Salience Divergence (Country-Election Level)',
          column.labels = c('Pooling','Fixed Effect', 'First-Difference'),
          dep.var.labels = 'Issue Salience Divergence',
          covariate.labels = c('Age of Democracy', 'ENEP',
                               'Polarization on Economic Issues', 'Polarization on Non-Economic Issues', 
                               'Election Year', 'Western Europe', 'Presidential System',
                               'Semi-Presidential System', 'Mixed Electoral System', 'Proportioinal Electoral System'),
          no.space =T,
          notes.align = 'l')

###Alternative Measure of disproportionality###

election_alt <- inner_join(election, Data, by = c('countryname', 'year'))

#Drop years created due to duplication (Data lacks month indicators)

d1 <- which(election_alt$countryname == "Denmark" & election_alt$year == 1953 & election_alt$month == 4 & election_alt$gal_dis == 2.61)
d2 <- which(election_alt$countryname == "Denmark" & election_alt$year == 1953 & election_alt$month == 9 & election_alt$gal_dis == 0.81)
d3 <- which(election_alt$countryname == "Greece" & election_alt$year == 1989 & election_alt$month == 6 & election_alt$gal_dis == 3.94)
d4 <- which(election_alt$countryname == "Greece" & election_alt$year == 1989 & election_alt$month == 11 & election_alt$gal_dis == 4.37)
d5 <- which(election_alt$countryname == "Ireland" & election_alt$year == 1982 & election_alt$month == 02 & election_alt$gal_dis == 2.74)
d6 <- which(election_alt$countryname == "Ireland" & election_alt$year == 1982 & election_alt$month == 11 & election_alt$gal_dis == 1.69)



election_alt <- election_alt[-c(d1,d2,d3,d4, d5, d6),]
#remake pdata frame

electionp <- pdata.frame(election_alt, index = c('countryname', 'orders'))

p1_alt <- plm(diff_scale ~ age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale +
            year_10 +
            region +
            exec +
            gal_dis_scale,
          data = electionp,
          model = 'pooling')

p2_alt <- plm(diff_scale ~ age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale+
              gal_dis_scale ,
          data = electionp,
          model = 'within')

p3_alt <- plm(diff_scale ~ age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale+
              gal_dis_scale ,
          data = electionp,
          model = 'fd')

####Lagged dependent variable####

p1_lag <- plm(diff_scale ~ diff_lag +
                age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale +
            year_10 +
            region +
            exec +
              gal_dis_scale,
          data = electionp,
          model = 'pooling')


p2_lag <- plm(diff_scale ~ diff_lag +
            age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale + gal_dis_scale ,
          data = electionp,
          model = 'within')


p3_lag <- plm(diff_scale ~ diff_lag +
                age_10 +
            enep_scale + 
            econ_pol_scale +
            nonecon_pol_scale +  gal_dis_scale ,
          data = electionp,
          model = 'fd')






stargazer(p1_alt, p2_alt, p3_alt,
          title = 'OLS Models Explaining Variation in Issue Salience Divergence (Country-Election Level) Including Alternative Measures of Disproportionality',
          column.labels = c('Pooling','Fixed Effect', 'First-Difference'),
          dep.var.labels = 'Issue Salience Divergence',
          covariate.labels = c('Age of Democracy', 'ENEP',
                               'Polarization on Economic Issues', 'Polarization on Non-Economic Issues', 
                               'Election Year', 'Western Europe', 'Presidential System',
                               'Semi-Presidential System', 'Disproportionality (Gallagher)'),
          no.space =T,
          notes.align = 'l')

stargazer(p1_lag, p2_lag, p3_lag,
          title = 'OLS Models Explaining Variation in Issue Salience Divergence (Country-Election Level) Including A Lagged Dependent Variable',
          column.labels = c('Pooling','Fixed Effect', 'First-Difference'),
          dep.var.labels = 'Issue Salience Divergence',
          covariate.labels = c('Lagged ISD', 'Age of Democracy', 'ENEP',
                               'Polarization on Economic Issues', 'Polarization on Non-Economic Issues', 
                               'Election Year', 'Western Europe', 'Presidential System',
                               'Semi-Presidential System', 'Disproportionality (Gallagher)'),
          no.space =T,
          notes.align = 'l')


#####country-level dataset for saving and using in stata####

election<- election %>%
  mutate(period = case_when(
    year >= 1945 & year <= 1972 ~ "Golden Age",
    year >= 1973 & year <= 1979 ~ "Oil Shock",
    year >= 1980 & year <= 1992 ~ "Post-Oil Shock",
    year >= 1993 & year <= 2007 ~ "Transition to Knowledge Economy",
    year >= 2008 & year <= 2012 ~ "Financial Crisis",
    year >= 2013 ~ "Post-Crisis Period"
  ))

library(foreign)
#write.dta(election, "Data/election.dta")
