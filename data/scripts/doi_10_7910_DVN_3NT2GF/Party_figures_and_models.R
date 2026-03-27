######Party-level and family analysis#####

#Drop countries outside of sample for now#

party_level <- party_level %>%
  filter(countryname %in% sample) 

#Add first year gov participation#

party_level <- left_join(party_level, parl, by = 'party')%>%
  mutate(first_gov = as.numeric(first_gov),
         mainstream_gov = ifelse(is.na(first_gov), 'Challenger',
                             ifelse(year > first_gov, 'Mainstream',
                                    ifelse(year <= first_gov,  'Challenger', NA))))

#####Summary Plots and values#####

panel_a <- ggplot(party_level, aes(x = sal_dif)) +
  geom_histogram(fill = 'black', color = 'grey') +
  theme_bw()+
    xlim(-1, 0.25) +
  labs(x = '',
       y = '',
       title = "A. Party-Election")

bot <- paste("Issue Salience Divergence")
left <- paste("Count")

fig2 <- grid.arrange(panel_a, panel_b,
                     nrow =1,
                     left = "Count",
                     bottom = "Issue Salience Divergence")

#ggsave("Figures/party_country_histogram.png",plot = fig2, dpi = 700, width = 11, height = 8.5, unit = 'in')

summary(party_level$sal_dif)

tapply(party_level$sal_dif, party_level$countryname, summary, na.rm =T)

tapply(party_level$sal_dif, party_level$family, summary, na.rm =T)

#By Family

family_level <- party_level %>%
  group_by(family, year) %>%
  dplyr::summarize(diff = mean(sal_dif),
                   diff_w = weighted.mean(sal_dif, pervote)) %>%
  ungroup() %>%
  group_by(family)%>%
  mutate(med = median(diff, na.rm = T)) %>%
  filter(!is.na(family))

#Breaking Up by Mainstream and challenger party#
party_level$Mainstream <- ifelse(party_level$family %in% c('Christian Dem.', 'Conservative', 'Liberal', 'Social Dem.'), 
                                 "Mainstream",
                                 "Challenger")

mainstream_level <- party_level %>%
  dplyr::group_by(mainstream_gov, year) %>%
  dplyr::summarize(diff = mean(sal_dif, na.rm = T))

family_scatter <- ggplot(family_level, aes(x = year, y = diff)) +
  geom_point() +
  geom_smooth(se =F, color = 'black') +
  facet_wrap(~ family) +
  theme_bw() +
  labs(x = 'Year',
       y = 'Issue Salience Divergence') +
  ylim(-1,.1)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(text=element_text(size=12,  family="Times New Roman"))

#ggsave("Figures/family_scatter.png", plot = family_scatter, width = 11, height = 8.5, dpi = 700, units = "in")

party_level <- party_level %>%
  group_by(family) %>%
  mutate(med = median(sal_dif, na.rm = T)) %>%
  ungroup() %>%
  group_by(countryname) %>%
  mutate(med_country = median(sal_dif, na.rm = T))

party_family_box <- ggplot(party_level[!is.na(party_level$family),], aes(x = reorder(family, med), y = sal_dif)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Party Family", 
       y = "Issue Salience Divergence")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#ggsave("Figures/party_family_box.png", plot = party_family_box, width = 11, height = 8.5, dpi = 700, units = "in")

party_country_box <- ggplot(party_level, aes(x = reorder(countryname, med_country), y = sal_dif)) +
  geom_boxplot() +
  theme_bw() +
  labs(x = "Country", 
       y = "Issue Salience Divergence")+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  theme(text=element_text(size=12,  family="Times New Roman"))

#ggsave("Figures/party_country_box.png", plot = party_country_box, width = 11, height = 8.5, dpi = 700, units = "in")

mainstream_scatter <- ggplot(mainstream_level, aes(x = year, y = diff, color = mainstream_gov)) +
  geom_point() +
  geom_smooth(se =F)  +
  theme_bw() +
  labs(x = 'Year',
       y = 'Issue Salience Divergence') +
  ylim(-1,.1) +
  scale_color_grey() +
  labs(color = "Party Status")

#ggsave("Figures/mainstream_gov_scatter.png", plot = mainstream_scatter, width = 11, height = 8.5, dpi = 700, units = "in")


table(party_level$Mainstream, party_level$mainstream_gov)

dplyr::group_by(party_level, Mainstream, mainstream_gov) %>% dplyr::summarize(dif = mean(sal_dif, na.rm  = T))


#ggsave("Figures/mainstream_scatter.png", plot = mainstream_scatter, width = 11, height = 8.5, dpi = 700, units = "in")


t_test <- mainstream_level %>%
  pivot_wider(names_from = mainstream_gov, values_from = diff)

t.test(t_test$Challenger, t_test$Mainstream)

(-.4467966 - - .5520621)/sd(party_level$sal_dif, na.rm = T) #44% of a standard deviation


#East-West Comparison#

reg_comp <- election %>%
  filter(year >= 1990) 

set.seed(85241)

region_box <- ggplot(reg_comp, aes(x = region, y = diff)) + 
  geom_boxplot() +
  geom_jitter() +
  theme_linedraw() +
  theme(panel.grid.major.x = element_blank()) +
  labs(x = "",
       y = "Issue Salience Divergence",
       title = "Elections after 1990")


#ggsave("Figures/region_box.png", plot = region_box, width = 11, height = 8.5, dpi = 700, units = "in")


##Initial models##

#Adjust variables#

party_level <- party_level %>%
  group_by(party) %>%
  arrange(party, edate) %>%
  mutate(sal_dif_lag = dplyr::lag(sal_dif),
         sal_dif_fd = sal_dif - sal_dif_lag,
         sal_dif_abs = abs(sal_dif_fd),
         pervote_lag = dplyr::lag(pervote),
         pervote_fd = pervote - pervote_lag,
         pervote_abs = abs(pervote_fd),
         first = min(year),
         age = year - first,
         age2 = age * age,
         age3 = age2 * age)

#merge in election-level factors

party_level <- left_join(party_level, election, by = c('countryname', 'edate')) %>%
  mutate(year = year.x - 1945)

party_level$year_10 <- party_level$year/10
party_level$age_10 <- party_level$age.x/10
party_level$age2_10 <- party_level$age2.x/10
party_level$age3_10 <- party_level$age3/10

#Multilevel Models#

m0 <-lmer(scale(sal_dif) ~ 
            (1 | party) + (1 | countryname), data = party_level)

resids <- as.data.frame(residuals(m0))
resids$squared <- resids$`residuals(m0)`^2
mean(resids$squared)

m1 <- lmer(scale(sal_dif) ~ year_10 +
            age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             Mainstream +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level)

summary(m1)  
icc(m1)
resids <- as.data.frame(residuals(m1))
resids$squared <- resids$`residuals(m1)`^2
mean(resids$squared)

m1_alt <- lmer(scale(sal_dif) ~ year_10 +
             age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             mainstream_gov +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level)

summary(m1_alt)  
icc(m1_alt)

m2 <- lmer(scale(sal_dif) ~ year_10 +
             age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             Mainstream +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level[party_level$region == 'East',])

summary(m2) 
icc(m2)

resids <- as.data.frame(residuals(m2))
resids$squared <- resids$`residuals(m2)`^2
mean(resids$squared)

m2_alt <- lmer(scale(sal_dif) ~ year_10 +
             age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             mainstream_gov +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level[party_level$region == 'East',])

summary(m2_alt) 
icc(m2_alt)

m3 <- lmer(scale(sal_dif) ~ year_10 +
             age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             Mainstream +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level[party_level$region == 'West',])

summary(m3) 
icc(m3)

resids <- as.data.frame(residuals(m3))
resids$squared <- resids$`residuals(m3)`^2
mean(resids$squared)

m3_alt <- lmer(scale(sal_dif) ~ year_10 +
             age_10 + 
             age2_10 +
             age3_10 +  
             scale(pervote_lag) + 
             scale(pervote_fd) +
             mainstream_gov +
             scale(enep) + 
             scale(econ_pol) +
             scale(nonecon_pol) +
             exec +
             type +
             (1 | party) + (1 | countryname), data = party_level[party_level$region == 'West',])

summary(m3_alt) 
icc(m3_alt)

###Check Robustness with Lagged dv###

m1_lag <- lmer(scale(sal_dif) ~ sal_dif_lag + year_10 +
                 age_10 + 
                 age2_10 +
                 age3_10 +  
                 scale(pervote_lag) + 
                 scale(pervote_fd) +
                Mainstream +
                 scale(enep) + 
                 scale(econ_pol) +
                 scale(nonecon_pol) +
                 exec +
                 type +
                 (1 | party) + (1 | countryname), data = party_level)

summary(m1_lag)  


m2_lag <- lmer(scale(sal_dif) ~ sal_dif_lag + year_10 +
                 age_10 + 
                 age2_10 +
                 age3_10 +  
                 scale(pervote_lag) + 
                 scale(pervote_fd) +
                 Mainstream+
                 scale(enep) + 
                 scale(econ_pol) +
                 scale(nonecon_pol) +
                 exec +
                type +
                 (1 | party) + (1 | countryname), data = party_level[party_level$region == 'East',])

summary(m2_lag) 



m3_lag <- lmer(scale(sal_dif) ~ sal_dif_lag + year_10 +
                 age_10 + 
                 age2_10 +
                 age3_10 +  
                 scale(pervote_lag) + 
                 scale(pervote_fd) +
                Mainstream +
                 scale(enep) + 
                 scale(econ_pol) +
                 scale(nonecon_pol) +
                 exec +
                 type +
                 (1 | party) + (1 | countryname), data = party_level[party_level$region == 'West',])

summary(m3_lag) 


####Make Tables####

party_level <- party_level %>%
  group_by(party) %>%
  arrange(party, edate) %>%
  mutate(sal_dif_scale = scale(sal_dif),
         sal_dif_scale_lag = dplyr::lag(sal_dif_scale))

texreg(list(m1, m3, m2),
       custom.model.names = c('Pooled', 'West', 'East'),
       custom.coef.names = c('Intercept', 'Years since 1945', 'Party Age', 'Party Age Squared',
                             'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                             'Mainstream Party', 'ENEP ', 'Polarization on Economic Issues',
                             'Polarization on Non-Economic Issues', 'Presidential System',
                             'Semi-Presidential System', 'Mixed Electoral System', 
                             'Proportional Electoral System'),
       caption = "Multilevel Linear Models Predicting Issue Salience Divergence (Party-Election Level)",
       stars = c(0.01, 0.05, 0.1))
screenreg(list(m1, m3, m2),
       custom.model.names = c('Pooled', 'West', 'East'),
       custom.coef.names = c('Intercept', 'Years since 1945', 'Party Age', 'Party Age Squared',
                             'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                             'Mainstream Party', 'ENEP ', 'Polarization on Economic Issues',
                             'Polarization on Non-Economic Issues', 'Presidential System',
                             'Semi-Presidential System', 'Mixed Electoral System', 
                             'Proportional Electoral System'),
       label = "Multilevel Linear Models Predicting Issue Salience Divergence (Party-Election Level)")


texreg(list(m1_lag, m3_lag, m2_lag),
       custom.model.names = c('Pooled', 'West', 'East'),
       custom.coef.names = c('Intercept', 'Lagged ISD', 'Years since 1945', 'Party Age', 'Party Age Squared',
                             'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                             'Mainstream Party (Gov. Particpation)', 'ENEP ', 'Polarization on Economic Issues',
                             'Polarization on Non-Economic Issues', 'Presidential System',
                             'Semi-Presidential System', 'Mixed Electoral System', 
                             'Proportional Electoral System'),
       caption = "Multilevel Linear Models Predicting Issue Salience Divergence (Party-Election Level)",
       stars = c(0.01, 0.05, 0.1))

texreg(list(m1_alt, m3_alt, m2_alt),
       custom.model.names = c('Pooled', 'West', 'East'),
       custom.coef.names = c('Intercept', 'Years since 1945', 'Party Age', 'Party Age Squared',
                             'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                             'Mainstream Party (Gov. Particpation)', 'ENEP ', 'Polarization on Economic Issues',
                             'Polarization on Non-Economic Issues', 'Presidential System',
                             'Semi-Presidential System', 'Mixed Electoral System', 
                             'Proportional Electoral System'),
       caption = "Multilevel Linear Models Predicting Issue Salience Divergence (Party-Election Level)",
       stars = c(0.01, 0.05, 0.1))

screenreg(list(m1_alt, m3_alt, m2_alt),
          custom.model.names = c('Pooled', 'West', 'East'),
          custom.coef.names = c('Intercept', 'Years since 1945', 'Party Age', 'Party Age Squared',
                                'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                                'Mainstream Party (Gov. Participation)', 'ENEP ', 'Polarization on Economic Issues',
                                'Polarization on Non-Economic Issues', 'Presidential System',
                                'Semi-Presidential System', 'Mixed Electoral System', 
                                'Proportional Electoral System'),
          label = "Multilevel Linear Models Predicting Issue Salience Divergence (Party-Election Level)")


#####Party-level dataset for saving and using in stata####

party_level<- party_level %>%
  mutate(period = case_when(
    year.x >= 1945 & year.x <= 1972 ~ "Golden Age",
    year.x >= 1973 & year.x <= 1979 ~ "Oil Shock",
    year.x >= 1980 & year.x <= 1992 ~ "Post-Oil Shock",
    year.x >= 1993 & year.x <= 2007 ~ "Transition to Knowledge Economy",
    year.x >= 2008 & year.x <= 2012 ~ "Financial Crisis",
    year.x >= 2013 ~ "Post-Crisis Period"
)) %>%
  select(party, year_10, sal_dif, Mainstream, mainstream_gov, sal_dif_lag, 
         pervote_lag, econ_pol, nonecon_pol, enep, exec, type, gal_dis, age.x, age2.x, age3, region, period) %>%
  rename(age = age.x,
         age2 = age2.x) %>%
  group_by(party) %>%
  mutate(time = row_number())

library(haven)
#write_dta(party_level, "Data/party_level.dta")
