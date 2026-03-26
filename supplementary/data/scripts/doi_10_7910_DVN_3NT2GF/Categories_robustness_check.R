#####Robustness Check: % of Manifesto Covered#####

salience <- mp_maindataset() %>% #download the data
  filter(countryname %in% sample) %>%
  filter(!is.na(per401) & !is.na(pervote)) %>% #Drop parties with missing data (per401 is only NA when all others are also NA)
  group_by(party, edate) %>%
  mutate(rile = per104 + per201 + per203 + per305 +per401 + per402 + per407 + per414 + per505 + per601 + per603 + per605 + per606 +
           per103 + per105 + per106 + per107 + per403 + per404 + per406 + per412 + per413 + per504 + per506 + per701 + per202,
         vm = per403 + per404 +per412 + per413 + per401 + per402 + per504 + per505 + per701 + per702 + per409 + per414 + per416 + per410 +
           per607 + per705 + per608 + per501 + per411 + per105 + per106 + per104 + per604 + per603 + per202 + per605 + per108 + per110 + per407 + per406 + per107 + per109 + per602 + per601 + per301 + per302)
summary(salience$rile)
summary(salience$vm)

rile_plot <- ggplot(salience, aes(x = rile)) + 
  geom_histogram(color = 'grey', fill = 'black') + 
  theme_minimal() + 
  labs(x = "% of Manifesto",
       y = "Number of Parties",
       title = "Categories used in RILE Index")


vm_plot <- ggplot(salience, aes(x = vm)) + 
  geom_histogram(color = 'grey', fill = 'black') +
  theme_minimal() +
  labs(x = "% of Manifesto",
       y = "Number of Parties",
       title = "Categories used in Volkens and Merz (2018)")

perc_sal <- grid.arrange(rile_plot, vm_plot)

salience$sal_dif <- salience$vm - salience$rile

summary(salience$sal_dif)

ggplot(salience, aes (x = rile, y = vm)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, color = 'red') + 
  theme_minimal() +
  labs(x = "% Covered by RILE",
       y = "% Covered by Volkens and Merz (2018)")

ggplot(salience, aes(x = sal_dif)) + 
  geom_density(color = 'grey', fill = 'black') + 
  theme_minimal() +
  labs(x = "Volkens and Merz (2018) - RILE",
       y = "Density") +
  geom_vline(xintercept = 0, color = 'red')

cor.test(salience$vm, salience$rile) # r = 0.312***

salience$dif_pos <- ifelse(salience$sal_dif >= 0, 1, 0)

summary(salience$dif_pos) # Greater for 73.8 % of parties

ggplot(salience, aes(x = edate, y = vm)) + 
  geom_point() + 
  geom_smooth()

ggplot(salience, aes(x = edate, y = rile)) + 
  geom_point() + 
  geom_smooth()

salience$half <- ntile(salience$vm, 2)
salience$third <- ntile(salience$vm, 3)

salience_list <- salience %>%
  select(party, edate, half, third, vm) 



#####Rerun models excluding parties with poor fit to the categories#####

party_level <- left_join(party_level, salience_list, by = c("party", "edate"))

m1_vm_control <- lmer(scale(sal_dif) ~ year_10 +
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
               vm +
             (1 | party) + (1 | countryname), data = party_level)

texreg(list(m1_vm_control),
       custom.model.names = 'Controlling for % of Manifesto Included',
       custom.coef.names = c('Intercept', 'Years since 1945', 'Party Age', 'Party Age Squared',
                             'Party Age Cubed', 'Vote Share (t_1)', 'Change in Vote Share',
                             'Mainstream Party', 'ENEP ', 'Polarization on Economic Issues',
                             'Polarization on Non-Economic Issues', 'Presidential System',
                             'Semi-Presidential System', 'Mixed Electoral System', 
                             'Proportional Electoral System', 'Percent of Manifesto Included'),
       caption = "Multilevel Linear Model Predicting Issue Salience Divergence (Party-Election Level)",
       stars = c(0.01, 0.05, 0.1))

