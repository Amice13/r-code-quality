####Calculation Example####

mp_setapikey(key = "e0651f3ae260c42f92028272c768c705")

sample <- c( "United Kingdom")

#Make Data Set - simple sum of components#

party_level <- mp_maindataset() %>% #download the data
  filter(countryname %in% sample & edate == "1979-05-03") %>%
  dplyr::select(countryname, edate, party, partyname, parfam, pervote, per401, 
                per402, per403, per404, per412, per413, per505, per504, per702, 
                per701, per414, per409, per410, per416, per608, per607, per705,
                per411, per501, per104, per105, per106, per603, per604, per605, per202,
                per110, per108, per406, per407, per109, per107, per601, per602, per302,
                per301) %>% #keep only relevant variables from Volkens and Merz (2013)
  filter(!is.na(per401) & !is.na(pervote)) %>% #Drop parties with missing data (per401 is only NA when all others are also NA)
  group_by(party, edate) %>% #Group by party-election
  dplyr::mutate(role_of_the_state_salience = per401 + per402 + per403 + per404 + per412 + per413 ,
                welfare_state_salience = per505 + per504  ,
                union_salience = per702 + per701 ,
                finance_salience = per414 + per409,
                growth_salience = per410 + per416 ,
                multiculturalism_salience = per608 + per607 + per705 ,
                environment_salience = per411 + per501  ,
                peace_salience = per104 + per105 + per106  ,
                moral_salience = per603 + per604  ,
                civil_rights_salience = per605 + per202 ,
                eu_salience = per110 + per108,
                protectionism_salience = per407 + per406 ,
                internationalism_salience = per109 + per107  ,
                nationalism_salience = per601 + per602  ,
                centralization_salience = per302 + per301)   %>% #Create a variable for each salience
  arrange(countryname, party, edate) %>% #arrange the data set by country-pary-election
  separate(edate, into = c('year', 'month','day'), convert = TRUE, remove = F) %>% #adjust date variable
  mutate(family = case_when(
    parfam == 10 ~ 'Green',
    parfam == 20 ~ 'Left',
    parfam == 30 ~ 'Social Dem.',
    parfam == 40 ~ 'Liberal',
    parfam == 50 ~ 'Christian Dem.',
    parfam == 60 ~ 'Conservative',
    parfam == 70 ~ 'Nationalist',
    parfam == 80 ~ 'Agrarian',
    parfam == 90 ~ 'Ethnic',
    parfam == 95 ~ 'Special Issue'
  )) %>% #Add party family labels
  select(partyname, role_of_the_state_salience:centralization_salience) %>%
  ungroup()

#Pivot longer to have a column per party, each row an issue, and values as the percent of manifesto#

uk <- party_level %>%
  select(-party, -edate) %>%
  pivot_longer(cols = ends_with("salience"),
               names_to = "issue",
               values_to = "perc") %>%
  pivot_wider(names_from = partyname,
              values_from = perc)
uk$topic <- c("Role of the State", "Welfare State", "Unions", "Finance", "Growth", 
           "Multiculturalism", "Environment", "Peace", "Morality", "Civil Rights",
           "EU", "Protectionism", "Internationalism", "Nationalism", "Centralization")
#Make Plots#
library(ggrepel)

cor.test(uk$`Labour Party`, uk$`Conservative Party`) #0.514

labcon <- ggplot(uk, aes(x = `Labour Party`, y = `Conservative Party`, label = topic)) +
  theme(text=element_text(size=12,  family="Times New Roman"))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(family = "Times New Roman") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme_bw() +
  ggtitle("r = 0.514") +
  theme(text=element_text(size=12,  family="Times New Roman"))

cor.test(uk$`Liberal Party`, uk$`Conservative Party`)

libcon <- ggplot(uk, aes(x = `Liberal Party`, y = `Conservative Party`, label = topic)) + 
  theme(text=element_text(size=12,  family="Times New Roman"))+
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(family = "Times New Roman") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme_bw() +
  ggtitle("r = 0.097")+
  theme(text=element_text(size=12,  family="Times New Roman"))

cor.test(uk$`Labour Party`, uk$`Liberal Party`)

lablib <- ggplot(uk, aes(x = `Labour Party`, y = `Liberal Party`, label = topic)) + 
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_text_repel(family = "Times New Roman") +
  xlim(0, 25) +
  ylim(0, 25) +
  theme_bw() +
  ggtitle("r = 0.548")+
  theme(text=element_text(size=12,  family="Times New Roman"))

library(gridExtra)

uk_1979 <- grid.arrange(labcon, lablib, libcon, nrow = 1)

#ggsave("C:/Users/gundejac/Dropbox/Dissertation/Figures/fig_1_1.png", plot = uk_1979, dpi = 700, width = 15, height =5, units = 'in')
