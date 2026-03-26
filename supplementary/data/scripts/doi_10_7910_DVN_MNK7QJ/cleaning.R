temp <- select(data, c(1,3, 5:24))

temp <- temp %>%
  pivot_longer(cols = -(1:2),
               names_to = "year",
               values_to = "value"
  )
temp <- temp %>%
  pivot_wider(names_from = 'Series.Name',
              values_from = 'value'
  )
panel <- temp

panel <- panel %>%
  rename(Country = `Country.Name`, 
         Year = `year`, 
         labor.force.participation = `Labor force participation rate, total (% of total population ages 15+) (modeled ILO estimate)`,
         subsidies.and.transfers = `Subsidies and other transfers (% of expense)`,
         GDP.per.capita = `GDP per capita (constant 2015 US$)`,
         gross.capital.formation = `Gross capital formation (% of GDP)` ,
         life.expectancy = `Life expectancy at birth, total (years)` ,
         school.enrollment = `School enrollment, secondary (% gross)`, 
         GNI.per.capita = `GNI per capita, Atlas method (current US$)`,
         labor.force.participation.female = `Labor force participation rate, female (% of female population ages 15+) (modeled ILO estimate)`,
         labor.force.participation.male = `Labor force participation rate, male (% of male population ages 15+) (modeled ILO estimate)`,
         life.expectancy.female = `Life expectancy at birth, female (years)`,
         life.expectancy.male = `Life expectancy at birth, male (years)`,
         school.enrollment.female = `School enrollment, secondary, female (% gross)`,
         school.enrollment.male = `School enrollment, secondary, male (% gross)`)

panel <- panel %>% 
  mutate(labor.force.participation = ifelse(labor.force.participation =='..', NA, labor.force.participation),
         subsidies.and.transfers = ifelse(subsidies.and.transfers=='..', NA, subsidies.and.transfers),
         GDP.per.capita = ifelse(GDP.per.capita=='..', NA , GDP.per.capita),
         gross.capital.formation = ifelse(gross.capital.formation=='..', NA, gross.capital.formation),
         life.expectancy = ifelse(life.expectancy=='..', NA, life.expectancy),
         school.enrollment = ifelse(school.enrollment =='..', NA, school.enrollment),
         GNI.per.capita = ifelse(GNI.per.capita=='..', NA, GNI.per.capita),
         labor.force.participation.female = ifelse(labor.force.participation.female=='..', NA, labor.force.participation.female),
         labor.force.participation.male = ifelse(labor.force.participation.male=='..', NA, labor.force.participation.male),
         life.expectancy.female = ifelse(life.expectancy.female=='..', NA, life.expectancy.female),
         life.expectancy.male = ifelse(life.expectancy.male=='..', NA, life.expectancy.male),
         school.enrollment.female = ifelse(school.enrollment.female=='..', NA, school.enrollment.female),
         school.enrollment.male = ifelse(school.enrollment.male=='..', NA, school.enrollment.male))

i <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
panel[ , i] <- apply(panel[ , i], 2,            
                     function(x) as.numeric(as.character(x)))

panel <- panel %>% 
  mutate(labor.force.participation = ifelse(labor.force.participation=='..', NA, labor.force.participation),
         subsidies.and.transfers = ifelse(subsidies.and.transfers=='..', NA, subsidies.and.transfers),
         GDP.per.capita = ifelse(GDP.per.capita=='..', NA , GDP.per.capita),
         gross.capital.formation = ifelse(gross.capital.formation=='..', NA, gross.capital.formation),
         life.expectancy = ifelse(life.expectancy=='..', NA, life.expectancy),
         school.enrollment = ifelse(school.enrollment =='..', NA, school.enrollment),
         GNI.per.capita = ifelse(GNI.per.capita =='..', NA, GNI.per.capita),
         labor.force.participation.female = ifelse(labor.force.participation.female=='..', NA, labor.force.participation.female),
         labor.force.participation.male = ifelse(labor.force.participation.male=='..', NA, labor.force.participation.male),
         life.expectancy.female = ifelse(life.expectancy.female=='..', NA, life.expectancy.female),
         life.expectancy.male = ifelse(life.expectancy.male=='..', NA, life.expectancy.male),
         school.enrollment.female = ifelse(school.enrollment.female=='..', NA, school.enrollment.female),
         school.enrollment.male = ifelse(school.enrollment.male=='..', NA, school.enrollment.male))

panel <- panel %>% 
  mutate(Year = as.numeric(substring(Year, 2, 5)))

panel <- panel %>% 
  mutate(income_level = ifelse(GNI.per.capita < 1085, "Low Income", 
                               ifelse( GNI.per.capita < 4255, "Low Middle Income", 
                                       ifelse( GNI.per.capita < 13205, "Upper Middle Income", "High Income"))))

panel <- panel %>%
  mutate(low_income = ifelse(income_level == 'Low Income', 1, 0),
         low_middle_income = ifelse(income_level == 'Low Middle Income', 1, 0),
         upper_middle_income = ifelse(income_level == 'Upper Middle Income', 1, 0),
         high_income = ifelse(income_level == 'High Income', 1, 0))

panel <- panel %>% 
  mutate(log.labor.force.participation = log(labor.force.participation),
         log.subsidies.and.transfers = log(subsidies.and.transfers),
         log.GDP.per.capita = log(GDP.per.capita),
         log.gross.capital.formation = log(gross.capital.formation),
         log.life.expectancy = log(life.expectancy) ,
         log.school.enrollment = log(school.enrollment),
         log.labor.force.participation.female = log(labor.force.participation.female),
         log.labor.force.participation.male = log(labor.force.participation.male),
         log.life.expectancy.female = log(life.expectancy.female),
         log.life.expectancy.male = log(life.expectancy.male),
         log.school.enrollment.female = log(school.enrollment.female),
         log.school.enrollment.male = log(school.enrollment.male))
