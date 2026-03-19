# map

rm(list=ls())
gc()

#library(tigris)
library(usmap)
library(ggplot2)
library(tidyverse)
library(mapproj)
library(ggthemes)
library(patchwork)
library(haven)
library(devtools)
#install_github('yaweige/ggfun') # need github version not cran version of ggfun
library(ggfun)
library(maps)
library(tidyverse)
library(tinytiger)
library(glue)
library(wacolors)
#options(tigris_class = "sf")
############ Figure 2: County map of changes in Exposure and places contributing to partisan segregation


tot.rate = read_csv('data/counties_analysis.csv')|>
  summarise(
    w.tot.rate12 = weighted.mean(total12/reg12, w=reg12, na.rm=T),
  w.tot.rate16 = weighted.mean(total16/reg16, w=reg16, na.rm=T),
  w.tot.rate20 = weighted.mean(total20/reg20, w=reg20, na.rm=T)


) |>
  mutate(w.tot.rate12 = round(w.tot.rate12*100, 1),
         w.tot.rate16 = round(w.tot.rate16*100, 1),
         w.tot.rate20 = round(w.tot.rate20*100, 1)
         )

## Get data
dat.cty = read_csv('data/counties_analysis.csv')%>%
  mutate(t12 = case_when(total12/reg12 < .68 ~ '< 60%',
                         total12/reg12 >= .68 & total12/reg12 < .75 ~ '60% - 75%',
                         total12/reg12 >= .75 & total12/reg12 < 0.8 ~ '75% - 80%',
                         total12/reg12 >= .8  ~ '> 80%'

                         ),
         t16 = case_when(total16/reg16 < .68 ~ '< 60%',
                         total16/reg16 >= .68 & total16/reg16 < .75 ~ '60% - 75%',
                         total16/reg16 >= .75 & total16/reg16 < 0.8 ~ '75% - 80%',
                         total16/reg16 >= .8  ~ '> 80%'

         ),
         t20 = case_when(total20/reg20 < .68 ~ '< 60%',
                         total20/reg20 >= .68 & total20/reg20 < .75 ~ '60% - 75%',
                         total20/reg20 >= .75 & total20/reg20 < 0.8 ~ '75% - 80%',
                         total20/reg20 >= .8  ~ '> 80%'

         ))%>%
  select(state,fips, t12, t16, t20)


my.colors = c(`< 60%` = '#BC7A7D',
              `60% - 75%` = '#925C78',
              `75% - 80%` = '#6E3D71',
              `> 80%` = '#352C5A',
              `No data` = '#F3F3F3'

)

#q12 = quantile(dat.cty$t12)
#q16 = quantile(dat.cty$t16)
#q20 = quantile(dat.cty$t20)
#




state_map <- us_map(regions = "states")



county_map <-  as.numeric(us_map(regions = "counties")$fips)%>%
  unique%>%
  as_tibble %>%
  select(fips=value)%>%
  left_join(dat.cty,by='fips')%>%
  mutate(t12 = ifelse(is.na(t12), 'No data', t12),
         t16 = ifelse(is.na(t16), 'No data', t16),
         t20 = ifelse(is.na(t20), 'No data', t20))%>%
  mutate(t12 = factor(t12, levels = c('< 60%', '60% - 75%', '75% - 80%', '> 80%', 'No data')),
         t16 = factor(t16, levels = c('< 60%', '60% - 75%', '75% - 80%', '> 80%', 'No data')),
         t20 = factor(t20, levels = c('< 60%', '60% - 75%', '75% - 80%', '> 80%', 'No data')))%>%
  pivot_longer(t12:t20)%>%
  mutate(Election = case_when(name == 't12' ~ glue('2012\nNational turnout: {tot.rate$w.tot.rate12}%\nof registered voters'),
                              name == 't16' ~ glue('2016\nNational turnout: {tot.rate$w.tot.rate16}%\nof registered voters'),
                              name == 't20' ~ glue('2020\nNational turnout: {tot.rate$w.tot.rate20}%\nof registered voters')))

counties <- tigris::counties(year = 2018)

states = plot_usmap('states', exclude = c('02', '15'))
counties = plot_usmap(data = county_map, values = 'value', regions=c('counties'),
                       exclude = us_map(regions = "counties")$fips[substr(us_map(regions = "counties")$fips,1,2)%in%c('02','15')]
)


g1 = ggplot() +
  geom_polygon(data=counties[[1]],
               aes(x=x,
                   y=y,
                   group=group,
                   fill = counties[[1]]$value),
               color = "darkgrey",
               size = 0.1) +
  geom_polygon(data=states[[1]],
               aes(x=x,
                   y=y,
                   group=group),
               color = "black",
               fill = alpha(0.01)) +
  coord_equal() +
  theme_map() +
  theme(legend.title=element_blank(),
        text = element_text(family = 'Times New Roman', size = 20))+
  theme(legend.position='bottom', strip.background =element_rect(fill="white", color = 'white'),
        legend.key.size = unit(.5,"line")) +
  scale_fill_manual(values = my.colors)+
 scale_y_continuous(expand = c(0, 0))+
facet_grid(. ~ Election)
ggsave(plot = g1, filename = 'figures/county-map.png', dpi = 300, width = 11, height = 6, units = 'in')

