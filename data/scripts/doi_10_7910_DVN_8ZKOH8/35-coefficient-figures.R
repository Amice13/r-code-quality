rm(list=ls())
gc()
library(tidyverse)
files = list.files('output')
files = files[grepl('coefficient',files)]
files = files[grepl('weighted',files)]
files = files[!grepl('logged',files)]
l = lapply(files, FUN =function(x){
  read_csv(paste0('output/',x))%>%
    mutate(geo = ifelse(grepl('county',x), 'County', 'District'),
           training = as.character(training),
           test = as.character(test))

})

l = do.call(bind_rows, l) |>
  mutate(merge.col = as.numeric(substr(training, nchar(training)-3, nchar(training))))|>
  left_join(read_csv('data/state-1976-2020-president.csv')|>
              filter(year%in%c(2012,2016,2020))|>
  group_by(state_po,year)|>
  summarise(demvotes = sum(candidatevotes[party_simplified=='DEMOCRAT'],na.rm=T),
            repvotes = sum(candidatevotes[party_simplified=='REPUBLICAN'],na.rm=T),
            totalvotes = totalvotes[1] )|>
  mutate(repshare = repvotes/totalvotes)|>
  ungroup()|>
  select(state=state_po, merge.col=year, repshare),
  by=c('state','merge.col')
  )




g1 = l %>%
  filter(model=='Registration')%>%
  drop_na %>%

  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('Registration coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(geo), rows = vars(training),
             scales = 'free')+
  ggtitle('Registration model')

g2 = l %>%
  filter(model=='Lagged Vote')%>%
  drop_na %>%

  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('Lagged vote coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(geo), rows = vars(training),
             scales = 'free')+
  ggtitle('Lagged vote model')

g4 = l %>%
  filter(model=='Early Vote')%>%
  drop_na %>%

  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('Early vote coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(geo), rows = vars(training),
             scales = 'free')+
  ggtitle('Early vote model')

g3 = l %>%
  filter(model=='Demographics' & variable == 'pct_white' & geo == 'County')%>%
  drop_na %>%
  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('% White coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(geo), rows = vars(training),
             scales = 'free')+
  ggtitle('Demographic model')

g5 = l %>%
  filter(model=='Lagged Margin' & variable == 'Lagged Margin')%>%
  drop_na %>%
  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('Lagged margin coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(geo), rows = vars(training),
             scales = 'free')+
  ggtitle('Competition model')




library(patchwork)


g1 + g2 + g3 + g4 + g5
  plot_layout(ncol = 3, nrow = 2)
ggsave(filename='figures/coefficients-weighted.png',dpi=300,width = 9, height = 6, units ='in')




g = l %>%
  filter(model=='Demographics'  & geo == 'County')%>%
  mutate(variable = dplyr::recode(variable,
                                  `pct_white` = '% White',
                                  `pct_black` = '% Black',
                                  `pct_hispanic` = '% Hisp.',
                                  `pct_married` = '% Married',
                                  `pct_college` = '% College',
                                  `median_hh_income` = 'Med. HH. Inc.',
                                  `pct_age_15_24` = '% Age 15-24',
                                  `pct_age_25_34` = '% Age 25-34',
                                  `pct_age_35_64` = '% Age 35-64',
                                  `pct_age_65_up` = '% Age 65+',
                                  `tot_pop` = 'Pop.'

                                  ))%>%
  drop_na %>%
  ggplot(aes(x = coef))+
  geom_histogram(fill = 'grey', color = 'black')+
  theme_bw()+
  xlab('Coefficient')+
  ylab('Count')+
  facet_grid(cols = vars(variable), rows = vars(training),
             scales = 'free')+
  ggtitle('Demographic model')+
  theme(axis.text.x = element_text(angle = 90))

ggsave(filename='figures/demographic-coefficients-weighted.png',dpi=300,width = 11, height = 5, units ='in')




## scatter plots


g1 = l %>%
  filter(model=='Registration' )%>%
  drop_na %>%

  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('Registration coefficient')+
  xlab('Republican vote share\n(training election)')+
  facet_grid(rows = vars(geo), cols = vars(training),
             scales = 'free')+
  ggtitle('Registration model')

g2 = l %>%
  filter(model=='Lagged Vote' )%>%
  drop_na %>%

  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('Lagged vote coefficient')+
  xlab('Republican vote share\n(training election)')+
  facet_grid(rows = vars(geo), cols = vars(training),
             scales = 'free')+
  ggtitle('Lagged vote model')

g4 = l %>%
  filter(model=='Early Vote' )%>%
  drop_na %>%

  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('Early vote coefficient')+
  xlab('Republican vote share\n(training election)')+
  facet_grid(rows = vars(geo), cols = vars(training),
             scales = 'free')+
  ggtitle('Early vote model')


g3 = l %>%
  filter(model=='Demographics' & variable == 'pct_white' & geo == 'County')%>%
  drop_na %>%
  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('% white coefficient')+
  xlab('Republican vote share (training election)')+
  facet_grid(rows = vars(geo), cols = vars(training),
             scales = 'free')+
  ggtitle('Demographics model')

g5 = l %>%
  filter(model=='Lagged Margin' & variable == 'Lagged Margin')%>%
  drop_na %>%
  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('Lagged margin coefficient')+
  xlab('Republican vote share\n(training election)')+
  facet_grid(rows = vars(geo), cols = vars(training),
             scales = 'free')+
  ggtitle('Competition model')




library(patchwork)


g1 + g2 + g3 + g4 + g5
plot_layout(ncol = 3, nrow = 2)
ggsave(filename='figures/scatter-coefficients-weighted.png',dpi=300,width = 9, height = 6, units ='in')


l %>%
  mutate(variable=dplyr::recode(variable,
                                `pct_white` = "% White",
                                `pct_black` = "% Black",
                                `pct_hispanic` = "% Hispanic",
                                `pct_age_15_24` = "% Age 15-24",
                                `pct_age_25_34` = "% Age 25-34",
                                `pct_age_35_64` = "% Age 35-64",
                                `pct_age_65_up` = "% Age 65+",
                                `tot_pop`='Total Pop.',
                                `pct_married` = "% Married",
                                `pct_college` = "% College",
                                `median_hh_income`='Med. Hh. Inc.'
                                ))|>
  filter(model=='Demographics'  & geo == 'County')%>%
  drop_na %>%
  ggplot(aes(y = coef, x = repshare))+
  geom_text(aes(label=state), size = 3)+
  theme_bw()+
  ylab('Coefficient')+
  xlab('Republican vote share (training election)')+
  facet_grid(rows = vars(variable), cols = vars(training),
             scales = 'free')+
  ggtitle('Demographics model')
ggsave(filename='figures/scatter-coefficients-weighted-demo-model.png',dpi=300,width = 9, height = 13, units ='in')

