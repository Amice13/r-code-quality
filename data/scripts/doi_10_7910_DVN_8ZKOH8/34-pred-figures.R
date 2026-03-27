rm(list=ls())
gc()
library(tidyverse)
files = list.files('output')
files = files[grepl('scatter',files)]
files = files[!grepl('unweighted|logged',files)]


l = lapply(files, FUN =function(x){
  read_csv(paste0('output/',x))%>%
    mutate(geo = ifelse(grepl('county',x), 'County', 'District'),
           training = as.character(training),
           test = as.character(test))

})

l = do.call(bind_rows, l)%>%
  mutate(years = paste('Train:', training, '--', 'Test:', test))

holder = l %>%
  filter(model=='Registration')%>%
  drop_na

g1 = holder %>%
  ggplot(aes(x = votes/1000, y = pred/1000, label = state))+
  geom_text()+
  theme_bw()+
  xlab('Total votes (1,000s)')+
  ylab('Predicted votes (1,000s)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Registration model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  ylim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(size = 10))


holder = l %>%
  filter(model=='Lagged Vote')%>%
  drop_na

g2 = holder %>%
  ggplot(aes(x = votes/1000, y = pred/1000, label = state))+
  geom_text()+
  theme_bw()+
  xlab('Total votes (1,000s)')+
  ylab('Predicted votes (1,000s)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Lagged vote model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  ylim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(size = 10))


holder = l %>%
  filter(model=='Early Vote')%>%
  drop_na

 g4 = holder %>%
   ggplot(aes(x = votes/1000, y = pred/1000, label = state))+
   geom_text()+
   theme_bw()+
   xlab('Total votes (1,000s)')+
   ylab('Predicted votes (1,000s)')+
   facet_grid(cols = vars(years), rows = vars(geo))+
   ggtitle('Early vote model')+
   geom_abline(linetype='dashed')+
   geom_smooth(method='lm')+
   xlim(0, max(c(holder$votes/1000, holder$pred/1000)))+
   ylim(0, max(c(holder$votes/1000, holder$pred/1000)))+
   theme(axis.text.x = element_text(angle = 45, vjust = .5),
         text = element_text(size = 10))

holder = l %>%
  filter(model=='Demographics')%>%
  drop_na

g3 = holder %>%
  ggplot(aes(x = votes/1000, y = pred/1000, label = state))+
  geom_text()+
  theme_bw()+
  xlab('Total votes (1,000s)')+
  ylab('Predicted votes (1,000s)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Demographics model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  ylim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(size = 10))

holder = l %>%
  filter(model=='Competition')%>%
  drop_na

g5 = holder %>%
  ggplot(aes(x = votes/1000, y = pred/1000, label = state))+
  geom_text()+
  theme_bw()+
  xlab('Total votes (1,000s)')+
  ylab('Predicted votes (1,000s)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Competition model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  ylim(0, max(c(holder$votes/1000, holder$pred/1000)))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(size = 10))




library(patchwork)


g1 + g2 + g3 + g4 + g5
  plot_layout(ncol = 3, nrow = 2)
ggsave(filename='figures/scatter-weighted.png',dpi=300,width = 11, height = 6, units ='in')


# logged scatterplot

holder = l %>%
  filter(model=='Registration')%>%
  drop_na

g1 = holder %>%
  ggplot(aes(x = log(votes), y = log(pred), label = state))+
  geom_text(size = 3)+
  theme_bw()+
  xlab('Total votes (logged)')+
  ylab('Predicted votes (logged)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Registration model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
         ylim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(family = 'Times New Roman'))


holder = l %>%
  filter(model=='Lagged Vote')%>%
  drop_na

g2 = holder %>%
  ggplot(aes(x = log(votes), y = log(pred), label = state))+
  geom_text(size = 3)+
  theme_bw()+
  xlab('Total votes (logged)')+
  ylab('Predicted votes (logged)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Lagged vote model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
  xlim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
         ylim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(family = 'Times New Roman'))


holder = l %>%
  filter(model=='Early Vote')%>%
  drop_na

g4 = holder %>%
  ggplot(aes(x = log(votes), y = log(pred), label = state))+
  geom_text(size=3)+
  theme_bw()+
  xlab('Total votes (logged)')+
  ylab('Predicted votes (logged)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Early vote model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
 xlim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
        ylim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(family = 'Times New Roman'))

holder = l %>%
  filter(model=='Demographics')%>%
  drop_na

g3 = holder %>%
  ggplot(aes(x = log(votes), y = log(pred), label = state))+
  geom_text(size=3)+
  theme_bw()+
  xlab('Total votes (logged)')+
  ylab('Predicted votes (logged)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Demographics model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
xlim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
       ylim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(family = 'Times New Roman'))

holder = l %>%
  filter(model=='Competition')%>%
  drop_na

g5 = holder %>%
  ggplot(aes(x = log(votes), y = log(pred), label = state))+
  geom_text(size = 3)+
  theme_bw()+
  xlab('Total votes (logged)')+
  ylab('Predicted votes (logged)')+
  facet_grid(cols = vars(years), rows = vars(geo))+
  ggtitle('Competition model')+
  geom_abline(linetype='dashed')+
  geom_smooth(method='lm')+
xlim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
       ylim(min(c(log(holder$votes), log(holder$pred))), max(c(log(holder$votes), log(holder$pred))))+
  theme(axis.text.x = element_text(angle = 45, vjust = .5),
        text = element_text(family = 'Times New Roman'))




library(patchwork)


g1 + g2 + g3 + g4 + g5
plot_layout(ncol = 3, nrow = 2)
ggsave(filename='figures/scatter-weighted-main-models-log-axes.png',dpi=300,width = 11, height = 6, units ='in')


