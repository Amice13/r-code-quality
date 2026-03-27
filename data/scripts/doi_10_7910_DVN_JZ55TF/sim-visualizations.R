library(tidyverse)

x = read_csv('output/sim-strong-psis-output.csv') %>%
  arrange(gibbs.type)%>%
  group_by(delta.type, iter, Sim.Covariates)%>%
  summarize(value=100*(var[1]-var[2])/var[2])%>%
    rbind(read_csv('output/sim-med-psis-output.csv') %>%
            arrange(gibbs.type)%>%
            group_by(delta.type, iter, Sim.Covariates)%>%
            summarize(value=100*(var[1]-var[2])/var[2]))%>%
  rbind(read_csv('output/sim-weak-psis-output.csv') %>%
          arrange(gibbs.type)%>%
          group_by(delta.type, iter, Sim.Covariates)%>%
          summarize(value=100*(var[1]-var[2])/var[2]))%>%
  rbind(read_csv('output/sim-weak-betas-output.csv') %>%
          arrange(gibbs.type)%>%
          group_by(delta.type, iter, Sim.Covariates)%>%
          summarize(value=100*(var[1]-var[2])/var[2]))%>%
  rbind(read_csv('output/sim-med-betas-output.csv') %>%
          arrange(gibbs.type)%>%
          group_by(delta.type, iter, Sim.Covariates)%>%
          summarize(value=100*(var[1]-var[2])/var[2]))%>%
  rbind(read_csv('output/sim-strong-betas-output.csv') %>%
          arrange(gibbs.type)%>%
          group_by(delta.type, iter, Sim.Covariates)%>%
          summarize(value=100*(var[1]-var[2])/var[2]))%>%
  mutate(Coefficient = case_when(grepl('psis',Sim.Covariates) ~ 'Varying influence\nof covariates\non strata type',
                                 grepl('betas',Sim.Covariates) ~ 'Varying influence\nof covariates\non outcome'),
         Level = case_when(grepl('weak',Sim.Covariates) ~ 'Weak',
                           grepl('med',Sim.Covariates) ~ 'Medium',
                           grepl('strong',Sim.Covariates) ~ 'Strong'))%>%
  mutate(Level = factor(Level, levels = c('Weak', 'Medium','Strong')),
         delta.type = recode(delta.type, `delta.1` = paste('In-sample'), `delta.2`=paste('Population')))

p = ggplot(data = x ,
           aes(y = value, x = Level
               , fill = delta.type
               ))+
  geom_boxplot(position = position_dodge(.85))+
  #geom_linerange(aes(ymax=mean.var+1.96*se.var, ymin = mean.var-1.96*se.var)+
  facet_grid(Coefficient~.)+
  theme_bw()+
  geom_hline(yintercept=0,linetype='dashed')+
  ylab(paste('% Change in Variance'))+
  xlab('Influence of covariates in DGP')+
  theme(legend.title = element_blank(), legend.position = 'bottom',
        plot.caption = element_text(hjust = 0))#+
 # labs(caption = paste("Note: Boxplot present distribution of % variance reduction of", expression(delta), "from Gibbs across 1,000 draws of simulated data.\nFor each draw 4 Gibbs chains were run with covariates and 4 were run without covariates.\nGibbs parameters: 1,000 iterations, 200 burn-in, 2 thinning parameter, simulated data n = 1,000."))


ggsave(plot = p, file = 'figures/sim-plot-covariates.pdf',width=8,height=5,dpi=300)


#########
rm(x, p)
# rm(list=ls()[ls()!='p.time'])
gc()

x = read_csv('output/sim-monotonicity-TRUE-stable-TRUE-output.csv') %>%
  group_by(delta.type, iter)%>%
  mutate(count=1:n())%>%
mutate(gibbs.type = case_when(count == 1 ~ 'Both',
                              count == 2 ~ 'Stability',
                              count == 3 ~ 'Monotonicity',
                              count == 4 ~ 'Neither'),
       sim.type = case_when(Monotonicity == T & Stable == T ~ 'Both',
                            Monotonicity == F & Stable == T  ~ 'Stability',
                            Monotonicity == T & Stable == F  ~ 'Monotonicity',
                            Monotonicity == F & Stable == F  ~ 'Neither')) %>%
  ungroup %>%
  group_by(delta.type, sim.type)%>%
  mutate(value=100*(var-var[gibbs.type=='Neither'])/var[gibbs.type=='Neither'])%>%
  filter(gibbs.type!='Neither')%>%
  # rbind(read_csv('output/sim-monotonicity-TRUE-stable-FALSE-output.csv') %>%
  #         group_by(delta.type, iter)%>%
  #         mutate(count=1:n())%>%
  #         mutate(gibbs.type = case_when(count == 1 ~ 'Both',
  #                                       count == 2 ~ 'Stability',
  #                                       count == 3 ~ 'Monotonicity',
  #                                       count == 4 ~ 'Neither'),
  #                sim.type = case_when(Monotonicity == T & Stable == T ~ 'Both',
  #                                     Monotonicity == F & Stable == T  ~ 'Stability',
  #                                     Monotonicity == T & Stable == F  ~ 'Monotonicity',
  #                                     Monotonicity == F & Stable == F  ~ 'Neither')) %>%
  #         ungroup %>%
  #         group_by(delta.type, sim.type)%>%
  #         mutate(value=100*(var-var[gibbs.type=='Neither'])/var[gibbs.type=='Neither'])%>%
  #         filter(gibbs.type!='Neither'))%>%
  # rbind(read_csv('output/sim-monotonicity-FALSE-stable-FALSE-output.csv') %>%
  #         group_by(delta.type, iter)%>%
  #         mutate(count=1:n())%>%
  #         mutate(gibbs.type = case_when(count == 1 ~ 'Both',
  #                                       count == 2 ~ 'Stability',
  #                                       count == 3 ~ 'Monotonicity',
  #                                       count == 4 ~ 'Neither'),
  #                sim.type = case_when(Monotonicity == T & Stable == T ~ 'Both',
  #                                     Monotonicity == F & Stable == T  ~ 'Stability',
  #                                     Monotonicity == T & Stable == F  ~ 'Monotonicity',
  #                                     Monotonicity == F & Stable == F  ~ 'Neither')) %>%
  #         ungroup %>%
  #         group_by(delta.type, sim.type)%>%
  #         mutate(value=100*(var-var[gibbs.type=='Neither'])/var[gibbs.type=='Neither'])%>%
  #         filter(gibbs.type!='Neither'))%>%
  # rbind(read_csv('output/sim-monotonicity-FALSE-stable-TRUE-output.csv') %>%
  #         group_by(delta.type, iter)%>%
  #         mutate(count=1:n())%>%
  #         mutate(gibbs.type = case_when(count == 1 ~ 'Both',
  #                                       count == 2 ~ 'Stability',
  #                                       count == 3 ~ 'Monotonicity',
  #                                       count == 4 ~ 'Neither'),
  #                sim.type = case_when(Monotonicity == T & Stable == T ~ 'Both',
  #                                     Monotonicity == F & Stable == T  ~ 'Stability',
  #                                     Monotonicity == T & Stable == F  ~ 'Monotonicity',
  #                                     Monotonicity == F & Stable == F  ~ 'Neither')) %>%
  #         ungroup %>%
  #         group_by(delta.type, sim.type)%>%
  #         mutate(value=100*(var-var[gibbs.type=='Neither'])/var[gibbs.type=='Neither'])%>%
  #         filter(gibbs.type!='Neither'))%>%
  mutate(Sim = paste0('Sim. Data:\n',sim.type),
         Gibbs = paste0('Gibbs:\n',gibbs.type))%>%
  mutate(Sim = factor(Sim, levels = paste0('Sim. Data:\n',c('Neither', 'Stability', 'Monotonicity','Both'))),
         delta.type = recode(delta.type, `delta.1` = paste('In-sample'), `delta.2`=paste('Population')),
         Gibbs = factor(Gibbs, levels = paste0('Gibbs:\n',c('Neither', 'Stability', 'Monotonicity','Both'))))%>%
  filter(Sim=='Sim. Data:\nBoth')



#
# p = ggplot(data = x %>%
#              mutate(Gibbs=str_replace(Gibbs,'Gibbs:\n',''))
#              ,
#
#            aes(y = value, x = Gibbs
#                , shape = Gibbs
#            ))+
#   geom_point(position = position_dodge(.5),size=3)+
#   #geom_linerange(aes(ymax=mean.var+1.96*se.var, ymin = mean.var-1.96*se.var)+
#   facet_grid(Sim~delta.type)+
#   theme_bw()+
#   geom_hline(yintercept=0,linetype='dashed')+
#   ylab(paste('% Change in Variance'))+
#   xlab('Gibbs Assumptions')+
#   theme(legend.title = element_blank(), legend.position = 'bottom',
#         plot.caption = element_text(hjust = 0))#+
#   #labs(caption = paste("Note: Points represent average % Change in variance of \u03B4 from Gibbs across 100 draws of simulated data compared to no Gibbs assumptions.\nFor each draw 4 Gibbs chains were run for each combination of assumptions.\nGibbs parameters: 1,000 iterations, 200 burn-in, 2 thinning parameter, simulated data n = 1,000."))
#
#
# ggsave(plot = p, file = 'output/sim-plot-assumptions.pdf',width=11,height=5)


p = ggplot(data = x %>%
             mutate(Gibbs=str_replace(Gibbs,'Gibbs:\n',''))%>%
             mutate(Gibbs = factor(Gibbs,levels =c('Stability','Monotonicity','Both'))),
           aes(y = value, x = Gibbs
               , fill = delta.type
           ))+
  geom_boxplot(position = position_dodge(.85))+
  #geom_linerange(aes(ymax=mean.var+1.96*se.var, ymin = mean.var-1.96*se.var)+
  #facet_grid(delta.type~.)+
  theme_bw()+
  geom_hline(yintercept=0,linetype='dashed')+
  ylab(paste('% Change in Variance'))+
  xlab('Gibbs Assumptions')+  theme(legend.title = element_blank(), legend.position = 'bottom',
        plot.caption = element_text(hjust = 0))#+
#labs(caption = paste("Note: Boxplot present distribution of % variance reduction of \u03B4 from Gibbs across 100 draws of simulated data compared to no Gibbs assumptions.\nFor each draw 4 Gibbs chains were run for each combination of assumptions.\nGibbs parameters: 1,000 iterations, 200 burn-in, 2 thinning parameter, simulated data n = 1,000."))


ggsave(plot = p, file = 'figures/sim-plot-assumptions.pdf',width=8,height=5,dpi=300)

