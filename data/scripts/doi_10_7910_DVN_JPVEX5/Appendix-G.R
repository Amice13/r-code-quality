library(tidyverse)

setwd('C:/Users/jdt34/Dropbox/VNA_Responsiveness/20191201_LaborLawMechanisms/')

vna = haven::read_dta('data/working2.dta') %>%
  mutate(IPW=1/(as.integer(str_extract(bucket, '\\d{2,3}'))/100),
         ws_share=plyr::mapvalues(ws_share, from=0, to=1)) %>%
  subset(ID!=148) %>%
    group_by(province) %>%
    mutate(t1_share=mean(citizen_electoral),
           t2_share=mean(firm_electoral),
           t3_share=mean(citizen_upward),
           t4_share=mean(firm_upward)) %>%
    ungroup
ri_data = haven::read_dta('data/RI_Sample1000.dta') %>% 
  merge(subset(vna, select=c(ID, ws_ts, ws_share)), by='ID', all.x=T) %>%
  mutate(IPW=1/(as.integer(str_extract(Bucket, '\\d{2,3}'))/100)) %>%
  group_by(province) %>%
  mutate(t1_share=mean(citizen_electoral),
         t2_share=mean(firm_electoral),
         t3_share=mean(citizen_upward),
         t4_share=mean(firm_upward)) %>%
  ungroup


m1_outcomes = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive, 
                 data=vna)
m2_outcomes = lm(critical ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive, 
                 data=vna)
m3_outcomes = lm(prolabor ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive, 
                 data=vna)
m4_outcomes = lm(mean_reflect ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive, 
                 data=vna)
m5_outcomes = lm(ws_ts ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive, 
                 data=vna, 
                 weights=ws_share)
m6_outcomes = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive + 
                   t1_share + t2_share + t3_share + t4_share, 
                 data=vna)
m7_outcomes = lm(critical ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive + 
                   t1_share + t2_share + t3_share + t4_share, 
                 data=vna)
m8_outcomes = lm(prolabor ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive + 
                   t1_share + t2_share + t3_share + t4_share, 
                 data=vna)
m9_outcomes = lm(mean_reflect ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                   fulltime + centnom + competitive + 
                   t1_share + t2_share + t3_share + t4_share, 
                 data=vna)
m10_outcomes = lm(ws_ts ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                    fulltime + centnom + competitive + 
                    t1_share + t2_share + t3_share + t4_share, 
                  data=vna, 
                  weights=ws_share)
c(coef(m1_outcomes)['citizen_electoral'],
  coef(m2_outcomes)['citizen_electoral'],
  coef(m3_outcomes)['citizen_electoral'],
  coef(m4_outcomes)['citizen_electoral'],
  coef(m5_outcomes)['citizen_electoral'],
  coef(m6_outcomes)['citizen_electoral'],
  coef(m7_outcomes)['citizen_electoral'],
  coef(m8_outcomes)['citizen_electoral'],
  coef(m9_outcomes)['citizen_electoral'],
  coef(m10_outcomes)['citizen_electoral']) %>%
  round(digits=3) %>%
  setNames(paste0('(', 1:10, ')'))



m1_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward, 
        data=vna)
m2_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                         fulltime + centnom + competitive, 
        data=vna)
m3_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                         fulltime + centnom + competitive + 
                         t1_share + t2_share + t3_share + t4_share, 
        data=vna)
m4_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                         fulltime + centnom + competitive, 
        data=vna, 
        weights=IPW)
m5_specifications = lm(spoke ~ citizen_electoral + 
                         fulltime + centnom + competitive + 
                         bucket, 
        data=vna)
m6_specifications = lm(spoke ~ citizen_electoral + 
                         fulltime + centnom + competitive + 
                         t1_share, 
        data=vna)
m7_specifications = lm(spoke ~ citizen_electoral + 
                         fulltime + centnom + competitive, 
        data=vna, 
        weights=IPW)
m8_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                         fulltime + centnom + competitive + 
                         bucket, 
        data=subset(vna, bucket!='100% Treated'))
m9_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                         fulltime + centnom + competitive + 
                         t1_share + t2_share + t3_share + t4_share, 
        data=subset(vna, bucket!='100% Treated'))
m10_specifications = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                          fulltime + centnom + competitive, 
         data=subset(vna, bucket!='100% Treated'), 
         weights=IPW)
c(coef(m1_specifications)['citizen_electoral'],
  coef(m2_specifications)['citizen_electoral'],
  coef(m3_specifications)['citizen_electoral'],
  coef(m4_specifications)['citizen_electoral'],
  coef(m5_specifications)['citizen_electoral'],
  coef(m6_specifications)['citizen_electoral'],
  coef(m7_specifications)['citizen_electoral'],
  coef(m8_specifications)['citizen_electoral'],
  coef(m9_specifications)['citizen_electoral'],
  coef(m10_specifications)['citizen_electoral']) %>%
  round(digits=3) %>%
  setNames(paste0('(', 1:10, ')'))

ri_coefs_outcomes = ri_coefs_specifications = NULL
for(i in unique(ri_data$Iteration)) {
  cat(paste0(i, '...'))
  tmp = subset(ri_data, Iteration==i)
  tmp_m1 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp)
  tmp_m2 = lm(critical ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp)
  tmp_m3 = lm(prolabor ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp)
  tmp_m4 = lm(mean_reflect ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp)
  tmp_m5 = lm(ws_ts ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp, 
              weights=ws_share)
  tmp_m6 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=tmp)
  tmp_m7 = lm(critical ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=tmp)
  tmp_m8 = lm(prolabor ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=tmp)
  tmp_m9 = lm(mean_reflect ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=tmp)
  tmp_m10 = lm(ws_ts ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                 fulltime + centnom + competitive + 
                 t1_share + t2_share + t3_share + t4_share, 
               data=tmp, 
               weights=ws_share)
  ri_coefs_outcomes = data.frame(m1=coef(tmp_m1)['citizen_electoral'],
                                 m2=coef(tmp_m2)['citizen_electoral'],
                                 m3=coef(tmp_m3)['citizen_electoral'],
                                 m4=coef(tmp_m4)['citizen_electoral'],
                                 m5=coef(tmp_m5)['citizen_electoral'],
                                 m6=coef(tmp_m6)['citizen_electoral'],
                                 m7=coef(tmp_m7)['citizen_electoral'],
                                 m8=coef(tmp_m8)['citizen_electoral'],
                                 m9=coef(tmp_m9)['citizen_electoral'],
                                 m10=coef(tmp_m10)['citizen_electoral'],
                                 Iteration=i) %>%
    rbind(ri_coefs_outcomes, .)
  
  
  tmp_m1 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward, 
              data=tmp)
  tmp_m2 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp)
  tmp_m3 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=tmp)
  tmp_m4 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive, 
              data=tmp, 
              weights=IPW)
  tmp_m5 = lm(spoke ~ citizen_electoral + 
                fulltime + centnom + competitive + 
                Bucket, 
              data=tmp)
  tmp_m6 = lm(spoke ~ citizen_electoral + 
                fulltime + centnom + competitive + 
                t1_share, 
              data=tmp)
  tmp_m7 = lm(spoke ~ citizen_electoral + 
                fulltime + centnom + competitive, 
              data=tmp, 
              weights=IPW)
  tmp_m8 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                Bucket, 
              data=subset(tmp, Bucket!='100% Treated'))
  tmp_m9 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                fulltime + centnom + competitive + 
                t1_share + t2_share + t3_share + t4_share, 
              data=subset(tmp, Bucket!='100% Treated'))
  tmp_m10 = lm(spoke ~ citizen_electoral + citizen_upward + firm_electoral + firm_upward + 
                 fulltime + centnom + competitive, 
               data=subset(tmp, Bucket!='100% Treated'), 
               weights=IPW)
  ri_coefs_specifications = data.frame(m1=coef(tmp_m1)['citizen_electoral'],
                                       m2=coef(tmp_m2)['citizen_electoral'],
                                       m3=coef(tmp_m3)['citizen_electoral'],
                                       m4=coef(tmp_m4)['citizen_electoral'],
                                       m5=coef(tmp_m5)['citizen_electoral'],
                                       m6=coef(tmp_m6)['citizen_electoral'],
                                       m7=coef(tmp_m7)['citizen_electoral'],
                                       m8=coef(tmp_m8)['citizen_electoral'],
                                       m9=coef(tmp_m9)['citizen_electoral'],
                                       m10=coef(tmp_m10)['citizen_electoral'],
                                       Iteration=i) %>%
    rbind(ri_coefs_specifications, .)
  
}; rm(list=c('tmp','tmp_m1','tmp_m2','tmp_m3','tmp_m4','tmp_m5',
             'tmp_m6','tmp_m7','tmp_m8','tmp_m9','tmp_m10','i'))


observed_pvalues_outcomes = c(0.033, 0.011, 0.042, 0.027, 0.102,
                              0.048, 0.064, 0.176, 0.060, 0.037) %>%
  setNames(c('Spoke','Critical','Pro-labor','Reflected','Wordscores',
             paste(c('Spoke','Critical','Pro-labor','Reflected','Wordscores'), '(sat.)')))
ri_pvalues_outcomes = ri_coefs_outcomes %>%
  mutate(p1=(1e3-rank(m1, ties.method='min'))/1e3,
         p2=(1e3-rank(m2, ties.method='min'))/1e3,
         p3=(1e3-rank(m3, ties.method='min'))/1e3,
         p4=(1e3-rank(m4, ties.method='min'))/1e3,
         p5=(1e3-rank(m5, ties.method='min'))/1e3,
         p6=(1e3-rank(m6, ties.method='min'))/1e3,
         p7=(1e3-rank(m7, ties.method='min'))/1e3,
         p8=(1e3-rank(m8, ties.method='min'))/1e3,
         p9=(1e3-rank(m9, ties.method='min'))/1e3,
         p10=(1e3-rank(m10, ties.method='min'))/1e3) %>%
  subset(select=c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)) %>%
  magrittr::set_rownames(NULL)


observed_pvalues_specifications = c(0.041, 0.033, 0.048, 0.038, 0.057,
                                    0.035, 0.030, 0.126, 0.110, 0.098) %>%
  setNames(paste('Model', str_pad(1:10, 3, 'left', ' ')))
ri_pvalues_specifications = ri_coefs_specifications %>%
  mutate(p1=(1e3-rank(m1, ties.method='min'))/1e3,
         p2=(1e3-rank(m2, ties.method='min'))/1e3,
         p3=(1e3-rank(m3, ties.method='min'))/1e3,
         p4=(1e3-rank(m4, ties.method='min'))/1e3,
         p5=(1e3-rank(m5, ties.method='min'))/1e3,
         p6=(1e3-rank(m6, ties.method='min'))/1e3,
         p7=(1e3-rank(m7, ties.method='min'))/1e3,
         p8=(1e3-rank(m8, ties.method='min'))/1e3,
         p9=(1e3-rank(m9, ties.method='min'))/1e3,
         p10=(1e3-rank(m10, ties.method='min'))/1e3) %>%
  subset(select=c(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)) %>%
  magrittr::set_rownames(NULL)


false_positives_outcomes = false_positives_specifications = NULL
for(alpha in seq(0.0001, 1, 0.0001)) {
  if(as.integer(10000*alpha) %% 100L == 0) cat(paste(alpha, '\n'))
  tmp = ri_pvalues_outcomes %>%
    apply(1, function(x) sum(x<=alpha)) %>%
    table %>%
    prop.table %>%
    data.frame %>%
    magrittr::set_colnames(c('FP','Prop'))
  missing = setdiff(as.character(0:10), tmp$FP)
  if(length(missing)>0) {
    missing = data.frame(FP=missing, Prop=0)
  } else {
    missing = NULL
  }
  tmp = rbind(tmp, missing) %>%
    mutate(FPinteger=as.integer(as.character(FP))) %>%
    arrange(desc(FPinteger)) %>%
    mutate(CumProp=cumsum(Prop),
           Alpha=alpha,
           Observed=F)
  tmp$Observed[tmp$FPinteger==sum(observed_pvalues_outcomes<=alpha)] = T
  false_positives_outcomes = rbind(false_positives_outcomes, tmp)

  
  tmp = ri_pvalues_specifications %>%
    apply(1, function(x) sum(x<=alpha)) %>%
    table %>%
    prop.table %>%
    data.frame %>%
    magrittr::set_colnames(c('FP','Prop'))
  missing = setdiff(as.character(0:10), tmp$FP)
  if(length(missing)>0) {
    missing = data.frame(FP=missing, Prop=0)
  } else {
    missing = NULL
  }
  tmp = rbind(tmp, missing) %>%
    mutate(FPinteger=as.integer(as.character(FP))) %>%
    arrange(desc(FPinteger)) %>%
    mutate(CumProp=cumsum(Prop),
           Alpha=alpha,
           Observed=F)
  tmp$Observed[tmp$FPinteger==sum(observed_pvalues_specifications<=alpha)] = T
  false_positives_specifications = rbind(false_positives_specifications, tmp)
}; rm(list=c('tmp','alpha'))


false_positives_outcomes %>% 
  ggplot(aes(x=Alpha, y=CumProp, group=FP, color=FP)) +
  geom_line(size=2) +
  geom_point(data=subset(false_positives_outcomes, Observed), 
             aes(x=Alpha, y=CumProp), 
             color='black', size=2, inherit.aes=F) +
  geom_hline(yintercept=0.05, linetype=2) +
  geom_hline(yintercept=0.1, linetype=3) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  geom_abline(slope=1, intercept=0, linetype=4) +
  scale_color_brewer(type='div', palette='Spectral') +
  coord_fixed(ratio=1, xlim=c(0, 1), ylim=c(0, 1), expand=F) +
  theme_bw() +
  theme(text=element_text(size=14))
true_positives_outcomes = false_positives_outcomes %>%
  mutate(PlausibleFP=(CumProp>=Alpha)) %>% 
  group_by(Alpha) %>%
  group_modify(~ {
    max_plausible_FP = max(subset(.x, PlausibleFP)$FPinteger)
    all_P = subset(.x, Observed)$FPinteger
    min_TP = all_P - max_plausible_FP
    data.frame(MaxFP=max_plausible_FP,
               TotalP=all_P,
               MinTP=max(0, min_TP))
  })
true_positives_outcomes %>%
  ggplot(aes(x=Alpha, y=MinTP)) +
  geom_step(size=2) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  labs(x='Pr(False positives)',
       y='Minimum # of true positives') +
  theme_bw() +
  theme(text=element_text(size=14))
tp_plot_outcomes = true_positives_outcomes %>%
  subset(Alpha<=0.15) %>%
  ggplot(aes(x=Alpha, y=MinTP)) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(9, MinTP), ymax=pmax(10, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[10], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(8, MinTP), ymax=pmax(9, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[9], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(7, MinTP), ymax=pmax(8, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[8], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(6, MinTP), ymax=pmax(7, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[7], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(5, MinTP), ymax=pmax(6, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[6], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(4, MinTP), ymax=pmax(5, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[5], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(3, MinTP), ymax=pmax(4, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[4], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(2, MinTP), ymax=pmax(3, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[3], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(1, MinTP), ymax=pmax(2, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[2], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(0, MinTP), ymax=pmax(1, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[1], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(9, MinTP), ymax=pmin(10, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[10]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(8, MinTP), ymax=pmin(9, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[9]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(7, MinTP), ymax=pmin(8, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[8]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(6, MinTP), ymax=pmin(7, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[7]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(5, MinTP), ymax=pmin(6, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[6]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(4, MinTP), ymax=pmin(5, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[5]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(3, MinTP), ymax=pmin(4, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[4]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(2, MinTP), ymax=pmin(3, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[3]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(1, MinTP), ymax=pmin(2, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[2]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(0, MinTP), ymax=pmin(1, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[1]) +
  geom_text(data=data.frame(DV=names(observed_pvalues_outcomes),
                            Rank=rank(observed_pvalues_outcomes)-0.3),
            aes(label=DV, y=Rank), x=0.145, hjust=1) +
  geom_text(data=data.frame(pValue=paste0('p=', str_pad(sort(observed_pvalues_outcomes), 5, 'right', 0)),
                            Rank=seq(1:10)-0.7),
            aes(label=pValue, y=Rank), x=0.145, hjust=1) +
  geom_step(size=2) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  scale_x_continuous(breaks=seq(0, 0.14, 0.02)) +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  coord_cartesian(xlim=c(0, 0.15), ylim=c(0, 10), expand=F) +
  labs(x='Pr(False positives)',
       y='Minimum # of true positives') +
  theme_bw() +
  theme(text=element_text(size=14)); tp_plot_outcomes
ggsave(filename='figure-g1.png', 
       plot=tp_plot_outcomes, 
       width=8.5, height=4, units='in')
for(i in 1:max(true_positives_outcomes$MinTP)) {
  which(true_positives_outcomes$MinTP==i) %>%
    min %>%
    divide_by(1e4) %>%
    round(digits=3) %>%
    paste0('\n') %>%
    cat
}


false_positives_specifications %>% 
  ggplot(aes(x=Alpha, y=CumProp, group=FP, color=FP)) +
  geom_line(size=2) +
  geom_point(data=subset(false_positives_specifications, Observed), 
             aes(x=Alpha, y=CumProp), 
             color='black', size=2, inherit.aes=F) +
  geom_hline(yintercept=0.05, linetype=2) +
  geom_hline(yintercept=0.1, linetype=3) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  geom_abline(slope=1, intercept=0, linetype=4) +
  scale_color_brewer(type='div', palette='Spectral') +
  coord_fixed(ratio=1, xlim=c(0, 1), ylim=c(0, 1), expand=F) +
  theme_bw() +
  theme(text=element_text(size=14))
true_positives_specifications = false_positives_specifications %>%
  mutate(PlausibleFP=(CumProp>=Alpha)) %>%
  group_by(Alpha) %>%
  group_modify(~ {
    max_plausible_FP = max(subset(.x, PlausibleFP)$FPinteger)
    all_P = subset(.x, Observed)$FPinteger
    min_TP = all_P - max_plausible_FP
    data.frame(MaxFP=max_plausible_FP,
               TotalP=all_P,
               MinTP=max(0, min_TP))
  })
true_positives_specifications %>%
  ggplot(aes(x=Alpha, y=MinTP)) +
  geom_step(size=2) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  labs(x='Pr(False positives)',
       y='Minimum # of true positives') +
  theme_bw() +
  theme(text=element_text(size=14))
tp_plot_specifications = true_positives_specifications %>%
  subset(Alpha<=0.15) %>%
  ggplot(aes(x=Alpha, y=MinTP)) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(9, MinTP), ymax=pmax(10, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[10], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(8, MinTP), ymax=pmax(9, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[9], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(7, MinTP), ymax=pmax(8, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[8], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(6, MinTP), ymax=pmax(7, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[7], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(5, MinTP), ymax=pmax(6, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[6], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(4, MinTP), ymax=pmax(5, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[5], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(3, MinTP), ymax=pmax(4, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[4], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(2, MinTP), ymax=pmax(3, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[3], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(1, MinTP), ymax=pmax(2, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[2], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmax(0, MinTP), ymax=pmax(1, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[1], alpha=0.125) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(9, MinTP), ymax=pmin(10, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[10]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(8, MinTP), ymax=pmin(9, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[9]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(7, MinTP), ymax=pmin(8, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[8]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(6, MinTP), ymax=pmin(7, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[7]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(5, MinTP), ymax=pmin(6, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[6]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(4, MinTP), ymax=pmin(5, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[5]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(3, MinTP), ymax=pmin(4, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[4]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(2, MinTP), ymax=pmin(3, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[3]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(1, MinTP), ymax=pmin(2, MinTP)),
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[2]) +
  geom_ribbon(aes(x=Alpha, ymin=pmin(0, MinTP), ymax=pmin(1, MinTP)), 
              fill=RColorBrewer::brewer.pal(n=10, name='Spectral')[1]) +
  geom_text(data=data.frame(DV=names(observed_pvalues_specifications),
                            Rank=rank(observed_pvalues_specifications, ties.method='random')-0.3),
            aes(label=DV, y=Rank), x=0.135, hjust=1) +
  geom_text(data=data.frame(pValue=paste0('p=', str_pad(sort(observed_pvalues_specifications), 5, 'right', 0)),
                            Rank=seq(1:10)-0.7),
            aes(label=pValue, y=Rank), x=0.135, hjust=1) +
  geom_step(size=2) +
  geom_vline(xintercept=0.05, linetype=2) +
  geom_vline(xintercept=0.1, linetype=3) +
  scale_x_continuous(breaks=seq(0, 0.14, 0.02)) +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  coord_cartesian(xlim=c(0, 0.15), ylim=c(0, 10), expand=F) +
  labs(x='Pr(False positives)',
       y='Minimum # of true positives') +
  theme_bw() +
  theme(text=element_text(size=14)); tp_plot_specifications
ggsave(filename='figure-g2.png', 
       plot=tp_plot_specifications, 
       width=8.5, height=4, units='in')
for(i in 1:max(true_positives_specifications$MinTP)) {
 which(true_positives_specifications$MinTP==i) %>%
   min %>%
   divide_by(1e4) %>%
   round(digits=3) %>%
   paste0('\n') %>%
   cat
}
 
