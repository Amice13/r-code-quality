df <- read.csv('mpx_tested_mpx_positives_2007_2018.csv', header=TRUE, sep=',')
head(df)
require(ggplot2)
require(reshape2)
monthLevels <- df$mon.Yr

df_long <- reshape(df,
                   varying = c('flu_positive','piv_positive','corona_positive','rsv_positive','rhino_positive'),
                   v.names = "prevalence",
                   timevar = "virus",
                   times = c('flu_positive','piv_positive','corona_positive','rsv_positive','rhino_positive'),
                   direction = "long")

head(df_long)
hrv_df <- df_long[df_long$virus == 'rhino_positive',]
hrv_df$prop <- hrv_df$prevalence / hrv_df$mpx_tested
rsv_df <- df_long[df_long$virus == 'rsv_positive',]
rsv_df$prop <- rsv_df$prevalence / rsv_df$mpx_tested
head(rsv_df)
head(hrv_df)

hrv_rsv <- rbind(hrv_df, rsv_df)


ggplot(hrv_rsv, aes(x = factor(mon.Yr, levels = monthLevels), y = prop*100)) +
  geom_line(aes(color = virus, group = virus), size = 1) +
  labs(y = 'Proportion (%)\n', x = '\nYear') +
  scale_color_discrete(name = '', labels = c('HRV','RSV')) +
  guides(group = NULL) +
  scale_x_discrete(breaks = levels(as.factor(df_long$mon.Yr))[c(F, rep(T, 1))]) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title = element_text(face = 'bold', size = 12),
        legend.position = c(0.9,0.9),
        legend.text = element_text(face = 'bold'))

