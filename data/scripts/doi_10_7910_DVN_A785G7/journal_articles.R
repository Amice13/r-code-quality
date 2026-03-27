# Replication materials for "Messy Data, Robust Inference? Navigating Obstancles to Inference with bigKRLS"
# By: Pete Mohanty (pmohanty@stanford.edu) and Robert Shaffer (rbshaffer@utexas.edu)

require(pacman)
p_load(ggplot2, scales, data.table)

data <- read.csv('journal_articles.csv')
data <- data[data$type != 'Non-Empirical',]
data$type <- ifelse(data$type == 'Experimental', 'Experimental', 'Observational')


gg <- data.table(data)[,list(x=density(log(n), na.rm=T)$x, y=density(log(n), na.rm=T)$y), by='type']
gg_sub <- subset(gg, x >= log(3500) & x <= log(15000))


# 6x3.5 figure size output
ggplot(data) + geom_line(aes(x=n, linetype=type), stat='density', alpha=.5) + coord_trans(x='log') +
  geom_vline(aes(xintercept=3500), size=1, linetype='dotted') + 
  geom_vline(aes(xintercept=15000), size=1, linetype='dotted') + 
  geom_ribbon(data=subset(gg_sub, type=='Experimental'),
              aes(x=exp(x),ymax=y),ymin=0,fill='grey20', alpha=0.5)+
  geom_ribbon(data=subset(gg_sub, type=='Observational'),
              aes(x=exp(x),ymax=y),ymin=0,fill='grey10', alpha=0.5) + 
  xlab('Sample Size') + 
  ylab('Density') + 
  scale_x_continuous(trans = log_trans(), breaks = c(10, 100, 1000, 10000, 100000), labels=comma) + 
  labs(linetype='Dataset Type') + 
  theme_bw() + 
  theme(panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank())
