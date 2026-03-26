rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# *NOTE*: install rdrobust version 2.0.3 as indicated below
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_2.0.3.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

## load required packages
library(data.table)
library(ggplot2)
library(rdrobust)
library(stringr)

## load data
data <- readRDS('./data/campaigns/campaign_issues.RDS')

## Figure H1a: Campaign issue coverage 
# count issues per campaign in all and close elections
data_long <- melt(data, id.vars = c('election', 'constituency_code', 'minority_victory_margin'), 
                  measure.vars = patterns(issue = 'issue_'))
data_long[,num_topics:=sum(issue), by=.(election, constituency_code)]
data_long[,variable:=sub('.*_', '', variable)]
data_long[variable=='brexit', variable:='europe/brexit']

data_all <- data_long[num_topics>0, .(all = sum(issue)/data_long[,sum(issue)]), by=.(variable)]
data_close <- data_long[((abs(minority_victory_margin) <= 22) & num_topics>0), .(close = sum(issue)/data_long[((abs(minority_victory_margin) <= 22) & num_topics>0), sum(issue)]), by=.(variable)]

data_plot <- merge(data_all, data_close, by='variable')
colnames(data_plot) <- c("variable", "All", "Close")

ggplot(melt(data_plot, id.vars = 'variable'), aes(fill=variable, y=value, x=variable.1)) + 
  geom_bar(position="fill", stat="identity") + ylab('Proportion') + xlab(NULL) +
  scale_fill_grey(start = 0.2, end = 0.8) +
  theme(legend.title= element_blank(),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size =14),
        axis.text.y = element_text(size = 12),
        legend.text=element_text(size=12))
ggsave(file = './output/figures/figureH1a.pdf', width=6, height=4.85)

## Figure H1b: RD estimates on campaign issues
## Figure H1c: p-values of RD estimates on campaign issues
# RD estimates and p-values
data_long <- melt(data, id.vars = c('election', 'constituency_code', 'minority_victory_margin'), 
                  measure.vars = patterns(issue = 'issue_'))
data_long[,variable:=sub('.*_', '', variable)]
data <- merge(data, data_long[,.(num_topics=sum(issue)), by=.(election, constituency_code)], by=c('election','constituency_code'))
baseline <- colnames(data)[grep('^issue_', colnames(data))]
data[ , (baseline) := lapply(.SD, "/", num_topics), .SDcols = baseline]

baseline_pv <- c()
normalized_effect <- c()
coef <- c()
bandwidth <- c()
effect_percent <- c()

for (i in baseline) {
  rdout <- rdrobust(data[num_topics>0, get(i)],
                    data[num_topics>0, minority_victory_margin], kernel = "triangular", p = 1)
  
  baseline_pv <- c(baseline_pv, rdout$pv[3])
  coef <- c(coef, rdout$coef[1])
  sd <- data[((minority_victory_margin >= -rdout$bws[1,1] & minority_victory_margin < 0) & num_topics>0), sd(get(i))]
  normalized_effect <- c(normalized_effect, (rdout$coef[1]/sd))
  effect_percent <- c(effect_percent, rdout$coef[1]/rdout$beta_p_l[1])
  bandwidth <- c(bandwidth, rdout$bws[1,1])
  
}

baseline_labels <- sub('.*_', '', baseline)
data_graph <- data.frame('baseline'=baseline_labels, 'p_values'=baseline_pv,
                         'percent_difference' = effect_percent,
                         'coef'=coef,
                         'normalized_effect'= normalized_effect, 'bandwidth'=bandwidth)
data_graph <- data.table(data_graph)
data_graph <- data_graph[order(p_values)]
data_graph[baseline=='brexit', baseline:='europe/brexit']
data_graph[, baseline:=str_to_title(baseline)]

ggplot(data_graph) +
  aes(x=coef, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=1.5) +
  geom_vline(xintercept = 0,
             linetype='longdash', colour='#666666') +
  scale_x_continuous(breaks = seq(-0.05,0.05,0.01)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=12),
        axis.title.x = element_text(size =12)) +
  xlab('Percent difference in campaign issue coverage') +
  ylab(NULL)
ggsave('./output/figures/figureH1b.pdf', width=6.2, height=4.85)

ggplot(data_graph) +
  aes(x=p_values, y=reorder(baseline,-p_values)) +
  geom_point(colour='#0000FF', size=1.5) +
  geom_vline(xintercept = 0.05,
             linetype='longdash', colour='#666666') +
  scale_x_continuous(breaks = seq(0,1,0.1)) +
  theme(plot.title = element_text(size=14, lineheight=3),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=10), axis.text.y = element_text(size=12),
        axis.title.x = element_text(size =12)) +
  xlab('P-value of test for continuity of means around cutoff') +
  ylab(NULL)
ggsave('./output/figures/figureH1c.pdf', width=6.2, height=4.85)
