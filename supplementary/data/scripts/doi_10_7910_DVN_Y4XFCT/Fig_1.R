rm(list = ls())
options(scipen=999)
library(data.table)
library(ggplot2)
library(ggthemes)
library(here)

load(here('data','underspending_table.rdata'))

underspending <- ggplot(data=table_delta[description == 'All candidates' & !is.na(value)], 
                     aes(x = reorder(sigla_partido_hoje, value), y=value/1000)) +
                geom_bar(stat="identity", position=position_dodge()) +
                theme_minimal(base_size = 12) +
                scale_fill_manual(values=c("grey", "black")) +
                theme(legend.position = "top") +
                theme(legend.title=element_blank()) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                #scale_x_discrete(guide = guide_axis(n.dodge=3)) +
                xlab("") +
                ylab('Median underspending\n(in thousands of reais)')

# ggsave(here('img','Fig_1.pdf'), plot = plot.candidates, device = 'pdf',height = 13, width = 13, units = 'cm') 
