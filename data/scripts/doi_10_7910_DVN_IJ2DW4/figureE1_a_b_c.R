rm(list = ls())

# set working directory to YOUR PATH TO REPLICATION MATERIALS
setwd('~/PATH TO REPLICATION MATERIALS/gz_replication_materials/')

# load packages
library(data.table)
library(ggplot2)
library(sf)

# load data
candidates <- readRDS('./data/candidates/minority_candidates_2010_2019.RDS')
wm_boundaries <- st_read('./data/shapefiles/Westminster_Parliamentary_Constituencies_2017/Westminster_Parliamentary_Constituencies__December_2017__UK_BSC_V2.shp')

# in-text numbers
candidates[grepl('^(E|W)',ons_id),.N]
candidates[grepl('^(E|W)',ons_id), length(unique(ons_id))]



### Figure E1a. Number of ethnic minority candidates standing for Parliament across elections by election result (won/lost)
candidates[, win:=ifelse(victory_margin>0,1,0)]
tbl <- with(candidates[grepl('^(E|W)',ons_id)], table(win, election))

pdf('./output/figures/figureE1a.pdf', width=6, height=4.85)

opar = par(oma = c(2,0,0,0))
barplot(tbl, beside = TRUE, col = grey.colors(2), cex.axis=1.5, cex.names=1.5)
par(opar) # Reset par
opar = par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE, cex = 1.5)
legend(x = "bottom", legend = c('lost', 'won'), bty = "n", ncol = 2, inset = -0.15, fill = grey.colors(2))
par(opar) # reset par

dev.off()

### Figure E1b. Number of strongest ethnic minority candidates by party and election result (won/lost)
candidates[, max_share:= max(share), by=.(election, ons_id)]

ggplot(candidates[grepl('^(E|W)',ons_id) & max_share==share, .(N =.N), by=.(party_name, win)]) + 
  aes(x=party_name, y=N, fill=as.factor(win)) +
  geom_col(position='dodge') +
  xlab(NULL) + ylab(NULL) +
  scale_y_continuous(breaks = seq(0,150,25)) +
  scale_fill_grey(start=0.3, end=0.8, name= NULL, labels = c('lost', 'won')) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=11),
        axis.text.y = element_text(size=12),
        legend.text=element_text(size=12),
        #legend.position="bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave('./output/figures/figureE1b.pdf', width=6, height=4.85)

# in-text numbers
candidates[grepl('^(E|W)',ons_id) & max_share==share, .N]
candidates[grepl('^(E|W)',ons_id) & max_share==share & victory_margin>0, .N]/candidates[grepl('^(E|W)',ons_id) & max_share==share, .N]
candidates[grepl('^(E|W)',ons_id) & max_share==share, length(unique(ons_id))]/(533+40) # note 533 constituencies in England and 40 in Wales

### Figure E1c. Geographical coverage of strongest ethnic minority candidates across four elections
plot_data <- merge(wm_boundaries[grepl('^(E|W)',wm_boundaries$PCON17CD), ],candidates[grepl('^(E|W)',ons_id) & max_share==share, .(
  N =ifelse(.N>0,1,0) ), by=.(ons_id)], by.x='PCON17CD', by.y='ons_id', all.x=TRUE)

ggplot(data = plot_data) + 
  geom_sf(aes(fill = N), lwd=0.005) +
  scale_fill_continuous(high = '#154360', low = '#a9cce3') +
  theme(legend.position = 'none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.text = element_blank(), axis.ticks = element_blank())
ggsave('./output/figures/figureE1c.pdf', width=6, height=4.85)
