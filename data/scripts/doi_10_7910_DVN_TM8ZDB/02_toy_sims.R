### With thanks to:
## https://www.r-bloggers.com/2021/02/some-computational-redistricting-methods-or-how-to-sniff-out-a-gerrymander-in-a-pinch/

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#devtools::install_github('alarm-redist/redist@dev')
library(tidyverse)
library(sf)
library(sp)

set.seed(10012)

### First things first: make a data set

dat = data.frame(
  precinct = c("A1", "A2", "A3", "A4", "A5",
                "B1", "B2", "B3", "B4",
                "C1", "C2", "C3", "C4",
                "D1", "D2", "D3",
                "E1", "E2", "E3", "E4"),
  voteshare = c(0.5, 0.4, 0.3, 0.3, 0.25,
                0.85, 0.95, 0.45, 0.4,
                0.45, 0.55, 0.4, 0.35,
                0.4, 0.15, 0.3,
                0.65, 0.1, 0.2, 0.05),
  firms = c(4, 5, 5, 6, 5,
            5, 6, 9, 4,
            4, 12, 11, 4,
            2, 4, 6,
            1, 0, 3, 4),
  NW_x = c(0:4,
           0:3,
           0:3,
           0:2,
           0:3),
  NW_y = c(rep(5,5),
           rep(4,4),
           rep(3,4),
           rep(2,3),
           rep(1,4))
)

dat = dat %>% mutate(NE_x = NW_x + 1,
                     NE_y = NW_y,
                     zSE_x = NE_x,
                     zSE_y = NE_y - 1,
                     SW_x = NW_x, 
                     SW_y = NW_y - 1,
                     centroid_x = (NE_x+ NW_x)/2,
                     centroid_y = (NE_y+ zSE_y)/2,
                     pop = 100)
row.names(dat) = dat$precinct

### Now let's convert this to spatial polygons
dat2 = dat %>% reshape2::melt(id.vars = c("precinct", "voteshare", "pop",
                                            "firms", "centroid_x", "centroid_y")) %>%
  separate(variable, into = c("corner", "coord"), sep="_") %>%
  reshape2::dcast(precinct + voteshare + firms + centroid_x + centroid_y +
                    pop + corner ~ coord)

## sp step
dat_sp = lapply(X = unique(dat2$precinct),
                FUN = function(X) Polygon(coords = cbind(dat2$x[dat2$precinct == X],
                                                         dat2$y[dat2$precinct == X]))
                )
dat_sp = lapply(1:20, FUN = function(x) Polygons(list(dat_sp[[x]]),
                                                 ID=dat$precinct[x]))
dat_sp = SpatialPolygons(dat_sp)
dat_sp = SpatialPolygonsDataFrame(Sr = dat_sp,
                                  data = dat[,c(1,2,3,12,13,14)])
plot(dat_sp)

dat_sf = sf::st_as_sf(dat_sp)

gdata::keep(dat_sf, sure=T)


## Next, make the base plots

ggplot(dat_sf) +
  geom_sf(aes(fill = firms)) +
  scale_fill_gradient(low = "white", high = "darkgreen")+ 
  theme_bw()+ 
  annotate("text", x = dat_sf$centroid_x, y = dat_sf$centroid_y,
           label = dat_sf$firms, size=12) +
  annotate("text", x = -0.5, y = c(0.5, 1.5, 2.5, 3.5, 4.5),
           label = c("E", "D", "C", "B", "A"), size=12) +
  annotate("text", y = 5.5, x = c(0.5, 1.5, 2.5, 3.5, 4.5),
           label = c("1", "2", "3", "4", "5"), size=12) +
  ylab("") + xlab("") + theme(legend.position = "none",
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.border = element_blank())
ggsave("../output/figures/fig1a_right.png")

ggplot(dat_sf) +
  geom_sf(aes(fill = voteshare)) +
  scale_fill_gradient(low = "red", high = "blue") + 
  theme_bw() + 
  annotate("text", x = dat_sf$centroid_x, y = dat_sf$centroid_y,
           label = paste0(dat_sf$voteshare*100, "%"), size=10, color = "white") +
  annotate("text", x = -0.5, y = c(0.5, 1.5, 2.5, 3.5, 4.5),
           label = c("E", "D", "C", "B", "A"), size=12) +
  annotate("text", y = 5.5, x = c(0.5, 1.5, 2.5, 3.5, 4.5),
           label = c("1", "2", "3", "4", "5"), size=12) +
  ylab("") + xlab("") + theme(legend.position = "none",
                              panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              axis.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.border = element_blank())
ggsave("../output/figures/fig1a_left.png")


### Now to start the simulations!

## Make the adjacency matrix
adj  <- redist::redist.adjacency(dat_sf)
cont <- geomander::check_contiguity(adjacency = adj, group = rep(1, nrow(dat_sf)))
max(cont$component)

## Draw some plans
set.seed(1)
smch <- redist::redist.smc(adj = adj,
                           total_pop = dat_sf$pop,
                           nsims = 3000,
                           ndists = 5, 
                           pop_tol = 0.001,
                           compactness=0)  

## Map a couple of plans

ensembles <- smch$plans %>% 
  data.table::data.table() %>%
  mutate(precinct = c("A1", "A2", "A3", "A4", "A5",
                      "B1", "B2", "B3", "B4",
                      "C1", "C2", "C3", "C4",
                      "D1", "D2", "D3",
                      "E1", "E2", "E3", "E4")) %>%
  data.table::melt('precinct')
colnames(ensembles)[2:3] = c('partition', 'district')

set.seed(99)
samps = sample(unique(ensembles$partition), 4)


returns = dat_sf %>% select(precinct, voteshare, firms) %>%data.table::as.data.table()

ps = unique(ensembles$partition)
fens = list()

for (q in 1:length(unique(ensembles$partition))) {
  x <- subset(ensembles, partition == ps[q])
  x1 <- x[returns, on = 'precinct']
  tmp <- x1[ , list(voteshare = mean(voteshare),
                    firms = sum(firms)),
             by = list(precinct, partition, district)] 
  fens[[q]] <- tmp
  print(q)
}


group_one = function(x){
  tmp = x %>% group_by(district) %>%
    summarize(name = paste(precinct, collapse="_"))
  tmp2 = sort(tmp$name)
  return(tmp2)
}

fens2 = lapply(fens, group_one)
idx = !duplicated(fens2)


x2 <- fens[idx] %>% data.table::rbindlist()


comp <- x2 %>%
  group_by(partition, district) %>%
  summarize(voteshare = mean(voteshare),
            firms = sum(firms),
        margin = abs(voteshare - 0.5),
         party = ifelse(voteshare > 0.5, 'D', 'R'))

comp2 = comp %>%
  group_by(partition) %>%
  summarize(seats = sum(party=='D'),
            firms = sum(firms[party=="D"])) 



summary(comp2$firms)
hist(comp2$firms)


p2 = comp2 %>%
  mutate(seats = seats,
         firms = firms/sum(dat_sf$firms)) %>%
  ggplot(aes(x = firms, y = as.factor(seats))) + 
  ggridges::geom_density_ridges(bandwidth = 0.025) + 
  theme_bw() + xlab("Firms controlled by Dems") + 
  xlim(c(0.1,1)) +
  ylab("Seats controlled by Dems") +
  ggtitle("Toy State") 
p2



p3 =comp2 %>% 
  mutate(seats = seats,
         firms = firms) %>%
  ggplot() +
  geom_histogram(aes(firms),binwidth = 1,boundary=1) +
  ggthemes::scale_fill_economist()+
  theme_minimal()+
  theme(legend.position = 'none',
        panel.grid.minor.y = element_blank() )  + ylab("") +
  xlab("Dem firms")
p3


p4 = ggplot() +
  geom_bar(data = comp2 %>%
             mutate(seats = factor(seats, levels = c("0", "1", "2", "3", "4", "5"))),
           aes(x = seats),# position = position_nudge(x=-0.5),
           width = 1, drop=FALSE) + 
  scale_x_discrete(labels = c("0D/5R","1D/4R", "2D/3R", "3D/2R", "4D/1R", "5D/0R"),
                   drop=FALSE) + 
  theme_minimal()+
  theme(legend.position = 'none',
        panel.grid.minor.y = element_blank())  + xlab("") + ylab("")
p4

tmp1=cowplot::plot_grid(p2, p4, ncol=2, align = "hv", rel_widths = c(4,1))
tmp1


tmp = cowplot::align_plots(p2, p3, align = "hv")


gs = list(tmp1, tmp[[2]])

hlay <- rbind(c(1,1,1,1,1),
              c(1,1,1,1,1),
              c(1,1,1,1,1),
              c(2,2,2,2,NA))

select_grobs <- function(lay) {
  id <- unique(c(t(lay))) 
  id[!is.na(id)]
} 
gridExtra::grid.arrange(grobs=gs[select_grobs(hlay)], layout_matrix=hlay)




maxfirm = sum(dat_sf$firms)
maxdist = 5
statename = "Toy State"


comp2 %>% ggplot(aes(x = firms, y=seats+1)) +
  geom_bin2d(bins=c(maxfirm,maxdist+1)) +
  scale_fill_gradient(low="lightblue1",high="darkblue",trans="log10") +
  scale_y_continuous(breaks=c(0:maxdist)+0.5,
                     labels=c(0:maxdist),
                     limits=c(0,maxdist+1),
                     expand = c(0,0))+
  scale_x_continuous(breaks=floor(seq(0,maxfirm, length.out = maxdist+1)),
                     limits=c(0,maxfirm),
                     expand=c(0,0))+
  geom_vline(xintercept = maxfirm/2, lwd=0.5, col="darkgrey") + 
  geom_hline(yintercept = ((maxdist+1)/2), lwd=0.5, col="darkgrey") + 
  #geom_abline(intercept=0.5,
  #            slope = (maxdist+1)/maxfirm,
  #            lwd=0.5, col="darkgrey")+
  labs(title = statename,
       x = "Dem Firms",
       y = "Dem Seats",
       fill = "Simulated Plans") +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()) 



summstats = comp2 %>% group_by(seats) %>%
                summarize(maxfirms = max(firms),
                minfirms = min(firms),
                medfirms = median(firms))

samps = list(comp2$partition[comp2$seats == 1 & comp2$firms == summstats$minfirms[1]][1],
             comp2$partition[comp2$seats == 1 & comp2$firms == summstats$medfirms[1]][1],
             comp2$partition[comp2$seats == 1 & comp2$firms == summstats$maxfirms[1]][1],
             comp2$partition[comp2$seats == 2 & comp2$firms == summstats$minfirms[2]][1],
             comp2$partition[comp2$seats == 2 & comp2$firms == summstats$medfirms[2]][1],
             comp2$partition[comp2$seats == 2 & comp2$firms == summstats$maxfirms[2]][1],
             comp2$partition[comp2$seats == 3 & comp2$firms == summstats$maxfirms[3]][1])

firmcount = c(summstats[1,3], summstats[1,4],summstats[1,2],
              summstats[2,3], summstats[2,4], summstats[2,2],
              summstats[3,4])
samp_plots = list()
for(i in 1:7){
  
  p = x2 %>% 
    filter(partition == samps[[i]]) %>%
    left_join(dat_sf, by='precinct') %>%
    group_by(district) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    left_join(comp %>% filter(partition == samps[[i]]), by = 'district') %>%
    
    ggplot(aes(geometry = geometry)) + 
    geom_sf(aes(fill = party),
            color = 'white',
            alpha = .65,
            lwd = .3) +
    scale_fill_manual(values = c('blue', 'red')) +
    geom_sf_text(aes(label = district),
                 color = 'black',
                 size = 2.5, 
                 check_overlap = TRUE) +
    theme_minimal() + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = 'none',
          panel.grid = element_blank()) +
    xlab(paste0("Firms: ", firmcount[i])) + ylab('')
  
  samp_plots[[i]] = p
}
cowplot::plot_grid(samp_plots[[1]], samp_plots[[2]], samp_plots[[3]], 
                   samp_plots[[4]], samp_plots[[5]], samp_plots[[6]],
                   samp_plots[[7]], 
                   nrow=3, ncol=3, byrow = TRUE)



### plotting every map
samp_plots2 = list()
u = unique(x2$partition)

for(i in 1:100){
  
  p = x2 %>% 
    filter(partition == u[i]) %>%
    left_join(dat_sf, by='precinct') %>%
    group_by(district) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup() %>%
    left_join(comp %>% filter(partition == u[i]), by = 'district') %>%
    
    ggplot(aes(geometry = geometry)) + 
    geom_sf(aes(fill = party),
            color = 'white',
            alpha = .65,
            lwd = .3) +
    scale_fill_manual(values = c('blue', 'red')) +
    geom_sf_text(aes(label = district),
                 color = 'black',
                 size = 2.5, 
                 check_overlap = TRUE) +
    theme_minimal() + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          legend.position = 'none',
          panel.grid = element_blank()) +
    xlab('') + ylab('')
  samp_plots2[[i]] = p
  print(i)
}

# Four pages of plots in one PDF file
pdf("allmaps.pdf", 7, 5, onefile=TRUE)
for (i in 1:length(samp_plots2)) {
  plot(samp_plots2[[i]])
  print(i)
}
dev.off()
