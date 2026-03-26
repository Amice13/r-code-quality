################################################################
## Figure 2-4
################################################################

#####################################################################
## color map
################################################################

# set working directory
setwd("Replication_visits")
####### 1 create making visit network
load("./Data/makevisit.RData")
load("./Data/hostvisit.RData")

years <- seq(min(makevisit$year), max(makevisit$year), by =1)

###this is a list of matrix
makevisiList <-list()

for(i in 1:length(years)){
  
  slice <- makevisit[which(years[i]==makevisit$year),] 
  slice <- as.data.frame(slice)
  actors <- sort(unique(unlist(slice[c("isoa", "isob")])))
  smat = matrix(0, nrow=length(actors), ncol=length(actors), dimnames=list(actors, actors))
  # fill in the matrix with matrix indexing on row and column names
  smat[as.matrix(slice[c("isoa", "isob")])] <- slice[["makevisit_dum"]]
  
  #make it as symetric
  # smat[lower.tri(smat)] = t(smat)[lower.tri(smat)]
  #not define diag
  diag(smat) <- NA
  
  makevisiList[[i]]=smat
}
names(makevisiList)=years


# get a uniqu set of actors
actorSet <- sort(unique(unlist( lapply(makevisiList, rownames) )))


###### hosting a visit
###this is a list of matrix
hostvisitList <-list()

for(i in 1:length(years)){
  
  slice <- hostvisit[which(years[i]==hostvisit$year),] 
  slice <- as.data.frame(slice)
  actors <- sort(unique(unlist(slice[c("isoa", "isob")])))
  smat = matrix(0, nrow=length(actors), ncol=length(actors), dimnames=list(actors, actors))
  # fill in the matrix with matrix indexing on row and column names
  smat[as.matrix(slice[c("isoa", "isob")])] <- slice[["hostvisit_dum"]]
  
  #make it as symetric
  # smat[lower.tri(smat)] = t(smat)[lower.tri(smat)]
  #not define diag
  diag(smat) <- NA
  
  hostvisitList[[i]]=smat
}
names(hostvisitList)=years


library(cshapes)
#make a map legend
map2014=cshp(date=as.Date('2014-12-31'),useGW = F)
coords=coordinates(map2014)
dst=as.matrix(dist(coords, upper = TRUE, diag=TRUE))
xy=cmdscale(dst, k=3)
r=rank(xy[,1])/dim(xy)[1]
g=rank(xy[,2])/dim(xy)[1]
b=rank(xy[,3])/dim(xy)[1]
farben=rgb(g,r,0)
map2014$mapcolors=farben
library(dplyr)

# create color-cntry frame
netColors=data.frame(cbind(as.character(map2014$ISO1AL3), 
                           as.character(map2014$COWCODE),
                           map2014$mapcolors))
names(netColors) <- c("iso", "ccode", "color")

netColors$iso=actorSet[match(netColors$iso, actorSet)]
netColors = netColors %>% filter(!is.na(iso)) %>% arrange(iso)

pdf('figs/MapLegend.pdf', width = 5, height = 3)
par(mar=c(0,0,0,0), oma=c(0,0,0,0))
plot(map2014, col=farben, lwd=1e-200, main = "")
dev.off()


################################################################
## Figure 2-3
################################################################

##### all figures
library(igraph)
yrs <- seq(1979, 1988, by = 1)

# for loop to create matrix

for (i in seq_along(yrs)){
  p = graph_from_adjacency_matrix(makevisiList[[i]])
  node_name = V(p)$name
  node_colors <-  netColors[match(node_name,netColors$iso), 3]
  deg = degree(p, mode = "out")
  ## save the network
  pdf(file = paste0(paste("figs/makevisit_network", yrs[i], sep = "_"), ".pdf"))
  #bottom, left, top, right)
  par(mar=c(0,0,0,0))
  plot(p,layout=layout.fruchterman.reingold,
       vertex.color = node_colors, label.pos = 5,
       vertex.label=V(p)$name, vertex.size=deg,
       vertex.label.dist=1, vertex.label.cex=.7, 
       edge.arrow.size=.4, main = "")
  dev.off()
}          

## node size not weighted by out-degree

yrs <- seq(1989, 2020, by = 1)

# for loop to create matrix

for (i in seq_along(yrs)){
  p = graph_from_adjacency_matrix(makevisiList[[i]])
  node_name = V(p)$name
  node_colors <-  netColors[match(node_name,netColors$iso), 3]
  #deg = degree(p, mode = "out")
  ## save the network
  pdf(file = paste0(paste("figs/makevisit_network", yrs[i], sep = "_"), ".pdf"))
  par(mar=c(0,0,0,0))
  plot(p,layout=layout.fruchterman.reingold,
       vertex.color = node_colors, label.pos = 5,
       vertex.label=V(p)$name, vertex.size=5,
       vertex.label.dist=1, vertex.label.cex=.7, 
       edge.arrow.size=.4,main = "")
  dev.off()
}    




################################################################
## Figure 4
################################################################


########## new figure for edges and out-in-degree
years <- seq(min(makevisit$year), max(makevisit$year), by =1)

deg_all <- list()
deg_out <- list()
deg_in <- list()
for (i in seq_along(years)){
  p = graph_from_adjacency_matrix(makevisiList[[i]])
  deg_all[[i]] = degree(p, mode = "all")
  deg_out[[i]] = degree(p, mode = "out")
  deg_in[[i]] = degree(p, mode = "in")
  }
names(deg_all) <- years
names(deg_out) <- years
names(deg_in) <- years
library(purrr)

library(dplyr)
deg_all_df <- map(deg_all, data.frame) %>% 
                  map2_df(., names(.), ~mutate(.x, 
                                           year = .y,
                                           iso = rownames(.))) %>% 
            rename(degree = `.x..i..`)

deg_out_df <- map(deg_out, data.frame) %>% 
  map2_df(., names(.), ~mutate(.x, 
                               year = .y,
                               iso = rownames(.))) %>% 
  rename(outdegree = `.x..i..`)

deg_in_df <- map(deg_in, data.frame) %>% 
  map2_df(., names(.), ~mutate(.x, 
                               year = .y,
                               iso = rownames(.))) %>% 
  rename(indegree = `.x..i..`)

degree_df <- left_join(deg_all_df, deg_out_df, by = c("iso", "year"))
degree_df <- left_join(degree_df, deg_in_df, by = c("iso", "year"))
save(degree_df, file = "Data/degree_df.RData")

library(showtext)
library(ggthemes)
library(scales)
library(extrafont)
showtext_auto(enable = TRUE)
font_add('KaiTi', 'KaiTi.ttf')

load("Data/degree_df.RData")
degree_df_agg <- degree_df %>% 
      group_by(year) %>% 
      mutate(year = as.integer(year)) %>% 
      summarise(degree = sum(degree),
                indegree = sum(indegree),
                outdegree = sum(outdegree),
                countries = n()) %>% 
  ungroup()

library(ggplot2)
library(ggthemes)
library(scales)
names(pdfFonts())
#0 = blank, 1 = solid, 2 = dashed, 3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash
p1 <- ggplot(degree_df_agg, aes(x = year)) +
  # Data
  geom_line(aes(y = outdegree), colour = "red", linetype = 5, size = 1)  +
  scale_x_continuous(breaks = seq(1979, 2020, 5)) +
  # Aesthetics
  labs(title = "出度 (所有国家)", x = "年",
       y = "") +
  theme_bw() + theme(text = element_text(size=14,family ='KaiTi'))

states <- degree_df %>% filter(iso == "USA"| iso== "CHN" | iso == "RUS")%>%
  mutate(year = as.integer(year)) 

#states$iso <- factor(states$iso, labels = c("中国","苏联/俄罗斯","美国"))
p2 <- ggplot(states, aes(x = year, y = outdegree,
                   group = iso, linetype = iso)) + geom_line() + 
  scale_x_continuous(breaks = seq(1979, 2020, 5)) +
  # Aesthetics
  labs(title = "出度 (部分国家)", x = "年",
       y = "") +
  scale_linetype(name = "")+
  theme_bw()+theme(text = element_text(size=14,family ='KaiTi'))

p3 <- ggplot(states, aes(x = year, y = indegree,
                   group = iso, linetype = iso)) + geom_line() + 
  scale_x_continuous(breaks = seq(1979, 2020, 5)) +
  # Aesthetics
  labs(title = "入度 (部分国家)", x = "年",
       y = "") +
  scale_linetype(name = "")+
  theme_bw()+theme(text = element_text(size=14,family ='KaiTi'))

library(grid)
library(gridExtra)
pdf("figs/degrees.pdf",height =6, width = 11)
grid.arrange(p1, p2, p3)
dev.off()


