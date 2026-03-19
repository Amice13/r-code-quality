require(ggplot2)
require(doBy)
require(fiftystater)

load('executive-actions-subset.Rda')
load('executive-action-coverage.Rda')
load('state-xw.Rda')

# Results In-Text: Proportion of Actions Covered by President
summaryBy(coverage~president,actions)
# -----------------------------------------------

# Case 1: Bush era steel tariffs

# find all lowande.uids that mention steel tariffs
steel.orders = actions$lowande.uid[grepl('steel',actions$title,ignore.case = T) & actions$president=='Bush']

# subset articles
bush.steel = articles_coverage_final_dates[articles_coverage_final_dates$lowande.uid%in%steel.orders,]

# summarize articles by location
bush.steel$mentions = 1

# state
x=summaryBy(mentions~pub_state,FUN=sum,bush.steel,keep.names=T)

# have to account for the number of publications per state
xw$n_pubs=1
y=summaryBy(n_pubs~pub_state,FUN=sum,xw,keep.names=T)
z=merge(x,y,by='pub_state',all=T)
z$mentions[is.na(z$mentions)]=0
z$mentions_per_pub=z$mentions/z$n_pubs

z = merge(z,unique(xw[,c('pub_state','id')]),by='pub_state',all.x=T)

# Figure 6.1a: What you read depends on where you live. 
BUSH = ggplot() + geom_map(data = z, aes(map_id = id, fill = log(mentions_per_pub+1)),map = fifty_states) +
  geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group), size = 0.25, colour='black', fill=NA) + expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  scale_fill_gradient(low='#d3d3d3',high='#000000') +
  theme_map() + theme(legend.position = 'none') +
  fifty_states_inset_boxes()
# -----------------------------------------------

# Case 2: Obama DACA
DACA = articles_coverage_final_dates[articles_coverage_final_dates$lowande.uid=='lr0001',]

# summarize articles by location
DACA$mentions = 1

# state
x=summaryBy(mentions~pub_state,FUN=sum,DACA,keep.names=T)

# have to account for the number of publications per state
xw$n_pubs=1
y=summaryBy(n_pubs~pub_state,FUN=sum,xw,keep.names=T)
z=merge(x,y,by='pub_state',all=T)
z$mentions[is.na(z$mentions)]=0
z$mentions_per_pub=z$mentions/z$n_pubs

z = merge(z,unique(xw[,c('pub_state','id')]),by='pub_state',all.x=T)

# Figure 6.1b: What you read depends on where you live. 
OBAMA = ggplot() + geom_map(data = z, aes(map_id = id, fill = log(mentions_per_pub+1)),map = fifty_states) +
  geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group), size = 0.25, colour='black', fill=NA) + expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  scale_fill_gradient(low='#d3d3d3',high='#000000') +
  theme_map() + theme(legend.position = 'none') +
  fifty_states_inset_boxes()
# -----------------------------------------------

# Case 3: Trump and HBCUs
HBC = articles_coverage_final_dates[articles_coverage_final_dates$lowande.uid=='gp0680',]

# summarize articles by location
HBC$mentions = 1

# state
x=summaryBy(mentions~pub_state,FUN=sum,HBC,keep.names=T)

# have to account for the number of publications per state
xw$n_pubs=1
y=summaryBy(n_pubs~pub_state,FUN=sum,xw,keep.names=T)
z=merge(x,y,by='pub_state',all=T)
z$mentions[is.na(z$mentions)]=0
z$mentions_per_pub=z$mentions/z$n_pubs

z = merge(z,unique(xw[,c('pub_state','id')]),by='pub_state',all.x=T)

# Figure 6.1c: What you read depends on where you live. 
TRUMP=ggplot() + geom_map(data = z, aes(map_id = id, fill = log(mentions_per_pub+1)),map = fifty_states) +
  geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group), size = 0.25, colour='black', fill=NA) + expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  scale_fill_gradient(low='#d3d3d3',high='#000000') +
  theme_map() + theme(legend.position = 'none') +
  fifty_states_inset_boxes()
# -----------------------------------------------

# Case 4: Clinton's National Monuments

monument.orders = actions$lowande.uid[grepl('monument',actions$title,ignore.case = T) & actions$president=='Clinton']

FOREST = articles_coverage_final_dates[articles_coverage_final_dates$lowande.uid%in%monument.orders,]

# summarize articles by location
FOREST$mentions = 1

# state
x=summaryBy(mentions~pub_state,FUN=sum,FOREST,keep.names=T)

# have to account for the number of publications per state
xw$n_pubs=1
y=summaryBy(n_pubs~pub_state,FUN=sum,xw,keep.names=T)
z=merge(x,y,by='pub_state',all=T)
z$mentions[is.na(z$mentions)]=0
z$mentions_per_pub=z$mentions/z$n_pubs

z = merge(z,unique(xw[,c('pub_state','id')]),by='pub_state',all.x=T)

# Figure 6.1d: What you read depends on where you live. 
CLINTON=ggplot() + geom_map(data = z, aes(map_id = id, fill = log(mentions_per_pub+1)),map = fifty_states) +
  geom_polygon(data=fifty_states, aes(x=long, y=lat, group = group), size = 0.25, colour='black', fill=NA) + expand_limits(x = fifty_states$long, y = fifty_states$lat) + 
  scale_fill_gradient(low='#d3d3d3',high='#000000') +
  theme_map() + theme(legend.position = 'none') +
  fifty_states_inset_boxes()
# -----------------------------------------------