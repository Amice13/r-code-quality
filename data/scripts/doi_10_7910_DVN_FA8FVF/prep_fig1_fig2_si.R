print("Running prep_fig1_fig2_si.R...")

library(tidyverse)

set.seed(02138)

f = list.files("../data/Shiny Data/shiny_results3", full.names = TRUE)

read1 = function(x){
  temp = read.csv(x)
  temp2 = data.frame(ID = rep(temp$x[1], 20), a = temp$x[2:21], b=temp$x[22:41], choice = temp$x[42:61])
  return(temp2)
}

l = lapply(f, read1)
dat = do.call(rbind, l)

# Identify and Drop the User IDs who just selected District 1 or District 2 all the way through.
# This is probably just lazy responses. It's going to affect the dimensions of your combos going down
# since we have fewer responses now

idrops <- dat %>% dplyr::group_by(ID) %>% dplyr::summarize(same = sum(choice == "District 1"))
drop.ids <- idrops$ID[idrops$same %in% c(0,20)]
dat <- dat[!(dat$ID %in% drop.ids),]


########################################################################################################################
# Calculate Intercoder Reliability
########################################################################################################################


rows = matrix(NA, nrow = length(unique(dat$ID)), ncol=20)
for(i in 1:length(unique(dat$ID))){
  start = (i-1)*20 + 1
  stop = (i)*20
  print(c(start, stop))
  rows[i,] = dat$choice[start:stop]
}

gr = expand.grid(1:length(unique(dat$ID)), 1:length(unique(dat$ID)))
gr = gr[gr$Var1 != gr$Var2,]
agrees = sapply(1:nrow(gr), FUN=function(x) mean(rows[gr$Var1[x],] == rows[gr$Var2[x],]))
placebo =  sapply(1:nrow(gr), FUN=function(x) mean(rbinom(20, 1, .5) == rbinom(20, 1, .5)))

pf = data.frame(val = c(agrees, placebo), Legend=c(rep("Truth", nrow(gr)), rep("Random", nrow(gr))))
pf$Legend = as.character(pf$Legend)

load("../results/fig3_data.RData")
fr = unique(c(cor(time1)))[-1]
fr = data.frame(val=fr, Legend="Full Ranks")

pf = rbind(pf, fr)

save(pf, file="../results/fig1_si.RData")





########################################################################################################################
# Calculate Intracoder Reliability
########################################################################################################################

f2 = list.files("../data/Shiny Data/shiny_results3", full.names = TRUE)


read1 = function(x){
  temp = read.csv(x)
  nameid = gsub("results_", "", x)
  nameid = gsub(".txt", "", nameid)
  temp2 = data.frame(ID = rep(nameid, 20), a = temp$x[2:21], b=temp$x[22:41], choice = temp$x[42:61])
  return(temp2)
}

l = lapply(f2, read1)
dat2 = do.call(rbind, l)


## Then, to do intra-coder stuff:
## Merge dat to walk, and dat2 to walk

load("../data/walk.RData") # This file is obscured by IRB requirement
walk2 = merge(dat, walk[,-1], by.x = "ID", by.y="response1")
dat2$ID = gsub("../data/Shiny Data/shiny_results3/", "", dat2$ID)
walk3 = merge(walk2, dat2, by.x=c("a", "b", "response"), by.y=c("a", "b", "ID")) 
tpc = walk3 %>%
  dplyr::group_by(response) %>%
  dplyr::summarize(source = "Paired Comparisons",
                   agree = mean(as.character(choice.x)==as.character(choice.y))      )

load("../results/fig4_data.RData")
intracoder <- intracoder[-2]
fr = data.frame(source = "Full Ranking", agree = intracoder) 
pf = rbind(fr, tpc[,-1])

#rand = replicate(1000, mean(rbinom(20,1,.5)==rbinom(20,1,.5)))
rand= placebo
rand = data.frame(source = "Random", agree=rand)
pf = rbind(pf, rand)

save(pf, file="../results/fig2_si.RData")
