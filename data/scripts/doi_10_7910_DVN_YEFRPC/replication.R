#####################################################################
## Replication file for                                            ##
## Who registers? Village Networks, Household Dynamics, and Voter  ##
##   Registration in Rural Uganda                                  ##
## Authors: Romain Ferrali, Guy Grossman, Melina R Platas,         ##
##   Jonathan Rodden                                               ##
## June 17, 2021                                                   ##
## Maintainer: Romain Ferrali (rferrali@nyu.edu)                   ##
#####################################################################

# run on Windows 10 PC, core i7, 12 threads, 32 GB ram, using R 4.0.3
library(car) # 3.0-7
library(igraph) # 1.2.5
library(tidyverse) # 1.3.0
library(multiwayvcov) # 1.2.3
library(stargazer) # 5.2.2
library(haven) # 2.2.0
library(lfe) # 2.8-5
library(readxl) # 1.3.1
library(mgcv) # 1.8-33
library(rstanarm) # 2.19.3
library(sandwich) # 2.5-1
library(lmtest) # 0.9-37
source("functions.R")




mods <- list()
modsVC <- list()
devel <- FALSE # set to TRUE to not use clustered standard errors -- faster

theme_set(theme_minimal(base_size = 8))

if(!devel) {
  library(parallel)
  options(mc.cores = 4)
  cl <- makePSOCKcluster(14)
  options(boot.ncpus = 14)
  clusterSetRNGStream(cl, 1989) # the wind of change
  nRepVc <- 1e4
  nRepRE <- 1e4
  nChainsRE <- 4
} else {
  cl <- F
  nRepRE <- 500
  nRepPermut <- 5
  nChainsRE <- 1
  nRepVc <- 1e3
}

#### read in data ####

df <-  read_csv("nodes.csv")
ties <-  read_csv("ties.csv", col_types = cols(dist = col_double()))
census1 <-  read_csv("census_1.csv", col_types = cols(villageId = col_double(), villageTxt = col_character()))
census2 <-  read_csv("census_2.csv")
microCensus <-  read_csv("micro_census.csv")

#### make main objects ####

## dfEst: dataset used for estimation

g <- makeGraphs(df, ties)
g$weighted <- map(1:nrow(g), function(i) {
  tmp <- as_adj(g$friend[[i]]) + as_adj(g$family[[i]]) + as_adj(g$lender[[i]]) + as_adj(g$solver[[i]])
  tmp <- graph.adjacency(tmp, mode = "undirected", weighted = T)
  gg <- g$union[[i]]
  V(tmp)$registered <- V(gg)$registered
  V(tmp)$villageId <- V(gg)$villageId
  V(tmp)$LC <- V(gg)$LC
  tmp
})

addGraphs <- function(graphs, graphs2, directed = F) {
  map2(graphs, graphs2, function(g1, g2) {
    g1 <- as_adj(g1)
    g2 <- as_adj(g2)
    g <- g1 + g2
    g[g == 1] <- 0
    g[g == 2] <- 1
    graph_from_adjacency_matrix(g, ifelse(directed, "directed", "undirected"))
  })
}

removeGraphs <- function(graphs, graphs2, directed = F) {
  map2(graphs, graphs2, function(g1, g2) {
    g1 <- as_adj(g1)
    g2 <- as_adj(g2)
    g <- g1 - g2
    g[g == -1] <- 0
    graph_from_adjacency_matrix(g, ifelse(directed, "directed", "undirected"))
  })
}

g$friFam <- addGraphs(g$friend, g$family)
g$lenFam <- addGraphs(g$lender, g$family)
g$solFam <- addGraphs(g$solver, g$family)

g$friNotFam <- removeGraphs(g$friend, g$family)
g$lenNotFam <- removeGraphs(g$lender, g$family)
g$solNotFam <- removeGraphs(g$solver, g$family)

g$lenSol <- addGraphs(g$lender, g$solver)
g$lenSol <- removeGraphs(g$lenSol, g$family)

dfEst <- df %>% mutate(
  nReg = netTreat("registered", "union", g),
  pctReg = netTreat("registered", "union", g, rowNormalize = T),
  geo = netTreat("registered", "geo", g, weight = "w"),
  age2 = age^2,
  nRegFam = netTreat("registered", "family", g),
  pctRegFam = netTreat("registered", "family", g, rowNormalize = T),
  nRegFri = netTreat("registered", "friend", g),
  pctRegFri = netTreat("registered", "friend", g, rowNormalize = T),
  nRegLen = netTreat("registered", "lender", g),
  pctRegLen = netTreat("registered", "lender", g, rowNormalize = T),
  nRegSol = netTreat("registered", "solver", g),
  pctRegSol = netTreat("registered", "solver", g, rowNormalize = T),
  nRegLenOut = netTreat("registered", "lenderU", g),
  pctRegLenOut = netTreat("registered", "lenderU", g, rowNormalize = T), 
  degreeLC = netTreat("LC", "union", g), 
  degreeLenOutLC = netTreat("LC", "lenderU", g),  
  pctRegLC = netTreat("registered", "union", g, rowNormalize = T, filter = "LC"), 
  pctRegLenLC = netTreat("registered", "lenderU", g, rowNormalize = T, filter = "LC"), 
  pctRegFriFam = netTreat("registered", "friFam", g, rowNormalize = T), 
  pctRegLenFam = netTreat("registered", "lenFam", g, rowNormalize = T), 
  pctRegSolFam = netTreat("registered", "solFam", g, rowNormalize = T), 
  pctRegFriNotFam = netTreat("registered", "friNotFam", g, rowNormalize = T), 
  pctRegLenNotFam = netTreat("registered", "lenNotFam", g, rowNormalize = T), 
  pctRegSolNotFam = netTreat("registered", "solNotFam", g, rowNormalize = T), 
  pctRegLenSol = netTreat("registered", "lenSol", g, rowNormalize = T), 
  pctRegURM = netTreat("registered", "unionRM", g, rowNormalize = T)
) %>% 
  rename(village = villageTxt)


dfEst$degree <- netCov(g$union, degree)
dfEst$closeness <- netCov(g$union, igraph::closeness)
dfEst$clustering <- netCov(g$union, transitivity, type = "local")
dfEst$eigenvector <- netCov(g$union, function(g) eigen_centrality(g)$vector)

dfEst$degreeFam <- netCov(g$family, degree)
dfEst$degreeFri <- netCov(g$friend, degree)
dfEst$degreeLen <- netCov(g$lender, degree)
dfEst$degreeSol <- netCov(g$solver, degree)
dfEst$degreeLenOut <- netCov(g$lenderU, degree, mode = "out")
dfEst$degreeLenIn <- netCov(g$lenderU, degree, mode = "out")
dfEst$degreeFriFam <- netCov(g$friFam, degree)
dfEst$degreeSolFam <- netCov(g$solFam, degree)
dfEst$degreeLenFam <- netCov(g$lenFam, degree)
dfEst$degreeFriNotFam <- netCov(g$friNotFam, degree)
dfEst$degreeSolNotFam <- netCov(g$solNotFam, degree)
dfEst$degreeLenNotFam <- netCov(g$lenNotFam, degree)
dfEst$degreeLenSol <- netCov(g$lenSol, degree)
dfEst$degreeURM <- netCov(g$unionRM, degree)

dfEst <- dfEst %>% group_by(hh) %>% 
  mutate(
    nRegHH = sum(registered) - registered, 
    size = n(), 
    pct = mean(registered),
    degreeHH = n() - 1, 
    pctRegHH = nRegHH / degreeHH
  ) %>% 
  replace_na(list(pctRegHH = 0))

df1 <- dfEst %>% filter(size == 1)
dfEst <- dfEst %>% filter(size > 1)


dfEst <- dfEst %>% 
  mutate(hasHead = sum(head, na.rm = T)) %>% 
  replace_na(list(hasHead = 0)) %>% 
  filter(hasHead == 1) %>% 
  select(-hasHead) %>% 
  ungroup()

## main model formula & associated labels

f <- formula(registered ~ female + age + age2 + income + edu + rlg + eth + leader + 
               prosoc_dic + pol_index + village + distance + geo + head)

regLabs <- c(
  "female", "age", "age$^2$", "income", "2ary education", "Catholic", "Lugbara", 
  "leader", "pro-sociality", "participation index", "distance (km)", "geography", 
  "household head"
)


#### figure SI-1 ####


dfAgg <- bind_rows(
  census1 %>% select(-villageId, -villageTxt) %>% gather(variable, value, -insample), 
  census2 %>% gather(variable, value, -insample)
)  %>% 
  mutate(
    variable = recode(
      variable, 
      edu = "pct. 2ary education", 
      female = "pct. females", 
      n = "adult population", 
      registered = "pct. registered", 
      pct20 = "pct. adults", 
      roof = "pct. thatch roofs", 
      subsistence = "pct. subsistence farming", 
      meals = "pct. malnourished HHs", 
      power = "pct. electrified HHs"
    )
  )


pl <- dfAgg
plVillages <- pl %>% filter(insample == 1)
plQuantiles <- pl %>% group_by(variable) %>% 
  summarize(
    lb = quantile(value, .2), 
    ub = quantile(value, .8)
  ) %>% gather(v, summary, lb, ub)

newLabels <- map_dfr(unique(plQuantiles$variable), function(var) {
  lb <- plQuantiles %>% filter(variable == var, v == "lb") %>% pull(summary)
  ub <- plQuantiles %>% filter(variable == var, v == "ub") %>% pull(summary)
  values <- plVillages %>% filter(variable == var) %>% pull(value)
  nIn <- sum(values >= lb & values <= ub)
  nLeft <- sum(values < lb)
  nRight <- sum(values > ub)
  tibble(
    variable = var, 
    newVariable = sprintf("%s (%s inside, %s left, %s right)", var, nIn, nLeft, nRight)
  )
})

pl <- pl %>% inner_join(newLabels) %>% select(-variable) %>% rename(variable = newVariable)
plVillages <- plVillages %>% inner_join(newLabels) %>% select(-variable) %>% rename(variable = newVariable)
plQuantiles <- plQuantiles %>% inner_join(newLabels) %>% select(-variable) %>% rename(variable = newVariable)

pl %>% 
  ggplot(aes(value)) + 
  geom_histogram(fill = "white", color = "black", bins = 60) + 
  facet_wrap(~ variable, scale = "free", ncol = 1) + 
  geom_vline(data = plQuantiles, aes(xintercept = summary), lwd = 1) + 
  geom_vline(data = plVillages, aes(xintercept = value), lty = "dotted")
ggsave("figures/representation.pdf", width = 7, height = 8)


#### inline number #### 
## overlapping lender/family ties
inline <- map_dfr(1:nrow(g), function(i) {
  g1 <- as_adj(g$lenderU[[i]])
  g2 <- as_adj(g$family[[i]])
  filtr <- as.numeric(colnames(g1))
  filtr <- which(filtr %in% dfEst$i)
  g1 <- g1[filtr, filtr]
  g2 <- g2[filtr, filtr]
  nTies <- sum(g1)
  g1 <- g1 + g2
  g1[g1 == 1] <- 0
  g1[g1 == 2] <- 1
  nOverlap <- sum(g1)
  tibble(nTies = nTies, nOverlap = nOverlap)
}) %>% summarize(
  nTies = sum(nTies), 
  nOverlap = sum(nOverlap)
) %>% mutate(pct = nOverlap / nTies)

inline
inline$pct

#### table 1 ####

makeRow <- function(var, label) {
  v00 <-mean(df1 %>% pull(var), na.rm = T) %>% round(3) %>% as.character() 
  v0 <- mean(dfEst %>% pull(var), na.rm = T) %>% round(3) %>% as.character()
  v1 <- mean(dfEst %>% filter(head == 1) %>% pull(var), na.rm = T) %>% round(3) %>% as.character()
  v2 <- mean(dfEst %>% filter(head == 0) %>% pull(var), na.rm = T) %>% round(3) %>% as.character()
  f <- sprintf("%s ~ head | 0 | 0 | village", var) %>% as.formula()
  fDesc <- sprintf("%s ~ sample | 0 | 0 | village", var) %>% as.formula()

  dfDesc <- bind_rows(
    df1 %>% select(c(var, "village")) %>% mutate(sample = 1), 
    dfEst %>% select(c(var, "village")) %>% mutate(sample = 0)
  )
  mod <- felm(fDesc, data = dfDesc)
  delta0 <- round(coef(mod)[2], 3)
  pval <- summary(mod)$coefficients[2,4]
  delta0 <- sprintf("%s%s", delta0, pvalToStars(pval))

  mod <- felm(f, data = dfEst)
  delta <- round(coef(mod)[2], 3)
  pval <- summary(mod)$coefficients[2,4]
  delta <- sprintf("%s%s", delta, pvalToStars(pval))
  tibble(
    Variable = label, 
    `HH size 1` = v00,
    Sample = v0, 
    `$\\Delta_0$` = delta0, 
    `HH head` = v1, 
    `Non-head` = v2, 
    `$\\Delta_1$` = delta
  )
}

desc <- map2_dfr(
  c(
    "registered", 
    "female", 
    "age", 
    "income", 
    "edu", 
    "rlg", 
    "eth", 
    "prosoc_dic", 
    "leader", 
    "pol_index", 
    "distance", 
    "degree", 
    "closeness", 
    "clustering", 
    "eigenvector"
  ), c(
    "\\% registered", 
    "\\% female", 
    "age", 
    "income", 
    "\\% 2ary education", 
    "\\% Catholic", 
    "\\% Lugbara", 
    "pro-sociality", 
    "leader", 
    "participation index", 
    "distance to registration center (km)", 
    "degree centrality", 
    "closeness centrality", 
    "clustering coefficient", 
    "eigenvector centrality"
  ), 
  makeRow
 ) %>% bind_rows(tibble(
   Variable = "$N$", 
   `HH size 1` = nrow(df1) %>% as.character(), 
  Sample = nrow(dfEst) %>% as.character(), 
  `$\\Delta_0$` = "", 
  `HH head` = dfEst %>% filter(head == 1) %>% nrow() %>% as.character(), 
  `Non-head` = dfEst %>% filter(head == 0) %>% nrow() %>% as.character(), 
  `$\\Delta_1$` = ""
 ))

sink("tables/desc.tex")
kableExtra::kable(
  desc, "latex", 
  booktabs = T, 
  escape = F, 
  linesep = "", 
  align = c("l", "c", "c", "c", "c", "c", "c")
) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::pack_rows(index = c(
    "Dependent variable" = 1, 
    "Individual" = 10, 
    "Network" = 4, 
    "Sample size" = 1
  ))
sink()

#### figure 2 ####

pl1 <- 
  tibble(size = "1", n = nrow(df1), type = "1") %>% 
  bind_rows(
    dfEst %>% 
    select(hh, size, pct) %>% 
    distinct() %>% 
    mutate(
      size = ifelse(size > 5, 5, size),
      size = ifelse(size == 5, "5+", size)
    ) %>% group_by(size) %>% 
    summarize(n = n()) %>% 
    mutate(type = "2")
  ) %>% 
  ggplot(aes(x = size, y = n, fill = type)) + 
  geom_col() + 
  scale_fill_manual(
    values = c("1" = "grey66", "2" = "grey33"), 
    guide = F
  ) + 
  labs(
    title = "(a) Distribution", 
    x = "N household members interviewed", 
    y = "Number of households"
  )

pl2 <- tibble(size = "1", pct = mean(df1$registered, na.rm = T), type = "1") %>% 
  bind_rows(
  dfEst %>% 
    select(hh, size, pct) %>% 
    distinct() %>% 
    mutate(
      size = ifelse(size > 5, 5, size),
      size = ifelse(size == 5, "5+", size)
    ) %>% group_by(size) %>% 
    summarize(pct = mean(pct)) %>% 
    mutate(type = "2")
  ) %>% 
  ggplot(aes(x = size, y = pct, fill = type)) + 
  geom_col() + 
  scale_fill_manual(
    values = c("1" = "grey66", "2" = "grey33"), 
    guide = F
  ) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(
    title = "(b) Registration rates", 
    x = "N household members interviewed", 
    y = "Mean registration rate"
  )

pl <- gridExtra::grid.arrange(pl1, pl2, nrow = 1)

ggsave("figures/distHouseholds.pdf", plot = pl, width = 6, height = 3)


#### figure 1 ####

reMods <- list(
  `full sample` = stan_lmer(
    registered ~ 1 + (1 | village), data = df %>% rename(village = villageTxt), 
    iter = nRepRE, 
    chains = nChainsRE, 
    seed = 1790
  ), 
  `HH of size 2+` = stan_lmer(
    registered ~ 1 + (1 | village), data = dfEst, 
    iter = nRepRE, 
    chains = nChainsRE, 
    seed = 1791
  )
)

## inline: within vs. across groups

icc <- function(mod) {
  icc <- c(VarCorr(mod)$village, sigma(mod))^2
  icc <- 100 * icc[1] / sum(icc)
  icc
}

map_dbl(reMods, icc)

## figure

makePl <- function(mod) {
  pl <- as.matrix(mod)
  pl <- pl[,1:(ncol(pl)-2)]
  x <- cbind(1, diag(ncol(pl)-1))
  pl <- x %*% t(pl)
  tibble(
    village = dfEst$village %>% unique() %>% sort(), 
    pe = apply(pl, 1, mean), 
    lb = apply(pl, 1, quantile, .025), 
    ub = apply(pl, 1, quantile, .975)
  )
}

pl1 <- map_dfr(reMods, makePl, .id = "sample")
pl2 <- tibble(
  sample = c("full sample", "HH of size 2+"), 
  registration = c(mean(df$registered), mean(dfEst$registered))
)

pl1 %>% 
  arrange(sample, pe) %>% 
  inner_join(
    ., 
    filter(., sample == "full sample") %>% mutate(newVillage = LETTERS[1:16]) %>% 
    select(village, newVillage)
  ) %>% 
  select(-village) %>% 
  rename(village = newVillage) %>% 
  mutate(village = fct_inorder(village)) %>% 
  ggplot(aes(x = village, y = pe, ymin = lb, ymax = ub, color = sample)) + 
  scale_color_grey() + 
  geom_point(position = position_dodge(.5)) + 
  geom_linerange(position = position_dodge(.5)) + 
  geom_hline(data = pl2, aes(yintercept = registration), lty = "dotted") + 
  scale_y_continuous(limits = c(0,1), labels = scales::percent) + 
  theme(legend.position = "bottom") + 
  labs(
    x = "village", 
    y = "registration rate"
  )
ggsave("figures/rates.pdf", width = 6, height = 3)

#### Table 2 ####

dfEst2 <- df %>% mutate(
    age2 = age^2, 
    geo = netTreat("registered", "geo", g, weight = "w"), 
    village = villageTxt
  ) %>% 
  group_by(hh) %>% 
  mutate(head = ifelse(is.na(head) & n() == 1, 1, head)) %>% 
  ungroup()

mods$full <- lm(f, data = dfEst2)
mods$base <- lm(f, data = dfEst)
mods$baseHead <- update(mods$base, . ~ . - head, data = dfEst, subset = head == 1)
mods$baseNoHead <- update(mods$base, . ~ . - head, data = dfEst, subset = head == 0)
mods$hh <- update(mods$base, . ~ . + degreeHH + pctRegHH)
mods$peerHead <- update(mods$base, . ~ . - head + degreeHH + pctRegHH, data = dfEst, subset = head == 1)
mods$peerNoHead <- update(mods$base, . ~ . - head + degreeHH + pctRegHH, data = dfEst, subset = head == 0)

modsVC <- map(mods, myVC)

sink("tables/main.tex")
mystar(
  mods[1:7], vc = F, se = starSD(names(mods)[1:7]), 
  covariate.labels = c(
    regLabs, 
    "degree HH", "\\% registered HH peers"
  ), column.labels = c(
    "All", "All (2+)", "Head", "Non-Head", "All (2+)", "Head", "Non-Head"
  )
)
sink()

#### Table SI-5 ####

corTable <- dfEst2 %>% select(
  registered, female, age, income, edu, edu_full, rlg, eth,    
    leader, prosoc_dic, pol_index, distance, geo, 
    head
) %>% mutate(edu_full = as.integer(edu_full == "No schooling")) %>% na.omit()
corTable <- corstarsl(corTable)
rownames(corTable) <- c(
  "registered", "female", "age", "income", "2ary education", "no schooling", 
  "Catholic", "Lugbara", "leader", "pro-sociality", "participation index", 
  "distance (km)", "geography", "household head"
)
colnames(corTable) <- c(
  "regis.", "female", "age", "income", "2ary edu.", "no school.", 
  "Catho.", "Lug.", "leader", "pro-soc.", "pol. particip.", 
  "dist.", "geo."
)
sink("tables/correlations.tex")
xtable::xtable(
  corTable, 
  align = c("p{3.5cm}", rep("c", 13)), 
  caption = "{\\bf Bivariate Correlations between DV and main control variables.}", 
  label = "tbl:cormatrix"
)
sink()

#### Table 4 ####

for(net in c("HH", "URM", "Fam", "Fri", "Len", "Sol")) {
  degVar <- sprintf("degree%s", net)
  peerVar <- sprintf("pctReg%s", net)
  modName <- sprintf("comp%s", net)
  dfTmp <- dfEst
  dfTmp$degree <- unlist(dfEst[,degVar])
  dfTmp$pct <- unlist(dfEst[,peerVar])
  mod <- lm(update(f, . ~ . + degree + pct), data = dfTmp)
  mods[[modName]] <- mod
  modsVC[[modName]] <- myVC(mod)
}


sink("tables/comp.tex")
mystar(
  mods[c(8:13)], vc = F, se = starSD(names(mods)[c(8:13)]),  
  covariate.labels = c(regLabs, "degree", "\\% registered peers"), 
  column.labels = c(
    "Household", "Non-HH", "Family", "Friends", "Lender", "Solver"
  )
)
sink()

#### Table SI-9 ####

for(net in c("URM", "Fam", "Fri", "Len", "Sol")) {
  degVar <- sprintf("degree%s", net)
  peerVar <- sprintf("pctReg%s", net)
  modName <- sprintf("comp%s2", net)
  dfTmp <- dfEst
  dfTmp$degree <- unlist(dfEst[,degVar])
  dfTmp$pct <- unlist(dfEst[,peerVar])
  mod <- lm(update(f, . ~ . + degree + pct + degreeHH + pctRegHH), data = dfTmp)
  mods[[modName]] <- mod
  modsVC[[modName]] <- myVC(mod)
}

sink("tables/comp2.tex")
mystar(
  mods[14:18], vc = F, se = starSD(names(mods)[14:18]),  
  covariate.labels = c(
    regLabs, 
    "degree", "\\% registered peers", 
    "degree HH", "\\% registered HH peers"
  ), 
  column.labels = c(
    "Non-HH", "Family", "Friends", "Lender", "Solver"
  )
)
sink()

#### Table 3 ####

dfEst <- dfEst %>% group_by(hh) %>% mutate(
  headRegistered = max(head * registered)
) %>% ungroup()

dfEst3 <- dfEst %>% 
  group_by(hh) %>% 
  mutate(
    nRegHHNotHead = nRegHH-headRegistered, 
    pctRegHHNotHead = nRegHHNotHead / (degreeHH-1)
  ) %>% 
  filter(head == 0 & degreeHH > 1)

mods$headReg <- update(mods$base, . ~ . - head + degreeHH + headRegistered, subset = head == 0)
mods$compHHPool <- update(mods$base, . ~ . - head + degreeHH + pctRegHH, data = dfEst3)
mods$compHHHead <- update(mods$base, . ~ . - head + degreeHH + headRegistered, data = dfEst3)
mods$compHHSep <- update(mods$base, . ~ . - head + degreeHH + headRegistered + pctRegHHNotHead, data = dfEst3)

modsVC$headReg <- myVC(mods$headReg)
modsVC$compHHPool <- myVC(mods$compHHPool)
modsVC$compHHHead <- myVC(mods$compHHHead)
modsVC$compHHSep <- myVC(mods$compHHSep)

sink("tables/compHH.tex")
mystar(
  mods[c(7,19:22)], vc = F, se = starSD(names(mods)[c(7,19:22)]), 
  covariate.labels = c(
    regLabs[-length(regLabs)], 
    "degree HH", 
    "\\% registered ", "head registered", 
    "\\% registered non-head " 
  ), column.labels = c(
    "Size 2+", "Size 2+", "Size 3+", "Size 3+", "Size 3+"
  )
)
sink()


## inline number

predict(
  mods$headReg, 
  newdata = model.frame(mods$headReg) %>% mutate(headRegistered = 0)
) %>% mean()

predict(
  mods$headReg, 
  newdata = model.frame(mods$headReg) %>% mutate(headRegistered = 1)
) %>% mean()

#### Table 5 ####

mostPopularMyLenderRegistered <- function(id) {
  village <- dfEst %>% filter(i == id) %>% pull(villageId)
  thisG <- g %>% filter(villageId == village) %>% pull(lenderU)
  thisG <- thisG[[1]]
  node <- V(thisG)[V(thisG)$name == id]
  nei <- neighborhood(thisG, nodes = node, mode = "out")[[1]]
  cond <- nei$name != id
  nei <- nei[cond]
  nei
  if(length(nei) == 0) {
    return(0)
  } else if(length(nei) == 1) {
    v <- nei
  } else {
    nei <- sample(nei, length(nei))
    degs <- degree(thisG, nei, "in")
    v <- nei[which.max(degs)]
  }
  v <- as.numeric(v$name)
  out <- df %>% filter(i == v) %>% pull(registered) + 1
  return(out)
}

mostPopularLenderRegistered <- function(id) {
  village <- dfEst %>% filter(i == id) %>% pull(villageId)
  thisG <- g %>% filter(villageId == village) %>% pull(lenderU)
  thisG <- thisG[[1]]
  mostPop <- which.max(degree(thisG, mode = "in"))
  mostPop <- V(thisG)[mostPop]
  node <- V(thisG)[V(thisG)$name == id]
  nei <- neighborhood(thisG, nodes = node, mode = "out")[[1]]
  if(!mostPop %in% nei) {
    return(0)
  } else {
    v <- as.numeric(mostPop$name)
    out <- df %>% filter(i == v) %>% pull(registered) + 1
    return(out)
  }
}

distLC <- function(g, stg = F) {
  if(stg) g <- delete_edges(g, E(g)[E(g)$weight < 2])
  dists <- distances(g, V(g), V(g)[V(g)$LC == 1], mode = "out", weights = NA)
  vs <- rownames(dists) %>% as.numeric()
  stat <- apply(dists, 1, min)
  stat[is.infinite(stat)] <- 0
  stat[V(g)$LC == 1] <- 0
  tibble(i = vs, dist = stat)
}


dfEst <- dfEst %>% mutate(
  degree0 = as.integer(degreeLen > 0), 
  degree0Out = as.integer(degreeLenOut > 0), 
  degree0LC = as.integer(degreeLenOutLC > 0), 
  mostPopReg = as.factor(map_dbl(i, mostPopularMyLenderRegistered)), 
  mostPopAllReg = as.factor(map_dbl(i, mostPopularLenderRegistered)), 
  LCReg = as.factor((degreeLenOutLC > 0) + (pctRegLenLC > 0))
)

dfEst <- dfEst %>% 
  inner_join(map_dfr(g$lender, distLC)) %>% rename(distLCLen = dist) %>% 
  inner_join(map_dfr(g$weighted, ~ distLC(., stg = T))) %>% rename(distLCStg = dist) %>% 
  mutate(
    stgLC = as.integer(distLCStg == 1), 
    distLCStg = ifelse(distLCStg >= 5, 5, distLCStg) %>% as_factor(), 
    distLCLen = ifelse(distLCLen >= 5, 5, distLCLen) %>% as_factor(), 
  )
mods$newLender <- update(mods$base, . ~ . + degree0 + degreeLen +  pctRegLen)
mods$lenderOut <- update(mods$base, . ~ . + degree0Out + degreeLenOut + pctRegLenOut)
mods$mostPop <- update(mods$base, . ~ . + mostPopReg)

modsVC$newLender <- myVC(mods$newLender)
modsVC$lenderOut <- myVC(mods$lenderOut)
modsVC$mostPop <- myVC(mods$mostPop)

sink("tables/lender.tex")
mystar(
  mods[23:25], vc = F, se = starSD(names(mods)[23:25]), 
  covariate.labels = c(
    regLabs, 
    "degree $>$ 0", "degree", "\\% registered lenders", 
    "out degree $>$ 0", "out degree", "\\% registered out lenders", 
    "most popular lender not registered", 
    "most popular lender registered"
  )
)
sink()

#### Table 6 ####

dfEst$distLC <- as.factor(as.integer(dfEst$degree0LC == 1))
mods$LC <- update(mods$base, . ~ . + distLC)
modsVC$LC <- myVC(mods$LC)
dfEst$distLC <- ifelse(as.character(dfEst$distLCLen) %in% c("4", "5"), "0", as.character(dfEst$distLCLen))
mods$distLC <- update(mods$base, . ~ . + distLC)
modsVC$distLC <- myVC(mods$distLC)

sink("tables/distToLC.tex")
mystar(
  mods[c(26,27)], vc = F, se = starSD(names(mods)[c(26,27)]), 
  covariate.labels = c(
    regLabs, 
    "distance = 1", 
    "distance = 2", 
    "distance = 3"
  )
)
sink()


#### Table SI-11 ####

mttest <- function(var, net) {
  df <- dfEst
  df$var <- df %>% pull(var)
  df$net <- switch(
    net, 
    strong = df$distLCStg, 
    lender = df$distLCLen
  )
  df$net <- as.integer(as.character(df$net) %in% as.character(1:3))
  df <- df %>% select(var, net, village) %>% na.omit()
  mod <- lm(var ~ net + village, data = df)
  out <- coeftest(mod, myVC(mod))
  diff <- sprintf("%s%s", round(out[2,1], 3), pvalToStars(out[2,4]))
  tibble(
    var = var, 
    net = net, 
    mean1 = mean(df$var[df$net == 1]) %>% round(3), 
    mean0 = mean(df$var[df$net == 0]) %>% round(3), 
    diff = diff
  )
}

desc2 <- expand.grid(
  var = c(
    "registered", 
    "head", "female", "age", "income", "edu", "rlg", "eth", "leader", 
    "prosoc_dic", "pol_index", "distance", 
    "degree", "closeness", "clustering", "eigenvector"
  ), 
  net = c("lender", "strong")
) %>% mutate_all(as.character) %>% 
  pmap_dfr(mttest) %>% 
  pivot_wider(
    names_from = net, 
    values_from = c(mean1, mean0, diff)
  ) %>% 
  select(
    var, 
    ends_with("lender"), 
    ends_with("strong")
  ) %>% 
  mutate(var = c(
    "\\% registered", 
    "\\% HH head", 
    "\\% female", 
    "age", 
    "income", 
    "\\% 2ary education", 
    "\\% Catholic", 
    "\\% Lugbara", 
    "pro-sociality", 
    "leader", 
    "participation index", 
    "distance to registration center (km)", 
    "degree centrality", 
    "closeness centrality", 
    "clustering coefficient", 
    "eigenvector centrality"
  )) %>% 
  bind_rows(
    tibble(
      var = "$N$", 
      mean1_lender = nrow(dfEst %>% 
        filter(as.character(distLCLen) %in% as.character(1:3))) %>% 
        round(), 
      mean0_lender = nrow(dfEst %>% 
        filter(!as.character(distLCLen) %in% as.character(1:3))) %>% 
        round(), 
      mean1_strong = nrow(dfEst %>% 
        filter(as.character(distLCStg) %in% as.character(1:3))) %>% 
        round(),
      mean0_strong = nrow(dfEst %>% 
        filter(!as.character(distLCStg) %in% as.character(1:3))) %>% 
        round(), 
      diff_lender = "", 
      diff_strong = ""
    )
  )

colnames(desc2) <- c("Variable", "Close", "Far", "$\\Delta$", "Close", "Far", "$\\Delta$")
 
sink("tables/descFarClose.tex")
kableExtra::kable(
  desc2, "latex", 
  booktabs = T, 
  escape = F, 
  linesep = "", 
  align = c("l", "c", "c", "c", "c", "c", "c")
) %>% 
  kableExtra::kable_styling() %>% 
  kableExtra::pack_rows(index = c(
    "Dependent variable" = 1, 
    "Individual" = 11, 
    "Network" = 4, 
    "Sample size" = 1
  )) %>% 
  kableExtra::add_header_above(c(" " = 1, "Lending network" = 3, "Strong ties network" = 3))
sink()


#### Table SI-8 ####

dfEst <- dfEst %>% mutate_at(
  vars(closeness, clustering, eigenvector), 
  ~ (. - min(.)) / (max(.) - min(.))
)

mods$netDeg <- update(mods$base, . ~ . + degree)
mods$netClo <- update(mods$base, . ~ . + degree + closeness)
mods$netClu <- update(mods$base, . ~ . + degree + clustering)
mods$netEig <- update(mods$base, . ~ . + degree + eigenvector)

modsVC$netDeg <- myVC(mods$netDeg)
modsVC$netClo <- myVC(mods$netClo)
modsVC$netClu <- myVC(mods$netClu)
modsVC$netEig <- myVC(mods$netEig)

sink("tables/netMods.tex")
mystar(
  mods[28:31], vc = F, se = starSD(names(mods)[28:31]), 
  covariate.labels = c(
    regLabs, 
    "degree", "closeness", 
    "clustering", "eigenvector"
  )
)
sink()

#### Table SI-6 ####

map_dfr(g$lenderU, function(g) {
  tibble(
    nLC = length(V(g)[V(g)$LC == 1]), 
    nLC0 = length(V(g)[V(g)$LC == 1 & degree(g, mode = "in") == 0])
  )
}) %>% 
  summarize(nLC = sum(nLC), nLC0 = sum(nLC0), pct  = nLC0 / nLC)


mods$hhN <- update(mods$base, . ~ . + degreeHH + nRegHH)
mods$peerHeadN <- update(mods$base, . ~ . - head + degreeHH + nRegHH, data = dfEst, subset = head == 1)
mods$peerNoHeadN <- update(mods$base, . ~ . - head + degreeHH + nRegHH, data = dfEst, subset = head == 0)

modsVC$hhN <- myVC(mods$hhN)
modsVC$peerHeadN <- myVC(mods$peerHeadN)
modsVC$peerNoHeadN <- myVC(mods$peerNoHeadN)

sink("tables/compN.tex")
mystar(
  mods[32:34], vc = F, se = starSD(names(mods)[32:34]), 
  covariate.labels = c(
    regLabs, 
    "degree HH", "N registered HH peers"
  ), column.labels = c(
    "All", "Head", "Non-Head"
  )
)
sink()

#### Table SI-10 ####

for(net in c("LenFam", "LenNotFam", "LenSol")) {
  degVar <- sprintf("degree%s", net)
  peerVar <- sprintf("pctReg%s", net)
  modName <- sprintf("comp%s", net)
  dfTmp <- dfEst
  dfTmp$degree <- unlist(dfEst[,degVar])
  dfTmp$pct <- unlist(dfEst[,peerVar])
  mod <- lm(update(f, . ~ . + degree + pct), data = dfTmp)
  mods[[modName]] <- mod
  modsVC[[modName]] <- myVC(mod)
}

sink("tables/compLen.tex")
mystar(
  mods[c(12,35:37)], vc = F, se = starSD(names(mods)[c(12,35:37)]),  
  covariate.labels = c(regLabs, "degree", "\\% registered peers"), 
  column.labels = c(
    "Both", "Lender \\& Family", "Lender \\& not Family", "Lender \\& Solver \\& not Family"
  )
)
sink()

#### Table SI-12 ####

lenders <- list(
  mostPop = map_dbl(g$lenderU, function(g) {
      degs <- degree(g, mode = "in")
      vs <- V(g)[which.max(degs)]
      return(as.numeric(vs$name))
    }), 
  LC = map(g$lenderU, function(g) {
    nodes <- V(g)[V(g)$LC == 1]
    return(as.numeric(nodes$name))
  }) %>% do.call(c, .), 
  myMostPop = map(dfEst$i, function(id) {
      v <- dfEst %>% filter(i == id) %>% pull(villageId)
      thisG <- g %>% filter(villageId == v) %>% pull(lenderU)
      thisG <- thisG[[1]]
      vtx <- V(thisG)[V(thisG)$name == id]
      nei <- neighborhood(thisG, nodes = vtx, mode = "out")[[1]]
      nnei <- nei$name
      nei <- nei[nnei != id]
      if(length(nei) == 0) return(character(0))
      return(nei$name)
    }) %>% do.call(c, .) %>% unique() %>% as.numeric(), 
  allLenders = map(g$lenderU, function(g) {
      degs <- degree(g, mode = "in")
      vs <- V(g)[degs > 0]
      return(as.numeric(vs$name))
    }) %>% do.call(c, .), 
  all = dfEst$i
)


map_dbl(g$lenderU, function(g) {
  degs <- degree(g, mode = "in")
  nodes <- V(g)[degs == max(degs)]
  nei <- names(unlist(neighborhood(g, nodes = nodes)))
  nodes <- names(nodes)
  nei <- nei[!nei %in% nodes]
  hh <- unique(substr(nei, 1, nchar(nei)-2))
  allNodes <- names(V(g))
  allNodes <- substr(allNodes, 1, nchar(allNodes)-2)
  nei <- allNodes[allNodes %in% hh]
  length(nei) / length(allNodes)
}) %>% mean()

desc <- map_dfr(
  lenders, 
  ~ map_dfr(g$lenderU, function(g, ids) {
    vs <- V(g)[V(g)$name %in% ids]
    reg <- df %>% filter(i %in% ids) %>% pull(registered) %>% mean()
    deg <- (degree(g, vs, mode = "in") %>% mean()) / length(V(g))
    tibble(n = length(vs), reg = reg, deg = deg)
  }, ids = .) %>% summarize(
    `N` = sum(n), 
    `pct. registered` = weighted.mean(reg, n),
    `mean connectedness` =  weighted.mean(deg, n)
  ), .id = "population"
) %>% mutate(
  population = recode(
    population, 
    mostPop = "most popular lenders", 
    LC = "LC lenders", 
    myMostPop = "most popular lenders to ego", 
    allLenders = "all lenders", 
    all = "all individuals"
  )
)

sink("tables/descLenders.tex")
kableExtra::kable(
  desc, "latex", 
  digits = 2, booktabs = T, 
  align = c("l", "c", "c", "c")
)
sink()

#### Figure SI-3 ####

bivariateRel <- function(var) {
  dfReg <- dfEst %>% select(registered, village) 
  dfReg$iv <- dfEst %>% pull(var)
  mod <- lm(registered ~ iv + village, data = dfReg)
  vc <- myVC(mod)
  pe <- coef(mod)["iv"]
  ci <- lmtest::coefci(mod, "iv", vcov. = vc)
  tibble(
    var = var, 
    pe = pe, 
    lb = ci[1], 
    ub = ci[2]
  )
}

pl <- map_dfr(c("female","age", "edu", "income","rlg","eth","leader","prosoc_dic","pol_index","head", "distance"), bivariateRel)
pl <- pl %>% mutate(var = recode(
  var, 
  rlg = "Catholic", 
  eth = "Lugbara", 
  prosoc_dic = "pro-sociality", 
  pol_index = "participation index", 
  distance = "distance (km)", 
  head = "household head", 
  edu = "2ary education"
)) %>% arrange(pe) %>% mutate(var = fct_inorder(var))

ggplot(pl, aes(x = var, y = pe, ymin = lb, ymax = ub)) + 
  geom_point() + 
  geom_linerange() + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dotted") + 
  labs(x = "Variable", y = "Effect on Pr(registration) (in percentage points)")
ggsave("figures/bivariate.pdf", width = 5, height = 2.5)


#### Table SI-1 ####

pctTopCoding <- function(gs) {
  topCoding <- gs %>% map_dfr(function(g) {
    degs <- degree(g, mode = "out")
    tibble(n = length(degs), n5 = sum(degs == 5))
  })
  topCoding %>% 
    summarize(n = sum(n), n5 = sum(n5)) %>% 
    mutate(pct = n5 / n) %>% 
    pull(pct)
}

dfTC <- ties %>% 
  filter(type %in% c("friend", "family", "lender", "solver")) %>% 
  group_by(i, type) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  bind_rows(
    df %>% filter(!i %in% ties$i) %>% select(i) %>% mutate(n = 0) %>% 
      mutate(j = 1) %>% 
      inner_join(tibble(j = 1, type = c("friend", "family", "lender", "solver"))) %>% 
      select(-j)
  ) %>%
  mutate(top = as.integer(n == 5)) %>% 
  group_by(type) %>% 
  summarize(n = n(), top = sum(top)) %>% 
  mutate(pct = top / n)


sink("tables/descTopCoding.tex")
tibble(
  Network = c("family", "friend", "lender", "solver"), 
  `Pct. top coding` = map_dbl(
    list(g$familyU, g$friendU, g$lenderU, g$solverU), 
    pctTopCoding
  )
) %>% kableExtra::kable(
  "latex", 
  digits = 3, booktabs = T, 
  align = c("l", "c")
)
sink()

#### Figure SI-2 ####

makeDeg <- function(gs, name) {
  dd <- lapply(gs, degree) %>% unlist
  data.frame(degree = dd, net = name)
}
pl <- rbind(
  makeDeg(g$friend, "Friend"), 
  makeDeg(g$family, "Family"), 
  makeDeg(g$lender, "Lender"), 
  makeDeg(g$solver, "Solver"), 
  makeDeg(g$union, "Union")
)
ggplot(pl, aes(x = degree, fill = net)) + 
  geom_density(alpha = .3) + 
  xlim(0, 40) + 
  labs(x = "Degree", fill = "Network")
ggsave("figures/degree_dist.pdf", 
       width = 5, height = 2.5)


#### Table SI-2 ####

inDegrees <- ties %>% 
  select(i, j, villageId) %>% 
  distinct() %>% 
  group_by(villageId, j) %>% 
  summarize(inDegree = n()) %>% 
  rename(i = j) %>% 
  ungroup() %>% 
  full_join(
    df %>% select(i, villageId) %>% mutate(interviewed = 1)
  ) %>% 
  replace_na(list(interviewed = 0, inDegree = 0)) %>% 
  inner_join(df %>% select(villageId, villageTxt) %>% distinct())

getInDegrees <- function(df, clusterSE) {
  mod <- lm(inDegree ~ interviewed, data = df)
  if(clusterSE) {
    se <- cluster.vcov(mod, ~ villageId)
  } else {
    se <- vcovHC(mod, type = "HC1")
  }
  out <- c(sum(coef(mod)), coef(mod)[1], coef(mod)[2])
  pval <- pvalToStars(coeftest(mod, se)[2,4])
  out <- paste0(round(out, 2), 
                c("", "", pval)
  )
  tibble(degInter = out[1], degNoInter = out[2], delta = out[3])
}

descDeg <- map_dfr(inDegrees %>% split(.$villageTxt), getInDegrees, clusterSE = F)
descDeg <- bind_rows(
  descDeg, 
  getInDegrees(inDegrees, clusterSE = T)
)


desc <- ties %>% 
  group_by(villageId) %>% 
  summarize(nInterview = length(unique(i)), 
            nMentioned = length(unique(c(i,j)))) %>% 
  inner_join(
    census1 %>% 
    select(villageId, villageTxt, pop = n)
  ) %>% 
  rename(village = villageTxt) %>% select(-villageId) %>% 
  arrange(village)
desc <- bind_rows(
  desc, 
  summarize(desc, 
            nInterview = sum(nInterview), 
            nMentioned = sum(nMentioned), 
            pop = sum(pop)) %>% 
    mutate(village = "All")
) %>% 
  mutate(pop = as.integer(pop), 
         pctMissing = round(1-(nInterview / nMentioned), 2), 
         pctPop = round(1 - (nInterview / pop), 2)) %>% 
  bind_cols(descDeg) %>% 
  select(village, everything())

colnames(desc) <- c("Village", "N interviewed", "N alters", "Adult population", 
                    "Pct. non-interviewed alters", 
                    "Pct. non-interviewed population", "In-degree interviewed", "In-degree non-interviewed", "$\\Delta$")

kableExtra::kable(desc, format = "latex", escape = F, booktabs = T, align = c("l", rep("c", 8)), linesep = "") %>% 
  write_file("tables/descNet.tex")

#### Table SI-3 ####

dfRep <- bind_rows(
  microCensus %>% 
    select(village = villageTxt, female, age, edu) %>% 
    mutate(sample = 0), 
  df %>% select(age, edu, female, village = villageTxt) %>% mutate(sample = 1)
)

sampleTest <- function(var) {
  df <- dfRep
  df$var <- df %>% pull(var)
  mod <- lm(var ~ sample, data = df)
  vc <- myVC(mod, vclus = df$village)
  out <- tibble(
    Variable = var, 
    `Sample mean` = sum(coef(mod)) %>% round(2), 
    `Census mean` = coef(mod)[1] %>% round(2), 
    Delta = coef(mod)[2] %>% round(2), 
    pval = coeftest(mod, vc)[2,4]
  ) %>% mutate(
    pval = map_chr(pval, pvalToStars), 
    Delta = paste0(Delta, pval)
  ) %>% select(-pval)
  out
}

desc <- map_dfr(c("age", "female", "edu"), sampleTest) %>% 
  rename(`$\\Delta$` = Delta)
desc <- desc %>% mutate(
  Variable = recode(Variable, female = "\\% female", age = "age", edu = "\\% 2ary education")
) %>% 
  select(Variable, everything()) %>% 
  arrange(Variable)
colnames(desc) <- c("Variable", "Sample mean", "Census mean", "$\\Delta$")

kableExtra::kable(desc, format = "latex", escape = F, booktabs = T, align = c("l",rep("c", 3)), linesep = "") %>% 
  write_file("tables/descSampling.tex")

#### Table SI-4 ####

mods$baseInterGl <- update(mods$base, . ~ . + degreeHH + pctRegHH + pctRegHH : age + pctRegHH : edu + pctRegHH : female)
mods$baseInterAge <- update(mods$base, . ~ . + degreeHH + pctRegHH + pctRegHH : age)
mods$baseInterEdu <- update(mods$base, . ~ . + degreeHH + pctRegHH + pctRegHH : edu)
mods$baseInterFemale <- update(mods$base, . ~ . + degreeHH + pctRegHH + pctRegHH : female)

modsVC$baseInterGl <- myVC(mods$baseInterGl)
modsVC$baseInterAge <- myVC(mods$baseInterAge)
modsVC$baseInterEdu <- myVC(mods$baseInterEdu)
modsVC$baseInterFemale <- myVC(mods$baseInterFemale)

sink("tables/modSampling.tex")
mystar(
  mods[38:41], vc = F, se = starSD(names(mods)[38:41]), 
  covariate.labels = c(
    regLabs, "degree HH", "\\% registered", 
    "age $\\times$ \\% registered",
    "secondary education $\\times$ \\% registered",
    "female $\\times$ \\% registered"
  )
)
sink()

#### Table SI-7 ####

mods$fullEdu <- update(mods$full, . ~ . - edu + edu_full)
mods$baseEdu <- update(mods$base, . ~ . - edu + edu_full)
mods$baseHeadEdu <- update(mods$baseHead, . ~ . - edu + edu_full)
mods$baseNoHeadEdu <- update(mods$baseNoHead, . ~ . - edu + edu_full)

modsVC$fullEdu <- myVC(mods$fullEdu)
modsVC$baseEdu <- myVC(mods$baseEdu)
modsVC$baseHeadEdu <- myVC(mods$baseHeadEdu)
modsVC$baseNoHeadEdu <- myVC(mods$baseNoHeadEdu)

sink("tables/modEdu.tex")
mystar(
  mods[42:45], vc = F, se = starSD(names(mods)[42:45]), 
  covariate.labels = c(
    regLabs[-5], 
    "some 1ary edu", 
    "some 2ary edu", 
    "some 3ary edu"
  ), 
  column.labels = c(
    "All", "All (2+)", "Head", "Non-Head"
  )
)
sink()

#### Figure 3 ####

gams <- list(
  base = mgcv::gam(registered ~ s(age) + village, data = model.frame(mods$base)), 
  controls = mgcv::gam(update(f, . ~ . - age - age2 + s(age)), data = dfEst)
)

x <- dfEst[-attr(model.frame(mods$base), "na.action"),]
ns <- nrow(x)
x <- map_dfr(18:70, ~ mutate(x, age = .))
x1 <- predict(gams$base, x, type = "lpmatrix")
x2 <- predict(gams$controls, x, type = "lpmatrix")
y1 <- as.numeric(rowsum(x1 %*% coef(gams$base), x$age)) / ns
y2 <- as.numeric(rowsum(x2 %*% coef(gams$controls), x$age)) / ns
ci1 <- rowsum(x1 %*% t(MASS::mvrnorm(1000, coef(gams$base), vcov(gams$base))), x$age) / ns
ci1 <- t(apply(ci1, 1, quantile, probs = c(.025, .975)))
ci2 <- rowsum(x2 %*% t(MASS::mvrnorm(1000, coef(gams$controls), vcov(gams$controls))), x$age) / ns
ci2 <- t(apply(ci2, 1, quantile, probs = c(.025, .975)))
pl <- tibble(
  model = rep(c("baseline", "controls"), length(18:70)) %>% sort(), 
  y = c(y1, y2), 
  age = rep(18:70, 2)
) %>% bind_cols(
  rbind(ci1, ci2) %>% as.data.frame()
)

ggplot(pl, aes(
  x = age, y = y, ymin = `2.5%`, ymax = `97.5%`, color = model, fill = model
)) + geom_line() + geom_ribbon(alpha = .2, color = NA) + 
  scale_color_grey() + 
  scale_fill_grey() + 
  theme(legend.position = "bottom") + 
  labs(
    x = "age", 
    y = "Pr(registered)"
  )
ggsave("figures/gam.pdf", width = 4, height = 4)

rm(x, x1, x2, y1, y2, ci1, ci2, pl, ns)


