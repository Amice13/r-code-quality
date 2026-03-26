#####################################################################
## Replication file for                                            ##
## It takes a village: Peer Effects and Externalities in           ##
##   Technology Adoption.                                          ##
## Authors: Romain Ferrali, Guy Grossman, Melina R Platas,         ##
##   Jonathan Rodden                                               ##
## June 1st, 2019                                                  ##
## Maintainer: Romain Ferrali (rferrali@nyu.edu)                   ##
#####################################################################


## INSTRUCTIONS
## This script was run on a Windows 10 PC, Intel Core i7. The analysis
## is time-consuming (4-6 hours), because it makes heavy uses of 
## bootstrapping and other numerical simulations. 
## We include a development and a production environment. The 
## development is meant for quick glancing at the code. It uses 
## analytical standard errors instead of bootstrapped ones, and has 
## simulations run for few iterations. The production environment 
## reproduces the analyses conducted in the paper. It is meant to 
## be executed in parallel on 8 threads. 
## The production environment is currently enabled. To enable 
## the development environment instead, switch the devel variable 
## below to TRUE. 

devel <- FALSE


#### loading packages ####

library(car)
library(flexmix)
library(rstanarm)
library(Matrix)
library(MASS) 
library(mgcv) 
library(readr)
library(ggplot2)
library(dplyr)
library(haven)
library(forcats) 
library(purrr)
library(tidyr)
library(tibble)
library(lubridate)
library(igraph) 
library(sandwich)
library(multiwayvcov)
library(lmtest)
library(MatchIt)
library(stargazer)
library(ggraph)
library(gridExtra)
library(knitr)
library(kableExtra)
library(stddiff)
library(optmatch)
library(cem)



#### Global parameters ####

source("functions.R")
set.seed(666) # the number of the beast
options(knitr.kable.NA = '')
theme_set(theme_bw(base_size = 8))
om <- paste0("(", 
             c("age", "female", "income", "edu", "phone", 
               "leader", "pol_index", 
               "attend", "prosoc_dic", "geo", "village", "Constant"), 
             ")")

mods <- list()
modsVC <- list()

if(!devel) {
  library(parallel)
  options(mc.cores = 4)
  cl <- makePSOCKcluster(8)
  options(boot.ncpus = 8)
  clusterSetRNGStream(cl, 666)
  nRepVc <- 1e4
  nRepRE <- 1e4
  nRepPermut <- 1e4
  nChainsRE <- 4
} else {
  cl <- F
  nRepRE <- 500
  nRepPermut <- 5
  nChainsRE <- 1
  nRepVc <- 1e3
}

if(!dir.exists("tables")) dir.create("tables")
if(!dir.exists("figures")) dir.create("figures")

#### read data #### 

villages <- read_csv("villages.csv")
census <- villages %>% 
  select(
    villageId, 
    starts_with("census_"), 
    starts_with("dist"), 
    starts_with("design_")
    ) %>% filter(!is.na(census_pop))
colnames(census) <- gsub("census_", "", colnames(census), fixed = T)
colnames(census) <- gsub("design_", "", colnames(census), fixed = T)
oldCensus <- villages %>% 
  select(
    villageId, 
    starts_with("ocensus_"), 
    starts_with("dist"), 
    starts_with("design_")
  )
colnames(oldCensus) <- gsub("ocensus_", "", colnames(oldCensus), fixed = T)
colnames(oldCensus) <- gsub("design_", "", colnames(oldCensus), fixed = T)
pg <- villages %>% 
  select(villageId, starts_with("pg_")) %>% 
  filter(!is.na(pg_pg))
colnames(pg) <- gsub("pg_", "", colnames(pg), fixed = T)
voting <- villages %>% 
  select(villageId, starts_with("elections_")) %>% 
  filter(!is.na(elections_winShare))
colnames(voting) <- gsub("elections_", "", colnames(voting), fixed = T)

nodes <- read_csv("nodes.csv")
ties <- read_csv("ties.csv", col_types = cols(dist = col_double()))
messages <- read_csv("messages.csv") %>% 
  mutate(month = floor_date(date, "month") %>% as_date())
microCensus <- read_csv("microCensus.csv")

pgSchools <- read_csv("schools.csv")

g <- makeGraphs(nodes, ties)

#### figure SI, 1: residuals of village selection regression ####

dfEst <- oldCensus %>% 
  mutate(commercial = as.integer(nCommercial > 0)) %>% 
  mutate_at(vars(pop, distArua, distHealth, distSchool, eth, noAgri, edu),
            ~ as.numeric(scale(.)))

mods$villageSelection <- lm(messagesPer100 ~ ctyMeet + diagMeet + pop + edu + noAgri + commercial + eth + distHealth + distSchool + distArua, data = dfEst)
modsVC$villageSelection <- myVC(mods$villageSelection, vclus = dfEst$cluster)

dfEst$prediction <- predict(mods$villageSelection)
dfEst$residual <- residuals(mods$villageSelection)

dfEst <- dfEst %>% arrange(residual) %>% 
  left_join(nodes %>% select(villageId) %>% distinct() %>% mutate(sampled = 1)) %>% 
  mutate(
    id = 1:nrow(.), 
    high = as.integer(id > (nrow(.)/2)), 
    high = ifelse(is.na(sampled), NA, high), 
    perf = case_when(
      id <= 10 ~ "low", 
      id > 10 & id < 77 ~ "medium", 
      id >= 77 ~ "high"
    ), 
    sampled = ifelse(is.na(high), "no", "yes")
  )

high <- dfEst %>% arrange(-residual) %>% select(villageId, high) %>% na.omit() %>% mutate(village = as.factor(LETTERS[1:nrow(.)]))
oldCensus <- oldCensus %>% left_join(high)
census <- census %>% left_join(high)
messages <- messages %>% left_join(high)
microCensus <- microCensus %>% inner_join(high)
pg <- pg %>% inner_join(high)
nodes <- nodes %>% inner_join(high)
ties <- ties %>% inner_join(high)
voting <- voting %>% inner_join(high)
g <- g %>% inner_join(high)


pl1 <- dfEst %>% 
  mutate(villageId = fct_inorder(as.factor(villageId))) %>% 
  ggplot(aes(x = residual, y = villageId, color = perf, shape = sampled)) + 
  geom_vline(xintercept = 0) + 
  geom_point() + 
  scale_color_grey() + 
  scale_y_discrete(breaks = NULL) + 
  theme(axis.text.y = element_blank()) + 
  guides(shape = F, color = F) + 
  labs(
    x = "Residual", 
    y = "Village", 
    title = "(a) Residuals"
  ) 

pl2 <- ggplot(dfEst, aes(x = prediction, y = messagesPer100, color = perf, shape = sampled)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, lty = "dotted") + 
  # xlim(-5,120) + ylim(-5, 120) + 
  scale_color_grey() + 
  labs(
    x = "Number of messages per 100 residents (predicted)", 
    y = "Number of messages per 100 residents (observed)", 
    color = "Residuals", 
    shape = "Sampled", 
    title = "(b) Observed and predicted value"
  ) 

pl <- grid.arrange(pl1, pl2, ncol = 2, widths = (3:4)/7)
ggsave("figures/residuals.pdf", pl, width = 6, height = 5)


#### table SI, 1: village selection ####

mystar(mods$villageSelection, 
       vc = F, 
       se = starSD("villageSelection"), 
       dep.var.caption = "Dependent variable: Number of messages per 100 inhabitants",  
       covariate.labels = c(
         "GAPP community meeting", "GAPP dialogue meeting", 
         "adult population", "pct. secondary education", "pct. non-agriculture", "trading center", 
         "ethnic fractionalization", "distance to health center", "distance to school", "distance to Arua"
       ), 
       out = "tables/villageSelection.tex")


#### figure 2:  message intensity over time #### 

pl <- messages %>% 
  filter(!is.na(villageId)) %>% 
  group_by(villageId, month) %>% 
  summarize(n = n()) %>% 
  ungroup()
pl <- complete(pl, villageId = census$villageId, month = seq.Date(min(pl$month), max(pl$month), "month"), fill = list(n = 0)) %>% 
  left_join(census %>% select(villageId, village, high, pop)) %>% 
  mutate(
    t = match(month, unique(sort(month))), 
    t2 = t^2, 
    nn = 100 * n / pop
  )

mods$mix <- flexmix(nn ~ t + t2 | villageId, data = pl, k = 2)
pl$cluster <- mods$mix@cluster - 1

pl1 <- pl %>% 
  filter(!is.na(village)) %>% 
  group_by(high, month) %>% 
  summarize(n = sum(n), pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(n = 100 * n / pop) %>% 
  mutate(panel = "Monthly")
pl1 <- bind_rows(
  pl1,
  pl1 %>%
    arrange(high, month) %>%
    group_by(high) %>%
    mutate(
      panel = "Cumulative",
      n = cumsum(n)
    ) %>% ungroup()
) %>% mutate(sample = "16 surveyed villages")

whichCl <- pl %>% 
  group_by(cluster, month) %>% 
  summarize(n = sum(n), pop = sum(pop)) %>% 
  group_by(cluster) %>% 
  summarize(n = sum(n) / unique(pop)) %>% 
  arrange(n) %>% 
  mutate(high = 0:1) %>% 
  select(cluster, high)
pl2 <- pl %>% 
  select(-high) %>% 
  inner_join(whichCl) %>% 
  group_by(high, month) %>% 
  summarize(n = sum(n), pop = sum(pop)) %>% 
  ungroup() %>% 
  mutate(n = 100 * n / pop) %>% 
  mutate(panel = "Monthly")
pl2 <- bind_rows(
  pl2,
  pl2 %>%
    arrange(high, month) %>%
    group_by(high) %>%
    mutate(
      panel = "Cumulative",
      n = cumsum(n)
    ) %>% ungroup()
) %>% mutate(sample = "All villages")

bind_rows(pl2, pl1) %>% 
  mutate(
    high = recode(high, `0` = "Low", `1` = "High"), 
    sample = fct_inorder(sample)
  ) %>% 
  ggplot(aes(x = month, y = n, color = high)) +
  geom_smooth(se = F) +
  facet_grid(panel ~ sample, scales = "free_y") +
  scale_color_manual(values = c("Low" = "grey50", "High" = "black")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b. %y") +
  labs(x = "", 
       y = "Number of relevant messages per 100 adult residents", 
       color = "Uptake") +
  theme(legend.position = "bottom")

ggsave("figures/MessagesAllOvertime.pdf", 
       width = 6, height = 4)

#### figure SI, 5: messages by topic and level ####

pl <- messages %>%
  filter(!level %in% c("Constituent Service", "National")) %>%
  group_by(topic, level) %>%
  summarize(n = n()) %>%
  group_by(topic) %>%
  mutate(count = sum(n)) %>% 
  filter(n > 5) %>% 
  ungroup() %>%
  mutate(
    topic = fct_reorder(topic, -count),
    level = fct_relevel(level, rev(c("District", "Village", "Private")))
  )

ggplot(pl, aes(x = topic, y = n, fill = level)) +
  geom_col() +
  scale_fill_manual(values = c(
    "District" = "black",
    "Village" = "grey50",
    "Private" = "grey75"
  )) + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) + 
  labs(
    x = "", y = "Number of messages", fill = "Level", 
    caption = sprintf("N = %s messages", scales::comma(sum(pl$n)))
  )
ggsave("figures/messagesLevels.pdf", height = 2.5, width = 6)


#### figure 1: number of relevant messages ####

pl <- messages %>% 
  filter(!is.na(villageId)) %>% 
  group_by(high, villageId) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  right_join(census %>% select(villageId, pop)) %>% 
  replace_na(list(n = 0)) %>% 
  mutate(n = 100 * n / pop) %>% 
  arrange(n) %>% 
  mutate(
    high = as.factor(high), 
    villageId = fct_inorder(as.factor(villageId))
  )

ggplot(pl, aes(x = n, y = villageId, yend = villageId)) + 
  geom_segment(xend = 0, color = "grey60") + 
  geom_point(data = pl %>% filter(!is.na(high)), aes(color = high)) + 
  scale_color_manual(
    values = c("0" = "grey40", "1" = "black"), 
    labels = c("0" = "Low", "1" = "High")
  ) + 
  scale_y_discrete(breaks = NULL) + 
  theme(
    axis.text.y = element_blank(), 
    legend.position = "bottom"
  ) + 
  labs(
    x = "Number of relevant messages per 100 adult residents", 
    y = "Village", 
    color = "Uptake", size = "Uptake"
  )
ggsave("figures/nMsgByVillage.pdf", width = 6, height = 4)

#### figure SI, 3: policy priorities across H and L villages ####

nodes %>% 
  select(priority, high) %>% 
  filter(!is.na(priority)) %>% 
  filter(priority != "Other [Please Specify]") %>% 
  rename(topic = priority) %>% 
  group_by(high, topic) %>% 
  summarize(pct = n()) %>% 
  group_by(high) %>% 
  mutate(pct = pct / sum(pct)) %>% 
  ungroup() %>% 
  arrange(-high, -pct) %>% 
  mutate(
    topic = recode(
      topic, 
      `Direct assistance to the poor (food, cash)` = "Assistance to the poor", 
      `Infrastructure, like roads and bridges` = "Infrastructure", 
      `Security, like the police and military` = "Security", 
      `Agricultural development` = "Agriculture", 
      `Access to clean water` = "Water"
    ), 
    topic = fct_inorder(fct_drop(topic)), 
    high = recode(high, `0` = "Low", `1` = "High")
  ) %>% 
  ggplot(aes(x = topic, y = pct, fill = high)) + 
  geom_col(position = position_dodge()) + 
  scale_fill_manual(values = c("Low" = "grey60", "High" = "black")) + 
  theme(legend.position = "bottom") + 
  labs(
    x = "", 
    y = "Percentage", 
    fill = "Uptake"
  )
ggsave("figures/surveyPriorities.pdf", height = 2.5, width = 6)


#### figure SI, 4: agreement over gov priorities ####

v <- match(nodes$priority, unique(nodes$priority))
bigG <- g %>% arrange(village) %>% pull(union) %>% 
  map(as_adj) %>% bdiag
bigVillage <- map(
  as.integer(table(nodes$village)),
  ~ Matrix(1, nrow = ., ncol = .)
) %>% bdiag()
diag(bigVillage) <- 0
bigG <- drop0(bigG)
bigVillage <- drop0(bigVillage)
bigG <- lowerTri2Vec(bigG)
bigVillage <- lowerTri2Vec(bigVillage)
big <- as.matrix(bigG + bigVillage) %>% as.integer()

doPermut <- function(i, v, big, shareAttr, lowerTri2Vec) {
  library(Matrix)
  library(dplyr)
  library(purrr)
  share <- sample(v, length(v))
  share <- shareAttr(share)
  share <- lowerTri2Vec(share)
  share <- as.matrix(share) %>% as.integer()
  tapply(share, big, mean)
}

if(devel) {
  permutationTest <- sapply(1:nRepPermut, doPermut, v = v, big = big, shareAttr = shareAttr, lowerTri2Vec = lowerTri2Vec)
} else {
  permutationTest <- parSapply(cl, 1:nRepPermut, doPermut, v = v, big = big, shareAttr = shareAttr, lowerTri2Vec = lowerTri2Vec)
}


share <- shareAttr(v)
share <- lowerTri2Vec(share)
share <- as.matrix(share) %>% as.integer()
share <- tapply(share, big, mean)
share <- tibble(
  value = share,
  type = c("across village", "within village", "neighbors (tie)")
) %>%
  mutate(type = fct_relevel(type, "neighbors (tie)", after = Inf))

pl <- as.tibble(t(permutationTest))
colnames(pl) <- c("across village", "within village", "neighbors (tie)")
pl %>%
  gather(type, value) %>%
  mutate(type = fct_relevel(type, "neighbors (tie)", after = Inf)) %>%
  ggplot(aes(value, fill = type)) +
  geom_density(alpha = .3, color = NA) +
  geom_vline(data = share, aes(xintercept = value, color = type)) +
  theme(legend.position = "bottom") +
  labs(x = "Probability of agreeing on top government priority",
       color = "Dyad type",
       fill = "Dyad type")
ggsave("figures/valence.pdf", width = 4, height = 4)

#### table 1: descriptive statistics ####

descStats <- function(var, label, df, census = F) {
  df <- df %>% select(c("high", "village", var)) %>% na.omit()
  colnames(df)[3] <- "var"
  means <- c(
    mean(df$var), 
    mean(df$var[df$high == 1]), 
    mean(df$var[df$high == 0])
  )
  means <- c(means, means[2] - means[3], NA, min(df$var), max(df$var))
  df$var <- df$var-3
  mod <- lm(var ~ high, data = df)
  if(!census) {
    vc <- myVC(mod, vclus = df$village)
  } else {
    vc <- vcovHC(mod, type = "HC1")
    if(is.integer(df$var)) {
      std <- stddiff.binary(as.data.frame(df), 1, 3)
    } else {
      std <- stddiff.numeric(as.data.frame(df), 1, 3)
    }
    means[5] <- std[1,"stddiff"]
    star <- ifelse(.25 > std[1,"stddiff.l"] & .25 < std[1,"stddiff.u"], "", "$^{*}$")
  }
  pval <- pvalToStars(coeftest(mod, vc)[2,4])
  means <- paste0(round(means, 2), c(rep("", 3), pval, star, "", ""))
  if(!census) {
    means[5] <- ""
  }
  out <- matrix(c(label, means), nrow = 1)
  out <- as.data.frame(out)
  colnames(out) <- c("Variable", "Sample", "High uptake", "Low uptake", "$\\Delta$", "Std. diff.", "min", "max")
  out
}

dfDesc <- nodes %>% mutate(
  degree = netCov(g$union, degree), 
  betweenness = netCov(g$union, betweenness, normalized = T), 
  clustering = netCov(g$union, transitivity, type = "local", isolates = "zero"), 
  prioEdu = as.integer(priority == "Education")
)

dfDescCensus <- census %>% filter(!is.na(high)) %>% 
  mutate(
    distSchool = distSchool / 1000, 
    distArua = distArua / 1000, 
    distHealth = distHealth / 1000
  ) %>% 
  inner_join(
    g %>% mutate(density = map_dbl(.$union, graph.density), 
                 mean_distance = map_dbl(.$union, mean_distance, directed = F), 
                 clusteringGl = map_dbl(.$union, transitivity, type = "global")) %>% 
      select(villageId, density, mean_distance, clusteringGl)
  ) %>% 
  inner_join(pg %>% select(pg, lg, village)) %>% 
  inner_join(voting %>% select(turnoutAdultPop, winShare, village)) %>% 
  mutate(diagMeet = as.integer(diagMeet), ctyMeet = as.integer(ctyMeet))


desc <- map2_dfr(
  c("adopt", "heard", "satisfaction", "age", "female", "income", "edu", 
    "hasPhone", "phone", "leader", "pol_index", "attend", "prosoc_dic", 
    "degree", "betweenness", "clustering", "prioEdu"), 
  c("\\% adopters", "\\% heard", "\\% satisfied", 
    "age", "\\% females", "income", "secondary education", 
    "\\% owns phone", "\\% use phone", "\\% leaders", 
    "political participation index", "\\% attend meeting", 
    "pro-sociality", "degree", "betweenness", "clustering coefficient", 
    "\\% education top priority"), 
  descStats, dfDesc
) %>% bind_rows(
  tibble(Variable = NA, 
         Sample = nrow(nodes), 
         `High uptake` = nodes %>% filter(high == 1) %>% nrow, 
         `Low uptake` = nodes %>% filter(high == 0) %>% nrow) %>% 
    mutate(`$\\Delta$` = `High uptake` - `Low uptake`) %>% 
    mutate_if(is.numeric, as.character)
) %>% 
  mutate(type = "A. Individuals", subtype = c(rep("Outcome", 3), rep("Individual", 10), rep("Network", 3), 
                                              rep("Preferences", 1), "$N$")) %>% 
  bind_rows(
    map2_dfr(
      c("ctyMeet", "diagMeet", 
        "density", "mean_distance", "clusteringGl", 
        "pop", "employ", "noAgri", "poverty", "eth", 
        "turnoutAdultPop", "winShare", 
        "distArua", "distHealth", "distSchool", 
        "pg", "lg"), 
      c("\\% community meeting", "\\% dialogue meeting", 
        "density", "path length", "global clustering", 
        "adult population", "\\% employed", "\\% non-agriculture", "ethnic fractionalization", "poverty score", 
        "LC5 Chair turnout", "share LC5 winner", 
        "dist. to Arua (km)", "dist. to health center (km)", "dist. to school (km)", 
        "public goods summary index", "local goods summary index"), 
      descStats, dfDescCensus, census = T) %>% 
      bind_rows(
        map2_dfr(
          c("V_teacher_absent_avg", "V_n_stud_perclass"), 
          c("teacher absenteeism", "students per class"), 
          descStats, 
          pgSchools %>% filter(time == 1) %>% select(schoolId, high, V_teacher_absent_avg, V_n_stud_perclass) %>% rename(village = schoolId), 
          census = T), 
        tibble(Variable = NA, 
               Sample = "16", `High uptake` = "9", 
               `Low uptake` = "7", `$\\Delta$` = "2")
      ) %>% 
      mutate(type = "B. Villages", subtype = c(rep("Design", 2), rep("Network", 3), rep("Population", 5), 
                                               rep("Politics", 2), 
                                               rep("Distances", 3), rep("Public goods", 4), "$N$"))
  )  %>% 
  select(type, subtype, everything())

kable(desc, format = "latex", escape = F, align = c("l", "l", "l", rep("c", 6)), booktabs = T, linesep = "") %>% 
  collapse_rows(1:2, row_group_label_position = 'stack', latex_hline = "custom", custom_latex_hline = 1:2, headers_to_remove = 1:2) %>% 
  write_file("tables/desc.tex")


#### table 3: descriptive stats about posteriors #### 

descPrior <- function(var, label, df) {
  df <- df %>% select(c("high", "village", var)) %>% na.omit()
  colnames(df)[3] <- "var"
  means <- c(
    mean(df$var), 
    mean(df$var[df$high == 1]), 
    mean(df$var[df$high == 0])
  )
  means <- c(means, means[2] - means[3])
  df$var <- df$var-3
  mods <- list(
    mod1 = lm(var ~ 1, data = df), 
    mod2 = lm(var ~ 1, data = df, subset = high == 1), 
    mod3 = lm(var ~ 1, data = df, subset = high == 0), 
    mod4 = lm(var ~ high, data = df)
  )
  clusterVars <- list(df$village, df$village[df$high == 1], df$village[df$high == 0], df$village)
  vcs <- map2(mods, clusterVars, ~ myVC(.x, vclus = .y))
  pvals <- map2_dbl(mods, vcs, ~ coeftest(.x, .y)[1,4])
  pvals[4] <- coeftest(mods$mod4, vcs$mod4)[2,4]
  pvals <- pvalToStars(pvals)
  means <- paste0(round(means, 2), pvals)
  out <- matrix(c(label, means), nrow = 1)
  out <- as.data.frame(out)
  colnames(out) <- c("Variable", "Sample", "High uptake", "Low uptake", "$\\Delta$")
  out
}

desc <- map2_dfr(
  c("post_responsive", "post_responsive_dyna", "post_capacity", "post_capacity_dyna", 
    "post_assets_edu", "post_assets_health", "post_assets_water", "post_assets_roads"), 
  c("Government responsiveness", "$\\Delta$ Government responsiveness", "Government capacity", "$\\Delta$ Government capacity", 
    "Quality of education", "Quality of health clinics", "Quality of access to water", "Quality of roads"), 
  descPrior, df = nodes
)

kable(desc, format = "latex", escape = F, align = c("l", "c", "c", "c", "c"), booktabs = T, linesep = "") %>% 
  write_file("tables/descPosterior.tex")


#### table SI, 6: balance: public goods #### 

pgTable <- function(var, label, df) {
  df <- df %>% select(c("high", "village", var)) %>% na.omit()
  colnames(df)[3] <- "var"
  means <- c(
    mean(df$var), 
    mean(df$var[df$high == 1]), 
    mean(df$var[df$high == 0])
  )
  if(is.integer(df$var)) {
    std <- stddiff.binary(as.data.frame(df), 1, 3)
  } else {
    std <- stddiff.numeric(as.data.frame(df), 1, 3)
  }
  star <- ifelse(.25 > std[1,"stddiff.l"] & .25 < std[1,"stddiff.u"], "", "$^{*}$")
  means <- c(means, means[2] - means[3], std[1,"stddiff"])
  mod <- lm(var ~ high, data = df)
  vc <- vcovHC(mod, type = "HC1")
  pval <- coeftest(mod, vc)[2,4]
  pval <- pvalToStars(pval)
  pval <- c("", "", "", pval, star)
  means <- paste0(round(means, 2), pval)
  if(is.na(std[1,"stddiff"])) {
    means[5] <- "n.d."
  }
  out <- matrix(c(label, means), nrow = 1)
  out <- as.data.frame(out)
  colnames(out) <- c("Variable", "Sample", "High uptake", "Low uptake", "$\\Delta$", "Std. diff.")
  out
}
pg <- pg %>% 
  mutate_at(
    vars(elec, roadtype, nursery, govprim, govhealth, center, biergarten, market, market_crops, field, instruments, mtn, orange, airtel), 
    as.integer
  )

desc <- map2_dfr(
  c("pg", "elec", "roadtype", "nursery", "govprim", "govhealth", "watersource", "toilets", 
    "lg", "savingsgroup", "farmersgroup", "center", "biergarten", "market", "market_crops", "field", "dryspace", "instruments", "bicycle", "church"), 
  c("Public goods summary index", "Working electricity grid in village", "Road accessible during all seasons", 
    "Nursery school in village", "Government primary school in village", "Government health facility in village", 
    "Number of functional water sources", "Number of functional public toilets/latrines", 
    "Local goods summary index", "Number of savings/community savings groups", "Number of functional farmers groups and cooperatives", 
    "Community center (with physical structure)", "Community bar (drinking establishment)", "General market located within village", 
    "Market place for crops in village", "Community playing field within village", "Number of community drying spaces", 
    "Community instruments (e.g. musical, kitchenware)", "Number of community bicycles", "Number of churches in village"), 
  pgTable, df = pg
)

kable(desc, format = "latex", escape = F, align = c("l", "c", "c", "c", "c"), booktabs = T, linesep = "") %>% 
  group_rows(index = c("Public goods" = 8, "Local goods" = 12)) %>% 
  write_file("tables/balance_pg.tex")


#### table SI, 7: balance: school performance #### 

pgPrePost <- function(var, label, df) {
  df <- df %>% select(c("schoolId", "high", "time", var))
  colnames(df)[4] <- "var"
  df$time <- as.integer(df$time == 3)
  means <- c(
    mean(df$var[df$high == 1 & df$time == 0], na.rm = T), 
    mean(df$var[df$high == 1 & df$time == 1], na.rm = T), 
    mean(df$var[df$high == 0 & df$time == 0], na.rm = T), 
    mean(df$var[df$high == 0 & df$time == 1], na.rm = T)
  )
  means <- c(means, means[2]-means[1], means[4]-means[3])
  means <- c(means, means[6]-means[5])
  means <- means[c(1,2,5,3,4,6,7)]
  if(is.na(means[2])) {
    means <- as.character(round(means,2))
    means[c(2,3,5,6,7)] <- "\\_"
  } else {
    dfm3 <- df %>% spread(time, var) %>% mutate(delta = `1` - `0`)
    mods <- list(
      lm(var ~ time, data = df, subset = high == 1), 
      lm(var ~ time, data = df, subset = high == 0), 
      lm(delta ~ high, data = dfm3)
    )
    vcs <- map(mods, ~ vcovHC(., "HC1"))
    pvals <- map2_dbl(mods, vcs, ~ coeftest(.x, .y)[2,4])
    pvals <- pvalToStars(pvals)
    pvals <- c("", "", pvals[1], "", "", pvals[2], pvals[3])
    means <- as.character(round(means,2))
    means <- paste0(means, pvals)
  }
  out <- matrix(c(label, means), nrow = 1)
  out <- as.data.frame(out)
  colnames(out) <- c("Variable", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "$\\Delta$")
  out
}

desc <- map2_dfr(
  c("V_teacher_absent_avg", "V_n_stud_perclass", "teach_per_stud", "V_stud_uniform", 
    "V_stud_book", "V_stud_material", "A_PLE_passrate", "A_PLE_Grade1rate", "A_PLE_Grade2rate"), 
  c("Teacher absenteeism", "Students per class", "Student teacher ratio", "Student uniform ratio", 
    "Student book ratio", "Student material ratio", "PLE pass rate", "PLE Grade 1 \\%", "PLE Grade 2 \\%"), 
  pgPrePost, df = pgSchools
) %>% bind_rows(
  tibble(Variable = "$N$ schools", 
         `(1)` = pgSchools %>% filter(time == 1 & high == 1) %>% nrow(), 
         `(2)` = pgSchools %>% filter(time == 1 & high == 1) %>% nrow(), 
         `(4)` = pgSchools %>% filter(time == 1 & high == 1) %>% nrow(), 
         `(5)` = pgSchools %>% filter(time == 1 & high == 1) %>% nrow(), 
         `(3)` = 0, `(6)` = 0, `$\\Delta$` = 0) %>% 
    mutate_if(is.numeric, as.character)
)

kable(desc, format = "latex", escape = F, align = c("l", "c", "c", "c", "c", "c", "c", "c"), booktabs = T, linesep = "") %>% 
  add_header_above(c(" ", "$t = 0$" = 1, "$t = 1$" = 1, "(2) - (1)" = 1, "$t = 0$" = 1, "$t = 1$" = 1, "(5) - (4)" = 1, "(3) - (6)" = 1), escape = F) %>% 
  add_header_above(c(" ", "High-uptake" = 3, "Low-uptake" = 3), escape = F) %>% 
  write_file("tables/balance_school.tex")

#### table SI, 8: balance: voting behavior ####

desc <- map2_dfr(
  c("turnoutRegistered", "turnoutAdultPop", "winShare"), 
  c("LC5 Chair Turnout of Registered", "LC5 Chair Turnout", "Share LC5 winner"), 
  pgTable, df = voting
)
kable(desc, format = "latex", escape = F, align = c("l", "c", "c", "c", "c", "c"), booktabs = T, linesep = "") %>% 
  write_file("tables/balance_vote.tex")


#### table SI, 9: descriptive of meeting attendees ####

desc <- map2_dfr(
  c("adopt", "satisfaction", "age", "female", "income", "edu", 
    "phone", "leader", "pol_index", "prosoc_dic", 
    "degree", "betweenness", "clustering"), 
  c("\\% adopters", "\\% satisfied", 
    "age", "\\% females", "income", "secondary education", 
    "\\% use phone", "\\% leaders", 
    "political participation index", 
    "pro-sociality", "degree", "betweenness", "clustering coefficient"), 
  descStats, dfDesc %>% filter(attend == 1)
) %>% bind_rows(
  tibble(Variable = NA, 
         Sample = nrow(nodes %>% filter(attend == 1)), 
         `High uptake` = nodes %>% filter(high == 1 & attend == 1) %>% nrow, 
         `Low uptake` = nodes %>% filter(high == 0 & attend == 1) %>% nrow) %>% 
    mutate(`$\\Delta$` = `High uptake` - `Low uptake`) %>% 
    mutate_if(is.numeric, as.character)
) 


kable(desc, format = "latex", escape = "F", align = c("l", rep("c", 6)), booktabs = T, linesep = "") %>% 
  group_rows(index = c("Outcome" = 2, "Individual" = 8, "Network" = 3, "$N$" = 1), escape = F) %>% 
  write_file("tables/descAtt.tex")

#### table SI, 10: balance: phone ####

desc <- bind_rows(
  map2_dfr(
    c("hasPhone", "phone",  "usePhoneCall", "usePhoneText"), 
    c("\\% own phone", "\\% has used phone for call or text in past month", 
      "\\% has used phone for call in past month", "\\% has used phone for text in past month"), 
    descStats, nodes
  ) %>% select(-min, -max), 
  map2_dfr(
    c("airtel", "mtn", "orange"), 
    c("\\% villages with Airtel coverage", "\\% villages with MTN coverage", "\\% villages with Orange coverage"), 
    pgTable, pg
  )
)

kable(desc, format = "latex", escape = "F", align = c("l", rep("c", 5)), booktabs = T, linesep = "") %>% 
  group_rows(index = c("Phone use" = 4, "Network coverage" = 3), escape = F) %>% 
  write_file("tables/descPhone.tex")



#### table SI, 2: top coding in network ties elicitation ####

topCoding <- ties %>% 
  filter(type %in% c("friend", "family", "lender", "solver")) %>% 
  group_by(villageId, i, type) %>% 
  summarize(n = n()) %>% 
  ungroup() %>% 
  bind_rows(
    nodes %>% filter(!i %in% ties$i) %>% select(villageId, i) %>% mutate(n = 0) %>% 
      mutate(j = 1) %>% 
      inner_join(tibble(j = 1, type = c("friend", "familty", "lender", "solver"))) %>% 
      select(-j)
  ) %>%
  mutate(top = as.integer(n == 5)) %>% 
  inner_join(g %>% select(villageId, high)) %>% 
  rename(village = villageId)


topCodingTest <- function(ty) {
  df <- topCoding %>% filter(type == ty)
  mod <- lm(top ~ high, data = df)
  vc <- myVC(mod, vclus = df$village)
  out <- c(
    mean(df$top), 
    sum(coef(mod)), 
    coef(mod)[1], 
    coef(mod)[2]
  )
  pval <- pvalToStars(coeftest(mod, vc)[2,4])
  out <- paste0(round(out, 2), c("", "", "", pval))
  tibble(
    type = ty, 
    all = out[1], 
    high = out[2], 
    low = out[3], 
    diff = out[4]
  )
}

desc <- map_dfr(c("family", "friend", "lender", "solver"), topCodingTest)
colnames(desc) <- c("Network", "Sample", "High uptake", "Low uptake", "$\\Delta$")
kable(desc, format = "latex", escape = F, booktabs = T, align = c("l", rep("c", 4)), linesep = "") %>% 
  write_file("tables/descTopCoding.tex")


#### table SI, 3: network sampling ####

inDegrees <- ties %>% 
  select(i, j, village) %>% 
  distinct() %>% 
  group_by(village, j) %>% 
  summarize(inDegree = n()) %>% 
  rename(i = j) %>% 
  ungroup() %>% 
  full_join(
    nodes %>% select(i, village) %>% mutate(interviewed = 1)
  ) %>% 
  replace_na(list(interviewed = 0, inDegree = 0)) %>% 
  inner_join(nodes %>% select(village, high) %>% distinct)

getInDegrees <- function(df, clusterSE) {
  mod <- lm(inDegree ~ interviewed, data = df)
  if(clusterSE) {
    se <- cluster.vcov(mod, ~ village)
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

descDeg <- map_dfr(inDegrees %>% split(.$village), getInDegrees, clusterSE = F)
descDeg <- bind_rows(
  descDeg, 
  getInDegrees(inDegrees %>% filter(high == 1), clusterSE = T), 
  getInDegrees(inDegrees %>% filter(high == 0), clusterSE = T), 
  getInDegrees(inDegrees, clusterSE = T)
)


desc <- ties %>% 
  group_by(village) %>% 
  summarize(nInterview = length(unique(i)), 
            nMentioned = length(unique(c(i,j)))) %>% 
  inner_join(census %>% select(village, pop)) %>% 
  inner_join(nodes %>% select(village, high) %>% distinct())
desc <- bind_rows(
  desc, 
  summarize(desc %>% filter(high == 1), 
            nInterview = sum(nInterview), 
            nMentioned = sum(nMentioned), 
            pop = sum(pop)) %>% 
    mutate(village = "High-uptake", high = 1), 
  summarize(desc %>% filter(high == 0), 
            nInterview = sum(nInterview), 
            nMentioned = sum(nMentioned), 
            pop = sum(pop)) %>% 
    mutate(village = "Low-uptake", high = 0), 
  summarize(desc, 
            nInterview = sum(nInterview), 
            nMentioned = sum(nMentioned), 
            pop = sum(pop)) %>% 
    mutate(village = "All", high = NA)
) %>% 
  mutate(pop = as.integer(pop), 
         pctMissing = round(1-(nInterview / nMentioned), 2), 
         pctPop = round(1 - (nInterview / pop), 2), 
         high = recode(high, `0` = "Low", `1` = "High")) %>% 
  replace_na(list(high = "All")) %>% 
  bind_cols(descDeg) %>% 
  select(-high) %>% 
  select(village, everything())

colnames(desc) <- c("Village", "N interviewed", "N alters", "Adult population", 
                    "Pct. non-interviewed alters", 
                    "Pct. non-interviewed population", "In-degree interviewed", "In-degree non-interviewed", "$\\Delta$")

kable(desc, format = "latex", escape = F, booktabs = T, align = c("l", rep("c", 8)), linesep = "") %>% 
  group_rows(index = c("High-uptake" = 9, "Low-uptake" = 7, "Pooled" = 3)) %>% 
  write_file("tables/descNet.tex")

#### table SI, 4: comparing network to census ####

dfRep <- bind_rows(
  microCensus %>% mutate(sample = 0), 
  nodes %>% select(age, edu, female, village, high) %>% mutate(sample = 1)
)

sampleTest <- function(var) {
  df <- dfRep
  df$var <- df %>% pull(var)
  mods <- list(
    lm(var ~ sample, data = df, subset = high == 0), 
    lm(var ~ sample, data = df, subset = high == 1), 
    lm(var ~ sample, data = df)
  )
  clus <- list(
    df$village[df$high == 0], 
    df$village[df$high == 1], 
    df$village 
  )
  vc <- map2(mods, clus, ~ myVC(.x, vclus = .y))
  out <- tibble(
    Variable = var, 
    Uptake = c("Low", "High", "All"), 
    `Sample mean` = map_dbl(mods, ~ sum(coef(.))) %>% round(2), 
    `Census mean` = map_dbl(mods, ~ coef(.)[1]) %>% round(2), 
    Delta = map_dbl(mods, ~ coef(.)[2]) %>% round(2), 
    pval = map2_dbl(mods, vc, ~ coeftest(.x, .y)[2,4])
  ) %>% mutate(
    pval = map_chr(pval, pvalToStars), 
    Delta = paste0(Delta, pval)
  ) %>% select(-pval)
  out
}

desc <- map_dfr(c("age", "female", "edu"), sampleTest) %>% 
  rename(`$\\Delta$` = Delta)
desc <- desc %>% mutate(
  Variable = recode(Variable, female = "\\% female", age = "Age", edu = "secondary education"), 
  Uptake = fct_inorder(Uptake)
) %>% 
  select(Uptake, Variable, everything()) %>% 
  arrange(Uptake, Variable)
colnames(desc) <- c("Uptake", "Variable", "Sample mean", "Census mean", "$\\Delta$")

kable(desc, format = "latex", escape = F, booktabs = T, align = c("l", "l" ,rep("c", 3)), linesep = "") %>% 
  write_file("tables/descSampling.tex")

#### figure SI, 2: degree distribution by network type ####

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

#### table 2: main result, adoption of ubridge ####


dfEst <- nodes %>% mutate(
  degree = netCov(g$union, degree), 
  nAdopt = netTreat("adopt", "union", g), 
  pctAdopt = netTreat("adopt", "union", g, rowNormalize = T), 
  geo = netTreat("adopt", "geo", g, weight = "w"), 
  nSat = netTreat("satisfaction", "union", g), 
  isSatBin = as.integer(nSat > 0)
)
dfEst$nSpeak <- netCov(g$speak, degree, mode = "in")
dfEst <- dfEst %>% 
  mutate(pctSpeak = nSpeak / degree) %>% 
  replace_na(list(pctSpeak = 0))

f <- formula(adopt ~ age + female + income + edu + phone + leader + 
               pol_index + attend + prosoc_dic + village + geo + degree + nAdopt + nAdopt:high)

mods$base <- lm(f, data = dfEst)
mods$parsim <- update(mods$base, . ~ village + degree + nAdopt + nAdopt:high, data = mods$base$model)
mods$baseF <- update(mods$base, . ~ . - nAdopt -nAdopt:high + pctAdopt + pctAdopt:high)
mods$parsimF <- update(mods$parsim, . ~ . - nAdopt -nAdopt:high + pctAdopt + pctAdopt:high, data = mods$baseF$model)

modsVC$base <- myVC(mods$base)
modsVC$parsim <- myVC(mods$parsim)
modsVC$baseF <- myVC(mods$baseF)
modsVC$parsimF <- myVC(mods$parsimF)

sink("tables/mod.tex")
mystar(mods$parsim, 
       mods$base, 
       mods$parsimF, 
       mods$baseF, 
       vc = F, 
       se = starSD(c("parsim", "base", "parsimF", "baseF")), 
       order = c("(nAdopt)", "(pctAdopt)","(degree)"),
       omit = om, 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = rep(c("Parsimonious",
                             "Baseline"), 2),
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)", "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)",  
                            "\\% adopting neighbors ($\\beta_1$)", "\\% adopting neighbors $\\times$ high-uptake ($\\beta_2$)",  
                            "degree"),
       add.lines = list(
         c(beta12(c("parsim", "base")), beta12(c("parsimF", "baseF"), c("pctAdopt", "pctAdopt:high"))[-1]), 
         c("Controls", "\\_", "\\checkmark", "\\_", "\\checkmark")))
sink()


#### table SI, 5: sensitivity to peer-effects for subpopulations ####

mods$baseInterGl <- update(mods$base, . ~ . - high - nAdopt:high + nAdopt:age + nAdopt:edu + nAdopt:female)
mods$baseInterAge <- update(mods$base, . ~ . - high - nAdopt:high  + nAdopt:age)
mods$baseInterEdu <- update(mods$base, . ~ . - high - nAdopt:high  + nAdopt:edu)
mods$baseInterFemale <- update(mods$base, . ~ . - high - nAdopt:high + nAdopt:female)

modsVC$baseInterGl <- myVC(mods$baseInterGl)
modsVC$baseInterAge <- myVC(mods$baseInterAge)
modsVC$baseInterEdu <- myVC(mods$baseInterEdu)
modsVC$baseInterFemale <- myVC(mods$baseInterFemale)

mystar(
  mods$baseInterGl, mods$baseInterAge, mods$baseInterEdu, mods$baseInterFemale, 
  vc = F, 
  se = starSD(c("baseInterGl", "baseInterAge", "baseInterEdu", "baseInterFemale")), 
  dep.var.caption = "Dependent variable: adopt", 
  order = c("^nAdopt$", "(degree)", "^age$", "^edu$", "^female$"),
  omit = c("(income)", "(phone)", "(leader)", 
           "(pol_index)",  "(attend)", "(prosoc_dic)",
           "(geo)", "(village)"),
  covariate.labels = c(
    "\\# adopting neighbors", "degree",
    "age", "secondary education", "female",
    "age $\\times$ \\# adopting neighbors",
    "secondary education $\\times$ \\# adopting neighbors",
    "female $\\times$ \\# adopting neighbors"
  ),
  add.lines = list(c("Controls", rep("\\checkmark", 4))),
  out = "tables/modSampling.tex"
)

#### figure 6: selection model ####

dfEst$nHeard <- netTreat("heard", "union", g)

mods$selection1 <- glm(update(f, heard ~ . - nAdopt -nAdopt:high - attend + nHeard + nHeard:high), dfEst, family = binomial)
mods$selection2 <- glm(f, dfEst, family = binomial, subset = heard == 1)
mods$selectionR <- glm(f, dfEst, family = binomial)

modsVC$selection1 <- myVC(mods$selection1)
modsVC$selection2 <- myVC(mods$selection2)
modsVC$selectionR <- myVC(mods$selectionR)

pl <- AMEselection("selection1", "selection2", "selectionR") %>% 
  mutate(Uptake = recode(Uptake, `0` = "Low", `1` = "High"))



ggplot(pl, aes(x = Effect, y = AME, ymin = LB, ymax = UB, color = Uptake)) + 
  geom_point(position = position_dodge(.5)) + 
  geom_linerange(position = position_dodge(.5)) + 
  geom_linerange(aes(ymin = LB90, ymax = UB90), lwd = 1, position = position_dodge(.5)) + 
  coord_flip() + 
  geom_hline(yintercept = 0, lty = "dotted") + 
  scale_color_manual(values = c("Low" = "grey66", "High" = "black")) + 
  labs(y = "Average Marginal Effect of an additional adopting neighbor")
ggsave("figures/learning_vs_influence3.pdf", width = 4.5, height = 2)


#### table SI, 23: two-stage selection model ####

sink("tables/selection.tex")
mystar(mods$selection1, 
       mods$selection2, 
       mods$selectionR, 
       vc = F, 
       se = starSD(c("selection1", "selection2", "selectionR")),
       keep.stat = c("n", "aic"),
       order = c("(nHeard)", "(nAdopt)","(degree)"),
       dep.var.labels = c("heard", "adopt"), 
       dep.var.labels.include = T,
       omit = om, 
       column.labels = c(
         "First stage", "Second stage", "Reduced form"
       ),
       covariate.labels = c(
         "\\# hearing neighbors ($\\beta_1$)", 
         "\\# hearing neighbors $\\times$ high-uptake ($\\beta_2$)", 
         "\\# adopting neighbors ($\\beta_1$)", 
         "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
         "degree"
       ),
       add.lines = list(
         c("Controls", rep("\\checkmark", 6)), 
         c(beta12("selection1", c("nHeard", "nHeard:high")), 
           beta12(c("selection2", "selectionR"))[-1])
       )) 
sink()


#### table SI, 12: main specification, varying threshold for adoption ####

dfs <- list(
  df3 = dfEst %>% mutate(
    adopt = as.integer(nSent >= 3), 
    nAdopt = netTreat("adopt3", "union", g), 
    geo = netTreat("adopt3", "geo", g, weight = "w")
  ), 
  df5 = dfEst %>% mutate(
    adopt = as.integer(nSent >= 5), 
    nAdopt = netTreat("adopt5", "union", g), 
    geo = netTreat("adopt5", "geo", g, weight = "w")
  )
)

mods$baseT3 <- lm(f, data = dfs$df3)
mods$baseT5 <- lm(f, data = dfs$df5)
mods$parsimT3 <- update(mods$baseT3, . ~ village + degree + nAdopt + nAdopt:high, data = mods$baseT3$model)
mods$parsimT5 <- update(mods$baseT3, . ~ village + degree + nAdopt + nAdopt:high, data = mods$baseT5$model)

modsVC$baseT3 <- myVC(mods$baseT3)
modsVC$baseT5 <- myVC(mods$baseT5)
modsVC$parsimT3 <- myVC(mods$parsimT3)
modsVC$parsimT5 <- myVC(mods$parsimT5)

sink("tables/modThreshold.tex")
mystar(mods$parsim, 
       mods$parsimT3, 
       mods$parsimT5,
       mods$base,
       mods$baseT3,
       mods$baseT5,
       vc = F, 
       se = starSD(c("parsim", "parsimT3", "parsimT5", "base", "baseT3", "baseT5")), 
       order = c("(nAdopt)", "(degree)"), 
       omit = om, 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = rep(sprintf("t = %s", c(1,3,5)), 2),
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)", "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", "degree"),
       add.lines = list(
         beta12(c("parsim", "parsimT3", "parsimT5", "base", "baseT3", "baseT5")), 
         c("Controls", rep("\\_", 3), rep("\\checkmark", 3))
       ))
sink()

#### table SI, 13: main specification, logistic regression ####

mods$parsimLog <- glm(formula(mods$parsim), family = binomial, data = model.frame(mods$parsim))
mods$baseFLog <- glm(formula(mods$baseF), family = binomial, data = model.frame(mods$baseF))
mods$parsimFLog <- glm(formula(mods$parsimF), family = binomial, data = model.frame(mods$parsimF))

modsVC$parsimLog <- myVC(mods$parsimLog)
modsVC$baseFLog <- myVC(mods$baseFLog)
modsVC$parsimFLog <- myVC(mods$parsimFLog)

sink("tables/modLogistic.tex")
mystar(mods$parsimLog, 
       mods$selectionR, 
       mods$parsimFLog, 
       mods$baseFLog, 
       vc = F, 
       se = starSD(c("parsimLog", "selectionR", "parsimFLog", "baseFLog")), 
       dep.var.caption = "Dependent variable: adopt", 
       omit = om, 
       order = c("(nAdopt)", "(pctAdopt)","(degree)"), 
       column.labels = rep(c("Parsimonious", "Baseline"), 2),
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)", "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)",
                            "\\% adopting neighbors ($\\beta_1$)", "\\% adopting neighbors $\\times$ high-uptake ($\\beta_2$)",
                            "degree"),
       add.lines = list(
         c(beta12(c("parsimLog", "selectionR")), beta12(c("parsimFLog", "baseFLog"), c("pctAdopt", "pctAdopt:high"))[-1]), 
         c("Controls", rep(c("\\_", "\\checkmark"), 2))
       ), 
       keep.stat = c("n", "aic"), 
       font.size = "scriptsize")
sink()


#### table SI, 14: main specification, village A excluded ####

for(mod in c("parsim", "base", "parsimF", "baseF")) {
  modName <- sprintf("noA%s", mod)
  mods[[modName]] <- update(mods[[mod]], subset = village != "B")
  modsVC[[modName]] <- myVC(mods[[modName]])
}

sink("tables/modNoB.tex")
mystar(mods$noAparsim, 
       mods$noAbase, 
       mods$noAparsimF, 
       mods$noAbaseF, 
       vc = F, 
       se = starSD(c("noAparsim", "noAbase", "noAparsimF", "noAbaseF")), 
       order = c("(nAdopt)", "(pctAdopt)","(degree)"), 
       omit = om, 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = rep(c("Parsimonious", "Baseline"), 2),
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)", "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)",  
                            "\\% adopting neighbors ($\\beta_1$)", "\\% adopting neighbors $\\times$ high-uptake ($\\beta_2$)",  
                            "degree"),
       add.lines = list(
         c(beta12(c("noAparsim", "noAbase")), beta12(c("noAparsimF", "noAbaseF"), c("pctAdopt", "pctAdopt:high"))[-1]), 
         c("Controls", rep(c("\\_", "\\checkmark"), 2))
       ), 
       font.size = "scriptsize")
sink()


#### table SI, 15: main specification, directed ties ####

dfEst$degreeIn <- netCov(g$unionD, degree, mode = "in")
dfEst <- dfEst %>% mutate(
  nAdoptD = netTreat("adopt", "unionD", g, inTies = T), 
  nSatD = netTreat("satisfaction", "unionD", g, inTies = T), 
  pctAdoptD = nAdoptD / degreeIn, 
  isSatBinD = as.integer(nSatD > 0), 
  pctSpeakD = nSpeak / degreeIn
)


mods$ubase <- update(mods$base, . ~ . - degree - nAdopt - nAdopt:high + degreeIn + nAdoptD + nAdoptD:high)
mods$uparsim <- update(mods$ubase, 
                       . ~ village + degreeIn + nAdoptD + nAdoptD:high, data = mods$ubase$model)

mods$ubaseF <- update(mods$base, . ~ . -degree - nAdopt -nAdopt:high + degreeIn + pctAdoptD + pctAdoptD:high)
mods$uparsimF <- update(mods$uparsim, 
                        . ~ . - nAdoptD -nAdoptD:high + pctAdoptD + pctAdoptD:high, 
                        data = mods$ubaseF$model)

modsVC$ubase <- myVC(mods$ubase)
modsVC$uparsim <- myVC(mods$uparsim)
modsVC$ubaseF <- myVC(mods$ubaseF)
modsVC$uparsimF <- myVC(mods$uparsimF)

sink("tables/modDir.tex")
mystar(mods$uparsim, 
       mods$ubase, 
       mods$uparsimF, 
       mods$ubaseF, 
       vc = F, 
       se = starSD(c("uparsim", "ubase", "uparsimF", "ubaseF")), 
       omit = om, 
       order = c("(nAdoptD)", "(pctAdoptD)","(degree)"), 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = rep(c("Parsimonious", "Baseline"), 2),
       covariate.labels = c("\\# adopting in-neighbors ($\\beta_1$)", "\\# adopting in-neighbors $\\times$ high-uptake ($\\beta_2$)",
                            "\\% adopting in-neighbors ($\\beta_1$)", "\\% adopting in-neighbors $\\times$ high-uptake ($\\beta_2$)",
                            "degree"),
       add.lines = list(
         c(beta12(c("uparsim", "ubase"), c("nAdoptD", "nAdoptD:high")), beta12(c("uparsimF", "ubaseF"), c("pctAdoptD", "pctAdoptD:high"))[-1]), 
         c("Controls", rep(c("\\_", "\\checkmark"), 2))
       ), 
       font.size = "scriptsize")
sink()


#### table SI, 18: IV ####


fIV <- update(f, . ~ . - attend - nAdopt - nAdopt:high - geo + distMeeting + distMeeting:high)
dfIV <- model.frame(update(fIV, . ~ . + i + high), data = dfEst) %>% 
  arrange(village, i)
gIV <- g %>% arrange(village) %>% select(union, village, high)
gIV$nodes <- split(dfIV, dfIV$village)
gIV$union <- map2(.x = gIV$nodes, .y = gIV$union, 
                  ~ induced_subgraph(graph = .y, vids = as.character(.x$i)))
dfIV <- dfIV %>% mutate(
  nAdopt = netTreat("adopt", "union", gIV), 
  pctAdopt = netTreat("adopt", "union", gIV, rowNormalize = T), 
  meanDist = netTreat("distMeeting", "union", gIV, rowNormalize = T)
)
dfIV$degree <- netCov(gIV$union, degree)


mods$iv <- iv(fIV, data = dfIV, g = gIV, interaction = T)
mods$ivF <- iv(fIV, data = dfIV, g = gIV, rowNormalize = T, interaction = T)
mods$ivParsim <- iv(adopt ~ distMeeting + distMeeting:high + degree + village, data = dfIV, g = gIV, interaction = T)
mods$ivParsimF <- iv(adopt ~ distMeeting + distMeeting:high + degree + village, data = dfIV, g = gIV, rowNormalize = T, interaction = T)
mods$reduced <- lm(update(fIV, . ~ . + nAdopt + nAdopt:high), data = dfIV)
mods$reducedF <- update(mods$reduced, . ~ . - nAdopt - nAdopt:high + pctAdopt + pctAdopt:high)

modsVC$iv <- vcov(mods$iv)
modsVC$ivF <- vcov(mods$ivF)
modsVC$ivParsim <- vcov(mods$ivParsim)
modsVC$ivParsimF <- vcov(mods$ivParsimF)
modsVC$reduced <- vcov(mods$reduced)
modsVC$reducedF <- vcov(mods$reducedF)

sink("tables/modMyIv.tex")
mystar(mods$ivParsim$second,
       mods$iv$second,
       mods$reduced,
       mods$ivParsimF$second,
       mods$ivF$second,
       mods$reducedF,
       vc = F,
       dep.var.caption = "Dependent variable: adopt",
       se = list(ss(mods$ivParsim), ss(mods$iv), NULL,
                 ss(mods$ivParsimF), ss(mods$ivF), NULL),
       column.labels = c("Parsimonious IV", "IV", "OLS",
                         "Parsimonious IV", "IV", "OLS"),
       omit = om,
       order = c("(nAdopt)", "(pctAdopt)", "(degree)", "(distMeeting)"),
       covariate.labels = c(
         "\\# adopting neighbors ($\\beta_1$)", 
         "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
         "\\% adopting neighbors ($\\beta_1$)", 
         "\\% adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
         "degree", 
         "distance to meeting (km)", 
         "distance to meeting (km) $\\times$ high-uptake"
       ),
       add.lines = list(c("F statistic",
                          FtestIV(mods$ivParsim$first, T),
                          FtestIV(mods$iv$first, T), "\\_",
                          FtestIV(mods$ivParsimF$first, T),
                          FtestIV(mods$ivF$first, T), "\\_"),
                        c(beta12(c("ivParsim", "iv", "reduced")), beta12(c("ivParsimF", "ivF", "reducedF"), c("pctAdopt", "pctAdopt:high"))[-1]), 
                        c("Village FE", rep("\\checkmark", 6)),
                        c("Controls",
                          "\\_", "\\checkmark", "\\checkmark",
                          "\\_", "\\checkmark", "\\checkmark")),
       font.size = "scriptsize")
sink()



#### table SI, 17: IV approach, 1st stage ####

mystar(mods$ivParsim$first, mods$iv$first, 
       dep.var.caption = "Dependent variable: adopt", 
       no.space = T, omit = om, 
       vc = F, 
       column.labels = c("Parsimonious IV", "IV"),
       order = c("(distMeeting)", "(degree)"),
       covariate.labels = c(
         "distance to meeting (km)", 
         "distance to meeting (km) $\\times$ high-uptake", 
         "degree"
       ),
       add.lines = list(
         c("Village FE", rep("\\checkmark", 2)), 
         c("Controls", "\\_", "\\checkmark")
       ),
       out = "tables/modMyIv1st.tex")

#### table SI, 19: placebo tests for IV #### 

fPlacebo <- adopt ~ meanDist + age + female + income + edu + 
  village + prosoc_dic
mods$placebo <- lm(fPlacebo, dfIV)
mods$placePol <- update(mods$placebo, pol_index ~ .)
mods$placeLeader <- update(mods$placebo, leader ~ .)
mods$placePhone <- update(mods$placebo, phone ~ .)

modsVC$placebo <- myVC(mods$placebo)
modsVC$placePol <- myVC(mods$placePol)
modsVC$placeLeader <- myVC(mods$placeLeader)
modsVC$placePhone <- myVC(mods$placePhone)

mystar(mods$placebo, mods$placePol, mods$placeLeader, mods$placePhone, 
       vc = F,
       se = starSD(c("placebo", "placePol", "placeLeader", "placePhone")), 
       no.space = T, omit = "(village)", 
       dep.var.caption = "Dependent variable: ", 
       column.labels = c("adopt", "pol. participation", "leader", "phone"),
       covariate.labels = c("mean peer distance to meeting (km)",
                            "age", "female", "income", "secondary education",
                            # "immigrant", 
                            "pro-sociality"),
       out = "tables/placebo.tex")





#### table SI, 20: flexible controls for degree ####

dfTemp <- dfEst %>% 
  mutate(degree = cut(degree, 
                      quantile(degree, seq(from = 0, to = 1, by = .1)), 
                      include.lowest = T))
mods$degStrata <- update(mods$base, data = dfTemp)
modsVC$degStrata <- myVC(mods$degStrata)

mods$gam <- gam(update(f, . ~ . - degree + s(degree)), 
                data = model.frame(mods$base), 
                family = gaussian)
modsVC$gam <- vcov(mods$gam)

sink("tables/modExtraDeg.tex")
mystar(mods$degStrata, mods$gam, 
       dep.var.caption = "Dependent variable: adopt", 
       omit = c(om, "(s\\()"), 
       order =  c("(nAdopt)", "(degree)"), 
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)",
                            "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
                            "degre $\\in [8,9]$",
                            "degree = 10",
                            "degre $\\in [11,12]$",
                            "degree = 13",
                            "degre $\\in [14,15]$",
                            "degre $\\in [16,17]$",
                            "degre $\\in [18,20]$",
                            "degre $\\in [21,25]$",
                            "degree > 25"),
       column.labels = c("degree strata", "GAM"),
       se = starSD(c("degStrata", "gam")),
       vc = F, 
       font.size = "scriptsize", 
       add.lines = list(
         beta12(c("degStrata", "gam")), 
         c("Controls", rep("\\checkmark", 2))
       ), 
       keep.stat = c("n", "rsq", "ubre"))
sink()

#### table SI, 21: network covariates ####

dfEst <- dfEst %>% mutate(
  betweenness = netCov(g$union, betweenness, normalized = T), 
  clustering = netCov(g$union, transitivity, type = "local", isolates = "zero"), 
  closeness = netCov(g$union, closeness, normalized = T), 
  eigenvector = netCov(g$union, function(g) eigen_centrality(g)$vector), 
  bonpow = netCov(g$union, bonpow)
)

for(var in c("degree", "betweenness", "closeness", "eigenvector", "bonpow", "clustering")) {
  modName <- paste0("net", var, collapse = "")
  dfTemp <- dfEst
  dfTemp$v <- dfTemp %>% pull(var)
  dfTemp$v <- cut(
    dfTemp$v, 
    breaks = quantile(dfTemp$v, c(0, 1/3, 2/3, 1)), 
    labels = c("low", "medium", "high"), 
    include.lowest = T
  )
  mods[[modName]] <- lm(update(f, . ~ . - degree + v), data = dfTemp)
  modsVC[[modName]] <- myVC(mods[[modName]])
}

sink("tables/modNtwk.tex")
mystar(
  mods$netdegree, mods$netbetweenness, mods$netcloseness, mods$neteigenvector, mods$netbonpow, mods$netclustering, 
  vc = F, 
  se = starSD(c("netdegree", "netbetweenness", "netcloseness", "neteigenvector", "netbonpow", "netclustering")), 
  column.labels = c("Degree", "Betweenness", "Closeness", "Eigenvector", "Bonacich", "Clustering"), 
  order = c("(nAdopt)", "(vmedium)", "(vhigh)"), 
  covariate.labels = 
    c("\\# adopting neighbors ($\\beta_1$)", 
      "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
      "medium centrality", 
      "high centrality"),
  dep.var.caption = "Dependent variable: adopt", 
  omit = om, 
  add.lines = list(
    c("Controls", rep("\\checkmark", 6)), 
    beta12(c("netdegree", "netbetweenness", "netcloseness", "neteigenvector", "netbonpow", "netclustering"))
  ), 
  font.size = "scriptsize")
sink()


#### table SI, 22: matching estimates ####


fMat <- formula(nAdoptBin ~ edu + phone + pol_index + attend + 
                  degree + eigenvector + high)
matching <- list(matchDf = list(),
                 matchObj = list())
for (i in 1:3) {
  mf <- model.frame(update(f, . ~ . + eigenvector), dfEst) %>% 
    mutate(nAdoptBin = as.integer(nAdopt >= i))
  matches <- map(c("cem", "nearest", "full"), 
                 ~ matchit(fMat, data = mf, method = .x))
  best <- map_dbl(matches, ~ summary(.x)$reduction[1,1])
  best <- which.max(best)
  cat("\nthreshold = ", i, "; best algorithm is ", 
      switch(best, `1` = "CEM", `2` = "nearest", `3` = "full"))
  ndf <- match.data(matches[[best]])
  mod <- update(mods$base, . ~ . - nAdopt - nAdopt:high + nAdoptBin + nAdoptBin:high, data = ndf, weights = weights)
  modName <- sprintf("match%s", i)
  mods[[modName]] <- mod
  modsVC[[modName]] <- myVC(mod)
  matching$matchDf[[modName]] <- ndf
  matching$matchObj[[modName]] <- matches[[best]]
}

sink("tables/modRob.tex")
mystar(mods$match1, mods$match2, mods$match3,  
       vc = F, 
       se = starSD(c("match1", "match2", "match3")), 
       dep.var.caption = "Dependent variable: adopt",
       omit = om, 
       order = c("(nAdoptBin)", "(degree)"),
       covariate.labels = c(
         "\\# adopting neighbors $\\geq t$ ($\\beta_1$)",
         "\\# adopting neighbors $\\geq t \\times$ high-uptake ($\\beta_2$)", 
         "degree"
       ),
       column.labels = c("Matched sample, $t = 1$", 
                         "Matched sample, $t = 2$", 
                         "Matched sample, $t = 3$"), 
       add.lines = list(
         beta12(c("match1", "match2", "match3"), c("nAdoptBin", "nAdoptBin:high")), 
         c("Controls", rep("\\checkmark", 3))
       ), 
       font.size = "scriptsize")
sink()


#### figure SI, 6: matching, balance diagnostics ####

vars <- c("edu", "phone", "pol_index", "attend", "degree", "eigenvector", "high")
pl <- matching$matchDf[[1]]
pl <- map_dfr(vars, 
              function(v) {
                df <- pl[,c("nAdoptBin", v)]
                colnames(df)[2] <- "v"
                mod <- lm(v ~ nAdoptBin, data = df)
                pval <- summary(mod)$coefficients[2,4]
                mod <- update(mod, weights = pl$weights)
                pvalW <- summary(mod)$coefficients[2,4]
                data.frame(matched = pvalW, all = pval)
              }, .id = "variable")
pl$variable <- vars  
pl <- pl %>% gather(sample, `p value`, - variable) %>%
  mutate(variable = recode(variable, 
                           pol_index = "political participation", 
                           phone = "use phone", 
                           edu = "secondary education", 
                           attend = "meeting attendance", 
                           eigenvector = "eigenvector centrality", 
                           high = "in high-uptake village"), 
         cutoff = cut(`p value`, 
                      breaks = c(0, .01, .05, .1, 1), include.lowest = T, 
                      labels = c("p < .01", "p < .05", "p < .10", "p > .10")))
ggplot(pl, aes(x = `p value`, y = variable, shape = sample, 
               color = cutoff)) + 
  geom_vline(xintercept = .1, lty = "dotted") + 
  geom_point() + 
  scale_color_manual("Significance", 
                     values = c("p < .01" = "black", 
                                "p < .05" = "grey40", 
                                "p < .10" = "grey66", 
                                "p > .10" = "grey75")) +
  labs(x = "p value of difference (treatment - control)", 
       shape = "Sample")
ggsave("figures/balance.pdf", height = 1.9, width = 6)

#### table SI, 24: signal discounting ####

dfEst$nAdoptO <- dfEst$nAdopt - dfEst$nSat

mods$decompH <- lm(update(f, . ~ . - nAdopt:high -nAdopt + nSat + nAdoptO), data = dfEst, subset = high == 1)
mods$decompL <- lm(update(f, . ~ . - nAdopt:high -nAdopt + nSat + nAdoptO), data = dfEst, subset = high == 0)
mods$decomp <- lm(update(f, . ~ . - nAdopt:high -nAdopt + nSat + nSat:high + nAdoptO + nAdoptO:high), data = dfEst)

modsVC$decompH <- myVC(mods$decompH)
modsVC$decompL <- myVC(mods$decompL)
modsVC$decomp <- myVC(mods$decomp)

thisFTest <- function(m, augmented = F) {
  mod <- mods[[m]]
  vc <- modsVC[[m]]
  hypmat <- matrix(0, nrow = 1, ncol = length(coef(mod)), dimnames = list(NULL, names(coef(mod))))
  hypmat[,c("nAdoptO", "nSat")] <- c(1,-1)
  if(augmented) {
    hypmat[,c("nAdoptO:high", "nSat:high")] <- c(1,-1)
  }
  test <- car::linearHypothesis(mod, hypmat, test = "F", vcov. = vc)
  paste0(round(test[2,3], 2), pvalToStars(test[2,4]))
}

sink("tables/decomposition.tex")
mystar(mods$decompL, mods$decompH, mods$decomp, 
       vc = F, 
       se = starSD(c("decompL", "decompH", "decomp")), 
       omit = om, 
       order = c("(nAdoptO)", "(nSat)", "(degree)"), 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = c("Low-uptake", "High-uptake", "Full sample"), 
       dep.var.labels.include = F, 
       covariate.labels = c(
         "\\# non-satisfied adopting neighbors ($\\beta_1$)", 
         "\\# non-satisfied adopting neighbors $\\times$ high-uptake ($\\beta_2$)", 
         "\\# satisfied adopting neighbors ($\\beta_3$)", 
         "\\# satisfied adopting neighbors $\\times$ high-uptake ($\\beta_4$)", 
         "degree"
       ), 
       add.lines = list(
         c("Controls", rep("\\checkmark", 3)), 
         c("$H_0: \\beta_1 \\neq \\beta_3$", thisFTest("decompL"), thisFTest("decompH"), thisFTest("decomp")), 
         c("$H_0: \\beta_1 + \\beta_2 \\neq \\beta_3 + \\beta_4$", "", "", thisFTest("decomp", T)), 
         c("$\\beta_1 + \\beta_2$", "", "", beta12("decomp", c("nAdoptO", "nAdoptO:high"))[-1]), 
         c("$\\beta_3 + \\beta_4$", "", "", beta12("decomp", c("nSat", "nSat:high"))[-1])
       )
)
sink()


#### table SI, 25: strength of network ties #### 

dfEst <- dfEst %>% mutate(
  nAdoptFam1 = netTreatSimple("family", g), 
  nAdoptFri1 = netTreatSimple("friend", g), 
  nAdoptLen1 = netTreatSimple("lender", g), 
  nAdoptSol1 = netTreatSimple("solver", g), 
  nAdopt2 = netTreatCpl("union", g), 
  nAdopt1 = nAdopt - nAdopt2
)

mods$byType <- update(mods$base, . ~ . - nAdopt - nAdopt:high + nAdoptFam1 + nAdoptFri1 + 
                        nAdoptLen1 + nAdoptSol1 + nAdopt2)
mods$stg <- update(mods$base, 
                   . ~ . - nAdopt - nAdopt:high + nAdopt1 + nAdopt2) 
modsVC$byType <- myVC(mods$byType)
modsVC$stg <- myVC(mods$stg)

LHS <- matrix(0, nrow = 1, ncol = length(coef(mods$stg)))
colnames(LHS) <- names(coef(mods$stg))
LHS[,c("nAdopt1","nAdopt2")] <- c(-1,1)


mystar(mods$stg, mods$byType, 
       vc = F, 
       se = starSD(c("stg", "byType")), 
       order = c("(nAdopt1)", "(nAdoptFam1)", "(nAdoptFri1)", 
                 "(nAdoptLen1)", "(nAdoptSol1)", "(nAdopt2)", "(degree)"), 
       omit = om, 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = c("Simple vs. complex ties", "Types of relationships"),
       covariate.labels = c("\\# adopting simple ties, $\\beta_s$",
                            "\\# adopting simple family",
                            "\\# adopting simple friends",
                            "\\# adopting simple lender",
                            "\\# adopting simple solver",
                            "\\# adopting complex ties, $\\beta_c$",
                            "degree"),
       add.lines = list(
         c("$\\beta_c - \\beta_s \\neq 0$, F statistic",
           Ftest(mods$stg, modsVC$stg, LHS), "\\_"), 
         c("Controls", rep("\\checkmark", 2))
       ),
       out = "tables/modStg.tex")

#### table SI, 16: main specification by network type #### 

for (type in c("family", "friend", "lender", "solver")) {
  dfTemp <- dfEst
  dfTemp$degree <- netCov(g %>% pull(type), degree)
  dfTemp$nAdopt <- netTreat("adopt", type, g)
  mods[[type]] <- update(mods$base, data = dfTemp)
  modsVC[[type]] <- myVC(mods[[type]])
  typeParsim <- paste0(type, "Parsim", collapse = "")
  mods[[typeParsim]] <- update(mods$parsim, data = model.frame(mods[[type]]))
  modsVC[[typeParsim]] <- myVC(mods[[typeParsim]])
}

sink("tables/modOneType.tex")
mystar(mods$familyParsim, 
       mods$family, 
       mods$friendParsim, 
       mods$friend,
       mods$lenderParsim, 
       mods$lender, 
       mods$solverParsim, 
       mods$solver, 
       vc = F, 
       se = starSD(c("familyParsim", "family", "friendParsim", "friend", "lenderParsim", "lender", "solverParsim", "solver")), 
       order = c("(nAdopt)", "(degree)"), 
       omit = om, 
       dep.var.caption = "Dependent variable: adopt", 
       column.labels = c("Family", "Family", "Friends", "Friends", 
                         "Lender", "Lender", "Solver", "Solver"),
       covariate.labels = c("\\# adopting neighbors ($\\beta_1$)", "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)", "degree"),
       add.lines = list(
         c("Controls", rep(c("\\_", "\\checkmark"), 4)), 
         beta12(c("familyParsim", "family", "friendParsim", "friend", "lenderParsim", "lender", "solverParsim", "solver"))
       ), 
       font.size = "scriptsize")
sink()


#### figure 5: average marginal effect by village ####

getLeaderCon <- function(thresh) {
  df <- ties %>% filter(type == "Contgame") %>%
    group_by(village, j) %>%
    summarize(n = n()) %>%
    filter(n > thresh) %>%
    group_by(village) %>%
    mutate(tot = sum(n),
           sq = (n/tot)^2) %>%
    summarize(leaderCon = sum(sq)) %>%
    filter(village != "B")
  colnames(df)[2] <- sprintf("leaderCon%s", thresh)
  df
}

mfMultilevel <-
  model.frame(adopt ~ age + female + income + edu + phone + leader + pol_index +
                attend + prosoc_dic + geo + degree + nAdopt + village + nHeard + heard +
                high,
              data= dfEst) %>%
  filter(village != "B") %>%
  inner_join(getLeaderCon(0)) %>%
  inner_join(getLeaderCon(1)) %>%
  inner_join(getLeaderCon(2)) %>%
  inner_join(getLeaderCon(3)) %>%
  inner_join(census %>% select(village, eth, rlg)) %>%
  inner_join(
    nodes %>% group_by(village) %>%
      select(prosoc_pub, prosoc_dic) %>%
      summarize(mean_pub = mean(prosoc_pub),
                mean_dic = mean(prosoc_dic))
  )



mods$byVillage <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree + (1+nAdopt | village),
            data = mfMultilevel, chains = nChainsRE, iter = nRepRE, seed = 777)



pl <- as.data.frame(summary(mods$byVillage, pr = c(.025, .05, .95, .975))) %>%
  mutate(param = rownames(.)) %>%
  filter(grepl("b[nAdopt", param, fixed = T)) %>%
  mutate(
    village = gsub("b[nAdopt village:", "", param, fixed = T),
    village = gsub("]", "", village, fixed = T)
  ) %>%
  inner_join(high) %>%
  mutate(
    high = recode(high, `0` = "Low", `1` = "High"),
    village = fct_reorder(village, desc(mean))
  ) %>%
  rename(pe = mean, LB = `2.5%`, UB = `97.5%`, LB90 = `5%`, UB90 = `95%`)

ggplot(pl, aes(x = village, y = pe, ymin = LB, ymax = UB, shape = high)) +
  geom_point() +
  geom_linerange() +
  geom_linerange(aes(ymax = UB90, ymin = LB90), lwd = 1) +
  geom_hline(yintercept = 0, lty = "dotted") +
  labs(x = "Village", y = "Average Marginal Effect", shape = "Uptake")
ggsave("figures/AMEbyVillageComp.pdf", width = 4.5, height = 2)


#### figure SI, 7: mediators in high and low uptake villages ####

pl <- mfMultilevel %>%
  select(village, high, starts_with("leaderCon"), mean_pub, mean_dic, eth, rlg) %>%
  # select(-leaderCon) %>%
  distinct() %>%
  mutate(high = recode(high,
                       `0` = "Low",
                       `1` = "High"))
pl <-
  pl %>%
  gather(var, value, -village, -high) %>%
  mutate(
    order = recode(
      var,
      eth = 1,
      rlg = 2,
      mean_pub = 3,
      mean_dic = 4,
      leaderCon0 = 5,
      leaderCon1 = 6,
      leaderCon2 = 7,
      leaderCon3 = 8
    ),
    order = as.integer(order),
    var = recode(
      var,
      eth = "Ethnic concentration",
      rlg = "Religious concentration",
      mean_pub = "Pro-sociality (public good)",
      mean_dic = "Pro-sociality (dictator)",
      leaderCon0 = "Leadership concentration, t = 1",
      leaderCon1 = "Leadership concentration, t = 2",
      leaderCon2 = "Leadership concentration, t = 3",
      leaderCon3 = "Leadership concentration, t = 4"
    )) %>%
  arrange(order) %>%
  mutate(var = fct_reorder(var, order))
pl2 <- pl %>%
  group_by(var, high) %>%
  summarize(value = mean(value))
ggplot(pl, aes(x = high, y = value)) +
  geom_boxplot() +
  geom_point(data = pl2, shape = 21, color = "black", fill = "white") +
  facet_wrap(~ var, ncol = 4) +
  labs(x = "", y = "")
ggsave("figures/corPE.pdf", height = 3, width = 6.5)

#### table SI, 11: main random effect models ####

getCoefRE <- function(mod, parm) {
  df <- as.data.frame(summary(mod, pr = c(.025, .05, .95, .975)))
  df$var <- rownames(df)
  rownames(df) <- NULL
  df <- df %>%
    filter(var == parm)
  df <- df[,c("var", "mean", "2.5%", "5%", "95%", "97.5%")]
  colnames(df) <- c("var", "pe", "LB", "LB90", "UB90", "UB")
  df
}

mods$byVillageE <- list()
for(thresh in 0:3) {
  mfMultilevel$leaderCon <- as.numeric(mfMultilevel[,sprintf("leaderCon%s", thresh)])
  mods$byVillageE[[sprintf("RE%s", thresh)]] <-
    stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
                attend + prosoc_dic + geo + degree +
                leaderCon + leaderCon:nAdopt + (1+nAdopt | village),
              data = mfMultilevel,
              seed = 999 + thresh, chains = nChainsRE, iter = nRepRE)
}

mods$byVillageE2 <- list()
mods$byVillageE2$rlg <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree +
              rlg + rlg:nAdopt + (1+nAdopt | village),
            data = mfMultilevel,
            seed = 1005 + thresh, chains = nChainsRE, iter = nRepRE)
mods$byVillageE2$eth <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree +
              eth + eth:nAdopt + (1+nAdopt | village),
            data = mfMultilevel,
            seed = 1006 + thresh, chains = nChainsRE, iter = nRepRE)
mods$byVillageE2$mean_pub <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree +
              mean_pub + mean_pub:nAdopt + (1+nAdopt | village),
            data = mfMultilevel,
            seed = 1007 + thresh, chains = nChainsRE, iter = nRepRE)
mods$byVillageE2$mean_dic <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree +
              mean_dic + mean_dic:nAdopt + (1+nAdopt | village),
            data = mfMultilevel,
            seed = 1008 + thresh, chains = nChainsRE, iter = nRepRE)

mods$RE <-
  stan_lmer(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree + nAdopt + nAdopt:high + (1|village),
            data = mfMultilevel,
            seed = 667, chains = nChainsRE, iter = nRepRE)



myExtract <- function(model, obj = "coef", keepRSlope = F) {
  pr <- c(.025,.975)
  df <- as.data.frame(summary(model, probs = pr))
  vars <- rownames(df)
  keep <- grepl("sigma", vars, ignore.case = T) |
    grepl("mean_PPD", vars, fixed = T) |
    grepl("log-posterior", vars, fixed = T)
  if(!keepRSlope) {
    keep <- keep | grepl("b[", vars, fixed = T)
  } else {
    keep <- keep | grepl("b[(Intercept)", vars, fixed = T)
  }
  keep <- !keep
  df <- df[keep,]
  coef <- df$mean
  ci <- as.matrix(df[,4:5])
  nams <- rownames(df)
  if(keepRSlope) {
    nams <- gsub("b[nAdopt village:", "nAdopt:village", nams, fixed = T)
    nams <- gsub("]", "", nams, fixed = T)
  }
  rownames(ci) <- names(coef) <- nams

  if(obj == "coef") {
    coef
  } else if (obj == "ci") {
    ci
  }
}

test <- rstan::extract(mods$RE$stanfit, c("nAdopt", "nAdopt:high"))
test <- test$nAdopt + test$`nAdopt:high`
test <- list(pe = mean(test), ci = quantile(test, probs = c(.025, .975)))
test <- map(test, ~ as.character(round(., 3)))
test$ci <- sprintf("(%s, %s)", test$ci[1], test$ci[2])

placeholder <- list(
  base = lm(adopt ~ age + female + income + edu + phone + leader + pol_index +
              attend + prosoc_dic + geo + degree + nAdopt + nAdopt:high, data = mfMultilevel)
)
placeholder$m1 <- update(placeholder$base, . ~ . - nAdopt - nAdopt:high + nAdopt:village, data = mfMultilevel)

sink("tables/randomEffects.tex")
mystar(placeholder, vc = F,
       coef = map2(list(mods$RE,mods$byVillage),
                   c(F, T), ~ myExtract(.x, "coef", .y)),
       ci = T,
       p = list(NA,NA),
       ci.custom = map2(list(mods$RE,mods$byVillage),
                        c(F, T), ~ myExtract(.x, "ci", .y)),
       omit = c(om[!om %in% c("(village)", "(age)")], "^age$"),
       order = c("(nAdopt)", "(degree)"),
       dep.var.caption = "Dependent variable: adopt",
       covariate.labels = c(
         "\\# adopting neighbors ($\\beta_1$)",
         "\\# adopting neighbors $\\times$ high-uptake ($\\beta_2$)",
         sprintf("\\# adopting neighbors (village %s)", c("A", LETTERS[3:16])),
         "degree"
       ),
       add.lines = list(
         c("Controls", rep("\\checkmark",2)),
         c("Random intercept", rep("\\checkmark",2)),
         c("Random slope", "\\_", "\\checkmark"),
         c("$\\beta_1 + \\beta_2$", test$pe, ""),
         c("", test$ci, "")
       ),
       keep.stat = "n",
       font.size = "scriptsize",
       notes = "95 percent credible intervals in parenthesis.",
       notes.append = F)
sink()

#### table SI, 26: random effect models, leadership concentration ####

placeholder <- list(
  m1 = lm(adopt ~ age + female + income + edu + phone + leader + pol_index +
            attend + prosoc_dic + geo + degree + leaderCon + leaderCon:nAdopt,
          data = mfMultilevel %>% mutate())
)
placeholder$m2 <- placeholder$m3 <- placeholder$m4 <- placeholder$m1

mystar(placeholder, vc = F,
       coef = map(mods$byVillageE,
                  myExtract, obj = "coef"),
       ci = T,
       p = list(NA,NA,NA,NA),
       ci.custom = map(mods$byVillageE,
                       myExtract, obj = "ci"),
       omit = c(om[om != "(leader)"], "^leader$"),
       order = c("^leaderCon:nAdopt$", "^leaderCon$",
                 "(degree)"),
       dep.var.caption = "Threshold: ",
       column.labels = sprintf("$t = %s$", 1:4),
       covariate.labels = c("leadership concentration $\\times$ \\# adopting neighbors",
                            "leadership concentration",
                            "degree"),
       add.lines = list(
         c("Controls", rep("\\checkmark",4)),
         c("Random intercept", rep("\\checkmark",4)),
         c("Random slope", rep("\\checkmark",4))
       ),
       keep.stat = "n",
       font.size = "scriptsize",
       notes = "95 percent credible intervals in parenthesis.",
       notes.append = F,
       out = "tables/randomEffects2.tex")

#### table SI, 27: random effect models, other mediators ####

placeholder <- list(
  m1 = lm(adopt ~ age + female + income + edu + phone + leader + pol_index +
            attend + prosoc_dic + geo + degree + rlg + rlg:nAdopt,
          data = mfMultilevel)
)
placeholder$m2 <- update(placeholder$m1,
                         . ~ . - rlg - rlg:nAdopt + eth + eth:nAdopt)
placeholder$m3 <- update(placeholder$m1,
                         . ~ . - rlg - rlg:nAdopt + mean_pub + mean_pub:nAdopt)
placeholder$m4 <- update(placeholder$m1,
                         . ~ . - rlg - rlg:nAdopt + mean_dic + mean_dic:nAdopt)


mystar(placeholder, vc = F,
       coef = map(mods$byVillageE2,
                  myExtract, obj = "coef"),
       ci = T,
       p = list(NA,NA,NA,NA),
       ci.custom = map(mods$byVillageE2,
                       myExtract, obj = "ci"),
       omit = om,
       order = c("^rlg:nAdopt$", "^rlg$",
                 "^eth:nAdopt$", "^eth$",
                 "^mean_pub:nAdopt$", "^mean_pub$",
                 "^mean_dic:nAdopt$", "^mean_dic$",
                 "(degree)"),
       dep.var.caption = "Dependent variable: adopt",
       column.labels = c("Religion", "Ethnicity", "Pro-sociality (1)", "Pro-sociality (2)"),
       covariate.labels = c("religious concentration $\\times$ \\# adopting neighbors",
                            "religious concentration",
                            "ethnic concentration $\\times$ \\# adopting neighbors",
                            "ethnic concentration",
                            "pro-sociality (public goods) $\\times$ \\# adopting neighbors",
                            "pro-sociality (public goods)",
                            "pro-sociality (dictator) $\\times$ \\# adopting neighbors",
                            "pro-sociality (dictator)",
                            "degree"),
       add.lines = list(
         c("Controls", rep("\\checkmark",4)),
         c("Random intercept", rep("\\checkmark",4)),
         c("Random slope", rep("\\checkmark",4))
       ),
       keep.stat = "n",
       font.size = "scriptsize",
       notes = "95 percent credible intervals in parenthesis.",
       notes.append = F,
       out = "tables/randomEffects3.tex")


#### figure 4: union network in P and F ####

set_graph_style()
set.seed(1789)
names(g$union) <- g$village
mg <- g$union$K
V(mg)$behavior <- V(mg)$adopt + V(mg)$heard
V(mg)$behavior <- recode(V(mg)$behavior, `0` = "none", `1` = "hear", `2` = "adopt")

pl1 <- ggraph(mg) +
  geom_edge_link(alpha = .05) +
  scale_color_manual("Behavior",
                     values = c("none" = "black",
                                "hear" = "grey50",
                                "adopt" = "grey75"),
                     guide = F) +
  geom_node_point(aes(color = behavior), size = 1) +
  labs(title = "Village K", subtitle = "Low uptake") +
  theme_graph(plot_margin = margin(1,1,1,1),
              base_size = 8,
              title_size = 9,
              subtitle_size = 8,
              caption_size = 7)

mg <- g$union$H
V(mg)$behavior <- V(mg)$adopt + V(mg)$heard
V(mg)$behavior <- recode(V(mg)$behavior, `0` = "none", `1` = "hear", `2` = "adopt")


pl2 <- ggraph(mg) +
  geom_edge_link(alpha = .05) +
  scale_color_manual("Behavior",
                     values = c("none" = "black",
                                "hear" = "grey50",
                                "adopt" = "grey75")) +
  geom_node_point(aes(color = behavior), size = 1) +
  labs(title = "Village H", subtitle = "High uptake") +
  theme_graph(plot_margin = margin(1,1,1,1),
              base_size = 8,
              title_size = 9,
              subtitle_size = 8,
              caption_size = 7)

pl <- grid.arrange(pl1, pl2, nrow = 1)
ggsave("figures/two_nets.png", plot = pl,
       height = 3, width = 6)


#### inline numbers ####

# effect size

(mean(model.frame(mods$base)$adopt[model.frame(mods$base)$high == 1])+coef(mods$base)["nAdopt"]+coef(mods$base)["nAdopt:high"]) /
  mean(model.frame(mods$base)$adopt[model.frame(mods$base)$high == 1])

# pct respondents w no ties to adopter
mean(model.frame(mods$baseF)$pctAdopt == 0)
# mean pct adopters for respondents with ties to at least one adopter
mean(model.frame(mods$baseF)$pctAdopt[model.frame(mods$baseF)$pctAdopt > 0])
# calibration
mean(model.frame(mods$baseF)$pctAdopt[model.frame(mods$baseF)$pctAdopt > 0]) * (coef(mods$baseF)["pctAdopt"]+coef(mods$baseF)["pctAdopt:high"])
mean(model.frame(mods$baseF)$pctAdopt[model.frame(mods$baseF)$pctAdopt > 0]) * (coef(mods$baseF)["pctAdopt"])


# effect of satisfied neighbor pvalue
coeftest(mods$decompH, modsVC$decompH)

# IV: exclusion restriction
cor(
  map(g$union, ~ as.numeric(as_adj(.x))) %>% unlist(),
  map(g$geo, ~ as.numeric(as_adj(.x, attr = "dist"))) %>% unlist()
)

# inline F-test

anova(update(mods$parsim, . ~ . - nAdopt - degree,
             data = model.frame(mods$base)),
      update(mods$parsim, . ~ . + geo + attend,
             data = model.frame(mods$base)))

anova(update(mods$parsim, . ~ . - nAdopt - degree,
             data = model.frame(mods$base)),
      update(mods$base, . ~ . - nAdopt - degree,
             data = model.frame(mods$base)))



#### stop cluster ####


if(!devel) {
  stopCluster(cl)
}
