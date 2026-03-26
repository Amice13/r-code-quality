"""
This script reads in the data files (described below) to produce the main
results of the paper. This code requires `R`, as well as the `dplyr`,
`ggplot2`, `reshape2`, `Hmisc`, and `xtable` packages. It has been tested
with `R` version 3.1.0, `dplyr` v0.4.1, `ggplot` v1.0.0, `reshape` 1.4,
`Hmisc` 3.14, and `xtable` 1.7.
"""

library(ggplot2)
library(scales)
library(dplyr)
library(reshape2)
library(Hmisc)
library(xtable)

# Need to set workingdir to the location of the archive
workingdir <- '.'
setwd(workingdir)
options(digits=3)

## Themes and colors for plots
theme_science <- theme_bw() +
  theme(
    axis.text.y=element_text(size=12), axis.text.x=element_text(size=12),
    axis.title.x=element_text(size=13,vjust=0), axis.title.y=element_text(size=13,angle=90),
    legend.key=element_blank(), strip.background=element_rect(fill='grey92', colour='grey30')
  )
affilcol3 <- c('Liberal'="#3929AA",'Moderate'="#AAAAAA",'Conservative'="#AA3929")
aligncol3 <- c('Liberal'="#3929AA",'Neutral'="#AAAAAA",'Conservative'="#AA3929")

### Abbreviations
## url: uniform resource locator
## pimp: potential impression
## vpv: viewport view (also sometimes referred to as an impression)
## op: opposite

#################################################################
#### CODE THAT RUNS ON FACEBOOK'S SERVERS TO RETRIEVE DATA
#################################################################

## We use a few strategies to aggregate the data for distribution and
## make it easier to work with the data in memory.
## - We construct strata using round(100*log2(num_potential_impressions)) buckets
## to reduce the cardinality of the data. Since these strata are quite
## granular over the majority of the support of the potential and actual
## impressions, and the data are weighted inversely proportional to how
## many impressions or potential impressions there are per user, to compute
## the averages over users, this bucketing introduces minimal distortion.
## - We truncate min_pos to be <= 110, covers 95% of all VPVs.

### Additional notes:
## The points at which we cut url_alignment_continuous (-0.291 and 0.467)
## represent the 40th and 60th percentiles of the measure, respectively.

#piq <- "
#SELECT
# viewer_affiliation,
# IF(url_alignment_continuous <= -0.291, -1, IF(url_alignment_continuous >= 0.467, 1, 0))
#   AS url_alignment,
# has_vpv, has_click, min_pos,
# round(100*log2(num_pimp)) AS pimp_bucket,
# COUNT(1) AS freq
# FROM <RAW DATA>
# WHERE
#   (min_pos <= 110 or min_pos is null) and num_click>0 and
#   (num_l2 + num_l1 + num_m + num_r1 + num_r2) > 20 and
#   (url_alignment_continuous <= -0.291 or url_alignment_continuous >= 0.467)
# GROUP BY
#   viewer_affiliation, has_vpv, has_click, min_pos,
#   IF(url_alignment_continuous <= -0.291, -1, IF(url_alignment_continuous >= 0.467, 1, 0)),
#   round(100*log2(num_pimp))
#potential_imps_compact <- presto(piq)
#"

## num.potential is the number of observations we have for a user.
## Since we are constructing per-user averages, we weight each observation by 1/num.potential.
## Finally, because each row in compact_pimps represents freq instances of a particular
## event summary, the final weight of our averages is freq/num.potential.

#pimpd <- potential_imps_compact %>%
# mutate(
#   freq=freq+0.0,
#   num.potential=2^(pimp_bucket/100.0),
#   weight=freq*(1/num.potential)
# )
#nrow(pimpd)
#save(pimpd, file='pimpd.Rda')


## This code has been executed on Facebook's servers to generate
## impression summary data: vpvd.Rda

### The data is aggregated based on impressions rather than friend shares
### (potential impressions).  This is necessary to compute average rates
### at which individuals click on cross-cutting content.

# iq <- "
#SELECT
#  viewer_affiliation,
#  IF(url_alignment_continuous <= -0.291, -1, IF(url_alignment_continuous >= 0.467, 1, 0))
#    AS url_alignment,
#  has_click, min_pos,
#  round(100*log2(num_vpv)) AS imp_bucket,
#  COUNT(1) AS freq
#  FROM <RAW_DATA>
#  WHERE
#    (min_pos <= 110 or min_pos is null) and num_click>0 and
#    (num_l2 + num_l1 + num_m + num_r1 + num_r2) > 20 and
#    has_vpv = 1 and
#    (url_alignment_continuous <= -0.291 or url_alignment_continuous >= 0.467)
#  GROUP BY
#    viewer_affiliation, has_click, min_pos,
#    IF(url_alignment_continuous <= -0.291, -1, IF(url_alignment_continuous >= 0.467, 1, 0)),
#    round(100*log2(num_vpv))
#"

#vpvd_compact <- presto(iq)
#vpvd <- vpvd_compact %>%
#   mutate(
#       freq=freq+0.0,
#       n_i = 2^(imp_bucket/100.0),
#       weight=freq*(1/n_i)
#   )
#save(vpvd, file='vpvd.Rda')


#################################################################
#### CODE FOR ANALYSIS OF PUBLIC, DE-IDENTIFIED DATA
#################################################################

load('pimpd.Rda')

pimpd.description <- "
### `pimpd.Rda`
`pimpd.Rda` summarizes the number of cases in which an
individual could potentially be exposed to URLs shared by friends
('potential impressions'), whether they saw these stories in their News
Feeds, and/or clicked on the link.

- `viewer_affiliation`: {-2, -1, 1, 2}-valued variable indicating the
self-reported ideological affiliation of the viewer, corresponding to
users that are very liberal, liberal, moderate, conservative, and very
very conservative.
- `url_alignment`: {-1, 1}-valued variable indicating the ideological
alignment bin for the URL shared by a friend.
- `has_vpv`: {0, 1}-valued variable which is 1 when a validated viewport
view (VPV) was recorded.
- `has_click`: {0, 1}-valued variable which is 1 if the user had clicked
on the link.
- `min_pos`: Integer-valued variable with the position of the story, if a
VPV exists. When the story is not viewed, this value is NA.
- `pimp_bucket`: Approximate number of friends who shared hard content
(as a logarithmically-spaced bucket).
- `freq`: Number of observations with the above covariates.
- `num.potential`: Number of potential impressions (`2^(pimp_bucket/10.0)`)
- `weight`: The observation weight for the row: `freq/num.potential`.

Note that these potential impressions only represent a subset
of all stories seen by users; in particular, it only pertains to
the population described in the supporting materials of [1], and
only includes URLs classified as 'hard content' shared some minimum
number of users.
"

# code users as liberal or conservative, and whether content is ideologically countervailing
rdb <- pimpd %>%
  mutate(
    viewer_affiliation=sign(viewer_affiliation),
    viewer_label=ifelse(viewer_affiliation==-1, "Liberal", "Conservative"),
    is_op=sign(viewer_affiliation) != sign(url_alignment)
  )


## This computes summary statistics of proportions needed for conducting the core analyses.
#  (0.4535789, 0.4031255) reflect the proportion of hard content that is classified as being
#  conservatively and liberally aligned, respectively.
proportions.by.affiliation <- rdb %>%
  group_by(viewer_label) %>%
  summarise(
      stratum.weight=sum(weight),
      frac.op.pimp.rand=ifelse(sign(viewer_affiliation[1])==-1, 0.4535789, 0.4031255),
      frac.op.pimp=wtd.mean(is_op, weight),
      frac.op.vpv=wtd.mean(is_op[has_vpv==1], weight[has_vpv==1]),
      pr.op.vpv=wtd.mean(has_vpv[is_op==1], weight[is_op==1]),
      pr.vpv=wtd.mean(has_vpv, weight),
      pr.same.vpv=(pr.vpv-frac.op.pimp*pr.op.vpv)/(1-frac.op.pimp)
  )

rand_pimp_results <- proportions.by.affiliation %>%
  select(viewer_label, frac.op.pimp.rand, frac.op.pimp, frac.op.vpv)
print(rand_pimp_results)

### ANALYSIS OF CLICK EVENTS

load('vpvd.Rda')

vpvd.description <- "
### `vpvd.Rda`
`vpvd.Rda` summarizes interactions with content that appeared
in viewers' News Feeds.

- `viewer_affiliation`: {-2, -1, 1, 2}-valued variable indicating the
self-reported ideological affiliation of the viewer, corresponding to
users that are very liberal, liberal, moderate, conservative, and very
very conservative.
- `url_alignment`: {-1, 1}-valued variable indicating the ideological
alignment bin for the URL shared by a friend.
- `has_vpv`: {0, 1}-valued variable which is 1 when a validated viewport
view (VPV) was recorded.
- `has_click`: {0, 1}-valued variable which is 1 if the user had clicked
on the link.
- `min_pos`: Integer-valued variable with the position of the story, if
a VPV exists. When the story is not viewed, this value is NA.
- `imp_bucket`: Approximate number of friends whose content users saw in
their News Feeds (as a logarithmically-spaced bucket).
- `freq`: Number of observations with the above covariates.
- `n_i`: Number of validated viewport views (`2^(imp_bucket/10.0)`).
- `weight`: The observation weight for the row: `freq/n_i`.
"

# code users as liberal or conservative, and whether content is ideologically countervailing
rdbi <- vpvd %>%
    mutate(
        viewer_affiliation=sign(viewer_affiliation),
        viewer_label=ifelse(viewer_affiliation==-1, "Liberal", "Conservative"),
        is_op=sign(viewer_affiliation) != sign(url_alignment)
    )


## This computes summary statistics of proportions needed for conducting
## the core analyses.  The numbers (0.4535789, 0.4031255) reflect the
## proportion of hard content that is classified as conservative and
## liberal, respectively.

## Compute unadjusted data for Figure 3b
proportions.by.affiliation.imps <- rdbi %>%
    group_by(viewer_label) %>%
    summarise(
        frac.op.click=wtd.mean(is_op[has_click==1], weight[has_click==1])
    )

click_results <- proportions.by.affiliation.imps %>%
    select(frac.op.click)

res <- cbind(rand_pimp_results, click_results)
print(res)

############ FIGURES ############
fig3b.data <- melt(
  res,
  id.vars=c('viewer_label'),
  measure.var=c('frac.op.pimp.rand', 'frac.op.pimp', 'frac.op.vpv', 'frac.op.click')
) %>%
  mutate(
    variable=ordered(
      variable,
      levels=c('frac.op.pimp.rand', 'frac.op.pimp', 'frac.op.vpv', 'frac.op.click'),
      labels=c(
        'Random',
        'Potential\nfrom network',
        'Exposed',
        'Selected'
      ))
  )

fig3b.plot <- qplot(
  variable, value, color=viewer_label, data=fig3b.data,
  geom='point',
  ylab='Percent cross-cutting content', xlab='') +
  scale_color_manual(values=affilcol3, name="Viewer affiliation") +
  scale_y_continuous(label=percent, limits=c(0.15, 0.5)) +
  geom_line(aes(group = viewer_label)) + theme_science

fig3b.plot + theme(legend.position=c(0.8,0.83))

#ggsave('prop_xcutting.pdf', width=5.5, height=4.5)

#################################################################
#### Analysis of potential confounding from position
# Shows the extent to which ideologically cross-cutting content is ranked lower
# in the News Feed, and the extent to which individuals have a tendency to click on
# hard content that appears higher up in the News Feed

position.effects <- rdbi %>%
  filter(min_pos>0 & min_pos <= 40) %>%
  group_by(min_pos, viewer_label) %>%
  summarise(
    pr.click=wtd.mean(has_click, weight),
    pr.op.click=wtd.mean(has_click[is_op==1], weight[is_op==1]),
    pr.same.click=wtd.mean(has_click[is_op==0], weight[is_op==0]),
    frac.op.vpv=wtd.mean(is_op, weight)
  ) %>%
  ungroup() %>%
  arrange(viewer_label, min_pos)

## Fig S5a
qplot(min_pos, pr.click, data=position.effects, color=as.factor(viewer_label),
      geom=c('point', 'line'),
      xlab='Min position',
      ylab='Percent of stories clicked on'
) +
  scale_y_continuous(labels=percent) +
  scale_color_manual(values=affilcol3, name="Viewer affiliation") +
  theme_science

#ggsave('ctr_vs_position.pdf', width=6.25, height=5.0)

qplot(
  min_pos, frac.op.vpv, data=position.effects, color=viewer_label,
  geom=c('point', 'line'),
  xlab='Min position',
  ylab='Percent countervailing stories'
) +
  scale_y_continuous(labels=percent) +
  scale_color_manual(values=affilcol3, name="Viewer affiliation") +
  theme_science


## Fig S5b
cd <- position.effects %>%
  select(min_pos, viewer_label, pr.op.click, pr.same.click) %>%
  melt(., id.vars=c('min_pos', 'viewer_label')) %>%
  mutate(
    variable=ordered(
      variable,
      levels=c('pr.same.click', 'pr.op.click'),
      labels=c('Ideologically congruent', 'Ideologically cross cutting')
     )
  )

qplot(min_pos, value, data=cd, linetype=variable, color=as.factor(viewer_label),
  shape=variable,
     facets=.~viewer_label,
      geom=c('line'),
      xlab='Min position',
      ylab='Selection (click) rate'
) +
  scale_y_continuous(labels=percent) +
  scale_color_manual(values=affilcol3, name="Viewer affiliation") +
  scale_linetype_manual(values=c(1,2), name="Content type") +
  theme_science

# ggsave('frac_op_vs_position.pdf', width=9.28, height=3.68)

######### "FILTER BUBBLE" ANALYSIS ########
compute.proportions <- function(df) {
  df %>%
    summarise(
      stratum.weight=sum(weight),
      frac.op.pimp.rand=ifelse(sign(viewer_affiliation[1])==-1, 0.4535789, 0.4031255),
      frac.op.pimp=wtd.mean(is_op, weight),
      frac.op.vpv=wtd.mean(is_op[has_vpv==1], weight[has_vpv==1]),
      frac.op.click=wtd.mean(is_op[has_click==1], weight[has_click==1]),
      pr.op.vpv=wtd.mean(has_vpv[is_op==1], weight[is_op==1]),
      pr.vpv=wtd.mean(has_vpv, weight),
      # click probabilities are conditional on exposure
      pr.op.click=wtd.mean(has_click[is_op==1 & has_vpv==1], weight[is_op==1 & has_vpv==1]),
      pr.click=wtd.mean(has_click[has_vpv==1], weight[has_vpv==1]),
      pr.same.vpv = (pr.vpv-frac.op.pimp*pr.op.vpv)/(1-frac.op.pimp),
      pr.same.click = (pr.click-frac.op.vpv*pr.op.click)/(1-frac.op.vpv)
    )
}

stratified.est <- function(base.pimps, base.vpvs) {
  # "naive" is the unadjusted computation
  naive.selection <- base.vpvs %>%
    mutate(has_vpv=1) %>%
    group_by(viewer_affiliation) %>%
    do(compute.proportions(.)) %>%
    mutate(
      pr.op.click.naive=pr.op.click,
      pr.same.click.naive=pr.same.click
    ) %>%
    select(viewer_affiliation, pr.op.click.naive, pr.same.click.naive)

  # Here we compute each statistic separately for every (viewer_affiliation, position),
  # pair (strata) and then average over the statistics, weighting according to the number of
  # of observations within each strata.
  stratified.selection <- base.vpvs %>%
    mutate(has_vpv=1) %>%
    group_by(viewer_affiliation, min_pos) %>%
    do(compute.proportions(.)) %>%
    ungroup() %>% group_by(viewer_affiliation) %>%
    summarise(
      pr.op.click.strat=wtd.mean(pr.op.click, stratum.weight),
      pr.same.click.strat=wtd.mean(pr.same.click, stratum.weight)
    ) %>%
    select(viewer_affiliation, pr.op.click.strat, pr.same.click.strat)

  naive.ranking <- base.pimps %>%
    group_by(viewer_affiliation) %>%
    do(compute.proportions(.))

  naive.ranking %>%
    inner_join(naive.selection, by='viewer_affiliation') %>%
    inner_join(stratified.selection, by='viewer_affiliation') %>%
    mutate(
      network=frac.op.pimp*(1-frac.op.pimp.rand) / (frac.op.pimp.rand*(1-frac.op.pimp))-1,
      ranking=(pr.op.vpv-pr.same.vpv)/pr.same.vpv,
      selection=(pr.op.click-pr.same.click)/pr.same.click,
      selection.adjusted=(pr.op.click.strat-pr.same.click.strat)/pr.same.click.strat
    )
}

pos.adjusted <- stratified.est(rdb, rdbi)

# Table S5
fracs <- pos.adjusted %>%
  select(viewer_affiliation, frac.op.pimp.rand, frac.op.pimp, frac.op.vpv, frac.op.click)
xtable(data.frame(fracs), digits=3)
fracs

# Table S6
som.ratio.table <- pos.adjusted %>%
  select(viewer_affiliation, network, ranking, selection, selection.adjusted)
xtable(data.frame(som.ratio.table), digits=3)
som.ratio.table
