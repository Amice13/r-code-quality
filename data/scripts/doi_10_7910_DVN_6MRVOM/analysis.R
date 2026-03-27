# Replication code for 'Pricing Immigration'

## packages
requireNamespace("rio")
requireNamespace("car")
requireNamespace("stargazer")
library("ggplot2")
library("survey")

# load data
d <- rio::import("data.sav")
stopifnot(dim(d) == c(3636, 45))

codebook <- lapply(d, attr, "label")

# ------
# The experiment consisted of a within-subjects and between-subjects design.
# First respondents were asked for their raw preferences for levels of eu and non-eu immigration (time 1).
# Then they were told about potentially high or low personal or social costs as a 2x2 factorial and
# their preferences were measured again (time 2).
# We preferences as support for a specific level of immigration (between 0 and 165000) and measure treatment effects
# as differences between experimental conditions and differences-in-differences between conditions between 
# time 1 (unconstrained) and time 2 (cost-constrained) preferences.
# ------

# recode
## generate outcome measure
d$Q1[d$Q1 == 997] <- NA_real_    # missing values
d$Q1b[d$Q1b == 997] <- NA_real_  # missing values
d$levels_eu1 <- as.numeric(d$Q1)
d$levels_noneu1 <- as.numeric(d$Q1b)
d$levels_eu2 <- as.numeric(d$migration_q2b)
d$levels_noneu2 <- as.numeric(d$migration_q3b)
d$levels_all1 <- d$levels_eu1 + d$levels_noneu1
d$levels_all2 <- d$levels_eu2 + d$levels_noneu2
d$diff_eu <- d$levels_eu2 - d$levels_eu1
d$diff_noneu <- d$levels_noneu2 - d$levels_noneu1
d$diff_all <- d$levels_all2 - d$levels_all1

## generate measures of reductions in immigration from current levels (165000 per year)
d$reduction_eu1 <- 165000 - as.numeric(d$levels_eu1)
d$reduction_noneu1 <- 165000 - as.numeric(d$levels_noneu1)
d$reduction_eu2 <- 165000 - as.numeric(d$levels_eu2)
d$reduction_noneu2 <- 165000 - as.numeric(d$levels_noneu2)
d$reduction_all1 <- d$reduction_eu1 + d$reduction_noneu1
d$reduction_all2 <- d$reduction_eu2 + d$reduction_noneu2
d$reductdiff_eu <- as.numeric(d$reduction_eu2) - as.numeric(d$reduction_eu1)
d$reductdiff_noneu <- as.numeric(d$reduction_noneu2) - as.numeric(d$reduction_noneu1)
d$reductdiff_all <- as.numeric(d$reduction_all2) - as.numeric(d$reduction_all1)

## manipulation check
attr(d$Q4, "labels") <- attr(d$Q4, "labels")[1:5]
d$check <- rio::factorize(d$Q4)

## Relative shares of EU and non-EU immigration (Q5)
d$eushare <- ifelse(d$ExpSplitQ5 == 1, d$Q5T, d$Q5)
d$eushare[d$eushare == 997] <- NA_real_

d$tr_eushare <- factor(d$ExpSplitQ5, levels = 1:2, labels = c("Information", "Control"))

## cost treatment variable
### ExpSplit2A == 1 (10%/personal income) 2 (10%/GDP) 3 (5%/personal income) 4 (5%/GDP)
d$tr <- factor(d$ExpSplit2A, levels = 4:1, labels = c("5% (GDP)", "5% (Personal Income)", "10% (GDP)", "10% (Personal Income)"))
d$tr_level <- car::recode(d$ExpSplit2A, "1=1;2=1;3=0;4=0;else=NA")
d$tr_gdp <- car::recode(d$ExpSplit2A, "1=0;2=1;3=0;4=1;else=NA")

## demographics
### vote in 2016 referendum on EU membership
d$leave <- factor(car::recode(d$pastvote_EURef, "2=1;1=0;else=NA"), levels = 0:1, labels = c("Remain", "Leave"))
d$conservative <- factor(car::recode(d$pastvote_2015, "1=1;2=0;else=NA"), levels = 0:1, labels = c("Labour", "Conservative"))
### social grade
d$socialgrade <- rio::factorize(d$socialgrade)

# build survey object to handle weighted analysis
svy <- svydesign(data = d, ids = ~ 0, weights = ~ W8)

svy_stacked <- svydesign(data = rbind(
                                  stats::setNames(cbind(d[c("reduction_eu1", "tr", "W8")], "EU (Unconstrained)"), c("reduction", "tr", "W8", "Group")),
                                  stats::setNames(cbind(d[c("reduction_noneu1", "tr", "W8")], "Non-EU (Unconstrained)"), c("reduction", "tr", "W8", "Group")), 
                                  stats::setNames(cbind(d[c("reduction_eu2", "tr", "W8")], "EU (Constrained)"), c("reduction", "tr", "W8", "Group")),
                                  stats::setNames(cbind(d[c("reduction_noneu2", "tr", "W8")], "Non-EU (Constrained)"), c("reduction", "tr", "W8", "Group"))),
                         ids = ~ 0, weights = ~ W8)
svy_stacked$variables$Group <- factor(svy_stacked$variables$Group, levels = c("EU (Unconstrained)", "EU (Constrained)", "Non-EU (Unconstrained)", "Non-EU (Constrained)"))

# -----
# Results
# -----

## Table 1
table(d$tr)
##   5% (GDP)  5% (Personal Income)      10% (GDP) 10% (Personal Income)
##        897                   903            914                   922

# Results reported in text
svymean(~ reduction_eu1 + reduction_noneu1 + reduction_all1 + reduction_eu2 + reduction_noneu2 + reduction_all2, design = svy, na.rm = TRUE)
##                    mean     SE
## reduction_eu1     91325 1201.8
## reduction_noneu1 105311 1115.4
## reduction_all1   196636 2210.3
## reduction_eu2     80543 1227.7
## reduction_noneu2  85136 1246.5
## reduction_all2   165679 2348.5

svyby(~ reduction_all1, ~ leave, design = svy, FUN = svymean, na.rm = TRUE)
##         leave reduction_all1       se
## Remain Remain       134972.6 3128.935
## Leave   Leave       249081.5 2313.804

# Footnote 5
## Relative share of EU immigration (no treatment effects, apparently!)
svymean(~ eushare, design = svy, na.rm = TRUE)
##          mean     SE
## eushare 46.87 0.6145
svyby(~ eushare, ~ tr_eushare, design = svy, FUN = svymean, na.rm = TRUE)
##              tr_eushare  eushare        se
## Information Information 46.98297 0.9185899
## Control         Control 46.75926 0.8188476
svyby(~ eushare, ~ tr, design = svy, FUN = svymean, na.rm = TRUE)
##                                          tr  eushare       se
## 5% (GDP)                           5% (GDP) 46.53247 1.122145
## 5% (Personal Income)   5% (Personal Income) 48.08681 1.211938
## 10% (GDP)                         10% (GDP) 45.61316 1.321380
## 10% (Personal Income) 10% (Personal Income) 47.32981 1.226158
svyby(~ eushare, ~ tr_eushare + tr, design = svy, FUN = svymean, na.rm = TRU$
##                                    tr_eushare                    tr  eushare       se
## Information.5% (GDP)              Information              5% (GDP) 45.25042 1.732320
## Control.5% (GDP)                      Control              5% (GDP) 47.66254 1.445773
## Information.5% (Personal Income)  Information  5% (Personal Income) 48.95943 1.783853
## Control.5% (Personal Income)          Control  5% (Personal Income) 47.18672 1.616826
## Information.10% (GDP)             Information             10% (GDP) 45.11826 1.972651
## Control.10% (GDP)                     Control             10% (GDP) 46.13919 1.737517
## Information.10% (Personal Income) Information 10% (Personal Income) 48.78517 1.711418
## Control.10% (Personal Income)         Control 10% (Personal Income) 45.98051 1.750371

## Figure 1
ggplot(
  svyby(~ reduction, ~ Group + tr, design = svy_stacked, FUN = svymean, na.rm = TRUE)
, aes(x = factor(Group), y = -reduction, group = factor(tr), colour = factor(tr))
) + 
  geom_point(position = position_dodge(width = .5)) + 
  geom_errorbar(aes(ymin = -reduction - (1.96 * se), ymax = -reduction + (1.96 * se)), width = 0, position = position_dodge(width = .5)) + 
  geom_hline(yintercept = 0, colour = "gray") +
  guides(colour = guide_legend(reverse = TRUE)) + 
  coord_flip() + 
  #ggtitle("Preferred Reductions in EU and non-EU Immigration", subtitle = "Unconstrained or Constrained by Personal or Societal Costs") + 
  ylab("Preferred Reduction in Immigration Levels") + 
  xlab("") +
  theme_minimal() + 
  theme(legend.position = c(0.7,0.2),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12, colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 12),
        legend.title = element_blank())
ggsave("figure-01.png", width = 8, height = 4)

## Table 2
stargazer::stargazer(
  svyglm(diff_eu ~ tr, design = svy),
  svyglm(diff_noneu ~ tr, design = svy),
  svyglm(diff_all ~ tr, design = svy),
  dep.var.caption = "Change in Preferred Immigration Levels",
  dep.var.labels.include = FALSE,
  column.labels = c("EU", "Non-EU", "All"),
  covariate.labels = c("5\\% Personal Income", "10\\% GDP", "10\\% Personal Income"),
  star.char = c("*"),
  star.cutoffs = c(0.05),
  notes = c("* $p<0.05$"), 
  notes.append = FALSE,
  model.numbers = FALSE,
  type = "latex",
  float = FALSE, 
  digits = 1,
  align = TRUE, 
  out = "table-02.tex"
)


## Figure 2
ggplot(d, aes(x = levels_all1, y = levels_all2)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(col = "gray", alpha = 0.4) + 
  geom_smooth(col = "black", method = "loess") + 
  scale_x_continuous(labels = scales::comma, limits = c(0,330000)) +
  scale_y_continuous(labels = scales::comma, limits = c(0,330000)) +
  #ggtitle("Preferred Constrained and Unconstrained Levels of Immigration", subtitle = "EU and non-EU Combined") + 
  xlab("Preferred Total Immigration without Costs") +
  ylab("Preferred Total Immigration with Costs") +
  coord_fixed() + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16, colour = "black"),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20),
  )
ggsave("figure-02.png", width = 8, height = 8)

## Figure 3
ggplot(d, aes(x = levels_eu1, y = levels_eu2)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(col = "gray", alpha = 0.4) + 
  geom_smooth(col = "black", method = "loess") + 
  scale_x_continuous(labels = scales::comma, limits = c(0,165000)) +
  scale_y_continuous(labels = scales::comma, limits = c(0,165000)) +
  #ggtitle("Preferred Constrained and Unconstrained Levels of Immigration", subtitle = "EU Immigration") + 
  xlab("Preferred EU Immigration without Costs") +
  ylab("Preferred EU Immigration with Costs") +
  coord_fixed() + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16, colour = "black"),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20),
  )
ggsave("figure-03-eu.png", width = 8, height = 8)

## Figure 3
ggplot(d, aes(x = levels_noneu1, y = levels_noneu2)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(col = "gray", alpha = 0.4) + 
  geom_smooth(col = "black", method = "loess") + 
  scale_x_continuous(labels = scales::comma, limits = c(0,165000)) +
  scale_y_continuous(labels = scales::comma, limits = c(0,165000)) +
  #ggtitle("Preferred Constrained and Unconstrained Levels of Immigration", subtitle = "Non-EU Immigration") + 
  xlab("Preferred Non-EU Immigration without Costs") +
  ylab("Preferred Non-EU Immigration with Costs") +
  coord_fixed() + 
  theme_minimal() +
  theme(
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16, colour = "black"),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 20),
  )
ggsave("figure-03-noneu.png", width = 8, height = 8)

## Table 3
stargazer::stargazer(
  svyglm(diff_all ~ tr, design = subset(svy, leave == "Leave")),
  svyglm(diff_all ~ tr, design = subset(svy, leave == "Remain")),
  dep.var.caption = "Change in Preferred Immigration Levels",
  dep.var.labels = "",
  column.labels = c("Leave", "Remain"),
  covariate.labels = c("5\\% Personal Income", "10\\% GDP", "10\\% Personal Income"),
  star.char = c("*"),
  star.cutoffs = c(0.05),
  notes = c("* $p<0.05$"),
  notes.append = FALSE,
  model.numbers = FALSE,
  type = "latex",
  float = FALSE,
  digits = 1,
  align = TRUE,
  out = "table-03.tex"
)


## Appendices

### Table A1
stargazer::stargazer(
  {tmp <- svy; tmp <- update(tmp, levels1 = levels_eu1); svyglm(levels_eu2 ~ levels1 * tr, design = tmp)},
  {tmp <- svy; tmp <- update(tmp, levels1 = levels_noneu1); svyglm(levels_noneu2 ~ levels1 * tr, design = tmp)},
  {tmp <- svy; tmp <- update(tmp, levels1 = levels_all1); svyglm(levels_all2 ~ levels1 * tr, design = tmp)},
  dep.var.caption = "Preferred Immigration Levels (Cost-Constrained)",
  dep.var.labels.include = FALSE,
  column.labels = c("EU", "Non-EU", "All"),
  covariate.labels = c(
    "Unconstrained",
    "5\\% Personal Income", "10\\% GDP", "10\\% Personal Income",
    "Unconstrained * 5\\% Personal Income", "Unconstrained * 10\\% GDP", "Unconstrained * 10\\% Personal Income"
    ),
  star.char = c("*"),
  star.cutoffs = c(0.05),
  notes = c("* $p<0.05$"), 
  notes.append = FALSE,
  model.numbers = FALSE,
  type = "latex",
  float = FALSE, 
  digits = 1,
  align = TRUE, 
  out = "table-a1.tex"
)

### Table A2
stargazer::stargazer(
  svyglm(diff_all ~ tr, design = subset(svy, socialgrade == "AB")),
  svyglm(diff_all ~ tr, design = subset(svy, socialgrade == "C1")),
  svyglm(diff_all ~ tr, design = subset(svy, socialgrade == "C2")),
  svyglm(diff_all ~ tr, design = subset(svy, socialgrade == "DE")),
  dep.var.caption = "Change in Preferred Immigration Levels",
  dep.var.labels = "",
  column.labels = c("AB", "C1", "C2", "DE"),
  covariate.labels = c("5\\% Personal Income", "10\\% GDP", "10\\% Personal Income"),
  star.char = c("*"),
  star.cutoffs = c(0.05),
  notes = c("* $p<0.05$"),
  notes.append = FALSE,
  model.numbers = FALSE,
  type = "latex",
  float = FALSE, 
  digits = 1,
  align = TRUE,
  out = "table-a2.tex"
)

### Table A3
stargazer::stargazer(
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Labour" & leave == "Leave")),
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Labour" & leave == "Remain")),
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Conservative" & leave == "Leave")),
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Conservative" & leave == "Remain")),
  dep.var.caption = "Change in Preferred Immigration Levels",
  dep.var.labels = "",
  column.labels = c("Lab.-Leave", "Lab.-Remain", "Cons.-Leave", "Cons.-Remain"),
  covariate.labels = c("5\\% Personal Income", "10\\% GDP", "10\\% Personal Income"),
  star.char = c("*"),
  star.cutoffs = c(0.05),
  notes = c("* $p<0.05$"),
  notes.append = FALSE,
  model.numbers = FALSE,
  type = "latex",
  float = FALSE,
  digits = 1,
  align = TRUE,
  out = "table-a3.tex"
)

### Table A3 (unreported version with just party split)
stargazer::stargazer(
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Labour")),
  svyglm(diff_all ~ tr, design = subset(svy, conservative == "Conservative")),
  dep.var.caption = "Change in Preferred Immigration Levels",
  dep.var.labels = "",
  column.labels = c("Labour", "Conservative"),
  covariate.labels = c("5\\% Personal Income", "10\\% GDP", "10\\% Personal Income"),
  model.numbers = FALSE,
  type = "latex", float = FALSE, 
  digits = 1, align = TRUE, 
  out = "table-a3-partyvote.tex"
)

### Figure A1
ggplot(d[d$leave %in% c("Remain", "Leave"),], aes(x = levels_all1, y = levels_all2)) + 
  geom_abline(slope = 1, intercept = 0) + 
  geom_point(col = "gray", alpha = 0.4) + 
  geom_smooth(aes(linetype = leave), colour = "black", method = "loess") + 
  scale_x_continuous(labels = scales::comma, limits = c(0,330000)) +
  scale_y_continuous(labels = scales::comma, limits = c(0,330000)) +
  #ggtitle("Preferred Constrained and Unconstrained Levels of Immigration", subtitle = "EU and non-EU Combined") + 
  xlab("Preferred Total Immigration without Costs") +
  ylab("Preferred Total Immigration with Costs") +
  coord_fixed() + 
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.2),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16, colour = "black"),
    #panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.text = element_text(size = 16),
    legend.title = element_blank(),
  )
ggsave("../figures/scatterplot-all-by-vote.png", width = 8, height = 8)

### Figure A2
ggplot(
  as.data.frame(svytable(~ check + tr, design = svy)),
  aes(x = tr, y = Freq, fill = check)
) +
  geom_col(position = position_fill(reverse = TRUE), size = 1) +
  scale_fill_grey() + 
  coord_flip() +
  #ggtitle("Perceived Cost of Reducing Immigration, by Treatment") +
  xlab("") +
  ylab("Proportion of Respondents (Weighted)") +
  guides(colour = guide_legend(reverse = TRUE, nrow = 1L)) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16, colour = "black"),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 8),
        legend.title = element_blank(),
        legend.key.size = unit(1, "line")
        )  
ggsave("figure-a2.png", width = 8, height = 3)

chisq.test(svytable(~ check + tr, design = svy))
##         Pearson's Chi-squared test              
##                                                 
## data:  svytable(~check + tr, design = svy)      
## X-squared = 27.209, df = 12, p-value = 0.00721  

chisq.test(svytable(~ check + tr_gdp, design = svy))
##         Pearson's Chi-squared test              
##                                                 
## data:  svytable(~check + tr_gdp, design = svy)  
## X-squared = 14.441, df = 4, p-value = 0.006012  

chisq.test(svytable(~ check + tr_level, design = svy))
##         Pearson's Chi-squared test              
##                                                 
## data:  svytable(~check + tr_level, design = svy)
## X-squared = 8.6832, df = 4, p-value = 0.06952   
