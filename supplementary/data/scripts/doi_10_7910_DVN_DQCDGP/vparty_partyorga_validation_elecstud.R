
##### ToC V-Party Validation ----

# Figures and Regression Analysis for Electoral Studies Data Paper

# 1 Load main data
# 2 Figures for main text
# 3 Regression analysis





##### Prerequisites ----

    # clean env
rm(list = ls())

    # load packages
library(tidyverse)
library(fuzzyjoin)
library(grid)
library(gridExtra)
library(ggridges)
library(ggpubr)
library(lme4)
library(dotwhisker)
library(broom.mixed)
library(ggeffects)
library(export)

    # load v-party
load("./data/vparty_for_partyorga.Rdata")





## Figure 1 Face Validity ----

     
    ###### Germany ----
line1 <- 
    df %>% 
    filter(v2paid == "1816") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Germany: B90/Grue")

    ###### Hungary ----
line2 <- 
    df %>%
    filter(v2paid == "1691") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Hungary: Fidesz")

    ###### Turkey ----
line3 <- 
    df %>%
    filter(v2paid == "306") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Turkey: AKP")

    ###### Brazil ----
line4 <- 
    df %>%
    filter(v2paid == "356") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Brazil: PT")

    ###### Mexico ----
line5 <- 
    df %>%
    filter(v2paid == "1474") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Mexico: PRI")

    ###### Japan ----
line6 <- 
    df %>%
    filter(v2paid == "1746") %>% 
    select(year, orgext, powercon, cohesion) %>% 
    ggplot(aes(x = year)) +
    coord_cartesian(ylim = c(-6, 6)) +
    geom_line(aes(y = orgext,   color = "Organizational extensiveness"),    color = "#377EB8", size = 1) +
    geom_line(aes(y = powercon, color = "Intra-party power concentration"), color = "#4DAF4A", size = 1) +
    geom_line(aes(y = cohesion, color = "Elite cohesion"),                  color = "#E41A1C", size = 1) +
    scale_y_continuous(limits = c(-100, 100), breaks = waiver()) +
    scale_x_continuous(limits = c(1970, 2020), breaks = waiver()) + 
    theme_minimal() +
    theme(legend.position = "none",
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    ggtitle("Japan: LDP")

    # Write outfile for main text
grid.newpage()
#combo_out <- ggarrange(line1, line2, line3, line4, line5, line6, ncol = 3, nrow = 2, common.legend = TRUE, legend="bottom")
grid.newpage()
combo_out <- 
    ggarrange(line1, line2, line3, line4, line5, line6, ncol = 3, nrow = 2, align = "h") %>% 
    annotate_figure(bottom = text_grob("\u25a0 Organizational extensiveness",    face = "bold", size = 10, color = "#377EB8", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Intra-party power concentration", face = "bold", size = 10, color = "#4DAF4A", hjust = 0, x = 0.3)) %>% 
    annotate_figure(bottom = text_grob("\u25a0 Elite cohesion",                  face = "bold", size = 10, color = "#E41A1C", hjust = 0, x = 0.3))

#graph2svg(x = combo_out, file = "./analyze_partyorga/elecstud_fig1.svg", width = 8, height = 8) # for print
graph2png(x = combo_out, file = "./analyze_partyorga/elecstud_fig1.png", width = 8, height = 8) # for submission





## Figure 2 Criterion Validity ----


    ###### Rohrschneider and Whitefield 2012 ----
match1 <- crossv_rw12 %>% select(v2paid, year, q21, q22)
match2 <- df %>% select(v2x_regime, v2paid, year, v2paactcom, v2pasoctie)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, q21, q22, v2paactcom, v2pasoctie) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, q21, q22, v2paactcom, v2pasoctie) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat1 <- 
    overlap %>%
    ggplot(aes(q21, v2paactcom)) + 
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Membership base (q21)", y = "Active community presence (v2paactcom)")

scat2 <-
    overlap %>%
    ggplot(aes(q22, v2pasoctie)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Affiliation with interest groups (q22)", y = "Affiliate organizations (v2pasoctie)")


    ###### Kitschelt (2013) ----
match1 <- crossv_dalp %>% select(v2paid, year, a1, a5) %>% mutate(a1rev = a1 * -1)
match2 <- df %>% select(v2x_regime, v2paid, year, v2palocoff, v2panom)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, a1rev, a5, v2palocoff, v2panom) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, a1rev, a5, v2palocoff, v2panom) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat3 <- 
    overlap %>%
    ggplot(aes(a1rev, v2palocoff)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Maintain offices at the local level (a1)", y = "Local party offices (v2palocoff)")

scat4 <-
    overlap %>% 
    ggplot(aes(a5, v2panom)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "pearson", p.accuracy = 0.001, r.accuracy = 0.001, label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Candidate selection for elections (a5)", y = "Candidate nomination (v2panom)")


    ###### Janda (1980) ----
match1 <- crossv_ja80 %>% select(v2paid, year, selparliacan, ideofac) %>% mutate(selparliacanrev = selparliacan * -1, ideofacrev = ideofac * -1)
match2 <- df %>% select(v2x_regime, v2paid, year, v2panom, v2padisa)
overlap <- match1 %>% fuzzy_left_join(match2, by = c("v2paid" = "v2paid", "year" = "year"), match_fun = list(`==`, `>=`)) %>% filter(year.x - 5 <= year.y) %>% filter(v2x_regime > 0)

tmp_n <- overlap %>% select(v2paid.x, selparliacanrev, ideofacrev, v2panom, v2padisa) %>% filter(complete.cases(.)) %>% nrow() # no of obs
tmp_N <- overlap %>% select(v2paid.x, selparliacanrev, ideofacrev, v2panom, v2padisa) %>% filter(complete.cases(.)) %>% distinct(v2paid.x) %>% nrow() # no of obs
annotation <- paste0("n = ", tmp_n, ", N = ", tmp_N)

scat5 <- 
    overlap %>%
    ggplot(aes(selparliacanrev, v2panom)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "spearman", p.accuracy = 0.001, r.accuracy = 0.001, cor.coef.name = "rho", label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Selecting parliamentary candidates (selparliacan)", y = "Candidate nomination (v2panom)")

scat6 <-
    overlap %>% 
    ggplot(aes(ideofacrev, v2padisa)) +
    geom_point(alpha=0.1) + 
    geom_smooth(method = "lm", size=2) + 
    stat_cor(method = "spearman", p.accuracy = 0.001, r.accuracy = 0.001, cor.coef.name = "rho", label.sep = sprintf(paste0(", ", annotation, ","))) +
    theme_minimal() + theme(axis.title=element_text(size=8)) +
    labs(x = "Struggle over ideology (ideofac)", y = "Elite cohesion (v2padisa)")



    # Write outfile for main text
combo1 <- ggarrange(scat1, scat2, ncol = 2) %>% annotate_figure(top = text_grob("Rohrschneider and Whitefield (2012)", face = "bold", size = 10))
combo2 <- ggarrange(scat3, scat4, ncol = 2) %>% annotate_figure(top = text_grob("\n Kitschelt (2013)", face = "bold", size = 10))
combo3 <- ggarrange(scat5, scat6, ncol = 2) %>% annotate_figure(top = text_grob("\n Janda (1980)", face = "bold", size = 10))

grid.newpage()
combo_out <- ggarrange(combo1, combo2, combo3, ncol = 1)
#graph2svg(x = combo_out, file = "./analyze_partyorga/elecstud_fig2.svg", width = 8, height = 10.5) # for print
graph2png(x = combo_out, file = "./analyze_partyorga/elecstud_fig2.png", width = 8, height = 8) # for submission





##### Figure 3 Construct Validity ----


    # define common sample as some obs do not have complete data e.g. due to lags
df_analysis <- 
    df %>%
    filter(v2x_regime > 0) %>% 
    select(death1, orgext, powercon, v2padisa, statefund, logconsecel, v2x_polyarchy, parl, proportional, mixed, logcounter1, country_id, v2paid) %>%
    filter(complete.cases(.))

    # base model
m1 <- glmer(death1 ~ orgext + powercon + v2padisa + logcounter1 + (1|country_id) + (1|v2paid),
            family = binomial(link = "logit"), control = glmerControl(optimizer = "Nelder_Mead"), nAGQ = 0, data = df_analysis)
summary(m1)

    # model including controls
m2 <- glmer(death1 ~ orgext + powercon + v2padisa + statefund + logconsecel + v2x_polyarchy + parl + proportional + mixed + logcounter1 + (1|country_id) + (1|v2paid),
            family = binomial(link = "logit"), control = glmerControl(optimizer = "Nelder_Mead"), nAGQ = 0, data = df_analysis)
summary(m2)

    # prepare output
m1tidy <- tidy(m1) %>% mutate(model = "Model 1")
m2tidy <- tidy(m2) %>% mutate(model = "Model 2")

models_out <- bind_rows(m1tidy, m2tidy) %>% filter(!term %in% c("(Intercept)", "sd__(Intercept)"))


    # predicted probabilities 
m1pred <- ggpredict(m2, terms = c("orgext[all]"))
plot1 <- plot(m1pred, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black", alpha = 0.05) + ggtitle("") + ylab("Pr(Party death)") + xlab("Organizational extensiveness") + ylim(0,1)

m2pred <- ggpredict(m2, terms = c("powercon[all]"))
plot2 <- plot(m2pred, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black", alpha = 0.05) + ggtitle("") + ylab("Pr(Party death)") + xlab("Intra-party power concentration") + ylim(0,0.8)

m2pred1 <- ggpredict(m2, terms = c("v2padisa[all]"))
plot3 <- plot(m2pred1, ci.style = "ribbon", line.size = 0.8) + geom_rug(sides = "b", color = "black", alpha = 0.05) + ggtitle("") + ylab("Pr(Party death)") + xlab("Elite cohesion") + ylim(0,0.8)

grid.newpage()
combo_out <- ggarrange(plot1, plot2, plot3, nrow = 1)
#graph2svg(x = combo_out, file = "./analyze_partyorga/elecstud_fig3.svg", width = 8, height = 4) # for print
graph2png(x = combo_out, file = "./analyze_partyorga/elecstud_fig3.png", width = 8, height = 4) # for submission

