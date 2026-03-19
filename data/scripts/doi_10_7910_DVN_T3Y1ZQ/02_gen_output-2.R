dir.create("plots")
dir.create("tables")

# Hypothetical examples of party polarization ####
ex <- data.frame(country = c(rep("A", 4),
                             rep("B", 4),
                             rep("E", 4),
                             rep("D", 4),
                             rep("C", 4),
                             rep("F", 4)), 
                 dim1 = c(0, 0, 10, 10,  
                          4.5, 4.5, 5.5, 5.5, 
                          0, 0, 10, 5,
                          2, 4, 6, 8,  
                          2, 4, 6, 8,
                          2, 4, 6, 8
                 ), 
                 dim2 = c(0, 10, 0, 10, 
                          5.5, 4.5, 4.5, 5.5,
                          0, 10, 0, 5,
                          2, 4, 6, 8, 
                          2, 6, 4, 8, 
                          6, 2, 8, 4))

res <- data.frame(country = unique(ex$country), md_v_shan = NA, ed = NA)

for (i in unique(ex$country)){
  tmp <- filter(ex, country == i)
  cov_tmp <- cov.wt(na.omit(tmp[,c("dim1", "dim2")]))
  x <- estimate.ED(cov_tmp$cov, cov.mat = TRUE, round.digits = 10)
  res$ed[res$country == i] <- x$n1
  res$md_v_shan[res$country == i] <- sum(diag(cov_tmp$cov) * x$n1/2)
}

res %>% 
  filter(country %in% c("A", "B", "C", "D")) %>% 
  mutate(md_v_shan = md_v_shan / (66+2/3),
         country_num = paste0(country, ": ED = ", round(ed, 2), ", Polar. = ", round(md_v_shan, 2))) %>% 
  merge(ex) %>% 
  ggplot(aes(dim1, dim2)) + geom_point(size = 3) + theme_bw() + facet_wrap(~country_num, ncol = 4) + xlab("") + ylab("")
ggsave("plots/fig3.png", width = 7.5, height = 7.5/4+.25, dpi = 600)

# The Relationship Between the Effective Number of Dimensions and
# Dimensional Correlation in Two Dimensions ###
x <- seq(0, 1, by = .01)
ed <- ((1-x)/2)^((x-1)/2) * ((1+x)/2)^((-x-1)/2)
data.frame(r = x, ed = ed) %>% 
  ggplot(aes(x = r, y = ed)) + geom_line() + theme_bw() + xlab(expression(rho)) + ylab("ED")
ggsave("plots/fig7.png", width = 3, height = 3, dpi = 600)

# pure descriptives ####
df_out <- read.csv("data/ches_level_measures.csv") %>% 
  mutate(country_year = paste0(country_label, ".", substring(year, 3, 4)))

df_out %>%
  summarize(cc1 = cor(lr_v, md_v_shan),
            cc2 = cor(ed_v, md_v_shan),
            cc3 = cor(ed_v, ed),
            cc4 = cor(md_shan, md_v_shan),
            cc5 = cor(md_eu_v_shan, md_v_shan),
            cc6 = cor(ed_v, ed_eu_v))

# Comparison of one- and two-dimensional party polarization measures ### 
df_out %>% 
  mutate(md_v_shan = md_v_shan / (66+2/3),
         lr_v = lr_v / 50) %>% 
  ggplot(aes(lr_v, md_v_shan, label = country_year)) + geom_smooth(method = "lm") + theme_bw() + geom_abline(slope = 1, intercept = 0) + geom_point() + 
  xlab("One-dimensional Measure") + ylab("Two-dimensional Measure")
ggsave("plots/fig5a.png", width = 4, height = 4, dpi = 600)

# Effective dimensionality by country ###
df_out %>% 
  ggplot(aes(forcats::fct_reorder(country_label, ed_v), ed_v)) + geom_boxplot() + theme_bw() + 
  xlab("Country") + ylab("Effective Dimensionality") + ylim(1, 2)
ggsave("plots/fig2.png", width = 8, height = 4, dpi = 600)

df_out %>% 
  ggplot(aes(forcats::fct_reorder(country_label, ed_eu_v), ed_eu_v)) + geom_boxplot() + theme_bw() + 
  xlab("Country") + ylab("Effective Dimensionality (incl. EU)") + ylim(1, 3)
ggsave("plots/fig9.png", width = 8, height = 4, dpi = 600)

# Two-dimensional party polarization by country ###
df_out %>% 
  mutate(md_v_shan = md_v_shan / (66+2/3)) %>% 
  ggplot(aes(forcats::fct_reorder(country_label, md_v_shan), md_v_shan)) + geom_boxplot() + theme_bw() + 
  xlab("Country") + ylab("Two-dimensional Measure")
ggsave("plots/fig4.png", width = 8, height = 4, dpi = 600)

df_out %>% 
  mutate(md_eu_v_shan = md_eu_v_shan / sum(diag(cov(expand.grid(c(0,10), c(0,10), c(0,10)))))) %>% 
  ggplot(aes(forcats::fct_reorder(country_label, md_eu_v_shan), md_eu_v_shan)) + geom_boxplot() + theme_bw() + 
  xlab("Country") + ylab("Three-dimensional Measure")
ggsave("plots/fig10.png", width = 8, height = 4, dpi = 600)

# appendix ####
ches <- read.csv("1999-2019_CHES_dataset_means(v1).csv") %>% 
  merge(df_out %>% select(country, country_label) %>% distinct())

df_out1 <- df_out %>% 
  select(country, country_label) %>% 
  distinct() %>% 
  mutate(ed = NA, econ = NA, galt = NA)

for (i in unique(ches$country)){
  df_tmp <- filter(ches, country == i) %>%
    dplyr::select(lrecon, galtan, vote) %>%
    na.omit()
  cov_tmp <- cov.wt(df_tmp[,c("lrecon", "galtan")], wt = df_tmp$vote)
  x <- estimate.ED(cov_tmp$cov, cov.mat = TRUE, round.digits = 10)
  df_out1[df_out1$country==i, c("econ", "galt")] <- diag(cov_tmp$cov)
  df_out1$ed[df_out1$country==i] <- x$n1
}

# Party positions in two-dimensions across countries and survey waves (CHES) ###
ches %>% 
  merge(df_out1) %>% 
  mutate(econ = econ / 50,
         galt = galt / 50,
         ed = paste0("", round(ed,2), " / ", round(econ,2), " / ", round(galt, 2))) %>% 
  select(lrecon, galtan, vote, country_label, ed) %>% 
  bind_rows(c(country_label = "", ed = "ED / V(Econ) / V(GAL-TAN)")) %>%
  ggplot(aes(lrecon, galtan)) + geom_point(aes(size = vote)) + facet_wrap(~country_label+ed, ncol = 5) + theme_bw() + xlim(c(0, 10)) + ylim(c(0, 10)) +
  xlab("Economic") + ylab("GAL-TAN") + 
  theme(legend.position = "bottom") + scale_size_continuous(name = "Vote share")
ggsave("plots/fig8.png", width = 10, height = 10, dpi = 600)

# ches %>% 
#   merge(df_out %>% select(country, year, country_label, md_v_shan, country_year) %>% distinct()) %>% 
#   filter(country_year %in% c("BG.02", "LV.10", "PL.06", "GR.19")) %>% 
#   mutate(md_v_shan = round(md_v_shan / (66+2/3), 2),
#          country_year = paste0(country_year, ", ", md_v_shan)) %>% 
#   ggplot(aes(lrecon, galtan)) + geom_point(aes(size = vote)) + facet_wrap(~country_year, ncol = 2) + theme_bw() + xlim(c(0, 10)) + ylim(c(0, 10)) +
#   xlab("Economic") + ylab("GAL-TAN") + 
#   theme(legend.position = "bottom") + scale_size_continuous(name = "Vote share")
# ggsave("plots/ex2.pdf", width = 4, height = 4)

# Mutually reinforcing (PT) and cross-cutting (NL) party polarization ###
ches %>% 
  merge(df_out %>% select(country, year, country_label, md_v_shan, country_year) %>% distinct()) %>% 
  filter(country_year %in% c("NL.19", "PT.19")) %>% 
  mutate(ccyy = relevel(as.factor(paste0(country_label, ", ", year)), ref = "PT, 2019"),
         party = if_else(party == "PVdD", "PvdD", party)) %>% 
  ggplot(aes(lrecon, galtan, label = party))+ geom_hline(aes(yintercept = 5)) + geom_vline(aes(xintercept = 5)) + geom_point(size = 2) + ggrepel::geom_label_repel() + facet_wrap(~ccyy, ncol = 2) + theme_bw() + xlim(c(0, 10)) + ylim(c(0, 10)) +
  xlab("Economic") + ylab("Cultural") + 
  theme(legend.position = "bottom") 
ggsave("plots/fig1b.png", width = 7.5, height = 3.5, dpi = 600)

ches %>% 
  merge(df_out %>% select(country, year, country_label, md_v_shan, country_year) %>% distinct()) %>% 
  filter(country_year %in% c("NL.19", "PT.19")) %>% 
  mutate(ccyy = relevel(as.factor(paste0(country_label, ", ", year)), ref = "PT, 2019"),
         party = if_else(party == "PVdD", "PvdD", party)) %>% 
  ggplot(aes(lrgen, 0,  label = party)) + geom_point(size = 2) + ggrepel::geom_label_repel() + facet_wrap(~ccyy, ncol = 2) + theme_bw() + xlim(c(0, 10)) + ylim(c(-2, 2)) +
  xlab("General Left-Right") + ylab("") + geom_hline(aes(yintercept = 0)) + 
  theme(legend.position = "bottom", axis.text.y = element_blank(), axis.ticks.y = element_blank())
ggsave("plots/fig1a.png", width = 7.5, height = 3.5, dpi = 600)

# some estimation ####
dff <- select(df_out, country, year, ed_v, md_v_shan, lr_v, galtan_v, econ_v) %>% 
  mutate(md_v_shan = md_v_shan / (66+2/3),
         across(c(lr_v, galtan_v, econ_v), function(.) . / 50),
         avg_v = (galtan_v + econ_v) / 2,
         diff = (md_v_shan - lr_v),
         diff_v = (avg_v - lr_v))

m1 <- lme4::lmer(diff ~ ed_v + diff_v + (1 | country), dff %>% mutate(diff = scale(diff), diff_v = scale(diff_v), ed_v = scale(ed_v)))
m2 <- lme4::lmer(abs(diff) ~ ed_v + abs(diff_v) + (1 | country), dff %>% mutate(diff = diff / sd(diff), diff_v = diff_v/sd(diff_v), ed_v = scale(ed_v)))

texreg::screenreg(list(m1, m2))
texreg::texreg(list(m1, m2), file = "tables/table1.tex", label = "table:app_diff", caption = "Differences in Polarization Measures",
               custom.coef.names = c("Intercept", "Eff. Dim.", "Diff. in Var.", "Abs. Diff. in Var."))


m0 <- lme4::lmer(lr_v ~ prop + effnr + (1|country), df_out %>% mutate(across(c(lr_v, prop, effnr, md_v_shan), scale)))
m1 <- lme4::lmer(md_v_shan ~ prop + effnr + (1|country), df_out %>% mutate(across(c(lr_v, prop, effnr, md_v_shan), scale)))
texreg::screenreg(list(m0, m1))
texreg::texreg(list(m0, m1), file = "tables/table2.tex", label = "table:desc", caption = "Polarization and System-level Features",
               custom.coef.names = c("Intercept", "Proportionality", "Eff. Nr. of Parties"))

plot_df <- data.frame(vars = c(names(fixef(m0))[-1],
                               names(fixef(m0))[-1]),
                      dv = c(rep("1D", 2),
                             rep("2D", 2)),
                      mm = c(fixef(m0)[-1], 
                             fixef(m1)[-1]),
                      sds = c(diag(as.matrix(vcov(m0)))[-1],
                              diag(as.matrix(vcov(m1)))[-1]))
plot_df %>% 
  mutate(vars = ifelse(vars == "prop", "Proportionality", "Effective Nr. of Parties")) %>% 
  ggplot(aes(vars, mm, ymin = mm - 1.96 * sqrt(sds), ymax = mm + 1.96 * sqrt(sds), shape = dv)) +
  geom_pointrange(position = position_dodge(width = .2)) + theme_bw() + geom_hline(aes(yintercept = 0)) + 
  xlab("") + ylab("Coefficient") + coord_flip() + theme(legend.position = "bottom", axis.text.y = element_text(angle = 90, hjust = .5)) + scale_shape_discrete(name = "")
ggsave("plots/fig5b.png", width = 4, height = 4, dpi = 600)

# more serious estimation ####
load("data/cses_level_measures.Rdata")

df_many <- df_many %>% 
  select(country, countrylong, electionyear, year, partisan, lr_v, md_v_shan, effnr, age, education, income, lr_self, challengers, galtan_salience, lrecon_salience, prop, gender, union, respid) %>%
  mutate(countryyear = paste0(country, ".", electionyear))

df_country <- df_many %>% 
  select(country, countrylong, electionyear, year, countryyear, lr_v, md_v_shan, effnr, prop, galtan_salience, lrecon_salience, challengers) %>% 
  distinct() %>% 
  mutate(galtan_sal_rel = galtan_salience - lrecon_salience) %>% 
  mutate(across(lr_v:galtan_sal_rel, scale))
df_individuals <- df_many %>% 
  select(country, countrylong, electionyear, year, countryyear, age, education, income, lr_self, partisan, gender, union, respid) %>% 
  distinct() %>% 
  mutate(across(age:lr_self, scale))
df_many <- left_join(df_individuals, df_country)

# main condition
m3 <- lme4::glmer(partisan ~ effnr + prop + lr_v + (1|country) + (1|electionyear), df_many, family = binomial("probit"))
m4 <- lme4::glmer(partisan ~ effnr + prop + md_v_shan + (1|country) + (1|electionyear), df_many, family = binomial("probit"))

m01 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                     lr_self + I(lr_self^2) + as.factor(union) + 
                     effnr + prop + lr_v + (1|country) + (1|electionyear), df_many, family = binomial("probit"))
m02 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                     lr_self + I(lr_self^2) + as.factor(union) + 
                     effnr + prop + md_v_shan + (1|country) + (1|electionyear), df_many, family = binomial("probit"))

m5 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                    lr_self + I(lr_self^2) + as.factor(union) + 
                    effnr + prop + lr_v + (1|country) + (1|electionyear), df_many %>% filter(year == electionyear | year == electionyear+1), family = binomial("probit"))
m6 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                    lr_self + I(lr_self^2) + as.factor(union) + 
                    effnr + prop + md_v_shan + (1|country) + (1|electionyear), df_many %>% filter(year == electionyear | year == electionyear+1), family = binomial("probit"))

m7 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income +
                    lr_self + I(lr_self^2) + as.factor(union) +
                    effnr + prop + lr_v + I(lr_v^2) + (1|country) + (1|electionyear), df_many, family = binomial("probit"))
m8 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income +
                    lr_self + I(lr_self^2) + as.factor(union) +
                    effnr + prop + md_v_shan + I(md_v_shan^2) + (1|country) + (1|electionyear), df_many, family = binomial("probit"))

# m1 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
#                     lr_self + I(lr_self^2) + as.factor(union) + 
#                     effnr + prop + lr_v*challengers + (1|country) + (1|electionyear), df_many, family = binomial("probit"))
m2 <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                    lr_self + I(lr_self^2) + as.factor(union) + effnr + prop + md_v_shan*challengers + (1|country) + (1|electionyear), df_many, family = binomial("probit"))
m2s <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                    lr_self + I(lr_self^2) + as.factor(union) + effnr + prop + md_v_shan*galtan_salience + (1|electionyear), df_many, family = binomial("probit"))
m2r <- lme4::glmer(partisan ~ age + I(age^2) + as.factor(gender) + education + income + 
                    lr_self + I(lr_self^2) + as.factor(union) + effnr + prop + md_v_shan*galtan_sal_rel + (1|electionyear), df_many, family = binomial("probit"))

texreg::screenreg(list(m3, m4, m01, m02, m5, m6, m7, m8, m2, m2s, m2r))
texreg::texreg(list(m3, m4, m01, m02, m5, m6, m7, m8, m2, m2s, m2r), file = "tables/table3.tex", label = "table:app", caption = "Polarization and Mass Partisanship", sideways = TRUE,
               custom.coef.map = list("lr_v" = "1D Polar.", "md_v_shan"= "2D Polar.", 
                                      "I(lr_v^2)" = "1D Polar.^2", "I(md_v_shan^2)"= "2D Polar.^2", 
                                      "md_v_shan:challengers"= "2D Polar.*2nd Dim. Parties",
                                      "md_v_shan:galtan_salience" = "2D Polar.*2nd Dim. Salience", "md_v_shan:galtan_sal_rel" = "2D Polar.*2nd Dim. Salience (rel.)",
                                      "challengers" = "2nd Dim. Parties", "galtan_salience" = "2nd Dim. Salience", "galtan_sal_rel" = "2nd Dim. Salience (rel.)",
                                      "effnr" = "Eff. Nr. of Parties", "prop" = "Proportionality", 
                                      "lr_self" = "LR Self", "I(lr_self^2)" = "LR Self^2", "as.factor(union)1" = "Union",
                                      "age" = "Age", "I(age^2)" = "Age^2", "as.factor(gender)2" = "Female",
                                      "education" = "Education", "income" = "Income", "(Intercept)" = "Intercept"),
               fontsize = "scriptsize")

coefs <- MASS::mvrnorm(1000, fixef(m01)[c("(Intercept)", "lr_v")], vcov(m01)[c("(Intercept)", "lr_v"),c("(Intercept)", "lr_v")])
v1 <- pnorm(c(1,0)) %*% t(coefs)
v2 <- pnorm(c(1,1)) %*% t(coefs)
vuni <- quantile(v2 - v1, c(.025, .5, .975))

coefs <- MASS::mvrnorm(1000, fixef(m02)[c("(Intercept)", "md_v_shan")], vcov(m02)[c("(Intercept)", "md_v_shan"),c("(Intercept)", "md_v_shan")])
v1 <- pnorm(c(1,0)) %*% t(coefs)
v2 <- pnorm(c(1,1)) %*% t(coefs)
vmulti <- quantile(v2 - v1, c(.025, .5, .975))

plot_df <- bind_rows(vuni, vmulti)
names(plot_df) <- c("min", "median", "max")
plot_df$version <- c("1D", "2D")
# Party polarization and mass partisanship ###
plot_df %>%
  ggplot(aes(version, median, ymin = min, ymax = max)) +
  geom_pointrange() + theme_bw() + geom_hline(aes(yintercept = 0)) +
  xlab("Polarization") + ylab("Difference in Pred. Probability") + coord_flip() + theme(legend.position = "bottom")
ggsave("plots/fig6a.png", width = 4, height = 3, dpi = 600)

# coefs <- MASS::mvrnorm(1000, fixef(m1)[c("(Intercept)", "lr_v", "lr_v:challengers")], vcov(m1)[c("(Intercept)", "lr_v", "lr_v:challengers"),c("(Intercept)", "lr_v", "lr_v:challengers")])
# v1 <- t(sapply(seq(-2, 2, by = .1), function(x) quantile(pnorm((c(1, 1, 1*x)) %*% t(coefs)) - pnorm((c(1, 0, 0*x)) %*% t(coefs)), c(.025, .5, .975))))

coefs <- MASS::mvrnorm(1000, fixef(m2)[c("(Intercept)", "md_v_shan", "challengers", "md_v_shan:challengers")], vcov(m2)[c("(Intercept)", "md_v_shan", "challengers","md_v_shan:challengers"),c("(Intercept)", "md_v_shan", "challengers","md_v_shan:challengers")])
v2 <- t(sapply(seq(-2, 2, by = .1), function(x) quantile(pnorm((c(1, 1, x, 1*x)) %*% t(coefs)) - pnorm((c(1, 0, x, 0*x)) %*% t(coefs)), c(.025, .5, .975))))

plot_df <- bind_rows(# as.data.frame(v1) %>% mutate(version = "1D", fake = seq(-2, 2, by = .1)), 
                     as.data.frame(v2) %>% mutate(version = "2D", fake = seq(-2, 2, by = .1)))
names(plot_df)[1:3] <- c("lb", "m", "ub")

tmp2 <- select(df_many, country, year, challengers) %>% distinct()
tmp <- plot_df %>% 
  filter(version == "2D")

ggplot() + 
  geom_ribbon(data = tmp, aes(fake, m, ymin = lb, ymax = ub), alpha = .1 ) + 
  geom_line(data = tmp, aes(fake, m, ymin = lb, ymax = ub), ) + geom_hline(aes(yintercept = 0)) + theme_bw() + ylab("Difference in Pred. Probability") + xlab("Strength of 2nd Dim. Parties") +
  geom_rug(aes(x = tmp2$challengers)) + xlim(-2, 2)
ggsave("plots/fig6b.png", width = 4, height = 3, dpi = 600)
