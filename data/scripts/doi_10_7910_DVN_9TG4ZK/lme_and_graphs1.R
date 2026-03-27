# R code to produce mixed linear models and graph the results

# Unraveling Global Impacts of Climate Change on Amphibians Distributions: 
# A Life-History and Biogeographic-Based Approach (paper in review)

library(effects)
library(MuMIn)
library(emmeans)
library(performance)
library(lme4)
library(lmerTest)

setwd("G:/Meu Drive/Doutorado/Projeto/Artigo_revisao/AmphiBIO_v1/output")
list.files()

data_complete <- read.csv("data_complete_data_localities.csv", header = T)
head(data_complete)
summary(data_complete)

## Global models
# without using resolution as covariate
global <- lmer(sqrt(prop_rel) ~ habitat_type  + realm +
                  sqrt(dist_current) +   mean_elev +
                  reproductive_mode + habit + sqrt(body_size_mm) + mean_b3_pres + 
                  (1|specie_match_iucn) + (1|author), REML = FALSE, data_complete)

drop1(global)
summary(global)

# using resolution as covariate
global2 <- lmer(sqrt(prop_rel) ~ habitat_type  + realm +
                 sqrt(dist_current) +   mean_elev +
                 reproductive_mode + habit + sqrt(body_size_mm) + mean_b3_pres + 
                 (1|specie_match_iucn) + (1|author)+ (1|res), REML = FALSE, data_complete)

summary(global2)

#residuals
qqnorm(resid(global))

## Model selection
options(na.action = "na.fail")
dredgemod_mr <- dredge(global, m.lim =c(0,3))

## AICc
dredgemod_sel <- MuMIn::model.sel(dredgemod_mr, rank = "AICc")

## Average model
avgm_mr <- model.avg(dredgemod_sel)
summary(avgm_mr)

## Variable importance (sum of model weights)
data.frame(sw(avgm_mr))


#### Graphs #####
# model with the significant variables
m_best <- lmer(sqrt(prop_rel) ~ habitat_type + 
                 sqrt(dist_current) + iucn_cat +
                 mean_b3_pres + mean_elev +
                 realm + (1|specie_match_iucn) + habit +
                 (1|author) + (1|res), REML = FALSE, data_complete)
summary(m_best)

## visualize
emmeans(m_best, "habitat_type")
plot(ref_grid(m_best)) #coeficients and CI

p <- allEffects(m_best)
plot(p$`sqrt(dist_current)`)

## visualize with EMMEANS
library(emmeans)
library(ggeffects)
library(ggplot2)

# Isothermality
g_bio3 <- ggemmeans(m_best, terms = "mean_b3_pres", type = "re")
g_bio3

i1 <- emmeans(m_best, ~ mean_b3_pres, infer = TRUE)
i1

g_bio3 <- ggplot(g_bio3, aes(x, predicted)) +
  geom_smooth(method = lm, se = T,
              colour = "black", lwd = .8) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_continuous(name = "Isothermality") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  theme(legend.position = c(.25, .9)) +
  theme(legend.title = element_blank())

g_bio3 <- g_bio3 +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.length = unit(.25, "cm"))
windows()
g_bio3

# Elevation
g_elev <- ggemmeans(m_best, terms = "mean_elev", type = "re")
g_elev

e1 <- emmeans(m_best, ~ mean_elev, infer = TRUE)
e1

g_elev <- ggplot(g_elev, aes(x, predicted)) +
  geom_smooth(method = lm, se = T,
              colour = "black", lwd = .8) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_continuous(name = "Elevation") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  theme(legend.position = c(.25, .9)) +
  theme(legend.title = element_blank())

g_elev <- g_elev +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.length = unit(.25, "cm"))

g_elev

# Dist current
g_area <- ggemmeans(m_best, terms = "dist_current", type = "re", infer = TRUE)

a1 <- emmeans(m_best, ~"sqrt(dist_current)", infer = TRUE)
a1

g_area <- ggplot(g_area, aes(x, predicted)) +
  geom_smooth(method = lm, se = T,
              colour = "black", lwd = .8) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_continuous(name = "Baseline area") +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  theme(legend.position = c(.25, .9)) +
  theme(legend.title = element_blank())

g_area <- g_area +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(legend.text = element_text(size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.ticks.length = unit(.25, "cm"))

g_area

# Realm
pd <- position_dodge(.35)

c1 <- emmeans(m_best, ~ realm, pbkrtest.limit = 6000, infer = TRUE)
c1
c1 <- multcomp::cld(c1,
                    alpha = 0.05,
                    Letters = letters,
                    adjust = "sidak")

c1 <- as.data.frame(c1)

c1$order <- factor(c1$realm, levels = c("Neotropic", "Indo-Malayan", "Indo/Palearctic", "Palearctic", "Nearctic/Neotropic", "Nearctic", "Oceania", "Afrotropic"))

g_realm <- ggplot(c1, aes(x = reorder(realm, -emmean),
                          y = emmean)) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_discrete(name = "Ecoregion") +
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                width = .2,
                size = .5) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2.2) +
  geom_text(show.legend = F,
            aes(x = realm, y = emmean + SE,
                label = gdata::trim(.group),
                fontface = 2),
            position = pd,
            hjust = 0.5,
            vjust = -.5,
            size = 5)

g_realm <- g_realm +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(axis.ticks.length = unit(.25, "cm"))

g_realm


# Habitat type
pd <- position_dodge(.35)

c2 <- emmeans(m_best, ~ habitat_type, pbkrtest.limit = 6000, infer = TRUE)
c2
c2 <- multcomp::cld(c2,
                    alpha = 0.05,
                    Letters = letters,
                    adjust = "sidak")

c2 <- as.data.frame(c2)

c2$order <- factor(c2$habitat_type, levels = c("Rupestrial", "Forest", "Forest_Open", "Wetlands", "Open_areas", "Dry_env"))

g_habitat <- ggplot(c2, aes(x = reorder(habitat_type, -emmean),
                            y = emmean)) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_discrete(name = "Habitat") +
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                width = .2,
                size = .5) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2.2) +
  geom_text(show.legend = F,
            aes(x = habitat_type, y = emmean + SE,
                label = gdata::trim(.group),
                fontface = 2),
            position = pd,
            hjust = 0.5,
            vjust = -.5,
            size = 5)

g_habitat <- g_habitat +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(axis.ticks.length = unit(.25, "cm"))

g_habitat

# Iucn category
pd <- position_dodge(.35)

c3 <- emmeans(m_best, ~ iucn_cat, pbkrtest.limit = 6000)

c3 <- multcomp::cld(c3,
                    alpha = 0.05,
                    Letters = letters,
                    adjust = "sidak")

c3 <- as.data.frame(c3)

c3$order <- factor(c3$iucn_cat, levels = c("EN", "VU", "LC", "DD", "NT", "CR"))

g_iucn <- ggplot(c3, aes(x = reorder(iucn_cat, -emmean),
                         y = emmean)) +
  scale_y_continuous(name = "Predicted proportion of suitable area") +
  scale_x_discrete(name = "IUCN category") +
  geom_errorbar(aes(ymin = emmean - SE,
                    ymax = emmean + SE),
                width = .2,
                size = .5) +
  theme_classic() +
  theme(plot.margin = unit(c(1, 1, 1, 1), 'cm')) +
  geom_line(position = pd) +
  geom_point(position = pd, size = 2.2) +
  geom_text(show.legend = F,
            aes(x = iucn_cat, y = emmean + SE,
                label = gdata::trim(.group),
                fontface = 2),
            position = pd,
            hjust = 0.5,
            vjust = -.5,
            size = 5)

g_iucn <- g_iucn +
  theme(axis.text = element_text(colour = "black",
                                 size = 15)) +
  theme(text = element_text(size = 15)) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  theme(axis.ticks.length = unit(.25, "cm"))

g_iucn
