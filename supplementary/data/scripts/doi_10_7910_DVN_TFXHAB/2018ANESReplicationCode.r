############################################################
############################################################
## 2018 ANES Replication Code
############################################################
############################################################

# Models using survey package in R.
library(survey)
datnew <- svydesign(ids = ~1, data = dat, weights = dat$weight) 

mod1a <- svyglm(metoo ~ education + age + Gender + race + income + ideology2 + partyid2 + interest + media, design=datnew)
mod1b <- svyglm(metoo ~ education + age + Gender + race + income + ideology2 + interest + media + dem1, design=datnew)
mod1c <- svyglm(metoo ~ education + age + Gender + race + income + ideology2 + interest + media + rep1, design=datnew)
mod1d <- svyglm(metoo ~ education + age + race + income + ideology2 + partyid2 + interest + media + women, design=datnew)
mod1e <- svyglm(metoo ~ education + age + race + income + ideology2 + partyid2 + interest + media + men, design=datnew)
mod1f <- svyglm(metoo ~ education + age + Gender + race + income + ideology2 + interest + media + ind1, design=datnew)
mod1g <- svyglm(metoo ~ education + age + Gender*partyid2 + race + income + ideology2 + interest + media, design=datnew)

# Calculating predicted probabilities
library(effects)
eff1a <- effect("Gender", mod1a, default.levels=100)
eff1aa <- effect("partyid2", mod1a, default.levels=100)
eff1b <- effect("Gender", mod1b, default.levels=100)
eff1c <- effect("Gender", mod1c, default.levels=100)
eff1d <- effect("partyid2", mod1d, default.levels=100)
eff1e <- effect("partyid2", mod1e, default.levels=100)

# Robustness checks holding independent variables at different survey means
eff1bb <- effect("Gender", mod1b, default.levels=100, given.values=c("age" = 46.16, "education" = 3.25, "race" = .64, "income" = 5.7, "ideology2" = 3.94, "interest" = 2.11, "media" = 2.59))
eff1cc <- effect("Gender", mod1c, default.levels=100, given.values=c("age" = 46.16, "education" = 3.25, "race" = .64, "income" = 5.7, "ideology2" = 3.94, "interest" = 2.11, "media" = 2.59))

eff1dd <- effect("partyid2", mod1d, default.levels=100, given.values=c("age" = 46.16, "education" = 3.25, "race" = .64, "income" = 5.7, "ideology2" = 3.94, "interest" = 2.11, "media" = 2.59))
eff1ee <- effect("partyid2", mod1e, default.levels=100, given.values=c("age" = 46.16, "education" = 3.25, "race" = .64, "income" = 5.7, "ideology2" = 3.94, "interest" = 2.11, "media" = 2.59))


# Example code for creating figures plotting effects - excel datasets created from the calculated predicted probabilities
######### Plotting Effects  #########
GeomPointrange$draw_key <-  function (data, params, size)     {

         draw_key_vpath <- function (data, params, size) {
           # only need to change the x&y coords so that the line is horizontal
           # originally, the vertical line was `0.5, 0.1, 0.5, 0.9`
              segmentsGrob(0.1, 0.5, 0.9, 0.5, 
              gp = gpar(col = alpha(data$colour, data$alpha), 
              lwd = data$size * .pt, lty = data$linetype, 
              lineend = "butt"), arrow = params$arrow)
              }

    grobTree(draw_key_vpath(data, params, size), 
             draw_key_point(transform(data, size = data$size * 4), params))
}

# Load excel sheet with variable names and categories
dats <- read.xls(file.choose())

# Load dataset with predicted probabilities data - Example: samples split by party
dats1 <- read.xls(file.choose())

# Plot Feeling Thermometer
plot1 <-ggplot() +
  geom_pointrange(data=dats1, aes(y=estimate, x=variable, ymin = lower, ymax = upper, color=Gender)) +
stat_summary(show.legend=FALSE) +
scale_color_manual(values = c("black", "darkgray")) +
labs( x = "", y = "#MeToo Movement Feeling Thermometer") +
scale_x_discrete(limits=rev(dats$variable)) +
coord_flip(ylim=c(20,80)) +
theme_bw() + 
theme(legend.position = "none")
























###################### Notes ####################
# Note: In Table 3, Appendix B, there is a minor mislabelling issue in the descriptive statistics. The row of the table should read 35.3% Republican and 47.3% Democrat, and not the reverse as the final version does. 