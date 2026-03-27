############################################################
############################################################
## 2020 ANES Replication Code
############################################################
############################################################

# Models using survey package in R.
library(survey)
svy.dat <- svydesign(ids = ~1, data = dat, weights = dat$V200010a) 

mod1a <- svyglm(metoof ~ education + age + gender + race + income + ideology + partyid + interest + media, design=svy.dat)
mod1b <- svyglm(metoof ~ education + age + gender + race + income + ideology + interest + media + dem, design=svy.dat)
mod1c <- svyglm(metoof ~ education + age + gender + race + income + ideology + interest + media + rep, design=svy.dat)
mod1d <- svyglm(metoof ~ education + age + race + income + ideology + partyid + interest + media + women, design=svy.dat)
mod1e <- svyglm(metoof ~ education + age + race + income + ideology + partyid + interest + media + men, design=svy.dat)
mod1f <- svyglm(metoof ~ education + age + gender + race + income + ideology + interest + media + ind, design=svy.dat)
mod1g <- svyglm(metoof ~ education + age + gender*partyid + race + income + ideology + interest + media, design=svy.dat)
mod1h <- svyglm(metoof ~ gender*partyid, design=svy.dat)

# Calculating predicted probabilities
library(effects)
eff1a <- effect("gender", mod1a, default.levels=100)
eff1aa <- effect("partyid", mod1a, default.levels=100)
eff1b <- effect("gender", mod1b, default.levels=100)
eff1c <- effect("gender", mod1c, default.levels=100)
eff1d <- effect("partyid", mod1d, default.levels=100)
eff1e <- effect("partyid", mod1e, default.levels=100)
eff1f <- effect("gender", mod1f, default.levels=100)

# Robustness checks holding independent variables at different survey means
eff1bb <- effect("gender", mod1b, default.levels=100, given.values=c("age" = 48.37, "education" = 3.09, "race" = .66, "income" = 13.03, "ideology" = 5.7, "interest" = 1.85, "media" = 2.42))
eff1cc <- effect("gender", mod1c, default.levels=100, given.values=c("age" = 48.37, "education" = 3.09, "race" = .66, "income" = 13.03, "ideology" = 5.7, "interest" = 1.85, "media" = 2.42))

eff1dd <- effect("partyid", mod1d, default.levels=100, given.values=c("age" = 48.37, "education" = 3.09, "race" = .66, "income" = 13.03, "ideology" = 5.7, "interest" = 1.85, "media" = 2.42))
eff1ee <- effect("partyid", mod1e, default.levels=100, given.values=c("age" = 48.37, "education" = 3.09, "race" = .66, "income" = 13.03, "ideology" = 5.7, "interest" = 1.85, "media" = 2.42))


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

# Load dataset with predicted probabilities data - Example: samples split by Gender
dats1 <- read.xls(file.choose())

# Plot Feeling Thermometer
plot1 <-ggplot() +
  geom_pointrange(data=dats1, aes(y=estimate, x=variable, ymin = lower, ymax = upper, color=Party)) +
stat_summary(show.legend=FALSE) +
scale_color_manual(values = c("darkgray", "black")) +
labs( x = "", y = "#MeToo Movement Feeling Thermometer") +
scale_x_discrete(limits=rev(dats$variable)) +
coord_flip(ylim=c(20,80)) +
theme_bw() + 
theme(legend.position = "none")





















###################### Notes ####################
# Note: The post-election survey post stratification weight variable appeared to have issue in the 24th May 2021 release of the 2020 ANES when using it in conjunction with the survey package. We were receiving errors when using the weights with the survey package. Therefore, we utilized the pre-election survey weights. However, we utilized the basic "lm" command and "weights" function using the post-election weights as a robustness check. The results were substantively the same. 


###################### Other Article Notes ####################
# Note: In Footnote 10, it should say Figure 3 instead of Figure 1.