library(data.table)
library(interflex)
library(multcomp)
library(texreg)
library(readxl)

set.seed(1989)

# set working directory to appropriate location on your computer
# setwd("")

full_data_county <- fread("full_data_county.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_town <- fread("full_data_town.csv", header = TRUE, stringsAsFactors = FALSE)
full_data_town_LBBH <- fread("full_data_town_LBBH.csv", header = TRUE, stringsAsFactors = FALSE)

# Figure SI.3

# marginal effects for Models 1a and 1b in Tables 1 and 2

tab3mod1a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1a)

tab3mod1b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1b)

tab4mod1a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1a)

tab4mod1b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1b)

tab3mod1a_preds <- predict(tab3mod1a, newdata = data.frame("beach" = c(rep(0,101),rep(1,101)),
                                                           "Num_hotels_per1000_rescaled_noessex" = rep(seq(0,1,0.01),2),
                                                           "machine_ab" = 0,
                                                           "wilson1912" = median(full_data_county$wilson1912)),
                           se.fit = TRUE)

tab3mod1b_preds <- predict(tab3mod1b, newdata = data.frame("beach" = c(rep(0,101),rep(1,101)),
                                                           "Num_hotelrooms_per1000_rescaled_noessex" = rep(seq(0,1,0.01),2),
                                                           "machine_ab" = 0,
                                                           "wilson1912" = median(full_data_county$wilson1912)),
                           se.fit = TRUE)

tab4mod1a_preds <- predict(tab4mod1a, newdata = data.frame("Beach" = c(rep(0,101),rep(1,101)),
                                                           "Num_hotels_per100_rescaled1" = rep(seq(0,1,0.01),2)),
                           se.fit = TRUE)

tab4mod1b_preds <- predict(tab4mod1b, newdata = data.frame("Beach" = c(rep(0,101),rep(1,101)),
                                                           "Num_hotelrooms_per100_rescaled1" = rep(seq(0,1,0.01),2)),
                           se.fit = TRUE)

png(file = "Figures//marginal_effects_percapita_rescaled.png", family = "Times", height = 16, width = 14, units = "in", res = 500)
layout(matrix(c(1,2,3,4,5,5), nrow=3, byrow=TRUE), 
       heights = c(0.40, 0.40, 0.20))
plot(y = tab3mod1a_preds$fit[1:101],
     x = seq(0, 1, 0.01),
     type = "l",
     lwd = 2,
     ylim = c(0.10, 0.70),
     ylab = "Wilson 1916 Vote Share",
     xlab = "Relative concentration of hotels per 1,000 people (rescaled)",
     main = "Table 1, Model 1a",
     cex.main = 2,
     cex.lab = 1.5)
lines(y = tab3mod1a_preds$fit[1:101] + 1.96*tab3mod1a_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab3mod1a_preds$fit[1:101] - 1.96*tab3mod1a_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab3mod1a_preds$fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2,
      lwd = 2)
lines(y = tab3mod1a_preds$fit[102:202] + 1.96*tab3mod1a_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
lines(y = tab3mod1a_preds$fit[102:202] - 1.96*tab3mod1a_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
points(x = full_data_county$Num_hotels_per1000_rescaled_noessex[which(full_data_county$beach==1)],
       y = full_data_county$wilson1916[which(full_data_county$beach==1)],
       pch = 19,
       cex = 2)
points(x = full_data_county$Num_hotels_per1000_rescaled_noessex[which(full_data_county$beach==0)],
       y = full_data_county$wilson1916[which(full_data_county$beach==0)],
       pch = 17,
       cex = 2)

plot(y = tab3mod1b_preds$fit[1:101],
     x = seq(0, 1, 0.01),
     type = "l",
     lwd = 2,
     ylim = c(0.10, 0.80),
     ylab = "Wilson 1916 Vote Share",
     xlab = "Relative concentration of hotel rooms per 1,000 people (rescaled)",
     main = "Table 1, Model 1b",
     cex.main = 2,
     cex.lab = 1.5)
lines(y = tab3mod1b_preds$fit[1:101] + 1.96*tab3mod1b_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab3mod1b_preds$fit[1:101] - 1.96*tab3mod1b_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab3mod1b_preds$fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2,
      lwd = 2)
lines(y = tab3mod1b_preds$fit[102:202] + 1.96*tab3mod1b_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
lines(y = tab3mod1b_preds$fit[102:202] - 1.96*tab3mod1b_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
points(x = full_data_county$Num_hotelrooms_per1000_rescaled_noessex[which(full_data_county$beach==1)],
       y = full_data_county$wilson1916[which(full_data_county$beach==1)],
       pch = 19,
       cex = 2)
points(x = full_data_county$Num_hotelrooms_per1000_rescaled_noessex[which(full_data_county$beach==0)],
       y = full_data_county$wilson1916[which(full_data_county$beach==0)],
       pch = 17,
       cex = 2)

plot(y = tab4mod1a_preds$fit[1:101],
     x = seq(0, 1, 0.01),
     type = "l",
     lwd = 2,
     ylim = c(-0.5, 0.5),
     ylab = "Change in Wilson Vote Share",
     xlab = "Relative concentration of hotels per 100 people (rescaled)",
     main = "Table 2, Model 1a",
     cex.main = 2,
     cex.lab = 1.5)
lines(y = tab4mod1a_preds$fit[1:101] + 1.96*tab4mod1a_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab4mod1a_preds$fit[1:101] - 1.96*tab4mod1a_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab4mod1a_preds$fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2,
      lwd = 2)
lines(y = tab4mod1a_preds$fit[102:202] + 1.96*tab4mod1a_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
lines(y = tab4mod1a_preds$fit[102:202] - 1.96*tab4mod1a_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
points(x = full_data_town$Num_hotels_per100_rescaled1[which(full_data_town$Beach==1)],
       y = full_data_town$votechange[which(full_data_town$Beach==1)],
       pch = 19,
       cex = 2)
points(x = full_data_town$Num_hotels_per100_rescaled1[which(full_data_town$Beach==0)],
       y = full_data_town$votechange[which(full_data_town$Beach==0)],
       pch = 17,
       cex = 2)

plot(y = tab4mod1b_preds$fit[1:101],
     x = seq(0, 1, 0.01),
     type = "l",
     lwd = 2,
     ylim = c(-0.7, 0.5),
     ylab = "Change in Wilson Vote Share",
     xlab = "Relative concentration of hotel rooms per 100 people (rescaled)",
     main = "Table 2, Model 1b",
     cex.main = 2,
     cex.lab = 1.5)
lines(y = tab4mod1b_preds$fit[1:101] + 1.96*tab4mod1b_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab4mod1b_preds$fit[1:101] - 1.96*tab4mod1b_preds$se.fit[1:101],
      x = seq(0, 1, 0.01))
lines(y = tab4mod1b_preds$fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2,
      lwd = 2)
lines(y = tab4mod1b_preds$fit[102:202] + 1.96*tab4mod1b_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
lines(y = tab4mod1b_preds$fit[102:202] - 1.96*tab4mod1b_preds$se.fit[102:202],
      x = seq(0, 1, 0.01),
      lty = 2)
points(x = full_data_town$Num_hotelrooms_per100_rescaled1[which(full_data_town$Beach==1)],
       y = full_data_town$votechange[which(full_data_town$Beach==1)],
       pch = 19,
       cex = 2)
points(x = full_data_town$Num_hotelrooms_per100_rescaled1[which(full_data_town$Beach==0)],
       y = full_data_town$votechange[which(full_data_town$Beach==0)],
       pch = 17,
       cex = 2)

par(mar=c(0,0,0,0))
plot(0,0, type="n", axes=FALSE, xlab="", ylab="")
legend("center",legend=c("Beach Communities",
                         "Non-Beach Communities",
                         "Beach Community Marginal Effect",
                         "Non-Beach Community Marginal Effect"), 
       col="white", text.col = "white", cex=2.50,
       title = "Legend", title.col = "black",
       ncol = 1)
legend("center",legend=c("Beach Communities",
                         "Non-Beach Communities",
                         "Beach Community Fitted Values",
                         "Non-Beach Community Fitted Values"),
       pch=c(19,17,NA,NA), 
       lty = c(NA,NA,2,1),
       lwd = c(NA,NA,2,2),
       col=c("black","black","black","black"),
       title=" ", cex=2,
       ncol = 1, bty = "n")
dev.off()

# Table SI.1

tab3mod1raw_a <- lm(wilson1916 ~ beach*Num_hotels + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1raw_a)

tab3mod2raw_a <- lm(wilson1916 ~ beach*Num_hotels + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2raw_a)

tab3mod3raw_a <- lm(wilson1916 ~ attack*Num_hotels + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3raw_a)

tab3mod4raw_a <- lm(wilson1916 ~ coastal*Num_hotels + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4raw_a)

tab3mod5raw_a <- lm(wilson1916 ~ beach*Num_hotels + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5raw_a)

tab3mod6raw_a <- lm(wilson1916 ~ beach*Num_hotels + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6raw_a)

tab3mod1raw_b <- lm(wilson1916 ~ beach*Num_hotelrooms + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1raw_b)

tab3mod2raw_b <- lm(wilson1916 ~ beach*Num_hotelrooms + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2raw_b)

tab3mod3raw_b <- lm(wilson1916 ~ attack*Num_hotelrooms + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3raw_b)

tab3mod4raw_b <- lm(wilson1916 ~ coastal*Num_hotelrooms + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4raw_b)

tab3mod5raw_b <- lm(wilson1916 ~ beach*Num_hotelrooms + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5raw_b)

tab3mod6raw_b <- lm(wilson1916 ~ beach*Num_hotelrooms + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6raw_b)

wordreg(l = list(tab3mod1raw_a, tab3mod1raw_b, tab3mod2raw_a, tab3mod2raw_b, tab3mod3raw_a, tab3mod3raw_b, 
                 tab3mod4raw_a, tab3mod4raw_b, tab3mod5raw_a, tab3mod5raw_b, tab3mod6raw_a, tab3mod6raw_b),
        file = "TableSI1.doc",
        custom.coef.map = list("beach" = "Beach County",
                               "attack" = "Attack County",
                               "coastal" = "Coastal County",
                               "Num_hotels" = "Num. Hotels",
                               "beach:Num_hotels" = "Beach:Num. Hotels",
                               "attack:Num_hotels" = "Attack:Num. Hotels",
                               "coastal:Num_hotels" = "Coastal:Num. Hotels",
                               "Num_hotelrooms" = "Num. Hotel Rooms",
                               "beach:Num_hotelrooms" = "Beach:Num. Hotel Rooms",
                               "attack:Num_hotelrooms" = "Attack:Num. Hotel Rooms",
                               "coastal:Num_hotelrooms" = "Coastal:Num. Hotel Rooms",
                               "machine_ab" = "Machine (Achen and Bartels)",
                               "machine_mayhew" = "Machine (Mayhew)",
                               "wilson1912" = "Wilson 1912 Vote Share",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Number of Hotel Rooms) and Exposure to Shark Attacks (County-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)

# Table SI.2

tab4mod1raw_a <- lm(votechange ~ Beach*Num_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1raw_a)

tab4mod2raw_a <- lm(votechange ~ Beach*Num_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2raw_a)

tab4mod3raw_a <- lm(votechange ~ Beach*Num_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3raw_a)

tab4mod1raw_b <- lm(votechange ~ Beach*Num_hotelrooms, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1raw_a)

tab4mod2raw_b <- lm(votechange ~ Beach*Num_hotelrooms, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2raw_a)

tab4mod3raw_b <- lm(votechange ~ Beach*Num_hotelrooms, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3raw_b)

tab4mod4raw_a <- lm(votechange ~ Beach*Num_hotels, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4raw_a)

tab4mod5raw_a <- lm(votechange ~ Beach*Num_hotels, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5raw_a)

tab4mod6raw_a <- lm(votechange ~ Beach*Num_hotels + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6raw_a)

tab4mod4raw_b <- lm(votechange ~ Beach*Num_hotelrooms, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4raw_b)

tab4mod5raw_b <- lm(votechange ~ Beach*Num_hotelrooms, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5raw_b)

tab4mod6raw_b <- lm(votechange ~ Beach*Num_hotelrooms + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6raw_b)

wordreg(l = list(tab4mod1raw_a, tab4mod1raw_b, tab4mod2raw_a, tab4mod2raw_b, tab4mod3raw_a, tab4mod3raw_b, 
                 tab4mod4raw_a, tab4mod4raw_b, tab4mod5raw_a, tab4mod5raw_b, tab4mod6raw_a, tab4mod6raw_b),
        file = "TableSI2.doc",
        custom.coef.map = list("Beach" = "Beach Town",
                               "Num_hotels" = "Num. Hotels",
                               "Beach:Num_hotels" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms" = "Num. Hotels",
                               "Beach:Num_hotelrooms" = "Beach Town:Num. Hotel Rooms",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Number of Hotel Rooms) and Exposure to Shark Attacks (Town-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)

# Table SI.3

# reading in hotel counts that include information on hotels/rooms in neighboring
# towns
hotels_spillover <- read_excel("spillover_data.xlsx")

full_data_town <- merge(full_data_town, hotels_spillover, by = c("County", "City"), all = TRUE)

colnames(full_data_town)[33:35] <- c("neighboringtown_county", "hotels_neighboring", "hotelrooms_neighboring")

full_data_town$Num_hotels_spillover <- full_data_town$Num_hotels + full_data_town$hotels_neighboring
full_data_town$Num_hotelrooms_spillover <- full_data_town$Num_hotelrooms + full_data_town$hotelrooms_neighboring

# creating hotels/hotel rooms per 1k people in counties and 100 people in towns
full_data_town$Num_hotels_spillover_per100_rescaled <- full_data_town$Num_hotels_spillover / (full_data_town$total1916 / 100)
full_data_town$Num_hotelrooms_spillover_per100_rescaled <- full_data_town$Num_hotelrooms_spillover / (full_data_town$total1916 / 100)

# because the town-level models each use very different subsets, we need to rescale for each subset
full_data_town$Num_hotels_spillover_per100_rescaled1[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] <- (full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")])) / (max(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]) - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]))
full_data_town$Num_hotelrooms_spillover_per100_rescaled1[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] <- (full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")] - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")])) / (max(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]) - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park")]))

full_data_town$Num_hotels_spillover_per100_rescaled2[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] <- (full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))])) / (max(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]) - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]))
full_data_town$Num_hotelrooms_spillover_per100_rescaled2[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] <- (full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))] - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))])) / (max(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]) - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1))]))

full_data_town$Num_hotels_spillover_per100_rescaled3[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] <- (full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)])) / (max(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]) - min(full_data_town$Num_hotels_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]))
full_data_town$Num_hotelrooms_spillover_per100_rescaled3[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] <- (full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)] - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)])) / (max(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]) - min(full_data_town$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0)]))

# repeating for the LBBH dataframe
# first need to consolidate Long Beach and Beach Haven in the spillover data
hotels_spillover <- rbind(hotels_spillover, 
                          data.frame("County" = "Ocean",
                                     "City" = "LBBH",
                                     "Parcels" = c("48, 49, 50, 51, 52, 53, 55, 56, 60"),
                                     "Neighboring Town (County)" = c("Harvey Cedars, Barnegat City, Surf City"),
                                     "Hotels Spillover" = 12,
                                     "Hotel Rooms Spillover" = 735,
                                     check.names = FALSE))
hotels_spillover <- hotels_spillover[which(hotels_spillover$City!="Beach Haven" &
                                             hotels_spillover$City!="Long Beach"),]

full_data_town_LBBH <- merge(full_data_town_LBBH, hotels_spillover, by = c("County", "City"))

colnames(full_data_town_LBBH)[39:41] <- c("neighboringtown_county", "hotels_neighboring", "hotelrooms_neighboring")

full_data_town_LBBH$Num_hotels_spillover <- full_data_town_LBBH$Num_hotels + full_data_town_LBBH$hotels_neighboring
full_data_town_LBBH$Num_hotelrooms_spillover <- full_data_town_LBBH$Num_hotelrooms + full_data_town_LBBH$hotelrooms_neighboring

full_data_town_LBBH$Num_hotels_spillover_per100_rescaled <- full_data_town_LBBH$Num_hotels_spillover / (full_data_town_LBBH$total1916 / 100)
full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled <- full_data_town_LBBH$Num_hotelrooms_spillover / (full_data_town_LBBH$total1916 / 100)

full_data_town_LBBH$Num_hotels_spillover_per100_rescaled4[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled4[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

full_data_town_LBBH$Num_hotels_spillover_per100_rescaled5[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled5[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

full_data_town_LBBH$Num_hotels_spillover_per100_rescaled6[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotels_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))
full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled6[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] <- (full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)] - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)])) / (max(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]) - min(full_data_town_LBBH$Num_hotelrooms_spillover_per100_rescaled[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0)]))

tab4mod1spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1spillover_a)

tab4mod2spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2spillover_a)

tab4mod3spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3spillover_a)

tab4mod1spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1spillover_b)

tab4mod2spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2spillover_b)

tab4mod3spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3spillover_b)

tab4mod4spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4spillover_a)

tab4mod5spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5spillover_a)

tab4mod6spillover_a <- lm(votechange ~ Beach*Num_hotels_spillover_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6spillover_a)

tab4mod4spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4spillover_b)

tab4mod5spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5spillover_b)

tab4mod6spillover_b <- lm(votechange ~ Beach*Num_hotelrooms_spillover_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6spillover_b)

wordreg(l = list(tab4mod1spillover_a, tab4mod1spillover_b, tab4mod2spillover_a, tab4mod2spillover_b, tab4mod3spillover_a, tab4mod3spillover_b, 
                 tab4mod4spillover_a, tab4mod4spillover_b, tab4mod5spillover_a, tab4mod5spillover_b, tab4mod6spillover_a, tab4mod6spillover_b),
        file = "TableSI3.doc",
        custom.coef.map = list("Beach" = "Beach",
                               "Num_hotels_spillover_per100_rescaled1" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled1" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled1" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled1" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_spillover_per100_rescaled2" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled2" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled2" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled2" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_spillover_per100_rescaled3" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled3" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled3" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled3" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_spillover_per100_rescaled4" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled4" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled4" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled4" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_spillover_per100_rescaled5" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled5" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled5" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled5" = "Beach Town:Num. Hotel Rooms",
                               "Num_hotels_spillover_per100_rescaled6" = "Num. Hotels",
                               "Beach:Num_hotels_spillover_per100_rescaled6" = "Beach Town:Num. Hotels",
                               "Num_hotelrooms_spillover_per100_rescaled6" = "Num. Hotel Rooms",
                               "Beach:Num_hotelrooms_spillover_per100_rescaled6" = "Beach Town:Num. Hotel Rooms",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Number of Hotel Rooms) and Exposure to Shark Attacks (Town-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)

# NOTE: in text, we note we further interrogated models 5a and 5b using the interflex
# package to assess whether they meet the liner interaction
# effect assumption; the replication code for these further analyses are included
# below in the replication code corresponding with Supplemental Information 
# Section SI.D.5, where we do this for all models

# Table SI.4

# NOTE 1: the Cook's distance value cutoffs for each model are specified in
# the first column of the table

# NOTE 2: in this section of the Supplemental Information (SI.D.4), we note that
# whenever we detected influential observations, we refit the model using
# robust regression; this refitting accompanies the examination of
# influential observations for each model in turn

# NOTE 3: we also note in-text that we further interrogated models 2a and 4a from
# Table 2 using the interflex package to assess whether they meet the liner interaction
# effect assumption because they yielded significant interaction terms when using robust
# regression; the replication code for these further analyses are included
# below in the replication code corresponding with Supplemental Information 
# Section SI.D.5, where we do this for all models

# Table 1, Model 1a
tab3mod1a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1a)
# any influential observations?
sort(cooks.distance(tab3mod1a), decreasing = TRUE) 

# Table 1, Model 1b
tab3mod1b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1b)
# any influential observations?
sort(cooks.distance(tab3mod1b), decreasing = TRUE) 
# yes, use robust regression
tab3mod1br <- rlm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod1br)

# Table 1, Model 2a
tab3mod2a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2a)
# any influential observations?
sort(cooks.distance(tab3mod2a), decreasing = TRUE)
# yes, use robust regression
tab3mod2ar <- rlm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2ar)

# Table 1, Model 2b
tab3mod2b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2b)
# any influential observations?
sort(cooks.distance(tab3mod2b), decreasing = TRUE) 
# yes, use robust regression
tab3mod2br <- rlm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_wessex + machine_ab + wilson1912, data = full_data_county)
summary(tab3mod2br)

# Table 1, Model 3a
tab3mod3a <- lm(wilson1916 ~ attack*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3a)
# any influential observations?
sort(cooks.distance(tab3mod3a), decreasing = TRUE) 
# yes, use robust regression
tab3mod3ar <- rlm(wilson1916 ~ attack*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3ar)

# Table 1, Model 3b
tab3mod3b <- lm(wilson1916 ~ attack*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod3b)
# any influential observations?
sort(cooks.distance(tab3mod3b), decreasing = TRUE) 

# Table 1, Model 4a
tab3mod4a <- lm(wilson1916 ~ coastal*Num_hotels_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4a)
# any influential observations?
sort(cooks.distance(tab3mod4a), decreasing = TRUE) 

# Table 1, Model 4b

tab3mod4b <- lm(wilson1916 ~ coastal*Num_hotelrooms_per1000_rescaled_noessex + machine_ab + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod4b)
# any influential observations?
sort(cooks.distance(tab3mod4b), decreasing = TRUE) 

# Table 1, Model 5a
tab3mod5a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5a)
# any influential observations?
sort(cooks.distance(tab3mod5a), decreasing = TRUE) 

# Table 1, Model 5b
tab3mod5b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5b)
# any influential observations?
sort(cooks.distance(tab3mod5b), decreasing = TRUE) 
# yes, use robust regression
tab3mod5br <- rlm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod5br)

# Table 1, Model 6a
tab3mod6a <- lm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6a)
# any influential observations?
sort(cooks.distance(tab3mod6a), decreasing = TRUE) 
# yes, use robust regression
tab3mod6ar <- rlm(wilson1916 ~ beach*Num_hotels_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6ar)

# Table 1, Model 6b
tab3mod6b <- lm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6b)
# any influential observations?
sort(cooks.distance(tab3mod6b), decreasing = TRUE) 
# yes, use robust regression
tab3mod6br <- rlm(wilson1916 ~ beach*Num_hotelrooms_per1000_rescaled_noessex + machine_mayhew + wilson1912, data = full_data_county[which(full_data_county$County!="Essex"),])
summary(tab3mod6br)

# Table 2, Model 1a
tab4mod1a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1a)
# any influential observations?
sort(cooks.distance(tab4mod1a), decreasing = TRUE) 
# we can also use robust regression
tab4mod1ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916), maxit = 100)
summary(tab4mod1ar)

# Table 2, Model 1b
tab4mod1b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1b)
# any influential observations?
sort(cooks.distance(tab4mod1b), decreasing = TRUE) 
# reestimate with robust regression
tab4mod1br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled1, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1br)

# Table 2, Model 2a
tab4mod2a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2a)
# any influential observations?
sort(cooks.distance(tab4mod2a), decreasing = TRUE) 
# we can also use robust regression
tab4mod2ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2ar)

# Table 2, Model 2b
tab4mod2b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2b)
# any influential observations?
sort(cooks.distance(tab4mod2b), decreasing = TRUE) 
# we can also use robust regression
tab4mod2br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled2, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2br)

# Table 2, Model 3a
tab4mod3a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3a)
# any influential observations?
sort(cooks.distance(tab4mod3a), decreasing = TRUE) 
# we can also use robust regression
tab4mod3ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3ar)

# Table 2, Model 3b
tab4mod3b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3b)
sort(cooks.distance(tab4mod3b), decreasing = TRUE) 
# we can also use robust regression
tab4mod3br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled3, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3br)

# Table 2, Model 4a
tab4mod4a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4a)
# any influential observations?
sort(cooks.distance(tab4mod4a), decreasing = TRUE) 
# we can also use robust regression
tab4mod4ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4ar)

# Table 2, Model 4b
tab4mod4b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4b)
# any influential observations?
sort(cooks.distance(tab4mod4b), decreasing = TRUE) 
# we can also use robust regression
tab4mod4br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled4, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4br)

# Table 2, Model 5a
tab4mod5a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5a)
# any influential observations?
sort(cooks.distance(tab4mod5a), decreasing = TRUE) 
# we can also use robust regression
tab4mod5ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5ar)

# Table 2, Model 5b
tab4mod5b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5b)
# any influential observations?
sort(cooks.distance(tab4mod5b), decreasing = TRUE) 
# we can also use robust regression
tab4mod5br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled5, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5br)

# Table 2, Model 6a
tab4mod6a <- lm(votechange ~ Beach*Num_hotels_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6a)
# any influential observations?
sort(cooks.distance(tab4mod6a), decreasing = TRUE) 
# we can also use robust regression
tab4mod6ar <- rlm(votechange ~ Beach*Num_hotels_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6ar)

# Table 2, Model 6b
tab4mod6b <- lm(votechange ~ Beach*Num_hotelrooms_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6b)
# any influential observations?
sort(cooks.distance(tab4mod6b), decreasing = TRUE) 
# we can also use robust regression
tab4mod6br <- rlm(votechange ~ Beach*Num_hotelrooms_per100_rescaled6 + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6br)

# SI Section D.5--Interaction Effect Assumptions & Alternative Estimation Methods

# NOTE 1: in this section, we note that we used interflex to assess the linear
# interaction effect assumption for all models and then used a kernel estimator
# for models where this assumption was violated, but we only provided these
# supplemental analyses for a subset of models in the body of the Supplemental Information;
# we provide these analyses for ALL models here

# NOTE 2: all the town level analyses exclude weights, as the interflex package
# would not work with the 1916 vote totals as weights included

# NOTE 3: for some models, interflex had difficulty calculating optimal
# bandwidths; in these cases, the authors manually set a bandwidth

# Table 1, Model 1a

interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 1b

interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 2a

interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_wessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county, 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_wessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county, 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 2b

interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_wessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county, 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_wessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county, 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 3a

interflex(Y = "wilson1916", D = "attack", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "attack", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 3b

interflex(Y = "wilson1916", D = "attack", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "attack", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 4a

interflex(Y = "wilson1916", D = "coastal", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "coastal", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 4b

interflex(Y = "wilson1916", D = "coastal", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "coastal", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_ab", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 5a

interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 5b

interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 6a

interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_mayhew", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw", 
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotels_per1000_rescaled_noessex", 
          Z = c("machine_mayhew", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 1, Model 6b

interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_mayhew", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "wilson1916", D = "beach", X = "Num_hotelrooms_per1000_rescaled_noessex", 
          Z = c("machine_mayhew", "wilson1912"),
          data = full_data_county[which(full_data_county$County!="Essex"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 1a

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled1",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled1", weights = "total1916",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8, 
          bw = 1.5,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 1b

interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled1",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], 
          estimator = "raw", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled1",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 1,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 2a

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled2",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], 
          estimator = "raw", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled2",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 2b

interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled2",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], 
          estimator = "raw", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled2",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 1,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 3a

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled3",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], 
          estimator = "raw", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled3",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 3b
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled3",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], 
          estimator = "raw", nboots = 200, parallel = TRUE, cores = 8,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled3",
          data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 4a

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled4",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled4", weights = "total1916",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 4b
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled4",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled4", weights = "total1916",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.5,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 5a

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled5",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled5", weights = "total1916",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 5b

interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled5",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw",
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled5", weights = "total1916",
          data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8,
          bw = 0.25,
          main = "Marginal Effects", 
          Xlabel = "Number of Hotels", Ylabel = "1916 Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)

# evaluation points are fixed for the final two kernel estimators in order to 
# facilitate the creation of SI Tables SI.5 and SI.6

# Table 2, Model 6a (raw plot used as left panel of Figure SI.4, kernel plot
# used as left panel of Figure SI.5, kernel plot with trimmed data used as 
# left panel of Figure SI.6)

# interflex requires County to be a factor rather than a character
full_data_town_LBBH$County <- as.factor(full_data_town_LBBH$County)

interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled6", Z = "County",
          data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw", 
          main = "", 
          Xlabel = "Number of Hotels Per Capita (Rescaled)", Ylabel = "Change in Wilson Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
tab4mod6a_if <- interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled6", Z = "County",
                            weights = "total1916",
                            data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
                            estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8, neval = 51,
                            main = "", 
                            Xlabel = "Number of Hotels Per Capita (Rescaled)", Ylabel = "Vote Share",
                            ylim = c(-0.75, 0.25), na.rm = TRUE, theme.bw = TRUE)
# repeating analysis using trimmed data (trimmed to exclude observations with
# values greater than 70th percentile)
tab4mod6a_if_trimmed <- interflex(Y = "votechange", D = "Beach", X = "Num_hotels_per100_rescaled6", Z = "County",
                                    weights = "total1916",
                                    data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0 & full_data_town_LBBH$Num_hotels_per100_rescaled6 <= 0.3049451),], 
                                    estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8, X.eval = seq(0,0.31,0.01),
                                    main = "", 
                                    Xlabel = "Number of Hotels Per Capita (Rescaled)", Ylabel = "Vote Share",
                                    ylim = c(-0.30, 0.30), na.rm = TRUE, theme.bw = TRUE)

# Table 2, Model 6b (raw plot used as right panel of Figure SI.4, kernel plot
# used as right panel of Figure SI.5, kernel plot with trimmed data used as 
# right panel of Figure SI.6)

interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled6", Z = "County",
          data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
          estimator = "raw", 
          main = "", 
          Xlabel = "Number of Hotel Rooms Per Capita (Rescaled)", Ylabel = "Vote Share",
          ylim = c(-1, 1), na.rm = TRUE, theme.bw = TRUE)
tab4mod6b_if <- interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled6", Z = "County",
                            weights = "total1916",
                            data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], 
                            estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8, neval = 51,
                            main = "", 
                            Xlabel = "Number of Hotel Rooms Per Capita (Rescaled)", Ylabel = "Vote Share",
                            ylim = c(-0.75, 0.25), na.rm = TRUE, theme.bw = TRUE)
# repeating analysis using trimmed data (trimmed to exclude observations with
# values greater than 70th percentile)
tab4mod6b_if_trimmed <- interflex(Y = "votechange", D = "Beach", X = "Num_hotelrooms_per100_rescaled6", Z = "County",
                                    weights = "total1916",
                                    data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0 & full_data_town_LBBH$Num_hotelrooms_per100_rescaled6 <= 0.15525),], 
                                    estimator = "kernel", nboots = 200, parallel = TRUE, cores = 8, X.eval = seq(0,0.16,0.01),
                                    main = "", 
                                    Xlabel = "Number of Hotel Rooms Per Capita (Rescaled)", Ylabel = "Vote Share",
                                    ylim = c(-0.30, 0.30), na.rm = TRUE, theme.bw = TRUE)

# Table SI.5

tab4mod6a_if$est.kernel
tab4mod6b_if$est.kernel

# Table SI.6

tab4mod6a_if_trimmed$est.kernel
tab4mod6b_if_trimmed$est.kernel

# Table SI.7

tab4mod1_binary <- lm(votechange ~ Beach*any_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$City!="Sea Side Park"),], weights = as.numeric(total1916))
summary(tab4mod1_binary)

tab4mod2_binary <- lm(votechange ~ Beach*any_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1)),], weights = as.numeric(total1916))
summary(tab4mod2_binary)

tab4mod3_binary <- lm(votechange ~ Beach*any_hotels, data = full_data_town[which(full_data_town$County=="Ocean" & full_data_town$abspopchange<0.25 & (as.numeric(full_data_town$Beach) + as.numeric(full_data_town$NearBeach) + as.numeric(full_data_town$Both)==1) & full_data_town$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod3_binary)

tab4mod4_binary <- lm(votechange ~ Beach*any_hotels, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & full_data_town_LBBH$abspopchange<0.25 & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod4_binary)

tab4mod5_binary <- lm(votechange ~ Beach*any_hotels, data = full_data_town_LBBH[which(full_data_town_LBBH$County=="Ocean" & (as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod5_binary)

tab4mod6_binary <- lm(votechange ~ Beach*any_hotels + factor(County), data = full_data_town_LBBH[which((as.numeric(full_data_town_LBBH$Beach) + as.numeric(full_data_town_LBBH$NearBeach) + as.numeric(full_data_town_LBBH$Both==1)) & full_data_town_LBBH$BoundaryChange_FH==0),], weights = as.numeric(total1916))
summary(tab4mod6_binary)

wordreg(l = list(tab4mod1_binary, tab4mod2_binary, tab4mod3_binary, tab4mod4_binary,
                 tab4mod5_binary, tab4mod6_binary),
        file = "TableSI7.doc",
        custom.coef.map = list("Beach" = "Beach Town",
                               "any_hotels" = "Any Hotels",
                               "Beach:any_hotels" = "Beach Town:Any Hotels",
                               "(Intercept)" = "Intercept"),
        caption = "Interactive Effect of Hotel Industry (Any Hotels) and Exposure to Shark Attacks (Town-Level)",
        caption.above = TRUE,
        stars = 0.05,
        include.rsquared = TRUE,
        include.adjrs = FALSE)

# Table SI.8

# non-beach, no hotels vs. non-beach, hotels
confint(glht(tab4mod6_binary, linfct = "any_hotels = 0"))

# non-beach, no hotels vs. beach, no hotels
confint(glht(tab4mod6_binary, linfct = "Beach = 0"))

# non-beach, no hotels vs. beach, hotels
confint(glht(tab4mod6_binary, linfct = "any_hotels + Beach + Beach:any_hotels = 0"))

# non-beach, hotels vs. beach, no hotels
confint(glht(tab4mod6_binary, linfct = "any_hotels - Beach = 0"))

# non-beach, hotels vs. beach, hotels
confint(glht(tab4mod6_binary, linfct = "Beach + Beach:any_hotels = 0"))

# beach, no hotels vs. beach, hotels
confint(glht(tab4mod6_binary, linfct = "any_hotels + Beach:any_hotels = 0"))

