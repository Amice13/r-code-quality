#LIBRARIES
library(rstudioapi)
library(tidyverse)
library(twang)
library(survey)
library(estimatr)
library(did)
library(foreach)
library(doParallel)
library(skimr)
library(cowplot)

#SET WORKING DIRECTORY TO SOURCE FILE LOCATION
fileloc <- getSourceEditorContext()$path %>%
  str_remove("VietnamPFESReplication.R")
setwd(fileloc)

#READ IN DATA
hexes <- read.csv("VietnamPFESData.csv", stringsAsFactors = FALSE)

#SET PLOTTING DEFAULTS
theme_opts <- theme_bw() + theme(axis.title.x=element_text(size=18, colour="black", face="bold"),
                                 axis.text.x=element_text(size=12, colour="black", face="bold"),
                                 axis.title.y=element_text(size=18, colour="black", face="bold"),
                                 axis.text.y=element_text(size=12, colour="black", face="bold"),
                                 plot.title = element_text(size=18, colour="black", face="bold"),
                                 strip.text=element_text(size=12, colour="black", face="bold"),
                                 legend.title=element_text(size=18, colour="black", face="bold"),
                                 legend.text=element_text(size=12, colour="black", face="bold"),
                                 legend.position = "none")

#FOREST LOSS PATTERNS - FIGURE 1
hexes$PlotStatus <- ifelse(hexes$EverPFES, "Pre-PFES",
                           "Never PFES")
hexes$PlotStatus[hexes$PFES==1] <- "PFES Active"

g <- ggplot(data=hexes, aes(x=Year, y=Deforested*100))+
  geom_smooth()+
  scale_x_continuous("", breaks = c(2006, 2009, 2012, 2015, 2018))+
  scale_y_continuous("Annual Deforestation Rate (%)")+
  facet_wrap(~PlotStatus, ncol=1)+
  theme_opts
png("Figure1.png", width=8, height=6, units="in", res=300)
g
dev.off()

#CREATE MATCHING WEIGHTS FOR PFES AREAS
hexes <- hexes[complete.cases(hexes[,c("ELEVM", "TRI",
                                                      "Crop", "RoadDist",
                                                      "PopDenEst", "PovRate",
                                                      "ForCover", "PFES",
                                                      "Protected", "EverPFES")]),]

ps.KM10 <- ps(PFES ~ Protected + ELEVM + TRI + Crop +
                RoadDist + PopDenEst + PovRate + ForCover,
              data = as.data.frame(hexes),
              estimand = "ATE",
              verbose = TRUE,
              stop.method = "ks.mean",
              n.trees = 15000)

tab <- bal.table(ps.KM10)
write.csv(tab, "BalanceTable.csv", row.names=FALSE)

hexes$weights <- get.weights(ps.KM10, stop.method = "ks.mean")
hexesdesign <- svydesign(ids=~REGID, weights=~weights, data=hexes)

#ESTIMATE MATCHED FIXED EFFECTS MODEL - TABLE A2
lmmat <- svyglm(Deforested ~ 1 + PFES + PFESL1 + PFESL2 + PFESL3 +
                  PFESL4 + Protected + I(PFES*Protected) + BorderPFES + ELEVM + TRI + Crop +
                  RoadDist + PopDenEst + PovRate + ForCover +
                  factor(Year) + factor(KM10ID), design=hexesdesign)
summary(lmmat)
write.csv(tidy(lmmat), "Table A2.csv", row.names = FALSE)

#EVALUATE LAGGED POLICY IMPLEMENTATION - FIGURE 2 TOP
matlagvcov <- vcov(lmmat)[2:6,2:6]
lagp <- as_tibble(matrix(NA, nrow=5, ncol=2))
names(lagp) <- c("Estimate", "SE")
lags <- c(1,0,0,0,0)
lagcfs <- coef(lmmat)[2:6]
for(i in 1:nrow(lagp)){
  lags[i] <- 1
  lagp[i,1] <- sum(lagcfs[1:i])
  lagp[i,2] <- sqrt(t(lags) %*% matlagvcov %*% lags) 
}

lagp <- lagp %>%
  mutate(Lower99 = Estimate - (SE*2.58),
         Upper99 = Estimate + (SE*2.58),
         Lower95 = Estimate - (SE*1.96),
         Upper95 = Estimate + (SE*1.96),
         Duration = factor(c("Currently Active", "Two Years", "Three Years", "Four Years", "Five Years"),
                           levels=c("Currently Active", "Two Years", "Three Years", "Four Years", "Five Years"),
                           ordered=TRUE),
         Group = 1)

g <- ggplot(data=lagp, aes(x = Duration, y = Estimate*100,
                                 group=Group))+
  geom_hline(yintercept = 0, size = 1, linetype=1, color="black")+
  geom_hline(yintercept = -1.052, size=1.5, linetype=1, color="white")+
  geom_hline(yintercept = -1.052, size=1, linetype=2, color="black")+
  geom_hline(yintercept = -0.46, size=1.5, linetype=1, color="white")+
  geom_hline(yintercept = -0.46, size=1, linetype=2, color="black")+
  geom_ribbon(aes(x = Duration, ymin = Lower99*100,
                  ymax = Upper99*100), fill="lightgray",
              alpha=0.5)+
  geom_ribbon(aes(x = Duration, ymin = Lower95*100,
                  ymax = Upper95*100), fill="gray",
              alpha=0.5)+
  geom_line(color="black", size=1)+
  scale_x_discrete("Years of PFES Activity") +
  scale_y_continuous("Estimated Change in Mean\nAnnual Deforestation (%)")+
  theme_opts
png("Figure2Top.png", width=12, height=6, units="in", res=300)
g
dev.off()

#TWO-WAY FE MODEL COEFFCIEINTS - FIGURE 2 BOTTOM
coefs <- tidy(lmmat)[2:6,] %>%
  mutate(conf.low = estimate - (std.error*2.58),
   conf.high = estimate + (std.error*2.58),
   term = lagp$Duration,
   sig = (conf.low*conf.high)>0)

g <- ggplot(data = coefs, aes(x = term, y = estimate*100))+
  geom_hline(yintercept = 0, size = 1, linetype=1, color="black")+
  geom_segment(aes(y = conf.low*100, yend = conf.high*100, x = term, xend = term),
               size=2)+
  geom_point(size=4)+
  geom_point(data=coefs[coefs$sig,], size=2, color="white")+
  scale_x_discrete("PFES Activity Lag")+
  scale_y_continuous("Treatment and Lagged\nTreatment Coefficients (%)")+
  theme_opts
png("Figure2Bottom.png", width=12, height=4, units="in", res=300)
g
dev.off()

#PROTECTED AREA COMPARISON - TABLE A3
ps.KM10prot <- ps(Protected ~ PFES + ELEVM + TRI + Crop +
                    RoadDist + PopDenEst + PovRate + ForCover,
                  data = as.data.frame(hexes),
                  estimand = "ATE",
                  verbose = TRUE,
                  stop.method = "ks.mean",
                  n.trees = 15000)

tab <- bal.table(ps.KM10prot)
write.csv(tab, "BalanceTableProt.csv", row.names=FALSE)

hexes$protweights <- get.weights(ps.KM10prot, stop.method = "ks.mean")
hexesdesignprot <- svydesign(ids=~REGID, weights=~protweights, data=hexes)

lmmatfeprot <- svyglm(Deforested ~ 1 + Protected + I(PFES*Protected) + PFES + 
                        BorderPFES + ELEVM + TRI + Crop +
                        RoadDist + PopDenEst + PovRate + ForCover +
                        factor(Year) + factor(KM10ID), design=hexesdesignprot)
summary(lmmatfeprot)
write.csv(tidy(lmmatfeprot), "Table A3.csv", row.names=FALSE)

#LEAKAGE ANALYSIS
ps.KM10bord <- ps(BorderPFES ~ Protected + ELEVM + TRI + Crop +
                    RoadDist + PopDenEst + PovRate + ForCover,
                  data = as.data.frame(hexes),
                  estimand = "ATE",
                  verbose = TRUE,
                  stop.method = "ks.mean",
                  n.trees = 15000)

tab <- bal.table(ps.KM10bord)
write.csv(tab, "BalanceTableBord.csv", row.names=FALSE)

hexes$bordweights <- get.weights(ps.KM10bord, stop.method = "ks.mean")
hexesdesignbord <- svydesign(ids=~REGID, weights=~bordweights, data=hexes)

lmmatfebord <- svyglm(Deforested ~ 1 + Protected + I(PFES*Protected) + PFES + 
                        BorderPFES + ELEVM + TRI + Crop +
                        RoadDist + PopDenEst + PovRate + ForCover +
                        factor(Year) + factor(KM10ID), design=hexesdesignbord)
summary(lmmatfebord)
write.csv(tidy(lmmatfebord), "BorderPFESFE.csv", row.names=FALSE)
cat(capture.output(summary(lmmatfebord)),
    file="BorderPFESOutputs.txt")

#DIFFERENCE-IN-DIFFERENCES ESTIMATION - FIGURE 3
hexes$REGID <- as.numeric(hexes$REGID)
temp <- lm_robust(Deforested ~ 1 + Protected +
                    ELEVM + TRI + Crop +
                    RoadDist + PopDenEst + PovRate + ForCover,
                  fixed_effects = factor(KM10ID),
                  clusters = REGID,
                  se_type="stata",
                  alpha = 0.01,
                  data=hexes)
write.csv(tidy(temp), "Table A4.csv", row.names = FALSE)
hexes$Resids <- hexes$Deforested-temp$fitted.values

hexes$PFESStart <- hexes$PFESYR*hexes$EverPFES

cdp <- att_gt(yname = "Resids",
              tname = "Year",
              idname = "REGID",
              gname = "PFESStart",
              panel = TRUE,
              allow_unbalanced_panel = TRUE,
              xformla=~1, 
              data=hexes,
              control_group = "notyettreated",
              weightsname = "weights",
              bstrap = TRUE,
              clustervars = "REGID",
              cores = 6,
              alp = 0.01)

agg.es <- aggte(cdp, type = "dynamic")
summary(agg.es)

g <- ggdid(agg.es) +
  ggtitle("") +
  scale_x_continuous("Year Relative to PFES Activity Onset") +
  scale_y_continuous("Average Annual Deforestation\nRate Treatment Effect (%)",
                     labels = scales::percent)+
  geom_vline(xintercept=0, linetype=2)+
  scale_colour_manual(values = c("gray", "black"))+
  theme_opts

png("Figure3.png", width=7, height=5, units="in", res=300)
g
dev.off()

#BALANCE PLOTS - ONLINE APPENDIX
balpfes <- read.csv("BalanceTable.csv", stringsAsFactors = FALSE)
balpfes <- balpfes[,names(balpfes)[str_detect(names(balpfes), fixed("mn"))|
                                     str_detect(names(balpfes), fixed("sd"))]] %>%
  mutate(term = c("Protected Area", "Elevation (m)",
                  "Terrain Ruggedness Index", "Proportion Cropland within Two km",
                  "Distance from Major Road (m)", "People per Square km",
                  "Poverty Rate", "Total Sampled Pixels")) %>%
  pivot_longer(cols = -term) %>%
  mutate(version = ifelse(str_detect(name, fixed("unw")),
                          "Unweighted", "Weighted"),
         group = ifelse(str_detect(name, fixed("tx")),
                        "PFES", "Non-PFES"),
         measure = ifelse(str_detect(name, fixed("sd")),
                          "SD", "MN"),
         label = factor(paste0(group, "\n", version),
                        levels = c("PFES\nUnweighted",
                                   "Non-PFES\nUnweighted",
                                   "PFES\nWeighted",
                                   "Non-PFES\nWeighted"),
                        ordered=TRUE)) %>%
  select(-name) %>%
  pivot_wider(names_from = measure,
              values_from = value) %>%
  ggplot(aes(x = label, y = MN, color=version)) +
    geom_segment(aes(xend = label, y = MN - (2*SD),
                     yend = MN + (2*SD)),
                 size=2)+
    geom_segment(aes(xend = label, y = MN - SD,
                     yend = MN + SD),
                 size=3)+
    geom_point(size=6)+
    scale_x_discrete("")+
    scale_y_continuous("")+
    scale_color_manual(values=c("gray", "black"))+
    facet_wrap(~term, ncol=2, scales="free_y")+
    theme_opts
 png("FigureA1.png", width=12, height=8, units="in", res=300)
 balpfes
 dev.off()
 
 balprot <- read.csv("BalanceTableProt.csv", stringsAsFactors = FALSE)
 balprot <- balprot[,names(balprot)[str_detect(names(balprot), fixed("mn"))|
                                      str_detect(names(balprot), fixed("sd"))]] %>%
    mutate(term = c("PFES", "Elevation (m)",
                   "Terrain Ruggedness Index", "Proportion Cropland within Two km",
                   "Distance from Major Road (m)", "People per Square km",
                   "Poverty Rate", "Total Sampled Pixels")) %>%
   pivot_longer(cols = -term) %>%
   mutate(version = ifelse(str_detect(name, fixed("unw")),
                           "Unweighted", "Weighted"),
          group = ifelse(str_detect(name, fixed("tx")),
                         "Protected", "Non-Protected"),
          measure = ifelse(str_detect(name, fixed("sd")),
                           "SD", "MN"),
          label = factor(paste0(group, "\n", version),
                         levels = c("Protected\nUnweighted",
                                    "Non-Protected\nUnweighted",
                                    "Protected\nWeighted",
                                    "Non-Protected\nWeighted"),
                         ordered=TRUE)) %>%
   select(-name) %>%
   pivot_wider(names_from = measure,
               values_from = value) %>%
   ggplot(aes(x = label, y = MN, color=version)) +
   geom_segment(aes(xend = label, y = MN - (2*SD),
                    yend = MN + (2*SD)),
                size=2)+
   geom_segment(aes(xend = label, y = MN - SD,
                    yend = MN + SD),
                size=3)+
   geom_point(size=6)+
   scale_x_discrete("")+
   scale_y_continuous("")+
   scale_color_manual(values=c("gray", "black"))+
   facet_wrap(~term, ncol=2, scales="free_y")+
   theme_opts
 png("FigureA2.png", width=12, height=8, units="in", res=300)
 balprot
 dev.off()

 #SUMMARY STATISTICS - ONLINE APPENDIX TABLE A1
 labels <- cbind(c("ELEVM", "TRI", "RoadDist", "PopDenEst", "PovRate",
                   "Crop"),
                 c("Elevation (m)", "Terrain Ruggedness Index",
                   "Distance from Major Road (m)", "People per Square km",
                   "Poverty Rate", "Proportion Cropland within Two km"))
 summaries <- hexes[,c("KM10ID", "PFES", "PFESL1", "PFESL2", "PFESL3",
                       "PFESL4", "Deforested", "ELEVM",
                       "TRI", "RoadDist", "PopDenEst",
                       "PovRate", "Crop", "BorderPFES",
                       "ForCover", "Protected")] %>%
   pivot_longer(cols = -KM10ID) %>%
   group_by(name) %>%
   summarise(valueMN = mean(value, na.rm=TRUE),
             valueSD = sd(value, na.rm=TRUE),
             valueMin = min(value, na.rm=TRUE),
             valueMax = max(value, na.rm=TRUE)) %>%
   mutate(labels = labels[match(name, labels[,1]),2])
 write.csv(summaries, "Table A1.csv", row.names=FALSE)
 
#ALTERNATIVE DID SPECIFICATIONS - ONLINE APPENDIX
 cdp2 <- att_gt(yname = "Resids",
               tname = "Year",
               idname = "REGID",
               gname = "PFESStart",
               panel = TRUE,
               allow_unbalanced_panel = TRUE,
               xformla=~1, 
               data=hexes,
               control_group = "notyettreated",
               bstrap = TRUE,
               clustervars = "REGID",
               cores = 6,
               alp = 0.01)
 
 agg.es2 <- aggte(cdp2, type = "dynamic")
 summary(agg.es2)
 
 g.es2 <- ggdid(agg.es2) +
   ggtitle("") +
   scale_x_continuous("Year Relative to PFES Activity Onset") +
   scale_y_continuous("",
                      labels = scales::percent)+
   geom_vline(xintercept=0, linetype=2)+
   scale_colour_manual(values = c("gray", "black"))+
   ggtitle("Detrended Difference-in-Differences without Weights") +
   theme_opts
 
 cdp3 <- att_gt(yname = "Deforested",
                tname = "Year",
                idname = "REGID",
                gname = "PFESStart",
                panel = TRUE,
                allow_unbalanced_panel = TRUE,
                xformla=~1, 
                data=hexes,
                control_group = "notyettreated",
                bstrap = TRUE,
                clustervars = "REGID",
                cores = 6,
                alp = 0.01)
 
 agg.es3 <- aggte(cdp3, type = "dynamic")
 summary(agg.es3)
 
 g.es3 <- ggdid(agg.es3) +
   ggtitle("") +
   scale_x_continuous("") +
   scale_y_continuous("",
                      labels = scales::percent)+
   geom_vline(xintercept=0, linetype=2)+
   scale_colour_manual(values = c("gray", "black"))+
   ggtitle("Difference-in-Differences without Weights") +
   theme_opts
 
 cdp4 <- att_gt(yname = "Deforested",
                tname = "Year",
                idname = "REGID",
                gname = "PFESStart",
                panel = TRUE,
                allow_unbalanced_panel = TRUE,
                xformla=~1, 
                data=hexes,
                control_group = "notyettreated",
                bstrap = TRUE,
                clustervars = "REGID",
                weightsname = "weights",
                cores = 6,
                alp = 0.01)
 
 agg.es4 <- aggte(cdp4, type = "dynamic")
 summary(agg.es4)
 
 g.es4 <- ggdid(agg.es4) +
   ggtitle("") +
   scale_x_continuous("Year Relative to PFES Activity Onset") +
   scale_y_continuous("Average Annual Deforestation\nRate Treatment Effect (%)",
                      labels = scales::percent)+
   geom_vline(xintercept=0, linetype=2)+
   scale_colour_manual(values = c("gray", "black"))+
   ggtitle("Difference-in-Differences with Weights") +
   theme_opts
 
 g.es.comps <- plot_grid(g.es3, g.es4, g.es2, nrow = 3)
 
 png("FigureA3.png", width=12, height=8, units="in", res=300)
 g.es.comps
 dev.off()