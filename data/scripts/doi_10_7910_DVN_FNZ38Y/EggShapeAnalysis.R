### Egg shape analysis ####

# Load dataset 
d<-read.csv("EggMeasurementsData.csv")

# packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(visreg)
library(broom)

# Correct data format
d$ClutchID<-as.factor(d$ClutchID)
d$EggPosition <- factor(d$EggPosition, levels = c("Center","Periphery"))

# Calculate mean egg diameters and volumes per clutch and position
clutch_av <- d %>%
  group_by(ClutchID, EggPosition) %>%
  summarise(
    across(c(EggLongDiameter, EggShortDiameter, EggVolume), mean),
    ProthoraxWidth = first(ProthoraxWidth),
    .groups = "drop")

# Wide format for Figure 2
wide_clutch_av <- clutch_av %>%
  pivot_wider(names_from = EggPosition,
              values_from = c(EggLongDiameter, EggShortDiameter, EggVolume),
              names_sep = "_")
# Figure 2A
ggplot(wide_clutch_av, aes(x=EggLongDiameter_Periphery,y=EggLongDiameter_Center))+
  geom_point() +
  geom_abline(intercept=0,slope=1,linetype = 2)+
  coord_cartesian(xlim = c(1.02, 1.22),ylim = c(1.02, 1.22))

# Figure 2B
ggplot(wide_clutch_av, aes(x=EggShortDiameter_Periphery,y=EggShortDiameter_Center))+
  geom_point() +
  geom_abline(intercept=0,slope=1,linetype = 2)+
  coord_cartesian(xlim = c(0.64,0.74), ylim = c(0.64,0.74))

# Figure 2C
ggplot(wide_clutch_av, aes(x=EggVolume_Periphery,y=EggVolume_Center))+
  geom_point() +
  geom_abline(intercept=0,slope=1,linetype = 2)+
  coord_cartesian(xlim = c(0.22,0.34), ylim = c(0.22,0.34))

# Figure S2A
ggplot(d,aes(x=ClutchID,y=EggLongDiameter,colour=EggPosition))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5))

# Figure S2B
ggplot(d,aes(x=ClutchID,y=EggShortDiameter,colour=EggPosition))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5))

# Figure S2C
ggplot(d,aes(x=ClutchID,y=EggVolume,colour=EggPosition))+
  geom_boxplot()+
  geom_jitter(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.5))

# Figure 3
clutch_av_center <- subset(clutch_av, EggPosition == "Center")
clutch_av_center <- clutch_av_center %>%
  mutate(
    log_EggLongDiameter = log(EggLongDiameter),
    log_EggShortDiameter = log(EggShortDiameter),
    log_ProthoraxWidth  = log(ProthoraxWidth)
  )

clutch_av_center$resid_ShortDiameter <- residuals(lm(log_EggShortDiameter ~ log_ProthoraxWidth,data=clutch_av_center))
clutch_av_center$resid_LongDiameter <- residuals(lm(log_EggLongDiameter ~ log_ProthoraxWidth,data=clutch_av_center))

model <- lm(resid_LongDiameter ~ resid_ShortDiameter, clutch_av_center)

visreg(model, xvar = "resid_ShortDiameter", overlay = TRUE, scale = "response", gg = TRUE, rug = FALSE) +
  geom_point(data = clutch_av_center, aes(x = resid_ShortDiameter, y = resid_LongDiameter)) + 
  geom_abline(intercept = 0, slope = 1, linetype = 2)

confint(model)

# Statistical analysis
d$EggPosition <- relevel(factor(d$EggPosition), ref = "Periphery")
# Egg long diameter
model<-lmer(EggLongDiameter~EggPosition+(1|ClutchID),d)
summary(model)

# Egg short diameter
model<-lmer(EggShortDiameter~EggPosition+(1|ClutchID),d)
summary(model)

# Egg volume
model<-lmer(EggVolume~EggPosition+(1|ClutchID),d)
summary(model)

# Elongation model with interaction
model<-lmer(EggLongDiameter~EggShortDiameter*EggPosition+(1|ClutchID),d)
summary(model) 

# Elongation model without interaction (table 1)
model<-lmer(EggLongDiameter~EggShortDiameter+EggPosition+(1|ClutchID),d)
summary(model)