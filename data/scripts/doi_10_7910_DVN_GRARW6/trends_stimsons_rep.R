###### Using the DyadRatios package to estimate Stimson's dyad-ratios algorithm
# Libraries
# devtools:::install_github("davidaarmstrong/DyadRatios")
if (!require("pacman")) install.packages("pacman")
p_load(ggplot2, tidyverse, dplyr, DyadRatios, gmodels, labelled, sjPlot, car, forcats, viridis, plyr)

# Loop code for doing the algorithm for each main type of support variable
type <- list("parl", "gov", "polpar", "civil", "leg", "police")

for (x in type) {
# Read data
  infile <- paste("Stims", x, "sc.csv", sep ="_")
  x.stims = read.csv(infile)
  x.stims$date2=as.Date(x.stims$date, format="%d %b %Y")
  
  # Include only countries which were liberal or electoral democracies in a majority of years included, according to the VDem Regions of the world measure
  x.stims.dem <- subset(x.stims, vdem_row_maj == 1)
  
  # Remove observations before 1990 because they are few and unrepresentative globally (almost only in the WENA region)
  x.stims.dem2 <- x.stims.dem [!x.stims.dem$year < 1990,]
  
  # Stimson's dyad-ratios algorithm for global trends in each trust measure
  extracted.dem <- DyadRatios::extract(x.stims.dem2$varname, x.stims.dem2$date2, x.stims.dem2$value,
                           ncases = x.stims.dem2$ncases, unit = "A", mult = 40, 
                       begindt = NA, enddt = NA, npass = 1, smoothing = TRUE, 
                       endmonth = 12, plot=TRUE, verbose=TRUE)
  
  # Extract as data-frame
  year.dem <- extracted.dem$period
  latent_x.dem <- extracted.dem$latent1
  
  outfile.dem <- as.data.frame(cbind(year.dem, latent_x.dem))
  
  colnames(outfile.dem) <- c("year", str_c("latent_", x, ".dem"))
  
  assign(paste("Stims", x, "dem", sep = "."), outfile.dem)
  
  # Stimsons' algorithm by region, when only including democracies
  x.stims.dem2$regfact <- as.factor(x.stims.dem2$regpol6)
  x.stims.dem2$regfact <- factor(x.stims.dem2$regfact,levels = 
                            c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                              "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))
  x.stims.dem2$regnum <- as.numeric(x.stims.dem2$regfact)
  reglist <- list(2, 3, 4, 5, 6)
  
  for (y in reglist) {
    x.stims.dem.reg <- subset(x.stims.dem2, regnum == y)
    try(extracted.reg <- DyadRatios::extract(x.stims.dem.reg$varname, x.stims.dem.reg$date2, x.stims.dem.reg$value, ncases = x.stims.dem.reg$ncases, unit = "A", mult = 40, 
                             begindt = NA, enddt = NA, npass = 1, smoothing = TRUE, 
                             endmonth = 12, plot=TRUE, verbose=TRUE))
    
    # Extract as data-frames
    year.reg <- extracted.reg$period
    latent_x.reg <- extracted.reg$latent1
    
    outfile.reg <- as.data.frame(cbind(year.reg, latent_x.reg))
    
    colnames(outfile.reg) <- c("year", str_c("latent_", x))
    
    assign(paste("Stims", x, "reg", y, sep = "."), outfile.reg)
  }
  
  # Run separately for the WENA region, using data since 1980
  x.stims.dem$regfact <- as.factor(x.stims.dem$regpol6)
  x.stims.dem$regfact <- factor(x.stims.dem$regfact,levels = 
                                   c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                     "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))
  x.stims.dem$regnum <- as.numeric(x.stims.dem$regfact)
  x.stims.dem.reg1 <- subset(x.stims.dem, regnum == 1)
  
  # Remove observations before 1980 because they are few and unrepresentative
  x.stims.dem.reg1 <- x.stims.dem.reg1 [!x.stims.dem.reg1$year < 1980,]
  
  extracted.reg1 <- DyadRatios::extract(x.stims.dem.reg1$varname, x.stims.dem.reg1$date2, x.stims.dem.reg1$value,
                           ncases = x.stims.dem.reg1$ncases, unit = "A", mult = 40, 
                           begindt = NA, enddt = NA, npass = 1, smoothing = TRUE, 
                           endmonth = 12, plot=TRUE, verbose=TRUE)
  # Extract as a data-frame
  year.reg1 <- extracted.reg1$period
  latent_x.reg1 <- extracted.reg1$latent1
  outfile.reg1 <- as.data.frame(cbind(year.reg1, latent_x.reg1))
  colnames(outfile.reg1) <- c("year", str_c("latent_", x))
  assign(paste("Stims", x, "reg", "1", sep = "."), outfile.reg1)
}

# Merge
formerge.dem <- list (Stims.parl.dem, Stims.gov.dem, Stims.polpar.dem, Stims.leg.dem, Stims.civil.dem, Stims.police.dem)
Stims.glob.dem <- join_all(formerge.dem, by=c("year"))

write.csv(Stims.glob.dem, "Stims_global_dem.csv", row.names=FALSE)

# Regions
formerge.reg.1 <- list (Stims.parl.reg.1, Stims.gov.reg.1, Stims.polpar.reg.1,
                        Stims.leg.reg.1, Stims.civil.reg.1, Stims.police.reg.1)
Stims.reg.1 <- join_all(formerge.reg.1, by=c("year"))
Stims.reg.1$Region="W-Europe & N-America"

write.csv(Stims.reg.1, "Stims_reg1.csv", row.names=FALSE)

formerge.reg.2 <- list (Stims.parl.reg.2, Stims.gov.reg.2, Stims.polpar.reg.2,
                        Stims.leg.reg.2, Stims.civil.reg.2, Stims.police.reg.2)
Stims.reg.2 <- join_all(formerge.reg.2, by=c("year"))
Stims.reg.2$Region="E-Europe & C-Asia"

write.csv(Stims.reg.2, "Stims_reg2.csv", row.names=FALSE)

formerge.reg.3 <- list (Stims.parl.reg.3, Stims.gov.reg.3, Stims.polpar.reg.3,
                        Stims.leg.reg.3, Stims.civil.reg.3, Stims.police.reg.3)
Stims.reg.3 <- join_all(formerge.reg.3, by=c("year"))
Stims.reg.3$Region="L-America & Caribbean"

write.csv(Stims.reg.3, "Stims_reg3.csv", row.names=FALSE)

formerge.reg.4 <- list (Stims.parl.reg.4, Stims.gov.reg.4, Stims.polpar.reg.4,
                        Stims.leg.reg.4, Stims.civil.reg.4, Stims.police.reg.4)
Stims.reg.4 <- join_all(formerge.reg.4, by=c("year"))
Stims.reg.4$Region="M-East & N-Africa"

write.csv(Stims.reg.4, "Stims_reg4.csv", row.names=FALSE)

formerge.reg.5 <- list (Stims.parl.reg.5, Stims.gov.reg.5, Stims.polpar.reg.5,
                        Stims.leg.reg.5, Stims.civil.reg.5, Stims.police.reg.5)
Stims.reg.5 <- join_all(formerge.reg.5, by=c("year"))
Stims.reg.5$Region="Sub-Saharan Africa"

write.csv(Stims.reg.5, "Stims_reg5.csv", row.names=FALSE)

formerge.reg.6 <- list (Stims.parl.reg.6, Stims.gov.reg.6, Stims.polpar.reg.6,
                        Stims.leg.reg.6, Stims.civil.reg.6, Stims.police.reg.6)
Stims.reg.6 <- join_all(formerge.reg.6, by=c("year"))
Stims.reg.6$Region="Asia & Pacific"

write.csv(Stims.reg.6, "Stims_reg6.csv", row.names=FALSE)

Stims.reg.all <- rbind (Stims.reg.1, Stims.reg.2, Stims.reg.3, Stims.reg.4, Stims.reg.5, Stims.reg.6)
write.csv(Stims.reg.all, "Stims_reg_all.csv", row.names=FALSE)

#Plot
col_swd = rgb(0, 158, 115, 255, maxColorValue = 255)
col_swd_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_parl = rgb(213, 94, 0, 255, maxColorValue = 255)
col_parl_ci = rgb(213, 94, 0, 100, maxColorValue = 255)
col_gov = rgb(220, 135, 0, 255, maxColorValue = 255)
col_gov_ci = rgb(220, 135, 0, 100, maxColorValue = 255)
col_polpar = rgb(230, 159, 0, 255, maxColorValue = 255)
col_polpar_ci = rgb(230, 159, 0, 100, maxColorValue = 255)

col_civil = rgb(0, 158, 115, 255, maxColorValue = 255)
col_civil_ci = rgb(0, 158, 115, 100, maxColorValue = 255)
col_leg = rgb(10, 128, 158, 255, maxColorValue = 255)
col_leg_ci = rgb(10, 128, 158, 100, maxColorValue = 255)
col_police = rgb(19,93,216, 255, maxColorValue = 255)
col_police_ci = rgb(19,93,216, 100, maxColorValue = 255)

col_ap = rgb(0, 255, 0, 255, maxColorValue = 255)
col_eeca = rgb(255, 0, 0, 255, maxColorValue = 255)
col_lac = rgb(255, 165, 0, 255, maxColorValue = 255)
col_mena = rgb(0, 96, 0, 255, maxColorValue = 255)
col_ssa = rgb(156, 136, 71, 255, maxColorValue = 255)
col_wena = rgb(65, 105, 225, 255, maxColorValue = 255)
col_glob = rgb(0, 0, 0, 255, maxColorValue = 255)

col_gov2 = rgb(0, 0, 0, 255, maxColorValue = 255)
col_gov_ci2 = rgb(0, 0, 0, 100, maxColorValue = 255)
col_leg2 = rgb(128, 128, 128, 255, maxColorValue = 255)
col_leg_ci2 = rgb(128, 128, 128, 100, maxColorValue = 255)

df.dem <- Stims.glob.dem %>%
  gather(key = "variable", value = "value", -year)
head(df)

df.dem$value <- as.numeric(df.dem$value)
df.dem <- df.dem[!is.na(df.dem$value),]

df.dem$variable[df.dem$variable=="latent_parl.dem"] <- "Parliament"
df.dem$variable[df.dem$variable=="latent_gov.dem"] <- "Government"
df.dem$variable[df.dem$variable=="latent_polpar.dem"] <- "Political parties"
df.dem$variable[df.dem$variable=="latent_leg.dem"] <- "Legal system"
df.dem$variable[df.dem$variable=="latent_civil.dem"] <- "Civil service"
df.dem$variable[df.dem$variable=="latent_police.dem"] <- "Police"

df.dem$variable_f <- factor(df.dem$variable, levels = c("Parliament", "Government", "Political parties",
                                                        "Civil service", "Legal system", "Police"))
viridis_6 <- viridis(6, end=0.9, direction = -1)

st_glob_plot.dem <- ggplot(df.dem, aes(x = year, y = value)) + 
  geom_line(aes(color = variable_f, linetype = "solid"), size=1.2) + 
  scale_color_manual(values = col_type,
                     labels = c("Parliament", "Government", "Political parties",
                                "Legal system", "Civil service", "Police")) +
  facet_wrap(~variable_f) +
  theme_minimal() +
  scale_x_continuous("", breaks = c(1980, 1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020)) +
  scale_y_continuous("", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
                     labels = c("0%","10%", "20%", "30%", "40%","50%", "60%", "70%", "80%", "90%", "100%")) +
  expand_limits(y = c(0, 100)) +
  labs(title = "", color = "") + # adds a title
  theme(legend.position="bottom", legend.box = "horizontal",
        axis.text.x = element_text(size = 6)) +
  guides(linetype = "none") +
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))
st_glob_plot.dem
ggsave("Stimsons_global_trust6.png", st_glob_plot.dem)

#By region
#Drop series for gov, polpar and civil in SSA because there are very few observations behind those
Stims.reg.all$latent_polpar[Stims.reg.all$Region == "Sub-Saharan Africa"] <- NA
Stims.reg.all$latent_gov[Stims.reg.all$Region == "Sub-Saharan Africa"] <- NA
Stims.reg.all$latent_civil[Stims.reg.all$Region == "Sub-Saharan Africa"] <- NA

#Reshape
st.reg <- Stims.reg.all %>%
  gather(key = "variable", value = "value", -year, -Region)
head(st.reg)

st.reg$value <- as.numeric(st.reg$value)
st.reg <- st.reg[!is.na(st.reg$value),]

st.reg$variable[st.reg$variable=="latent_parl"] <- "Parliament"
st.reg$variable[st.reg$variable=="latent_gov"] <- "Government"
st.reg$variable[st.reg$variable=="latent_polpar"] <- "Political parties"
st.reg$variable[st.reg$variable=="latent_leg"] <- "Legal system"
st.reg$variable[st.reg$variable=="latent_civil"] <- "Civil service"
st.reg$variable[st.reg$variable=="latent_police"] <- "Police"

st.reg$variable_f <- factor(st.reg$variable, levels = c("Parliament", "Government", "Political parties",
                                                       "Civil service", "Legal system", "Police"))

st.reg$Region <- as.factor(st.reg$Region)
st.reg$Region <- factor(st.reg$Region,levels = 
                                 c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                   "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific"))
st_reg_plot <- st.reg %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(color = variable_f, linetype = variable_f), size=0.8) + 
  scale_color_manual(values = col_type,
                     labels = c("Parliament", "Government", "Political parties",
                                "Legal system", "Civil service", "Police")) +
  theme_minimal() +
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous("", breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  facet_wrap(~Region, labeller = ) + # this makes a separate plot for each region
  expand_limits(y = c(0, 100)) +
  labs(title = "", color = "") + # adds a title
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(linetype = "none") +
  theme(panel.spacing = unit(1, "lines")) + #
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE))
st_reg_plot
ggsave("Stimsons_reg_all.png", st_reg_plot)

st_regtype_plot <- st.reg %>%
  ggplot(aes(x = year, y = value)) + 
  geom_line(aes(color = Region, linetype = Region), size = 0.8) + 
  scale_color_manual(values = c(col_wena, col_eeca, col_lac, col_mena, col_ssa, col_ap),
                     labels = c("W-Europe & N-America", "E-Europe & C-Asia", "L-America & Caribbean",
                                "M-East & N-Africa", "Sub-Saharan Africa", "Asia & Pacific")) +
  theme_minimal() +
  scale_x_continuous("", breaks = c(1980, 1990, 2000, 2010, 2020)) +
  scale_y_continuous("", breaks = c(0, 20, 40, 60, 80, 100),
                     labels = c("0%", "20%", "40%", "60%", "80%", "100%")) +
  facet_wrap(~variable_f) + # this makes a separate plot for each variable
  expand_limits(y = c(0, 100)) +
  labs(title = "", color = "") + # adds a title
  theme(legend.position="bottom", legend.box = "horizontal") +
  guides(linetype = "none") +
  theme(panel.spacing = unit(1, "lines")) + #
  theme(plot.background = element_rect(fill = 'white', colour = 'white')) +
  guides(color = guide_legend(override.aes = list(linetype = c("solid", "22", "42", "44", "13", "1343")),
                              byrow = TRUE))
st_regtype_plot
ggsave("Stimsons_all_reg.png", st_regtype_plot)
