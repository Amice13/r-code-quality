##### Replication Code: 
##### Illustration 1, Illustration 2, and Appendix in 
##### "Who, what, and where? Linking violence to civil wars"
#### Last updated: 17 February 2025

##### Install and load necessary packages #####
# load pacman
if(!require("pacman")) install.packages("pacman")
# load packages 
p_load(tidyverse, waffle,  grid, gridExtra, stargazer, haven, sf, rnaturalearth, 
       furrr, magrittr, cshapes, lwgeom)
# clean environment
rm(list = ls())


##### Data Illustration 1: Conflict Complexity #####

###### Statistics mentioned in the text (Information on unlinked events) ######
ged <- read_rds("ged_prepared_totracklinks_v241.rds")
names(ged)
ged <- ged %>%
  mutate(across(c(link_actor_type1:link_spatial_communal), ~if_else(is.na(.), 0, .)))
ged <- ged %>%
  mutate(spatiallylinked = apply(select(., starts_with("link_spatial")), 1, function(row) {
    ifelse(any(row == 1, na.rm = TRUE), 1, 0)
  }))

nolink <- ged %>% filter(actorlinked==0 & spatiallylinked==0)
74625/394922
table(nolink$type_of_violence, useNA = "always")
18313/74625
nolinkwrv <- nolink %>% filter(type_of_violence!=1)
nolinkwrv <- nolinkwrv %>%
  group_by(dyad_new_id) %>%
  summarise(type_of_violence = first(type_of_violence),
            numevents = n(),
            best = sum(best),
            .groups = 'drop')


######  Figure 2: Waffle chart ######  
ucdp <- read_rds("div_GED-to-ACD_conflict_month_final_v241.rds") # generated with user choice to divide deaths across conflict zones

# Drop conflicts with missing conflict zones
ucdp <- ucdp %>% dplyr::filter(missingzone!=1)

# Generate overall violence measure by combining battle and war-related violence
ucdp <- ucdp %>% mutate(allviol = battledeaths + zone_wrv)
summary(ucdp$allviol)

# Statistics mentioned in text
allviol <- sum(ucdp$allviol)
battle <- sum(ucdp$battledeaths)
collateral <- sum(ucdp$collateralciv)
osvall <- sum(ucdp$zone_osv)
nsvall <- sum(ucdp$zone_nsv)

print(allviol)
battle/allviol
collateral/allviol
osvall/allviol
nsvall/allviol

# wrv linked at all, by actor or by space
wrv_byactor <- ged %>% filter((type_of_violence==2 | type_of_violence==3) & (actorlinked==1))
wrv_byspace <- ged %>% filter((type_of_violence==2 | type_of_violence==3) & (actorlinked==0 & spatiallylinked==1))
sum(wrv_byactor$best)
sum(wrv_byspace$best)
361119/(361119+271345)

# Prepare data to produce the waffle chart in Figure 2
communal <- sum(ucdp$communal)
internsag <- sum(ucdp$internsag)
interreb <- sum(ucdp$interreb)
govosv <- sum(ucdp$govosv)
rebelosv <- sum(ucdp$rebelosv)
nsagosv <- sum(ucdp$nsagosv)
collateralciv <- sum(ucdp$collateralciv)
combatant <- (sum(ucdp$battledeaths)-collateralciv)
summary_df <- data.frame(
  category = c("communal", "internsag", "interreb", "govosv", "rebelosv", "nsagosv", "collateralciv", "combatant"),
  value = c(communal, internsag, interreb, govosv, rebelosv, nsagosv, collateralciv, combatant)
)
names(summary_df)
summary_df <- summary_df %>% mutate(violtype = if_else(category=="combatant" | category=="collateralciv","battle",NA),
                                    violtype = if_else(category=="govosv" | category=="rebelosv" | category=="nsagosv","osv",violtype),
                                    violtype = if_else(category=="interreb" | category=="internsag" | category=="communal","nsv",violtype))

head(summary_df)
summary_df$valuerounded <- round(summary_df$value / 10000) * 10000
names(summary_df)
summary_df <- summary_df %>% mutate(numsymb = valuerounded/10000)

# Produce the waffle chart in Figure 2
waffle1 <- waffle(
  c('Combatant Deaths' = 170, 'Collateral Civilian Deaths' = 21), rows = 10, colors = c("azure4", "black"),
  #title = 'Battle', 
  legend_pos="bottom", 
) +
  theme(plot.title = element_text(size = 10, face = "bold"), legend.text = element_text(size = 24))


waffle2 <- waffle(
  c('Government OSV' = 29, 'Rebel OSV' = 17, 'Other NSAG OSV' = 16), rows = 10, colors = c("darkgreen", "green", "darkolivegreen"),
  #title = 'Violence Against Civilians (OSV)', 
  legend_pos="bottom"
) +
  theme(plot.title = element_text(size = 10, face = "bold"), legend.text = element_text(size = 25))

waffle3 <- waffle(
  c('Between Rebels' = 13, 'Between other NSAG' = 8, 'Communal' = 2), rows = 10, colors = c("darkslategrey", "darkslategray1", "cyan4"),
  #title = 'Non-State Violence', 
  legend_pos="bottom"
) +
  theme(plot.title = element_text(size = 10, face = "bold"), legend.text = element_text(size = 25)) 

# combined_plot <- grid.arrange(waffle1, waffle2, waffle3, ncol = 1) 


###### Figure 3 ######
rm(list = ls())

ucdp <- read_rds("GED-to-ACD_conflict_month_final_v241.rds") # generated with default user choices, but with all postwar/interwar (inactive) months

# Generate overall violence measure by combining battle and war-related violence
ucdp <- ucdp %>% mutate(allviol = battledeaths + zone_wrv)


kashmir <- ucdp %>% filter(conflict_id==364)
naga <- ucdp %>% filter(conflict_id==251)

kashmir <- kashmir %>% group_by(year) %>% 
  summarise(battledeaths = sum(battledeaths),
            zone_osv = sum(zone_osv),
            zone_nsv = sum(zone_nsv),
            allviol = sum(allviol),
            active = max(active))

kashmir <- kashmir %>% filter(active==1)

naga <- naga %>% group_by(year) %>% 
  summarise(battledeaths = sum(battledeaths),
            zone_osv = sum(zone_osv),
            zone_nsv = sum(zone_nsv),
            allviol = sum(allviol),
            active = max(active))
naga <- naga %>% filter(year>=1992)

# Battle, with active or not
summary(naga$battledeaths)
summary(naga$zone_osv)
summary(naga$zone_nsv)

# Make all the graphs
# Naga plots 
ggplot(naga, aes(x = factor(year))) + 
  geom_col(aes(y = battledeaths), fill = "grey10", width = 0.8) +  
  geom_col(aes(y = ifelse(active == 1, -7, 0)), fill = "brown2", width = 0.8) +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1992, 2023, by = 2),  
    labels = seq(1992, 2023, by = 2) 
  ) +
  scale_y_continuous(
    name = "Battledeaths",
    limits = c(-7, 160),
    expand = c(0, 0)
  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    plot.title = element_text(size = 18, face = "bold"),  
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +  
  labs(
    title = "Military Battle",
    subtitle = "Red Year = War Active",
    x = "Year",
    y = "Battledeaths"
  )

ggplot(naga, aes(x = factor(year), y = zone_osv)) + 
  geom_col(fill = "grey30") +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1992, 2023, by = 2), 
    labels = seq(1992, 2023, by = 2) 
  ) +  
  scale_y_continuous(
    name = "Civilian Targeting",
    limits = c(-7, 160),
    expand = c(0, 0)
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  # Add margin to push labels down
    axis.text.y = element_text(size = 15),  # Increase y-axis label size
    axis.title.x = element_text(size = 15),  # Increase x-axis title size
    axis.title.y = element_text(size = 15),  # Increase y-axis title size
    plot.title = element_text(size = 18, face = "bold"),  # Increase x-axis title size
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) + 
  labs(title = "Civilian Targeting", x = "Year", y = "Deaths from OSV")

ggplot(naga, aes(x = factor(year), y = zone_nsv)) + 
  geom_col(fill = "grey60") +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1992, 2023, by = 2),  
    labels = seq(1992, 2023, by = 2) 
  ) +  
  scale_y_continuous(
    name = "Non-State Violence",
    limits = c(-7, 160),
    expand = c(0, 0)
  ) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),  
    plot.title = element_text(size = 18, face = "bold"), 
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +  
  labs(title = "Non-State Violence", x = "Year", y = "Deaths from NSV")

# Kashmir plots
summary(kashmir$battledeaths)
summary(kashmir$zone_osv)
summary(kashmir$zone_nsv)

ggplot(kashmir, aes(x = factor(year))) + 
  geom_col(aes(y = battledeaths), fill = "grey10", width = 0.8) +  
  geom_col(aes(y = ifelse(active == 1, -100, 0)), fill = "brown2", width = 0.8) +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1990, 2023, by = 2),  
    labels = seq(1990, 2023, by = 2) 
  ) +
  scale_y_continuous(name = "Battledeaths", limits = c(-120, 2400), breaks = seq(0, 2400, 600), expand = c(0, 0)) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    plot.title = element_text(size = 18, face = "bold"),  
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +  
  labs(
    title = "Military Battle",
    subtitle = "Red Year = War Active",
    x = "Year",
    y = "Battledeaths"
  )

ggplot(kashmir, aes(x = factor(year), y = zone_osv)) + 
  geom_col(fill = "grey30") +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1990, 2023, by = 2),  
    labels = seq(1990, 2023, by = 2) 
  ) +  
  scale_y_continuous(name = "Civilian Targeting", limits = c(-120, 2400), breaks = seq(0, 2400, 600), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    plot.title = element_text(size = 18, face = "bold"),  
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +  
  labs(title = "Civilian Targeting", x = "Year", y = "Deaths from OSV")

ggplot(kashmir, aes(x = factor(year), y = zone_nsv)) + 
  geom_col(fill = "grey60") +  
  # geom_line(aes(y = allviol, group = 1), color = "black", size = 0.2) +  
  scale_x_discrete(
    name = "Year",
    expand = c(0, 0),
    breaks = seq(1990, 2023, by = 2),  
    labels = seq(1990, 2023, by = 2) 
  ) +  
  scale_y_continuous(name = "Non-State Violence", limits = c(-120, 2400), breaks = seq(0, 2400, 600), expand = c(0, 0)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 15, angle = 90, hjust = 1, vjust = 0.5, 
                               margin = margin(t = 0)),  
    axis.text.y = element_text(size = 15),  
    axis.title.x = element_text(size = 15),  
    axis.title.y = element_text(size = 15),  
    plot.title = element_text(size = 18, face = "bold"),  
    plot.subtitle = element_text(size = 15, face = "italic"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "lines")
  ) +  
  labs(title = "Non-State Violence", x = "Year", y = "Deaths from NSV")


##### Data Illustration 2: Measuring Conflict Severity #####
rm(list = ls())
ucdp <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")

###### Correlations mentioned in text ######

# Only active wars with no missing zone
ucdp <- ucdp %>% dplyr::filter(active==1)
ucdp <- ucdp %>% dplyr::filter(missingzone!=1)
ucdp <- ucdp %>% mutate(allviol = battledeaths + zone_wrv)

# Spearman on conflict-month level
spearman_corr <- cor(ucdp$battledeaths, ucdp$allviol, method = "spearman")

# Spearman on conflict-year level
ucdpy <- ucdp %>% group_by(conflict_id, year) %>% 
  summarise(battledeaths = sum(battledeaths),
            zone_wrv = sum(zone_wrv),
            allviol = sum(allviol))
spearman_corr <- cor(ucdpy$battledeaths, ucdpy$allviol, method = "spearman")

# OLS regression of how well (ln) bd predicts other (ln) wrv
ucdp <- ucdp %>% mutate(lnbd = log(battledeaths+1),
                          lnwrv = log(zone_wrv + 1),
                          lnall = log(allviol + 1))

ucdpy <- ucdpy %>% group_by(conflict_id, year) %>% 
  summarise(battledeaths = sum(battledeaths),
            zone_wrv = sum(zone_wrv),
            allviol = sum(allviol))

ucdpy <- ucdpy %>% mutate(lnbd = log(battledeaths+1),
                            lnwrv = log(zone_wrv + 1),
                            lnall = log(allviol + 1))

summary(model <- lm(ucdp$lnwrv ~ ucdp$lnbd)) # on conflict-month
summary(model <- lm(ucdpy$lnwrv ~ ucdpy$lnbd)) # on conflict-month

###### Figure 4a ######
d_range <- ucdp %>% select(conflict_id, mdate, location, year, zone_wrv, battledeaths, side_a, side_a_2nd, side_b, side_b_2nd) %>% 
  group_by(conflict_id, mdate,) %>% 
  mutate(vtotal = zone_wrv + battledeaths) %>% 
  ungroup() %>% 
  group_by(conflict_id) %>% 
  summarise(vtotal = sum(vtotal, na.rm = T),
            vbattle = sum(battledeaths, na.rm = T), 
            location = str_c(unique(location), collapse = ",")
  ) %>% 
  arrange(desc(vtotal)) %>% 
  slice(1:20) %>% 
  mutate(location = ifelse(conflict_id == 299, "Syria (Government)",
                           ifelse(conflict_id == 13604, "Syria (IS)", 
                                  ifelse(conflict_id == 267, "Ethiopia (Government)",
                                         ifelse(conflict_id == 275, "Ethiopia (Eritrea)",
                                                ifelse(conflict_id == 297, "Nigeria (Government)",
                                                       ifelse(conflict_id == 13641, "Nigeria (IS)",
                                                              location))))))) %>% 
  select(location, vtotal, vbattle) %>% 
  mutate(vbattle = log(vbattle),    
         vtotal = log(vtotal))
# reorder again 
d_range$location <- fct_reorder(d_range$location, d_range$vtotal, .desc = TRUE)

# need to log transform
# plot 
d_range %>%
  ggplot(aes(x = location)) +
  geom_linerange(aes(ymin = vbattle, ymax = vtotal, x = location),
                 size = 1.5, alpha = 0.075) +
  geom_point(aes(y = vbattle), colour = "grey75", size = 3) + 
  geom_point(aes(y = vtotal), colour = "black", size = 3) +
  coord_flip() +
  #scale_y_continuous(labels = scales::dollar_format()) +
  ylab("Fatalities (ln)") +
  theme_bw(base_size = 18) +
  theme(axis.title.y = element_blank()) +
  scale_y_continuous(limits = c(9, 13), breaks = c(9,10,11,12,13), expand = c(0, 0))


###### Figure 4b ######
names(ucdp)

# collapse to year (sum violence)
ucdpm <- ucdp %>% group_by(conflict_id, year) %>% 
  summarise(sideb = first(sideb_constant),
            missingzone = first(missingzone),
            battledeaths = sum(battledeaths),
            collateralciv = sum(collateralciv),
            rebelosv = sum(rebelosv),
            interreb = sum(interreb),
            combatantosv = sum(combatantosv),
            govosv = sum(govosv),
            confgovosv = sum(confgovosv),
            nsagosv = sum(nsagosv),
            internsag = sum(internsag),
            communal = sum(communal),
            zone_osv = sum(zone_osv),
            zone_nsv = sum(zone_nsv),
            zone_wrv = sum(zone_wrv),
            zone_wrv_civ = sum(zone_wrv_civ),
            zone_nsv_collciv = sum(zone_nsv_collciv))
ucdpm <- ucdpm %>% dplyr::filter(missingzone!=1)


### Merge info on conflict types from Balcells and Kalyvas (PRIO10
balkal <- read_dta("PRIO100_replication_final.dta")
names(balkal)
options(scipen=999)
balkal <- balkal %>% dplyr::select(id, ccode, year, conv, snc, irr)

# Translate ID to new conflict IDs
trans <- read_csv("translate_conf.csv")
names(trans)
names(balkal)
class(trans$old_id)
balkal$id <- as.character(balkal$id)
balkal <- left_join(balkal,trans, by = c("id"="old_id"))
balkal <- balkal %>% relocate(new_id, .after = id)

# Merge their data to ours
summary(balkal$year)
names(ucdpm)
names(balkal)
balkal$new_id <- as.character(balkal$new_id)
ucdpm <- left_join(ucdpm,balkal, by = c("conflict_id"="new_id", "year"="year"))

ucdpm <- ucdpm %>% mutate(conftype = if_else(conv==1,1,NA),
                                                conftype = if_else(irr==1,2,conftype),
                                                conftype = if_else(snc==1,3,conftype))

table(ucdpm$conftype, useNA = "always")

ourswbk <- ucdpm

### Merge info on RAD (reben access to armaments, whether MCW or not)
rad <- read_csv("RAD_grouplevel_data.csv")
names(rad)
rad <- rad %>% dplyr::select(Group_UCDP_ID, Year, Groupyear_MCW)
summary(rad$Year)

# Need to bring our data to conflict-rebel-year
names(ucdpm)
ucdpreb <- ucdpm %>% separate_rows(sideb, sep = ", ")

# Merge in RAD by rebel ID year
class(ucdpreb$sideb)
class(rad$Group_UCDP_ID)
rad$Group_UCDP_ID <- as.character(rad$Group_UCDP_ID)
test <- left_join(ucdpreb,rad, by = c("sideb"="Group_UCDP_ID", "year"="Year"))
ucdpreb <- test

# Bring this back to annual (only the RAD info) and then merge it into ucdpm via conflict_id, year
table(ucdpreb$Groupyear_MCW, useNA = "always")
ucdpreb <- ucdpreb %>% mutate(hasmcw = if_else(Groupyear_MCW>=1,1,Groupyear_MCW))
table(ucdpreb$hasmcw, useNA = "always")
names(ucdpreb)
ucdpreb_agg <- ucdpreb %>%
  group_by(conflict_id, year) %>%
  dplyr::summarize(
    hasmcw = if (any(hasmcw == 1, na.rm = TRUE)) {
      1
    } else if (all(is.na(hasmcw))) {
      NA_integer_  
    } else {
      0
    },
    .groups = 'drop'
  ) %>%
  as.data.frame() 
table(ucdpreb_agg$hasmcw, useNA = "always")

ucdpm <- left_join(ucdpm,ucdpreb_agg, by = c("conflict_id", "year"))
ourswrad <- ucdpm

###### Figure 4b left ######
names(ourswbk)
ourswbk <- ourswbk %>% dplyr::filter(!is.na(conftype))

table(ourswbk$conftype)
62/405
325/405
18/405

ourswbk <- ourswbk %>% mutate(allviol = battledeaths + zone_wrv)
summary(ourswbk$allviol)

forgraphbk <- ourswbk %>% group_by(conftype) %>% 
  summarise(meanbd = mean(battledeaths),
            meanallviol = mean(allviol),
            lnmeanbd = log(meanbd),
            lnmeanallviol = log(meanallviol))


forgraphbk$conftype <- factor(forgraphbk$conftype, levels = c(3, 2, 1),
                            labels = c("SNCs", "Insurgencies", "Conventional Wars"))

# Calculate the difference between meanbd and meanallviol
forgraphbk$lndiff <- forgraphbk$lnmeanbd - forgraphbk$lnmeanallviol
forgraphbk$diff <- forgraphbk$meanbd - forgraphbk$meanallviol

# And the percentage increase of going from one to the next (not logged)
forgraphbk <- forgraphbk %>%
  mutate(
    percent_increase = ((meanallviol - meanbd) / meanbd) * 100
  )

# Create the plot
names(forgraphbk)


forgraphbk <- forgraphbk %>%
  mutate(
    midpoint = (lnmeanbd + lnmeanallviol) / 2,
    percent_increase_label = ifelse(percent_increase >= 0, 
                                    paste("+", round(percent_increase), "%", sep = ""), 
                                    paste(round(percent_increase), "%", sep = ""))
  )

# Balcells Kalyvas Plot (left-hand side of Figure 4b)
graphbk <- ggplot(forgraphbk, aes(x = lndiff, y = conftype)) +
  geom_point(aes(x = lnmeanbd, color = "Battle"), size = 3) +
  geom_point(aes(x = lnmeanallviol, color = "Total"), size = 3) +
  geom_segment(aes(x = lnmeanbd, xend = lnmeanallviol, y = conftype, yend = conftype), linetype = "dashed") +
  scale_color_manual(values = c("Battle" = "grey", "Total" = "black")) +
  labs(x = "Difference in (ln) mean deaths", y = NULL, title = "Technology of Rebellion") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none") +
  geom_text(aes(x = midpoint, y = conftype, label = percent_increase_label, hjust = 0.5, vjust = -0.5), size = 4.5)

ggsave("graphbk.png", graphbk, width = 10, height = 6, dpi = 600)


###### Figure 4b right ######s
names(ourswrad)
ourswrad <- ourswrad %>% dplyr::filter(!is.na(hasmcw)) 

table(ourswrad$hasmcw)

ourswrad <- ourswrad %>% mutate(allviol = battledeaths + zone_wrv)
summary(ourswrad$allviol)

test <- ourswrad %>%
  filter(!(conflict_id == "374" & year == 1994))
ourswrad <- test

forgraphrad <- ourswrad %>% group_by(hasmcw) %>% 
  summarise(meanbd = mean(battledeaths),
            meanallviol = mean(allviol),
            lnmeanbd = log(meanbd),
            lnmeanallviol = log(meanallviol))


# Calculate the difference between meanbd and meanallviol
forgraphrad$lndiff <- forgraphrad$lnmeanbd - forgraphrad$lnmeanallviol
forgraphrad$diff <- forgraphrad$meanbd - forgraphrad$meanallviol

# And the percentage increase of going from one to the next (not logged)
forgraphrad <- forgraphrad %>%
  mutate(
    percent_increase = ((meanallviol - meanbd) / meanbd) * 100
  )


# Create the plot
names(forgraphrad)

forgraphrad <- forgraphrad %>%
  mutate(
    midpoint = (lnmeanbd + lnmeanallviol) / 2,
    percent_increase_label = ifelse(percent_increase >= 0, 
                                    paste("+", round(percent_increase), "%", sep = ""), 
                                    paste(round(percent_increase), "%", sep = ""))  
  ) 
forgraphrad$hasmcw <- factor(forgraphrad$hasmcw, levels = c(0, 1),
                              labels = c("Rebels have no MCW", "Rebels have MCW"))

graphrad <- ggplot(forgraphrad, aes(x = lndiff, y = hasmcw)) +
  geom_point(aes(x = lnmeanbd, color = "Battle"), size = 3) +
  geom_point(aes(x = lnmeanallviol, color = "Total"), size = 3) +
  geom_segment(aes(x = lnmeanbd, xend = lnmeanallviol, y = hasmcw, yend = hasmcw), linetype = "dashed") +
  scale_color_manual(values = c("Battle" = "grey", "Total" = "black")) +
  labs(x = "Difference in (ln) mean deaths", y = NULL, title = "Rebel Armaments") +
  theme_minimal(base_size = 18) +
  theme(legend.position = "none") +
  geom_text(aes(x = midpoint, y = hasmcw, label = percent_increase_label, hjust = 0.5, vjust = -0.5), size = 4.5)

ggsave("graphrad.png", graphrad, width = 10, height = 6, dpi = 600)

#### Combined Graph 
combined_plot <- grid.arrange(graphbk, graphrad, ncol = 2)
ggsave("combined_replication_plot.png", plot = combined_plot, width = 16, height = 8, dpi = 300)


##### Appendix #####

###### Choice of Buffer (Appendix Table I) ######
rm(list = ls())

### First select all GED events that are linked via actor ID in our linking procedure
ged <- read_rds("ged_prepared_totracklinks_v241.rds")
names(ged)
table(ged$type_of_violence, useNA = "always")

# Reduce to war-related violence
ged <- ged %>% filter(type_of_violence!=1)

# How many percent of GED possibly war-related are actor-linked? (Figure mentioned in the manuscript)
table(ged$actorlinked, useNA = "always")
61505/140953 # Only 44%
actorlinked <- ged %>% filter(actorlinked==1)

# Load intersections (created by applying different user choices for buffering in our data creation code)
gedinter0 <- read_rds("b0_gedintersection_v24.rds") %>% mutate(capturedb0 = 1) %>% select(idsplit,side_a_new_id,side_b_new_id,final_date,capturedb0,z_conf_id) %>% st_drop_geometry()
gedinter10 <- read_rds("b10_gedintersection_v24.rds") %>% mutate(capturedb10 = 1) %>% select(idsplit,side_a_new_id,side_b_new_id,final_date,capturedb10,z_conf_id) %>% st_drop_geometry()
gedinter20 <- read_rds("b20_gedintersection_v24.rds") %>% mutate(capturedb20 = 1) %>% select(idsplit,side_a_new_id,side_b_new_id,final_date,capturedb20,z_conf_id) %>% st_drop_geometry()
gedinter30 <- read_rds("b30_gedintersection_v24.rds") %>% mutate(capturedb30 = 1) %>% select(idsplit,side_a_new_id,side_b_new_id,final_date,capturedb30,z_conf_id) %>% st_drop_geometry()

# by how many actorlinked events () does a zone increase as it increases by 10% each?
alsmall <- actorlinked %>% select(idsplit,side_a_new_id,side_b_new_id,final_date,actorlinked)
gedinter0 <- left_join(gedinter0,alsmall, by = c("idsplit","side_a_new_id","side_b_new_id","final_date"))
gedinter10 <- left_join(gedinter10,alsmall, by = c("idsplit","side_a_new_id","side_b_new_id","final_date"))
gedinter20 <- left_join(gedinter20,alsmall, by = c("idsplit","side_a_new_id","side_b_new_id","final_date"))
gedinter30 <- left_join(gedinter30,alsmall, by = c("idsplit","side_a_new_id","side_b_new_id","final_date"))

# Reduce to only actorlinked events
gedinter0 <- gedinter0 %>% filter(actorlinked==1)
gedinter10 <- gedinter10 %>% filter(actorlinked==1)
gedinter20 <- gedinter20 %>% filter(actorlinked==1)
gedinter30 <- gedinter30 %>% filter(actorlinked==1)

# Need to merge in info which zone is even "possible" (which conflict active) at any point in time
base <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")
baserelevantepimonths <- base %>% select(epi_id, mdate) %>% mutate(activeepisodes = 1)
gedinter0 <- left_join(gedinter0, baserelevantepimonths, by = c("z_conf_id" = "epi_id", "final_date" = "mdate"))
gedinter0 <- gedinter0 %>% filter(activeepisodes == 1)
gedinter10 <- left_join(gedinter10, baserelevantepimonths, by = c("z_conf_id" = "epi_id", "final_date" = "mdate"))
gedinter10 <- gedinter10 %>% filter(activeepisodes == 1)
gedinter20 <- left_join(gedinter20, baserelevantepimonths, by = c("z_conf_id" = "epi_id", "final_date" = "mdate"))
gedinter20 <- gedinter20 %>% filter(activeepisodes == 1)
gedinter30 <- left_join(gedinter30, baserelevantepimonths, by = c("z_conf_id" = "epi_id", "final_date" = "mdate"))
gedinter30 <- gedinter30 %>% filter(activeepisodes == 1)

# Statistics used to make Table I in the Appendix

# Percentage increase in the number of events (compared to unbuffered zone)
(115251-98447)/98447*100
(123449-98447)/98447*100
(134336-98447)/98447*100

# Unique events
geduniq0 <- gedinter0 %>% distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = T)
geduniq10 <- gedinter10 %>% distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = T)
geduniq20 <- gedinter20 %>% distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = T)
geduniq30 <- gedinter30 %>% distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = T)

# Percentage increase in unique events (compared to unbuffered zone)
(51949-51448)/51448*100
(52159-51448)/51448*100
(52338-51448)/51448*100

# Events missed (actorlinked but not in the intersection)
(1-(51448/61505))*100
(1-(51949/61505))*100
(1-(52159/61505))*100
(1-(52338/61505))*100

# duplicates
dup0 <- gedinter0 %>%
  group_by(idsplit, side_a_new_id, side_b_new_id, final_date) %>%
  mutate(conflict_zone_count = n_distinct(z_conf_id)) %>%
  distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = TRUE)
summary(dup0$conflict_zone_count)

dup10 <- gedinter10 %>%
  group_by(idsplit, side_a_new_id, side_b_new_id, final_date) %>%
  mutate(conflict_zone_count = n_distinct(z_conf_id)) %>%
  distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = TRUE)
summary(dup10$conflict_zone_count)

dup20 <- gedinter20 %>%
  group_by(idsplit, side_a_new_id, side_b_new_id, final_date) %>%
  mutate(conflict_zone_count = n_distinct(z_conf_id)) %>%
  distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = TRUE)
summary(dup20$conflict_zone_count)

dup30 <- gedinter30 %>%
  group_by(idsplit, side_a_new_id, side_b_new_id, final_date) %>%
  mutate(conflict_zone_count = n_distinct(z_conf_id)) %>%
  distinct(idsplit, side_a_new_id, side_b_new_id, final_date, .keep_all = TRUE)
summary(dup30$conflict_zone_count)


###### Zones Crossing Country Borders ######
rm(list = ls())

# How many zones (unclipped, zero buffer) cross into other countries?
# Load cshapes to have country borders
cshapes <- cshp(date = NA, useGW = TRUE, dependencies = FALSE) %>%
  filter(end == "2019-12-31")

cshapes <- cshapes %>%
  mutate(geometry = if_else(st_is_valid(geometry), geometry, st_make_valid(geometry)))

zones <- st_read("Conflict Zones/b0_episodezones_v241.shp")

border <- st_intersection(zones, cshapes) %>% st_set_geometry(NULL)

crosses <- border %>%
  group_by(epi_id) %>%
  summarise(
    total = n(),  
    countries = toString(unique(country_name))
  ) %>%
  ungroup()

table(crosses$total, useNA = "always")
(308-81)/308 # 74% of zones cross a country border

# Two examples
clippedzones <- st_read("Conflict Zones/clipped_episodezones_v241.shp")

isr <- zones %>% filter(epi_id=="426_2")
isr_c <- clippedzones %>% filter(epi_id=="426_2")

ggplot() +
  geom_sf(data = cshapes, fill = "gray", color = "white") +  
  geom_sf(data = isr, fill = "darkblue", color = "darkblue", alpha = 0.5) +  
  geom_sf(data = isr_c, fill = "red", color = "darkred", alpha = 0.6) +  
  coord_sf(xlim = st_bbox(isr)[c("xmin", "xmax")], ylim = st_bbox(isr)[c("ymin", "ymax")], expand = FALSE) +
  theme_minimal() +
  labs(title = "Israel (Hezbollah): Original (blue) vs clipped (red) conflict zone")

drc <- zones %>% filter(epi_id=="283_1")
drc_c <- clippedzones %>% filter(epi_id=="283_1")

ggplot() +
  geom_sf(data = cshapes, fill = "gray", color = "white") +  
  geom_sf(data = drc, fill = "darkblue", color = "darkblue", alpha = 0.5) +  
  geom_sf(data = drc_c, fill = "red", color = "red", alpha = 0.6) +  
  coord_sf(xlim = st_bbox(drc)[c("xmin", "xmax")], ylim = st_bbox(drc)[c("ymin", "ymax")], expand = FALSE) +
  theme_minimal() +
  labs(title = "DR Congo: Original (blue) vs clipped (red) conflict zone")

### Sensitivity of conflict-month dataset to border clipping

# Load final conflict-month datasets for clipped and unclipped borders
basecl <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")
baseun <- read_rds("unclipped_GED-to-ACD_conflict_month_final_v241.rds")

baseun <-  baseun %>% select(epi_id,mdate,zone_wrv) %>% rename(unclipped_zone_wrv = zone_wrv)

base <- left_join(basecl,baseun, by = c("epi_id","mdate"))

# Reduce to active and 2 postwar years, and to episodes where we have conflict zones
base <- base %>% filter(active==1 | (active==0 & postwarmonth<=24)) %>% select(conflict_id,epi_id,mdate,active,location,missingzone,zone_wrv,unclipped_zone_wrv)
base <- base %>% filter(missingzone==0)

summary(base$zone_wrv)
summary(base$unclipped_zone_wrv)

# percentage of observations where deaths differ by more than 20% between the two versions
base <- base %>%
  mutate(substdiff = ifelse(zone_wrv == 0 & unclipped_zone_wrv >= 10, 1,
                            ifelse(zone_wrv != 0 & unclipped_zone_wrv > 1.2 * zone_wrv, 1, 0)))

table(base$substdiff, useNA = "always")

isrcm <- base %>% filter(epi_id=="426_2")
names(isrcm)
isrcm <- isrcm %>%
  mutate(
    mdate = as.Date(mdate, format = "%Y-%m-%d"),  
    YearMonth = format(mdate, "%Y-%m")            
  )
ggplot(isrcm, aes(x = YearMonth)) +
  geom_bar(aes(y = unclipped_zone_wrv, fill = "Total WRV (incl. unclipped areas)"), stat = "identity", color = "black", size = 0.25) +
  geom_bar(aes(y = zone_wrv, fill = "Reported Zone WRV"), stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = c("Total WRV (incl. unclipped areas)" = "red", "Reported Zone WRV" = "blue"),
                    name = "",  
                    labels = c("Total WRV (incl. unclipped areas)" = "No clip",
                               "Reported Zone WRV" = "Clipped")) +
  labs(title = "Israel (Hezbollah): Monthly War-Related Violence",
       x = "Month",
       y = "WRV Totals",
       subtitle = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

drccm <- base %>% filter(epi_id=="283_1")
drccm <- drccm %>%
  mutate(mdate = as.Date(paste0(mdate, "-01")))
ggplot(drccm, aes(x = mdate)) +
  geom_bar(aes(y = unclipped_zone_wrv, fill = "Total WRV (incl. unclipped areas)"), stat = "identity", color = "black", size = 0.25) +
  geom_bar(aes(y = zone_wrv, fill = "Reported Zone WRV"), stat = "identity", color = "black", size = 0.25) +
  scale_fill_manual(values = c("Total WRV (incl. unclipped areas)" = "red", "Reported Zone WRV" = "blue"),
                    name = "",  
                    labels = c("Total WRV (incl. unclipped areas)" = "No clip",
                               "Reported Zone WRV" = "Clipped")) +
  scale_x_date(date_breaks = "2 months", date_labels = "%Y-%m") +  
  labs(title = "DR Congo: Monthly War-Related Violence",
       x = "Month",
       y = "WRV Totals",
       subtitle = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

# What violence in the DRC?
drcinter <- read_rds("b0_gedintersection_v24.rds")
names(drcinter)
drcinter <- drcinter %>% filter(z_conf_id == "283_1" & final_date>="1997-05-01" & final_date<="1997-12-01")
drcinter <- drcinter %>% group_by(dyad_new_id) %>% 
  summarize(best = sum(best),
            type = first(type_of_violence))


###### Zone Overlap ######
rm(list = ls())

# What does overlap look like (even with clipped conflict zones)?
clippedzones <- st_read("Conflict Zones/clipped_episodezones_v241.shp")
base <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")
base <- base %>% filter(active==1) %>% select(epi_id,year,gwno_a) %>% distinct(epi_id,year, .keep_all = T)
clippedzones <- left_join(clippedzones,base, by = c("epi_id"))
names(clippedzones)
india <- clippedzones %>% filter(gwno_a==750)
india2000 <- india %>% filter(year==2000)


cshapes <- cshp(date = NA, useGW = TRUE, dependencies = FALSE) %>%
  filter(end == "2019-12-31")

cshapes <- cshapes %>%
  mutate(geometry = if_else(st_is_valid(geometry), geometry, st_make_valid(geometry)))

names(cshapes)
names(india2000)
india2000 <- india2000 %>% filter(epi_id!="227_2" & epi_id!="364_1")

ggplot() +
  geom_sf(data = cshapes, fill = "gray", color = "white") +  
  geom_sf(data = india2000, aes(fill = as.factor(epi_id)), color = "black", alpha = 0.6, size = 0.25) +  
  scale_fill_viridis_d(name = "Epi ID", option = "C") +  
  coord_sf(xlim = st_bbox(india2000)[c("xmin", "xmax")], ylim = st_bbox(india2000)[c("ymin", "ymax")], expand = FALSE) +
  theme_minimal() +
  labs(title = "Conflict Zones in Northeast India in 2000", subtitle = "") +
  theme(legend.position = "bottom")


# Overlap has to be calculated by year (otherwise we also include overlap between zones of different and temporally distinct episodes of the same conflict)

# Unclipped zones (and in which years the respective episode is active)
unclippedzones <- st_read("Conflict Zones/b0_episodezones_v241.shp")
base <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")
base <- base %>% filter(active==1) %>% select(epi_id,year) %>% distinct(epi_id,year)
unclippedzones <- left_join(unclippedzones,base, by = c("epi_id"))
unclippedzones <- st_make_valid(unclippedzones)

# for 1995
zones_1995 <- unclippedzones %>% 
  filter(year == 1995) 
zones_1995$area <- st_area(zones_1995)
onebigzone <- zones_1995 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_1995 %>% st_intersection(zones_1995)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)

# for 2005
zones_2005 <- unclippedzones %>% 
  filter(year == 2005) 
zones_2005$area <- st_area(zones_2005)
onebigzone <- zones_2005 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_2005 %>% st_intersection(zones_2005)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)

# for 2015
zones_2015 <- unclippedzones %>% 
  filter(year == 2015) 
zones_2015$area <- st_area(zones_2015)
onebigzone <- zones_2015 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_2015 %>% st_intersection(zones_2015)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)


# Unclipped zones (and in which years the respective episode is active)
clippedzones <- st_read("Conflict Zones/clipped_episodezones_v241.shp")
base <- read_rds("GED-to-ACD_conflict_month_final_v241.rds")
base <- base %>% filter(active==1) %>% select(epi_id,year) %>% distinct(epi_id,year)
clippedzones <- left_join(clippedzones,base, by = c("epi_id"))
clippedzones <- st_make_valid(clippedzones)

# for 1995
zones_1995 <- clippedzones %>% 
  filter(year == 1995) 
zones_1995$area <- st_area(zones_1995)
onebigzone <- zones_1995 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_1995 %>% st_intersection(zones_1995)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)

# for 2005
zones_2005 <- clippedzones %>% 
  filter(year == 2005) 
zones_2005$area <- st_area(zones_2005)
onebigzone <- zones_2005 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_2005 %>% st_intersection(zones_2005)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)

# for 2015
zones_2015 <- clippedzones %>% 
  filter(year == 2015) 
zones_2015$area <- st_area(zones_2015)
onebigzone <- zones_2015 %>% mutate(grouper = 1)
table(onebigzone$grouper, useNA = "always")
onebigzone <- onebigzone %>%
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% ungroup()
onebigzone <- st_make_valid(onebigzone)
onebigzone$area <- st_area(onebigzone)
intersection <- zones_2015 %>% st_intersection(zones_2015)
intersection <- intersection %>% mutate(same = ifelse(epi_id == epi_id.1,1,0))
table(intersection$same, useNA = "always")
intersection <- intersection %>% filter(same == 0)
intersection <- st_make_valid(intersection)
sf::sf_use_s2(FALSE)
overlap <- intersection %>% 
  mutate(grouper = 1) %>% 
  group_by(grouper) %>% 
  summarize(geometry = st_union(geometry)) %>% 
  ungroup()
overlap <- st_as_sf(overlap)
overlap <- st_make_valid(overlap)
overlap$area <- st_area(overlap)
percentage <- overlap$area/onebigzone$area*100
print(percentage)
