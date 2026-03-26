# Code for CPS Figures 2, 3, 4, 7 and Appendix Figures A6, A7, A9, as well as Appendix Tables A7, A8, A9

# List of required packages
required_packages <- c("dplyr", "ggplot2", "tidyr", "reshape2", "plyr", "janitor", "xtable", "ggpubr", "scales")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}

# Load all required libraries
lapply(required_packages, library, character.only = TRUE)

setwd('~/Dropbox/CPS/CPS_Materials') # set working directory for the project

# start with the cleaned data from Step 1
HUNdata <- read.csv(file="HungarianProtests_cleaned.csv", header=TRUE)

# (1) Create 4 datasets by subsetting our cleaned protest data: (1) right, (2) mainright, (3) radright, (4) left
right <- subset(HUNdata, right_all==1)
mainright <- subset(HUNdata, rightist==1)
radright <- subset(HUNdata, radrightist==1)
left <- subset(HUNdata, leftist==1)

# Figure 2 data preparation
# Aggregate radical right protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_selsojobb <- ddply(HUNdata,.(v004year),summarize,count.protest_selsojobb=sum(radrightist))
numprotest_selsojobb <- ddply(HUNdata,.(v004year),summarize, num.radright.protestors=mean(number_protestors[radrightist==1], na.rm=T))
duration_selsojobb <- ddply(HUNdata,.(v004year),summarize, dur.radright.protest=mean(protest_duration[radrightist==1], na.rm=T))
salience_selsojobb <- ddply(HUNdata,.(v004year),summarize, salience.radright.protestors=mean(salience[radrightist==1], na.rm=T))

# Aggregate mainstream right protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_centerjobb <- ddply(HUNdata,.(v004year),summarize,count.protest_jobb=sum(rightist))
numprotest_centerjobb <- ddply(HUNdata,.(v004year),summarize, num.rightist.protestors=mean(number_protestors[rightist==1], na.rm=T))
duration_centerjobb <- ddply(HUNdata,.(v004year),summarize, dur.rightist.protest=mean(protest_duration[rightist==1], na.rm=T))
salience_centerjobb <- ddply(HUNdata,.(v004year),summarize, salience.rightist.protestors=mean(salience[rightist==1], na.rm=T))

# Aggregate left protest count by year, then aggregate average number of protesters, average protest duration, average salience by year
protest_bal <- ddply(HUNdata,.(v004year),summarize,count.protest_bal=sum(leftist))
numprotest_bal <- ddply(HUNdata,.(v004year),summarize, num.leftist.protestors=mean(number_protestors[leftist==1], na.rm=T))
duration_bal <- ddply(HUNdata,.(v004year),summarize, dur.leftist.protest=mean(protest_duration[leftist==1], na.rm=T))
salience_bal <- ddply(HUNdata,.(v004year),summarize, salience.leftist.protestors=mean(salience[leftist==1], na.rm=T))

# Put this all in a dataframe of partisan protest yearly volume and average yearly size
partisanDF_size <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, numprotest_selsojobb, numprotest_centerjobb, numprotest_bal)

# Keep only the necessary (non-redundant v004year columns) after combining these dataframes 
# and filter out the first few years where we have NAs for partisan protest, starting with the 1994 election year
partisanDF_size <- subset(partisanDF_size, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "num.radright.protestors", "num.rightist.protestors", "num.leftist.protestors"))
partisanDF_size <-subset(partisanDF_size, v004year>1993)

#let's reshape these dataframes: one for the counts, one for the number of partisan protestors
partisan_counts <- reshape2::melt(partisanDF_size[,c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb")], id=c("v004year"))
partisan_size <- reshape2::melt(partisanDF_size[,c("v004year", "num.radright.protestors", "num.rightist.protestors", "num.leftist.protestors")], id=c("v004year"))

partisan_counts <- partisan_counts %>%
  mutate(Ideology = recode(variable, count.protest_bal = 'Left', count.protest_jobb = 'Mainstream right', count.protest_selsojobb =  'Radical right' ))
partisan_counts  <- subset(partisan_counts , select=c("v004year", "value", "Ideology"))
partisan_counts <-partisan_counts %>% 
  dplyr::rename("Count" = "value")

partisan_size <- partisan_size %>%
  mutate(Ideology = recode(variable, num.leftist.protestors = 'Left', num.rightist.protestors = 'Mainstream right', num.radright.protestors =  'Radical right' ))
partisan_size  <- subset(partisan_size , select=c("v004year", "value", "Ideology"))
partisan_size <- partisan_size %>% 
  dplyr::rename("Size (participants)"= "value")

df_countssize <- merge(partisan_counts, partisan_size)

BalJobb_size <- ggplot(df_countssize, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Size (participants)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 15)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Size",
    x = "Year", 
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_size


# Put this all in a dataframe of partisan protest yearly volume and average yearly duration
partisanDF_duration <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, duration_selsojobb, duration_centerjobb, duration_bal)
partisanDF_duration <- subset(partisanDF_duration, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "dur.radright.protest", "dur.rightist.protest", "dur.leftist.protest"))
partisanDF_duration <- subset(partisanDF_duration, v004year>1993)

partisan_duration <- reshape2::melt(partisanDF_duration[,c("v004year", "dur.radright.protest", "dur.rightist.protest", "dur.leftist.protest")], id=c("v004year"))

partisan_duration <- partisan_duration %>%
  mutate(Ideology = recode(variable, dur.leftist.protest = 'Left', dur.rightist.protest = 'Mainstream right', dur.radright.protest =  'Radical right' ))
partisan_duration  <- subset(partisan_duration , select=c("v004year", "value", "Ideology"))
partisan_duration <- partisan_duration %>% 
  dplyr::rename("Duration (days)"= "value")

df_countsduration <- merge(partisan_counts, partisan_duration)

BalJobb_dur <- ggplot(df_countsduration, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Duration (days)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 15)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Duration",
    x = "Year", 
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_dur

# Put this all in a dataframe of partisan protest yearly volume and average yearly salience
partisanDF_salience <- data.frame(protest_bal,protest_centerjobb, protest_selsojobb, salience_selsojobb, salience_centerjobb, salience_bal)
partisanDF_salience <- subset(partisanDF_salience, select=c("v004year", "count.protest_bal","count.protest_jobb", "count.protest_selsojobb", "salience.radright.protestors", "salience.rightist.protestors", "salience.leftist.protestors"))
partisanDF_salience <-subset(partisanDF_salience, v004year>1993)

partisan_salience <- reshape2::melt(partisanDF_salience[,c("v004year", "salience.radright.protestors", "salience.rightist.protestors", "salience.leftist.protestors")], id=c("v004year"))

partisan_salience <- partisan_salience %>%
  mutate(Ideology = recode(variable, salience.leftist.protestors = 'Left', salience.rightist.protestors = 'Mainstream right', salience.radright.protestors =  'Radical right' ))
partisan_salience  <- subset(partisan_salience , select=c("v004year", "value", "Ideology"))
partisan_salience <- partisan_salience %>% 
  dplyr::rename("Salience (0-9)"= "value")

df_countssalience <- merge(partisan_counts, partisan_salience)

BalJobb_salience <- ggplot(df_countssalience, aes(v004year, y = Count, color = Ideology)) +
  geom_line() +
  geom_point(aes(size = `Salience (0-9)`)) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 1998, 2002, 2006, 2010)) +
  scale_size(range = c(0, 10)) +
  scale_color_grey(
    name = "Ideology",
    labels = c("Left", "Mainstream right", "Radical right"),
    start = 0.9, end = 0.2
  ) +
  labs(
    title = "Partisan Protests in Hungary, 1994-2011",
    subtitle = "Bubble Size Represents Average Annual Partisan Protest Salience",
    x = "Year",
    y = "Count of Protests",
    color = "Ideology"
  ) +
  guides(
    size = guide_legend(order = 1),
    color = guide_legend(order = 2)
  ) +
  theme_classic(base_size = 12)

BalJobb_salience

# Now put all three sub-figures (size, duration, salience) together for Figure 2 in the main text
BubblePlots <- ggarrange(BalJobb_size, BalJobb_dur, BalJobb_salience,
                         labels = c("", ""),
                         ncol = 1, nrow = 3,
                         common.legend =F,
                         legend = "right")
BubblePlots
ggsave(BubblePlots, file="./Figure2_BubblePlots.pdf", height = 13, width = 8, dpi = 250)

# Now let's create Figure 4, on left and right protest demands. These are derived from the v080 variables classifying demands.

discriminatory <- subset(HUNdata, v08025==1)
antielite <- subset(HUNdata, v08023==1)
antisystem <- subset(HUNdata, v08024==1)
antilegalsystem <- subset(HUNdata, v08022==1)
antiliberalism <- subset(HUNdata, v08010==1)

protest_discriminatory <- ddply(discriminatory,.(v004year),summarize,count.protest_discriminatory=sum(right_all))
protest_antielite <- ddply(antielite,.(v004year),summarize,count.protest_antielite=sum(right_all))
protest_antisystem <- ddply(antisystem,.(v004year),summarize,count.protest_antisystem=sum(right_all))
protest_antilegalsystem <- ddply(antilegalsystem,.(v004year),summarize,count.protest_antilegalsystem=sum(right_all))
protest_antiliberalism <- ddply(antiliberalism,.(v004year),summarize,count.protest_antiliberalism=sum(right_all))

# Combine datasets into a list
datasets <- list(protest_antiliberalism, protest_antilegalsystem, protest_discriminatory, protest_antielite, protest_antisystem)

# Merge all datasets by "id" using full join
protestdemands_df <- purrr::reduce(datasets, full_join, by = "v004year")
protestdemands_df[is.na(protestdemands_df)] <- 0
print(protestdemands_df)

colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antiliberalism'] <- 'Anti-liberal'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antilegalsystem'] <- 'Anti-legal system'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_discriminatory'] <- 'Discriminatory'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antielite'] <- 'Anti-elite'
colnames(protestdemands_df)[colnames(protestdemands_df) == 'count.protest_antisystem'] <- 'Anti-system'
long_protestdemands_df <- reshape2::melt(protestdemands_df,id="v004year")
long_protestdemands_df$Ideology <- 'Mainstream or radical right'

AllRight <- ggplot(long_protestdemands_df, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Anti-liberal", "Anti-legal system", "Discriminatory", "Anti-elite", "Anti-system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Right Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

AllRight

protest_discriminatory_mainright <- ddply(discriminatory,.(v004year),summarize,count.protest_discriminatory=sum(rightist))
protest_antielite_mainright <- ddply(antielite,.(v004year),summarize,count.protest_antielite=sum(rightist))
protest_antisystem_mainright <- ddply(antisystem,.(v004year),summarize,count.protest_antisystem=sum(rightist))
protest_antilegalsystem_mainright <- ddply(antilegalsystem,.(v004year),summarize,count.protest_antilegalsystem=sum(rightist))
protest_antiliberalism_mainright <- ddply(antiliberalism,.(v004year),summarize,count.protest_antiliberalism=sum(rightist))

# Combine datasets into a list
datasets_mainright <- list(protest_antiliberalism_mainright, protest_antilegalsystem_mainright, protest_discriminatory_mainright,
                 protest_antielite_mainright, protest_antisystem_mainright)

# Merge all datasets by "id" using full join
protestdemands_df_mainright <- purrr::reduce(datasets_mainright, full_join, by = "v004year")
protestdemands_df_mainright[is.na(protestdemands_df_mainright)] <- 0
print(protestdemands_df_mainright)

colnames(protestdemands_df_mainright)[colnames(protestdemands_df_mainright) == 'count.protest_antiliberalism'] <- 'Anti-liberal'
colnames(protestdemands_df_mainright)[colnames(protestdemands_df_mainright) == 'count.protest_antilegalsystem'] <- 'Anti-legal system'
colnames(protestdemands_df_mainright)[colnames(protestdemands_df_mainright) == 'count.protest_discriminatory'] <- 'Discriminatory'
colnames(protestdemands_df_mainright)[colnames(protestdemands_df_mainright) == 'count.protest_antielite'] <- 'Anti-elite'
colnames(protestdemands_df_mainright)[colnames(protestdemands_df_mainright) == 'count.protest_antisystem'] <- 'Anti-system'
long_protestdemands_df_mainright <- reshape2::melt(protestdemands_df_mainright,id="v004year")
long_protestdemands_df_mainright$Ideology <- 'Mainstream right'

MainstreamRight <- ggplot(long_protestdemands_df_mainright, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Anti-liberal", "Anti-legal system", "Discriminatory", "Anti-elite", "Anti-system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Mainstream Right Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

MainstreamRight

protest_discriminatory_radright <- ddply(discriminatory,.(v004year),summarize,count.protest_discriminatory=sum(radrightist))
protest_antielite_radright <- ddply(antielite,.(v004year),summarize,count.protest_antielite=sum(radrightist))
protest_antisystem_radright <- ddply(antisystem,.(v004year),summarize,count.protest_antisystem=sum(radrightist))
protest_antilegalsystem_radright <- ddply(antilegalsystem,.(v004year),summarize,count.protest_antilegalsystem=sum(radrightist))
protest_antiliberalism_radright <- ddply(antiliberalism,.(v004year),summarize,count.protest_antiliberalism=sum(radrightist))

# Combine datasets into a list
datasets_radright <- list(protest_antiliberalism_radright, protest_antilegalsystem_radright, protest_discriminatory_radright,
                           protest_antielite_radright, protest_antisystem_radright)

# Merge all datasets by "id" using full join
protestdemands_df_radright <- purrr::reduce(datasets_radright, full_join, by = "v004year")
protestdemands_df_radright[is.na(protestdemands_df_radright)] <- 0
print(protestdemands_df_radright)

colnames(protestdemands_df_radright)[colnames(protestdemands_df_radright) == 'count.protest_antiliberalism'] <- 'Anti-liberal'
colnames(protestdemands_df_radright)[colnames(protestdemands_df_radright) == 'count.protest_antilegalsystem'] <- 'Anti-legal system'
colnames(protestdemands_df_radright)[colnames(protestdemands_df_radright) == 'count.protest_discriminatory'] <- 'Discriminatory'
colnames(protestdemands_df_radright)[colnames(protestdemands_df_radright) == 'count.protest_antielite'] <- 'Anti-elite'
colnames(protestdemands_df_radright)[colnames(protestdemands_df_radright) == 'count.protest_antisystem'] <- 'Anti-system'
long_protestdemands_df_radright <- reshape2::melt(protestdemands_df_radright,id="v004year")
long_protestdemands_df_radright$Ideology <- 'Radical right'

RadicalRight <- ggplot(long_protestdemands_df_radright, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Anti-liberal", "Anti-legal system", "Discriminatory", "Anti-elite", "Anti-system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Radical Right Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

RadicalRight

rightvradright <- ggarrange(MainstreamRight, RadicalRight,
                            labels = c("", ""),
                            ncol = 1, nrow = 2)
rightvradright

ggsave(rightvradright, file="./Appendix_FigureA7.pdf", height = 10, width = 8, dpi = 250)

# Now let's do the same analysis but for leftist demands in leftist protests
democracy <- subset(HUNdata, v08011==1)
individualrights <- subset(HUNdata, v08014==1)
law <- subset(HUNdata, v08013==1)
socialequity <- subset(HUNdata, v08015==1)
workdignity <- subset(HUNdata, v08005==1)

protest_democracy_left <- ddply(democracy,.(v004year),summarize,count.protest_democracy=sum(leftist))
protest_individualrights_left <- ddply(individualrights,.(v004year),summarize,count.protest_individualrights=sum(leftist))
protest_law_left <- ddply(law,.(v004year),summarize,count.protest_law=sum(leftist))
protest_socialequity_left <- ddply(socialequity,.(v004year),summarize,count.protest_socialequity=sum(leftist))
protest_workdignity_left <- ddply(workdignity,.(v004year),summarize,count.protest_workdignity=sum(leftist))

# Combine datasets into a list
datasets_left <- list(protest_workdignity_left, protest_socialequity_left, protest_individualrights_left,
                      protest_democracy_left,  protest_law_left)
                    
# Merge all datasets by "id" using full join
protestdemands_df_left<- purrr::reduce(datasets_left, full_join, by = "v004year")
protestdemands_df_left[is.na(protestdemands_df_left)] <- 0
print(protestdemands_df_left)

colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_workdignity'] <- 'Labor rights'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_democracy'] <- 'Pro-democracy'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_individualrights'] <- 'Individual rights'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_socialequity'] <- 'Social equity'
colnames(protestdemands_df_left)[colnames(protestdemands_df_left) == 'count.protest_law'] <- 'Pro-legal system'
long_protestdemands_df_left <- reshape2::melt(protestdemands_df_left,id="v004year")

Left <- ggplot(long_protestdemands_df_left, aes(v004year, y = value, color = variable)) +
  geom_line() +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(1994, 2011), breaks = c(1994, 2002, 2010)) +
  ylim(0, 150) +
  scale_color_grey(
    name = "Protest type",
    labels = c("Labor rights", "Social equity", "Individual rights", "Pro-democracy", "Pro-legal system"),
    start = 0.8, end = 0.2
  ) +
  facet_wrap(~variable, nrow = 1) +
  labs(
    title = "Left Protests by Demand, 1994-2011",
    x = "Year",
    y = "Count of Protests"
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text.x = element_text(size = 8)
  )

Left


Figure4 <- ggarrange(AllRight, Left,
                         labels = c("", ""),
                         ncol = 1, nrow = 2,
                         common.legend =F,
                         legend = "right")
Figure4
ggsave(Figure4, file="./Figure4_ProtestDemands.pdf", height = 10, width = 8, dpi = 250)


# Now we move on to data preparation for Figure 3
left$ideology <-'Left'
right$ideology <-'Right'
agghun_R<-ddply(right,.(v004year),numcolwise(sum, na.rm = TRUE))			# Aggregate numeric variables 
agghun_L<-ddply(left,.(v004year),numcolwise(sum, na.rm = TRUE))				# Aggregate numeric variables 

# Figure on methods of protest
protest_methods_R <-data.frame(agghun_R,"action_nondisruptive","action_disruptive")
protest_methods_R <- subset(protest_methods_R, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_R <- reshape2::melt(protest_methods_R,id="v004year")
long_protest_methods_R$Ideology <-'Right'

protest_methods_L <-data.frame(agghun_L,"action_nondisruptive","action_disruptive")
protest_methods_L <- subset(protest_methods_L, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_L <- reshape2::melt(protest_methods_L,id="v004year")
long_protest_methods_L$Ideology <-'Left'

long_protest_methods <-rbind(long_protest_methods_R, long_protest_methods_L)
colnames(long_protest_methods)[colnames(long_protest_methods) == "variable"] ="Method"
long_protest_methods$Method<-as.character(long_protest_methods$Method)
long_protest_methods$Method[long_protest_methods$Method=='action_nondisruptive']<-'Non-disruptive'
long_protest_methods$Method[long_protest_methods$Method=='action_disruptive']<-'Disruptive'

top_Figure3 <- ggplot(long_protest_methods,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Protest Methods in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                            color = guide_legend(order = 2))
top_Figure3

# v026 asks if protest actions were legal. "1" means no (e.g., illegal)
protest_jobb_illeg <- ddply(right,.(v004year),summarize,count.protest_jobbilleg=sum(v026==1))
protest_jobb_viol <- ddply(right,.(v004year),summarize,count.protest_jobbviolent=sum(action_violent==1))
protest_bal_illeg <- ddply(left,.(v004year),summarize,count.protest_balilleg=sum(v026==1))
protest_bal_viol <- ddply(left,.(v004year),summarize,count.protest_balviolent=sum(action_violent==1))

partisanDF_jobb <- merge(protest_jobb_illeg, protest_jobb_viol,  by="v004year", all = T)
partisanDF_bal <- merge(protest_bal_illeg, protest_bal_viol,  by="v004year", all = T)
partisanDF_types  <- merge(partisanDF_bal, partisanDF_jobb,  by="v004year", all = T)
long_partisanDFtypes <- reshape2::melt(partisanDF_types,id="v004year")

long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_jobbilleg" |
                                long_partisanDFtypes$variable=="count.protest_jobbviolent"] <- "Right"
long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_balilleg" |
                                long_partisanDFtypes$variable=="count.protest_balviolent"] <- "Left"

long_partisanDFtypes$Method<-NA
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbilleg' | long_partisanDFtypes$variable=='count.protest_balilleg']<-'Illegal'
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbviolent' | long_partisanDFtypes$variable=='count.protest_balviolent']<-'Aggressive'

bottom_Figure3 <- ggplot(long_partisanDFtypes,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Extreme Disruptive Protests in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                                    color = guide_legend(order = 2))
bottom_Figure3

Figure3 <- ggarrange(top_Figure3, bottom_Figure3,
                         labels = c("", ""),
                         ncol = 1, nrow = 2,
                         common.legend =F,
                         legend = "right")
Figure3 

ggsave(Figure3, file="./Figure3_ProtestMethods.pdf", height = 10, width = 8, dpi = 250)

# For Appendix Figure A6, we want to disaggregate mainstream right + radical right
agghun_mainright<-ddply(mainright,.(v004year),numcolwise(sum, na.rm = TRUE))
agghun_rad<-ddply(radright,.(v004year),numcolwise(sum, na.rm = TRUE))			

protest_methods_mainright <-data.frame(agghun_mainright,"action_nondisruptive","action_disruptive")
protest_methods_mainright <- subset(protest_methods_mainright, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_mainright <- reshape2::melt(protest_methods_mainright,id="v004year")
long_protest_methods_mainright$Ideology <-'Mainstream right'

protest_methods_radright <-data.frame(agghun_rad,"action_nondisruptive","action_disruptive")
protest_methods_radright <- subset(protest_methods_radright, select=c("v004year", "action_nondisruptive","action_disruptive"))
long_protest_methods_radright <- reshape2::melt(protest_methods_radright,id="v004year")
long_protest_methods_radright$Ideology <-'Radical right'

long_protest_methods_disagg <-rbind(long_protest_methods_mainright, long_protest_methods_radright)
colnames(long_protest_methods_disagg)[colnames(long_protest_methods_disagg) == "variable"] ="Method"
long_protest_methods_disagg$Method<-as.character(long_protest_methods_disagg$Method)
long_protest_methods_disagg$Method[long_protest_methods_disagg$Method=='action_nondisruptive']<-'Non-disruptive'
long_protest_methods_disagg$Method[long_protest_methods_disagg$Method=='action_disruptive']<-'Disruptive'

top_FigureA6 <- ggplot(long_protest_methods_disagg,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Protest Methods in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                            color = guide_legend(order = 2))
top_FigureA6

protest_jobb_illeg <- ddply(mainright,.(v004year),summarize,count.protest_jobbilleg=sum(v026==1))
protest_jobb_viol <- ddply(mainright,.(v004year),summarize,count.protest_jobbviolent=sum(action_violent==1))
protest_radjobb_illeg <- ddply(radright,.(v004year),summarize,count.protest_radjobbilleg=sum(v026==1))
protest_radjobb_viol <- ddply(radright,.(v004year),summarize,count.protest_radjobbviolent=sum(action_violent==1))
partisanDF_jobb <- merge(protest_jobb_illeg, protest_jobb_viol,  by="v004year", all = T)
partisanDF_radjobb <- merge(protest_radjobb_illeg, protest_radjobb_viol,  by="v004year", all = T)
partisanDF_types  <- merge(partisanDF_radjobb, partisanDF_jobb,  by="v004year", all = T)
long_partisanDFtypes <- reshape2::melt(partisanDF_types,id="v004year")

long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_jobbilleg" |
                                long_partisanDFtypes$variable=="count.protest_jobbviolent"] <- "Mainstream right"
long_partisanDFtypes$Ideology[long_partisanDFtypes$variable=="count.protest_radjobbilleg" |
                                long_partisanDFtypes$variable=="count.protest_radjobbviolent"] <- "Radical right"

long_partisanDFtypes$Method<-NA
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbilleg' | long_partisanDFtypes$variable=='count.protest_radjobbilleg']<-'Illegal'
long_partisanDFtypes$Method[long_partisanDFtypes$variable=='count.protest_jobbviolent' | long_partisanDFtypes$variable=='count.protest_radjobbviolent']<-'Aggressive'

bottom_FigureA6 <- ggplot(long_partisanDFtypes,aes(v004year, y=value, color=Ideology, linetype=Method)) +
  geom_line() +geom_point(size=3) + xlab("Year") + ylab("Count of Protests") +
  scale_x_continuous(limits=c(1994,2011), breaks=c(1994,1998,2002,2006,2010)) + scale_color_grey() +
  theme_classic(base_size = 12)+
  ggtitle("Extreme Disruptive Protests in Hungary, 1994-2011") + guides(linetype= guide_legend(order = 1), 
                                                                        color = guide_legend(order = 2))
bottom_FigureA6

FigureA6 <- ggarrange(top_FigureA6,bottom_FigureA6,
                         labels = c("", ""),
                         ncol = 1, nrow = 2,
                         common.legend =F,
                         legend = "right")
FigureA6 

ggsave(FigureA6, file="./Appendix_FigureA6.pdf", height = 10, width = 8, dpi = 250)


# Appendix Figure A9 (monthly frequency of protest events) preparation

# Create a new cleaned year, month, day variable.
# This includes recording (as NA) 2 cases of "0" and 1 case of "19" for month (presumably due to input error)
# We will also visualize election periods by shading those periods.
HUNdata$year <- HUNdata$v004year
HUNdata$month <- HUNdata$v004month
HUNdata$month[HUNdata$month==0 | HUNdata$month==19] <- NA 
HUNdata$day <- HUNdata$v004day

testHUN <- ddply(HUNdata, .(year, month), summarise,
                 count.protest=length(unique(uniqueID)))
# We are only looking for month-level specificity, so assigning first day of the month to each event for visual ease.
testHUN$day <- "01"
testHUN$month[testHUN$month==1] <- "01"
testHUN$month[testHUN$month==2] <- "02"
testHUN$month[testHUN$month==3] <- "03"
testHUN$month[testHUN$month==4] <- "04"
testHUN$month[testHUN$month==5] <- "05"
testHUN$month[testHUN$month==6] <- "06"
testHUN$month[testHUN$month==7] <- "07"
testHUN$month[testHUN$month==8] <- "08"
testHUN$month[testHUN$month==9] <- "09"
testHUN$month[testHUN$month==10] <- "10"
testHUN$month[testHUN$month==11] <- "11"
testHUN$month[testHUN$month==12] <- "12"

# We have such little protest data in 1988 that we will remove it altogether from this figure.
testHUN <- subset(testHUN, testHUN$year!=1988)
# We a few missing values for months due to the code above recording the month 0 and 19s as NAs
testHUN <- na.omit(testHUN)
# Now let's use the strptime function to create an interpretable date variable.
testHUN$date <- strptime(paste(testHUN$year, testHUN$month, testHUN$day, sep="-"), 
                         format="%Y-%m-%d")

# Finally, we add some annotations to shade the areas before and after each election 
FigureA9 <- ggplot(testHUN, aes(x=as.POSIXct(date), y=count.protest)) + geom_point(size=2) + geom_line(size=1) + 
  xlab("Year and month") + ylab("Count of Protests") + ggtitle("Monthly Protests in Hungary (1989-2011)") + theme_classic(base_size=12)+
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  scale_x_datetime(labels=date_format("%b %Y"),
                   breaks=c(as.POSIXct("1989/09/01"),as.POSIXct("1990/03/01"),as.POSIXct("1990/09/01"),
                            as.POSIXct("1993/11/01"),as.POSIXct("1994/05/01"),as.POSIXct("1994/11/01"),
                            as.POSIXct("1997/11/01"),as.POSIXct("1998/05/01"),as.POSIXct("1998/11/01"),
                            as.POSIXct("2001/10/01"),as.POSIXct("2002/04/01"),as.POSIXct("2002/10/01"),
                            as.POSIXct("2005/10/01"),as.POSIXct("2006/04/01"),as.POSIXct("2006/10/01"),
                            as.POSIXct("2009/10/01"),as.POSIXct("2010/04/01"),as.POSIXct("2010/10/01")), 
                   limits=c(as.POSIXct("1989/01/01"),as.POSIXct("2011/12/01"))) +
  ggplot2::annotate("rect", xmin=as.POSIXct("1989/09/01"), xmax=as.POSIXct("1990/09/01"), ymin=0, ymax=70, alpha=0.2) +
  ggplot2::annotate("rect", xmin=as.POSIXct("1993/11/01"), xmax=as.POSIXct("1994/11/01"), ymin=0, ymax=70, alpha=0.2) +
  ggplot2::annotate("rect", xmin=as.POSIXct("1997/11/01"), xmax=as.POSIXct("1998/11/01"), ymin=0, ymax=70, alpha=0.2) +
  ggplot2::annotate("rect", xmin=as.POSIXct("2001/10/01"), xmax=as.POSIXct("2002/10/01"), ymin=0, ymax=70, alpha=0.2) +
  ggplot2::annotate("rect", xmin=as.POSIXct("2005/10/01"), xmax=as.POSIXct("2006/10/01"), ymin=0, ymax=70, alpha=0.2) +
  ggplot2::annotate("rect", xmin=as.POSIXct("2009/10/01"), xmax=as.POSIXct("2010/10/01"), ymin=0, ymax=70, alpha=0.2) +
  geom_smooth(stat="smooth", method="loess", se=FALSE, size=2, colour="gray60")
FigureA9
ggsave(FigureA9, file="./Appendix_FigureA9.pdf", height = 6, width = 9, dpi = 250)


HUNdata_Left<- subset(HUNdata, leftist==1)
testHUN_L <- ddply(HUNdata_Left, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_L$day <- "01"
testHUN_L$month[testHUN_L$month==1] <- "01"
testHUN_L$month[testHUN_L$month==2] <- "02"
testHUN_L$month[testHUN_L$month==3] <- "03"
testHUN_L$month[testHUN_L$month==4] <- "04"
testHUN_L$month[testHUN_L$month==5] <- "05"
testHUN_L$month[testHUN_L$month==6] <- "06"
testHUN_L$month[testHUN_L$month==7] <- "07"
testHUN_L$month[testHUN_L$month==8] <- "08"
testHUN_L$month[testHUN_L$month==9] <- "09"
testHUN_L$month[testHUN_L$month==10] <- "10"
testHUN_L$month[testHUN_L$month==11] <- "11"
testHUN_L$month[testHUN_L$month==12] <- "12"
testHUN_L <- subset(testHUN_L, testHUN_L$year!=1988)
testHUN_L <- na.omit(testHUN_L)
testHUN_L$date <- strptime(paste(testHUN_L$year, testHUN_L$month, testHUN_L$day, sep="-"), 
                           format="%Y-%m-%d")

HUNdata_Rad<- subset(HUNdata, radrightist==1)
testHUN_Rad <- ddply(HUNdata_Rad, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_Rad$day <- "01"
testHUN_Rad$month[testHUN_Rad$month==1] <- "01"
testHUN_Rad$month[testHUN_Rad$month==2] <- "02"
testHUN_Rad$month[testHUN_Rad$month==3] <- "03"
testHUN_Rad$month[testHUN_Rad$month==4] <- "04"
testHUN_Rad$month[testHUN_Rad$month==5] <- "05"
testHUN_Rad$month[testHUN_Rad$month==6] <- "06"
testHUN_Rad$month[testHUN_Rad$month==7] <- "07"
testHUN_Rad$month[testHUN_Rad$month==8] <- "08"
testHUN_Rad$month[testHUN_Rad$month==9] <- "09"
testHUN_Rad$month[testHUN_Rad$month==10] <- "10"
testHUN_Rad$month[testHUN_Rad$month==11] <- "11"
testHUN_Rad$month[testHUN_Rad$month==12] <- "12"
testHUN_Rad <- subset(testHUN_Rad, testHUN_Rad$year!=1988)
testHUN_Rad <- na.omit(testHUN_Rad)
testHUN_Rad$date <- strptime(paste(testHUN_Rad$year, testHUN_Rad$month, testHUN_Rad$day, sep="-"), 
                             format="%Y-%m-%d")

HUNdata_Right<- subset(HUNdata, rightist==1)
testHUN_Right <- ddply(HUNdata_Right, .(year, month), summarise, count.protest=length(unique(uniqueID)))
testHUN_Right$day <- "01"
testHUN_Right$month[testHUN_Right$month==1] <- "01"
testHUN_Right$month[testHUN_Right$month==2] <- "02"
testHUN_Right$month[testHUN_Right$month==3] <- "03"
testHUN_Right$month[testHUN_Right$month==4] <- "04"
testHUN_Right$month[testHUN_Right$month==5] <- "05"
testHUN_Right$month[testHUN_Right$month==6] <- "06"
testHUN_Right$month[testHUN_Right$month==7] <- "07"
testHUN_Right$month[testHUN_Right$month==8] <- "08"
testHUN_Right$month[testHUN_Right$month==9] <- "09"
testHUN_Right$month[testHUN_Right$month==10] <- "10"
testHUN_Right$month[testHUN_Right$month==11] <- "11"
testHUN_Right$month[testHUN_Right$month==12] <- "12"
testHUN_Right <- subset(testHUN_Right, testHUN_Right$year!=1988)
testHUN_Right <- na.omit(testHUN_Right)
testHUN_Right$date <- strptime(paste(testHUN_Right$year, testHUN_Right$month, testHUN_Right$day, sep="-"), 
                               format="%Y-%m-%d")

testHUN_Right<-subset(testHUN_Right, year > 2009)
testHUN_Right$ideology <- "Mainstream right"
testHUN_Rad<-subset(testHUN_Rad, year > 2009)
testHUN_Rad$ideology <- "Radical right"
testHUN_L<-subset(testHUN_L, year > 2009)
testHUN_L$ideology <- "Left"

FideszEra_leftright <- dplyr::full_join(testHUN_L, testHUN_Right)
FideszEra_protests <- dplyr::full_join(FideszEra_leftright, testHUN_Rad)

Figure7 <- ggplot(FideszEra_protests, aes(x=as.POSIXct(date), y=count.protest, color=ideology)) + geom_point(size=2) + geom_line(size=1) + 
  xlab("Year and month") + ylab("Count of Protests") + ggtitle("Monthly partisan protests in Hungary (2010-2011)") + 
  scale_color_grey(name="Ideology",labels=c("Left","Mainstream right", "Radical right"),start=.8,end=.2) +
  scale_x_datetime(labels=date_format("%b %Y"),
                   breaks=c(as.POSIXct("2010/01/01"), as.POSIXct("2010/05/01"),
                            as.POSIXct("2010/09/01"),
                            as.POSIXct("2011/01/01"),as.POSIXct("2011/05/01"),
                            as.POSIXct("2011/09/01")),
                   limits=c(as.POSIXct("2010/01/01"),as.POSIXct("2011/12/01"))) + 
  ggplot2::annotate("rect", xmin=as.POSIXct("2010/07/01"), xmax=as.POSIXct("2011/04/01"), ymin=0, ymax=30, alpha=0.2) +
  geom_vline(xintercept = as.POSIXct("2010/04/01")) +
  ggplot2:: annotate("text", x =as.POSIXct("2010/05/10") , y =20, label="2010 \n election")   +
  theme_classic(base_size=12)
Figure7
ggsave(Figure7, file="./Figure7.pdf", height = 6, width = 9, dpi = 250)


# Appendix 2 descriptive tables.

# Table A7: Type of Protest Action
Protest_method <- HUNdata %>%
  # Create separate binary columns for each method
  dplyr::mutate(
    letters_statements_appeals = ifelse(v02416 == 1, 1, 0),
    demonstration_march = ifelse(v02409 == 1, 1, 0),
    legal_action = ifelse(v02417 == 1, 1, 0),
    strike_alert = ifelse(v02415 == 1, 1, 0),
    blockade = ifelse(v02410 == 1, 1, 0),
    strike = ifelse(v02407 == 1, 1, 0),
    rally_meeting = ifelse(v02414 == 1, 1, 0),
    hunger_strike = ifelse(v02420 == 1, 1, 0),
    boycott = ifelse(v02419 == 1, 1, 0)
  ) %>%
  # Summarize counts for each method
  summarise(
    letters_statements_appeals = sum(letters_statements_appeals),
    demonstration_march = sum(demonstration_march),
    legal_action = sum(legal_action),
    strike_alert = sum(strike_alert),
    blockade = sum(blockade),
    strike = sum(strike),
    rally_meeting = sum(rally_meeting),
    hunger_strike = sum(hunger_strike),
    boycott = sum(boycott)
  ) %>%
  # Pivot longer to reshape for percentages
  tidyr::pivot_longer(
    cols = everything(),
    names_to = "method",
    values_to = "count"
  ) %>%
  # Calculate percentages based on total (4836)
  dplyr::mutate(percent = round(100 * count / 4836, digits = 0))

# Print the result
print(xtable(Protest_method, include.rownames = FALSE))


# Table A8: Distribution of Protest Duration
Protest_duration <- HUNdata %>% 
  dplyr::mutate(
    # Create categories
    duration = dplyr::case_when(
      v008 ==1 ~ "<8 hrs",
      v008 ==2 ~ "8-24 hrs",
      v008 ==3 ~ "2-7 days",
      v008 ==4 ~ "8 days – 1 month",
      v008 ==5 ~ "1 month +",
      v008 ==6 ~ "letters, statements",
      v008 ==NA ~ "data unavailable"
    ),
    # Convert to factor
    duration = factor(
      duration,
      level = c("<8 hrs", "8-24 hrs", "2-7 days", "8 days – 1 month", 
                "1 month +", "letters, statements", "data unavailable") )
  )  %>%
  group_by(duration) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  dplyr::mutate(percent = round(100 * count / sum(count), digits = 0))
print(xtable(Protest_duration), include.rownames=FALSE)

# Table A9: Distribution of Protest Participation
Protest_size<- HUNdata %>% 
  #filter(v016 < 9 & v016 > 0) %>% 
  dplyr::mutate(
    # Create categories
    size = dplyr::case_when(
      v016 ==1 ~ "0-20",
      v016 ==2 ~ "21-200",
      v016 ==3 ~ "201-500",
      v016 ==4 ~ "501-1,000",
      v016 ==5 ~ "1,001-2,000",
      v016 ==6 ~ "2,001-10,000",
      v016 ==7 ~ "10,001-50,000",
      v016 ==8 ~ "50,000+",
    ),
    # Convert to factor
    size = factor(
      size,
      level = c("0-20", "21-200", "201-500", "501-1,000", "1,001-2,000",
                "2,001-10,000","10,001-50,000", "50,000+" ) )
  )  %>%
  group_by(size) %>% 
  dplyr::summarise(count = n(), .groups = "drop") %>% 
  dplyr::mutate(percent = round(100 * count / sum(count), digits = 0))
print(xtable(Protest_size), include.rownames=FALSE)


# Section 6.1 in the manuscript, commentary on leftist protest duration before/after 2010
newleft <- subset(left, v004year>2009)
oldleft <- subset(left, v004year<2010)
mean(oldleft$protest_duration, na.rm=T)
mean(newleft$protest_duration, na.rm=T)